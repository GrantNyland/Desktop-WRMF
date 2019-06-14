//
//
//  UNIT      : Contains TDailyDiversionGaugeDataLoadAgent Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 28/08/2006
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UDailyDiversionGaugeDataLoadAgent;

interface

uses
  Classes,
  Contnrs,
  VoaimsCom_TLB,

  UFilesActionAbstractManager,
  UFileNames,
  UFileNameConstants,

  UFilePathsDatabaseAgent,
  UFile14Agent,

  UUtilities,
  UDailyDiversionGaugeSQLAgent,
  UDailyDiversionGaugeData,
  UChannelData,
  UNetworkFeaturesSQLAgent,
  UNetworkFeaturesLoadAgent,
  UDailyIFRData,
  UDataFileObjects,
  UReleaseStructureObject,
  UIFRFeatures,
  UDailyDiversionFileDataObject,
  UAbstractObject;

type
  TDailyDiversionGaugeDataLoadAgent = class(TAbstractAppObject)
  protected
    FSQLAgent: TDailyDiversionGaugeSQLAgent;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function LoadCompensationValues(ADiversionGauge : TDiversionGauge) : boolean;
    function LoadThresholdValues(ADiversionGauge : TDiversionGauge): boolean;
    function LoadCalculatedMonthlyFlowData(ADiversionGauge : TDiversionGauge;ADailyFlowData : TDailyFlowData) : boolean;
    function FlowDiversionRelationLoaded(ADiversionGauge : TDiversionGauge) : boolean;
    function LoadRelationship(AFactor : integer;ADiversionGauge:TDiversionGauge) : boolean;
    function CanGenerateRelationship(ADiversionGauge : TDiversionGauge): boolean;

  public
    function LoadDailyFlowDataStartEndDate(ADiversionGauge : TDiversionGauge) : boolean;
    function LoadDailyFlowData(ADiversionGauge : TDiversionGauge) : boolean;
    function LoadDailyInstreamFlowData(ADiversionGauge : TDiversionGauge) : boolean;
    function LoadMonthlyInstreamFlowData(ADiversionGauge : TDiversionGauge; ADailyInstreamFlowData : TDailyInstreamFlowData) :boolean;
    function ClearDailyFlowDataFromCSVFile(ADiversionGauge : TDiversionGauge) : boolean;
    function ClearDailyInstreamFlowFileData(ADiversionGauge : TDiversionGauge) : boolean;
    function ClearFlowDiversionRelation(ADiversionGauge : TDiversionGauge) : boolean;
    function GenerateFlowDiversionRelation(ADiversionGauge : TDiversionGauge; AStartDate, AEndDate : TDateTime) : boolean;
    function GenerateWRYMData(ADiversionGauge : TDiversionGauge) : boolean;
    function LoadDailyDiversionGaugeData(ADailyDiversionGaugeDataList : TDailyDiversionGaugeDataList) : boolean;
    //function LoadFile14IFRDataFromDB(AStationNo:string;ADailyDiversionGaugeDataList: TDailyDiversionGaugeDataList;ADailyIFRData : TDailyIFRData): boolean;
    function LoadDailyDiversionGauges(ADailyDiversionGaugeDataList: TDailyDiversionGaugeDataList): boolean;
    function LoadFlowDiversionRelation(ADiversionGauge : TDiversionGauge; AStartDate, AEndDate : TDateTime) : boolean;
    function LoadFile14IFRData(ADailyIFRData: TDailyIFRData; ADataObject: TDataFileObjects): boolean;
    function GenerateDailyIFR(ADiversionGauge: TDiversionGauge;ADailyIFRData: TDailyIFRData) : boolean;

end;
implementation
uses
  System.UITypes,
  VCL.Controls,
  SysUtils,
  VCL.Dialogs,
  UDataSetType,
  UConstants,
  UDiversionChannelDialog,
  UChannelDataLoadAgent,
  UDailyDiversionDataObject,
  UErrorHandlingOperations, DB, UAbstractComponent;

{ TDailyDiversionGaugeDataLoadAgent }

procedure TDailyDiversionGaugeDataLoadAgent.CreateMemberObjects;
const OPNAME = 'TDailyDiversionGaugeDataLoadAgent.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FSQLAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyDiversionGaugeDataLoadAgent.DestroyMemberObjects;
const OPNAME = 'TDailyDiversionGaugeDataLoadAgent.DestroyMemberObjects';
begin
  try
    FreeAndNil(FSQLAgent);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeDataLoadAgent.LoadCalculatedMonthlyFlowData(ADiversionGauge: TDiversionGauge;ADailyFlowData : TDailyFlowData): boolean;
const OPNAME = 'TDailyDiversionGaugeDataLoadAgent.LoadCalculatedMonthlyFlowData';
var
  LYear : word;
  LMonth : word;
  LDay : word;
begin
  Result := False;
  try
    if (ADiversionGauge <> nil) and (ADailyFlowData <> nil)then
    begin
      DecodeDate(ADailyFlowData.DiversionDate,LYear,LMonth,LDay);
      ADiversionGauge.AddMonthlyFlowData(LYear,LMonth,ADailyFlowData.QualityCode,
      ADailyFlowData.AvgFlow,DateToStr(ADailyFlowData.DiversionDate),ADailyFlowData.StationID);
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeDataLoadAgent.LoadMonthlyInstreamFlowData(ADiversionGauge : TDiversionGauge; ADailyInstreamFlowData : TDailyInstreamFlowData) :boolean;
const OPNAME = 'TDailyDiversionGaugeDataLoadAgent.LoadMonthlyInstreamFlowData';
var
  LYear : word;
  LMonth : word;
  LDay : word;
begin
  Result := False;
  try
    if (ADiversionGauge <> nil) and (ADailyInstreamFlowData <> nil) then
    begin
      DecodeDate(ADailyInstreamFlowData.InstreamDate,LYear,LMonth,LDay);

      ADiversionGauge.AddMonthlyInstreamFlowData(LYear,LMonth,ADailyInstreamFlowData.QualityCode,
      ADailyInstreamFlowData.AvgFlow, ADailyInstreamFlowData.DailyDiversionFlow[LMonth],ADiversionGauge.StationID);

    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeDataLoadAgent.LoadDailyDiversionGauges(
  ADailyDiversionGaugeDataList: TDailyDiversionGaugeDataList): boolean;
const OPNAME = 'TDailyDiversionGaugeDataLoadAgent.LoadDailyDiversionGauges';
var
  LDiversionGauge : TDiversionGauge;
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    if ADailyDiversionGaugeDataList <> nil then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        if Assigned(LDataSet) then
        begin
          LDataSet.SetSQL(FSQLAgent.GetDailyDiversionStationSQL);
          LDataset.DataSet.Open;
          while (not LDataset.DataSet.EOF) do
          begin
            LDiversionGauge := ADailyDiversionGaugeDataList.AddDiversionGauge;
            LDiversionGauge.PopulateGaugeData(Trim(LDataset.DataSet.FieldByName('StationNo').AsString),
                                        LDataset.DataSet.FieldByName('StationID').AsInteger,
                                        Trim(LDataset.DataSet.FieldByName('Place').AsString),
                                        Trim(LDataset.DataSet.FieldByName('Latitude').AsString),
                                        Trim(LDataset.DataSet.FieldByName('Longitude').AsString),
                                        LDataset.DataSet.FieldByName('CatchmentArea').AsFloat,
                                        LDataset.DataSet.FieldByName('CatchmentScaleFactor').AsFloat);

            LDataset.DataSet.Next;
          end;
        end;
      finally
        LDataset.Free;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{function TDailyDiversionGaugeDataLoadAgent.LoadFile14IFRDataFromDB(AStationNo:string;ADailyDiversionGaugeDataList: TDailyDiversionGaugeDataList;ADailyIFRData : TDailyIFRData): boolean;
const OPNAME = 'TDailyDiversionGaugeDataLoadAgent.LoadFile14IFRDataFromDB';
var
  LNetworkFeaturesLoadAgent : TNetworkFeaturesLoadAgent;
  LChannelDataLoadAgent : TChannelDataLoadAgent;
begin
  Result := False;
  try

    LChannelDataLoadAgent := TChannelDataLoadAgent.Create(FAppModules);
    LNetworkFeaturesLoadAgent := TNetworkFeaturesLoadAgent.Create(FAppModules);
    try
      if ADailyIFRData.ChannelList <> nil then
      begin
        ADailyIFRData.Initialise;
        LChannelDataLoadAgent.LoadGeneralFlowChannels(ADailyIFRData.ChannelList);
        LNetworkFeaturesLoadAgent.LoadIFRFeatures(ADailyIFRData.ChannelList, ADailyIFRData.IFRFeatureList);
        if (ADailyDiversionGaugeDataList <> nil) then
          ADailyIFRData := ADailyDiversionGaugeDataList.AddDailyIFRData(AStationNo);
      end;
    finally
      FreeAndNil(LNetworkFeaturesLoadAgent);
      FreeAndNil(LChannelDataLoadAgent);
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
}
function TDailyDiversionGaugeDataLoadAgent.LoadDailyDiversionGaugeData(
  ADailyDiversionGaugeDataList: TDailyDiversionGaugeDataList): boolean;
const OPNAME = 'TDailyDiversionGaugeDataLoadAgent.LoadDailyDiversionGaugeData';
var
  LDiversionGauge : TDiversionGauge;
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    if ADailyDiversionGaugeDataList <> nil then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        if Assigned(LDataSet) then
        begin
          LDataSet.SetSQL(FSQLAgent.GetDailyDiversionStationSQL);
          LDataset.DataSet.Open;
          while (not LDataset.DataSet.EOF) do
          begin
            LDiversionGauge := ADailyDiversionGaugeDataList.AddDiversionGauge;
            LDiversionGauge.Initialise;
            if LDiversionGauge.PopulateGaugeData(Trim(LDataset.DataSet.FieldByName('StationNo').AsString),
                                        LDataset.DataSet.FieldByName('StationID').AsInteger,
                                        Trim(LDataset.DataSet.FieldByName('Place').AsString),
                                        Trim(LDataset.DataSet.FieldByName('Latitude').AsString),
                                        Trim(LDataset.DataSet.FieldByName('Longitude').AsString),
                                        LDataset.DataSet.FieldByName('CatchmentArea').AsFloat,
                                        LDataset.DataSet.FieldByName('CatchmentScaleFactor').AsFloat) then
            begin
              LoadDailyFlowData(LDiversionGauge);
              LoadDailyFlowDataStartEndDate(LDiversionGauge);
              LDiversionGauge.AddDailyFlowDataGaps;
              LoadDailyInstreamFlowData(LDiversionGauge);
              LoadFlowDiversionRelation(LDiversionGauge, LDiversionGauge.StartDate,LDiversionGauge.EndDate);
            end;
            LDataset.DataSet.Next;
          end;
        end;
      finally
        LDataset.Free;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeDataLoadAgent.LoadDailyFlowData(ADiversionGauge : TDiversionGauge) : boolean;
const OPNAME = 'TDailyDiversionGaugeDataLoadAgent.LoadDailyFlowData';
var
  LDataSet     : TAbstractModelDataset;
  LNonSuspectDailyFlowData,
  LDailyFlowData : TDailyFlowData;
  LAvgFlow : double;
  LFileNameObject: TFileNameObject;
  LFile14Agent: TFile14Agent;
  LModelCode,
  LStudyAreaCode,
  LSubAreaCode,
  LScenarioCode,
  LStationNo,
  LFileName : string;
  LDailyIFRData : TDailyIFRData;
  LFileDataObject : TDailyDiversionFileDataObject;
begin
  Result := False;
  try
    if (ADiversionGauge <> nil) then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      LFileDataObject := TDailyDiversionFileDataObject.Create;
      LFile14Agent := TFile14Agent.Create(FAppModules);
      try
        if Assigned(LDataSet) then
        begin
          if (LoadCompensationValues(ADiversionGauge)) and (LoadThresholdValues(ADiversionGauge))then
          begin

            LDataSet.SetSQL(FSQLAgent.GetDailyDiversionFileDataSQL(ADiversionGauge.StationID));
            LDataset.DataSet.Open;
            Result := True;

            while (not LDataset.DataSet.EOF) do
            begin
              LDailyFlowData := ADiversionGauge.AddDailyFlowData;
              LDailyFlowData.Initialise;
              if LDataSet.DataSet.FieldByName('AvgFlow').IsNull then
                LAvgFlow := NullFloat
              else
                LAvgFlow := LDataSet.DataSet.FieldByName('AvgFlow').AsFloat;

              if LDailyFlowData.Poulate(LDataSet.DataSet.FieldByName('StationID').AsInteger,
                                        LDataSet.DataSet.FieldByName('Identifier').AsInteger,
                                        LDataSet.DataSet.FieldByName('DiversionDate').AsDateTime,
                                        LAvgFlow,
                                        ADiversionGauge.CatchmentScaleFactor,
                                        LDataSet.DataSet.FieldByName('QualityCode').AsInteger) then
              LoadCalculatedMonthlyFlowData(ADiversionGauge,LDailyFlowData);

              if (LDataSet.DataSet.FieldByName('QualityCode').AsInteger in [1,2,3,4,5,6,7,50,60,65,66,91,150]) and
                 not(LDataSet.DataSet.FieldByName('AvgFlow').IsNull) then
              begin
                LNonSuspectDailyFlowData := ADiversionGauge.AddNonSuspectDailyFlowData;
                LNonSuspectDailyFlowData.Initialise;
                LNonSuspectDailyFlowData.Poulate(LDataSet.DataSet.FieldByName('StationID').AsInteger,
                                        LDataSet.DataSet.FieldByName('Identifier').AsInteger,
                                        LDataSet.DataSet.FieldByName('DiversionDate').AsDateTime,
                                        LAvgFlow,
                                        ADiversionGauge.CatchmentScaleFactor,
                                        LDataSet.DataSet.FieldByName('QualityCode').AsInteger)

              end;

              LDataset.DataSet.Next;
            end;

            LDataset.DataSet.Close;

            LDailyIFRData := TDailyDiversionDataObject(FAppModules.Model.ModelData).DailyIFRDataList.AddDailyIFRData(ADiversionGauge.StationNo);
            LModelCode := FAppModules.ViewIni.ReadString('TFilesActionDailyDiversionManager','Model','');
            LStudyAreaCode := FAppModules.ViewIni.ReadString('TFilesActionDailyDiversionManager','StudyAreaName_'+FAppModules.StudyArea.StudyAreaCode,'');
            LSubAreaCode := FAppModules.ViewIni.ReadString('TFilesActionDailyDiversionManager','SubArea_'+FAppModules.StudyArea.SubAreaCode,'');
            LScenarioCode := FAppModules.ViewIni.ReadString('TFilesActionDailyDiversionManager','Scenario_'+FAppModules.StudyArea.ScenarioCode,'');
            LStationNo := FAppModules.ViewIni.ReadString('TFilesActionDailyDiversionManager','StationNo_'+ADiversionGauge.StationNo,'');
            LFileName := FAppModules.ViewIni.ReadString('TFilesActionDailyDiversionManager','File14_'+ADiversionGauge.StationNo,'');

            if (UpperCase(Trim(ADiversionGauge.StationNo))=UpperCase(Trim(LStationNo))) and
              (LScenarioCode=FAppModules.StudyArea.ScenarioCode) and
              (LSubAreaCode=FAppModules.StudyArea.SubAreaCode) and
              (LStudyAreaCode=FAppModules.StudyArea.StudyAreaCode) and
              (LModelCode=FAppModules.StudyArea.ModelCode) and
              FileExists(LFileName) then
            begin

              TDailyDiversionDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastConfigFileNames.FileNameObject[13].FileName := LFileName;
              LFileNameObject := TFileNameObject.Create(FAppModules);
              LFileNameObject.SetFileName(ExtractFilePath(LFileName),Trim(LFileName),False,fgConfiguration,fgConfiguration,Now,Now);

              LFile14Agent.ReadModelDataFromFile(LFileNameObject,LFileDataObject,nil);

              LoadFile14IFRData(LDailyIFRData,LFileDataObject);
            end;

          end;
        end;

      finally
        LDataset.Free;
        FreeAndNil(LFileDataObject);
        FreeandNil(LFile14Agent);

      end;
    end;
    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TDailyDiversionGaugeDataLoadAgent.LoadDailyFlowDataStartEndDate(ADiversionGauge : TDiversionGauge) : boolean;
const OPNAME = 'TDailyDiversionGaugeDataLoadAgent.LoadDailyFlowDataStartEndDate';
var
  LDataSet     : TAbstractModelDataset;
begin
  Result := False;
  try
    if ADiversionGauge <> nil then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        if Assigned(LDataSet) then
        begin
          LDataSet.SetSQL(FSQLAgent.GetStartEndDateSQL(ADiversionGauge.StationID));
          LDataSet.DataSet.open;
          if not LDataSet.DataSet.Eof then
          begin
            ADiversionGauge.StartDate := LDataSet.DataSet.FieldByName('StartDate').AsDateTime;
            ADiversionGauge.EndDate := LDataSet.DataSet.FieldByName('EndDate').AsDateTime;
          end;
        end;
      finally
        FreeAndNil(LDataSet);
      end;    
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeDataLoadAgent.LoadDailyInstreamFlowData(ADiversionGauge : TDiversionGauge) : boolean;
const OPNAME = 'TDailyDiversionGaugeDataLoadAgent.LoadDailyInstreamFlowData';
var
  LDataSet     : TAbstractModelDataset;
  LDailyInstreamFlowData : TDailyInstreamFlowData;
  LDay,LYear,
  LMonth : word;
  LAvgFlow : double;
begin
  Result := False;
  try
    if ADiversionGauge <> nil then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        if Assigned(LDataSet) then
        begin
          LDataSet.SetSQL(FSQLAgent.GetDailyInstreamFileDataSQL(ADiversionGauge.StationID));
          LDataset.DataSet.Open;
          Result := True;
          ADiversionGauge.ClearDailyInstreamFlowFileData;
          while (not LDataset.DataSet.EOF) do
          begin
            LDailyInstreamFlowData := ADiversionGauge.AddInstreamFlowData(LDataSet.DataSet.FieldByName('InstreamDate').AsDateTime);
            LDailyInstreamFlowData.Initialise;
            if not LDataSet.DataSet.FieldByName('AvgFlow').IsNull then
              LAvgFlow := LDataSet.DataSet.FieldByName('AvgFlow').AsFloat
            else
              LAvgFlow := NullFloat;
            DecodeDate(LDataSet.DataSet.FieldByName('InstreamDate').AsDateTime, LYear,LMonth,LDay);
            if LDailyInstreamFlowData.Poulate(LDataSet.DataSet.FieldByName('StationID').AsInteger,
                                      LDataSet.DataSet.FieldByName('Identifier').AsInteger,
                                      LAvgFlow,
                                      ADiversionGauge.InstreamScaleFactor,ADiversionGauge.CompensationValueByIndex[LMonth],
                                      ADiversionGauge.CapacityOfDiversion,LDataSet.DataSet.FieldByName('QualityCode').AsInteger) then
              LoadMonthlyInstreamFlowData(ADiversionGauge,LDailyInstreamFlowData);
            LDataset.DataSet.Next;
          end;
          LDataset.DataSet.Close;

        end;

      finally
        LDataset.Free;
      end;
    end;
    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TDailyDiversionGaugeDataLoadAgent.FlowDiversionRelationLoaded(ADiversionGauge : TDiversionGauge) : boolean;
const OPNAME = 'TDailyDiversionGaugeDataLoadAgent.FlowDiversionRelationLoaded';
var
  LDataSet : TAbstractModelDataset;
  LWRYMData : TWRYMChannelData;
begin
  Result := False;
  try
    if ADiversionGauge <> nil then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        if Assigned(LDataSet) then
        begin
          LDataSet.SetSQL(FSQLAgent.GetDailyDiversionWRYMDataSQL(ADiversionGauge.StationID));
          LDataset.DataSet.Open;
          while (not LDataset.DataSet.EOF) do
          begin
            LWRYMData := ADiversionGauge.CreateWRYMData;
            if LWRYMData <> nil then
              LWRYMData.PopulateAll(LDataSet.DataSet.FieldByName('Identifier').AsInteger,
                                    LDataSet.DataSet.FieldByName('StationID').AsInteger,
                                    LDataSet.DataSet.FieldByName('ReferenceFlow').AsFloat,
                                    LDataSet.DataSet.FieldByName('DiversionFlow').AsFloat,
                                    Trim(LDataSet.DataSet.FieldByName('DivFlowValueedited').AsString),
                                    Trim(LDataSet.DataSet.FieldByName('RefFlowValueEdited').AsString),
                                    LDataSet.DataSet.FieldByName('NonDiversionFlow').AsFloat,
                                    Trim(LDataSet.DataSet.FieldByName('NonDivFlowValueedited').AsString));
            LDataset.DataSet.Next;
          end;
          LDataset.DataSet.Close;

        end;

      finally
        LDataset.Free;
      end;
    end;
    Result := (ADiversionGauge.WRYMDataCount > 0);
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;


function TDailyDiversionGaugeDataLoadAgent.LoadCompensationValues(ADiversionGauge : TDiversionGauge) : boolean;
const OPNAME = 'TDailyDiversionGaugeDataLoadAgent.LoadCompensationValues';
var
  LDataSet     : TAbstractModelDataset;
  LCompensationValues : TMonthlyDoubleArray;
  LIndex : integer;
begin
  Result := False;
  try
    if ADiversionGauge <> nil then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        if Assigned(LDataSet) then
        begin
          LDataSet.SetSQL(FSQLAgent.GetCompensationValuesSQL(ADiversionGauge.StationID));
          LDataset.DataSet.Open;
          while (not LDataset.DataSet.EOF) do
          begin
            for LIndex := MinMonths to MaxMonths do
            begin
              if not (LDataset.DataSet.FieldByName(Format('Value%2.2d',[LIndex])).IsNull) then
                LCompensationValues[LIndex] := LDataset.DataSet.FieldByName(Format('Value%2.2d',[LIndex])).AsFloat
              else
                LCompensationValues[LIndex] := 0.0;
            end;
            ADiversionGauge.PopulateCompensationValues(LDataSet.DataSet.FieldByName('CapacityOfDiversion').AsFloat,
                                                       LDataSet.DataSet.FieldByName('ScaleFactor').AsFloat,
                                                       LDataSet.DataSet.FieldByName('StartDate').AsDateTime,
                                                       LDataSet.DataSet.FieldByName('EndDate').AsDateTime,
                                                       LCompensationValues);
            LDataset.DataSet.Next;
          end;
          LDataset.DataSet.Close;
          Result := True;
        end;

      finally
        LDataset.Free;
      end;
    end;
    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TDailyDiversionGaugeDataLoadAgent.LoadThresholdValues(ADiversionGauge : TDiversionGauge): boolean;
const OPNAME = 'TDailyDiversionGaugeDataLoadAgent.LoadThresholdValues';
var
  LDataSet     : TAbstractModelDataset;
  LThresholdValues : TMonthlyDoubleArray;
  LIndex : integer;
begin
  Result := False;
  try
    if ADiversionGauge <> nil then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        if Assigned(LDataSet) then
        begin
          LDataSet.SetSQL(FSQLAgent.GetThresholdValuesSQL(ADiversionGauge.StationID));
          LDataset.DataSet.Open;
          while (not LDataset.DataSet.EOF) do
          begin
            for LIndex := MinMonths to MaxMonths do
            begin
              if not (LDataset.DataSet.FieldByName(Format('Value%2.2d',[LIndex])).IsNull) then
                LThresholdValues[LIndex] := LDataset.DataSet.FieldByName(Format('Value%2.2d',[LIndex])).AsFloat
              else
                LThresholdValues[LIndex] := 0.0;
            end;
            ADiversionGauge.PopulateThresholdValues(LThresholdValues);
            LDataset.DataSet.Next;
          end;
          LDataset.DataSet.Close;
          Result := True;
        end;
      finally
        LDataset.Free;
      end;
    end;
    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TDailyDiversionGaugeDataLoadAgent.LoadFlowDiversionRelation(ADiversionGauge : TDiversionGauge; AStartDate, AEndDate : TDateTime) : boolean;
const OPNAME = 'TDailyDiversionGaugeDataLoadAgent.LoadFlowDiversionRelation';
var
  LDataSet     : TAbstractModelDataset;
  LRankedFlowDiversionRelationship : TFlowDiversionRelationship;
  LUnRankedFlowDiversionRelationship : TFlowDiversionRelationship;
  LWRYMData : TWRYMChannelData;
  LIdentifier,
  LFactor : integer;
  LReferenceFlow, LDiversionFlow,LNonDiversionFlow : double;
begin
  Result := False;
  try
    if ADiversionGauge <> nil then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        if Assigned(LDataSet) then
        begin
          LDataset.DataSet.Close;
          LDataSet.SetSQL(FSQLAgent.GetUnRankedFlowDiversionRelationSQL(ADiversionGauge.StationID));
          LDataset.DataSet.Open;
          ADiversionGauge.ClearFlowDiversionRelationship;
          while (not LDataset.DataSet.EOF) do
          begin
            LUnRankedFlowDiversionRelationship := ADiversionGauge.AddFlowDiversionRelationship;
            if not LDataSet.DataSet.FieldByName('ReferenceFlow').IsNull then
              LReferenceFlow := LDataSet.DataSet.FieldByName('ReferenceFlow').AsFloat
            else
              LReferenceFlow := NullFloat;
            if not LDataSet.DataSet.FieldByName('DiversionFlow').IsNull then
              LDiversionFlow := LDataSet.DataSet.FieldByName('DiversionFlow').AsFloat
            else
              LDiversionFlow := NullFloat;

            if not LDataSet.DataSet.FieldByName('NonDiversionFlow').IsNull then
              LNonDiversionFlow := LDataSet.DataSet.FieldByName('NonDiversionFlow').AsFloat
            else
              LNonDiversionFlow := NullFloat;


            LUnRankedFlowDiversionRelationship.Populate(LDataSet.DataSet.FieldByName('Identifier').AsInteger,
                                                        ADiversionGauge.StationID,
                                                        LDataSet.DataSet.FieldByName('RelationshipDate').AsDateTime,
                                                        LReferenceFlow,
                                                        LDiversionFlow,LNonDiversionFlow);
            LDataset.DataSet.Next;
          end;
          LDataset.DataSet.Close;
          LDataSet.ClearSQL;
          LDataSet.SetSQL(FSQLAgent.GetRankedFlowDiversionRelationshipSQL(ADiversionGauge.StationID));
          LDataset.DataSet.Open;
          while (not LDataset.DataSet.EOF) do
          begin
            LRankedFlowDiversionRelationship := ADiversionGauge.AddRankedFlowDiversionRelationship;

            LRankedFlowDiversionRelationship.Populate(LDataSet.DataSet.FieldByName('Identifier').AsInteger,
                                                      ADiversionGauge.StationID,
                                                      LDataSet.DataSet.FieldByName('RelationshipDate').AsDateTime,
                                                      LDataSet.DataSet.FieldByName('ReferenceFlow').AsFloat,
                                                      LDataSet.DataSet.FieldByName('DiversionFlow').AsFloat,
                                                      LDataSet.DataSet.FieldByName('NonDiversionFlow').AsFloat);
            LDataset.DataSet.Next;
          end;
          LDataset.DataSet.Close;
          Result := (ADiversionGauge.RankedFlowDiversionRelationshipCount > 0);
          if not (FlowDiversionRelationLoaded(ADiversionGauge)) and (Result) then
          begin
            LWRYMData := ADiversionGauge.AddWRYMData(LIdentifier);
            LWRYMData.Populate(LIdentifier,ADiversionGauge.StationID);
            LWRYMData.ReferenceFlow := 0.000;
            LWRYMData.RefFlowValueEdited := False;
            LWRYMData.DiversionFlow := 0.000;
            LWRYMData.DivFlowValueEdited := False;
            LWRYMData.NonDiversionFlow := 0.000;
            LWRYMData.NonDivFlowValueEdited := False;

            LFactor := Round(ADiversionGauge.RankedFlowDiversionRelationshipCount/10);
            LoadRelationship(LFactor,ADiversionGauge);
          end;
        end;
      finally
        LDataset.Free;
      end;
      Result := (ADiversionGauge.WRYMDataCount > 0);
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TDailyDiversionGaugeDataLoadAgent.CanGenerateRelationship(ADiversionGauge : TDiversionGauge): boolean;
const OPNAME = 'TDailyDiversionGaugeDataLoadAgent.CanGenerateRelationship';
var
  LDataSet : TAbstractModelDataset;
  LDivStartDate : TDateTime;
  LDivEndDate : TDateTime;
  LRefStartDate : TDateTime;
  LRefEndDate : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try


      LDataSet.ClearSQL;
      LDataSet.SetSQL(FSQLAgent.GetDiversionPeriod(ADiversionGauge.StationID));
      LDataSet.DataSet.Open;

      LDivStartDate := LDataSet.DataSet.FieldByName('DivStartDate').AsDateTime;
      LDivEndDate := LDataSet.DataSet.FieldByName('DivEndDate').AsDateTime;
      LDataSet.DataSet.Close;

      if ADiversionGauge.ExcludeSuspectDailyData then
      begin
        LDataSet.ClearSQL;
        LDataSet.SetSQL(FSQLAgent.GetExclSusReferencePeriod(ADiversionGauge.StationID));
        LDataSet.DataSet.Open;
        LRefStartDate := LDataSet.DataSet.FieldByName('RefStartDate').AsDateTime;
        LRefEndDate :=  LDataSet.DataSet.FieldByName('RefEndDate').AsDateTime;
      end
      else
      begin
        LDataSet.ClearSQL;
        LDataSet.SetSQL(FSQLAgent.GetReferencePeriod(ADiversionGauge.StationID));
        LDataSet.DataSet.Open;
        LRefStartDate := LDataSet.DataSet.FieldByName('RefStartDate').AsDateTime;
        LRefEndDate := LDataSet.DataSet.FieldByName('RefEndDate').AsDateTime;
      end;


      Result := (LDivStartDate = LRefStartDate) and (LDivEndDate = LRefEndDate);
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TDailyDiversionGaugeDataLoadAgent.GenerateFlowDiversionRelation(ADiversionGauge : TDiversionGauge; AStartDate, AEndDate : TDateTime) : boolean;
const OPNAME = 'TDailyDiversionGaugeDataLoadAgent.GenerateFlowDiversionRelation';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
  LMonthlyInstreamFlowData : TMonthlyInstreamFlowData;
  LMonthlyFlowData : TMonthlyFlowData;
  LFlowDiversionRelationship : TFlowDiversionRelationship;
  LIndex : integer;
  LRelationshipDate : TDateTime;
  LRecCount,
  LHydroYear,
  LCount : integer;
  LNonDiversionFlow : double;
begin
  Result := False;
  try
    if ADiversionGauge <> nil then
    begin
      if not CanGenerateRelationship(ADiversionGauge) then
      begin
        ShowMessage('Error: Diversion Flow Start and End Period must be the same as Reference Flow Start and End Period.');
        Exit;
      end;
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        if Assigned(LDataSet) then
        begin
          LDataset.DataSet.Close;
          if (ADiversionGauge.MonthlyDailyFlowCount > 0 ) and (ADiversionGauge.MonthlyInstreamFlowCount > 0)then
          begin
            LRecCount := 0;
            FSQLAgent.DeleteFlowDiversionRelation(ADiversionGauge.StationID);
            for LIndex := 0 to ADiversionGauge.MonthlyDailyFlowCount - 1 do
            begin
              LCount := 0;
              LMonthlyInstreamFlowData := nil;
              LMonthlyFlowData := ADiversionGauge.MonthlyFlowDataByIndex[LIndex];
              while (LMonthlyInstreamFlowData = nil) and (LCount < ADiversionGauge.MonthlyInstreamFlowCount) do
              begin
                if ADiversionGauge.MonthlyInstreamFlowDataByIndex[LCount].Year = LMonthlyFlowData.Year then
                  LMonthlyInstreamFlowData := ADiversionGauge.MonthlyInstreamFlowDataByIndex[LCount]
                else
                  LCount := LCount + 1;
              end;
              if (LMonthlyFlowData <> nil) and (LMonthlyInstreamFlowData <> nil) then
              begin
                for LCount := MinMonths to MaxMonths do
                begin
                  inc(LRecCount);
                  LNonDiversionFlow := 0.000;
                  LDataset.SetSQL(FSQLAgent.InsertDailyDiversionFlowUnrankedRelationshipSQL(ADiversionGauge.StationID));
                  LDataset.ClearQueryParams();
                  LDataSet.SetParams(
                  ['Model','StudyAreaName','SubArea','Scenario','Identifier','StationID'],
                  [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
                  FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode,
                  IntToStr(LRecCount), IntToStr(ADiversionGauge.StationID)]);
                  if CHydroMonths[LCount] > 9 then
                    LHydroYear := LMonthlyFlowData.Year
                  else
                    LHydroYear := LMonthlyFlowData.Year + 1;
                  LRelationshipDate := EncodeDate(LHydroYear,CHydroMonths[LCount],1);
                  LDataSet.SetParams(['RelationshipDate'],[FormatDateTime('yyyy/mm', LRelationshipDate)]);

                  if (LMonthlyFlowData.AvgFlowByIndex[LCount] <> NullFloat) and (LMonthlyFlowData.AvgFlowByIndex[LCount] >=0)  then
                    LDataSet.SetParams(['ReferenceFlow'],[ FormatFloat('####0.000',LMonthlyFlowData.AvgFlowByIndex[LCount])])
                  else
                    LDataSet.SetParams(['ReferenceFlow'],[ FormatFloat('####0.000',LMonthlyFlowData.AvgFlowByIndex[LCount])]);


                  if (LMonthlyInstreamFlowData.DailyDiversionFlowByIndex[LCount] <> NullFloat) and (LMonthlyInstreamFlowData.DailyDiversionFlowByIndex[LCount] >=0)  then
                    LDataSet.SetParams(['DiversionFlow'],[ FormatFloat('####0.000',LMonthlyInstreamFlowData.DailyDiversionFlowByIndex[LCount])])
                  else
                    LDataSet.SetParams(['DiversionFlow'],[ FormatFloat('####0.000',LMonthlyInstreamFlowData.DailyDiversionFlowByIndex[LCount])]);


                  if (LMonthlyInstreamFlowData.DailyDiversionFlowByIndex[LCount] <> NullFloat) and
                     (LMonthlyInstreamFlowData.DailyDiversionFlowByIndex[LCount] >=0) and
                     (LMonthlyFlowData.AvgFlowByIndex[LCount] <> NullFloat) and
                     (LMonthlyFlowData.AvgFlowByIndex[LCount] >=0) and
                     (LMonthlyFlowData.AvgFlowByIndex[LCount] >=
                      LMonthlyInstreamFlowData.DailyDiversionFlowByIndex[LCount])
                      then
                  begin

                      LDataSet.SetParams(['NonDiversionFlow'],[ FormatFloat('####0.000',
                      LMonthlyFlowData.AvgFlowByIndex[LCount]-LMonthlyInstreamFlowData.DailyDiversionFlowByIndex[LCount])]);
                      LNonDiversionFlow := LMonthlyFlowData.AvgFlowByIndex[LCount]-LMonthlyInstreamFlowData.DailyDiversionFlowByIndex[LCount];
                  end
                  else
                    LDataSet.SetParams(['NonDiversionFlow'],[ FormatFloat('####0.000',0.000)]);
                    
                  LDataSet.ExecSQL;
                  LFlowDiversionRelationship :=  ADiversionGauge.AddFlowDiversionRelationship;
                  LFlowDiversionRelationship.Populate(LIndex+1,ADiversionGauge.StationID,LRelationshipDate,LMonthlyFlowData.AvgFlowByIndex[LCount],
                     LMonthlyInstreamFlowData.DailyDiversionFlowByIndex[LCount],LNonDiversionFlow);
                end;
              end;
            end;
          end;
          FAppModules.StudyArea.LastUpdateDate := Now();
          LImportDate := FAppModules.StudyArea.GetStudyImportDate;
          if LImportDate = NullDateTime then
            FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);
        end;
      finally
        LDataset.Free;
      end;
      Result := LoadFlowDiversionRelation(ADiversionGauge,AStartDate, AEndDate);
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TDailyDiversionGaugeDataLoadAgent.GenerateWRYMData(ADiversionGauge : TDiversionGauge) : boolean;
const OPNAME = 'TDailyDiversionGaugeDataLoadAgent.GenerateWRYMData';
var
  LDataSet      : TAbstractModelDataset;
  LIdentifier,
  LIndex        : integer;
  LChanelIdentifier,
  LChanelNames : TStringlist;
  LDiversionChannelDialog : TDiversionChannelDialog;
begin
  Result := False;
  try
    if ADiversionGauge <> nil then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      LChanelNames := TStringlist.Create;
      LChanelIdentifier := TStringList.Create;
      try
        if FSQLAgent.WRYMDataIsLoaded(LChanelNames) then
        begin
          LDiversionChannelDialog := TDiversionChannelDialog.CreateWithoutDFM(nil,FAppModules);
          try
            LDiversionChannelDialog.Initialise;
            LDiversionChannelDialog.LanguageHasChanged;
            LDiversionChannelDialog.ShowModal;
            LChanelIdentifier.CommaText := LDiversionChannelDialog.IdentifierForSelectedChannelsCommaText;

            if (LDiversionChannelDialog.ModalResult = mrOK) then
            begin
              if (LDiversionChannelDialog.SelectedChannelsCommaText <> '') then
              begin
                for LIndex := 0 to LChanelIdentifier.Count - 1 do
                begin
                  LIdentifier := StrToInt(LChanelIdentifier[LIndex]);
                  Result := FSQLAgent.InsertWRYMData(ADiversionGauge,LIdentifier);
                end
              end
            end;
          finally
            FreeAndNil(LDiversionChannelDialog);
          end;
        end
        else
          MessageDlg(FAppModules.Language.GetString('Message.DiversionFeatureMustExistInYieldModel'),
            mtInformation, [mbOK],0);
      finally
        FreeAndNil(LDataSet);
        FreeAndNil(LChanelNames);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeDataLoadAgent.ClearFlowDiversionRelation(ADiversionGauge : TDiversionGauge) : boolean;
const OPNAME = 'TDailyDiversionGaugeDataLoadAgent.ClearFlowDiversionRelation';
begin
  Result := False;
  try
    if ADiversionGauge <> nil then
      Result := FSQLAgent.DeleteFlowDiversionRelation(ADiversionGauge.StationID);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeDataLoadAgent.ClearDailyInstreamFlowFileData(ADiversionGauge : TDiversionGauge) : boolean;
const OPNAME = 'TDailyDiversionGaugeDataLoadAgent.ClearDailyInstreamFlowFileData';
begin
  Result := False;
  try
    if ADiversionGauge <> nil then
      Result := FSQLAgent.DeleteDailyInstreamData(ADiversionGauge.StationID);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeDataLoadAgent.ClearDailyFlowDataFromCSVFile(ADiversionGauge : TDiversionGauge) : boolean;
const OPNAME = 'TDailyDiversionGaugeDataLoadAgent.ClearDailyFlowDataFromCSVFile';
begin
  Result := False;
  try
    if ADiversionGauge <> nil then
      Result := FSQLAgent.DeleteDailyFlowDataFromCSVFile(ADiversionGauge.StationID);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeDataLoadAgent.LoadFile14IFRData(ADailyIFRData: TDailyIFRData; ADataObject: TDataFileObjects): boolean;
const OPNAME = 'TDailyDiversionGaugeDataLoadAgent.LoadFile14IFRData';
var
  LControlStructureDetails : TReleaseControlStructureDetails;
  LReleaseControlStructureObject: TReleaseControlStructureObject;
  LChannel : TGeneralFlowChannel;
  LIndex : integer;
  LInflowVariableLine :TInflowVariableLine;
  LMonth,
  LCount : integer;
  LReferenceNodeNumbers : TStringList;
  LInflows : TIFRArray;
  LReleases : TIFRArray;
  LIFRFeature : TIFRFeature;
  LAnnualInflowProperty,
  LIFRVariables,
  LExceedenceProperty,
  LIFRReleaseVariables : TAbstractFieldProperty;
  LExceedenceArray : TExceedencePercentagesArray;
  LAnnualInflow : TExceedencePercentagesArray;
  procedure LoadInflowIFRData;
  const OPNAME = 'LoadInflowIFRData';
  var
    LMonth,
    LCount : integer;
  begin
    for LCount := 1 to LControlStructureDetails.FInflowVariableLine.Count do
    begin
      LInflowVariableLine := TInflowVariableLine(LControlStructureDetails.FInflowVariableLine[LCount-1]);
      for LMonth := MinReleaseStructure to MaxReleaseStructure do
      begin
        if LInflowVariableLine.FInflowVariable[LMonth].FInitalised  then
          LInflows[LCount,LMonth] := LInflowVariableLine.FInflowVariable[LMonth].FData;
        if LInflowVariableLine.FReleaseVariable[LMonth].FInitalised  then
          LReleases[LCount,LMonth] := LInflowVariableLine.FReleaseVariable[LMonth].FData;
      end;
    end;
  end;
begin
  Result := False;
  try
    LReferenceNodeNumbers := TStringList.Create;
    try
      LIFRVariables := FAppModules.FieldProperties.FieldProperty('DiversionIFRVariables');
      if not Assigned(LIFRVariables) then
        raise Exception.Create('Field (DiversionIFRVariables) not found in field properties');
      SetLength(lInflows,LIFRVariables.ArrayLength, LIFRVariables.ArrayLength(1));
      LIFRReleaseVariables := FAppModules.FieldProperties.FieldProperty('DiversionIFRReleaseVariables');
      if not Assigned(LIFRReleaseVariables) then
        raise Exception.Create('Field (DiversionIFRReleaseVariables) not found in field properties');
      SetLength(lReleases,LIFRReleaseVariables.ArrayLength, LIFRReleaseVariables.ArrayLength(1));
      LExceedenceProperty := FAppModules.FieldProperties.FieldProperty('DiversionExceedencePercentage');
      if not Assigned(LExceedenceProperty) then
        raise Exception.Create('Field (DiversionExceedencePercentage) not found in field properties');
      SetLength(LExceedenceArray,LExceedenceProperty.ArrayLength);

     LAnnualInflowProperty := FAppModules.FieldProperties.FieldProperty('DiversionAnnualInflow');
    if (LAnnualInflowProperty = nil) then
      raise Exception.Create('Field (AnnualInflow) not found in field properties');
    SetLength(LAnnualInflow,LAnnualInflowProperty.ArrayLength);

      if ADailyIFRData <> nil then
      begin
        LReleaseControlStructureObject := ADataObject.FReleaseControlStructureObject;
        if not Assigned(LReleaseControlStructureObject) then
          Exit;
        if  LReleaseControlStructureObject.FReleaseControlStructureDetails.Count > 0 then
        begin
          ADailyIFRData.ChannelList.Initialise;

          for LIndex := 0 to LReleaseControlStructureObject.FReleaseControlStructureDetails.Count-1 do
          begin
            LControlStructureDetails := TReleaseControlStructureDetails(LReleaseControlStructureObject.FReleaseControlStructureDetails[LIndex]);
            if LControlStructureDetails <> nil then
            begin
              if LControlStructureDetails.FControlStructureData.FIFRChannelNumber.FInitalised then
              begin
                LChannel := ADailyIFRData.ChannelList.NewGeneralFlowChannel;
                LChannel.PopulateGeneralFlowChannel(LIndex,
                                                    LControlStructureDetails.FControlStructureData.FIFRChannelNumber.FData,
                                                    7,0,IntToStr(LControlStructureDetails.FControlStructureData.FIFRChannelNumber.FData),
                                                    0,0,0,'',0,'',LChannel.RequiresFirmYieldAnalysis);
                for LCount := Low(LExceedenceArray) to High(LExceedenceArray) do
                  LExceedenceArray[LCount] := NullFloat;

                if LControlStructureDetails.FControlStructureData.FRefNodeNumber.Count > 0 then
                  LReferenceNodeNumbers.CommaText := LControlStructureDetails.FControlStructureData.FRefNodeNumber.CommaText;
                //if LInflowVariableLine <> nil then
                //begin
                  for LCount := LIFRVariables.ArrayLow to LIFRVariables.ArrayHigh do
                  begin
                    for LMonth := 1 to 12 do
                    begin
                      LInflows[LCount, LMonth]  := NullFloat;
                      LReleases[LCount, LMonth] := NullFloat;
                    end;
                      if LCount <= 10 then
                        LAnnualInflow[LCount-1] := NullFloat;

                  end;
                  LoadInflowIFRData;
                //end;
                LIFRFeature := ADailyIFRData.IFRFeatureList.NewMonthlyIFRFeature;
                LIFRFeature.Initialise;
                LIFRFeature.Populate(+1, LChannel.ChannelName,LChannel.ChannelNumber,7,0,
                                     0,
                                     LControlStructureDetails.FControlStructureData.FLagInMonthsCount.FData,
                                     0,0,LExceedenceArray,
                                     LReferenceNodeNumbers,
                                     LInflows, LReleases,LAnnualInflow,0,0,'');
                LChannel.IFRFeature := LIFRFeature;
              end;
            end;
          end;
        end;
      end;
    finally
      FreeAndNil(LReferenceNodeNumbers);
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeDataLoadAgent.GenerateDailyIFR(ADiversionGauge: TDiversionGauge;ADailyIFRData: TDailyIFRData): boolean;
const OPNAME = 'TDailyDiversionGaugeDataLoadAgent.GenerateDailyIFR';
var
  LIndex : integer;
  LDailyInstreamFlowData : TDailyInstreamFlowData;
begin
  Result := False;
  try
    if (ADiversionGauge <> nil) and (ADailyIFRData <> nil) then
    begin
      if ADailyIFRData.DailyIFRList.Count > 0 then
      begin
        if FSQLAgent.DeleteDailyInstreamData(ADiversionGauge.StationID) then
          ADiversionGauge.ClearDailyInstreamFlowFileData;
        for LIndex := 0 to ADailyIFRData.DailyIFRList.Count-1 do
        begin
          LDailyInstreamFlowData := TDailyInstreamFlowData(ADailyIFRData.DailyIFRList.Objects[LIndex]);
          if LDailyInstreamFlowData <> nil then
          begin
            FSQLAgent.InsertDailyInstreamData(ADiversionGauge.StationID, LIndex+1,
                                              LDailyInstreamFlowData.InstreamDate,LDailyInstreamFlowData.AvgFlow,999);
          end;
        end;
        LoadDailyInstreamFlowData(ADiversionGauge);
      end;
    end;
    Result := True;;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionGaugeDataLoadAgent.LoadRelationship(AFactor: integer;ADiversionGauge :TDiversionGauge): boolean;
const OPNAME = 'TDailyDiversionGaugeDataLoadAgent.LoadRelationship';
var
  LReferenceTotalSum : double;
  LDiversionTotalSum : double;
  LRecCount : integer;
  LWRYMData : TWRYMChannelData;
  LIdentifier : integer;
  LRankedFlowDiversionRelationship : TFlowDiversionRelationship;
  LReferenceAvg,
  LDiversionAvg : double;
  LPointsCount,
  LIndex : integer;
begin
  Result := False;
  try
    if ADiversionGauge <> nil then
    begin
      LRecCount := 0;
      LReferenceTotalSum := 0;
      LDiversionTotalSum := 0;
      LPointsCount := 0;
      LWRYMData := nil;
      for LIndex := 0  to ADiversionGauge.RankedFlowDiversionRelationshipCount - 1 do
      begin
        LRecCount := LRecCount + 1;
        LRankedFlowDiversionRelationship := ADiversionGauge.RankedFlowDiversionRelationshipByIndex[LIndex];
        LReferenceTotalSum :=  LReferenceTotalSum + LRankedFlowDiversionRelationship.ReferenceFlow;
        LDiversionTotalSum :=  LDiversionTotalSum + LRankedFlowDiversionRelationship.DiversionFlow;
        if (LRecCount = AFactor) or
          ((LPointsCount < 10) and (LIndex =ADiversionGauge.RankedFlowDiversionRelationshipCount - 1)) then
        begin
          if LRecCount > 0 then
          begin
            LReferenceAvg :=  LReferenceTotalSum/LRecCount;
            LDiversionAvg :=  LDiversionTotalSum/LRecCount; 
            LWRYMData := ADiversionGauge.AddWRYMData(LIdentifier);
            LWRYMData.Populate(LIdentifier,ADiversionGauge.StationID);
            LWRYMData.ReferenceFlow := StrToFloat(FormatFloat('####0.000',LReferenceAvg));
            LWRYMData.RefFlowValueEdited := False;
            LWRYMData.DiversionFlow := StrToFloat(FormatFloat('####0.000',LDiversionAvg));
            LWRYMData.DivFlowValueEdited := False;
            if LReferenceAvg>=LDiversionAvg then
              LWRYMData.NonDiversionFlow := StrToFloat(FormatFloat('####0.000',LReferenceAvg-LDiversionAvg))
            else
              LWRYMData.NonDiversionFlow := StrToFloat(FormatFloat('####0.000',0));
            LWRYMData.NonDivFlowValueEdited := False;

            LReferenceTotalSum := 0;
            LDiversionTotalSum := 0;
            LRecCount := 0;
            inc(LPointsCount);
          end;
        end;
      end;
      if (LWRYMData <> nil) then
      begin
        LReferenceAvg := LWRYMData.ReferenceFlow*10;
        LWRYMData := ADiversionGauge.AddWRYMData(LIdentifier);
        LWRYMData.Populate(LIdentifier,ADiversionGauge.StationID);
        LWRYMData.ReferenceFlow := StrToFloat(FormatFloat('####0.000',LReferenceAvg));
        LWRYMData.RefFlowValueEdited := False;
        LWRYMData.DiversionFlow := StrToFloat(FormatFloat('####0.000',ADiversionGauge.CapacityOfDiversion));
        LWRYMData.DivFlowValueEdited := False;
        if (LReferenceAvg>=ADiversionGauge.CapacityOfDiversion) then
          LWRYMData.NonDiversionFlow := StrToFloat(FormatFloat('####0.000',LReferenceAvg-ADiversionGauge.CapacityOfDiversion))
        else
          LWRYMData.NonDiversionFlow := StrToFloat(FormatFloat('####0.000',0));
        LWRYMData.NonDivFlowValueEdited := False;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
