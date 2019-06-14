//
//
//  UNIT      : Contains TFile13DatabaseAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 05/08/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UFile13DatabaseAgent;

interface

uses
  Classes, sysutils,Db,

  //  DWAF VCL
  UFileNames,
  UConstants,
  UDatabaseUtilities,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UPowerDemandObject,
  UAbstractDatabaseAgent,
  UYieldModelDataObject;

type

  TFile13DatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadF13UnkownDataSQL: string;
    function ReadMasterControlSQL: string;

    function WriteMasterControlSQL: string;
    function WriteF13UnkownDataSQL: string;

  public
    { Public declarations }
    function ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function ClearModelDataInDatabase(AFileName:TFileNameObject;AProgressFunction: TProgressUpdateFuntion;
             AQuetly: boolean = False): boolean; override;
  end;


implementation

uses
  System.Contnrs,
  UUtilities,
  UDataSetType,
  UChannelDescriptionObject,
  UErrorHandlingOperations;

function TFile13DatabaseAgent.ReadF13UnkownDataSQL: string;
const OPNAME = 'TFile13DatabaseAgent.ReadF13UnkownDataSQL';
begin

  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,LineNumber,LineData '+
              ' FROM WRYMFileLines'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND FileGroup     = :FileGroup' +
              ' AND FileType      = :FileType'+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,LineNumber,LineSection';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile13DatabaseAgent.ReadMasterControlSQL: string;
const OPNAME = 'TFile13DatabaseAgent.ReadMasterControlSQL';
begin
  Result := '';
  try
    Result :=
      'SELECT Model, StudyAreaName, SubArea, Scenario, Identifier, FeatureName,'+
      ' ChannelNumber, MasterControlType,DistributionPattern,StorageFraction,' +
      ' Value01, Value02, Value03, Value04, Value05, Value06,' +
      ' Value07, Value08, Value09, Value10, Value11, Value12' +
      ' FROM MasterControlFeature' +
      ' WHERE (Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
      '      (StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      '      (SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      '      (Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') ' +
      ' ORDER BY Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile13DatabaseAgent.WriteMasterControlSQL: string;
const OPNAME = 'TFile13DatabaseAgent.WriteMasterControlSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO MasterControlFeature ' +
              '(Model, StudyAreaName, SubArea, Scenario, Identifier, FeatureName,'+
              ' ChannelNumber, MasterControlType,DistributionPattern,StorageFraction, ' +
              ' Value01, Value02, Value03, Value04, Value05, Value06, ' +
              ' Value07, Value08, Value09, Value10, Value11, Value12) ' +
              ' Values ' +
              ' (:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, :FeatureName,' +
              ' :ChannelNumber,:MasterControlType,:DistributionPattern,:StorageFraction, ' +
              ' :Value01, :Value02, :Value03, :Value04, :Value05, :Value06, ' +
              ' :Value07, :Value08, :Value09, :Value10, :Value11, :Value12)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile13DatabaseAgent.WriteF13UnkownDataSQL: string;
const OPNAME = 'TFile13DatabaseAgent.WriteF13UnkownDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRYMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile13DatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile13DatabaseAgent.ReadModelDataFromDatabase';
var
  LFieldName,
  LMessage: string;
  LDataSet : TAbstractModelDataset;
  LCount : Integer;
  LPowerDemandObject: TPowerDemandObject;
  LPowerDemand:TPowerDemand;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile13DatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LPowerDemandObject := ADataObject.FPowerDemandObject;

    if not LPowerDemandObject.Initialise then
    Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try

      LDataSet.SetSQL(ReadMasterControlSQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile13DatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
      end
      else
      begin
        while not LDataSet.DataSet.EOF do
        begin
          LPowerDemand := TPowerDemand.Create;
          LPowerDemandObject.FPowerDemandsLines.Add(LPowerDemand);

          if not LDataSet.DataSet.FieldByName('FeatureName').IsNull then
          begin
            LPowerDemand.FFeatureName.FData := Trim(LDataSet.DataSet.FieldByName('FeatureName').AsString);
            LPowerDemand.FFeatureName.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
          begin
            LPowerDemand.FChannelNumber.FData :=LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
            LPowerDemand.FChannelNumber.FInitalised := True;
          end;
          
          if not LDataSet.DataSet.FieldByName('DistributionPattern').IsNull then
          begin
            LPowerDemand.FDistributionPattern.FData :=LDataSet.DataSet.FieldByName('DistributionPattern').AsInteger;
            LPowerDemand.FDistributionPattern.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('StorageFraction').IsNull then
          begin
            LPowerDemand.FStorageFraction.FData :=LDataSet.DataSet.FieldByName('StorageFraction').AsFloat;
            LPowerDemand.FStorageFraction.FInitalised := True;
          end;

          for LCount := MinPowerControl to MaxPowerControl do
          begin
            LFieldName := Format('%s%2.2d',['Value',LCount]);
            if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
            begin
              LPowerDemand.FValues[LCount].FData := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
              LPowerDemand.FValues[LCount].FInitalised := True;
            end
            else
              Break;
          end;
          LDataSet.DataSet.Next;
        end;
      end;

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadF13UnkownDataSQL);
      LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
      LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
      LDataSet.DataSet.Open;

      while not LDataSet.DataSet.EOF do
      begin
        LPowerDemandObject.FF13ExtraLines.Add(TrimRight(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;

      LMessage := FAppModules.Language.GetString('TFile13DatabaseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile13DatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile13DatabaseAgent.WriteModelDataToDatabase';
var
  LFieldName         : string;
  LMessage           : string;
  LCount             : integer;
  LLinesCount        : integer;
  LCounter           : integer;
  LDataSet           : TAbstractModelDataset;
  LPowerDemandObject : TPowerDemandObject;
  LPowerDemand       : TPowerDemand;
  LStop              : boolean;
  lMasterControlObj  : TMasterChannelObject;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile13DatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not ClearModelDataInDatabase(AFileName,AProgressFunction,True) then
      Exit;

    LPowerDemandObject := ADataObject.FPowerDemandObject;
    if not Assigned(LPowerDemandObject) then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try

      for LLinesCount := 0 to LPowerDemandObject.FPowerDemandsLines.Count-1 do
      begin

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteMasterControlSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LLinesCount+1)]);

        LPowerDemand := TPowerDemand(LPowerDemandObject.FPowerDemandsLines[LLinesCount]);
        lMasterControlObj := ADataObject.FChannelDescrObject.FindMasterChannel(LPowerDemand.FChannelNumber.FData);

        if LPowerDemand.FFeatureName.FInitalised then
         LDataSet.SetParams(['FeatureName'], [LPowerDemand.FFeatureName.FData]);

        if LPowerDemand.FChannelNumber.FInitalised then
         LDataSet.SetParams(['ChannelNumber'], [IntToStr(LPowerDemand.FChannelNumber.FData)]);

        if (Assigned(lMasterControlObj)) then
          LDataSet.SetParams(['MasterControlType'], [lMasterControlObj.FChannelType.FData]);

        if LPowerDemand.FFeatureName.FInitalised then
         LDataSet.SetParams(['DistributionPattern'], [IntToStr(LPowerDemand.FDistributionPattern.FData)]);

        if LPowerDemand.FFeatureName.FInitalised then
         LDataSet.SetParams(['StorageFraction'], [FloatToStr(LPowerDemand.FStorageFraction.FData)]);

        for LCount := MinPowerControl to MaxPowerControl do
        begin
          if not LPowerDemand.FValues[LCount].FInitalised then
            Break;
          LFieldName := Format('%s%2.2d',['Value',LCount]);
          LDataSet.SetParams([LFieldName], [FloatToStr(LPowerDemand.FValues[LCount].FData)]);
        end;

        LDataSet.ExecSQL;
      end;

      for LCounter := 0 to LPowerDemandObject.FF13ExtraLines.Count - 1 do
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteF13UnkownDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
        LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
        LDataSet.SetParams(['LineNumber'], [IntToStr(LCounter+1)]);
        LDataSet.SetParams(['LineSection'], [IntToStr(0)]);
        LDataSet.SetParams(['LineData'], [LPowerDemandObject.FF13ExtraLines[LCounter]]);

        LDataSet.ExecSQL;
      end;
      LDataSet.DataSet.Close;

      Result := InsertFileName(AFileName);
      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TFile13DatabaseAgent.strWriteEnded');
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    finally
      LDataSet.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile13DatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TFile13DatabaseAgent.ClearModelDataInDatabase';
var
  LTableNames,
  LMessage: string;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    if not AQuetly then
    begin
      LMessage := FAppModules.Language.GetString('TAbstractDatabaseAgent.strClearModelDataInDatabaseStarted');
      AProgressFunction(LMessage,ptNone,LStop);
    end;

    if not Assigned(AFileName) then
      raise Exception.Create('File name object parameter is not yet assigned.');

    LTableNames := 'MasterControlFeature,WaterUseProportions';
    Result := DeleteModelData(LTableNames,'',AProgressFunction,AQuetly);
    Result := Result and DeleteUnknownModelData(AFileName,AProgressFunction,AQuetly);
    Result := Result and DeleteFileName(AFileName);

    if Result and (not AQuetly)then
    begin
      LMessage := FAppModules.Language.GetString('TAbstractDatabaseAgent.strClearModelDataInDatabaseEnded');
      AProgressFunction(LMessage,ptNone,LStop);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
