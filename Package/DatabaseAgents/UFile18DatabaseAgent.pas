//  UNIT      : Contains TFile18DatabaseAgent Class
//  AUTHOR    : Maurice Marinus
//  DATE      : 05/08/2006
//  COPYRIGHT : Copyright © 2006 DWAF
unit UFile18DatabaseAgent;

interface

uses
  Classes, Sysutils, Db,

  //  DWAF VCL
  UFileNames,
  UConstants,
  UDatabaseUtilities,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UWetlandObject,
  UAbstractDatabaseAgent,
  UYieldModelDataObject;

type

  TFile18DatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadF18UnkownDataSQL: string;
    function ReadWetlandDataSQL: string;

    function WriteF18UnkownDataSQL: string;
    function WriteWetlandDataSQL: string;
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

uses UUtilities,
     UDataSetType,
     UChannelDescriptionObject,
     UErrorHandlingOperations;

function TFile18DatabaseAgent.ReadF18UnkownDataSQL: string;
const OPNAME = 'TFile18DatabaseAgent.ReadF18UnkownDataSQL';
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

function TFile18DatabaseAgent.ReadWetlandDataSQL: string;
const
  OPNAME      = 'TFile18DatabaseAgent.ReadWetlandDataSQL';
begin
  Result := '';
  try
    Result  := Format('SELECT Model ,StudyAreaName, SubArea, Scenario, Identifier, '+
                ' NodeNumber, WetlandName, UpstreamThreshold, InflowProportion, Storage, '+
                ' OutflowProportion,InflowChannelNumber,OutflowChannelNumber'+
                ' FROM Wetland '+
                ' WHERE Model=%s AND StudyAreaName=%s AND SubArea=%s AND Scenario=%s '+
                ' ORDER BY Model ,StudyAreaName, SubArea, Scenario, Identifier ',[QuotedStr(FAppModules.StudyArea.ModelCode),
                                  QuotedStr(FAppModules.StudyArea.StudyAreaCode),
                                  QuotedStr(FAppModules.StudyArea.SubAreaCode),
                                  QuotedStr(FAppModules.StudyArea.ScenarioCode)]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile18DatabaseAgent.WriteWetlandDataSQL: string;
const
  OPNAME      = 'TFile18DatabaseAgent.WriteWetlandDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO Wetland '+
              '(Model ,StudyAreaName, SubArea, Scenario, Identifier, NodeNumber, WetlandName, UpstreamThreshold,'+
              ' InflowProportion, Storage, OutflowProportion,InflowChannelNumber,OutflowChannelNumber)'+
              'Values '+
              '(:Model ,:StudyAreaName, :SubArea, :Scenario, :Identifier, :NodeNumber, :WetlandName, '+
              ' :UpstreamThreshold, :InflowProportion, :Storage, :OutflowProportion,:InflowChannelNumber,:OutflowChannelNumber)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile18DatabaseAgent.WriteF18UnkownDataSQL: string;
const OPNAME = 'TFile18DatabaseAgent.WriteF18UnkownDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRYMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile18DatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile18DatabaseAgent.ReadModelDataFromDatabase';
var
  LMessage        : string;
  LDataSet,
  LDataSet2       : TAbstractModelDataset;
  LWetlandObject  : TWetlandObject;
  LWetland        : TWetland;
  LStop           : Boolean;
  LWetlandChannel : TWetlandChannelObject;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile18DatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LWetlandObject := ADataObject.FWetlandObject;

    if not LWetlandObject.Initialise then
      Exit;

    FAppModules.Database.CreateDataset(Integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(Integer(dtExecSQL), LDataSet2);
    try
      LDataSet.SetSQL(ReadWetlandDataSQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile18DatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
      end
      else
      begin
        while not LDataSet.DataSet.Eof do
        begin
          LWetland  := LWetlandObject.AddWetland;

          if not LDataSet.DataSet.FieldByName('Identifier').IsNull then
          begin
            LWetland.Identifier.FData       := LDataSet.DataSet.FieldByName('Identifier').AsInteger;
            LWetland.Identifier.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('NodeNumber').IsNull then
          begin
            LWetland.NodeNumber.FData        := LDataSet.DataSet.FieldByName('NodeNumber').AsInteger;
            LWetland.NodeNumber.FInitalised  := True;
          end;
          if not LDataSet.DataSet.FieldByName('WetlandName').IsNull then
          begin
            LWetland.Name.FData        := Trim(LDataSet.DataSet.FieldByName('WetlandName').AsString);
            LWetland.Name.FLength      := Length(LWetland.Name.FData);
            LWetland.Name.FInitalised  := True;
          end;
          if not LDataSet.DataSet.FieldByName('UpstreamThreshold').IsNull then
          begin
            LWetland.UpstreamThreshold.FData := LDataSet.DataSet.FieldByName('UpstreamThreshold').AsFloat;
            LWetland.UpstreamThreshold.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('InflowProportion').IsNull then
          begin
            LWetland.InflowProportion.FData := LDataSet.DataSet.FieldByName('InflowProportion').AsFloat;
            LWetland.InflowProportion.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('Storage').IsNull then
          begin
            LWetland.StorageVolume.FData := LDataSet.DataSet.FieldByName('Storage').AsFloat;
            LWetland.StorageVolume.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('OutflowProportion').IsNull then
          begin
            LWetland.OutflowProportion.FData := LDataSet.DataSet.FieldByName('OutflowProportion').AsFloat;
            LWetland.OutflowProportion.FInitalised := True;
          end;

          if LWetland.NodeNumber.FInitalised then
          begin
            LWetlandChannel := ADataObject.FChannelDescrObject.FindWetlandByNodeNumber(LWetland.NodeNumber.FData);
            if(LWetlandChannel <> nil) then
            begin
              if not LDataSet.DataSet.FieldByName('InflowChannelNumber').IsNull then
              begin
                LWetlandChannel.FInflowChannelNr.FData := LDataSet.DataSet.FieldByName('InflowChannelNumber').AsInteger;
                LWetlandChannel.FInflowChannelNr.FInitalised := True;
              end;
               if not LDataSet.DataSet.FieldByName('OutflowChannelNumber').IsNull then
              begin
                LWetlandChannel.FOutflowChannelNr.FData := LDataSet.DataSet.FieldByName('OutflowChannelNumber').AsInteger;
                LWetlandChannel.FOutflowChannelNr.FInitalised := True;
              end;
            end;
          end;
          LDataSet.DataSet.Next;
        end;
      end;

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadF18UnkownDataSQL);
      LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
      LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
      LDataSet.DataSet.Open;

      while not LDataSet.DataSet.Eof do
      begin
        LWetlandObject.ExtraLines.Add(TrimRight(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;

      LMessage := FAppModules.Language.GetString('TFile18DatabaseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile18DatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile18DatabaseAgent.WriteModelDataToDatabase';
var
  LMessage        : string;
  LLinesCount,
  LCounter        : Integer;
  LDataSet        : TAbstractModelDataset;
  LWetlandObject  : TWetlandObject;
  LWetland        : TWetland;
  LStop           : Boolean;
  LWetlandChannel : TWetlandChannelObject;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile18DatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');
    if not ClearModelDataInDatabase(AFileName,AProgressFunction,True) then
      Exit;
    LWetlandObject := ADataObject.FWetlandObject;
    if not Assigned(LWetlandObject) then
      Exit;

    FAppModules.Database.CreateDataset(Integer(dtExecSQL), LDataSet);
    try
      for LLinesCount := 0 to LWetlandObject.WetlandCount-1 do
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteWetlandDataSQL);
        LDataSet.ClearQueryParams();

        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LLinesCount+1)]);

        LWetland := LWetlandObject.WetlandObjectByIndex[LLinesCount];
        if LWetland.NodeNumber.FInitalised then
          LDataSet.SetParams(['NodeNumber'], [IntToStr(LWetland.NodeNumber.FData)]);
        if LWetland.Name.FInitalised then
          LDataSet.SetParams(['WetlandName'], [LWetland.Name.FData]);
        if LWetland.UpstreamThreshold.FInitalised then
          LDataSet.SetParams(['UpstreamThreshold'], [FloatToStr(LWetland.UpstreamThreshold.FData)]);
        if LWetland.InflowProportion.FInitalised then
          LDataSet.SetParams(['InflowProportion'], [FloatToStr(LWetland.InflowProportion.FData)]);
        if LWetland.StorageVolume.FInitalised then
          LDataSet.SetParams(['Storage'], [FloatToStr(LWetland.StorageVolume.FData)]);
        if LWetland.OutflowProportion.FInitalised then
          LDataSet.SetParams(['OutflowProportion'], [FloatToStr(LWetland.OutflowProportion.FData)]);

        if LWetland.NodeNumber.FInitalised then
        begin
          LWetlandChannel := ADataObject.FChannelDescrObject.FindWetlandByNodeNumber(LWetland.NodeNumber.FData);
          if(LWetlandChannel <> nil) then
          begin
            if  LWetlandChannel.FInflowChannelNr.FInitalised then
              LDataSet.SetParams(['InflowChannelNumber'], [IntToStr(LWetlandChannel.FInflowChannelNr.FData)]);
            if  LWetlandChannel.FOutflowChannelNr.FInitalised then
              LDataSet.SetParams(['OutflowChannelNumber'], [IntToStr(LWetlandChannel.FOutflowChannelNr.FData)]);
          end;
        end;

        LDataSet.ExecSQL;
        LDataSet.DataSet.Close;
      end;

      for LCounter := 0 to LWetlandObject.ExtraLines.Count - 1 do
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteF18UnkownDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
        LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
        LDataSet.SetParams(['LineNumber'], [IntToStr(LCounter+1)]);
        LDataSet.SetParams(['LineSection'], [IntToStr(0)]);
        LDataSet.SetParams(['LineData'], [LWetlandObject.ExtraLines[LCounter]]);

        LDataSet.ExecSQL;
      end;
      LDataSet.DataSet.Close;

      Result := InsertFileName(AFileName);
      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TFile18DatabaseAgent.strWriteEnded');
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile18DatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TFile18DatabaseAgent.ClearModelDataInDatabase';
var
  LTableNames,
  LMessage  : string;
  LStop     : boolean;
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

    LTableNames := ' Wetland ';
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
