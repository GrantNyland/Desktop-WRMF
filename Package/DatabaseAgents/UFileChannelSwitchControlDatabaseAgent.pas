//
//
//  UNIT      : Contains TFileChannelSwitchControlDatabaseAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 04/01/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UFileChannelSwitchControlDatabaseAgent;

interface

uses
  Classes, sysutils,Db,

  //  DWAF VCL
  VoaimsCom_TLB,
  UFileNames,
  UConstants,
  UDatabaseUtilities,
  UAbstractObject,
  UDataFileObjects,
  UChannelSwitchControlFileDataObjects,
  UAbstractFileNamesObject,
  UAbstractDatabaseAgent,
  UPlanningFileDataObjects;

type

  TFileChannelSwitchControlDatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadChannelSwitchControlSQL(AFileNumber: integer): string;
    function WriteChannelSwitchControlSQL: string;
    function ReadChannelSwitchControlUnkownDataSQL(AFileID:integer): string;
    function WriteChannelSwitchControlUnkownDataSQL: string;
    function WriteSwitchDefinitionSQL: string;
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
     UFileNameConstants,
     UErrorHandlingOperations;

function TFileChannelSwitchControlDatabaseAgent.ReadChannelSwitchControlUnkownDataSQL(AFileID:integer): string;
const OPNAME = 'TFileChannelSwitchControlDatabaseAgent.ReadChannelSwitchControlUnkownDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,LineNumber,LineData  '+
              ' FROM WRYMFileLines'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND FileType      =  '+IntToStr(AFileID)+
              ' AND FileGroup     =  '+IntToStr(fgChannelSwitchControl)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,LineNumber,LineSection';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileChannelSwitchControlDatabaseAgent.WriteChannelSwitchControlUnkownDataSQL: string;
const OPNAME = 'TFileChannelSwitchControlDatabaseAgent.WriteChannelSwitchControlUnkownDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRYMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileChannelSwitchControlDatabaseAgent.ReadChannelSwitchControlSQL(AFileNumber: integer): string;
const OPNAME = 'TFileChannelSwitchControlDatabaseAgent.ReadChannelSwitchControlSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,ChannelSwitchID, ChannelNumber,'+
              ' SwitchDefinitionID, SwitchAssociatedNodeNr, SwitchWaterLevel, SwitchType, SwitchInitialStatus'+
              ' FROM ChannelSwitchControl'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND SwitchDefinitionID   =  '+ IntToStr(AFileNumber)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,ChannelSwitchID';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileChannelSwitchControlDatabaseAgent.WriteChannelSwitchControlSQL: string;
const OPNAME = 'TFileChannelSwitchControlDatabaseAgent.WriteChannelSwitchControlSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO ChannelSwitchControl'+
              ' (Model,StudyAreaName,SubArea,Scenario,ChannelSwitchID, ChannelNumber,'+
              ' SwitchDefinitionID, SwitchAssociatedNodeNr, SwitchWaterLevel, SwitchType, SwitchInitialStatus)'+
              ' Values (:Model,:StudyAreaName,:SubArea,:Scenario, :ChannelSwitchID, :ChannelNumber,'+
              ' :SwitchDefinitionID, :SwitchAssociatedNodeNr, :SwitchWaterLevel, :SwitchType, :SwitchInitialStatus)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileChannelSwitchControlDatabaseAgent.WriteSwitchDefinitionSQL: string;
const OPNAME = 'TFileChannelSwitchControlDatabaseAgent.WriteSwitchDefinitionSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO ChannelSwitchDefinition'+
              ' (Model,StudyAreaName,SubArea,Scenario,SwitchDefID,SwitchDefStartYear, SwitchDefStartMonth,SwitchDefFileName)'+
              ' Values (:Model,:StudyAreaName,:SubArea,:Scenario, :SwitchDefID, :SwitchDefStartYear, :SwitchDefStartMonth, :SwitchDefFileName)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileChannelSwitchControlDatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileChannelSwitchControlDatabaseAgent.ReadModelDataFromDatabase';
var
  LMessage       : string;
  LDataSet       : TAbstractModelDataset;
  LStop          : boolean;
  LSwitchChannel         : TChannelSwitchControlObject;
  LChannelSwitchControl  : TChannelSwitchControlFileDataObject;
  LPlanningFileDataObject : TPlanningFileDataObjects;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileChannelSwitchControlDatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LChannelSwitchControl    := LPlanningFileDataObject.AddChannelSwitchControlFileDataObject(AFileName.FileNumber);
    if(LChannelSwitchControl = nil) then
      Exit;
    if not LChannelSwitchControl.Initialise then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try

      LDataSet.SetSQL(ReadChannelSwitchControlSQL(AFileName.FileNumber));
      LDataSet.DataSet.Open;
      //Check if there is any data.
      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFileChannelSwitchControlDatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
        //Exit;
      end;

      while not LDataSet.DataSet.Eof do
      begin

        LSwitchChannel := LChannelSwitchControl.AddChannelSwitchControl;

        if not LDataSet.DataSet.FieldByName('SwitchType').IsNull then
        begin
          LSwitchChannel.SwitchType.FData := LDataSet.DataSet.FieldByName('SwitchType').AsInteger;
          LSwitchChannel.SwitchType.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
        begin
          LSwitchChannel.ChannelNumber.FData := LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
          LSwitchChannel.ChannelNumber.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('SwitchAssociatedNodeNr').IsNull then
        begin
          LSwitchChannel.NodeNumber.FData := LDataSet.DataSet.FieldByName('SwitchAssociatedNodeNr').AsInteger;
          LSwitchChannel.NodeNumber.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('SwitchWaterLevel').IsNull then
        begin
          LSwitchChannel.SwitchLevel.FData := LDataSet.DataSet.FieldByName('SwitchWaterLevel').AsFloat;
          LSwitchChannel.SwitchLevel.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('SwitchInitialStatus').IsNull then
        begin
          LSwitchChannel.SwitchInitialState.FData := LDataSet.DataSet.FieldByName('SwitchInitialStatus').AsInteger;
          LSwitchChannel.SwitchInitialState.FInitalised := True;
        end;
        LDataSet.DataSet.Next;
      end;

      //line6 on wards+++++++++++++++++++++++++++
      LDataSet.DataSet.Active := False;
      LDataSet.SetSQL(ReadChannelSwitchControlUnkownDataSQL(AFilename.FileNumber));
      LDataSet.DataSet.Open;

      while not LDataSet.DataSet.Eof do
      begin
        LChannelSwitchControl.FMExtraLines.Add(TrimRight(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;

      LMessage := FAppModules.Language.GetString('TFileChannelSwitchControlDatabaseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileChannelSwitchControlDatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileChannelSwitchControlDatabaseAgent.WriteModelDataToDatabase';
var
  LMessage       : string;
  LDataSet       : TAbstractModelDataset;
  LStop          : boolean;
  LChannelSwitchID,
  LCount         : integer;
  LSwitchChannel         : TChannelSwitchControlObject;
  LChannelSwitchControl  : TChannelSwitchControlFileDataObject;
  LPlanningFileDataObject : TPlanningFileDataObjects;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileChannelSwitchControlDatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not ClearModelDataInDatabase(TFileNameObject(AFileName),AProgressFunction,True) then
      Exit;

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LChannelSwitchControl    := LPlanningFileDataObject.ChannelSwitchControlFileDataObjectByFileNumber[AFileName.FileNumber];
    if(LChannelSwitchControl = nil) then
      Exit;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.DataSet.Close;
      LDataSet.SetSQL('SELECT MAX(ChannelSwitchID) AS MaxID FROM ChannelSwitchControl');
      LDataSet.DataSet.Open;
      LChannelSwitchID := LDataSet.DataSet.FieldByName('MaxID').AsInteger;

      for LCount := 0 to LChannelSwitchControl.ChannelSwitchControlCount -1 do
      begin
        LSwitchChannel := LChannelSwitchControl.ChannelSwitchControlByIndex[LCount];
        LChannelSwitchID := LChannelSwitchID + 1;

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteChannelSwitchControlSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(
          ['Model','StudyAreaName','SubArea','Scenario'],
          [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode]);

        LDataSet.SetParams(['ChannelSwitchID'], [IntToStr(LChannelSwitchID)]);
        LDataSet.SetParams(['SwitchDefinitionID'], [IntToStr(AFileName.FileNumber)]);

        if LSwitchChannel.SwitchType.FInitalised then
          LDataSet.SetParams(['SwitchType'], [IntToStr(LSwitchChannel.SwitchType.FData)]);

        if LSwitchChannel.ChannelNumber.FInitalised then
          LDataSet.SetParams(['ChannelNumber'], [IntToStr(LSwitchChannel.ChannelNumber.FData)]);

        if LSwitchChannel.NodeNumber.FInitalised then
          LDataSet.SetParams(['SwitchAssociatedNodeNr'], [IntToStr(LSwitchChannel.NodeNumber.FData)]);

        if LSwitchChannel.SwitchLevel.FInitalised then
          LDataSet.SetParams(['SwitchWaterLevel'], [FloatToStr(LSwitchChannel.SwitchLevel.FData)]);

        if LSwitchChannel.SwitchInitialState.FInitalised then
          LDataSet.SetParams(['SwitchInitialStatus'], [IntToStr(LSwitchChannel.SwitchInitialState.FData)]);

        LDataSet.ExecSQL;
      end;

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(WriteSwitchDefinitionSQL);
      LDataset.ClearQueryParams();
      LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
      LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
      LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
      LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
      LDataSet.SetParams(['SwitchDefID'], [IntToStr(LChannelSwitchControl.FileNumber)]);

      if LChannelSwitchControl.SwitchDefFileName.FInitalised then
      LDataSet.SetParams(['SwitchDefFileName'], [LChannelSwitchControl.SwitchDefFileName.FData]);

      if LChannelSwitchControl.SwitchDefStartYear.FInitalised then
        LDataSet.SetParams(['SwitchDefStartYear'], [IntToStr(LChannelSwitchControl.SwitchDefStartYear.FData)]);

      if LChannelSwitchControl.SwitchDefStartMonth.FInitalised then
        LDataSet.SetParams(['SwitchDefStartMonth'], [IntToStr(LChannelSwitchControl.SwitchDefStartMonth.FData)]);
      LDataSet.ExecSQL;

       //line 6 onwards++++++++++++++++++++++++++++
      for LCount := 0 to LChannelSwitchControl.FMExtraLines.Count - 1 do
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteChannelSwitchControlUnkownDataSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
        LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
        LDataSet.SetParams(['LineNumber'], [IntToStr(1+LCount)]);
        LDataSet.SetParams(['LineSection'], [IntToStr(0)]);
        LDataSet.SetParams(['LineData'], [LChannelSwitchControl.FMExtraLines[LCount]]);

        LDataSet.ExecSQL;
      end;
      LDataSet.DataSet.Close;

      Result := InsertFileName(TFileNameObject(AFileName));
      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TFileChannelSwitchControlDatabaseAgent.strWriteEnded');
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    finally
      LDataSet.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileChannelSwitchControlDatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TFileChannelSwitchControlDatabaseAgent.ClearModelDataInDatabase';
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
    LTableNames := 'ChannelSwitchControl';
    Result := DeleteModelData(LTableNames,' AND SwitchDefinitionID   =  '+IntToStr(AFileName.FileNumber),AProgressFunction,AQuetly);
    LTableNames := 'ChannelSwitchDefinition';
    Result :=  Result and DeleteModelData(LTableNames,' AND SwitchDefID   =  '+IntToStr(AFileName.FileNumber),AProgressFunction,AQuetly);
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
