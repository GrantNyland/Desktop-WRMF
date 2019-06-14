//
//
//  UNIT      : Contains TFilePumpingChannelControlDatabaseAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 04/01/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UFilePumpingChannelControlDatabaseAgent;

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
  UPumpingChannelControlFileDataObjects,
  UAbstractFileNamesObject,
  UAbstractDatabaseAgent,
  UPlanningFileDataObjects;

type

  TFilePumpingChannelControlDatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadChannelTimeControlSQL: string;
    function WriteChannelTimeControlSQL: string;
    function ReadChannelTimeControlUnkownDataSQL(AFileID:integer): string;
    function WriteChannelTimeControlUnkownDataSQL: string;
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

function TFilePumpingChannelControlDatabaseAgent.ReadChannelTimeControlUnkownDataSQL(AFileID:integer): string;
const OPNAME = 'TFilePumpingChannelControlDatabaseAgent.ReadChannelTimeControlUnkownDataSQL';
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
              ' AND FileGroup     =  '+IntToStr(fgPumpingChannelControl)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,LineNumber,LineSection';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilePumpingChannelControlDatabaseAgent.WriteChannelTimeControlUnkownDataSQL: string;
const OPNAME = 'TFilePumpingChannelControlDatabaseAgent.WriteChannelTimeControlUnkownDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRYMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilePumpingChannelControlDatabaseAgent.ReadChannelTimeControlSQL: string;
const OPNAME = 'TFilePumpingChannelControlDatabaseAgent.ReadChannelTimeControlSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,Identifier,ChannelNumber, ChannelType,'+
              ' DataYears, ChannelStartYear, ChannelStartMonth, ChannelEndYear, ChannelEndMonth,'+
              ' ChannelEconomicLife, ChannelCapitalCost, ChannelFixedOMCost,'+
              ' ChannelVariableOMCost, ConstructionYears, ChannelCostSchedule,'+
              ' ChannelEscalationCost'+
              ' FROM ChannelTimeControl'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND ChannelType   =  9'+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilePumpingChannelControlDatabaseAgent.WriteChannelTimeControlSQL: string;
const OPNAME = 'TFilePumpingChannelControlDatabaseAgent.WriteChannelTimeControlSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO ChannelTimeControl'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,ChannelNumber, ChannelType, DataYears, ChannelStartYear,'+
              ' ChannelStartMonth, ChannelEndYear, ChannelEndMonth, ChannelEconomicLife, ChannelCapitalCost,'+
              ' ChannelFixedOMCost, ChannelVariableOMCost, ConstructionYears, ChannelCostSchedule,'+
              ' ChannelEscalationCost)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier, :ChannelNumber, :ChannelType, :DataYears, :ChannelStartYear,'+
              ' :ChannelStartMonth, :ChannelEndYear, :ChannelEndMonth, :ChannelEconomicLife, :ChannelCapitalCost,'+
              ' :ChannelFixedOMCost, :ChannelVariableOMCost, :ConstructionYears, :ChannelCostSchedule,'+
              ' :ChannelEscalationCost)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilePumpingChannelControlDatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilePumpingChannelControlDatabaseAgent.ReadModelDataFromDatabase';
var
  LMessage       : string;
  LDataSet       : TAbstractModelDataset;
  LStop          : boolean;
  LPumpingChannel         : TPumpingChannelControlObject;
  LPumpingChannelControl  : TPumpingChannelControlFileDataObject;
  LPlanningFileDataObject : TPlanningFileDataObjects;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFilePumpingChannelControlDatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LPumpingChannelControl    := LPlanningFileDataObject.PumpingChannelControlFileData;
    if(LPumpingChannelControl = nil) then
      Exit;
    if not LPumpingChannelControl.Initialise then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LPumpingChannelControl.DataYears.FData := 0;
      LPumpingChannelControl.DataYears.FInitalised := True;

      LDataSet.SetSQL(ReadChannelTimeControlSQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFilePumpingChannelControlDatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
        //Exit;
      end;

      LPumpingChannelControl.DataYears.FData       := LDataSet.DataSet.FieldByName('DataYears').AsInteger;
      LPumpingChannelControl.DataYears.FInitalised := True;

      while not LDataSet.DataSet.Eof do
      begin

        LPumpingChannel := LPumpingChannelControl.AddPumpingChannelControl;

        if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
        begin
          LPumpingChannel.ChannelNumber.FData := LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
          LPumpingChannel.ChannelNumber.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('ChannelStartYear').IsNull then
        begin
          LPumpingChannel.YearChannelActive.FData := LDataSet.DataSet.FieldByName('ChannelStartYear').AsInteger;
          LPumpingChannel.YearChannelActive.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('ChannelStartMonth').IsNull then
        begin
          LPumpingChannel.MonthChannelActive.FData := LDataSet.DataSet.FieldByName('ChannelStartMonth').AsInteger;
          LPumpingChannel.MonthChannelActive.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('ChannelEndYear').IsNull then
        begin
          LPumpingChannel.YearChannelAbsolete.FData := LDataSet.DataSet.FieldByName('ChannelEndYear').AsInteger;
          LPumpingChannel.YearChannelAbsolete.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('ChannelEndMonth').IsNull then
        begin
          LPumpingChannel.MonthChannelAbsolete.FData := LDataSet.DataSet.FieldByName('ChannelEndMonth').AsInteger;
          LPumpingChannel.MonthChannelAbsolete.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('ChannelEconomicLife').IsNull then
        begin
          LPumpingChannel.EconomicLifeOfChannel.FData := LDataSet.DataSet.FieldByName('ChannelEconomicLife').AsInteger;
          LPumpingChannel.EconomicLifeOfChannel.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('ChannelCapitalCost').IsNull then
        begin
          LPumpingChannel.CapitalCost.FData := LDataSet.DataSet.FieldByName('ChannelCapitalCost').AsFloat;
          LPumpingChannel.CapitalCost.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('ChannelFixedOMCost').IsNull then
        begin
          LPumpingChannel.FixedMaintenanceCost.FData := LDataSet.DataSet.FieldByName('ChannelFixedOMCost').AsFloat;
          LPumpingChannel.FixedMaintenanceCost.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('ChannelVariableOMCost').IsNull then
        begin
          LPumpingChannel.VariableMaintenanceCost.FData := LDataSet.DataSet.FieldByName('ChannelVariableOMCost').AsFloat;
          LPumpingChannel.VariableMaintenanceCost.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('ConstructionYears').IsNull then
        begin
          LPumpingChannel.YearsInConstruction.FData := LDataSet.DataSet.FieldByName('ConstructionYears').AsInteger;
          LPumpingChannel.YearsInConstruction.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('ChannelCostSchedule').IsNull then
        begin
          LPumpingChannel.CostSchedule.FData := Trim(LDataSet.DataSet.FieldByName('ChannelCostSchedule').AsString);
          LPumpingChannel.CostSchedule.FData := StringReplace(LPumpingChannel.CostSchedule.FData,',',' ',[rfReplaceAll, rfIgnoreCase]);
          LPumpingChannel.CostSchedule.FLength := Length(LPumpingChannel.CostSchedule.FData);
          LPumpingChannel.CostSchedule.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('ChannelEscalationCost').IsNull then
        begin
          LPumpingChannel.EscaltionCosts.FData := Trim(LDataSet.DataSet.FieldByName('ChannelEscalationCost').AsString);
          LPumpingChannel.EscaltionCosts.FData := StringReplace(LPumpingChannel.EscaltionCosts.FData,',',' ',[rfReplaceAll, rfIgnoreCase]);
          LPumpingChannel.EscaltionCosts.FLength := Length(LPumpingChannel.EscaltionCosts.FData);
          LPumpingChannel.EscaltionCosts.FInitalised := True;
        end;
        LDataSet.DataSet.Next;
      end;

      //line5 on wards+++++++++++++++++++++++++++
      LDataSet.DataSet.Active := False;
      LDataSet.SetSQL(ReadChannelTimeControlUnkownDataSQL(AFilename.FileNumber));
      LDataSet.DataSet.Open;

      while not LDataSet.DataSet.Eof do
      begin
        LPumpingChannelControl.FMExtraLines.Add(Trim(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;

      LMessage := FAppModules.Language.GetString('TFilePumpingChannelControlDatabaseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilePumpingChannelControlDatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilePumpingChannelControlDatabaseAgent.WriteModelDataToDatabase';
var
  LChannelType,
  LWhereClause,
  LMessage       : string;
  LDataSet       : TAbstractModelDataset;
  LStop          : boolean;
  LCount         : integer;
  LPumpingChannel         : TPumpingChannelControlObject;
  LPumpingChannelControl  : TPumpingChannelControlFileDataObject;
  LPlanningFileDataObject : TPlanningFileDataObjects;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFilePumpingChannelControlDatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not ClearModelDataInDatabase(TFileNameObject(AFileName),AProgressFunction,True) then
      Exit;

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LPumpingChannelControl    := LPlanningFileDataObject.PumpingChannelControlFileData;
    if(LPumpingChannelControl = nil) then
      Exit;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LChannelType := '9';
      for LCount := 0 to LPumpingChannelControl.PumpingChannelControlCount -1 do
      begin
        LPumpingChannel := LPumpingChannelControl.PumpingChannelControlByIndex[LCount];

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteChannelTimeControlSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(
          ['Model','StudyAreaName','SubArea','Scenario'],
          [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode]);

        LDataSet.SetParams(['Identifier'], [IntToStr(LCount+1)]);
        LDataSet.SetParams(['ChannelType'], [LChannelType]);
        LDataSet.SetParams(['DataYears'], [IntToStr(LPumpingChannelControl.DataYears.FData)]);

        if LPumpingChannel.ChannelNumber.FInitalised then
          LDataSet.SetParams(['ChannelNumber'], [IntToStr(LPumpingChannel.ChannelNumber.FData)]);

        if LPumpingChannel.YearChannelActive.FInitalised then
          LDataSet.SetParams(['ChannelStartYear'], [IntToStr(LPumpingChannel.YearChannelActive.FData)]);

        if LPumpingChannel.MonthChannelActive.FInitalised then
          LDataSet.SetParams(['ChannelStartMonth'], [IntToStr(LPumpingChannel.MonthChannelActive.FData)]);

        if LPumpingChannel.YearChannelAbsolete.FInitalised then
          LDataSet.SetParams(['ChannelEndYear'], [IntToStr(LPumpingChannel.YearChannelAbsolete.FData)]);

        if LPumpingChannel.MonthChannelAbsolete.FInitalised then
          LDataSet.SetParams(['ChannelEndMonth'], [IntToStr(LPumpingChannel.MonthChannelAbsolete.FData)]);

        if LPumpingChannel.EconomicLifeOfChannel.FInitalised then
          LDataSet.SetParams(['ChannelEconomicLife'], [IntToStr(LPumpingChannel.EconomicLifeOfChannel.FData)]);

        if LPumpingChannel.CapitalCost.FInitalised then
          LDataSet.SetParams(['ChannelCapitalCost'], [FloatToStr(LPumpingChannel.CapitalCost.FData)]);

        if LPumpingChannel.FixedMaintenanceCost.FInitalised then
          LDataSet.SetParams(['ChannelFixedOMCost'], [FloatToStr(LPumpingChannel.FixedMaintenanceCost.FData)]);

        if LPumpingChannel.VariableMaintenanceCost.FInitalised then
          LDataSet.SetParams(['ChannelVariableOMCost'], [FloatToStr(LPumpingChannel.VariableMaintenanceCost.FData)]);

        if LPumpingChannel.YearsInConstruction.FInitalised then
          LDataSet.SetParams(['ConstructionYears'], [IntToStr(LPumpingChannel.YearsInConstruction.FData)]);

        LDataSet.ExecSQL;

        if LPumpingChannel.CostSchedule.FInitalised then
        begin
          LWhereClause := ' WHERE Model       = ' + QuotedStr(FAppModules.StudyArea.ModelCode)+
                          ' AND StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
                          ' AND SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)+
                          ' AND Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)+
                          ' AND ChannelNumber = ' + IntToStr(LPumpingChannel.ChannelNumber.FData)+
                          ' AND ChannelType   = ' + LChannelType;
           FAppModules.Database.UpdateMemoField('ChannelTimeControl','ChannelCostSchedule',LWhereClause,
                                                 LPumpingChannel.CostSchedule.FData);
        end;

        if LPumpingChannel.EscaltionCosts.FInitalised then
        begin
          LWhereClause := ' WHERE Model       = ' + QuotedStr(FAppModules.StudyArea.ModelCode)+
                          ' AND StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
                          ' AND SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)+
                          ' AND Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)+
                          ' AND ChannelNumber = ' + IntToStr(LPumpingChannel.ChannelNumber.FData)+
                          ' AND ChannelType   = ' + LChannelType;
           FAppModules.Database.UpdateMemoField('ChannelTimeControl','ChannelEscalationCost',LWhereClause,
                                                 LPumpingChannel.EscaltionCosts.FData);
        end;
      end;

       //line13 onwards++++++++++++++++++++++++++++
      for LCount := 0 to LPumpingChannelControl.FMExtraLines.Count - 1 do
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteChannelTimeControlUnkownDataSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
        LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
        LDataSet.SetParams(['LineNumber'], [IntToStr(1+LCount)]);
        LDataSet.SetParams(['LineSection'], [IntToStr(0)]);
        LDataSet.SetParams(['LineData'], [LPumpingChannelControl.FMExtraLines[LCount]]);

        LDataSet.ExecSQL;
      end;
      LDataSet.DataSet.Close;

      Result := InsertFileName(TFileNameObject(AFileName));
      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TFilePumpingChannelControlDatabaseAgent.strWriteEnded');
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    finally
      LDataSet.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilePumpingChannelControlDatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TFilePumpingChannelControlDatabaseAgent.ClearModelDataInDatabase';
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
    LTableNames := 'ChannelTimeControl';
    Result := DeleteModelData(LTableNames,' AND ChannelType   =  9 ',AProgressFunction,AQuetly);
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
