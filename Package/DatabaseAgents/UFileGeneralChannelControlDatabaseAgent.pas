//
//
//  UNIT      : Contains TFileGeneralChannelControlDatabaseAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 04/01/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UFileGeneralChannelControlDatabaseAgent;

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
  UGeneralChannelControlFileDataObjects,
  UAbstractFileNamesObject,
  UAbstractDatabaseAgent,
  UPlanningFileDataObjects;

type

  TFileGeneralChannelControlDatabaseAgent = class(TAbstractDatabaseAgent)
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

function TFileGeneralChannelControlDatabaseAgent.ReadChannelTimeControlUnkownDataSQL(AFileID:integer): string;
const OPNAME = 'TFileGeneralChannelControlDatabaseAgent.ReadChannelTimeControlUnkownDataSQL';
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
              ' AND FileGroup     =  '+IntToStr(fgGeneralChannelControl)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,LineNumber,LineSection';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileGeneralChannelControlDatabaseAgent.WriteChannelTimeControlUnkownDataSQL: string;
const OPNAME = 'TFileGeneralChannelControlDatabaseAgent.WriteChannelTimeControlUnkownDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRYMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileGeneralChannelControlDatabaseAgent.ReadChannelTimeControlSQL: string;
const OPNAME = 'TFileGeneralChannelControlDatabaseAgent.ReadChannelTimeControlSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,Identifier,ChannelNumber, ChannelType,'+
              ' DataYears,ChannelStartYear, ChannelStartMonth, ChannelEndYear, ChannelEndMonth,'+
              ' ChannelEconomicLife, ChannelCapitalCost, ChannelFixedOMCost,'+
              ' ChannelVariableOMCost, ConstructionYears, ChannelCostSchedule,'+
              ' ChannelEscalationCost'+
              ' FROM ChannelTimeControl'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND ChannelType   in  (8,6,5,7,10,18,19,12)'+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileGeneralChannelControlDatabaseAgent.WriteChannelTimeControlSQL: string;
const OPNAME = 'TFileGeneralChannelControlDatabaseAgent.WriteChannelTimeControlSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO ChannelTimeControl'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,ChannelNumber, ChannelType, ChannelStartYear,'+
              ' DataYears,ChannelStartMonth, ChannelEndYear, ChannelEndMonth, ChannelEconomicLife, ChannelCapitalCost,'+
              ' ChannelFixedOMCost, ChannelVariableOMCost, ConstructionYears, ChannelCostSchedule,'+
              ' ChannelEscalationCost)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier, :ChannelNumber, :ChannelType, :ChannelStartYear,'+
              ' :DataYears,:ChannelStartMonth, :ChannelEndYear, :ChannelEndMonth, :ChannelEconomicLife, :ChannelCapitalCost,'+
              ' :ChannelFixedOMCost, :ChannelVariableOMCost, :ConstructionYears, :ChannelCostSchedule,'+
              ' :ChannelEscalationCost)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileGeneralChannelControlDatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileGeneralChannelControlDatabaseAgent.ReadModelDataFromDatabase';
var
  LMessage       : string;
  LDataSet       : TAbstractModelDataset;
  LStop          : boolean;
  LGeneralChannel         : TGeneralChannelControlObject;
  LGeneralChannelControl  : TGeneralChannelControlFileDataObject;
  LPlanningFileDataObject : TPlanningFileDataObjects;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileGeneralChannelControlDatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LGeneralChannelControl    := LPlanningFileDataObject.GeneralChannelControlFileData;
    if(LGeneralChannelControl = nil) then
      Exit;
    if not LGeneralChannelControl.Initialise then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LGeneralChannelControl.DataYears.FData := 0;
      LGeneralChannelControl.DataYears.FInitalised := True;

      LDataSet.SetSQL(ReadChannelTimeControlSQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFileGeneralChannelControlDatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
        //Exit;
      end;

      LGeneralChannelControl.DataYears.FData       := LDataSet.DataSet.FieldByName('DataYears').AsInteger;
      LGeneralChannelControl.DataYears.FInitalised := True;

      if(LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger > 0) then
      begin
      while not LDataSet.DataSet.Eof do
      begin

        LGeneralChannel := LGeneralChannelControl.AddGeneralChannelControl;

        if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
        begin
          LGeneralChannel.ChannelNumber.FData := LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
          LGeneralChannel.ChannelNumber.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('ChannelStartYear').IsNull then
        begin
          LGeneralChannel.YearChannelActive.FData := LDataSet.DataSet.FieldByName('ChannelStartYear').AsInteger;
          LGeneralChannel.YearChannelActive.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('ChannelStartMonth').IsNull then
        begin
          LGeneralChannel.MonthChannelActive.FData := LDataSet.DataSet.FieldByName('ChannelStartMonth').AsInteger;
          LGeneralChannel.MonthChannelActive.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('ChannelEndYear').IsNull then
        begin
          LGeneralChannel.YearChannelAbsolete.FData := LDataSet.DataSet.FieldByName('ChannelEndYear').AsInteger;
          LGeneralChannel.YearChannelAbsolete.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('ChannelEndMonth').IsNull then
        begin
          LGeneralChannel.MonthChannelAbsolete.FData := LDataSet.DataSet.FieldByName('ChannelEndMonth').AsInteger;
          LGeneralChannel.MonthChannelAbsolete.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('ChannelEconomicLife').IsNull then
        begin
          LGeneralChannel.EconomicLifeOfChannel.FData := LDataSet.DataSet.FieldByName('ChannelEconomicLife').AsInteger;
          LGeneralChannel.EconomicLifeOfChannel.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('ChannelCapitalCost').IsNull then
        begin
          LGeneralChannel.CapitalCost.FData := LDataSet.DataSet.FieldByName('ChannelCapitalCost').AsFloat;
          LGeneralChannel.CapitalCost.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('ChannelFixedOMCost').IsNull then
        begin
          LGeneralChannel.FixedMaintenanceCost.FData := LDataSet.DataSet.FieldByName('ChannelFixedOMCost').AsFloat;
          LGeneralChannel.FixedMaintenanceCost.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('ChannelVariableOMCost').IsNull then
        begin
          LGeneralChannel.VariableMaintenanceCost.FData := LDataSet.DataSet.FieldByName('ChannelVariableOMCost').AsFloat;
          LGeneralChannel.VariableMaintenanceCost.FInitalised := True;
        end;

          if not LDataSet.DataSet.FieldByName('ConstructionYears').IsNull then
          begin
            LGeneralChannel.YearsInConstruction.FData := LDataSet.DataSet.FieldByName('ConstructionYears').AsInteger;
            LGeneralChannel.YearsInConstruction.FInitalised := True;
          end;

        if not LDataSet.DataSet.FieldByName('ChannelCostSchedule').IsNull then
        begin
          LGeneralChannel.CostSchedule.FData := Trim(LDataSet.DataSet.FieldByName('ChannelCostSchedule').AsString);
          LGeneralChannel.CostSchedule.FData := StringReplace(LGeneralChannel.CostSchedule.FData,',',' ',[rfReplaceAll, rfIgnoreCase]);
          LGeneralChannel.CostSchedule.FLength := Length(LGeneralChannel.CostSchedule.FData);
          LGeneralChannel.CostSchedule.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('ChannelEscalationCost').IsNull then
        begin
          LGeneralChannel.EscaltionCosts.FData := Trim(LDataSet.DataSet.FieldByName('ChannelEscalationCost').AsString);
          LGeneralChannel.EscaltionCosts.FData := StringReplace(LGeneralChannel.EscaltionCosts.FData,',',' ',[rfReplaceAll, rfIgnoreCase]);
          LGeneralChannel.EscaltionCosts.FLength := Length(LGeneralChannel.EscaltionCosts.FData);
          LGeneralChannel.EscaltionCosts.FInitalised := True;
        end;
        LDataSet.DataSet.Next;
      end;
      end;
      //line6 on wards+++++++++++++++++++++++++++
      LDataSet.DataSet.Active := False;
      LDataSet.SetSQL(ReadChannelTimeControlUnkownDataSQL(AFilename.FileNumber));
      LDataSet.DataSet.Open;

      while not LDataSet.DataSet.Eof do
      begin
        LGeneralChannelControl.FMExtraLines.Add(Trim(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;

      LMessage := FAppModules.Language.GetString('TFileGeneralChannelControlDatabaseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileGeneralChannelControlDatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileGeneralChannelControlDatabaseAgent.WriteModelDataToDatabase';
var
  LMessage       : string;
  LWhereClause   : string;
  LChannelType   : string;
  LDataSet       : TAbstractModelDataset;
  LStop          : boolean;
  LCount         : integer;
  LGeneralChannel         : TGeneralChannelControlObject;
  LGeneralChannelControl  : TGeneralChannelControlFileDataObject;
  LPlanningFileDataObject : TPlanningFileDataObjects;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileGeneralChannelControlDatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not ClearModelDataInDatabase(TFileNameObject(AFileName),AProgressFunction,True) then
      Exit;

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LGeneralChannelControl    := LPlanningFileDataObject.GeneralChannelControlFileData;
    if(LGeneralChannelControl = nil) then
      Exit;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LChannelType := '12';
      if(LGeneralChannelControl.GeneralChannelControlCount = 0) then
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteChannelTimeControlSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(
          ['Model','StudyAreaName','SubArea','Scenario'],
          [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode]);

        LDataSet.SetParams(['Identifier'], ['1']);
        LDataSet.SetParams(['ChannelNumber'], ['0']);
        LDataSet.SetParams(['ChannelType'], [LChannelType]);
        LDataSet.SetParams(['DataYears'], [IntToStr(LGeneralChannelControl.DataYears.FData)]);

        LDataSet.ExecSQL;
      end
      else
      begin
      for LCount := 0 to LGeneralChannelControl.GeneralChannelControlCount -1 do
      begin
        LGeneralChannel := LGeneralChannelControl.GeneralChannelControlByIndex[LCount];

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteChannelTimeControlSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(
          ['Model','StudyAreaName','SubArea','Scenario'],
          [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode]);

          LDataSet.SetParams(['Identifier'], [IntToStr(LCount+1)]);
        LDataSet.SetParams(['ChannelType'], [LChannelType]);
          LDataSet.SetParams(['DataYears'], [IntToStr(LGeneralChannelControl.DataYears.FData)]);

        if LGeneralChannel.ChannelNumber.FInitalised then
          LDataSet.SetParams(['ChannelNumber'], [IntToStr(LGeneralChannel.ChannelNumber.FData)]);

        if LGeneralChannel.YearChannelActive.FInitalised then
          LDataSet.SetParams(['ChannelStartYear'], [IntToStr(LGeneralChannel.YearChannelActive.FData)]);

        if LGeneralChannel.MonthChannelActive.FInitalised then
          LDataSet.SetParams(['ChannelStartMonth'], [IntToStr(LGeneralChannel.MonthChannelActive.FData)]);

        if LGeneralChannel.YearChannelAbsolete.FInitalised then
          LDataSet.SetParams(['ChannelEndYear'], [IntToStr(LGeneralChannel.YearChannelAbsolete.FData)]);

        if LGeneralChannel.MonthChannelAbsolete.FInitalised then
          LDataSet.SetParams(['ChannelEndMonth'], [IntToStr(LGeneralChannel.MonthChannelAbsolete.FData)]);

        if LGeneralChannel.EconomicLifeOfChannel.FInitalised then
          LDataSet.SetParams(['ChannelEconomicLife'], [IntToStr(LGeneralChannel.EconomicLifeOfChannel.FData)]);

        if LGeneralChannel.CapitalCost.FInitalised then
          LDataSet.SetParams(['ChannelCapitalCost'], [FloatToStr(LGeneralChannel.CapitalCost.FData)]);

        if LGeneralChannel.FixedMaintenanceCost.FInitalised then
          LDataSet.SetParams(['ChannelFixedOMCost'], [FloatToStr(LGeneralChannel.FixedMaintenanceCost.FData)]);

        if LGeneralChannel.VariableMaintenanceCost.FInitalised then
          LDataSet.SetParams(['ChannelVariableOMCost'], [FloatToStr(LGeneralChannel.VariableMaintenanceCost.FData)]);

          if LGeneralChannel.YearsInConstruction.FInitalised then
            LDataSet.SetParams(['ConstructionYears'], [IntToStr(LGeneralChannel.YearsInConstruction.FData)]);

        LDataSet.ExecSQL;

        if LGeneralChannel.CostSchedule.FInitalised then
        begin
          LWhereClause := ' WHERE Model         =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
                          ' AND StudyAreaName   =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
                          ' AND SubArea         =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
                          ' AND Scenario        =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
                          ' AND ChannelNumber = ' + IntToStr(LGeneralChannel.ChannelNumber.FData)+
                          ' AND ChannelType   = ' + LChannelType;
           FAppModules.Database.UpdateMemoField('ChannelTimeControl','ChannelCostSchedule',LWhereClause,
                                                 LGeneralChannel.CostSchedule.FData);
        end;

        if LGeneralChannel.EscaltionCosts.FInitalised then
        begin
          LWhereClause := ' WHERE Model       = ' + QuotedStr(FAppModules.StudyArea.ModelCode)+
                          ' AND StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
                          ' AND SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)+
                          ' AND Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)+
                          ' AND ChannelNumber = ' + IntToStr(LGeneralChannel.ChannelNumber.FData)+
                          ' AND ChannelType   = ' + LChannelType;
           FAppModules.Database.UpdateMemoField('ChannelTimeControl','ChannelEscalationCost',LWhereClause,
                                                 LGeneralChannel.EscaltionCosts.FData);
        end;
      end;
      end;
       //line 6 onwards++++++++++++++++++++++++++++
      for LCount := 0 to LGeneralChannelControl.FMExtraLines.Count - 1 do
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
        LDataSet.SetParams(['LineData'], [LGeneralChannelControl.FMExtraLines[LCount]]);

        LDataSet.ExecSQL;
      end;
      LDataSet.DataSet.Close;

      Result := InsertFileName(TFileNameObject(AFileName));
      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TFileGeneralChannelControlDatabaseAgent.strWriteEnded');
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    finally
      LDataSet.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileGeneralChannelControlDatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TFileGeneralChannelControlDatabaseAgent.ClearModelDataInDatabase';
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
    Result := DeleteModelData(LTableNames,' AND ChannelType   =  12 ',AProgressFunction,AQuetly);
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
