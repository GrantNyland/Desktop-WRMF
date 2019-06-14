//
//
//  UNIT      : Contains TFileReservoirImplementationDatabaseAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 04/01/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UFileReservoirImplementationDatabaseAgent;

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
  UReservoirImplementationFileDataObjects,
  UAbstractFileNamesObject,
  UAbstractDatabaseAgent,
  UPlanningFileDataObjects;

type

  TFileReservoirImplementationDatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadReservoirTimeControlSQL: string;
    function WriteReservoirTimeControlSQL: string;
    function ReadReservoirSwitchDefinitionSQL(AFileGroupID : integer): string;
    function WriteReservoirSwitchDefinitionSQL: string;
    function ReadReservoirSwitchFileNameSQL(AFileGroupID,ASwitchDefID : integer): string;
    function WriteReservoirSwitchFileNameSQL: string;
    function ReadImplementationUnkownDataSQL(AFileID:integer): string;
    function WriteImplementationUnkownDataSQL: string;
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
     UBasicObjects,
     UUtilities,
     UDataSetType,
     UFileNameConstants,
     UErrorHandlingOperations;

function TFileReservoirImplementationDatabaseAgent.ReadImplementationUnkownDataSQL(AFileID:integer): string;
const OPNAME = 'TFileReservoirImplementationDatabaseAgent.ReadImplementationUnkownDataSQL';
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
              ' AND FileGroup     =  '+IntToStr(fgReservoirImplementation)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,LineNumber,LineSection';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileReservoirImplementationDatabaseAgent.WriteImplementationUnkownDataSQL: string;
const OPNAME = 'TFileReservoirImplementationDatabaseAgent.WriteImplementationUnkownDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRYMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileReservoirImplementationDatabaseAgent.ReadReservoirTimeControlSQL: string;
const OPNAME = 'TFileReservoirImplementationDatabaseAgent.ReadReservoirTimeControlSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,Identifier,ReservoirNumber,BaseNodeNumber,ReservoirStartYear,'+
              ' ReservoirStartMonth, ReservoirEndYear, ReservoirEndMonth, ReservoirEconomicLife,'+
              ' ReservoirCapitalCost, ReservoirOMCost, ReservoirCostSchedule'+
              ' FROM ReservoirTimeControl'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileReservoirImplementationDatabaseAgent.WriteReservoirTimeControlSQL: string;
const OPNAME = 'TFileReservoirImplementationDatabaseAgent.WriteReservoirTimeControlSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO ReservoirTimeControl'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,ReservoirNumber,BaseNodeNumber,ReservoirStartYear,'+
              ' ReservoirStartMonth, ReservoirEndYear, ReservoirEndMonth, ReservoirEconomicLife,'+
              ' ReservoirCapitalCost, ReservoirOMCost)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:ReservoirNumber,:BaseNodeNumber,:ReservoirStartYear,'+
              ' :ReservoirStartMonth, :ReservoirEndYear, :ReservoirEndMonth, :ReservoirEconomicLife,'+
              ' :ReservoirCapitalCost, :ReservoirOMCost)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileReservoirImplementationDatabaseAgent.ReadReservoirSwitchDefinitionSQL(AFileGroupID : integer): string;
const OPNAME = 'TFileReservoirImplementationDatabaseAgent.ReadReservoirSwitchDefinitionSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,FileGroupID,SwitchDefID,StartYear,StartMonth'+
              ' FROM ReservoirSwitchDefinition'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND FileGroupID   =  '+IntToStr(AFileGroupID)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,FileGroupID,SwitchDefID';
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFileReservoirImplementationDatabaseAgent.WriteReservoirSwitchDefinitionSQL: string;
const OPNAME = 'TFileReservoirImplementationDatabaseAgent.WriteReservoirSwitchDefinitionSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO ReservoirSwitchDefinition'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileGroupID,SwitchDefID,StartYear, StartMonth)'+
              ' Values (:Model,:StudyAreaName,:SubArea,:Scenario,:FileGroupID,:SwitchDefID,:StartYear,:StartMonth)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileReservoirImplementationDatabaseAgent.ReadReservoirSwitchFileNameSQL(AFileGroupID, ASwitchDefID: integer): string;
const OPNAME = 'TFileReservoirImplementationDatabaseAgent.ReadReservoirSwitchDefinitionSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,FileGroupID,SwitchDefID,FileIdentifier,FileName'+
              ' FROM ReservoirSwitchFileName'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND FileGroupID   =  '+IntToStr(AFileGroupID)+
              ' AND SwitchDefID   =  '+IntToStr(ASwitchDefID)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,FileGroupID,SwitchDefID,FileIdentifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileReservoirImplementationDatabaseAgent.WriteReservoirSwitchFileNameSQL: string;
const OPNAME = 'TFileReservoirImplementationDatabaseAgent.WriteReservoirSwitchDefinitionSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO ReservoirSwitchFileName'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileGroupID,SwitchDefID,FileIdentifier,FileName)'+
              ' Values (:Model,:StudyAreaName,:SubArea,:Scenario,:FileGroupID,:SwitchDefID,:FileIdentifier,:FileName)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileReservoirImplementationDatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileReservoirImplementationDatabaseAgent.ReadModelDataFromDatabase';
var
  LTemp,
  LMessage       : string;
  LDataSet       : TAbstractModelDataset;
  LSubDataSet    : TAbstractModelDataset;
  LStop          : boolean;

  LFileName                 : TString;
  LReservoirImplementation  : TReservoirImplementationObject;
  LShortTermFamilyGroup     : TShortTermFamilyGroupObject;
  LChannelSwitchFile        : TChannelSwitchFileObject;
  LHydroPowerAllocFile      : THydroPowerAllocFileObject;
  LPlanningFileDataObject   : TPlanningFileDataObjects;
  LReservoirAndFilesImplementation : TReservoirAndFilesImplementationObject;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileReservoirImplementationDatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LReservoirAndFilesImplementation    := LPlanningFileDataObject.ReservoirAndFilesImplementation;
    if not LReservoirAndFilesImplementation.Initialise then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LSubDataSet);
    try

      //line 2 +++++++++++++++++++++++++
      LDataSet.SetSQL(ReadReservoirTimeControlSQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFileReservoirImplementationDatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
        //Exit;
      end;

      while not LDataSet.DataSet.Eof do
      begin
        LReservoirImplementation := LReservoirAndFilesImplementation.AddReservoirImplementationObject;

        if not LDataSet.DataSet.FieldByName('ReservoirNumber').IsNull then
        begin
          LReservoirImplementation.ReservoirNumber.FData := LDataSet.DataSet.FieldByName('ReservoirNumber').AsInteger;
          LReservoirImplementation.ReservoirNumber.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('BaseNodeNumber').IsNull then
        begin
          LReservoirImplementation.ReplacedReservoirNumber.FData := LDataSet.DataSet.FieldByName('BaseNodeNumber').AsInteger;
          LReservoirImplementation.ReplacedReservoirNumber.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('ReservoirStartYear').IsNull then
        begin
          LReservoirImplementation.YearDamActive.FData := LDataSet.DataSet.FieldByName('ReservoirStartYear').AsInteger;
          LReservoirImplementation.YearDamActive.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('ReservoirStartMonth').IsNull then
        begin
          LReservoirImplementation.MonthDamActive.FData := LDataSet.DataSet.FieldByName('ReservoirStartMonth').AsInteger;
          LReservoirImplementation.MonthDamActive.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('ReservoirEndYear').IsNull then
        begin
          LReservoirImplementation.YearDamAbsolete.FData := LDataSet.DataSet.FieldByName('ReservoirEndYear').AsInteger;
          LReservoirImplementation.YearDamAbsolete.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('ReservoirEndMonth').IsNull then
        begin
          LReservoirImplementation.MonthDamAbsolete.FData := LDataSet.DataSet.FieldByName('ReservoirEndMonth').AsInteger;
          LReservoirImplementation.MonthDamAbsolete.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('ReservoirEconomicLife').IsNull then
        begin
          LReservoirImplementation.EconomicLifeOfDam.FData := LDataSet.DataSet.FieldByName('ReservoirEconomicLife').AsInteger;
          LReservoirImplementation.EconomicLifeOfDam.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('ReservoirCapitalCost').IsNull then
        begin
          LReservoirImplementation.CapitalCost.FData := LDataSet.DataSet.FieldByName('ReservoirCapitalCost').AsFloat;
          LReservoirImplementation.CapitalCost.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('ReservoirOMCost').IsNull then
        begin
          LReservoirImplementation.MaintenanceCost.FData := LDataSet.DataSet.FieldByName('ReservoirOMCost').AsFloat;
          LReservoirImplementation.MaintenanceCost.FInitalised := True;
        end;

        LTemp := '';
        if not LDataSet.DataSet.FieldByName('ReservoirCostSchedule').IsNull then
        begin
          LTemp := Trim(LDataSet.DataSet.FieldByName('ReservoirCostSchedule').AsString);
          LReservoirImplementation.CostSchedule.FData := StringReplace(LTemp,',',' ',[rfReplaceAll, rfIgnoreCase]);
          LReservoirImplementation.CostSchedule.FLength := Length(LReservoirImplementation.CostSchedule.FData);
          LReservoirImplementation.CostSchedule.FInitalised := True;
        end;
        LReservoirImplementation.YearsInConstruction.FData := StringsItemsCount(LTemp);
        LReservoirImplementation.YearsInConstruction.FInitalised := True;
        LDataSet.DataSet.Next;
      end;

      //line 5a +++++++++++++++++++++++++
      LDataSet.DataSet.Active := False;
      LDataSet.SetSQL(ReadReservoirSwitchDefinitionSQL(fgFamily));
      LDataSet.DataSet.Open;
      while not LDataSet.DataSet.Eof do
      begin
        LShortTermFamilyGroup := LReservoirAndFilesImplementation.AddShortTermFamilyGroupObject;

        if not LDataSet.DataSet.FieldByName('StartYear').IsNull then
        begin
          LShortTermFamilyGroup.YearFileActive.FData := LDataSet.DataSet.FieldByName('StartYear').AsInteger;
          LShortTermFamilyGroup.YearFileActive.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('StartMonth').IsNull then
        begin
          LShortTermFamilyGroup.MonthFileActive.FData := LDataSet.DataSet.FieldByName('StartMonth').AsInteger;
          LShortTermFamilyGroup.MonthFileActive.FInitalised := True;
        end;

        //line 5b +++++++++++++++++++++++++
        LSubDataSet.DataSet.Active := False;
        LSubDataSet.SetSQL(ReadReservoirSwitchFileNameSQL(fgFamily,LDataSet.DataSet.FieldByName('SwitchDefID').AsInteger));
        LSubDataSet.DataSet.Open;
        while not LSubDataSet.DataSet.Eof do
        begin
          LFileName := LShortTermFamilyGroup.CreateShortTermFamilyFileObject;

          if not LSubDataSet.DataSet.FieldByName('FileName').IsNull then
          begin
            LFileName.FData := Trim(LSubDataSet.DataSet.FieldByName('FileName').AsString);
            LFileName.FLength := Length(LFileName.FData);
            LFileName.FInitalised := True;
          end;
          LSubDataSet.DataSet.Next;
        end;
        LDataSet.DataSet.Next;
      end;

      //line 7 +++++++++++++++++++++++++
      LDataSet.DataSet.Active := False;
      LDataSet.SetSQL(ReadReservoirSwitchDefinitionSQL(fgSwitch));
      LDataSet.DataSet.Open;
      while not LDataSet.DataSet.Eof do
      begin
        LChannelSwitchFile := LReservoirAndFilesImplementation.AddChannelSwitchFileObject;

        if not LDataSet.DataSet.FieldByName('StartYear').IsNull then
        begin
          LChannelSwitchFile.YearFileActive.FData := LDataSet.DataSet.FieldByName('StartYear').AsInteger;
          LChannelSwitchFile.YearFileActive.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('StartMonth').IsNull then
        begin
          LChannelSwitchFile.MonthFileActive.FData := LDataSet.DataSet.FieldByName('StartMonth').AsInteger;
          LChannelSwitchFile.MonthFileActive.FInitalised := True;
        end;

        LSubDataSet.DataSet.Active := False;
        LSubDataSet.SetSQL(ReadReservoirSwitchFileNameSQL(fgSwitch,LDataSet.DataSet.FieldByName('SwitchDefID').AsInteger));
        LSubDataSet.DataSet.Open;

        if not LSubDataSet.DataSet.FieldByName('FileName').IsNull then
        begin
          LChannelSwitchFile.FileName.FData := Trim(LSubDataSet.DataSet.FieldByName('FileName').AsString);
          LChannelSwitchFile.FileName.FLength := Length(LChannelSwitchFile.FileName.FData);
          LChannelSwitchFile.FileName.FInitalised := True;
        end;
        LDataSet.DataSet.Next;
      end;

      //line 9 +++++++++++++++++++++++++
      LDataSet.DataSet.Active := False;
      LDataSet.SetSQL(ReadReservoirSwitchDefinitionSQL(fgHydroPower));
      LDataSet.DataSet.Open;
      while not LDataSet.DataSet.Eof do
      begin
        LHydroPowerAllocFile := LReservoirAndFilesImplementation.AddHydroPowerAllocFileObject;

        if not LDataSet.DataSet.FieldByName('StartYear').IsNull then
        begin
          LHydroPowerAllocFile.YearFileActive.FData := LDataSet.DataSet.FieldByName('StartYear').AsInteger;
          LHydroPowerAllocFile.YearFileActive.FInitalised := True;
        end;

        LSubDataSet.DataSet.Active := False;
        LSubDataSet.SetSQL(ReadReservoirSwitchFileNameSQL(fgHydroPower,LDataSet.DataSet.FieldByName('SwitchDefID').AsInteger));
        LSubDataSet.DataSet.Open;

        if not LSubDataSet.DataSet.FieldByName('FileName').IsNull then
        begin
          LHydroPowerAllocFile.FileName.FData := Trim(LSubDataSet.DataSet.FieldByName('FileName').AsString);
          LHydroPowerAllocFile.FileName.FLength := Length(LHydroPowerAllocFile.FileName.FData);
          LHydroPowerAllocFile.FileName.FInitalised := True;
        end;
        LDataSet.DataSet.Next;
      end;


      //line 10 +++++++++++++++++++++++++
      if(ADataObject.FRunParametersObject.FAllocationControlOption.FData = 'I') then
      begin
        LSubDataSet.DataSet.Active := False;
        LSubDataSet.SetSQL(ReadReservoirSwitchFileNameSQL(fgCur,2));
        LSubDataSet.DataSet.Open;

        if not LSubDataSet.DataSet.FieldByName('FileName').IsNull then
        begin
          LReservoirAndFilesImplementation.AllocationControlChannelFileName.FData := Trim(LSubDataSet.DataSet.FieldByName('FileName').AsString);
          LReservoirAndFilesImplementation.AllocationControlChannelFileName.FLength := Length(LReservoirAndFilesImplementation.AllocationControlChannelFileName.FData);
          LReservoirAndFilesImplementation.AllocationControlChannelFileName.FInitalised := True;
        end;
        LDataSet.DataSet.Active := False;
      end
      else
      if(ADataObject.FRunParametersObject.FAllocationControlOption.FData = 'Y') then
      begin
        LSubDataSet.DataSet.Active := False;
        LSubDataSet.SetSQL(ReadReservoirSwitchFileNameSQL(fgAllocation,1));
        LSubDataSet.DataSet.Open;

        if not LSubDataSet.DataSet.FieldByName('FileName').IsNull then
        begin
          LReservoirAndFilesImplementation.AllocationControlChannelFileName.FData := Trim(LSubDataSet.DataSet.FieldByName('FileName').AsString);
          LReservoirAndFilesImplementation.AllocationControlChannelFileName.FLength := Length(LReservoirAndFilesImplementation.AllocationControlChannelFileName.FData);
          LReservoirAndFilesImplementation.AllocationControlChannelFileName.FInitalised := True;
        end;
        LDataSet.DataSet.Active := False;
      end;


      //line 11 on wards+++++++++++++++++++++++++++
      LDataSet.DataSet.Active := False;
      LDataSet.SetSQL(ReadImplementationUnkownDataSQL((AFilename.FileNumber)));
      LDataSet.DataSet.Open;

      while not LDataSet.DataSet.Eof do
      begin
        LReservoirAndFilesImplementation.FMExtraLines.Add(Trim(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close ;
      LDataSet.DataSet.Close;

      LMessage := FAppModules.Language.GetString('TFileReservoirImplementationDatabaseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  True;
    finally
      LDataSet.Free;
      LSubDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileReservoirImplementationDatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileReservoirImplementationDatabaseAgent.WriteModelDataToDatabase';
var
  LWhereClause,
  LMessage       : string;
  LDataSet       : TAbstractModelDataset;
  LStop          : boolean;
  LIndex         : integer;
  LIndex2        : integer;

  LFileName                 : TString;
  LReservoirImplementation  : TReservoirImplementationObject;
  LShortTermFamilyGroup     : TShortTermFamilyGroupObject;
  LChannelSwitchFile        : TChannelSwitchFileObject;
  LHydroPowerAllocFile      : THydroPowerAllocFileObject;
  LPlanningFileDataObject   : TPlanningFileDataObjects;
  LReservoirAndFilesImplementation : TReservoirAndFilesImplementationObject;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileReservoirImplementationDatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not ClearModelDataInDatabase(TFileNameObject(AFileName),AProgressFunction,True) then
      Exit;

    LPlanningFileDataObject          := TPlanningFileDataObjects(ADataObject);
    LReservoirAndFilesImplementation := LPlanningFileDataObject.ReservoirAndFilesImplementation;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      //line 2 ++++++++++++++++++++++++++++
      for LIndex := 0 to LReservoirAndFilesImplementation.ReservoirImplementationObjectCount -1 do
      begin
        LReservoirImplementation := LReservoirAndFilesImplementation.ReservoirImplementationObjectByIndex[LIndex];

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteReservoirTimeControlSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(
          ['Model','StudyAreaName','SubArea','Scenario','Identifier'],
          [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode,IntToStr(LIndex+1)]);

        if LReservoirImplementation.ReservoirNumber.FInitalised then
          LDataSet.SetParams(['ReservoirNumber'], [IntToStr(LReservoirImplementation.ReservoirNumber.FData)]);

        if LReservoirImplementation.ReplacedReservoirNumber.FInitalised then
          LDataSet.SetParams(['BaseNodeNumber'], [IntToStr(LReservoirImplementation.ReplacedReservoirNumber.FData)]);

        if LReservoirImplementation.YearDamActive.FInitalised then
          LDataSet.SetParams(['ReservoirStartYear'], [IntToStr(LReservoirImplementation.YearDamActive.FData)]);

        if LReservoirImplementation.MonthDamActive.FInitalised then
          LDataSet.SetParams(['ReservoirStartMonth'], [IntToStr(LReservoirImplementation.MonthDamActive.FData)]);

        if LReservoirImplementation.YearDamAbsolete.FInitalised then
          LDataSet.SetParams(['ReservoirEndYear'], [IntToStr(LReservoirImplementation.YearDamAbsolete.FData)]);

        if LReservoirImplementation.MonthDamAbsolete.FInitalised then
          LDataSet.SetParams(['ReservoirEndMonth'], [IntToStr(LReservoirImplementation.MonthDamAbsolete.FData)]);

        if LReservoirImplementation.EconomicLifeOfDam.FInitalised then
          LDataSet.SetParams(['ReservoirEconomicLife'], [IntToStr(LReservoirImplementation.EconomicLifeOfDam.FData)]);

        if LReservoirImplementation.CapitalCost.FInitalised then
          LDataSet.SetParams(['ReservoirCapitalCost'], [FloatToStr(LReservoirImplementation.CapitalCost.FData)]);

        //line 3 ++++++++++++++++++++++++++++
        if LReservoirImplementation.MaintenanceCost.FInitalised then
          LDataSet.SetParams(['ReservoirOMCost'], [FloatToStr(LReservoirImplementation.MaintenanceCost.FData)]);

        LDataSet.ExecSQL;
        if LReservoirImplementation.CostSchedule.FInitalised then
        begin
          LWhereClause := ' WHERE Model         =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
                          ' AND StudyAreaName   =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
                          ' AND SubArea         =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
                          ' AND Scenario        =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
                          ' AND Identifier      =  '+IntToStr(LIndex+1);
           FAppModules.Database.UpdateMemoField('ReservoirTimeControl','ReservoirCostSchedule',LWhereClause,
                                                 LReservoirImplementation.CostSchedule.FData);
        end;
      end;


      //line 5a ++++++++++++++++++++++++++++
      for LIndex := 0 to LReservoirAndFilesImplementation.ShortTermFamilyGroupObjectCount -1 do
      begin
        LShortTermFamilyGroup := LReservoirAndFilesImplementation.ShortTermFamilyGroupObjectByIndex[LIndex];

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteReservoirSwitchDefinitionSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(
          ['Model','StudyAreaName','SubArea','Scenario'],
          [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode]);

        LDataSet.SetParams(['FileGroupID'], [IntToStr(fgFamily)]);
        LDataSet.SetParams(['SwitchDefID'], [IntToStr(LIndex+1)]);

        if LShortTermFamilyGroup.YearFileActive.FInitalised then
          LDataSet.SetParams(['StartYear'], [IntToStr(LShortTermFamilyGroup.YearFileActive.FData)]);

        if LShortTermFamilyGroup.MonthFileActive.FInitalised then
          LDataSet.SetParams(['StartMonth'], [IntToStr(LShortTermFamilyGroup.MonthFileActive.FData)]);
        LDataSet.ExecSQL;

        //line 5b ++++++++++++++++++++++++++++
        for LIndex2 := 0 to LShortTermFamilyGroup.ShortTermFamilyFileObjectCount-1 do
        begin
          LFileName :=  LShortTermFamilyGroup.ShortTermFamilyFileObjectByIndex[LIndex2];
          LDataSet.DataSet.Close;
          LDataSet.SetSQL(WriteReservoirSwitchFileNameSQL);  
          LDataset.ClearQueryParams();
          LDataSet.SetParams(
            ['Model','StudyAreaName','SubArea','Scenario'],
            [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
             FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode]);

          LDataSet.SetParams(['FileGroupID'], [IntToStr(fgFamily)]);
          LDataSet.SetParams(['SwitchDefID'], [IntToStr(LIndex+1)]);
          LDataSet.SetParams(['FileIdentifier'], [IntToStr(LIndex2+1)]);

          if LFileName.FInitalised then
            LDataSet.SetParams(['FileName'], [LFileName.FData]);
          LDataSet.ExecSQL;
        end;
      end;

      //line 7 ++++++++++++++++++++++++++++
      for LIndex := 0 to LReservoirAndFilesImplementation.ChannelSwitchFileObjectCount -1 do
      begin
        LChannelSwitchFile := LReservoirAndFilesImplementation.ChannelSwitchFileObjectByIndex[LIndex];

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteReservoirSwitchDefinitionSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(
          ['Model','StudyAreaName','SubArea','Scenario'],
          [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode]);

        LDataSet.SetParams(['FileGroupID'], [IntToStr(fgSwitch)]);
        LDataSet.SetParams(['SwitchDefID'], [IntToStr(LIndex+1)]);

        if LChannelSwitchFile.YearFileActive.FInitalised then
          LDataSet.SetParams(['StartYear'], [IntToStr(LChannelSwitchFile.YearFileActive.FData)]);

        if LChannelSwitchFile.MonthFileActive.FInitalised then
          LDataSet.SetParams(['StartMonth'], [IntToStr(LChannelSwitchFile.MonthFileActive.FData)]);
        LDataSet.ExecSQL;

        LFileName :=  LChannelSwitchFile.FileName;
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteReservoirSwitchFileNameSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(
          ['Model','StudyAreaName','SubArea','Scenario'],
          [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode]);

        LDataSet.SetParams(['FileGroupID'], [IntToStr(fgSwitch)]);
        LDataSet.SetParams(['SwitchDefID'], [IntToStr(LIndex+1)]);
        LDataSet.SetParams(['FileIdentifier'], ['1']);

        if LFileName.FInitalised then
          LDataSet.SetParams(['FileName'], [LFileName.FData]);
        LDataSet.ExecSQL;
      end;

      //line 9 ++++++++++++++++++++++++++++
      for LIndex := 0 to LReservoirAndFilesImplementation.HydroPowerAllocFileObjectCount -1 do
      begin
        LHydroPowerAllocFile := LReservoirAndFilesImplementation.HydroPowerAllocFileObjectByIndex[LIndex];

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteReservoirSwitchDefinitionSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(
          ['Model','StudyAreaName','SubArea','Scenario'],
          [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode]);

        LDataSet.SetParams(['FileGroupID'], [IntToStr(fgHydroPower)]);
        LDataSet.SetParams(['SwitchDefID'], [IntToStr(LIndex+1)]);

        if LHydroPowerAllocFile.YearFileActive.FInitalised then
          LDataSet.SetParams(['StartYear'], [IntToStr(LHydroPowerAllocFile.YearFileActive.FData)]);

        LDataSet.ExecSQL;

        LFileName :=  LHydroPowerAllocFile.FileName;
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteReservoirSwitchFileNameSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(
          ['Model','StudyAreaName','SubArea','Scenario'],
          [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode]);

        LDataSet.SetParams(['FileGroupID'], [IntToStr(fgHydroPower)]);
        LDataSet.SetParams(['SwitchDefID'], [IntToStr(LIndex+1)]);
        LDataSet.SetParams(['FileIdentifier'], ['1']);

        if LFileName.FInitalised then
          LDataSet.SetParams(['FileName'], [LFileName.FData]);
        LDataSet.ExecSQL;
      end;

      //line 10 ++++++++++++++++++++++++++++

      if(ADataObject.FRunParametersObject.FAllocationControlOption.FData = 'I') then
      begin
        if LReservoirAndFilesImplementation.AllocationControlChannelFileName.FInitalised  then
        begin
          LDataSet.DataSet.Close;
          LDataSet.SetSQL(WriteReservoirSwitchFileNameSQL);
          LDataset.ClearQueryParams();
          LDataSet.SetParams(
            ['Model','StudyAreaName','SubArea','Scenario'],
            [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
             FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode]);

          LDataSet.SetParams(['FileGroupID'], [IntToStr(fgCur)]);
          LDataSet.SetParams(['SwitchDefID'], ['2']);
          LDataSet.SetParams(['FileIdentifier'], ['2']);

          LDataSet.SetParams(['FileName'], [LReservoirAndFilesImplementation.AllocationControlChannelFileName.FData]);
          LDataSet.ExecSQL;
        end;
      end
      else
      if(ADataObject.FRunParametersObject.FAllocationControlOption.FData = 'Y') then
      begin

        if LReservoirAndFilesImplementation.AllocationControlChannelFileName.FInitalised  then
        begin
          LDataSet.DataSet.Close;
          LDataSet.SetSQL(WriteReservoirSwitchFileNameSQL);
          LDataset.ClearQueryParams();
          LDataSet.SetParams(
            ['Model','StudyAreaName','SubArea','Scenario'],
            [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
             FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode]);

          LDataSet.SetParams(['FileGroupID'], [IntToStr(fgAllocation)]);
          LDataSet.SetParams(['SwitchDefID'], ['1']);
          LDataSet.SetParams(['FileIdentifier'], ['1']);

          LDataSet.SetParams(['FileName'], [LReservoirAndFilesImplementation.AllocationControlChannelFileName.FData]);
          LDataSet.ExecSQL;
        end;
      end;

       //line11 onwards++++++++++++++++++++++++++++
      for LIndex := 0 to LReservoirAndFilesImplementation.FMExtraLines.Count - 1 do
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteImplementationUnkownDataSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
        LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
        LDataSet.SetParams(['LineNumber'], [IntToStr(1+LIndex)]);
        LDataSet.SetParams(['LineSection'], [IntToStr(0)]);
        LDataSet.SetParams(['LineData'], [LReservoirAndFilesImplementation.FMExtraLines[LIndex]]);

        LDataSet.ExecSQL;
      end;
      LDataSet.DataSet.Close;

      Result := InsertFileName(TFileNameObject(AFileName));
      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TFileReservoirImplementationDatabaseAgent.strWriteEnded');
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    finally
      LDataSet.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileReservoirImplementationDatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TFileReservoirImplementationDatabaseAgent.ClearModelDataInDatabase';
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
    LTableNames := 'ReservoirTimeControl,ReservoirSwitchDefinition,ReservoirSwitchFileName';
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
