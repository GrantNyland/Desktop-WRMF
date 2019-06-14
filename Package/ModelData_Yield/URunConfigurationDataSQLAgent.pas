//
//
//  UNIT      : Contains TConfigurationsDataSQLAgent Class
//  AUTHOR    : Titi Ngubane (Arivia)
//  DATE      : 2003/07/10
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit URunConfigurationDataSQLAgent;

interface

uses
  Classes,
  UAbstractObject;

type
  TRunConfigurationDataSQLAgent = class(TAbstractSQLAgent)
  protected
    function GetScenarioWhereClause: string;
    function GetMaxFileIdentifier : integer;
    function InsertHydrologyDataSQL: string;
    function WriteReservoirSwitchFileNameSQL: string;
    function ReadReservoirSwitchFileNameSQL(AFileName : string): string;
    function InsertHydrologyData(ADataSet: TAbstractModelDataset;AFileName:string;AIdentifier : integer): boolean;
  public

    function GetMonthsNamesSQL: string;
    function GetMonthsDaysSQL: string;
    function GetConfigurationSQL: string;
    function GetStochasticSequenceSQL: string;
    function GetRunTitleSQL: string;
    function GetDecisionDateSQL: string;
    function GetDataFilePathSQL: string;
    function GetResevoirHydroUnitsCodeSQL: string;
    function InsertDamImplimentationFile(AFileName,AAllocationControlOption : string) : boolean;

    function GetTargetYieldSQL: string;
    function GetMaximumYieldSQL: string;
    function GetTargetPowerSQL: string;
    procedure LoadMonthsDataContextData(AContextData: TStringList; AFieldNameIdentifier: string);
    procedure LoadPeriodDataContextData(AContextData: TStringList);
    function GetMinMaxYearFromHydrologyDB(AFileName: string; var AStartYear, AEndYear: integer): boolean;
    function GetMinMaxYearFromDemandDB(AFileNumber: integer; var AStartYear, AEndYear: integer): boolean;
    function InsertNrOfDecisionDateSQL(ANrOfDecisionDate: integer) : boolean;
    function GetInflowFileRecordCount(AFileName: string): integer;
    function SetInflowFileToStochastic(AFileName: string;AFileDate : TDateTime;ARunType:string): boolean;
  end;

implementation

uses
  SysUtils,
  UConstants,
  UDataSetType,
  UStringDateTimeOperations,
  UErrorHandlingOperations;

function TRunConfigurationDataSQLAgent.GetScenarioWhereClause: string;
const OPNAME = 'TRunConfigurationDataSQLAgent.GetScenarioWhereClause';
begin
  Result := '';
  try
    Result :=
      ' (A.Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
      ' (A.StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      ' (A.SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      ' (A.Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunConfigurationDataSQLAgent.GetMaxFileIdentifier : integer;
const OPNAME = 'TRunConfigurationDataSQLAgent.GetMonthsDaysSQL';
var
  LDataSet: TAbstractModelDataset;
  LSQL: string;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL := 'SELECT MAX(Identifier) as MaxID'+
                ' FROM FileNames A WHERE '+
                GetScenarioWhereClause;
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(LSQL);
        LDataset.DataSet.Open;
        Result :=  LDataset.DataSet.fieldByName('MaxID').AsInteger+1;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunConfigurationDataSQLAgent.GetMonthsDaysSQL: string;
const OPNAME = 'TRunConfigurationDataSQLAgent.GetMonthsDaysSQL';
begin
  Result := '';
  try
    Result := 'SELECT ' +
              ' Days1, Days2, Days3, Days4, Days5, Days6, Days7, Days8, ' +
              ' Days9, Days10, Days11, Days12 FROM DaysPerMonth A WHERE ' +
        GetScenarioWhereClause ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunConfigurationDataSQLAgent.GetMonthsNamesSQL: string;
const OPNAME = 'TRunConfigurationDataSQLAgent.GetMonthsNamesSQL';
begin
  Result := '';
  try
    Result := 'SELECT ' +
              ' Month1, Month2, Month3, Month4, Month5, Month6, Month7, Month8, ' +
              ' Month9, Month10, Month11, Month12 FROM MonthNames A WHERE ' +
        GetScenarioWhereClause ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunConfigurationDataSQLAgent.GetConfigurationSQL: string;
const OPNAME = 'TRunConfigurationDataSQLAgent.GetConfigurationSQL';
begin
  Result := '';
  try
    Result := 'SELECT ' +
              ' NumPeriods,StartYearG,StartYearO,DebugInit,DebugFinal,DebugLevel,'+
              ' SummaryLevel,SummaryOut,StoreYield,RandomOpt,PlotOpt,LimitOpt,MultPeriodOpt,'+
              ' CalcHistoryOpt,TargetRecurrenceInterval,ReduceSeqOpt,YearsCount,HydroSeqCount,'+
              ' LoadCasesCount,StartMonthNo,RunType,StartType,ParamFile,DetailedOption,SupplyOption,'+
              ' AnnualSummary,EconomicOption,PlanningSummary,InputSummary,WaterQualityOption,'+
              ' PeriodsPerYear,CalendarStartMonth,ShortTermPlanningOption,HydroPowerOption,'+
              ' AllocationControlOption' +
              ' FROM RunParameters A WHERE ' +
        GetScenarioWhereClause ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunConfigurationDataSQLAgent.GetStochasticSequenceSQL: string;
const OPNAME = 'TRunConfigurationDataSQLAgent.GetStochasticSequenceSQL';
begin
  Result := '';
  try
    Result := 'SELECT ' +
              ' Seq1,Seq2,Seq3,Seq4,Seq5,Seq6,Seq7,Seq8,Seq9,Seq10 FROM AnlySequences A WHERE ' +
        GetScenarioWhereClause ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunConfigurationDataSQLAgent.GetMaximumYieldSQL: string;
const OPNAME = 'TRunConfigurationDataSQLAgent.GetMaximumYieldSQL';
begin
  Result := '';
  try
    Result := 'SELECT ' +
      ' MYield1,MYield2,MYield3,MYield4,MYield5,MYield6,MYield7,MYield8,MYield9,MYield10'+
      ' FROM MaxYield A WHERE '+
        GetScenarioWhereClause ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunConfigurationDataSQLAgent.GetTargetPowerSQL: string;
const OPNAME = 'TRunConfigurationDataSQLAgent.GetTargetPowerSQL';
begin
  Result := '';
  try
    Result := 'SELECT ' +
      ' TPower1,TPower2,TPower3,TPower4,TPower5,TPower6,TPower7,TPower8,TPower9,TPower10'+
      ' FROM TargetPower A WHERE '+
      GetScenarioWhereClause ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunConfigurationDataSQLAgent.GetTargetYieldSQL: string;
const OPNAME = 'TRunConfigurationDataSQLAgent.GetTargetYieldSQL';
begin
  Result := '';
  try
    Result := 'SELECT ' +
      ' TYield1,TYield2,TYield3,TYield4,TYield5,TYield6,TYield7,TYield8,TYield9,TYield10'+
      ' FROM TargetYield A WHERE '+
      GetScenarioWhereClause ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationDataSQLAgent.LoadMonthsDataContextData(AContextData: TStringList; AFieldNameIdentifier: string);
const OPNAME = 'TRunConfigurationDataSQLAgent.LoadMonthsDataContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('FieldNameIdentifier=' + AFieldNameIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRunConfigurationDataSQLAgent.LoadPeriodDataContextData(AContextData: TStringList);
const OPNAME = 'TRunConfigurationDataSQLAgent.LoadPeriodDataContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TRunConfigurationDataSQLAgent.GetRunTitleSQL: string;
const OPNAME = 'TRunConfigurationDataSQLAgent.GetRunTitleSQL';
begin
  Result := '';
  try
    Result := 'SELECT ' +
      ' Title1,Title2,Title3'+
      ' FROM RunTitle A WHERE '+
      GetScenarioWhereClause ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunConfigurationDataSQLAgent.GetDecisionDateSQL: string;
const OPNAME = 'TRunConfigurationDataSQLAgent.GetDecisionDateSQL';
begin
  Result := '';
  try
    Result := 'SELECT ' +
      ' NrOfDecisionDates,' +
      ' DecisionMonth01,DecisionMonth02,DecisionMonth03,DecisionMonth04,DecisionMonth05,DecisionMonth06,'+
      ' DecisionMonth07,DecisionMonth08,DecisionMonth09,DecisionMonth10,DecisionMonth11,DecisionMonth12,'+
      ' DecisionType01,DecisionType02,DecisionType03,DecisionType04,DecisionType05,DecisionType06,'+
      ' DecisionType07,DecisionType08,DecisionType09,DecisionType10,DecisionType11,DecisionType12,'+
      ' HydroPowerIndicator01,HydroPowerIndicator02,HydroPowerIndicator03,HydroPowerIndicator04,'+
      ' HydroPowerIndicator05,HydroPowerIndicator06,HydroPowerIndicator07,HydroPowerIndicator08,' +
      ' HydroPowerIndicator09,HydroPowerIndicator10,HydroPowerIndicator11,HydroPowerIndicator12'+
      ' FROM DecisionDate A WHERE '+
      GetScenarioWhereClause ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunConfigurationDataSQLAgent.GetDataFilePathSQL: string;
const OPNAME = 'TRunConfigurationDataSQLAgent.GetDataFilePathSQL';
begin
  Result := '';
  try
    Result := 'SELECT ' +
      ' FilePrefix, InputPath, OutputPath, HydrologyPath, SpecifiedDemandPath'+
      ' FROM WRYMDat A WHERE '+
      GetScenarioWhereClause ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TRunConfigurationDataSQLAgent.GetResevoirHydroUnitsCodeSQL: string;
const OPNAME = 'TRunConfigurationDataSQLAgent.GetResevoirHydroUnitsCodeSQL';
begin
  Result := '';
  try
    Result := 'SELECT ' +
              ' HydroUnitsCode'+
              ' FROM Reservoir A WHERE ' +
        GetScenarioWhereClause ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunConfigurationDataSQLAgent.GetMinMaxYearFromHydrologyDB(AFileName: string; var AStartYear, AEndYear: integer): boolean;
const OPNAME = 'TRunConfigurationDataSQLAgent.GetMinMaxYearFromHydrologyDB';
var
  LDataSet: TAbstractModelDataset;
  LSQL: string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        AFileName := UpperCase(ExtractFileName(Trim(AFileName)));
        LSQL := 'SELECT MIN(HydroYearValue) AS MinYear, MAX(HydroYearValue) as MaxYear'+
                ' FROM HydrologyFileData WHERE FileName = '+ QuotedStr(AFileName)+
                  ' AND StudyAreaName = '+QuotedStr(FAppModules.StudyArea.StudyAreaCode);
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(LSQL);
        LDataset.DataSet.Open;
        if not(LDataset.DataSet.Eof and LDataset.DataSet.Bof) then
        begin
          AStartYear := LDataset.DataSet.FieldByName('MinYear').AsInteger;
          AEndYear   := LDataset.DataSet.FieldByName('MaxYear').AsInteger;
          Result     := True;
        end;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunConfigurationDataSQLAgent.GetMinMaxYearFromDemandDB(AFileNumber: integer; var AStartYear, AEndYear: integer): boolean;
const OPNAME = 'TRunConfigurationDataSQLAgent.GetMinMaxYearFromDemandDB';
var
  LDataSet: TAbstractModelDataset;
  LSQL: string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL := 'SELECT MIN(DemandYearValue) AS MinYear, MAX(DemandYearValue) as MaxYear'+
                ' FROM DemandFileData A'+
              ' WHERE '+ GetScenarioWhereClause +
              ' AND (FileNumber = ' + IntToStr(AFileNumber) + ')';

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(LSQL);
        LDataset.DataSet.Open;
        if not(LDataset.DataSet.Eof and LDataset.DataSet.Bof) then
        begin
          AStartYear := LDataset.DataSet.FieldByName('MinYear').AsInteger;
          AEndYear   := LDataset.DataSet.FieldByName('MaxYear').AsInteger;
          Result     := True;
        end;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunConfigurationDataSQLAgent.InsertNrOfDecisionDateSQL(ANrOfDecisionDate: integer) : boolean;
const OPNAME = 'TRunConfigurationDataSQLAgent.InsertNrOfDecisionDateSQL';
var
  lDataSet    : TAbstractModelDataset;
  lImportDate : TDateTime;
  lSQL        : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lSQL:= 'INSERT INTO DecisionDate ' +
              '(Model, StudyAreaName, SubArea, Scenario,NrOfDecisionDates)' +
              ' VALUES (' +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
              QuotedStr(IntToStr(ANrOfDecisionDate)) +
              ')';

        lDataSet.SetSQL(lSQL);
        lDataset.ExecSQL;
        lDataset.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();

        lImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if (lImportDate = NullDateTime) then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := TRUE;
      end;
    finally
      lDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;



function TRunConfigurationDataSQLAgent.GetInflowFileRecordCount(AFileName: string): integer;
const OPNAME = 'TRunConfigurationDataSQLAgent.GetInflowFileRecordCount';
var
  LDataSet: TAbstractModelDataset;
  LSQL: string;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        AFileName := UpperCase(ExtractFileName(Trim(AFileName)));
        LSQL := 'SELECT COUNT(*)  as RowsCount'+
                ' FROM HydrologyFileData WHERE FileName = '+ QuotedStr(AFileName)+
                ' AND StudyAreaName = '+QuotedStr(FAppModules.StudyArea.StudyAreaCode);
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(LSQL);
        LDataset.DataSet.Open;
        if not(LDataset.DataSet.Eof and LDataset.DataSet.Bof) then
        begin
          Result := LDataset.DataSet.FieldByName('RowsCount').AsInteger;;
        end;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TRunConfigurationDataSQLAgent.WriteReservoirSwitchFileNameSQL: string;
const OPNAME = 'TFileReservoirImplementationDatabaseAgent.WriteReservoirSwitchDefinitionSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO ReservoirSwitchFileName'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileGroupID,SwitchDefID,FileIdentifier,FileName)'+
              ' Values (:Model,:StudyAreaName,:SubArea,:Scenario,:FileGroupID,:SwitchDefID,:FileIdentifier,:FileName)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunConfigurationDataSQLAgent.ReadReservoirSwitchFileNameSQL(AFileName : string): string;
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
              ' AND FileName      =  '+QuotedStr(AFileName)+
             // ' AND SwitchDefID   =  '+IntToStr(ASwitchDefID)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,FileGroupID,SwitchDefID,FileIdentifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunConfigurationDataSQLAgent.InsertHydrologyDataSQL: string;
const OPNAME = 'TRunConfigurationDataSQLAgent.InsertHydrologyDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO HydrologyFileData'+
              ' (StudyAreaName,FileName,Identifier,HydroYearValue,HydroYearValuePatch,HydroTotalValue'+
              ' ,HydroMonthValue01,HydroMonthValue02,HydroMonthValue03,HydroMonthValue04,HydroMonthValue05,HydroMonthValue06'+
              ' ,HydroMonthValue07,HydroMonthValue08,HydroMonthValue09,HydroMonthValue10,HydroMonthValue11,HydroMonthValue12'+
              ' ,HydroMonthValue01Patch,HydroMonthValue02Patch,HydroMonthValue03Patch,HydroMonthValue04Patch,HydroMonthValue05Patch,HydroMonthValue06Patch'+
              ' ,HydroMonthValue07Patch,HydroMonthValue08Patch,HydroMonthValue09Patch,HydroMonthValue10Patch,HydroMonthValue11Patch,HydroMonthValue12Patch'+
              ')'+
              ' Values'+
              ' (:StudyAreaName,:FileName,:Identifier,:HydroYearValue,:HydroYearValuePatch,:HydroTotalValue'+
              ' ,:HydroMonthValue01,:HydroMonthValue02,:HydroMonthValue03,:HydroMonthValue04,:HydroMonthValue05,:HydroMonthValue06'+
              ' ,:HydroMonthValue07,:HydroMonthValue08,:HydroMonthValue09,:HydroMonthValue10,:HydroMonthValue11,:HydroMonthValue12'+
              ' ,:HydroMonthValue01Patch,:HydroMonthValue02Patch,:HydroMonthValue03Patch,:HydroMonthValue04Patch,:HydroMonthValue05Patch,:HydroMonthValue06Patch'+
              ' ,:HydroMonthValue07Patch,:HydroMonthValue08Patch,:HydroMonthValue09Patch,:HydroMonthValue10Patch,:HydroMonthValue11Patch,:HydroMonthValue12Patch'+
              ')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunConfigurationDataSQLAgent.SetInflowFileToStochastic(AFileName: string;AFileDate : TDateTime;ARunType:string): boolean;
const OPNAME = 'TRunConfigurationDataSQLAgent.SetInflowFileToStochastic';
var
  LDataSet: TAbstractModelDataset;
  LPrefix,
  LSQL: string;
  LFileName : string;
  LResult : boolean;
  LCount : integer;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        AFileName := UpperCase(ExtractFileName(Trim(AFileName)));
        LFileName := AFileName;
        LPrefix := Trim(Copy(AFileName,Pos('.',AFileName)-1,1));
        if (UpperCase(LPrefix) = 'H') and (ARunType = 'S') then
          LFileName := Copy(AFileName,1,Pos('.',AFileName)-2)+'S'+ ExtractFileExt(Trim(AFileName))
        else
        if (UpperCase(LPrefix) = 'S') and (ARunType = 'H') then
          LFileName := Copy(AFileName,1,Pos('.',AFileName)-2)+'H'+ ExtractFileExt(Trim(AFileName));

        LSQL := ' SELECT COUNT(*)  as RowsCount FROM FileNames A '+
                'WHERE '+ GetScenarioWhereClause + ' AND '+
                ' (A.FileName = '+ QuotedStr(LFileName)+') ';
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(LSQL);
        LDataset.DataSet.Open;
        LResult := (LDataset.DataSet.FieldByName('RowsCount').AsInteger>0);
        if not LResult then
        begin
          LSQL := ' SELECT * '+
                  ' FROM HydrologyFileData WHERE FileName = '+ QuotedStr(LFileName)+
                  ' AND StudyAreaName = '+QuotedStr(FAppModules.StudyArea.StudyAreaCode);
          LDataSet.DataSet.Close;
          LDataSet.ClearSQL;
          LDataSet.SetSQL(LSQL);
          LDataset.DataSet.Open;
          LDataset.DataSet.Last;
          LDataset.DataSet.First;
          LResult := (LDataset.DataSet.RecordCount>0);
          if not LResult then
          begin
            LSQL := ' INSERT  INTO FileNames (Model,StudyAreaName,SubArea,Scenario, '+
                    ' Identifier,FileGroup,FileName,ImportDate,FileDate)'+
                    ' VALUES ( '+
                    QuotedStr(FAppModules.StudyArea.ModelCode)+','+
                    QuotedStr(FAppModules.StudyArea.StudyAreaCode)+','+
                    QuotedStr(FAppModules.StudyArea.SubAreaCode)+','+
                    QuotedStr(FAppModules.StudyArea.ScenarioCode)+','+
                    IntToStr(GetMaxFileIdentifier)+',5,'+
                    QuotedStr(LFileName)+','+
                    DateTimeToStamp(Now)+','+
                    DateTimeToStamp(AFileDate)+')';
            LDataSet.ClearQueryParams();
            LDataSet.ClearSQL;
            LDataSet.SetSQL(LSQL);
            LDataset.ExecSQL;

            LSQL := ' SELECT * '+
                    ' FROM HydrologyFileData WHERE FileName = '+ QuotedStr(AFileName)+
                    ' AND StudyAreaName = '+QuotedStr(FAppModules.StudyArea.StudyAreaCode);
            LDataSet.ClearSQL;
            LDataSet.SetSQL(LSQL);
            LDataset.DataSet.Open;
            LCount := 0;
            while not LDataset.DataSet.Eof do
            begin
              Inc(LCount);
              InsertHydrologyData(LDataset,LFileName,LCount);
              LDataset.DataSet.Next;
            end;
          end;
        end;

      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunConfigurationDataSQLAgent.InsertDamImplimentationFile(AFileName, AAllocationControlOption: string): boolean;
const OPNAME = 'TRunConfigurationDataSQLAgent.InsertHydrologyData';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try

      LDataSet.DataSet.Active := False;
      //if AAllocationControlOption = 'I' then
        LDataSet.SetSQL(ReadReservoirSwitchFileNameSQL(AFileName));
      //else
      //if AAllocationControlOption = 'Y' then
      //  LDataSet.SetSQL(ReadReservoirSwitchFileNameSQL(fgAllocation,1));
      LDataSet.DataSet.Open;

      if LDataSet.DataSet.FieldByName('FileName').IsNull then
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteReservoirSwitchFileNameSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(
          ['Model','StudyAreaName','SubArea','Scenario'],
          [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode]);

        if AAllocationControlOption = 'I' then
        begin
          LDataSet.SetParams(['FileGroupID'], [IntToStr(fgCur)]);
          LDataSet.SetParams(['SwitchDefID'], ['2']);
          LDataSet.SetParams(['FileIdentifier'], ['2']);

        end
        else
        if AAllocationControlOption = 'Y' then
        begin
          LDataSet.SetParams(['FileGroupID'], [IntToStr(fgAllocation)]);

          LDataSet.SetParams(['SwitchDefID'], ['1']);
          LDataSet.SetParams(['FileIdentifier'], ['1']);
        end;

        LDataSet.SetParams(['FileName'], [AFileName]);
        LDataSet.ExecSQL;
        LDataSet.DataSet.Close;
      end;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunConfigurationDataSQLAgent.InsertHydrologyData(ADataSet: TAbstractModelDataset;AFileName:string;AIdentifier : integer): boolean;
const OPNAME = 'TRunConfigurationDataSQLAgent.InsertHydrologyData';
var
  LFieldName : string;
  LCount: integer;
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(InsertHydrologyDataSQL);
      LDataSet.ClearQueryParams();
      LDataSet.SetParams(['StudyAreaName'], [UpperCase(FAppModules.StudyArea.StudyAreaCode)]);
      LDataSet.SetParams(['FileName'], [UpperCase(ExtractFileName(Trim(AFileName)))]);
      LDataSet.SetParams(['Identifier'], [IntToStr(AIdentifier)]);
      if not ADataSet.DataSet.FieldByName('HydroYearValue').IsNull then
        LDataSet.SetParams(['HydroYearValue'], [Trim(ADataSet.DataSet.FieldByName('HydroYearValue').AsString)]);
      if not ADataSet.DataSet.FieldByName('HydroYearValuePatch').IsNull then
        LDataSet.SetParams(['HydroYearValuePatch'], [Trim(ADataSet.DataSet.FieldByName('HydroYearValuePatch').AsString)]);
      if not ADataSet.DataSet.FieldByName('HydroTotalValue').IsNull then
        LDataSet.SetParams(['HydroTotalValue'], [Trim(ADataSet.DataSet.FieldByName('HydroTotalValue').AsString)]);

      for LCount := MinMonths to MaxMonths do
      begin
        LFieldName := Format('%s%2.2d',['HydroMonthValue',LCount]);
        if not ADataSet.DataSet.FieldByName(LFieldName).IsNull then
          LDataSet.SetParams([LFieldName], [ADataSet.DataSet.FieldByName(LFieldName).Value]);
        LFieldName := Format('HydroMonthValue%2.2dPatch',[LCount]);
        if not ADataSet.DataSet.FieldByName(LFieldName).IsNull then
          LDataSet.SetParams([LFieldName], [ADataSet.DataSet.FieldByName(LFieldName).Value]);
      end;
      LDataSet.ExecSQL;
      LDataSet.DataSet.Close;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.



