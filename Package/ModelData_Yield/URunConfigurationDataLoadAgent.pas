//
//
//  UNIT      : Contains TStudyConfigurationDataLoadAgent Class
//  AUTHOR    : Titi Ngubane (Arivia)
//  DATE      : 2003/07/10
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit URunConfigurationDataLoadAgent;

interface

uses
  Classes,
  URunConfigurationData,
  UDataFilePaths,
  UAbstractObject,
  URunConfigurationDataSQLAgent;

type
  TRunConfigurationDataLoadAgent = class(TAbstractAppObject)
  protected
    FSQLAgent: TRunConfigurationDataSQLAgent;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function LoadMonthsData(ARunConfigurationData: TRunConfigurationData): boolean;
    function LoadConfigurationData(ARunConfigurationData: TRunConfigurationData;ADataFilePaths: TDataFilePaths): boolean;
    function LoadStochasticSequenceData(ARunConfigurationData: TRunConfigurationData): boolean;
    function LoadSystemYieldAndPowerData(ARunConfigurationData: TRunConfigurationData): boolean;
    function LoadDecisionDatesData(ARunConfigurationData: TRunConfigurationData): boolean;
    function LoadDataFilePathsData(ADataFilePaths: TDataFilePaths; ARunConfigurationData: TRunConfigurationData): boolean;
  public
    function ConstructData(ARunConfigurationData: TRunConfigurationData;ADataFilePaths: TDataFilePaths): boolean;
    procedure LoadMonthsDataContextData(AContextData: TStringList; AFieldNameIdentifier: string);
    procedure LoadPeriodDataContextData(AContextData: TStringList);
  end;

implementation

uses
  SysUtils,
  UConstants,
  UDataSetType,
  UErrorHandlingOperations, DB;

function TRunConfigurationDataLoadAgent.ConstructData(ARunConfigurationData: TRunConfigurationData;
         ADataFilePaths: TDataFilePaths): boolean;
const OPNAME = 'TRunConfigurationDataLoadAgent.ConstructData';
begin
  Result := False;
  try
    ARunConfigurationData.Initialise;
    Result := LoadMonthsData(ARunConfigurationData) and
              LoadConfigurationData(ARunConfigurationData,ADataFilePaths) and
              LoadStochasticSequenceData(ARunConfigurationData) and
              LoadSystemYieldAndPowerData(ARunConfigurationData) and
              LoadDecisionDatesData(ARunConfigurationData) and
              LoadDataFilePathsData(ADataFilePaths,ARunConfigurationData);
  except on E : Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

procedure TRunConfigurationDataLoadAgent.CreateMemberObjects;
const OPNAME = 'TRunConfigurationDataLoadAgent.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FSQLAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRunConfigurationDataLoadAgent.DestroyMemberObjects;
const OPNAME = 'TRunConfigurationDataLoadAgent.DestroyMemberObjects';
begin
  try
    FreeAndNil(FSQLAgent);
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TRunConfigurationDataLoadAgent.LoadConfigurationData(ARunConfigurationData: TRunConfigurationData;
         ADataFilePaths: TDataFilePaths): boolean;
const OPNAME = 'TRunConfigurationDataLoadAgent.LoadConfigurationData';
var
  LStartYearGregorian          : integer;
  LStartYearOther              : integer;
  LYearsInAnalysis             : integer;
  LPeriodsInAnalysis           : integer;
  LStartMonthNumber            : integer;

  LCalculateHistoricFirmYield  : Integer;
  LTargetRecurrenceInterval    : integer;
  LLimitOption                 : boolean;
  LRunSequenceType             : string;

  LMultiplePeriodLengths       : boolean;
  LReduceSequences             : boolean;
  LNumberOfSequencesInAnalysis : integer;
  LGeneratedFlowFlag           : integer;

  LDebugLevel                  : integer;
  LStartDebugPeriod            : integer;
  LEndDebugPeriod              : integer;

  LRunTitle1                   : string;
  LRunTitle2                   : string;
  LRunTitle3                   : string;

  LHydroUnitsCode              : string;

  LOutputSummaryLevel          : integer;
  LCreateDataFile              : boolean;
  LCreateYieldFile             : boolean;
  LCreatePlotFile              : boolean;

  //LDataFilePaths               : TDataFilePaths;
  LParamFileName               : string;
  LRandomNumberOption          : integer;

  LDetailedOption              : boolean;
  LSupplyOption                : boolean;
  LAnnualSummary               : string;
  LEconomicOption              : boolean;
  LPlanningSummary             : boolean;
  LInputSummary                : boolean;
  LWaterQualityOption          : boolean;
  LPeriodsPerYear              : integer;
  LCalendarStartMonth          : integer;
  LShortTermPlanningOption     : string;
  LHydroPowerOption            : string;
  LAllocationControlOption     : string;

  LDataSet                     : TAbstractModelDataset;
begin
  Result := False;
  try

    {LCalculateHistoricFirmYield  := NullInteger;
    LLimitOption                 := False;
    LRunSequenceType             := '';

    LMultiplePeriodLengths       := False;
    LReduceSequences             := False;

    LRunTitle1                   := '';
    LRunTitle2                   := '';
    LRunTitle3                   := '';

    LHydroUnitsCode              := '';

    LOutputSummaryLevel          := NullInteger;
    LCreateDataFile              := False;
    LCreateYieldFile             := False;
    LCreatePlotFile              := False;

    LParamFileName               := '';
    LRandomNumberOption          := NullInteger;

    LDetailedOption              := False;
    LSupplyOption                := False;
    LAnnualSummary               := '';
    LEconomicOption              := False;
    LPlanningSummary             := False;
    LInputSummary                := False;
    LWaterQualityOption          := False;
    LShortTermPlanningOption     := '';
    LHydroPowerOption            := '';
    }
    // Loop for all the records in the table.
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetConfigurationSQL);
        LDataSet.DataSet.Open;
        if(LDataSet.DataSet.RecordCount > 0) then
        begin
          LPeriodsInAnalysis  := LDataSet.DataSet.FieldByName('NumPeriods').Asinteger;
          LStartYearGregorian := LDataSet.DataSet.FieldByName('StartYearG').Asinteger;
          LStartYearOther     := LDataSet.DataSet.FieldByName('StartYearO').Asinteger;
          LStartDebugPeriod   := LDataSet.DataSet.FieldByName('DebugInit').Asinteger;
          LEndDebugPeriod     := LDataSet.DataSet.FieldByName('DebugFinal').Asinteger;
          LDebugLevel         := LDataSet.DataSet.FieldByName('DebugLevel').Asinteger;
          //if(FAppModules.Model.ModelName = CYield) then
          //begin
            LOutputSummaryLevel          := LDataSet.DataSet.FieldByName('SummaryLevel').Asinteger;
            LCreateDataFile              := (Trim(LDataSet.DataSet.FieldByName('SummaryOut').AsString) = '1');
            LCreateYieldFile             := (Trim(LDataSet.DataSet.FieldByName('StoreYield').AsString) = '1');
            LRandomNumberOption          := StrToIntDef(Trim(LDataSet.DataSet.FieldByName('RandomOpt').AsString),0);
            LCreatePlotFile              := (Trim(LDataSet.DataSet.FieldByName('PlotOpt').AsString) = 'Y');
            LLimitOption                 := (Trim(LDataSet.DataSet.FieldByName('LimitOpt').AsString) = '1');
            LMultiplePeriodLengths       := (Trim(LDataSet.DataSet.FieldByName('MultPeriodOpt').AsString) = '1');
            LCalculateHistoricFirmYield  := LDataSet.DataSet.FieldByName('CalcHistoryOpt').AsInteger;
            LReduceSequences             := (Trim(LDataSet.DataSet.FieldByName('ReduceSeqOpt').AsString) = '1');
          //end
          //else
          //begin
            LDetailedOption     := (Trim(LDataSet.DataSet.FieldByName('DetailedOption').AsString) = '1');
            LSupplyOption       := (Trim(LDataSet.DataSet.FieldByName('SupplyOption').AsString) = '1');
            LAnnualSummary      := LDataSet.DataSet.FieldByName('AnnualSummary').AsString;
            LEconomicOption     := (Trim(LDataSet.DataSet.FieldByName('EconomicOption').AsString) = '1');
            LPlanningSummary    := (Trim(LDataSet.DataSet.FieldByName('PlanningSummary').AsString) = '1');
            LInputSummary       := (Trim(LDataSet.DataSet.FieldByName('InputSummary').AsString) = '1');
            LWaterQualityOption := (Trim(LDataSet.DataSet.FieldByName('WaterQualityOption').AsString) = 'Y');
          //end;

          LYearsInAnalysis             := LDataSet.DataSet.FieldByName('YearsCount').Asinteger;
          LNumberOfSequencesInAnalysis := LDataSet.DataSet.FieldByName('HydroSeqCount').Asinteger;
          LStartMonthNumber            := LDataSet.DataSet.FieldByName('StartMonthNo').Asinteger;
          LRunSequenceType             := Trim(LDataSet.DataSet.FieldByName('RunType').AsString);
          LGeneratedFlowFlag           := LDataSet.DataSet.FieldByName('StartType').Asinteger;
          LParamFileName               := Trim(LDataSet.DataSet.FieldByName('ParamFile').AsString);
          LPeriodsPerYear     := LDataSet.DataSet.FieldByName('PeriodsPerYear').Asinteger;
          LCalendarStartMonth := LDataSet.DataSet.FieldByName('CalendarStartMonth').Asinteger;
          LShortTermPlanningOption := Trim(LDataSet.DataSet.FieldByName('ShortTermPlanningOption').AsString);
          LHydroPowerOption := Trim(LDataSet.DataSet.FieldByName('HydroPowerOption').AsString);
          LAllocationControlOption := Trim(LDataSet.DataSet.FieldByName('AllocationControlOption').AsString);

          if LDataSet.DataSet.FieldbyName('TargetRecurrenceInterval').IsNull then
            LTargetRecurrenceInterval   := 0
          else
            LTargetRecurrenceInterval   := LDataSet.DataSet.FieldbyName('TargetRecurrenceInterval').Asinteger;

          ADataFilePaths.PopulateParamFile(LParamFileName);

          LDataSet.DataSet.Close;
          LDataSet.SetSQL(FSQLAgent.GetRunTitleSQL);
          LDataSet.DataSet.Open;
          if (LDataSet.DataSet.RecordCount > 0) then
            LRunTitle1  := Trim(LDataSet.DataSet.FieldByName('Title1').AsString);
            LRunTitle2  := Trim(LDataSet.DataSet.FieldByName('Title2').AsString);
            LRunTitle3  := Trim(LDataSet.DataSet.FieldByName('Title3').AsString);
          begin
          end;

          LDataSet.DataSet.Close;
          LDataSet.SetSQL(FSQLAgent.GetResevoirHydroUnitsCodeSQL);
          LDataSet.DataSet.Open;
          if (LDataSet.DataSet.RecordCount > 0) then
          begin
            LHydroUnitsCode  := Trim(LDataSet.DataSet.FieldByName('HydroUnitsCode').AsString);
          end;

          ARunConfigurationData.PopulatePeriodData(LStartYearGregorian,LStartYearOther,
                      LYearsInAnalysis,LPeriodsInAnalysis,LStartMonthNumber);

          ARunConfigurationData.PopulateRunInfoData(LCalculateHistoricFirmYield,
           LTargetRecurrenceInterval,LLimitOption,LRunSequenceType,LRunTitle1,LRunTitle2,LRunTitle3,
           LRandomNumberOption,LParamFileName);

          ARunConfigurationData.PopulateStochasticData(LMultiplePeriodLengths,LReduceSequences,
            LGeneratedFlowFlag,LNumberOfSequencesInAnalysis);

          ARunConfigurationData.PopulateDebugInfoData(LDebugLevel,LStartDebugPeriod,LEndDebugPeriod);

          ARunConfigurationData.PopulateOutputConfigurationData(LOutputSummaryLevel,
            LCreateDataFile,LCreateYieldFile,LCreatePlotFile);



          ARunConfigurationData.PopulateHydroUnitsCode(LHydroUnitsCode);

          ARunConfigurationData.PopulatePlanningConfigurationData(LDetailedOption,LSupplyOption,LAnnualSummary,
            LEconomicOption,LPlanningSummary,LInputSummary,LWaterQualityOption,LPeriodsPerYear,
            LCalendarStartMonth,LShortTermPlanningOption,LHydroPowerOption,LAllocationControlOption)
        end;
        Result :=  True;
      end;
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;

  // Handle exceptions.
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TRunConfigurationDataLoadAgent.LoadMonthsData(ARunConfigurationData: TRunConfigurationData): boolean;
const OPNAME = 'TRunConfigurationDataLoadAgent.LoadMonthsData';
var
  LIndex: integer;
  LFieldName: string;
  LDataSet: TAbstractModelDataset;
  LMonthNamesArray: TMonthNamesArray;
  LMonthsDaysArray: TMonthDaysArray;
  LMonthNamesField,
  LMonthDays: TAbstractFieldProperty;
begin
  Result := False;
  try
    LMonthNamesField := FAppModules.FieldProperties.FieldProperty('Month');
    if not Assigned(LMonthNamesField) then
      raise Exception.Create('Field (Month) not found in field properties');
    SetLength(LMonthNamesArray,LMonthNamesField.ArrayLength);

    LMonthDays := FAppModules.FieldProperties.FieldProperty('Days');
    if not Assigned(LMonthDays) then
      raise Exception.Create('Field (Days) not found in field properties');
    SetLength(LMonthsDaysArray,LMonthDays.ArrayLength);

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetMonthsNamesSQL);
        LDataSet.DataSet.Open;
        if (LDataSet.DataSet.RecordCount > 0) then
        begin
          for LIndex := 1 to 12 do
          begin
            LFieldName := Format('Month%d',[LIndex]);
            LMonthNamesArray[LIndex] := Trim(LDataSet.DataSet.FieldByName(LFieldName).AsString);
          end;

          LDataSet.DataSet.Close;
          LDataSet.SetSQL(FSQLAgent.GetMonthsDaysSQL);
          LDataSet.DataSet.Open;
          if (LDataSet.DataSet.RecordCount > 0) then
          begin
            for LIndex := 1 to 12 do
            begin
              LFieldName := Format('Days%d',[LIndex]);
              LMonthsDaysArray[LIndex] := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
            end;
            ARunConfigurationData.PopulateMonthsData(LMonthNamesArray,LMonthsDaysArray);
          end;
        end;
      end;
      Result := True;
    finally
      Finalize(LMonthNamesArray);
      Finalize(LMonthsDaysArray);
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;

  // Handle exceptions.
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TRunConfigurationDataLoadAgent.LoadStochasticSequenceData(ARunConfigurationData: TRunConfigurationData): boolean;
const OPNAME = 'TRunConfigurationDataLoadAgent.LoadStochasticSequenceData';
var
  LIndex: integer;
  LFieldName: string;
  LDataSet: TAbstractModelDataset;
  LSequencesToBeAnalysedArray: TSequencesToBeAnalysedArray;
  LHistoricSequenceStartYear: integer;
  LStartSequenceNumber: integer;
  LSequencesToBeAnalysedField: TAbstractFieldProperty;
begin
  Result := False;
  try
    LSequencesToBeAnalysedField := FAppModules.FieldProperties.FieldProperty('Month');
    if not Assigned(LSequencesToBeAnalysedField) then
      raise Exception.Create('Field (Month) not found in field properties');
    SetLength(LSequencesToBeAnalysedArray,LSequencesToBeAnalysedField.ArrayLength);

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetStochasticSequenceSQL);
        LDataSet.DataSet.Open;
        if(LDataSet.DataSet.RecordCount > 0) then
        begin
          for LIndex := 1 to 10 do
          begin
            LSequencesToBeAnalysedArray[LIndex] := 0;
            LFieldName := Format('Seq%d',[LIndex]);
            if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
              LSequencesToBeAnalysedArray[LIndex] := LDataSet.DataSet.FieldByName(LFieldName).AsInteger;
          end;
          LHistoricSequenceStartYear := ARunConfigurationData.StartYearOther;
          LStartSequenceNumber       := 1;
          if(ARunConfigurationData.RunSequenceType = 'H') then
            LHistoricSequenceStartYear := ARunConfigurationData.StartYearOther +
                                          LSequencesToBeAnalysedArray[1] -1;
          if(ARunConfigurationData.NumberOfSequencesInAnalysis > 10) then
            LStartSequenceNumber := LSequencesToBeAnalysedArray[1];

          ARunConfigurationData.PopulateSequencesToBeAnalysedArrayData(
              LHistoricSequenceStartYear,LStartSequenceNumber,LSequencesToBeAnalysedArray);
        end;
        Result := True;
      end;
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;

  // Handle exceptions.
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TRunConfigurationDataLoadAgent.LoadSystemYieldAndPowerData( ARunConfigurationData: TRunConfigurationData): boolean;
const OPNAME = 'TRunConfigurationDataLoadAgent.LoadSystemYieldAndPowerData';
var
  LIndex: integer;
  LFieldName: string;
  LLoadCasesCount: integer;
  LDataSet: TAbstractModelDataset;
  LTargetYieldArray  : TTargetYieldArray;
  LMaximumYieldArray : TMaximumYieldArray;
  LTargetPowerArray  : TTargetPowerArray;
  LTargetYieldField,
  LMaximumYieldField,
  LTargetPowerField: TAbstractFieldProperty;
begin
  Result := False;
  try
    LTargetYieldField := FAppModules.FieldProperties.FieldProperty('TYield');
    if not Assigned(LTargetYieldField) then
      Raise Exception.Create('Field (TYield) not found in field properties');
      SetLength(LTargetYieldArray,LTargetYieldField.ArrayLength);

    LMaximumYieldField := FAppModules.FieldProperties.FieldProperty('MYield');
    if not Assigned(LMaximumYieldField) then
      raise Exception.Create('Field (MYield) not found in field properties');
      SetLength(LMaximumYieldArray,LMaximumYieldField.ArrayLength);

    LTargetPowerField := FAppModules.FieldProperties.FieldProperty('TPower');
    if not Assigned(LTargetPowerField) then
      raise Exception.Create('Field (TPower) not found in field properties');
      SetLength(LTargetPowerArray,LTargetPowerField.ArrayLength);

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetConfigurationSQL);
        LDataSet.DataSet.Open;
        LLoadCasesCount := -1;
        if(LDataSet.DataSet.RecordCount > 0) then
        begin
          LLoadCasesCount := LDataSet.DataSet.FieldByName('LoadCasesCount').Asinteger;
        end;

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(FSQLAgent.GetTargetYieldSQL);
        LDataSet.DataSet.Open;
        if(LDataSet.DataSet.RecordCount > 0) then
        begin
          for LIndex := 1 to 10 do
          begin
            LTargetYieldArray[LIndex] := NullFloat;
            LFieldName := Format('TYield%d',[LIndex]);
            if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
              LTargetYieldArray[LIndex] := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
          end;
        end;

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(FSQLAgent.GetMaximumYieldSQL);
        LDataSet.DataSet.Open;
        if(LDataSet.DataSet.RecordCount > 0) then
        begin
          for LIndex := 1 to 10 do
          begin
            LMaximumYieldArray[LIndex] := NullFloat;
            LFieldName := Format('MYield%d',[LIndex]);
            if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
              LMaximumYieldArray[LIndex] := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
          end;
        end;

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(FSQLAgent.GetTargetPowerSQL);
        LDataSet.DataSet.Open;
        if(LDataSet.DataSet.RecordCount > 0) then
        begin
          for LIndex := 1 to 10 do
          begin
            LTargetPowerArray[LIndex] := NullFloat;
            LFieldName := Format('TPower%d',[LIndex]);
            if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
              LTargetPowerArray[LIndex] := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
          end;
        end;

        ARunConfigurationData.PopulateSystemYieldAndPowerData(
           LLoadCasesCount,LTargetYieldArray,LMaximumYieldArray,LTargetPowerArray);
        Result :=  True;
      end;
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TRunConfigurationDataLoadAgent.LoadDecisionDatesData(ARunConfigurationData: TRunConfigurationData): boolean;
const OPNAME = 'TRunConfigurationDataLoadAgent.LoadDecisionDatesData';
var
  LIndex                 : integer;
  LFieldName             : string;
  LNrOfDecisionMonths    : integer;
  LDataSet               : TAbstractModelDataset;
  LDecisionMonths        : TDecisionMonthArray;
  LDecisionTypes         : TDecisionTypeArray;
  LHydroPowerIndicators  : THydroPowerIndicatorArray;
  LDecisionMonthField,
  LDecisionTypeField,
  LHydroPowerIndicatorField : TAbstractFieldProperty;
begin
  Result := False;
  try
    LDecisionMonthField := FAppModules.FieldProperties.FieldProperty('DecisionMonth');
    if not Assigned(LDecisionMonthField) then
      Raise Exception.Create('Field (DecisionMonth) not found in field properties');
      SetLength(LDecisionMonths,LDecisionMonthField.ArrayLength);

    LDecisionTypeField := FAppModules.FieldProperties.FieldProperty('DecisionType');
    if not Assigned(LDecisionTypeField) then
      raise Exception.Create('Field (DecisionType) not found in field properties');
      SetLength(LDecisionTypes,LDecisionTypeField.ArrayLength);

    LHydroPowerIndicatorField := FAppModules.FieldProperties.FieldProperty('HydroPowerIndicator');
    if not Assigned(LHydroPowerIndicatorField) then
      raise Exception.Create('Field (HydroPowerIndicator) not found in field properties');
      SetLength(LHydroPowerIndicators,LHydroPowerIndicatorField.ArrayLength);

    LNrOfDecisionMonths := 0;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetDecisionDateSQL);
        LDataSet.DataSet.Open;
        if(LDataSet.DataSet.RecordCount > 0) then
        begin
          LNrOfDecisionMonths := LDataSet.DataSet.FieldByName('NrOfDecisionDates').AsInteger;
          for LIndex := 1 to 12 do
          begin
            LDecisionMonths[LIndex] := NullInteger;
            LFieldName := Format('DecisionMonth%2.2d',[LIndex]);
            if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
              LDecisionMonths[LIndex] := LDataSet.DataSet.FieldByName(LFieldName).AsInteger;

            LDecisionTypes[LIndex] := '';
            LFieldName := Format('DecisionType%2.2d',[LIndex]);
            if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
              LDecisionTypes[LIndex] := Trim(LDataSet.DataSet.FieldByName(LFieldName).AsString);

            LHydroPowerIndicators[LIndex] := '';
            LFieldName := Format('HydroPowerIndicator%2.2d',[LIndex]);
            if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
              LHydroPowerIndicators[LIndex] := Trim(LDataSet.DataSet.FieldByName(LFieldName).AsString);
          end;
        end;

        ARunConfigurationData.PopulateDecisionDates(LNrOfDecisionMonths,LDecisionMonths,LDecisionTypes,
                                                    LHydroPowerIndicators);
        Result :=  True;
      end;
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;
  // Handle exceptions.
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TRunConfigurationDataLoadAgent.LoadDataFilePathsData(ADataFilePaths: TDataFilePaths;
                                                              ARunConfigurationData: TRunConfigurationData): boolean;
const OPNAME = 'TRunConfigurationDataLoadAgent.LoadDataFilePathsData';
var
  LDataSet         : TAbstractModelDataset;
  LDataFilePrefix,
  LDataFilePath,
  LOutputFilePath,
  LHydrologyPath,
  LSpecifiedDemandPath: string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetDataFilePathSQL);
        LDataSet.DataSet.Open;
        if(LDataSet.DataSet.RecordCount > 0) then
        begin
          LDataFilePrefix      := Trim(LDataSet.DataSet.FieldByName('FilePrefix').AsString);
          LDataFilePath        := Trim(LDataSet.DataSet.FieldByName('InputPath').AsString);
          LOutputFilePath      := Trim(LDataSet.DataSet.FieldByName('OutputPath').AsString);
          LHydrologyPath       := Trim(LDataSet.DataSet.FieldByName('HydrologyPath').AsString);
          LSpecifiedDemandPath := Trim(LDataSet.DataSet.FieldByName('SpecifiedDemandPath').AsString);
        end;

        ADataFilePaths.PopulateDataFilePaths(LDataFilePrefix,LDataFilePath,LOutputFilePath,LHydrologyPath,LSpecifiedDemandPath);
        Result :=  True;
      end;
    finally                             
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;


procedure TRunConfigurationDataLoadAgent.LoadMonthsDataContextData(AContextData: TStringList; AFieldNameIdentifier: string);
const OPNAME = 'TRunConfigurationDataLoadAgent.LoadMonthsDataContextData';
begin
  try
    FSQLAgent.LoadMonthsDataContextData(AContextData, AFieldNameIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRunConfigurationDataLoadAgent.LoadPeriodDataContextData(AContextData: TStringList);
const OPNAME = 'TRunConfigurationDataLoadAgent.LoadPeriodDataContextData';
begin
  try
    FSQLAgent.LoadPeriodDataContextData(AContextData);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

end.
