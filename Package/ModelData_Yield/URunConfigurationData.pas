//
//
//  UNIT      : Contains RunConfigurationData Class
//  AUTHOR    : Dziedzi Ramulondi(Arivia)
//  DATE      : 09/09/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//                                                 
unit URunConfigurationData;

interface

uses
  Classes,

  UAbstractObject,
  VoaimsCom_TLB;

type

  TRunConfigurationData = class(TAbstractAppObject,IRunConfigurationData)
  private
    FMonthNamesArray             : TMonthNamesArray;
    FMonthsDaysArray             : TMonthDaysArray;
    FStartYearGregorian          : integer;
    FStartYearOther              : integer;
    FYearsInAnalysis             : integer;
    FPeriodsInAnalysis           : integer;
    FStartMonthNumber            : integer;
    FCalculateHistoricFirmYield  : Integer;
    FTargetRecurrenceInterval    : integer;
    FRunTitle1                   : string;
    FRunTitle2                   : string;
    FRunTitle3                   : string;
    FLimitOption                 : boolean;
    FRunSequenceType             : string;
    FMultiplePeriodLengths       : boolean;
    FReduceSequences             : boolean;
    FNumberOfSequencesInAnalysis : integer;
    FStartSequenceNumber         : integer;
    FGeneratedFlowFlag           : integer;
    FSequencesToBeAnalysedArray  : TSequencesToBeAnalysedArray;
    FDebugLevel                  : integer;
    FStartDebugPeriod            : integer;
    FEndDebugPeriod              : integer;
    FOutputSummaryLevel          : integer;
    FCreateDataFile              : boolean;
    FCreateYieldFile             : boolean;
    FCreatePlotFile              : boolean;
    FTargetYieldArray            : TTargetYieldArray;
    FMaximumYieldArray           : TMaximumYieldArray;
    FTargetPowerArray            : TTargetPowerArray;
    FNrOfActiveLoadCases         : integer;
    FOutputDataHasBeenPopulated  : boolean;
    FHydroUnitsCode              : string;
    FRandomNumberOption          : integer;
    FParamFileName               : string;

    // Planning model data menbers
    FDetailedOption               : boolean;
    FSupplyOption                 : boolean;
    FAnnualSummary                : string;
    FEconomicOption               : boolean;
    FPlanningSummary              : boolean;
    FInputSummary                 : boolean;
    FWaterQualityOption           : boolean;
    FPeriodsPerYear               : integer;
    FCalendarStartMonth           : integer;
    FShortTermPlanningOption      : string;
    FHydroPowerOption             : string;
    FAllocationControlOption      : string;
    FNrOfDecisionMonths           : integer;
    FDecisionMonths               : TDecisionMonthArray;
    FDecisionTypes                : TDecisionTypeArray;
    FHydroPowerIndicators         : THydroPowerIndicatorArray;

  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure DeleteLoadCaseColumn (AIndex : integer);
    procedure WriteMaximumYieldToDB(AIndex: integer; AMaximumYield: double);
    procedure WriteTargetPowerToDB(AIndex: integer; ATargetPower: double);
    procedure WriteTargetYieldToDB(AIndex: integer;ATargetYield: double);
    procedure UpdateDamImplimentationFile(AAllocationControlOption : string);
    procedure SortYield;
    procedure CopyInflowFilesOnTheDisk;
    procedure CopyInflowFilesInTheDatabase;
    procedure CopyInflowFiles(AOldDirFile,ANewDirFile:string);

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function GetMinMaxYearFromHydrologyDB(const AFileName: string; var AStartYear, AEndYear: integer): boolean;
    function GetMinMaxYearFromDemandDB(const AFileNumber: integer; var AStartYear, AEndYear: integer): boolean;
    function CreateNrOfDecisionDates(ANrOfDecisionDate : integer) : boolean;

    function ValidateStartYearOther(AErrorMessages: TStrings): boolean;
    function ValidateStartYearGregorian(AErrorMessages: TStrings): boolean;
    function ValidateNumberOfYears(AErrorMessages: TStrings): Boolean;
    function ValidateNumberOfYearsFromDemandFiles(AErrorMessages: TStrings): Boolean;
    function ValidateNumberOfYearsFromHydrologyFiles(AErrorMessages: TStrings): Boolean;
    function ValidateNumberOfPeriods(AErrorMessages: TStrings): Boolean;
    function ValidateStartMonthNo(AErrorMessages: TStrings): Boolean;
    function ValidateMonthNames(AErrorMessages: TStrings; AErrorColumns: TStringList): Boolean;
    function ValidateMonthsDays(AErrorMessages: TStrings; AErrorColumns: TStringList): boolean;
    function ValidateMultiplePeriods(AErrorMessages: TStrings): Boolean;
    function ValidateReduceSequences(AErrorMessages: TStrings): Boolean;
    function ValidateNumberOfSequences(AErrorMessages: TStrings): Boolean;
    function ValidateSequenceInAnalysis(AErrorMessages: TStrings;AErrorColumns  : TStringList): Boolean;
    function ValidateStartType(AErrorMessages: TStrings): Boolean;
    function ValidateDebugStartPeriod(AErrorMessages: TStrings): Boolean;
    function ValidateDebugEndPeriod(AErrorMessages: TStrings): Boolean;
    function ValidateDebugLevel(AErrorMessages: TStrings): Boolean;
    function ValidateSummaryLevel(AErrorMessages: TStrings): Boolean;
    function ValidateCalcFirmYield(AErrorMessages: TStrings): Boolean;
    function ValidateTargetRecurrenceInterval(AErrorMessages: TStrings): Boolean;
    function ValidateRunTitle1(AErrorMessages: TStrings): Boolean;
    function ValidateRunTitle2(AErrorMessages: TStrings): Boolean;
    function ValidateRunTitle3(AErrorMessages: TStrings): Boolean;
    function ValidateTargetYield(AErrorMessages: TStrings;AErrorColumns  : TStringList): Boolean;
    function ValidateMaximumYield(AErrorMessages: TStrings;AErrorColumns  : TStringList): Boolean;
    function ValidateTargetPower(AErrorMessages: TStrings; AErrorColumns  : TStringList): Boolean;
    function ValidateNrOfLoadCases(AErrorMessages: TStrings): Boolean;
    function ValidateStartYearSequence(AErrorMessages: TStrings): Boolean;
    function ValidateHydroUnitsCode(AErrorMessages: TStrings): Boolean;

    function ValidatePeriodPerYear(AErrorMessages: TStrings): Boolean;
    function ValidateCalendarStartMonth(AErrorMessages: TStrings): Boolean;
    function ValidateDecisionMonth(AErrorMessages: TStrings;AErrorColumns  : TStringList): Boolean;
    function ValidateDecisionType(AErrorMessages: TStrings;AErrorColumns  : TStringList): Boolean;
    function ValidateHydroPowerIndicator(AErrorMessages: TStrings; AErrorColumns  : TStringList): Boolean;

  public
    function Get_MonthNameByIndex(Index: Integer): WideString; safecall;
    procedure Set_MonthNameByIndex(Index: Integer; const Value: WideString); safecall;
    function Get_MonthDaysByIndex(Index: Integer): Double; safecall;
    procedure Set_MonthDaysByIndex(Index: Integer; Value: Double); safecall;
    function Get_MonthIndexByName(const Name: WideString): Integer; safecall;
    procedure Set_MonthIndexByName(const Name: WideString; Value: Integer); safecall;
    function Get_MonthDaysByName(const Name: WideString): Double; safecall;
    procedure Set_MonthDaysByName(const Name: WideString; Value: Double); safecall;
    function GetEndYearAndMonth (var AEndYear  : integer; var AEndMonth : integer) : WordBool; safecall;

    function Get_TotalDaysInAYear:double;
    function Get_StartYearGregorian: Integer; safecall;
    procedure Set_StartYearGregorian(Value: Integer); safecall;
    function Get_StartYearOther: Integer; safecall;
    procedure Set_StartYearOther(Value: Integer); safecall;
    function Get_YearsInAnalysis: Integer; safecall;
    procedure Set_YearsInAnalysis(Value: Integer); safecall;
    function Get_PeriodsInAnalysis: Integer; safecall;
    procedure Set_PeriodsInAnalysis(Value: Integer); safecall;
    function Get_StartMonthNumber: Integer; safecall;
    procedure Set_StartMonthNumber(Value: Integer); safecall;
    function Get_CalculateHistoricFirmYield: Integer; safecall;
    procedure Set_CalculateHistoricFirmYield(Value: Integer); safecall;
    function Get_TargetRecurrenceInterval: Integer; safecall;
    procedure Set_TargetRecurrenceInterval(Value: Integer); safecall;
    function Get_LimitOption: WordBool; safecall;
    procedure Set_LimitOption(Value: WordBool); safecall;
    function Get_RunSequenceType: WideString; safecall;
    procedure Set_RunSequenceType(const Value: WideString); safecall;
    function Get_MultiplePeriodLengths: WordBool; safecall;
    procedure Set_MultiplePeriodLengths(Value: WordBool); safecall;
    function Get_ReduceSequences: WordBool; safecall;
    procedure Set_ReduceSequences(Value: WordBool); safecall;
    function Get_HistoricSequenceStartYear: Integer; safecall;
    procedure Set_HistoricSequenceStartYear(Value: Integer); safecall;
    function Get_StartSequenceNumber: Integer; safecall;
    procedure Set_StartSequenceNumber(Value: Integer); safecall;
    function Get_NumberOfSequencesInAnalysis: Integer; safecall;
    procedure Set_NumberOfSequencesInAnalysis(Value: Integer); safecall;
    function Get_GeneratedFlowFlag: Integer; safecall;
    procedure Set_GeneratedFlowFlag(Value: Integer); safecall;
    function Get_SequenceToBeAnalysedByIndex(Index: Integer): Integer; safecall;
    procedure Set_SequenceToBeAnalysedByIndex(Index: Integer; Value: Integer); safecall;
    function Get_DebugLevel: Integer; safecall;
    procedure Set_DebugLevel(Value: Integer); safecall;
    function Get_StartDebugPeriod: Integer; safecall;
    procedure Set_StartDebugPeriod(Value: Integer); safecall;
    function Get_EndDebugPeriod: Integer; safecall;
    procedure Set_EndDebugPeriod(Value: Integer); safecall;
    function Get_StartDebugDate: TDateTime; safecall;
    procedure Set_StartDebugDate(Value: TDateTime); safecall;
    function Get_EndDebugDate: TDateTime; safecall;
    procedure Set_EndDebugDate(Value: TDateTime); safecall;
    function Get_OutputSummaryLevel: Integer; safecall;
    procedure Set_OutputSummaryLevel(Value: Integer); safecall;
    function Get_CreateDataFile: WordBool; safecall;
    procedure Set_CreateDataFile(Value: WordBool); safecall;
    function Get_CreateYieldFile: WordBool; safecall;
    procedure Set_CreateYieldFile(Value: WordBool); safecall;
    function Get_CreatePlotFile: WordBool; safecall;
    procedure Set_CreatePlotFile(Value: WordBool); safecall;
    function Get_TargetYieldByIndex(Index: Integer): Double; safecall;
    procedure Set_TargetYieldByIndex(Index: Integer; Value: Double); safecall;
    function Get_MaximumYieldByIndex(Index: Integer): Double; safecall;
    procedure Set_MaximumYieldByIndex(Index: Integer; Value: Double); safecall;
    function Get_TargetPowerByIndex(Index: Integer): Double; safecall;
    procedure Set_TargetPowerByIndex(Index: Integer; Value: Double); safecall;
    function Get_HasBeenPopulated: WordBool; safecall;
    function Get_OutputDataHasBeenPopulated: WordBool; safecall;
    function Get_YieldRunTitle1: WideString; safecall;
    procedure Set_YieldRunTitle1(const Value: WideString); safecall;
    function Get_YieldRunTitle2: WideString; safecall;
    procedure Set_YieldRunTitle2(const Value: WideString); safecall;
    function Get_YieldRunTitle3: WideString; safecall;
    procedure Set_YieldRunTitle3(const Value: WideString); safecall;
    function Get_NrOfActiveLoadCases: integer; safecall;
    procedure Set_NrOfActiveLoadCases(Const Value: integer); safecall;
    function Get_HydroUnitsCode: WideString; safecall;
    procedure Set_HydroUnitsCode(const Value: WideString); safecall;
    function Get_RandomNumberOption: Integer; safecall;
    procedure Set_RandomNumberOption(Value: Integer); safecall;
    function Get_ParamFileName: WideString; safecall;
    procedure Set_ParamFileName(const Value: WideString); safecall;

    function Get_DetailedOption: WordBool; safecall;
    procedure Set_DetailedOption(Value: WordBool); safecall;
    function Get_SupplyOption: WordBool; safecall;
    procedure Set_SupplyOption(Value: WordBool); safecall;
    function Get_AnnualSummary: WideString; safecall;
    procedure Set_AnnualSummary(const Value: WideString); safecall;
    function Get_EconomicOption: WordBool; safecall;
    procedure Set_EconomicOption(Value: WordBool); safecall;
    function Get_PlanningSummary: WordBool; safecall;
    procedure Set_PlanningSummary(Value: WordBool); safecall;
    function Get_InputSummary: WordBool; safecall;
    procedure Set_InputSummary(Value: WordBool); safecall;
    function Get_WaterQualityOption: WordBool; safecall;
    procedure Set_WaterQualityOption(Value: WordBool); safecall;
    function Get_PeriodsPerYear: Integer; safecall;
    procedure Set_PeriodsPerYear(Value: Integer); safecall;
    function Get_CalendarStartMonth: Integer; safecall;
    procedure Set_CalendarStartMonth(Value: Integer); safecall;
    function Get_ShortTermPlanningOption: WideString; safecall;
    procedure Set_ShortTermPlanningOption(const Value: WideString); safecall;
    function Get_HydroPowerOption: WideString; safecall;
    procedure Set_HydroPowerOption(const Value: WideString); safecall;

    function Get_AllocationControlOption: WideString; safecall;
    procedure Set_AllocationControlOption(const Value: WideString); safecall;

    //function Get_AllocationControlOption: WordBool; safecall;
    //procedure Set_AllocationControlOption(Value: WordBool); safecall;
    function Get_NrOfDecisionMonths: Integer; safecall;
    procedure Set_NrOfDecisionMonths(Value: Integer); safecall;
    function Get_DecisionMonthByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_DecisionMonthByIndex(AIndex: Integer; AValue: Integer); safecall;
    function Get_DecisionTypeByIndex(AIndex: Integer): WideString; safecall;
    procedure Set_DecisionTypeByIndex(AIndex: Integer; const AValue: WideString); safecall;
    function Get_HydroPowerIndicatorByIndex(AIndex: Integer): WideString; safecall;
    procedure Set_HydroPowerIndicatorByIndex(AIndex: Integer; const AValue: WideString); safecall;

    procedure ActivateLoadCase (AIndex : integer);
    procedure DeactivateLoadCase (AIndex : integer);

    function Initialise: boolean; override;
    function PopulatePeriodData(AStartYearGregorian, AStartYearOther, AYearsInAnalysis, APeriodsInAnalysis,
                                AStartMonthNumber: integer): boolean;
    function PopulateRunInfoData(ACalculateHistoricFirmYield: Integer;ATargetRecurrenceInterval: integer;ALimitOption: boolean;
                                ARunSequenceType: string;ATitle1: string; ATitle2: string; ATitle3: string;
                                ARandomNumberOption : integer; AParamFileName : string): boolean;
    function PopulateStochasticData(AMultiplePeriodLengths,AReduceSequences: boolean;
                                                    AGeneratedFlowFlag,ANumberOfSequencesInAnalysis: integer): boolean;
    function PopulateDebugInfoData(ADebugLevel,AStartDebugPeriod, AEndDebugPeriod: integer): boolean;
    function PopulateOutputConfigurationData(AOutputSummaryLevel: integer;ACreateDataFile,ACreateYieldFile,
                                            ACreatePlotFile: boolean): boolean;
    function PopulateMonthsData(AMonthNamesArray: TMonthNamesArray; AMonthsDaysArray: TMonthDaysArray): boolean;
    function PopulateSequencesToBeAnalysedArrayData(AHistoricSequenceStartYear,AStartSequenceNumber: integer;
             ASequencesToBeAnalysedArray: TSequencesToBeAnalysedArray): boolean;
    function PopulateSystemYieldAndPowerData(ANrOfLoadCases: integer;ATargetYieldArray: TTargetYieldArray;
             AMaximumYieldArray: TMaximumYieldArray; ATargetPowerArray: TTargetPowerArray): boolean;
    function PopulateHydroUnitsCode(AHydroUnitsCode : string): boolean;
    function PopulatePlanningConfigurationData(ADetailedOption,ASupplyOption : boolean;AAnnualSummary : string;
                                               AEconomicOption,APlanningSummary,AInputSummary : boolean;
                                               AWaterQualityOption : boolean; APeriodsPerYear,ACalendarStartMonth: integer;
                                               AShortTermPlanningOption,AHydroPowerOption : string;AAllocationControlOption : string): boolean;

    function PopulateDecisionDates(ANrOfDecisionDates: integer;
                                   ADecisionMonths: TDecisionMonthArray;
                                   ADecisionTypes: TDecisionTypeArray;
                                   AHydroPowerIndicators: THydroPowerIndicatorArray): boolean;



    function GetBaseValue(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function GetKeyValues (const AParamField : WideString; const AFieldIndex: WideString) : WideString; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function MaximumPeriodInHydrologyFiles : integer;
    function MonthNameByMonthNumber(AMonthNumber: Integer): WideString;

    property TotalDaysInAYear : double read Get_TotalDaysInAYear;
    property MonthNameByIndex[Index: Integer]: WideString read Get_MonthNameByIndex write Set_MonthNameByIndex;
    property MonthDaysByIndex[Index: Integer]: Double read Get_MonthDaysByIndex write Set_MonthDaysByIndex;
    property MonthIndexByName[const Name: WideString]: Integer read Get_MonthIndexByName write Set_MonthIndexByName;
    property MonthDaysByName[const Name: WideString]: Double read Get_MonthDaysByName write Set_MonthDaysByName;
    property StartYearGregorian: Integer read Get_StartYearGregorian write Set_StartYearGregorian;
    property StartYearOther: Integer read Get_StartYearOther write Set_StartYearOther;
    property YearsInAnalysis: Integer read Get_YearsInAnalysis write Set_YearsInAnalysis;
    property PeriodsInAnalysis: Integer read Get_PeriodsInAnalysis write Set_PeriodsInAnalysis;
    property StartMonthNumber: Integer read Get_StartMonthNumber write Set_StartMonthNumber;
    property CalculateHistoricFirmYield: Integer read Get_CalculateHistoricFirmYield write Set_CalculateHistoricFirmYield;
    property LimitOption: WordBool read Get_LimitOption write Set_LimitOption;
    property RunSequenceType: WideString read Get_RunSequenceType write Set_RunSequenceType;
    property MultiplePeriodLengths: WordBool read Get_MultiplePeriodLengths write Set_MultiplePeriodLengths;
    property ReduceSequences: WordBool read Get_ReduceSequences write Set_ReduceSequences;
    property HistoricSequenceStartYear: Integer read Get_HistoricSequenceStartYear write Set_HistoricSequenceStartYear;
    property StartSequenceNumber: Integer read Get_StartSequenceNumber write Set_StartSequenceNumber;
    property NumberOfSequencesInAnalysis: Integer read Get_NumberOfSequencesInAnalysis write Set_NumberOfSequencesInAnalysis;
    property GeneratedFlowFlag: Integer read Get_GeneratedFlowFlag write Set_GeneratedFlowFlag;
    property SequenceToBeAnalysedByIndex[Index: Integer]: Integer read Get_SequenceToBeAnalysedByIndex write Set_SequenceToBeAnalysedByIndex;
    property DebugLevel: Integer read Get_DebugLevel write Set_DebugLevel;
    property StartDebugPeriod: Integer read Get_StartDebugPeriod write Set_StartDebugPeriod;
    property EndDebugPeriod: Integer read Get_EndDebugPeriod write Set_EndDebugPeriod;
    property StartDebugDate: TDateTime read Get_StartDebugDate write Set_StartDebugDate;
    property EndDebugDate: TDateTime read Get_EndDebugDate write Set_EndDebugDate;
    property OutputSummaryLevel: Integer read Get_OutputSummaryLevel write Set_OutputSummaryLevel;
    property CreateDataFile: WordBool read Get_CreateDataFile write Set_CreateDataFile;
    property CreateYieldFile: WordBool read Get_CreateYieldFile write Set_CreateYieldFile;
    property CreatePlotFile: WordBool read Get_CreatePlotFile write Set_CreatePlotFile;
    property TargetYieldByIndex[Index: Integer]: Double read Get_TargetYieldByIndex write Set_TargetYieldByIndex;
    property MaximumYieldByIndex[Index: Integer]: Double read Get_MaximumYieldByIndex write Set_MaximumYieldByIndex;
    property TargetPowerByIndex[Index: Integer]: Double read Get_TargetPowerByIndex write Set_TargetPowerByIndex;
    property TargetRecurrenceInterval: Integer read Get_TargetRecurrenceInterval write Set_TargetRecurrenceInterval;
    property YieldRunTitle1: WideString read Get_YieldRunTitle1 write Set_YieldRunTitle1;
    property YieldRunTitle2: WideString read Get_YieldRunTitle2 write Set_YieldRunTitle2;
    property YieldRunTitle3: WideString read Get_YieldRunTitle3 write Set_YieldRunTitle3;
    property NumberOfActiveLoadCases : integer read Get_NrOfActiveLoadCases{ write Set_NrOfActiveLoadCases};
    property HasBeenPopulated: WordBool read Get_HasBeenPopulated;
    property OutputDataHasBeenPopulated: WordBool read Get_OutputDataHasBeenPopulated;
    property MonthNamesArray  : TMonthNamesArray read FMonthNamesArray;
    property MonthsDaysArray  : TMonthDaysArray  read FMonthsDaysArray;
    property HydroUnitsCode   : WideString read Get_HydroUnitsCode write Set_HydroUnitsCode;

    property DetailedOption: WordBool read Get_DetailedOption write Set_DetailedOption;
    property SupplyOption: WordBool read Get_SupplyOption write Set_SupplyOption;
    property AnnualSummary: WideString read Get_AnnualSummary write Set_AnnualSummary;
    property EconomicOption: WordBool read Get_EconomicOption write Set_EconomicOption;
    property PlanningSummary: WordBool read Get_PlanningSummary write Set_PlanningSummary;
    property InputSummary: WordBool read Get_InputSummary write Set_InputSummary;
    property WaterQualityOption: WordBool read Get_WaterQualityOption write Set_WaterQualityOption;
    property PeriodsPerYear: Integer read Get_PeriodsPerYear write Set_PeriodsPerYear;
    property CalendarStartMonth: Integer read Get_CalendarStartMonth write Set_CalendarStartMonth;
    property ShortTermPlanningOption: WideString read Get_ShortTermPlanningOption write Set_ShortTermPlanningOption;
    property HydroPowerOption: WideString read Get_HydroPowerOption write Set_HydroPowerOption;
    property AllocationControlOption: WideString read Get_AllocationControlOption write Set_AllocationControlOption;
    property NrOfDecisionMonths: Integer read Get_NrOfDecisionMonths write Set_NrOfDecisionMonths;
    property DecisionMonthByIndex[AIndex: Integer]: Integer read Get_DecisionMonthByIndex write Set_DecisionMonthByIndex;
    property DecisionTypeByIndex[AIndex: Integer]: WideString read Get_DecisionTypeByIndex write Set_DecisionTypeByIndex;
    property HydroPowerIndicatorByIndex[AIndex: Integer]: WideString read Get_HydroPowerIndicatorByIndex write Set_HydroPowerIndicatorByIndex;

  end;

implementation

uses
  System.UITypes,
  SysUtils,
  DateUtils,
  VCL.Dialogs,
  UUtilities,
  Contnrs,
  VCL.Controls,
  UConstants,
  UFileNames,
  UAbstractFileNamesObject,
  UYieldModelDataObject,
  URunConfigurationDataSQLAgent,
  UMainMenuEventType,
  UErrorHandlingOperations, Variants;


{ TRunConfigurationData }

function TRunConfigurationData._AddRef: Integer;
const OPNAME = 'TRunConfigurationData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData._Release: Integer;
const OPNAME = 'TRunConfigurationData._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.ValidateSummaryLevel(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TRunConfigurationData.ValidateSummaryLevel';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('SummaryLevel',
            IntToStr(OutputSummaryLevel),LMessage)) then
      AErrorMessages.Add(LMessage)
    else
      Result := true;
  except on E : exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.ValidateCalcFirmYield(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TRunConfigurationData.ValidateCalcFirmYield';
var
  LMessage : string;
  lValue   : string;
begin
  Result := False;
  try
    LMessage := '';
    lValue := IntToStr(CalculateHistoricFirmYield);
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('CalcHistoryOpt',
            lValue, LMessage)) then
      AErrorMessages.Add(LMessage)
    else
      Result := true;
  except on E : exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.ValidateDebugLevel(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TRunConfigurationData.ValidateDebugLevel';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('DebugLevel',
            FloatToStr(DebugLevel),LMessage)) then
      AErrorMessages.Add(LMessage)
    else
      Result := true;
  except on E : exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.ValidateTargetRecurrenceInterval(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TRunConfigurationData.ValidateTargetRecurrenceInterval';
var
  LMessage : string;
begin
  Result := FALSE;
  try
    if FAppModules.StudyArea.ModelVersion <> '7' then
      Result := True
    else
    begin
      if (NOT FAppModules.FieldProperties.ValidateFieldProperty('TargetRecurrenceInterval',
            IntToStr(TargetRecurrenceInterval),LMessage)) then
        AErrorMessages.Add(LMessage)
      else
        Result := true;
    end;
  except on E : exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.ValidateRunTitle1(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TRunConfigurationData.ValidateRunTitle1';
var
  LMessage : string;
begin
  Result := FALSE;
  try
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('Title1',
                                        YieldRunTitle1, LMessage)) then
      AErrorMessages.Add(LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.ValidateRunTitle2(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TRunConfigurationData.ValidateRunTitle2';
var
  LMessage : string;
begin
  Result := FALSE;
  try
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('Title2',
                                        YieldRunTitle2, LMessage)) then
      AErrorMessages.Add(LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.ValidateRunTitle3(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TRunConfigurationData.ValidateRunTitle3';
var
  LMessage : string;
begin
  Result := FALSE;
  try
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('Title3',
                                        YieldRunTitle3, LMessage)) then
      AErrorMessages.Add(LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.ValidateStartType(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TRunConfigurationData.ValidateStartType';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty
      ('StartType', IntToStr(GeneratedFlowFlag),LMessage)) then
      AErrorMessages.Add(LMessage)
    else
      Result := True;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.ValidateDebugStartPeriod(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TRunConfigurationData.ValidateDebugStartPeriod';
var
  LMessage : string;
begin
  Result := FALSE;
  try
    LMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('DebugInit',
            FloatToStr(StartDebugPeriod), LMessage)) then
      AErrorMessages.Add(LMessage)
    else
    begin
      if (StartDebugPeriod > YearsInAnalysis * 12) then
        AErrorMessages.Add(FAppModules.Language.GetString('ContextValidation.DebugPeriodOutsideAnalysis'))
      else
      if (StartDebugPeriod > EndDebugPeriod) then
        AErrorMessages.Add(FAppModules.Language.GetString('ContextValidation.DebugPeriodStartAndEnd'))
      else
        Result := TRUE;
    end;
  except on E : exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.ValidateDebugEndPeriod(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TRunConfigurationData.ValidateDebugEndPeriod';
var
  LMessage : string;
begin
  Result := FALSE;
  try
    LMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('DebugFinal',
           FloatToStr(EndDebugPeriod),LMessage)) then
      AErrorMessages.Add(LMessage)
    else
    begin
      if (EndDebugPeriod > YearsInAnalysis * 12) then
        AErrorMessages.Add(FAppModules.Language.GetString('ContextValidation.DebugPeriodOutsideAnalysis'))
      else
      if (StartDebugPeriod > EndDebugPeriod) then
        AErrorMessages.Add(FAppModules.Language.GetString('ContextValidation.DebugPeriodStartAndEnd'))
      else
        Result := TRUE;
    end;
  except on E : exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.ValidateMonthsDays(AErrorMessages: TStrings; AErrorColumns  : TStringList): boolean;
const OPNAME = 'TRunConfigurationData.ValidateMonthsDays';
var
  LIndex           : integer;
  LResult          : boolean;
  lStopOnFirstError: boolean;
  LMessage         : string;
  LMonthsDays      : TAbstractFieldProperty;
begin
  result := False;
  try
    LResult := TRUE;
    LMonthsDays := FAppModules.FieldProperties.FieldProperty('Days');
    if (LMonthsDays <> nil) then
    begin
      lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
      for LIndex := LMonthsDays.ArrayLow to LMonthsDays.ArrayHigh do
      begin
        LMessage := '';
        if (NOT FAppModules.FieldProperties.ValidateFieldProperty('Days',
           FloatToStr(MonthDaysByIndex[LIndex]),LMessage,LIndex)) then
        begin
          LResult := False;
          AErrorMessages.Add(LMessage);
          AErrorColumns.Add(IntToStr(lIndex));
          if (lStopOnFirstError) then
            Break;
        end;
      end;
      Result := LResult;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.ValidateNumberOfPeriods(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TRunConfigurationData.ValidateNumberOfPeriods';
var
  LMessage : String;
begin
  Result := False;
  try
    LMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('NumPeriods',
            IntToStr(PeriodsInAnalysis),LMessage)) then
      AErrorMessages.Add(LMessage)
    else
      result := True;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.ValidateMultiplePeriods(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TRunConfigurationData.ValidateMultiplePeriods';
var
  LMessage : string;
  lValue   : string;
begin
  Result := FALSE;
  try
    LMessage := '';
    if (MultiplePeriodLengths) then
      lValue := '1'
    else
      lValue := '0';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('MultPeriodOpt',
            lValue ,LMessage)) then
      AErrorMessages.Add(LMessage)
    else
      Result := TRUE;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.ValidateReduceSequences(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TRunConfigurationData.ValidateReduceSequences';
var
  LMessage : string;
  lValue   : string;
begin
  Result := FALSE;
  try
    LMessage := '';
    if (ReduceSequences) then
      lValue := '1'
    else
      lValue := '0';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('ReduceSeqOpt',
            lValue ,LMessage)) then
      AErrorMessages.Add(LMessage)
    else
      Result := TRUE;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.ValidateNumberOfSequences(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TRunConfigurationData.ValidateNumberOfSequences';
var
  LMessage : string;
begin
  result := false;
  try
    LMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('HydroSeqCount',
            FloatToStr(NumberOfSequencesInAnalysis),LMessage)) then
      AErrorMessages.Add(LMessage)
    else
      Result := True;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.ValidateNumberOfYearsFromDemandFiles(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TRunConfigurationData.ValidateNumberOfYearsFromDemandFiles';
var
  LResult           : Boolean;
  LFeatureList      : ISpecifiedDemandFeatureList;
  LFeature          : ISpecifiedDemandFeature;
  LDemandFile       : TAbstractModelFileName;
  LIndex            : integer;
  lStartYear        : integer;
  lEndYear          : integer;
  LMessage          : string;
  LFileNames        : TStringList;
  LFileName         : string;
  LTempStartYear    : integer;
  LTempEndYear      : integer;
  LMaxYears         : integer;
begin
  Result := FALSE;
  try
    LResult := TRUE;
    if (Trim(UpperCase(RunSequenceType)) = 'H') then
    begin
      LFileNames  := TStringList.Create;
      try
        lFeatureList   := TYieldModelDataObject(FAppModules.Model.ModelData).
                            NetworkFeaturesData.SpecifiedDemandFeatureList;
        for lIndex := 0 to lFeatureList.SpecifiedDemandFeatureCount - 1 do
        begin
          lFeature    := lFeatureList.SpecifiedDemandFeatureByIndex[lIndex];
          if (lFeature.SpecifiedDemandFileName <> '') then
          begin
            LFileNames.Add(lFeature.SpecifiedDemandFileName);
          end;
        end;

        lStartYear := 0;
        lEndYear   := 0;

        for lIndex := 0 to LFileNames.Count - 1 do
        begin
          LFileName := LFileNames[lIndex];
          LDemandFile := TYieldModelDataObject(FAppModules.Model.ModelData).
                            FileNamesObject.DemandFileNames.FindFile(lFileName);
          if (LDemandFile = nil) then
          begin
            lResult := FALSE;
            lMessage := FAppModules.Language.GetString('Message.SpecifiedDemand') +
                         ExtractFileName(LFileName) +
                        FAppModules.Language.GetString('Message.DoesNotExist');
            AErrorMessages.Add(lMessage);
            Break;
          end
          else
          if not LDemandFile.SavedInDB then
          begin
            lResult := FALSE;
            lMessage := FAppModules.Language.GetString('Message.SpecifiedDemand') +
                         ExtractFileName(LFileName) +
                        FAppModules.Language.GetString('Message.NotImported');
            AErrorMessages.Add(lMessage);
            Break;
          end
          else
          begin
            lTempStartYear := 0;
            lTempEndYear   := 0;
            if GetMinMaxYearFromDemandDB(LDemandFile.FileNumber,lTempStartYear,lTempEndYear) then
            begin
              if (lStartYear = 0) then
                lStartYear := lTempStartYear
              else
              if (lTempStartYear <> lStartYear) then
              begin
                lResult := FALSE;
                lMessage := FAppModules.Language.GetString('Message.DemandFiles');
                AErrorMessages.Add(lMessage);
                Break;
              end;
              if ((lEndYear = 0) OR (lTempEndYear < lEndYear)) then
                lEndYear := lTempEndYear;
            end
            else
            begin
              lResult := FALSE;
              lMessage := FAppModules.Language.GetString('Message.Demand') +
                           ExtractFileName(LFileName) +
                           FAppModules.Language.GetString('Message.ContainsNoData');
              AErrorMessages.Add(lMessage);
              Break;
            end;
          end;
          if (lResult) then
          begin
            lMaxYears := lEndYear - lStartYear + 1;
            if (StartYearOther < lStartYear) then
            begin
              lResult := FALSE;
              lMessage := FAppModules.Language.GetString('Message.NumberOfYears') +
                          FAppModules.Language.GetString('Message.HydrologySequence') + IntToStr(lStartYear) + ' .';
              AErrorMessages.Add(lMessage);
            end;
            if (StartYearOther > lEndYear) then
             begin
              lResult := FALSE;
              lMessage := FAppModules.Language.GetString('Message.YearsInAnalysis') +
                          FAppModules.Language.GetString('Message.SequenceFiles') + IntToStr(lEndYear) + ' .';
              AErrorMessages.Add(lMessage);
            end;
            if (YearsInAnalysis > lMaxYears) then
            begin
              lResult := FALSE;
              lMessage := FAppModules.Language.GetString('Message.AnalysisIsGreater') +
                          FAppModules.Language.GetString('Message.DemandSequenceFiles') + IntToStr(lMaxYears) + ' .';
              AErrorMessages.Add(lMessage);
            end;
          end;
        end;
      finally
        FreeAndNil(LFileNames);
      end;
    end;
    Result := LResult;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.ValidateNumberOfYearsFromHydrologyFiles(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TRunConfigurationData.ValidateNumberOfYearsFromHydrologyFiles';
var
  lMessage          : string;
  lResult           : Boolean;
  lRefList          : TStringList;
  lReservoirList    : IReservoirDataList;
  lReservoir        : IReservoirConfigurationData;
  LHydrologyFile    : TAbstractModelFileName;
  lIndex            : integer;
  lCatchRefNo       : integer;
  lFileName         : string;
  lStartYear        : integer;
  lEndYear          : integer;
  lMaxYears         : integer;
  lParamSetup       : IParamSetup;
  lCatchmentRef     : IParamReference;
  lTempStartYear    : integer;
  lTempEndYear      : integer;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    if (Trim(UpperCase(RunSequenceType)) = 'H') then
    begin
      lRefList  := TStringList.Create;
      try
        lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                          NetworkElementData.ReservoirList;
        for lIndex := 0 to lReservoirList.ReservoirAndNodesCount - 1 do
        begin
          lReservoir := lReservoirList.ReservoirOrNodeByIndex[lIndex].ReservoirConfigurationData;
          if not (lReservoir.NodeType in [ntNodeWithoutInflow,ntIrrigationNode,ntDemandCentreNode,ntMineNode,ntGroundWater]) then
          begin
            lCatchRefNo := lReservoir.CatchmentRef;
            if ((lCatchRefNo > 0) AND (lRefList.IndexOf(IntToStr(lCatchRefNo)) < 0)) then
               lRefList.Add(IntToStr(lCatchRefNo));
          end;
        end;

        lStartYear := 0;
        lEndYear   := 0;

        lParamSetup := TYieldModelDataObject(FAppModules.Model.ModelData).ParamSetup;
        if (lParamSetup <> nil) then
        begin
          for lIndex := 0 to lRefList.Count - 1 do
          begin
            lCatchRefNo   := StrToInt(lRefList.Strings[lIndex]);
            lCatchmentRef := lParamSetup.ReferenceDataByCatchNumber[lCatchRefNo];
            if(lCatchmentRef = nil) then Continue;
            lFileName     := lCatchmentRef.FileReference + '.INC';
            LHydrologyFile := TYieldModelDataObject(FAppModules.Model.ModelData).
                              FileNamesObject.HydrologyFileNames.FindFile(lFileName);
            if (LHydrologyFile = nil) then
            begin
              lResult := FALSE;
              lMessage := FAppModules.Language.GetString('Message.HydrologyFile') + ' ('+
                           ExtractFileName(lCatchmentRef.FileReference) + '.INC) '+
                          FAppModules.Language.GetString('Message.DoesNotExist');
              AErrorMessages.Add(lMessage);
              Break;
            end
            else
            if not LHydrologyFile.SavedInDB then
            begin
              lResult := FALSE;
              lMessage := FAppModules.Language.GetString('Message.HydrologyFile') +  ' ('+
                           ExtractFileName(lCatchmentRef.FileReference) + '.INC) '+
                          FAppModules.Language.GetString('Message.NotImportedYet');
              AErrorMessages.Add(lMessage);
              Break;
            end
            else
            begin
              lTempStartYear := 0;
              lTempEndYear   := 0;
              if GetMinMaxYearFromHydrologyDB(lFileName,lTempStartYear,lTempEndYear) then
              begin
                if (lStartYear = 0) then
                  lStartYear := lTempStartYear
                else
                if (lTempStartYear <> lStartYear) then
                begin
                  lResult := FALSE;
                  lMessage := FAppModules.Language.GetString('Message.NoSameStartOnHydrologyFiles');
                  AErrorMessages.Add(lMessage);
                  Break;
                end;
                if ((lEndYear = 0) OR (lTempEndYear < lEndYear)) then
                  lEndYear := lTempEndYear;
              end
              else
              begin
                lResult := FALSE;
                lMessage := FAppModules.Language.GetString('Message.HydrologyFile') +  ' ('+
                             ExtractFileName(lCatchmentRef.FileReference) +'.INC) '+
                             FAppModules.Language.GetString('Message.ContainsNoData');
                AErrorMessages.Add(lMessage);
                Break;
              end;
            end;
            if (lResult) then
            begin

              lMaxYears := lEndYear - lStartYear + 1;
              if (StartYearOther < lStartYear) then
              begin
                lResult := FALSE;
                lMessage := FAppModules.Language.GetString('Message.YearsInAnalysisIsLess') +
                            FAppModules.Language.GetString('Message.HydrologySequence') + IntToStr(lStartYear) + ' .';
                AErrorMessages.Add(lMessage);
              end;
              if (StartYearOther > lEndYear) then
              begin
                lResult := FALSE;
                lMessage := FAppModules.Language.GetString('Message.YearsInAnalysisIsGeater')+
                            FAppModules.Language.GetString('Message.HydrologySequence') + IntToStr(lEndYear) + ' .';
                AErrorMessages.Add(lMessage);
              end;
              if (YearsInAnalysis > lMaxYears) then
              begin
                lResult := FALSE;
                lMessage := FAppModules.Language.GetString('Message.YearsInAnalysisIsGeater') +
                            FAppModules.Language.GetString('Message.HydrologySequence') + IntToStr(lMaxYears) + ' .';
                AErrorMessages.Add(lMessage);
              end;
            end;
          end;
        end;
      finally
        FreeAndNil(lRefList);
      end;
    end;
    Result := lResult;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.ValidateNumberOfYears(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TRunConfigurationData.ValidateNumberOfYears';
var
  lMessage          : string;
begin
  Result  := False;
  try
    lMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty('YearsCount',IntToStr(YearsInAnalysis),LMessage);
    if not Result then
      AErrorMessages.Add(lMessage);
    Result := Result and ValidateNumberOfYearsFromHydrologyFiles(AErrorMessages);
    Result := Result and ValidateNumberOfYearsFromDemandFiles(AErrorMessages);
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.MaximumPeriodInHydrologyFiles : integer;
const OPNAME = 'TRunConfigurationData.MaximumPeriodInHydrologyFiles';
var
  lResult           : Boolean;
  lRefList          : TStringList;
  lFeatureList      : ISpecifiedDemandFeatureList;
  lFeature          : ISpecifiedDemandFeature;
  lReservoirList    : IReservoirDataList;
  lReservoir        : IReservoirConfigurationData;
  lIndex            : integer;
  lCatchRefNo       : integer;
  lFileName         : string;
  lStartYear        : integer;
  lEndYear          : integer;
  lParamSetup       : IParamSetup;
  lCatchmentRef     : IParamReference;
  lFileData         : TStringList;
  lTempYear         : integer;
begin
  Result  := 0;
  LResult := TRUE;
  try
    try
      lRefList  := TStringList.Create;
      lFileData := TStringList.Create;
      lFeatureList   := TYieldModelDataObject(FAppModules.Model.ModelData).
                          NetworkFeaturesData.SpecifiedDemandFeatureList;
      for lIndex := 0 to lFeatureList.SpecifiedDemandFeatureCount - 1 do
      begin
        lFeature    := lFeatureList.SpecifiedDemandFeatureByIndex[lIndex];
        lCatchRefNo := lFeature.CatchmentRefNumber;
        if ((lCatchRefNo > 0) AND (lRefList.IndexOf(IntToStr(lCatchRefNo)) < 0)) then
          lRefList.Add(IntToStr(lCatchRefNo));
      end;
      lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                          NetworkElementData.ReservoirList;
      for lIndex := 0 to lReservoirList.ReservoirAndNodesCount - 1 do
      begin
        lReservoir := lReservoirList.ReservoirOrNodeByIndex[lIndex].ReservoirConfigurationData;
        if not (lReservoir.NodeType in [ntNodeWithoutInflow,ntIrrigationNode,ntDemandCentreNode,ntMineNode,ntGroundWater]) then
        begin
          lCatchRefNo := lReservoir.CatchmentRef;
          if ((lCatchRefNo > 0) AND (lRefList.IndexOf(IntToStr(lCatchRefNo)) < 0)) then
            lRefList.Add(IntToStr(lCatchRefNo));
        end;
      end;

      lStartYear := 0;
      lEndYear   := 0;

      lParamSetup := TYieldModelDataObject(FAppModules.Model.ModelData).ParamSetup;
      if (lParamSetup <> nil) then
      begin
        for lIndex := 0 to lRefList.Count - 1 do
        begin
          lCatchRefNo   := StrToInt(lRefList.Strings[lIndex]);
          lCatchmentRef := lParamSetup.ReferenceDataByCatchNumber[lCatchRefNo];
          lFileName     := lCatchmentRef.FileReference + '.INC';
          if (NOT FileExists(lFileName)) then
          begin
            lResult := FALSE;
            Break;
          end
          else
          begin
            lFileData.LoadFromFile(lFileName);
            if (lFileData.Count = 0) then
            begin
              lResult := FALSE;
              Break;
            end
            else
            begin
              lTempYear := StrToInt(Trim(Copy(lFileData[0], 1, 8)));
              if (lStartYear = 0) then
                lStartYear := lTempYear
              else if (lTempYear <> lStartYear) then
              begin
                lResult := FALSE;
                Break;
              end;
              lTempYear := StrToInt(Trim(Copy(lFileData[lFileData.Count - 1], 1, 8)));
              if ((lEndYear = 0) OR (lTempYear < lEndYear)) then
                lEndYear := lTempYear;
            end;
          end;
        end;
        if (lResult) then
          Result := lEndYear - lStartYear + 1;
      end;
    finally
      FreeAndNil(lRefList);
      FreeAndNil(lFileData);
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.ValidateSequenceInAnalysis(AErrorMessages: TStrings; AErrorColumns  : TStringList): Boolean;
const OPNAME = 'TRunConfigurationData.ValidateSequenceInAnalysis';
var
  LResult             : boolean;
  LIndex              : integer;
  LMessage            : string;
  LSequenceInAnalysis : TAbstractFieldProperty;
  lStopOnFirstError   : boolean;
begin
  Result := FALSE;
  try
    LResult := TRUE;
    LSequenceInAnalysis := FAppModules.FieldProperties.FieldProperty('Seq');
    if (LSequenceInAnalysis <> nil) then
    begin
      lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
      if (NumberOfSequencesInAnalysis <= 10) then
      begin
        for LIndex := LSequenceInAnalysis.ArrayLow to NumberOfSequencesInAnalysis do
        begin
          LMessage := '';
          if (NOT FAppModules.FieldProperties.ValidateFieldProperty('Seq',
                  FloatToStr(SequenceToBeAnalysedByIndex[LIndex]),LMessage,LIndex)) then
          begin
            LResult := FALSE;
            AErrorMessages.Add(LMessage);
            AErrorColumns.Add(IntToStr(lIndex));
            if (lStopOnFirstError) then
              Break;
          end;
        end;
      end
      else
      begin
        LMessage := '';
        if (NOT FAppModules.FieldProperties.ValidateFieldProperty('Seq',
                FloatToStr(StartSequenceNumber), LMessage, 1)) then
        begin
          LResult := FALSE;
          AErrorMessages.Add(LMessage);
        end;
      end;
      Result := LResult;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.ValidateStartMonthNo(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TRunConfigurationData.ValidateStartMonthNo';
var
  LMessage : String;
begin
  Result := False;
  try
    LMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('StartMonthNo', IntToStr(StartMonthNumber),LMessage)) then
      AErrorMessages.Add(LMessage)
    else
      Result := True;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.ValidateStartYearGregorian(AErrorMessages: TStrings): boolean;
const OPNAME = 'TRunConfigurationData.ValidateStartYearGregorian';
var
  LMessage : String;
begin
  Result := False;
  try
    LMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('StartYearG',
            IntToStr(StartYearGregorian),LMessage)) then
      AErrorMessages.Add(LMessage)
    else
      result := True;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.ValidateStartYearOther(AErrorMessages: TStrings): boolean;
const OPNAME = 'TRunConfigurationData.ValidateStartYearOther';
var
  LMessage : String;
begin
  Result := False;
  try
    lMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty('StartYearO',IntToStr(StartYearOther),LMessage);
    if not Result then
      AErrorMessages.Add(lMessage);
    Result := Result and ValidateNumberOfYearsFromHydrologyFiles(AErrorMessages);
    Result := Result and ValidateNumberOfYearsFromDemandFiles(AErrorMessages);
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.ValidateMonthNames(AErrorMessages: TStrings;AErrorColumns: TStringList): Boolean;
const OPNAME = 'TRunConfigurationData.ValidateMonthNames';
var
  LIndex           : integer;
  LResult          : boolean;
  lStopOnFirstError: boolean;
  LMessage         : string;
  LMonthNames      : TAbstractFieldProperty;
begin
  result := False;
  try
    LResult := TRUE;
    LMonthNames := FAppModules.FieldProperties.FieldProperty('Month');
    if (LMonthNames <> nil) then
    begin
      lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
      for LIndex := LMonthNames.ArrayLow to LMonthNames.ArrayHigh do
      begin
        LMessage := '';
        if (NOT FAppModules.FieldProperties.ValidateFieldProperty('Month',
           MonthNameByIndex[LIndex],LMessage,LIndex)) then
        begin
          LResult := False;
          AErrorMessages.Add(LMessage);
          AErrorColumns.Add(IntToStr(LIndex));
          if (lStopOnFirstError) then
            break;
        end;
      end;
      Result := LResult;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TRunConfigurationData.Validate';
var
  LStopOnFirstError : boolean;
  LMessages         : TStringList;
  lErrorCols        : TStringList;
begin
  Result := True;
  try
    LMessages  := TStringList.Create;
    lErrorCols := TStringList.Create;
    try
      if (AContext = 'StartYearOther') then
        Result := ValidateStartYearOther(LMessages)
      else
      if (AContext = 'StartYearGregorian') then
        Result := ValidateStartYearGregorian(LMessages)
      else
      if (AContext = 'NumberOfYears') then
        Result := ValidateNumberOfYears(LMessages)
      else
      if (AContext = 'NumberOfPeriods') then
        Result := ValidateNumberOfPeriods(LMessages)
      else
      if (AContext = 'HydroUnitsCode') then
        Result := ValidateHydroUnitsCode(LMessages)
      else
      if (AContext = 'StartMonthNo') then
        Result := ValidateStartMonthNo(LMessages)
      else
      if (AContext = 'MonthNames') then
      begin
        Result := ValidateMonthNames(LMessages,lErrorCols);
        if (NOT Result) then
        begin
          if (lErrorCols.Count = 0) then
            AErrors := AErrors + LMessages.Text
          else
            AErrors := AErrors + CTStringsSeparator + LMessages.Text +
                                 CTStringsSeparator + lErrorCols.Text+ CTStringsSeparator;
          LMessages.Clear;
          lErrorCols.Clear;
        end;
      end
      else
      if (AContext = 'MonthsDays') then
      begin
        Result := ValidateMonthsDays(LMessages,lErrorCols);
        if (NOT Result) then
        begin
          if (lErrorCols.Count = 0) then
            AErrors := AErrors + LMessages.Text
          else
            AErrors := AErrors + CTStringsSeparator + LMessages.Text +
                                 CTStringsSeparator + lErrorCols.Text+ CTStringsSeparator;
          LMessages.Clear;
          lErrorCols.Clear;
        end;
      end
      else
      if (AContext = 'MultiplePeriods') then
        Result := ValidateMultiplePeriods(LMessages)
      else
      if (AContext = 'ReduceSequences') then
        Result := ValidateReduceSequences(LMessages)
      else
      if (AContext = 'NumberOfSequences') then
        Result := ValidateNumberOfSequences(LMessages)
      else
      if (AContext = 'SequenceInAnalysis') then
      begin
        Result := ValidateSequenceInAnalysis(LMessages,lErrorCols);
        if (NOT Result) then
        begin
          if (lErrorCols.Count = 0) then
            AErrors := AErrors + LMessages.Text
          else
            AErrors := AErrors + CTStringsSeparator + LMessages.Text +
                                 CTStringsSeparator + lErrorCols.Text+ CTStringsSeparator;
          LMessages.Clear;
          lErrorCols.Clear;
        end;
      end
      else
      if (AContext = 'StartType') then
        Result := ValidateStartType(LMessages)
      else
      if (AContext = 'DebugStartPeriod') then
        Result := ValidateDebugStartPeriod(LMessages)
      else
      if (AContext = 'DebugEndPeriod') then
        Result := ValidateDebugEndPeriod(LMessages)
      else
      if (AContext = 'DebugLevel') then
        Result := ValidateDebugLevel(LMessages)
      else
      if (AContext = 'SummaryLevel') then
        Result := ValidateSummaryLevel(LMessages)
      else
      if (AContext = 'CalcFirmYield') then
        Result := ValidateCalcFirmYield(LMessages)
      else
      if (AContext = 'TargetYield') then
      begin
        Result := ValidateTargetYield(LMessages,lErrorCols);
        if (NOT Result) then
        begin
          if (lErrorCols.Count = 0) then
            AErrors := AErrors + LMessages.Text
          else
            AErrors := AErrors + CTStringsSeparator + LMessages.Text +
                                 CTStringsSeparator + lErrorCols.Text+ CTStringsSeparator;
          LMessages.Clear;
          lErrorCols.Clear;
        end;
      end
      else
      if (AContext = 'MaximumYield') then
      begin
        Result := ValidateMaximumYield(LMessages,lErrorCols);
        if (NOT Result) then
        begin
          if (lErrorCols.Count = 0) then
            AErrors := AErrors + LMessages.Text
          else
            AErrors := AErrors + CTStringsSeparator + LMessages.Text +
                                 CTStringsSeparator + lErrorCols.Text+ CTStringsSeparator;
          LMessages.Clear;
          lErrorCols.Clear;
        end;
      end
      else
      if (AContext = 'TargetPower') then
      begin
        Result := ValidateTargetPower(LMessages,lErrorCols);
        if (NOT Result) then
        begin
          if (lErrorCols.Count = 0) then
            AErrors := AErrors + LMessages.Text
          else
            AErrors := AErrors + CTStringsSeparator + LMessages.Text +
                                 CTStringsSeparator + lErrorCols.Text+ CTStringsSeparator;
          LMessages.Clear;
          lErrorCols.Clear;
        end;
      end
      else
      if (AContext = 'NrOfLoadCases') then
        Result := ValidateNrOfLoadCases(LMessages)
      else
      if (AContext = 'TargetRecurrenceInterval') then
        Result := ValidateTargetRecurrenceInterval(LMessages)
      else
      if (AContext = 'RunTitle1') then
        Result := ValidateRunTitle1(LMessages)
      else
      if (AContext = 'RunTitle2') then
        Result := ValidateRunTitle2(LMessages)
      else
      if (AContext = 'RunTitle3') then
        Result := ValidateRunTitle3(LMessages)
      else
      if (AContext = 'SequenceStartYear') then
        Result := ValidateStartYearSequence(LMessages)
      else
      if (AContext = 'PeriodsPerYear') then
        Result := ValidatePeriodPerYear(LMessages)
      else
      if (AContext = 'CalendarStartMonth') then
        Result := ValidateCalendarStartMonth(LMessages)
      else
      if (AContext = 'DecisionMonth') then
      begin
        Result := ValidateDecisionMonth(LMessages,lErrorCols);
        if (NOT Result) then
        begin
          if (lErrorCols.Count = 0) then
            AErrors := AErrors + LMessages.Text
          else
            AErrors := AErrors + CTStringsSeparator + LMessages.Text +
                                 CTStringsSeparator + lErrorCols.Text+ CTStringsSeparator;
          LMessages.Clear;
          lErrorCols.Clear;
        end;
      end
      else
      if (AContext = 'DecisionType') then
      begin
        Result := ValidateDecisionType(LMessages,lErrorCols);
        if (NOT Result) then
        begin
          if (lErrorCols.Count = 0) then
            AErrors := AErrors + LMessages.Text
          else
            AErrors := AErrors + CTStringsSeparator + LMessages.Text +
                                 CTStringsSeparator + lErrorCols.Text+ CTStringsSeparator;
          LMessages.Clear;
          lErrorCols.Clear;
        end;
      end
      else
      if (AContext = 'HydroPowerIndicator') then
      begin
        Result := ValidateHydroPowerIndicator(LMessages,lErrorCols);
        if (NOT Result) then
        begin
          if (lErrorCols.Count = 0) then
            AErrors := AErrors + LMessages.Text
          else
            AErrors := AErrors + CTStringsSeparator + LMessages.Text +
                                 CTStringsSeparator + lErrorCols.Text+ CTStringsSeparator;
          LMessages.Clear;
          lErrorCols.Clear;
        end;
      end

      else
      begin
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        if (NOT ValidateStartYearOther(LMessages)) then
          Result := FALSE;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateStartYearGregorian(LMessages)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateNumberOfYears(LMessages)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateNumberOfPeriods(LMessages)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateStartMonthNo(LMessages)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateMonthNames(LMessages,lErrorCols)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateMonthsDays(LMessages,lErrorCols)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateMultiplePeriods(LMessages)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateReduceSequences(LMessages)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateNumberOfSequences(LMessages)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateSequenceInAnalysis(LMessages,lErrorCols)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateStartType(LMessages)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateDebugStartPeriod(LMessages)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateDebugEndPeriod(LMessages)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateDebugLevel(LMessages)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateSummaryLevel(LMessages)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateTargetYield(LMessages,lErrorCols)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateMaximumYield(LMessages,lErrorCols)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateTargetPower(LMessages, lErrorCols)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateNrOfLoadCases(LMessages)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateCalcFirmYield(LMessages)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateHydroUnitsCode(LMessages)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateTargetRecurrenceInterval(LMessages)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateRunTitle1(LMessages)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateRunTitle2(LMessages)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateRunTitle3(LMessages)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateStartYearSequence(LMessages)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidatePeriodPerYear(LMessages)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateCalendarStartMonth(LMessages)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateDecisionMonth(LMessages,lErrorCols)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateDecisionType(LMessages,lErrorCols)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateHydroPowerIndicator(LMessages,lErrorCols)) then
            Result := FALSE;
        end;

      end;
      AErrors := AErrors + LMessages.Text;
    finally
      LMessages.Free;
    end;

  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.GetMinMaxYearFromHydrologyDB(
         const AFileName: string; var AStartYear, AEndYear: integer): boolean;
const OPNAME = 'TRunConfigurationData.GetMinMaxYearFromHydrologyDB';
var
  LSQLAgent: TRunConfigurationDataSQLAgent;
begin
  Result := False;
  try
    LSQLAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      Result := LSQLAgent.GetMinMaxYearFromHydrologyDB(AFileName,AStartYear, AEndYear);
    finally
      LSQLAgent.Free;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.GetMinMaxYearFromDemandDB(
         const AFileNumber: integer; var AStartYear, AEndYear: integer): boolean;
const OPNAME = 'TRunConfigurationData.GetMinMaxYearFromDemandDB';
var
  LSQLAgent: TRunConfigurationDataSQLAgent;
begin
  Result := False;
  try
    LSQLAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      Result := LSQLAgent.GetMinMaxYearFromDemandDB(AFileNumber,AStartYear, AEndYear);
    finally
      LSQLAgent.Free;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.CreateNrOfDecisionDates(ANrOfDecisionDate: integer): boolean;
const OPNAME = 'TRunConfigurationData.CreateNrOfDecisionDates';
var
  lSQLAgent: TRunConfigurationDataSQLAgent;
begin
  Result := False;
  try
    lSQLAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      Result := LSQLAgent.InsertNrOfDecisionDateSQL(ANrOfDecisionDate);
    finally
      LSQLAgent.Free;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;


function TRunConfigurationData.ValidateTargetYield(AErrorMessages: TStrings;AErrorColumns  : TStringList): Boolean;
const OPNAME = 'TRunConfigurationData.ValidateTargetYield';
var
  lIndex            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lResult           : Boolean;
  lFieldProperty    : TAbstractFieldProperty;
  lGap              : integer;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    lFieldProperty := FAppModules.FieldProperties.FieldProperty('TYield');
    lGap   := 0;
    for lIndex := lFieldProperty.ArrayLow to lFieldProperty.ArrayHigh do
    begin
      lMessage := '';
      if (TargetYieldByIndex[lIndex] = NullFloat) then
      begin
        if (lGap = 0) then
          lGap := 1;
      end
      else
      begin
        if (lGap = 1) then
        begin
          lResult := FALSE;
          lGap := 2;
          AErrorMessages.Add(FAppModules.Language.GetString('ContextValidation.InvalidNullValue'));
          if (lStopOnFirstError) then
            Break;
        end;
        if (NOT FAppModules.FieldProperties.ValidateFieldProperty
             ('TYield', FloatToStr(TargetYieldByIndex[lIndex]), lMessage, lIndex)) then
        begin
          lResult := FALSE;
          AErrorMessages.Add(lMessage);
          AErrorColumns.Add(IntToStr(lIndex));
          if (lStopOnFirstError) then
            Break;
        end;
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.ValidateMaximumYield (AErrorMessages: TStrings;AErrorColumns  : TStringList): Boolean;
const OPNAME = 'TRunConfigurationData.ValidateMaximumYield';
var
  lIndex            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lResult           : Boolean;
  lFieldProperty    : TAbstractFieldProperty;
  lGap              : integer;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    lFieldProperty := FAppModules.FieldProperties.FieldProperty('MYield');
    lGap := 0;
    for lIndex := lFieldProperty.ArrayLow to lFieldProperty.ArrayHigh do
    begin
      lMessage := '';
      if (MaximumYieldByIndex[lIndex] = NullFloat) then
      begin
        if (lGap = 0) then
          lGap := 1;
      end
      else
      begin
        if (lGap = 1) then
        begin
          lResult := FALSE;
          lGap := 2;
          AErrorMessages.Add(FAppModules.Language.GetString('ContextValidation.InvalidNullValue'));
          if (lStopOnFirstError) then
            Break;
        end;
        if (NOT FAppModules.FieldProperties.ValidateFieldProperty
             ('MYield', FloatToStr(MaximumYieldByIndex[lIndex]), lMessage, lIndex)) then
        begin
          lResult := FALSE;
          AErrorMessages.Add(lMessage);
          AErrorColumns.Add(IntToStr(lIndex));
          if (lStopOnFirstError) then
            Break;
        end;
        if (lResult OR (NOT lStopOnFirstError)) then
        begin
          if (MaximumYieldByIndex[lIndex] < TargetYieldByIndex[lIndex]) then
          begin
            lResult := FALSE;
            AErrorColumns.Add(IntToStr(lIndex));
            AErrorMessages.Add(FAppModules.Language.Getstring('MasterControl.ValidMaximumSystemYield'));
            if (lStopOnFirstError) then
              Break;
          end
        end;
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.ValidateTargetPower (AErrorMessages: TStrings;AErrorColumns  : TStringList): Boolean;
const OPNAME = 'TRunConfigurationData.ValidateTargetPower';
var
  lIndex            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lResult           : Boolean;
  lFieldProperty    : TAbstractFieldProperty;
  lGap              : integer;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    lFieldProperty := FAppModules.FieldProperties.FieldProperty('TPower');
    lGap   := 0;
    for lIndex := lFieldProperty.ArrayLow to lFieldProperty.ArrayHigh do
    begin
      lMessage := '';
      if (TargetPowerByIndex[lIndex] = NullFloat) then
      begin
        if (lGap = 0) then
          lGap := 1;
      end
      else
      begin
        if (lGap = 1) then
        begin
          lResult := FALSE;
          lGap := 2;
          AErrorMessages.Add(FAppModules.Language.GetString('ContextValidation.InvalidNullValue'));
          if (lStopOnFirstError) then
            Break;
        end;
        if (NOT FAppModules.FieldProperties.ValidateFieldProperty
              ('TPower', FloatToStr(TargetPowerByIndex[lIndex]), lMessage, lIndex)) then
        begin
          lResult := FALSE;
          AErrorMessages.Add(lMessage);
          AErrorColumns.Add(IntToStr(lIndex));
          if (lStopOnFirstError) then
            Break;
        end;
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.ValidateNrOfLoadCases (AErrorMessages: TStrings): Boolean;
const OPNAME = 'TRunConfigurationData.ValidateNrOfLoadCases';
var
  lMessage : string;
  lResult  : Boolean;
begin
  Result := FALSE;
  try
    if(FAppModules.Model.ModelName <> CYield) then
      Result := True
    else
    begin
      lMessage := '';
      lResult := FAppModules.FieldProperties.ValidateFieldProperty
                   ('LoadCasesCount', IntToStr(NumberOfActiveLoadCases), lMessage);
      if (NOT lResult) then
      begin
        AErrorMessages.Add(lMessage);
        Exit;
      end;
      Result := lResult;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.ValidateStartYearSequence(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TRunConfigurationData.ValidateStartYearSequence';
var
  lMessage    : string;
  LYearsCount : integer;
begin
   Result := FALSE;
   try
     if (NOT FAppModules.FieldProperties.ValidateFieldProperty
                ('SequenceStartYear', IntToStr(StartSequenceNumber), lMessage)) then
     begin
       AErrorMessages.Add(lMessage);
     end
     else
     begin
       LYearsCount := StartYearOther + YearsInAnalysis;
       if ((HistoricSequenceStartYear < StartYearOther) OR (HistoricSequenceStartYear > LYearsCount)) then
       begin
         AErrorMessages.Add(FAppModules.language.GetString('TRunConfigurationDialog.SequenceStartYearRange'));
       end;
     end;
     Result := TRUE;
   except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.ValidateHydroUnitsCode(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TRunConfigurationData.ValidateHydroUnitsCode';
var
  lMessage    : string;
begin
  Result := FALSE;
  try
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty
        ('HydroUnitsCode', HydroUnitsCode, lMessage)) then
    begin
      AErrorMessages.Add(lMessage);
    end
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.ValidatePeriodPerYear(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TRunConfigurationData.ValidatePeriodPerYear';
var
  lMessage    : string;
begin
  Result := FALSE;
  try
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty
        ('PeriodsPerYear', IntToStr(PeriodsPerYear), lMessage)) then
    begin
      AErrorMessages.Add(lMessage);
    end
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.ValidateCalendarStartMonth(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TRunConfigurationData.ValidateCalendarStartMonth';
var
  lMessage    : string;
begin
  if FAppModules.Model.ModelName = CYield then
  begin
    Result := True;
    Exit;
  end;
  Result := FALSE;
  try
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty
        ('CalendarStartMonth', IntToStr(CalendarStartMonth), lMessage)) then
    begin
      AErrorMessages.Add(lMessage);
    end
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.ValidateHydroPowerIndicator(AErrorMessages: TStrings;
                                                           AErrorColumns : TStringList): Boolean;
const OPNAME = 'TRunConfigurationData.ValidateHydroPowerIndicator';
var
  lIndex            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lResult           : Boolean;
  lFieldProperty    : TAbstractFieldProperty;
  lGap              : integer;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    lFieldProperty := FAppModules.FieldProperties.FieldProperty('HydroPowerIndicator');
    lGap   := 0;
    for lIndex := lFieldProperty.ArrayLow to lFieldProperty.ArrayHigh do
    begin
      lMessage := '';
      if (HydroPowerIndicatorByIndex[lIndex] = '') then
      begin
        if (lGap = 0) then
          lGap := 1;
      end
      else
      begin
        if (lGap = 1) then
        begin
          lResult := FALSE;
          lGap := 2;
          AErrorMessages.Add(FAppModules.Language.GetString('ContextValidation.InvalidNullValue'));
          if (lStopOnFirstError) then
            Break;
        end;
        if (NOT FAppModules.FieldProperties.ValidateFieldProperty
             ('HydroPowerIndicator', HydroPowerIndicatorByIndex[lIndex], lMessage, lIndex)) then
        begin
          lResult := FALSE;
          AErrorMessages.Add(lMessage);
          AErrorColumns.Add(IntToStr(lIndex));
          if (lStopOnFirstError) then
            Break;
        end;
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.ValidateDecisionMonth(AErrorMessages: TStrings;
                                                     AErrorColumns: TStringList): Boolean;
const OPNAME = 'TRunConfigurationData.ValidateDecisionMonth';
var
  lIndex            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lResult           : Boolean;
  lFieldProperty    : TAbstractFieldProperty;
  lGap              : integer;
begin
  if FAppModules.Model.ModelName = CYield then
  begin
    Result := True;
    Exit;
  end;
  
  Result := FALSE;
  try
    lResult := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    lFieldProperty := FAppModules.FieldProperties.FieldProperty('DecisionMonth');
    lGap   := 0;
    for lIndex := lFieldProperty.ArrayLow to lFieldProperty.ArrayHigh do
    begin
      lMessage := '';
      if (DecisionMonthByIndex[lIndex] = NullInteger) then
      begin
        if (lGap = 0) then
          lGap := 1;
      end
      else
      begin
        if (lGap = 1) then
        begin
          lResult := FALSE;
          lGap := 2;
          AErrorMessages.Add(FAppModules.Language.GetString('ContextValidation.InvalidNullValue'));
          if (lStopOnFirstError) then
            Break;
        end;
        if (NOT FAppModules.FieldProperties.ValidateFieldProperty
             ('DecisionMonth', IntToStr(DecisionMonthByIndex[lIndex]), lMessage, lIndex)) then
        begin
          lResult := FALSE;
          AErrorMessages.Add(lMessage);
          AErrorColumns.Add(IntToStr(lIndex));
          if (lStopOnFirstError) then
            Break;
        end;
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.ValidateDecisiontype(AErrorMessages: TStrings;
                                                           AErrorColumns: TStringList): Boolean;
const OPNAME = 'TRunConfigurationData.ValidateDecisiontype';
var
  lIndex            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lResult           : Boolean;
  lFieldProperty    : TAbstractFieldProperty;
  lGap              : integer;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    lFieldProperty := FAppModules.FieldProperties.FieldProperty('DecisionType');
    lGap   := 0;
    for lIndex := lFieldProperty.ArrayLow to lFieldProperty.ArrayHigh do
    begin
      lMessage := '';
      if (DecisionTypeByIndex[lIndex] = '') then
      begin
        if (lGap = 0) then
          lGap := 1;
      end
      else
      begin
        if (lGap = 1) then
        begin
          lResult := FALSE;
          lGap := 2;
          AErrorMessages.Add(FAppModules.Language.GetString('ContextValidation.InvalidNullValue'));
          if (lStopOnFirstError) then
            Break;
        end;
        if (NOT FAppModules.FieldProperties.ValidateFieldProperty
             ('DecisionType', DecisionTypeByIndex[lIndex], lMessage, lIndex)) then
        begin
          lResult := FALSE;
          AErrorMessages.Add(lMessage);
          AErrorColumns.Add(IntToStr(lIndex));
          if (lStopOnFirstError) then
            Break;
        end;
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.MonthNameByMonthNumber(AMonthNumber: Integer): WideString;
const OPNAME = 'TRunConfigurationData.MonthNameByMonthNumber';
var
  LPos,
  LIndex: integer;
  LMonthIndex : array[1..12] of integer;
begin
   Result := '';
   try

     if(AMonthNumber >= 1) and (AMonthNumber <= 12) then
     begin
       LPos := FAppModules.StudyArea.CalendarStartMonth;
       for LIndex := 1 to 12 do
       begin
         LMonthIndex[LIndex] := LPos;
         LPos := LPos + 1;
         if(LPos > 12) then
           LPos := 1;
       end;
       LPos := 0;
       for LIndex := 1 to 12 do
       begin
         if(LMonthIndex[LIndex] = AMonthNumber) then
         begin
           LPos := LIndex;
           Break;
         end;
       end;
       if(LPos > 0) then
         Result := Get_MonthNameByIndex(LPos);
     end;
   except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.CreateMemberObjects;
const OPNAME = 'TRunConfigurationData.CreateMemberObjects';
var
  LMonthNamesField,
  LMonthDays: TAbstractFieldProperty;
  LSequencesToBeAnalysedField: TAbstractFieldProperty;
  LTargetYieldField,
  LMaximumYieldField,
  LTargetPowerField: TAbstractFieldProperty;

  LDecisionMonths,
  LDecisionTypes,
  LHydroPowerIndicators : TAbstractFieldProperty;

begin
  inherited CreateMemberObjects;
  try
    LMonthNamesField := FAppModules.FieldProperties.FieldProperty('Month');
    if (LMonthNamesField = nil) then
      raise Exception.Create('Field (Month) not found in field properties');
    SetLength(FMonthNamesArray,LMonthNamesField.ArrayLength);

    LMonthDays := FAppModules.FieldProperties.FieldProperty('Days');
    if (LMonthDays = nil) then
      raise Exception.Create('Field (Days) not found in field properties');
    SetLength(FMonthsDaysArray,LMonthDays.ArrayLength);

    LSequencesToBeAnalysedField := FAppModules.FieldProperties.FieldProperty('Seq');
    if (LSequencesToBeAnalysedField = nil) then
       Raise Exception.Create('Field (Seq) not found in field properties');
       SetLength(FSequencesToBeAnalysedArray,LSequencesToBeAnalysedField.ArrayLength);

    LTargetYieldField := FAppModules.FieldProperties.FieldProperty('TYield');
    if (LTargetYieldField = nil) then
      Raise Exception.Create('Field (TYield) not found in field properties');
      SetLength(FTargetYieldArray,LTargetYieldField.ArrayLength);

    LMaximumYieldField := FAppModules.FieldProperties.FieldProperty('MYield');
    if (LMaximumYieldField = nil) then
      raise Exception.Create('Field (MYield) not found in field properties');
      SetLength(FMaximumYieldArray,LMaximumYieldField.ArrayLength);

    LTargetPowerField := FAppModules.FieldProperties.FieldProperty('TPower');
    if (LTargetPowerField = nil) then
      raise Exception.Create('Field (TPower) not found in field properties');
      SetLength(FTargetPowerArray,LTargetPowerField.ArrayLength);

    LDecisionMonths := FAppModules.FieldProperties.FieldProperty('DecisionMonth');
    if (LDecisionMonths = nil) then
      raise Exception.Create('Field (DecisionMonth) not found in field properties');
      SetLength(FDecisionMonths,LDecisionMonths.ArrayLength);

    LDecisionTypes := FAppModules.FieldProperties.FieldProperty('DecisionType');
    if (LDecisionTypes = nil) then
      raise Exception.Create('Field (DecisionType) not found in field properties');
      SetLength(FDecisionTypes,LDecisionTypes.ArrayLength);

    LHydroPowerIndicators := FAppModules.FieldProperties.FieldProperty('HydroPowerIndicator');
    if (LHydroPowerIndicators = nil) then
      raise Exception.Create('Field (HydroPowerIndicator) not found in field properties');
      SetLength(FHydroPowerIndicators,LHydroPowerIndicators.ArrayLength);

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.DestroyMemberObjects;
const OPNAME = 'TRunConfigurationData.DestroyMemberObjects';
begin
  try
    try
      Finalize(FMonthNamesArray);
      Finalize(FMonthsDaysArray);
      Finalize(FSequencesToBeAnalysedArray);
      Finalize(FTargetYieldArray);
      Finalize(FMaximumYieldArray);
      Finalize(FTargetPowerArray);
      Finalize(FDecisionTypes);
      Finalize(FDecisionMonths);
      Finalize(FHydroPowerIndicators);
    finally
      inherited DestroyMemberObjects;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.DeleteLoadCaseColumn(AIndex: integer);
const OPNAME = 'TRunConfigurationData.DeleteLoadCaseColumn';
var
  lIndexA : integer;
begin
  try
    for lIndexA := AIndex+1 to High(FTargetYieldArray) do
    begin
      WriteTargetYieldToDB (lIndexA-1, FTargetYieldArray[lIndexA]);
      WriteMaximumYieldToDB(lIndexA-1, FMaximumYieldArray[lIndexA]);
      WriteTargetPowerToDB (lIndexA-1, FTargetPowerArray[lIndexA]);
    end;
    WriteTargetYieldToDB (High(FTargetYieldArray), NullFloat);
    WriteMaximumYieldToDB(High(FTargetYieldArray), NullFloat);
    WriteTargetPowerToDB (High(FTargetYieldArray), NullFloat);
    if (AIndex <= FNrOfActiveLoadCases) then
      Set_NrOfActiveLoadCases(FNrOfActiveLoadCases - 1);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Initialise: boolean;
const OPNAME = 'TRunConfigurationData.Initialise';
var
  LIndex                 : integer;
  LMonthsNames,
  LMonthDays             : TAbstractFieldProperty;
  LSequencesToBeAnalysed : TAbstractFieldProperty;
  LTargetYieldField,
  LMaximumYieldField,
  LTargetPowerField      : TAbstractFieldProperty;

  LDecisionMonths,
  LDecisionTypes,
  LHydroPowerIndicators : TAbstractFieldProperty;
begin
  Result := inherited Initialise;
  try
    FStartYearGregorian          := 0;
    FStartYearOther              := 0;
    FYearsInAnalysis             := 0;
    FPeriodsInAnalysis           := 0;
    FStartMonthNumber            := 0;
    FCalculateHistoricFirmYield  := 0;
    FLimitOption                 := False;
    FRunSequenceType             := '';
    FRunTitle1                   := '';
    FRunTitle2                   := '';
    FRunTitle3                   := '';
    FMultiplePeriodLengths       := False;
    FReduceSequences             := False;
    FStartSequenceNumber         := 0;
    FGeneratedFlowFlag           := 0;
    FNumberOfSequencesInAnalysis := 0;
    FDebugLevel                  := 0;
    FStartDebugPeriod            := 0;
    FEndDebugPeriod              := 0;
    FOutputSummaryLevel          := 0;
    FCreateDataFile              := False;
    FCreateYieldFile             := False;
    FCreatePlotFile              := False;
    FTargetRecurrenceInterval    := 0;
    FNrOfActiveLoadCases         := 0;
    FHydroUnitsCode              := '';
    FRandomNumberOption          := 0;
    FParamFileName               := '';

    FDetailedOption              := False;
    FSupplyOption                := False;
    FAnnualSummary               := '';
    FEconomicOption              := False;
    FPlanningSummary             := False;
    FInputSummary                := False;
    FWaterQualityOption          := False;
    FPeriodsPerYear              := 0;
    FCalendarStartMonth          := 0;
    FShortTermPlanningOption     := '';
    FHydroPowerOption            := '';
    FAllocationControlOption     := '';
    FNrOfDecisionMonths          := 0;

    LDecisionMonths := FAppModules.FieldProperties.FieldProperty('DecisionMonth');
    for LIndex := LDecisionMonths.ArrayLow to LDecisionMonths.ArrayHigh do
      FDecisionMonths[LIndex] := 0;

    LDecisionTypes := FAppModules.FieldProperties.FieldProperty('DecisionType');
    for LIndex := LDecisionTypes.ArrayLow to LDecisionTypes.ArrayHigh do
      FDecisionTypes[LIndex] := '';

    LHydroPowerIndicators := FAppModules.FieldProperties.FieldProperty('HydroPowerIndicator');
    for LIndex := LHydroPowerIndicators.ArrayLow to LHydroPowerIndicators.ArrayHigh do
      FHydroPowerIndicators[LIndex] := '';

    LMonthsNames := FAppModules.FieldProperties.FieldProperty('Month');
    for LIndex := LMonthsNames.ArrayLow to LMonthsNames.ArrayHigh do
      FMonthNamesArray[LIndex] := '';

    LMonthDays  := FAppModules.FieldProperties.FieldProperty('Days');
    for LIndex := LMonthDays.ArrayLow to LMonthDays.ArrayHigh do
      FMonthsDaysArray[LIndex] := 0.0;

    LSequencesToBeAnalysed := FAppModules.FieldProperties.FieldProperty('Seq');
    for LIndex := LSequencesToBeAnalysed.ArrayLow to LSequencesToBeAnalysed.ArrayHigh do
      FSequencesToBeAnalysedArray[LIndex] := 0;

    LTargetYieldField := FAppModules.FieldProperties.FieldProperty('TYield');
    for LIndex := LTargetYieldField.ArrayLow to LTargetYieldField.ArrayHigh do
      FTargetYieldArray[LIndex] := NullFloat;

    LMaximumYieldField := FAppModules.FieldProperties.FieldProperty('MYield');
    for LIndex := LMaximumYieldField.ArrayLow to LMaximumYieldField.ArrayHigh do
      FMaximumYieldArray[LIndex] := NullFloat;

    LTargetPowerField := FAppModules.FieldProperties.FieldProperty('TPower');
    for LIndex := LTargetPowerField.ArrayLow to LTargetPowerField.ArrayHigh do
      FTargetPowerArray[LIndex] := NullFloat;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_CalculateHistoricFirmYield: Integer;
const OPNAME = 'TRunConfigurationData.Get_CalculateHistoricFirmYield';
var
  lFieldProperty : TAbstractFieldProperty;
begin
  Result := NullInteger;
  try
    lFieldProperty := FAppModules.FieldProperties.FieldProperty('CalcHistoryOpt');
    if (FCalculateHistoricFirmYield = NullInteger) then
      Result := FCalculateHistoricFirmYield
    else
      Result := StrToInt(FAppModules.Changes.GetParameterValue
                                            (lFieldProperty.FieldName,
                                            GetKeyValues(lFieldProperty.FieldName, ''),
                                            IntToStr(FCalculateHistoricFirmYield),
                                            ''));
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_CreateDataFile: WordBool;
const OPNAME = 'TRunConfigurationData.Get_CreateDataFile';
var
  lFieldProperty : TAbstractFieldProperty;
  lInValue       : string;
  lOutValue      : string;
begin
  Result := False;
  try
    lFieldProperty := FAppModules.FieldProperties.FieldProperty('SummaryOut');
    if (FCreateDataFile) then
      lInValue := '1'
    else
      lInValue := '0';
    lOutValue := FAppModules.Changes.GetParameterValue
                                      (lFieldProperty.FieldName,
                                      GetKeyValues(lFieldProperty.FieldName, ''),
                                      lInValue,
                                      '');
    Result := lOutValue = '1';
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_CreatePlotFile: WordBool;
const OPNAME = 'TRunConfigurationData.Get_CreatePlotFile';
var
  lFieldProperty : TAbstractFieldProperty;
  lInValue       : string;
  lOutValue      : string;
begin
  Result := False;
  try
    lFieldProperty := FAppModules.FieldProperties.FieldProperty('PlotOpt');
    if (FCreatePlotFile) then
      lInValue := 'Y'
    else
      lInValue := 'N';
    lOutValue := FAppModules.Changes.GetParameterValue
                                      (lFieldProperty.FieldName,
                                      GetKeyValues(lFieldProperty.FieldName, ''),
                                      lInValue,
                                      '');
    Result := lOutValue = 'Y';
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_CreateYieldFile: WordBool;
const OPNAME = 'TRunConfigurationData.Get_CreateYieldFile';
var
  lFieldProperty : TAbstractFieldProperty;
  lInValue       : string;
  lOutValue      : string;
begin
  Result := False;
  try
    lFieldProperty := FAppModules.FieldProperties.FieldProperty('StoreYield');
    if (FCreateYieldFile) then
      lInValue := '1'
    else
      lInValue := '0';
    lOutValue := FAppModules.Changes.GetParameterValue
                                      (lFieldProperty.FieldName,
                                      GetKeyValues(lFieldProperty.FieldName, ''),
                                      lInValue,
                                      '');
    Result := lOutValue = '1';
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_DebugLevel: Integer;
const OPNAME = 'TRunConfigurationData.Get_DebugLevel';
var
  lFieldProperty : TAbstractFieldProperty;
begin
  Result := NullInteger;
  try
    lFieldProperty := FAppModules.FieldProperties.FieldProperty('DebugLevel');
    if (FDebugLevel = NullInteger) then
      Result := FDebugLevel
    else
      Result := StrToInt(FAppModules.Changes.GetParameterValue
                                            (lFieldProperty.FieldName,
                                            GetKeyValues(lFieldProperty.FieldName, ''),
                                            IntToStr(FDebugLevel),
                                            ''));
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_EndDebugDate: TDateTime;
const OPNAME = 'TRunConfigurationData.Get_EndDebugDate';
begin
  Result := 0;
  try
    //Result := FEndDebugDate;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_EndDebugPeriod: Integer;
const OPNAME = 'TRunConfigurationData.Get_EndDebugPeriod';
begin
  Result := NullInteger;
  try
    Result := FEndDebugPeriod;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_GeneratedFlowFlag: Integer;
const OPNAME = 'TRunConfigurationData.Get_GeneratedFlowFlag';
var
  lFieldProperty : TAbstractFieldProperty;
begin
  Result := NullInteger;
  try
    lFieldProperty := FAppModules.FieldProperties.FieldProperty('StartType');
    if (FGeneratedFlowFlag = NullInteger) then
      Result := FGeneratedFlowFlag
    else
      Result := StrToInt(FAppModules.Changes.GetParameterValue
                                            (lFieldProperty.FieldName,
                                            GetKeyValues(lFieldProperty.FieldName, ''),
                                            IntToStr(FGeneratedFlowFlag),
                                            ''));
//    Result := FGeneratedFlowFlag;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_HistoricSequenceStartYear: Integer;
const OPNAME = 'TRunConfigurationData.Get_HistoricSequenceStartYear';
begin
  Result := NullInteger;
  try
    if (RunSequenceType = 'H') then
    begin
      Result := SequenceToBeAnalysedByIndex[1];
      Result := StartYearOther + Result - 1;
    end
    else
      Result := StartYearOther;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_LimitOption: WordBool;
const OPNAME = 'TRunConfigurationData.Get_LimitOption';
var
  lFieldProperty : TAbstractFieldProperty;
  lInValue       : string;
  lOutValue      : string;
begin
  Result := False;
  try
    lFieldProperty := FAppModules.FieldProperties.FieldProperty('LimitOpt');
    if (FLimitOption) then
      lInValue := '1'
    else
      lInValue := '0';
    lOutValue := FAppModules.Changes.GetParameterValue
                                      (lFieldProperty.FieldName,
                                      GetKeyValues(lFieldProperty.FieldName, ''),
                                      lInValue,
                                      '');
    Result := lOutValue = '1';
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_MonthDaysByIndex(Index: Integer): Double;
const OPNAME = 'TRunConfigurationData.Get_MonthDaysByIndex';
begin
  Result := NullFloat;
  try
    Result := FMonthsDaysArray[Index];
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_TotalDaysInAYear: double;
const OPNAME = 'TRunConfigurationData.Get_TotalDaysInAYear';
var
  LIndex: integer;
  LMonthDays: TAbstractFieldProperty;
begin
  Result := 0;
  try
    LMonthDays := FAppModules.FieldProperties.FieldProperty('Days');
    for LIndex := LMonthDays.ArrayLow to LMonthDays.ArrayHigh do
      Result := Result + FMonthsDaysArray[LIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_MonthDaysByName(const Name: WideString): Double;
const OPNAME = 'TRunConfigurationData.Get_MonthDaysByName';
var
  LIndex: integer;
  LMonthsNames: TAbstractFieldProperty;
begin
  Result := 0.0;
  try
    LMonthsNames  := FAppModules.FieldProperties.FieldProperty('Month');
    for LIndex := LMonthsNames.ArrayLow to LMonthsNames.ArrayHigh do
      if(FMonthNamesArray[LIndex] = Name) then
        Result := FMonthsDaysArray[LIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_MonthIndexByName(const Name: WideString): Integer;
const OPNAME = 'TRunConfigurationData.Get_MonthIndexByName';
var
  LIndex: integer;
  LMonthsNames: TAbstractFieldProperty;
begin
  Result := 0;
  try
    LMonthsNames  := FAppModules.FieldProperties.FieldProperty('Month');
    for LIndex := LMonthsNames.ArrayLow to LMonthsNames.ArrayHigh do
      if(FMonthNamesArray[LIndex] = Name) then
        Result := LIndex;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_MonthNameByIndex(Index: Integer): WideString;
const OPNAME = 'TRunConfigurationData.Get_MonthNameByIndex';
begin
  Result := '';
  try
    Result := FMonthNamesArray[Index];
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_MultiplePeriodLengths: WordBool;
const OPNAME = 'TRunConfigurationData.Get_MultiplePeriodLengths';
var
  lFieldProperty : TAbstractFieldProperty;
  lInValue       : string;
  lOutValue      : string;
begin
  Result := False;
  try
    lFieldProperty := FAppModules.FieldProperties.FieldProperty('MultPeriodOpt');
    if (FMultiplePeriodLengths) then
      lInValue := '1'
    else
      lInValue := '0';
    lOutValue := FAppModules.Changes.GetParameterValue
                                      (lFieldProperty.FieldName,
                                      GetKeyValues(lFieldProperty.FieldName, ''),
                                      lInValue,
                                      '');
    Result := lOutValue = '1';
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_NumberOfSequencesInAnalysis: Integer;
const OPNAME = 'TRunConfigurationData.Get_NumberOfSequencesInAnalysis';
begin
  Result := NullInteger;
  try
    Result := FNumberOfSequencesInAnalysis;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_OutputSummaryLevel: Integer;
const OPNAME = 'TRunConfigurationData.Get_OutputSummaryLevel';
var
  lFieldProperty : TAbstractFieldProperty;
begin
  Result := NullInteger;
  try
    lFieldProperty := FAppModules.FieldProperties.FieldProperty('SummaryLevel');
    if (FOutputSummaryLevel = NullInteger) then
      Result := FOutputSummaryLevel
    else
      Result := StrToInt(FAppModules.Changes.GetParameterValue
                                            (lFieldProperty.FieldName,
                                            GetKeyValues(lFieldProperty.FieldName, ''),
                                            IntToStr(FOutputSummaryLevel),
                                            ''));
//    Result := FOutputSummaryLevel;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_PeriodsInAnalysis: Integer;
const OPNAME = 'TRunConfigurationData.Get_PeriodsInAnalysis';
begin
  Result := NullInteger;
  try
    Result := FPeriodsInAnalysis;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_ReduceSequences: WordBool;
const OPNAME = 'TRunConfigurationData.Get_ReduceSequences';
var
  lFieldProperty : TAbstractFieldProperty;
  lInValue       : string;
  lOutValue      : string;
begin
  Result := False;
  try
    lFieldProperty := FAppModules.FieldProperties.FieldProperty('ReduceSeqOpt');
    if (FReduceSequences) then
      lInValue := '1'
    else
      lInValue := '0';
    lOutValue := FAppModules.Changes.GetParameterValue
                                      (lFieldProperty.FieldName,
                                      GetKeyValues(lFieldProperty.FieldName, ''),
                                      lInValue,
                                      '');
    Result := lOutValue = '1';
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_RunSequenceType: WideString;
const OPNAME = 'TRunConfigurationData.Get_RunSequenceType';
begin
  Result := '';
  try
    Result := FRunSequenceType;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_SequenceToBeAnalysedByIndex(Index: Integer): Integer;
const OPNAME = 'TRunConfigurationData.Get_SequenceToBeAnalysedByIndex';
var
  LSequencesToBeAnalysed: TAbstractFieldProperty;
begin
  Result := 0;
  try
    LSequencesToBeAnalysed := FAppModules.FieldProperties.FieldProperty('Seq');
    if (Index >= LSequencesToBeAnalysed.ArrayLow) and (Index <=  LSequencesToBeAnalysed.ArrayHigh) then
    begin
      {if FAppModules.GlobalData.COMServerMode then
      begin
        if(FNumberOfSequencesInAnalysis > 10) and (Trim(UpperCase(RunSequenceType)) <> 'H') then
        begin
          if(Index = 1) then
            Result := FSequencesToBeAnalysedArray[Index]-1
          else
            Result := 0;
        end
        else
          Result := FSequencesToBeAnalysedArray[Index];
      end
      else}
      Result := FSequencesToBeAnalysedArray[Index];
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_StartDebugDate: TDateTime;
const OPNAME = 'TRunConfigurationData.Get_StartDebugDate';
begin
  Result := 0;
  try
    //Result := FStartDebugDate;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_StartDebugPeriod: Integer;
const OPNAME = 'TRunConfigurationData.Get_StartDebugPeriod';
begin
  Result := NullInteger;
  try
    Result := FStartDebugPeriod;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_StartMonthNumber: Integer;
const OPNAME = 'TRunConfigurationData.Get_StartMonthNumber';
begin
  Result := NullInteger;
  try
    Result := FStartMonthNumber;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_StartSequenceNumber: Integer;
const OPNAME = 'TRunConfigurationData.Get_StartSequenceNumber';
begin
  Result := NullInteger;
  try
    Result := FStartSequenceNumber;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_StartYearGregorian: Integer;
const OPNAME = 'TRunConfigurationData.Get_StartYearGregorian';
begin
  Result := NullInteger;
  try
    Result := FStartYearGregorian;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_StartYearOther: Integer;
const OPNAME = 'TRunConfigurationData.Get_StartYearOther';
begin
  Result := NullInteger;
  try
    Result := FStartYearOther;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_TargetYieldByIndex(Index: Integer): Double;
const OPNAME = 'TRunConfigurationData.Get_TargetYieldByIndex';
var
  LTargetYieldField : TAbstractFieldProperty;
  LValue: string;
begin
  Result := 0.0;
  try
    LTargetYieldField := FAppModules.FieldProperties.FieldProperty('TYield');
    if (Index >= LTargetYieldField.ArrayLow) and (Index <= LTargetYieldField.ArrayHigh) then
    begin
      if (FTargetYieldArray[Index] = NullFloat) then
        Result := FTargetYieldArray[Index]
      else
      begin
        LValue := FAppModules.Changes.GetParameterValue('TYield',
                                              GetKeyValues('TYield', IntToStr(Index)),
                                              SmartFloatFormat(FTargetYieldArray[Index],LTargetYieldField.FieldWidth,LTargetYieldField.NumberOfDecimals),
                                              IntToStr(Index));
        Result  := StrToFloat(LValue);
      end;
    end;
//      Result := FTargetYieldArray[Index];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_MaximumYieldByIndex(Index: Integer): Double;
const OPNAME = 'TRunConfigurationData.Get_MaximumYieldByIndex';
var
  LMaximumField : TAbstractFieldProperty;
  lValue        : string;
begin
  Result := 0.0;
  try
    LMaximumField := FAppModules.FieldProperties.FieldProperty('MYield');
    if (Index >= LMaximumField.ArrayLow) and (Index <= LMaximumField.ArrayHigh) then
    begin
      if (FMaximumYieldArray[Index] = NullFloat) then
        Result := FMaximumYieldArray[Index]
      else
      begin
        lValue := FAppModules.Changes.GetParameterValue
                                         ('MYield',
                                         GetKeyValues('MYield', IntToStr(Index)),
                                         SmartFloatFormat(FMaximumYieldArray[Index], LMaximumField.FieldWidth, LMaximumField.NumberOfDecimals),
                                         IntToStr(Index));
        Result := StrToFloat(lValue);
      end;
    end;
//    Result := FMaximumYieldArray[Index];
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_TargetPowerByIndex(Index: Integer): Double;
const OPNAME = 'TRunConfigurationData.Get_TargetPowerByIndex';
var
  LTargetPowerField : TAbstractFieldProperty;
  lValue            : string;
begin
  Result := 0.0;
  try
    LTargetPowerField := FAppModules.FieldProperties.FieldProperty('TPower');
    if (Index >= LTargetPowerField.ArrayLow) and (Index <= LTargetPowerField.ArrayHigh) then
    begin
      if (FTargetPowerArray[Index] = NullFloat) then
        Result := FTargetPowerArray[Index]
      else
      begin
        lValue := FAppModules.Changes.GetParameterValue
                                              ('TPower',
                                              GetKeyValues('TPower', IntToStr(Index)),
                                              SmartFloatFormat(FTargetPowerArray[Index],LTargetPowerField.FieldWidth, LTargetPowerField.NumberOfDecimals),
                                              IntToStr(Index));
        Result := StrToFloat(lValue);
      end;
    end;
//      Result := FTargetPowerArray[Index];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_YearsInAnalysis: Integer;
const OPNAME = 'TRunConfigurationData.Get_YearsInAnalysis';
begin
  Result := NullInteger;
  try
    Result := FYearsInAnalysis;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_CalculateHistoricFirmYield(Value: Integer);
const OPNAME = 'TRunConfigurationData.Set_CalculateHistoricFirmYield';
var
  LLoadAgent    : TRunConfigurationDataSQLAgent;
  LContextData  : TStringList;
  LNewValue     : string;
  LOldValue     : string;
  lModelVersion : string;
begin
  try
    if (Value = FCalculateHistoricFirmYield) then Exit;
    LOldValue := IntToStr(FCalculateHistoricFirmYield);
    LNewValue := IntToStr(Value);

    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'CalcHistoryOpt', LNewValue, LOldValue, LContextData) then
        begin
          if (Value = 0) then
          begin
            FCalculateHistoricFirmYield := Value;
            if (DebugLevel >= 0) then
              DebugLevel := -3;
          end
          else
          begin
            lModelVersion := FAppModules.StudyArea.ModelVersion;
            if ((LModelVersion <> '6') AND (RunSequenceType = 'S')) then
              FCalculateHistoricFirmYield := 2
            else
              FCalculateHistoricFirmYield := 1;
            if (DebugLevel < 0) then
              DebugLevel := 0;
          end;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'CalcHistoryOpt',LOldValue, LNewValue);
          SortYield;
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_CreateDataFile(Value: WordBool);
const OPNAME = 'TRunConfigurationData.Set_CreateDataFile';
var
  LLoadAgent: TRunConfigurationDataSQLAgent;
  LContextData: TStringList;
  LCreateFile,LCreateDataFile: string;
begin
  try
    if FCreateDataFile then
      LCreateDataFile := '1'
    else
      LCreateDataFile := '0';

    if Value then
      LCreateFile := '1'
    else
      LCreateFile := '0';
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'SummaryOut', LCreateFile, LCreateDataFile, LContextData) then
        begin
          FCreateDataFile := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SummaryOut',LCreateDataFile,LCreateFile);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_CreatePlotFile(Value: WordBool);
const OPNAME = 'TRunConfigurationData.Set_CreatePlotFile';
var
  LLoadAgent: TRunConfigurationDataSQLAgent;
  LContextData: TStringList;
  LCreateFile,LCreatePlotFile: string;
begin
  try
    if FCreatePlotFile then
      LCreatePlotFile := 'Y'
    else
      LCreatePlotFile := 'N';

    if Value then
      LCreateFile := 'Y'
    else
      LCreateFile := 'N';
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'PlotOpt', LCreateFile, LCreatePlotFile, LContextData) then
        begin
          FCreatePlotFile := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'PlotOpt',LCreatePlotFile,LCreateFile);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_CreateYieldFile(Value: WordBool);
const OPNAME = 'TRunConfigurationData.Set_CreateYieldFile';
var
  LLoadAgent: TRunConfigurationDataSQLAgent;
  LContextData: TStringList;
  LCreateFile,LCreateYieldFile: string;
begin
  try
    if FCreateYieldFile then
      LCreateYieldFile := '1'
    else
      LCreateYieldFile := '0';

    if Value then
      LCreateFile := '1'
    else
      LCreateFile := '0';
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'StoreYield', LCreateFile, LCreateYieldFile, LContextData) then
        begin
          FCreateYieldFile := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'StoreYield',LCreateYieldFile,LCreateFile);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_DebugLevel(Value: Integer);
const OPNAME = 'TRunConfigurationData.Set_DebugLevel';
var
  LLoadAgent: TRunConfigurationDataSQLAgent;
  LContextData: TStringList;
  LOldValue: integer;
begin
  try
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'DebugLevel', IntToStr(Value), IntToStr(FDebugLevel), LContextData) then
        begin
          LOldValue := FDebugLevel;
          FDebugLevel := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'DebugLevel',IntToStr(LOldValue),IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_EndDebugDate(Value: TDateTime);
const OPNAME = 'TRunConfigurationData.Set_EndDebugDate';
begin
  try
    //FEndDebugDate := Value;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_EndDebugPeriod(Value: Integer);
const OPNAME = 'TRunConfigurationData.Set_EndDebugPeriod';
var
  LLoadAgent: TRunConfigurationDataSQLAgent;
  LContextData: TStringList;
  LOldValue: integer;
begin
  try
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'DebugFinal', IntToStr(Value), IntToStr(FEndDebugPeriod), LContextData) then
        begin
          LOldValue := FEndDebugPeriod;
          FEndDebugPeriod := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'DebugFinal',IntToStr(LOldValue),IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_GeneratedFlowFlag(Value: Integer);
const OPNAME = 'TRunConfigurationData.Set_GeneratedFlowFlag';
var
  LLoadAgent: TRunConfigurationDataSQLAgent;
  LContextData: TStringList;
  LOldValue: integer;
begin
  try
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'StartType', IntToStr(Value), IntToStr(FGeneratedFlowFlag), LContextData) then
        begin
          LOldValue := FGeneratedFlowFlag;
          FGeneratedFlowFlag := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'StartType',IntToStr(LOldValue),IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_HistoricSequenceStartYear(Value: Integer);
const OPNAME = 'TRunConfigurationData.Set_HistoricSequenceStartYear';
var
  LIndex: integer;
  LOldValue: integer;
  LNewValue: integer;
  LFieldProperty: TAbstractFieldProperty;
begin
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('Seq');
    LOldValue := SequenceToBeAnalysedByIndex[1];
    LNewValue := Value - StartYearOther +  1;
    SequenceToBeAnalysedByIndex[1] := LNewValue;
    for Lindex:= 2 to LFieldProperty.ArrayHigh do
    begin
      SequenceToBeAnalysedByIndex[Lindex] := 0;
    end;
    FAppModules.Model.StudyDataHasChanged(sdccEdit,'HistoricSequenceStartYear',IntToStr(LOldValue),IntToStr(Value));
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_LimitOption(Value: WordBool);
const OPNAME = 'TRunConfigurationData.Set_LimitOption';
var
  LLoadAgent: TRunConfigurationDataSQLAgent;
  LContextData: TStringList;
  LLimit,LLimitOption: string;
begin
  try
    if FLimitOption then
      LLimitOption := '1'
    else
      LLimitOption := '0';

    if Value then
      LLimit := '1'
    else
      LLimit := '0';
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'LimitOpt', LLimit, LLimitOption, LContextData) then
        begin
          FLimitOption := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'LimitOpt',LLimitOption,LLimit);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_MaximumYieldByIndex(Index: Integer; Value: Double);
const OPNAME = 'TRunConfigurationData.Set_MaximumYieldByIndex';
var
  LMaximumYieldField : TAbstractFieldProperty;
  lOldValue          : string;
begin
  try
    lOldValue := FloatToStr(FMaximumYieldArray[Index]);
    LMaximumYieldField := FAppModules.FieldProperties.FieldProperty('MYield');
    if (Index >= LMaximumYieldField.ArrayLow) AND (Index <=  LMaximumYieldField.ArrayHigh) then
    begin
      if (FMaximumYieldArray[Index] <> Value) then
      begin
        if (Value < 0) then
          DeleteLoadCaseColumn(Index)
        else
          WriteMaximumYieldToDB(Index, Value);
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'MYield', lOldValue, FloatToStr(Value));
        SortYield;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_MonthDaysByIndex(Index: Integer;Value: Double);
const OPNAME = 'TRunConfigurationData.Set_MonthDaysByIndex';
var
  LLoadAgent: TRunConfigurationDataSQLAgent;
  LContextData: TStringList;
begin
  try
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadMonthsDataContextData(LContextData, IntToStr(Index));
        if FAppModules.FieldProperties.UpdateFieldValue(
          'Days', FloatToStr(Value), FloatToStr(FMonthsDaysArray[Index]), LContextData) then
        begin
          FMonthsDaysArray[Index] := Value;
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_MonthDaysByName(const Name: WideString;Value: Double);
const OPNAME = 'TRunConfigurationData.Set_MonthDaysByName';
begin
  try
    raise Exception.Create('Setter not yet implemented');
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_MonthIndexByName(const Name: WideString; Value: Integer);
const OPNAME = 'TRunConfigurationData.Set_MonthIndexByName';
begin
  try
    raise Exception.Create('Setter not yet implemented');
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_MonthNameByIndex(Index: Integer;const Value: WideString);
const OPNAME = 'TRunConfigurationData.Set_MonthNameByIndex';
var
  LLoadAgent: TRunConfigurationDataSQLAgent;
  LContextData: TStringList;
  lOldValue: string;
begin
  try
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadMonthsDataContextData(LContextData, IntToStr(Index));
        if FAppModules.FieldProperties.UpdateFieldValue(
          'Month', Value, FMonthNamesArray[Index], LContextData) then
        begin
          lOldValue := FMonthNamesArray[Index];
          FMonthNamesArray[Index] := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'Month', lOldValue, FMonthNamesArray[Index]);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_MultiplePeriodLengths(Value: WordBool);
const OPNAME = 'TRunConfigurationData.Set_MultiplePeriodLengths';
var
  LLoadAgent: TRunConfigurationDataSQLAgent;
  LContextData: TStringList;
  LMultiple,LMultiplePeriodLengths: string;
begin
  try
    if FMultiplePeriodLengths then
      LMultiplePeriodLengths := '1'
    else
      LMultiplePeriodLengths := '0';

    if Value then
      LMultiple := '1'
    else
      LMultiple := '0';
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'MultPeriodOpt', LMultiple, LMultiplePeriodLengths, LContextData) then
        begin
          FMultiplePeriodLengths := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'',LMultiplePeriodLengths,LMultiple);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_NumberOfSequencesInAnalysis(Value: Integer);
const OPNAME = 'TRunConfigurationData.Set_NumberOfSequencesInAnalysis';
var
  LLoadAgent: TRunConfigurationDataSQLAgent;
  LFieldProperty: TAbstractFieldProperty;
  LContextData: TStringList;
  LOldValue: integer;
  Lindex : integer;
begin
  try
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        if (FRunSequenceType = 'H') then
          Value := 1;
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'HydroSeqCount', IntToStr(Value), IntToStr(FNumberOfSequencesInAnalysis), LContextData) then
        begin
          LOldValue := FNumberOfSequencesInAnalysis;
          FNumberOfSequencesInAnalysis := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'HydroSeqCount',IntToStr(LOldValue),IntToStr(Value));
          LFieldProperty := FAppModules.FieldProperties.FieldProperty('Seq');
          if(Value > LFieldProperty.ArrayHigh) then
          begin
            if (FRunSequenceType = 'H') then
              FStartSequenceNumber := 1
            else
              FStartSequenceNumber := 0;
            if (FRunSequenceType = 'H') then
              SequenceToBeAnalysedByIndex[LFieldProperty.ArrayLow] := 1
            else
              SequenceToBeAnalysedByIndex[LFieldProperty.ArrayLow] := 0;
            for Lindex := LFieldProperty.ArrayLow+1 to LFieldProperty.ArrayHigh do
              SequenceToBeAnalysedByIndex[Lindex] := 0;
          end
          else
          begin
            if(LOldValue > LFieldProperty.ArrayHigh) then
            begin
            if (FRunSequenceType = 'H') then
              SequenceToBeAnalysedByIndex[LFieldProperty.ArrayLow] := 1
            else
              SequenceToBeAnalysedByIndex[LFieldProperty.ArrayLow] := 0;
              for Lindex := LFieldProperty.ArrayLow+1 to LFieldProperty.ArrayHigh do
                SequenceToBeAnalysedByIndex[Lindex] := 0;
              LOldValue := 1;
            end;

            if(Value > LOldValue) then
            begin
              for Lindex := LOldValue+1 to Value do
              begin
                if(Lindex >= LFieldProperty.ArrayLow) and (Lindex <= LFieldProperty.ArrayHigh) then
                SequenceToBeAnalysedByIndex[Lindex] := Lindex-1;
              end;
            end
            else
            begin
              for Lindex := Value+1 to LOldValue do
              begin
                if(Lindex >= LFieldProperty.ArrayLow) and (Lindex <=LFieldProperty.ArrayHigh) then
                  SequenceToBeAnalysedByIndex[Lindex] := 0;
              end;
            end;
          end;
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_OutputSummaryLevel(Value: Integer);
const OPNAME = 'TRunConfigurationData.Set_OutputSummaryLevel';
var
  LLoadAgent: TRunConfigurationDataSQLAgent;
  LContextData: TStringList;
  LOldValue: integer;
begin
  try
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'SummaryLevel', IntToStr(Value), IntToStr(FOutputSummaryLevel), LContextData) then
        begin
          LOldValue := FOutputSummaryLevel;
          FOutputSummaryLevel := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SummaryLevel',IntToStr(LOldValue),IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_PeriodsInAnalysis(Value: Integer);
const OPNAME = 'TRunConfigurationData.Set_PeriodsInAnalysis';
var
  LLoadAgent   : TRunConfigurationDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : integer;
begin
  try
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'NumPeriods', IntToStr(Value), IntToStr(FPeriodsInAnalysis), LContextData) then
        begin
          LOldValue := FPeriodsInAnalysis;
          FPeriodsInAnalysis := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'NumPeriods',IntToStr(LOldValue), IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_ReduceSequences(Value: WordBool);
const OPNAME = 'TRunConfigurationData.Set_ReduceSequences';
var
  LLoadAgent         : TRunConfigurationDataSQLAgent;
  LContextData       : TStringList;
  LReduce            : string;
  LReduceSequences   : string;
  LIndexA,
  LIndexB            : integer;
  LTargetYield       : TTargetYieldArray;
  LMaximumYield      : TMaximumYieldArray;
  LTargetPower       : TTargetPowerArray;
  LNrOfLoadCases     : integer;
  LTempYield         : double;
  LTempMax           : double;
  LTempPower         : double;
  LTargetYieldField,
  LMaximumYieldField,
  LTargetPowerField: TAbstractFieldProperty;
begin
  try
    if (Value = FReduceSequences) then Exit;

    LTargetYieldField := FAppModules.FieldProperties.FieldProperty('TYield');
    if (LTargetYieldField = nil) then
      Raise Exception.Create('Field (TYield) not found in field properties');
      SetLength(LTargetYield,LTargetYieldField.ArrayLength);

    LMaximumYieldField := FAppModules.FieldProperties.FieldProperty('MYield');
    if (LMaximumYieldField = nil) then
      raise Exception.Create('Field (MYield) not found in field properties');
      SetLength(LMaximumYield,LMaximumYieldField.ArrayLength);

    LTargetPowerField := FAppModules.FieldProperties.FieldProperty('TPower');
    if (LTargetPowerField = nil) then
      raise Exception.Create('Field (TPower) not found in field properties');
      SetLength(LTargetPower,LTargetPowerField.ArrayLength);

    if FReduceSequences then
      LReduceSequences := '1'
    else
      LReduceSequences := '0';

    if Value then
      LReduce := '1'
    else
      LReduce := '0';

    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'ReduceSeqOpt', LReduce, LReduceSequences, LContextData) then
        begin
          FReduceSequences := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ReduceSeqOpt',LReduceSequences,LReduce);
          if FReduceSequences then
          begin
            LNrOfLoadCases     := Get_NrOfActiveLoadCases;
            LTargetYield       := FTargetYieldArray;
            LMaximumYield      := FMaximumYieldArray;
            LTargetPower       := FTargetPowerArray;
            for LIndexA := 1 to LNrOfLoadCases-1 do
            begin
              for LIndexB := LIndexA+1 to LNrOfLoadCases do
              begin
                if (LTargetYield[LIndexB] > LTargetYield[LIndexA]) then
                begin
                  LTempYield := LTargetYield[LIndexA];
                  LTempMax   := LMaximumYield[LIndexA];
                  LTempPower := LTargetPower[LIndexA];
                  LTargetYield[LIndexA]  := LTargetYield[LIndexB];
                  LMaximumYield[LIndexA] := LMaximumYield[LIndexB];
                  LTargetPower[LIndexA]  := LTargetPower[LIndexB];
                  LTargetYield[LIndexB]  := LTempYield;
                  LMaximumYield[LIndexB] := LTempMax;
                  LTargetPower[LIndexB]  := LTempPower;
                end;
              end;
            end;
            for LIndexA := LTargetYieldField.ArrayLow to LTargetYieldField.ArrayHigh do
            begin
              TargetYieldByIndex[LIndexA] := LTargetYield[LIndexA];
              MaximumYieldByIndex[LIndexA] := LMaximumYield[LIndexA];
              TargetPowerByIndex[LIndexA] := LTargetPower[LIndexA];
            end;
          end;
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_RunSequenceType(const Value: WideString);
const OPNAME = 'TRunConfigurationData.Set_RunSequenceType';
var
  LLoadAgent: TRunConfigurationDataSQLAgent;
  LContextData: TStringList;
  LMesg,
  LOldValue: string;
  LIndex: integer;
  LFieldProperty: TAbstractFieldProperty;
  LSpecifiedInflowFeatureList :ISpecifiedInflowFeatureList;
begin
  try
    if((Value = 'H') or (Value = 'S')) then
    begin
      LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadPeriodDataContextData(LContextData);
          if FAppModules.FieldProperties.UpdateFieldValue(
               'RunType', Value, FRunSequenceType, LContextData) then
          begin
            LOldValue := FRunSequenceType;
            FRunSequenceType := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'RunType',LOldValue,Value);
            LFieldProperty := FAppModules.FieldProperties.FieldProperty('Seq');
            if(Value = 'H') then
            begin
              SequenceToBeAnalysedByIndex[LFieldProperty.ArrayLow] := 1;
              for LIndex := LFieldProperty.ArrayLow+1 to LFieldProperty.ArrayHigh do
                SequenceToBeAnalysedByIndex[LIndex] := 0;
            end
            else
            begin
              for LIndex := LFieldProperty.ArrayLow to LFieldProperty.ArrayHigh do
                SequenceToBeAnalysedByIndex[LIndex] := 0;

            end;
            CalculateHistoricFirmYield   := 0;
            TargetRecurrenceInterval     := 0;
            MultiplePeriodLengths        := False;
            ReduceSequences              := False;
            LimitOption                  := False;
            GeneratedFlowFlag            := 0;
            NumberOfSequencesInAnalysis  := 1;

            LSpecifiedInflowFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.SpecifiedInflowFeatureList;
            if LSpecifiedInflowFeatureList <> nil then
            begin
              if (LSpecifiedInflowFeatureList.SpecifiedInflowFeatureCount>0) then
              begin
                LMesg := FAppModules.Language.GetString('Message.InflowFiles');
                if (MessageDlg(LMesg,mtConfirmation,[mbYes, mbNo],0) = mrYes) then
                begin
                  CopyInflowFilesOnTheDisk;
                  CopyInflowFilesInTheDatabase;                  
                end;
              end;
            end;
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_SequenceToBeAnalysedByIndex(Index,Value: Integer);
const OPNAME = 'TRunConfigurationData.Set_SequenceToBeAnalysedByIndex';
var
  LLoadAgent: TRunConfigurationDataSQLAgent;
  LContextData: TStringList;
  LOldValue : integer;
begin
  try
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LOldValue :=  FSequencesToBeAnalysedArray[Index];
        LLoadAgent.LoadMonthsDataContextData(LContextData, IntToStr(Index));
        if(Value = NullInteger) then
        begin
          if FAppModules.FieldProperties.UpdateFieldValue(
            'Seq', '', IntToStr(FSequencesToBeAnalysedArray[Index]), LContextData) then
          begin
            FSequencesToBeAnalysedArray[Index] := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'Seq',IntToStr(LOldValue),IntToStr(Value));
          end;
        end
        else
        begin
          if FAppModules.FieldProperties.UpdateFieldValue(
            'Seq', IntToStr(Value), IntToStr(FSequencesToBeAnalysedArray[Index]), LContextData) then
          begin
            FSequencesToBeAnalysedArray[Index] := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'Seq',IntToStr(LOldValue),IntToStr(Value));
          end;
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_StartDebugDate(Value: TDateTime);
const OPNAME = 'TRunConfigurationData.Set_StartDebugDate';
begin
  try
    //FStartDebugDate := Value;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_StartDebugPeriod(Value: Integer);
const OPNAME = 'TRunConfigurationData.Set_StartDebugPeriod';
var
  LLoadAgent: TRunConfigurationDataSQLAgent;
  LContextData: TStringList;
  LOldValue: integer;
begin
  try
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'DebugInit', IntToStr(Value), IntToStr(FStartDebugPeriod), LContextData) then
        begin
          LOldValue := FStartDebugPeriod;
          FStartDebugPeriod := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'DebugInit',IntToStr(LOldValue),IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
   except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_StartMonthNumber(Value: Integer);
const OPNAME = 'TRunConfigurationData.Set_StartMonthNumber';
var
  LLoadAgent   : TRunConfigurationDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : integer;
begin
  try
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'StartMonthNo', IntToStr(Value), IntToStr(FStartMonthNumber), LContextData) then
        begin
          LOldValue := FStartMonthNumber;
          FStartMonthNumber := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'StartMonthNo',IntToStr(LOldValue), IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
   except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_StartSequenceNumber(Value: Integer);
const OPNAME = 'TRunConfigurationData.Set_StartSequenceNumber';
var
  LIndex: integer;
  LOldValue: integer;
  LSequencesToBeAnalysed: TAbstractFieldProperty;
begin
  try
    SequenceToBeAnalysedByIndex[1] := Value;
    LSequencesToBeAnalysed := FAppModules.FieldProperties.FieldProperty('Seq');
    for Lindex:= LSequencesToBeAnalysed.ArrayLow+1 to LSequencesToBeAnalysed.ArrayHigh do
    begin
      SequenceToBeAnalysedByIndex[Lindex] := 0;
    end;
    LOldValue := FStartSequenceNumber;
    FStartSequenceNumber := Value;
    FAppModules.Model.StudyDataHasChanged(sdccEdit,'StartSequenceNumber',intToStr(LOldValue),IntToStr(Value));
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_StartYearGregorian(Value: Integer);
const OPNAME = 'TRunConfigurationData.Set_StartYearGregorian';
var
  LLoadAgent   : TRunConfigurationDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : integer;
begin
  try
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'StartYearG', IntToStr(Value), IntToStr(FStartYearGregorian), LContextData) then
        begin
          LOldValue := FStartYearGregorian;
          FStartYearGregorian := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'StartYearG',IntToStr(LOldValue), IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_StartYearOther(Value: Integer);
const OPNAME = 'TRunConfigurationData.Set_StartYearOther';
var
  LLoadAgent   : TRunConfigurationDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : integer;
  LOldHistoricSequenceStartYear: integer;
begin
  try
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'StartYearO', IntToStr(Value), IntToStr(FStartYearOther), LContextData) then
        begin
          LOldHistoricSequenceStartYear := HistoricSequenceStartYear;
          LOldValue := FStartYearOther;
          FStartYearOther := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'StartYearO',IntToStr(LOldValue), IntToStr(Value));
          if(LOldHistoricSequenceStartYear < FStartYearOther) then
             HistoricSequenceStartYear := FStartYearOther;
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_TargetPowerByIndex(Index: Integer; Value: Double);
const OPNAME = 'TRunConfigurationData.Set_TargetPowerByIndex';
var
 LTargetPowerField : TAbstractFieldProperty;
 lOldValue         : string;
begin
  try
    lOldValue := FloatToStr(FTargetPowerArray[Index]);
    LTargetPowerField := FAppModules.FieldProperties.FieldProperty('TPower');
    if (Index >= LTargetPowerField.ArrayLow) and (Index <=  LTargetPowerField.ArrayHigh) then
    begin
      if (FTargetPowerArray[Index] <> Value) then
      begin
        if (Value < 0) then
          DeleteLoadCaseColumn(Index)
        else
          WriteTargetPowerToDB(Index, Value);
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'TPower', lOldValue, FloatToStr(Value));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;end;

procedure TRunConfigurationData.Set_TargetYieldByIndex(Index: Integer; Value: Double);
const OPNAME = 'TRunConfigurationData.Set_TargetYieldByIndex';
var
  LTargetYieldField : TAbstractFieldProperty;
  lOldValue         : string;
begin
  try
    lOldValue := SmartFloatFormat(FTargetYieldArray[Index],6,2);
    LTargetYieldField := FAppModules.FieldProperties.FieldProperty('TYield');
    if (Index >= LTargetYieldField.ArrayLow) and (Index <= LTargetYieldField.ArrayHigh) then
    begin
      if (FTargetYieldArray[Index] <> Value) then
      begin
        if (Value < 0) then
          DeleteLoadCaseColumn(Index)
        else
          WriteTargetYieldToDB(Index, Value);
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'TYield', lOldValue, FloatToStr(Value));
        SortYield;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_YearsInAnalysis(Value: Integer);
const OPNAME = 'TRunConfigurationData.Set_YearsInAnalysis';
var
  LLoadAgent      : TRunConfigurationDataSQLAgent;
  LContextData    : TStringList;
  LOldValue       : integer;
begin
  try
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'YearsCount', IntToStr(Value), IntToStr(FYearsInAnalysis), LContextData) then
        begin
          LOldValue := FYearsInAnalysis;
          FYearsInAnalysis  := Value;
          PeriodsInAnalysis := Value * 12;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'YearsCount',IntToStr(LOldValue), IntToStr(Value));
        end;
        if (FYearsInAnalysis > 10) then
        begin
          MultiplePeriodLengths := False;
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_HasBeenPopulated: WordBool;
const OPNAME = 'TRunConfigurationData.Get_HasBeenPopulated';
begin
  Result := FALSE;
  try
    Result := OutputDataHasBeenPopulated;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_OutputDataHasBeenPopulated: WordBool;
const OPNAME = 'TRunConfigurationData.Get_OutputDataHasBeenPopulated';
begin
  Result := False;
  try
    Result :=   FOutputDataHasBeenPopulated;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_TargetRecurrenceInterval: Integer;
const OPNAME = 'TRunConfigurationData.Get_TargetRecurrenceInterval';
begin
  Result := 0;
  try
    Result := FTargetRecurrenceInterval;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_TargetRecurrenceInterval(Value: Integer);
const OPNAME = 'TRunConfigurationData.Set_TargetRecurrenceInterval';
var
  LLoadAgent   : TRunConfigurationDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : integer;
begin
  try
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData  := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue('TargetRecurrenceInterval', IntToStr(Value),
                       IntToStr(FTargetRecurrenceInterval), LContextData) then
        begin
          LOldValue :=  FTargetRecurrenceInterval;
          FTargetRecurrenceInterval := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit, 'TargetRecurrenceInterval', IntToStr(LOldValue),
                        IntToStr(Value));
        end;
      finally;
        LContextData.Free;
      end;
    finally;
      LLoadAgent.Free;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_YieldRunTitle1: WideString;
const OPNAME = 'TRunConfigurationData.Get_YieldRunTitle1';
var
  lFieldProperty : TAbstractFieldProperty;
begin
  Result := '';
  try
    lFieldProperty := FAppModules.FieldProperties.FieldProperty('Title1');
    Result         := FAppModules.Changes.GetParameterValue
                                      (lFieldProperty.FieldName,
                                      GetKeyValues(lFieldProperty.FieldName, ''),
                                      FRunTitle1,
                                      '');
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_YieldRunTitle2: WideString;
const OPNAME = 'TRunConfigurationData.Get_YieldRunTitle2';
var
  lFieldProperty : TAbstractFieldProperty;
begin
  Result := '';
  try
    lFieldProperty := FAppModules.FieldProperties.FieldProperty('Title2');
    Result         := FAppModules.Changes.GetParameterValue
                                      (lFieldProperty.FieldName,
                                      GetKeyValues(lFieldProperty.FieldName, ''),
                                      FRunTitle2,
                                      '');
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_YieldRunTitle3: WideString;
const OPNAME = 'TRunConfigurationData.Get_YieldRunTitle3';
var
  lFieldProperty : TAbstractFieldProperty;
begin
  Result := '';
  try
    lFieldProperty := FAppModules.FieldProperties.FieldProperty('Title3');
    Result         := FAppModules.Changes.GetParameterValue
                                      (lFieldProperty.FieldName,
                                      GetKeyValues(lFieldProperty.FieldName, ''),
                                      FRunTitle3,
                                      '');
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_YieldRunTitle1(const Value: WideString);
const OPNAME = 'TRunConfigurationData.Set_YieldRunTitle1';
var
  LLoadAgent   : TRunConfigurationDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData  := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue('Title1', Value, LOldValue, LContextData) then
        begin
          LOldValue  :=  FRunTitle1;
          FRunTitle1 := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit, 'Title1', LOldValue,Value);
        end;
      finally;
        LContextData.Free;
      end;
    finally;
      LLoadAgent.Free;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_YieldRunTitle2(const Value: WideString);
const OPNAME = 'TRunConfigurationData.Set_YieldRunTitle2';
var
  LLoadAgent   : TRunConfigurationDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue('Title2', Value, LOldValue, LContextData) then
        begin
          LOldValue  := FRunTitle2;
          FRunTitle2 := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit, 'Title2', LOldValue, Value);
        end;
      finally;
        LContextData.Free;
      end;
    finally;
      LLoadAgent.Free;
    end;
 except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_YieldRunTitle3(const Value: WideString);
const OPNAME = 'TRunConfigurationData.Set_YieldRunTitle3';
var
  LLoadAgent   : TRunConfigurationDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue('Title3', Value, LOldValue, LContextData) then
        begin
          LOldValue  := FRunTitle3;
          FRunTitle3 := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'Title3', LOldValue, Value);
        end;
      finally;
        LContextData.Free;
      end;
    finally;
      LLoadAgent.Free;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

{procedure TRunConfigurationData.DeleteLoadCaseColumn(AIndex: integer);
const OPNAME = 'TRunConfigurationData.DeleteLoadCaseColumn';
var
  lIndexA : integer;
begin
  try
    for lIndexA := AIndex+1 to High(FTargetYieldArray) do
    begin
      WriteTargetYieldToDB (lIndexA-1, FTargetYieldArray[lIndexA]);
      WriteMaximumYieldToDB(lIndexA-1, FMaximumYieldArray[lIndexA]);
      WriteTargetPowerToDB (lIndexA-1, FTargetPowerArray[lIndexA]);
    end;
    WriteTargetYieldToDB (High(FTargetYieldArray), NullFloat);
    WriteMaximumYieldToDB(High(FTargetYieldArray), NullFloat);
    WriteTargetPowerToDB (High(FTargetYieldArray), NullFloat);
    if (AIndex <= FNrOfActiveLoadCases) then
      Set_NrOfActiveLoadCases(FNrOfActiveLoadCases - 1);
  except on E: Exception do HandleError(E, OPNAME); end;
end;}

procedure TRunConfigurationData.WriteMaximumYieldToDB(AIndex: integer; AMaximumYield: double);
const OPNAME = 'TRunConfigurationData.WriteMaximumYieldToDB';
var
  LLoadAgent: TRunConfigurationDataSQLAgent;
  LContextData: TStringList;
begin
  try
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadMonthsDataContextData(LContextData, IntToStr(AIndex));
        if(AMaximumYield >= 0) then
        begin
          if FAppModules.FieldProperties.UpdateFieldValue(
            'MYield', FloatToStr(AMaximumYield), FloatToStr(FMaximumYieldArray[AIndex]), LContextData) then
          begin
            FMaximumYieldArray[AIndex] := AMaximumYield;
          end;
        end
        else
        begin
          if (FMaximumYieldArray[AIndex] >= 0) then
          begin
            if FAppModules.FieldProperties.UpdateFieldValue(
              'MYield', '', FloatToStr(FMaximumYieldArray[AIndex]), LContextData) then
            begin
              FMaximumYieldArray[AIndex] := AMaximumYield;
            end;
          end
          else
            FMaximumYieldArray[AIndex] := AMaximumYield;
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.WriteTargetPowerToDB(AIndex: integer;ATargetPower: double);
const OPNAME = 'TRunConfigurationData.WriteTargetPowerToDB';
var
  LLoadAgent: TRunConfigurationDataSQLAgent;
  LContextData: TStringList;
begin
  try
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadMonthsDataContextData(LContextData, IntToStr(AIndex));
        if(ATargetPower >= 0) then
        begin
          if FAppModules.FieldProperties.UpdateFieldValue(
            'TPower', FloatToStr(ATargetPower), FloatToStr(FTargetPowerArray[AIndex]), LContextData) then
          begin
            FTargetPowerArray[AIndex] := ATargetPower;
          end;
        end
        else
        begin
          if (FTargetPowerArray[AIndex] >= 0) then
          begin
            if FAppModules.FieldProperties.UpdateFieldValue(
              'TPower', '', FloatToStr(FTargetPowerArray[AIndex]), LContextData) then
            begin
              FTargetPowerArray[AIndex] := ATargetPower;
            end;
          end
          else
            FTargetPowerArray[AIndex] := ATargetPower;
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.WriteTargetYieldToDB(AIndex: integer;ATargetYield: double);
const OPNAME = 'TRunConfigurationData.WriteTargetYieldToDB';
var
  LLoadAgent: TRunConfigurationDataSQLAgent;
  LContextData: TStringList;
begin
  try
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadMonthsDataContextData(LContextData, IntToStr(AIndex));
        if (ATargetYield >= 0) then
        begin
          if FAppModules.FieldProperties.UpdateFieldValue(
            'TYield', FloatToStr(ATargetYield), SmartFloatFormat(FTargetYieldArray[AIndex],6,2), LContextData) then
          begin
            FTargetYieldArray[AIndex] := ATargetYield;
          end;
        end
        else
        begin
          if (FTargetYieldArray[AIndex] >= 0) then
          begin
            if FAppModules.FieldProperties.UpdateFieldValue(
              'TYield', '', SmartFloatFormat(FTargetYieldArray[AIndex],6,2), LContextData) then
            begin
              FTargetYieldArray[AIndex] := ATargetYield;
            end;
          end
          else
            FTargetYieldArray[AIndex] := ATargetYield;
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_NrOfActiveLoadCases: integer;
const OPNAME = 'TRunConfigurationData.Get_NrOfActiveLoadCases';
begin
  Result := 0;
  try
    Result := FNrOfActiveLoadCases;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_NrOfActiveLoadCases(const Value: integer);
const OPNAME = 'TRunConfigurationData.Set_NrOfActiveLoadCases';
var
  LLoadAgent: TRunConfigurationDataSQLAgent;
  LContextData: TStringList;
  LOldValue: integer;
begin
  try
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'LoadCasesCount', IntToStr(Value), IntToStr(FNrOfActiveLoadCases), LContextData) then
        begin
          LOldValue := FNrOfActiveLoadCases;
          FNrOfActiveLoadCases := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'LoadCasesCount',IntToStr(LOldValue), IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.ActivateLoadCase (AIndex  : integer);
const OPNAME = 'TRunConfigurationData.ActivateLoadCase';
var
  lIndexA     : integer;
  lTempTarget : double;
  lTempMax    : double;
  lTempPower  : double;
  lFound      : Boolean;
  LTargetYieldField: TAbstractFieldProperty;
begin
  try
    LTargetYieldField := FAppModules.FieldProperties.FieldProperty('TYield');
    if (AIndex >= LTargetYieldField.ArrayLow) and (AIndex <=  LTargetYieldField.ArrayHigh) then
    begin
      if FReduceSequences then
      begin
        lTempTarget := FTargetYieldArray[AIndex];
        lTempMax    := FMaximumYieldArray[AIndex];
        lTempPower  := FTargetPowerArray[AIndex];
        lIndexA := AIndex - 1;
        lFound  := FALSE;
        while ((NOT lFound) AND (lIndexA > 0)) do
        begin
          if ((lIndexA > FNrOfActiveLoadCases) OR
              (lTempTarget > FTargetYieldArray[lIndexA])) then
          begin
            Set_TargetYieldByIndex (lIndexA+1, FTargetYieldArray[lIndexA]);
            Set_MaximumYieldByIndex(lIndexA+1, FMaximumYieldArray[lIndexA]);
            Set_TargetPowerByIndex (lIndexA+1, FTargetPowerArray[lIndexA]);
            lIndexA := lIndexA - 1;
          end
          else
            lFound := TRUE;
        end;
        Set_TargetYieldByIndex (lIndexA+1, lTempTarget);
        Set_MaximumYieldByIndex(lIndexA+1, lTempMax);
        Set_TargetPowerByIndex (lIndexA+1, lTempPower);
        Set_NrOfActiveLoadCases(FNrOfActiveLoadCases + 1);
      end
      else
      begin
        lTempTarget := FTargetYieldArray[AIndex];
        lTempMax    := FMaximumYieldArray[AIndex];
        lTempPower  := FTargetPowerArray[AIndex];
        for lIndexA := AIndex - 1 downto FNrOfActiveLoadCases + 1 do
        begin
          Set_TargetYieldByIndex (lIndexA+1, FTargetYieldArray[lIndexA]);
          Set_MaximumYieldByIndex(lIndexA+1, FMaximumYieldArray[lIndexA]);
          Set_TargetPowerByIndex (lIndexA+1, FTargetPowerArray[lIndexA]);
        end;
        Set_NrOfActiveLoadCases(FNrOfActiveLoadCases + 1);
        Set_TargetYieldByIndex (FNrOfActiveLoadCases, lTempTarget);
        Set_MaximumYieldByIndex(FNrOfActiveLoadCases, lTempMax);
        Set_TargetPowerByIndex (FNrOfActiveLoadCases, lTempPower);
      end;
      SortYield;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.DeactivateLoadCase (AIndex  : integer);
const OPNAME = 'TRunConfigurationData.DeactivateLoadCase';
var
  lIndexA     : integer;
  lTempTarget : double;
  lTempMax    : double;
  lTempPower  : double;
  LTargetYieldField: TAbstractFieldProperty;
begin
  try
    LTargetYieldField := FAppModules.FieldProperties.FieldProperty('TYield');
    if (AIndex >= LTargetYieldField.ArrayLow) and (AIndex <=  LTargetYieldField.ArrayHigh) then
    begin
      lTempTarget := FTargetYieldArray[AIndex];
      lTempMax    := FMaximumYieldArray[AIndex];
      lTempPower  := FTargetPowerArray[AIndex];
      for lIndexA := AIndex + 1 to FNrOfActiveLoadCases do
      begin
        Set_TargetYieldByIndex (lIndexA-1, FTargetYieldArray[lIndexA]);
        Set_MaximumYieldByIndex(lIndexA-1, FMaximumYieldArray[lIndexA]);
        Set_TargetPowerByIndex (lIndexA-1, FTargetPowerArray[lIndexA]);
      end;
      Set_TargetYieldByIndex (FNrOfActiveLoadCases, lTempTarget);
      Set_MaximumYieldByIndex(FNrOfActiveLoadCases, lTempMax);
      Set_TargetPowerByIndex (FNrOfActiveLoadCases, lTempPower);
      Set_NrOfActiveLoadCases(FNrOfActiveLoadCases - 1);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_HydroUnitsCode: WideString;
const OPNAME = 'TRunConfigurationData.Get_HydroUnitsCode';
begin
  Result := '';
  try
    Result := FHydroUnitsCode;
    if(Result = '') then
      Result := 'MCM';
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_HydroUnitsCode(const Value: WideString);
const OPNAME = 'TRunConfigurationData.Set_HydroUnitsCode';
var
  LLoadAgent   : TRunConfigurationDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData  := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue('HydroUnitsCode', Value, LOldValue, LContextData) then
        begin
          LOldValue  :=  FHydroUnitsCode;
          FHydroUnitsCode := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit, 'HydroUnitsCode', LOldValue,Value);
        end;
      finally;
        LContextData.Free;
      end;
    finally;
      LLoadAgent.Free;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_ParamFileName: WideString;
const OPNAME = 'TRunConfigurationData.Get_ParamFileName';
begin
  Result := '';
  try
    Result := FParamFileName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_ParamFileName(const Value: WideString);
const OPNAME = 'TRunConfigurationData.Set_ParamFileName';
var
  LLoadAgent   : TRunConfigurationDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData  := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue('ParamFile', Value, LOldValue,
                                                         LContextData) then
        begin
          LOldValue  :=  FParamFileName;
          FParamFileName := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit, 'ParamFile', LOldValue,Value);
        end;
      finally;
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_RandomNumberOption: Integer;
const OPNAME = 'TRunConfigurationData.Get_RandomNumberOption';
begin
  Result := 0;
  try
    Result := FRandomNumberOption;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_RandomNumberOption(Value: Integer);
const OPNAME = 'TRunConfigurationData.Set_RandomNumberOption';
var
  LLoadAgent   : TRunConfigurationDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : integer;
begin
  try
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'RandomNumberOption', IntToStr(Value), IntToStr(FRandomNumberOption), LContextData) then
        begin
          LOldValue := FRandomNumberOption;
          FRandomNumberOption := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'RandomNumberOption',IntToStr(LOldValue), IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.PopulatePeriodData(AStartYearGregorian,AStartYearOther, AYearsInAnalysis, APeriodsInAnalysis,
                                                  AStartMonthNumber: integer): boolean;
const OPNAME = 'TRunConfigurationData.PopulatePeriodData';
begin
  Result := False;
  try
    FStartYearGregorian           := AStartYearGregorian;
    FStartYearOther               := AStartYearOther;
    FYearsInAnalysis              := AYearsInAnalysis;
    FPeriodsInAnalysis            := APeriodsInAnalysis;
    FStartMonthNumber             :=AStartMonthNumber;
    FOutputDataHasBeenPopulated   := TRUE;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.PopulateRunInfoData(ACalculateHistoricFirmYield, ATargetRecurrenceInterval: integer;ALimitOption: boolean;
                                                   ARunSequenceType, ATitle1, ATitle2,ATitle3: string;
                                                   ARandomNumberOption : integer; AParamFileName : string): boolean;
const OPNAME = 'TRunConfigurationData.PopulateRunInfoData';
begin
  Result := False;
  try
    FCalculateHistoricFirmYield := ACalculateHistoricFirmYield;
    FTargetRecurrenceInterval   := ATargetRecurrenceInterval;
    FLimitOption                := ALimitOption;
    FRunSequenceType            := ARunSequenceType;
    FRunTitle1                  := ATitle1;
    FRunTitle2                  := ATitle2;
    FRunTitle3                  := ATitle3;
    FRandomNumberOption         := ARandomNumberOption;
    FParamFileName              := AParamFileName;
    FOutputDataHasBeenPopulated := TRUE;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.PopulateStochasticData(AMultiplePeriodLengths, AReduceSequences: boolean; AGeneratedFlowFlag,
                                                      ANumberOfSequencesInAnalysis: integer): boolean;
const OPNAME = 'TRunConfigurationData.PopulateStochasticData';
begin
  Result := False;
  try
    FMultiplePeriodLengths       := AMultiplePeriodLengths;
    FReduceSequences             := AReduceSequences;
    FNumberOfSequencesInAnalysis := ANumberOfSequencesInAnalysis;
    FGeneratedFlowFlag           := AGeneratedFlowFlag;
    FOutputDataHasBeenPopulated  := TRUE;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.PopulateDebugInfoData(ADebugLevel,AStartDebugPeriod, AEndDebugPeriod: integer): boolean;
const OPNAME = 'TRunConfigurationData.PopulateDebugInfoData';
begin
  Result := inherited Initialise;
  try
    FDebugLevel                 := ADebugLevel;
    FStartDebugPeriod           := AStartDebugPeriod;
    FEndDebugPeriod             := AEndDebugPeriod;
    FOutputDataHasBeenPopulated := TRUE;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;

end;

function TRunConfigurationData.PopulateOutputConfigurationData(AOutputSummaryLevel: integer; ACreateDataFile, ACreateYieldFile,
                                                               ACreatePlotFile: boolean): boolean;
const OPNAME = 'TRunConfigurationData.PopulateOutputConfigurationData';
begin
  Result := inherited Initialise;
  try
    FOutputSummaryLevel := AOutputSummaryLevel;
    FCreateDataFile     := ACreateDataFile;
    FCreateYieldFile    := ACreateYieldFile;
    FCreatePlotFile     := ACreatePlotFile;
    FOutputDataHasBeenPopulated   := TRUE;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.PopulateMonthsData(AMonthNamesArray: TMonthNamesArray; AMonthsDaysArray: TMonthDaysArray): boolean;
const OPNAME = 'TRunConfigurationData.PopulateMonthsData';
var
  LIndex: integer;
  LMonthDays,
  LMonthsNames: TAbstractFieldProperty;
begin
  Result := False;
  try
    LMonthsNames := FAppModules.FieldProperties.FieldProperty('Month');
    LMonthDays  := FAppModules.FieldProperties.FieldProperty('Days');
    for LIndex := LMonthDays.ArrayLow to LMonthDays.ArrayHigh do
      FMonthsDaysArray[LIndex] := AMonthsDaysArray[LIndex];
    for LIndex := LMonthsNames.ArrayLow to LMonthsNames.ArrayHigh do
      FMonthNamesArray[LIndex] := AMonthNamesArray[LIndex];
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.PopulateSequencesToBeAnalysedArrayData(AHistoricSequenceStartYear,AStartSequenceNumber: integer;
         ASequencesToBeAnalysedArray: TSequencesToBeAnalysedArray): boolean;
const OPNAME = 'TRunConfigurationData.PopulateSequencesToBeAnalysedArrayData';
var
  LIndex: integer;
  LSequencesToBeAnalysed: TAbstractFieldProperty;
begin
  Result := False;
  try
    FStartSequenceNumber   := AStartSequenceNumber;
    LSequencesToBeAnalysed := FAppModules.FieldProperties.FieldProperty('Seq');
    if(RunSequenceType = 'H') then
    begin
      FSequencesToBeAnalysedArray[1] := ASequencesToBeAnalysedArray[1];
      for LIndex := 2 to LSequencesToBeAnalysed.ArrayHigh do
        FSequencesToBeAnalysedArray[LIndex] := 0;
    end
    else
    begin
      for LIndex := LSequencesToBeAnalysed.ArrayLow to LSequencesToBeAnalysed.ArrayHigh do
        FSequencesToBeAnalysedArray[LIndex] := ASequencesToBeAnalysedArray[LIndex];
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.PopulateSystemYieldAndPowerData(ANrOfLoadCases: integer;ATargetYieldArray: TTargetYieldArray;
  AMaximumYieldArray: TMaximumYieldArray; ATargetPowerArray: TTargetPowerArray): boolean;
const OPNAME = 'TRunConfigurationData.PopulateSystemYieldAndPowerData';
var
  LIndex: integer;
  LTargetYieldField,
  LMaximumYieldField,
  LTargetPowerField: TAbstractFieldProperty;
begin
  Result := inherited Initialise;
  try

    LTargetYieldField := FAppModules.FieldProperties.FieldProperty('TYield');
    LMaximumYieldField := FAppModules.FieldProperties.FieldProperty('MYield');
    LTargetPowerField := FAppModules.FieldProperties.FieldProperty('TPower');

    FNrOfActiveLoadCases := ANrOfLoadCases;
    for LIndex := LTargetYieldField.ArrayLow to LTargetYieldField.ArrayHigh do
      FTargetYieldArray[LIndex] := ATargetYieldArray[LIndex];

    for LIndex := LMaximumYieldField.ArrayLow to LMaximumYieldField.ArrayHigh do
      FMaximumYieldArray[LIndex] := AMaximumYieldArray[LIndex];

    for LIndex := LTargetPowerField.ArrayLow to LTargetPowerField.ArrayHigh do
      FTargetPowerArray[LIndex] := ATargetPowerArray[LIndex];
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.PopulateHydroUnitsCode(AHydroUnitsCode: string): boolean;
const OPNAME = 'TRunConfigurationData.PopulateHydroUnitsCode';
begin
  Result := FALSE;
  try
    FHydroUnitsCode := AHydroUnitsCode;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.PopulatePlanningConfigurationData(ADetailedOption, ASupplyOption: boolean; AAnnualSummary: string;
                                                                 AEconomicOption, APlanningSummary, AInputSummary, AWaterQualityOption : boolean;
                                                                 APeriodsPerYear, ACalendarStartMonth: integer;AShortTermPlanningOption,AHydroPowerOption: string;
                                                                 AAllocationControlOption : string): boolean;
const OPNAME = 'TRunConfigurationData.PopulatePlanningConfigurationData';
begin
  Result := FALSE;
  try
    FDetailedOption          := ADetailedOption;
    FSupplyOption            := ASupplyOption;
    FAnnualSummary           := AAnnualSummary;
    FEconomicOption          := AEconomicOption;
    FPlanningSummary         := APlanningSummary;
    FInputSummary            := AInputSummary;
    FWaterQualityOption      := AWaterQualityOption;
    FPeriodsPerYear          := APeriodsPerYear;
    FCalendarStartMonth      := ACalendarStartMonth;
    FShortTermPlanningOption := AShortTermPlanningOption;
    FHydroPowerOption        := AHydroPowerOption;
    FAllocationControlOption := AAllocationControlOption;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.PopulateDecisionDates(ANrOfDecisionDates   : integer;
                                                     ADecisionMonths      : TDecisionMonthArray;
                                                     ADecisionTypes       : TDecisionTypeArray;
                                                     AHydroPowerIndicators: THydroPowerIndicatorArray): boolean;
const OPNAME = 'TRunConfigurationData.PopulateDecisionDates';
var
  LIndex : integer;
  LDecisionMonthsField,
  LDecisionTypesField,
  LHydroPowerIndicatorsField: TAbstractFieldProperty;
begin
  Result := False;
  try
    FNrOfDecisionMonths := ANrOfDecisionDates;

    LDecisionMonthsField       := FAppModules.FieldProperties.FieldProperty('DecisionMonth');
    LDecisionTypesField        := FAppModules.FieldProperties.FieldProperty('DecisionType');
    LHydroPowerIndicatorsField := FAppModules.FieldProperties.FieldProperty('HydroPowerIndicator');

    FDecisionMonths := ADecisionMonths;
    for LIndex := LDecisionMonthsField.ArrayLow to LDecisionMonthsField.ArrayHigh do
      FDecisionMonths[LIndex] := ADecisionMonths[LIndex];

    FDecisionTypes := ADecisionTypes;
    for LIndex := LDecisionTypesField.ArrayLow to LDecisionTypesField.ArrayHigh do
      FDecisionTypes[LIndex] := ADecisionTypes[LIndex];

    FHydroPowerIndicators := AHydroPowerIndicators;
    for LIndex := LHydroPowerIndicatorsField.ArrayLow to LHydroPowerIndicatorsField.ArrayHigh do
      FHydroPowerIndicators[LIndex] := AHydroPowerIndicators[LIndex];
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;



procedure TRunConfigurationData.SortYield;
const OPNAME = 'TRunConfigurationData.SortYield';
var
  lTempTarget : double;
begin
  try
    if(CalculateHistoricFirmYield > 0) then
    begin
      if(FTargetYieldArray[1] < FTargetYieldArray[2]) then
      begin
        lTempTarget := FTargetYieldArray[1];
        Set_TargetYieldByIndex(1,FTargetYieldArray[2]);
        Set_TargetYieldByIndex(2,lTempTarget);
      end;
      if(FMaximumYieldArray[1] < FMaximumYieldArray[2]) then
      begin
        lTempTarget := FMaximumYieldArray[1];
        Set_MaximumYieldByIndex(1,FMaximumYieldArray[2]);
        Set_MaximumYieldByIndex(2,lTempTarget);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.UpdateDamImplimentationFile(AAllocationControlOption: string);
const OPNAME = 'TRunConfigurationData.UpdateDamImplimentationFile';
var
  LLoadAgent : TRunConfigurationDataSQLAgent;
  LFileNamesList : TFileNamesList;
  LFileName : string;
begin
  try
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LFileNamesList := nil;
      if AAllocationControlOption = 'I' then
        LFileNamesList := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastCurtailFileNames
      else
      if AAllocationControlOption = 'Y' then
        LFileNamesList := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastChannelSwitchControlFileNames;
      if LFileNamesList <> nil then
      begin
        LFileName := LFileNamesList.FileNameObject[0].FileName;
        LLoadAgent.InsertDamImplimentationFile(LFileName, AAllocationControlOption);
      end;

    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.CopyInflowFilesOnTheDisk;
const OPNAME = 'TRunConfigurationData.CopyInflowFilesOnTheDisk';
var
  LFileNamesList : TFileNamesList;
  LFileName : string;
  LIndex : integer;
  LHydroFilePath : string;
  LSeachfiles : TSearchRec;
  LPrefix : string;
  LMoreFiles : boolean;
  LModelFileName : TAbstractModelFileName;
  LNewFileIndex : integer;
begin
  try
    LFileNamesList := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastHydrologyFileNames;
    LHydrofilePath := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFilesPath;
    try
      for LIndex:= 0 to LFileNamesList.FilesCount - 1 do
      begin
        LFileName := LFileNamesList.FileNameObject[LIndex].FileName;
        if(UpperCase(ExtractFileExt(LFileName)) = '.INF') then
        begin
          LMoreFiles := (FindFirst(LFileName, faAnyFile, LSeachfiles) = 0);
          while LMoreFiles do
          begin

            LPrefix := Trim(Copy(LFileName,Pos('.',LFileName)-1,1));
            if (UpperCase(LPrefix) = 'H') and (RunSequenceType = 'S') then
              LFileName := Copy(LFileName,1,Pos('.',LFileName)-2)+'S'+ ExtractFileExt(Trim(LFileName))
            else
            if (UpperCase(LPrefix) = 'S') and (RunSequenceType = 'H') then
              LFileName := Copy(LFileName,1,Pos('.',LFileName)-2)+'H'+ ExtractFileExt(Trim(LFileName));
            LModelFileName := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFileNames.FindFile(LFileName);
            if(LModelFileName = nil) then
            begin
              LNewFileIndex := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFileNames.Count + 1;
                               TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.
                               AddHydrologyFileName(LNewFileIndex,LFileName,False,0.0,FileLastWriteDate(LFileName));
              LModelFileName := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFileNames.FindFile(LFileName);

              if (LModelFileName <> nil) then
              begin
                 if not (FileExists(LFileNamesList.FileNameObject[LIndex].FileName)) then
                   FAppModules.Model.ProcessEvent(CmeExportFile,LFileNamesList.FileNameObject[LIndex]);

                CopyInflowFiles(LFileNamesList.FileNameObject[LIndex].FileName,LModelFileName.FileName);
                FAppModules.Model.StudyDataHasChanged(sdccImport,'','','');
              end;

            end;
            LMoreFiles := (FindNext(LSeachfiles)= 0);
          end;
          SysUtils.FindClose(LSeachfiles);

        end;
      end;
    finally

    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.CopyInflowFiles(AOldDirFile,ANewDirFile : string );
const OPNAME = 'TRunConfigurationData.CopyInflowFiles';
var
  LNewFile: TFileStream;
  LOldFile: TFileStream;
begin
  if (UpperCase(aOldDirFile) = UpperCase(aNewDirFile)) then Exit;
  try
    if (FileExists(aNewDirFile)) then
      DeleteFile(aNewDirFile);
    LOldFile := TFileStream.Create(AOldDirFile,fmOpenRead or fmShareDenyWrite);
    try
      LNewFile := TFileStream.Create(ANewDirFile,fmCreate or fmShareExclusive);
      try
        LNewFile.CopyFrom(LOldFile,LOldFile.Size);
      finally
        FreeAndNil(LNewFile);
      end;
    finally
      FreeAndNil(LOldFile);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TRunConfigurationData.CopyInflowFilesInTheDatabase;
const OPNAME = 'TRunConfigurationData.CopyInflowFilesInTheDatabase';
var
  LLoadAgent : TRunConfigurationDataSQLAgent;
  LFileNamesList : TFileNamesList;
  LFileName : string;
  LIndex : integer;
begin
  try
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LFileNamesList := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastHydrologyFileNames;
      for LIndex:= 0 to LFileNamesList.FilesCount - 1 do
      begin
        LFileName := LFileNamesList.FileNameObject[LIndex].FileName;
        if(UpperCase(ExtractFileExt(LFileName)) = '.INF') then
          LLoadAgent.SetInflowFileToStochastic(LFileName,LFileNamesList.FileNameObject[LIndex].FileDate,RunSequenceType);
      end;
    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TRunConfigurationData.GetKeyValues (const AParamField : WideString;
                                                     const AFieldIndex : WideString) : WideString;
const OPNAME = 'TRunConfigurationData.GetKeyValues';
begin
  Result := '';
  try
    Result := GetDefaultKey;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunConfigurationData.GetBaseValue (const AParamField: WideString;
                                                     const AFieldIndex: WideString): WideString;
const OPNAME = 'TRunConfigurationData.GetBaseValue';
var
  lFieldProperty : TAbstractFieldProperty;
  lFormatStr     : string;
begin
  Result := '';
  try
    lFormatStr := '';
    lFieldProperty := FAppModules.FieldProperties.FieldProperty(AParamField);
    if (lFieldProperty <> nil) then
      lFormatStr := lFieldProperty.FormatStringGrid;

    if (AParamField = 'TYield') then
    begin
      if (lFormatStr = '') then
        Result := SmartFloatFormat(FTargetYieldArray[StrToInt(AFieldIndex)],6,2)
      else
        Result := SmartFloatFormat(FTargetYieldArray[StrToInt(AFieldIndex)],6,2);
    end
    else
    if (AParamField = 'MYield') then
    begin
      if (lFormatStr = '') then
        Result := FloatToStr(FMaximumYieldArray[StrToInt(AFieldIndex)])
      else
        Result := Format(lFormatStr, [FMaximumYieldArray[StrToInt(AFieldIndex)]]);
    end
    else
    if (AParamField = 'TPower') then
    begin
      if (lFormatStr = '') then
        Result := FloatToStr(FTargetPowerArray[StrToInt(AFieldIndex)])
      else
        Result := Format(lFormatStr, [FTargetPowerArray[StrToInt(AFieldIndex)]]);
    end
    else
    if (AParamField = 'SummaryLevel') then
    begin
      if (lFormatStr = '') then
        Result := IntToStr(FOutputSummaryLevel)
      else
        Result := Format(lFormatStr, [FOutputSummaryLevel]);
    end
    else
    if (AParamField = 'DebugLevel') then
    begin
      if (lFormatStr = '') then
        Result := IntToStr(FDebugLevel)
      else
        Result := Format(lFormatStr, [FDebugLevel]);
    end
    else
    if (AParamField = 'LimitOpt') then
    begin
      if (FLimitOption) then
        Result := '1'
      else
        Result := '0';
    end
    else
    if (AParamField = 'SummaryOut') then
    begin
      if (FCreateDataFile) then
        Result := '1'
      else
        Result := '0';
    end
    else
   if (AParamField = 'StoreYield') then
    begin
      if (FCreateYieldFile) then
        Result := '1'
      else
        Result := '0';
    end
    else
    if (AParamField = 'PlotOpt') then
    begin
      if (FCreatePlotFile) then
        Result := 'Y'
      else
        Result := 'N';
    end
    else
    if (AParamField = 'MultPeriodOpt') then
    begin
      if (FMultiplePeriodLengths) then
        Result := '1'
      else
        Result := '0';
    end;
    if (AParamField = 'ReduceSeqOpt') then
    begin
      if (FReduceSequences) then
        Result := '1'
      else
        Result := '0';
    end;
    if (AParamField = 'CalcHistoryOpt') then
      Result := IntToStr(FCalculateHistoricFirmYield)
    else
    if (AParamField = 'StartType') then
      Result := IntToStr(FGeneratedFlowFlag)
    else
    if (AParamField = 'Title1') then
      Result := FRunTitle1
    else
    if (AParamField = 'Title2') then
      Result := FRunTitle2
    else
    if (AParamField = 'Title3') then
      Result := FRunTitle3;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.GetEndYearAndMonth (var AEndYear  : integer;
                                                   var AEndMonth : integer) : WordBool;
const OPNAME = 'TRunConfigurationData.GetEndYearAndMonth';
var
  lEndYear  : integer;
  lEndMonth : integer;
begin
  Result := FALSE;
  try
    lEndYear  := FStartYearOther + (FPeriodsInAnalysis div 12);
    lEndMonth := FStartMonthNumber - 1 + (FPeriodsInAnalysis mod 12);
    if (lEndMonth = 0) then
      lEndMonth := 12
    else
    if (lEndMonth > 12) then
    begin
      lEndMonth := lEndMonth - 12;
      if (lEndMonth > (12 + 1 - FAppModules.StudyArea.CalendarStartMonth)) then
        lEndYear := lEndYear + 1;
    end;
    AEndYear  := lEndYear;
    AEndMonth := lEndMonth;
    Result    := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_DetailedOption: WordBool;
const OPNAME = 'TRunConfigurationData.Get_DetailedOption';
begin
  Result := FALSE;
  try
    Result := FDetailedOption;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_DetailedOption(Value: WordBool);
const OPNAME = 'TRunConfigurationData.Set_DetailedOption';
var
  LLoadAgent         : TRunConfigurationDataSQLAgent;
  LContextData       : TStringList;
  LOldDetailedOption : string;
  LNewDetailedOption : string;
begin
  try
    if FDetailedOption then
      LOldDetailedOption := '1'
    else
      LOldDetailedOption := '0';

    if Value then
      LNewDetailedOption := '1'
    else
      LNewDetailedOption := '0';

    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'DetailedOption', LNewDetailedOption, LOldDetailedOption, LContextData) then
        begin
          FDetailedOption := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'DetailedOption',LOldDetailedOption,
                                               LNewDetailedOption);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_SupplyOption: WordBool;
const OPNAME = 'TRunConfigurationData.Get_SupplyOption';
begin
  Result := FALSE;
  try
    Result := FSupplyOption;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_SupplyOption(Value: WordBool);
const OPNAME = 'TRunConfigurationData.Set_SupplyOption';
var
  LLoadAgent       : TRunConfigurationDataSQLAgent;
  LContextData     : TStringList;
  LOldSupplyOption : string;
  LNewSupplyOption : string;
begin
  try
    if FSupplyOption then
      LOldSupplyOption := '1'
    else
      LOldSupplyOption := '0';

    if Value then
      LNewSupplyOption := '1'
    else
      LNewSupplyOption := '0';

    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'SupplyOption', LNewSupplyOption, LOldSupplyOption, LContextData) then
        begin
          FSupplyOption := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SupplyOption',LOldSupplyOption,
                                                LNewSupplyOption);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_AnnualSummary: WideString;
const OPNAME = 'TRunConfigurationData.Get_AnnualSummary';
begin
  Result := '';
  try
    Result := FAnnualSummary;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_AnnualSummary(const Value: WideString);
const OPNAME = 'TRunConfigurationData.Set_AnnualSummary';
var
  LLoadAgent       : TRunConfigurationDataSQLAgent;
  LContextData     : TStringList;
  LOldValue        : string;
begin
  try
    if((Value = 'Q') or (Value = 'D') or (Value = 'Y') or (Value = 'N')) then
    begin
      LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadPeriodDataContextData(LContextData);
          if FAppModules.FieldProperties.UpdateFieldValue(
               'AnnualSummary', Value, FAnnualSummary, LContextData) then
          begin
            LOldValue      := FAnnualSummary;
            FAnnualSummary := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'AnnualSummary', LOldValue, Value);
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_EconomicOption: WordBool;
const OPNAME = 'TRunConfigurationData.Get_EconomicOption';
begin
  Result := FALSE;
  try
    Result := FEconomicOption;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_EconomicOption(Value: WordBool);
const OPNAME = 'TRunConfigurationData.Set_EconomicOption';
var
  LLoadAgent         : TRunConfigurationDataSQLAgent;
  LContextData       : TStringList;
  LOldEconomicOption : string;
  LNewEconomicOption : string;
begin
  try
    if FEconomicOption then
      LOldEconomicOption := '1'
    else
      LOldEconomicOption := '0';

    if Value then
      LNewEconomicOption := '1'
    else
      LNewEconomicOption := '0';

    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'EconomicOption', LNewEconomicOption, LOldEconomicOption, LContextData) then
        begin
          FEconomicOption := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'EconomicOption',LOldEconomicOption,
                                                LNewEconomicOption);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_PlanningSummary: WordBool;
const OPNAME = 'TRunConfigurationData.Get_PlanningSummary';
begin
  Result := FALSE;
  try
    Result := FPlanningSummary;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_PlanningSummary(Value: WordBool);
const OPNAME = 'TRunConfigurationData.Set_PlanningSummary';
var
  LLoadAgent          : TRunConfigurationDataSQLAgent;
  LContextData        : TStringList;
  LOldPlanningSummary : string;
  LNewPlanningSummary : string;
begin
  try
    if FPlanningSummary then
      LOldPlanningSummary := '1'
    else
      LOldPlanningSummary := '0';

    if Value then
      LNewPlanningSummary := '1'
    else
      LNewPlanningSummary := '0';

    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'PlanningSummary', LNewPlanningSummary, LOldPlanningSummary, LContextData) then
        begin
          FPlanningSummary := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'PlanningSummary',LOldPlanningSummary,
                                                LNewPlanningSummary);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_InputSummary: WordBool;
const OPNAME = 'TRunConfigurationData.Get_InputSummary';
begin
  Result := FALSE;
  try
    Result := FInputSummary;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_InputSummary(Value: WordBool);
const OPNAME = 'TRunConfigurationData.Set_InputSummary';
var
  LLoadAgent       : TRunConfigurationDataSQLAgent;
  LContextData     : TStringList;
  LOldInputSummary : string;
  LNewInputSummary : string;
begin
  try
    if FInputSummary then
      LOldInputSummary := '1'
    else
      LOldInputSummary := '0';

    if Value then
      LNewInputSummary := '1'
    else
      LNewInputSummary := '0';

    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'InputSummary', LNewInputSummary, LOldInputSummary, LContextData) then
        begin
          FInputSummary := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'InputSummary',LOldInputSummary,
                                                LNewInputSummary);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_WaterQualityOption: WordBool;
const OPNAME = 'TRunConfigurationData.Get_WaterQualityOption';
begin
  Result := FALSE;
  try
    Result := FWaterQualityOption;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_WaterQualityOption(Value: WordBool);
const OPNAME = 'TRunConfigurationData.Set_WaterQualityOption';
var
  LLoadAgent             : TRunConfigurationDataSQLAgent;
  LContextData           : TStringList;
  LOldWaterQualityOption : string;
  LNewWaterQualityOption : string;
begin
  try
    if FWaterQualityOption then
      LOldWaterQualityOption := 'Y'
    else
      LOldWaterQualityOption := 'N';

    if Value then
      LNewWaterQualityOption := 'Y'
    else
      LNewWaterQualityOption := 'N';

    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'WaterQualityOption', LNewWaterQualityOption, LOldWaterQualityOption, LContextData) then
        begin
          FWaterQualityOption := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'WaterQualityOption',LOldWaterQualityOption,
                                                LNewWaterQualityOption);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_PeriodsPerYear: Integer;
const OPNAME = 'TRunConfigurationData.Get_PeriodsPerYear';
begin
  Result := 0;
  try
    Result := FPeriodsPerYear;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_PeriodsPerYear(Value: Integer);
const OPNAME = 'TRunConfigurationData.Set_PeriodsPerYear';
var
  LLoadAgent   : TRunConfigurationDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : integer;
begin
  try
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'PeriodsPerYear', IntToStr(Value), IntToStr(FPeriodsPerYear), LContextData) then
        begin
          LOldValue := FPeriodsPerYear;
          FPeriodsPerYear := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'PeriodsPerYear',IntToStr(LOldValue), IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_CalendarStartMonth: Integer;
const OPNAME = 'TRunConfigurationData.Get_CalendarStartMonth';
begin
  Result := 0;
  try
    Result := FCalendarStartMonth;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_CalendarStartMonth(Value: Integer);
const OPNAME = 'TRunConfigurationData.Set_CalendarStartMonth';
var
  LLoadAgent   : TRunConfigurationDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : integer;
begin
  try
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'CalendarStartMonth', IntToStr(Value), IntToStr(FCalendarStartMonth), LContextData) then
        begin
          LOldValue := FCalendarStartMonth;
          FCalendarStartMonth := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'CalendarStartMonth',IntToStr(LOldValue), IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_ShortTermPlanningOption: WideString;
const OPNAME = 'TRunConfigurationData.Get_ShortTermPlanningOption';
begin
  Result := '';
  try
    Result := FShortTermPlanningOption;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_ShortTermPlanningOption(const Value: WideString);
const OPNAME = 'TRunConfigurationData.Set_ShortTermPlanningOption';
var
  LLoadAgent   : TRunConfigurationDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    if ((Value = 'N') or (Value = 'P') or (Value = 'M')) then
    begin
      LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadPeriodDataContextData(LContextData);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'ShortTermPlanningOption', Value, FShortTermPlanningOption, LContextData) then
          begin
            LOldValue := FShortTermPlanningOption;
            FShortTermPlanningOption := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'ShortTermPlanningOption',LOldValue, Value);
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_HydroPowerOption: WideString;
const OPNAME = 'TRunConfigurationData.Get_HydroPowerOption';
begin
  Result := '';
  try
    Result := FHydroPowerOption;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_HydroPowerOption(const Value: WideString);
const OPNAME = 'TRunConfigurationData.Set_HydroPowerOption';
var
  LLoadAgent           : TRunConfigurationDataSQLAgent;
  LContextData         : TStringList;
  LOldValue            : string;
begin
  try
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadPeriodDataContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'HydroPowerOption', Value, FHydroPowerOption, LContextData) then
        begin
          LOldValue         := FHydroPowerOption;
          FHydroPowerOption := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'HydroPowerOption',LOldValue,FHydroPowerOption);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_AllocationControlOption: WideString;
const OPNAME = 'TRunConfigurationData.Get_AllocationControlOption';
begin
  Result := '';
  try
    Result := FAllocationControlOption;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_AllocationControlOption(const Value: WideString);
const OPNAME = 'TRunConfigurationData.Set_AllocationControlOption';
var
  LLoadAgent                  : TRunConfigurationDataSQLAgent;
  LContextData                : TStringList;
  LOldAllocationControlOption : string;
  LNewAllocationControlOption : string;

begin
  try
    LNewAllocationControlOption := Value;
    LOldAllocationControlOption := FAllocationControlOption;
    if LNewAllocationControlOption <> LOldAllocationControlOption then
    begin
      LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadPeriodDataContextData(LContextData);
          if FAppModules.FieldProperties.UpdateFieldValue(
               'AllocationControlOption', LNewAllocationControlOption, LOldAllocationControlOption, LContextData) then
          begin
            FAllocationControlOption := Value;
            UpdateDamImplimentationFile(Value);
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'AllocationControlOption',LOldAllocationControlOption,
                                                  LNewAllocationControlOption);
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_NrOfDecisionMonths: Integer;
const OPNAME = 'TRunConfigurationData.Get_NrOfDecisionMonths';
begin
  Result := 0;
  try
    Result := FNrOfDecisionMonths;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_NrOfDecisionMonths(Value: Integer);
const OPNAME = 'TRunConfigurationData.Set_NrOfDecisionMonths';
var
  LLoadAgent   : TRunConfigurationDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : integer;
begin
  try
    if (FNrOfDecisionMonths = 0) then
      CreateNrOfDecisionDates(Value);
    begin
      LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadPeriodDataContextData(LContextData);
          if FAppModules.FieldProperties.UpdateFieldValue(
               'NrOfDecisionDates', IntToStr(Value), IntToStr(FNrOfDecisionMonths), LContextData) then
          begin
            LOldValue := FNrOfDecisionMonths;
            FNrOfDecisionMonths := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'NrOfDecisionDates',IntToStr(LOldValue), IntToStr(Value));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_DecisionMonthByIndex(AIndex: Integer): Integer;
const OPNAME = 'TRunConfigurationData.Get_DecisionMonthByIndex';
begin
  Result := 0;
  try
    Result := FDecisionMonths[AIndex]
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_DecisionMonthByIndex(AIndex, AValue: Integer);
const OPNAME = 'TRunConfigurationData.Set_DecisionMonthByIndex';
var
  LLoadAgent   : TRunConfigurationDataSQLAgent;
  LContextData : TStringList;
begin
  try
    LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadMonthsDataContextData(LContextData, IntToStr(AIndex));
        if FAppModules.FieldProperties.UpdateFieldValue(
          'DecisionMonth', IntToStr(AValue), IntToStr(FDecisionMonths[AIndex]), LContextData) then
        begin
          FDecisionMonths[AIndex] := AValue;
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_DecisionTypeByIndex(AIndex: Integer): WideString;
const OPNAME = 'TRunConfigurationData.Get_DecisionTypeByIndex';
begin
  Result := '';
  try
    Result :=  FDecisionTypes[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_DecisionTypeByIndex (AIndex: Integer;
                                                         const AValue: WideString);
const OPNAME = 'TRunConfigurationData.Set_DecisionTypeByIndex';
var
  LLoadAgent   : TRunConfigurationDataSQLAgent;
  LContextData : TStringList;
begin
  try
    if ((AValue = 'M') or (AValue = 'R')) then
    begin
      LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadMonthsDataContextData(LContextData, IntToStr(AIndex));
          if FAppModules.FieldProperties.UpdateFieldValue(
            'DecisionType', AValue, FDecisionTypes[AIndex], LContextData) then
          begin
            FDecisionTypes[AIndex] := AValue;
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationData.Get_HydroPowerIndicatorByIndex(AIndex: Integer): WideString;
const OPNAME = 'TRunConfigurationData.Get_HydroPowerIndicatorByIndex';
begin
  Result := '';
  try
    Result := FHydroPowerIndicators[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationData.Set_HydroPowerIndicatorByIndex(AIndex: Integer; const AValue: WideString);
const OPNAME = 'TRunConfigurationData.Set_HydroPowerIndicatorByIndex';
var
  LLoadAgent   : TRunConfigurationDataSQLAgent;
  LContextData : TStringList;
begin
  try
   LLoadAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadMonthsDataContextData(LContextData, IntToStr(AIndex));
          if FAppModules.FieldProperties.UpdateFieldValue(
            'HydroPowerIndicator', AValue, FHydroPowerIndicators[AIndex], LContextData) then
          begin
            FHydroPowerIndicators[AIndex] := AValue;
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
