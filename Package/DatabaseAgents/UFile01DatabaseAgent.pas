//
//
//  UNIT      : Contains TFile01DatabaseAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 21/01/2002
//  COPYRIGHT : Copyright © 2001 DWAF
//
//
unit UFile01DatabaseAgent;

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
  UAbstractFileNamesObject,
  URunParametersObject,
  UAbstractDatabaseAgent,
  UYieldModelDataObject;

type

  TFile01DatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadF01KnownDataSQL: string;
    function ReadF01UnkownDataSQL: string;
    function ReadDecisionDateSQL: string;
    function ReadDemandCentreSQL: string;
    function ReadInterBasinSupportSQL: string;
    function ReadDiscountRateSQL: string;

    function ReadManualAnalysisSQL: string;
    function ReadMinMaxBoundsSQL: string;
    function ReadWQConstraintsSQL: string;

    function WriteRunTitleDataSQL: string;
    function WriteRunParametersDataSQL: string;
    function WriteMonthNamesDataSQL: string;
    function WriteDaysPerMonthDataSQL: string;
    function WriteAnlySequencesDataSQL: string;
    function WriteTargetYieldDataSQL: string;
    function WriteMaxYieldDataSQL: string;
    function WriteTargetPowerDataSQL: string;
    function WriteDecisionDateSQL: string;
    function WriteDemandCentreSQL: string;
    function WriteInterBasinSupportSQL: string;
    function WriteDiscountRateSQL: string;
    function WriteUnknownDataSQL: string;

    function WriteManualAnalysisSQL: string;
    function WriteMinMaxBoundsSQL: string;
    function WriteWQConstraintsSQL: string;
    procedure UpdateWithChangeLists(ARunParameters : TRunParametersObject);

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
  UErrorHandlingOperations;

function TFile01DatabaseAgent.ReadF01UnkownDataSQL: string;
const OPNAME = 'TFile01DatabaseAgent.ReadF01UnkownDataSQL';
begin

  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,LineNumber,LineData  '+
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

function TFile01DatabaseAgent.ReadF01KnownDataSQL: string;
const OPNAME = 'TFile01DatabaseAgent.ReadF01KnownDataSQL';
begin

  Result := '';
  try
    Result := 'SELECT'+
      ' Title1,Title2,Title3'+
      ' ,Seq1,Seq2,Seq3,Seq4,Seq5,Seq6,Seq7,Seq8,Seq9,Seq10'+
      ' ,Days1,Days2,Days3,Days4,Days5,Days6,Days7,Days8,Days9,Days10,Days11,Days12'+
      ' ,MYield1,MYield2,MYield3,MYield4,MYield5,MYield6,MYield7,MYield8,MYield9,MYield10'+
      ' ,Month1,Month2,Month3,Month4,Month5,Month6,Month7,Month8,Month9,Month10,Month11,Month12'+
      ' ,NumPeriods, StartYearG, StartYearO, DebugInit, DebugFinal, DebugLevel, SummaryLevel, SummaryOut'+
      ' ,StoreYield, RandomOpt, PlotOpt, LimitOpt, MultPeriodOpt, CalcHistoryOpt, ReduceSeqOpt, YearsCount'+
      ' ,HydroSeqCount, LoadCasesCount, StartMonthNo, RunType, StartType, ParamFile, TargetRecurrenceInterval'+
      ' ,DetailedOption, SupplyOption, AnnualSummary, EconomicOption, PlanningSummary, InputSummary,WaterQualityOption'+
      ' ,PeriodsPerYear, CalendarStartMonth, ShortTermPlanningOption, HydroPowerOption,AllocationControlOption'+
      ' ,TPower1,TPower2,TPower3,TPower4,TPower5,TPower6,TPower7,TPower8,TPower9,TPower10'+
      ' ,TYield1,TYield2,TYield3,TYield4,TYield5,TYield6,TYield7,TYield8,TYield9,TYield10' +
      ' FROM '+
      '  RunTitle'+
      ' ,AnlySequences'+
      ' ,DaysPerMonth'+
      ' ,MaxYield'+
      ' ,MonthNames'+
      ' ,RunParameters'+
      ' ,TargetPower'+
      ' ,TargetYield'+
      ' WHERE RunTitle.Model            =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
      ' AND RunTitle.StudyAreaName      =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
      ' AND RunTitle.SubArea            =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
      ' AND RunTitle.Scenario           =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
      ' AND AnlySequences.Model         = RunTitle.Model'+
      ' AND AnlySequences.StudyAreaName = RunTitle.StudyAreaName'+
      ' AND AnlySequences.SubArea       = RunTitle.SubArea'+
      ' AND AnlySequences.Scenario      = RunTitle.Scenario'+
      ' AND DaysPerMonth.Model         = RunTitle.Model'+
      ' AND DaysPerMonth.StudyAreaName = RunTitle.StudyAreaName'+
      ' AND DaysPerMonth.SubArea       = RunTitle.SubArea'+
      ' AND DaysPerMonth.Scenario      = RunTitle.Scenario'+
      ' AND MaxYield.Model         = RunTitle.Model'+
      ' AND MaxYield.StudyAreaName = RunTitle.StudyAreaName'+
      ' AND MaxYield.SubArea       = RunTitle.SubArea'+
      ' AND MaxYield.Scenario      = RunTitle.Scenario'+
      ' AND MonthNames.Model         = RunTitle.Model'+
      ' AND MonthNames.StudyAreaName = RunTitle.StudyAreaName'+
      ' AND MonthNames.SubArea       = RunTitle.SubArea'+
      ' AND MonthNames.Scenario      = RunTitle.Scenario'+
      ' AND RunParameters.Model         = RunTitle.Model'+
      ' AND RunParameters.StudyAreaName = RunTitle.StudyAreaName'+
      ' AND RunParameters.SubArea       = RunTitle.SubArea'+
      ' AND RunParameters.Scenario      = RunTitle.Scenario'+
      ' AND TargetPower.Model         = RunTitle.Model'+
      ' AND TargetPower.StudyAreaName = RunTitle.StudyAreaName'+
      ' AND TargetPower.SubArea       = RunTitle.SubArea'+
      ' AND TargetPower.Scenario      = RunTitle.Scenario'+
      ' AND TargetYield.Model         = RunTitle.Model'+
      ' AND TargetYield.StudyAreaName = RunTitle.StudyAreaName'+
      ' AND TargetYield.SubArea       = RunTitle.SubArea'+
      ' AND TargetYield.Scenario      = RunTitle.Scenario;';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile01DatabaseAgent.ReadDecisionDateSQL: string;
const OPNAME = 'TFile01DatabaseAgent.ReadDecisionDateSQL';
begin
  Result := '';
  try
    Result := 'SELECT NrOfDecisionDates,'+
              ' DecisionMonth01, DecisionMonth02, DecisionMonth03, DecisionMonth04,'+
              ' DecisionMonth05, DecisionMonth06, DecisionMonth07,DecisionMonth08,'+
              ' DecisionMonth09, DecisionMonth10, DecisionMonth11, DecisionMonth12,'+
              ' DecisionType01, DecisionType02, DecisionType03, DecisionType04, DecisionType05, DecisionType06,'+
              ' DecisionType07, DecisionType08, DecisionType09, DecisionType10, DecisionType11, DecisionType12,'+
              ' HydroPowerIndicator01, HydroPowerIndicator02, HydroPowerIndicator03, HydroPowerIndicator04,'+
              ' HydroPowerIndicator05, HydroPowerIndicator06, HydroPowerIndicator07, HydroPowerIndicator08,'+
              ' HydroPowerIndicator09, HydroPowerIndicator10, HydroPowerIndicator11, HydroPowerIndicator12'+
              ' FROM DecisionDate'+
              ' WHERE Model            =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName      =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea            =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario           =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile01DatabaseAgent.ReadDemandCentreSQL: string;
const OPNAME = 'TFile01DatabaseAgent.ReadDemandCentreSQL';
begin
  Result := '';
  try
    Result := 'SELECT DemandCentreID,DemandCentreType,ChannelNumber,AnnualDemand,MinimumDemand,IncludeInOutput,Comment'+
              ' FROM DemandCentre'+
              ' WHERE Model            =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName      =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea            =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario           =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,DemandCentreID';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile01DatabaseAgent.ReadDiscountRateSQL: string;
const OPNAME = 'TFile01DatabaseAgent.ReadDiscountRateSQL';
begin
  Result := '';
  try
    Result := 'SELECT NrOfDiscountRates,DiscountRate01, DiscountRate02, DiscountRate03, DiscountRate04,'+
              ' DiscountRate05,DiscountRate06, DiscountRate07, DiscountRate08, DiscountRate09, DiscountRate10'+
              ' FROM DiscountRate'+
              ' WHERE Model            =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName      =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea            =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario           =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile01DatabaseAgent.ReadInterBasinSupportSQL: string;
const OPNAME = 'TFile01DatabaseAgent.ReadInterBasinSupportSQL';
begin
  Result := '';
  try
    Result := 'SELECT InterBasinSupportID,SummaryRequired,ChannelNumber,UpperLimit,DemandCentreID,Comment'+
              ' FROM InterBasinSupport'+
              ' WHERE Model            =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName      =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea            =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario           =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' ORDER BY InterBasinSupportID';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile01DatabaseAgent.ReadManualAnalysisSQL: string;
const OPNAME = 'TFile01DatabaseAgent.ReadInterBasinSupportSQL';
begin
  Result := '';
  try
    Result := 'SELECT AnalysisType,Seq1,Seq2,Seq3,Seq4,Seq5'+
              ' FROM AnlyManual'+
              ' WHERE Model            =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName      =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea            =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario           =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile01DatabaseAgent.ReadMinMaxBoundsSQL: string;
const OPNAME = 'TFile01DatabaseAgent.ReadInterBasinSupportSQL';
begin
  Result := '';
  try
    Result := 'SELECT ChannelNumber,ReferenceChannelCount,ReferenceChannels'+
              ' FROM MinMaxBoundChannel'+
              ' WHERE Model            =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName      =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea            =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario           =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' ORDER BY Identifier';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile01DatabaseAgent.ReadWQConstraintsSQL: string;
const OPNAME = 'TFile01DatabaseAgent.ReadInterBasinSupportSQL';
begin
  Result := '';
  try
    Result := 'SELECT Identifier, ChannelNumber,WQTarget,BlendingRefChannelCount,   '+
              ' ReservoirRef,WQConType,ReferenceChannels,ReferenceChannelFactors,SlopeLimit,EstimatedRelease,Concentration'+
              ' FROM MinMaxWQConstrain'+
              ' WHERE Model            =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName      =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea            =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario           =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' ORDER BY Identifier';

  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFile01DatabaseAgent.WriteDecisionDateSQL: string;
const OPNAME = 'TFile01DatabaseAgent.WriteDecisionDateSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO DecisionDate (Model,StudyAreaName,SubArea,Scenario,NrOfDecisionDates,'+
              ' DecisionMonth01, DecisionMonth02, DecisionMonth03, DecisionMonth04,'+
              ' DecisionMonth05, DecisionMonth06, DecisionMonth07, DecisionMonth08,'+
              ' DecisionMonth09, DecisionMonth10, DecisionMonth11, DecisionMonth12,'+
              ' DecisionType01, DecisionType02, DecisionType03, DecisionType04, DecisionType05, DecisionType06,'+
              ' DecisionType07, DecisionType08, DecisionType09, DecisionType10, DecisionType11, DecisionType12,'+
              ' HydroPowerIndicator01, HydroPowerIndicator02, HydroPowerIndicator03, HydroPowerIndicator04,'+
              ' HydroPowerIndicator05, HydroPowerIndicator06, HydroPowerIndicator07, HydroPowerIndicator08,'+
              ' HydroPowerIndicator09, HydroPowerIndicator10, HydroPowerIndicator11, HydroPowerIndicator12)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:NrOfDecisionDates,'+
              ' :DecisionMonth01, :DecisionMonth02, :DecisionMonth03, :DecisionMonth04,'+
              ' :DecisionMonth05, :DecisionMonth06, :DecisionMonth07, :DecisionMonth08,'+
              ' :DecisionMonth09, :DecisionMonth10, :DecisionMonth11, :DecisionMonth12,'+
              ' :DecisionType01, :DecisionType02, :DecisionType03, :DecisionType04, :DecisionType05, :DecisionType06,'+
              ' :DecisionType07, :DecisionType08, :DecisionType09, :DecisionType10, :DecisionType11, :DecisionType12,'+
              ' :HydroPowerIndicator01, :HydroPowerIndicator02, :HydroPowerIndicator03, :HydroPowerIndicator04,'+
              ' :HydroPowerIndicator05, :HydroPowerIndicator06, :HydroPowerIndicator07, :HydroPowerIndicator08,'+
              ' :HydroPowerIndicator09, :HydroPowerIndicator10, :HydroPowerIndicator11, :HydroPowerIndicator12)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile01DatabaseAgent.WriteDemandCentreSQL: string;
const OPNAME = 'TFile01DatabaseAgent.WriteDemandCentreSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO DemandCentre (Model,StudyAreaName,SubArea,Scenario,'+
              ' DemandCentreID,DemandCentreType,ChannelNumber,AnnualDemand,MinimumDemand,IncludeInOutput,Comment)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,'+
              ' :DemandCentreID,:DemandCentreType,:ChannelNumber,:AnnualDemand,:MinimumDemand,:IncludeInOutput,:Comment)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile01DatabaseAgent.WriteDiscountRateSQL: string;
const OPNAME = 'TFile01DatabaseAgent.WriteDiscountRateSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO DiscountRate (Model,StudyAreaName,SubArea,Scenario,NrOfDiscountRates,'+
              ' DiscountRate01, DiscountRate02, DiscountRate03, DiscountRate04, DiscountRate05,'+
              ' DiscountRate06, DiscountRate07, DiscountRate08, DiscountRate09, DiscountRate10)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:NrOfDiscountRates,'+
              ' :DiscountRate01, :DiscountRate02, :DiscountRate03, :DiscountRate04, :DiscountRate05,'+
              ' :DiscountRate06, :DiscountRate07, :DiscountRate08, :DiscountRate09, :DiscountRate10)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile01DatabaseAgent.WriteInterBasinSupportSQL: string;
const OPNAME = 'TFile01DatabaseAgent.WriteInterBasinSupportSQL';
begin
  Result := '';
  try
    Result := ' INSERT INTO InterBasinSupport (Model,StudyAreaName,SubArea,Scenario,'+
              ' InterBasinSupportID,SummaryRequired,ChannelNumber,UpperLimit,DemandCentreID,Comment)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,'+
              ' :InterBasinSupportID,:SummaryRequired,:ChannelNumber,:UpperLimit,:DemandCentreID,:Comment)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile01DatabaseAgent.WriteRunTitleDataSQL: string;
const OPNAME = 'TFile01DatabaseAgent.WriteRunTitleDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO RunTitle'+
              ' (Model,StudyAreaName,SubArea,Scenario,Title1,Title2,Title3)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:Title1,:Title2,:Title3)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile01DatabaseAgent.WriteRunParametersDataSQL: string;
const OPNAME = 'TFile01DatabaseAgent.WriteRunParametersDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO RunParameters (Model,StudyAreaName,SubArea,Scenario'+
              ' ,NumPeriods, StartYearG, StartYearO, DebugInit, DebugFinal, DebugLevel, SummaryLevel, SummaryOut'+
              ' ,StoreYield, RandomOpt, PlotOpt, LimitOpt, MultPeriodOpt, CalcHistoryOpt, ReduceSeqOpt, YearsCount'+
              ' ,HydroSeqCount, LoadCasesCount, StartMonthNo, RunType, StartType, ParamFile, TargetRecurrenceInterval'+
              ' ,DetailedOption, SupplyOption, AnnualSummary, EconomicOption, PlanningSummary, InputSummary,WaterQualityOption'+
              ' ,PeriodsPerYear, CalendarStartMonth, ShortTermPlanningOption, HydroPowerOption, AllocationControlOption)'+
              ' Values (:Model,:StudyAreaName,:SubArea,:Scenario'+
              ' ,:NumPeriods, :StartYearG, :StartYearO, :DebugInit, :DebugFinal, :DebugLevel, :SummaryLevel, :SummaryOut'+
              ' ,:StoreYield, :RandomOpt, :PlotOpt, :LimitOpt, :MultPeriodOpt, :CalcHistoryOpt, :ReduceSeqOpt, :YearsCount'+
              ' ,:HydroSeqCount, :LoadCasesCount, :StartMonthNo, :RunType, :StartType, :ParamFile, :TargetRecurrenceInterval'+
              ' ,:DetailedOption, :SupplyOption, :AnnualSummary, :EconomicOption, :PlanningSummary, :InputSummary,:WaterQualityOption'+
              ' ,:PeriodsPerYear, :CalendarStartMonth, :ShortTermPlanningOption, :HydroPowerOption, :AllocationControlOption)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile01DatabaseAgent.WriteMonthNamesDataSQL: string;
const OPNAME = 'TFile01DatabaseAgent.WriteMonthNamesDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO MonthNames'+
              ' (Model,StudyAreaName,SubArea,Scenario,Month1,Month2,Month3,Month4,Month5,Month6,Month7,Month8,'+
              ' Month9,Month10,Month11,Month12)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:Month1,:Month2,:Month3,:Month4,:Month5,:Month6,:Month7,'+
              ' :Month8,:Month9,:Month10,:Month11,:Month12)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile01DatabaseAgent.WriteDaysPerMonthDataSQL: string;
const OPNAME = 'TFile01DatabaseAgent.WriteDaysPerMonthDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO DaysPerMonth'+
              ' (Model,StudyAreaName,SubArea,Scenario,Days1,Days2,Days3,Days4,Days5,Days6,Days7,Days8,Days9,'+
              ' Days10,Days11,Days12)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:Days1,:Days2,:Days3,:Days4,:Days5,:Days6,:Days7,'+
              ' :Days8,:Days9,:Days10,:Days11,:Days12)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile01DatabaseAgent.WriteAnlySequencesDataSQL: string;
const OPNAME = 'TFile01DatabaseAgent.WriteAnlySequencesDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO AnlySequences'+
              ' (Model,StudyAreaName,SubArea,Scenario,Seq1,Seq2,Seq3,Seq4,Seq5,Seq6,Seq7,Seq8,Seq9,Seq10)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:Seq1,:Seq2,:Seq3,:Seq4,:Seq5,:Seq6,:Seq7,:Seq8,:Seq9,:Seq10)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile01DatabaseAgent.WriteTargetYieldDataSQL: string;
const OPNAME = 'TFile01DatabaseAgent.WriteTargetYieldDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO TargetYield'+
              ' (Model,StudyAreaName,SubArea,Scenario,TYield1,TYield2,TYield3,TYield4,TYield5,TYield6,TYield7,'+
              ' TYield8,TYield9,TYield10)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:TYield1,:TYield2,:TYield3,:TYield4,:TYield5,'+
              ' :TYield6,:TYield7,:TYield8,:TYield9,:TYield10)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile01DatabaseAgent.WriteMaxYieldDataSQL: string;
const OPNAME = 'TFile01DatabaseAgent.WriteMaxYieldDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO MaxYield'+
              ' (Model,StudyAreaName,SubArea,Scenario,MYield1,MYield2,MYield3,MYield4,MYield5,MYield6,MYield7,'+
              ' MYield8,MYield9,MYield10)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:MYield1,:MYield2,:MYield3,:MYield4,:MYield5,'+
              ' :MYield6,:MYield7,:MYield8,:MYield9,:MYield10)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile01DatabaseAgent.WriteTargetPowerDataSQL: string;
const OPNAME = 'TFile01DatabaseAgent.WriteTargetPowerDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO TargetPower'+
              ' (Model,StudyAreaName,SubArea,Scenario,TPower1,TPower2,TPower3,TPower4,TPower5,TPower6,TPower7,TPower8,'+
              ' TPower9,TPower10)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:TPower1,:TPower2,:TPower3,:TPower4,:TPower5,:TPower6,'+
              ' :TPower7,:TPower8,:TPower9,:TPower10)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;



function TFile01DatabaseAgent.WriteManualAnalysisSQL: string;
const OPNAME = 'TFile01DatabaseAgent.WriteManualAnalysisSQL';
begin
  Result := '';
  try
    Result := ' INSERT INTO AnlyManual(Model,StudyAreaName,SubArea,Scenario,'+
              ' AnalysisType, Seq1, Seq2, Seq3, Seq4, Seq5)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,'+
              ' :AnalysisType, :Seq1, :Seq2, :Seq3, :Seq4, :Seq5)';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

 {
function TFile01DatabaseAgent.WriteMinMaxBlockTypeConfigSQL: string;
const OPNAME = 'TFile01DatabaseAgent.WriteMinMaxBlockTypeConfigSQL';
begin
  Result := '';
  try
    Result := ' INSERT INTO MinMaxBlockTypeConfig (Model,StudyAreaName,SubArea,Scenario,'+
              ' BlockType, BlockCount)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,'+
              ' :BlockType, :BlockCount)';

  except on E: Exception do HandleError(E, OPNAME) end;
end;
  }
function TFile01DatabaseAgent.WriteMinMaxBoundsSQL: string;
const OPNAME = 'TFile01DatabaseAgent.WriteMinMaxBoundsSQL';
begin
  Result := '';
  try
    Result := ' INSERT INTO MinMaxBoundChannel (Model,StudyAreaName,SubArea,Scenario,Identifier,'+
              ' ChannelNumber, ReferenceChannelCount, ReferenceChannels)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,'+
              ' :ChannelNumber, :ReferenceChannelCount, :ReferenceChannels)';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile01DatabaseAgent.WriteWQConstraintsSQL: string;
const OPNAME = 'TFile01DatabaseAgent.WriteWQConstraintsSQL';
begin
  Result := '';
  try
    Result := ' INSERT INTO MinMaxWQConstrain (Model,StudyAreaName,SubArea,Scenario,Identifier,'+
              ' ChannelNumber, WQTarget, BlendingRefChannelCount, ReservoirRef,WQConType,ReferenceChannels,'+
              ' ReferenceChannelFactors, SlopeLimit, EstimatedRelease, Concentration)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,'+
              ' :ChannelNumber, :WQTarget, :BlendingRefChannelCount, :ReservoirRef,:WQConType,:ReferenceChannels,'+
              ' :ReferenceChannelFactors, :SlopeLimit, :EstimatedRelease, :Concentration)';

  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFile01DatabaseAgent.WriteUnknownDataSQL: string;
const OPNAME = 'TFile01DatabaseAgent.WriteUnknownDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRYMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile01DatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile01DatabaseAgent.ReadModelDataFromDatabase';
var
  LIndex         : integer;
  LFieldName,
  LMessage       : string;
  LDataSet       : TAbstractModelDataset;
  //LNoData        : Boolean;
  LRunParameters : TRunParametersObject;
  LDemandCentre  : TDemandCentreObject;
  LInterBasinTransfer : TInterBasinTransferObject;
  LStop          : boolean;
  LFilePrefix    : string;
  LMinMaxBoundsObject : TMinMaxBoundsObject;
  LWQConstraintsObject : TWQConstraintsObject;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile01DatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LRunParameters := ADataObject.FRunParametersObject;

    if not LRunParameters.Initialise then
    Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try

      LDataSet.SetSQL(ReadF01KnownDataSQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile01DatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
       // Exit;
      end;

      // go to the last record if there is more than one record.
      LDataSet.DataSet.Last;

      //Read the F01 file
      //Title Line 1
      for LIndex := 1 to 3 do
      begin
        LFieldName := 'Title'+ IntToStr(LIndex);
        if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
        begin
          LRunParameters.FTitle[LIndex].FData := Trim(LDataSet.DataSet.FieldByName(LFieldName).AsString);
          LRunParameters.FTitle[LIndex].FInitalised := True;
        end;
      end;

      //line4 +++++++++++++++++++++++++
      if not LDataSet.DataSet.FieldByName('NumPeriods').IsNull then
      begin
        LRunParameters.FTimePeriods.FData := LDataSet.DataSet.FieldByName('NumPeriods').AsInteger;
        LRunParameters.FTimePeriods.FInitalised := True;
      end;

      if not LDataSet.DataSet.FieldByName('StartYearG').IsNull then
      begin
        LRunParameters.FStartGregorian.FData := LDataSet.DataSet.FieldByName('StartYearG').AsInteger;
        LRunParameters.FStartGregorian.FInitalised := True;
      end;

      if not LDataSet.DataSet.FieldByName('StartYearO').IsNull then
      begin
        LRunParameters.FStartOther.FData := LDataSet.DataSet.FieldByName('StartYearO').AsInteger;
        LRunParameters.FStartOther.FInitalised := True;
      end;

      if not LDataSet.DataSet.FieldByName('DebugInit').IsNull then
      begin
        LRunParameters.FStartDebug.FData := LDataSet.DataSet.FieldByName('DebugInit').AsInteger;
        LRunParameters.FStartDebug.FInitalised := True;
      end;

      if not LDataSet.DataSet.FieldByName('DebugFinal').IsNull then
      begin
        LRunParameters.FEndDebug.FData := LDataSet.DataSet.FieldByName('DebugFinal').AsInteger;
        LRunParameters.FEndDebug.FInitalised := True;
      end;

      if not LDataSet.DataSet.FieldByName('DebugLevel').IsNull then
      begin
        LRunParameters.FDebugLevel.FData := LDataSet.DataSet.FieldByName('DebugLevel').AsInteger;
        LRunParameters.FDebugLevel.FInitalised := True;
      end;

      if not LDataSet.DataSet.FieldByName('PlotOpt').IsNull then
      begin
        LRunParameters.FPlotOption.FData := (Trim(LDataSet.DataSet.FieldByName('PlotOpt').AsString)+' ')[1];
        LRunParameters.FPlotOption.FInitalised := True;
      end;

      if not LDataSet.DataSet.FieldByName('RandomOpt').IsNull then
      begin
        LRunParameters.FRandomNumber.FData := StrToIntDef(Trim(LDataSet.DataSet.FieldByName('RandomOpt').AsString),0);
        LRunParameters.FRandomNumber.FInitalised := True;
      end;

      if(FAppModules.Model.ModelName = CYield) then
      begin
        if not LDataSet.DataSet.FieldByName('SummaryLevel').IsNull then
        begin
          LRunParameters.FSummaryLevel.FData := LDataSet.DataSet.FieldByName('SummaryLevel').AsInteger;
          LRunParameters.FSummaryLevel.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('SummaryOut').IsNull then
        begin
          LRunParameters.FSummaryOutput.FData := StrToInt(Trim(LDataSet.DataSet.FieldByName('SummaryOut').AsString));
          LRunParameters.FSummaryOutput.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('StoreYield').IsNull then
        begin
          LRunParameters.FStoreYield.FData := StrToInt(Trim(LDataSet.DataSet.FieldByName('StoreYield').AsString));
          LRunParameters.FStoreYield.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('LimitOpt').IsNull then
        begin
          LRunParameters.FLimitOption.FData := StrToInt(Trim(LDataSet.DataSet.FieldByName('LimitOpt').AsString));
          LRunParameters.FLimitOption.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('MultPeriodOpt').IsNull then
        begin
          LRunParameters.FMultiplePeriods.FData := LDataSet.DataSet.FieldByName('MultPeriodOpt').AsInteger;
          LRunParameters.FMultiplePeriods.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('CalcHistoryOpt').IsNull then
        begin
          LRunParameters.FCalcHistYield.FData := LDataSet.DataSet.FieldByName('CalcHistoryOpt').AsInteger;
          LRunParameters.FCalcHistYield.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('ReduceSeqOpt').IsNull then
        begin
          LRunParameters.FReduceSequence.FData := LDataSet.DataSet.FieldByName('ReduceSeqOpt').AsInteger;
          LRunParameters.FReduceSequence.FInitalised := True;
        end;
      end
      else
      begin
        //line4 Planning+++++++++++++++++++++++++
        if not LDataSet.DataSet.FieldByName('DetailedOption').IsNull then
        begin
          LRunParameters.FDetailedOption.FData := (Trim(LDataSet.DataSet.FieldByName('DetailedOption').AsString)+' ')[1];
          LRunParameters.FDetailedOption.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('SupplyOption').IsNull then
        begin
          LRunParameters.FSupplyOption.FData := (Trim(LDataSet.DataSet.FieldByName('SupplyOption').AsString)+' ')[1];
          LRunParameters.FSupplyOption.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('AnnualSummary').IsNull then
        begin
          LRunParameters.FAnnualSummary.FData := (Trim(LDataSet.DataSet.FieldByName('AnnualSummary').AsString)+' ')[1];
          LRunParameters.FAnnualSummary.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('EconomicOption').IsNull then
        begin
          LRunParameters.FEconomicOption.FData := (Trim(LDataSet.DataSet.FieldByName('EconomicOption').AsString)+' ')[1];
          LRunParameters.FEconomicOption.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('PlanningSummary').IsNull then
        begin
          LRunParameters.FPlanningSummary.FData := (Trim(LDataSet.DataSet.FieldByName('PlanningSummary').AsString)+' ')[1];
          LRunParameters.FPlanningSummary.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('InputSummary').IsNull then
        begin
          LRunParameters.FInputSummary.FData := (Trim(LDataSet.DataSet.FieldByName('InputSummary').AsString)+' ')[1];
          LRunParameters.FInputSummary.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('WaterQualityOption').IsNull then
        begin
          LRunParameters.FWaterQualityOption.FData := (Trim(LDataSet.DataSet.FieldByName('WaterQualityOption').AsString)+' ')[1];
          LRunParameters.FWaterQualityOption.FInitalised := True;
        end;
      end;

      for LIndex := 1 to 12 do
      begin
        //line5 +++++++++++++++++++++++++
        LFieldName := 'Month'+ IntToStr(LIndex);
        if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
        begin
          LRunParameters.FMonthNames[LIndex].FData := Trim(LDataSet.DataSet.FieldByName(LFieldName).AsString);
          LRunParameters.FMonthNames[LIndex].FInitalised := True;
        end;

        //line6 +++++++++++++++++++++++++
        LFieldName := 'Days'+ IntToStr(LIndex);
        if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
        begin
          LRunParameters.FMonthDays[LIndex].FData := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
          LRunParameters.FMonthDays[LIndex].FInitalised := True;
        end;
      end;

      //line7 +++++++++++++++++++++++++
      if not LDataSet.DataSet.FieldByName('YearsCount').IsNull then
      begin
        LRunParameters.FHydYears.FData := LDataSet.DataSet.FieldByName('YearsCount').AsInteger;
        LRunParameters.FHydYears.FInitalised := True;
      end;

      if not LDataSet.DataSet.FieldByName('HydroSeqCount').IsNull then
      begin
        LRunParameters.FMaxHydSequences.FData := LDataSet.DataSet.FieldByName('HydroSeqCount').AsInteger;
        LRunParameters.FMaxHydSequences.FInitalised := True;
      end;

      if not LDataSet.DataSet.FieldByName('LoadCasesCount').IsNull then
      begin
        LRunParameters.FNoLoadCases.FData := LDataSet.DataSet.FieldByName('LoadCasesCount').AsInteger;
        LRunParameters.FNoLoadCases.FInitalised := True;
      end;

      if not LDataSet.DataSet.FieldByName('StartMonthNo').IsNull then
      begin
        LRunParameters.FStartMonth.FData := LDataSet.DataSet.FieldByName('StartMonthNo').AsInteger;
        LRunParameters.FStartMonth.FInitalised := True;
      end;

      if not LDataSet.DataSet.FieldByName('RunType').IsNull then
      begin
        LRunParameters.FHistStoch.FData := (Trim(LDataSet.DataSet.FieldByName('RunType').AsString)+' ')[1];
        LRunParameters.FHistStoch.FInitalised := True;
      end;

      if not LDataSet.DataSet.FieldByName('StartType').IsNull then
      begin
        LRunParameters.FHistRand.FData := LDataSet.DataSet.FieldByName('StartType').AsInteger;
        LRunParameters.FHistRand.FInitalised := True;
      end;

      if not LDataSet.DataSet.FieldByName('ParamFile').IsNull then
      begin
        LFilePrefix := IncludeTrailingPathDelimiter(Trim(ADataObject.FPathsObject.HydrologyPath.FData));
        LRunParameters.FParamName.FData := LFilePrefix + Trim(LDataSet.DataSet.FieldByName('ParamFile').AsString);
        LRunParameters.FParamName.FInitalised := True;
      end;

      //line7 Planning+++++++++++++++++++++++++
      if not LDataSet.DataSet.FieldByName('PeriodsPerYear').IsNull then
      begin
        LRunParameters.FEconomicTimePeriods.FData := LDataSet.DataSet.FieldByName('PeriodsPerYear').AsInteger;
        LRunParameters.FEconomicTimePeriods.FInitalised := True;
      end;

      LRunParameters.FAnalysisStartYear.FData := LRunParameters.FStartOther.FData;
      LRunParameters.FAnalysisStartYear.FInitalised := LRunParameters.FStartOther.FInitalised;

      if not LDataSet.DataSet.FieldByName('CalendarStartMonth').IsNull then
      begin
        LRunParameters.FMonthStartNewYear.FData := LDataSet.DataSet.FieldByName('CalendarStartMonth').AsInteger;
        LRunParameters.FMonthStartNewYear.FInitalised := True;
      end;

      if not LDataSet.DataSet.FieldByName('ShortTermPlanningOption').IsNull then
      begin
        LRunParameters.FShortTermPlanningOption.FData := (Trim(LDataSet.DataSet.FieldByName('ShortTermPlanningOption').AsString)+' ')[1];
        LRunParameters.FShortTermPlanningOption.FInitalised := True;
      end;

      if not LDataSet.DataSet.FieldByName('HydroPowerOption').IsNull then
      begin
        LRunParameters.FHydroPowerOption.FData := (Trim(LDataSet.DataSet.FieldByName('HydroPowerOption').AsString)+' ')[1];
        LRunParameters.FHydroPowerOption.FInitalised := True;
      end;

      LRunParameters.FAllocationControlOption.FData := 'N';
      LRunParameters.FAllocationControlOption.FInitalised := True;
      if not LDataSet.DataSet.FieldByName('AllocationControlOption').IsNull then
      begin
        LRunParameters.FAllocationControlOption.FData := (Trim(LDataSet.DataSet.FieldByName('AllocationControlOption').AsString)+'N')[1];
        LRunParameters.FAllocationControlOption.FInitalised := True;
      end;

       //line8++++++++++++++++++++++++++++
      for LIndex := 1 to 10 do
      begin
        LFieldName := 'Seq'+ IntToStr(LIndex);
        if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
        begin
          LRunParameters.FAnalSequences[LIndex].FData := LDataSet.DataSet.FieldByName(LFieldName).AsInteger;
          LRunParameters.FAnalSequences[LIndex].FInitalised := True;
        end;

        //line9++++++++++++++++++++++++++++
        LFieldName := 'TYield'+ IntToStr(LIndex);
        if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
        begin
          LRunParameters.FTargetYield[LIndex].FData := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
          LRunParameters.FTargetYield[LIndex].FInitalised := True;
        end;

        //line10+++++++++++++++++++++++++++
        LFieldName := 'TYield'+ IntToStr(LIndex);
        if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
        begin
          LRunParameters.FMaxYield[LIndex].FData := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
          LRunParameters.FMaxYield[LIndex].FInitalised := True;
        end;

        //line11+++++++++++++++++++++++++++
        LFieldName := 'TPower'+ IntToStr(LIndex);
        if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
        begin
          LRunParameters.FTargetPower[LIndex].FData := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
          LRunParameters.FTargetPower[LIndex].FInitalised := True;
        end;
      end;

      {if(LRunParameters.FMaxHydSequences.FData > 10) and
        (UpperCase(LRunParameters.FHistStoch.FData) <> 'H') then
       LRunParameters.FAnalSequences[1].FData := LRunParameters.FAnalSequences[1].FData - 1;}

      // Line12+++++++++++++++++++++++++++++++++++++
      if not LDataSet.DataSet.FieldByName('TargetRecurrenceInterval').IsNull then
      begin
        LRunParameters.FTargetRecurrenceInterval.FData := LDataSet.DataSet.FieldByName('TargetRecurrenceInterval').AsInteger;
        LRunParameters.FTargetRecurrenceInterval.FInitalised := True;
      end;

      // PLANNING+++++++++++++++++++++++++++++++++++++
      if(FAppModules.Model.ModelName = CPlanning) then
      begin

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(ReadDecisionDateSQL);
        LDataSet.DataSet.Open;
        if not LDataSet.DataSet.Eof then
        begin
        if not LDataSet.DataSet.FieldByName('NrOfDecisionDates').IsNull then
          begin
            LRunParameters.FDecisionMonthsNumber.FData := LDataSet.DataSet.FieldByName('NrOfDecisionDates').AsInteger;
            LRunParameters.FDecisionMonthsNumber.FInitalised := True;
          end;

          for LIndex := 1 to 12 do
          begin
            //line8+++++++++++++++++++++++++++
            LFieldName := Format('%s%2.2d',['DecisionMonth',LIndex]);
            if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
            begin
              LRunParameters.FDecisionMonths[LIndex].FData := LDataSet.DataSet.FieldByName(LFieldName).AsInteger;
              LRunParameters.FDecisionMonths[LIndex].FInitalised := True;

              LRunParameters.FDecisionMonthsNumber.FData := LIndex;
              LRunParameters.FDecisionMonthsNumber.FInitalised := True;
            end;

            //line9+++++++++++++++++++++++++++
            LFieldName := Format('%s%2.2d',['DecisionType',LIndex]);
            if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
            begin
              LRunParameters.FDecisionMonthsType[LIndex].FData := (Trim(LDataSet.DataSet.FieldByName(LFieldName).AsString)+' ')[1];
              LRunParameters.FDecisionMonthsType[LIndex].FInitalised := True;
            end;

            //line10+++++++++++++++++++++++++++
            LFieldName := Format('%s%2.2d',['HydroPowerIndicator',LIndex]);
            if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
            begin
              LRunParameters.FHydroPowerDecision[LIndex].FData := (Trim(LDataSet.DataSet.FieldByName(LFieldName).AsString)+' ')[1];
              LRunParameters.FHydroPowerDecision[LIndex].FInitalised := True;
            end;
          end;
        end;

        //line12 +++++++++++++++++++++++++++
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(ReadDemandCentreSQL);
        LDataSet.DataSet.Open;
        while not LDataSet.DataSet.Eof do
        begin
          LDemandCentre  := LRunParameters.AddDemandCenter;
          if not LDataSet.DataSet.FieldByName('DemandCentreType').IsNull then
          begin
            LDemandCentre.FDemandCentreType.FData := (Trim(LDataSet.DataSet.FieldByName('DemandCentreType').AsString)+' ')[1];
            LDemandCentre.FDemandCentreType.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
          begin
            LDemandCentre.FChannelNumber.FData := LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
            LDemandCentre.FChannelNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('AnnualDemand').IsNull then
          begin
            LDemandCentre.FAnnualDemand.FData := LDataSet.DataSet.FieldByName('AnnualDemand').AsFloat;
            LDemandCentre.FAnnualDemand.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('MinimumDemand').IsNull then
          begin
            LDemandCentre.FMinimumDemand.FData := LDataSet.DataSet.FieldByName('MinimumDemand').AsFloat;
            LDemandCentre.FMinimumDemand.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('IncludeInOutput').IsNull then
          begin
            LDemandCentre.FOutputResults.FData := (Trim(LDataSet.DataSet.FieldByName('IncludeInOutput').AsString)+' ')[1];
            LDemandCentre.FOutputResults.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('Comment').IsNull then
          begin
            LDemandCentre.FComment.FData       := TrimRight(LDataSet.DataSet.FieldByName('Comment').AsString);
            LDemandCentre.FComment.FLength     := Length(LDemandCentre.FComment.FData);
            LDemandCentre.FComment.FInitalised := True;
          end;
          LDataSet.DataSet.Next;
        end;
        LRunParameters.FNumberOfDemandCentres.FData := LRunParameters.FDemandCentresList.Count;
        LRunParameters.FNumberOfDemandCentres.FInitalised := True;

        //line13 +++++++++++++++++++++++++++
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(ReadInterBasinSupportSQL);
        LDataSet.DataSet.Open;

        while not LDataSet.DataSet.Eof do
        begin
          LInterBasinTransfer  := LRunParameters.AddInterBasinTransferTransfer;
          if not LDataSet.DataSet.FieldByName('SummaryRequired').IsNull then
          begin
            LInterBasinTransfer.FSummaryRequired.FData := (Trim(LDataSet.DataSet.FieldByName('SummaryRequired').AsString)+' ')[1];
            LInterBasinTransfer.FSummaryRequired.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
          begin
            LInterBasinTransfer.FChannelNumber.FData := LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
            LInterBasinTransfer.FChannelNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('UpperLimit').IsNull then
          begin
            LInterBasinTransfer.FUpperTransferLimit.FData := LDataSet.DataSet.FieldByName('UpperLimit').AsFloat;
            LInterBasinTransfer.FUpperTransferLimit.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('DemandCentreID').IsNull then
          begin
            LInterBasinTransfer.FDemandCentreNumber.FData := LDataSet.DataSet.FieldByName('DemandCentreID').AsInteger;
            LInterBasinTransfer.FDemandCentreNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('Comment').IsNull then
          begin
            LInterBasinTransfer.FComment.FData := TrimRight(LDataSet.DataSet.FieldByName('Comment').AsString);
            LInterBasinTransfer.FComment.FLength := Length(LInterBasinTransfer.FComment.FData);
            LInterBasinTransfer.FComment.FInitalised := True;
          end;
          LDataSet.DataSet.Next;
        end;
        LRunParameters.FNumberOfInterBasinChannels.FData := LRunParameters.FInterBasinTransferList.Count;
        LRunParameters.FNumberOfInterBasinChannels.FInitalised := True;

        //line14 +++++++++++++++++++++++++++
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(ReadDiscountRateSQL);
        LDataSet.DataSet.Open;
        LRunParameters.FNumberOfDiscountRates.FData := 0;
        LRunParameters.FNumberOfDiscountRates.FInitalised := True;
        if(LDataSet.DataSet.RecordCount > 0) then
        begin
          {if not LDataSet.DataSet.FieldByName('NrOfDiscountRates').IsNull then
          begin
            LRunParameters.FNumberOfDiscountRates.FData := LDataSet.DataSet.FieldByName('NrOfDiscountRates').AsInteger;
            LRunParameters.FNumberOfDiscountRates.FInitalised := True;
          end;}
          for LIndex := 1 to 10 do
          begin
            //line8+++++++++++++++++++++++++++
            LFieldName := Format('%s%2.2d',['DiscountRate',LIndex]);
            if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
            begin
              LRunParameters.FDiscountRates[LIndex].FData := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
              LRunParameters.FDiscountRates[LIndex].FInitalised := True;
              LRunParameters.FNumberOfDiscountRates.FData := LRunParameters.FNumberOfDiscountRates.FData + 1;
            end;
          end;
        end;

        //Line16+++++++++++++++++++++++++++++++++
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(ReadManualAnalysisSQL);
        LDataSet.DataSet.Open;
        while not LDataSet.DataSet.Eof do
        begin
          if (LDataSet.DataSet.FieldByName('AnalysisType').AsString = 'Hist') then
          begin
            for LIndex := 1 to 5 do
            begin
              LFieldName := Format('%s%d',['Seq',LIndex]);
              if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
              begin
                LRunParameters.FHistManualAnalysis[LIndex].FData := LDataSet.DataSet.FieldByName(LFieldName).AsInteger;
                LRunParameters.FHistManualAnalysis[LIndex].FInitalised := True;
              end;
            end;
          end;
          if (LDataSet.DataSet.FieldByName('AnalysisType').AsString = 'Stoc') then
          begin
            for LIndex := 1 to 5 do
            begin
              LFieldName := Format('%s%d',['Seq',LIndex]);
              if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
              begin
                LRunParameters.FStocManualAnalysis[LIndex].FData := LDataSet.DataSet.FieldByName(LFieldName).AsInteger;
                LRunParameters.FStocManualAnalysis[LIndex].FInitalised := True;
              end;
            end;
          end;

          LDataSet.DataSet.Next;
        end;
        //Line16+++++++++++++++++++++++++++++++++
       { LDataSet.DataSet.Close;
        LDataSet.SetSQL(ReadMinMaxBlockTypeConfigSQL);
        LDataSet.DataSet.Open;
        while not LDataSet.DataSet.Eof do
        begin
          if (LDataSet.DataSet.FieldByName('BlockType').AsString = 'UB') then
          begin
            LRunParameters.FNoOfFlowsUpperBounds.FData := LDataSet.DataSet.FieldByName('BlockCount').AsInteger;
            LRunParameters.FNoOfFlowsUpperBounds.FInitalised := True;
          end;
          if (LDataSet.DataSet.FieldByName('BlockType').AsString = 'WQCon') then
          begin
            LRunParameters.FNoOfWQConstraints.FData := LDataSet.DataSet.FieldByName('BlockCount').AsInteger;
            LRunParameters.FNoOfWQConstraints.FInitalised := True;
          end;
          LDataSet.DataSet.Next;
        end;
         }
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(ReadMinMaxBoundsSQL);
        LDataSet.DataSet.Open;
        LDataSet.DataSet.Last;
        LDataSet.DataSet.First;
        if (LDataSet.DataSet.RecordCount>0) then
        begin
          LRunParameters.FNoOfFlowsUpperBounds.FData := LDataSet.DataSet.RecordCount;
          LRunParameters.FNoOfFlowsUpperBounds.FInitalised := True;
        end;
        while not LDataSet.DataSet.Eof do
        begin
          LMinMaxBoundsObject := LRunParameters.AddMinMaxBoundsObject;
          if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
          begin
            LMinMaxBoundsObject.FMinMaxBoundedChannel.FData := LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
            LMinMaxBoundsObject.FMinMaxBoundedChannel.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('ReferenceChannelCount').IsNull then
          begin
            LMinMaxBoundsObject.FNoOfRefChannels.FData := LDataSet.DataSet.FieldByName('ReferenceChannelCount').AsInteger;
            LMinMaxBoundsObject.FNoOfRefChannels.FInitalised := True;
          end;
          if LMinMaxBoundsObject.FNoOfRefChannels.FInitalised then
            LMinMaxBoundsObject.FRefChannels.CommaText:= LDataSet.DataSet.FieldByName('ReferenceChannels').AsString;

          LDataSet.DataSet.Next;
        end;

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(ReadWQConstraintsSQL);
        LDataSet.DataSet.Open;
        LDataSet.DataSet.Last;
        LDataSet.DataSet.First;
        if (LDataSet.DataSet.RecordCount>0) then
        begin
          LRunParameters.FNoOfWQConstraints.FData := LDataSet.DataSet.RecordCount;
          LRunParameters.FNoOfWQConstraints.FInitalised := True;
        end;
        while not LDataSet.DataSet.Eof do
        begin
          LWQConstraintsObject := LRunParameters.AddWQConstraintsObject;
          if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
          begin
            LWQConstraintsObject.FWQConMinMaxChannel.FData := LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
            LWQConstraintsObject.FWQConMinMaxChannel.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('WQTarget').IsNull then
          begin
            LWQConstraintsObject.FWQConTarget.FData := LDataSet.DataSet.FieldByName('WQTarget').AsFloat;
            LWQConstraintsObject.FWQConTarget.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('BlendingRefChannelCount').IsNull then
          begin
            LWQConstraintsObject.FNoOfRefChannelsBlending.FData := LDataSet.DataSet.FieldByName('BlendingRefChannelCount').AsInteger;
            LWQConstraintsObject.FNoOfRefChannelsBlending.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('ReservoirRef').IsNull then
          begin
            LWQConstraintsObject.FReservoirRef.FData := LDataSet.DataSet.FieldByName('ReservoirRef').AsInteger;
            LWQConstraintsObject.FReservoirRef.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('WQConType').IsNull then
          begin
            LWQConstraintsObject.FWQConType.FData := LDataSet.DataSet.FieldByName('WQConType').AsInteger;
            LWQConstraintsObject.FWQConType.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('ReferenceChannels').IsNull then
            LWQConstraintsObject.FReferenceChannel.CommaText := LDataSet.DataSet.FieldByName('ReferenceChannels').AsString;

          if not LDataSet.DataSet.FieldByName('ReferenceChannelFactors').IsNull then
            LWQConstraintsObject.FRefChannelFactor.CommaText := LDataSet.DataSet.FieldByName('ReferenceChannelFactors').AsString;

          if not LDataSet.DataSet.FieldByName('SlopeLimit').IsNull then
          begin
            LWQConstraintsObject.FSlopeLimit.FData := LDataSet.DataSet.FieldByName('SlopeLimit').AsInteger;
            LWQConstraintsObject.FSlopeLimit.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('EstimatedRelease').IsNull then
            LWQConstraintsObject.FEstimatedRelease.CommaText := LDataSet.DataSet.FieldByName('EstimatedRelease').AsString;

          if not LDataSet.DataSet.FieldByName('Concentration').IsNull then
            LWQConstraintsObject.FConcentration.CommaText := LDataSet.DataSet.FieldByName('Concentration').AsString;

          LDataSet.DataSet.Next;

        end;

      end;
      //line13 on wards+++++++++++++++++++++++++++
      LDataSet.DataSet.Active := False;
      LDataSet.SetSQL(ReadF01UnkownDataSQL);
      LDataSet.SetParams(['FileType', 'FileGroup'],
        [IntToStr(AFileName.FileNumber), IntToStr(AFileName.FileGroup)]);
      LDataSet.DataSet.Open;

      while not LDataSet.DataSet.Eof do
      begin
        LRunParameters.FF01ExtraLines.Add(TrimRight(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;
      if(FAppModules.Model.ModelName <> CPlanning) then
        UpdateWithChangeLists(LRunParameters);

      LMessage := FAppModules.Language.GetString('TFile01DatabaseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile01DatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile01DatabaseAgent.WriteModelDataToDatabase';
var
  LParamName,
  LMessage       : string;
  LIndex,
  LCount         : integer;
  LDataSet       : TAbstractModelDataset;
  LRunParameters : TRunParametersObject;
  LStop          : boolean;
  LDemandCentre  : TDemandCentreObject;
  LInterBasinTransfer : TInterBasinTransferObject;
  LMinMaxBoundsObject : TMinMaxBoundsObject;
  LWQConstraintsObject : TWQConstraintsObject;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile01DatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not ClearModelDataInDatabase(TFileNameObject(AFileName),AProgressFunction,True) then
      Exit;

    LRunParameters := ADataObject.FRunParametersObject;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try

      //Title Lines
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(WriteRunTitleDataSQL);

      LDataset.ClearQueryParams(); {RHS}
      LDataSet.SetParams(
        ['Model', 'StudyAreaName', 'SubArea', 'Scenario'],
        [FAppModules.StudyArea.ModelCode, FAppModules.StudyArea.StudyAreaCode,
         FAppModules.StudyArea.SubAreaCode, FAppModules.StudyArea.ScenarioCode]);
      if LRunParameters.FTitle[1].FInitalised then
        LDataSet.SetParams(['Title1'], [LRunParameters.FTitle[1].FData]);
      if LRunParameters.FTitle[2].FInitalised then
        LDataSet.SetParams(['Title2'], [LRunParameters.FTitle[2].FData]);
      if LRunParameters.FTitle[3].FInitalised then
        LDataSet.SetParams(['Title3'], [LRunParameters.FTitle[3].FData]);

      LDataSet.ExecSQL;

      //line4++++++++++++++++++++++++++++
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(WriteRunParametersDataSQL);
      LDataset.ClearQueryParams();
      LDataSet.SetParams(
        ['Model','StudyAreaName','SubArea','Scenario'],
        [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
         FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode]);

      if LRunParameters.FTimePeriods.FInitalised then
         LDataSet.SetParams(['NumPeriods'], [IntToStr(LRunParameters.FTimePeriods.FData)]);

      if LRunParameters.FStartGregorian.FInitalised then
         LDataSet.SetParams(['StartYearG'], [IntToStr(LRunParameters.FStartGregorian.FData)]);

      if LRunParameters.FStartOther.FInitalised then
         LDataSet.SetParams(['StartYearO'], [IntToStr(LRunParameters.FStartOther.FData)]);

      if LRunParameters.FStartDebug.FInitalised then
         LdataSet.SetParams(['DebugInit'], [IntToStr(LRunParameters.FStartDebug.FData)]);

      if LRunParameters.FEndDebug.FInitalised then
         LDataSet.SetParams(['DebugFinal'], [IntToStr(LRunParameters.FEndDebug.FData)]);

      if LRunParameters.FDebugLevel.FInitalised then
         LDataSet.SetParams(['DebugLevel'], [IntToStr(LRunParameters.FDebugLevel.FData)]);

      if LRunParameters.FPlotOption.FInitalised then
         LDataSet.SetParams(['PlotOpt'], [LRunParameters.FPlotOption.FData]);

      if LRunParameters.FRandomNumber.FInitalised then
         LDataSet.SetParams(['RandomOpt'], [IntToStr(LRunParameters.FRandomNumber.FData)]);

      if(FAppModules.Model.ModelName = CYield) then
      begin
        if LRunParameters.FSummaryLevel.FInitalised then
           LDataSet.SetParams(['SummaryLevel'], [IntToStr(LRunParameters.FSummaryLevel.FData)]);

        if LRunParameters.FSummaryOutput.FInitalised then
           LDataSet.SetParams(['SummaryOut'], [IntToStr(LRunParameters.FSummaryOutput.FData)]);

        if LRunParameters.FStoreYield.FInitalised then
           LDataSet.SetParams(['StoreYield'], [IntToStr(LRunParameters.FStoreYield.FData)]);

        if LRunParameters.FLimitOption.FInitalised then
           LDataSet.SetParams(['LimitOpt'], [IntToStr(LRunParameters.FLimitOption.FData)]);

        if LRunParameters.FMultiplePeriods.FInitalised then
           LDataSet.SetParams(['MultPeriodOpt'], [IntToStr(LRunParameters.FMultiplePeriods.FData)]);

        if LRunParameters.FCalcHistYield.FInitalised then
           LDataSet.SetParams(['CalcHistoryOpt'], [IntToStr(LRunParameters.FCalcHistYield.FData)]);

        if LRunParameters.FReduceSequence.FInitalised then
           LDataSet.SetParams(['ReduceSeqOpt'], [IntToStr(LRunParameters.FReduceSequence.FData)]);
       end
       else
       begin
        if LRunParameters.FDetailedOption.FInitalised then
           LDataSet.SetParams(['DetailedOption'], [LRunParameters.FDetailedOption.FData]);

        if LRunParameters.FSupplyOption.FInitalised then
           LDataSet.SetParams(['SupplyOption'], [LRunParameters.FSupplyOption.FData]);

        if LRunParameters.FAnnualSummary.FInitalised then
           LDataSet.SetParams(['AnnualSummary'], [LRunParameters.FAnnualSummary.FData]);

        if LRunParameters.FEconomicOption.FInitalised then
           LDataSet.SetParams(['EconomicOption'], [LRunParameters.FEconomicOption.FData]);

        if LRunParameters.FPlanningSummary.FInitalised then
           LDataSet.SetParams(['PlanningSummary'], [LRunParameters.FPlanningSummary.FData]);

        if LRunParameters.FInputSummary.FInitalised then
           LDataSet.SetParams(['InputSummary'], [LRunParameters.FInputSummary.FData]);

        if LRunParameters.FWaterQualityOption.FInitalised then
           LDataSet.SetParams(['WaterQualityOption'], [LRunParameters.FWaterQualityOption.FData]);
       end;

      //line7 +++++++++++++++++++++++++++++
      if LRunParameters.FHydYears.FInitalised then
         LDataSet.SetParams(['YearsCount'], [IntToStr(LRunParameters.FHydYears.FData)]);

      if LRunParameters.FMaxHydSequences.FInitalised then
         LDataSet.SetParams(['HydroSeqCount'], [IntToStr(LRunParameters.FMaxHydSequences.FData)]);

      if LRunParameters.FNoLoadCases.FInitalised then
         LDataSet.SetParams(['LoadCasesCount'], [IntToStr(LRunParameters.FNoLoadCases.FData)]);

      if LRunParameters.FStartMonth.FInitalised then
         LDataSet.SetParams(['StartMonthNo'], [IntToStr(LRunParameters.FStartMonth.FData)]);

      if LRunParameters.FHistStoch.FInitalised then
         LDataSet.SetParams(['RunType'], [LRunParameters.FHistStoch.FData]);

      if LRunParameters.FHistRand.FInitalised then
         LDataSet.SetParams(['StartType'], [IntToStr(LRunParameters.FHistRand.FData)]);

      if LRunParameters.FParamName.FInitalised then
         LDataSet.SetParams(['ParamFile'], [ExtractFileName(LRunParameters.FParamName.FData)]);

      if LRunParameters.FEconomicTimePeriods.FInitalised then
         LDataSet.SetParams(['PeriodsPerYear'], [IntToStr(LRunParameters.FEconomicTimePeriods.FData)]);

      if LRunParameters.FMonthStartNewYear.FInitalised then
         LDataSet.SetParams(['CalendarStartMonth'], [IntToStr(LRunParameters.FMonthStartNewYear.FData)]);

      if LRunParameters.FShortTermPlanningOption.FInitalised then
         LDataSet.SetParams(['ShortTermPlanningOption'], [LRunParameters.FShortTermPlanningOption.FData]);

      if LRunParameters.FHydroPowerOption.FInitalised then
         LDataSet.SetParams(['HydroPowerOption'], [LRunParameters.FHydroPowerOption.FData]);

      if LRunParameters.FAllocationControlOption.FInitalised then
         LDataSet.SetParams(['AllocationControlOption'], [LRunParameters.FAllocationControlOption.FData]);

      //Line 12+++++++++++++++++++++++++++++++++++
      if LRunParameters.FTargetRecurrenceInterval.FInitalised then
         LDataSet.SetParams(['TargetRecurrenceInterval'], [IntToStr(LRunParameters.FTargetRecurrenceInterval.FData)]);


      LDataSet.ExecSQL;

       //months
      //line5 ++++++++++++++++++++++++++++
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(WriteMonthNamesDataSQL);
      LDataset.ClearQueryParams();
      LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
      LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
      LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
      LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
      if LRunParameters.FMonthNames[1].FInitalised then
         LDataSet.SetParams(['Month1'], [LRunParameters.FMonthNames[1].FData]);

      if LRunParameters.FMonthNames[2].FInitalised then
         LDataSet.SetParams(['Month2'], [LRunParameters.FMonthNames[2].FData]);

      if LRunParameters.FMonthNames[3].FInitalised then
         LDataSet.SetParams(['Month3'], [LRunParameters.FMonthNames[3].FData]);

      if LRunParameters.FMonthNames[4].FInitalised then
         LDataSet.SetParams(['Month4'], [LRunParameters.FMonthNames[4].FData]);

      if LRunParameters.FMonthNames[5].FInitalised then
         LDataSet.SetParams(['Month5'], [LRunParameters.FMonthNames[5].FData]);

      if LRunParameters.FMonthNames[6].FInitalised then
         LDataSet.SetParams(['Month6'], [LRunParameters.FMonthNames[6].FData]);

      if LRunParameters.FMonthNames[7].FInitalised then
         LDataSet.SetParams(['Month7'], [LRunParameters.FMonthNames[7].FData]);

      if LRunParameters.FMonthNames[8].FInitalised then
         LDataSet.SetParams(['Month8'], [LRunParameters.FMonthNames[8].FData]);

      if LRunParameters.FMonthNames[9].FInitalised then
         LDataSet.SetParams(['Month9'], [LRunParameters.FMonthNames[9].FData]);

      if LRunParameters.FMonthNames[10].FInitalised then
         LDataSet.SetParams(['Month10'], [LRunParameters.FMonthNames[10].FData]);

      if LRunParameters.FMonthNames[11].FInitalised then
         LDataSet.SetParams(['Month11'], [LRunParameters.FMonthNames[11].FData]);

      if LRunParameters.FMonthNames[12].FInitalised then
         LDataSet.SetParams(['Month12'], [LRunParameters.FMonthNames[12].FData]);

      LDataSet.ExecSQL;

      //monthdays
      //line6 +++++++++++++++++++++++++++++

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(WriteDaysPerMonthDataSQL);
      LDataset.ClearQueryParams();
      LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
      LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
      LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
      LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
      if LRunParameters.FMonthDays[1].FInitalised then
         LDataSet.SetParams(['Days1'], [FloatToStr(LRunParameters.FMonthDays[1].FData)]);

      if LRunParameters.FMonthDays[2].FInitalised then
         LDataSet.SetParams(['Days2'], [FloatToStr(LRunParameters.FMonthDays[2].FData)]);

      if LRunParameters.FMonthDays[3].FInitalised then
         LDataSet.SetParams(['Days3'], [FloatToStr(LRunParameters.FMonthDays[3].FData)]);

      if LRunParameters.FMonthDays[4].FInitalised then
         LDataSet.SetParams(['Days4'], [FloatToStr(LRunParameters.FMonthDays[4].FData)]);

      if LRunParameters.FMonthDays[5].FInitalised then
         LDataSet.SetParams(['Days5'], [FloatToStr(LRunParameters.FMonthDays[5].FData)]);

      if LRunParameters.FMonthDays[6].FInitalised then
         LDataSet.SetParams(['Days6'], [FloatToStr(LRunParameters.FMonthDays[6].FData)]);

      if LRunParameters.FMonthDays[7].FInitalised then
         LDataSet.SetParams(['Days7'], [FloatToStr(LRunParameters.FMonthDays[7].FData)]);

      if LRunParameters.FMonthDays[8].FInitalised then
         LDataSet.SetParams(['Days8'], [FloatToStr(LRunParameters.FMonthDays[8].FData)]);

      if LRunParameters.FMonthDays[9].FInitalised then
         LDataSet.SetParams(['Days9'], [FloatToStr(LRunParameters.FMonthDays[9].FData)]);

      if LRunParameters.FMonthDays[10].FInitalised then
         LDataSet.SetParams(['Days10'], [FloatToStr(LRunParameters.FMonthDays[10].FData)]);

      if LRunParameters.FMonthDays[11].FInitalised then
         LDataSet.SetParams(['Days11'], [FloatToStr(LRunParameters.FMonthDays[11].FData)]);

      if LRunParameters.FMonthDays[12].FInitalised then
         LDataSet.SetParams(['Days12'], [FloatToStr(LRunParameters.FMonthDays[12].FData)]);

      LDataSet.ExecSQL;


       //line8++++++++++++++++++++++++++++
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(WriteAnlySequencesDataSQL);
      LDataset.ClearQueryParams();
      LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
      LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
      LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
      LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
      {if LRunParameters.FAnalSequences[1].FInitalised then
      begin
        if(LRunParameters.FMaxHydSequences.FData > 10) and
          (UpperCase(LRunParameters.FHistStoch.FData) <> 'H') then
         LDataSet.SetParams(['Seq1'], [IntToStr(LRunParameters.FAnalSequences[1].FData+1)])
        else
         LDataSet.SetParams(['Seq1'], [IntToStr(LRunParameters.FAnalSequences[1].FData)]);
      end;}

      if LRunParameters.FAnalSequences[1].FInitalised then
         LDataSet.SetParams(['Seq1'], [IntToStr(LRunParameters.FAnalSequences[1].FData)]);

      if LRunParameters.FAnalSequences[2].FInitalised then
         LDataSet.SetParams(['Seq2'], [IntToStr(LRunParameters.FAnalSequences[2].FData)]);

      if LRunParameters.FAnalSequences[3].FInitalised then
         LDataSet.SetParams(['Seq3'], [IntToStr(LRunParameters.FAnalSequences[3].FData)]);

      if LRunParameters.FAnalSequences[4].FInitalised then
         LDataSet.SetParams(['Seq4'], [IntToStr(LRunParameters.FAnalSequences[4].FData)]);

      if LRunParameters.FAnalSequences[5].FInitalised then
         LDataSet.SetParams(['Seq5'], [IntToStr(LRunParameters.FAnalSequences[5].FData)]);

      if LRunParameters.FAnalSequences[6].FInitalised then
         LDataSet.SetParams(['Seq6'], [IntToStr(LRunParameters.FAnalSequences[6].FData)]);

      if LRunParameters.FAnalSequences[7].FInitalised then
         LDataSet.SetParams(['Seq7'], [IntToStr(LRunParameters.FAnalSequences[7].FData)]);

      if LRunParameters.FAnalSequences[8].FInitalised then
         LDataSet.SetParams(['Seq8'], [IntToStr(LRunParameters.FAnalSequences[8].FData)]);

      if LRunParameters.FAnalSequences[9].FInitalised then
         LDataSet.SetParams(['Seq9'], [IntToStr(LRunParameters.FAnalSequences[9].FData)]);

      if LRunParameters.FAnalSequences[10].FInitalised then
         LDataSet.SetParams(['Seq10'], [IntToStr(LRunParameters.FAnalSequences[10].FData)]);

      LDataSet.ExecSQL;

       //line9++++++++++++++++++++++++++++
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(WriteTargetYieldDataSQL);
      LDataset.ClearQueryParams();
      LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
      LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
      LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
      LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
      if LRunParameters.FTargetYield[1].FInitalised then
         LDataSet.SetParams(['TYield1'], [FloatToStr(LRunParameters.FTargetYield[1].FData)]);

      if LRunParameters.FTargetYield[2].FInitalised then
         LDataSet.SetParams(['TYield2'], [FloatToStr(LRunParameters.FTargetYield[2].FData)]);

      if LRunParameters.FTargetYield[3].FInitalised then
         LDataSet.SetParams(['TYield3'], [FloatToStr(LRunParameters.FTargetYield[3].FData)]);

      if LRunParameters.FTargetYield[4].FInitalised then
         LDataSet.SetParams(['TYield4'], [FloatToStr(LRunParameters.FTargetYield[4].FData)]);

      if LRunParameters.FTargetYield[5].FInitalised then
         LDataSet.SetParams(['TYield5'], [FloatToStr(LRunParameters.FTargetYield[5].FData)]);

      if LRunParameters.FTargetYield[6].FInitalised then
         LDataSet.SetParams(['TYield6'], [FloatToStr(LRunParameters.FTargetYield[6].FData)]);

      if LRunParameters.FTargetYield[7].FInitalised then
         LDataSet.SetParams(['TYield7'], [FloatToStr(LRunParameters.FTargetYield[7].FData)]);

      if LRunParameters.FTargetYield[8].FInitalised then
         LDataSet.SetParams(['TYield8'], [FloatToStr(LRunParameters.FTargetYield[8].FData)]);

      if LRunParameters.FTargetYield[9].FInitalised then
         LDataSet.SetParams(['TYield9'], [FloatToStr(LRunParameters.FTargetYield[9].FData)]);

      if LRunParameters.FTargetYield[10].FInitalised then
         LDataSet.SetParams(['TYield10'], [FloatToStr(LRunParameters.FTargetYield[10].FData)]);

      LDataSet.ExecSQL;

       //line10++++++++++++++++++++++++++++
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(WriteMaxYieldDataSQL);
      LDataset.ClearQueryParams();
      LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
      LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
      LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
      LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
      if LRunParameters.FMaxYield[1].FInitalised then
         LDataSet.SetParams(['MYield1'], [FloatToStr(LRunParameters.FMaxYield[1].FData)]);

      if LRunParameters.FMaxYield[2].FInitalised then
         LDataSet.SetParams(['MYield2'], [FloatToStr(LRunParameters.FMaxYield[2].FData)]);

      if LRunParameters.FMaxYield[3].FInitalised then
         LDataSet.SetParams(['MYield3'], [FloatToStr(LRunParameters.FMaxYield[3].FData)]);

      if LRunParameters.FMaxYield[4].FInitalised then
         LDataSet.SetParams(['MYield4'], [FloatToStr(LRunParameters.FMaxYield[4].FData)]);

      if LRunParameters.FMaxYield[5].FInitalised then
         LDataSet.SetParams(['MYield5'], [FloatToStr(LRunParameters.FMaxYield[5].FData)]);

      if LRunParameters.FMaxYield[6].FInitalised then
         LDataSet.SetParams(['MYield6'], [FloatToStr(LRunParameters.FMaxYield[6].FData)]);

      if LRunParameters.FMaxYield[7].FInitalised then
         LDataSet.SetParams(['MYield7'], [FloatToStr(LRunParameters.FMaxYield[7].FData)]);

      if LRunParameters.FMaxYield[8].FInitalised then
         LDataSet.SetParams(['MYield8'], [FloatToStr(LRunParameters.FMaxYield[8].FData)]);

      if LRunParameters.FMaxYield[9].FInitalised then
         LDataSet.SetParams(['MYield9'], [FloatToStr(LRunParameters.FMaxYield[9].FData)]);

      if LRunParameters.FMaxYield[10].FInitalised then
         LDataSet.SetParams(['MYield10'], [FloatToStr(LRunParameters.FMaxYield[10].FData)]);

      LDataSet.ExecSQL;

      //line11++++++++++++++++++++++++++++
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(WriteTargetPowerDataSQL);
      LDataset.ClearQueryParams();
      LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
      LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
      LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
      LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
      if LRunParameters.FTargetPower[1].FInitalised then
         LDataSet.SetParams(['TPower1'], [FloatToStr(LRunParameters.FTargetPower[1].FData)]);

      if LRunParameters.FTargetPower[2].FInitalised then
         LDataSet.SetParams(['TPower2'], [FloatToStr(LRunParameters.FTargetPower[2].FData)]);

      if LRunParameters.FTargetPower[3].FInitalised then
         LDataSet.SetParams(['TPower3'], [FloatToStr(LRunParameters.FTargetPower[3].FData)]);

      if LRunParameters.FTargetPower[4].FInitalised then
         LDataSet.SetParams(['TPower4'], [FloatToStr(LRunParameters.FTargetPower[4].FData)]);

      if LRunParameters.FTargetPower[5].FInitalised then
         LDataSet.SetParams(['TPower5'], [FloatToStr(LRunParameters.FTargetPower[5].FData)]);

      if LRunParameters.FTargetPower[6].FInitalised then
         LDataSet.SetParams(['TPower6'], [FloatToStr(LRunParameters.FTargetPower[6].FData)]);

      if LRunParameters.FTargetPower[7].FInitalised then
         LDataSet.SetParams(['TPower7'], [FloatToStr(LRunParameters.FTargetPower[7].FData)]);

      if LRunParameters.FTargetPower[8].FInitalised then
         LDataSet.SetParams(['TPower8'], [FloatToStr(LRunParameters.FTargetPower[8].FData)]);

      if LRunParameters.FTargetPower[9].FInitalised then
         LDataSet.SetParams(['TPower9'], [FloatToStr(LRunParameters.FTargetPower[9].FData)]);

      if LRunParameters.FTargetPower[10].FInitalised then
         LDataSet.SetParams(['TPower10'], [FloatToStr(LRunParameters.FTargetPower[10].FData)]);

      LDataSet.ExecSQL;

      //PLanning++++++++++++++++++++++++++++
      if(FAppModules.Model.ModelName = CPlanning) then
      begin

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteDecisionDateSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);

        if LRunParameters.FDecisionMonthsNumber.FInitalised then
           LDataSet.SetParams(['NrOfDecisionDates'], [IntToStr(LRunParameters.FDecisionMonthsNumber.FData)]);

        for LIndex := 1 to 12 do
        begin
          //line8+++++++++++++++++++++++++++
          if LRunParameters.FDecisionMonths[LIndex].FInitalised then
          begin
            LParamName := Format('%s%2.2d',['DecisionMonth',LIndex]);
            LDataSet.SetParams([LParamName], [IntToStr(LRunParameters.FDecisionMonths[LIndex].FData)]);
          end;

          //line9+++++++++++++++++++++++++++
          if LRunParameters.FDecisionMonthsType[LIndex].FInitalised then
          begin
            LParamName := Format('%s%2.2d',['DecisionType',LIndex]);
            LDataSet.SetParams([LParamName], [LRunParameters.FDecisionMonthsType[LIndex].FData]);
          end;

          //line10+++++++++++++++++++++++++++
          if LRunParameters.FHydroPowerDecision[LIndex].FInitalised then
          begin
            LParamName := Format('%s%2.2d',['HydroPowerIndicator',LIndex]);
            LDataSet.SetParams([LParamName], [LRunParameters.FHydroPowerDecision[LIndex].FData]);
          end;
        end;
        LDataSet.ExecSQL;

        //line12+++++++++++++++++++++++++++
        for LIndex := 0 to LRunParameters.FDemandCentresList.Count -1 do
        begin
          LDataSet.DataSet.Close;
          LDataSet.SetSQL(WriteDemandCentreSQL);
          LDataset.ClearQueryParams();
          LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LDataSet.SetParams(['DemandCentreID'], [IntToStr(LIndex+1)]);

          LDemandCentre  := TDemandCentreObject(LRunParameters.FDemandCentresList[LIndex]);
          if LDemandCentre.FDemandCentreType.FInitalised then
             LDataSet.SetParams(['DemandCentreType'], [LDemandCentre.FDemandCentreType.FData]);
          if LDemandCentre.FChannelNumber.FInitalised then
             LDataSet.SetParams(['ChannelNumber'], [IntToStr(LDemandCentre.FChannelNumber.FData)]);
          if LDemandCentre.FAnnualDemand.FInitalised then
             LDataSet.SetParams(['AnnualDemand'], [FloatToStr(LDemandCentre.FAnnualDemand.FData)]);
          if LDemandCentre.FMinimumDemand.FInitalised then
             LDataSet.SetParams(['MinimumDemand'], [FloatToStr(LDemandCentre.FMinimumDemand.FData)]);
          if LDemandCentre.FOutputResults.FInitalised then
             LDataSet.SetParams(['IncludeInOutput'], [LDemandCentre.FOutputResults.FData]);
          if LDemandCentre.FComment.FInitalised then
             LDataSet.SetParams(['Comment'], [LDemandCentre.FComment.FData]);
          LDataSet.ExecSQL;
        end;

        //line13+++++++++++++++++++++++++++
        for LIndex := 0 to LRunParameters.FInterBasinTransferList.Count -1 do
        begin
          LDataSet.DataSet.Close;
          LDataSet.SetSQL(WriteInterBasinSupportSQL);
          LDataset.ClearQueryParams();
          LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LDataSet.SetParams(['InterBasinSupportID'], [IntToStr(LIndex+1)]);

          LInterBasinTransfer  := TInterBasinTransferObject(LRunParameters.FInterBasinTransferList[LIndex]);
          if LInterBasinTransfer.FSummaryRequired.FInitalised then
             LDataSet.SetParams(['SummaryRequired'], [LInterBasinTransfer.FSummaryRequired.FData]);
          if LInterBasinTransfer.FChannelNumber.FInitalised then
             LDataSet.SetParams(['ChannelNumber'], [IntToStr(LInterBasinTransfer.FChannelNumber.FData)]);
          if LInterBasinTransfer.FUpperTransferLimit.FInitalised then
             LDataSet.SetParams(['UpperLimit'], [FloatToStr(LInterBasinTransfer.FUpperTransferLimit.FData)]);
          if LInterBasinTransfer.FDemandCentreNumber.FInitalised then
             LDataSet.SetParams(['DemandCentreID'], [IntToStr(LInterBasinTransfer.FDemandCentreNumber.FData)]);
          if LInterBasinTransfer.FComment.FInitalised then
             LDataSet.SetParams(['Comment'], [LInterBasinTransfer.FComment.FData]);
          LDataSet.ExecSQL;
        end;

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteDiscountRateSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['NrOfDiscountRates'], [IntToStr(LRunParameters.FNumberOfDiscountRates.FData)]);

        for LIndex := 1 to 10 do
        begin
          //line8+++++++++++++++++++++++++++
          if LRunParameters.FDiscountRates[LIndex].FInitalised then
          begin
            LParamName := Format('%s%2.2d',['DiscountRate',LIndex]);
            LDataSet.SetParams([LParamName], [FloatToStr(LRunParameters.FDiscountRates[LIndex].FData)]);
          end;
        end;
        LDataSet.ExecSQL;

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteManualAnalysisSQL);
        LDataset.ClearQueryParams();

        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['AnalysisType'], ['Hist']);
        for LIndex := 1 to 5 do
        begin
          if LRunParameters.FHistManualAnalysis[LIndex].FInitalised then
          begin
            LParamName := Format('%s%d',['Seq',LIndex]);
            LDataSet.SetParams([LParamName], [FloatToStr(LRunParameters.FHistManualAnalysis[LIndex].FData)]);
          end;
        end;
        LDataSet.ExecSQL;

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteManualAnalysisSQL);
        LDataset.ClearQueryParams();

        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['AnalysisType'], ['Stoc']);
        for LIndex := 1 to 5 do
        begin
          if LRunParameters.FStocManualAnalysis[LIndex].FInitalised then
          begin
            LParamName := Format('%s%d',['Seq',LIndex]);
            LDataSet.SetParams([LParamName], [FloatToStr(LRunParameters.FStocManualAnalysis[LIndex].FData)]);
          end;
        end;
        LDataSet.ExecSQL;

        if (LRunParameters.FWaterQualityOption.FInitalised) and (LRunParameters.FWaterQualityOption.FData = 'Y') then
        begin
          if LRunParameters.FNoOfFlowsUpperBounds.FInitalised then
          begin
        
            for LIndex := 1 to LRunParameters.FNoOfFlowsUpperBounds.FData do
            begin
              LMinMaxBoundsObject := TMinMaxBoundsObject(LRunParameters.FMinMaxUpperBoundList[LIndex-1]);
              if LMinMaxBoundsObject <> nil then
              begin
                LDataSet.DataSet.Close;
                LDataSet.SetSQL(WriteMinMaxBoundsSQL);
                LDataset.ClearQueryParams();
                LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
                LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
                LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
                LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
                LDataSet.SetParams(['Identifier'],[IntToStr(LIndex)]);
                if LMinMaxBoundsObject.FMinMaxBoundedChannel.FInitalised then
                  LDataSet.SetParams(['ChannelNumber'],[IntToStr(LMinMaxBoundsObject.FMinMaxBoundedChannel.FData)]);
                if LMinMaxBoundsObject.FNoOfRefChannels.FInitalised then
                begin
                  LDataSet.SetParams(['ReferenceChannelCount'],[IntToStr(LMinMaxBoundsObject.FNoOfRefChannels.FData)]);
                  LDataSet.SetParams(['ReferenceChannels'],[LMinMaxBoundsObject.FRefChannels.CommaText]);
                end;
                LDataSet.ExecSQL;
              end;
            end;
          end;

          if LRunParameters.FNoOfWQConstraints.FInitalised then
          begin 
            for LIndex := 1 to LRunParameters.FNoOfWQConstraints.FData do
            begin
              LWQConstraintsObject := TWQConstraintsObject(LRunParameters.FWQConstraintsList[LIndex-1]);
              if LWQConstraintsObject <> nil then
              begin
                LDataSet.DataSet.Close;
                LDataSet.SetSQL(WriteWQConstraintsSQL);
                LDataset.ClearQueryParams();
                LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
                LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
                LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
                LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
                LDataSet.SetParams(['Identifier'],[IntToStr(LIndex)]);

                if LWQConstraintsObject.FWQConMinMaxChannel.FInitalised then
                  LDataSet.SetParams(['ChannelNumber'],[IntToStr(LWQConstraintsObject.FWQConMinMaxChannel.FData)]);
                if LWQConstraintsObject.FWQConTarget.FInitalised then
                  LDataSet.SetParams(['WQTarget'],[FloatToStr(LWQConstraintsObject.FWQConTarget.FData)]);
                if LWQConstraintsObject.FNoOfRefChannelsBlending.FInitalised then
                  LDataSet.SetParams(['BlendingRefChannelCount'],[IntToStr(LWQConstraintsObject.FNoOfRefChannelsBlending.FData)]);
                if LWQConstraintsObject.FReservoirRef.FInitalised then
                  LDataSet.SetParams(['ReservoirRef'],[IntToStr(LWQConstraintsObject.FReservoirRef.FData)]);
                if LWQConstraintsObject.FWQConType.FInitalised then
                  LDataSet.SetParams(['WQConType'],[IntToStr(LWQConstraintsObject.FWQConType.FData)]);

                LDataSet.SetParams(['ReferenceChannels'],[LWQConstraintsObject.FReferenceChannel.CommaText]);
                LDataSet.SetParams(['ReferenceChannelFactors'],[LWQConstraintsObject.FRefChannelFactor.CommaText]);
                if LWQConstraintsObject.FSlopeLimit.FInitalised then
                  LDataSet.SetParams(['SlopeLimit'],[IntToStr(LWQConstraintsObject.FSlopeLimit.FData)]);

                LDataSet.SetParams(['EstimatedRelease'],[LWQConstraintsObject.FEstimatedRelease.CommaText]);
                LDataSet.SetParams(['Concentration'],[LWQConstraintsObject.FConcentration.CommaText]);
                LDataSet.ExecSQL;
              end;
            end;
          end;
        end;
      end;

      //line13 onwards++++++++++++++++++++++++++++
      for LCount := 0 to LRunParameters.FF01ExtraLines.Count - 1 do
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteUnknownDataSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
        LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
        LDataSet.SetParams(['LineNumber'], [IntToStr(11+LCount)]);
        LDataSet.SetParams(['LineSection'], [IntToStr(0)]);
        LDataSet.SetParams(['LineData'], [LRunParameters.FF01ExtraLines[LCount]]);

        LDataSet.ExecSQL;
      end;
      LDataSet.DataSet.Close;

      Result := InsertFileName(TFileNameObject(AFileName));
      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TFile01DatabaseAgent.strWriteEnded');
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    finally
      LDataSet.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile01DatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TFile01DatabaseAgent.ClearModelDataInDatabase';
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

    LTableNames := 'RunTitle,AnlySequences,DaysPerMonth,MaxYield,MonthNames,RunParameters,TargetPower,TargetYield';
    if(FAppModules.Model.ModelName = CPlanning) then
      LTableNames := LTableNames + ',DecisionDate,DemandCentre,DiscountRate,InterBasinSupport,AnlyManual,MinMaxBoundChannel,MinMaxWQConstrain';
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

procedure TFile01DatabaseAgent.UpdateWithChangeLists(ARunParameters : TRunParametersObject);
const OPNAME = 'TFile01DatabaseAgent.UpdateWithChangeLists';
var
  LRunConfigurationData: IRunConfigurationData;
  LIndex: integer;
begin
  try
    if Assigned(ARunParameters) then
    begin
      if(FAppModules.Model <> nil) and (FAppModules.Model.ModelData <> nil) then
      begin
        LRunConfigurationData := (FAppModules.Model.ModelData as IYieldModelData).RunConfigurationData;
        //TargetYield
        ARunParameters.FNoLoadCases.FData := LRunConfigurationData.NrOfActiveLoadCases;
        for LIndex := 1 to LRunConfigurationData.NrOfActiveLoadCases do
        begin
          if (LRunConfigurationData.TargetYieldByIndex[LIndex] = NullFloat) then
            ARunParameters.FTargetYield[LIndex].FData := 0.0
          else
            ARunParameters.FTargetYield[LIndex].FData := LRunConfigurationData.TargetYieldByIndex[LIndex];
          //MaxYield
          if (LRunConfigurationData.MaximumYieldByIndex[LIndex] = NullFloat) then
            ARunParameters.FMaxYield[LIndex].FData := 0.0
          else
            ARunParameters.FMaxYield[LIndex].FData := LRunConfigurationData.MaximumYieldByIndex[LIndex];
          //TargetPower
          if (LRunConfigurationData.TargetPowerByIndex[LIndex] = NullFloat) then
            ARunParameters.FTargetPower[LIndex].FData := 0.0
          else
            ARunParameters.FTargetPower[LIndex].FData := LRunConfigurationData.TargetPowerByIndex[LIndex];
        end;

        //LimitOption
        if ARunParameters.FLimitOption.FInitalised then
        begin
          if LRunConfigurationData.LimitOption then
            ARunParameters.FLimitOption.FData := 1
          else
            ARunParameters.FLimitOption.FData := 0;
        end;

        //Debug Level
        if ARunParameters.FDebugLevel.FInitalised then
          ARunParameters.FDebugLevel.FData := LRunConfigurationData.DebugLevel;

        //Summary Level
        if ARunParameters.FSummaryLevel.FInitalised then
          ARunParameters.FSummaryLevel.FData := LRunConfigurationData.OutputSummaryLevel;

        //Summary Output
        if ARunParameters.FSummaryOutput.FInitalised then
        begin
          if LRunConfigurationData.CreateDataFile then
            ARunParameters.FSummaryOutput.FData := 1
          else
            ARunParameters.FSummaryOutput.FData := 0;
        end;

        //Store Yield
        if ARunParameters.FStoreYield.FInitalised then
        begin
          if LRunConfigurationData.CreateYieldFile then
            ARunParameters.FStoreYield.FData := 1
          else
            ARunParameters.FStoreYield.FData := 0;
        end;

        //PlotOption
        if ARunParameters.FPlotOption.FInitalised then
        begin
          if LRunConfigurationData.CreatePlotFile then
            ARunParameters.FPlotOption.FData := 'Y'
          else
            ARunParameters.FPlotOption.FData := 'N';
        end;

        //RandomNumber
        if ARunParameters.FRandomNumber.FInitalised then
          ARunParameters.FRandomNumber.FData := LRunConfigurationData.GeneratedFlowFlag;

        //Multiple Period Lengths
        if ARunParameters.FMultiplePeriods.FInitalised then
        begin
          if LRunConfigurationData.MultiplePeriodLengths then
            ARunParameters.FMultiplePeriods.FData := 1
          else
            ARunParameters.FMultiplePeriods.FData := 0;
        end;

        //Calculate historic Firm Yield
        if ARunParameters.FCalcHistYield.FInitalised then
          ARunParameters.FCalcHistYield.FData := LRunConfigurationData.CalculateHistoricFirmYield;

        //Reduce Sequences
        if ARunParameters.FReduceSequence.FInitalised then
        begin
          if LRunConfigurationData.ReduceSequences then
            ARunParameters.FReduceSequence.FData := 1
          else
            ARunParameters.FReduceSequence.FData := 0;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
