//
//  UNIT      : Contains field update SQL.
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/01/23
//  COPYRIGHT : Copyright © 2003 DWAF
//
unit UFieldUpdateSQL_YieldModel_Configuration;

interface

type TFieldUpdateSQLStepItemAddFunction = procedure (
  AStepNo: integer; AFieldName, ATableName, AFieldInTable, AUpdateSQL, AGetValueSQL: string) of object;

procedure LoadFieldPropertyUpdateSQLSteps(AAdd: TFieldUpdateSQLStepItemAddFunction);

implementation

uses
  UFieldUpdateSQLCommonClauses;

const
  CAnlySequences     = ' FROM AnlySequences A   ' + CWhereScenario;
  CDaysPerMonth      = ' FROM DaysPerMonth A    ' + CWhereScenario;
  CMaxYield          = ' FROM MaxYield A        ' + CWhereScenario;
  CMonthNames        = ' FROM MonthNames A      ' + CWhereScenario;
  CRunParameters     = ' FROM RunParameters A   ' + CWhereScenario;
  CDecisionDates     = ' FROM DecisionDate A    ' + CWhereScenario;
  CRunTitle          = ' FROM RunTitle A        ' + CWhereScenario;
  CTargetPower       = ' FROM TargetPower A     ' + CWhereScenario;
  CTargetYield       = ' FROM TargetYield A     ' + CWhereScenario;
  CChannelComments   = ' FROM ChannelComments A ' + CWhereScenario;
  CConfiguratioFiles = ' FROM WRYMDat A '         + CWhereScenario;
  CFileNames         = ' FROM FileNames A       ' + CWhereScenarioIdentifier +
                     ' AND (A.FileGroup    = :AFileGroup   )     ' ;

procedure LoadFieldPropertyUpdateSQLSteps(AAdd: TFieldUpdateSQLStepItemAddFunction);
const OPNAME = 'LoadFieldPropertyUpdateSQLSteps';
begin

  //
  // AnlySequences.
  //
  AAdd(0,'Seq','AnlySequences','FieldNameIdentifier,1=Seq%d',
    ' SELECT *' + CAnlySequences, '');
  //
  // DaysPerMonth.
  //
  AAdd(0,'Days','DaysPerMonth','FieldNameIdentifier,1=Days%d',
    ' SELECT *' +  CDaysPerMonth, '');

  //
  // MaxYield
  //
  AAdd(0,'MYield','MaxYield','FieldNameIdentifier,1=MYield%d',
    ' SELECT *' +  CMaxYield, '');

  //
  // MonthNames.
  //
  AAdd(0,'Month','MonthNames','FieldNameIdentifier,1=Month%d',
    ' SELECT *' + CMonthNames, '');

  //
  // RunParameters.
  //
  AAdd(0,'NumPeriods',              'RunParameters','NumPeriods',              'SELECT *               ' + CRunParameters, '');
  AAdd(0,'StartYearG',              'RunParameters','StartYearG',              'SELECT *               ' + CRunParameters, '');
  AAdd(0,'StartYearO',              'RunParameters','StartYearO',              'SELECT *               ' + CRunParameters, '');
  AAdd(0,'DebugInit',               'RunParameters','DebugInit',               'SELECT *                ' + CRunParameters, '');
  AAdd(0,'DebugFinal',              'RunParameters','DebugFinal',              'SELECT *               ' + CRunParameters, '');
  AAdd(0,'DebugLevel',              'RunParameters','DebugLevel',              'SELECT *               ' + CRunParameters, '');
  AAdd(0,'SummaryLevel',            'RunParameters','SummaryLevel',            'SELECT *             ' + CRunParameters, '');
  AAdd(0,'SummaryOut',              'RunParameters','SummaryOut',              'SELECT *               ' + CRunParameters, '');
  AAdd(0,'StoreYield',              'RunParameters','StoreYield',              'SELECT *               ' + CRunParameters, '');
  AAdd(0,'RandomOpt',               'RunParameters','RandomOpt',               'SELECT *                ' + CRunParameters, '');
  AAdd(0,'PlotOpt',                 'RunParameters','PlotOpt',                 'SELECT *                  ' + CRunParameters, '');
  AAdd(0,'LimitOpt',                'RunParameters','LimitOpt',                'SELECT *                 ' + CRunParameters, '');
  AAdd(0,'MultPeriodOpt',           'RunParameters','MultPeriodOpt',           'SELECT *            ' + CRunParameters, '');
  AAdd(0,'CalcHistoryOpt',          'RunParameters','CalcHistoryOpt',          'SELECT *           ' + CRunParameters, '');
  AAdd(0,'TargetRecurrenceInterval','RunParameters','TargetRecurrenceInterval','SELECT * '         + CRunParameters, '');
  AAdd(0,'ReduceSeqOpt',            'RunParameters','ReduceSeqOpt',            'SELECT *             ' + CRunParameters, '');
  AAdd(0,'YearsCount',              'RunParameters','YearsCount',              'SELECT *               ' + CRunParameters, '');
  AAdd(0,'HydroSeqCount',           'RunParameters','HydroSeqCount',           'SELECT *            ' + CRunParameters, '');
  AAdd(0,'LoadCasesCount',          'RunParameters','LoadCasesCount',          'SELECT *           ' + CRunParameters, '');
  AAdd(0,'StartMonthNo',            'RunParameters','StartMonthNo',            'SELECT *             ' + CRunParameters, '');
  AAdd(0,'RunType',                 'RunParameters','RunType',                 'SELECT *                  ' + CRunParameters, '');
  AAdd(0,'StartType',               'RunParameters','StartType',               'SELECT *                ' + CRunParameters, '');
  AAdd(0,'ParamFile',               'RunParameters','ParamFile',               'SELECT *                ' + CRunParameters, '');

  //RunParameters Planning
  AAdd(0,'DetailedOption',          'RunParameters','DetailedOption',          'SELECT *           ' + CRunParameters, '');
  AAdd(0,'SupplyOption',            'RunParameters','SupplyOption',            'SELECT *             ' + CRunParameters, '');
  AAdd(0,'AnnualSummary',           'RunParameters','AnnualSummary',           'SELECT *            ' + CRunParameters, '');
  AAdd(0,'EconomicOption',          'RunParameters','EconomicOption',          'SELECT *           ' + CRunParameters, '');
  AAdd(0,'PlanningSummary',         'RunParameters','PlanningSummary',         'SELECT *          ' + CRunParameters, '');
  AAdd(0,'InputSummary',            'RunParameters','InputSummary',            'SELECT *             ' + CRunParameters, '');
  AAdd(0,'WaterQualityOption',      'RunParameters','WaterQualityOption',      'SELECT *       ' + CRunParameters, '');
  AAdd(0,'PeriodsPerYear',          'RunParameters','PeriodsPerYear',          'SELECT *           ' + CRunParameters, '');
  AAdd(0,'CalendarStartMonth',      'RunParameters','CalendarStartMonth',      'SELECT *       ' + CRunParameters, '');
  AAdd(0,'ShortTermPlanningOption', 'RunParameters','ShortTermPlanningOption', 'SELECT *  ' + CRunParameters, '');
  AAdd(0,'HydroPowerOption',        'RunParameters','HydroPowerOption',        'SELECT *         ' + CRunParameters, '');
  AAdd(0,'AllocationControlOption', 'RunParameters','AllocationControlOption', 'SELECT *  ' + CRunParameters, '');
  AAdd(0,'NrOfDecisionDates',       'DecisionDate' ,'NrOfDecisionDates',       'SELECT *        ' + CDecisionDates, '');

  AAdd(0,'DecisionMonth','DecisionDate','FieldNameIdentifier,1=DecisionMonth%2.2d',
    ' SELECT *' + CDecisionDates, '');

 AAdd(0,'DecisionType','DecisionDate','FieldNameIdentifier,1=DecisionType%2.2d',
    ' SELECT *' + CDecisionDates, '');

 AAdd(0,'HydroPowerIndicator','DecisionDate','FieldNameIdentifier,1=HydroPowerIndicator%2.2d',
    ' SELECT *' +  CDecisionDates, '');

  //
  // RunTitle
  //
  AAdd(0,'Title1','RunTitle','Title1',  ' SELECT *   ' + CRunTitle, '');
  AAdd(0,'Title2','RunTitle','Title2',  ' SELECT *   ' + CRunTitle, '');
  AAdd(0,'Title3','RunTitle','Title3',  ' SELECT *   ' + CRunTitle, '');

  //
  // TargetPower
  //
  AAdd(0,'TPower','TargetPower','FieldNameIdentifier,1=TPower%d',
    ' SELECT *' +  CTargetPower, '');
  //
  // TargetYield
  //
  AAdd(0,'TYield','TargetYield','FieldNameIdentifier,1=TYield%d',
    ' SELECT *' + CTargetYield, '');
  //
  // ChannelComments
  //
  AAdd(0,'Comment01','ChannelComments','Comment01',  ' SELECT *   ' + CChannelComments, '');
  AAdd(0,'Comment02','ChannelComments','Comment02',  ' SELECT *   ' + CChannelComments, '');
  AAdd(0,'Comment03','ChannelComments','Comment03',  ' SELECT *   ' + CChannelComments, '');
  AAdd(0,'Comment04','ChannelComments','Comment04',  ' SELECT *   ' + CChannelComments, '');
  AAdd(0,'Comment05','ChannelComments','Comment05',  ' SELECT *   ' + CChannelComments, '');
  AAdd(0,'Comment06','ChannelComments','Comment06',  ' SELECT *   ' + CChannelComments, '');
  AAdd(0,'Comment07','ChannelComments','Comment07',  ' SELECT *   ' + CChannelComments, '');
  AAdd(0,'Comment08','ChannelComments','Comment08',  ' SELECT *   ' + CChannelComments, '');
  AAdd(0,'Comment09','ChannelComments','Comment09',  ' SELECT *   ' + CChannelComments, '');
  AAdd(0,'Comment10','ChannelComments','Comment10',  ' SELECT *   ' + CChannelComments, '');
  AAdd(0,'Comment11','ChannelComments','Comment11',  ' SELECT *   ' + CChannelComments, '');
  AAdd(0,'Comment12','ChannelComments','Comment12',  ' SELECT *   ' + CChannelComments, '');
  AAdd(0,'Comment13','ChannelComments','Comment13',  ' SELECT *   ' + CChannelComments, '');
  AAdd(0,'Comment22','ChannelComments','Comment22',  ' SELECT *   ' + CChannelComments, '');
  //
  // FileNames
  //
  AAdd(0,'DirectoryFileName'   ,'FileNames' ,'FileName'            , ' SELECT * '            + CFileNames, '');
  AAdd(1,'ParamFile'           ,'FileNames' ,'FileName'            , ' SELECT * '            + CFileNames, '');
  AAdd(1,'InputPath'           ,'WRYMDat'   ,'InputPath'           , ' SELECT * '           + CConfiguratioFiles, '');
  AAdd(1,'OutputPath'          ,'WRYMDat'   ,'OutputPath'          , ' SELECT * '          + CConfiguratioFiles, '');
  AAdd(1,'HydrologyPath'       ,'WRYMDat'   ,'HydrologyPath'       , ' SELECT * '       + CConfiguratioFiles, '');
  AAdd(1,'SpecifiedDemandPath' ,'WRYMDat'   ,'SpecifiedDemandPath' , ' SELECT * ' + CConfiguratioFiles, '');
end;

end.

