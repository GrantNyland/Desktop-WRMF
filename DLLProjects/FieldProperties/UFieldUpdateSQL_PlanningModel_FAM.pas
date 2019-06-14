{******************************************************************************}
{*  UNIT      : Contains Contains field update SQL for Planning model.
{*  AUTHOR    : Riana Steyn
{*  DATE      : 10/01/2006
{*  COPYRIGHT : Copyright © 2006 DWAF
{******************************************************************************}

unit UFieldUpdateSQL_PlanningModel_FAM;

interface

type TFieldUpdateSQLStepItemAddFunction = procedure (AStepNo        : integer;
                                                     AFieldPropName : string;
                                                     ATableName     : string;
                                                     AFieldInTable  : string;
                                                     AUpdateSQL     : string;
                                                     AGetValueSQL   : string) of object;

procedure LoadFieldPropertyUpdateSQLSteps (AAdd : TFieldUpdateSQLStepItemAddFunction);
procedure LoadFieldPropertyUpdateSQLSteps1 (AAdd : TFieldUpdateSQLStepItemAddFunction);

implementation

uses
  UFieldUpdateSQLCommonClauses;

const

  CWhereScenarioAllocDef        = CWhereScenario +
                                  ' AND (A.AllocDefID = :AAllocDefID ) ' ;
  CFMAllocationDefinition       = ' FROM FMAllocationDefinition A ' +
                                  CWhereScenarioAllocDef;
  CFMUserCategory               = ' FROM FMUserCategory A ' +
                                  CWhereScenarioAllocDef +
                                  ' AND (A.UserCategoryID = :AUserCategoryID ) ' ;
  CFMAllocationLevel            = ' FROM FMAllocationLevel A ' +
                                  CWhereScenarioAllocDef +
                                  ' AND (A.AllocLevelID = :AAllocLevelID ) ' ;
  CFMSubSystem                  = ' FROM FMSubSystem A ' +
                                  CWhereScenarioAllocDef +
                                  ' AND (A.SubSystemID = :ASubSystemID)';
  CFMCoefficients               = ' FROM FMCoefficients A ' +
                                  CWhereScenarioAllocDef +
                                  ' AND (A.SubSystemID = :ASubSystemID)' +
                                  ' AND (A.StartStorageNr = :AStartStorageNr)' +
                                  ' AND (A.CurveSetNr = :ACurveSetNr)' +
                                  ' AND (A.LoadCaseNr = :ALoadCaseNr)';
  CFMDemandDefinition           = ' FROM FMDemandDefinition A ' +
                                  CWhereScenarioAllocDef +
                                  ' AND (A.DemandDefID = :ADemandDefID)';
  CFMSupportSubSystem           = ' FROM FMSupportSubSystem A ' +
                                  CWhereScenarioAllocDef +
                                  ' AND (A.DemandDefID = :ADemandDefID)' +
                                  ' AND (A.SupportSubSystemID = :ASupportSubSystemID)';
  CFMSupportChannel             = ' FROM FMSupportChannel A ' +
                                  CWhereScenarioAllocDef +
                                  ' AND (A.SupportChannelID = :ASupportChannelID)';
  CFMSolveFixedPosition         = ' FROM FMSolveFixedPosition A ' +
                                  CWhereScenarioAllocDef +
                                  ' AND (A.FixedPositionID = :AFixedPositionID)';
  CFMSolveSpecificOrder         = ' FROM FMSolveSpecificOrder A ' +
                                  CWhereScenarioAllocDef +
                                  ' AND (A.SpecificOrderID = :ASpecificOrderID)';
  CReservoirTimeControl         = ' FROM ReservoirTimeControl A ' +
                                  CWhereScenario +
                                  ' AND (A.ReservoirNumber = :AReservoirNumber)';
  CChannelTimeControl           = ' FROM ChannelTimeControl A ' +
                                  CWhereScenario +
                                  ' AND (A.Identifier = :AIdentifier)'+
                                  ' AND (A.ChannelNumber = :AChannelNumber)';
  CChannelSwitchDefinition      = ' FROM ChannelSwitchDefinition A ' +
                                  CWhereScenario +
                                  ' AND (A.SwitchDefID = :ASwitchDefID ) ' ;
  CSwitchDefinition             = ' FROM SwitchDefinition A ' +
                                  CWhereScenario +
                                  ' AND (A.SwitchDefID = :ASwitchDefID ) ' ;
  CChannelSwitchControl         = ' FROM ChannelSwitchControl A ' +
                                  CWhereScenario +
                                  ' AND (A.ChannelSwitchID = :AChannelSwitchID)';
  CGrowthFactorConfig           = ' FROM GrowthFactorConfig A ' +
                                  CWhereScenario;
  CGrowthFactorDemand           = ' FROM GrowthFactorDemand A ' +
                                  CWhereScenarioIdentifier;
  CGrowthFactorHydrology        = ' FROM GrowthFactorHydrology A ' +
                                  CWhereScenarioIdentifier;
  CGrowthFactorMinMax           = ' FROM GrowthFactorMinMax A ' +
                                  CWhereScenarioIdentifier;
  CExcelDemandGrowthFactorConfig   = ' FROM GrowthFactorExcelConfig A ' +
                                  CWhereScenario;
  CExcelMinMaxGrowthFactors    = ' FROM GrowthFactorExcelMinMax A ' +
                                  CWhereScenarioIdentifier;
  CExcelDemandGrowthFactors    = ' FROM GrowthFactorExcelDemand A ' +
                                  CWhereScenarioIdentifier;
  CExcelHydrologyGrowthFactors = ' FROM GrowthFactorExcelHydrology A ' +
                                  CWhereScenarioIdentifier;
  CDisbenefitFunction          = ' FROM DisbenefitFunction A ' +
                                    CWhereScenario +
                                  ' AND (A.ChannelNumber = :AChannelNumber)';
  CReturnFlowChannelDetail     = ' FROM ReturnFlowChannelDetail A ' +
                                  CWhereScenarioIdentifier;
  CTariffCalculationConfig     = ' FROM TariffCalculationConfig A ' +
                                  CWhereScenario;
  CTariffCalculation           = ' FROM TariffCalculation A ' +
                                  CWhereScenarioIdentifier;
  CReturnFlowMonthlyEvaporation  = ' FROM ReturnFlowMonthlyEvaporation A ' +
                                  CWhereScenarioIdentifier +
                                  ' AND (A.DemandChannel = :ADemandChannel)';
  CReturnFlowCorrespondingChannel = ' FROM ReturnFlowCorrespondingChannel A ' +
                                  CWhereScenarioIdentifier +
                                  ' AND (A.DemandChannel = :ADemandChannel) '+
                                  ' AND (A.ChannelNumber = :AChannelNumber) ';
 CChannelConfig   = ' FROM ChannelConfig A ' + CWhereScenario;

 CReservoirSwitchDefinition = ' FROM ReservoirSwitchDefinition A '+
                               CWhereScenario +
                              ' AND (A.SwitchDefID = :ASwitchDefID) '+
                              ' AND (A.FileGroupID = :AFileGroupID) ';
 {
 CNoOfRefChannels = ' FROM MinMaxBlockTypeConfig A '+
                               CWhereScenario +
                              ' AND (A.BlockType = :ABlockType) ';  }

 CMinMaxWQConstrain  = ' FROM MinMaxWQConstrain A ' +
                                  CWhereScenarioIdentifier;

 CMinMaxBoundChannel  = ' FROM MinMaxBoundChannel A ' +
                                  CWhereScenarioIdentifier;

 CMultiResChannelCurtail  = ' FROM MultiResChannelCurtail A ' +
                            CWhereScenarioIdentifier;

 CMultiResChannelElevation  = ' FROM MultiResChannelElevation A ' +
                            CWhereScenarioIdentifier;

 CMultiResChannelFactor  = ' FROM MultiResChannelFactor A ' +
                            CWhereScenarioIdentifier;

 // MIMM
 CMineGrowthFactors    =   ' FROM MineGrowthFactors A ' +
                            CWhereScenarioIdentifier +
                           ' AND (A.MineIdentifier = :AMineIdentifier) '+
                           ' AND (A.OCIdentifier   = :AOCIdentifier) '+
                           ' AND (A.SlurryIdentifier = :ASlurryIdentifier) '+
                           ' AND (A.UDGIdentifier = :AUDGIdentifier) ';
 CMineLoadGenerationFlow = ' FROM MineLoadGenerationFlow A ' +
                            CWhereScenarioIdentifier +
                           ' AND (A.MineIdentifier = :AMineIdentifier) '+
                           ' AND (A.OpenCastIdentifier   = :AOCIdentifier) '+
                           ' AND (A.SDIdentifier = :ASDIdentifier) '+
                           ' AND (A.UDGIdentifier = :AUDGIdentifier) ';
 CMineLoadGenerationMeanOfSalt = ' FROM MineLoadGenerationMeanOfSalt A ' +
                            CWhereScenarioIdentifier +
                           ' AND (A.MineIdentifier = :AMineIdentifier) '+
                           ' AND (A.OpenCastIdentifier   = :AOCIdentifier) '+
                           ' AND (A.SDIdentifier = :ASDIdentifier) '+
                           ' AND (A.UDGIdentifier = :AUDGIdentifier) ';

 CMineOpenCast =
     ' FROM MineOpenCast A                ' +
       CWhereScenarioIdentifier +
     ' AND   (A.MineIdentifier = :AMineIdentifier     )   ' ;

 CMineSlurryDump =
     ' FROM MineSlurryDump A                ' +
       CWhereScenarioIdentifier               +
     ' AND (A.MineIdentifier = :AMineIdentifier     )   ';

 CMineDetails =
     ' FROM Mine A                  ' +
       CWhereScenario +
     ' AND (A.Identifier    = :AIdentifier)     ' ;


procedure LoadFieldPropertyUpdateSQLSteps(AAdd: TFieldUpdateSQLStepItemAddFunction);
const OPNAME = 'LoadFieldPropertyUpdateSQLSteps';
begin

  AAdd(0,'InflowPenaltyNo','ChannelConfig','InflowPenaltyNo',  ' SELECT *   ' + CChannelConfig, '');
  AAdd(0,'STYieldStartYear','ReservoirSwitchDefinition','StartYear',  ' SELECT *   ' + CReservoirSwitchDefinition, '');
  AAdd(0,'STYieldMonth','ReservoirSwitchDefinition','StartMonth',  ' SELECT *   ' + CReservoirSwitchDefinition, '');

{* FMAllocationDefinition *****************************************************}
  AAdd(0,'AllocDefName',       'FMAllocationDefinition', 'AllocDefName',
    'SELECT * ' +
    CFMAllocationDefinition, '');
  AAdd(0,'AllocDefFileName',   'FMAllocationDefinition', 'FamilyFile',
    'SELECT * ' +
    CFMAllocationDefinition, '');
  AAdd(0,'AllocDefStartYear',  'FMAllocationDefinition', 'AllocDefStartYear',
    'SELECT * ' +
    CFMAllocationDefinition, '');
  AAdd(0,'AllocDefStartMonth', 'FMAllocationDefinition', 'AllocDefStartMonth',
    'SELECT * ' +
    CFMAllocationDefinition, '');
  AAdd(0,'AllocDefEndYear',    'FMAllocationDefinition', 'AllocDefEndYear',
    'SELECT * ' +
    CFMAllocationDefinition, '');
  AAdd(0,'AllocDefEndMonth',   'FMAllocationDefinition', 'AllocDefEndMonth',
    'SELECT * ' +
    CFMAllocationDefinition, '');
  AAdd(0,'NrOfReliabilityClasses',  'FMAllocationDefinition', 'NrOfReliabilityClasses',
    'SELECT * ' +
    CFMAllocationDefinition, '');
  AAdd(0,'NrOfLoadCases',           'FMAllocationDefinition', 'NrOfLoadCases',
    'SELECT * ' +
    CFMAllocationDefinition, '');
  AAdd(0,'NrOfStartStoragePercs',   'FMAllocationDefinition', 'NrOfStartStoragePercs',
    'SELECT * ' +
    CFMAllocationDefinition, '');
  AAdd(0,'NrOfCurveSets',           'FMAllocationDefinition', 'NrOfCurveSets',
    'SELECT * ' +
    CFMAllocationDefinition, '');
  AAdd(0,'PeriodLength',            'FMAllocationDefinition', 'PeriodLength',
    'SELECT * ' +
    CFMAllocationDefinition, '');
  AAdd(0,'SupportStrategy',         'FMAllocationDefinition', 'SupportStrategy',
    'SELECT * ' +
    CFMAllocationDefinition, '');
  AAdd(0,'BalancingOption',         'FMAllocationDefinition', 'BalancingOption',
    'SELECT * ' +
    CFMAllocationDefinition, '');
  AAdd(0,'RIValue',                 'FMAllocationDefinition', 'FieldNameIdentifier,1=RIValue%2.2d',
    'SELECT *' +
    CFMAllocationDefinition, '');
  AAdd(0,'RILabel',                 'FMAllocationDefinition', 'FieldNameIdentifier,1=RILabel%2.2d',
    'SELECT * ' +
    CFMAllocationDefinition, '');
  AAdd(0,'MonthCurveSet',           'FMAllocationDefinition', 'FieldNameIdentifier,1=MonthCurveSet%2.2d',
    'SELECT * ' +
    CFMAllocationDefinition, '');
  AAdd(0,'StartStoragePerc',        'FMAllocationDefinition', 'FieldNameIdentifier,1=StartStoragePerc%2.2d',
    'SELECT * ' +
    CFMAllocationDefinition, '');

{* FMUserCategory *************************************************************}
  AAdd(0,'UserCategoryName',   'FMUserCategory', 'UserCategoryName',
    'SELECT * ' +
    CFMUserCategory, '');
  AAdd(0,'Distribution', 'FMUserCategory', 'FieldNameIdentifier,1=Distribution%2.2d',
    'SELECT * ' +
    CFMUserCategory, '');

{* FMAllocationLevel **********************************************************}
  AAdd(0,'AllocLevelName',     'FMAllocationLevel', 'AllocLevelName',
    'SELECT * ' +
    CFMAllocationLevel, '');
  AAdd(0,'Curtailment', 'FMAllocationLevel', 'FieldNameIdentifier,1=Curtailment%2.2d',
    'SELECT * ' +
    CFMAllocationLevel, '');

{* FMSubSystem ****************************************************************}
  AAdd(0,'SubSystemName',          'FMSubSystem', 'SubSystemName',
    'SELECT * ' +
    CFMSubSystem, '');
  AAdd(0,'SubSystemOrder',         'FMSubSystem', 'SubSystemOrder',
    'SELECT * ' +
    CFMSubSystem, '');
  AAdd(0,'SubSystemStartYear',     'FMSubSystem', 'SubSystemStartYear',
    'SELECT * ' +
    CFMSubSystem, '');
  AAdd(0,'SubSystemStartMonth',    'FMSubSystem', 'SubSystemStartMonth',
    'SELECT * ' +
    CFMSubSystem, '');
  AAdd(0,'SubSystemEndYear',       'FMSubSystem', 'SubSystemEndYear',
    'SELECT * ' +
    CFMSubSystem, '');
  AAdd(0,'SubSystemEndMonth',      'FMSubSystem', 'SubSystemEndMonth',
    'SELECT * ' +
    CFMSubSystem, '');
  AAdd(0,'SubtractedSubSystemID',  'FMSubSystem', 'SubtractedSubSystemID',
    'SELECT * ' +
    CFMSubSystem, '');
  AAdd(0,'SupportingSubSystemID',   'FMSubSystem', 'SupportingSubSystemID',
    'SELECT * ' +
    CFMSubSystem, '');
  AAdd(0,'SupportingChannelNr',     'FMSubSystem', 'SupportingChannelNr',
    'SELECT * ' +
    CFMSubSystem, '');
  AAdd(0,'ShortTermYield',         'FMSubSystem', 'ShortTermYield',
    'SELECT * ' +
    CFMSubSystem, '');
  AAdd(0,'LongTermYield',          'FMSubSystem', 'LongTermYield',
    'SELECT * ' +
    CFMSubSystem, '');
  AAdd(0,'LowestStreamFlow',       'FMSubSystem', 'LowestStreamFlow',
    'SELECT * ' +
    CFMSubSystem, '');
  AAdd(0,'FirmYield',              'FMSubSystem', 'FirmYield',
    'SELECT * ' +
    CFMSubSystem, '');
  AAdd(0,'SupportCalcType',        'FMSubSystem', 'SupportCalcType',
    'SELECT * ' +
    CFMSubSystem, '');
  AAdd(0,'SubSystemReservoirNrs',  'FMSubSystem', 'SubSystemReservoirNrs',
    'SELECT * ' +
    CFMSubSystem, '');
  AAdd(0,'RoutingChannelNr',       'FMSubSystem', 'FieldNameIdentifier,1=RoutingChannelNr%2.2d',
    'SELECT *, RoutingChannelNr02, RoutingChannelNr03, RoutingChannelNr04, RoutingChannelNr05 ' +
    CFMSubSystem, '');

{* FMCoefficients **************************************************************}
  AAdd(0,'LoadCase',          'FMCoefficients', 'LoadCase',
    'SELECT * ' +
    CFMCoefficients, '');
  AAdd(0,'CoefficientA',      'FMCoefficients', 'CoefficientA',
    'SELECT * ' +
    CFMCoefficients, '');
  AAdd(0,'CoefficientB',      'FMCoefficients', 'CoefficientB',
    'SELECT * ' +
    CFMCoefficients, '');
  AAdd(0,'CoefficientC',      'FMCoefficients', 'CoefficientC',
    'SELECT * ' +
    CFMCoefficients, '');
  AAdd(0,'CoefficientD',      'FMCoefficients', 'CoefficientD',
    'SELECT * ' +
    CFMCoefficients, '');
  AAdd(0,'RiskProportion',    'FMCoefficients', 'RiskProportion',
    'SELECT * ' +
    CFMCoefficients, '');

{* FMDemandDefinition *************************************************************}
  AAdd(0,'DemandDefName',   'FMDemandDefinition', 'DemandDefName',
    'SELECT * ' +
    CFMDemandDefinition, '');
  AAdd(0,'DemandDefOrder',  'FMDemandDefinition', 'DemandDefOrder',
    'SELECT * ' +
    CFMDemandDefinition, '');
  AAdd(0,'ParentSubSystemID',  'FMDemandDefinition', 'ParentSubSystemID',
    'SELECT * ' +
    CFMDemandDefinition, '');
  AAdd(0,'GrowthType',         'FMDemandDefinition', 'GrowthType',
    'SELECT * ' +
    CFMDemandDefinition, '');
  AAdd(0,'TargetDemand',       'FMDemandDefinition', 'TargetDemand',
    'SELECT * ' +
    CFMDemandDefinition, '');
  AAdd(0,'DDDemandCentreID',   'FMDemandDefinition', 'DemandCentreID',
    'SELECT * ' +
    CFMDemandDefinition, '');
  AAdd(0,'DCUserCategoryID',   'FMDemandDefinition', 'DCUserCategoryID',
    'SELECT * ' +
    CFMDemandDefinition, '');
  AAdd(0,'SupportArc1',        'FMDemandDefinition', 'SupportArc1',
    'SELECT * ' +
    CFMDemandDefinition, '');
  AAdd(0,'SupportArc2',        'FMDemandDefinition', 'SupportArc2',
    'SELECT * ' +
    CFMDemandDefinition, '');

{* FMSupportSubSystem *********************************************************}
  AAdd(0,'SupSubSystemID',   'FMSupportSubSystem', 'SupSubSystemID',
    'SELECT * ' +
    CFMSupportSubSystem, '');
  AAdd(0,'SupSubSysChannelNr',     'FMSupportSubSystem', 'FieldNameIdentifier,1=SupSubSysChannelNr%2.2d',
    'SELECT * ' +
    CFMSupportSubSystem, '');

{Water Quality Channels****************************}
 {  AAdd(0,'WQConstriantsChannel',     'MinMaxBlockTypeConfig', 'BlockCount',
    'SELECT * ' +
    CNoOfRefChannels, '');

   AAdd(0,'MinMaxUpperBoundChannel',     'MinMaxBlockTypeConfig', 'BlockCount',
    'SELECT * ' +
    CNoOfRefChannels, '');
      }
   AAdd(0,'NoOfRefChannelsBlending',     'MinMaxWQConstrain', 'BlendingRefChannelCount',
    'SELECT * ' +
    CMinMaxWQConstrain, '');

   AAdd(0,'WQConstTarget',     'MinMaxWQConstrain', 'WQTarget',
    'SELECT * ' +
    CMinMaxWQConstrain, '');

   AAdd(0,'ReservoirRef',     'MinMaxWQConstrain', 'ReservoirRef',
    'SELECT * ' +
    CMinMaxWQConstrain, '');

    AAdd(0,'WQConType',     'MinMaxWQConstrain', 'WQConType',
    'SELECT * ' +
    CMinMaxWQConstrain, '');
    AAdd(0,'ReferenceChannel',     'MinMaxWQConstrain', 'ReferenceChannels',
    'SELECT * ' +
    CMinMaxWQConstrain, '');

    AAdd(0,'RefChannelFactor',     'MinMaxWQConstrain', 'ReferenceChannelFactors',
    'SELECT * ' +
    CMinMaxWQConstrain, '');

    AAdd(0,'SlopeLimit',     'MinMaxWQConstrain', 'SlopeLimit',
    'SELECT * ' +
    CMinMaxWQConstrain, '');

    AAdd(0,'EstimatedRelease',     'MinMaxWQConstrain', 'EstimatedRelease',
    'SELECT * ' +
    CMinMaxWQConstrain, '');

    AAdd(0,'Concentration',     'MinMaxWQConstrain', 'Concentration',
    'SELECT * ' +
    CMinMaxWQConstrain, '');

    AAdd(0,'BoundedChannels',     'MinMaxBoundChannel', 'ReferenceChannelCount',
    'SELECT * ' +
    CMinMaxBoundChannel, '');


    AAdd(0,'ReferenceChannels',     'MinMaxBoundChannel', 'ReferenceChannels',
    'SELECT * ' +
    CMinMaxBoundChannel, '');

    AAdd(0,'MultiCurChannel',     'MultiResChannelCurtail', 'ChannelNo',
    'SELECT * ' +
    CMultiResChannelCurtail, '');
    AAdd(1,'MultiCurChannel',     'MultiResChannelElevation', 'ChannelNo',
    'SELECT * ' +
    CMultiResChannelElevation, '');
    AAdd(2,'MultiCurChannel',     'MultiResChannelFactor', 'ChannelNo',
    'SELECT * ' +
    CMultiResChannelFactor, '');

   AAdd(0,'MultiCurReservoir',     'MultiResChannelCurtail', 'ReservoirNo',
    'SELECT * ' +
    CMultiResChannelCurtail, '');
     AAdd(1,'MultiCurReservoir',     'MultiResChannelElevation', 'ReservoirNo',
    'SELECT * ' +
    CMultiResChannelElevation, '');
     AAdd(2,'MultiCurReservoir',     'MultiResChannelFactor', 'ReservoirNo',
    'SELECT * ' +
    CMultiResChannelFactor, '');

    AAdd(0,'MultiCurDecisionMonth',     'MultiResChannelCurtail', 'DecisionMonth',
    'SELECT * ' +
    CMultiResChannelCurtail, '');

    AAdd(0,'MultiCurStartMonth',     'MultiResChannelCurtail', 'StartMonth',
    'SELECT * ' +
    CMultiResChannelCurtail, '');


    AAdd(0,'MultiCurElevation',     'MultiResChannelElevation', 'FieldNameIdentifier,1=Elevation%2.2d',
    'SELECT * ' +
    CMultiResChannelElevation, '');


    AAdd(0,'MultiCurFactor',     'MultiResChannelFactor', 'FieldNameIdentifier,1=Factor%2.2d',
    'SELECT * ' +
    CMultiResChannelFactor, '');




  LoadFieldPropertyUpdateSQLSteps1(AAdd);

end;

procedure LoadFieldPropertyUpdateSQLSteps1 (AAdd : TFieldUpdateSQLStepItemAddFunction);
begin

{* FMSupportChannel ***********************************************************}
  AAdd(0,'SupportChannelNr',     'FMSupportChannel', 'ChannelNumber',
    'SELECT * ' +
    CFMSupportChannel, '');
  AAdd(0,'NrOfCntrlSubSystems',  'FMSupportChannel', 'NrOfCntrlSubSystems',
    'SELECT * ' +
    CFMSupportChannel, '');
  AAdd(0,'CntrlSubSystemID',     'FMSupportChannel', 'FieldNameIdentifier,1=CntrlSubSystemID%2.2d',
    'SELECT * ' +
    CFMSupportChannel, '');
  AAdd(0,'CntrlFactor',          'FMSupportChannel', 'FieldNameIdentifier,1=CntrlFactor%2.2d',
    'SELECT * ' +
    CFMSupportChannel, '');

{* FMSolveFixedPosition *******************************************************}
  AAdd(0,'FixedPositionNr',     'FMSolveFixedPosition', 'FixedPositionNr',
    'SELECT * ' +
    CFMSolveFixedPosition, '');
  AAdd(0,'FixedPosSubSystemID', 'FMSolveFixedPosition', 'FixedPosSubSystemID',
    'SELECT * ' +
    CFMSolveFixedPosition, '');

{* FMSolveSpecificOrder *******************************************************}
  AAdd(0,'BeforeSubSystemID',     'FMSolveSpecificOrder', 'BeforeSubSystemID',
    'SELECT * ' +
    CFMSolveSpecificOrder, '');
  AAdd(0,'AfterSubSystemID',     'FMSolveSpecificOrder', 'AfterSubSystemID',
    'SELECT * ' +
    CFMSolveSpecificOrder, '');

{* ReservoirTimeControl***********************************************************}
  AAdd(0,'BaseNodeNumber',                'ReservoirTimeControl', 'BaseNodeNumber',
    'SELECT * '           +
    CReservoirTimeControl, '');
  AAdd(0,'ReservoirStartYear',            'ReservoirTimeControl', 'ReservoirStartYear',
    'SELECT * '       +
    CReservoirTimeControl, '');
  AAdd(0,'ReservoirStartMonth',           'ReservoirTimeControl', 'ReservoirStartMonth',
    'SELECT * '      +
    CReservoirTimeControl, '');
  AAdd(0,'ReservoirEndYear',              'ReservoirTimeControl', 'ReservoirEndYear',
    'SELECT * '         +
    CReservoirTimeControl, '');
  AAdd(0,'ReservoirEndMonth',             'ReservoirTimeControl', 'ReservoirEndMonth',
    'SELECT * '        +
    CReservoirTimeControl, '');
  AAdd(0,'ReservoirEconomicLife',         'ReservoirTimeControl', 'ReservoirEconomicLife',
    'SELECT *'     +
    CReservoirTimeControl, '');
  AAdd(0,'ReservoirCapitalCost',          'ReservoirTimeControl', 'ReservoirCapitalCost',
    'SELECT * '     +
    CReservoirTimeControl, '');
  AAdd(0,'ReservoirOMCost',               'ReservoirTimeControl', 'ReservoirOMCost',
    'SELECT *'           +
    CReservoirTimeControl, '');
  AAdd(0,'ReservoirCostSchedule',         'ReservoirTimeControl', 'ReservoirCostSchedule',
    'SELECT * '    +
    CReservoirTimeControl, '');

{* ChannelTimeControl******************************************************}
  AAdd(0,'ChannelStartYear',              'ChannelTimeControl', 'ChannelStartYear',
    'SELECT * '         +
    CChannelTimeControl, '');
  AAdd(0,'ChannelStartMonth',             'ChannelTimeControl', 'ChannelStartMonth',
    'SELECT * '        +
    CChannelTimeControl, '');
  AAdd(0,'ChannelEndYear',                'ChannelTimeControl', 'ChannelEndYear',
    'SELECT * '           +
    CChannelTimeControl, '');
  AAdd(0,'ChannelEndMonth',               'ChannelTimeControl', 'ChannelEndMonth',
    'SELECT * '          +
    CChannelTimeControl, '');
  AAdd(0,'ChannelEconomicLife',           'ChannelTimeControl', 'ChannelEconomicLife',
    'SELECT *'       +
    CChannelTimeControl, '');
  AAdd(0,'ChannelCapitalCost',            'ChannelTimeControl', 'ChannelCapitalCost',
    'SELECT * '       +
    CChannelTimeControl, '');
  AAdd(0,'ChannelFixedOMCost',            'ChannelTimeControl', 'ChannelFixedOMCost',
    'SELECT *'        +
    CChannelTimeControl, '');
  AAdd(0,'ChannelVariableOMCost',         'ChannelTimeControl', 'ChannelVariableOMCost',
    'SELECT *'     +
    CChannelTimeControl, '');
  AAdd(0,'ChannelCostSchedule',           'ChannelTimeControl', 'ChannelCostSchedule',
    'SELECT *'       +
    CChannelTimeControl, '');
  AAdd(0,'ChannelEscalationCost',        'ChannelTimeControl', 'ChannelEscalationCost',
    'SELECT *'     +
    CChannelTimeControl, '');

  {* ChannelSwitchDefinition *****************************************************}
  AAdd(0,'SwitchDefFileName',   'ChannelSwitchDefinition', 'SwitchDefFileName',
    'SELECT * ' +
    CChannelSwitchDefinition, '');
  AAdd(0,'SwitchDefStartYear',  'ChannelSwitchDefinition', 'SwitchDefStartYear',
    'SELECT * ' +
    CChannelSwitchDefinition, '');
  AAdd(0,'SwitchDefStartMonth', 'ChannelSwitchDefinition', 'SwitchDefStartMonth',
    'SELECT * ' +
    CChannelSwitchDefinition, '');

{* SwitchDefinition *****************************************************}
  AAdd(0,'SwitchDefFileName',   'SwitchDefinition', 'SwitchDefFileName',
    'SELECT * ' +
    CSwitchDefinition, '');
  AAdd(0,'SwitchDefStartYear',  'SwitchDefinition', 'SwitchDefStartYear',
    'SELECT * ' +
    CSwitchDefinition, '');
  AAdd(0,'SwitchDefStartMonth', 'SwitchDefinition', 'SwitchDefStartMonth',
    'SELECT * ' +
    CSwitchDefinition, '');

{* ChannelSwitchControl********************************************************}
  AAdd(0,'ChannelSwitchID',           'ChannelSwitchControl', 'ChannelSwitchID',
    'SELECT * '            +
    CChannelSwitchControl, '');
  AAdd(0,'SwitchDefinitionID',        'ChannelSwitchControl', 'SwitchDefinitionID',
    'SELECT * '            +
    CChannelSwitchControl, '');
  AAdd(0,'SwitchAssociatedNodeNr',    'ChannelSwitchControl', 'SwitchAssociatedNodeNr',
    'SELECT * ' +
    CChannelSwitchControl, '');
  AAdd(0,'SwitchWaterlevel',          'ChannelSwitchControl', 'SwitchWaterlevel',
    'SELECT * '       +
    CChannelSwitchControl, '');
  AAdd(0,'SwitchType',                'ChannelSwitchControl', 'SwitchType',
    'SELECT * '             +
    CChannelSwitchControl, '');
  AAdd(0,'SwitchInitialStatus',       'ChannelSwitchControl', 'SwitchInitialStatus',
    'SELECT * '    +
    CChannelSwitchControl, '');

{* ConfigGrowthFactors****************************************************************}
  AAdd(0,'GrowthFactorsYearCount','GrowthFactorConfig','YearsCount',
       'SELECT * ' +
        CGrowthFactorConfig,'');
{* DemandGrowthFactors****************************************************************}
  AAdd(0,'ChannelNumber','GrowthFactorDemand','ChannelNumber',
       'SELECT * ' +
        CGrowthFactorDemand,'');
  AAdd(0,'DemandGrowthFactors','GrowthFactorDemand','Factors',
       'SELECT * ' +
        CGrowthFactorDemand,'');
  AAdd(0,'ValidFactors','GrowthFactorDemand','ValidFactors',
       'SELECT * ' +
        CGrowthFactorDemand,'');


{* MinMaxGrowthFactors****************************************************************}

  AAdd(0,'ArcNumber','GrowthFactorMinMax','ArcNumber',
       'SELECT * '+
        CGrowthFactorMinMax,'');
 AAdd(0,'ChannelNumber','GrowthFactorMinMax','ChannelNumber',
       'SELECT * ' +
        CGrowthFactorMinMax,'');
  AAdd(0,'MinMaxGrowthFactors','GrowthFactorMinMax','Factors',
       'SELECT * '+
        CGrowthFactorMinMax,'');

  AAdd(0,'MinMaxValidFactors','GrowthFactorMinMax','ValidFactors',
       'SELECT * '+
        CGrowthFactorMinMax,'');



{* HydrologyGrowthFactors**************************************************************** }
  AAdd(0,'GrowthFactorGaugeNumber','GrowthFactorHydrology','GaugeNumber',
       'SELECT * '+
        CGrowthFactorHydrology,'');
  AAdd(0,'AFFGrowthFactors','GrowthFactorHydrology','AFFFactors',
       'SELECT * '+
        CGrowthFactorHydrology,'');
  AAdd(0,'IRRGrowthFactors','GrowthFactorHydrology','IRRFactors',
       'SELECT * '+
        CGrowthFactorHydrology,'');
  AAdd(0,'URBGrowthFactors','GrowthFactorHydrology','URBFactors',
       'SELECT * ' +
        CGrowthFactorHydrology,'');

{*ExcelDemandGrowthFactors *************************************************************  }
  AAdd(0,'GrowthProjectionYearsCount','GrowthFactorExcelConfig','YearsCount',
         'SELECT * '+
        CExcelDemandGrowthFactorConfig,'');
  AAdd(0,'GrowthProjectionBaseYear','GrowthFactorExcelConfig','BaseYear',
         'SELECT * '+
        CExcelDemandGrowthFactorConfig,'');
  AAdd(0,'GrowthProjectionStartYear','GrowthFactorExcelConfig','StartYear',
         'SELECT * '+
        CExcelDemandGrowthFactorConfig,'');
  AAdd(0,'ExcelDemandGrowthFactors','GrowthFactorExcelDemand','Factors',
       'SELECT * '+
        CExcelDemandGrowthFactors,'');
  AAdd(0,'ExcelMinMaxGrowthFactors','GrowthFactorExcelMinMax','Factors',
       'SELECT * '+
        CExcelMinMaxGrowthFactors,'');
  AAdd(0,'ExcelAFFGrowthFactors','GrowthFactorExcelHydrology','AFFFactors',
       'SELECT * '+
        CExcelHydrologyGrowthFactors,'');
  AAdd(0,'ExcelIRRGrowthFactors','GrowthFactorExcelHydrology','IRRFactors',
       'SELECT * '+
        CExcelHydrologyGrowthFactors,'');
  AAdd(0,'ExcelURBGrowthFactors','GrowthFactorExcelHydrology','URBFactors',
       'SELECT * '+
        CExcelHydrologyGrowthFactors,'');

{* Disbenefit Function definition **************************************}

  AAdd(0,'YearDemandChannelActive','DisbenefitFunction','YearActive',
       'SELECT * '+
        CDisbenefitFunction,'');
  AAdd(0,'MonthDemandChannelActive','DisbenefitFunction','MonthActive',
       'SELECT * '+
        CDisbenefitFunction,'');
  AAdd(0,'YearDemandChannelObsolete','DisbenefitFunction','YearObsolete',
       'SELECT * '+
        CDisbenefitFunction,'');
  AAdd(0,'MonthDemandChannelObsolete','DisbenefitFunction','MonthObsolete',
       'SELECT * '+
        CDisbenefitFunction,'');
  AAdd(0,'TDSConcentration','DisbenefitFunction','FieldNameIdentifier,1=TDSConcentration%2.2d',
    'SELECT *' +
        CDisbenefitFunction,'' );
   AAdd(0,'WaterQualityConstraint','DisbenefitFunction','WQConstraint',
       'SELECT * '+
        CDisbenefitFunction,'');

  AAdd(0,'NrOfEconomicVariableYears','DisbenefitFunction','NrOfEconomicYears',
       'SELECT * '+
        CDisbenefitFunction,'');
  AAdd(0,'EquationFunctionX','DisbenefitFunction','EquationDisbenefitX',
       'SELECT * '+
        CDisbenefitFunction,'');
  AAdd(0,'EquationFunctionY','DisbenefitFunction','EquationDisbenefitY',
       'SELECT * '+
        CDisbenefitFunction,'');
  AAdd(0,'EquationFunctionCostY','DisbenefitFunction','EquationDisbenefitCost',
       'SELECT * '+
        CDisbenefitFunction,'');
  AAdd(0,'EquationFunctionNonSupply','DisbenefitFunction','EquationDisbenefitNonSupply',
       'SELECT * '+
        CDisbenefitFunction,'');
  AAdd(0,'DisbenefitEscalationRate','DisbenefitFunction','EscalationRate',
       'SELECT * '+
        CDisbenefitFunction,'');

 (* Return Flow channel**********************************************************)
 AAdd(0,'NumOfCorrespondingChannels','ReturnFlowChannelDetail','CorrespondingChannels',
      'SELECT * '+
      CReturnFlowChannelDetail,'' );
 AAdd(0,'ReturnFlowGaugeNumber','ReturnFlowChannelDetail','GaugeNumber',
      'SELECT * '+
      CReturnFlowChannelDetail,'' );
 AAdd(0,'MonthlyAvrgFactor','ReturnFlowChannelDetail','MonthlyAvrgFactor',
      'SELECT * '+
      CReturnFlowChannelDetail,'' );
 AAdd(0,'CalibrationFactor','ReturnFlowChannelDetail','CalibrationFactor',
      'SELECT * '+
      CReturnFlowChannelDetail,'' );
 AAdd(0,'MonthlyAvrgNetEvap','ReturnFlowChannelDetail','MonthlyAvrgFactorEvap',
      'SELECT * '+
      CReturnFlowChannelDetail,'' );
 AAdd(0,'RoutingConstant','ReturnFlowChannelDetail','RoutingConstant',
      'SELECT * '+
      CReturnFlowChannelDetail,'' );
 AAdd(0,'CurtailmentFactor','ReturnFlowChannelDetail','CurtailmentFactor',
      'SELECT * '+
      CReturnFlowChannelDetail,'' );
 AAdd(0,'MultiplicationFactor','ReturnFlowChannelDetail','MultiplicationFactor',
      'SELECT * '+
      CReturnFlowChannelDetail,'' );

 (* Channel Tarrif Calculation**********************************************************)
 AAdd(0,'DataYears','TariffCalculationConfig','DataYears',
      'SELECT * '+
      CTariffCalculationConfig,'' );
 AAdd(0,'ChannelNumber','TariffCalculation','ChannelNumber',
      'SELECT * '+
      CTariffCalculation,'' );
 AAdd(0,'Tariff','TariffCalculation','Tariff',
      'SELECT * '+
      CTariffCalculation,'' );
 AAdd(0,'EscalationFactors','TariffCalculation','EscalationFactors',
      'SELECT * '+
      CTariffCalculation,'' );


(*PotentialMonthlyEvap**********************************************************)
 AAdd(0,'PotentialMonthlyEvap','ReturnFlowMonthlyEvaporation','FieldNameIdentifier,1=PotentialMonthlyEvap%2.2d',
    'SELECT * ' +
      CReturnFlowMonthlyEvaporation,'' );
(*Corresponding channels***********************************************************)
 AAdd(0,'CorrespondingChannel','ReturnFlowCorrespondingChannel','ChannelNumber',
      'SELECT * '+
      CReturnFlowCorrespondingChannel,'' );
 AAdd(0,'AbstractionChannel','ReturnFlowCorrespondingChannel','AbstractionChannel',
      'SELECT * '+
      CReturnFlowCorrespondingChannel,'' );
 AAdd(0,'AssumedFactor','ReturnFlowCorrespondingChannel','AssumedFactor',
      'SELECT * '+
      CReturnFlowCorrespondingChannel,'' );


 (**MIMM Mine Growth Factor*******)

 AAdd(0,'InterpolationMethod','MineGrowthFactors','InterpolationMethod',
      'SELECT * '+
      CMineGrowthFactors,'' );

 AAdd(0,'NoOfPoints','MineGrowthFactors','NoOfPoints',
      'SELECT * '+
      CMineGrowthFactors,'' );
 AAdd(0,'NYR','MineGrowthFactors','NoOfYears',
      'SELECT * '+
      CMineGrowthFactors,'' );
 AAdd(0,'GrowthFactors','MineGrowthFactors','GrowthFactors',
      'SELECT * '+
      CMineGrowthFactors,'' );
 AAdd(0,'StdDeviation','MineLoadGenerationFlow','StdDeviation',
      'SELECT * '+
      CMineLoadGenerationFlow,'');
 AAdd(1,'StdDeviation','MineLoadGenerationMeanOfSalt','StdDeviation',
      'SELECT * '+
      CMineLoadGenerationMeanOfSalt,'');
 AAdd(0,'Flow',     'MineLoadGenerationFlow', 'FieldNameIdentifier,1=Flow%2.2d',
    'SELECT * ' +
    CMineLoadGenerationFlow, '');
 AAdd(0,'MeanOfSalt',     'MineLoadGenerationMeanOfSalt', 'FieldNameIdentifier,1=MeanOfSalt%2.2d',
    'SELECT * ' +
    CMineLoadGenerationMeanOfSalt, '');
 AAdd(0, 'AbstractionIndicator', 'MineOpenCast', 'Abstraction', 'SELECT * ' + CMineOpenCast, '');
 AAdd(0, 'PCDIniConcentration', 'MineOpenCast', 'PCDIniConcentration', 'SELECT * ' + CMineOpenCast, '');
 AAdd(0, 'WorkingCommYear', 'MineOpenCast', 'WorkingCommYear', 'SELECT * ' + CMineOpenCast, '');
 AAdd(0, 'WorkingCommMonth', 'MineOpenCast', 'WorkingCommMonth', 'SELECT * ' + CMineOpenCast, '');
 AAdd(0, 'WorkingDecommYear', 'MineOpenCast', 'WorkingDecommYear', 'SELECT * ' + CMineOpenCast, '');
 AAdd(0, 'WorkingDecommMonth', 'MineOpenCast', 'WorkingDecommMonth', 'SELECT * ' + CMineOpenCast, '');
 AAdd(0, 'RunoffSaltWashOffEfficiencyFactor', 'MineOpenCast', 'RunoffSaltWashOffEfficiencyFactor', 'SELECT * ' + CMineOpenCast, '');
 AAdd(0, 'OpenCastIniSaltStore', 'MineOpenCast', 'IniSaltStore', 'SELECT * ' + CMineOpenCast, '');
 AAdd(0, 'RechargeRate', 'MineOpenCast', 'ReChargeRate', 'SELECT * ' + CMineOpenCast, '');
 AAdd(0, 'AbstractToEvap', 'MineOpenCast', 'AbstractToEvap', 'SELECT * ' + CMineOpenCast, '');
 AAdd(0, 'AbstractToRiver', 'MineOpenCast', 'AbstractToRiver', 'SELECT * ' + CMineOpenCast, '');
 AAdd(0, 'AbstractToCPD', 'MineOpenCast', 'AbstractToPCD', 'SELECT * ' + CMineOpenCast, '');
 AAdd(0, 'AbstractMonthTimeSeriesFile', 'MineOpenCast', 'AbstractMonthTimeSeriesFile', 'SELECT * ' + CMineOpenCast, '');

 AAdd(0, 'SaltConcentration', 'MineSlurryDump','SaltConcentration','SELECT * ' + CMineSlurryDump, '');

 AAdd(0,'SaltWashoffNo',     'Mine', 'SaltWashOffNo',
    'SELECT * ' +
    CMineDetails, '');

 AAdd(0,'PlanningMineRainfallFile',     'Mine', 'RainfallFileName',
    'SELECT * ' +
    CMineDetails, '');

 AAdd(0,'MeanAnnualPrecipitation',     'Mine', 'MeanAnnPrecip',
    'SELECT * ' +
    CMineDetails, '');

 AAdd(0,'SaltBuildUpRate',     'Mine', 'SaltBuildUpRate',
    'SELECT * ' +
    CMineDetails, '');

  AAdd(0,'SaltWashOffEfficiencyFactor',     'Mine', 'SaltWashOffEfficiencyFactor',
    'SELECT * ' +
    CMineDetails, '');

   AAdd(0,'IniSaltStore',     'Mine', 'IniSaltStore',
    'SELECT * ' +
    CMineDetails, '');






end;

end.



