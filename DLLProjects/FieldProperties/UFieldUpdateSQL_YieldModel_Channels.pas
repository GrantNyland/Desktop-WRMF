//
//  UNIT      : Contains field update SQL.
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/01/23
//  COPYRIGHT : Copyright © 2003 DWAF
//
unit UFieldUpdateSQL_YieldModel_Channels;

interface

type TFieldUpdateSQLStepItemAddFunction = procedure (
  AStepNo: integer; AFieldName, ATableName, AFieldInTable, AUpdateSQL, AGetValueSQL: string) of object;

procedure LoadFieldPropertyUpdateSQLSteps(AAdd: TFieldUpdateSQLStepItemAddFunction);
procedure LoadFieldPropertyUpdateSQLSteps1(AAdd: TFieldUpdateSQLStepItemAddFunction);
procedure LoadFieldPropertyUpdateSQLSteps2(AAdd: TFieldUpdateSQLStepItemAddFunction);
procedure LoadIrrigationBlockFieldPropertyUpdateSQLSteps(AAdd: TFieldUpdateSQLStepItemAddFunction);
procedure LoadWetlandFieldPropertyUpdateSQLSteps(AAdd: TFieldUpdateSQLStepItemAddFunction);
procedure LoadYMDemandCentreFieldPropertyUpdateSQLSteps(AAdd: TFieldUpdateSQLStepItemAddFunction);
procedure LoadSFRFieldPropertyUpdateSQLSteps(AAdd: TFieldUpdateSQLStepItemAddFunction);
procedure LoadYMDemandCentreReturnFlowFeatureFieldPropertyUpdateSQLSteps(AAdd: TFieldUpdateSQLStepItemAddFunction);
procedure LoadDailyDiversionFieldPropertyUpdateSQLSteps(AAdd: TFieldUpdateSQLStepItemAddFunction);
procedure LoadParamStochasticsFieldPropertyUpdateSQLSteps(AAdd: TFieldUpdateSQLStepItemAddFunction);

implementation

uses
  UFieldUpdateSQLCommonClauses;

const

  CWhereScenarioIdentifier_SubIdentifier =
    ' WHERE                                                 ' +
    '   (A.Model         = :AModelCode          ) AND ' +
    '   (A.StudyAreaName = :AStudyAreaCode      ) AND ' +
    '   (A.SubArea       = :ASubAreaCode        ) AND ' +
    '   (A.Scenario      = :AScenarioCode       ) AND ' +
    '   (A.Identifier    = :AIdentifier         ) AND ' +
    '   (A.SubIdentifier = :ASubIdentifier)';

  CChannelDetails =
    ' FROM                                            ' +
    '   ChannelDetails A                              ' +
    CWhereScenarioIdentifier;

  CChannelDetailsChannelID =
    ' FROM                                            ' +
    '   ChannelDetails A                              ' +
    CWhereScenarioIdentifier;

  CParamStochastics =
    ' FROM                                            ' +
    '   ParamStochastics A                              ' +
    CWhereScenarioIdentifier;

  CChannelChannelNumber =
    ' FROM                                            ' +
    '   ChannelDetails A                              ' +
    CWhereScenario                                      + '  AND ' +
    '   (A.ChannelNumber = :AChannelNumber)    ' ;

  CChannelArcPenalty =
    ' FROM                                            ' +
    '   ChannelArcPenalty A                           ' +
    CWhereScenarioIdentifier;

  CMinMaxChannel =
    ' FROM MinMaxChannel A                            ' +
    CWhereScenarioIdentifier;

  CPumpingFeature =
    ' FROM PumpingFeature A                           ' +
    CWhereScenarioIdentifier;

  CSpecifiedDemandFeature =
    ' FROM SpecifiedDemandFeature A                   ' +
    CWhereScenarioIdentifier;

  CSpecifiedInflowFeature =
    ' FROM SpecifiedInflowFeature A                   ' +
    CWhereScenarioIdentifier;

  CMinFlowChannel =
    ' FROM MinFlowChannel A                           ' +
    CWhereScenarioIdentifier;

  CMinFlowChannelValue =
    ' FROM MinFlowChannelValue A                      ' +
    CWhereScenarioIdentifier;

  CLossFeature =
    ' FROM LossFeature A                              ' +
    CWhereScenarioIdentifier;

  CLossFeatureValue =
    ' FROM LossFeatureValue A                         ' +
    CWhereScenarioIdentifier_SubIdentifier;

  CMinMaxChannelFlow =
    ' FROM MinMaxChannelFlow A                        ' +
    CWhereScenarioIdentifier_SubIdentifier;

  CMinMaxChannelDistribution =
    ' FROM MinMaxChannelDistribution A                        ' +
    CWhereScenarioIdentifier_SubIdentifier;

  CMasterControlFeature =
    ' FROM MasterControlFeature A                     ' +
    CWhereScenarioIdentifier;

  CMasterControlDistributionFactors =
    ' FROM MasterControlDistributionFactors A         ' +
    CWhereScenarioIdentifier_SubIdentifier;

  CCurtailment =
    ' FROM Curtailment A         ' +
    CWhereScenario;

  CCurtailedChannel =
    ' FROM CurtailmentChannel A         ' +
    CWhereScenarioIdentifier;

  CDroughtRestriction =
    ' FROM DroughtRestriction A                     ' +
    CWhereScenarioIdentifier;

  CDroughtRestrictionFactors =
     ' FROM DroughtRestrictionFactors A               ' +
    CWhereScenarioIdentifier;

  CDroughtRestrictionVolumes =
    ' FROM DroughtRestrictionStorageVolumes A                     ' +
    CWhereScenarioIdentifier;



  {****************************************************************************}
  {* Network Features                                                         *}
  {****************************************************************************}

  CWhereScenario =
    ' WHERE                                           ' +
    '   (A.Model         = :AModelCode          ) AND ' +
    '   (A.StudyAreaName = :AStudyAreaCode      ) AND ' +
    '   (A.SubArea       = :ASubAreaCode        ) AND ' +
    '   (A.Scenario      = :AScenarioCode       )     ';

  CDiversionFeatures =
    ' FROM DiversionFeatures A                  ' +
    CWhereScenario +
    ' AND (A.Identifier    = :AIdentifier)     ' ;

  CDiversionFeaturesType1n2_Demands =
    ' FROM DiversionFeaturesType1n2 A                  ' +
    CWhereScenario +
    ' AND (A.Identifier    = :AIdentifier)     ' +
    ' AND (A.DiversionCode = 1)     ' ;

  CDiversionFeaturesType1n2_Flows =
    ' FROM DiversionFeaturesType1n2 A                  ' +
    CWhereScenario +
    ' AND (A.Identifier    = :AIdentifier)     ' +
    ' AND (A.DiversionCode = 2)     ' ;

  CDiversionEfficienciesLevels =
    ' FROM DiversionFeaturesType3 A                ' +
    CWhereScenario +
    ' AND (A.Identifier    = :AIdentifier)     ';

  CDiversionFeaturesType3Proportions =
    ' FROM DiversionFeaturesType3Proportions A                 ' +
    CWhereScenario +
    ' AND (A.Identifier     = :AIdentifier)    ' +
    ' AND (A.DiversionIndex = :ADiversionIndex)       ';

  CConstraintsDetails =
    ' FROM FlowConstraints A                       ' +
    CWhereScenario +
    ' AND (A.Identifier     = :AIdentifier)    ';

  CConstraintsValue =
    ' FROM FlowConstraintsValue A ' +
    CWhereScenario +
    ' AND (A.Identifier     = :AIdentifier)' +
    ' AND (A.GroupNumber    = :AGroupNumber)' +
    ' AND (A.SubGroupNumber = :ASubGroupNumber)' +
    ' AND (A.LineNumber     = :ALineNumber)';


  CIFRFeatures =
    ' FROM IFRFeatures A                  ' +
    CWhereScenario +
    ' AND (A.Identifier     = :AIdentifier)    ';

  CIFRReference =
    ' FROM IFRReference A                  ' +
    CWhereScenario;

  CIFRFeaturesDetails =
    ' FROM IFRFeaturesDetails A               ' +
    CWhereScenario +
    ' AND (A.Identifier     = :AIdentifier)    ' +
    ' AND (A.LineNumber     = :ALineNumber)           ';

  CIrrigationAreas =
    ' FROM IrrigationAreas A                                 ' +
    CWhereScenario +
    ' AND (A.Identifier     = :AIdentifier)    ';

  CIrrigationAreasDiversionFlow =
    ' FROM IrrigationAreasDiversionFlow A                         ' +
    CWhereScenario +
    ' AND (A.Identifier     = :AIdentifier)    ';

  CIrrigationAreasReturnFlow =
    ' FROM IrrigationAreasReturnFlow A                            ' +
    CWhereScenario +
    ' AND (A.Identifier     = :AIdentifier)    ';

  CYMDemandCentre =
    ' FROM YMDemandCentre A   ' +
    CWhereScenario +
    ' AND (A.Identifier     = :AIdentifier)    ';

  CWhereScenarioIdentifier_YMDemandCentreReturnFlowChannel =
    ' WHERE                                                 ' +
    '   (A.Model            = :AModelCode          ) AND ' +
    '   (A.StudyAreaName    = :AStudyAreaCode      ) AND ' +
    '   (A.SubArea          = :ASubAreaCode        ) AND ' +
    '   (A.Scenario         = :AScenarioCode       ) AND ' +
    '   (A.DemandCentreID   = :ADemandCentreID    ) AND ' +
    '   (A.Identifier       = :AIdentifier)';
  CYMDemandCentreReturnFlowChannel =
    ' FROM YMDemandCentreReturnFlowChannel A   ' +
    CWhereScenarioIdentifier_YMDemandCentreReturnFlowChannel;

  CIrrigationBlock =
    ' FROM IrrigationBlock A   ' +
    CWhereScenario +
    ' AND (A.Identifier     = :AIdentifier)    ';

  CIrrigationBlockdDetails =
    ' FROM IrrigationBlockDetails A   ' +
    CWhereScenario +
    ' AND (A.Identifier     = :AIdentifier)    ';

  CIrrigationBlockAPanConvFactor =
    ' FROM IrrigationBlockAPanConvFactor A   ' +
    CWhereScenario +
    ' AND (A.Identifier     = :AIdentifier)    ';

  CIrrigationBlockRainfallFactor =
    ' FROM IrrigationBlockRainfallFactor A   ' +
    CWhereScenario +
    ' AND (A.Identifier     = :AIdentifier)    ';

  CIrrigationBlockPanEvaporation =
    ' FROM IrrigationBlockPanEvaporation A   ' +
    CWhereScenario +
    ' AND (A.Identifier     = :AIdentifier)    ';

  CWhereScenarioIdentifier_IrrigationBlockWaterUsageFactor =
    ' WHERE                                                 ' +
    '   (A.Model            = :AModelCode          ) AND ' +
    '   (A.StudyAreaName    = :AStudyAreaCode      ) AND ' +
    '   (A.SubArea          = :ASubAreaCode        ) AND ' +
    '   (A.Scenario         = :AScenarioCode       ) AND ' +
    '   (A.BlockIdentifier  = :ABlockIdentifier    ) AND ' +
    '   (A.Identifier       = :AIdentifier)';
  CIrrigationBlockWaterUsageFactor =
    ' FROM IrrigationBlockWaterUsageFactor A   ' +
    CWhereScenarioIdentifier_IrrigationBlockWaterUsageFactor;

   CIrrigationBlockMaxMeanRainfallFactor =
    ' FROM IrrigationBlockMaxMeanRainfallFactor A   ' +
     CWhereScenario +
    ' AND (A.Identifier     = :AIdentifier)    ';

  CPowerPlants =
    ' FROM PowerPlants A                         ' +
    CWhereScenario +
    ' AND (A.Identifier     = :AIdentifier)    ';

  CPowerPlantsDetails_Efficiency =
    ' FROM PowerPlantsDetails A                      ' +
    CWhereScenario +
    ' AND (A.Identifier = :AIdentifier)        ' +
    ' AND (A.FactorCode = 1)     ' ;

  CPowerPlantsDetails_NetHead =
    ' FROM PowerPlantsDetails A                      ' +
    CWhereScenario +
    ' AND (A.Identifier = :AIdentifier)        ' +
    ' AND (A.FactorCode = 2)     ' ;

  CPowerPlantsDetails_Discharge =
    ' FROM PowerPlantsDetails A                      ' +
    CWhereScenario +
    ' AND (A.Identifier = :AIdentifier)        ' +
    ' AND (A.FactorCode = 3)     ' ;

  CPowerPlantsDetails_Tailwater =
    ' FROM PowerPlantsDetails A                      ' +
    CWhereScenario +
    ' AND (A.Identifier = :AIdentifier)        ' +
    ' AND (A.FactorCode = 4)     ' ;

  CPowerPlantsDemands_Generation =
    ' FROM PowerPlantsDemands A                      ' +
    CWhereScenario +
    ' AND (A.Identifier = :AIdentifier)        ' +
    ' AND (A.PowerCode = 1)     ' ;

  CPowerPlantsDemands_Release =
    ' FROM PowerPlantsDemands A                      ' +
    CWhereScenario +
    ' AND (A.Identifier = :AIdentifier)        ' +
    ' AND (A.PowerCode = 2)     ' ;

  CWaterDemandRiskCriteria =
    ' FROM WaterDemandRiskCriteria A           ' +
    CWhereScenario;

  CWaterDemandCategories =
    ' FROM WaterDemandCategories A             ' +
    CWhereScenario +
    ' AND (A.CategoryID = :ACategoryID)        ' ;

  CWaterDemandFeatures =
    ' FROM WaterDemandFeatures A               ' +
    CWhereScenario +
    ' AND (A.FeatureID = :AFeatureID)         ' ;

  CWaterDemandCounts =
    ' FROM WaterDemandCounts A               ' +
    CWhereScenario;

  CWaterDemandFileCreate =
    ' FROM FileCreate A                      ' +
    CWhereScenario;


  CChannelAreas =
    ' FROM ChannelArea A                     ' +
  CWhereScenario +
    ' AND (A.AreaID = :AAreaID)        ' ;

  CWaterUseProportions =
  ' FROM WaterUseProportions A         '+
  CWhereScenario +
  ' AND A.ChannelNumber = :AChannelNumber      ';

  CDemandCentre =
  ' FROM DemandCentre A '+
  CWhereScenario +
  ' AND A.ChannelNumber = :AChannelNumber      ';

  CWetland =
    ' FROM Wetland A   ' +
    CWhereScenario +
    ' AND (A.Identifier     = :AIdentifier)    ';

CSFRSubCatchment =
    ' FROM SFRSubCatchment A   ' +
    CWhereScenario +
    ' AND (A.Identifier     = :AIdentifier)    ';


(*****Daily Diversion PreProcessor*******)
  CDailyDiversionStation =
    ' FROM DailyDiversionStation A   ' +
    CWhereScenario +
    ' AND (A.StationID     = :AStationID)    ';

  CDailyDiversionFileData =
    ' FROM DailyDiversionFileData A   ' +
    CWhereScenario +
    ' AND (A.StationID     = :AStationID)    '+
    ' AND (A.Identifier     = :AIdentifier)    ';

  CDailyInstreamFileData =
    ' FROM DailyInstreamFileData A   ' +
    CWhereScenario +
    ' AND (A.StationID     = :AStationID)    '+
    ' AND (A.Identifier     = :AIdentifier)    ';

  CDailyDiversionCompensationValues =
    ' FROM DailyDiversionCompensationValues A   ' +
    CWhereScenario +
    ' AND (A.StationID     = :AStationID)    ';

  CDailyDiversionThresholdValue =
    ' FROM DailyDiversionMonthlyThreshold A   ' +
    CWhereScenario +
    ' AND (A.StationID     = :AStationID)    ';

 CDailyDiversionFlowRelationship =
    ' FROM DailyDiversionFlowRelationship A   ' +
    CWhereScenario +
    ' AND (A.StationID     = :AStationID)    '+
    ' AND (A.Identifier     = :AIdentifier)    ';

 CDailyDiversionWRYMData =
    ' FROM DailyDiversionWRYMData A   ' +
    CWhereScenario +
    ' AND (A.StationID     = :AStationID)    '+
    ' AND (A.Identifier     = :AIdentifier)    ';

(*****Study MetaData Errors*******)
  CStudyMetaDataErrors =
    ' FROM StudyMetaData A   ' +
    CWhereScenario +
    ' AND (A.Identifier     = :AIdentifier)    ';

procedure LoadFieldPropertyUpdateSQLSteps1(AAdd: TFieldUpdateSQLStepItemAddFunction);
const OPNAME = 'LoadFieldPropertyUpdateSQLSteps';
begin

  AAdd(0,'FirmYieldCalc',  'ChannelDetails','FirmYieldCalc',  'SELECT *  ' + CChannelDetails, '');
  AAdd(0,'SummaryOutput',  'ChannelDetails','SummaryOutput',  'SELECT *  ' + CChannelDetails, '');
  AAdd(0,'FlowOutput',     'ChannelDetails','FlowOutput',     'SELECT *     ' + CChannelDetails, '');
  AAdd(0,'ChannelNumber',  'ChannelDetails','ChannelNumber',  'SELECT *  ' + CChannelDetails, '');
  AAdd(0,'ChannelName',    'ChannelDetails','ChannelName',    'SELECT *    ' + CChannelDetails, '');
  AAdd(0,'ChannelSubType', 'ChannelDetails','ChannelSubType', 'SELECT * ' + CChannelDetailsChannelID, '');
  AAdd(0,'Comment',        'ChannelDetails','Comment',        'SELECT *        ' + CChannelDetails, '');
  AAdd(0,'ChannelType',    'ChannelDetails','ChannelType',    'SELECT *    ' + CChannelDetailsChannelID, '');
  AAdd(0,'DownNodeNumber', 'ChannelDetails','DownNodeNumber', 'SELECT * ' + CChannelDetails, '');
  AAdd(0,'PenaltyNumber',  'ChannelDetails','PenaltyNumber',  'SELECT *  ' + CChannelDetails, '');
  AAdd(0,'UpNodeNumber',   'ChannelDetails','UpNodeNumber',   'SELECT *   ' + CChannelDetails, '');
  AAdd(0,'ChannelAreaID',  'ChannelDetails','ChannelAreaID',  'SELECT *  ' + CChannelDetails, '');

  //
  // ChannelArcPenalty
  //
  AAdd(0, 'ChannelPenaltyName', 'ChannelArcPenalty', 'PenaltyName', 'SELECT * ' + CChannelArcPenalty, '');
  AAdd(0, 'ArcCount', 'ChannelArcPenalty', 'ArcCount', 'SELECT * ' + CChannelArcPenalty, '');
  AAdd(0, 'Penalty',  'ChannelArcPenalty', 'FieldNameIdentifier,1=Penalty%2.2d',
    ' SELECT *' + CChannelArcPenalty, '');

  //
  // PumpingFeature
  //
  AAdd(0,'PumpEfficiency', 'PumpingFeature', 'PumpingEfficiency',
    'SELECT * ' + CPumpingFeature, '');
  AAdd(0,'PumpingHead',    'PumpingFeature', 'PumpingHead',
    'SELECT *       ' + CPumpingFeature, '');
  AAdd(0,'PumpingFeatureName', 'PumpingFeature', 'FeatureName',
    'SELECT *       ' + CPumpingFeature, '');
  AAdd(0,'PumpingFeatureChannelNumber', 'PumpingFeature', 'ChannelNumber',
    'SELECT *     ' + CPumpingFeature, '');

  //
  // SpecifiedDemandFeature
  //
  AAdd(0,'SpecifiedDemandFeatureName', 'SpecifiedDemandFeature', 'FeatureName',
    'SELECT * ' + CSpecifiedDemandFeature, '');
  AAdd(0,'SpecifiedDemandFeatureChannelNumber', 'SpecifiedDemandFeature', 'ChannelNumber',
    'SELECT * ' + CSpecifiedDemandFeature, '');
  AAdd(0,'Fullname',    'SpecifiedDemandFeature','Fullname',
    'SELECT *    ' + CSpecifiedDemandFeature, '');
  AAdd(0,'GaugeNumber', 'SpecifiedDemandFeature','GaugeNumber',
    'SELECT * ' + CSpecifiedDemandFeature, '');
  AAdd(0,'Stochastic',  'SpecifiedDemandFeature','Stochastic',
    'SELECT *  ' + CSpecifiedDemandFeature, '');

  //
  // SpecifiedInflowFeature
  //
  AAdd(0,'SpecifiedInflowFeatureName', 'SpecifiedInflowFeature', 'FeatureName',
    'SELECT * ' + CSpecifiedInflowFeature, '');
  AAdd(0,'InflowFileName', 'SpecifiedInflowFeature', 'InflowFileName',
    'SELECT * ' + CSpecifiedInflowFeature, '');
  AAdd(0,'SpecifiedInflowFeatureChannelNumber', 'SpecifiedInflowFeature', 'ChannelNumber',
    'SELECT * ' + CSpecifiedInflowFeature, '');

  //
  // MinMaxChannel
  //
  AAdd(0,'MinMaxChannelName','MinMaxChannel','MinMaxChannelName',
       'SELECT * ' + CMinMaxChannel, '');
  AAdd(0,'MinMaxChannelNumber','MinMaxChannel','MinMaxChannelNumber',
       'SELECT * ' + CMinMaxChannel, '');

  //
  // MasterControlFeature
  //
  AAdd(0,'MasterControlChannelNumber', 'MasterControlFeature', 'ChannelNumber',
    'SELECT *            ' + CMasterControlFeature, '');
  AAdd(0,'MasterControlFeatureName',   'MasterControlFeature', 'FeatureName',
    'SELECT *              ' + CMasterControlFeature, '');
  AAdd(0,'MasterChannelType',          'MasterControlFeature', 'MasterControlType',
    'SELECT *        ' + CMasterControlFeature, '');

  AAdd(0,'WaterSupplyDistribution','MasterControlDistributionFactors','FieldNameIdentifier,1=Value%2.2d',
    ' SELECT *' + CMasterControlDistributionFactors, '');

  AAdd(0,'MinEnergyDemand','MasterControlFeature','FieldNameIdentifier,1=Value%2.2d',
    ' SELECT *' + CMasterControlFeature, '');

  //
  // MinFlowChannel
  //
  AAdd(0,'MinFlowChannelName',  'MinFlowChannel', 'MinFlowChannelName',
       'SELECT * ' + CMinFlowChannel, '');
  AAdd(0,'MinFlowFeatureChannelNumber','MinFlowChannel', 'MinFlowChannelNumber',
       'SELECT * ' + CMinFlowChannel, '');

  //
  // MinFlowChannelValue
  //
  AAdd(0,'MinFlowDemand','MinFlowChannelValue','FieldNameIdentifier,1=Value%2.2d',
    ' SELECT *' + CMinFlowChannelValue, '');

  //
  // LossFeature
  //
  AAdd(0,'LossFeatureName','LossFeature','FeatureName',
    'SELECT * ' + CLossFeature, '');
  AAdd(0,'LossFeatureChannelNumber', 'LossFeature','ChannelNumber',
    'SELECT * ' + CLossFeature, '');
  AAdd(0,'Reference',      'LossFeature','Reference',
    'SELECT *   ' + CLossFeature, '');

  //
  // LossFeatureValue .... WaterLoss
  //
  AAdd(0,'WaterLoss','LossFeatureValue','FieldNameIdentifier,1=Value%2.2d',
    ' SELECT *' +  CLossFeatureValue, '');

  //
  // LossFeatureValue .... DivertedFlowP
  //
  AAdd(0,'DivertedFlowP','LossFeatureValue','FieldNameIdentifier,1=Value%2.2d',
    ' SELECT *' + CLossFeatureValue, '');

  //
  // MinMaxChannelFlow
  //
  AAdd(0,'FlowConstraints','MinMaxChannelFlow','FieldNameIdentifier,1=MFlow%2.2d',
    ' SELECT *' + CMinMaxChannelFlow, '');

  AAdd(0,'MinMaxDistribution','MinMaxChannelDistribution','FieldNameIdentifier,1=Distribution%2.2d',
    ' SELECT *' + CMinMaxChannelDistribution, '');



  {****************************************************************************}
  {* Diversion Features                                                       *}
  {****************************************************************************}
  AAdd(0,'DiversionChannelName',   'DiversionFeatures', 'DivChannelName',
    'SELECT * '   +
    CDiversionFeatures, '');
  AAdd(0,'DiversionChannelNumber', 'DiversionFeatures', 'DivChannelNumber',
    'SELECT * ' +
    CDiversionFeatures, '');
  AAdd(0,'DiversionChannelType',      'DiversionFeatures', 'DivChannelType',
    'SELECT * '   +
    CDiversionFeatures, '');
  AAdd(0,'DivStation',      'DiversionFeatures', 'DivStation',
    'SELECT * '   +
    CDiversionFeatures, '');

  AAdd(0,'DiversionDemand',           'DiversionFeaturesType1n2', 'FieldNameIdentifier,1=DivFactor%2.2d',
    'SELECT *' + CDiversionFeaturesType1n2_Demands, '');
  AAdd(0,'FlowRange',           'DiversionFeaturesType1n2', 'FieldNameIdentifier,1=DivFactor%2.2d',
    'SELECT *' + CDiversionFeaturesType1n2_Demands, '');
  AAdd(0,'NetNaturalInflow',          'DiversionFeaturesType1n2', 'FieldNameIdentifier,1=DivFactor%2.2d',
    'SELECT *' + CDiversionFeaturesType1n2_Flows, '');
  AAdd(0,'ActualDivertedFlow',        'DiversionFeaturesType1n2', 'FieldNameIdentifier,1=DivFactor%2.2d',
    'SELECT *' + CDiversionFeaturesType1n2_Flows, '');
  AAdd(0,'ControllingResNodeNumber',  'DiversionFeaturesType3',  'NodeNumber',
    'SELECT * ' +  CDiversionEfficienciesLevels, '');
  AAdd(0,'ReservoirStorageNumber',    'DiversionFeaturesType3',  'ResLevelsCount',
    'SELECT * ' + CDiversionEfficienciesLevels, '');
  AAdd(0,'ReferenceFlowsCount',       'DiversionFeaturesType3',  'RefFlowsCount',
    'SELECT * ' + CDiversionEfficienciesLevels, '');
  AAdd(0,'ControllingResLevels',      'DiversionFeaturesType3', 'FieldNameIdentifier,1=DivLevel%2.2d',
    ' SELECT *' + CDiversionEfficienciesLevels, '');
  AAdd(0,'FlowValue',      'DiversionFeaturesType3Proportions', 'FlowValue',
    'SELECT * ' + CDiversionFeaturesType3Proportions, '');
  AAdd(0,'DivertedFlow',   'DiversionFeaturesType3Proportions', 'FieldNameIdentifier,1=DivProp%2.2d',
    ' SELECT *' +  CDiversionFeaturesType3Proportions, '');

  {****************************************************************************}
  {* Physical Flow Constraints                                                *}
  {****************************************************************************}
  AAdd(0, 'ConstraintsChannelNumber',  'FlowConstraints', 'ConstraintsChannelNumber',
    'SELECT * '  +  CConstraintsDetails, '');
  AAdd(0, 'PhysicalFlowConstraintFeatureName',  'FlowConstraints', 'FeatureName',
    'SELECT * '  +  CConstraintsDetails, '');
  AAdd(0, 'UpStreamReservoirNumber',   'FlowConstraints', 'UpStreamReservoirNumber',
    'SELECT * '   +  CConstraintsDetails, '');
  AAdd(0, 'DownStreamReservoirNumber', 'FlowConstraints', 'DownStreamReservoirNumber',
    'SELECT * ' + CConstraintsDetails, '');
  AAdd(0, 'PointsElevationNumber',     'FlowConstraints', 'PointsElevationNumber',
    'SELECT * '     +  CConstraintsDetails, '');
  AAdd(0, 'SillElevation',             'FlowConstraints', 'SillElevation',
    'SELECT * '     +  CConstraintsDetails, '');
  AAdd(0, 'WaterLevelAtDownstreamNode',             'FlowConstraints', 'WaterLevelAtDownstreamNode',
    'SELECT * '     +  CConstraintsDetails, '');
  AAdd(0, 'ReferenceElevation',                'FlowConstraints', 'ReferenceElevation',
    'SELECT * '     + CConstraintsDetails, '');
  AAdd(0, 'GateHeight',                'FlowConstraints', 'GateHeight',
    'SELECT * '     + CConstraintsDetails, '');

end;

procedure LoadFieldPropertyUpdateSQLSteps2(AAdd: TFieldUpdateSQLStepItemAddFunction);
const OPNAME = 'LoadFieldPropertyUpdateSQLSteps2';
begin

  AAdd(0, 'StructureType',             'FlowConstraints', 'StructureType',
    'SELECT * '     +
    CConstraintsDetails, '');
  AAdd(0, 'DischargeCoefficient',      'FlowConstraints', 'DischargeCoefficient',
    'SELECT * '      +
    CConstraintsDetails, '');
  AAdd(0, 'ControlStructureLength',    'FlowConstraints', 'ControlStructureLength',
    'SELECT * '    +
    CConstraintsDetails, '');
  AAdd(0,'ConstraintElevation', 'FlowConstraintsValue', 'FieldNameIdentifier,1=Value%2.2d',
    ' SELECT * ' +  CConstraintsValue, '');
  AAdd(0,'ConstraintDischarge', 'FlowConstraintsValue', 'FieldNameIdentifier,1=Value%2.2d',
    ' SELECT * ' + CConstraintsValue, '');
  AAdd(0,'ConstraintChannelNumber',   'FlowConstraintsValue', 'FieldNameIdentifier,1=Value%2.2d',
    ' SELECT * ' + CConstraintsValue, '');
  AAdd(0,'ConstraintKFactor',             'FlowConstraintsValue', 'FieldNameIdentifier,1=Value%2.2d',
    ' SELECT * ' + CConstraintsValue, '');
  AAdd(0,'ConstraintHeadDifferences',         'FlowConstraintsValue', 'FieldNameIdentifier,1=Value%2.2d',
    ' SELECT *' +  CConstraintsValue, '');
  AAdd(0,'ConstraintAquiferFlows',         'FlowConstraintsValue', 'FieldNameIdentifier,1=Value%2.2d',
    ' SELECT * ' + CConstraintsValue, '');
  AAdd(0,'ConstraintDownStreamNodeInflows', 'FlowConstraintsValue', 'FieldNameIdentifier,1=Value%2.2d',
    ' SELECT * ' + CConstraintsValue, '');
  AAdd(0,'ConstraintRiverDepths', 'FlowConstraintsValue', 'FieldNameIdentifier,1=Value%2.2d',
    ' SELECT *' + CConstraintsValue, '');
  AAdd(0,'ConstraintElevationDifferences', 'FlowConstraintsValue', 'FieldNameIdentifier,1=Value%2.2d',
    ' SELECT * ' +  CConstraintsValue, '');
  AAdd(0,'ConstraintMonthlyAverageInflows', 'FlowConstraintsValue', 'FieldNameIdentifier,1=Value%2.2d',
    ' SELECT * ' + CConstraintsValue, '');
  AAdd(0,'ConstraintMonthlyAverageDivertedFlow', 'FlowConstraintsValue', 'FieldNameIdentifier,1=Value%2.2d',
    ' SELECT *' + CConstraintsValue, '');
  AAdd(0,'ConstraintPumpingHeads', 'FlowConstraintsValue', 'FieldNameIdentifier,1=Value%2.2d',
    ' SELECT *' + CConstraintsValue, '');
  AAdd(0,'ConstraintPumpingDischarges', 'FlowConstraintsValue', 'FieldNameIdentifier,1=Value%2.2d',
    ' SELECT * ' +  CConstraintsValue, '');


  {****************************************************************************}
  {* Instream flow requirement (IFR) Features                                 *}
  {****************************************************************************}

  AAdd(0, 'IFRInflowOption',    'IFRReference', 'InflowOption',
    'SELECT * '    +
    CIFRReference, '');
  AAdd(0, 'IFRChannelNumber',    'IFRFeatures', 'IFRChannelNumber',
    'SELECT * '    +
    CIFRFeatures, '');
  AAdd(0, 'IFRFeatureName',      'IFRFeatures', 'FeatureName',
    'SELECT * '    +
    CIFRFeatures, '');
  AAdd(0, 'ReferenceNodeCount',  'IFRFeatures', 'ReferenceNodeCount',
    'SELECT * '  +
    CIFRFeatures, '');
  AAdd(0, 'LagInMonthsCount',    'IFRFeatures', 'LagInMonthsCount',
    'SELECT * '    +
    CIFRFeatures, '');
  AAdd(0, 'IFRPointsCount',      'IFRFeatures', 'PointsCount',
    'SELECT * '      +
    CIFRFeatures, '');
  AAdd(0,'RefNodeNumber',        'IFRFeatures', 'RefNodeNumbers',
    ' SELECT * ' +
    CIFRFeatures, '');
  AAdd(0, 'IFRSiteID',  'IFRFeatures', 'IFRSiteID',
    'SELECT * '  +
    CIFRFeatures, '');
  AAdd(0, 'ExceedencePercentage',  'IFRFeatures', 'ExceedencePerc',
    'SELECT * '  +
    CIFRFeatures, '');
  AAdd(0, 'IFRCalcOption',  'IFRFeatures', 'CalculationOption',
    'SELECT * '  +
    CIFRFeatures, '');

  AAdd(0, 'IFRStatusIndicator',  'IFRFeatures', 'IFRStatusIndicator',
    'SELECT * '  +
    CIFRFeatures, '');

 AAdd(0,'IFRLoss',  'IFRFeaturesDetails', 'IFRLoss',
    ' SELECT  * ' +
    CIFRFeatures, '');

  AAdd(0,'IFRVariables',         'IFRFeaturesDetails', 'FieldNameIdentifier,1=InflowVar%2.2d',
    ' SELECT *' +
    CIFRFeaturesDetails, '');
  AAdd(0,'IFRReleaseVariables',  'IFRFeaturesDetails', 'FieldNameIdentifier,1=ReleaseVar%2.2d',
    ' SELECT *' +
    CIFRFeaturesDetails, '');

  AAdd(0,'AnnualInflow',  'IFRFeaturesDetails', 'AnnualInflow',
    ' SELECT  * ' +
    CIFRFeaturesDetails, '');

  AAdd(0,'MonthlyIFRLossStr',  'IFRFeaturesDetails', 'MonthlyIFRLoss',
    ' SELECT  * ' +
    CIFRFeatures, '');


  {****************************************************************************}
  {* Irrigation Areas                                                         *}
  {****************************************************************************}
  AAdd(1,'IrrigationNodeNumber', 'IrrigationAreas', 'NodeNumber',
    'SELECT * ' +
    CIrrigationAreas, '');
  AAdd(0,'AreaName',             'IrrigationAreas', 'AreaName',
    'SELECT * '   +
    CIrrigationAreas, '');
  AAdd(0,'ConsumptiveChannelNumber',    'IrrigationAreas','ConsumptiveChannelNumber',
    'SELECT *     ' + CIrrigationAreas, '');
  AAdd(0,'IrrDiversionChannelNumber',   'IrrigationAreas','DiversionChannelNumber',
    'SELECT *       ' + CIrrigationAreas, '');
  AAdd(0,'ReturnChannelNumber',         'IrrigationAreas','ReturnFlowChannelNumber',
    'SELECT *      ' + CIrrigationAreas, '');
  AAdd(0,'IrrigationNodeNumber',        'IrrigationAreas','IrrigationNodeNumber',
    'SELECT *         ' + CIrrigationAreas, '');
  AAdd(0,'RelaxationDemand',            'IrrigationAreas','RelaxationDemand',
    'SELECT *             ' + CIrrigationAreas, '');

  AAdd(0,'DiversionFlow',        'IrrigationAreasDiversionFlow', 'FieldNameIdentifier,1=DFlow%2.2d',
    ' SELECT                                             ' +
    '   DFlow01, DFlow02, DFlow03, DFlow04, DFlow05, DFlow06, ' +
    '   DFlow07, DFlow08, DFlow09, DFlow10, DFlow11, DFlow12 ' +
    CIrrigationAreasDiversionFlow, '');
  AAdd(0,'ReturnFlow',           'IrrigationAreasReturnFlow',    'FieldNameIdentifier,1=RFlow%2.2d',
    ' SELECT *' +
    CIrrigationAreasReturnFlow, '');

  {****************************************************************************}
  {* Param Stochastics                                                        *}
  {****************************************************************************}
  LoadParamStochasticsFieldPropertyUpdateSQLSteps(AAdd);
  {****************************************************************************}
  {* Irrigation Block                                                         *}
  {****************************************************************************}
   LoadIrrigationBlockFieldPropertyUpdateSQLSteps(AAdd);
  {****************************************************************************}
  {* Wetland                                                                  *}
  {****************************************************************************}
   LoadWetlandFieldPropertyUpdateSQLSteps(AAdd);
  {****************************************************************************}
  {* YMDemandChannels                                                         *}
  {****************************************************************************}
   LoadYMDemandCentreFieldPropertyUpdateSQLSteps(AAdd);
  {****************************************************************************}
  {* Stream Flow Reductions                                                         *}
  {****************************************************************************}
   LoadSFRFieldPropertyUpdateSQLSteps(AAdd);
  {****************************************************************************}
  {* Return Flow feature                                                      *}
  {****************************************************************************}
   LoadYMDemandCentreReturnFlowFeatureFieldPropertyUpdateSQLSteps(AAdd);
   (******Daily Diversion Pre Processor**********)
   LoadDailyDiversionFieldPropertyUpdateSQLSteps(AAdd);
  {****************************************************************************}
  {* Power plants                                                             *}
  {****************************************************************************}
  AAdd(0,'PowerPlantName',     'PowerPlants', 'PowerPlantName',
    'SELECT * '   +
    CPowerPlants, '');
  AAdd(0,'PowerChannelNumber', 'PowerPlants', 'PowerChannelNumber',
    'SELECT * ' +
    CPowerPlants, '');

end;

procedure LoadFieldPropertyUpdateSQLSteps(AAdd: TFieldUpdateSQLStepItemAddFunction);
const OPNAME = 'LoadFieldPropertyUpdateSQLSteps';
begin

  //
  // ChannelDetails.
  //
  LoadFieldPropertyUpdateSQLSteps1(AAdd);
  LoadFieldPropertyUpdateSQLSteps2(AAdd);
  AAdd(0,'SpillChannelNumber', 'PowerPlants', 'SpillChannelNumber',
    'SELECT * ' +
    CPowerPlants, '');
  AAdd(0,'DownStreamPowerChannelNumber', 'PowerPlants', 'FieldNameIdentifier,1=Channel%2.2d',
    ' SELECT * ' +
    CPowerPlants, '');
  AAdd(0,'MaxCapGenerator',    'PowerPlants', 'MaxCapGenerator',
    'SELECT * '  +
    CPowerPlants, '');
  AAdd(0,'MaxCapTurbine',      'PowerPlants', 'MaxCapTurbine',
    'SELECT * '    +
    CPowerPlants, '');
  AAdd(0,'PowerEfficiency',    'PowerPlants', 'Efficiency',
    'SELECT * '       +
    CPowerPlants, '');
  AAdd(0,'PowerPlantStatus',   'PowerPlants', 'PowerPlantStatus',
    'SELECT * ' +
    CPowerPlants, '');
  AAdd(0,'HeadLoss',           'PowerPlants', 'HeadLoss',
    'SELECT * '         +
    CPowerPlants, '');
  AAdd(0,'DesignHead',         'PowerPlants', 'DesignHead',
    'SELECT * '       +
    CPowerPlants, '');
  AAdd(0,'MaxNetHead',         'PowerPlants', 'MaxNetHead',
    'SELECT * '       +
    CPowerPlants, '');
  AAdd(0,'MinNetHead',         'PowerPlants', 'MinNetHead',
    'SELECT * '       +
    CPowerPlants, '');
  AAdd(0,'PowerPointsCount',   'PowerPlants', 'PointsCount',
    'SELECT * '      +
    CPowerPlants, '');
  AAdd(0,'TailWaterCount',     'PowerPlants', 'TailWaterCount',
    'SELECT * '   +
    CPowerPlants, '');
  AAdd(0,'TailWaterTypeCode',   'PowerPlants', 'TailWaterTypeCode',
    'SELECT * ' +
    CPowerPlants, '');
  AAdd(0,'DownStreamPowerChannelCount', 'PowerPlants','DownStreamPowerChannelCount',
    'SELECT *  ' +
    CPowerPlants, '');
  AAdd(0,'EfficiencyFactor',    'PowerPlantsDetails', 'FieldNameIdentifier,1=Factor%2.2d',
    ' SELECT *' +
    CPowerPlantsDetails_Efficiency, '');
  AAdd(0,'NetHeadFactors',      'PowerPlantsDetails', 'FieldNameIdentifier,1=Factor%2.2d',
    ' SELECT *' +
    CPowerPlantsDetails_NetHead, '');
  AAdd(0,'DownStreamLevel',     'PowerPlantsDetails', 'FieldNameIdentifier,1=Factor%2.2d',
    ' SELECT *' +
    CPowerPlantsDetails_Discharge, '');
  AAdd(0,'TailWaterElevation',  'PowerPlantsDetails', 'FieldNameIdentifier,1=Factor%2.2d',
    ' SELECT *' +
    CPowerPlantsDetails_Tailwater, '');
  AAdd(0,'MinEnergyGenerated',  'PowerPlantsDemands', 'FieldNameIdentifier,1=MinPower%2.2d',
    ' SELECT *' +
    CPowerPlantsDemands_Generation, '');
  AAdd(0,'MinPowerChannelRelease', 'PowerPlantsDemands', 'FieldNameIdentifier,1=MinPower%2.2d',
    ' SELECT *' +
    CPowerPlantsDemands_Release, '');

  {****************************************************************************}
  {* Water Demand Features                                                    *}
  {****************************************************************************}
  AAdd(0,'RecurrenceInterval', 'WaterDemandRiskCriteria', 'FieldNameIdentifier,1=Interval%2.2d',
    'SELECT *' +
    CWaterDemandRiskCriteria, '');
  AAdd(0,'WaterDemandCategoryName', 'WaterDemandCategories', 'CategoryDescription',
    'SELECT * '   +
    CWaterDemandCategories, '');
  AAdd(0,'DemandPortion', 'WaterDemandCategories', 'FieldNameIdentifier,1=CriteriaPortion%2.2d',
    'SELECT *' +
    CWaterDemandCategories, '');
  AAdd(0,'WaterDemandFeatureName', 'WaterDemandFeatures', 'FeatureName',
    'SELECT * '   +
    CWaterDemandFeatures, '');
  AAdd(0,'WaterDemandChannelNumber', 'WaterDemandFeatures', 'ChannelNumber',
    'SELECT * ' +
    CWaterDemandFeatures, '');
  AAdd(0,'WaterDemandCategory', 'WaterDemandFeatures', 'CategoryID',
    'SELECT * '   +
    CWaterDemandFeatures, '');
  AAdd(0,'ScenarioPortion', 'WaterDemandFeatures', 'FieldNameIdentifier,1=ScenarioPortion%2.2d',
    'SELECT *' +
    CWaterDemandFeatures, '');

  AAdd(0,'WaterDemandScenarioCount', 'WaterDemandCounts', 'ScenarioCount',
    'SELECT * ' +
    CWaterDemandCounts, '');

  AAdd(0,'WaterUsePortion', 'WaterUseProportion', 'FieldNameIdentifier,1=Category%2.2d',
    'SELECT *' +
    CWaterUseProportions, '');

  AAdd(0,'WaterDemandFileCreate', 'FileCreate', 'ImplementFile',
    'SELECT * ' +
    CWaterDemandFileCreate, '');

  {****************************************************************************}
  {* Channel Area                                                             *}
  {****************************************************************************}
  AAdd(0,'ChannelAreaName', 'ChannelArea', 'AreaName', 'SELECT * '   + CChannelAreas, '');


  {****************************************************************************}
  {* Curtailment                                                              *}
  {****************************************************************************}
  AAdd(0,'CurtailmentStartMonthValue','Curtailment','FieldNameIdentifier,1=Month%2.2d',
    ' SELECT *' +
    CCurtailment, '');
  AAdd(0,'CurtailmentPeriodCount', 'Curtailment', 'CurtailmentPeriodCount', 'SELECT * '   + CCurtailment, '');
  AAdd(0,'CurtailmentFileCreate',  'Curtailment','InUse',    ' SELECT *     ' + CCurtailment, '');

  {****************************************************************************}
  {* Curtailment Channel                                                      *}
  {****************************************************************************}
  AAdd(0,'CurtailmentFactors','Curtailment','FieldNameIdentifier,1=Factor%2.2d',
    ' SELECT *' +
    CCurtailedChannel, '');


  {****************************************************************************}
  {* Drought Restriction                                                      *}
  {****************************************************************************}
  AAdd(0,'DroughtRestrictionName', 'DroughtRestriction', 'Name', 'SELECT * '   + CDroughtRestriction, '');
  AAdd(0,'DroughtRestrictionReservoirNumber', 'DroughtRestriction', 'ReservoirNumbers', 'SELECT * '   + CDroughtRestriction, '');
  AAdd(0,'DroughtRestrictionChannelNumber', 'DroughtRestriction', 'ChannelNumbers', 'SELECT * '   + CDroughtRestriction, '');

  AAdd(0,'AllocationFactors', 'DroughtRestrictionFactors', 'FieldNameIdentifier,1=Factor%2.2d',
    ' SELECT *' +
    CDroughtRestrictionFactors, '');

 AAdd(0,'ReferenceStorageVolumes', 'DroughtRestrictionStorageVolumes', 'FieldNameIdentifier,1=Volume%2.2d',
    ' SELECT *' +
    CDroughtRestrictionVolumes, '');

  //
  // Demand Centre
  //
  AAdd(0,'DemandCentreType', 'DemandCentre', 'DemandCentreType',
     'SELECT *' + CDemandCentre, '');
  AAdd(0,'AnnualDemand',  'DemandCentre', 'AnnualDemand',
     'SELECT * '+ CDemandCentre, '');
  AAdd(0,'MinimumDemand', 'DemandCentre', 'MinimumDemand',
    'SELECT * '+ CDemandCentre, '');
  AAdd(0,'IncludeInOutput', 'DemandCentre', 'IncludeInOutput',
    'SELECT *' + CDemandCentre, '');

  //
  // Study MetaData Errors
  //
  AAdd(0,'ImportedBy', 'StudyMetaData', 'ImportedBy',
     'SELECT * ' + CStudyMetaDataErrors, '');
  AAdd(0,'ErrorType', 'StudyMetaData', 'ErrorType',
     'SELECT * ' + CStudyMetaDataErrors, '');
  AAdd(0,'ErrorDescription', 'StudyMetaData', 'ErrorDescription',
     'SELECT * ' + CStudyMetaDataErrors, '');
  AAdd(0,'StudyErrors', 'StudyMetaData', 'StudyErrors',
     'SELECT * ' + CStudyMetaDataErrors, '');
  AAdd(0,'CorrectiveAction', 'StudyMetaData', 'CorrectiveAction',
     'SELECT * ' + CStudyMetaDataErrors, '');
  AAdd(0,'ReadOnly', 'StudyMetaData', 'ReadOnly',
     'SELECT * ' + CStudyMetaDataErrors, '');
end;

procedure LoadDailyDiversionFieldPropertyUpdateSQLSteps(AAdd: TFieldUpdateSQLStepItemAddFunction);
const OPNAME = 'LoadDailyDiversionFieldPropertyUpdateSQLSteps';
begin
AAdd(0,'Place', 'DailyDiversionStation', 'Place',
     'SELECT *' + CDailyDiversionStation, '');
  AAdd(0,'Latitude', 'DailyDiversionStation', 'Latitude',
     'SELECT *' + CDailyDiversionStation, '');
  AAdd(0,'Longitude', 'DailyDiversionStation', 'Longitude',
     'SELECT *' + CDailyDiversionStation, '');
  AAdd(0,'CatchmentArea', 'DailyDiversionStation', 'CatchmentArea',
     'SELECT *' + CDailyDiversionStation, '');
  AAdd(0,'CatchmentScaleFactor', 'DailyDiversionStation', 'CatchmentScaleFactor',
     'SELECT *' + CDailyDiversionStation, '');

  AAdd(0,'DiversionDate', 'DailyDiversionFileData', 'DiversionDate',
     'SELECT *' + CDailyDiversionFileData, '');
  AAdd(0,'AvgFlow', 'DailyDiversionFileData', 'AvgFlow',
     'SELECT *' + CDailyDiversionFileData, '');
  AAdd(0,'QualityCode', 'DailyDiversionFileData', 'QualityCode',
     'SELECT *' + CDailyDiversionFileData, '');

  AAdd(0,'InstreamDate', 'DailyInstreamFileData', 'InstreamDate',
     'SELECT *' + CDailyInstreamFileData, '');
  AAdd(0,'InstreamAvgFlow', 'DailyInstreamFileData', 'AvgFlow',
     'SELECT *' + CDailyInstreamFileData, '');
  AAdd(0,'InstreamQualityCode', 'DailyInstreamFileData', 'QualityCode',
     'SELECT *' + CDailyInstreamFileData, '');

  AAdd(0,'InstreamQualityCode', 'DailyInstreamFileData', 'QualityCode',
     'SELECT *' + CDailyInstreamFileData, '');
  AAdd(0,'CapacityOfDiversion', 'DailyDiversionCompensationValues', 'CapacityOfDiversion',
     'SELECT *' + CDailyDiversionCompensationValues, '');
  AAdd(0,'InstreamScaleFactor', 'DailyDiversionCompensationValues', 'ScaleFactor',
     'SELECT *' + CDailyDiversionCompensationValues, '');
  AAdd(0,'DailyDiversionStartDate', 'DailyDiversionCompensationValues', 'StartDate',
     'SELECT *' + CDailyDiversionCompensationValues, '');
  AAdd(0,'DailyDiversionEndDate', 'DailyDiversionCompensationValues', 'EndDate',
     'SELECT *' + CDailyDiversionCompensationValues, '');

  AAdd(0,'CompensationValue', 'DailyDiversionCompensationValues', 'FieldNameIdentifier,1=Value%2.2d',
    'SELECT * ' +
    CDailyDiversionCompensationValues, '');

  AAdd(0,'ThresholdValue', 'DailyDiversionMonthlyThreshold', 'FieldNameIdentifier,1=Value%2.2d',
    'SELECT * ' +
    CDailyDiversionThresholdValue, '');

  AAdd(0,'RelationDiversionFlow', 'DailyDiversionFlowRelationship', 'DiversionFlow',
     'SELECT *' + CDailyDiversionFlowRelationship, '');

  AAdd(0,'RelationNonDiversionFlow', 'DailyDiversionFlowRelationship', 'NonDiversionFlow',
     'SELECT *' + CDailyDiversionFlowRelationship, '');

 AAdd(0,'ReferenceFlow', 'DailyDiversionFlowRelationship', 'ReferenceFlow',
     'SELECT *' + CDailyDiversionFlowRelationship, '');

  AAdd(0,'WRYMDiversionFlow', 'DailyDiversionWRYMData', 'DiversionFlow',
     'SELECT *' + CDailyDiversionWRYMData, '');

  AAdd(0,'WRYMNonDiversionFlow', 'DailyDiversionWRYMData', 'NonDiversionFlow',
     'SELECT *' + CDailyDiversionWRYMData, '');

 AAdd(0,'WRYMReferenceFlow', 'DailyDiversionWRYMData', 'ReferenceFlow',
     'SELECT *' + CDailyDiversionWRYMData, '');

 AAdd(0,'DivFlowValueedited', 'DailyDiversionWRYMData', 'DivFlowValueedited',
     'SELECT *' + CDailyDiversionWRYMData, '');

 AAdd(0,'NonDivFlowValueEdited', 'DailyDiversionWRYMData', 'NonDivFlowValueEdited',
     'SELECT *' + CDailyDiversionWRYMData, '');

 AAdd(0,'RefFlowValueEdited', 'DailyDiversionWRYMData', 'RefFlowValueEdited',
     'SELECT *' + CDailyDiversionWRYMData, '');

end;

procedure LoadIrrigationBlockFieldPropertyUpdateSQLSteps(AAdd: TFieldUpdateSQLStepItemAddFunction);
const OPNAME = 'LoadIrrigationBlockFieldPropertyUpdateSQLSteps';
begin


  AAdd(1,'IrrigationBlockUpperZoneReturnFlow',            'IrrigationBlock', 'UpperZoneReturnFlow',           'SELECT * ' + CIrrigationBlock, '');
  AAdd(1,'IrrigationBlockLowerZoneReturnFlow',            'IrrigationBlock', 'LowerZoneReturnFlow',           'SELECT * ' + CIrrigationBlock, '');
  AAdd(1,'IrrigationBlockReturnFlowLoss',                 'IrrigationBlock', 'ReturnFlowLoss',                'SELECT * ' + CIrrigationBlock, '');
  AAdd(1,'IrrigationBlockUpperZoneSoilMoistureCapacity',  'IrrigationBlock', 'UpperZoneSoilMoistureCapacity', 'SELECT * ' + CIrrigationBlock, '');
  AAdd(1,'IrrigationBlockLowerZoneSoilMoistureCapacity',  'IrrigationBlock', 'LowerZoneSoilMoistureCapacity', 'SELECT * ' + CIrrigationBlock, '');
  AAdd(1,'IrrigationBlockUpperZoneSoilMoistureTarget',    'IrrigationBlock', 'UpperZoneSoilMoistureTarget',   'SELECT * ' + CIrrigationBlock, '');
  AAdd(1,'IrrigationBlockInitialSoilMoistureStorage',     'IrrigationBlock', 'InitialSoilMoistureStorage',    'SELECT * ' + CIrrigationBlock, '');

  AAdd(1,'IrrigationBlockBlockNumber'                    ,'IrrigationBlock', 'BlockNumber'            ,'SELECT * '              + CIrrigationBlock, '');
  AAdd(1,'IrrigationBlockName'                           ,'IrrigationBlock', 'BlockName'              ,'SELECT * '                + CIrrigationBlock, '');
  AAdd(1,'IrrigationBlockDescription'                    ,'IrrigationBlock', 'Description'            ,'SELECT * '              + CIrrigationBlock, '');
  AAdd(1,'IrrigationBlockMaxWaterAllocation'             ,'IrrigationBlock', 'MaxWaterAllocation'     ,'SELECT * '       + CIrrigationBlock, '');
  AAdd(1,'IrrigationBlockAllocatedIrrigationArea'        ,'IrrigationBlock', 'AllocatedIrrigationArea','SELECT * '  + CIrrigationBlock, '');
  AAdd(1,'IrrigationBlockCanalTransportLoss'             ,'IrrigationBlock', 'CanalTransportLoss'     ,'SELECT * '       + CIrrigationBlock, '');
  AAdd(1,'IrrigationBlockReturnFlowFactor'               ,'IrrigationBlock', 'ReturnFlowFactor'       ,'SELECT * '         + CIrrigationBlock, '');
  AAdd(1,'IrrigationBlockEfficiencyFactor'               ,'IrrigationBlock', 'EfficiencyFactor'       ,'SELECT * '         + CIrrigationBlock, '');
  AAdd(1,'IrrigationBlockNodeNumber'                     ,'IrrigationBlock', 'NodeNumber'             ,'SELECT * '               + CIrrigationBlock, '');
  AAdd(1,'IrrigationBlockFileName'                       ,'IrrigationBlock', 'FileName'               ,'SELECT * '                 + CIrrigationBlock, '');
  AAdd(1,'IrrigationBlockCatchmentFileName'              ,'IrrigationBlock', 'CatchmentFileName'      ,'SELECT * '        + CIrrigationBlock, '');

  AAdd(1,'IrrigationBlockRainAboveRainFactorSpecValue'   ,'IrrigationBlock', 'RainAboveRainFactorSpecValue' ,'SELECT * ' + CIrrigationBlock, '');
  AAdd(1,'IrrigationBlockRainBelowRainFactor'            ,'IrrigationBlock', 'RainBelowRainFactor'          ,'SELECT * '          + CIrrigationBlock, '');

  AAdd(1,'IrrigationBlockRainCatchmentScalingFactor'     ,'IrrigationBlock', 'RainCatchmentScalingFactor'   ,'SELECT * '   + CIrrigationBlock, '');
  AAdd(1,'IrrigationBlockDroughtApplicable'              ,'IrrigationBlock', 'DroughtApplicable'            ,'SELECT * '            + CIrrigationBlock, '');
  AAdd(1,'IrrigationBlockCropWaterUseType'               ,'IrrigationBlock', 'CropWaterUseType'             ,'SELECT * '             + CIrrigationBlock, '');

  AAdd(0,'IrrigationBlockAPanConvFactor'                 ,'IrrigationBlockAPanConvFactor'    , 'FieldNameIdentifier,1=Factor%2.2d'        ,'SELECT * ' + CIrrigationBlockAPanConvFactor, '');
  AAdd(0,'IrrigationBlockPanEvaporation'                 ,'IrrigationBlockPanEvaporation'    , 'FieldNameIdentifier,1=Evaporation%2.2d'   ,'SELECT * ' + CIrrigationBlockPanEvaporation, '');
  AAdd(0,'IrrigationBlockRainfallFactor'                 ,'IrrigationBlockRainfallFactor'    , 'FieldNameIdentifier,1=Factor%2.2d'        ,'SELECT * ' + CIrrigationBlockRainfallFactor, '');

  AAdd(1,'IrrigationBlockWaterUsageCropName'             ,'IrrigationBlockWaterUsageFactor'  , 'CropName'              ,'SELECT * '        + CIrrigationBlockWaterUsageFactor, '');
  AAdd(1,'IrrigationBlockPercAreaUnderCropType'          ,'IrrigationBlockWaterUsageFactor'  , 'PercAreaUnderCropType' ,'SELECT * ' + CIrrigationBlockWaterUsageFactor, '');
  AAdd(0,'IrrigationBlockWaterUsageFactor'               ,'IrrigationBlockWaterUsageFactor'  , 'FieldNameIdentifier,1=Factor%2.2d'        ,'SELECT * ' + CIrrigationBlockWaterUsageFactor, '');



  AAdd(0,'IrrigationType'                                ,'IrrigationBlockDetails'           , 'IrrigationType'                           ,'SELECT * '             + CIrrigationBlockdDetails, '');
  AAdd(0,'CurtailIrrigationAbstraction'                  ,'IrrigationBlockDetails'           , 'CurtailIrrigationAbstraction'             ,'SELECT * '             + CIrrigationBlockdDetails, '');
  AAdd(0,'CanalSeepageLoss'                              ,'IrrigationBlockDetails'           , 'CanalSeepageLoss'                         ,'SELECT * '             + CIrrigationBlockdDetails, '');
  AAdd(0,'CanalTransmissionLoss'                         ,'IrrigationBlockDetails'           , 'CanalTransmissionLoss'                    ,'SELECT * '             + CIrrigationBlockdDetails, '');
  AAdd(0,'UpperSoilOutflow'                              ,'IrrigationBlockDetails'           , 'UpperSoilOutflow'                         ,'SELECT * '             + CIrrigationBlockdDetails, '');
  AAdd(0,'MaxUpperZoneMoisture'                          ,'IrrigationBlockDetails'           , 'MaxUpperZoneMoisture'                     ,'SELECT * '             + CIrrigationBlockdDetails, '');
  AAdd(0,'MinUpperZoneMoisture'                          ,'IrrigationBlockDetails'           , 'MinUpperZoneMoisture'                     ,'SELECT * '             + CIrrigationBlockdDetails, '');
  AAdd(0,'IrrBlockMultiplicationFactor'                  ,'IrrigationBlockDetails'           , 'MultiplicationFactor'                     ,'SELECT * '             + CIrrigationBlockdDetails, '');

  AAdd(0,'MaxMeanRainfallFactor'                         ,'IrrigationBlockRainfallFactor'    , 'FieldNameIdentifier,1=Factor%2.2d'        ,'SELECT * '             + CIrrigationBlockRainfallFactor, '');

  AAdd(0,'CropTypesCount'                                ,'IrrigationBlockDetails'           , 'CropTypesCount'                           ,'SELECT * '             + CIrrigationBlockdDetails, '');
  AAdd(0,'IrrigationSupplyCapacity'                      ,'IrrigationBlockDetails'           , 'IrrigationSupplyCapacity'                 ,'SELECT * '             + CIrrigationBlockdDetails, '');

  AAdd(0,'MethodIrrigatedAreas'                          ,'IrrigationBlockDetails'           , 'MethodIrrigatedAreas'                     ,'SELECT * '             + CIrrigationBlockdDetails, '');

  AAdd(0,'BreakpointYearsDefined'                        ,'IrrigationBlockDetails'           , 'IABreakpointYear'                         ,'SELECT * '             + CIrrigationBlockdDetails, '');
  AAdd(0,'BreakpointArea'                                ,'IrrigationBlockDetails'           , 'IABreakpointArea'                         ,'SELECT * '             + CIrrigationBlockdDetails, '');

  AAdd(0,'MaxWaterAllocationCount'                      ,'IrrigationBlockDetails'           , 'MaxWaterAllocationCount'                     ,'SELECT * '             + CIrrigationBlockdDetails, '');
  AAdd(0,'MethodMaxWaterAllocation'                     ,'IrrigationBlockDetails'           , 'MethodMaxWaterAllocation'                    ,'SELECT * '             + CIrrigationBlockdDetails, '');
  AAdd(0,'ReturnFlowVolumePointsCount'                  ,'IrrigationBlockDetails'           , 'ReturnFlowVolumePointsCount'                 ,'SELECT * '             + CIrrigationBlockdDetails, '');
  AAdd(0,'MethodReturnFlowVolume'                       ,'IrrigationBlockDetails'           , 'MethodReturnFlowVolume'                      ,'SELECT * '             + CIrrigationBlockdDetails, '');
  AAdd(0,'MethodSupplyCapacity'                         ,'IrrigationBlockDetails'           , 'MethodSupplyCapacity'                        ,'SELECT * '             + CIrrigationBlockdDetails, '');

  AAdd(0,'SupplyCapacityPointsCount'                    ,'IrrigationBlockDetails'           , 'SupplyCapacityPointsCount'                   ,'SELECT * '             + CIrrigationBlockdDetails, '');

  AAdd(0,'IrrigationEfficienciesPointsCount'            ,'IrrigationBlockDetails'           , 'IrrigationEfficienciesPointsCount'           ,'SELECT * '             + CIrrigationBlockdDetails, '');
  AAdd(0,'MethodIrrigationEfficiencies'                 ,'IrrigationBlockDetails'           , 'MethodIrrigationEfficiencies'                ,'SELECT * '             + CIrrigationBlockdDetails, '');
  AAdd(0,'ReturnFlowFactorsCount'                       ,'IrrigationBlockDetails'           , 'ReturnFlowFactorsCount'                      ,'SELECT * '             + CIrrigationBlockdDetails, '');
  AAdd(0,'MethodReturnFlowFactors'                      ,'IrrigationBlockDetails'           , 'MethodReturnFlowFactors'                     ,'SELECT * '             + CIrrigationBlockdDetails, '');

  AAdd(0,'BreakpointYearsMaxWaterAllocation'            ,'IrrigationBlockDetails'           , 'MWABreakpointYear'                           ,'SELECT * '             + CIrrigationBlockdDetails, '');
  AAdd(0,'BreakpointMaxWaterAllocation'                 ,'IrrigationBlockDetails'           , 'MWAWaterAllocation'                          ,'SELECT * '             + CIrrigationBlockdDetails, '');
  AAdd(0,'BreakpointMaxWaterAllocationGrowth'           ,'IrrigationBlockDetails'           , 'MWAWaterAllocationGrowth'                    ,'SELECT * '             + CIrrigationBlockdDetails, '');


  AAdd(0,'BreakpointYearsReturnFlowVolume'              ,'IrrigationBlockDetails'           , 'RFVBreakpointYear'                           ,'SELECT * '             + CIrrigationBlockdDetails, '');

  AAdd(0,'AllocatedAreaPointsCount'                     ,'IrrigationBlockDetails'           , 'AllocatedAreaPointsCount'                    ,'SELECT * '             + CIrrigationBlockdDetails, '');


  AAdd(0,'BreakpointReturnFlowVolume'                  ,'IrrigationBlockDetails'            , 'RFVFlowVolume'                               ,'SELECT * '             + CIrrigationBlockdDetails, '');


  AAdd(0,'BreakpointYearsSupplyCapacity'                ,'IrrigationBlockDetails'           , 'SCBreakpointYear'                            ,'SELECT * '             + CIrrigationBlockdDetails, '');
  AAdd(0,'BreakpointSupplyCapacity'                     ,'IrrigationBlockDetails'           , 'SCSupplyCapacity'                            ,'SELECT * '             + CIrrigationBlockdDetails, '');
  AAdd(0,'BreakpointYearsIrrigationEfficiencies'        ,'IrrigationBlockDetails'           , 'IEBreakpointYear'                            ,'SELECT * '             + CIrrigationBlockdDetails, '');
  AAdd(0,'BreakpointIrrigationEfficiencies'             ,'IrrigationBlockDetails'           , 'IEIrrigationEfficiency'                      ,'SELECT * '             + CIrrigationBlockdDetails, '');
  AAdd(0,'BreakpointYearsReturnFlowFactor'              ,'IrrigationBlockDetails'           , 'RFFBreakpointYear'                           ,'SELECT * '             + CIrrigationBlockdDetails, '');
  AAdd(0,'BreakpointReturnFlowFactor'                   ,'IrrigationBlockDetails'           , 'RFFReturnFlowFactor'                         ,'SELECT * '             + CIrrigationBlockdDetails, '');


end;


procedure LoadWetlandFieldPropertyUpdateSQLSteps(AAdd: TFieldUpdateSQLStepItemAddFunction);
const OPNAME = 'LoadWetlandFieldPropertyUpdateSQLSteps';
begin
  AAdd(1,'WetlandNodeNumber'  ,'Wetland', 'NodeNumber'        ,'SELECT * '         + CWetland, '');
  AAdd(1,'WetlandName'        ,'Wetland', 'WetlandName'       ,'SELECT * '        + CWetland, '');
  AAdd(1,'UpstreamThreshold'  ,'Wetland', 'UpstreamThreshold' ,'SELECT * '  + CWetland, '');
  AAdd(1,'InflowProportion'   ,'Wetland', 'InflowProportion'  ,'SELECT * '   + CWetland, '');
  AAdd(1,'StorageVolume'      ,'Wetland', 'Storage'           ,'SELECT * '            + CWetland, '');
  AAdd(1,'OutflowProportion'  ,'Wetland', 'OutflowProportion' ,'SELECT * '  + CWetland, '');
end;

procedure LoadYMDemandCentreFieldPropertyUpdateSQLSteps(AAdd: TFieldUpdateSQLStepItemAddFunction);
const OPNAME = 'LoadYMDemandCentreFieldPropertyUpdateSQLSteps';
begin
  AAdd(1,'YMDemandCentreNodeNumber'   ,'YMDemandCentre'                   , 'NodeNumber'            ,'SELECT * '             + CYMDemandCentre, '');
  AAdd(1,'YMDemandCentreName'         ,'YMDemandCentre'                   , 'CentreName'            ,'SELECT * '             + CYMDemandCentre, '');
  AAdd(1,'YMDemandCentreDescription'  ,'YMDemandCentre'                   , 'CentreDescription'     ,'SELECT * '      + CYMDemandCentre, '');
  AAdd(1,'NodeRefNr'                  ,'YMDemandCentre'                   , 'NodeRefNr'             ,'SELECT * '              + CYMDemandCentre, '');
  AAdd(1,'AveReturnFlowFactor'        ,'YMDemandCentre'                   , 'AveReturnFlowFactor'   ,'SELECT * '    + CYMDemandCentre, '');
  AAdd(1,'AveEvaporation'             ,'YMDemandCentre'                   , 'AveEvaporation'        ,'SELECT * '         + CYMDemandCentre, '');
  AAdd(1,'StdDeviationFactor'         ,'YMDemandCentre'                   , 'StdDeviationFactor'    ,'SELECT * '     + CYMDemandCentre, '');
  AAdd(1,'YMDemandCentreRoutingConstant','YMDemandCentre'                 , 'RoutingConstant'       ,'SELECT * '        + CYMDemandCentre, '');
  AAdd(1,'RainfallScalingFactor'      ,'YMDemandCentre'                   , 'RainfallScalingFactor' ,'SELECT * '  + CYMDemandCentre, '');
  AAdd(1,'TotalFlowLost'              ,'YMDemandCentre'                   , 'TotalFlowLost'         ,'SELECT * '          + CYMDemandCentre, '');
  AAdd(0,'EvapoTranspiration'         ,'YMDemandCentre'                   , 'FieldNameIdentifier,1=EvapoTranspiration%2.2d' ,'SELECT * '  + CYMDemandCentre, '');
  AAdd(1,'DemandCentreConsumptiveChannelNumber'       ,'YMDemandCentre'   , 'ConsumptiveChannelNr'  ,'SELECT * '   + CYMDemandCentre, '');
  AAdd(1,'DemandCentreReclaimationChannelNumber'      ,'YMDemandCentre'   , 'ReclaimationChannelNr' ,'SELECT * '  + CYMDemandCentre, '');
end;

procedure LoadSFRFieldPropertyUpdateSQLSteps(AAdd: TFieldUpdateSQLStepItemAddFunction);
const OPNAME = 'LoadSFRFieldPropertyUpdateSQLSteps';
begin
  AAdd(1,'InflowNodeNumber'     ,'SFRSubCatchment', 'InflowNodeNumber'     ,'SELECT * '     + CSFRSubCatchment, '');
  AAdd(1,'CoveredArea'          ,'SFRSubCatchment', 'CoveredArea'          ,'SELECT * '          + CSFRSubCatchment, '');
  AAdd(1,'UnitRunoffFileName'   ,'SFRSubCatchment', 'UnitRunoffFileName'   ,'SELECT * '   + CSFRSubCatchment, '');
  AAdd(1,'SoilMoistureFileName' ,'SFRSubCatchment', 'SoilMoistureFileName' ,'SELECT * ' + CSFRSubCatchment, '');
  AAdd(1,'SFRName'              ,'SFRSubCatchment', 'SFRName'              ,'SELECT * '              + CSFRSubCatchment, '');
  AAdd(1,'SFRDescr'             ,'SFRSubCatchment', 'SFRDescr'             ,'SELECT * '             + CSFRSubCatchment, '');
end;

procedure LoadYMDemandCentreReturnFlowFeatureFieldPropertyUpdateSQLSteps(AAdd: TFieldUpdateSQLStepItemAddFunction);
const OPNAME = 'LoadYMDemandCentreReturnFlowFeatureFieldPropertyUpdateSQLSteps';
begin
//  AAdd(1,'YMDCReturnFlowFeatureName'  ,'YMDemandCentreReturnFlowChannel'  , 'FeatureName'           ,'SELECT * '             + CYMDemandCentre, '');
  AAdd(1,'ReturnFlowChannelNr'        ,'YMDemandCentreReturnFlowChannel'  , 'ChannelNr'             ,'SELECT * '              + CYMDemandCentreReturnFlowChannel, '');
  AAdd(1,'TotalReturnFlow'            ,'YMDemandCentreReturnFlowChannel'  , 'TotalReturnFlow'       ,'SELECT * '        + CYMDemandCentreReturnFlowChannel, '');
  AAdd(1,'FlowDiversion'              ,'YMDemandCentreReturnFlowChannel'  , 'FlowDiversion'         ,'SELECT * '          + CYMDemandCentreReturnFlowChannel, '');
end;

procedure LoadParamStochasticsFieldPropertyUpdateSQLSteps(AAdd: TFieldUpdateSQLStepItemAddFunction);
const OPNAME = 'LoadParamStochasticsFieldPropertyUpdateSQLSteps';
begin
  AAdd(1,'ArmaPhi1'                ,'ParamStochastics'  , 'ArmaPhi1'            ,'SELECT *'        + CParamStochastics, '');
  AAdd(1,'ArmaPhi2'                ,'ParamStochastics'  , 'ArmaPhi2'            ,'SELECT *'        + CParamStochastics, '');
  AAdd(1,'ArmaTheta1'              ,'ParamStochastics'  , 'ArmaTheta1'          ,'SELECT *'      + CParamStochastics, '');
  AAdd(1,'ArmaTheta2'              ,'ParamStochastics'  , 'ArmaTheta2'          ,'SELECT *'      + CParamStochastics, '');
  AAdd(1,'CatchmentAreaParam'      ,'ParamStochastics'  , 'CatchmentArea'       ,'SELECT *'   + CParamStochastics, '');
  AAdd(1,'GaugeName'               ,'ParamStochastics'  , 'GaugePathName'       ,'SELECT *'   + CParamStochastics, '');
  AAdd(1,'NumberOfYearsParam'      ,'ParamStochastics'  , 'YearsNumber'         ,'SELECT *'     + CParamStochastics, '');
  AAdd(1,'ParamAIC'                ,'ParamStochastics'  , 'ParamAIC'            ,'SELECT *'        + CParamStochastics, '');
  AAdd(1,'ParamANC'                ,'ParamStochastics'  , 'ParamANC'            ,'SELECT *'        + CParamStochastics, '');
  AAdd(1,'ParamXa'                 ,'ParamStochastics'  , 'ParamXA'             ,'SELECT *'         + CParamStochastics, '');
  AAdd(1,'ParamXSD'                ,'ParamStochastics'  , 'ParamXSD'            ,'SELECT *'        + CParamStochastics, '');
  AAdd(1,'PhiZero'                 ,'ParamStochastics'  , 'PhiZero'             ,'SELECT *'         + CParamStochastics, '');
  AAdd(1,'Residual1'               ,'ParamStochastics'  , 'Residual1'           ,'SELECT *'       + CParamStochastics, '');
  AAdd(1,'Residual2'               ,'ParamStochastics'  , 'Residual2'           ,'SELECT *'       + CParamStochastics, '');
  AAdd(1,'ResidualMean'            ,'ParamStochastics'  , 'ResidualMean'        ,'SELECT *'    + CParamStochastics, '');
  AAdd(1,'ResidualStdDev'          ,'ParamStochastics'  , 'ResidualStdDev'      ,'SELECT *'  + CParamStochastics, '');
  AAdd(1,'StartYearParam'          ,'ParamStochastics'  , 'YearStart'           ,'SELECT *'       + CParamStochastics, '');
  AAdd(1,'TransformDelta'          ,'ParamStochastics'  , 'TransformDelta'      ,'SELECT *'  + CParamStochastics, '');
  AAdd(1,'TransformGamma'          ,'ParamStochastics'  , 'TransformGamma'      ,'SELECT *'  + CParamStochastics, '');
  AAdd(1,'TransformType'           ,'ParamStochastics'  , 'TransformType'       ,'SELECT *'   + CParamStochastics, '');
  AAdd(1,'TransformXi'             ,'ParamStochastics'  , 'TransformXi'         ,'SELECT *'     + CParamStochastics, '');
  AAdd(1,'TransformXlam'           ,'ParamStochastics'  , 'TransformXlam'       ,'SELECT *'   + CParamStochastics, '');
  AAdd(1,'Variate1'                ,'ParamStochastics'  , 'Variate1'            ,'SELECT *'        + CParamStochastics, '');
  AAdd(1,'Variate2'                ,'ParamStochastics'  , 'Variate2'            ,'SELECT *'        + CParamStochastics, '');
  AAdd(1,'ZTVariates'              ,'ParamStochastics'  , 'ZTVariates'          ,'SELECT *'      + CParamStochastics, '');
end;

end.



