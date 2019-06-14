//
//  UNIT      : Contains table property data.
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/01/23
//  COPYRIGHT : Copyright © 2003 DWAF
//
unit UTableData;

interface

type TTablePropertyItemAddFunction = procedure (AFieldName, APrimaryKeys: string) of object;

procedure LoadTablePropertyData(AAdd: TTablePropertyItemAddFunction);

implementation

procedure LoadTablePropertyData(AAdd: TTablePropertyItemAddFunction);
const OPNAME = 'LoadTablePropertyData';
begin
  AAdd('AnlySequences',                     'Model,StudyAreaName,SubArea,Scenario');

  AAdd('ChannelArea',                       'Model,StudyAreaName,SubArea,Scenario,AreaID');
  AAdd('ChannelArcPenalty',                 'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('ChannelComments',                   'Model,StudyAreaName,SubArea,Scenario');
  AAdd('ChannelConfig',                     'Model,StudyAreaName,SubArea,Scenario');
  AAdd('ChannelDetails',                    'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('ChannelTypes',                      'ChannelType');
  AAdd('ChangeGroup',                       'GroupID');
  AAdd('ChangeGroupElement',                'GroupID,ElementID,IsElementGroup');
  AAdd('ChangeList',                        'ChangeListKey,ChangeListID');
  AAdd('ChangeParameter',                   'ChangeListID,ParamField,KeyValues,FieldIndex');
  AAdd('ChannelSwitchControl',              'Model,StudyAreaName,SubArea,Scenario,ChannelSwitchID');
  AAdd('ChannelTimeControl',                'Model,StudyAreaName,SubArea,Scenario,Identifier,ChannelNumber,ChannelType');
  AAdd('Curtailment',                       'Model,StudyAreaName,SubArea,Scenario');
  AAdd('CurtailmentChannel',                'Model,StudyAreaName,SubArea,Scenario,Identifier');

  AAdd('DaysPerMonth',                      'Model,StudyAreaName,SubArea,Scenario');
  AAdd('DecisionDate',                      'Model,StudyAreaName,SubArea,Scenario');
  AAdd('DemandCentre',                      'Model,StudyAreaName,SubArea,Scenario,DemandCentreID');
  AAdd('DemandFileData',                    'Model,StudyAreaName,SubArea,Scenario,Identifier,FileType,FileNumber');
  AAdd('DisbenefitFunction',                'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('DiversionFeatures',                 'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('DiversionFeaturesType1n2',          'Model,StudyAreaName,SubArea,Scenario,Identifier,DiversionCode');
  AAdd('DiversionFeaturesType3',            'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('DiversionFeaturesType3Proportions', 'Model,StudyAreaName,SubArea,Scenario,Identifier,DiversionIndex');

  AAdd('DailyDiversionFlowRelationship',    'Model,StudyAreaName,SubArea,Scenario,Identifier,StationID');
  AAdd('DailyDiversionWRYMData',            'Model,StudyAreaName,SubArea,Scenario,Identifier,StationID');
  AAdd('DailyDiversionFileData',            'Model,StudyAreaName,SubArea,Scenario,Identifier,StationID');
  AAdd('DailyInstreamFileData',             'Model,StudyAreaName,SubArea,Scenario,Identifier,StationID');
  AAdd('DailyDiversionStation',             'Model,StudyAreaName,SubArea,Scenario,StationID');
  AAdd('DailyDiversionCompensationValues',  'Model,StudyAreaName,SubArea,Scenario,StationID');
  AAdd('DailyDiversionMonthlyThreshold',     'Model,StudyAreaName,SubArea,Scenario,StationID');
  AAdd('DroughtRestriction'                 ,'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('DroughtRestrictionFactors'          ,'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('DroughtRestrictionStorageVolumes'   ,'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('DDTSDetails',                       'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('DDTSInputMinMax',                   'Model,StudyAreaName,SubArea,Scenario,Identifier');

  AAdd('FileCreate',                        'Model,StudyAreaName,SubArea,Scenario,ImplementFile');
  AAdd('FileGroup',                         'FileGroup');
  AAdd('FileNames',                         'Model,StudyAreaName,SubArea,Scenario,Identifier,FileGroup');
  AAdd('FileTypes',                         'FileType');
  AAdd('FlowConstraints',                   'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('FlowConstraintsValue',              'Model,StudyAreaName,SubArea,Scenario,Identifier,GroupNumber,SubGroupNumber,LineNumber');
  {AAdd('FlowConstraintsDeltaH',             'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('FlowConstraintsDischarge',          'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('FlowConstraintsDiversion',          'Model,StudyAreaName,SubArea,Scenario,Identifier,SubIdentifier');
  AAdd('FlowConstraintsElevation',          'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('FlowConstraintsType11Depth',        'Model,StudyAreaName,SubArea,Scenario,Identifier,LineNumber');
  AAdd('FlowConstraintsType11Flow',         'Model,StudyAreaName,SubArea,Scenario,Identifier,LineNumber');
  }
  AAdd('FMAllocationDefinition',            'Model,StudyAreaName,SubArea,Scenario,AllocDefID');
  AAdd('FMAllocationLevel',                 'Model,StudyAreaName,SubArea,Scenario,AllocDefID,AllocLevelID');
  AAdd('FMCoefficients',                    'Model,StudyAreaName,SubArea,Scenario,AllocDefID,SubSystemID,StartStorageNr,CurveSetNr,LoadCaseNr');
  AAdd('FMDemandDefinition',                'Model,StudyAreaName,SubArea,Scenario,AllocDefID,DemandDefID');
  AAdd('FMSolveFixedPosition',              'Model,StudyAreaName,SubArea,Scenario,AllocDefID,FixedPositionID');
  AAdd('FMSolveSpecificOrder',              'Model,StudyAreaName,SubArea,Scenario,AllocDefID,SpecificOrderID');
  AAdd('FMSubSystem',                       'Model,StudyAreaName,SubArea,Scenario,AllocDefID,SubSystemID');
  AAdd('FMSupportChannel',                  'Model,StudyAreaName,SubArea,Scenario,AllocDefID,ChannelNumber');
  AAdd('FMSupportSubSystem',                'Model,StudyAreaName,SubArea,Scenario,AllocDefID,DemandDefID,SupportSubSystemID');
  AAdd('FMUserCategory',                    'Model,StudyAreaName,SubArea,Scenario,AllocDefID,UserCategoryID');

  AAdd('ReservoirSwitchDefinition',         'Model,StudyAreaName,SubArea,Scenario,FileGroupID,SwitchDefID');


  AAdd('GrowthFactorConfig',                 'Model,StudyAreaName,SubArea,Scenario');
  AAdd('GrowthFactorDemand',                 'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('GrowthFactorMinMax',                 'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('GrowthFactorHydrology',              'Model,StudyAreaName,SubArea,Scenario,Identifier');

  AAdd('GrowthFactorExcelConfig',           'Model,StudyAreaName,SubArea,Scenario');
  AAdd('GrowthFactorExcelDemand',           'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('GrowthFactorExcelMinMax',           'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('GrowthFactorExcelHydrology',        'Model,StudyAreaName,SubArea,Scenario,Identifier');

  AAdd('GroundWater',                       'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('GroundWaterEvaporation',            'Model,StudyAreaName,SubArea,Scenario,GroundWaterIdentifier');
  AAdd('GroundWaterPitman',                 'Model,StudyAreaName,SubArea,Scenario,GroundWaterIdentifier');
  AAdd('GroundWaterSubCatchment',           'Model,StudyAreaName,SubArea,Scenario,GroundWaterIdentifier');
  AAdd('GroundWaterUsageFactor',            'Model,StudyAreaName,SubArea,Scenario,GroundWaterIdentifier');

  AAdd('HydrologyDataType',                 'DataType');
  AAdd('HydrologyDetails',                  'Model,StudyAreaName,SubArea,Scenario');
  AAdd('HydrologyFileData',                 'StudyAreaName,FileName,Identifier');

  AAdd('IFRSite',                           'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('IFRSiteFlows',                      'Model,StudyAreaName,SubArea,Scenario,Identifier,FlowIdentifier');
  AAdd('IFRFeatures',                       'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('IFRFeaturesDetails',                'Model,StudyAreaName,SubArea,Scenario,Identifier,LineNumber');
  AAdd('IFRReference',                      'Model,StudyAreaName,SubArea,Scenario');
  AAdd('IrrigationAreas',                   'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('IrrigationAreasDiversionFlow',      'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('IrrigationAreasReturnFlow',         'Model,StudyAreaName,SubArea,Scenario,Identifier');

  AAdd('IrrigationBlock',                   'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('IrrigationBlockDetails',            'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('IrrigationBlockAPanConvFactor',     'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('IrrigationBlockPanEvaporation',     'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('IrrigationBlockRainfallFactor',     'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('IrrigationBlockWaterUsageFactor',   'Model,StudyAreaName,SubArea,Scenario,BlockIdentifier,Identifier');

  AAdd('LossFeature',                       'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('LossFeatureValue',                  'Model,StudyAreaName,SubArea,Scenario,Identifier,SubIdentifier'); {Riana}

  AAdd('MasterControlFeature',              'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('MaxYield',                          'Model,StudyAreaName,SubArea,Scenario');
  AAdd('MetaDataItem',                      'MetaDataListID,ParamField,KeyValues,FieldIndex');
  AAdd('MetaDataList',                      'MetaDataListKey,MetaDataListID');
  AAdd('MinFlowChannel',                    'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('MinFlowChannelValue',               'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('MinFlowChannel',                    'Model,StudyAreaName,SubArea,Scenario,Identifier');

  AAdd('Mine',                              'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('MineLakeEvaporation',               'Model,StudyAreaName,SubArea,Scenario,MineIdentifier');
  AAdd('MinePanEvaporation',                'Model,StudyAreaName,SubArea,Scenario,MineIdentifier');
  AAdd('MineOpenCast',                      'Model,StudyAreaName,SubArea,Scenario,MineIdentifier,Identifier');
  AAdd('MineRechargeFactor',                'Model,StudyAreaName,SubArea,Scenario,MineIdentifier,RechargeFactorParentType,ParentIdentifier,RechargeFactorType');
  AAdd('MineSlurryDump',                    'Model,StudyAreaName,SubArea,Scenario,MineIdentifier,Identifier');
  AAdd('MineUGUpstreamRunoff',              'Model,StudyAreaName,SubArea,Scenario,MineIdentifier,UGIdentifier');
  AAdd('MineUnderGround',                   'Model,StudyAreaName,SubArea,Scenario,MineIdentifier,Identifier');
  AAdd('MineSubCatchment',                  'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('MineSubCatchmentFlowVolume',        'Model,StudyAreaName,SubArea,Scenario,MineSubCatchmentIdentifier');
  AAdd('MinMaxChannel',                     'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('MinMaxChannelFlow',                 'Model,StudyAreaName,SubArea,Scenario,Identifier,SubIdentifier');
  AAdd('MinMaxChannelDistribution',         'Model,StudyAreaName,SubArea,Scenario,Identifier,SubIdentifier');
  AAdd('MinMaxBoundChannel',                'Model,StudyAreaName,SubArea,Scenario,Identifier,ChannelNumber');
  AAdd('MinMaxWQConstrain',                 'Model,StudyAreaName,SubArea,Scenario,Identifier');


  AAdd('MonthNames',                        'Model,StudyAreaName,SubArea,Scenario');
  AAdd('NodesDetails',                      'Model,StudyAreaName,SubArea,Scenario,Identifier');

  AAdd('ParamHeader',                       'Model,StudyAreaName,SubArea,Scenario');
  AAdd('ParamMatrix',                       'Model,StudyAreaName,SubArea,Scenario,Identifier,MatrixType');

  AAdd('ParamMatrixComm',                   'Model,StudyAreaName,SubArea,Scenario');
  AAdd('ParamMatrixType',                   'MatrixType');
  AAdd('ParamStochastics',                  'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('PowerPlants',                       'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('PowerPlantsDemands',                'Model,StudyAreaName,SubArea,Scenario,Identifier,PowerCode');  {Riana}
  AAdd('PowerPlantsDetails',                'Model,StudyAreaName,SubArea,Scenario,Identifier,FactorCode'); {Riana}
  AAdd('PumpingFeature',                    'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('RainfallCatchment',                 'Model,StudyAreaName,SubArea,Scenario');
  AAdd('RainfallCatchmentSource',           'Model,StudyAreaName,SubArea,Scenario,CatchmentID,SourcePatchID,StationID');
  AAdd('RainfallDailyData',                 'StationID');

  AAdd('RainfallMonthlyPatchData',          'PatchID,StationID,Year');
  AAdd('RainfallMonthlyRAWData',            'StationID,Year');
  AAdd('RainfallMonthlyWRCData',            'PatchID,StationID,Year');
  AAdd('RainfallPatchR',                    'StudyAreaName,SubArea,PatchID');
  AAdd('RainfallPatchSource',               'PatchID,SourceStationID,SourcePatchID');
  AAdd('RainfallPatchType',                 'PatchTypeID');
  AAdd('RainfallPatchWRC',                  'PatchID');
  AAdd('RainfallProjectGauges',             'Model,StudyAreaName,SubArea,Scenario,StationID');
  AAdd('RainfallRAWFlags',                  'RAWFlags');
  AAdd('RainfallRAWSplits',                 'StationID,HydroStartYear,HydroEndYear');
  AAdd('RainfallStations',                  'StationID');
  AAdd('RainfallUserMonthlyData',           'StationID,Year');
  AAdd('RainfallUserStations',              'StationID');

  AAdd('Reservoir',                         'Model,StudyAreaName,SubArea,Scenario');
  AAdd('ReservoirArea',                     'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('ReservoirChannels',                 'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('ReservoirDetails',                  'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('ReservoirElevation',                'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('ReservoirEvap',                     'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('ReservoirHydrologyFile',            'Model,StudyAreaName,SubArea,Scenario,Identifier,CatchmentRef');
  AAdd('ReservoirInitialLevels',            'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('ReservoirLevels',                   'Model,StudyAreaName,SubArea,Scenario,RecordIdentifier');
  AAdd('ReservoirTimeControl',              'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('ReservoirVolume',                   'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('ReservoirGroup',                    'Model,StudyAreaName,SubArea,Scenario,GroupID');
//  AAdd('ReservoirZonePenalty',              'Model,StudyAreaName,SubArea,Scenario,Identifier');


  AAdd('ReturnFlowChannelDetail',           'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('ReturnFlowCorrespondingChannel',    'Model,StudyAreaName,SubArea,Scenario,Identifier,DemandChannel,ChannelNumber');
  AAdd('ReturnFlowMonthlyEvaporation',      'Model,StudyAreaName,SubArea,Scenario,Identifier,DemandChannel');
  AAdd('RunParameters',                     'Model,StudyAreaName,SubArea,Scenario');
  AAdd('RunTitle',                          'Model,StudyAreaName,SubArea,Scenario');

  AAdd('SFRSubCatchment',                   'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('SpecifiedDemandFeature',            'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('SpecifiedInflowFeature',            'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('StorageZones',                      'Model,StudyAreaName,SubArea,Scenario');
  AAdd('StorageZoneDetails',                'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('StudyArea',                         'Model,StudyAreaName');
  AAdd('StudyDocuments',                    'StudyAreaName,Category,Identifier');
  AAdd('StudyMetaData',                     'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('StudyModel',                        'Model');
  AAdd('StudyScenario',                     'Model,StudyAreaName,SubArea,Scenario');
  AAdd('StudyScenarioDocuments',            '');
  AAdd('StudySubArea',                      'Model,StudyAreaName,SubArea');
  AAdd('ChannelSwitchDefinition',           'Model,StudyAreaName,SubArea,Scenario,SwitchDefID');
  AAdd('suBlockAnualAverageInflowValues',   'Model,StudyAreaName,SubArea,Scenario,BlockNumber,RunNumber,Identifier');
  AAdd('suBlockAnualSummaryValues',         'Model,StudyAreaName,SubArea,Scenario,BlockNumber,RunNumber,Identifier');
  AAdd('suBlockAvarageValues',              'Model,StudyAreaName,SubArea,Scenario,BlockNumber,RunNumber');
  AAdd('suBlockCriticalPeriodsValues',      'Model,StudyAreaName,SubArea,Scenario,BlockNumber,RunNumber,Identifier');
  AAdd('suBlockDescription',                'Model,StudyAreaName,SubArea,Scenario,BlockNumber,RunNumber');
  AAdd('suBlockGenericValues',              'Model,StudyAreaName,SubArea,Scenario,BlockNumber,RunNumber,Identifier');
  AAdd('suBlockHeader',                     'Model,StudyAreaName,SubArea,Scenario,BlockNumber,RunNumber,Identifier');
  AAdd('suBlockOutputSummaryValues',        'Model,StudyAreaName,SubArea,Scenario,BlockNumber,RunNumber,Identifier');
  AAdd('suBlockSequencesWithFailuresValues','Model,StudyAreaName,SubArea,Scenario,BlockNumber,RunNumber,Identifier');
  AAdd('suBlockTypes',                      'BlockType');
  AAdd('suUnknownData',                     'Model,StudyAreaName,SubArea,Scenario,FileNumber,LineNumber,LineSection');

  AAdd('TargetPower',                       'Model,StudyAreaName,SubArea,Scenario');
  AAdd('TargetYield',                       'Model,StudyAreaName,SubArea,Scenario');
  AAdd('TariffCalculation',                 'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('TSCChart',                          'Model,StudyAreaName,SubArea,ChartName');
  AAdd('TSCChartSeries',                    'Model,StudyAreaName,SubArea,ChartName,SeriesName');
  AAdd('TSCSeries',                         'Model,StudyAreaName,SubArea,Scenario,ChartName,SeriesName,ViewID,ParentID,TopParentID');
  AAdd('TSCView',                           'Model,StudyAreaName,SubArea,ViewName');
  AAdd('TSCViewChart',                      'Model,StudyAreaName,SubArea,ViewName,ChartName');
  AAdd('TSCViewScenario',                   'Model,StudyAreaName,SubArea,Scenario,ViewName');

  AAdd('UserRights',                        'UserRights');
  AAdd('User',                              'UserID');
  AAdd('ViewDataJump',                      'ViewID,JumpToID');
  AAdd('ViewDataNode',                      'Model,ViewID');
  AAdd('ViewDataSet',                       'ViewID');

  AAdd('WaterDemandCategories',             'Model,StudyAreaName,SubArea,Scenario,CategoryID');
  AAdd('WaterDemandCounts',                 'Model,StudyAreaName,SubArea,Scenario');
  AAdd('WaterDemandFeatures',               'Model,StudyAreaName,SubArea,Scenario,FeatureID');
  AAdd('WaterDemandRiskCriteria',           'Model,StudyAreaName,SubArea,Scenario');
  AAdd('WaterUseCategories',                'Model,StudyAreaName,SubArea,Scenario,CategoryID');
  AAdd('WaterUseCounts',                    'Model,StudyAreaName,SubArea,Scenario');
  AAdd('WaterUseProportion',                'Model,StudyAreaName,SubArea,Scenario,ChannelID');
  AAdd('Wetland',                           'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('WRYMDat',                           'Model,StudyAreaName,SubArea,Scenario');
  AAdd('WRYMFileLines',                     'Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection');

  AAdd('YMDemandCentre',                    'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('YMDemandCentreReturnFlowChannel',   'Model,StudyAreaName,SubArea,Scenario,DemandCentreID,Identifier');

  AAdd('YRCChart',                          'Model,StudyAreaName,SubArea,Scenario,ChartID');
  AAdd('YRCChartProperty',                  'Model,StudyAreaName,SubArea,Scenario,ChartID');
  AAdd('YRCChartViewType',                  'ChartViewType');
  AAdd('YRCCLanguageStrings',               'Model,StudyAreaName,SubArea,Scenario,ChartID');
  AAdd('YRCPlane',                          'Model,StudyAreaName,SubArea,Scenario,ChartID,PlaneNumber');
  AAdd('YRCTargetDraft',                    'Model,StudyAreaName,SubArea,Scenario,ChartID,PlaneNumber,TargetDraftID');
  AAdd('YRCTargetDraftConst',               'Model,StudyAreaName,SubArea,Scenario,ChartID,PlaneNumber,TargetDraftID,CurveType');
  AAdd('YRCTargetDraftPoint',               'Model,StudyAreaName,SubArea,Scenario,ChartID,PlaneNumber,TargetDraftID,CurveType,PointIndex');


  AAdd('MultiResChannelCurtail', 'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('MultiResChannelElevation', 'Model,StudyAreaName,SubArea,Scenario,Identifier');
  AAdd('MultiResChannelFactor', 'Model,StudyAreaName,SubArea,Identifier');

  AAdd('MineLoadGenerationFlow', 'Model,StudyAreaName,SubArea,Scenario,MineIdentifier,Identifier');
  AAdd('MineLoadGenerationMeanOfSalt', 'Model,StudyAreaName,SubArea,Scenario,MineIdentifier,Identifier');
  AAdd('MineGrowthFactors', 'Model,StudyAreaName,SubArea,Scenario,MineIdentifier,Identifier');
end;

end.
