//
//
//  UNIT      : Contains TContextValidationType enumeration.
//  AUTHOR    : Dziedzi Ramulondi (CornaStone)
//  DATE      : 2004/01/06
//  COPYRIGHT : Copyright © 2004 DWAF
//
//
unit UYieldContextValidationType;

interface

uses
  SysUtils;

type
  TDialogValidationType = integer;

  const
    dvtConfigurationAll                        = 0;
    dvtStartYearO                              = 1;
    dvtStartYearG                              = 2;
    dvtNumberOfYears                           = 3;
    dvtNumberOfPeriods                         = 4;
    dvtStartMonthNo                            = 5;
    dvtMonthNames                              = 6;
    dvtMonthsDays                              = 7;
    dvtRunType                                 = 8;
    dvtMultiplePeriods                         = 9;
    dvtReduceSequences                         = 10;
    dvtNumberOfSequences                       = 11;
    dvtSequenceInAnalysis                      = 12;
    dvtDebugStartPeriod                        = 13;
    dvtDebugEndPeriod                          = 14;
    dvtDebugLevel                              = 15;
    dvtSummaryLevel                            = 16;
    dvtCalcFirmYield                           = 17;
    dvtTargetRecurrenceInterval                = 18;
    dvtStartType                               = 19;
    dvtRunYRCWizardStep1                       = 20;
    dvtRunYRCWizardStep2                       = 21;
    dvtRunYRCWizardStep3                       = 22;
    dvtRunYRCWizardStep4                       = 23;
    dvtRunYRCWizardStep5                       = 24;
    dvtRunHistoricWizardStep1                  = 25;
    dvtRunHistoricWizardStep2                  = 26;
    dvtRunHistoricWizardStep3                  = 27;
    dvtRunHistoricWizardStep4                  = 28;
    dvtRunHistoricWizardStep5                  = 29;
    dvtRunHistoricWizardStep6                  = 30;
    dvtRunStochasticWizardStep1                = 31;
    dvtRunStochasticWizardStep2                = 32;
    dvtRunStochasticWizardStep3                = 33;
    dvtRunStochasticWizardStep4                = 34;
    dvtRunStochasticWizardStep5                = 35;
    dvtRunStochasticWizardStep6                = 36;
    dvtRunStochasticWizardStep7                = 37;
    dvtRunStochasticWizardStep8                = 38;
    dvtRunStochasticWizardStep9                = 39;
    dvtRunTitle1                               = 40;
    dvtRunTitle2                               = 41;
    dvtRunTitle3                               = 42;
    dvtSequenceStartYear                       = 43;

    dvtResCatchAll                             = 44;
    dvtResCatchRef                             = 45;
    dvtResCatchSumDrainageScale                = 46;
    dvtResCatchSumIrrigationScale              = 47;
    dvtSumAfforestationScale                   = 48;

    dvtResPropAll                              = 49;
    dvtResPropReservoirName                    = 50;
    dvtResPropReservoirNumber                  = 51;
    dvtReservoirPenalty                        = 52;
    dvtResPropPriority                         = 53;
    dvtResPropRainCoef                         = 54;

    dvtReservoirCharacteristicsAll             = 55;
    dvtReservoirCharOne                        = 56;
    dvtReservoirCharTwo                        = 57;
    dvtReservoirAreaWhenFull                   = 58;
    dvtReservoirStartingStorageLevel           = 59;
    dvtReservoirFullStorageLevel               = 60;
    dvtReservoirDeadStorageLevel               = 61;
    dvtReservoirBottomOfReservoir              = 62;
    dvtReservoirElevation                      = 63;
    dvtReservoirVolume                         = 64;
    dvtReservoirSurfaceArea                    = 65;
    dvtReservoirEvaporation                    = 66;
    dvtPointsCount                             = 67;

    dvtReservoirEvaporationAll                 = 68;
    dvtReservoirZoneElevationAll               = 69;

    dvtChanPropAll                             = 70;
    dvtChanPropWizardStep1                     = 71;
    dvtChanPropWizardStep2                     = 72;
    dvtChanPropName                            = 73;
    dvtChanPropNumber                          = 74;
    dvtChanPropPenalty                         = 75;
    dvtChanPropUpstreamNode                    = 76;
    dvtChanPropDownstreamNode                  = 77;

    dvtChannelsMaxZeroUpstream                 = 78;
    dvtChannelsMaxZeroDownstream               = 79;

    dvtChanPenaltyAll                          = 80;
    dvtChanPenalty                             = 81;

    dvtMasterControlFeature                    = 82;
    dvtMasterControlFeatureName                = 83;
    dvtMasterControlFeatureType                = 84;
    dvtMasterControlFeatureFactors             = 85;
    dvtMasterControlWizardStep1                = 86;
    dvtTargetYield                             = 87;
    dvtMaximumYield                            = 88;
    dvtTargetPower                             = 89;
    dvtNrOfLoadCases                           = 90;

    dvtLossFeature                             = 91;
    dvtLossFeatureName                         = 92;
    dvtLossFeatureWaterLoss                    = 93;
    dvtLossFeatureDivertedFlow                 = 94;

    dvtMinFlowConstraints                      = 95;
    dvtMinFlowFeatureName                      = 96;
    dvtMinFlowDemand                           = 97;

    dvtMinMaxConstraints                       = 98;
    dvtMinMaxFeatureName                       = 99;
    dvtMinMaxFlowConstraints                   = 100;

    dvtPumpingFeature                          = 101;
    dvtPumpingFeatureName                      = 102;
    dvtPumpingFeatureHead                      = 103;
    dvtPumpingFeatureEfficiency                = 104;

    dvtSpecifiedDemandFeature                  = 105;
    dvtSpecifiedDemandFeatureName              = 106;
    dvtSpecifiedDemandStochasticIndicator      = 107;
    dvtSpecifiedDemandCatchmentRef             = 108;
    dvtSpecifiedDemandFullName                 = 109;
    dvtDamlevelFileName                        = 110;

    dvtSpecifiedInflowFeature                  = 111;
    dvtSpecifiedInflowFeatureName              = 112;

    dvtDivFeature                              = 113;
    dvtDivFeatureName                          = 114;
    dvtDivFeatureType                          = 115;
    dvtDivFeatureType1DiversionDemands         = 116;
    dvtDivFeatureType1NetNaturalInflows        = 117;
    dvtDivFeatureType2FlowRanges               = 118;
    dvtDivFeatureType2ActualDivertedFlows      = 119;
    dvtDivFeatureControllingReservoir          = 120;
    dvtDivFeatureType3Levels                   = 121;
    dvtDivFeatureType3Flows                    = 122;
    dvtDivFeatureType3Proportions              = 123;

    dvtPhysConstraint                          = 124;
    dvtPhysConstraintFeatureName               = 125;
    dvtPhysConstraintUpstreamReservoir         = 126;
    dvtPhysConstraintDownstreamReservoir       = 127;
    dvtPhysConstraintStructureType             = 128;
    dvtPhysConstraintElevationOfSill           = 129;
    dvtPhysConstraintMaximumGateHeight         = 130;
    dvtPhysConstraintDischargeCoefficient      = 131;
    dvtPhysConstraintStructureLength           = 132;
    dvtPhysConstraintArrays                    = 133;
    dvtPhysConstraintArraysType10              = 134;
    dvtPhysConstraintArraysType11              = 135;

    dvtIFRFeature                              = 136;
    dvtIFRFeatureWizardStep1                   = 137;
    dvtIFRFeatureWizardStep2                   = 138;
    dvtIFRFeatureName                          = 139;
    dvtIFRFeatureLagInMonths                   = 140;
    dvtIFRFeatureReferenceNodes                = 141;
    dvtIFRFeatureInflows                       = 142;
    dvtIFRFeatureReleases                      = 143;

    dvtIrrArea                                 = 144;
    dvtIrrAreaName                             = 145;
    dvtIrrAreaWizardStep1                      = 146;
    dvtIrrAreaWizardStep2                      = 147;
    dvtIrrAreaWizardStep3                      = 148;
    dvtIrrAreaDiversionUpstreamNode            = 149;
    dvtIrrAreaReturnFlowDownstreamNode         = 150;
    dvtIrrAreaRelaxationPolicy                 = 151;
    dvtIrrAreaDiversionFlows                   = 152;
    dvtIrrAreaReturnFlows                      = 153;

    dvtPowerPlant                              = 154;
    dvtPowerPlantInfo                          = 155;
    dvtPowerPlantInfoWizardStep1               = 156;
    dvtPowerPlantInfoWizardStep2               = 157;
    dvtPowerPlantInfoWizardStep3               = 158;
    dvtPowerPlantInfoWizardStep4               = 159;
    dvtPowerPlantInfoPowerSpillUpstreamNode    = 160;
    dvtPowerPlantInfoPowerDownstreamNode       = 161;
    dvtPowerPlantInfoSpillDownstreamNode       = 162;
    dvtPowerPlantName                          = 163;
    dvtPowerPlantInfoData                      = 164;
    dvtPowerPlantMaximumGeneratorCapacity      = 165;
    dvtPowerPlantMaximumTurbineCapacity        = 166;
    dvtPowerPlantHeadLoss                      = 167;
    dvtPowerPlantDownstreamPowerPlants         = 168;
    dvtPowerPlantFactors                       = 169;
    dvtPowerPlantCombinedEfficiency            = 170;
    dvtPowerPlantDesignHead                    = 171;
    dvtPowerPlantMaximumNetHead                = 172;
    dvtPowerPlantMinimumNetHead                = 173;
    dvtPowerPlantEfficiencyFactors             = 174;
    dvtPowerPlantNetHeadFactors                = 175;
    dvtPowerPlantTailwater                     = 176;
    dvtPowerPlantTailwaterDischarges           = 177;
    dvtPowerPlantTailwaterElevations           = 178;
    dvtPowerPlantDemands                       = 179;
    dvtPowerPlantGenerations                   = 180;
    dvtPowerPlantReleases                      = 181;

    dvtWaterDemandReconciliationAnalysis       = 182;
    dvtWaterDemandWaterUseProportioning        = 183;
    dvtWaterDemandConfiguration                = 184;
    dvtWaterDemandCategoryCount                = 185;
    dvtWaterDemandCategories                   = 186;
    dvtWaterDemandCategoryName                 = 187;
    dvtWaterDemandCategoryPortion              = 188;
    dvtWaterDemandRiskCriteriaCount            = 189;
    dvtWaterDemandRecurrenceInterval           = 190;
    dvtWaterDemandFeature                      = 191;
    dvtWaterDemandFeatureName                  = 192;
    dvtWaterDemandFeatureCategory              = 193;
    dvtWaterDemandScenarioCount                = 194;
    dvtWaterDemandScenarioPortion              = 195;
    dvtWaterUseCategoryPortion                 = 196;

    dvtChannelAreas                            = 197;
    dvtChannelAreaCount                        = 198;
    dvtChannelAreaName                         = 199;

    dvtAllocDefPropAll                         = 200;
    dvtAllocDefName                            = 201;
    dvtAllocDefStartYear                       = 202;
    dvtAllocDefStartMonth                      = 203;
    dvtAllocDefFileName                        = 204;
    dvtAllocDefEndMonth                        = 205;

    dvtUserPriorityAll                         = 206;
    dvtNrOfReliabilityClasses                  = 207;
    dvtRIValue                                 = 208;
    dvtRILabel                                 = 209;
    dvtNrOfUserCategories                      = 210;
    dvtUserCategoryName                        = 211;
    dvtDistribution                            = 212;
    dvtNrOfAllocationLevels                    = 213;
    dvtAllocLevelName                          = 214;
    dvtCurtailment                             = 215;

    dvtYieldCharactersticsAll                  = 216;
    dvtPlanNrOfLoadCases                       = 217;
    dvtNrOfStartStoragePercs                   = 218;
    dvtNrOfCurveSets                           = 219;
    dvtPeriodLength                            = 220;
    dvtStartStoragePercs                       = 221;

    dvtNrOfSubSystems                          = 222;
    dvtSubSystemPropAll                        = 223;
    dvtSubSystemName                           = 224;
    dvtSubSystemStartYear                      = 225;
    dvtSubSystemStartMonth                     = 226;
    dvtSubSystemEndYear                        = 227;
    dvtSubSystemEndMonth                       = 228;
    dvtSubtractedSubSystemID                   = 229;
    dvtSupportingSubSystemID                   = 230;
    dvtSupportingChannelNr                     = 231;
    dvtShortTermYield                          = 232;
    dvtLowestStreamFlow                        = 233;
    dvtLongTermYield                           = 234;
    dvtFirmYield                               = 235;
    dvtSubSystemReservoirNrs                   = 236;
    dvtRoutingChannelNrs                       = 237;
    dvtSupportCalcType                         = 238;
    dvtCoefficients                            = 239;

    dvtSupportChannelsAll                      = 240;
    dvtNrOfSupportChannels                     = 241;
    dvtSupportChannels                         = 242;
    dvtNrOfCntrlSubSystems                     = 243;
    dvtCntrlSubSystems                         = 244;

    dvtSupportStrategyAll                      = 245;
    dvtSupportStrategy                         = 246;
    dvtBalancingOption                         = 247;
    dvtFixedPositionNr                         = 248;
    dvtFixedPosSubSystemID                     = 249;
    dvtBeforeSubSystemID                       = 250;
    dvtAfterSubSystemID                        = 251;

    dvtDemandDefAll                            = 252;
    dvtNrOfDemandDefs                          = 253;
    dvtDemandCentreID                          = 254;
    dvtParentSubSystemID                       = 255;
    dvtGrowthType                              = 256;
    dvtTargetDemand                            = 257;
    dvtDCUserCategoryID                        = 258;
    dvtSupportArc1                             = 259;
    dvtSupportArc2                             = 260;

    dvtNrOfSupportSubSystems                   = 261;
    dvtSupSubSystemID                          = 262;
    dvtSupSubSysChannelNr                      = 263;

    dvtSwitchDefPropAll                        = 264;
    dvtSwitchDefFileName                       = 265;
    dvtSwitchDefStartYear                      = 266;
    dvtSwitchDefStartMonth                     = 267;

    dvtReservoirTimeControlAll                 = 268;
    dvtBaseNodeNumber                          = 269;
    dvtReservoirStartYear                      = 270;
    dvtReservoirStartMonth                     = 271;
    dvtReservoirEndYear                        = 272;
    dvtReservoirEndMonth                       = 273;
    dvtReservoirEconomicLife                   = 274;
    dvtReservoirCapitalCost                    = 275;
    dvtReservoirOMCost                         = 276;
    dvtReservoirYearsToConstruct               = 277;
    dvtReservoirCostSchedule                   = 278;

    dvtChannelTimeControlAll                   = 279;
    dvtChannelYearsInAnalysis                  = 280;
    dvtChannelStartYear                        = 281;
    dvtChannelStartMonth                       = 282;
    dvtChannelEndYear                          = 283;
    dvtChannelEndMonth                         = 284;
    dvtChannelEconomicLife                     = 285;
    dvtChannelCapitalCost                      = 286;
    dvtChannelFixedOMCost                      = 287;
    dvtChannelVariableOMCost                   = 288;
    dvtChannelYearsToConstruct                 = 289;
    dvtChannelCostSchedule                     = 290;
    dvtChannelEscalationCost                   = 291;

    dvtChannelSwitchControlAll                 = 292;
    dvtSwitchDefinitionID                      = 293;
    dvtSwitchAssociatedNodeNr                  = 294;
    dvtSwitchWaterlevel                        = 295;
    dvtSwitchType                              = 296;
    dvtSwitchInitialStatus                     = 297;
    dvtHydroUnitsCode                          = 298;

    dvtPeriodsPerYear                          = 299;
    dvtCalendarStartMonth                      = 300;
    dvtDecisionMonth                           = 301;
    dvtDecisionType                            = 302;
    dvtHydroPowerIndicator                     = 303;
    dvtAnnualDemand                            = 304;
    dvtMinimumDemand                           = 305;

    dvtResCatchSumUrbanRunOff                  = 306;
    dvtSumUrbanRunOff                          = 307;

    dvtDemandGrowthFactors                     = 308;
    dvtMinMaxGrowthFactors                     = 309;
    dvtArcNumber                               = 310;
    dvtAFFGrowthFactors                        = 311;
    dvtIRRGrowthFactors                        = 312;
    dvtURBGrowthFactors                        = 313;
    dvtStartYear                               = 314;
    dvtBaseYear                                = 315;

    dvtNrOfEconomonicYears                     = 316;
    dvtEquationFunctionX                       = 317;
    dvtEquationFunctionY                       = 318;
    dvtEquationFunctionNonSupply               = 319;
    dvtEquationFunctionCostY                   = 320;

    dvtCalibrationFactor                       = 321;
    dvtPotentialMonthlyEvap                    = 322;
    dvtAbstractionChannel                      = 323;
    dvtAssumedFactor                           = 324;
    dvtCorrespondingChannel                    = 325;
    dvtNumOfCorrespondingChannels              = 326;
    dvtGaugeNumber                             = 327;
    dvtMonthlyAvrgFactor                       = 328;
    dvtMonthlyAvrgNetEvap                      = 329;
    dvtRoutingConstant                         = 330;
    dvtCurtailmentFactor                       = 331;
    dvtMultiplicationFactor                    = 332;
    dvtReturnFlowChannelData                   = 333;

    dvtUpperZoneReturnFlow                     = 334;
    dvtLowerZoneReturnFlow                     = 335;
    dvtReturnFlowLoss                          = 336;
    dvtUpperZoneSoilMoistureCapacity           = 337;
    dvtLowerZoneSoilMoistureCapacity           = 338;
    dvtUpperZoneSoilMoistureTarget             = 339;
    dvtInitialSoilMoistureStorage              = 340;

    dvtIrrigationBlockNumber                   = 341;
    dvtIrrigationBlockName                     = 342;
    dvtIrrigationBlockDescription              = 343;

    dvtIrrigationBlockMaxWaterAllocation       = 344;
    dvtIrrigationBlockAllocatedIrrigationArea  = 345;

    dvtIrrigationBlockCanalTransportLoss       = 346;
    dvtIrrigationBlockReturnFlowFactor         = 347;
    dvtIrrigationBlockEfficiencyFactor         = 348;
    dvtIrrigationBlockNodeNumber               = 349;

    dvtIrrigationBlockNumberOfCropTypes             = 350;
    dvtIrrigationBlockRainAboveRainFactorSpecValue  = 351;
    dvtIrrigationBlockRainBelowRainFactor           = 352;
    dvtIrrigationBlockRainCatchmentScalingFactor    = 353;
    dvtIrrigationBlockAll                           = 354;

    dvtIrrigationBlockPanEvaporation                = 355;
    dvtIrrigationBlockAPanConvFactor                = 356;
    dvtIrrigationBlockRainfallFactor                = 357;

    dvtIrrigationBlockWaterUseCropName              = 358;
    dvtIrrigationBlockWaterUsePercArea              = 359;
    dvtIrrigationBlockWaterUseMonthly               = 360;

    dvtIrrigationBlockDiversionUpstreamNode         = 361;
    dvtIrrigationBlockReturnFlowDownstreamNode      = 362;
    dvtIrrigationBlockFileName                      = 363;
    dvtIrrigationBlockCatchmentFileName             = 364;

    dvtWetlandAll                   = 365;
    dvtWetlandNodeNumber            = 366;
    dvtWetlandName                  = 367;
    dvtWetlandStorageVolume         = 368;
    dvtWetlandInflowProportion      = 369;
    dvtWetlandOutflowProportion     = 370;
    dvtWetlandUpstreamThreshold     = 371;
    dvtWetlandInflowUpstreamNode    = 372;
    dvtWetlandOutflowDownstreamNode = 373;
    dvtMinMaxDistribution           = 374;

    dvtYMDemandCentreAll                    = 375;
    dvtYMDemandCentreName                   = 376;
    dvtYMDemandCentreDescription            = 377;
    dvtYMDemandCentreNodeNumber             = 378;
    dvtYMDemandCentreAveReturnFlowFactor    = 379;
    dvtYMDemandCentreAveMonthlyEvaporation  = 380;
    dvtYMDemandCentreRoutingConstant        = 381;
    dvtYMDemandCentreRainScalingFactor      = 382;
    dvtYMDemandCentreNodeNumberRef          = 383;
    dvtYMDemandCentreTotalFlowLost          = 384;
    dvtYMDemandCentreEvapoTranspiration     = 385;
    dvtYMDemandCentreStdDeviationFactor     = 386;
    dvtYMDemandCentreConsumptive            = 387;
    dvtYMDemandCentreReclaimation           = 388;

    dvtSreamFlowReductionAll                = 389;
    dvtInflowNodeNumber                     = 390;
    dvtCoveredArea                          = 391;
    dvtUnitRunoffFileName                   = 392;
    dvtSoilMoistureFileName                 = 393;
    dvtSFRName                              = 394;
    dvtSFRDescription                       = 395;

    dvtYMDCReturnFlowFeatureAll             = 396;
    dvtYMDCReturnFlowFeatureChannelNr       = 397;
    dvtYMDCReturnFlowFeatureName            = 398;
    dvtYMDCReturnFlowFeatureTotalReturnFlow = 399;
    dvtYMDCReturnFlowFeatureFlowDiversion   = 400;

    dvtParamCatchmentArea                   = 401;

    dvtMineAll                              = 402;
    dvtMineName                             = 403;
    dvtRiverChannelNumber                   = 404;
    dvtHydrologyNodeNumber                  = 405;
    dvtBeneficiationPlantArea               = 406;
    dvtBeneficiationRunOffFactor            = 407;
    dvtMinePanEvaporationFactors            = 408;
    dvtMineLakeEvaporationFactors           = 409;

    dvtOpenCastAll                                = 410;
    dvtPitName                                    = 411;
    dvtCoalReserveArea                            = 412;
    dvtWorkingsArea                               = 413;
    dvtDisturbedWorkingsArea                      = 414;
    dvtDisturbedArea                              = 415;
    dvtWaterSurfaceEvapArea                       = 416;
    dvtDisturbedAreaRunOff                        = 417;
    dvtDisturbedWorkingsAreaRunOff                = 418;
    dvtDecantVolume                               = 419;
    dvtSeepageVolume                              = 420;
    dvtAnalysisStartVolume                        = 421;
    dvtMaximumSeepageRate                         = 422;
    dvtSeepageExponent                            = 423;
    dvtOpenCastPCDSurfaceArea                     = 424;
    dvtOpenCastPCDStorageCapacity                 = 425;
    dvtOpenCastPCDAnalysisStartVolume             = 426;
    dvtOpenCastDisturbedAreaRechargeFactors       = 427;
    dvtOpenCastDisturbedWorkingsRechargeFactors   = 428;

    dvtUndergroundAll                   = 429;
    dvtUndergroundSectionName           = 430;
    dvtChannelNumberToUGDam             = 431;
    dvtUpstreamCatchmentArea            = 432;
    dvtBoardPillarCatchmentArea         = 433;
    dvtHighExtractionCatchmentArea      = 434;
    dvtHighExtractionAreaRunoffFactor   = 435;
    dvtMineUGUpstreamRunoff             = 436;
    dvtUGBoardPillarRechargeFactors     = 437;
    dvtUGHighExtractionRechargeFactors  = 438;

    dvtSlurryDumpAll                    = 439;
    dvtDumpName                         = 440;
    dvtDumpSurfaceArea                  = 441;
    dvtRunoffFactorToPCD                = 442;
    dvtSeepageSplitFactor               = 443;
    dvtDumpPCDStorageCapacity           = 444;
    dvtDumpPCDSurfaceArea               = 445;
    dvtDumpPCDAnalysisStartVolume       = 446;
    dvtDumpRechargeFactors              = 447;
    dvtIFRFeatureCalculationOption      = 448;
    dvtIFRFeatureAnnualInflows          = 449;
    dvtIrrigationBlockDroughtApplicable = 450;

    dvtSpecifiedInflowFileName          = 451;

    dvtCurtailmentAll                   = 452;
    dvtNrOfCurtailmentPeriod            = 453;
    dvtCurtailmentStartMonth            = 454;

    dvtDroughtRestrictionAll            = 455;
    dvtDroughtRestrictionName           = 456;
    dvtAllocationFactors                = 457;
    dvtReferenceStorageVolumes          = 458;
    dvtNrOfCurtailedChannel             = 459;

    dvtGroundWaterAll                        = 460;
    dvtGroundWaterDescription                = 461;
    dvtGroundWaterName                       = 462;
    dvtAquiferStorativity                    = 463;
    dvtAquiferStaticWaterLevel               = 464;
    dvtUnsaturatedStorageCapacity            = 465;
    dvtInitialUnsaturatedStorage             = 466;
    dvtMaximumDischargeRate                  = 467;
    dvtMovingAverageRecharge                 = 468;
    dvtPitmanSoilMoistureCapacity            = 469;
    dvtPitmanSoilMoistureStorageCapacity     = 470;
    dvtPitmansoilMoistureFlowState           = 471;
    dvtPitmanSoilMoistureFlowEquation        = 472;
    dvtPitmanMaximumGroundwaterFlow          = 473;
    dvtPitmanSoilMoistureRechargeEquation    = 474;
    dvtPitmanGroundwaterFlow                 = 475;
    dvtMaximumRateOfGroundwaterBaseFlow      = 476;
    dvtPowerHeadDifferenceBaseFlowEquation   = 477;
    dvtMaximumHydrologicalGradient           = 478;
    dvtAquiferTransmissivity                 = 479;
    dvtBoreHoleDistanceToRiver               = 480;
    dvtMaximumGroundwaterAbstraction         = 481;
    dvtParameterK2                           = 482;
    dvtParameterK3                           = 483;
    dvtGroundWaterEvaporationArea            = 484;
    dvtGroundWaterEvaporation                = 485;
    dvtGroundWaterRefNodeNumber              = 486;
    dvtReservoirAreas                        = 487;
    dvtReservoirAreasCount                   = 488;
    dvtReservoirAreaName                     = 489;

    dvtMineNodeRefNr                         = 490;
    dvtProportionAntecedentFlow              = 491;
    dvtGroundwaterFlowVolume                 = 492;
    dvtAntecedentRunoffDecayFactor           = 493;
    dvtMinimunGroundwaterFlowVolume          = 494;
    dvtMineSubCatchmentAll                   = 495;
    dvtAllGrowthFactors                      = 496;
    dvtReplacements                          = 497;
    dvtTariffCalculationAll                  = 498;
    dvtTariffCalculationChannelNumber        = 499;
    dvtTariffCalculationTariff               = 500;
    dvtTariffCalculationEscalation           = 501;
	
    dvtYearActive                            = 502;
    dvtMonthActive                           = 503;
    dvtYearObsolete                          = 504;
    dvtMonthObsolete                         = 505;
    dvtWQConstraint                          = 506;
    dvtTDSConcentration                      = 507;

    dvtAllType4Growth                        = 508;
    dvtSupplyCapacityPointsCount             = 509;
    dvtSupplyCapacityMethod                  = 510;
    dvtSupplyCapacityBreakpointYears         = 511;
    dvtBreakpointSupplyCapacity              = 512;
    dvtIrrBlockEfficiencyPointsCount         = 513;
    dvtMethodIrrigationEfficiencies          = 514;
    dvtBreakpointYearsIrrigationEfficiencies = 515;
    dvtReturnFlowFactorBreakPointsCount      = 516;
    dvtMethodIrrReturnFlowFactors            = 517;
    dvtReturnFlowFactorBreakpointYear        = 518;
    dvtBreakpointReturnFlowFactor            = 519;
    dvtIrrigatedAreasPointsCount             = 520;
    dvtBreakpointIrrigatedAreas              = 521;
    dvtBreakpointYearsIrrigatedAreas         = 522;
    dvtMaxWaterAllocationCount               = 523;
    dvtMethodMaxWaterAllocation              = 524;
    dvtReturnFlowVolumeCount                 = 525;
    dvtcmbMethodIrrReturnFlowVolume          = 526;

    dvtMaxUpperZoneMoisture                  = 527;
    dvtMinUpperZoneMoisture                  = 528;
    dvtCanalTransmissionLoss                 = 529;
    dvtIrrBlockMultiplicationFactor          = 530;

    dvtMultiRestrictionAll                   = 531;
    dvtMultiRestrictionStartMonth            = 532;
    dvtMultiRestrictionDecisionMonth         = 533;
    dvtMultiRestrictionRes                   = 534;
    dvtMultiRestrictionElev                  = 535;
    dvtMultiRestrictionFactor                = 536;

    dvtNoOfPoints                            = 537;
    dvtInterpolationMethod                   = 538;
    dvtNYR                                   = 539; //No of years growthfactor
    dvtGrowthFactor                          = 540;

    dvtAbstraction                           = 541;
    dvtAbstractToEvap                        = 542;
    dvtAbstractToCPD                         = 543;
    dvtAbstractToRiver                       = 544;
    dvtAbstractionTimeSeries                 = 545;

    dvtCommYear                              = 546;
    dvtCommMonth                             = 547;
    dvtDecommYear                            = 548;
    dvtDecommMonth                           = 549;
    dvtRunOffSaltWashEfficieny               = 550;
    dvtIniSaltStore                          = 551;
    dvtReChargeRate                          = 552;
    dvtPCDIniConcentration                   = 553;

    dvtStandardDeviation                     = 554;
    dvtFlow                                  = 555;
    dvtMeanOfSalt                            = 556;
    dvtAssocSaltWashoff                      = 557;
    dvtRainfallFileName                      = 558;
    dvtMAP                                   = 559;
    dvtSaltBuildUpRate                       = 560;
    dvtSaltWashOffEfficiencyFactor           = 561;


implementation


end.

