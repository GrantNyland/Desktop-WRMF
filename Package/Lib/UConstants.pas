//$HEADER$

unit UConstants;


interface

uses
  SysUtils;
const
  {MAXNODES   =  1000; // Maximum number of nodes in the system.
  MAXARCS    =  4000; // Maximum number of arcs in the system.
  MAXCHANNEL =  2000; // Maximum number of channels in the system.
  MAXGRP     =   500; // Maximum number of reservoirs in the system - nodes with a storage curve defined.
  MAXCHNTYP  =   150; // Maximum number of channel penalty structures in the system.
  MAXPOWCHN  =    20; // Maximum number of power channels in the system.
  MAXZONES   =    20; // Maximum number of reservoir zones in the system.
  MAXRESTYP  =    20; // Maximum number of reservoir penalty structures in the system.
  P10        = (MAXCHANNEL + MAXGRP + MAXNODES + 1) * 2;
  MAXRESNUM  = 10000; // MAXIMUM EXTERNAL RESERVOIR NUMBER
  MAXCHANNUM = 10000; // Maximum value given for an external channel number.
  MAXRESPTS  =    15; // Maximum number of points in the reservoir area capacity curve.
  MNDESC     =    10; // MAX. # OF DECISION STRUCTURES
  MNSEQ      =  1000; // Max. # OF SEQUENCES
  MNYC       =   200; // Maximum number of years.
  MNRR       =   200; // Maximum number of irrigation blocks.
  NGM        =   300; // Maximum number of gauging station allowed (number of elements from param.dat).
  NS         =  1000; // Maximum number of stochastic sequences.
  MNY        =   200; // The maximum # of years in a stochastic sequence.
  MNGS       =   500; // The maximum number of gauges for random numbers in the stochastic process.
  NEXARCMAX  =  1000; // Maximum number of channels that flow out of the system (end at the sink node).
  MNSDSMAX   =   200; // Maximum number of specified demand channels.
  NOCHANMAX  =   200; // Maximum number of channels which demands will be reduced if criteria are violated.}
  MNSYLDMAX  =   300; // Maximum number of channels requiring firm yield analyses.
  {MNCCHNMAX  =   300; // Maximum number of channels that can be curtailed.
  MNCONNMAX  =  2000; // Maximum number of channels on a node, in and out.
  MNCITNMAX  =  1000; // Maximum number of channels into a node. (Maximum channels out of a node is MNCONNMAX - MNCITNMAX)
  }
  NullInteger  = -2147483647;
  NullFloat    = -922337203685477.00;
  NullString   = '';
  NullDateTime = 0.0;
  NullChar     = ' ';

  // User rights sets
  CUR_EditData = [2,3,100];
  CUR_SysAdmin = 100;  
  Char_Set     = ['0'..'9'];
  tmpFileName  = 'temp.txt';

  dtDatabase    = 1;
  dtFile        = 2;
  dtSystem      = 3;
  dtKnownData   = 4;

  dtpControl    = 1;
  dtpReservior  = 2;
  dtpChannel    = 3;
  dtpStructure  = 4;
  dtpZone       = 5;

  ftpControl           = 1;
  ftpReservior         = 2;
  ftpChannel           = 3;
  ftpStructure         = 4;
  ftpZone              = 5;
  ftpInitialResLevels  = 6;

  stpStudyArea    = 1;
  stpFileType     = 2;
  stpLanguage     = 3;
  stpLanguageText = 4;
  stpUser         = 5;

  kdRunTitle      = 1;
  kdRunParameters = 2;
  kdMonthNames    = 3;
  kdDaysPerMonth  = 4;
  kdAnlySequences = 5;
  kdTargetYield   = 6;
  kdMaxYield      = 7;
  kdTargetPower   = 8;
  kdReservoir     = 9;
  kdChannel       = 10;
  kdStructure     = 11;
  kdZone          = 12;

  //F01.dat
  MinMonths        = 1;
  MaxMonths        = 12;
  MinLoadCase      = 1;
  MaxLoadCase      = 15;

  //F02.dat
  MinPowerChannels = 1;
  MaxPowerChannels = 20;
  MinPoints        = 1;
  MaxPoints        = 15;
  WrapTextCount    = 8;

  //F03.dat
  MinArcs   = 1;
  MaxArcs   = 5;
  MinDownStreamPowerChannels = 1;
  MaxDownStreamPowerChannels = 10;

  //F04.dat
  MinCurvePoints   = 1;
  MaxCurvePoints   = 10;

  //F05.dat
  MinPenaltyStructure    = 1;
  MaxPenaltyStructure    = 20;
  MinReservoirMonths     = 1;
  MaxReservoirMonths     = 12;
  MinPenaltyZone         = 1;
  MaxPenaltyZone         = 20;
  MinAverageMonthlyLevel = 1;
  MaxAverageMonthlyLevel = 7;

  //F06.dat
  MinReservoirInitialLevel = 1;
  MaxReservoirInitialLevel = 8;

  //F07.dat
  MinNumberOfPoints = 1;
  MaxNumberOfPoints = 10;

  //F08.dat
  MinPowerDemand = 1;
  MaxPowerDemand = 12;

  //F09.dat
  MinDiversionFlow = 1;
  MaxDiversionFlow = 12;
  MinReturnFlow = 1;
  MaxReturnFlow = 12;

  //F10.dat
  MinDiversionDemand = 1;
  MaxDiversionDemand = 12;

  //F11.dat
  MinMinFlow = 1;
  MaxMinFlow = 12;
  MinLoss = 1;
  MaxLoss = 12;
  MinDiverted = 1;
  MaxDiverted = 12;

  //F12.dat
  MinFlowConstraints = 1;
  MaxFlowConstraints = 12;

  //F13.dat
  MinPowerControl = 1;
  MaxPowerControl = 12;
  MinWaterSupply = 1;
  MaxWaterSupply = 12;

  //F14.dat
  MinReleaseStructure = 1;
  MaxReleaseStructure = 12;


  //F15.dat
  MinCurtailmentPeriod = 1;
  MaxCurtailmentPeriod = 10;

  MinStorageVolumes = 1;
  MaxStorageVolumes = 10;

  MinAllocationFactors = 1;
  MaxAllocationFactors = 10;

  //F17.dat
  MinNoOfCrops  = 1;
  MaxNoOfCrops  = 20;

  //F19.dat
  MinNoReturnFlowChannels = 1;
  MaxNoReturnFlowChannels = 20;

  //Param.dat
  MinKeyGauge = 1;
  MaxKeyGauge = 200;
  MinMatrix   = 1;
  MaxMatrix   = 10;
 //GTH.DAT
  MinGFactorYear = 1;
  MaxGFactorYear = 100;
  //FileTypes
  ftWRSM2000File  = 00;
  ftConfigFile01  = 01;
  ftConfigFile02  = 02;
  ftConfigFile03  = 03;
  ftConfigFile04  = 04;
  ftConfigFile05  = 05;
  ftConfigFile06  = 06;
  ftConfigFile07  = 07;
  ftConfigFile08  = 08;
  ftConfigFile09  = 09;
  ftConfigFile10  = 10;
  ftConfigFile11  = 11;
  ftConfigFile12  = 12;
  ftConfigFile13  = 13;
  ftConfigFile14  = 14;
  ftParamFile     = 15;
  ftWrymFile      = 16;

 //DAM.DAT
  fgFamily        = 1;
  fgSwitch        = 2;
  fgHydroPower    = 3;
  fgAllocation    = 4;
  fgCur           = 5;

  //Planning output file groups
  pufgDAT         = 21;
  pufgDBG         = 22;
  pufgSUM         = 23;
  pufgSYS         = 24;
  pufgRES         = 25;
  pufgPLN         = 26;
  pufgRLX         = 27;
  pufgPLT         = 28;
  pufgPMP         = 29;

  //MinMax FieldValue Constants
   NegativeInf = '-#';
   PositiveInf = '~#';

  //Treeview nodes identifier. This are the same as in the database
  tvnidAReservoir                 = 120;
  tvnidNodesWithInflow            = 10000;
  tvnidNodesWithoutInFlow         = 10010;
  tvnidChannelDetails12           = 10110;
  tvnidChannelDetails6            = 10050;
  tvnidChannelDetails8            = 10070;
  tvnidChannelDetails9            = 10080;
  tvnidChannelDetails7            = 10060;
  tvnidChannelDetails11           = 10100;
  tvnidChannelDetails5            = 10040;
  tvnidChannelDetails10           = 10090;
  tvnidChannelDetails18           = 10120;
  tvnidChannelDetails19           = 10130;
  tvnidIrrigationAreas            = 210;
  tvnidPowerPlants                = 200;
  tvnidMasterControlConfiguration = 10003;
  tvnidIrrigationBlock            = 211;
  tvnidWetland                    = 212;
  tvnidStreamFlowReduction        = 213;
  tvnidYMDemandCentre             = 214;
  tvnidMine                       = 215;
  tvnidOpenCast                   = 216;
  tvnidAllocationDefinition       = 170;
  tvnidSwitchDefinition           = 180;
  tvnidCurtailment                = 217;
  tvnidDroughtRestriction         = 218;
  tvnidGroundWater                = 219;

  //Model Data view names, passed to to FAppModules.Model.View Model Data functions
  mdvnRunTitle                                       = 'RUNTITLE';
  mdvnMetaData                                       = 'METADATA';
  mdvnRunConfiguration                               = 'RUNCONFIGURATION';
  mdvnOutputConfiguration                            = 'OUTPUTCONFIGURATION';
  mdvnMasterControlConfiguration                     = 'MASTERCONTROLCONFIGURATION';
  mdvnCatchmentProportions                           = 'CATCHMENTPROPORTIONS';
  mdvnChannelPenalties                               = 'CHANNELPENALTIES';
  mdvnReservoirPenalty                               = 'RESERVOIRPENALTY';
  mdvnChannelArea                                    = 'CHANNELAREA';
  mdvnReconciliationAnalysis                         = 'RECONCILIATIONANALYSIS';
  mdvnRestrictions                                   = 'RESTRICTIONS';
  mdvnCurtailments                                   = 'CURTAILMENT';
  mdvnReservoir                                      = 'RESERVOIR';
  mdvnNodesWithInflow                                = 'NODESWITHINFLOW';
  mdvnNodesWithoutInFlow                             = 'NODESWITHOUTINFLOW';
  mdvnChannel                                        = 'CHANNEL';
  mdvnPowerPlant                                     = 'POWERPLANT';
  mdvnIrrigationArea                                 = 'IRRIGATIONAREA';
  mdvnIrrigationBlock                                = 'IRRIGATIONBLOCK';
  mdvnWetland                                        = 'WETLAND';
  mdvnStreamFlowReduction                            = 'STREAMFLOWREDUCTION';
  mdvnYMDemandCentre                                 = 'YMDEMANDCENTRE';
  mdvnMine                                           = 'MINE';
  mdvnPCDDam                                         = 'PCDDAM';
  mdvnUndegroundDam                                  = 'UNDEGROUNDDAM';
  mdvnReservoirAreaGroup                             = 'RESERVOIRAREA';

  mdvnHydrologyAllocation                            = 'HYDROLOGYALLOCATION';
  mdvnWaterUseProportioning                          = 'WATERUSEPROPORTIONING';
  mdvnModelCapability                                = 'MODELCAPABILITY';
  mdvnIFRFeaturesProperty                            = 'IFRFEATURESPROPERTY';
  mdvnAllocationDefinition                           = 'ALLOCATIONDEFINITION';
  mdvnSwitchDefinition                               = 'SWITCHDEFINITION';
  mdvnGrowthFactors                                  = 'GROWTHFACTORS';
  mdvnInputViewDataGrid                              = 'INPUTVIEWDATAGRID';
  mdvnInputViewDataGraph                             = 'INPUTVIEWDATAGRAPH';
  mdvnModelViews                                     = 'MODELVIEWS';
  mdvnInput                                          = 'INPUT';
  mdvnOutput                                         = 'OUTPUT';
  mdvnOutputComparisonFileSelection                  = 'OUTPUTCOMPARISONFILESELECTION';
  mdvnOutputReservoirComparison                      = 'OUTPUTRESERVOIRCOMPARISON';
  mdvnOutputChannelComparison                        = 'OUTPUTCHANNELCOMPARISON';
  mdvnGroundWater                                    = 'GROUNDWATER';
  mdvnAquiferNode                                    = 'AQUIFERNODE';
  mdvnBaseFlowNode                                   = 'BASEFLOWNODE';
  mdvnAbstractionNode                                = 'ABSTRACTIONNODE';
  mdvnCollectionNode                                 = 'COLLECTIONNODE';
  mdvnSumOutDataSource                               = 'SUMOUTDATASOURCE';
  mdvnSystemYield                                    = 'SYSTEMYIELD';
  mdvnGroundwaterSubcatchment                        = 'GROUNDWATERSUBCATCHMENT';
  mdvnReviewMonthlyReservoirResult                   = 'REVIEWMONTHLYRESERVOIRRESULT';
  mdvnReviewMonthlyChannelResult                     = 'REVIEWMONTHLYCHANNELRESULT';
  mdvnReviewDemands                                  = 'REVIEWDEMANDS';
  mdvnReviewSubSystems                               = 'REVIEWSUBSYSTEMS';
  mdvnReviewDemandChannelsGraphSummary               = 'REVIEWDEMANDCHANNELSGRAPHSUMMARY';
  mdvnReviewDemandChannelsGridSummary                = 'REVIEWDEMANDCHANNELSGRIDSUMMARY';

  mdvnReviewCollateOutputFiles                       = 'REVIEWCOLLATEOUTPUTFILES';
  mdvnReviewMasterControlConfiguration               = 'REVIEWMASTERCONTROLCONFIGURATION';
  mdvnReviewSubSystemStorage                         = 'REVIEWSUBSYSTEMSTORAGE';
  mdvnReviewTotalSystemStorage                       = 'REVIEWTOTALSYSTEMSTORAGE';
  mdvnReviewSubSystemCurtailment                     = 'REVIEWSUBSYSTEMCURTAILMENT';
  mdvnReviewTotalSystemCurtailment                   = 'REVIEWTOTALSYSTEMCURTAILMENT';
  mdvnReviewDemandSupply                             = 'REVIEWDEMANDSUPPLY';
  mdvnReviewInterBasinSupport                        = 'REVIEWINTERBASINSUPPORT';
  mdvnReviewDamStorage                               = 'REVIEWDAMSTORAGE';

  ofcnSystemStorage                                  = 'SYS VOL';
  ofcnSystemCurtailment                              = 'SYSTEM';
  {mdvnReviewCurtailments                             = 'REVIEWCURTAILMENT';
  mdvnReviewDemandSupply                             = 'REVIEWDEMANDSUPPLY';
  mdvnReviewSystemStorage                            = 'REVIEWSYSTEMSTORAGE';
  mdvnReviewTotalSystemStorage                       = 'REVIEWTOTALSYSTEMSTORAGE';
  mdvnReviewInterBasinSupport                        = 'REVIEWINTERBASINSUPPORT';}

  //RWH Model
  strHeadingFile1                                    = 'Date        Rainfall   Storage     Spill   Deficit Tank Size = ';
  strHeadingFile2                                    = '                                           Number of Days Supplied per Month';
  RainfallDataFileStartRow                           = 0;
  OutputFileDataStartRow                             = 8;
  VolumeUnitName                                     = 'Litre';
  SACurrencyUnit                                     = 'R';
implementation

end.
