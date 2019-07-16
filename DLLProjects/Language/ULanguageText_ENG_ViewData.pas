//
//  UNIT      : Contains language text.
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/01/23
//  COPYRIGHT : Copyright © 2003 DWAF
//
unit ULanguageText_ENG_ViewData;

interface

type TTextItemAddFunction = procedure (AContext, AConstant, AText: string) of object;

procedure LoadLanguageText(AAdd: TTextItemAddFunction);

implementation

procedure LoadLanguageText(AAdd: TTextItemAddFunction);
const OPNAME = 'LoadLanguageText';
begin
  AAdd('ViewData','AllocationDefinition', 'Allocation Control ');
  AAdd('ViewData','SwitchDefinition',     'Switch Control ');

  AAdd('ViewData','AreaCapValues','Area Capacity Relationship');
  AAdd('ViewData','AnlySequences','Analysis Sequences');
  AAdd('ViewData','AnlySequencesAll','Analysis Sequences');
  AAdd('ViewData','AReservoir','Reservoirs');

  AAdd('ViewData','CatchmentProportions','Catchment Hydrology');
  AAdd('ViewData','ChannelArea','Channel Area');
  AAdd('ViewData','ChannelArcPenalty','Channel Arc Penalty');
  AAdd('ViewData','ChannelArcPenaltyAll','Channel Arc Penalty');
  AAdd('ViewData','ChannelComments','Channel Comments');
  AAdd('ViewData','ChannelCommentsAll','Channel Comments');
  AAdd('ViewData','ChannelCounts','Channel Counts');
  AAdd('ViewData','ChannelCountsAll','Channel Counts');
  AAdd('ViewData','ChannelDetails','Channel Details');
  AAdd('ViewData','ChannelDetails2','Master Control Channels');
  AAdd('ViewData','ChannelDetails3','Power Channels');
  AAdd('ViewData','ChannelDetails4','Irrigation Channels');
  AAdd('ViewData','ChannelDetails5','Diversion Channels');
  AAdd('ViewData','ChannelDetails6','Minimum Flow Channels');
  AAdd('ViewData','ChannelDetails7','Loss Channels');
  AAdd('ViewData','ChannelDetails8','Multi-Purpose Min-Max Channels');
  AAdd('ViewData','ChannelDetails9','Pumping Channels');
  AAdd('ViewData','ChannelDetails10','Inflow Channels');
  AAdd('ViewData','ChannelDetails11','Demand Channels');
  AAdd('ViewData','ChannelDetails12','General Flow Channels');
  AAdd('ViewData','ChannelDetails18','IFR Channels');
  AAdd('ViewData','ChannelDetails19','Flow Constraints Channels');
  AAdd('ViewData','ChannelDetails20','Return Flow Channels');  
  AAdd('ViewData','ChannelPenalties','Channel Penalty Structures');
  AAdd('ViewData','ChannelPenalty2','Master Control Channels');
  AAdd('ViewData','ChannelPenalty3','Power Channels');
  AAdd('ViewData','ChannelPenalty4','Irrigation Channels');
  AAdd('ViewData','ChannelPenalty5','Diversion Channels');
  AAdd('ViewData','ChannelPenalty6','Minimum Flow Channels');
  AAdd('ViewData','ChannelPenalty7','Loss Channels');
  AAdd('ViewData','ChannelPenalty8','Multi-Purpose Min-Max Channels');
  AAdd('ViewData','ChannelPenalty9','Pumping Channels');
  AAdd('ViewData','ChannelPenalty10','Inflow Channels');
  AAdd('ViewData','ChannelPenalty11','Demand Channels');
  AAdd('ViewData','ChannelPenalty12','General Flow Channels');
  AAdd('ViewData','ChannelPenalty18','IFR Channels');
  AAdd('ViewData','ChannelPenalty19','Flow Constraints Channels');
  AAdd('ViewData','ChannelPenalty20','Return Flow Channels');  
  AAdd('ViewData','ChannelDetailsHead','Channel Details');
  AAdd('ViewData','ChannelDetailsHead1','Channel Penalties');
  AAdd('ViewData','ChannelPenalty','Channel Penalties');

  AAdd('ViewData','ChannelDetailsAll','Channel Details');
  AAdd('ViewData','ChannelHeading','Channels');
  AAdd('ViewData','ChannelHeadingAll','Channel Descriptions');
  AAdd('ViewData','ChannelPowerDownStream','Channel Power Down Stream');
  AAdd('ViewData','ChannelPowerDownStreamAll','Channel Power Down Stream');
  AAdd('ViewData','CompareData','Model Data Across Study Areas');
  AAdd('ViewData','Constraints','Constraints');
  AAdd('ViewData','ConstraintsAll','Constraints');
  AAdd('ViewData','ConstraintsDetailsAll','Constraints Details');
  AAdd('ViewData','ConstraintsDischargeAll','Flow Constraints Discharge');
  AAdd('ViewData','ConstraintsElevationAll','Flow Constraints Elevation');
  AAdd('ViewData','ConstraintsHeading','Physical Flow Constraints for Channels');
  AAdd('ViewData','ConstraintsHeadingAll','Physical Flow Constraints for Channels');
  AAdd('ViewData','Curtailment','Curtailments');

  AAdd('ViewData','DataReservoirPlots','Reservoir Characteristics Plots');
  AAdd('ViewData','DataReservoirElevationPlot','Reservoir Elevation Plot');
  AAdd('ViewData','DataReservoirAreaPlot','Reservoir Area Plot');
  AAdd('ViewData','DataReservoirAreaByElevationPlot','Reservoir Area By Elevation');
  AAdd('ViewData','DataReservoirVolumePlot','Reservoir Volume');
  AAdd('ViewData','DataReservoirVolumeByElevationPlot','Reservoir Volume By Elevation');

  AAdd('ViewData','DataReservoirElevationPlotXDescr','');
  AAdd('ViewData','DataReservoirElevationPlotYDescr','Reservoir Elevation Plot');
  AAdd('ViewData','DataReservoirElevationPlotGDescr','Reservoir Elevation Plot');

  AAdd('ViewData','DataReservoirAreaPlotXDescr','');
  AAdd('ViewData','DataReservoirAreaPlotYDescr','Reservoir Area Plot');
  AAdd('ViewData','DataReservoirAreaPlotGDescr','Reservoir Area Plot');

  AAdd('ViewData','DataReservoirAreaByElevationPlotXDescr','');
  AAdd('ViewData','DataReservoirAreaByElevationPlotYDescr','Reservoir Area By Elevation');
  AAdd('ViewData','DataReservoirAreaByElevationPlotGDescr','Reservoir Area By Elevation');

  AAdd('ViewData','DataReservoirVolumePlotXDescr','');
  AAdd('ViewData','DataReservoirVolumePlotYDescr','Reservoir Volume');
  AAdd('ViewData','DataReservoirVolumePlotGDescr','Reservoir Volume');

  AAdd('ViewData','DataReservoirVolumeByElevationPlotXDescr','');
  AAdd('ViewData','DataReservoirVolumeByElevationPlotYDescr','Reservoir Volume By Elevation');
  AAdd('ViewData','DataReservoirVolumeByElevationPlotGDescr','Reservoir Volume By Elevation');

  AAdd('ViewData','DataTimeSeries','Hydrology/Demand Time Series');

  AAdd('ViewData','DataDemandYearMonthFilePlotsABS','ABS Demand Files Plot');
  AAdd('ViewData','DataDemandYearMonthFilePlotsAFF','AFF Demand Files Plot');
  AAdd('ViewData','DataDemandYearMonthFilePlotsCIR','CIR Demand Files Plot');
  AAdd('ViewData','DataDemandYearMonthFilePlotsDEM','DEM Demand Files Plot');
  AAdd('ViewData','DataDemandYearMonthFilePlotsINC','INC  Demand Files Plot');
  AAdd('ViewData','DataDemandYearMonthFilePlotsIRD','IRD Demand Files Plot');
  AAdd('ViewData','DataDemandYearMonthFilePlotsIRR','IRR Demand Files Plot');
  AAdd('ViewData','DataDemandYearMonthFilePlotsRAN','RAN  Demand Files Plot');
  AAdd('ViewData','DataDemandYearMonthFilePlotsRNK','RNK  Demand Files Plot');
  AAdd('ViewData','DataDemandYearMonthFilePlotsURB','URB Demand Files Plot');

  AAdd('ViewData','DaysPerMonth','Days Per Month');
  AAdd('ViewData','DaysPerMonthAll','Days Per Month');
  AAdd('ViewData','DemandYearMonthFileABS','ABS Demand Files');
  AAdd('ViewData','DemandYearMonthFileAFF','AFF Demand Files');
  AAdd('ViewData','DemandYearMonthFileCIR','CIR Demand Files');
  AAdd('ViewData','DemandYearMonthFileDEM','DEM Demand Files');
  AAdd('ViewData','DemandYearMonthFileINC','INC Demand Files');
  AAdd('ViewData','DemandYearMonthFileIRD','IRD Demand Files');
  AAdd('ViewData','DemandYearMonthFileIRR','IRR Demand Files');
  AAdd('ViewData','DemandYearMonthFilePlotsABS','ABS Demand Files Plot');
  AAdd('ViewData','DemandYearMonthFilePlotsAFF','AFF Demand Files Plot');
  AAdd('ViewData','DemandYearMonthFilePlotsCIR','CIR Demand Files Plot');
  AAdd('ViewData','DemandYearMonthFilePlotsDEM','DEM Demand Files Plot');
  AAdd('ViewData','DemandYearMonthFilePlotsINC','INC  Demand Files Plot');
  AAdd('ViewData','DemandYearMonthFilePlotsIRD','IRD Demand Files Plot');
  AAdd('ViewData','DemandYearMonthFilePlotsIRR','IRR Demand Files Plot');
  AAdd('ViewData','DemandYearMonthFilePlotsRAN','RAN  Demand Files Plot');
  AAdd('ViewData','DemandYearMonthFilePlotsRNK','RNK  Demand Files Plot');
  AAdd('ViewData','DemandYearMonthFilePlotsURB','URB Demand Files Plot');
  AAdd('ViewData','DemandYearMonthFileRAN','RAN Demand Files');
  AAdd('ViewData','DemandYearMonthFileRNK','RNK  Demand Files');
  AAdd('ViewData','DemandYearMonthFileURB','URB Demand Files');
  AAdd('ViewData','DroughtRestrictionStructures','Drought Restriction Structures');

  AAdd('ViewData','FeaturesHeading','Network Features');
  AAdd('ViewData','FileAff','Afforestation Files');
  AAdd('ViewData','FileConfiguration','System Configuration Data Files');
  AAdd('ViewData','FileDemand','Demand Files');
  AAdd('ViewData','FileDirectory','Directories File');

  AAdd('ViewData','FileAllocationDefinition','Allocation Definition Files');
  AAdd('ViewData','FileReservoirImplementation','Reservoir Implementation Date Definition File');
  AAdd('ViewData','FileDisbenefitDefinition','Disbenefit Function Definition File');
  AAdd('ViewData','FileGrowthFactors','Annual Growth Factors File');
  AAdd('ViewData','FileMonthlyWaterRequirement','Monthly Water Requirement File');
  AAdd('ViewData','FileHydropowerAllocation',' Hydropower Allocation Control File');
  AAdd('ViewData','FilePumpingChannelControl','Pumping Channel Control File');
  AAdd('ViewData','FileGeneralChannelControl','General and Purification Channel Control File');
  AAdd('ViewData','FileReclamationPlantControl','Reclamation Plant Control File');
  AAdd('ViewData','FileReturnFlowChannel','Return Flow Channel File');
  AAdd('ViewData','FileChannelSwitchControl','Channel Switch Control Files');
  AAdd('ViewData','FileTariffCalculation','Tariff Calculation File');
  AAdd('ViewData','FileAllocationChannel','Allocation Channel Control File');
  AAdd('ViewData','FileReleaseStructure','Controlled Release Structure File');
  AAdd('ViewData','FileCurtail','Multi-Reservoir-Channel Restriction Rule Definition');

  AAdd('ViewData','FileSaltsWashoff','WQT - Salts Washoff File(s)');
  AAdd('ViewData','FileMine','WQT - Mine File(s)');
  AAdd('ViewData','FileMineRanfall','WQT - Rainfall File(s)');


  AAdd('ViewData','FileHydrological','Hydrology Files');
  AAdd('ViewData','FileHydrology','Wrsm2000 file');
  AAdd('ViewData','FileInc','Incremental Flow Files');
  AAdd('ViewData','FileIrr','Iirrigation Demand Files');
  AAdd('ViewData','FileOutput','Output Files');
  AAdd('ViewData','FileParam','Parameter File');
  AAdd('ViewData','FileRan','Point Rainfall Files');
  AAdd('ViewData','FileDamWaterLevels','Dam Water Levels File');
  AAdd('ViewData','FlowConstraints','Flow Constraints');
  AAdd('ViewData','FlowConstraintsDischarge','Flow Constraints Discharge');
  AAdd('ViewData','FlowConstraintsElevation','Flow Constraints Elevation');

  AAdd('ViewData','GrowthFactors','Annual Growth Factors');

  AAdd('ViewData','H/S','H/S');
  AAdd('ViewData','HydrologyAllocation','Hydrology Allocation');
  AAdd('ViewData','HydrologyFiles','Hydrology Files Data');
  AAdd('ViewData','HydrologyFilesAFF','AFF Hydrology Files');
  AAdd('ViewData','HydrologyFilesINC','INC  Hydrology Files');
  AAdd('ViewData','HydrologyFilesIRR','IRR Hydrology Files');
  AAdd('ViewData','HydrologyFilesRAN','RAN Hydrology Files');
  AAdd('ViewData','HydrologyFileNamesABS','ABS Hydrology Files');
  AAdd('ViewData','HydrologyFileNamesAFF','AFF Hydrology Files');
  AAdd('ViewData','HydrologyFileNamesCIR','CIR  Hydrology Files');
  AAdd('ViewData','HydrologyFileNamesDEM','DEM Hydrology Files');
  AAdd('ViewData','HydrologyFileNamesINC','INC Hydrology Files');
  AAdd('ViewData','HydrologyFileNamesIRD','IRD Hydrology Files');
  AAdd('ViewData','HydrologyFileNamesIRR','IRR Hydrology Files');
  AAdd('ViewData','HydrologyFileNamesRAN','RAN  Hydrology Files');
  AAdd('ViewData','HydrologyFileNamesRNK','RNK Hydrology Files');
  AAdd('ViewData','HydrologyFileNamesURB','URB Hydrology Files');
  AAdd('ViewData','HydrologyFilePlotAFF','AFF Hydrology File Plot');
  AAdd('ViewData','HydrologyFilePlotINC','INC Hydrology File Plot');
  AAdd('ViewData','HydrologyFilePlotIRR','IRR Hydrology File Plot');
  AAdd('ViewData','HydrologyFilePlotRAN','RAN Hydrology File Plot');
  AAdd('ViewData','HydrologyYearDetails','Hydrology Data');
  AAdd('ViewData','HydrologyYearDetailsAll','Hydrology Data');

  AAdd('ViewData','IncrRainfall','Incremental Rainfall');
  AAdd('ViewData','IrrigationAreas','Irrigation Areas');
  AAdd('ViewData','IrrigationBlock','Irrigation Block');
  AAdd('ViewData','IrrigatedArea','Irrigated Area');
  AAdd('ViewData','ReviewIrrigationBlock','Irrigation Blocks');
  AAdd('ViewData','CropWater','Crop Requirement');
  AAdd('ViewData','SoilProperties','Soil Properties');
  AAdd('ViewData','EvaporationTransporation','Climate');
  AAdd('ViewData','IrrigationBlockReturnFlow','Return Flow Channel');
  AAdd('ViewData','IrrigationBlockReturnFlows','Return Flow');
  AAdd('ViewData','IFRSiteData','IFR Site(s)');

  AAdd('ViewData','MasterControlConfiguration','Master Control Configuration');
  AAdd('ViewData','MainDemand','Demand Graphs');
  AAdd('ViewData','MainRainfall','Rainfall Graphs');
  AAdd('ViewData','MaxYield','Maximum Yield');
  AAdd('ViewData','MaxYieldAll','Maximum Yield');
  AAdd('ViewData','MetaData','Meta Data');
  AAdd('ViewData','ModelCapability','Model Capability');
  AAdd('ViewData','ModelData','Model Data Per Study Area');
  AAdd('ViewData','ModelViews','ModelViews');
  AAdd('ViewData','ModelElementID','Node Number');
  AAdd('ViewData','MonthNames','Month Names');
  AAdd('ViewData','MonthNamesAll','Month Names');
  AAdd('ViewData','Mine','Mining');
  AAdd('ViewData','MineSubcatchment','MineSubcatchment');
  AAdd('ViewData','ReviewMine','Mining');
  AAdd('ViewData','Groundwater','Groundwater');
  AAdd('ViewData','ReviewGroundwater','Groundwater');
  AAdd('ViewData','GroundwaterSubcatchment','Mine Sub-catchments');

  AAdd('ViewData','NetworkMasterElementProp','Master Element Properties');
  AAdd('ViewData','NetworkVisualiser','Network Visualiser');
  AAdd('ViewData','NetworkVisualiserPos','Positions');
  AAdd('ViewData','NodesDetails','Nodes With InFlow');
  AAdd('ViewData','NodesWithInflow','Nodes With InFlow');
  AAdd('ViewData','NodesWithoutInFlow','Nodes Without InFlow');
  AAdd('ViewData','NodesDetails1','Nodes With Inflow');
  AAdd('ViewData','NodesHeading','Nodes');
  AAdd('ViewData','NodesDetailHeading','Nodes Details');
  AAdd('ViewData','NodesDetailsAll','Nodes Details');
  AAdd('ViewData','NVDrawing','Drawing');
  AAdd('ViewData','NVDrawingElements','Drawing Elements');
  AAdd('ViewData','NVDrawingGroup','Drawing Groups');
  AAdd('ViewData','NVDrawingInst','Drawing Instance');
  AAdd('ViewData','NVDrawingPos','Drawing Position');
  AAdd('ViewData','NVDrawingProp','Drawing Properties');
  AAdd('ViewData','NVMaster','Drawing Master');
  AAdd('ViewData','NVMasterElements','Master Elements');
  AAdd('ViewData','NVMasterPenaltyC','Channel Penalties');
  AAdd('ViewData','NVMasterPenaltyR','Reservoir Penalties');
  AAdd('ViewData','NVMasterPropPrim','Master Primary Properties');
  AAdd('ViewData','NVMasterPropSec','Master Secondary Properties');
  AAdd('ViewData','NVDrawingInstCaption','Drawing Instance Caption');
  AAdd('ViewData','NVDrawingPosMast','Position of Master Elements');
  AAdd('ViewData','NVDrawingInstChannel','Drawing Instance Channel');
  AAdd('ViewData','NVDrawingPosMCAP','Position of Master Captions');
  AAdd('ViewData','NVDrawingPosMPen','Position of Penalty Tables');

  AAdd('ViewData','OutputConfiguration','Output Configuration');
  AAdd('ViewData','OutputComparisonSheet','Output Comparison');
  AAdd('ViewData','Comparison','Comparison');
  AAdd('ViewData','ComparisonFileSelection','Comparison File Selection');
  AAdd('ViewData','ComparisonAReservoir','Reservoir Comparison');
  AAdd('ViewData','ComparisonChannelHeading','Channel Comparison');

  AAdd('ViewData','ParametersHeading','Configuration');
  AAdd('ViewData','ParametersHeadingAll','Parameters');
  AAdd('ViewData','PointRainFall','Point Rain Fall');
  AAdd('ViewData','PowerPlants','Power Plants');
  AAdd('ViewData','PolicyVariables','Policy Variables');
  AAdd('ViewData','DailyDiversionStationData','Daily Diversion Station(s)');
  AAdd('ViewData','DDTSDamData','Reservoir');

  AAdd('ViewData','Reservoir','Reservoir');
  AAdd('ViewData','ReservoirAndChannel','Reservoir and Channel');
  AAdd('ViewData','ReservoirAll','Reservoir');
  AAdd('ViewData','ReservoirArea','Reservoir Area');
  AAdd('ViewData','ReservoirAreaAll','Reservoir Area');
  AAdd('ViewData','ReservoirAreaByElevationPlot','Reservoir Area By Elevation');
  AAdd('ViewData','ReservoirAreaPlot','Reservoir Area Plot');
  AAdd('ViewData','ReservoirChannels','Reservoir Channels');
  AAdd('ViewData','ReservoirChannelsAll','Reservoir Channels');
  AAdd('ViewData','ReservoirDetails','Reservoir Details');
  AAdd('ViewData','ReservoirConfigurations','Reservoir Configurations');
  AAdd('ViewData','ReservoirDetailsAll','Reservoir Details');
  AAdd('ViewData','ReservoirElevation','Reservoir Elevation');
  AAdd('ViewData','ReservoirElevationAll','Reservoir Elevation');
  AAdd('ViewData','ReservoirElevationPlot','Reservoir Elevation Plot');
  AAdd('ViewData','ReservoirEvap','Reservoir Evaporation');
  AAdd('ViewData','ReservoirEvapAll','Reservoir Evaporation');
  AAdd('ViewData','ReservoirHeading','Reservoirs');
  AAdd('ViewData','ReservoirDetailsHeading','Area Capacity Relationship');
  AAdd('ViewData','ReservoirHeadingAll','Resevoir Data and Inflow Node Data');
  AAdd('ViewData','ReservoirLevels','Reservoir Levels');
  AAdd('ViewData','ReservoirLevel','Draw Down Levels');
  AAdd('ViewData','ReservoirLevelsAll','Reservoir Levels');
  AAdd('ViewData','ReservoirPenalty','Reservoir Penalty Structures');
  AAdd('ViewData','ReservoirPlots','Reservoir Characteristics Plots');
  AAdd('ViewData','ReservoirVolume','Reservoir Volume');
  AAdd('ViewData','ReservoirVolumeAll','Reservoir Volume');
  AAdd('ViewData','ReservoirVolumeByElevationPlot','Reservoir Volume By Elevation');
  AAdd('ViewData','ReservoirVolumePlot','Reservoir Volume');
  AAdd('ViewData','ReservoirZoneHeading','Reservoir Zone and Rule Curve Definitions');
  AAdd('ViewData','ReservoirZoneHeadingAll','Reservoir Zone and Rule Curve Definitions');
  AAdd('ViewData','RunConfiguration','Run Configuration');
  AAdd('ViewData','ReconciliationAnalysis','Reconciliation Analysis');
  AAdd('ViewData','DroughtRestrictions','Drought Restrictions');
  AAdd('ViewData','RunParameters','Run Parameters');
  AAdd('ViewData','RunParametersAll','Run Parameters');
  AAdd('ViewData','RunTitle','Run Title');
  AAdd('ViewData','RunTitleAll','Run Title');
  AAdd('ViewData','ReservoirPenaltyLev','Penalties and Levels');

  AAdd('ViewData','BoxPlotGraph','Box Plot Graph');
  AAdd('ViewData','WhiskerPlotGraph','Whisker Plot Graph');

  AAdd('ViewData','Wetland','Wetlands');
  AAdd('ViewData','ReviewWetland','Wetlands');
  AAdd('ViewData','StreamFlowReduction','Stream Flow Reduction');
  AAdd('ViewData','ReviewStreamFlowReduction','Stream Flow Reduction');

  AAdd('ViewData','SumOutHeading','Output Files');
  AAdd('ViewData','SumOutDetailsHead','Details');
  AAdd('ViewData','SumOutDetailsHead1','What');
  AAdd('ViewData','SumOutYearFile1','Month-End Reservoir Volume');
  AAdd('ViewData','SumOutYearFile2','Month-End Reservoir Elevation');
  AAdd('ViewData','SumOutYearFile3','Net Basin Runoff Into Res Area');
  AAdd('ViewData','SumOutYearFile4','Rainfall On Reservoir Surface)');
  AAdd('ViewData','SumOutYearFile5','Gross Evaporation Loss From Reservoir');
  AAdd('ViewData','SumOutYearFile6','Monthly Average Spill Flow (m3/s)');
  AAdd('ViewData','SumOutYearFile7','Monthly Average Stacked Capacity (mw)');
  AAdd('ViewData','SumOutYearFile8','Monthly Average Stacked Energy (mw continuous)');
  AAdd('ViewData','SumOutYearFile9','Monthly Average Irrigation Deficits (m3/s)');
  AAdd('ViewData','SumOutYearFile10','Monthly Average Irrigation Deficits (m3/s)');
  AAdd('ViewData','SumOutYearFile11','Monthly Average Channel Flow');
  AAdd('ViewData','SumOutYearFile12','Monthly Pumping Energy (gwh)');
  AAdd('ViewData','strNoDataReturned','There is no data in this scenario for the selected item.');
//  AAdd('ViewData','strNoDataReturnedChannelInflow','There are no Inflow channels in the current scenario.');
  AAdd('ViewData','strNoDataReturnedChannelInflow','There is no data for this selected item in the current scenario.');
  AAdd('ViewData','strNoDataReturnedChannelDemand','There is no data for this selected item in the current scenario.');
  AAdd('ViewData','strNoDataReturnedChannelGeneral','There is no data for this selected item in the current scenario.');
  AAdd('ViewData','strNoDataReturnedChannelMaster','TThere is no data for this selected item in the current scenario.');
  AAdd('ViewData','strNoDataReturnedChannelPower','There is no data for this selected item in the current scenario.');
  AAdd('ViewData','strNoDataReturnedChannelIrrigation','There is no data for this selected item in the current scenario.');
  AAdd('ViewData','strNoDataReturnedChannelDiversion','There is no data for this selected item in the current scenario.');
  AAdd('ViewData','strNoDataReturnedChannelMinFlow','There is no data for this selected item  in the current scenario.');
  AAdd('ViewData','strNoDataReturnedChannelLoss','There is no data for this selected item  in the current scenario.');
  AAdd('ViewData','strNoDataReturnedChannelMultiPurpose','There is no data for this selected item  in the current scenario.');
  AAdd('ViewData','strNoDataReturnedChannelPumping','There is no data for this selected item  in the current scenario.');
  AAdd('ViewData','strNoDataReturnedChannelReturnFlow','There is no data for this selected item  in the current scenario.');
  AAdd('ViewData','strNoDataReturnedChannelIFR','There is no data for this selected item  in the current scenario.');
  AAdd('ViewData','strNoDataReturnedChannelPFC','There is no data for this selected item  in the current scenario.');
  AAdd('ViewData','strNoWetland','There is no wetlands for this selected item  in the current scenario.');
  AAdd('ViewData','strNoIrrigationBlock','There is no irrigation block for this selected item  in the current scenario.');
  AAdd('ViewData','strNoYMDemandCentre','There are no demand centres for this selected item  in the current scenario.');
  AAdd('ViewData','strNoStreamFlowReductions','There is no stream flow data in the current scenario.');
  AAdd('ViewData','strNoMine','There is no mining data in the current scenario.');
  AAdd('ViewData','strNoGroundWater','There is no groundwater data in the current scenario.');

  AAdd('ViewData','StorageZoneDetails','Storage ZoneDetails');
  AAdd('ViewData','StorageZoneDetailsAll','Storage ZoneDetails');
  AAdd('ViewData','StorageZones','Storage Zones');
  AAdd('ViewData','StorageZonesAll','Storage Zones');
  AAdd('ViewData','SumOutYearFilePlots1','MONTH-END RESERVOIR VOLUME(MCM)');
  AAdd('ViewData','SumOutYearFilePlots2','MONTH-END RESERVOIR ELEVATION(M)');
  AAdd('ViewData','SumOutYearFilePlots3','NET BASIN RUNOFF INTO RES AREA(M3/S)');
  AAdd('ViewData','SumOutYearFilePlots4','RAINFALL ON RESERVOIR SURFACE(M3/S)');
  AAdd('ViewData','SumOutYearFilePlots5','GROSS EVAPORATION LOSS FROM RESERVOIR(M3/S)');
  AAdd('ViewData','SumOutYearFilePlots6','MONTHLY AVERAGE POWER FLOW (M3/S)');
  AAdd('ViewData','SumOutYearFilePlots7','MONTHLY AVERAGE SPILL FLOW (M3/S)');
  AAdd('ViewData','SumOutYearFilePlots8','MONTHLY AVERAGE STACKED CAPACITY (MW)');
  AAdd('ViewData','SumOutYearFilePlots9','MONTHLY AVERAGE STACKED ENERGY (MW CONTINUOUS)');
  AAdd('ViewData','SumOutYearFilePlots10','MONTHLY AVERAGE IRRIGATION DEFICITS (M3/S)');
  AAdd('ViewData','SumOutYearFilePlots11','MONTHLY AVERAGE CHANNEL FLOW (M3/S)');
  AAdd('ViewData','SumOutYearFilePlots12','MONTHLY PUMPING ENERGY (GWH)');
  AAdd('ViewData','SumOutTimeSeries','Output Files Time Series');

  AAdd('ViewData','SumOutYearFileGraphPlots1','MONTH-END RESERVOIR VOLUME(MCM)');
  AAdd('ViewData','SumOutYearFileGraphPlots2','MONTH-END RESERVOIR ELEVATION(M)');
  AAdd('ViewData','SumOutYearFileGraphPlots3','NET BASIN RUNOFF INTO RES AREA(M3/S)');
  AAdd('ViewData','SumOutYearFileGraphPlots4','RAINFALL ON RESERVOIR SURFACE(M3/S)');
  AAdd('ViewData','SumOutYearFileGraphPlots5','GROSS EVAPORATION LOSS FROM RESERVOIR(M3/S)');
  AAdd('ViewData','SumOutYearFileGraphPlots6','MONTHLY AVERAGE POWER FLOW (M3/S)');
  AAdd('ViewData','SumOutYearFileGraphPlots7','MONTHLY AVERAGE SPILL FLOW (M3/S)');
  AAdd('ViewData','SumOutYearFileGraphPlots8','MONTHLY AVERAGE STACKED CAPACITY (MW)');
  AAdd('ViewData','SumOutYearFileGraphPlots9','MONTHLY AVERAGE STACKED ENERGY (MW CONTINUOUS)');
  AAdd('ViewData','SumOutYearFileGraphPlots10','MONTHLY AVERAGE IRRIGATION DEFICITS (M3/S)');
  AAdd('ViewData','SumOutYearFileGraphPlots11','MONTHLY AVERAGE CHANNEL FLOW (M3/S)');
  AAdd('ViewData','SumOutYearFileGraphPlots12','MONTHLY PUMPING ENERGY (GWH)');
  AAdd('ViewData','SwitchDefinition','Switch Control ');

  AAdd('ViewData','TargetPower','Target Power');
  AAdd('ViewData','TargetPowerAll','Target Power');
  AAdd('ViewData','TargetYield','Target Yield');
  AAdd('ViewData','TargetYieldAll','Target Yield');
  AAdd('ViewData','TimeSeries','Hydrology/Demand Time Series');

  AAdd('ViewData','WaterUseProportioning','Water Use Proportioning');
  AAdd('ViewData','WRSM2000Details','WRSM2000 Details');
  AAdd('ViewData','WRSM2000DetailsAll','WRSM2000 Details');
  AAdd('ViewData','WRSM2000FlowGaugesDetail','FlowGauges Detail');
  AAdd('ViewData','WRSM2000FlowGaugesDetailAll','FlowGauges Detail');
  AAdd('ViewData','WRSM2000Heading','WRSM2000 Data');
  AAdd('ViewData','WRSM2000HeadingAll','WRSM2000 Data');
  AAdd('ViewData','WRSM2000RouteDetails','Route Details');
  AAdd('ViewData','WRSM2000RouteDetailsAll','Route Details');
  AAdd('ViewData','WRSM2000SubModuleDetails','SubModule Details');
  AAdd('ViewData','WRSM2000SubModuleDetailsAll','SubModule Details');

  AAdd('ViewData','Wetland','Wetlands');

  AAdd('ViewData','SystemYield','System Yield');
  AAdd('ViewData','Historic','Historical Yield Results');
  AAdd('ViewData','Stochastic','Stochastic Yield Results');
  AAdd('ViewData','EstimatedHistoric','Estimated Historical Firm Yield');  

  AAdd('ViewData','SumOutGraph','Output Files Graph');
  AAdd('ViewData','SumOutYearFileGraphPlots1','MONTH-END RESERVOIR VOLUME(MCM)');
  AAdd('ViewData','SumOutYearFileGraphPlots2','MONTH-END RESERVOIR ELEVATION(M)');
  AAdd('ViewData','SumOutYearFileGraphPlots3','NET BASIN RUNOFF INTO RES AREA(M3/S)');
  AAdd('ViewData','SumOutYearFileGraphPlots4','RAINFALL ON RESERVOIR SURFACE(M3/S)');
  AAdd('ViewData','SumOutYearFileGraphPlots5','GROSS EVAPORATION LOSS FROM RESERVOIR(M3/S)');
  AAdd('ViewData','SumOutYearFileGraphPlots6','MONTHLY AVERAGE POWER FLOW (M3/S)');
  AAdd('ViewData','SumOutYearFileGraphPlots7','MONTHLY AVERAGE SPILL FLOW (M3/S)');
  AAdd('ViewData','SumOutYearFileGraphPlots8','MONTHLY AVERAGE STACKED CAPACITY (MW)');
  AAdd('ViewData','SumOutYearFileGraphPlots9','MONTHLY AVERAGE STACKED ENERGY (MW CONTINUOUS)');
  AAdd('ViewData','SumOutYearFileGraphPlots10','MONTHLY AVERAGE IRRIGATION DEFICITS (M3/S)');
  AAdd('ViewData','SumOutYearFileGraphPlots11','MONTHLY AVERAGE CHANNEL FLOW (M3/S)');
  AAdd('ViewData','SumOutYearFileGraphPlots12','MONTHLY PUMPING ENERGY (GWH)');

  AAdd('ViewData','Review','Review');
  AAdd('ViewData','ReviewChannelArea','Channel Area');
  AAdd('ViewData','ReviewChannelHeading','Channels');
  AAdd('ViewData','ReviewChannelDetails','Channel Details');
  AAdd('ViewData','ReviewChannelDetails2','Master Control Channels');
  AAdd('ViewData','ReviewChannelDetails3','Power Channels');
  AAdd('ViewData','ReviewChannelDetails4','Irrigation Channels');
  AAdd('ViewData','ReviewChannelDetails5','Diversion Channels');
  AAdd('ViewData','ReviewChannelDetails6','Minimum Flow Channels');
  AAdd('ViewData','ReviewChannelDetails7','Loss Channels');
  AAdd('ViewData','ReviewChannelDetails8','Multi-Purpose Min-Max Channels');
  AAdd('ViewData','ReviewChannelDetails9','Pumping Channels');
  AAdd('ViewData','ReviewChannelDetails10','Inflow Channels');
  AAdd('ViewData','ReviewChannelDetails11','Demand Channels');
  AAdd('ViewData','ReviewChannelDetails12','General Flow Channels');
  AAdd('ViewData','ReviewChannelDetails18','IFR Channels');
  AAdd('ViewData','ReviewChannelDetails19','Flow Constraints Channels');
  AAdd('ViewData','ReviewChannelDetails20','Return Flow Channels');
  AAdd('ViewData','ReviewParametersHeading','Configuration');
  AAdd('ViewData','ReviewNodesHeading','Nodes');
  AAdd('ViewData','ReviewNodesWithInflow','Nodes With InFlow');
  AAdd('ViewData','ReviewNodesWithoutInFlow','Nodes Without InFlow');
  AAdd('ViewData','ReviewMasterControlConfiguration','Master Control Configuration');
  AAdd('ViewData','ReviewAReservoir','Reservoirs');
  AAdd('ViewData','ReviewFeaturesHeading','Network Features');
  AAdd('ViewData','ReviewIrrigationAreas','Irrigation Areas');
  AAdd('ViewData','ReviewPowerPlants','Power Plants');
  AAdd('ViewData','ReviewSubSystemCurtailment','Sub-System Curtailment');
  AAdd('ViewData','ReviewTotalSystemCurtailment','System Curtailment');
  AAdd('ViewData','ReviewDemandSupply','Demand/Supply');
  AAdd('ViewData','ReviewSubSystemStorage','Sub-System Storage');
  AAdd('ViewData','ReviewTotalSystemStorage','Total System Storage');
  AAdd('ViewData','ReviewInterBasinSupport','Inter-Basin Support');

  AAdd('ViewData','ReviewMonthlyReservoirResult','Monthly Reservoir Results');
  AAdd('ViewData','ReviewDamStorage','Dam Storage');
  AAdd('ViewData','ReviewDemands','Demands');
  AAdd('ViewData','ReviewSubSystems','Sub-Systems');
  AAdd('ViewData','ReviewMonthlyChannelResult','Monthly Channel Results');

  AAdd('ViewData','ReviewWRPMChannelDetails2','Master Control Channels');
  AAdd('ViewData','ReviewWRPMChannelDetails3','Power Channels');
  AAdd('ViewData','ReviewWRPMChannelDetails4','Irrigation Channels');
  AAdd('ViewData','ReviewWRPMChannelDetails5','Diversion Channels');
  AAdd('ViewData','ReviewWRPMChannelDetails6','Minimum Flow Channels');
  AAdd('ViewData','ReviewWRPMChannelDetails7','Loss Channels');
  AAdd('ViewData','ReviewWRPMChannelDetails8','Multi-Purpose Min-Max Channels');
  AAdd('ViewData','ReviewWRPMChannelDetails9','Pumping Channels');
  AAdd('ViewData','ReviewWRPMChannelDetails10','Inflow Channels');
  AAdd('ViewData','ReviewWRPMChannelDetails11','Demand Channels');
  AAdd('ViewData','ReviewWRPMChannelDetails12','General Flow Channels');
  AAdd('ViewData','ReviewWRPMChannelDetails18','IFR Channels');
  AAdd('ViewData','ReviewWRPMChannelDetails19','Flow Constraints Channels');
  AAdd('ViewData','ReviewDemandChannelsGridSummary','Demand Channels');
  AAdd('ViewData','ReviewDemandChannelsGraphSummary','Demand Channels');

  AAdd('ViewData','ReviewReservoirArea','Reservoir Area');
  AAdd('ViewData','ReviewSummary','Summaries');
  AAdd('ViewData','ReviewSummaryGrid','Grids');
  AAdd('ViewData','ReviewSummaryGraph','Graphs');

  AAdd('ViewData','YMDemandCentre','Demand Centres');
  AAdd('ViewData','ReviewYMDemandCentre','Demand Centres');
  AAdd('ViewData','YMDemandCentreReturnFlowChannel','Return flow channels');
  AAdd('ViewData','YMDemandCentreSupplyChannel','Supply channels');
  AAdd('ViewData','ReviewSumOutDataSource','Output Data Source');
  AAdd('ViewData','ReviewSystemYield','System Yield');
  AAdd('ViewData','ReviewCollateOutputFiles','Collate Output Files');

end;

end.
