//
//  UNIT      : Contains language text.
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/01/23
//  COPYRIGHT : Copyright © 2003 DWAF
//
unit ULanguageText_ENG_VNV;

interface

type TTextItemAddFunction = procedure (AContext, AConstant, AText: string) of object;

procedure LoadLanguageText(AAdd: TTextItemAddFunction);
implementation

procedure LoadLanguageText(AAdd: TTextItemAddFunction);
const OPNAME = 'LoadLanguageText';
begin
  AAdd('VNV', 'nvtGeneral',          'General');
  AAdd('VNV', 'nvtDemandFile',       'Demand file');
  AAdd('VNV', 'nvtName',             'Name');
  AAdd('VNV', 'nvtInflow',           'Inflow');
  AAdd('VNV', 'nvtNetBasinRunoff',   'Net Basin Runoff');
  AAdd('VNV', 'nvtElevation',        'Elevation');
  AAdd('VNV', 'nvtVolume',           'Volume');
  AAdd('VNV', 'nvtRainfall',         'Rainfall');
  AAdd('VNV', 'nvtEvaporation',      'Evaporation');
  AAdd('VNV', 'nvtAvgNetBasinRunoff','Average Net Basin Runoff');
  AAdd('VNV', 'nvtAvgElevation',     'Average Elevation');
  AAdd('VNV', 'nvtAvgVolume',        'Average Volume');
  AAdd('VNV', 'nvtAvgRainfall',      'Average Rainfall');
  AAdd('VNV', 'nvtAvgEvaporation',   'Average Evaporation');
  AAdd('VNV', 'nvtPercFull',         'Percentage full');
  AAdd('VNV', 'nvtChannelFlow',      'Channel flow');
  AAdd('VNV', 'nvtReservoirStorageChange',      'Change in volume');
  AAdd('VNV', 'nvtAvgChannelFlow',   'Average Channel flow');
  AAdd('VNV', 'nvtTimePeriod',       'Time Period');
  AAdd('VNV', 'DlgCaptionReservoir', 'Please Select Reservoir');
  AAdd('VNV', 'DlgCaptionNode',      'Please Select Node');
  AAdd('VNV', 'DlgCaptionChannel',   'Please Select Channel');
  AAdd('VNV', 'DlgCaptionMine',      'Please Select Mine');
  AAdd('VNV', 'DlgCaptionGroundWater','Please Select Ground Water');
  AAdd('VNV', 'DlgCaptionPowerPlant','Please Select Power Plant');
  AAdd('VNV', 'UseExisting',         'Use existing');
  AAdd('VNV', 'CreateNew',           'Create new');
  AAdd('VNV', 'SelectReservoir',     'Please select a reservoir - or Cancel.');
  AAdd('VNV', 'SelectIrrigation',    'Please select an irrigation block - or Cancel.');
  AAdd('VNV', 'SelectIrrigationArea','Please select an irrigation area - or Cancel.');
  AAdd('VNV', 'SelectWetland',       'Please select a Wetland - or Cancel.');
  AAdd('VNV', 'SelectDemandCentre',  'Please select a Demand Centre - or Cancel.');
  AAdd('VNV', 'SelectNode',          'Please select a node - or Cancel.');
  AAdd('VNV', 'SelectChannel',       'Please select a channel - or Cancel.');
  AAdd('VNV', 'SelectResPenalty',    'Please select a reservoir penalty - or Cancel.');
  AAdd('VNV', 'SelectChanPenalty',   'Please select a channel penalty - or Cancel.');
  AAdd('VNV', 'SelectMine',          'Please select a mine - or Cancel.');
  AAdd('VNV', 'SelectGroundWater',          'Please select a Ground Water Sub-Catchment - or Cancel.');
  AAdd('VNV', 'SelectDroughtRestriction',  'Please select a drought restriction - or Cancel.');
  AAdd('VNV', 'SelectPowerPlant',          'Please select a power plant - or Cancel.');
  AAdd('VNV', 'MultiOutputSelectorErr',    'The diagram already contains an output selector.');
  AAdd('VNV', 'InclExistReservoirs',       'Include reservoirs already on diagram');
  AAdd('VNV', 'InclAllRestriction',        'Show all drought restrictions');
  AAdd('VNV', 'InclExistNodes',            'Include nodes already on diagram');
  AAdd('VNV', 'InclExistChannels',         'Include channels already on diagram');
  AAdd('VNV', 'InclExistResPenalties',     'Include reservoir penalties already on diagram');
  AAdd('VNV', 'InclExistChanPenalties',    'Include channel penalties already on diagram');
  AAdd('VNV', 'InclExistMines',            'Include mines already on diagram');
  AAdd('VNV', 'InclExistGroundWater',      'Include Ground Water already on diagram');
  AAdd('VNV', 'InclExistPowerPlant',       'Include Power Plant already on diagram');  
  AAdd('VNV', 'AddReservoirsFirst',        'Add reservoirs to the drawing first');
  AAdd('VNV', 'AddChannelsFirst',          'Add channels to the drawing first');
  AAdd('VNV', 'NoUserRights',              'This action is disallowed because the current user does not have rights to edit data.');
  AAdd('VNV', 'ScenarioLocked',            'This action is disallowed because the scenario is locked by another user.');
  AAdd('VNV', 'WithInflow',                'With inflow');
  AAdd('VNV', 'WithoutInflow',             'Without inflow');
  AAdd('VNV', 'DuplicateReservoir',        'Reservoir %d is already on the diagram. Are you sure you want to add a duplicate?');
  AAdd('VNV', 'DuplicateNode',             'Node %d is already on the diagram. Are you sure you want to add a duplicate?');
  AAdd('VNV', 'DuplicateChannel',          'Channel %d is already on the diagram. Are you sure you want to add a duplicate?');
  AAdd('VNV', 'DuplicateResPenalty',       'Reservoir %d penalty is already on the diagram. Are you sure you want to add a duplicate?');
  AAdd('VNV', 'DuplicateChanPenalty',      'Channel %d penalty is already on the diagram. Are you sure you want to add a duplicate?');
  AAdd('VNV', 'DuplicateIrrBlock',         'Irrigation block %d is already on the diagram. Are you sure you want to add a duplicate?');
  AAdd('VNV', 'DuplicateIrrArea',          'Irrigation area %d is already on the diagram. Are you sure you want to add a duplicate?');
  AAdd('VNV', 'DuplicateWetland',          'Wetland %d is already on the diagram. Are you sure you want to add a duplicate?');
  AAdd('VNV', 'DuplicateDemandCentre',     'Demand Centre %d is already on the diagram. Are you sure you want to add a duplicate?');
  AAdd('VNV', 'DuplicateMine',             'Mine %d is already on the diagram. Are you sure you want to add a duplicate?');
  AAdd('VNV', 'DuplicateGroundWater',      'Ground Water Sub-Catchment %d is already on the diagram. Are you sure you want to add a duplicate?');
  AAdd('VNV', 'DuplicatePowerPlant',       'Power Plant %d is already on the diagram. Are you sure you want to add a duplicate?');  
  AAdd('VNV', 'AddTextLabel',              'Add text label');
  AAdd('VNV', 'AssignTextToElement',       'Assign text label to network element.');
  AAdd('VNV', 'Reservoir',                 'Reservoir');
  AAdd('VNV', 'Node',                      'Node');
  AAdd('VNV', 'Channel',                   'Channel');
  AAdd('VNV', 'Wetland',                   'Wetland');
  AAdd('VNV', 'TextType',                  'Text type :');
  AAdd('VNV', 'SelectNetworkElement',      'Please select the network element to which the text label must be assigned.');
  AAdd('VNV', 'ReservoirOptions',          'Reservoir options');
  AAdd('VNV', 'NodeOprions',               'Node options');
  AAdd('VNV', 'ChannelOptions',            'Channel options');
  AAdd('VNV', 'Unassigned',                'Unassigned');
  AAdd('VNV', 'ChangeUpstreamNode',        'This action will change the upstream node number of channel %d ' +
                                           ' from %d to %d. This change will not only be applied to the diagram but ' +
                                           ' also to the scenario data. Are you sure you want to continue?');
  AAdd('VNV', 'ChangeDownstreamNode',      'This action will change the downstream node number of channel %d ' +
                                           ' from %d to %d. This change will not only be applied to the diagram but ' +
                                           ' also to the scenario data. Are you sure you want to continue?');
  AAdd('VNV', 'MasterControl',             'Master Control');
  AAdd('VNV', 'PowerChannel',              'Power');
  AAdd('VNV', 'PowerSpill',                'Power Spill');
  AAdd('VNV', 'IrrigationDiversion',       'Irrigation Diversion');
  AAdd('VNV', 'WetlandInflow',             'Wetland Inflow');
  AAdd('VNV', 'IrrigationConsumption',     'Irrigation Consumption');
  AAdd('VNV', 'WetlandOutflow',            'Wetland Outflow');
  AAdd('VNV', 'IrrigationReturnFlow',      'Irrigation Return Flow');
  AAdd('VNV', 'Diversion',                 'Diversion');
  AAdd('VNV', 'MinimumFlow',               'Minimum Flow');
  AAdd('VNV', 'Loss',                      'Loss');
  AAdd('VNV', 'MinMax',                    'Multi Purpose Min Max');
  AAdd('VNV', 'Pumping',                   'Pumping');
  AAdd('VNV', 'SpecifiedInflow',           'Specified Inflow');
  AAdd('VNV', 'SpecifiedDemand',           'Specified Demand');
  AAdd('VNV', 'GeneralFlow',               'General Flow');
  AAdd('VNV', 'MayNotAddInflow',           'Inflow symbols may not be added to drawing manually. ' + #10#13 +
                                           'They will be added automatically when a reservoir or ' + #10#13 +
                                           'node with inflow is added.');
  AAdd('VNV', 'MayNotDeleteInflow',        'Inflow symbols may not be deleted from drawing manually. ' + #10#13 +
                                           'They will be deleted automatically when a reservoir or ' + #10#13 +
                                           'node with inflow is deleted.');
  AAdd('VNV', 'ViewReservoirProperties',   'View Reservoir Properties');
  AAdd('VNV', 'ViewMineProperties',        'View Mine Properties');
  AAdd('VNV', 'ViewGroundWaterProperties', 'View Ground Water Properties');
  AAdd('VNV', 'ViewMineToRiver',           'View Mine to River Properties');
  AAdd('VNV', 'ViewMineOpenCastPits',      'View Mine''s Open Cast Pits Properties');
  AAdd('VNV', 'ViewUndergroundSection',    'View Mine''s Underground Section Properties');
  AAdd('VNV', 'ViewDiscardSlurryDump',     'View Mine''s Discard/Slurry Dump Properties');
  AAdd('VNV', 'ViewReservoirPenalty',      'View Reservoir Penalty');
  AAdd('VNV', 'ConfigReservoirPenalty',    'Add/Delete Reservoir Penalty');
  AAdd('VNV', 'ViewReservoirCatchment',    'View Reservoir Catchment Hydrology');
  AAdd('VNV', 'ConfigureReservoirCatchment','Add/Delete Reservoir Catchment Hydrology');
  AAdd('VNV', 'ViewReservoirElevation',    'View Reservoir Elevation');
  AAdd('VNV', 'ViewReservoirEvaporation',  'View Reservoir Evaporation');
  AAdd('VNV', 'ViewReservoirPhysical',     'View Reservoir Physical Characteristics');
  AAdd('VNV', 'ViewReservoirwaterBalance', 'View Reservoir Water Balance');
  AAdd('VNV', 'ViewReservoirOutputDataGrid', 'View Reservoir Output Data Grid');
  AAdd('VNV', 'ViewReservoirOutputDataGraph','View Reservoir Output Data Graph');
  AAdd('VNV', 'DeleteReservoirFromScenario', 'Delete Reservoir from Scenario');
  AAdd('VNV', 'DeleteMineFromScenario',      'Delete Mine from Scenario');
  AAdd('VNV', 'DeleteGroundWaterFromScenario','Delete Ground Water from Scenario');
  AAdd('VNV', 'MenuCancelOption',            'Cancel...');
  AAdd('VNV', 'SetPermenant',                'Set Permenant');
  AAdd('VNV', 'RemovePermenant',             'Allow Refresh');
  AAdd('VNV', 'ViewNodeProperties',          'View Node Properties');
  AAdd('VNV', 'ViewTextProperties',          'View Text Properties');
  AAdd('VNV', 'ViewChannelPenaltyProperties','View Channel Penalty Properties');
  AAdd('VNV', 'ViewReservoirPenaltyProperties','View Reservoir Penalty Properties');
  AAdd('VNV', 'NodeOptions',                 'View Node Properties');
  AAdd('VNV', 'ViewNodeCatchment',           'View Node Catchment Proportions');
  AAdd('VNV', 'DeleteNodeFromScenario',      'Delete Node from Scenario');
  AAdd('VNV', 'ViewChannelProperties',       'View Channel Properties');
  AAdd('VNV', 'ViewChannelPenalty',          'View Channel Penalty');
  AAdd('VNV', 'ConfigChannelPenalty',        'Add/Delete Channel Penalty');
  AAdd('VNV', 'ViewMinMaxFeature',           'View Min-max Feature');
  AAdd('VNV', 'ViewLossFeature',             'View Loss Feature');
  AAdd('VNV', 'ViewMinimumFeature',          'View Minimum Flow Feature');
  AAdd('VNV', 'ViewDiversionFeature',        'View Diversion Feature');
  AAdd('VNV', 'ViewPumpingFeature',          'View Pumping Feature');
  AAdd('VNV', 'ViewSpecifiedDemandFeature',  'View Specified Demand Feature');
  AAdd('VNV', 'ViewSpecifiedInflowFeature',  'View Specified Inflow Feature');
  AAdd('VNV', 'ViewPhysicalFlowConstraint',  'View Physical Flow Constraint');
  AAdd('VNV', 'ViewIFRFeature',              'View IFR Feature');
  AAdd('VNV', 'ViewWaterDemandFeature',      'View Water Demand Feature');
  AAdd('VNV', 'ViewMasterControlFeature',    'View Master Control Feature');
  AAdd('VNV', 'ViewIrrigationArea',          'View Irrigation Area');
  AAdd('VNV', 'ViewWetland',                 'View Wetland');
  AAdd('VNV', 'ViewPowerPlant',              'View Power Plant');
  AAdd('VNV', 'SplitChannel',                'Split Channel');
  AAdd('VNV', 'DeleteChannelFromScenario',   'Delete Channel from Scenario');
  AAdd('VNV', 'StencilDoesNotExist',         'Stencil file %s does not exist.');
  AAdd('VNV', 'Unassgined',                  'Unassigned');
  AAdd('VNV', 'RefreshShapes',               'Refreshing shapes...Please wait');
  AAdd('VNV', 'NrShapesUpdated',             'Nr of shapes updated = ');
  AAdd('VNV', 'OfTotal',                     ' of ');
  AAdd('VNV', 'DrawingReadOnly',             'Drawing Read&Only');
  AAdd('VNV', 'CopyDrawing',                 'Co&py Drawing');
  AAdd('VNV', 'PasteDrawing',                'P&aste Drawing');
  AAdd('VNV', 'ConfirmDeleteDrawing',        'Delete selected drawing?');
  AAdd('VNV', 'EnterGroupName',              'Enter drawing group name');
  AAdd('VNV', 'GroupName',                   'Drawing Group Name');
  AAdd('VNV', 'GroupNameEmpty',              'Drawing group name cannot be empty. Action not completed.');
  AAdd('VNV', 'GroupNameExists',             'Drawing group name already exists. Please specify a new one.');
  AAdd('VNV', 'SelectGroup',                 'Please select a drawing group node first.');
  AAdd('VNV', 'EnterNewGroupName',           'Enter new drawing group name');
  AAdd('VNV', 'NewGroupName',                'New Drawing Group Name');
  AAdd('VNV', 'ConfirmDeleteGroup',          'Group contains at least one drawing.'+#13+#10+'Deleting this group will delete those drawings as well.'+#13+#10+'Continue deleting group and all its drawings?');
  AAdd('VNV', 'EnterNewDrawingName',         'Enter new drawing name');
  AAdd('VNV', 'NewDrawingName',              'New Drawing Name');
  AAdd('VNV', 'DrawingNameEmpty',            'Drawing name cannot be empty. Action not completed.');
  AAdd('VNV', 'DrawingNameExistsInGroup',    'New drawing name already exists in the group.');
  AAdd('VNV', 'DrawingNameExists',           'Drawing name already exists. Please specify a new one.');
  AAdd('VNV', 'EnterDrawingName',            'Enter drawing name');
  AAdd('VNV', 'DrawingName',                 'Drawing Name');
  AAdd('VNV', 'SelectDrawing',               'Please select a drawing node first.');
  AAdd('VNV', 'FileDoesNotExists',           '%s does not exist');
  AAdd('VNV', 'AttemptToStartVisio',
                    'An attempt will be made to start VISIO in order to edit the diagram.' + #13+#10 +
                    'On success, this application will be suspended and will only resume ' + #13#10 +
                    'after editing has been finished.' + #13#10#13#10 +
                    'VERY IMPORTANT : ' + #13#10 +
                    'Once VISIO has been started, a message regarding establishment ' + #13#10 +
                    'of a connection to the COM server will be displayed.' + #13#10 +
                    'If such a message is NOT displayed, the VISIO settings for macro ' + #13#10 +
                    'security levels are most probably incorrect.' + #13#10#13#10 +
                    'WRMF will attempt to update Visio''s Macro Security Level to allow' + #13#10 +
                    'WRMF to communicate with Visio.');
  AAdd('VNV', 'VisioUpdateMacroSecLevelFailed',
                    'WRMF failed to update Visio''s Macro Security Settings automatically' + #13#10 +
                    'Please change the Macro Security Levels manually. Do so by clicking ' + #13#10 +
                    'on the Tools menu (in Visio), select Macros, select ' + #13#10 +
                    'Security and set the level to Low.' + #13#10 +
                    'Exit VISIO to return to the IMS and edit the diagram again.');
  AAdd('VNV', 'ReservoirAlreadyHasSFR','This Reservoir already has an SFR subcatchment assigned to it.');

  AAdd('VNV', 'GISMapError',           'An error occurred while exporting the GIS map.');
  AAdd('VNV', 'FileDoesNotExists',           '%s does not exist');
  AAdd('VNV', 'FailedToEditDiagram',          'Failed to edit the diagram');
  AAdd('VNV', 'FailedToUpdateIniFile',        '%s file does not exist and cannot be updated with the current study properties.');
  AAdd('VNV', 'NetworkVisualiser',            'Network Visualiser');
  AAdd('VNV', 'DlgCaptionIrrigation',         'Please Select Irrigation Block');
  AAdd('VNV', 'DlgCaptionIrrigationArea',     'Please Select Irrigation Area');
  AAdd('VNV', 'DlgCaptionWetland',            'Please Select Wetland');
  AAdd('VNV', 'DlgCaptionDemandCentre',       'Please Select Demand Centre');
  AAdd('VNV', 'DlgCaptionSubCatchment',       'Please Select SFRA Sub Catchment Area');
  AAdd('VNV', 'InclExistIrrigation',          'Include irrigation block already on diagram');
  AAdd('VNV', 'InclExistIrrigationArea',      'Include irrigation area already on diagram');
  AAdd('VNV', 'InclExistWetland',             'Include Wetland already on diagram');
  AAdd('VNV', 'InclExistDemandCentre',        'Include Demand Centres already on diagram');
  AAdd('VNV', 'InclExistSubCatchment',        'Include SFRA Sub Catchments already on diagram');
  AAdd('VNV', 'WetlandNoModelSupport',        'The current version of the model does not support wetlands.');
  AAdd('VNV', 'DemandCentreNoModelSupport',        'The current version of the model does not support Demand Centres.');
  AAdd('VNV', 'SFRANoModelSupport',                'The current version of the model does not support SFRA Sub Catchment Areas.');
  AAdd('VNV', 'IrrigationBlockNoModelSupport',     'The current version of the model does not support irrigation blocks.');
  AAdd('VNV', 'IrrigationAreaNoModelSupport',      'The current version of the model does not support irrigation areas.');
  AAdd('VNV', 'GroundWaterNoModelSupport',         'The current version of the model does not support Ground Water Sub-Catchments.');
  AAdd('VNV', 'PowerPlantNoModelSupport',         'The current version of the model does not support power plants.');  
  AAdd('VNV', 'ViewIrrigationBlockProperties',     'View Irrigation Block Properties');
  AAdd('VNV', 'ViewIrrigationAreaProperties',     'View Irrigation Area Properties');
  AAdd('VNV', 'ViewDiversionChannelProperties',    'View Diversion Channel Properties');
  AAdd('VNV', 'ViewReturnFlowChannelProperties',   'View Return Flow Channel Properties');
  AAdd('VNV', 'ViewIrrigationBlockClimate',        'View Climate');
  AAdd('VNV', 'ViewIrrigationBlockCropRequirement','View Crop Requirement');
  AAdd('VNV', 'ViewIrrigationBlockSoilProperties', 'View Soil Properties');
  AAdd('VNV', 'DeleteIrrigationBlockFromScenario', 'Delete Irrigation Block from Scenario');
  AAdd('VNV', 'DeleteIrrigationAreaFromScenario', 'Delete Irrigation Area from Scenario');
  AAdd('VNV', 'ViewWetlandProperties',             'View Wetland Properties');
  AAdd('VNV', 'AddRemoveReclamationFeature',        'Add/Remove Reclamation Plant Feature');
  AAdd('VNV', 'SelectConsumptiveChannel',        'Select Consumptive Channel');
  AAdd('VNV', 'AddReturnFlowChannel',        'Add a Return Flow Channel');
  AAdd('VNV', 'RemoveReturnFlowChannel',        'Remove a Return Flow Channel');
  AAdd('VNV', 'ViewInflowChannelProperties',       'View Inflow Channel Properties');
  AAdd('VNV', 'ViewSFRASubCatchmentProperties',       'View SFRA Sub Catchment Properties');
  AAdd('VNV', 'SFRADragDropError',       'Invalid action. To add SFRA Sub Catchment Areas, drag the SFRA Sub Catchment' + #13#10 +
                                         'Shape from the Stencil, and drop it on the desired Node or Reservoir.');
  AAdd('VNV', 'SFRASubCatchmentFor',       'SFRA Sub Catchment for ');
  AAdd('VNV', 'SFRASubCatchmentDeleteFailed', 'Subcatchment deletion failed.');
  AAdd('VNV', 'ViewOutFlowChannelProperties',      'View Out Flow Channel Properties');
  AAdd('VNV', 'DeleteWetlandFromScenario', 'Delete Wetland from Scenario');
  AAdd('VNV', 'ViewDemandCentreProperties', 'View Demand Centre Properties');
  AAdd('VNV', 'DeleteDemandCentreFromScenario', 'Delete Demand Centre from Scenario');
  AAdd('VNV', 'DeleteSFRAFromScenario', 'Delete SFRA Sub Catchment Area from Scenario');
  AAdd('VNV', 'MaintainReturnFlowChannels', 'Maintain Demand Centre Return Flow Channels');
  AAdd('VNV', 'MaintainSuplyChannels', 'Maintain Demand Centre Supply Channels');
  AAdd('VNV', 'RefreshDemandCentreDrawing', 'Refresh Demand Centre Drawing');

  AAdd('VNV', 'DemandCentreReclamationChannel', 'Reclamation Channel ');
  AAdd('VNV', 'DemandCentreConsumptiveChannel', 'Consumptive Channel ');
  AAdd('VNV', 'DemandCentreReturnFlowChannel', 'Return Flow Channel ');
  AAdd('VNV', 'DemandCentreSupplyChannel', 'Supply Channel ');

  AAdd('VNV', 'ViewRunConfiguration', 'View Run Configuration Data');
  AAdd('VNV', 'ViewConfigurationFiles', 'View Files Configuration Data');
  AAdd('VNV', 'ViewOutputConfiguration', 'View Output Configuration Data');
  AAdd('VNV', 'ViewReservoirAndChannelOutput', 'View Output Reservoir and Channels Data');
  AAdd('VNV', 'DrawingGISConfirmation','Do you want to create a drawing with GIS background?');
  AAdd('VNV', 'DrawingGIS','GIS');
  AAdd('VNV', 'DrawingNoGIS','Non GIS');
  AAdd('VNV', 'DrawingFileNotFound','Drawing file was not found on your hard-drive. Copying not done.');

  AAdd('VNV','ViewRunConfiguration','View Run Configuration');
  AAdd('VNV','ViewOutputConfiguration','View Output Configuration');
  AAdd('VNV','ViewCatchmentProportions','View Catchment Hydrology');
  AAdd('VNV','ViewChannelPenalties','View Channel Penalty Structures');
  AAdd('VNV','ViewReservoirPenalty','View Reservoir Penalty Structures');
  AAdd('VNV','ViewChannelArea','View Channel Area');
  AAdd('VNV','ViewReconciliationAnalysis','View Reconciliation Analysis');

  AAdd('VNV','AddAllNodes','Add all reservoirs and nodes to the drawing');
  AAdd('VNV','AddAllChannels','Add all channels to the drawing');
  AAdd('VNV','AddAllChannels','Add all channels to the drawing');
  AAdd('VNV','AddAllAverageChannelFlow','Add All Average Channel flow text labels.');
  AAdd('VNV','DeleteAllAverageChannelFlow','Delete All Average Channel flow text labels.');
  AAdd('VNV','AddAllChangeInVolume','Add All Change in Volume text labels.');
  AAdd('VNV','DeleteAllChangeInVolume','Delete All Change in Volume text labels.');
  AAdd('VNV','AddAllAverageVolume','Add All Average Volume text labels.');
  AAdd('VNV','DeleteAllAverageVolume','Delete All Average Volume text labels.');

  AAdd('VNV','AddAllAverageElevation','Add All Average Elevation text labels.');
  AAdd('VNV','DeleteAllAverageElevation','Delete All Average Elevation text labels.');
  AAdd('VNV','AddAllAverageEvaporation','Add All Average Evaporation text labels.');
  AAdd('VNV','DeleteAllAverageEvaporation','Delete All Average Evaporation text labels.');

  AAdd('VNV','AddAllAverageRainfall','Add All Average Rainfall text labels.');
  AAdd('VNV','DeleteAllAverageRainfall','Delete All Average Rainfall text labels.');
  AAdd('VNV','AddAllNetBasinRunoff','Add All Net Basin Runoff text labels.');
  AAdd('VNV','AddAllAvgNetBasinRunoff','Add All Average Net Basin Runoff text labels.');
  AAdd('VNV','DeleteAllNetBasinRunoff','Delete All Net Basin Runoff text labels.');
  AAdd('VNV','DeleteAllAvgNetBasinRunoff','Delete All Average Net Basin Runoff text labels.');
  AAdd('VNV','SetAllOutputLabelsPermanent','Set all output labels permanent');
  AAdd('VNV','AddAllReservoirPenalty','Add All reservoir penalty structures.');
  AAdd('VNV','AddAllChannelPenalty','Add All channel penalty structures.');
  AAdd('VNV','DeleteAllReservoirPenalty','Delete All reservoir penalty structures.');
  AAdd('VNV','DeleteAllChannelPenalty','Delete All channel penalty structures.');
  AAdd('VNV','DeleteAllSelectedItems','Delete selected items.');
  AAdd('VNV','AddAllPowerPlants','Add All power plants to drawing.');
  AAdd('VNV','DeleteAllPowerPlants','Delete All power plants from drawing.');

  AAdd('VNV','ViewMetaData','View Meta Data');
  AAdd('VNV','RunModel', 'Run Model');
  AAdd('VNV','ImportOutputFiles', 'Import Output Files');
  AAdd('VNV', 'ViewReservoirOutput', 'View Reservoir Output');
  AAdd('VNV', 'ViewNodeOutput', 'View Node Output');
  AAdd('VNV', 'ViewChannelOutput', 'View Channel Output');
  AAdd('VNV', 'ViewWetlandOutput', 'View Wetland Output');
  AAdd('VNV', 'CreateFirstRestriction', 'There are no drought restrictions in the system. Would you like to create one?');
  AAdd('VNV', 'PermanentColorOff', 'Allow colour refresh');
  AAdd('VNV', 'PermanentColorOn', 'Keep colour permanent');

  AAdd('VNV', 'DeleteText','Delete Output Text');
  AAdd('VNV', 'DeleteRelatedText','Delete Related Output Text');
//--
  AAdd('VNV', 'AddAllReservoirPenaltyLayer','Add All Reservoir Penalty Structures to a Layer');
  AAdd('VNV', 'ClearAllReservoirPenaltiesFromLayer','Clear All Reservoir Penalty Structures From Layers');

  AAdd('VNV', 'AddAllChannelPenaltyLayer','Add All Channel Penalty Structures to a Layer');
  AAdd('VNV', 'ClearAllChannelPenaltyLayer','Clear All Channel Penalty Structures from Layers');

  AAdd('VNV', 'AddAllChannelAndReservoirPenaltyLayer','Add All Channel And Reservoir Penalty Structures to a Layer');
  AAdd('VNV', 'ClearAllChannelAndReservoirPenaltyLayer','Clear All Channel And Reservoir Penalty Structures from Layers');

  AAdd('VNV', 'AddAllAverageChannelFlowsToALayer','Add All Average Channel Flows Text to a Layer');
  AAdd('VNV', 'ClearAllAverageChannelFlowsFromLayers','Clear All Average Channel Flows Text From Layers');

  AAdd('VNV', 'AddAllReservoirToALayer','Add All Reservoirs to a Layer');
  AAdd('VNV', 'ClearAllReservoirFromLayers','Clear All Reservoirs From Layers');

  AAdd('VNV', 'AddAllWetlandsToALayer','Add All Wetlands to a Layer');
  AAdd('VNV', 'ClearAllWetlandsFromLayers','Clear All Wetlands From Layers');

  AAdd('VNV', 'AddAllIrrigationBlocksToALayer','Add All Irrigation blocks to a Layer');
  AAdd('VNV', 'ClearAllIrrigationBlocksFromLayers','Clear All Irrigation blocks From Layers');

  AAdd('VNV', 'AddAllNodesWithoutInflowToALayer','Add All Nodes without inflow to a Layer');
  AAdd('VNV', 'ClearAllNodesWithoutInflowFromLayers','Clear All Nodes without inflow From Layers');

  AAdd('VNV', 'AddAllNodesWithInflowToALayer','Add All Nodes with inflow to a Layer');
  AAdd('VNV', 'ClearAllNodesWithInflowfromLayers','Clear All Nodes with inflow From Layers');

  AAdd('VNV', 'AddAllMinesToALayer','Add All Mines to a Layer');
  AAdd('VNV', 'ClearAllMinesFromLayers','Clear All Mines From Layers');

  AAdd('VNV', 'AddAllDemandCentresToALayer','Add All Demand Centres to a Layer');
  AAdd('VNV', 'ClearAllDemandCentresFromLayers','Clear All Demand Centres From Layers');

  AAdd('VNV', 'AddAllIrrigationAreasToALayer','Add All Irrigation Areas to a Layer');
  AAdd('VNV', 'ClearAllIrrigationAreasFromLayers','Clear All Irrigation Areas From Layers');

  AAdd('VNV', 'EnterNewLayerName','Enter New Layer Name');
  AAdd('VNV', 'LayerName','Layer Name');

  AAdd('VNV', 'AddAllPowerPlantsToALayer','Add All Power Plants to a Layer');
  AAdd('VNV', 'ClearAllPowerPlantsFromLayers','Clear All Power Plants From Layers');

  AAdd('VNV', 'ViewPowerPlantProperties', 'View Power Plant Properties');
  AAdd('VNV', 'ViewPowerPlantOutput', 'View Power Plant Output');
  AAdd('VNV', 'DeletePowerPlantFromScenario', 'Delete Power Plant from Scenario');

//-------------This part was moved from ULanguageText_ENG_General-----------------------
  {  AAdd('VNV', 'nvtGeneral',          'General');
  AAdd('VNV', 'nvtDemandFile',       'Demand file');
  AAdd('VNV', 'nvtName',             'Name');
  AAdd('VNV', 'nvtInflow',           'Inflow');
  AAdd('VNV', 'nvtNetBasinRunoff',   'Net Basin Runoff');
  AAdd('VNV', 'nvtElevation',        'Elevation');
  AAdd('VNV', 'nvtVolume',           'Volume');
  AAdd('VNV', 'nvtRainfall',         'Rainfall');
  AAdd('VNV', 'nvtEvaporation',      'Evaporation');
  AAdd('VNV', 'nvtAvgNetBasinRunoff','Average Net Basin Runoff');
  AAdd('VNV', 'nvtAvgElevation',     'Average Elevation');
  AAdd('VNV', 'nvtAvgVolume',        'Average Volume');
  AAdd('VNV', 'nvtAvgRainfall',      'Average Rainfall');
  AAdd('VNV', 'nvtAvgEvaporation',   'Average Evaporation');
  AAdd('VNV', 'nvtPercFull',         'Percentage full');
  AAdd('VNV', 'nvtChannelFlow',      'Channel flow');
  AAdd('VNV', 'nvtReservoirStorageChange',      'Change in volume');
  AAdd('VNV', 'nvtAvgChannelFlow',   'Average Channel flow');
  AAdd('VNV', 'DlgCaptionReservoir', 'Please Select Reservoir');
  AAdd('VNV', 'DlgCaptionNode',      'Please Select Node');
  AAdd('VNV', 'DlgCaptionChannel',   'Please Select Channel');
  AAdd('VNV', 'DlgCaptionMine',      'Please Select Mine');
  AAdd('VNV', 'DlgCaptionGroundWater','Please Select Ground Water');
  AAdd('VNV', 'DlgCaptionPowerPlant','Please Select Power Plant');
  AAdd('VNV', 'UseExisting',         'Use existing');
  AAdd('VNV', 'CreateNew',           'Create new');
  AAdd('VNV', 'SelectReservoir',     'Please select a reservoir - or Cancel.');
  AAdd('VNV', 'SelectIrrigation',    'Please select an irrigation block - or Cancel.');
  AAdd('VNV', 'SelectIrrigationArea','Please select an irrigation area - or Cancel.');
  AAdd('VNV', 'SelectWetland',       'Please select a Wetland - or Cancel.');
  AAdd('VNV', 'SelectDemandCentre',  'Please select a Demand Centre - or Cancel.');
  AAdd('VNV', 'SelectNode',          'Please select a node - or Cancel.');
  AAdd('VNV', 'SelectChannel',       'Please select a channel - or Cancel.');
  AAdd('VNV', 'SelectResPenalty',    'Please select a reservoir penalty - or Cancel.');
  AAdd('VNV', 'SelectChanPenalty',   'Please select a channel penalty - or Cancel.');
  AAdd('VNV', 'SelectMine',          'Please select a mine - or Cancel.');
  AAdd('VNV', 'SelectGroundWater',          'Please select a Ground Water Sub-Catchment - or Cancel.');
  AAdd('VNV', 'SelectDroughtRestriction',  'Please select a drought restriction - or Cancel.');
  AAdd('VNV', 'SelectPowerPlant',          'Please select a power plant - or Cancel.');
  AAdd('VNV', 'MultiOutputSelectorErr',    'The diagram already contains an output selector.');
  AAdd('VNV', 'InclExistReservoirs',       'Include reservoirs already on diagram');
  AAdd('VNV', 'InclAllRestriction',        'Show all drought restrictions');
  AAdd('VNV', 'InclExistNodes',            'Include nodes already on diagram');
  AAdd('VNV', 'InclExistChannels',         'Include channels already on diagram');
  AAdd('VNV', 'InclExistResPenalties',     'Include reservoir penalties already on diagram');
  AAdd('VNV', 'InclExistChanPenalties',    'Include channel penalties already on diagram');
  AAdd('VNV', 'InclExistMines',            'Include mines already on diagram');
  AAdd('VNV', 'InclExistGroundWater',      'Include Ground Water already on diagram');
  AAdd('VNV', 'InclExistPowerPlant',       'Include Power Plant already on diagram');
  AAdd('VNV', 'AddReservoirsFirst',        'Add reservoirs to the drawing first');
  AAdd('VNV', 'AddChannelsFirst',          'Add channels to the drawing first');
  AAdd('VNV', 'NoUserRights',              'This action is disallowed because the current user does not have rights to edit data.');
  AAdd('VNV', 'ScenarioLocked',            'This action is disallowed because the scenario is locked by another user.');
  AAdd('VNV', 'WithInflow',                'With inflow');
  AAdd('VNV', 'WithoutInflow',             'Without inflow');
  AAdd('VNV', 'DuplicateReservoir',        'Reservoir %d is already on the diagram. Are you sure you want to add a duplicate?');
  AAdd('VNV', 'DuplicateNode',             'Node %d is already on the diagram. Are you sure you want to add a duplicate?');
  AAdd('VNV', 'DuplicateChannel',          'Channel %d is already on the diagram. Are you sure you want to add a duplicate?');
  AAdd('VNV', 'DuplicateResPenalty',       'Reservoir %d penalty is already on the diagram. Are you sure you want to add a duplicate?');
  AAdd('VNV', 'DuplicateChanPenalty',      'Channel %d penalty is already on the diagram. Are you sure you want to add a duplicate?');
  AAdd('VNV', 'DuplicateIrrBlock',         'Irrigation block %d is already on the diagram. Are you sure you want to add a duplicate?');
  AAdd('VNV', 'DuplicateIrrArea',          'Irrigation area %d is already on the diagram. Are you sure you want to add a duplicate?');
  AAdd('VNV', 'DuplicateWetland',          'Wetland %d is already on the diagram. Are you sure you want to add a duplicate?');
  AAdd('VNV', 'DuplicateDemandCentre',     'Demand Centre %d is already on the diagram. Are you sure you want to add a duplicate?');
  AAdd('VNV', 'DuplicateMine',             'Mine %d is already on the diagram. Are you sure you want to add a duplicate?');
  AAdd('VNV', 'DuplicateGroundWater',      'Ground Water Sub-Catchment %d is already on the diagram. Are you sure you want to add a duplicate?');
  AAdd('VNV', 'DuplicatePowerPlant',       'Power Plant %d is already on the diagram. Are you sure you want to add a duplicate?');
  AAdd('VNV', 'AddTextLabel',              'Add text label');
  AAdd('VNV', 'AssignTextToElement',       'Assign text label to network element.');
  AAdd('VNV', 'Reservoir',                 'Reservoir');
  AAdd('VNV', 'Node',                      'Node');
  AAdd('VNV', 'Channel',                   'Channel');
  AAdd('VNV', 'Wetland',                   'Wetland');
  AAdd('VNV', 'TextType',                  'Text type :');
  AAdd('VNV', 'SelectNetworkElement',      'Please select the network element to which the text label must be assigned.');
  AAdd('VNV', 'ReservoirOptions',          'Reservoir options');
  AAdd('VNV', 'NodeOprions',               'Node options');
  AAdd('VNV', 'ChannelOptions',            'Channel options');
  AAdd('VNV', 'Unassigned',                'Unassigned');
  AAdd('VNV', 'ChangeUpstreamNode',        'This action will change the upstream node number of channel %d ' +
                                           ' from %d to %d. This change will not only be applied to the diagram but ' +
                                           ' also to the scenario data. Are you sure you want to continue?');
  AAdd('VNV', 'ChangeDownstreamNode',      'This action will change the downstream node number of channel %d ' +
                                           ' from %d to %d. This change will not only be applied to the diagram but ' +
                                           ' also to the scenario data. Are you sure you want to continue?');
  AAdd('VNV', 'MasterControl',             'Master Control');
  AAdd('VNV', 'PowerChannel',              'Power');
  AAdd('VNV', 'PowerSpill',                'Power Spill');
  AAdd('VNV', 'IrrigationDiversion',       'Irrigation Diversion');
  AAdd('VNV', 'WetlandInflow',             'Wetland Inflow');
  AAdd('VNV', 'IrrigationConsumption',     'Irrigation Consumption');
  AAdd('VNV', 'WetlandOutflow',            'Wetland Outflow');
  AAdd('VNV', 'IrrigationReturnFlow',      'Irrigation Return Flow');
  AAdd('VNV', 'Diversion',                 'Diversion');
  AAdd('VNV', 'MinimumFlow',               'Minimum Flow');
  AAdd('VNV', 'Loss',                      'Loss');
  AAdd('VNV', 'MinMax',                    'Multi Purpose Min Max');
  AAdd('VNV', 'Pumping',                   'Pumping');
  AAdd('VNV', 'SpecifiedInflow',           'Specified Inflow');
  AAdd('VNV', 'SpecifiedDemand',           'Specified Demand');
  AAdd('VNV', 'GeneralFlow',               'General Flow');
  AAdd('VNV', 'MayNotAddInflow',           'Inflow symbols may not be added to drawing manually. ' + #10#13 +
                                           'They will be added automatically when a reservoir or ' + #10#13 +
                                           'node with inflow is added.');
  AAdd('VNV', 'MayNotDeleteInflow',        'Inflow symbols may not be deleted from drawing manually. ' + #10#13 +
                                           'They will be deleted automatically when a reservoir or ' + #10#13 +
                                           'node with inflow is deleted.');
  AAdd('VNV', 'ViewReservoirProperties',   'View Reservoir Properties');
  AAdd('VNV', 'ViewMineProperties',        'View Mine Properties');
  AAdd('VNV', 'ViewGroundWaterProperties', 'View Ground Water Properties');
  AAdd('VNV', 'ViewMineToRiver',           'View Mine to River Properties');
  AAdd('VNV', 'ViewMineOpenCastPits',      'View Mine''s Open Cast Pits Properties');
  AAdd('VNV', 'ViewUndergroundSection',    'View Mine''s Underground Section Properties');
  AAdd('VNV', 'ViewDiscardSlurryDump',     'View Mine''s Discard/Slurry Dump Properties');
  AAdd('VNV', 'ViewReservoirPenalty',      'View Reservoir Penalty');
  AAdd('VNV', 'ConfigReservoirPenalty',    'Add/Delete Reservoir Penalty');
  AAdd('VNV', 'ViewReservoirCatchment',    'View Reservoir Catchment Hydrology');
  AAdd('VNV', 'ConfigureReservoirCatchment','Add/Delete Reservoir Catchment Hydrology');
  AAdd('VNV', 'ViewReservoirElevation',    'View Reservoir Elevation');
  AAdd('VNV', 'ViewReservoirEvaporation',  'View Reservoir Evaporation');
  AAdd('VNV', 'ViewReservoirPhysical',     'View Reservoir Physical Characteristics');
  AAdd('VNV', 'ViewReservoirwaterBalance', 'View Reservoir Water Balance');
  AAdd('VNV', 'ViewReservoirOutputDataGrid', 'View Reservoir Output Data Grid');
  AAdd('VNV', 'ViewReservoirOutputDataGraph','View Reservoir Output Data Graph');
  AAdd('VNV', 'DeleteReservoirFromScenario', 'Delete Reservoir from Scenario');
  AAdd('VNV', 'DeleteMineFromScenario',      'Delete Mine from Scenario');
  AAdd('VNV', 'DeleteGroundWaterFromScenario','Delete Ground Water from Scenario');
  AAdd('VNV', 'MenuCancelOption',            'Cancel...');
  AAdd('VNV', 'SetTextPermenant',            'Set Permenant');
  AAdd('VNV', 'RemoveTextPermenant',         'Allow Text Refresh');
  AAdd('VNV', 'ViewNodeProperties',          'View Node Properties');
  AAdd('VNV', 'ViewTextProperties',          'View Text Properties');
  AAdd('VNV', 'NodeOptions',                 'View Node Properties');
  AAdd('VNV', 'ViewNodeCatchment',           'View Node Catchment Proportions');
  AAdd('VNV', 'DeleteNodeFromScenario',      'Delete Node from Scenario');
  AAdd('VNV', 'ViewChannelProperties',       'View Channel Properties');
  AAdd('VNV', 'ViewChannelPenalty',          'View Channel Penalty');
  AAdd('VNV', 'ConfigChannelPenalty',        'Add/Delete Channel Penalty');
  AAdd('VNV', 'ViewMinMaxFeature',           'View Min-max Feature');
  AAdd('VNV', 'ViewLossFeature',             'View Loss Feature');
  AAdd('VNV', 'ViewMinimumFeature',          'View Minimum Flow Feature');
  AAdd('VNV', 'ViewDiversionFeature',        'View Diversion Feature');
  AAdd('VNV', 'ViewPumpingFeature',          'View Pumping Feature');
  AAdd('VNV', 'ViewSpecifiedDemandFeature',  'View Specified Demand Feature');
  AAdd('VNV', 'ViewSpecifiedInflowFeature',  'View Specified Inflow Feature');
  AAdd('VNV', 'ViewPhysicalFlowConstraint',  'View Physical Flow Constraint');
  AAdd('VNV', 'ViewIFRFeature',              'View IFR Feature');
  AAdd('VNV', 'ViewWaterDemandFeature',      'View Water Demand Feature');
  AAdd('VNV', 'ViewMasterControlFeature',    'View Master Control Feature');
  AAdd('VNV', 'ViewIrrigationArea',          'View Irrigation Area');
  AAdd('VNV', 'ViewWetland',                 'View Wetland');
  AAdd('VNV', 'ViewPowerPlant',              'View Power Plant');
  AAdd('VNV', 'SplitChannel',                'Split Channel');
  AAdd('VNV', 'DeleteChannelFromScenario',   'Delete Channel from Scenario');
  AAdd('VNV', 'StencilDoesNotExist',         'Stencil file %s does not exist.');
  AAdd('VNV', 'Unassgined',                  'Unassigned');
  AAdd('VNV', 'RefreshShapes',               'Refreshing shapes...Please wait');
  AAdd('VNV', 'NrShapesUpdated',             'Nr of shapes updated = ');
  AAdd('VNV', 'OfTotal',                     ' of ');
  AAdd('VNV', 'DrawingReadOnly',             'Drawing Read&Only');
  AAdd('VNV', 'CopyDrawing',                 'Co&py Drawing');
  AAdd('VNV', 'PasteDrawing',                'P&aste Drawing');
  AAdd('VNV', 'ConfirmDeleteDrawing',        'Delete selected drawing?');
  AAdd('VNV', 'EnterGroupName',              'Enter drawing group name');
  AAdd('VNV', 'GroupName',                   'Drawing Group Name');
  AAdd('VNV', 'GroupNameEmpty',              'Drawing group name cannot be empty. Action not completed.');
  AAdd('VNV', 'GroupNameExists',             'Drawing group name already exists. Please specify a new one.');
  AAdd('VNV', 'SelectGroup',                 'Please select a drawing group node first.');
  AAdd('VNV', 'EnterNewGroupName',           'Enter new drawing group name');
  AAdd('VNV', 'NewGroupName',                'New Drawing Group Name');
  AAdd('VNV', 'ConfirmDeleteGroup',          'Group contains at least one drawing.'+#13+#10+'Deleting this group will delete those drawings as well.'+#13+#10+'Continue deleting group and all its drawings?');
  AAdd('VNV', 'EnterNewDrawingName',         'Enter new drawing name');
  AAdd('VNV', 'NewDrawingName',              'New Drawing Name');
  AAdd('VNV', 'DrawingNameEmpty',            'Drawing name cannot be empty. Action not completed.');
  AAdd('VNV', 'DrawingNameExistsInGroup',    'New drawing name already exists in the group.');
  AAdd('VNV', 'DrawingNameExists',           'Drawing name already exists. Please specify a new one.');
  AAdd('VNV', 'EnterDrawingName',            'Enter drawing name');
  AAdd('VNV', 'DrawingName',                 'Drawing Name');
  AAdd('VNV', 'SelectDrawing',               'Please select a drawing node first.');
  AAdd('VNV', 'FileDoesNotExists',           '%s does not exist');
  AAdd('VNV', 'AttemptToStartVisio',
                    'An attempt will be made to start VISIO in order to edit the diagram.' + #13+#10 +
                    'On success, this application will be suspended and will only resume ' + #13#10 +
                    'after editing has been finished.' + #13#10#13#10 +
                    'VERY IMPORTANT : ' + #13#10 +
                    'Once VISIO has been started, a message regarding establishment ' + #13#10 +
                    'of a connection to the COM server will be displayed.' + #13#10 +
                    'If such a message is NOT displayed, the VISIO settings for macro ' + #13#10 +
                    'security levels are most probably incorrect.' + #13#10#13#10 +
                    'WRMF will attempt to update Visio''s Macro Security Level to allow' + #13#10 +
                    'WRMF to communicate with Visio.');
  AAdd('VNV', 'VisioUpdateMacroSecLevelFailed',
                    'WRMF failed to update Visio''s Macro Security Settings automatically' + #13#10 +
                    'Please change the Macro Security Levels manually. Do so by clicking ' + #13#10 +
                    'on the Tools menu (in Visio), select Macros, select ' + #13#10 +
                    'Security and set the level to Low.' + #13#10 +
                    'Exit VISIO to return to the IMS and edit the diagram again.');

  AAdd('VNV', 'GISMapError',           'An error occurred while exporting the GIS map.');
  AAdd('VNV', 'FileDoesNotExists',           '%s does not exist');
  AAdd('VNV', 'FailedToEditDiagram',          'Failed to edit the diagram');
  AAdd('VNV', 'FailedToUpdateIniFile',        '%s file does not exist and cannot be updated with the current study properties.');
  AAdd('VNV', 'NetworkVisualiser',            'Network Visualiser');
  AAdd('VNV', 'DlgCaptionIrrigation',         'Please Select Irrigation Block');
  AAdd('VNV', 'DlgCaptionIrrigationArea',     'Please Select Irrigation Area');
  AAdd('VNV', 'DlgCaptionWetland',            'Please Select Wetland');
  AAdd('VNV', 'DlgCaptionDemandCentre',       'Please Select Demand Centre');
  AAdd('VNV', 'DlgCaptionSubCatchment',       'Please Select SFRA Sub Catchment Area');
  AAdd('VNV', 'InclExistIrrigation',          'Include irrigation block already on diagram');
  AAdd('VNV', 'InclExistIrrigationArea',      'Include irrigation area already on diagram');
  AAdd('VNV', 'InclExistWetland',             'Include Wetland already on diagram');
  AAdd('VNV', 'InclExistDemandCentre',        'Include Demand Centres already on diagram');
  AAdd('VNV', 'InclExistSubCatchment',        'Include SFRA Sub Catchments already on diagram');
  AAdd('VNV', 'WetlandNoModelSupport',        'The current version of the model does not support wetlands.');
  AAdd('VNV', 'DemandCentreNoModelSupport',        'The current version of the model does not support Demand Centres.');
  AAdd('VNV', 'SFRANoModelSupport',                'The current version of the model does not support SFRA Sub Catchment Areas.');
  AAdd('VNV', 'IrrigationBlockNoModelSupport',     'The current version of the model does not support irrigation blocks.');
  AAdd('VNV', 'IrrigationAreaNoModelSupport',      'The current version of the model does not support irrigation areas.');
  AAdd('VNV', 'GroundWaterNoModelSupport',         'The current version of the model does not support Ground Water Sub-Catchments.');
  AAdd('VNV', 'PowerPlantNoModelSupport',         'The current version of the model does not support power plants.');
  AAdd('VNV', 'ViewIrrigationBlockProperties',     'View Irrigation Block Properties');
  AAdd('VNV', 'ViewIrrigationAreaProperties',     'View Irrigation Area Properties');
  AAdd('VNV', 'ViewDiversionChannelProperties',    'View Diversion Channel Properties');
  AAdd('VNV', 'ViewReturnFlowChannelProperties',   'View Return Flow Channel Properties');
  AAdd('VNV', 'ViewIrrigationBlockClimate',        'View Climate');
  AAdd('VNV', 'ViewIrrigationBlockCropRequirement','View Crop Requirement');
  AAdd('VNV', 'ViewIrrigationBlockSoilProperties', 'View Soil Properties');
  AAdd('VNV', 'DeleteIrrigationBlockFromScenario', 'Delete Irrigation Block from Scenario');
  AAdd('VNV', 'DeleteIrrigationAreaFromScenario', 'Delete Irrigation Area from Scenario');
  AAdd('VNV', 'ViewWetlandProperties',             'View Wetland Properties');
  AAdd('VNV', 'AddRemoveReclamationFeature',        'Add/Remove Reclamation Plant Feature');
  AAdd('VNV', 'SelectConsumptiveChannel',        'Select Consumptive Channel');
  AAdd('VNV', 'AddReturnFlowChannel',        'Add a Return Flow Channel');
  AAdd('VNV', 'RemoveReturnFlowChannel',        'Remove a Return Flow Channel');
  AAdd('VNV', 'ViewInflowChannelProperties',       'View Inflow Channel Properties');
  AAdd('VNV', 'ViewSFRASubCatchmentProperties',       'View SFRA Sub Catchment Properties');
  AAdd('VNV', 'SFRADragDropError',       'Invalid action. To add SFRA Sub Catchment Areas, drag the SFRA Sub Catchment' + #13#10 +
                                         'Shape from the Stencil, and drop it on the desired Node or Reservoir.');
  AAdd('VNV', 'SFRASubCatchmentFor',       'SFRA Sub Catchment for ');
  AAdd('VNV', 'SFRASubCatchmentDeleteFailed', 'Subcatchment deletion failed.');
  AAdd('VNV', 'ViewOutFlowChannelProperties',      'View Out Flow Channel Properties');
  AAdd('VNV', 'DeleteWetlandFromScenario', 'Delete Wetland from Scenario');
  AAdd('VNV', 'ViewDemandCentreProperties', 'View Demand Centre Properties');
  AAdd('VNV', 'DeleteDemandCentreFromScenario', 'Delete Demand Centre from Scenario');
  AAdd('VNV', 'DeleteSFRAFromScenario', 'Delete SFRA Sub Catchment Area from Scenario');
  AAdd('VNV', 'MaintainReturnFlowChannels', 'Maintain Demand Centre Return Flow Channels');
  AAdd('VNV', 'MaintainSuplyChannels', 'Maintain Demand Centre Supply Channels');
  AAdd('VNV', 'RefreshDemandCentreDrawing', 'Refresh Demand Centre Drawing');

  AAdd('VNV', 'DemandCentreReclamationChannel', 'Reclamation Channel ');
  AAdd('VNV', 'DemandCentreConsumptiveChannel', 'Consumptive Channel ');
  AAdd('VNV', 'DemandCentreReturnFlowChannel', 'Return Flow Channel ');
  AAdd('VNV', 'DemandCentreSupplyChannel', 'Supply Channel ');

  AAdd('VNV', 'ViewRunConfiguration', 'View Run Configuration Data');
  AAdd('VNV', 'ViewConfigurationFiles', 'View Files Configuration Data');
  AAdd('VNV', 'ViewOutputConfiguration', 'View Output Configuration Data');
  AAdd('VNV', 'ViewReservoirAndChannelOutput', 'View Output Reservoir and Channels Data');
  AAdd('VNV', 'DrawingGISConfirmation','Do you want to create a drawing with GIS background?');
  AAdd('VNV', 'DrawingGIS','GIS');
  AAdd('VNV', 'DrawingNoGIS','Non GIS');
  AAdd('VNV', 'DrawingFileNotFound','Drawing file was not found on your hard-drive. Copying not done.');

  AAdd('VNV','ViewRunConfiguration','View Run Configuration');
  AAdd('VNV','ViewOutputConfiguration','View Output Configuration');
  AAdd('VNV','ViewCatchmentProportions','View Catchment Hydrology');
  AAdd('VNV','ViewChannelPenalties','View Channel Penalty Structures');
  AAdd('VNV','ViewReservoirPenalty','View Reservoir Penalty Structures');
  AAdd('VNV','ViewChannelArea','View Channel Area');
  AAdd('VNV','ViewReconciliationAnalysis','View Reconciliation Analysis');}
    
end;

end.
