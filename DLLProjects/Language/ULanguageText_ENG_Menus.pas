//
//  UNIT      : Contains language text.
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/01/23
//  COPYRIGHT : Copyright © 2003 DWAF
//
unit ULanguageText_ENG_Menus;

interface

type TTextItemAddFunction = procedure (AContext, AConstant, AText: string) of object;

procedure LoadLanguageText(AAdd: TTextItemAddFunction);

implementation

procedure LoadLanguageText(AAdd: TTextItemAddFunction);
const OPNAME = 'LoadLanguageText';
begin
  AAdd('MenuCaption','Add','Add');
  AAdd('MenuCaption','ApplyFileSelection','Apply File Selection');
  AAdd('MenuCaption','ClearModelData','Clear Model Data');
  AAdd('MenuCaption','CompareFiles','Compare Data Files');
  AAdd('MenuCaption','CopyToClipboard','Copy To Clipboard');
  AAdd('MenuCaption','Data','Data');
  AAdd('MenuCaption','Delete','Delete');
  AAdd('MenuCaption','Edit','Edit');
  AAdd('MenuCaption','EditCopy','Copy');
  AAdd('MenuCaption','EditFind','Find');
  AAdd('MenuCaption','EditCut','Cut');
  AAdd('MenuCaption','EditPaste','Paste');
  AAdd('MenuCaption','EditUndo','Undo');
  AAdd('MenuCaption','EditSelectAll','Select All');
  AAdd('MenuCaption','Exit','Exit');
  AAdd('MenuCaption','ExportToFile','Export To File');
  AAdd('MenuCaption','ExportFiles ','Export Files');
  AAdd('MenuCaption','ExportFile ','Export Current File');
  AAdd('MenuCaption','File','File');
  AAdd('MenuCaption','FirstRecord','First Record');
  AAdd('MenuCaption','Functions','Functions');
  AAdd('MenuCaption','GenerateFiles','Generate File Names');
  AAdd('MenuCaption','Help','Help');
  AAdd('MenuCaption','HelpAbout','Help About');
  AAdd('MenuCaption','HelpContents','Help Contents');
  AAdd('MenuCaption','ReadMe','View ReadMe');
  AAdd('MenuCaption','ReleaseNote','View Release Note');
  AAdd('MenuCaption','HelpWhatIsThis','What''s this?');
  AAdd('MenuCaption','HelpTrainingManual','Training Manual');
  AAdd('MenuCaption','HelpTrainingMaterial','Training Material');
  AAdd('MenuCaption','HelpUserManual','User Manual');
  AAdd('MenuCaption','HelpWRYMUserGuide','WRYM User Guide ');
  AAdd('MenuCaption','HelpWRYMUserGuideOld','WRYM User Guide (old) ');
  AAdd('MenuCaption','HelpWRYMProceduralGuide','WRYM Procedural Guide');
  AAdd('MenuCaption','HelpManualSep','-');
  AAdd('MenuCaption','HelpExampleFiles','Example Files');
  AAdd('MenuCaption','HelpOpenFile','Open File');

  AAdd('MenuCaption','HelpAbout','Help About');
  AAdd('MenuCaption','HelpWhatIsThis','What''s this?');

  AAdd('MenuCaption','HelpDailyDiversionUserGuide','User Guide');
  AAdd('MenuCaption','HelpHydrologyParameterTables','Hydrology Parameter Tables');
  AAdd('MenuCaption','HelpIFRUserGuide','User Guide');
  AAdd('MenuCaption','HelpRWHUserGuide','User Guide');

  AAdd('MenuCaption','HelpRainfallCLASSRAndPATCHRMethodology','CLASSR And PATCHR Methodology');
  AAdd('MenuCaption','HelpRainfallClassROutput','ClassR Output');
  AAdd('MenuCaption','HelpRainfallPatchROutput','PatchR Output');
  AAdd('MenuCaption','HelpRainfallSAWSNumbering','SAWS Numbering');
  AAdd('MenuCaption','HelpRainfallTrainingMaterial','Training Material');
  AAdd('MenuCaption','HelpRainfallUserGuide','User Guide');

  AAdd('MenuCaption','HelpStomsaUserGuide','User Guide');

  AAdd('MenuCaption','HelpWRMFReleaseNote','WRYM Release Note');

  AAdd('MenuCaption','HelpWRPMParameterTables','WRPM Parameter Tables');
  AAdd('MenuCaption','HelpWRPMReleaseNote','WRPM Release Note');
  AAdd('MenuCaption','HelpWRPMUserGuide','User Guide');

  AAdd('MenuCaption','HelpWRYMParameterTables','ParameterTables');
  AAdd('MenuCaption','HelpWRYMProceduralManual','Procedural Manual');
  AAdd('MenuCaption','HelpWRYMTrainingMaterial','Training Material');
  AAdd('MenuCaption','HelpWRYMUserGuide','User Guide');

  AAdd('MenuCaption','HelpUserGuide','User Guide');
  AAdd('MenuCaption','HelpTrainingManual','Training Manual');
  AAdd('MenuCaption','HelpInputParameterTables','Files Parameter Tables');
  AAdd('MenuCaption','HelpHydroParameterTables','Hydrology Parameter Tables');
  AAdd('MenuCaption','HelpWRYMUserGuideOld','WRYM User Guide (old) ');

  AAdd('MenuCaption','HelpTrainingMaterial','Training Material');
  AAdd('MenuCaption','HelpUserManual','User Manual');

  AAdd('MenuCaption','HelpWRYMUserGuide','WRYM User Guide ');
  AAdd('MenuCaption','HelpWRYMUserGuideOld','WRYM User Guide (old) ');
  AAdd('MenuCaption','HelpWRYMProceduralGuide','WRYM Procedural Guide');
  AAdd('MenuCaption','HelpPlanningUserGuide','Planning User Guide');
  AAdd('MenuCaption','HelpStomsaUserGuide','Stomsa User Guide');

  AAdd('MenuCaption','RainIMSUserGuide','Rain IMS User Guide');
  AAdd('MenuCaption','RainIMSPatchROutput','Summary of PatchR Output');
  AAdd('MenuCaption','RainIMSClassROutput','Summary of ClassR Output');
  AAdd('MenuCaption','RainIMSSAWSNumbering','SAWS Numbering');
  AAdd('MenuCaption','RainIMSCLASSRAndPATCHRMethodology','CLASSR And PATCHR Methodology');

  AAdd('MenuCaption','PasteFromExcel','Paste from Excel');
  AAdd('MenuCaption','CopyColumnData','Copy column data');
  AAdd('MenuCaption','PasteColumnData','Paste column data');

  AAdd('MenuCaption','ImportFiles','Import Files');
  AAdd('MenuCaption','ImportFile','Import Current File');
  AAdd('MenuCaption','ImportMonthlyDamWaterLevels','Import Monthly Dam Water Levels File');
  AAdd('MenuCaption','ImportSumOut','Import SUM.OUT File');

  AAdd('MenuCaption','CreateIFRSite','Create IFR Site');
  AAdd('MenuCaption','DeleteIFRSite','Delete IFR Site');
  AAdd('MenuCaption','SaveIFRSite','Save IFR Site data');

  AAdd('MenuCaption','Hydrology','Hydrology');
  AAdd('MenuCaption','Rainfall','Rainfall');
  AAdd('MenuCaption','YRC','YRC');
  AAdd('MenuCaption','LastRecord','Last Record');
  AAdd('MenuCaption','LoadModelDataFromDB','Load Model Data From Database');
  AAdd('MenuCaption','LoadModelDataFromFiles','Load Model Data From Files');
  AAdd('MenuCaption','LogOff','Log Off');
  AAdd('MenuCaption','LogOn','Log On');
  AAdd('MenuCaption','Model','Model');
  AAdd('MenuCaption','NextRecord','Next Record');
  AAdd('MenuCaption','NVAddChannel','Add channel to drawing…');
  AAdd('MenuCaption','NVAddElementSep','-');
  AAdd('MenuCaption','NVAddNode','Add node to drawing…');
  AAdd('MenuCaption','NVAddReservoir','Add reservoir to drawing…');
//  AAdd('MenuCaption','NVChannel','New Channel Channel');
  AAdd('MenuCaption','NVChannelInst','Add Channel instance');
  AAdd('MenuCaption','NVCopyDrawing','Copy Drawing');
  AAdd('MenuCaption','NVCopyElement','Copy Element');
  AAdd('MenuCaption','NVDefaultZoom','Default Zoom');
  AAdd('MenuCaption','NVDeleteDrawing','Delete Drawing');
  AAdd('MenuCaption','NVRenameDrawing','Rename Drawing');
  AAdd('MenuCaption','NVRenameDrawingGroup','Rename Drawing Group');
  AAdd('MenuCaption','NVDeleteDrawingGroup','Delete Drawing Group');
  AAdd('MenuCaption','NVDrawingGroupSepEnd','-');
  AAdd('MenuCaption','NVAlignDrawing','Set GIS Positions to Schematic');
  AAdd('MenuCaption','NVAlignDrawingSep','-');
  AAdd('MenuCaption','NVDrawingSepEnd','-');
  AAdd('MenuCaption','NVElementNewPenalty','Create Master Penalty Table');
  AAdd('MenuCaption','NVElementSepEnd','-');
  AAdd('MenuCaption','NVElementSepStart','-');
  AAdd('MenuCaption','NVFindElement','Find Element');
  AAdd('MenuCaption','NVFullScreen','Show Network FullScreen');
  AAdd('MenuCaption','NVNewDrawing','New Drawing');
  AAdd('MenuCaption','NVModifyDrawing','Modify Drawing');
  AAdd('MenuCaption','NVNewDrawingGroup','New Drawing Group');
  AAdd('MenuCaption','NVOpenDrawing','Open Drawing');
  AAdd('MenuCaption','NVOpenDrawingGroup','Open Drawing Group');
  AAdd('MenuCaption','NVPasteElement','Paste Element');
  AAdd('MenuCaption','NVProperties','Element Properties');
  AAdd('MenuCaption','NVRenameDrawing','Rename Drawing');
  AAdd('MenuCaption','NVRenameDrawingGroup','Rename Drawing Group');
  AAdd('MenuCaption','NVSaveDrawing','Save Drawing');

  AAdd('MenuCaption','NVZoomSepStart','-');
  AAdd('MenuCaption','NVZoomIn','Zoom In');
  AAdd('MenuCaption','NVZoomOut','Zoom Out');
  AAdd('MenuCaption','NVZoomSepEnd','-');

  AAdd('MenuCaption','NVShow','Show');
  AAdd('MenuCaption','NVViewPenChannel','Show Channel Penalties');
  AAdd('MenuCaption','NVViewPenReservoir','Show Reservoir Penalties');
  AAdd('MenuCaption','NVViewNode','Show Nodes');
  AAdd('MenuCaption','NVViewReservoir','Show Reservoirs');
  AAdd('MenuCaption','NVViewChannel','Show Channels');
  AAdd('MenuCaption','NVViewMasterCaption','Show Element Captions');
  AAdd('MenuCaption','NVViewAdditionalCaption','Show User Captions ');
  AAdd('MenuCaption','NVViewChannelNode','Show Channel Segment Nodes');
  AAdd('MenuCaption','NVViewChannelSegment','Show Channel Segments');
  AAdd('MenuCaption','NVViewToggleSepEnd','-');

  AAdd('MenuCaption','NVViewGoto','Goto');
  AAdd('MenuCaption','ChannelDetailsHead','Channel Details');
  AAdd('MenuCaption','ChannelDetailsHead1','Channel Penalties');
  AAdd('MenuCaption','TimeSeries.Channnels','Time Series');
  AAdd('MenuCaption','NodesDetails','Nodes With InFlow');
  AAdd('MenuCaption','NodesWithoutInFlowHeading','Nodes Without InFlow');
  AAdd('MenuCaption','TimeSeries.Nodes','Time Series');
  AAdd('MenuCaption','HydrologyFilePlotAFF','AFF Hydrology File Plot');
  AAdd('MenuCaption','HydrologyFilePlotINC','INC Hydrology File Plot');
  AAdd('MenuCaption','HydrologyFilePlotIRR','IRR Hydrology File Plot');
  AAdd('MenuCaption','HydrologyFilePlotRAN','RAN Hydrology File Plot');
  AAdd('MenuCaption','AreaCapValues','Area Capacity Relationship');
  AAdd('MenuCaption','ReservoirAreaByElevationPlot','Reservoir Area By Elevation');
  AAdd('MenuCaption','ReservoirAreaPlot','Reservoir Area Plot');
  AAdd('MenuCaption','ReservoirDetails','Reservoir Details');
  AAdd('MenuCaption','ReservoirElevationPlot','Reservoir Elevation Plot');
  AAdd('MenuCaption','ReservoirPenaltyLev','Penalties and Levels');
  AAdd('MenuCaption','ReservoirPlots','Reservoir Characteristics Plots');
  AAdd('MenuCaption','ReservoirVolumeByElevationPlot','Reservoir Volume By Elevation');
  AAdd('MenuCaption','ReservoirVolumePlot','Reservoir Volume');
  AAdd('MenuCaption','ReservoirPlots','Reservoir Characteristics Plots');
  AAdd('MenuCaption','NVViewGotoSepEnd','-');

  AAdd('MenuCaption','Print','Print');
  AAdd('MenuCaption','PrintPreview','Print Preview');
  AAdd('MenuCaption','PrintSettings','Print Settings');
  AAdd('MenuCaption','PriorRecord','Prior Record');
  AAdd('MenuCaption','Reports','Reports');
  AAdd('MenuCaption','AddReport','Add a Report');
  AAdd('MenuCaption','DeleteReport','Delete a Report');
  AAdd('MenuCaption','EditReport','Edit a Report');

  AAdd('MenuCaption','RotateView','Rotate View');
  AAdd('MenuCaption','RunModel','Execute model');

  AAdd('MenuCaption','RDSelectByRectangle','Select By Rectangle');
  AAdd('MenuCaption','RDSelectByDistance', 'Select By Distance');
  AAdd('MenuCaption','RDSelectByBufferSepEnd','-');
  AAdd('MenuCaption','RDSelectByStationName','Select By Station Name');
  AAdd('MenuCaption','RDSelectByStationNumber','Select By Station Number');
  AAdd('MenuCaption','RDSelectBySAWSBlocks','Select By SAWS Blocks');
  AAdd('MenuCaption','RDSelectByQuatenary','Select By Quatenary Catchments');
  AAdd('MenuCaption','RDStationNumberSepEnd','-');

  AAdd('MenuCaption','RDExtractDailyData','Extract Daily Data');
  AAdd('MenuCaption','RDExtractMonthlyData','Extract Monthly Data');
  AAdd('MenuCaption','RDFileExtractSepEnd','-');
  AAdd('MenuCaption','RDFileSepEnd','-');
  AAdd('MenuCaption','RDFromSelectionFile','Select From a selection file');
  AAdd('MenuCaption','RDFromSelectionFileSepEnd','-');

  AAdd('MenuCaption','RDOptions','Options');
  AAdd('MenuCaption','RDOptionsReplaceSelection','Replace Selection');
  AAdd('MenuCaption','RDOptionsUpdateGISLive','Update GIS Viewer Live');

  AAdd('MenuCaption','RDSelectInvertSelection','Invert Selection');
  AAdd('MenuCaption','RDReloadMasterGaugeList','Reload Master Gaugelist');
  AAdd('MenuCaption','RDReloadMasterGaugeListSepEnd','-');

  AAdd('MenuCaption','RDSaveSelection','Save Selection');
  AAdd('MenuCaption','RDSelect','Select');
  AAdd('MenuCaption','RDSelectSelectAll','Select All Gauges');
  AAdd('MenuCaption','RDSelectUnSelectAll','Delete All Selected Gauges');
  AAdd('MenuCaption','RDSelectUnSelect','Delete Selected Gauge(s)');
  AAdd('MenuCaption','RDSelectionSepEnd','-');

  AAdd('MenuCaption','RDCreateReport','Create SubArea Report');
  AAdd('MenuCaption','RDCreateFiles','Create and export RAW, MP and PAT files');
  AAdd('MenuCaption','RDCreateSplit','Split rainfall record');
  AAdd('MenuCaption','RDDeleteSplit','Delete split section of rainfall record');
  AAdd('MenuCaption','RDUpdateSplit','Update Split');
  AAdd('MenuCaption','RDCreatePatch','Create Patch');
  AAdd('MenuCaption','RDDeletePatch','Delete Patch');
  AAdd('MenuCaption','RDRenamePatch','Rename Patch');
  AAdd('MenuCaption','RDAddGaugeToPatch','Add Gauge To Patch');
  AAdd('MenuCaption','RDRemoveGaugeFromPatch','Remove Gauge From Patch');
  AAdd('MenuCaption','RDSelectRAWFlags','Select RAW data flags');
  AAdd('MenuCaption','RDAdminToggleGrid','Display data grid');
  AAdd('MenuCaption','RDAdminToggleGraph','Display graph');
  AAdd('MenuCaption','RDAdminToggleTree','Display treeview');
  AAdd('MenuCaption','RDGraphToggleGrid','Display data grid');
  AAdd('MenuCaption','RDGraphToggleGraph','Display graph');
  AAdd('MenuCaption','RDGraphToggleTree','Display treeview');
  AAdd('MenuCaption','RDStatsToggleGrid','Display data grid');
  AAdd('MenuCaption','RDStatsToggleGraph','Display graph');
  AAdd('MenuCaption','RDStatsToggleTree','Display treeview');
  AAdd('MenuCaption','RDZoneToggleTree','Display treeview');
  AAdd('MenuCaption','RDGaugeToggleTree','Display treeview');
  AAdd('MenuCaption','RDAddGaugeToZone','Add Gauge To Zone');
  AAdd('MenuCaption','RDRemoveGaugeFromZone','Remove Gauge From Zone');
  AAdd('MenuCaption','RDViewRainfall','Rainfall');
  AAdd('MenuCaption','RDViewSepEnd','-');
  AAdd('MenuCaption','RDViewStatusBar','View Rainfall Data Status Bar');
  AAdd('MenuCaption','RDCreatePATFiles','Create and export RAW, MP and PAT-files for period');
  AAdd('MenuCaption','RDHighLightOutliers','Highlight possible outliers');
  AAdd('MenuCaption','RDFlagDataBlock','Flag Rainfall Data Block');
  AAdd('MenuCaption','RDUnFlagDataBlock','Un-Flag Rainfall Data Block');
  AAdd('MenuCaption','RDFlagSetup','Flag Setup');
  AAdd('MenuCaption','RDFlagClick','Flag Data');
  AAdd('MenuCaption','RDWeatherEvents','Show Significant Weather Events');
  AAdd('MenuCaption','RDImportUserData','Import User Data');
  AAdd('MenuCaption','RDClearUserData','Delete user station');
  AAdd('MenuCaption','RDImportSawsDwafData','Import Saws/Dwaf Data');
  AAdd('MenuCaption','LicenceModels','Register model licence/s');
  AAdd('MenuCaption','RDCreateCatchmentZone','Create Catchment Rainfall File');

  AAdd('MenuCaption','Save','Save');
  AAdd('MenuCaption','SaveFile','Save file');
  AAdd('MenuCaption','SaveOutputFiles','Save output files to database');
  AAdd('MenuCaption','SaveModelDataToDB','Save Model Data To Database');
  AAdd('MenuCaption','SaveModelDataToFiles','Save Model Data To Files');
  AAdd('MenuCaption','SelectFiles','Select File Names');
  AAdd('MenuCaption','SelectStudyArea','Select Study Area');
  AAdd('MenuCaption','TimeComparitorAddChart','Add current chart to current view');
  AAdd('MenuCaption','TimeComparitorAddSeries','Add current series to chart');
  AAdd('MenuCaption','TimeComparitorRemoveChart','Remove current chart from current view');
  AAdd('MenuCaption','TimeComparitorRemoveSeries','Remove current series from chart');
  AAdd('MenuCaption','TimeComparitorSaveView','Save View');
  AAdd('MenuCaption','TimeComparitorChartName','Set chart captions');
  AAdd('MenuCaption','TimeComparitorCreateChart','Create a new chart');
  AAdd('MenuCaption','TimeComparitorRenameChart','Rename current chart');
  AAdd('MenuCaption','TimeComparitorDeleteChart','Delete current chart');
  AAdd('MenuCaption','TimeComparitorCreateView','Create a new view');
  AAdd('MenuCaption','TimeComparitorRenameView','Rename current view');
  AAdd('MenuCaption','TimeComparitorDeleteView','Delete current view');
  AAdd('MenuCaption','TimeComparitorSeriesColor','Set series colour');
  AAdd('MenuCaption','TimeComparitorShowChartLegendDialog','Set chart legend properties');
  AAdd('MenuCaption','TimeComparitorToggleIndividualSeries','Toggle between normalised and absolute values');
  AAdd('MenuCaption','Undo','Undo');
  AAdd('MenuCaption','UserAdministration','User Administration');
  AAdd('MenuCaption','ValidateFile','Validate Current File');
  AAdd('MenuCaption','ValidateFiles','Validate data files');
  AAdd('MenuCaption','ValidateModelData','Validate Model Data');
  AAdd('MenuCaption','GenerateSystemConfigDataFiles','Generate System Configuration Data Files');
  AAdd('MenuCaption','View','View');
  AAdd('MenuCaption','ViewEditGrid','Data');
  AAdd('MenuCaption','ViewExpertUser','Expert User');
  AAdd('MenuCaption','ViewFileSelection','File Edit');
  AAdd('MenuCaption','ViewGraph','Graph');
  AAdd('MenuCaption','ViewNetworkVisualiser','Network Visualiser');
  AAdd('MenuCaption','ViewReset','Reset');
  AAdd('MenuCaption','ViewResults','Results');
  AAdd('MenuCaption','ViewStandardUser','Standard User');
  AAdd('MenuCaption','ViewStudyPanel','Study Panel');
  AAdd('MenuCaption','ViewToolBar','Tool Bar');
  AAdd('MenuCaption','YRCLoadFromDB','Load chart data from database');
  AAdd('MenuCaption','YRCLoadFromFile','Load chart data from file');
  AAdd('MenuCaption','YRCLoadCoefFile','Load curves from existing co-efficients');
  AAdd('MenuCaption','YRCResetChartData','Reset Current Mode Data');
  AAdd('MenuCaption','YRCSaveChart','Save chart to database');
  AAdd('MenuCaption','YRCToggleChartMode','Select Deterministic/Regression Manipulation');
  AAdd('MenuCaption','YRCTogglePlaneMode','Show All Periods');
  AAdd('MenuCaption','YRCToggleCurveManipulation','Disable/Enable Chart Editing');
  AAdd('MenuCaption','YRCDeleteTargetDraft','Delete selected target draft');
  AAdd('MenuCaption','YRCChartPrintMode','Prepare/Restore chart for printing');
  AAdd('MenuCaption','YRCFYNewChart','New Chart');
  AAdd('MenuCaption','YRCFYOpenChart','Open Chart');
  AAdd('MenuCaption','YRCFYSaveChart','Save Chart');
  AAdd('MenuCaption','YRCFYAddSeries','Add Series');
  AAdd('MenuCaption','YRCFYDeleteSeries','Delete Series');
  AAdd('MenuCaption','YRCDeleteChart','Delete the current chart');
  AAdd('MenuCaption','YRCEditYValue','Edit Y-Value for X at 100%');
  AAdd('MenuCaption','YRCStartRegressionEdit','Start editing chart in Regression mode');
  AAdd('MenuCaption','YRCEndRegressionEdit','Stop editing chart in Regression mode');
  AAdd('MenuCaption','YRCStartDeterministicEdit','Start editing chart in Deterministic mode');
  AAdd('MenuCaption','YRCEndDeterministicEdit','Stop editing chart in Deterministic mode');


  AAdd('MenuItem','ViewGoto','Goto');

  AAdd('MenuCaption','InvokeWizard',                 'Invoke Wizard');

  AAdd('MenuCaption','Planning',                     'Planning');
  AAdd('MenuCaption','AllocationControl',            'Allocation Controls');
  AAdd('MenuCaption','SwitchControl',                'Switch Controls');
  AAdd('MenuCaption','TimeControl',                  'Time Controls');
  AAdd('MenuCaption','CreateSwitchDef',              'Create Switch Definition');
  AAdd('MenuCaption','DeleteSwitchDef',              'Delete Switch Definition');
  AAdd('MenuCaption','CreateAllocDef',               'Create Allocation Definition');
  AAdd('MenuCaption','DeleteAllocDef',               'Delete Allocation Definition');
  AAdd('MenuCaption','CreateResTimeCntrl',           'Create Reservoir Time Control');
  AAdd('MenuCaption','DeleteResTimeCntrl',           'Delete Reservoir Time Control');
  AAdd('MenuCaption','CreateResReplacement',         'Create Reservoir Replacement');
  AAdd('MenuCaption','CreateChannelTimeCntrl',       'Create Channel Time Control');
  AAdd('MenuCaption','DeleteChannelTimeCntrl',       'Delete Channel Time Control');
  AAdd('MenuCaption','CreateChannelSwitchCntrl',     'Create Channel Switch Control');
  AAdd('MenuCaption','DeleteChannelSwitchCntrl',     'Delete Channel Switch Control');

  AAdd('MenuCaption','Reservoirs',                   'Reservoirs');
  AAdd('MenuCaption','CopyReservoir',                'Copy Reservoir');
  AAdd('MenuCaption','CreateReservoir',              'Create a new Reservoir');
  AAdd('MenuCaption','DeleteReservoir',              'Delete Reservoir');

  AAdd('MenuCaption','Nodes',                        'Nodes');
  AAdd('MenuCaption','CreateNodeWithInflow',         'Create a new Node with Inflow');
  AAdd('MenuCaption','DeleteNodeWithInflow',         'Delete Node with Inflow');
  AAdd('MenuCaption','CreateNodeWithoutInflow',      'Create a new Node without Inflow');
  AAdd('MenuCaption','DeleteNodeWithoutInflow',      'Delete Node without Inflow');

  AAdd('MenuCaption','Channels',                     'Channels');
  AAdd('MenuCaption','CreateChannel',                'Create a new Channel');
  AAdd('MenuCaption','CopyChannel',                  'Copy Channel');
  AAdd('MenuCaption','DeleteChannel',                'Delete Channel');
  AAdd('MenuCaption','ConvertChannel',               'Convert Channel to General Flow');

  AAdd('MenuCaption','CreateYMDemandCentreReturnFlowChannel',     'Create Demand Centre Return Flow Channel');
  AAdd('MenuCaption','DeleteYMDemandCentreReturnFlowChannel',     'Delete Demand Centre Return Flow Channel');

  AAdd('MenuCaption','ChannelFeatures',              'Channel Features');
  AAdd('MenuCaption','CreateMinimumFlowFeature',     'Create Minimum Flow Feature');
  AAdd('MenuCaption','CreateMinMaxFlowFeature',      'Create MinMax Flow Feature');
  AAdd('MenuCaption','CreatePumpingFeature',         'Create Pumping Feature');
  AAdd('MenuCaption','CopyPumpingFeature',           'Copy Pumping Feature');
  AAdd('MenuCaption','CreateLossFeature',            'Create Loss Feature');
  AAdd('MenuCaption','CreateSpecifiedDemandFeature', 'Create Specified Demand Feature');
  AAdd('MenuCaption','CreateDiversionFeature',       'Create Diversion Feature');
  AAdd('MenuCaption','CreateSpecifiedInflowFeature', 'Create Specified Inflow Feature');
  AAdd('MenuCaption','CreateIFRFeature',             'Create IFR Feature');
  AAdd('MenuCaption','CopyIFRFeature',               'Copy IFR Feature');
  AAdd('MenuCaption','CreateYMDemandCentreRecFeature','Create Demand Centre Reclaimation Feature');
  AAdd('MenuCaption','CreateSFRSubCatchment'         ,'Create Stream Flow Reduction SubCatchment');
  AAdd('MenuCaption','CopySFRSubCatchment'           ,'Copy Stream Flow Reduction SubCatchment');
  AAdd('MenuCaption','CreateMine'                    ,'Create a new Mine');
  AAdd('MenuCaption','CopyMine'                      ,'Copy a new Mine');
  AAdd('MenuCaption','CreateOpenCast'                ,'Create OpenCast Mining Section');
  AAdd('MenuCaption','CreateUnderGround'             ,'Create UnderGround Mining Section');
  AAdd('MenuCaption','CreateSlurryDump'              ,'Create Slurry Dump');
  AAdd('MenuCaption','Curtailment'                   ,'Curtailment');
  AAdd('MenuCaption','DroughtRestriction'            ,'Drought Restriction');

  AAdd('MenuCaption','CreatePhysicalFlowConstraint', 'Create Physical Flow Constraint');
  AAdd('MenuCaption','CopyPhysicalFlowConstraint', 'Copy Physical Flow Constraint');

  AAdd('MenuCaption','ScenarioCopy', 'Copy Data from a Scenario');
  AAdd('MenuCaption','ScenarioCopyReservoir', 'Copy Reservoirs');
  AAdd('MenuCaption','ScenarioCopyChannel', 'Copy Channels');
  AAdd('MenuCaption','ScenarioCopyNetworkFeature','Copy Network Features');
  AAdd('MenuCaption','ScenarioCopyIrrigationArea','Copy Irrigation Area');
  AAdd('MenuCaption','ScenarioCopyIrrigationBlock','Copy Irrigation Block');
  AAdd('MenuCaption','ScenarioCopyPowerPlant','Copy Power Plant');
  AAdd('MenuCaption','ScenarioCopyWetland','Copy Wetland');
  AAdd('MenuCaption','ScenarioCopyYMDemandCentre','Copy Demand Centre');
  AAdd('MenuCaption','ScenarioCopySFRSubCatchment','Copy Stream Flow Reduction SubCatchment');
  AAdd('MenuCaption','ScenarioCopyMine','Copy Mine');
  AAdd('MenuCaption','ScenarioCopyGroundWater','Copy Groundwater');

  AAdd('MenuCaption','NetworkFeatures',              'Network Features');
  AAdd('MenuCaption','CreateIrrigationArea',         'Create Irrigation Area');
  AAdd('MenuCaption','CopyIrrigationArea',           'Copy Irrigation Area');
  AAdd('MenuCaption','CreateIrrigationBlock',        'Create Irrigation Block');
  AAdd('MenuCaption','CopyIrrigationBlock',          'Copy Irrigation Block');
  AAdd('MenuCaption','CreateWetland',                'Create Wetland');
  AAdd('MenuCaption','CopyWetland',                  'Copy Wetland');
  AAdd('MenuCaption','CreateDroughtRestriction',     'Create Drought Restriction');
  AAdd('MenuCaption','CreateYMDemandCentre',         'Create Demand Centre');
  AAdd('MenuCaption','CopyYMDemandCentre',           'Copy Demand Centre');
  AAdd('MenuCaption','CreatePowerPlant',             'Create Power Plant');
  AAdd('MenuCaption','CopyPowerPlant',               'Copy Power Plant');
  AAdd('MenuCaption','CreateMasterControlFeature',   'Create Master Control Feature');
  AAdd('MenuCaption','CreateWaterDemandFeature',     'Create Water Demand Feature');
//  AAdd('MenuCaption','CreateWaterUseScenario',       'Create Water Use Scenario');
//  AAdd('MenuCaption','DeleteWaterUseScenario',       'Delete Water Use Scenario');
  AAdd('MenuCaption','DeleteMinimumFlowFeature',     'Delete Minimum Flow Feature');
  AAdd('MenuCaption','DeleteMinMaxFlowFeature',      'Delete MinMax Flow Feature');
  AAdd('MenuCaption','DeleteLossFeature',            'Delete Loss Feature');
  AAdd('MenuCaption','DeleteSpecifiedDemandFeature', 'Delete Specified Demand Feature');
  AAdd('MenuCaption','DeleteDiversionFeature',       'Delete Diversion Feature');
  AAdd('MenuCaption','DeleteSpecifiedInflowFeature', 'Delete Specified Inflow Feature');
  AAdd('MenuCaption','DeletePumpingFeature',         'Delete Pumping Feature');
  AAdd('MenuCaption','DeleteIFRFeature',             'Delete IFR Feature');
  AAdd('MenuCaption','DeleteYMDemandCentreRecFeature','Delete Demand Centre Reclaimation Feature');
  AAdd('MenuCaption','DeletePhysicalFlowConstraint', 'Delete Physical Flow Constraint');
  AAdd('MenuCaption','DeleteIrrigationArea',         'Delete Irrigation Area');
  AAdd('MenuCaption','DeleteIrrigationBlock',        'Delete Irrigation Block');
  AAdd('MenuCaption','DeletePowerPlant',             'Delete Power Plant');
  AAdd('MenuCaption','DeleteMasterControlFeature',   'Delete Master Control Feature');
  AAdd('MenuCaption','DeleteWaterDemandFeature',     'Delete Water Demand Feature');
  AAdd('MenuCaption','DeleteWetland',                'Delete Wetland');
  AAdd('MenuCaption','DeleteDroughtRestriction',     'Delete Drought Restriction');
  AAdd('MenuCaption','DeleteYMDemandCentre',         'Delete Demand Centre');
  AAdd('MenuCaption','DeleteSFRSubCatchment',        'Delete Stream Flow Reduction SubCatchment');
  AAdd('MenuCaption','DeleteMine'                    ,'Delete Mine');
  AAdd('MenuCaption','DeleteOpenCast'                ,'Delete OpenCast Mining Section');
  AAdd('MenuCaption','DeleteUnderGround'             ,'Delete UnderGround Mining Section');
  AAdd('MenuCaption','DeleteSlurryDump'              ,'Delete Slurry Dump');
  AAdd('MenuCaption','DeleteGroundWater'             ,'Delete Ground Water');
  AAdd('MenuCaption','CreateGroundWater'             ,'Create Ground Water');
  AAdd('MenuCaption','CopyGroundWater'               ,'Copy Groundwater');

  AAdd('MenuCaption','DisbenefitFunction',           'Disbenefit Functions');
  AAdd('MenuCaption','CreateDisbenefitFunction',     'Create Disbenefit Function');
  AAdd('MenuCaption','DeleteDisbenefitFunction',     'Delete Disbenefit Function');
  AAdd('MenuCaption','CreateReturnFlowChannel',     'Create Return Flow Channel');
  AAdd('MenuCaption','DeleteReturnFlowChannel',     'Delete Return Flow Channel');
  //Added by Karabo
  AAdd('MenuCaption','CreateMultiChannelCurtailRestriction', 'Create Multi Channel Curtailment Restriction');
  AAdd('MenuCaption','DeleteMultiChannelCurtailRestriction', 'Delete Multi Channel Curtailment Restriction');

  AAdd('MenuCaption','Wizards',                      'Wizards');
  AAdd('MenuCaption','RunYieldHistoric',             'Run Yield Model in Historic Mode');
  AAdd('MenuCaption','RunYieldStochastic',           'Run Yield Model in Stochastic Mode');
  AAdd('MenuCaption','RunYieldYRC',                  'Run Yield Model for YRC');

  AAdd('MenuCaption','VNVDrawingSep','-');
  AAdd('MenuCaption','VNVNewDrawingGroup',     'New drawing group');
  AAdd('MenuCaption','VNVDeleteDrawingGroup',  'Delete selected drawing group');
  AAdd('MenuCaption','VNVNewDrawing',          'New drawing');
  AAdd('MenuCaption','VNVDeleteDrawing',       'Delete selected drawing');
  AAdd('MenuCaption','VNVEditDrawing',         'Edit selected drawing');
  AAdd('MenuCaption','VNVViewDrawing',         'View selected drawing');
  AAdd('MenuCaption','VNVCopyDrawing',         'Copy selected drawing');
  AAdd('MenuCaption','VNVRenameDrawingGroup',  'Rename selected drawing group');
  AAdd('MenuCaption','VNVRenameDrawing',       'Rename selected drawing');

  AAdd('MenuCaption','Changes',      'Changes');
  AAdd('MenuCaption','CGCreateNew',  'Create New Changegroup');
  AAdd('MenuCaption','CGDelete',     'Delete Changegroup');
  AAdd('MenuCaption','CEActivate',   'Activate Change Element');
  AAdd('MenuCaption','CEDeactivate', 'Deactivate Change Element');
  AAdd('MenuCaption','CLCreateNew',  'New Changelist');
  AAdd('MenuCaption','CLCopy',       'Copy Changelist');
  AAdd('MenuCaption','CLDelete',     'Delete Changelist');
  AAdd('MenuCaption','CLMoveUp',     'Move up');
  AAdd('MenuCaption','CLMoveDown',   'Move down');
  AAdd('MenuCaption','CLActivate',   'Activate');
  AAdd('MenuCaption','CLDeactivate', 'Deactivate');
  AAdd('MenuCaption','CLApply',      'Apply Changelist(s)');
  AAdd('MenuCaption','CLImport',     'Import Changelist(s)');
  AAdd('MenuCaption','CLExport',     'Export Changelist(s)');
  AAdd('MenuCaption','CLParameter',  'Parameter Changes');
  AAdd('MenuCaption','CLPatchParameter',  'Patch Parameter Changes');
  AAdd('MenuCaption','CStationFilter',  'Filter Station');
  AAdd('MenuCaption','CPCreateNew',  'New Parameter Change');
  AAdd('MenuCaption','CPDelete',     'Delete Parameter Change');
  AAdd('MenuCaption','CPEdit',       'Edit Parameter Change');
  AAdd('MenuCaption','CPOK',         'OK');
  AAdd('MenuCaption','CPCancel',     'Cancel');
  AAdd('MenuCaption','CPClose',      'Close');

  AAdd('MenuCaption','MetaData',          'Meta Data');
  AAdd('MenuCaption','MetaDataCreateNew', 'Create Meta Data');
  AAdd('MenuCaption','MetaDataDelete',    'Delete Meta Data');
  AAdd('MenuCaption','MetaDataEdit',      'Edit Meta Data');
  AAdd('MenuCaption','CreateDailyDiversion','Create Daily Diversion');
  AAdd('MenuCaption','RenameDailyDiversion','Rename Daily Diversion');
  AAdd('MenuCaption','CreateDDTSDam','Create Dam To Model');

  AAdd('MenuCaption','ExportDailyIFR','Export Daily IFR');
  AAdd('MenuCaption','ExportMonthlyIFR','Export Monthly IFR');
  AAdd('MenuCaption','ExportFlowDiversionRelationship','Export Flow Diversion Relationship');

  AAdd('MenuCaption','DeleteDailyDiversion','Delete Daily Diversion');
  AAdd('MenuCaption','ImportDailyFlowDataFromCSVFile','Import Daily Flow Data File');
  AAdd('MenuCaption','ClearDailyFlowDataFromCSVFile','Clear Daily Flow Data File');
  AAdd('MenuCaption','ImportDailyInstreamFlowFile','Import Daily Instream Flow File');
  AAdd('MenuCaption','ClearDailyInstreamFlowFile','Clear Daily Instream Flow File');
  AAdd('MenuCaption','GenerateFlowDiversionRelation','Generate Flow Diversion Relationship');
  AAdd('MenuCaption','ImportFile14','Import File 14(F14 *.dat)');
  AAdd('MenuCaption','ClearFile14','Clear File 14(F14 *.dat)');  
  AAdd('MenuCaption','ClearFlowDiversionRelation','Clear Flow Diversion Relation');
  AAdd('MenuCaption','GenerateWRYMData','Export WRYM Data(F10.dat)');
  AAdd('MenuCaption','ClearWRYMData','Clear WRYM Data(F10.dat)');

  AAdd('MenuCaption','RDAVRGRAINFALL','Show Station Used Grid');
  AAdd('MenuCaption','RDINPUTRAINFALL','Show Catchment Output Grid');
  AAdd('MenuCaption','RDRESULT', 'Show .Run Grid');
  AAdd('MenuCaption','RDEXPORTDATA','Export Data');
  AAdd('MenuCaption','OutputChannelComplianceGrid','Compliance Grid');
  AAdd('MenuCaption','OutputChannelComplianceGraph','Compliance Graph');
  AAdd('MenuCaption','OutputChannelDistributionCurve','Distribution Curve');
  AAdd('MenuCaption','OutputComparitorShowChartLegendDialog','Set chart legend properties');

  AAdd('MenuCaption','StomsaData','Data');
  AAdd('MenuCaption','StomsaFileNew','New File');
  AAdd('MenuCaption','StomsaFileOpen','Open File');
  AAdd('MenuCaption','StomsaFileOpenParam','Open PARAM File');
  AAdd('MenuCaption','StomsaFileSave','Save File');
  AAdd('MenuCaption','StomsaFileSaveAs','Save File As');
  AAdd('MenuCaption','StomsaFileSaveANS','Save ANS Files');
  AAdd('MenuCaption','StomsaFileMerge','Merge Files');
  AAdd('MenuCaption','StomsaFileClose','Close File');
  AAdd('MenuCaption','StomsaFileExport','Export Files');
  AAdd('MenuCaption','StomsaFileImport','Import Files');

  AAdd('MenuCaption','RecentDatasets','Recent Datasets');
end;

end.
