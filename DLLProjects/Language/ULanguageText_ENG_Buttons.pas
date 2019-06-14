//
//  UNIT      : Contains language text.
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/01/23
//  COPYRIGHT : Copyright © 2003 DWAF
//

unit ULanguageText_ENG_Buttons;

interface

type TTextItemAddFunction = procedure (AContext, AConstant, AText: string) of object;

procedure LoadLanguageText(AAdd: TTextItemAddFunction);

implementation

procedure LoadLanguageText(AAdd: TTextItemAddFunction);
const OPNAME = 'LoadLanguageText';
begin

  //
  // Button captions.
  //

  AAdd('ButtonCaption','Accept'                       ,'Accept');
  AAdd('ButtonCaption','AddSupplyChannel'             ,'Add Supply Channel');
  AAdd('ButtonCaption','AddChannel'                   ,'Add Channel');
  AAdd('ButtonCaption','Add'                          ,'Add');
  AAdd('ButtonCaption','AreaManager'                  ,'Area Manager');

  AAdd('ButtonCaption','Cancel'                       ,'Cancel');
  AAdd('ButtonCaption','Categorizations'              ,'Categorizations');
  AAdd('ButtonCaption','Column'                       ,'Column');
  AAdd('ButtonCaption','Close'                        ,'Close');
  AAdd('ButtonCaption','Continue'                     ,'Continue');
  AAdd('ButtonCaption','Copy'                         ,'Copy');
  AAdd('ButtonCaption','CreateIFRSite'                ,'Create IFR Site');
  AAdd('ButtonCaption','CreateCatchment'              ,'Create Catchment');
  AAdd('ButtonCaption','CreateStudyMetadata'          ,'Create Study Metadata');
  AAdd('ButtonCaption','DeleteStudyMetadata'          ,'Delete Study Metadata');

  AAdd('ButtonCaption','Data'                         ,'Data');
  AAdd('ButtonCaption','DataDictionary'               ,'Data Dictionary');
  AAdd('ButtonCaption','Decline'                      ,'Decline');
  AAdd('ButtonCaption','DeleteSlurry'                 ,'Delete Slurry Dump');
  AAdd('ButtonCaption','DeleteOpencast'               ,'Delete Opencast');
  AAdd('ButtonCaption','DeleteUnderground'            ,'Delete Underground');
  AAdd('ButtonCaption','DeleteReturnFlow'             ,'Delete Return Flow Channel');
  AAdd('ButtonCaption','DeleteSupplyChannel'          ,'Delete Supply Channel');
  AAdd('ButtonCaption','DeleteChannel'                ,'Delete Channel');

  AAdd('ButtonCaption','DeleteIFRSite'                ,'Delete IFR Site');
  AAdd('ButtonCaption','DamBasinSurvey'               ,'Dam Basin Survey');
  AAdd('ButtonCaption','Delete'                       ,'Delete');
  AAdd('ButtonCaption','DeleteRow'                    ,'Delete Row');
  AAdd('ButtonCaption','DiversionRelationship'        ,'Diversion Relationship');
  AAdd('ButtonCaption','DemandProjection'             ,'Demand Projection');
  AAdd('ButtonCaption','DevOptionSequences'           ,'Development Option Sequences');

  AAdd('ButtonCaption','Edit'                         ,'Edit');
  AAdd('ButtonCaption','Extract'                      ,'Extract');

  AAdd('ButtonCaption','Filter'                       ,'Filter');
  AAdd('ButtonCaption','Finish'                       ,'Finish');

  AAdd('ButtonCaption','GenerateFactors'              ,'Generate Factors');
  AAdd('ButtonCaption','GenerateProjections'          ,'Generate Projections');
  AAdd('ButtonCaption','Group'                        ,'Group');

  AAdd('ButtonCaption','HydrologyFiles'               ,'Hydrology Files Source');

  AAdd('ButtonCaption','IFRStudyDate'                 ,'Study Date');
  AAdd('ButtonCaption','IFRStudyLevel'                ,'Study Level');
  AAdd('ButtonCaption','IFRStudyEnvManClass'          ,'Management Class');
  AAdd('ButtonCaption','InsertRow'                    ,'Insert Row');
  AAdd('ButtonCaption','InsertSlurry'                 ,'Insert Slurry Dump');
  AAdd('ButtonCaption','InsertOpencast'               ,'Insert Opencast');
  AAdd('ButtonCaption','InsertUnderground'            ,'Insert Underground');
  AAdd('ButtonCaption','InsertReturnFlow'             ,'Insert Return Flow Channel');

  AAdd('ButtonCaption','List'                         ,'List');

  AAdd('ButtonCaption','Muximum'                      ,'Set Y Maximum');

  AAdd('ButtonCaption','New'                          ,'New');
  AAdd('ButtonCaption','Next'                         ,'Next >>');
  AAdd('ButtonCaption','NewGroup','New Group');
  AAdd('ButtonCaption','NewList','New List');
  AAdd('ButtonCaption','NewItem','New Item');

  AAdd('ButtonCaption','Ok/Close'                     ,'Ok/Close');
  AAdd('ButtonCaption','OK'                           ,'OK');
  AAdd('ButtonCaption','Reset'                        ,'Reset');
  AAdd('ButtonCaption','OperatingRuleStrategy'        ,'Operating Rule Strategy');
  AAdd('ButtonCaption','OverallOperatingRulestrategy' ,'Overall Operating Rule Strategy');
  AAdd('ButtonCaption','StudyErrorMetaData'           ,'Study Error MetaData');

  AAdd('ButtonCaption','PatchRInput'                  ,'Create PatchR Input File');
  AAdd('ButtonCaption','ProposedInfrastructure'       ,'Proposed Infrastructure Changes');
  AAdd('ButtonCaption','Previous'                     ,'<< Previous');

  AAdd('ButtonCaption','Rainfall'                     ,'Push to Update GIS Viewer');
  AAdd('ButtonCaption','Report'                       ,'Report');
  AAdd('ButtonCaption','Report'                       ,'Report');
  AAdd('ButtonCaption','Run'                          ,'Run');
  AAdd('ButtonCaption','Remove'                       ,'Remove');
  AAdd('ButtonCaption','Run'                          ,'Run Model');

  AAdd('ButtonCaption','SaveFile'                     ,'Save File');
  AAdd('ButtonCaption','SaveIFRSite'                  ,'Save IFR Site data');
  AAdd('ButtonCaption','SenarioStrategy'              ,'Scenario Strategy');
  AAdd('ButtonCaption','SelectDemandFile'             ,'...');
  AAdd('ButtonCaption','StudyResults'                 ,'Study Results');
  AAdd('ButtonCaption','StudyReports'                 ,'Study Reports');
  AAdd('ButtonCaption','StudyStakeholders'            ,'Study Stakeholders');
  AAdd('ButtonCaption','Study'                        ,'Study Description');

  AAdd('ButtonCaption','ValidateProjections'          ,'Validate Projections');
  AAdd('ButtonCaption','View'                         ,'View');
  AAdd('ButtonCaption','&Ok'                          ,'&Ok');
  AAdd('ButtonCaption','&Cancel'                      ,'&Cancel');
  AAdd('ButtonCaption','GenerateIFR'                  ,'Generate IFR');

  AAdd('ButtonCaption','Calculate'                    ,'Fill in gaps');

  AAdd('ButtonCaption','MoreDetails'                  ,'>>>');
  AAdd('ButtonCaption','Details'                      ,'<<<');

  AAdd('ButtonCaption','RISelector'                   ,'Select Recurrence Intervals');
  AAdd('ButtonCaption','Compliance'                   ,'Show/Hide Compliance');
  AAdd('ButtonCaption','OutputPlotGraphLegend'        ,'Legend');
  AAdd('ButtonCaption','Import'                       ,'Import');

  AAdd('ButtonCaption','StomsaFileNew'                ,'New File');
  AAdd('ButtonCaption','StomsaFileOpen'               ,'Open File');
  AAdd('ButtonCaption','StomsaFileOpenParam'          ,'Open PARAM File');
  AAdd('ButtonCaption','StomsaFileSave'               ,'Save File');
  AAdd('ButtonCaption','StomsaFileMerge'              ,'Merge Files');
  AAdd('ButtonCaption','StomsaFileClose'              ,'Close File');
  AAdd('ButtonCaption','StomsaFileExport'             ,'Export Files');
  AAdd('ButtonCaption','StomsaFileImport'             ,'Import Files');

  AAdd('ButtonCaption','LoadDataset'                  ,'Load Dataset');
  AAdd('ButtonCaption','DeriveDatasets'               ,'Derive Datasets');
  AAdd('ButtonCaption','CompareDataset'               ,'Compare Dataset');
  AAdd('ButtonCaption','ChangeLists'                  ,'Change Lists');
  AAdd('ButtonCaption','FieldLists'                   ,'Field Lists');
  AAdd('ButtonCaption','DeriveFields'                 ,'Derive Fields');
  AAdd('ButtonCaption','AreaManagement'               ,'Geographical Feature Manager');
  AAdd('ButtonCaption','Apportionments'               ,'Apportionments');
  AAdd('ButtonCaption','ConvertDataset'               ,'Convert Dataset');
  AAdd('ButtonCaption','Stakeholders'                 ,'Stakeholders');

  //
  // Button hints.
  //
  AAdd('ButtonHint','ImportDiversionRelationship'    ,'Import Diversion Relationship From the Pre-processor');
  AAdd('ButtonHint','CreateDDTSDam'    ,'Ctreate the DAM to be MODELED');
  AAdd('ButtonHint','AddItemToChangeList','Add item to ChangeList');
  AAdd('ButtonHint','Add','Add a new record');
  AAdd('ButtonHint','CreateSwitchDef',           'Create a new Switch Definition');
  AAdd('ButtonHint','CreateNewList',             'Create new List');
  AAdd('ButtonHint','DeleteSwitchDef',           'Delete the selected Switch Definition');
  AAdd('ButtonHint','CreateAllocDef',            'Create a new Allocation Definition');
  AAdd('ButtonHint','CreateNewGroup',            'Create new group');
  AAdd('ButtonHint','DeleteAllocDef',            'Delete the selected Allocation Definition');
  AAdd('ButtonHint','CreateResTimeCntrl',        'Create Reservoir Time Control');
  AAdd('ButtonHint','DeleteResTimeCntrl',        'Delete Reservoir Time Control');
  AAdd('ButtonHint','DeleteSelectedChangelist',  'Delete selected changelist');
  AAdd('ButtonHint','CreateResReplacement',      'Create Reservoir Replacement');
  AAdd('ButtonHint','CreateChannelTimeCntrl',    'Create Channel Time Control');
  AAdd('ButtonHint','DeleteChannelTimeCntrl',    'Delete Channel Time Control');
  AAdd('ButtonHint','CreateChannelSwitchCntrl',  'Create Channel Switch Control');
  AAdd('ButtonHint','DeleteChannelSwitchCntrl',  'Delete Channel Switch Control');

  AAdd('ButtonHint','CreateReservoir','Create a new reservoir');
  AAdd('ButtonHint','DeleteReservoir','Delete the selected reservoir');
  AAdd('ButtonHint','CreateNodeWithInflow','Create a new node with inflow');
  AAdd('ButtonHint','DeleteNodeWithInflow','Delete the selected node with inflow');
  AAdd('ButtonHint','CreateNodeWithoutInflow','Create a new node without inflow');
  AAdd('ButtonHint','DeleteNodeWithoutInflow','Delete the selected node without inflow');
  AAdd('ButtonHint','CreateChannel','Create a new channel');
  AAdd('ButtonHint','CopyChannel','Copy Channel');
  AAdd('ButtonHint','DeleteChannel','Delete the selected channel');
  AAdd('ButtonHint','Cancel','Cancel this action');
  AAdd('ButtonHint','ClearModelData','Clear model data');
  AAdd('ButtonHint','Copy','Copy the record to a new record');
  AAdd('ButtonHint','CopyToClipBoard','Copy To ClipBoard');
  AAdd('ButtonHint','Delete','Delete current record');
  AAdd('ButtonHint','Edit','Edit current record');
  AAdd('ButtonHint','ExportFiles','Export Files');
  AAdd('ButtonHint','ExportFile','Export Current File');
  AAdd('ButtonHint','ExportToFile','Export To File');
  AAdd('ButtonHint','ExportData','Export Data');
  AAdd('ButtonHint','FirstRecord','First record');
  AAdd('ButtonHint','FullScreen','Show Network FullScreen');
  AAdd('ButtonHint','GenerateFiles','Generate file names');
  AAdd('ButtonHint','GISViewer','Invoke GIS');
  AAdd('ButtonHint','HelpContents','HelpContents');
  AAdd('ButtonHint','HelpWhatIsThis','What''s this?');
  AAdd('ButtonHint','ImportFiles','Import Files');
  AAdd('ButtonHint','ImportFile','Import Current File');
  AAdd('ButtonHint','ImportMonthlyDamWaterLevels','Import Monthly Dam Water Levels');
  AAdd('ButtonHint','ImportSumOut','Import SUM.OUT File to view in YRC.');
  AAdd('ButtonHint','Select','Select a penalty structure');
  AAdd('ButtonHint','LastRecord','Last record');
  AAdd('ButtonHint','New','Create a new record');
  AAdd('ButtonHint','NewPenaltyStructure','Create a new penalty structure.(Note: This feature is not yet implemanted)');
  AAdd('ButtonHint','NextRecord','Next record');
  AAdd('ButtonHint','OK','Accept file selections');
  AAdd('ButtonHint','Print','Print');
  AAdd('ButtonHint','PrintPreview','Print Preview');
  AAdd('Buttonhint','PrintSettings','Print Settings');
  AAdd('ButtonHint','PriorRecord','Previous record');
  AAdd('ButtonHint','Report','Launch the report for the current selection');
  AAdd('ButtonHint','RotateView','Rotate data by swapping rows and columns');
  AAdd('ButtonHint','RadMode','Reservoir/Penalty Mode');
  AAdd('ButtonHint','RadFiles','Individual files/Nett of all files');
  AAdd('ButtonHint','RunModel','Execute model');
  AAdd('ButtonHint','SelectDemandFile','Select new Demand File that has not yet been imported.');

  AAdd('ButtonHint','RDDeleteCatchmentZone','Delete Catchment Rainfall File');
  AAdd('ButtonHint','RDInvertSelection','Invert Selection');
  AAdd('ButtonHint','RDLoadGaugeList','Load Gauge List');
  AAdd('ButtonHint','RDSaveSelection','Save Selection');
  AAdd('ButtonHint','RDSelectAll','Select All');
  AAdd('ButtonHint','RDUnSelectAll','De-Select All');
  AAdd('ButtonHint','RDCreateFiles','Create and export RAW, MP and PAT files');
  AAdd('ButtonHint','RDCreateSplit','Split a rainfall record');
  AAdd('ButtonHint','RDDeleteSplit','Delete a split section of a rainfall record');
  AAdd('ButtonHint','RDCreateReport','Create SubArea Report');
  AAdd('ButtonHint','RDCreatePatch','Create patch');
  AAdd('ButtonHint','RDDeletePatch','Delete patch');
  AAdd('ButtonHint','RDRenamePatch','Rename patch');
  AAdd('ButtonHint','RDAddGaugeToPatch','Add gauge to patch');
  AAdd('ButtonHint','RDRemoveGaugeFromPatch','Remove gauge from patch');
  AAdd('ButtonHint','RDSelectRAWFlags','Select RAW data flags');
  AAdd('ButtonHint','RDAdminToggleGrid','Show/Hide data grid');
  AAdd('ButtonHint','RDAdminToggleGraph','Show/Hide graph');
  AAdd('ButtonHint','RDAdminToggleTree','Show/Hide treeview');
  AAdd('ButtonHint','RDGraphToggleGrid','Show/Hide data grid');
  AAdd('ButtonHint','RDGraphToggleGraph','Show/Hide graph');
  AAdd('ButtonHint','RDGraphToggleTree','Show/Hide treeview');
  AAdd('ButtonHint','RDStatsToggleGrid','Show/Hide data grid');
  AAdd('ButtonHint','RDStatsToggleGraph','Show/Hide graph');
  AAdd('ButtonHint','RDStatsToggleTree','Show/Hide treeview');
  AAdd('ButtonHint','RDZoneToggleTree','Display treeview');
  AAdd('ButtonHint','RDGaugeToggleTree','Show/Hide treeview');
  AAdd('ButtonHint','RDAddGaugeToZone','Add Gauge To Zone');
  AAdd('ButtonHint','RDRemoveGaugeFromZone','Remove Gauge From Zone');
  AAdd('ButtonHint','RDCreatePATFiles','Create and export RAW, MP and PAT-files for period');
  AAdd('ButtonHint','RDHighLightOutliers','Highlight possible outliers');
  AAdd('ButtonHint','RDFlagDataBlock','Flag a Block of Rainfall Data');
  AAdd('ButtonHint','RDUnFlagDataBlock','Un-Flag a Block of Rainfall Data');
  AAdd('ButtonHint','RDFlagSetup','Flag Setup');
  AAdd('ButtonHint','RDFlagClick','Flag Data');
  AAdd('ButtonHint','RDFirstWeatherRecord','First significant weather event');
  AAdd('ButtonHint','RDLastWeatherRecord' ,'Last significant weather event');
  AAdd('ButtonHint','RDNextWeatherRecord' ,'Next significant weather event');
  AAdd('ButtonHint','RDPriorWeatherRecord','Previous significant weather event');
  AAdd('ButtonHint','RDWeatherEvents','Show significant weather events');
  AAdd('ButtonHint','RDImportUserData','Import user data');
  AAdd('ButtonHint','RDClearUserData','Delete user station');
  AAdd('ButtonHint','RDUpdateSplit','Update Split');
  AAdd('ButtonHint','RDCreateCatchmentZone','Create Catchment Rainfall File');

  AAdd('ButtonHint','Save','Save record editing');
  AAdd('ButtonHint','SaveFile','Save file');
  AAdd('ButtonHint','SaveAll','Save Drawing');
  AAdd('ButtonHint','SaveOutputFiles','Save output files to database(override)');
  AAdd('ButtonHint','SelectFiles','Select file names');
  AAdd('ButtonHint','TimeComparitorAddChart','Add current chart to current view');
  AAdd('ButtonHint','TimeComparitorAddSeries','Add current series to chart');
  AAdd('ButtonHint','TimeComparitorRemoveChart','Remove current chart from current view');
  AAdd('ButtonHint','TimeComparitorRemoveSeries','Remove current series from chart');
  AAdd('ButtonHint','Color','Set series colour');
  AAdd('ButtonHint','TimeComparitorSaveView','Save View');
  AAdd('ButtonHint','TimeComparitorChartName','Set chart captions');
  AAdd('ButtonHint','TimeComparitorCreateChart','Create a new chart');
  AAdd('ButtonHint','TimeComparitorRenameChart','Rename current chart');
  AAdd('ButtonHint','TimeComparitorDeleteChart','Delete current chart');   
  AAdd('ButtonHint','TimeComparitorCreateView','Create a new view');
  AAdd('ButtonHint','TimeComparitorRenameView','Rename current view');
  AAdd('ButtonHint','TimeComparitorDeleteView','Delete current view');
  AAdd('ButtonHint','TimeComparitorShowChartLegendDialog','Set chart legend properties');
  AAdd('ButtonHint','TSCToggleView','Toggle between normalised and absolute values');
  AAdd('ButtonHint','ToggleLayout','Toggle between Schematic and GIS Layer');
  AAdd('ButtonHint','Undo','Discard record editing');
  AAdd('ButtonHint','ValidateFile','Validate Current File');
  AAdd('ButtonHint','ValidateFiles','Validate Files');
  AAdd('ButtonHint','ValidateModelData','Validate Model Data');
  AAdd('ButtonHint','ViewDataGrid','View data in a grid');
  AAdd('ButtonHint','ViewDataGraph','View data in a graph');
  AAdd('ButtonHint','YRCLoadFromDB','Load chart data from database');
  AAdd('ButtonHint','YRCLoadFromFile','Load chart data from file');
  AAdd('ButtonHint','YRCLoadFromCoefFile','Load curves from existing co-efficients');
  AAdd('ButtonHint','YRCResetChartData','Reset Current Mode Data');
  AAdd('ButtonHint','YRCToggleChartMode','Select Deterministic/Regression Manipulation');
  AAdd('ButtonHint','YRCTogglePlaneMode','Show All Periods');
  AAdd('ButtonHint','YRCToggleCurveManipulation','Disable/Enable Chart Editing');
  AAdd('ButtonHint','YRCDeleteTargetDraft','Delete selected target draft');
  AAdd('ButtonHint','YRCChartPrintMode','Prepare/Restore chart for printing');
  AAdd('ButtonHint','YRCDeleteChart','Delete chart from the database');

  AAdd('ButtonHint','YRCFYNewChart','New Chart');
  AAdd('ButtonHint','YRCFYOpenChart','Open Chart');
  AAdd('ButtonHint','YRCFYSaveChart','Save Chart');
  AAdd('ButtonHint','YRCFYAddSeries','Add Series');
  AAdd('ButtonHint','YRCFYDeleteSeries','Delete Series');

  AAdd('ButtonHint','YRCSaveChart','Save chart to database');
  AAdd('ButtonHint','ZoomIn','Zoom In');
  AAdd('ButtonHint','ZoomOut','Zoom Out');

  AAdd('ButtonHint','CGCreateNew',  'Create New Changegroup');
  AAdd('ButtonHint','CGDelete',     'Delete Changegroup');
  AAdd('ButtonHint','CEActivate',   'Activate Change Element');
  AAdd('ButtonHint','CEDeactivate', 'Deactivate Change Element');
  AAdd('ButtonHint','CLCreateNew',  'Create New Changelist');
  AAdd('ButtonHint','CLCopy',       'Copy Changelist');
  AAdd('ButtonHint','CLDelete',     'Delete Changelist');
  AAdd('ButtonHint','CLMoveUp',     'Move Changelist Up');
  AAdd('ButtonHint','CLMoveDown',   'Move Changelist Down');
  AAdd('ButtonHint','CLActivate',   'Activate Changelist');
  AAdd('ButtonHint','CLDeactivate', 'Deactivate Changelist');
  AAdd('ButtonHint','CLApply',      'Apply Changelist(s)');
  AAdd('ButtonHint','CLImport',     'Import Changelist(s)');
  AAdd('ButtonHint','CLExport',     'Export Changelist(s)');
  AAdd('ButtonHint','CLParameter',  'Parameter Changes');
  AAdd('ButtonHint','CLPatchChangelists','Patch Changelist');
  AAdd('ButtonHint','CStationFilter','Filter Station');
  AAdd('ButtonHint','CPCreateNew',  'New Parameter Change');
  AAdd('ButtonHint','CPDelete',     'Delete Parameter Change');
  AAdd('ButtonHint','CPEdit',       'Edit Parameter Change');
  AAdd('ButtonHint','CPOK',         'OK');
  AAdd('ButtonHint','CPCancel',     'Cancel');
  AAdd('ButtonHint','CPClose',      'Close');

  AAdd('ButtonHint','MetaData',          'Meta Data');
  AAdd('ButtonHint','MetaDataCreateNew', 'Create Meta Data');
  AAdd('ButtonHint','MetaDataDelete',    'Delete Meta Data');
  AAdd('ButtonHint','MetaDataEdit',      'Edit Meta Data');

  AAdd('ButtonHint','VNVNewDrawingGroup',     'Create a new drawing group.');
  AAdd('ButtonHint','VNVDeleteDrawingGroup',  'Delete selected drawing group and drawings inside the group.');
  AAdd('ButtonHint','VNVRenameDrawingGroup',  'Rename selected drawing group.');
  AAdd('ButtonHint','VNVNewDrawing',          'Create a new drawing.');
  AAdd('ButtonHint','VNVDeleteDrawing',       'Delete selected drawing.');
  AAdd('ButtonHint','VNVRenameDrawing',       'Rename selected drawing.');
  AAdd('ButtonHint','VNVEditDrawing',         'Edit the selected drawing.');
  AAdd('ButtonHint','VNVViewDrawing',         'View the selected drawing.');
  AAdd('ButtonHint','VNVCopyDrawing',         'Copy the selected drawing.');
  AAdd('ButtonHint','VNVRenameDrawingGroup',  'Rename selected drawing group');
  AAdd('ButtonHint','VNVRenameDrawing',       'Rename selected drawing');

  //MetaData
  AAdd('ButtonHint','DiversionRelationship' ,'Explanation for the derivation of the diversion relationship.');
  AAdd('ButtonHint','IFRStudyDate'          ,'Study date');
  AAdd('ButtonHint','IFRStudyLevel'         ,'Study level');
  AAdd('ButtonHint','IFRStudyEnvManClass'   ,'Environmental Management class');
  AAdd('ButtonHint','DamBasinSurvey'        ,'Dam Basin Survey');

//  study Selection metadata......
  AAdd('ButtonHint','HydrologyFiles'               ,'Description on the source of the hydrology files');
  AAdd('ButtonHint','StudyResults'                 ,'Description of the Study Results');
  AAdd('ButtonHint','StudyReports'                 ,'Description of the Study Reports');
  AAdd('ButtonHint','SenarioStrategy'              ,'Description of the Senario Strategy');
  AAdd('ButtonHint','StudyStakeholders'            ,'Description of the Study Stakeholders');
  AAdd('ButtonHint','ShowStationUsedGrid'          ,'Show Station Used Grid');
  AAdd('ButtonHint','ShowCatchmentOutput'          ,'Show Catchment Output');
  AAdd('ButtonHint','ShowRUNGrid'                  ,'Show *.RAN Grid');

  AAdd('ButtonHint','OverallOperatingRulestrategy' ,'Description of the Overall Operating Rule strategy');
  AAdd('ButtonHint','Study'                        ,'Description of the Study');
  AAdd('ButtonHint','ProposedInfrastructure'       ,'Description of the Proposed Infrastructure Changes');
  AAdd('ButtonHint','DemandProjection'             ,'Description of the Demand Projection');
  AAdd('ButtonHint','DevOptionSequences'           ,'Description of the Development Option Sequences');
  AAdd('ButtonHint','OperatingRuleStrategy'        ,'Description of the Operating Rule Strategy');
  AAdd('ButtonHint','StudyErrorMetaData'           ,'Description of the Study Error MetaData');

  //Output
  AAdd('ButtonHint','WaterBalanceDetails' ,'Click to show or hide details.');
  AAdd('ButtonHint','OutputComparitorShowChartLegendDialog','Set chart legend properties');

  // Daily Diversion Preprocessor
  AAdd('ButtonHint','CreateDailyDiversion','Create New Daily Diversion Station');
  AAdd('ButtonHint','RenameDailyDiversion','Rename Daily Diversion');
  AAdd('ButtonHint','DeleteDailyDiversion','Delete Daily Diversion Station');
  AAdd('ButtonHint','ImportDailyFlowDataFromCSVFile','Import Daily Flow Data From csv File');
  AAdd('ButtonHint','ClearDailyFlowDataFromCSVFile','Clear Daily Flow Data');
  AAdd('ButtonHint','ImportDailyInstreamFlowFile','Import Daily Instream Flow Requirement from csv file');
  AAdd('ButtonHint','ClearDailyInstreamFlowFile','Clear Daily Instream Flow');
  AAdd('ButtonHint','GenerateFlowDiversionRelation','Generate Flow Diversion Relationship');
  AAdd('ButtonHint','ImportFile14','Import File 14(F14 *.dat)');
  AAdd('ButtonHint','ClearFile14','Clear File 14(F14 *.dat)');
  AAdd('ButtonHint','ClearFlowDiversionRelation','Clear Flow Diversion Relationship');
  AAdd('ButtonHint','GenerateWRYMData','Export WRYM Data');
  AAdd('ButtonHint','ClearWRYMData','Clear WRYM Data');

  AAdd('ButtonHint','CreateIFRSite','Create a new IFR Site');
  AAdd('ButtonHint','DeleteIFRSite','Delete the seleted IFR Site');
  AAdd('ButtonHint','SaveIFRSite','Save new/updated IFR Site data');

  AAdd('ButtonHint','StomsaFileNew'      ,'New File');
  AAdd('ButtonHint','StomsaFileOpen'     ,'Open File');
  AAdd('ButtonHint','StomsaFileOpenParam','Open PARAM File');
  AAdd('ButtonHint','StomsaFileSave'     ,'Save File');
  AAdd('ButtonHint','StomsaFileMerge'    ,'Merge Files');
  AAdd('ButtonHint','StomsaFileClose'    ,'Close File');
  AAdd('ButtonHint','StomsaFileExport'    ,'Export files from database');
  AAdd('ButtonHint','StomsaFileImport'    ,'Import files into the database');
  //
  // Button status reasons.
  //
  AAdd('ButtonReason','ImportDiversionRelationship'    ,'This button is disabled because there is no relationship data'#13#10 +
                                                        ' in the Daily Diversion Pre-Processor ');
  AAdd('ButtonReason','RDFileIsLoaded','File is loaded');
  AAdd('ButtonReason','RDFileIsNotLoaded','File is not loaded');
  AAdd('ButtonReason','ClipboardDisabled',         'This button is disabled because the currently focussed control'#13#10 +
                                                   '  cannot export to the clipboard.');
  AAdd('ButtonReason','ExportDisabled',            'This button is disabled because the currently focussed control'#13#10 +
                                                   '  cannot export data.');
  AAdd('ButtonReason','PrintDisabled',             'This button is disabled because the currently focussed control'#13#10 +
                                                   '  cannot print it''s contents.');
  AAdd('ButtonReason','PrintSettingsDisabled',     'This button is disabled because there are no printer'#13#10 +
                                                   '  installed on this computer.');
  AAdd('ButtonReason','ActionCreateReservoirDisabled','This button is disabled because you do not have rights'#13#10 +
                                                   '  to create a reservoir');
  AAdd('ButtonReason','ActionDeleteReservoirDisabled','This button is disabled because you do not have rights'#13#10 +
                                                   '  to delete a reservoir or there is no selected reservoir');
  AAdd('ButtonReason','ActionCreateNodeWithInflowDisabled','This button is disabled because you do not have rights'#13#10 +
                                                   '  to create a node with inflow');
  AAdd('ButtonReason','ActionDeleteNodeWithInflowDisabled','This button is disabled because you do not have rights'#13#10 +
                                                   '  to delete a node with inflow or there is no selected node');
  AAdd('ButtonReason','ActionCreateNodeWithoutInflowDisabled','This button is disabled because you do not have rights'#13#10 +
                                                   '  to create a node without inflow');
  AAdd('ButtonReason','ActionDeleteNodeWithoutInflowDisabled','This button is disabled because you do not have rights'#13#10 +
                                                   '  to delete a node without inflow or there is no selected node');
  AAdd('ButtonReason','ActionPivotDisabled',         'This button is disabled because the current grid cannot'#13#10 +
                                                   '  rotate the data by swapping the rows and columns.');
  AAdd('ButtonReason','ValidateFilesDisabled',     'This button is disabled because the model'#13#10 +
                                                   '  data files cannot be found.');
  AAdd('ButtonReason','ImportFilesDisabled',       'This button is disabled because the model'#13#10 +
                                                   '  data files cannot be found.');
  AAdd('ButtonReason','ExportFilesDisabled',       'This button is disabled because the model data cannot be'#13#10 +
                                                   '  exported until a successful import has been performed.');
  AAdd('ButtonReason','ClearFilesDisabled',        'This button is disabled because the model data cannot be cleared'#13#10 +
                                                   '  from the database until a successful import has been performed.');
  AAdd('ButtonReason','RunModelDisabled',          'This button is disabled because the model'#13#10 +
                                                   '  data files cannot be found.');
  AAdd('ButtonReason','ValidateFileDisabled',      'This button is disabled because the current file cannot be'#13#10 +
                                                   '  found on the hard drive or a file is not yet selected.');
  AAdd('ButtonReason','ValidateModelDataDisabled', 'This button is disabled because there is no model data to'#13#10 +
                                                   '  be validated. Add some reservoirs or channels.');
  AAdd('ButtonReason','GenerateSysConfigFilesDisabled', 'This button is disabled because there is no system configuration data files to'#13#10 +
                                                   '  be genrated.');

  AAdd('ButtonReason','ImportFileDisabled',        'This button is disabled because the current file cannot be'#13#10 +
                                                   '  found on the hard drive or a file is not yet selected'#13#10 +
                                                   '  or all study data has not yet been successfully imported.');

  AAdd('ButtonReason','ImportMonthlyDamWaterLevelsDisabled','This button is disabled because the current file has not yet'#13#10 +
                                                   '  been imported into the database or a file is not yet selected.');

  AAdd('ButtonReason','ExportFileDisabled',        'This button is disabled because the current file has not yet'#13#10 +
                                                   '  been imported into the database or a file is not yet selected.');
  AAdd('ButtonReason','ClearFileDisabled',         'This button is disabled because the current file has not yet'#13#10 +
                                                   '  been imported into the database or a file is not yet selected.');
  AAdd('ButtonReason','SaveFileDisabled',          'This button is disabled because the current file'#13#10 +
                                                   '  has not yet been changed or a file is not yet selected.');
  AAdd('ButtonReason','AddSeriesDisabled',         'This button is disabled because there is no series selected'#13#10 +
                                                   '  yet or the selected series has already been added.');
  AAdd('ButtonReason','SeriesColorDisabled',         'This button is disabled because there is no series selected');
  AAdd('ButtonReason','RemoveSeriesDisabled',      'This button is disabled because there is no series selected'#13#10 +
                                                   '  yet or the selected series has not yet been added.');
  AAdd('ButtonReason','AddChartDisabled',          'This button is disabled because there is no chart selected'#13#10 +
                                                   '  yet or the selected chart has already been added.');
  AAdd('ButtonReason','RemoveChartDisabled',       'This button is disabled because there is no chart selected'#13#10 +
                                                   '  yet or the selected chart has not yet been added.');
  AAdd('ButtonReason','SaveViewDisabled',          'This button is disabled because there is no view'#13#10 +
                                                   '  currently selected or the current view has not yet been changed.');
  AAdd('ButtonReason','CreateChartDisabled',       'This button is disabled because the system can not '#13#10 +
                                                   '  create a new chart now. Please reselect the scenario.');
  AAdd('ButtonReason','CreateViewDisabled',        'This button is disabled because the system can not '#13#10 +
                                                   '  create a new view now. Please reselect the scenario.');
  AAdd('ButtonReason','DeleteChartDisabled',       'This button is disabled because there is no chart'#13#10 +
                                                   '  currently selected.');
  AAdd('ButtonReason','DeleteViewDisabled',        'This button is disabled because there is no view'#13#10 +
                                                   '  currently selected.');
  AAdd('ButtonReason','RenameChartDisabled',       'This button is disabled because there is no chart'#13#10 +
                                                   '  currently selected.');
  AAdd('ButtonReason','RenameViewDisabled',        'This button is disabled because there is no view'#13#10 +
                                                   '  currently selected.');
  AAdd('ButtonReason','SaveChartDisabled',         'This button is disabled because the chart'#13#10 +
                                                   '  does not need to be saved or there is no chart.');
  AAdd('ButtonReason','ChartNameDisabled',         'This button is disabled because there is'#13#10 +
                                                      '  no chart currently added to the view.');
  AAdd('ButtonReason','ShowChartLegendDialogDisabled',  'This button is disabled because there is'#13#10 +
                                                   '  no chart currently added to the view.');
  AAdd('ButtonReason','TSCToggleViewDisabled',     'This button is disabled because there is'#13#10 +
                                                   '  no chart currently added to the view.');


  AAdd('ButtonReason','TogglePlaneModeDisabled',   'This button is disabled because the chart is'#13#10 +
                                                   '  already in plane mode or there is no chart.');
  AAdd('ButtonReason','ToggleChartModeDisabled',   'This button is disabled because the chart is'#13#10 +
                                                   '  already in chart mode or there is no chart.');
  AAdd('ButtonReason','ToggleCurveManipulationDisabled', 'This button is disabled because there is'#13#10 +
                                                         '    plot plane currently applied.');
  AAdd('ButtonReason','ResetChartDataDisabled',    'This button is disabled because there is no chart'#13#10 +
                                                   ' or no changes has been made to the chart.');
  AAdd('ButtonReason','LoadFromDBDisabled',        'This button is disabled because there is no chart'#13#10 +
                                                   '  data previously stored in the database.');
  AAdd('ButtonReason','LoadFromFileDisabled',      'This button is disabled because there is no'#13#10 +
                                                   '  output file specified. (Check file editor tabsheet)');
  AAdd('ButtonReason','LoadFromCoefFileDisabled',  'This button is disabled because  the deterministic'#13#10 +
                                                   ' method of curve fitting has not been selected.');
  AAdd('ButtonReason','DeleteTargetDraftDisabled','This button is disabled because there is no selected target draft.');
  AAdd('ButtonReason','ChartPrintModeDisabled','This button is disabled because there is no chart.');

  AAdd('ButtonReason','NVDrawingDisabled',         'This button is disabled because no drawing is currently active.'#13#10 +
                                                   '  Select a drawing from the list.');
  AAdd('ButtonReason','NVDrawingNotDirtyDisabled', 'This button is disabled because the currently'#13#10 +
                                                   '  selected drawing does not need to be saved.');
  AAdd('ButtonReason','ActionCreateChannelDisabled',
       'This button is disabled because you do not have rights'#13#10 +
       '  to create a channel');
  AAdd('ButtonReason','ActionDeleteChannelDisabled',
       'This button is disabled because you do not have rights'#13#10 +
       '  to delete a channel or there is no selected channel');

  AAdd('ButtonReason','ActionConvertChannelDisabled',
       'This button is disabled because you do not have rights'#13#10 +
       '  to convert a channel, there is no selected channel or'#13#10 +
       '  selected channel is a general flow channel');
  AAdd('ButtonReason','ActionCreateFeatureDisabled',
       'This button is disabled because you do not have rights'#13#10 +
       '  to create a network feature or there is no general'#13#10 +
       '  flow channel selected');
  AAdd('ButtonReason','ActionCreatePumpingFeatureDisabled',
       'This button is disabled because you do not have rights'#13#10 +
       '  to create a pumping feature or there is no min-max'#13#10 +
       '  channel selected');
  AAdd('ButtonReason','ActionDeletePumpingFeatureDisabled',
       'This button is disabled because you do not have rights'#13#10 +
       '  to delete a pumping feature, there is no selected channel or'#13#10 +
       '  selected channel does not have a pumping feature');
  AAdd('ButtonReason','ActionCreateIFRFeatureDisabled',
       'This button is disabled because you do not have rights'#13#10 +
       '  to create an IFR feature or there is no min-max'#13#10 +
       '  channel selected');
  AAdd('ButtonReason','ActionCopyIFRFeatureDisabled',
       'This button is disabled because you do not have rights'#13#10 +
       '  to copy an IFR feature.');
  AAdd('ButtonReason','ActionCreateYMDemandCentreRecFeatureDisabled',
       'This button is disabled because you do not have rights'#13#10 +
       '  to create a Demand Centre Reclaimation feature or there is no Demand Centre'#13#10 +
       '  channel selected');
  AAdd('ButtonReason','ActionCreateSFRSubCatchmentDisabled',
       'This button is disabled because you do not have rights'#13#10 +
       '  to create a Stream Flow Reduction sub-catchment.');
  AAdd('ButtonReason','ActionCreateSpecifiedInflowFeatureDisabled',
       'This button is disabled because you do not have rights'#13#10 +
       '  to create a Specified Inflow feature or the downstream node'#13#10 +
       '  of the selected channel is a node without inflow');
  AAdd('ButtonReason','ActionDeleteIFRFeatureDisabled',
       'This button is disabled because you do not have rights'#13#10 +
       '  to delete an IFR feature, there is no selected channel or'#13#10 +
       '  selected channel does not have an IFR feature');
  AAdd('ButtonReason','ActionDeleteYMDemandCentreRecFeatureDisabled',
       'This button is disabled because you do not have rights'#13#10 +
       '  to delete a Demand Centre Reclaimation feature, there is no selected channel or'#13#10 +
       '  selected channel does not have an Demand Centre Reclaimation feature');
  AAdd('ButtonReason','ActionDeleteSFRSubCatchmentDisabled',
       'This button is disabled because you do not have rights'#13#10 +
       '  to delete a Stream Flow Reduction sub-catchment');
  AAdd('ButtonReason','ActionCreatePhysicalFlowConstraintDisabled',
       'This button is disabled because you do not have rights'#13#10 +
       '  to create a physical flow constraint or there is no min-max'#13#10 +
       '  or general flow channel selected');
  AAdd('ButtonReason','ActionDeletePhysicalFlowConstraintDisabled',
       'This button is disabled because you do not have rights'#13#10 +
       '  to delete a physical flow constraint, there is no selected channel'#13#10 +
       '  or selected channel does not have a physical flow constraint');
  AAdd('ButtonReason','ActionCreateMasterControlFeatureDisabled',
       'This button is disabled because you do not have rights'#13#10 +
       '  to create a master control feature or there is no'#13#10 +
       '  general flow channel selected');
  AAdd('ButtonReason','ActionDeleteMasterControlFeatureDisabled',
       'This button is disabled because you do not have rights'#13#10 +
       '  to delete a master control feature, there is no selected channel'#13#10 +
       '  or selected channel does not have a master control feature');
  AAdd('ButtonReason','ActionCreateIrrigationAreaDisabled',
       'This button is disabled because you do not have rights'#13#10 +
       '  to create an irrigation area');
  AAdd('ButtonReason','ActionDeleteIrrigationAreaDisabled',
       'This button is disabled because you do not have rights'#13#10 +
       '  to delete an irrigation area or there is no irrigation'#13#10 +
       '  area selected');
  AAdd('ButtonReason','ActionCreateIrrigationBlockDisabled',
       'This button is disabled because you do not have rights'#13#10 +
       '  to create an irrigation block');
  AAdd('ButtonReason','ActionDeleteIrrigationBlockDisabled',
       'This button is disabled because you do not have rights'#13#10 +
       '  to delete an irrigation block or there is no irrigation'#13#10 +
       '  area selected');
  AAdd('ButtonReason','ActionCreateWetlandDisabled',
       'This button is disabled because you do not have rights'#13#10 +
       '  to create a wetland');
  AAdd('ButtonReason','ActionDeleteWetlandDisabled',
       'This button is disabled because you do not have rights'#13#10 +
       '  to delete a wetland or there is no wetland'#13#10 +
       '  area selected');
  AAdd('ButtonReason','ActionCreateYMDemandCentreDisabled',
       'This button is disabled because you do not have rights'#13#10 +
       '  to create a demand centre');
  AAdd('ButtonReason','ActionDeleteYMDemandCentreDisabled',
       'This button is disabled because you do not have rights'#13#10 +
       '  to delete a demand centre or there is no demand centre'#13#10 +
       '  area selected');
  AAdd('ButtonReason','ActionCreatePowerPlantDisabled',
       'This button is disabled because you do not have rights'#13#10 +
       '  to create a power plant');
  AAdd('ButtonReason','ActionDeletePowerPlantDisabled',
       'This button is disabled because you do not have rights'#13#10 +
       '  to delete a power plant or there is no power'#13#10 +
       '  plant selected');
  AAdd('ButtonReason','ActionInvokeWizardDisabled',
       'This button is disabled because you do not have rights'#13#10 +
       '  to invoke a wizard, there is no selected item or'#13#10 +
       '  there is no wizard for the selected item');

  AAdd('ButtonReason','ActionCreateWaterDemandFeatureDisabled',
       'This button is disabled because you do not have rights'#13#10 +
       '  to create a water demand feature or there is no min-max'#13#10 +
       '  or specified demand channel selected');
  AAdd('ButtonReason','ActionDeleteMasterControlFeatureDisabled',
       'This button is disabled because you do not have rights'#13#10 +
       '  to delete a water demand feature, there is no selected channel'#13#10 +
       '  or selected channel does not have a water demand feature');
  AAdd('ButtonReason','DeleteReportDisabled',
       'This button is disabled because there are no reports for this study area.');
  AAdd('ButtonReason','EditReportDisabled',
       'This button is disabled because there are no reports for this study area.');

  AAdd('ButtonReason','VNVNewDrawingGroupDisabled',
       'This button is disabled because the system is unable to add a new drawing group.');
  AAdd('ButtonReason','VNVDeleteDrawingGroupDisabled',
       'This button is disabled because there is no selected drawing group to delete.');
  AAdd('ButtonReason','VNVNewDrawingDisabled',
       'This button is disabled because there is no selected drawing group to create a drawing under.');
  AAdd('ButtonReason','VNVDeleteDrawingDisabled',
       'This button is disabled because there is no selected drawing to delete.');
  AAdd('ButtonReason','VNVEditDrawingDisabled',
       'This button is disabled because there is no selected drawing to edit.');
  AAdd('ButtonReason','VNVViewDrawingDisabled',
       'This button is disabled because there is no selected drawing to view.');
  AAdd('ButtonReason','VNVCopyDrawingDisabled',
       'This button is disabled because there is no selected drawing to copy.');

  // study selection dialog buttons....
  AAdd('ButtonReason','Report',
       'This button is disabled because there is no study document detail');
  AAdd('ButtonReason','OK',
       'This button is disabled because there is no study selected to be loaded');
  AAdd('ButtonReason','Cancel',
       'This button is disabled because the dialog is busy');
  AAdd('ButtonReason','New',
       'This button is disabled because a new entry cannot be added at that level');
  AAdd('ButtonReason','Copy',
       'This button is disabled because the selected study/study sub-area/scenario cannot be copied');
  AAdd('ButtonReason','Edit',
       'This button is disabled because the selected study/study sub-area/scenario cannot be edited');
  AAdd('ButtonReason','Delete',
       'This button is disabled because the selected study/study sub-area/scenario cannot be Deleted');
  AAdd('ButtonReason','FirmYieldNewChartDisabled',
       'This button is disabled you are a read only user or there is no YRC chart that you are currently working on.');
  AAdd('ButtonReason','FirmYieldOpenChartDisabled',
       'This button is disabled because there is no YRC chart that you are currently working on.');
  AAdd('ButtonReason','FirmYieldSaveChartDisabled',
       'This button is disabled because there is no valid chart to be saved');
  AAdd('ButtonReason','FirmYieldAddSeriesDisabled',
       'This button is disabled because there is no chart to add the series to.');
  AAdd('ButtonReason','FirmYieldDeleteSeriesDisabled',
       'This button is disabled because there is no series currently selected');
  AAdd('ButtonReason','DeleteChartDisabled',
       'This button is disabled because there is no chart currently saved in the database');

  AAdd('ButtonReason','ActionDeleteSwitchDefDisabled',
       'This button is disabled because in the selected study/study sub-area/scenario,' + #13#10 +
       ' you can not delete switch definition ');
  AAdd('ButtonReason','ActionCreateSwitchDefDisabled',
       'This button is disabled because  in the selected study/study sub-area/scenario,' + #13#10 +
       ' you can not create switch definition ');
  AAdd('ButtonReason','ActionCreateAllocDefDisabled',
       'This button is disabled because  in the selected study/study sub-area/scenario,' + #13#10 +
       ' you can not create an alloction definition ');
  AAdd('ButtonReason','ActionDeleteAllocDefDisabled',
       'This button is disabled because  in the selected study/study sub-area/scenario,' + #13#10 +
       ' you can not delete an alloction definition ');

  AAdd('ButtonReason','CreateIFRSiteDisabled',
       'This button is disabled because the maximum number of IFR sites has been reached.');
  AAdd('ButtonReason','DeleteIFRSiteDisabled',
       'This button is disabled because there is no seleted IFR Site to be deleted');
  AAdd('ButtonReason','SaveIFRSiteDisabled',
       'This button is disabled because there is no new or changed IFR site data');

  AAdd('ButtonReason','StomsaFileNewDisabled'      ,
       'This button is disabled because there is no stomsa file selected or saved in the database');
  AAdd('ButtonReason','StomsaFileOpenDisabled'     ,
       'This button is disabled because there is no stomsa file selected or saved in the database');
  AAdd('ButtonReason','StomsaFileOpenParamDisabled',
       'This button is disabled because there is no stomsa file selected or saved in the database');
  AAdd('ButtonReason','StomsaFileSaveDisabled'     ,
       'This button is disabled because there is no stomsa file selected or saved in the database');
  AAdd('ButtonReason','StomsaFileMergeDisabled'    ,
       'This button is disabled because there is no stomsa file selected or saved in the database');
  AAdd('ButtonReason','StomsaFileCloseDisabled'    ,
       'This button is disabled because there is no stomsa file selected or saved in the database');
  AAdd('ButtonReason','StomsaFileExportDisabled'    ,
       'This button is disabled because there is no hydrology file/s imported into the database');
  AAdd('ButtonReason','StomsaFileImportDisabled'    ,
       'This button is disabled because there is no hydrology file/s selected');
end;

end.
