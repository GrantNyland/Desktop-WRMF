//
//  UNIT      : Contains language text.
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/01/23
//  COPYRIGHT : Copyright © 2003 DWAF
//
unit ULanguageText_ENG_General;

interface

type TTextItemAddFunction = procedure (AContext, AConstant, AText: string) of object;

procedure LoadLanguageText(AAdd: TTextItemAddFunction);
procedure LoadMoreLanguageText(AAdd: TTextItemAddFunction);
procedure LoadMoreLanguageText1(AAdd: TTextItemAddFunction);
procedure LoadMoreLanguageText2(AAdd: TTextItemAddFunction);
procedure LoadMoreLanguageText3(AAdd: TTextItemAddFunction);
procedure LoadMoreLanguageText4(AAdd: TTextItemAddFunction);

implementation

procedure LoadLanguageText(AAdd: TTextItemAddFunction);
const OPNAME = 'LoadLanguageText';
begin
  AAdd('WRYMFileLines','LineData','Unknown data');

  AAdd('YRC','BottomAxisCaption','Reliability of Supply %');
  AAdd('YRC','LeftAxisCaption','Target Draft');
  AAdd('YRC','LegendCaption','Recurrence interval = 1 / ((1-(1-Rn)^(1/n)))');
  AAdd('YRC','YearsFormatStr','%g in %g years');
  AAdd('YRC','YieldFormatStr','%g million m³/a');
  AAdd('YRC','LabelStr','No transfer from Thukela');
  AAdd('YRC','CaptionFormatStr1','Firm yield derived from (%s)');
  AAdd('YRC','CaptionFormatStr2','%d Stochastic Sequences - Plotting Base = %d years - Period Length = %d years');
  AAdd('YRC','SumOutOlder','The SUM.OUT file on the harddrive is older than the one imported into the database');
  AAdd('YRC','SumOutYounger','The SUM.OUT file on the harddrive is newer than the one imported into the database');
  AAdd('ZonePolicy','PenaltyStructureCount','Number of penalty structures');
  AAdd('ZonePolicy','StorageZoneCount','Number of storage zones in each reservoir');
  AAdd('ZonePolicy','StorageZoneNumber','Storage zone with rule curve as lower boundary');
  AAdd('PositiveInfinity','>0','>0');
  AAdd('NegativeInfinity','<0','<0');
  AAdd('OutputCompliance','LeftAxisCaption','Reservoir Storage Volume');
  AAdd('OutputBoxPlotGraphChart','LeftAxisCaption','Monthly reservoir storage');
  AAdd('ChannelOutputBoxPlotGraphChart','LeftAxisCaption','Monthly average channel flow (m³/s)');
  AAdd('MiningEvaporation','PanEvapLeftAxis','Mine Pan Evaporation');

  AAdd('YRCPopup','Seperator','-');

  AAdd('YRCPopup','HideAllRawPoints','Hide all raw data points');
  AAdd('YRCPopup','HideFittedPoints','Hide all fitted points');
  AAdd('YRCPopup','DisableRawPoints','Disable outlying raw data points');

  AAdd('YRCPopup','CurveFitted','Curve fitted');
  AAdd('YRCPopup','StartEdit','Start Edit...');
  AAdd('YRCPopup','EndEdit','End Edit...');

  AAdd('YRCPopup','YValue','When X=100% Y=');
  AAdd('YRCPopup','ForceCurve','Force curve to fit point');

  AAdd('YRCPopup','EditYValue','Edit Y-value...');
  AAdd('YRCPopup','AddRegressionPoints','Add Regression Points');
  AAdd('YRCPopup','ManipulateDeterministic','Manipulate Deterministic Points');

  LoadMoreLanguageText(AAdd);
  LoadMoreLanguageText1(AAdd);
  LoadMoreLanguageText2(AAdd);
  LoadMoreLanguageText3(AAdd);
  LoadMoreLanguageText4(AAdd);

end;

procedure LoadMoreLanguageText(AAdd: TTextItemAddFunction);
const OPNAME = 'LoadMoreLanguageText';
begin

  AAdd('Message','Copying','Copying : ');
  AAdd('Message','Deleting','Deleting : ');
  AAdd('Message','ProcessingRainfallMonthlyPatchData','Processing table : RainfallMonthlyPatchData');
  AAdd('Message','ProcessingRainfallPatchSource','Processing table : RainfallPatchSource');
  AAdd('Message','ProcessingRainfallPatchR','Processing table : RainfallPatchR');
  AAdd('Message','ProcessingRainfallProjectGauges','Processing table : RainfallProjectGauges');
  AAdd('Message','ProcessingRainfallCatchmentSource','Processing table : RainfallCatchmentSource');
  AAdd('Message','ProcessingRainfallcatchmentFiledData','Processing table : RainfallcatchmentFiledData');
  AAdd('Message','ProcessingRainfallcatchmentFiledDetail','Processing table : RainfallcatchmentFiledDetail');
  AAdd('Message','ProcessingRainfallCatchment','Processing table : RainfallCatchment');
  AAdd('Message','CreatingZipFile','Creating the zip file');
  AAdd('Message','CreatingRListFiles','Creating ClassR and PatchR List File.');
  AAdd('Message','IncludeRainfallBaseData','Include Rainfall Base Data');
  AAdd('Message','IncludeOutputData','Include Output Data');
  AAdd('Message','IncompatibleZipFile','Incompatible zip file. Please select the zip exported with EXPORT ALL option');
  AAdd('Message','ExtractingZipFile','Extracting zip file: ');
  AAdd('Message','ErrorWhileExtracting','Error: Extracting zip file: ');
  AAdd('Message','PreProcessingZipFile','PreProcessing zip file');
  AAdd('Message','UpgradingFileData','Upgrading file data.');
  AAdd('Message','PreProcessingRainfallUserStations','PreProcessing table RainfallUserStations.');
  AAdd('Message','PreProcessingRainfallUserMonthlyData','PreProcessing table RainfallUserMonthlyData.');
  AAdd('Message','PreProcessingRainfallPatchR','PreProcessing table RainfallPatchR.');
  AAdd('Message','PreProcessingStudyScenario','PreProcessing table StudyScenario.');
  AAdd('Message','PreProcessingRainfallRAWSplits','PreProcessing table RainfallRAWSplits.');
  AAdd('Message','PreProcessingRainfallProjectGauges','PreProcessing table RainfallProjectGauges.');
  AAdd('Message','PreProcessingRainfallCatchment','PreProcessing table RainfallCatchment.');
  AAdd('Message','PreProcessingRainfallCatchmentSource','PreProcessing table RainfallCatchmentSource.');
  AAdd('Message','PreProcessingRainfallPatchSource','PreProcessing table RainfallPatchSource.');
  AAdd('Message','PreProcessingRainfallMonthlyPatchData','PreProcessing table RainfallMonthlyPatchData.');
  AAdd('Message','PreProcessingChangeListFiles','Pre-Processing change list files');
  AAdd('Message','NetworkDiagramsFound','Would you also like to delete the network diagrams for this study?');
  AAdd('Message','subsystemsLoadCaseChange','You entered a less Load Case Number, specified Load Case Number would be'+#10#13+
                                        'applicable to all the sub-systems included within the allocation definition group. '+#10#13+
                                        'Continue?');
  AAdd('Message','NonOrDivImport','Do you want to import Non-Diversion or Diversion?');
  AAdd('Message','NonDiv','Non-Div.');
  AAdd('Message','MineNotExist','Mine (');
  AAdd('Message','GroundWaterNotExist','Ground Water (');
  AAdd('Message','ReservoirDeleteWarning','The Reservoir you have selected will be deleted. Would you like to delete the Reservoir?');

  AAdd('TIFRSiteDialog', 'IFRSiteName', 'Name');
  AAdd('TIFRSiteDialog', 'IFRSiteLocationGroup', 'Location');
  AAdd('TIFRSiteDialog', 'IFRSiteXCoord', 'XCoodinate');
  AAdd('TIFRSiteDialog', 'IFRSiteYCoord', 'YCoodinate');
  AAdd('TIFRSiteDialog', 'IFRSiteQuaternary', 'Quaternary catchment');
  AAdd('TIFRSiteDialog', 'IFRSiteRiver', 'River');
  AAdd('TIFRSiteDialog', 'IFRSiteDescription', 'Description');
  AAdd('TIFRSiteDialog', 'IFRSiteAssociatedEMC', 'Associated EMC');
  AAdd('TIFRSiteDialog', 'IFRSiteDetailLevel', 'Level of detail');
  AAdd('TIFRSiteDialog', 'IFRSiteConfidenceLevel', 'Level of confidence');
  AAdd('TIFRSiteDialog', 'IFRSiteInfoSource', 'Source');
  AAdd('TIFRSiteDialog', 'IFRSiteTableTabSheet', 'Grid');
  AAdd('TIFRSiteDialog', 'IFRSiteGraphTabSheet', 'Graph');
  AAdd('TIFRSiteDialog', 'IFRSiteGraphViewType', 'View Data');

  AAdd('TIFRSiteDialog', 'IFRSiteConfidenceLow', 'Low');
  AAdd('TIFRSiteDialog', 'IFRSiteConfidenceMedium', 'Medium');
  AAdd('TIFRSiteDialog', 'IFRSiteConfidenceHigh', 'High');

  AAdd('TIFRSiteDialog', 'IFRSiteDetailDesktop', 'Desktop');
  AAdd('TIFRSiteDialog', 'IFRSiteDetailRapid', 'Rapid');
  AAdd('TIFRSiteDialog', 'IFRSiteDetailIntermediate', 'Intermediate');
  AAdd('TIFRSiteDialog', 'IFRSiteDetailComprehensive', 'Comprehensive');
  AAdd('TIFRSiteDialog', 'IFRSiteAll', 'All');
  AAdd('TIFRSiteDialog', 'ExceedenceProbability','Exceedence Probability (%)');
  AAdd('TIFRSiteDialog', 'DefinedIFRS','Defined IFRS (Mm³/mon)');
  AAdd('TIFRFeatureDialog','DefinedReferenceFlow','Defined reference flow');
  AAdd('TDiversionChannelDialog','DiversionChannelDialog','Diversion Channel Dialog');


//Grid Headings

  AAdd('GridHeading','BoardPillarCatchmentArea','BoardPillar Catchmen Area');

  AAdd('GridHeading','CoalReserveArea','Coal Reserve Area (km²)');
  AAdd('GridHeading','ChannelNumberToUGDam','Channel Number To UG Dam');

  AAdd('GridHeading','DownStreamMineNode'    ,'Downstream Mine Node');
  AAdd('GridHeading','DownStreamPCDNode'    ,'Downstream PCD Node');
  AAdd('GridHeading','DisturbedWorkingsArea','Disturbed Workings Area');
  AAdd('GridHeading','DisturbedArea','Disturbed Area');
  AAdd('GridHeading','DisturbedAreaRunOff','Disturbed Area RunOff Factor');
  AAdd('GridHeading','DisturbedWorkingsAreaRunOff','Disturbed Workings Area RunOff Factor');
  AAdd('GridHeading','DisturbedMonthlyRecharge', 'Disturbed Monthly Recharge Factors');
  AAdd('GridHeading','DisturbedWorkingsMonthlyRecharge', 'Disturbed Workings Recharge Factors');
  AAdd('GridHeading','DumpID','Dump ID');

  AAdd('GridHeading','HighExtractionCatchmentArea','High Extraction Catchment Area');
  AAdd('GridHeading','HighExtractionAreaRunoffFactor','High Extraction Area Runoff Factor');
  AAdd('GridHeading','MineUGUpstreamRunoff','Mine UG Upstream Runoff');

  AAdd('GridHeading','PCDAnalysisStartVolume','PCD Analysis Start Volume');
  AAdd('GridHeading','PCDStorageCapacity','PCD Storage Capacity');
  AAdd('GridHeading','PCDSurfaceArea', 'PCD Surface Area');
  AAdd('GridHeading','PitID','Pit ID');

  AAdd('GridHeading','RunoffFactorToPCD', 'Runoff Factor To PCD');

  AAdd('GridHeading','UGBoardPillarRechargeFactors','UG BoardPillar Recharge Factors');
  AAdd('GridHeading','UGHighExtractionRechargeFactors','UG High Extraction Recharge Factors');
  AAdd('GridHeading','UndergroundMineID','Underground Mine ID');
  AAdd('GridHeading','UnderGroundSectionName','UnderGround Section Name');
  AAdd('GridHeading','UpstreamCatchmentArea','Upstream Catchment Area');
  AAdd('GridHeading','UpStreamMineNode'    ,'Upstream Mine Node');
  AAdd('GridHeading','UpStreamNode'    ,'UpStream Node');

  AAdd('GridHeading','WorkingsArea','Workings Area (km²)');
  AAdd('TDailyIFRDataValidator','strGenerateIFR','Generating Daily IFR From *.F14');

  AAdd('GridHeading','Count','Count');
  AAdd('GridHeading','Flow','Flow');
  AAdd('GridHeading','IFR','IFR');
  AAdd('GridHeading','Date','Date');
  AAdd('GridHeading','DailyIFR','Daily IFR');

  AAdd('GridHeading','FactoredFlow','Factored Flow');
  AAdd('GridHeading','FactoredIFR','Factored IFR');
  AAdd('GridHeading','ReferenceFlow','Reference Flow(m' + CHAR(179) + '/s)');
  AAdd('GridHeading','DiversionFlow','Diversion Flow(m' + CHAR(179) + '/s)');
  AAdd('GridHeading','NonDiversionFlow','Non-Diversion Flow(m' + CHAR(179) + '/s)');
  AAdd('GridHeading','Rank','Rank');
  AAdd('GridHeading','DatasetName','Dataset Name');
  AAdd('GridHeading','AreaDescription','Area Description');
  AAdd('GridHeading','Period','Period');
  AAdd('GridHeading','Replacement','Replacement');
  AAdd('GridHeading','MinePanEvaporation','Mine Pan Evaporation');
  AAdd('GridHeading','ReservoirsDescription1','Reservoirs Description [1]');
  AAdd('GridHeading','ReservoirsDescription2','Reservoirs Description [2]');
  AAdd('GridHeading','AverageVolume','Average Volume');
  AAdd('GridHeading','ChannelDescription1','Channel Description [1]');
  AAdd('GridHeading','ChannelDescription2','Channel Description [2]');
  AAdd('GridHeading','AverageFlow','Average Flow');
  AAdd('GridHeading','Iteration','Iteration');
  AAdd('GridHeading','TargetDraft','Target Draft');
  AAdd('GridHeading','Result','Result');
  AAdd('GridHeading','SymonsPanEvaporation','Symons Pan Evaporation (mm)');
  AAdd('ChartBottomAxis','ReferenceFlow','Reference Flow(m' + CHAR(179) + '/s)');
  AAdd('ChartLeftAxis','DiversionFlow','Diversion Flow(m' + CHAR(179) + '/s)');
  AAdd('ChartLeftAxis','NonDiversionFlow','Non-Diversion Flow(m' + CHAR(179) + '/s)');

  AAdd('FontName','CourierNew','Courier New');

  // CheckListBoxes
  AAdd('CheckListBoxText','StopOnFirstError', 'Stop on first error');

  // CheckBoxesCaption
  AAdd('CheckBoxCaption','ShowBaseDatasets', 'Show base datasets');
  AAdd('CheckBoxCaption','ShowUserDatasets', 'Show user datasets');
  AAdd('CheckBoxCaption','ShowDatasetFields', 'Show dataset fields');
  AAdd('CheckBoxCaption','ShowDatasetChangelists', 'Show dataset changelists');

  //Groupboxcaption
  AAdd('GroupBoxCaption','RowMetadata','Row metadata');
  AAdd('GroupBoxCaption','IFRTypeGrpBox','Please Specify IFR Type');

  //DialogCaption
  AAdd('DialogCaption','TBSPChangelistdialog'          ,'TBSPChangelistdialog');
  AAdd('DialogCaption','StartingStorageCalculate'      ,'Calculate starting storage level');
  AAdd('DialogCaption','TProgressDialog'               ,'Validate Growth Projection');
  AAdd('DialogCaption','TIFRTypeDialog'                ,'IFR Type');

  //PanelCaption
  AAdd('PanelCaption','pnlLeft','pnlLeft');
  AAdd('PanelCaption','PnlOf','of 0');

  //RadioGroupCaption
  AAdd('RadioGroupCaption','ViewType','View Type');
  AAdd('RadioGroupCaption','Filter','Filter');
  AAdd('RadioGroupCaption','OutputInWhichFormat',' Output in which format ? ');

  //RadioCaption
  AAdd('RadioCaption','CumulativeMass','Cumulative mass');
  AAdd('RadioCaption','MonthNumbers','Month Numbers');
  AAdd('RadioCaption','YearMonthNames','Year/Month Names');

  //MenuCaption
  AAdd('MenuCaption','Changes','Changes');
  AAdd('MenuCaption','Metadata','Metadata');
  AAdd('MenuCaption','AddNewGroupRoot','Add New Group - Root');
  AAdd('MenuCaption','AddNewGroup','Add New Group');
  AAdd('MenuCaption','AddNewList','Add New List');
  AAdd('MenuCaption','AddNewItem','Add new Item');
  AAdd('MenuCaption','DeleteSelected','Delete Selected');

  //FormCaption
  AAdd('FormCaption','NewChangeList','New Change List');
  AAdd('FormCaption','ChangeListsDataset','Change Lists for dataset: ');
  AAdd('FormCaption','NewGroup','New Group');
  AAdd('FormCaption','ChangeListItemInformation','ChangeList Item Information');
  AAdd('FormCaption','MetadataForm','Metadata Form');
  AAdd('FormCaption','BoxPlotSeries','Box plot series legend');
  AAdd('NetworkFeatures','MineToRiverChannelProperties','Mine To River Channel');
  AAdd('NetworkFeatures','MineToPCDChannelProperties','Mine To PCD Channel');
  AAdd('NetworkFeatures','MineToUndergroundChannelProperties','Mine To Underground Channel');
  AAdd('NetworkFeatures','NaturalInflowChannel','Natural Flow');

  //LabelCaption
  AAdd('LabelCaption','MultiplePeriodsWarning','NOTE: Multiple period lengths is usually not selected for sequence lengths greater than 10.');
  AAdd('LabelCaption','FirmYieldWarning','NOTE: Firm yield analysis must not be selected for YRC.');
  AAdd('LabelCaption','NumberOfYearsWarning','NOTE: If multiple period lengths is selected then number of years in simulation should be less than or equal to 10.');
  AAdd('LabelCaption','SummaryOutputWarning','NOTE: Level of summary output must be set to brief summary for a YRC run.');

  AAdd('LabelCaption','MultipleStochasticYieldResult','Stochastic Yield Result For Multiple Periods');
  AAdd('LabelCaption','StochasticYieldResult','Stochastic Yield Result');
  AAdd('LabelCaption','NrOfSequencesAnalysed','Number of Sequences analysed = ');

  AAdd('LabelCaption','FullSupplyDescr'            ,'Full supply level (masl)');
  AAdd('LabelCaption','OldStartingStorageDescr'    ,'Current starting storage level (masl(m)');
  AAdd('LabelCaption','PercentageDescr'            ,'Percentage of live full supply volume (masl)');
  AAdd('LabelCaption','NewStartingStorageDescr'    ,'New starting storage level(m)');

  AAdd('LabelCaption','EconomicLife'        ,'Economic life :');
  AAdd('LabelCaption','EconomicLifeUnits'   ,'(years)');
  AAdd('LabelCaption','CapitalCost'         ,'Capital cost :');
  AAdd('LabelCaption','CapitalCostUnits'    ,'(R millions)');
  AAdd('LabelCaption','OandMCost'           ,'Operating and Maintenance cost :');
  AAdd('LabelCaption','FixedOandMCost'      ,'Fixed O and M cost :');
  AAdd('LabelCaption','VariableOandMCost'   ,'Variable O and M cost :');
  AAdd('LabelCaption','OandMCostUnits'      ,'(R millions / year)');
  AAdd('LabelCaption','NrYearsConstruct'    ,'No. of years to construct :');
  AAdd('LabelCaption','Year'                ,'Year :');
  AAdd('LabelCaption','CostSchedule'        ,'Capital Expenditure (%) :');
  AAdd('LabelCaption','YearsInAnalysis'     ,'Years in analysis :');
  AAdd('LabelCaption','BaseNodeNumber'      ,'Base reservoir :');
  AAdd('LabelCaption','Replacements'        ,'Replacements :');

  AAdd('LabelCaption','SwitchDefinition'    ,'Switch definition :');
  AAdd('LabelCaption','AssociatedNode'      ,'Associated node / reservoir :');
  AAdd('LabelCaption','WaterLevel'          ,'Water level where status changes :');
  AAdd('LabelCaption','SwitchType'          ,'Type of switch :');
  AAdd('LabelCaption','InitialStatus'       ,'Initial status of channel :');

  AAdd('LabelCaption','BoxPlotInterval'     ,'Interval');
  AAdd('LabelCaption','SeriesSelecter'      ,'Requred Interval(s)');
  AAdd('LabelCaption','SeriesLoadCase'      ,'Load Case(s)');
  AAdd('LabelCaption','MonthTypeValues'     ,'Month Type Values:');

  AAdd('LabelCaption','NaturalOptionDescr'  ,'Natural - Option: The monthly inflows to a particular reference node is taken to be equal to the monthly runoffs in the corresponding *.INC file.');
  AAdd('LabelCaption','DevelopedOptionDescr1','Developed Option: For this option the monthly reference flows to a particular EWR reference node are calculated based on the developed (net) inflows');
  AAdd('LabelCaption','DevelopedOptionDescr2',', after the impact of diffuse water use (from the *.IRR and *.AFF files in the Hydro-folder) and other relevant network land-use features (such as Irrigation Blocks, Stream Flow Reductions, Mining)');
  AAdd('LabelCaption','Reset','Reset');
  AAdd('LabelCaption','Refresh','Refresh');

  //ErrorMessage strings
  AAdd('ErrorString','InvalidPenaltyStructure'            ,'invalid penalty structure number');
  AAdd('ErrorString','UnassignedDataContainer'            ,'Data container parameter is unassigned.');
  AAdd('ErrorString','UnassignedDataType'                 ,'Data type parameter is unassigned.');
  AAdd('ErrorString','UnassignedMasterControlFeature'     ,'Channel does not have a Master Control Feature assigned.');
  AAdd('ErrorString','UnassignedPenaltyStructure'         ,'Channel does not have a penalty structure assigned.');
  AAdd('ErrorString','UnassignedMinMaxFlowConstraint'     ,'Channel does not have a Min-Max Flow Constraint assigned.');
  AAdd('ErrorString','ChannelSUMOutFileNotImported'       ,'The SUM.OUT file has not yet been imported into the database or no channel has been included in the output summary.');
  AAdd('ErrorString','ReservoirSUMOutFileNotImported'     ,'The SUM.OUT file has not yet been imported into the database or no reservoir has been included in the output summary.');
  AAdd('ErrorString','SelectedChannelNotIncluded'         ,'The selected channel has not been included in the output summary.');
  AAdd('ErrorString','NoDataForSelectedNetworkElement'    ,'There is no data for the selected network element.');
  AAdd('ErrorString','SelectedNetworkElementNotIncluded'  ,'The selected network element has not been included in the output summary.');
  AAdd('ErrorString','ChannelTypeWarning'                 ,'Channel type must be Master Control or Specified Demand or Min-Max');
  AAdd('ErrorString','UnassignedDemandFeature'            ,'Channel does not have a Specified Demand Feature assigned.');
  AAdd('ErrorString','NoChannelDemandFileImported'        ,'There channel demand file has not yet been imported.');
  AAdd('ErrorString','NoDemandFileForSelectedChannel'     ,'There is no demand file for the selected channel.');
  AAdd('ErrorString','PLTOutFileNotImported'              ,'The PLT.OUT file has not yet been imported into the database.');
  AAdd('ErrorString','ChannelNoTypeError'                 ,'Channel number must be an integer');
  AAdd('ErrorString','GrowthProjectionTypeError'          ,'Growth projection must be a float');
  AAdd('ErrorString','ArcNumberTypeError'                 ,'Arc number must be an integer');

  AAdd('EstimatedHistoricDialog','HystoricFirmYield'      ,'Target Range of Hystoric Firm Yield');
  AAdd('EstimatedHistoricDialog','IterativeCalculation'  ,'Results of Iterative Calculation');

  AAdd('TCopyScenarioDataManager','strCopyReservoir'  ,'Copy Reservoir(s) Data into the current Scenario');
  AAdd('TCopyScenarioDataManager','strCopyChannel'  ,'Copy Channel(s) Data into the current Scenario');
  AAdd('TCopyScenarioDataManager','strIrrigationAreas'  ,'Copy Irrigation Area(s) Data into the current Scenario');
  AAdd('TCopyScenarioDataManager','strIrrigationBlock'  ,'Copy Irrigation Block(s) Data into the current Scenario');
  AAdd('TCopyScenarioDataManager','strWetland'  ,'Copy Wetland(s) Data into the current Scenario');
  AAdd('TCopyScenarioDataManager','strPowerPlant'  ,'Copy Power Plant(s) Data into the current Scenario');
  AAdd('TCopyScenarioDataManager','strYMDemandCentre'  ,'Copy Demand Centre(s) Data into the current Scenario');
  AAdd('TCopyScenarioDataManager','strSFR'  ,'Copy Stream Flow Reduction(s) Data into the current Scenario');
  AAdd('TCopyScenarioDataManager','strMine'  ,'Copy Mine(s) Data into the current Scenario');      
  AAdd('TCopyScenarioDataManager','strGroundWater'  ,'Copy Groundwater(s) Data into the current Scenario');      

  AAdd('TCopyScenarioDataDialog','strStudy'  ,'Study:');
  AAdd('TCopyScenarioDataDialog','strSubArea'  ,'SubArea:');
  AAdd('TCopyScenarioDataDialog','strScenario'  ,'Scenario:');
  AAdd('TCopyScenarioDataDialog','strCancel'  ,'Cancel');
  AAdd('TCopyScenarioDataDialog','strStartCopy'  ,'Start Copy');
  AAdd('TCopyScenarioDataDialog','strInclude'  ,'Include Existing');
  AAdd('TCopyScenarioDataDialog','strSelectAll'  ,'Select All');
  AAdd('TCopyScenarioDataDialog','strDeselectAll'  ,'Deselect All');

  AAdd('DataSources','DataSourceSumOutFile'  ,'SUM.OUT file on the harddrive');
  AAdd('DataSources','DataSourcePltOutFile'  ,'PLT.OUT file on the harddrive');
  AAdd('DataSources','DataSourceDatabase'  ,'SUM.OUT file imported into the database');
  AAdd('DataSources','DataSourceBlobFile'  ,'Binary output on the harddrive');
  AAdd('DataSources','DataSourceBlobDatabase'  ,'Binary output saved into the database');

  AAdd('NetworkFeatures','GroundWaterAquiferInflowChannel','Aquifer Inflow Channel');
  AAdd('NetworkFeatures','OutflowToDownStreamAquifer','Outflow To DownStream Aquifer Channel');

  AAdd('NetworkFeatures','InflowFromUpstreamAquifer','Inflow From Upstream Aquifer Channel');


  AAdd('NetworkFeatures','GroundWaterBaseFlow','GroundWater Baseflow Channel');
  AAdd('NetworkFeatures','AbtstractionFromAquifer','Abtstraction From Aquifer Channel');
  AAdd('NetworkFeatures','AbstractionFromGroundWater','Abstraction from Baseflow Channel');
  AAdd('NetworkFeatures','SurfaceRunOffChannel','Surface RunOff Channel');
  AAdd('NetworkFeatures','ExcessInterflow','Excess Interflow Channel');
  AAdd('NetworkFeatures','GroundWaterAbstraction','GroundWater Abstraction Channel');
  AAdd('NetworkFeatures','GroundWaterBaseFlowRemainder','GroundWater BaseFlow Remainder Channel');
  AAdd('NetworkFeatures','OutflowToNetwork','Outflow To System Network Channel');

  AAdd('Message','IncludeAll','The monthly average flow is calculated by summing the daily flows in a month and dividing '+
                              'by the number of days. All missing (null) flows are assumed to be zero. Flagged values '+
                              'are excepted as correct and used as is in the calculation of the monthly average. '+
                              'No months get discarded for this analysis');

  AAdd('Message','ExcludeSuspectDailyData','The monthly average flow is calculated by summing the daily flows in a month and dividing '+
                                           'by the number of days. All missing (null) flows are assumed to be zero. Flagged value '+
                                           'are assumed to be zero.  No months get discarded for this analysis.');

  AAdd('Message','Infillgabs','The monthly average flow is calculated by summing the daily flows in a month and dividing by the number of days. Missing '+
                 'flows are infilled by linear interpolation. Flagged values are excepted as correct and used as is in the calculation of the '+
                 'monthly average. There is the additional functionality,  to add a threshold % for infilling to this option. If there are more '+
                 'than a certain % of days missing from a month, it is discarded from the analysis. The default value is 25% or 7 days, however the '+
                 'user is be able to define his own preference. e.g. 0% means any missing days in a month will result in the month being '+
                 'recorded as missing and discarded from further use, or 100% represents all missing data being infilled and therefore no months will be discarded.');
  AAdd('LabelHeading','Total','Total:');
  AAdd('TCatchmentProportionsDlg','UrbanRunoffA','Urban Runoff (mcm/a)');
  AAdd('TCatchmentProportionsDlg','UrbanRunoffM','Urban Runoff (mcm/m)');

  AAdd('TCatchmentProportionsDlg','DrainageFactor','Drainage area scaling factor.');
  AAdd('TCatchmentProportionsDlg','AfforestationFactor','Afforestation abstraction scaling factor.');
  AAdd('TCatchmentProportionsDlg','IrrigationFactor','Irrigation abstraction scaling factor.');
  AAdd('TCatchmentProportionsDlg','PointRainfallFactor','Point rainfall scaling factor.');
  AAdd('TCatchmentProportionsDlg','UrbanRunoffPortion','Portion of urban runoff entering node (*.URB).');

  AAdd('TCatchmentProportionsDlg','NettValuesFile','Chart drawn based on nett values of all files (*.INC, *.AFF, *.IRR).');
  AAdd('TCatchmentProportionsDlg','IndividualValuesFiles','Chart drawn based on values of individual file.');

  AAdd('TCatchmentProportionsDlg','AnnualValues','Chart drawn based on annually values of files (*.INC, *.AFF, *.IRR, *.URB, *.RAN).');
  AAdd('TCatchmentProportionsDlg','MonthlyValues','Chart drawn based on monthly values of files (*.INC, *.AFF, *.IRR, *.URB, *.RAN).');

  AAdd('LabelCaption','Years','Years :');
  AAdd('LabelCaption','AnnualEscalation','Annual escalation factors :');
  AAdd('RunParameters','PlanningPMPChannelOutput','Click on this icon for channel to be printed in PMP.out file');
  AAdd('RunParameters','PlanningNrOfChanInPMP','Total nr of channels in PMP.out file');

  AAdd('TSpecifiedInflowDataValidator','InflowFileSavedInDB','This Inflow file has been imported into the database.');
  AAdd('TSpecifiedInflowDataValidator','InflowFileNotSavedInDB','This Inflow file has not yet been imported into the database.');

  AAdd('TabCaption','Planning','Planning Model');
  AAdd('LabelCaption','WrymDatFile','Wrym.dat File:');
  AAdd('LabelCaption','WrymFileData','Wrym File Data');
  AAdd('LabelCaption','WrypDatFile','Wrpm.dat File:');
  AAdd('LabelCaption','WrypFileData','Wrpm File Data');

  AAdd('LabelCaption','ImportedBy','Imported By');
  AAdd('LabelCaption','ErrorType','Error Type');
  AAdd('LabelCaption','ErrorDescription','Error Description');
  AAdd('LabelCaption','StudyErrors','Study Errors');
  AAdd('LabelCaption','CorrectiveAction','Corrective Action Taken');
  AAdd('TabCaption','StudyMetadata','Study Metadata');
  AAdd('TStudyMetaDataValidator','DataError','Data Validation Errors');
  AAdd('TStudyMetaDataValidator','DrawingError','Drawing Errors');
  AAdd('TStudyMetaDataValidator','WRMFError','WRMF Errors');
  AAdd('Message','MaxCharactersReached','Maximum number of characters reached.Characters after length of 255 will be cut.');
  AAdd('LabelCaption','StudyName','Study Area Name');

  AAdd('TWRPMOutputFilesCollator','PltOut','Plt.out');
  AAdd('TWRPMOutputFilesCollator','SumOut','Sum.out');
  AAdd('TWRPMOutputFilesCollator','SysOut','Sys.out');
  AAdd('TWRPMOutputFilesCollator','ResOut','Res.out');
  AAdd('TWRPMOutputFilesCollator','PmpOut','Pmp.out');
  AAdd('TWRPMOutputFilesCollator','TimePeriodsNotTheSame','Number of monthly time periods are not the same in the files');

  AAdd('TOutpuCollateFilesValidator','CollateFiles','Collate Files');
  AAdd('TOutpuCollateFilesValidator','FileTypes','File Types');
  AAdd('TOutpuCollateFilesValidator','FolderWithoutPltFile','A folder without plt file cannot be choosen.');

  AAdd('TOutpuCollateFilesValidator','SelectOutputDirectory','Select Output Files Directory');
  AAdd('TWRPMOutputFilesCollator','SequenceCountMoreThan100','There are %d Sequences to be collated. The maximum should be 1000 sequences. ');
  AAdd('TWRPMOutputFilesCollator','SequenceNumberNotInOrder','Sequence numbers are not in ascending order.The first sequence in this dataset is %d and the last sequence number in the last dataset is %d');
  AAdd('TWRPMOutputFilesCollator','SequenceNumberHasGaps','Sequence numbers has gaps or overlap.The first sequence number in this dataset is %d and the last sequence number in the last dataset was %d');
  AAdd('TWRPMOutputFilesCollator','StartEndSequencenumber','Start sequence number is %d and Last sequence number is %d');
  AAdd('TWRPMOutputFilesCollator','SubSystemChannelReservoirNotTheSame','Subsystems or Output Channels or Output Reservoirs are not the same in all files');
  AAdd('TOutpuCollateFilesValidator','OverwriteExistingFiles','Files to be created already exists, Do you want to overwrite them?');
  AAdd('TOutpuCollateFilesValidator','CollatingCompleted','Collating files completed successfully');
  AAdd('TOutpuCollateFilesValidator','CollatingFile','Collating %s Files');

  AAdd('ChkBoxCaption','ShowScreenAgain','Do not show this screen again');
  AAdd('LabelCaption','MetadataInformation','Metadata has been provided for some of the available systems. Please read this ' +
                                            'information on issues encountered by the User Support team when importing the original ' +
                                            'data into WRMF. This information may well assist you identify existing issues with the ' +
                                            'data early on before proceeding with further work.');
  AAdd('LabelCaption','CautionaryNote','Cautionary note on WRMF study data');
  AAdd('LabelCaption','UserResponsibility','It is the responsibility of the user to ensure that the data obtained from User ' +
                                           'Support is correct for their specific needs.  The data is provided in good faith ' +
                                           'and the data providers can not be held liable for any inaccuracies present.');

  AAdd('TSpecifiedInflowDataValidator','SelectInflowFile','Select inflow file.');
  AAdd('TSpecifiedInflowDataValidator','SelectInfFileToBeCopied','The selected inf file will be copied to the scenario folder before being imported. Continue?');
  AAdd('TSpecifiedInflowDataValidator','FileAlreadyExist','File already exist in the destination folder. Do you want to override it?');
  AAdd('TImportDiversionDataDialog','Cancel','Cancel');
  AAdd('TImportDiversionDataDialog','Import','Import');
  AAdd('TImportDiversionDataDialog','DiversionStation','Diversion Station');
  AAdd('TImportDiversionDataDialog','DiversionRelationship','Diversion Relationship');
  AAdd('TImportDiversionDataDialog','ImportDataFromOtherScenario','Import Diversion Data From Other Scenario ');
  AAdd('TImportDiversionDataDialog','Study','Study');
  AAdd('TImportDiversionDataDialog','SubArea','Sub-Area');
  AAdd('TImportDiversionDataDialog','Scenario','Scenario');

  AAdd('TOutputDeficitDurationValidator','DeficitDurations','Deficit Durations');
  AAdd('TOutputDeficitDurationValidator','DeficitDurationGraph','Deficit duration graph (');
  AAdd('TOutputDeficitDurationValidator','DeficitDuration','Deficit duration (months)');
  AAdd('TOutputDeficitDurationValidator','DeficitDurationBoxPlot','Deficit duration (Sequences)');
  AAdd('TOutputDeficitDurationValidator','MonthlyDeficitEvents','Number of monthly deficit events (non-inclusive)');

  AAdd('TOutputMonthlyDeficitValidator','MonthlyDeficitGraph','Monthly deficit graph (');
  AAdd('TOutputMonthlyDeficitValidator','DeficitDurations','Monthly Deficits');
  AAdd('TOutputMonthlyDeficitValidator','MonthlyDeficitEvents','Number of monthly deficit events');
  AAdd('TOutputMonthlyDeficitValidator','Months','Months');
  AAdd('DistributionCurve','Months','Months');
  AAdd('ErrorString','IrrigationOutputFileNotFound','Irrigation block demand output file not found.');
  AAdd('MasterControl','M3perMonth' ,'Million m³/Month');
  AAdd('OutputGraphType','Caption','Graph Type');
  AAdd('OutputGraphType','Bar','Bar');
  AAdd('OutputGraphType','BoxPlot','BoxPlot');

  AAdd('TTimeSeriesComparitorChartPanel','FSLMarkText','Full Supply Level (FSL)');
  AAdd('TTimeSeriesComparitorChartPanel','DSLMarkText','Dead Storage Level (DSL)');
  AAdd('TTimeSeriesComparitorChartPanel','BOTMarkText','Bottom Level of Reservoir (BOT)');
  AAdd('TTimeSeriesComparitorChartPanel','OperatingLevel','Operating Level %d');

end;

procedure LoadMoreLanguageText1(AAdd: TTextItemAddFunction);
begin
    AAdd(' ','PointsCountDescr','Number of points in area/elevation relationship (max = 15,cannot');
  AAdd('AnlySequences','Seq1','Sequence1');
  AAdd('AnlySequences','Seq10','Sequence10');
  AAdd('AnlySequences','Seq2','Sequence2');
  AAdd('AnlySequences','Seq3','Sequence3');
  AAdd('AnlySequences','Seq4','Sequence4');
  AAdd('AnlySequences','Seq5','Sequence4');
  AAdd('AnlySequences','Seq6','Sequence6');
  AAdd('AnlySequences','Seq7','Sequence7');
  AAdd('AnlySequences','Seq8','Sequence8');
  AAdd('AnlySequences','Seq9','Sequence9');
  AAdd('Channel','ChannelPenaltyCount','Total number of channel penalty types');
  AAdd('Channel','MasterControlChannelsCount','Number of master control channels');
  AAdd('Channel','PowerChannelCount','Number of power channels');
  AAdd('Channel','Source','Source');
  AAdd('Channel','Sink','Sink');
  AAdd('Channel','ArcValues','Arc Values');
  AAdd('Channel','NArcPenaltyStructures',' Arc Penalty Structures');
  AAdd('Channel','PenaltyStructures','Penalty Structures');
  AAdd('Channel','ChannelsUsingPenaltyStructureM','Channels using penalty structure ');
  AAdd('Channel','InAdditionToMasterControl','(In addition to master control)');
  AAdd('Channel','Arc','Arc');
  AAdd('Channel','Totals','Totals');
  AAdd('Channel','Minimum','(minimum)');
  AAdd('Channel','Maximum','(maximum)');
  AAdd('Channel','DistributionFactors','Monthly Water Supply Distribution Factors' );
  AAdd('Channel','FlowConstraints','Flow Constraints (m³/s)');
  AAdd('Channel','DistributionWarning','WARNING: Growth Factors have been applied to these demands');
  AAdd('Channel','Growth','Growth');

  AAdd('Channel','ReservoirLevelRange','<-- Reservoir level range -->');
  AAdd('Channel','MillionM3PerYear','(million m³/annum)');
  AAdd('Channel','Channel','Channel');
  AAdd('ChannelTypes','strChannelDemand','Demand channel');
  AAdd('ChannelTypes','strChannelDiversion','Diversion channel');
  AAdd('ChannelTypes','strChannelGeneral','General channel');
  AAdd('ChannelTypes','strChannelInflow','Inflow channel');
  AAdd('ChannelTypes','strChannelIrrigation','Irrigation channel');
  AAdd('ChannelTypes','strChannelLoss','Loss channel');
  AAdd('ChannelTypes','strChannelMaster','Master channel');
  AAdd('ChannelTypes','strChannelMinFlow','Minimum Flow channel');
  AAdd('ChannelTypes','strChannelMultiPurpose','Multiple Purpose channel');
  AAdd('ChannelTypes','strChannelPenalty','Penalty channel');
  AAdd('ChannelTypes','strChannelPower','Power channel');
  AAdd('ChannelTypes','strChannelPumping','Pumping channel');
  AAdd('ChannelTypes','strChannelSummary','Include Summary channel');
  AAdd('ControlStructure','ControlStructureCount','Number of control structures.');
  AAdd('DaysPerMonth','Days1','Number of days in Month1');
  AAdd('DaysPerMonth','Days10','Number of days in Month10');
  AAdd('DaysPerMonth','Days11','Number of days in Month11');
  AAdd('DaysPerMonth','Days12','Number of days in Month12');
  AAdd('DaysPerMonth','Days2','Number of days in Month2');
  AAdd('DaysPerMonth','Days3','Number of days in Month3');
  AAdd('DaysPerMonth','Days4','Number of days in Month4');
  AAdd('DaysPerMonth','Days5','Number of days in Month5');
  AAdd('DaysPerMonth','Days6','Number of days in Month6');
  AAdd('DaysPerMonth','Days7','Number of days in Month7');
  AAdd('DaysPerMonth','Days8','Number of days in Month8');
  AAdd('DaysPerMonth','Days9','Number of days in Month9');

  AAdd('TDisbenefitDefinitionDataDialog','NrOfEconomicVariables','Nr Of Economic Variables');
  AAdd('TDisbenefitDefinitionDataDialog','EquationFunctionX','Equation Function X');
  AAdd('TDisbenefitDefinitionDataDialog','EquationFunctionY','Equation Function Y');
  AAdd('TDisbenefitDefinitionDataDialog','EquationFunctionNonSupply','Equation Non-Supply');
  AAdd('TDisbenefitDefinitionDataDialog','EquationCostY','Equation Cost Y');
  AAdd('TDisbenefitDefinitionDataDialog','EscalationRate','Escalation Rate');
  AAdd('TDisbenefitDefinitionDataDialog','DefinitionDataDialog','Time Control');

  AAdd('TDisbenefitDefinitionDataDialog','YearActiveLabel','Year Active:');
  AAdd('TDisbenefitDefinitionDataDialog','MonthActiveLabel','Month Active:');
  AAdd('TDisbenefitDefinitionDataDialog','YearObsoleteLabel','Year Obsolute:');
  AAdd('TDisbenefitDefinitionDataDialog','MonthObsoleteLabel','Month Obsolete:');

  AAdd('TReturnFlowChanneldialog','ReturnFlowChannelDataDialog','Return Flow Channel Data');

  AAdd('TReturnFlowChanneldialog','ReturnFlowChannelDataDialog','Return Flow Channel Data');
  AAdd('TReturnFlowChanneldialog','AssumedFactor','Assumed Factor');
  AAdd('TReturnFlowChanneldialog','Abstraction','Abstraction');

  AAdd('TReturnFlowChanneldialog','NumOfCorrespondingChannels','No. of Corresponding Channel:');
  AAdd('TReturnFlowChanneldialog','CorrespondingChannel','Corresponding Channels');
  AAdd('TReturnFlowChanneldialog','MonthlyPotentialEvap','Potential monthly Evaporation');
  AAdd('TReturnFlowChanneldialog','ChannelProperties','Return Flow');

  AAdd('TReturnFlowChanneldialog','MonthlyAvrgFactor','Monthly Average:');
  AAdd('TReturnFlowChanneldialog','CalibrationFactor','Calibration Factor:');
  AAdd('TReturnFlowChanneldialog','MonthlyAvrgNetEvap','Net Monthly Average:');
  AAdd('TReturnFlowChanneldialog','RoutingConstant','Routing Constant:');
  AAdd('TReturnFlowChanneldialog','CurtailmentFactor','Curtailment Factor:');
  AAdd('TReturnFlowChanneldialog','MultiplicationFactor','Multiplication Factor:');

  AAdd('Field','CharactersOutOfRange','Field %s :Characters is out of range [%s..%s]  Characters allowed');
  AAdd('Field','EmptyFieldValue','Field %s Cannot be Empty');
  AAdd('Field','FieldOutOfRange','Field %s : Value out of range [%s .. %s]');
  AAdd('Field','FieldPropertyNotImplemented','Field %s does not have any properties implemented yet.');
  AAdd('Field','FieldParameterNotAssigned','Parameter field is not assigned.');
  AAdd('Field','IndexOutOfBounds','Field %s :Index %s out of bounds [%s..%s]');
  AAdd('Field','InvalidDate/TimeValue','Field Value %s is not a valid Date/Time value');
  AAdd('Field','InvalidFloatValue','Field Value %s is not a valid float value');
  AAdd('Field','InvalidIntegerValue','Field Value %s is not a valid Integer value');
  AAdd('Field','ValuesNotAccepted','Field %s: Value out of range [%s..%s]');

  AAdd('FileStatus','strFileImported','File Imported');
  AAdd('FileStatus','strFileInDir','File In Directory');
  AAdd('FileStatus','strFileReadOnly','File Read Only');
  AAdd('FileTypes','FileName','Name of the File');
  AAdd('FileTypes','FileTypeDescr','Description of the File Type');
  AAdd('StatusBarText','LoadNone',' Loading nodes');
  AAdd('StatusBarText','ReadFile','Reading file ...');
  AAdd('StatusBarText','Total','Total = %d');
  AAdd('StatusBarText','Selected','Selected = %d');
  AAdd('StatusBarText','UnSelected','Unselected = %d');

  AAdd('Language','LANGDESCR','Language Description');
  AAdd('LanguageText','STRTEXT','Description of String Constants');
  AAdd('MaxYield','MYield1','Maximum system yield 1');
  AAdd('MaxYield','MYield10','Maximum system yield 10');
  AAdd('MaxYield','MYield2','Maximum system yield 2');
  AAdd('MaxYield','MYield3','Maximum system yield 3');
  AAdd('MaxYield','MYield4','Maximum system yield 4');
  AAdd('MaxYield','MYield5','Maximum system yield 5');
  AAdd('MaxYield','MYield6','Maximum system yield 6');
  AAdd('MaxYield','MYield7','Maximum system yield 7');
  AAdd('MaxYield','MYield8','Maximum system yield 8');
  AAdd('MaxYield','MYield9','Maximum system yield 9');
  AAdd('Message','EmptyField','Field name to be validated is empty.Please pass a valid field name');
  AAdd('Message','nvDeleteDrawingsFirst','Delete drawings first');
  AAdd('Message','nvDrawingAlreadyExists','Drawing already exists');
  AAdd('Message','nvDrawingDoesNotExist','Drawing does not exist');
  AAdd('Message','NVEndNodeDoesNotExist','End node does not exist');
  AAdd('Message','NvEnterCaption','Enter Caption');
  AAdd('Message','nvEnterTitleOfNewDrawing','Enter title of new drawing');
  AAdd('Message','nvEnterTitleOfNewGroup','Enter title of new group');
  AAdd('Message','nvGroupAlreadyExists','Group already exists');
  AAdd('Message','nvNewDrawingGroupAlreadyExists','New Drawing Group Already Exists');
  AAdd('Message','nvNewGroupAlreadyExists','New group already exists');
  AAdd('Message','nvOldDrawingGroupDoesNotExist','Old Drawing Group Does Not Exist');
  AAdd('Message','nvOldGroupDoesNotExist','Group does not exist');
  AAdd('Message','nvPromptToSave','Do you wish save first ?');
  AAdd('Message','NVSelectAChannel','Select a channel');
  AAdd('Message','nvSelectADrawingFirst','Select a drawing first');
  AAdd('Message','NVSelectANode','Select a node');
  AAdd('Message','NVSelectAReservoir','Select a reservoir');
  AAdd('Message','NVSelectChannelEnd','Select Channel End');
  AAdd('Message','NVSelectChannelStart','Select Channel Start');
  AAdd('Message','nvSelectGroupFirst','Select a group first');
  AAdd('Message','nvSelectSourceDrawingFirst','Select source drawing first');
  AAdd('Message','NVStartNodeDoesNotExist','Start node does not exist');
  AAdd('Message','NVShowDisableMessage','GIS Viewer disabled in this release');
  AAdd('Message','NotInModelData','Not in model data');
  AAdd('Message','OtherReservoirsAffected','Other reservoir(s) affected:');
  AAdd('Message','NBFormula','NB: Yield = TD * ( 1 - Deficit Proportion )');
  AAdd('Message','NoGauges','Warning : No gauges were loaded.');
  AAdd('Message','ExtractionComplete','The data extraction is complete, now opening the folder with extracted files.');
  AAdd('Message','CatchmentZone','Catchment Zone Output file not found!!!');
  AAdd('Message','GaugesNotFound','No Gauges have been loaded');
  AAdd('Message','InvalidInput','Invalid input value.');
  AAdd('Message','SelectInTreeview','Please select an item in the treeview first');
  AAdd('Message','DisplayGraphOrGrid','Please display either the graph or the grid (not both) before copying to clipboard.');
  AAdd('Message','SelectRawStation','Select a "Raw" Station first');
  AAdd('Message','SelectPatch','Select a "Patch" first');
  AAdd('Message','CanNotAddPatch','You cannot add the patch gauge to itself.');
  AAdd('Message','AddGaugeTo','Select a patch to add Gauge to');
  AAdd('Message','SelectSourceStation','Select a source station first');
  AAdd('Message','SelectRawOrPatch','Select a source station first (Raw or Patch).');
  AAdd('Message','100Gauges','You have selected more than 100 gauges. ');
  AAdd('Message','TakeLongTime','This may take a very long time. ');
  AAdd('Message','WantToContinue','Do you want to continue?');
  AAdd('Message','LatitudeDegrees','Invalid latitude degrees.');
  AAdd('Message','LatitudeMinutes','Invalid latitude minutes.');
  AAdd('Message','LongitudeDegrees','Invalid longitude degrees.');
  AAdd('Message','LongitudeMinutes','Invalid longitude minutes.');
  AAdd('Message','RainGaugeNumber','Invalid rain gauge number.');
  AAdd('Message','KMDistance','Invalid km distance.');
  AAdd('Message','InvalidSearchCriteria',' is an invalid search criteria : Wildcard only allowed at begining and/or end of search string ' );
  AAdd('Message','EnterCharacters',' is an invalid search criteria : Enter at least 3 characters to search on');
  AAdd('Message','InvalidCriteria',' is an invalid search criteria');
  AAdd('Message','ActiveChangelists','The selected changelist will be applied to the current scenario. ' +
                                     'Once the list is applied, it will be deleted. Do you want to continue?');
{  AAdd('Message','ActiveChangelists','All active changelists up to and including the selected changelist ' +
                     'will be applied to the current scenario. Once they are applied, '+
                     'they will be deleted. Do you want to continue?');}
  AAdd('Message','DataNotImpl','Meta Data not implemented!');
  AAdd('Message','CustomEditor','Test Field custom editor');
  AAdd('Message','ScenarioNotExist',') does not exist in the selected scenario.');
  AAdd('Message','ChannelNotExist','Channel (');
  AAdd('Message','NodeNotExist','Node (');
  AAdd('Message','ReservoirNotExist','Reservoir (');
  AAdd('Message','IrrigationBlockNotExist','Irrigation Block (');
  AAdd('Message','IrrigationAreaNotExist','Irrigation Area (');
  AAdd('Message','WetlandNotExist','Wetland (');
  AAdd('Message','DemandCentreNotExist','Demand Centre (');
  AAdd('Message','SFRACentreNotExist','SFRA Sub Catchment (');
  AAdd('Message','ReservoirNode','Reservoir or node (');
  AAdd('Message','ImportedFiles','Remove Imported Files and Directories');
  AAdd('Message','CopyrightFile','Cannot Read Copyright file: ');
  AAdd('Message','SettingSuccessful','Copyright setting successful!');
  AAdd('Message','WrongLicenceCode','Wrong licence code!');
  AAdd('Message','NumberOfExcelCopiedRows','Number of Excel copied rows are not the same as the grid rows. Resize grid rows?');
  AAdd('Message','NumberOfExcelCopiedColumns','Number of Excel copied columns are not the same as the grid columns. Resize grid columns?');
  AAdd('Message','DeficitNotAllowed','Deficit is not allowed because there is no demand for selected channels');
  AAdd('Message','PercentageNotAllowed','Percentage is not allowed because there is no demand for selected channels');
  AAdd('Message','NoDeficitForSelectedChannel','There is no deficit for selected channel!');

  AAdd('Message','TheReservoirNumber','Reservoir number (');
  AAdd('Message','DoesNotExist',') does not exists.');
  AAdd('Message','TheNodeNumber','Node number (');
  AAdd('Message','TheZeroNode','Zero node cannot be deleted');
  AAdd('Message','InvalidDataPasted','Invalid data pasted from Excell. You should paste data from 3 Excel columns');
  AAdd('Message','NoMoreThan','No more than(');
  AAdd('Message','PhysicalCharacteristics',') Physical characteristics  can be added to the database');
  AAdd('Message','DataNotFloatNumbers','Invalid data pasted from Excell. Some of the data pasted are not float numbers');

  AAdd('Message','HydrologyFile','Hydrology file for Catchment Reference ');
  AAdd('Message','NotImportedYet',' has not yet been imported.');
  AAdd('Message','NoSameStartOnHydrologyFiles','Hydrology files do not have the same start year.');
  AAdd('Message','ContainsNoData',' does not exist or contains no data.');
  AAdd('Message','YearsInAnalysisIsLess','Years in analysis is Less that the number of years in ');
  AAdd('Message','YearsInAnalysisIsGeater','Years in analysis is greater that the number of years in ');
  AAdd('Message','HydrologySequence',' Hydrology sequence files which is ');
  AAdd('Message','InflowSequence',' Hydrology sequence files which is ');

  AAdd('Message','CropWaterMaxPercArea','Total percentage area cannot be greater than 100%');
  AAdd('Message','IrrBlockCropTypesLimit','The number of crops types are limited to %s.');
  AAdd('Message','YMDemandReturnFlowChannelsLimit','The number of return flow channels are limited to %s.');
  AAdd('Message','OutputDistributionCurveValidatorMsg1','The compliance graph is only implemented for Timestep = Annual' + #10#13 +
                    'Select output data and set Timestep to Annual');
  AAdd('Message','OutputDistributionCurveValidatorMsg2','The channel does not have a Water Demand Feature. ' + #10#13 +
                          'Create a Water Demand Feature for the channel and then ' + #10#13 +
                          'specify Output Water Use Proportions for the channel.');
  AAdd('Message','OutputDistributionCurveValidatorMsg3','The channel does not have a Water Demand Feature. ' + #10#13 +
                          'Create a Water Demand Feature for the channel and then ' + #10#13 +
                          'specify Output Water Use Proportions for the channel.');
  AAdd('Message','OutputDistributionCurveValidatorMsg4','The compliance graph is only implemented for Units = Percentage ' + #10#13 +
                            'for Specified Demand Channels.' + #10#13 +
                            'Select output data and set Units to Percentage.');
  AAdd('Message','GrowthFactorsValidatorMSG1','There is no data to generate growth factors from.');
  AAdd('Message','GrowthFactorsValidatorMSG2','There are no Demand Channel Growth Factors. Do you want to continue without them');
  AAdd('Message','GrowthFactorsValidatorMSG3','There are no MinMax Channel Growth Factors. Do you want to continue without them');
  AAdd('Message','GrowthFactorsValidatorMSG4','There are no Hydrology Growth Factors. Do you want to continue without them');
  AAdd('Message','GrowthFactorsValidatorMSG5','There is already Growth Projections. Do you want to continue and overwrite them');

  AAdd('Message','GrowthProjectionsValidatorMSG1','There is no data to generate growth factors from.');
  AAdd('Message','GrowthProjectionsValidatorMSG2','There are no Demand Channel Growth Projection. Do you want to continue without them?');
  AAdd('Message','GrowthProjectionsValidatorMSG3','There are no MinMax Channel Growth Projection. Do you want to continue without them?');
  AAdd('Message','GrowthProjectionsValidatorMSG4','There are no Hydrology Growth Projection. Do you want to continue without them?');
  AAdd('Message','GrowthProjectionsValidatorMSG5','The Start Date for one or more of the projections is not on the Available range '+
                   'to  Generate Growth Factors. You won''t be able to Generate them.'+
                   ' Do you want to continue without them?');
  AAdd('Message','GrowthProjectionsValidatorMSG6','There isn''t enough Demand Channel Growth Projection  . Do you want to continue with '+
                   ' the available ones?');
  AAdd('Message','GrowthProjectionsValidatorMSG7','There isn''t enough Min Max Channel Growth Projection  . Do you want to continue with '+
                   ' the available ones?');
  AAdd('Message','GrowthProjectionsValidatorMSG8','There isn''t enough Hydrology Channel Growth Projection  . Do you want to continue with '+
                   ' the available ones?');
  AAdd('Message','GrowthProjectionsValidatorMSG9','The Base Year and the Start Year are the same. Do you want to continue?');

  AAdd('Message','NetworkElementDataCreateReservoirsZone','The new zone will be added to all reservoirs');
  AAdd('Message','NetworkElementDataDeleteReservoirsZone','The selected zone will be removed from all reservoirs');
  AAdd('Message','ReservoirPenaltyStructureListDeletePenaltyZone','This is the last non-fixed zone left. Do you want to delete the whole zones structure.');
  AAdd('Message','AddBelowSelectedZone','Should the new zone be added below the selected zone?');
  AAdd('Message','CanOnlyAddAboveSelectedZone','Sorry, the rule is, it can only add above the selected zone on this one');

  AAdd('Message','DiversionFeatureMustExistInYieldModel',
  'A type 2 or type 4 diversion feature must exist in te Yield Model IMS for this information to be supported by the Yield Model,. Create one of these features in the Yield Model IMS then try again');
  AAdd('Message','ExistingChannelData','Existing Channel data would be over-written.Are you sure you want to continue?');

  AAdd('Message','SumOutDataExist','Sum.out data already exists. Would you like to combine two SUM.OUT files.');
  AAdd('Message','OSVersionMessage','This application only run on Win98,WinME,WinNT,Win2000,WinXP or later versions only');
  AAdd('Message','AFFfilesNotUsed','AFF files are not used when Stream Flow Reductions are implemented');
  AAdd('Message','ExecRunModelPrompt','The output files are going to be imported into the database, this may take some time. Do you want to import them now?');
  AAdd('Message','NoExcelDataMsg','There is no Excel data in the clipboard.');
  AAdd('Message','EstablishCOMConnection','Establishing connection to COM server in order to access model data...Press OK to continue.');
  AAdd('Message','AddChannelToDraw','Add Channels to the drawing first');
  AAdd('Message','AddReservoirToDraw','Add reservoirs to the drawing first');
  AAdd('Message','IFRType','Please specify if IFR relationship is based on:');
  AAdd('Message','WRMFGLFFiles','Do you want to use other WRMF GLF Files?');
  AAdd('Message','InflowFiles','This system contains Specified Inflow Feature(s). '+#10#13+
                               'Do you want to go through the .INF files, correct run type '+#10#13+
                               'file(s) prefix while copying file contents for the Model to run?');  AAdd('Model','Hydrology','Hydrology');

  AAdd('Message','StructType1Msg','StructureType 1 is only used for legacy data and'+#10#13+
                                'cannot be used for a new flow constraint.');
  AAdd('Model','Hydrology','Hydrology');
  AAdd('Model','Yield','Yield');
  AAdd('Model','YRC','YRC');
  AAdd('Model','Planning','Planning');
  AAdd('Model','Stomsa','Stomsa');
  AAdd('Model','DailyDiversion','DailyDiversion');
  AAdd('Model','IFR','IFR');

  AAdd('MonthNames','Month1','Month Name 1');
  AAdd('MonthNames','Month10','Month Name 10');
  AAdd('MonthNames','Month11','Month Name 11');
  AAdd('MonthNames','Month12','Month Name 12');
  AAdd('MonthNames','Month2','Month Name 2');
  AAdd('MonthNames','Month3','Month Name 3');
  AAdd('MonthNames','Month4','Month Name 4');
  AAdd('MonthNames','Month5','Month Name 5');
  AAdd('MonthNames','Month6','Month Name 6');
  AAdd('MonthNames','Month7','Month Name 7');
  AAdd('MonthNames','Month8','Month Name 8');
  AAdd('MonthNames','Month9','Month Name 9');

  AAdd('Reservoir','HydroUnitsCode','Code to indicate units of hydrological data');
  AAdd('Reservoir','ReservoirCount','Numbers of reservoirs and nodal points with inflow');
  AAdd('Reservoir','ReservoirName','Name of reservoir/nodal point');
  AAdd('ReservoirSeries','EvaporationSeries','Evaporation Series');
  AAdd('ReservoirSeries','StartingVolume','Starting Volume');
  AAdd('ReservoirSeries','Volume','Volume');
  AAdd('ReservoirSeries','SurfaceArea','Surface Area');
  AAdd('ReservoirSeries','Depth','Depth (m)');
  AAdd('ReservoirSeries','AreaWhenFull','Area When Full');
  AAdd('ReservoirSeries',' BOT_DSL_FSL',' BOT, DSL, FSL');

  AAdd('MasterControl','Primary','Primary');
  AAdd('MasterControl','Secondary','Secondary');
  AAdd('MasterControl','MonthlyTargetDraft','Monthly target draft');
  AAdd('MasterControl','MonthlyEnergyDemand','Monthly energy demand (MW-continuous)');
  AAdd('MasterControl','LoadCases','Load cases');
  AAdd('MasterControl','TargetSystemYield','Target system yield (million m³/a)');
  AAdd('MasterControl','ActiveLoadCases','Number of active load cases should be 2 when Calculate Firm Yield is on');
  AAdd('MasterControl','HistoricTargetYield','Target system yield disabled for Calculate Historic firm yield');
  AAdd('MasterControl','StochasticTargetYield','Target system yield disabled for Calculate Stochastic firm yield');
  AAdd('MasterControl','MaximumSystemYield','Maximum system yield (million m³/a)');
  AAdd('MasterControl','ValidMaximumSystemYield','The maximum system yield is not allowed to be less than the target system yield');
  AAdd('MasterControl','TargetPowerDemand','Target power demand (MW-continuous)');
  AAdd('MasterControl','Hydro','Hydropower');
  AAdd('MasterControl','Water','Water');
  AAdd('MasterControl','Power','Power');
  AAdd('MasterControl', 'IncludeInAnalysis', 'Include In Analysis');
  AAdd('MasterControl','MillionM3perAnnum','million m³/m');
  AAdd('MasterControl','MultiplePeriodWarning','NOTE: Multiple period lengths is usually not selected for sequence lengths greater than 10.');
  AAdd('MasterControl','ReduceSeqWarning','NOTE: Reduce number of sequences is usually selected for a stochastic run.');
  AAdd('MasterControl','StartTypeWarning','NOTE: Method of stochastic generation is usually set to start randomly.');
  AAdd('MasterControl','SummaryOutputWarning','NOTE: Level of summary output is usually set to brief summary for a stochastic run.');
  AAdd('MasterControl','StochasticDemandWarning','NOTE: All specified demand indicators are usually set to Stochastic (S).');

  AAdd('MasterControl','M3perSecond','m³/s');
  AAdd('MasterControl','M3perAnnum' ,'Million m³/Annum');
  AAdd('MasterControl','Cumecs','Cumecs');
  AAdd('MasterControl','MegaL','Mega Liters/Day');
  AAdd('MasterControl','FirmYieldWarning','NOTE: Firm yield analysis is usually selected for a historic run.');
  AAdd('MasterControl','NumberOfYearsWarning','WARNING: Number of years in analysis is usually the same as the number of years in the hydrological sequence.');
  AAdd('MasterControl','StartMonthNoWarning','WARNING: Start month is usually set = 1 (October)');
  AAdd('MasterControl','SummaryOutputWarning','NOTE: Level of summary output is usually set to full summary for a historic run.');
  AAdd('MasterControl','SpecifiedDemandWarning','NOTE: All specified demand indicators are usually set to Historic (H).');
  AAdd('MasterControl','SetHistoric','Set all specified demand indicators to Historic');
  AAdd('MasterControl','SetStochastic','Set all specified demand indicators to Stochastic');

  AAdd('MasterControl','DemandCentreType','Demand Centre Type.');
  AAdd('MasterControl','DemandCentre','Demand Centre.');
  AAdd('MasterControl','AnnualDemand','Base year demand (million m³/a)');
  AAdd('MasterControl','MinimumDemand','Minimum Demand');
  AAdd('MasterControl','ChannelTypeD','Demand Channel (D).');
  AAdd('MasterControl','ChannelTypeR','Return Flow Channel (R).');

  AAdd('AccessControl','SetPassword','Invalid password, please enter valid password.');
  AAdd('AccessControl','RecordsNotFound','Records could not be found for the specified UserId, please enter valid UserId.');
  AAdd('AccessControl','ConfirmUserDeletion',') will be deleted from the list of available Users, do you want to delete the user?');
  AAdd('AccessControl','User','User (');
  AAdd('AccessControl','NoModelAssignedToUser','The User does not have a model assigned, please register the required model');
  AAdd('AccessControl','StudyCannotBeSelected','Auto select study cannot be selected when auto logon is not selected.');
  AAdd('AccessControl','UserIDExist','The UserId you have entered already exist, Please enter a unique UserId.');
  AAdd('AccessControl','ChangingPasswordFailed','password changing failed, please enter new password and confirm your password with a valid password');
  AAdd('ModelRainfall','StartYear','Start year invalid');
  AAdd('ModelRainfall','EndYear','End year invalid');


end;
procedure LoadMoreLanguageText2(AAdd: TTextItemAddFunction);
begin
  AAdd('TimeSeriesComparitor','ConfirmationOfDeletingSeries','Are you sure you wish to delete this series?');
  AAdd('TimeSeriesComparitor','EmptyChartName','Chart name is empty. Please enter a chart name');
  AAdd('TimeSeriesComparitor','ChartNameAlreadyExist','Chart name already exists. Please enter a unique chart name');
  AAdd('TimeSeriesComparitor','EmptyViewName','View name is empty. Please enter a view name');
  AAdd('TimeSeriesComparitor','ViewNameAlreadyExist','View name already exists. Please enter a unique view name');

  AAdd('OutputComplianceGraph','ReservoirLevelTitle','Level');
  AAdd('OutputComplianceGraph','ReservoirVolumeTitle','Volume');
  AAdd('OutputComplianceGraph','ReservoirTimeTitle','Time');
  AAdd('OutputComplianceGraph','ChannelTimeTitle','Time');

  AAdd('RDDataExtract','ErrorMsg','Following file(s) are missing: %s' + #10#13 + 'and can be found on the WRC Project K5/1155/0/1 cd');

  AAdd('NetworkFeatures','ChannelProperties','Channel Properties');
  AAdd('NetworkFeatures','FeatureName','Feature name');
  AAdd('NetworkFeatures','LossFeature','Loss Feature');
  AAdd('NetworkFeatures','MasterControlFeature','Master Control Feature');
  AAdd('NetworkFeatures','MinimumFlowFeature','Minimum Flow Feature');
  AAdd('NetworkFeatures','MinMaxFlowFeature','Min-max Flow Feature');
  AAdd('NetworkFeatures','PumpingFeature','Pumping Feature');
  AAdd('NetworkFeatures','YMDemandCentreReturnFlowFeature','Return Flow Feature');
  AAdd('NetworkFeatures','SpecifiedDemand','Specified Demand');
  AAdd('NetworkFeatures','DiversionFeature','Diversion Feature');
  AAdd('NetworkFeatures','DiversionType1','(1)Diverted Flow dependent on month and natural inflow');
  AAdd('NetworkFeatures','DiversionType2','(2)Diversion Flow dependent on daily diversion efficiency relationship');
  AAdd('NetworkFeatures','DiversionType3','(3)Diversion Flow dependent on reservoir level and natural flow');
  AAdd('NetworkFeatures','DiversionType4','(4) Diversion flow dependent on inflow to upstream node');
  AAdd('NetworkFeatures','DiversionDemand1','Monthly diversion demand (m³/s)');
  AAdd('NetworkFeatures','DiversionDemand2','Flow ranges (Natural Inflow) (m³/s)');
  AAdd('NetworkFeatures','DiversionDemand4','Flow ranges (Total Inflow) (m³/s)');
  AAdd('NetworkFeatures','NetNaturalInflow1','Proportion of net diverted natural inflow');
  AAdd('NetworkFeatures','NetNaturalInflow2','Actual diverted flow (m³/s)');
  AAdd('NetworkFeatures','NetNaturalInflow4','Actual diverted flow (m³/s)');
  AAdd('NetworkFeatures','ControllingReservoir','Controlling reservoir');
  AAdd('NetworkFeatures','SpecifiedInflow','Specified Inflow Feature');
  AAdd('NetworkFeatures','InflowFileLabel','Inflow file names');
  AAdd('NetworkFeatures','InflowFileName','Full name of the specified inflow file');
  AAdd('NetworkFeatures','HistoricalSelectionOrder','Historical selection order');
  AAdd('NetworkFeatures','StochasticSelectionOrder','Stochastic selection order');
  AAdd('NetworkFeatures','Import','Import');
  AAdd('NetworkFeatures','Imported','Imported');
  AAdd('NetworkFeatures','NotImported','Not imported');
  AAdd('NetworkFeatures','PowerPlant','Power Plant');
  AAdd('NetworkFeatures','PowerChannelProperties','Power Channel Properties');
  AAdd('NetworkFeatures','SpillChannelProperties','Spill Channel Properties');
  AAdd('NetworkFeatures','PowerSpillUpstreamNode','Power and Spill channel upstream reservoir');
  AAdd('NetworkFeatures','PowerDownstreamNode','Power channel downstream node');
  AAdd('NetworkFeatures','SpillDownstreamNode','Spill channel downstream node');
  AAdd('NetworkFeatures','EfficiencyNetHeadFactorCurve','Efficiency/Net Head Factor Curve');
  AAdd('NetworkFeatures','Efficiency','Efficiency');
  AAdd('NetworkFeatures','NetHead','Net Head');
  AAdd('NetworkFeatures','TailwaterFunction','Tailwater Function');
  AAdd('NetworkFeatures','PowerGenerationDemands','Power Generation Demands');
  AAdd('NetworkFeatures','TailwaterType1','(1) Dependent on flow through plant');
  AAdd('NetworkFeatures','TailwaterType2','(2) Part of downstream reservoir');
  AAdd('NetworkFeatures','TailwaterType3','(3) Elevetion');
  AAdd('NetworkFeatures','TailwaterFunction','Tailwater function');
  AAdd('NetworkFeatures','TailwaterDischarge',' Discharge (m³/s)');
  AAdd('NetworkFeatures','DownstreamReservoirElevations','Downstream reservoir elevations');
  AAdd('NetworkFeatures','UpstreamReservoirElevations','Elevations');
  AAdd('NetworkFeatures','DiversionChannelProperties','Diversion Channel Properties');
  AAdd('NetworkFeatures','ConsumptiveChannelProperties','Consumptive Channel Properties');
  AAdd('NetworkFeatures','ReturnFlowChannelProperties','Return Flow Channel Properties');
  AAdd('NetworkFeatures','DemandCentreConsumptiveChannelProperties','Consumptive Water Use Channel Properties');
  AAdd('NetworkFeatures','DemandCentreReclaimationChannelProperties','Reclaimation Plant Loss Channel Properties');
  AAdd('NetworkFeatures','RelaxationPolicy0','(0) Reduce irrigation supply');
  AAdd('NetworkFeatures','RelaxationPolicy1','(1) Reduce return flow');
  AAdd('NetworkFeatures','RelaxationPolicy2','(2) Reduce both irrigation supply and return flow');
  AAdd('NetworkFeatures','MonthlyIrrigationFlows','Monthly flows');
  AAdd('NetworkFeatures','MinimumResevoirLevel','Dead storage level of controlling reservoir in meters.');
  AAdd('NetworkFeatures','MaximumResevoirLevel','Full supply level of controlling reservoir in meters.');
  AAdd('NetworkFeatures','IrrigationArea','Irrigation Area');
  AAdd('NetworkFeatures','DiversionUpstreamNode','Diversion channel upstream node');
  AAdd('NetworkFeatures','ReturnFlowDownstreamNode','Return flow channel downstream node');
  AAdd('NetworkFeatures','InflowChannelProperties','Inflow channel properties');
  AAdd('NetworkFeatures','OutflowChannelProperties','Outflow channel properties');

  AAdd('NetworkFeatures','Diversion','Diversion');
  AAdd('NetworkFeatures','Return','Return');
  AAdd('NetworkFeatures','Consumption','Consumption');
  AAdd('NetworkFeatures','UpStreamReservoir','Upstream Reservoir');
  AAdd('NetworkFeatures','DownStreamReservoir','Downstream Reservoir');
  AAdd('NetworkFeatures','PhysicalFlowConstraint','Physical Flow Constraint');
  AAdd('NetworkFeatures','MaximumSpecifiedChannelFlow','Maximum specified channel flow');
  AAdd('NetworkFeatures','StructureLength','Structure Length (m):');
  AAdd('NetworkFeatures','ReferenceElevation','Reference Elevation (m):');
  AAdd('NetworkFeatures','MaxFlowRate','Maximum Flow Rate (m3/s)');
  AAdd('NetworkFeatures','MaximumGateHeight','Maximum Gate Height (m):');
  AAdd('NetworkFeatures','UseChannelDownstreamNode','Use channel downstream node');
  AAdd('NetworkFeatures','UseChannelUpstreamNode','Use channel upstream node');
  AAdd('NetworkFeatures','PhysicalFlowConstraintName','Flow Constraint Name');
  AAdd('NetworkFeatures','Elevations','Elevations');
  AAdd('NetworkFeatures','Discharges','Discharges');
  AAdd('NetworkFeatures','PipeChannelNumbers','Pipe Channel Numbers');
  AAdd('NetworkFeatures','KFactors','K Factors');
  AAdd('NetworkFeatures','AquiferHeadDifference','Aquifer Head Difference');
  AAdd('NetworkFeatures','AquiferFlow','Aquifer Flow');
  AAdd('NetworkFeatures','DownstreamNodeInflow','Down-stream Node Inflow');
  AAdd('NetworkFeatures','RiverDepth','River Depth');
  AAdd('NetworkFeatures','NumberOfPoints','Number Of Points:');
  AAdd('NetworkFeatures','WaterLevelAtDownstreamNode','Water Level at Downstream Node (m):');

  AAdd('NetworkFeatures','IFRFeature','In-stream Flow Requirement');
  AAdd('NetworkFeatures','IrrigationSuppy','Maintains return flow at its allocated value but reduces irrigation supply');
  AAdd('NetworkFeatures','ReturnFlow','Return flow will be reduced while maintaining the full irrigation demand (if possible)');
  AAdd('NetworkFeatures','IrrigationAndReturnFlow','Both irrigation demand and return flow will be reduced in proportion to the shortfall');
  AAdd('NetworkFeatures','WaterDemandFeature','Water Demand Feature');
  AAdd('NetworkFeatures','Exceedence%','Exceedence %');
  AAdd('NetworkFeatures','Month','Month');
  AAdd('NetworkFeatures','IFRFeatureName','IFR feature name');
  AAdd('NetworkFeatures','ReferenceNodes','Reference nodes');
  AAdd('NetworkFeatures','PhysicalFlowConstraintType01','( 1) Weir formula for swamp areas');
  AAdd('NetworkFeatures','PhysicalFlowConstraintType02','( 2) Standard overflow weir with leakage');
  AAdd('NetworkFeatures','PhysicalFlowConstraintType03','( 3) Standard overflow weir without leakage');
  AAdd('NetworkFeatures','PhysicalFlowConstraintType04','( 4) Elevation/Discharge relationship with reduced flow');
  AAdd('NetworkFeatures','PhysicalFlowConstraintType05','( 5) Elevation/Discharge relationship without reduced flow');
  AAdd('NetworkFeatures','PhysicalFlowConstraintType06','( 6) Maximum flow value representing some physical channel limit');
  AAdd('NetworkFeatures','PhysicalFlowConstraintType07','( 7) Specific swamp area problem');
  AAdd('NetworkFeatures','PhysicalFlowConstraintType08','( 8) Elevation/Discharge with reduced flow - head specified as difference');
  AAdd('NetworkFeatures','PhysicalFlowConstraintType09','( 9) Elevation/Discharge without reduced flow - head specified as difference');
  AAdd('NetworkFeatures','PhysicalFlowConstraintType10','(10) Flow in tunnel section dependent on flow in upstream sections');
  AAdd('NetworkFeatures','PhysicalFlowConstraintType11','(11) Sand aquifer');
  AAdd('NetworkFeatures','PhysicalFlowConstraintType12','(12) Tunnel or Pipeline with submerged outlet');
  AAdd('NetworkFeatures','PhysicalFlowConstraintType13','(13) Pump Station');
  AAdd('NetworkFeatures','PhysicalFlowConstraintType14','(14) Elevation/Discharge through a gravity tunnel between two reservoirs');
  AAdd('NetworkFeatures','ChannelsInWaterDemandCategory','Channels in water use category');
  AAdd('NetworkFeatures','WaterUseCategoryDescr','Category description');
  AAdd('NetworkFeatures','ReconciliationAnalysis','Reconciliation Analysis (Yes or No)');

  AAdd('OutputReview','YearsFormatStr','%d in %d years');
  AAdd('OutputReview','OutputCompGridMonthlyDeficits','Monthly Deficits');
  AAdd('OutputReview','OutputCompGridSupplyDeficits','Supply and Deficit');
  AAdd('OutputReview','OutputCompGridSupplyComplianceAggregate','Supply and Comliance Aggregate');
  AAdd('OutputReview','OutputGridIFRStats','Required and Supplied IFR Stats');
  AAdd('OutputReview','OutputGridIFRData','Required and Supplied IFR Data');

  AAdd('OutputReview','OutputCompGraphTimeSeries','Monthly Deficits');
  AAdd('OutputReview','OutputGraphReservoirChannel','Reservoir Channel Comparison - Elevation');
  AAdd('OutputReview','OutputGraphReservoirChannelVolume','Reservoir Channel Comparison - Volume');
  AAdd('OutputReview','OutputGraphStorageVolumeNorm','Reservoir Elevation');
  AAdd('OutputReview','OutputGraphStorageVolumeNormStorage','Reservoir Volume');

  AAdd('OutputReview','AvailableDataSources','Available Data Sources');
  AAdd('OutputReview','CurrentDataSource','DataSource Currently in use');
  AAdd('OutputReview','LoadFromSource','Load data');

//  AAdd('OutputReview','OutputGraphStorageVolumeNorm','Storage Volume Comparison - Normal');
  AAdd('OutputReview','OutputGraphStorageVolumeBoxP','Storage Volume Comparison - Box Plot');

  AAdd('OutputReview','OutputGraphSuppliedIFR'  ,'Supplied IFR');
  AAdd('OutputReview','OutputGraphSuppliedIFRDescr'  ,'Simulated from the Yield Model');
  AAdd('OutputReview','OutputGraphRequiredIFR'  ,'Required IFR');
  AAdd('OutputReview','OutputGraphRequiredIFRDescr'  ,'Required IFR based on the IFR relationship and the reference flows');
  AAdd('OutputReview','OutputGraphDefinedIFR'   ,'Defined IFR (F14)');
  AAdd('OutputReview','OutputGraphDefinedIFRDescr'   ,'Defined IFR requirement (F14.dat)');
  AAdd('OutputReview','OutputGraphReferenceFlow','Reference Flow');
  AAdd('OutputReview','OutputGraphReferenceFlowDescr'   ,'Reference flows used to determine required IFR');
  AAdd('OutputReview','OutputGraphReferenceFlowVsIFR','Reference Flow versus IFR');
  AAdd('OutputReview','OutputGraphReferenceFlowVsIFRDescr','Defined reference flow-vs.-IFR relationship');

  AAdd('OutputReview','OutputGraphSuppliedVsRequired','Supplied versus Required');
  AAdd('OutputReview','OutputGraphDefinedIFRVsSupplied','Defined IFR versus Supplied');
  AAdd('OutputReview','OutputGraphDefinedIFRVsRequired','Defined IFR versus Required');
  AAdd('OutputReview','OutputGraphRequiredVsReferenceFlow','Requirement versus Reference Flow');
  AAdd('OutputReview','OutputGraphDifferenceSuppliedVsRequired','Difference between supply and requirement');

  AAdd('OutputReview','OutputGraphMonthlyAverageSuppliedVsRequired','Monthly Averages Supplied versus Required');

  AAdd('OutputReview','OutputGraphYear','Year');
  AAdd('OutputReview','OutputGraphActualAndRequiredIFR','Actual and Required IFR (%s)');
  AAdd('OutputReview','OutputGraphSupplyAndDemand','Supply and Demand (%s)');
  AAdd('OutputReview','DemandandSupplyStats','Demand and Supply Stats');
  AAdd('OutputReview','DemandandSupplyData','Demand and Supply Data');

  AAdd('PenaltyStructure','RuleCurve','Penalty values should be in ascending order below the rule curve.');
  AAdd('PlanningRunConfiguration','InvalidDecisionType','Decision Type should only be M or R.');
  AAdd('PlanningRunConfiguration','InvalidHydroPowerIndicator','H already exist, no duplicate allowed.');


  AAdd('RunConfiguration','SequenceInAnalysis','RunConfiguration: Sequence to be Analysed Must be less than or equal to Number of Sequence in analysis');

  AAdd('RunParameters','AnnualSummaryFlow','Annual Summary Flow only (Q).');
  AAdd('RunParameters','AnnualSummaryDemand','Annual Summary Demands only (D).');
  AAdd('RunParameters','AnnualSummaryBoth','Annual Summary Both (Y).');
  AAdd('RunParameters','AnnualSummaryNeither','Annual Summary Neither (N).');
  AAdd('RunParameters','AnnualSummaryQ','(Q)- Flow Only');
  AAdd('RunParameters','AnnualSummaryD','(D)- Demand Only');
  AAdd('RunParameters','AnnualSummaryY','(Y)- Both');
  AAdd('RunParameters','AnnualSummaryN','(N)- Neither');
  AAdd('RunParameters','ShortTermPlanningNDescr','planning according to set demands and interbasin support excluding curtailment.');
  AAdd('RunParameters','ShortTermPlanningPDescr','curtailment resource allocation model based on short-term yied.');
  AAdd('RunParameters','ShortTermPlanningMDescr','interactive or manual input of allocation decisions.');
  AAdd('RunParameters','ShortTermPlanningN','(N) Set demands and inter-basin support (No curtailment)');
  AAdd('RunParameters','ShortTermPlanningP','(P) Curtailments based on short-term yield');
  AAdd('RunParameters','ShortTermPlanningM','(M) Interactive/manual allocation decisions');

  AAdd('RunParameters','PeriodInfo','Analysis Period Information');
  AAdd('RunParameters','RunInfo','Run Control Information');
  AAdd('RunParameters','HistoricSeq','Historical run information');
  AAdd('RunParameters','StochasticSeq','Stochastic run information');
  AAdd('RunParameters','DebugInfo','Debug Information');
  AAdd('RunParameters','ControlParameters','Control Parameters for Analysis');
  AAdd('RunParameters','Historical','Historical');
  AAdd('RunParameters','Stochastic','Stochastic');
  AAdd('RunParameters','AccessTargetYields','Access target yields');
  AAdd('RunParameters','StartRandomly','Start randomly');
  AAdd('RunParameters','StartHistorical','Start with historical');
  AAdd('RunParameters','StartBootstrap','Bootstrap stochastic method');
  AAdd('RunParameters','OperationalUse','Operational Use');
  AAdd('RunParameters','SequenceOrder','List of sequences to analyse/Stochastic sequence indicator');
  AAdd('RunParameters','YieldSequenceOrder','List of sequences to analyse');

  AAdd('RunParameters','SequenceStartNumber','Sequence to analyse first');
  AAdd('RunParameters','SequenceStartYear','Year to analyse first');
  AAdd('RunParameters','SummaryLevelBrief','(0) Brief summary');
  AAdd('RunParameters','SummaryLevelAdditional','(1) Additional information');
  AAdd('RunParameters','SummaryLevelFull','(2) Full Summary');
  AAdd('RunParameters','SummaryLevelDescBrief','Brief summary output which provides details of various' +
                       'inflow hydrologies and details of system yield (suitable for stochastic ');
  AAdd('RunParameters','SummaryLevelDescAdditional','Brief summary output plus additional info on no. of' +
                       ' critical periods, ave length of critical period, ave critical period deficit & ave drawdown.');
  AAdd('RunParameters','SummaryLevelDescFull','Full summary output consisting of monthly data for each' +
                       'reservoir and specified channel together with all info supplied when option 2 is ');
  AAdd('RunParameters','NrOfActiveReservoirs','Total nr of active reservoirs');
  AAdd('RunParameters','NrOfResInSummary','Total nr of reservoirs included in summary output');
  AAdd('RunParameters','NrOfChanInSummary','Total nr of channels included in summary output');
  AAdd('RunParameters','NrOfChanInFirmYield','Total nr of channels included in firm yield analysis');
  AAdd('RunParameters','ClickIncludeInSummary','Click on this icon to include in summary output');
  AAdd('RunParameters','ClickActivateReservoir','Click on this icon to activate reservoir');
  AAdd('RunParameters','ClickIncludeInFirmYield','Click on this icon for channel yield results to summary output');

  AAdd('DroughtRestriction','ClickReservoirDroughtRestriction','Click on this icon to include reservoir in reference volume');
  AAdd('DroughtRestriction','ClickChannelDroughtRestriction','Click on this icon for channel to be included in restrictions');
  AAdd('DroughtRestriction','DroughtNrOfActiveReservoirs','Total storage volume');
  AAdd('DroughtRestriction','DroughtNrOfResInSummary','Total nr of reservoirs included in reference volume');

  AAdd('DroughtRestriction','DroughtNrOfChanInSummary','Total nr of channels to be restricted');

  AAdd('RunParameters','CalcHistoryOpt','Calculate historic firm yield option ');
  AAdd('RunParameters','DebugFinal','Final time period for debug');
  AAdd('RunParameters','DebugInit','Initial time period for debug');
  AAdd('RunParameters','DebugLevel','Level of debug output');
  AAdd('RunParameters','HydroSeqCount','Number of hydrologic sequences in analysis');
  AAdd('RunParameters','LimitOpt','Limit option');
  AAdd('RunParameters','LoadCasesCount','Number of load cases');
  AAdd('RunParameters','Paramlocation','Location of hydrology parameter file');
  AAdd('RunParameters','MultPeriodOpt','Multiple period lengths option');
  AAdd('RunParameters','NumPeriods','Number of time periods in simulation');
  AAdd('RunParameters','ParamFile','Name and directory of PARAM.DAT file');
  AAdd('RunParameters','PlotOpt','Plotting option');
  AAdd('RunParameters','RandomOpt','Random number option');
  AAdd('RunParameters','ReduceSeqOpt','Reduce number of sequences option');
  AAdd('RunParameters','RunType','Historic (H) or stochastic runs (S)');
  AAdd('RunParameters','RunDescription','Run Description');
  AAdd('RunParameters','StartMonthNo','Start month number');
  AAdd('RunParameters','StartTypeGenerateRandomly','First year of stochastic flows generated based on random seed numbers (this option is used in most cases)');
  AAdd('RunParameters','StartTypeUseHistoricalSequence','First year of stochastic flows generated based on the last year of the historical sequence');
  AAdd('RunParameters','StartTypeBootstrap','Stochastic sequences generated from historical flow sequences based on the technique of random selection and replacement');
  AAdd('RunParameters','StartYearG','Start year (Gregorian calendar)');
  AAdd('RunParameters','StartYearO','Start year (other calendar)');
  AAdd('RunParameters','StoreYield','Store yield results');
  AAdd('RunParameters','SummaryLevel','Level of summary output ');
  AAdd('RunParameters','SummaryOut','Output summary of input data ');
  AAdd('RunParameters','YearsCount','Number of years in hydrologic sequence');
  AAdd('RunTitle','Title1','General title ');
  AAdd('RunTitle','Title2','Description of run');
  AAdd('RunTitle','Title3','Description of any other relevant information');
  AAdd('StudyArea','StudyAreaDescr','Description of the Study Area.');

  AAdd('TabCaption','FileEdit','File Edit');
  AAdd('TabCaption','Graph','Graph');
  AAdd('TabCaption','ViewModel','View Model');
  AAdd('TabCaption','Data','Data');
  AAdd('TabCaption','Data/Graph','Data/Graph');
  AAdd('TabCaption','GridEditor','Grid');
  AAdd('TabCaption','NetworkVisualiser','Network');
  AAdd('TabCaption','Results','Results -2');
  AAdd('TabCaption','Table','Table');
  AAdd('TabCaption','ChangeList','Change Lists');
  AAdd('TabCaption','WeatherEvents','Weather Events');
  AAdd('TabCaption','MetaData','Meta Data');
  AAdd('TabCaption','ModelCapability','Parameter Limits');
  AAdd('TabCaption','ModelCapabilityArray','Array Fields');
  AAdd('TabCaption','ModelCapabilityField','Min-Max Fields');
  AAdd('TabCaption','ModelCapabilityValidations','Restricted Fields');
  AAdd('TabCaption','SystemYield','System Yield');
  AAdd('TabCaption','YRC','YRC');
  AAdd('TabCaption','FirmYields','Firm Yields Lines');
  AAdd('TabCaption','TimeSeriesComparitor','TSC');
  AAdd('TabCaption','TimeSeriesComparitor','Time Series Comparitor');
  AAdd('TabCaption','Properties','Properties');
  AAdd('TabCaption','CatchmentProportions','Catchment Hydrology');
  AAdd('TabCaption','Penalty','Penalty Structures');
  AAdd('TabCaption','PhysicalCharacteristics','Physical Characteristics');
  AAdd('TabCaption','ZoneElevations','Zone Elevations');
  AAdd('TabCaption','OutputReviewSheet','Results');
  AAdd('TabCaption','ReviewGraphSupply','Supply Graph');
  AAdd('TabCaption','ReviewGraphDeficit','Deficit Graph');
  AAdd('TabCaption','ReviewGraphStorage','Storage Graph');
  AAdd('TabCaption','OutputDistributionCurve','Distribution Curve');
  AAdd('TabCaption','WaterUserAssurance','Water User Assurance Criteria');
  AAdd('TabCaption','WaterUseOutputProportioning','Output Water Use Proportioning');
  AAdd('TabCaption','WaterUseScenario','Water Use Scenario');
  AAdd('TabCaption','OutputComplianceGraph','Compliance Graph');
  AAdd('TabCaption','OutputComplianceGrid','Compliance Grid');
  AAdd('TabCaption','OutputWaterUseComplianceGraph','Water Use Compliance Graph');
  AAdd('TabCaption','OutputBoxPlotGraph','Box Plot Graph');
  AAdd('TabCaption','ReviewMonthlyDistributionCurve','Monthly Distribution Curve');
  AAdd('TabCaption','WaterBalance','Water Balance');
  AAdd('TabCaption','ReviewWaterBalance','Average Water Balance');
  AAdd('TabCaption','OutputDataSelection','Output Data Selection');
  AAdd('TabCaption','Hydrology','Hydrology Model');
  AAdd('TabCaption','Yield','Yield Model');
  AAdd('TabCaption','Stomsa','Stomsa');
  AAdd('TabCaption','All','All Models');
  AAdd('TabCaption','CaptionKey','HDYP08Editing');
  AAdd('TabCaption','ModelCapability','Model Capability');
  AAdd('TabCaption','GridEditorKey','GridEditor');
  AAdd('TabCaption','GrowthProjections','Growth Projections');
  AAdd('TabCaption','DemandGrowthProjections','Demand Channels');
  AAdd('TabCaption','MinMaxGrowthProjections','Min Max Channels');
  AAdd('TabCaption','HydrologyGrowthProjections','Hydrology');
  AAdd('TabCaption','GrowthFactors','Growth Factors');
  AAdd('TabCaption','DemandGrowthFactors','Demand Channels');
  AAdd('TabCaption','MinMaxGrowthFactors','Min Max Channels');
  AAdd('TabCaption','HydrologyGrowthFactors','Hydrology');
  AAdd('TabCaption','DailyDiversion','Daily Diversion');
  AAdd('TabCaption','StreamFlowReduction','Stream Flow Reduction');
  AAdd('TabCaption','IFRTabSheetCaption','IFR Site Data');
  AAdd('TabCaption','InflowVsIFRTabSheetCaption','Inflow vs IFR Graph');
  AAdd('TabCaption','StomsaTabSheet','Data');
  AAdd('TabCaption','OutputComparisonReservoir','Reservoir Comparison');
  AAdd('TabCaption','SelectFiles','Selected Files');
  AAdd('TabCaption','OutputComparisonSheet','Output Comparison');
  AAdd('TabCaption','OutputComparisonChannel','Channel Comparison');
  AAdd('TabCaption','InputGrid','Grid Data View');
  AAdd('TabCaption','InputGraph','Graph Data View');
  AAdd('TabCaption','OutputGrid','Grid Data View');
  AAdd('TabCaption','OutputGraph','Graph Data View');
  AAdd('TabCaption','ChannelDemands','Demand');
  AAdd('LabelCaption','PCDExists'            ,'Polution Control Dam Exists');
  AAdd('TabCaption','MineToPCDTabSheet'            ,'Mine To PCD');
  AAdd('TabCaption','MineToRiverTabSheet'          ,'Mine To River');
  AAdd('TabCaption','MineToUnderGroundDamTabSheet' ,'Mine To UnderGround');
  AAdd('TabCaption','MineTabSheet'                 ,'Mine');
  AAdd('TabCaption','SlurryDumpTabSheet'           ,'Discard/Slurry Dump');
  AAdd('TabCaption','UnderGroundSectionTabSheet'   ,'UnderGround Section');
  AAdd('TabCaption','OpenCastPitTabSheet'          ,'Open Cast Pits');
  AAdd('TabCaption','CurtailmentStructure'         ,'Curtailment Structure');
  AAdd('TabCaption','IFRRuleCurve'                 ,'IFR Rule Curve');
  AAdd('TabCaption','IFRVSReferenceFlow'           ,'IFR Vs Reference Flow');
  AAdd('TabCaption','ExceedenceOfIFRReferenceFlows','IFR and Reference Flows');
  AAdd('TabCaption','OutputFixLongtermSupply'     ,'Longterm Supply');
  AAdd('TabCaption','OutputVarLongtermSupply'     ,'Variable Longterm Supply');

  AAdd('TabCaption','OutputDemandChannelSummaryGrid','Summary Grid');
  AAdd('TabCaption','OutputDemandChannelSummaryGraph','Summary Graph');
  AAdd('TabCaption','OutputFixLongtermSupply','Longterm Supply');

  AAdd('TreeViewCaption','GaugeGroup','Gauge Groups');
  AAdd('RadioCaption','Quality',' What quality should the output data have ? ');

end;
procedure LoadMoreLanguageText3(AAdd: TTextItemAddFunction);
begin
  AAdd('RadioCaption','QSum','QSum');
  AAdd('RadioCaption','BarChart','Bar chart');
  AAdd('RadioCaption','LineGraph','Line graph');
  AAdd('RadioCaption','LineGraph','Line graph');
  AAdd('MenuCaption','ReloadMaster','Reload Master Gauge List...');
  AAdd('MenuCaption','SaveSelection','Save selection...');
  AAdd('MenuCaption','DailyData','Extract Daily data...');
  AAdd('MenuCaption','MonthlyData','Extract Monthly data...');
  AAdd('MenuCaption','Exit','Exit');
  AAdd('MenuCaption','Toolbar','Toolbar');
  AAdd('MenuCaption','StatusBar','Status Bar');
  AAdd('MenuCaption','UnselectAll','Unselect All');
  AAdd('MenuCaption','ByRectangle','By Rectangle...');
  AAdd('MenuCaption','ByBuffer','By Buffer...');
  AAdd('MenuCaption','StationNumber','By Station Number...');
  AAdd('MenuCaption','StationName','By Station Name...');
  AAdd('MenuCaption','ReplaceSelection','Replace selection');
  AAdd('MenuCaption','UpdateGISViewer','Update GIS Viewer Live');
  AAdd('MenuCaption','Contents','Contents');
  AAdd('MenuCaption','About','About');
  AAdd('PanelCaption','Date','Date :');
  AAdd('PanelCaption','ModelType','Model Type : ');
  AAdd('PanelCaption','Study','Study : ');
  AAdd('ViewType','OutputGrid','OutputGrid');

  AAdd('GroupBoxCaption','HDYP08',' HDYP08 Input ');
  AAdd('GroupBoxCaption','PatchRInput',' Patch R Input ');
  AAdd('GroupBoxCaption','RunPatchR',' Run Patch R ');
  AAdd('OptionGrid','Diagnostics','Diagnostics');
  AAdd('OptionGrid','Plotting','Plotting');
  AAdd('OptionGrid','UpdateFiles','Update Files');
  AAdd('OptionGrid','No','No');
  AAdd('OptionGrid','Yes','Yes');
  AAdd('OptionGrid','Summary','Summary');
  AAdd('OptionGrid','LogOnly','Log Only');
  AAdd('OptionGrid','Full','Full');
  AAdd('OptionGrid','ScreenAndLog','Screen & Log');
  AAdd('SeasonGrid','Seasons','Seasons');
  AAdd('SeasonGrid','Oct','Oct');
  AAdd('SeasonGrid','Nov','Nov');
  AAdd('SeasonGrid','Dec','Dec');
  AAdd('SeasonGrid','Jan','Jan');
  AAdd('SeasonGrid','Feb','Feb');
  AAdd('SeasonGrid','Mar','Mar');
  AAdd('SeasonGrid','Apr','Apr');
  AAdd('SeasonGrid','May','May');
  AAdd('SeasonGrid','Jun','Jun');
  AAdd('SeasonGrid','Jul','Jul');
  AAdd('SeasonGrid','Aug','Aug');
  AAdd('SeasonGrid','Sep','Sep');
  AAdd('OutputGrid','Average','Average');

  AAdd('GridZone','GridZoneFont','Courier New');
  AAdd('GridZone','GridZoneNumber','Number');
  AAdd('GridZone','GridZoneName','Name');
  AAdd('GridZone','GridZoneType','Type');
  AAdd('GridZone','GridZoneLat','Lat');
  AAdd('GridZone','GridZoneLong','Long');
  AAdd('GridZone','GridZoneStart','Start');
  AAdd('GridZone','GridZoneEnd','End');
  AAdd('GridZone','GridZoneMAP','MAP(mm)');
  AAdd('GridZone','GridZoneStdDev','StdDev(mm)');
  AAdd('GridZone','GridZoneCV','CV %');
  AAdd('GridZone','GridZoneMissingMonths','%Missing months');
  AAdd('GridZone','GridZoneLength','Length');
  AAdd('GridZone','GridZoneHeight','Height');
  AAdd('GridFont','FontName','Courier');
  AAdd('PanelCaption','Disclaimer','Disclaimer:');

  AAdd('CheckBox','UnreliableData','Include unreliable data in statistics');
  AAdd('CheckBox','IncludeStats','Include in statistics');
  AAdd('InputBox','EnterDescription','Enter description of Patch:');

  AAdd('RainfallGraph','LeftTitle','(mm)');
  AAdd('RainfallGraph','BottomTitle','Date');

  AAdd('TargetPower','TPower1','Target system power demand 1');
  AAdd('TargetPower','TPower10','Target system power demand 10');
  AAdd('TargetPower','TPower2','Target system power demand 2');
  AAdd('TargetPower','TPower3','Target system power demand 3');
  AAdd('TargetPower','TPower4','Target system power demand 4');
  AAdd('TargetPower','TPower5','Target system power demand 5');
  AAdd('TargetPower','TPower6','Target system power demand 6');
  AAdd('TargetPower','TPower7','Target system power demand 7');
  AAdd('TargetPower','TPower8','Target system power demand 8');
  AAdd('TargetPower','TPower9','Target system power demand 9');
  AAdd('TargetYield','TYield1','Target system yield1');
  AAdd('TargetYield','TYield10','Target system yield10');
  AAdd('TargetYield','TYield2','Target system yield2');
  AAdd('TargetYield','TYield3','Target system yield3');
  AAdd('TargetYield','TYield4','Target system yield4');
  AAdd('TargetYield','TYield5','Target system yield5');
  AAdd('TargetYield','TYield6','Target system yield6');
  AAdd('TargetYield','TYield7','Target system yield7');
  AAdd('TargetYield','TYield8','Target system yield8');
  AAdd('TargetYield','TYield9','Target system yield9');
  AAdd('TFileEditTabSheet','strGettingHints','Getting hints for the selected file.');
  AAdd('TStudyDocumentManager','AcrobatReaderNotInstalled','Adobe Acrobat Reader is not installed. Install it from the installation CD or get the latest version from http://www.adobe.com/products/acrobat/');
  AAdd('TStudyDocumentManager','WordReaderNotInstalled','Word document reader not installed. Please instal Word document reader. e.g Microsoft Word');
  AAdd('TTargetDraftSelector','None','None');
  AAdd('TTargetDraftSelector','TargetDrafts','Target Drafts');
  AAdd('TTargetDraftSelector','Assurance','Assurance');
  AAdd('TTargetDraftSelector','ZoomSelection','Zoom Selection');
  AAdd('TTargetDraftSelector','ViewToggle','"0 - 100%","75 - 100%","X Axis Selection","Zoom View"');
  AAdd('TTargetDraftSelector','NoFormula','No Formula');
  AAdd('TTargetDraftSelector','HideTargetDrafts','Hide Non-Selected TDs');

  AAdd('TDiversionFeaturesDlg','DiversionChannelType1','This option is used when the diverted flow is considered to be dependent upon the month.');
  AAdd('TDiversionFeaturesDlg','DiversionChannelType2','This option is used when the diverted flow is considered to be a function of the natural inflow to the node.');
  AAdd('TDiversionFeaturesDlg','DiversionChannelType3','Based on natural runoff entering the upstream node, as well as the storage level in a specified reference reservoir.');

  AAdd('TCatchmentProportionsDlg','AllCatchments','All Catchments');
  AAdd('TCatchmentProportionsDlg','CurrentCatchment','Current Catchment');
  AAdd('TCatchmentProportionsDlg','Proportions','Proportions (as %)');
  AAdd('TCatchmentProportionsDlg','ChartTitleNett','Time Series Chart: Nett (INC - (AFF + IRR))');
  AAdd('TCatchmentProportionsDlg','ChartTitleFile','Time Series Chart: ');
  AAdd('TCatchmentProportionsDlg','NETTSeriesTitle','Avg : %s; Std Dev : %s; Number of Negatives : %s');

  AAdd('TCatchmentProportionsDlg','NettRunoff','Nett Incremental Runoff (mcm/a)');
  AAdd('TCatchmentProportionsDlg','IncrementalRunoffA','Incremental Runoff (mcm/a)');
  AAdd('TCatchmentProportionsDlg','IncrementalRunoffM','Incremental Runoff (mcm/m)');
  AAdd('TCatchmentProportionsDlg','AfforestationUseA','Afforestation Water Use (mcm/a)');
  AAdd('TCatchmentProportionsDlg','AfforestationUseM','Afforestation Water Use (mcm/m)');
  AAdd('TCatchmentProportionsDlg','DiffuseDemandA','Diffuse Irrigation Demand (mcm/a)');
  AAdd('TCatchmentProportionsDlg','DiffuseDemandM','Diffuse Irrigation Demand (mcm/m)');
  AAdd('TCatchmentProportionsDlg','PointRainfallA','Point Rainfall (mm/a)');
  AAdd('TCatchmentProportionsDlg','PointRainfallM','Point Rainfall (mm/m)');
  AAdd('TCatchmentProportionsDlg','RankedA','Ranked (mcm/a)');
  AAdd('TCatchmentProportionsDlg','RankedM','Ranked (mcm/m)');

  AAdd('TCatchmentProportionsDlg','FileNotFound','File has not yet been imported.');
  AAdd('TCatchmentProportionsDlg','ActionHintAllReservoir','Add this reservoir to current catcment by drag and drop or use the top button.');
  AAdd('TCatchmentProportionsDlg','ActionHintAllCatchment','Set of hydrological files, referenced in the PARAM.DAT file.');
  AAdd('TCatchmentProportionsDlg','ActionHintCurReservoir','Remove this reservoir from current catcment by drag and drop or use the bottom button.');
  AAdd('TCatchmentProportionsDlg','ActionHintCurCatchment','Selected group of hydrological files for a specified catchment, referenced in the PARAM.DAT file.');
  AAdd('TCatchmentProportionsDlg','AddButtonHint','Use button to Move catcment to all catchments.');
  AAdd('TCatchmentProportionsDlg','RemoveButtonHint','Use button to Remove catcment from all catchments.');

  AAdd('TInputGraphDialog','MeanSeriesTitle','Mean : %s');
  AAdd('TInputGraphDialog','StdDevAboveSeriesTitle','Std Dev Above Mean: %s');
  AAdd('TInputGraphDialog','StdDevBelowSeriesTitle','Std Dev Below Mean: %s');
  AAdd('TInputGraphDialog','MeanStdDevSeriesTitle','Mean : %s; Std Dev : %s');

  AAdd('TIFRFeatureDialog','RefenceFlow','Refrence Flow');
  AAdd('TIFRFeatureDialog','MonthlyRefenceFlow','Monthly');
  AAdd('TIFRFeatureDialog','AnnualRefenceFlow','Annual');
  AAdd('TIFRFeatureDialog','ExceedencePercentage','Exceedence Percentage');
  AAdd('TIFRFeatureDialog','ReferenceNodes','Reference node numbers used for "inflow"');
  AAdd('TIFRFeatureDialog','Exceedenceprobability','Exceedence probability (%)');
  AAdd('TIFRFeatureDialog','DefinedIFR','Defined IFRs');

  AAdd('TIFRFeatureDialog','InflowVariablesDescr','Total inflow of all reference nodes');
  AAdd('TIFRFeatureDialog','IFRVariables','In-stream flow requirement (IFR)');
  AAdd('TIFRFeatureDialog','UpdateIFRFromReferenceInflows','Update IFR From Reference Inflows');
  AAdd('TIFRFeatureDialog','IFRFeatureExists','IFR Feature Exists');
  AAdd('TIFRFeatureDialog','TotalMAR','Total MAR (m3) of the selected Reference Nodes:');

  AAdd('TIFRChannelComplianceGraph','SimulatedIFRRequirement','Simulated IFR Requirement (%s)');
  AAdd('TIFRChannelComplianceGraph','DefinedReferenceFlow','Defined reference flows (%s)');
  AAdd('TIFRChannelComplianceGraph','DefinedAndSimulatedIFR','Defined and simulated IFRs (%s)');
  AAdd('TIFRChannelComplianceGraph','SimulatedIFRSupply','Simulated IFR supplies (%s)');
  AAdd('TIFRChannelComplianceGraph','SimulatedReferenceFlow','Simulated Reference Flow (%s)');
  AAdd('TIFRChannelComplianceGraph','SimulatedIFRAndSupply','Simulated IFR requirements and supplies (%s)');
  AAdd('TIFRChannelComplianceGraph','SimulatedIFRReqVsSupply','Difference between simulated IFR req.s and supplies (%s)');

  AAdd('TIFRChannelComplianceGraph','SimulatedIFRAndRefFlow','Simulated IFR requirements and reference flows (%s)');
  AAdd('TIFRChannelComplianceGraph','DifferenceIFRAndSupplyFlow','Simulated IFR requirements minus Supply flows (%s)');

  AAdd('TIFRChannelComplianceGrid','IFRChannelHeading','Required and Supplied IFR Stats');
  AAdd('TIFRChannelComplianceGrid','IFRChannelCount','Count');
  AAdd('TIFRChannelComplianceGrid','IFRChannelFailure','Failure');
  AAdd('TIFRChannelComplianceGrid','RequiredIFR','Required IFR');
  AAdd('TIFRChannelComplianceGrid','SuppliedIFR','Supply IFR');
  AAdd('TIFRChannelComplianceGrid','RequiredAndSuppliedDifference','Difference');

  AAdd('TIFRChannelComplianceGrid','PercentageOfFailure','% Of Failure');
  AAdd('TIFRChannelComplianceGrid','AverageRequired','Average Required');
  AAdd('TIFRChannelComplianceGrid','AverageSupplied','Average Supply');
  AAdd('TIFRChannelComplianceGrid','MonthlyAverage','Average');

  AAdd('TMasterControlChannelDialog','FactorTotalEdit','Total number of Monthly Water Supply Distribution.');
  AAdd('TMasterControlChannelDialog','YearlyTotalEdit','Total monthly Target');

  AAdd('TMinMaxChannelDialog','PumpingChannels','Pumping Channel Types');

  AAdd('TReservoirAndChannelOutputDialog','NrOfResInSummaryHint','Total number of reservoirs and nodes included in summary output and plotted output.');
  AAdd('TReservoirAndChannelOutputDialog','NrOfActiveReservoirsHint','Total number of active (i.e. selected reservoirs).');
  AAdd('TReservoirAndChannelOutputDialog','NrOfChanInSummaryHint','Total number of channels and nodes included in summary output.');
  AAdd('TReservoirAndChannelOutputDialog','NrOfChanInAnalysesHint','Total number of channels selected for inclusion in summary output.');

  AAdd('TPowerPlantTailwaterDialog','TailWaterType','Tailwater Type');

  AAdd('TReservoirPenaltyDialog','Balancing','Gives option to view balancing data on grid.');
  AAdd('TReservoirPenaltyDialog','PenaltyMode','View all penalty structures used and reservoir grouping per penalty structure');
  AAdd('TReservoirPenaltyValidator','NewPenalty','New Penalty');
  AAdd('TReservoirPenaltyValidator','AddLast','Add At The End');

  AAdd('TReservoirZoneElevation','ViewAs','View As');
  AAdd('TReservoirZoneElevation','Height','Height');
  AAdd('TReservoirZoneElevation','Volume','Volume');
  AAdd('TReservoirZoneElevation','Percentage','Percentage');

  AAdd('TRunConfigurationDialog','RunTypeHistoric','Historical');
  AAdd('TRunConfigurationDialog','RunTypeStochastic','Stochastic');
  AAdd('TRunConfigurationDialog','StartTypeRadioGroup','Stochastic generation options');
  AAdd('TRunConfigurationDialog','DebugEndDatePicker','Final date for debug output');
  AAdd('TRunConfigurationDialog','LoadCaseAscending','Load cases will be sorted in descending order and the number of (active) load cases will be reduced to two');
  AAdd('TRunConfigurationDialog','LoadCaseDescending','Load cases will be sorted in descending order');
  AAdd('TRunConfigurationDialog','ForExpertUseOnly','(For expert use only)');
  AAdd('TRunConfigurationDialog','SequenceStartYearRange','Input must be between start year (StartYearO) and final year of analysis (StartYearO + YearsCount)');
  AAdd('TRunConfigurationDialog','Rigorouslabel','Rigorous Stochastic Method');

  AAdd('TSpecifiedDemandChannelDialog','StochasticIndicator','Indicates that the specified demands to be used in a stochastic analysis have to be generated stochastically.');
  AAdd('TSpecifiedDemandChannelDialog','HistoricIndicator','Indicates that the historical demands will be used to generate specified demands to be used in a stochastic analysis.');
  AAdd('TSpecifiedDemandChannelDialog','DemandFileSavedInDB','This demand file has been imported into the database.');
  AAdd('TSpecifiedDemandChannelDialog','DemandFileNotSavedInDB','This demand file has not yet been imported into the database.');

  AAdd('TStreamFlowReductionDialog','UnitRunoffFileSavedInDB','This unit runoff time-series file has been imported into the database.');
  AAdd('TStreamFlowReductionDialog','UnitRunoffFileNotSavedInDB','This unit runoff time-series file has not yet been imported into the database.');
  AAdd('TStreamFlowReductionDialog','SoilMoistureFileSavedInDB','This total soil moisture file has been imported into the database.');
  AAdd('TStreamFlowReductionDialog','SoilMoistureFileNotSavedInDB','This total soil moisture file has not yet been imported into the database.');

  AAdd('TSCCaptionSelector','Form_Caption','Edit Chart Captions');
  AAdd('TSCCaptionSelector','BtnOK_Caption','Ok');
  AAdd('TSCCaptionSelector','BtnCancel_Caption','Cancel');
  AAdd('TSCCaptionSelector','Grid_Caption1','Name');
  AAdd('TSCCaptionSelector','Grid_Caption2','Old Caption');
  AAdd('TSCCaptionSelector','Grid_Caption3','New Caption');
  AAdd('TSCCaptionChartPanel','Grid_Caption1','Header Caption');
  AAdd('TSCCaptionChartPanel','Grid_Caption2','Footer Caption');
  AAdd('TSCCaptionChartPanel','Grid_Caption3',' Caption');
  AAdd('TSCCaptionSelectorPanel','Views','Views');
  AAdd('TSCCaptionSelectorPanel','Charts','Charts');
  AAdd('TSCCaptionSelectorPanel','Series','Series');
  AAdd('TSCCaptionSelectorPanel','RightYAxis','Right Y-Axis');
  AAdd('TSCCaptionSelectorPanel','NoneSelected','  None Selected');
  AAdd('TSCCaptionSelectorPanel','ZoomCurrchartOnly','Zoom Current Chart Only');
  AAdd('TSCCaptionSelectorPanel','PlotReservoirFixedValues','Plot Reservoir Fixed Values (FSL,DSL,BOT)');


  AAdd('TRISelectorPanel','PanelCaption','Select the requred Assurance intervals');
  AAdd('TBoxPlotSeriesSelector','PanelCaption','Select the requred intervals');

  AAdd('TSCCaptionXYAxisReset','MinVertAxis','Min');
  AAdd('TSCCaptionXYAxisReset','MinHorizAxis','Start Date');
  AAdd('TSCCaptionXYAxisReset','MaxVertAxis','Max');
  AAdd('TSCCaptionXYAxisReset','MaxHorizAxis','End Date');
  AAdd('TSCCaptionXYAxisReset','FormCaption','Reset Horizontal and Vertical Axis Min and Max Values');
  AAdd('TSCCaptionXYAxisReset','IncrementVertBy','Increment');

  AAdd('TSCCaptionXYAxisReset','VerticalGroup','Vertical Axis');
  AAdd('TSCCaptionXYAxisReset','HorizontalGroup','Horizontal Axis' );

  AAdd('TSCChartActionForm','CreateChart_Caption','Create a new Chart');
  AAdd('TSCChartActionForm','RenameChart_Caption','Rename Chart');
  AAdd('TSCChartActionForm','DeleteChart_Caption','Delete Chart');
  AAdd('TSCChartActionForm','CreateView_Caption','Create a new View');
  AAdd('TSCChartActionForm','RenameView_Caption','Rename a View');
  AAdd('TSCChartActionForm','DeleteView_Caption','Delete a View');

  AAdd('TSCChartActionForm','ChartName','Chart Name:');
  AAdd('TSCChartActionForm','ViewName','View Name:');
  AAdd('TSCChartActionForm','OldChartName','Old Chart Name:');
  AAdd('TSCChartActionForm','NewChartName','New Chart Name:');
  AAdd('TSCChartActionForm','OldViewName','Old View Name:');
  AAdd('TSCChartActionForm','NewViewName','New View Name:');

  AAdd('TSCChartLegendDialog','GroupCaption','Legend Position');
  AAdd('TSCChartLegendDialog','DialogCaption','Legend Position');
  AAdd('TSCChartLegendDialog','BtnOkCaption','&Ok');
  AAdd('TSCChartLegendDialog','BtnCancelCaption','&Cancel');

  AAdd('TSCChartLegendDialog','LegendNone','None');
  AAdd('TSCChartLegendDialog','LegendLeft','Left Side');
  AAdd('TSCChartLegendDialog','LegendRight','Right Side');
  AAdd('TSCChartLegendDialog','LegendTop','Top');
  AAdd('TSCChartLegendDialog','LegendBottom','Bottom');

  AAdd('TSCSheet','NoChart','Please select a chart or create a chart before selecting a series.');
  AAdd('TSCSheet','X_AxisLabel','Time');

  AAdd('TYieldModelWizard','ChannelProperties','Channel Properties');
  AAdd('TYieldModelWizard','Upanddownstream','Up- and downstream nodes');
  AAdd('TYieldModelWizard','UpanddownstreamDescr','Up- and downstream nodes may not both be zero nodes');
  AAdd('TYieldModelWizard','Channelpenalty','Channel penalty');
  AAdd('TYieldModelWizard','ChannelpenaltyDescr','Channel penalty number may not be zero');
  AAdd('TYieldModelWizard','MasterControl','Master Control Feature');
  AAdd('TYieldModelWizard','Targetdrafts','Target drafts');
  AAdd('TYieldModelWizard','TargetdraftsDescr','At least one target draft must be specified');
  AAdd('TYieldModelWizard','Distributionfactors','Distribution factors');
  AAdd('TYieldModelWizard','DistributionfactorsDescr','The sum of the monthly distribution factors must be 12');
  AAdd('TYieldModelWizard','MinimumFlowFeature','Minimum Flow Feature');
  AAdd('TYieldModelWizard','MinimumFlowFeatureDescr','Monthly minimum flow demands must be specified');
  AAdd('TYieldModelWizard','LossFeature','Loss Feature');
  AAdd('TYieldModelWizard','LossFeatureDescr','Monthly water loss factors must be specified');
  AAdd('TYieldModelWizard','MinMaxFlowFeature','Min-Max Flow Feature');
  AAdd('TYieldModelWizard','MinMaxFlowFeatureDescr','Monthly flow constraints must be specified for each penalty arc');
  AAdd('TYieldModelWizard','PumpingFeature','Pumping Feature');
  AAdd('TYieldModelWizard','PumpingFeatureDescr','Pumping head and efficiency must be specified');
  AAdd('TYieldModelWizard','SpecifiedDemandFeature','Specified Demand Feature');
  AAdd('TYieldModelWizard','SpecifiedDemandFeatureDescr','Catchment reference and demand file must be specified');
  AAdd('TYieldModelWizard','PhysicalFlowConstraint','Physical Flow Constraint');
  AAdd('TYieldModelWizard','PhysicalFlowConstraintDescr','Constraint type and up- and downstream reservoirs must be specified');
  AAdd('TYieldModelWizard','IFRFeature','IFR Feature');
  AAdd('TYieldModelWizard','IFRRefereneceNodes','IFR Referenece Nodes');
  AAdd('TYieldModelWizard','IFRRefereneceNodesDescr','At least one reference node must be specified');
  AAdd('TYieldModelWizard','IFRInflowAndRelease','Inflow vs Release');
  AAdd('TYieldModelWizard','IFRRefereneceNodesDescr','Inflow vs. release values must be specified');
  AAdd('TYieldModelWizard','DiversionFeature','Diversion Feature');
  AAdd('TYieldModelWizard','DiversionFeatureDescr',
                           'Diversion type 1: Monthly diversion demand and proportion of net diverted flow must be specified. ' +
                           'Diversion type 2: Flow ranges and actual diverted flow must be specified. ' +
                           'Diversion type 3: Controlling reservoir and proportion of diverted flow must be specified.' +
                           'Diversion type 4: Monthly loss factors and actual diverted flow must be specified. ');
  AAdd('TYieldModelWizard','SpecifiedInflowFeature','Specified Inflow Feature');
  AAdd('TYieldModelWizard','SpecifiedInflowFeatureDescr','Specified inflow feature name must be specified');
  AAdd('TYieldModelWizard','NodeProperties','Node Properties');
  AAdd('TYieldModelWizard','NodePropertiesDescr','Node name must be defined');
  AAdd('TYieldModelWizard','HydrologicalCatchment','Catchment Hydrology');
  AAdd('TYieldModelWizard','HydrologicalCatchmentDescr','Hydrological catchment must be assigned');
  AAdd('TYieldModelWizard','ReservoirProperties','Reservoir Properties');
  AAdd('TYieldModelWizard','ReservoirPropertiesDescr','Reservoir penalty must be defined');
  AAdd('TYieldModelWizard','ReservoirEvaporation','Reservoir Evaporation');
  AAdd('TYieldModelWizard','ReservoirEvaporationDescr','Evaporation data must be defined');
  AAdd('TYieldModelWizard','PhysicalCharacteristics','Physical Characteristics');
  AAdd('TYieldModelWizard','StorageCharacteristics','Physical Characteristics');
  AAdd('TYieldModelWizard','StorageCharacteristicsDescr','At least one volume entry must be defined');
  AAdd('TYieldModelWizard','SurfaceArea','Surface Area');
  AAdd('TYieldModelWizard','SurfaceAreaDescr','At least one surface area entry must be defined');
  AAdd('TYieldModelWizard','ZoneElevations','Month-end zone elevations');
  AAdd('TYieldModelWizard','ZoneElevationsDescr','Month-end zone elevations must be assigned');
  AAdd('TYieldModelWizard','LReservoirPenalties','Reservoir penalty');
  AAdd('TYieldModelWizard','LReservoirPenaltiesDescr','Reservoir penalty number may not be zero');

  AAdd('TYRCSelectorsPanel','PlaneCaption','Period Length :');
  AAdd('TYRCSelectorsPanel','PlotPlaneCaption','Plotting Base :');
  AAdd('TYRCSelectorsPanel','LoadCaseCaption','Load Case :');
  AAdd('TYRCSelectorsPanel','ViewPointsCaption','View Points :');
  AAdd('TYRCSelectorsPanel','ViewTransformedCaption','View Transformed');
  AAdd('TYRCSelectorsPanel','CaptionNormal','Load chart from output file or database');
  AAdd('TYRCSelectorsPanel','CaptionHistoric','Historic yield was run. Run model with stochastic set on to produce YRC Graph.');
  AAdd('TYRCSelectorsPanel','CaptionNoOutputFile','The selected output file was not found on the hard drive. Check File Edit tabsheet');
  AAdd('TYRCSelectorsPanel','CaptionNoDbData','There is no data in the database for the current scenario.');

  AAdd('Users','FirstName','User''s FirstName');
  AAdd('Users','Initials','Initials of the User');
  AAdd('Users','LastName','User''s LastName');
  AAdd('Users','PreferedLanguage','User''s PreferedLanguage');
  AAdd('Users','SecondName','User''s SecondName');
  AAdd('UUtilities','strNoFileName','There is no file name passed to determine the hydrological file type.');
  AAdd('UUtilities','strNoExtention','Hydrology file name %s does not have an extention to use to detemine the file type.');
  AAdd('UUtilities','strExtentionNew','File %s has an unknown file extention. Hydrological file type could not be detemined.');

  AAdd('WaterUse','Channel','Channel');
  AAdd('WaterUse','Category','Category');
  AAdd('WaterUse','Scenario','Scenario');
  AAdd('WaterUse','No','No');
  AAdd('WaterUse','Name','Name');
  AAdd('WaterUse','WaterUse','Proportion of water use per channel');
end;

procedure LoadMoreLanguageText4(AAdd: TTextItemAddFunction);
begin
  AAdd('ChangeLists' ,'ChangeGroup'        ,'Change Group');
  AAdd('ChangeLists' ,'NewChangeGroup'     ,'New change group');
  AAdd('ChangeLists' ,'NewPatchChangeGroup','New Patch change group');
  AAdd('ChangeLists' ,'Absolut'            ,'Absolute');
  AAdd('ChangeLists' ,'AbsOrPerc'          ,'Abs / %');
  AAdd('ChangeLists' ,'Active'             ,'Active');
  AAdd('ChangeLists' ,'BaseValue'          ,'Base value');
  AAdd('ChangeLists' ,'Change'             ,'Change');
  AAdd('ChangeLists' ,'ChangeList'         ,'Change list');
  AAdd('ChangeLists' ,'ChangeListName'     ,'List name');
  AAdd('ChangeLists' ,'ChangeGroupName'    ,'Group name');
  AAdd('ChangeLists' ,'CreatedBy'          ,'Created by');
  AAdd('ChangeLists' ,'DateCreated'        ,'Date created');
  AAdd('ChangeLists' ,'Description'        ,'Description');
  AAdd('ChangeLists' ,'EntityDescr'        ,'Entity');
  AAdd('ChangeLists' ,'KeyValues'          ,'Key values');
  AAdd('ChangeLists' ,'ParameterChanges'   ,'Parameter Changes');
  AAdd('ChangeLists' ,'ParameterName'      ,'Parameter');
  AAdd('ChangeLists' ,'ParameterDescr'     ,'Descr');
  AAdd('ChangeLists' ,'Percentage'         ,'%');
  AAdd('ChangeLists' ,'NewValue'           ,'New value');
  AAdd('ChangeLists' ,'Nr'                 ,'Nr');
  AAdd('ChangeLists' ,'Order'              ,'Order');
  AAdd('ChangeLists' ,'Override'           ,'Override');
  AAdd('ChangeLists' ,'ShowAllChangeLists' ,'Show all change lists');
  AAdd('ChangeLists' ,'StartYearMonth'     ,'Start year and month');
  AAdd('ChangeLists' ,'EndYearMonth'       ,'End year and month');
  AAdd('ChangeLists' ,'PleaseSelectChangeList'    ,'Please select a change list');
  AAdd('ChangeLists' ,'PleaseSelectStartYear'     ,'Please select a start year');
  AAdd('ChangeLists' ,'PleaseSelectStartMonth'    ,'Please select a start month');
  AAdd('ChangeLists' ,'PleaseSelectEndYear'       ,'Please select a end year');
  AAdd('ChangeLists' ,'PleaseSelectEndMonth'      ,'Please select a end month');
  AAdd('ChangeLists' ,'EndDateBeforeStartDate'    ,'End date must be later than start date');
  AAdd('ChangeLists' ,'PleaseSelectChangeFlag'    ,'Please select a change/flag (not None)');
  AAdd('ChangeLists' ,'StartDateOutOfRange'       ,'Start date must fall between %s and %s.');
  AAdd('ChangeLists' ,'EndDateOutOfRange'         ,'End date must fall between %s and %s.');
  AAdd('ChangeLists' ,'AlreadyContains'           ,'%s already contains %s');
  AAdd('ChangeLists' ,'AlreadyContainsFlag'       ,'%s already contains a flag for %s');
  AAdd('ChangeLists' ,'DoesNotContainFlag'        ,'%s does not contain flag %s for %s');
  AAdd('ChangeLists' ,'PleaseEnterChangeValue'    ,'Please enter a change value');
  AAdd('ChangeLists' ,'NotYetImplementedForModel' ,'Not yet implemented for %s model.');
  AAdd('ChangeLists' ,'PleaseCreateChangeList'    ,'Please create a change list');
  AAdd('ChangeLists' ,'NewChangeList'             ,'New change list');
  AAdd('ChangeLists' ,'None'                      ,'None');
  AAdd('ChangeLists' ,'CopyOf'                    ,'Copy of %s');


  AAdd('MetaData'    ,'MetaData'                  ,'Meta Data');
  AAdd('MetaData'    ,'Comment'                   ,'Comment');
  AAdd('MetaData'    ,'PleaseEnterPerson'         ,'Please enter the name of the person who created the meta data.');
  AAdd('MetaData'    ,'PleaseEnterComment'        ,'Please enter a comment.');
  AAdd('LabelMean'   ,'Caption'                   ,'');
  AAdd('LineSeriesCheckbox' ,'Mean'               ,'Mean');
  AAdd('LineSeriesCheckbox' ,'StandardDeviation'  , 'Standard Deviation');
  //
// CheckBoxes
  AAdd('CheckBoxText','BalancingData', 'Balancing Data');

//
// Label Text
//
  AAdd('LabelText','ExtractFiles','Extract files');
  AAdd('LabelText','Yes', 'Yes');
  AAdd('LabelText','YesToALl', 'Yes To All');
  AAdd('LabelText','No', 'No');
  AAdd('LabelText','NoToAll', 'No To All');
  AAdd('LabelText','AreaWhenFull', 'Area when full (km²)');
  AAdd('LabelText','StartingStorage', 'Starting Storage Level (masl)');
  AAdd('LabelText','FullStorageLevel', 'Full Supply Level (masl)');
  AAdd('LabelText','DeadStorageLevel', 'Dead Storage Level (masl)');
  AAdd('LabelText','BottomOfReservoir', 'Bottom Level (masl)');
  AAdd('LabelText','FileInc', 'Incremental Run Off (INC)');
  AAdd('LabelText','FileRnk', 'Incremental Run Off Rank (RNK)');
  AAdd('LabelText','FileAff', 'Afforestation Demand (AFF)');
  AAdd('LabelText','FileIrr', 'Diffuse Demand (IRR)');
  AAdd('LabelText','FileRan', 'Rainfall (RAN)');
  AAdd('LabelText','FileCatchment', 'Catchment File');
  AAdd('LabelText','FileProportion', 'Proportion');
  AAdd('LabelText','FileName', 'File Name');
  AAdd('LabelText','RuleCurveLevel', 'Rule Curve');
  AAdd('LabelText','FullSupplyLevel', 'Full Supply Level (masl)');
  AAdd('LabelText','DeadStorageLevel', 'Dead Storage Level (masl)');
  AAdd('LabelText','BottomOfReservoirLevel', 'Bottom Level (masl)');
  AAdd('LabelText','NumberOfYears', 'Number of Years:');
  AAdd('LabelText','BaseYear', 'Base Year:');
  AAdd('LabelText','StartYear', 'Start Year :');
  AAdd('LabelText','EndYear', 'End Year :');
  AAdd('LabelText','PlotActualVolume','Plot Actual Volume');
  AAdd('LabelText','DummyValues','Values in the second from last column are dummy values required by the planning model');
  AAdd('LabelText','SelectStartMonth','Select Start Month e.g JAN = 1');

  //
  // Grid Heading
  //

  AAdd('GridHeading','AcceptableValues', 'Acceptable Values');
  AAdd('GridHeading','AnalysisStartVolume','Analysis Start Volume');
  AAdd('GridHeading','ArcNumber','Arc Number');
  AAdd('GridHeading','ArrayLength', 'Length Of Array');

  AAdd('GridHeading','BalancingDataColumn.S', 'S');
  AAdd('GridHeading','BalancingDataColumn.V', 'V');
  AAdd('GridHeading','BalancingDataColumn.R', 'R');
  AAdd('GridHeading','BaseYearDemand','Base Year Demand');
  AAdd('GridHeading','BOR', 'BOR');

  AAdd('GridHeading','ChannelNr','Channel Nr');
  AAdd('GridHeading','ChannelName','Channel Name');
  AAdd('GridHeading','ColumnID','ColumnID');
  AAdd('GridHeading','ColumnNameInFile','ColumnName In File');
  AAdd('GridHeading','CropNumber','Crop Number');
  AAdd('GridHeading','CropType','Crop Type');
  AAdd('GridHeading','CropTypePercArea','% Area');

  AAdd('GridHeading','DatasetID','DatasetID');
  AAdd('GridHeading','Depth', 'Depth (m)');
  AAdd('GridHeading','DecantVolume', 'Decant Volume');
  AAdd('GridHeading','DrawDownZone', 'Draw Down Zone');
  AAdd('GridHeading','DownStreamNode','Downstream Node');
  AAdd('GridHeading','DumpSurfaceArea','Dump Surface Area');
  AAdd('GridHeading','DSL', 'DSL');

  AAdd('GridHeading','Elevation', 'Elevation (masl)');
  AAdd('GridHeading','EndPeriod', 'EndPeriod');
  AAdd('GridHeading','Evaporation', 'Lake Evaporation (mm)');

  AAdd('GridHeading','Factor','Factor');
  AAdd('GridHeading','FactorType','Factor Type');
  AAdd('GridHeading','FieldDataType','Field DataType');
  AAdd('GridHeading','FieldDescription', 'Field Description');
  AAdd('GridHeading','FieldGroup', 'Field Group');
  AAdd('GridHeading','FieldLength','Field Length');
  AAdd('GridHeading','FieldName', 'Field Name');
  AAdd('GridHeading','FieldSize','Field Size');
  AAdd('GridHeading','FieldType','Field Type');
  AAdd('GridHeading','FileAff', 'AFF');
  AAdd('GridHeading','FileInc', 'INC');
  AAdd('GridHeading','FileUrb', 'URB');
  AAdd('GridHeading','FileIncItem1', 'Individual Files');
  AAdd('GridHeading','FileIncItem2', 'Nett of all files');
  AAdd('GridHeading','FileIrr', 'IRR');
  AAdd('GridHeading','FileName', 'File Field Name');
  AAdd('GridHeading','FlowDiversion'   ,'Portion diverted to reclamation plant');
  AAdd('GridHeading','FSL', 'FSL');

  AAdd('GridHeading','GaugeNumber','Gauge Number');

  AAdd('GridHeading','Institutions','Institutions');

  AAdd('GridHeading','MaximumSeepageRate','Maximum Seepage Rate');
  AAdd('GridHeading','MaximumValue', 'Maximum Value');
  AAdd('GridHeading','MinimumValue', 'Minimum Value');
  AAdd('GridHeading','ModelFieldDescription', 'Model Field Description');
  AAdd('GridHeading','ModelFieldName', 'Model Field Name');
  AAdd('GridHeading','Month', 'Month');
  AAdd('GridHeading','MonthlyRecharge', 'Monthly Recharge Factors');

  AAdd('GridHeading','Number'          ,'#');

  AAdd('GridHeading','PenaltyStructure','Penalty Structure');
  AAdd('GridHeading','PitName','Pit Name');
  AAdd('GridHeading','PrintOutput'     ,'Print Output?');

  AAdd('GridHeading','SeepageExponent', 'Seepage Exponent');
  AAdd('GridHeading','SeepageVolume', 'Seepage Volume');
  AAdd('GridHeading','DumpName', 'Dump Name');
  AAdd('GridHeading','SeepageSplitFactor', 'Seepage Split Factor');
  AAdd('GridHeading','SourceFileName', 'Source File Name');
  AAdd('GridHeading','StartPeriod', 'Start Period');
  AAdd('GridHeading','SurfaceArea', 'Surface Area (km²)');

  AAdd('GridHeading','Total', 'Total');
  AAdd('GridHeading','TotalReturnFlow' ,'Portion of total return flow');

  AAdd('GridHeading','UpStreamNode'    ,'UpStream Node');
  AAdd('GridHeading','Units'           ,'Units');
  AAdd('GridHeading','UnitType'        ,'Unit Type');

  AAdd('GridHeading','Volume', 'Volume (m³ x 10^6)');
  AAdd('GridHeading','VolumeInPCDAnalysis', 'PCD Analysis Start Volume');

  AAdd('GridHeading','Water Users','Water Users');
  AAdd('GridHeading','WaterSurfaceEvapArea','Water Surface Evaporation Area');

  AAdd('GridHeading','Zone', 'Level');
  AAdd('GridHeading','ZoneName', 'Zone Name');

  AAdd('GridHeading','ErrType', 'Error Type');
  AAdd('GridHeading','ErrCode', 'Error Code');
  AAdd('GridHeading','ErrDescription', 'Error Description');
  AAdd('GridHeading','ErrSource', 'Error mapping in source file');
  AAdd('GridHeading','ErrValue', 'Error Value');
  AAdd('GraphHeading','TargetDraft','Target Draft Million m³/a');
  AAdd('GraphHeading','SysYield','Yield (Million m³/a)');

  AAdd('GrowthFormula','Projection','Demand Projection  = Base Year Demand * (Growth Factor –1)');
  AAdd('GrowthFormula','Growth','Growth Factor = (Demand Projection/Base Year Demand) –1');

  AAdd('ChartTitle','Evaporation', 'Lake Evaporation (mm)');
  AAdd('ChartTitle','Month', 'Month');
  AAdd('ChartTitle','RecordLengthChart','Periods for Reservoir Time Control ');

  AAdd('LabelText' ,'Channel','Channel');
  AAdd('LabelText' ,'CatchmentScaleFactor','Catchment Scale Factor');
  AAdd('LabelText' ,'SelectAMonth','Select A Month');
  AAdd('LabelText' ,'MonthlyCompensationValues','Monthly Compensation Values (m³/s):');
  AAdd('LabelText' ,'CapacityOfDiversion','Capacity of diversion (m³/s):');
  AAdd('LabelText' ,'ScalingFactor','Scaling Factor:');
  AAdd('LabelText' ,'StationNo','Station no:');
  AAdd('LabelText' ,'Place','Place:');
  AAdd('LabelText' ,'Latitude','Latitude:');
  AAdd('LabelText' ,'Longitude','Longitude:');
  AAdd('LabelText' ,'CatchmentArea','Catchment area (sq. km):');
  AAdd('LabelText' ,'CatchmentFactor','Catchment Factor:');
  AAdd('LabelText' ,'Dataset','Dataset : ');
  AAdd('LabelText' ,'FileName','File Name : ');
  AAdd('LabelText' ,'MetaDataComment','Change list meta data comment');
  AAdd('LabelText' ,'LastUpdated','Last updated:');
  AAdd('LabelText' ,'PleaseEnterMetadata','Please enter metadata');
  AAdd('LabelText' ,'DatasetName','Dataset Name :');
  AAdd('LabelText' ,'CurrentChangeValue',' The current change value is: ');
  AAdd('LabelText' ,'NoValueAvailable',' No change value available for this field.');
  AAdd('LabelText','RecurranceInterval','Assurance Interval');
  AAdd('LabelText','FieldListMetadataComment','Field List Meta data comment');
  AAdd('LabelText','FieldListMetadataDateUpdated','Last Updated');

  AAdd('TabCaption','NetworkVisualiser','Network');
  AAdd('TabCaption','Channel','Channel : ');
  AAdd('TabCaption','CatchmentScaleFactor','Catchment Scale Factor : ');
  AAdd('TabCaption','Grid','Grid');
  AAdd('TabCaption','Graph','Graph');
  AAdd('TabCaption','DiversionGraph','Diversion Graph');
  AAdd('TabCaption','NonDiversionGraph','Non-Diversion Graph');

  AAdd('TabCaption','DailyIFRGrid','Daily Diversion Grid');
  AAdd('TabCaption','MonthlyIFRGrid','Monthly Diversion Grid');
  AAdd('TabCaption','DailyIFRGraph','Daily Diversion Graph');
  AAdd('TabCaption','MonthlyIFRGraph','Monthly Diversion Graph');
  AAdd('TabCaption','MonthlyFlowGraph','Monthly Flow Graph');
  AAdd('TabCaption','DailyFlowGraph','Daily Flow Graph');
  AAdd('TabCaption','DailyFlowGrid','Daily Flow Grid');
  AAdd('TabCaption','MonthlyFlowGrid','Monthly Flow Grid');
  AAdd('TabCaption','RunDefinitions','Run definitions');
  AAdd('TabCaption','Map','Map');
  AAdd('TabCaption','Summary','Summary');
  AAdd('TabCaption','BrowseDataset','Browse Dataset');
  AAdd('TabCaption','Changelists','Changelists');
  AAdd('TabCaption','Metadata','Metadata');
  AAdd('TabCaption','Lookup','Lookup');
  AAdd('TabCaption' ,'CreateDailyIFRData','Create Daily IFR Data');
  AAdd('TabCaption','Grid','Grid');
  AAdd('TabCaption','Groundwater','Groundwater');
  AAdd('TabCaption','GroundwaterPitman','Pitman module');
  AAdd('TabCaption','OutputDemandChannelSummaryGrid','Demand Channel Summary');

  AAdd('AccessControl','ReadOnly','Read Only');
  AAdd('AccessControl','WriteOnly','Write Only');
  AAdd('AccessControl','ReadAndWrite','Read And Write');
  AAdd('AccessControl','SystemAdministrator','System Administrator');

  AAdd('TBSPChangelistValidator' ,'SelectArea','Select Area');
  AAdd('TBSPChangelistValidator' ,'SelectPeriod','Select Period');
  AAdd('TBSPChangelistValidator' ,'SelectField','Select Field');

  AAdd('TGridEditorSheet','UnknownGridEditorEventType','Unknown grid editor event type [%d].');

  AAdd('TExplorerValidator','DatasetExplorer','Dataset Explorer');
  AAdd('TExplorerValidator','EnterDatasetDescription','Enter Dataset Group Description:');

  AAdd('TDailyIFRDataValidator' ,'SavingDataToDatabase','Saving Data to the Database...');
  AAdd('TDailyIFRDataValidator' ,'DoneSavingDataToDatabase','Done Saving Data to the Database!!');
  AAdd('TDailyIFRDataValidator' ,'ProcessingData','Processing data for %s');

  AAdd('TStudyPanel','SubArea','SubArea:');
  AAdd('TStudyPanel','Scenario','Scenario:');
  AAdd('TStudyPanel','User','User : ');
  AAdd('TStudyPanel','LastUser','Last User :');

  AAdd('TInformixDatabaseLayer','ServerName','SERVER NAME=');
  AAdd('TInformixDatabaseLayer','DatabaseName','DATABASE NAME=');
  AAdd('TInformixDatabaseLayer','UserName','USER NAME=');
  AAdd('TInformixDatabaseLayer','Password','PASSWORD=');
  AAdd('TInformixDatabaseLayer','SQLPassThruMode','SQLPASSTHRUMODE=SHARED AUTOCOMMIT');

  AAdd('TMonthlyReferenceFlowDialog' ,'MonthlyReferenceFlow','Monthly Reference Flow (m³/s)');
  AAdd('TMonthlyReferenceFlowDialog' ,'DailyReferenceFlow','Daily Reference Flow (m³/s)');
  AAdd('TMonthlyReferenceFlowDialog' ,'Time','Time (Months)');

  AAdd('TMonthlyDiversionFlowDialog' ,'MonthlyDiversionFlow','Monthly Diversion Flow (m³/s)');
  AAdd('TMonthlyDiversionFlowDialog' ,'DailyDiversionFlow','Daily Diversion Flow (m³/s)');
  AAdd('TMonthlyDiversionFlowDialog' ,'Time','Time (Months)');

  AAdd('Message','CannotAddChangeGroupToItself','ERROR: Cannot add a ChangeGroup to itself');
  AAdd('Message','CannotAddChangeGroupToGroupWithChangeList','ERROR: Cannot add a ChangeGroup to a Group that contains ChangeLists');
  AAdd('Message','ChangeListMayNotBeAddedAtTopLevel','ERROR: A ChangeList may not be added at top level, it must be added to a Group');
  AAdd('Message','CannotAddChangeGroupToGroupWithOtherGroups','ERROR: Cannot add a ChangeList to a Group that contains other Groups');
  AAdd('Message','DuplicateListInclusion','ERROR: Duplicate list inclusion');
  AAdd('Message','NotImplemented','Not implemented for this Model');
  AAdd('Message','RecordWithSameDateExist','Record with same date already exist');
  AAdd('Message','StationNoExist','Station No: already exist');
  AAdd('Message','YouCannotUse','You cannot use "');
  AAdd('Message','ChooseDifferentNameOrCancel','" please choose a different name or cancel.');
  AAdd('Message','DatasetExistsChooseDifferentName','Dataset Group already exists, choose a different name or cancel.');
  AAdd('Message','DatasetExistsChangeOrCancel','Dataset Group Already Exists, change or cancel.');
  AAdd('Message','EnterValidDatasetName','You must enter a valid dataset name. Please choose a different name or cancel.');
  AAdd('Message','DatasetNameExists','Dataset Name already exists, choose a different name or cancel.');
  AAdd('Message','PleaseEnterName','Please enter name.');
  AAdd('Message','PleaseEnterChangeValue','Please enter change value.');
  AAdd('Message','ItemsLinkedToSelectedNode','Items linked to selected node. Node can not be deleted.');
  AAdd('Message','TotalNumberOfReturnFlow','Total number of return flow specifications is 10.');
  AAdd('Message','FlowChannelDataExist','Return flow Channel Data exist already.');
  AAdd('Message','ApplicationLoadingStarted','Application loading started, please wait...');
  AAdd('Message','CurrentlyLoadingListOfGauges','Currently loading "List of Gauges", please wait ...');
  AAdd('Message','FinishedLoadingListOfGauges','Finished loading "List of Gauges".');
  AAdd('Message','InitialisingGISViewer','Currently Initialising GIS Viewer component (this will take a while, please wait...)');
  AAdd('Message','FinishedInitialisingGISViewer','Finished Initialising GIS Viewer component. Application loading finished');
  AAdd('Message','InvalidCoordinateEntered','Invalid co-ordinate entered.');
  AAdd('Message','OptionNotImplemented','This option has not being implemented.');
  AAdd('Message','ImplementWhenGISViewerImproves','(It will be implemented when the GIS Viewer performance improves).');
  AAdd('Message','MenuClickNotHandled','Menu click not handled.');
  AAdd('Message','CurrentSelectionNotSaved','The current selection has not being saved, do you want to save your selection ?');
  AAdd('Message','NoGaugesSelected','No gauges have been selected, please select at least 1 gauge first.');
  AAdd('Message','DataIsBeingExtracted','The data is being extracted, after extraction the data will be stored in "');
  AAdd('Message','MenuClicked','Menu clicked');
  AAdd('Message','RunConfiguration','Run Configuration');
  AAdd('Message','OldSumOut','Old sum.out has (');
  AAdd('Message','YearsWhile',') years while ');
  AAdd('Message','NewSumOut','New sum.out has (');
  AAdd('Message','Years',') years.');
  AAdd('Message','NumberYearsMustBeSame','The number of years must be the same to merge.');
  AAdd('Message','SequencesWhile',') sequences while');
  AAdd('Message','Sequences',') sequences. ');
  AAdd('Message','NumberSequencesMustBeSame','The number of sequences must be the same to merge.');

  AAdd('Message','SelectDemandFilePath','Please select the demand files path(in Run Configuration) before selecting demand files.');
  AAdd('Message','SelectDemandFromSpecifiedDir','Please select the demand only from the specified demand files directory only.');

  AAdd('Message','ProcessingTable','Processing Table :');
  AAdd('Message','CreatingScriptListFile','Creating script lis file.');
  AAdd('Message','CreatingDocumentsListFile','Creating documents fist file.');
  AAdd('Message','MovingFileToZipFile','Moving file to the zip file');
  AAdd('Message','File',' File');
  AAdd('Message','LastChangedOn',' last changed on ');
  AAdd('Message','AlreadyExists',' already exists.');
  AAdd('Message','WantToReplaceWithFile','Do you want to replace with file ');
  AAdd('Message','NotFound',' Not Found.');
  AAdd('Message','FileXML','File(');
  AAdd('Message','FileDoesNotExist',') does not exist and will be ignored.');
  AAdd('Message','ProcessingFile','Processing File:');
  AAdd('Message','LoadingMainBuildScriptFile','Loading Main Build Script File(.dat)...');
  AAdd('Message','Table','Table (');
  AAdd('Message','DoesNotHavePSroperties',') does not have properties and will be ignored.');
  AAdd('Message','LoadingSystemData','Loading System Data...');
  AAdd('Message','Done','Done!!');
  AAdd('Message','NoTableProperties','Table Properties does not exist.');
  AAdd('Message','NoFieldsDefinition','Fields Definition does not exist and will be ignored.');
  AAdd('Message','PreProcessingFileData','PreProcessing file data.');
  AAdd('Message','Processing','Processing : ');
  AAdd('Message','MovingStudiesZipFiles','Moving studies zip files to the main zip file');
  AAdd('Message','MovingSubZipFiles','Moving sub zip files to the main zip file');
  AAdd('TabCaption','RWHRunConfiguration','Run Configuration');
  AAdd('TabCaption','RWHRunModel','Run Model');
  AAdd('TabCaption','RWHGaugeSelection','Gauge Selection');
  AAdd('TabCaption','RWHViewOutput','View Output');
  AAdd('TabCaption','RWHViewGaugeData','View Gauge Data');
  AAdd('TabCaption','RWHViewContours','View Contours');
  AAdd('FormCaption','RWH','Rain Water Harvesting');
   AAdd('TabCaption','DDTS','Data');

end;


end.
