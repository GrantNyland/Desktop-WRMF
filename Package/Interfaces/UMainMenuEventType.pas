//
//
//  UNIT      : Contains the class TVaalDBMSMenu.
//  AUTHOR    : Grant Nyland
//  DATE      : 2002/02/20
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UMainMenuEventType;

interface

const
  CmeOK                             =  1;
  CmeCancel                         =  2;
  CmeSelectStudy                    =  3;
  CmePrintSettings                  =  4;
  CmePrintPreview                   =  5;
  CmePrint                          =  6;
  CmeLogOn                          =  7;
  CmeLogOff                         =  8;
  CmeExit                           =  9;
  CmeCopyToClipboard                = 10;
  CmeExportToFile                   = 11;
  CmeViewToolBar                    = 12;
  CmeViewStudyPanel                 = 13;
  CmeViewFileEdit                   = 14;
  CmeViewEditGrid                   = 15;
  CmeViewGraph                      = 16;
  CmeViewNetworkVisualiser          = 17;
  CmeViewReset                      = 18;
  CmeTabSheetCustomEvent            = 19;
  CmeLaunchStudyReport              = 20;
  CmeCustomModelEvent               = 21;
  CmeEditGridEditor                 = 24;
  CmeRefreshMenuItems               = 25;
  CmeSelectStudyDetails             = 26;
  CmeDataViewJump                   = 27;
  CmeViewResultGraphs               = 28;
  CmeResultPageControlChanged       = 29;
  CmeResultPageControlChanging      = 30;
  CmeRunModel                       = 31;
  CmeValidateFiles                  = 32;
  CmeImportFiles                    = 33;
  CmeExportFiles                    = 34;
  CmeExportFile                     = 35;
  CmeImportFile                     = 36;
  CmeClearModelData                 = 37;
  CmeValidateFile                   = 38;
  CmeYRCLoadFromDB                  = 39;
  CmeYRCLoadFromFile                = 40;
  CmeYRCSaveChart                   = 41;
  CmeRefreshFileHints               = 42;
  CmeCreateReservoir                = 47;
  CmeDeleteReservoir                = 48;
  CmeCreateNodeWithInflow           = 49;
  CmeDeleteNodeWithInflow           = 50;
  CmeCreateNodeWithoutInflow        = 51;
  CmeDeleteNodeWithoutInflow        = 52;
  CmeCreateChannel                  = 53;
  CmeDeleteChannel                  = 54;
  CmeConvertChannel                 = 55;
  CmeCreateMinimumFlowFeature       = 56;
  CmeCreateMinMaxFlowFeature        = 57;
  CmeCreatePumpingFeature           = 58;
  CmeCreateLossFeature              = 59;
  CmeCreateSpecifiedDemandFeature   = 60;
  CmeCreateDiversionFeature         = 61;
  CmeCreateSpecifiedInflowFeature   = 62;
  CmeCreateIFRFeature               = 63;
  CmeCreatePhysicalFlowConstraint   = 64;

  CmeCreateIrrigationArea           = 65;
  CmeCreatePowerPlant               = 66;

  CmeDeletePumpingFeature           = 67;
  CmeDeleteIFRFeature               = 68;
  CmeDeletePhysicalFlowConstraint   = 69;
  CmeDeleteIrrigationArea           = 70;
  CmeDeletePowerPlant               = 71;
  CmeCreateMasterControlFeature     = 72;
  CmeDeleteMasterControlFeature     = 73;
  CmeCreateWaterDemandFeature       = 74;
  CmeDeleteWaterDemandFeature       = 75;

  CmeWizardNewReservoir             = 76;
  CmeWizardNewNodeWithInflow        = 77;
  CmeWizardNewChannel               = 78;
  CmeInvokeWizard                   = 79;
  CmeWizardRunYieldHistoric         = 80;
  CmeWizardRunYieldStochastic       = 81;
  CmeExportFilesAndRunModel         = 82;
  CmeExpertUser                     = 83;
  CmeStandardUser                   = 84;
  CmeValidateModelData              = 85;
  CmeWizardRunYieldYRC              = 86;

  CmeAddReport                      = 87;
  CmeDeleteReport                   = 88;
  CmeEditReport                     = 89;

  CmeChangeListCreate               = 90;
  CmeChangeListDelete               = 91;
  CmeChangeListCopy                 = 92;
  CmeChangeElementMoveUp            = 93;
  CmeChangeElementMoveDown          = 94;
  CmeChangeElementActivate          = 95;
  CmeChangeElementDeactivate        = 96;
  CmeChangeParameter                = 97;
  CmeMetaData                       = 98;
  CmeChangeListApply                = 99;

  CmeImportDemandFile               = 100;
  CmeImportHydrologyFile            = 102;
  CmeImportParamFile                = 103;
  CmeClearParamFile                 = 104;

  CmeReloadModelData                = 105;
  CmeImportDamLevelsFile            = 106;
  CmeUserAdministration             = 107;
  //CmeLicenceModels                  = 108;

  CmeCreateAllocDef                 = 109;
  CmeDeleteAllocDef                 = 110;
  CmeCreateSwitchDef                = 111;
  CmeDeleteSwitchDef                = 112;
  CmeCreateResTimeCntrl             = 113;
  CmeDeleteResTimeCntrl             = 114;
  CmeCreateResReplacement           = 115;
  CmeCreateChannelTimeCntrl         = 116;
  CmeDeleteChannelTimeCntrl         = 117;
  CmeCreateChannelSwitchCntrl       = 118;
  CmeDeleteChannelSwitchCntrl       = 119;

  CmeChangeGroupCreate              = 120;
  CmeChangeGroupDelete              = 121;
  CmeCreateDisbenefitFunction       = 122;
  CmeDeleteDisbenefitFunction       = 123;

  CmeCreateReturnFlowChannel        = 124;
  CmeDeleteReturnFlowChannel        = 125;

  CmeCreateIrrigationBlock          = 126;
  CmeDeleteIrrigationBlock          = 127;

  CmeCreateWetland                  = 128;
  CmeDeleteWetland                  = 129;

  CmeCreateYMDemandCentre           = 130;
  CmeDeleteYMDemandCentre           = 131;

  CmeCreateSFRSubCatchment          = 132;
  CmeDeleteSFRSubCatchment          = 133;
  CmeCreateReturnFlowFeature        = 134;
  CmeDeleteReturnFlowFeature        = 135;

  CmeChangeListImport               = 136;
  CmeChangeListExport               = 137;

  CmeCreateIFRSite                  = 138;
  CmeDeleteIFRSite                  = 139;
  CmeSaveIFRSite                    = 140;

  CmeGenerateSysConfigFiles         = 141;
  CmeCreateAnnualIFRFeature         = 142;
  CmeDeleteAnnualIFRFeature         = 143;

  CmeCreateMine                     = 144;
  CmeDeleteMine                     = 145;
  CmeCreateDroughtRestriction       = 147;
  CmeDeleteDroughtRestriction       = 148;
  CmeCopyReservoir                  = 149;
  CmeCreateGroundWater              = 151;
  CmeCopyGroundWater                = 152;
  CmeDeleteGroundWater              = 153;
  //Added by Karabo to be used
  CmeCreateMultiChannelCurtailmentRule = 154;
  CmeDeleteMultiChannelCurtailmentRule = 155;

  // BSP Model Specfics START at 256 (changed from 512)
  CmeBSPRestoreDatasetExplorer      = 250;
  CmeBSPHideClientPanel             = 251;
  CmeBSPShowClientPanel             = 252;
  CmeBSPContextMetadata             = 253;
  CmeBSPTimeSeriesComparator        = 254;
  CmeBSPSpatialAnalysis             = 255;
  CmeBSPApportionments              = 256;
  CmeBSPStakeholders                = 257;
  CmeBSPCategorisations             = 258;
  CmeBSPDeriveDatasets              = 259;
  CmeBSPFieldLists                  = 260;
  CmeBSPAreaManager                 = 261;
  CmeBSPAreaManagerAreaList         = 262;
  CmeBSPAreaManagerApportions       = 263;
  CmeBSPChangeLists                 = 264;
  CmeBSPDeriveFields                = 265;
  CmeBSPTSCRelatedDatasets          = 266;
  CmeBSPDerivedRelatedDatasets      = 267;
  CmeBSPImportDataset               = 268;
  CmeBSPImportApportionments        = 269;
  CmeBSPExportDataset               = 270;
  CmeBSPExportStakeholders          = 271;
  CmeBSPExportCategorisations       = 272;
  CmeBSPExportMetadata              = 273;
  CmeBSPCustomClassification        = 274;
  CmeBSPViewData                    = 275;
  CmeBSPRelatedDatasetsDataset      = 276;
  CmeBSPExportGridData              = 277;
  CmeBSPAggregateData               = 278;
  CmeBSPVerifyDataset               = 279;
  CmeBSPVerifyStreamFile            = 280;
  CmeBSPImportStakeholders          = 281;
  CmeBSPImportCategorisations       = 282;
  CmeBSPContextConfigureToolbar     = 283;
  CmeBSPUpdateToolbarButtons        = 284;
  CmeBSPDatasetCompare              = 285;
  CmeBSPDatasetTransferOut          = 286;
  CmeBSPDatasetTransferIn           = 287;
  CmeBSPCaptureDataset              = 288;
  CmeBSPDataDictionary              = 289;
  CmeBSPRefreshFieldListsInOutput   = 290;
  CmeBSPMultimediaFile		          = 291;
  CmeBSPToggleFullScreen            = 292;
  CmeBSPUpdateOutputViews           = 293;
  CmeBSPDisableButtons              = 294;
  CmeBSPEnableButtons               = 295;
  CmeBSPRefreshAreaHierarchyInOutput= 296;
  CmeBSPRefreshViewDefObjects       = 297;
  CmeBSPLoadViews                   = 298;
  CmeBSPDisableDatasetButtons       = 299;
  CmeBSPEnableDatasetButtons        = 230;
  CmeBSPRefreshGridData             = 300;
  CmeBSPAggregationDisaggregation   = 301;

  //Daily Diversion Pre-processor
  CmeExportMonthlyIFR                = 519;
  CmeExportDailyIFR                  = 520;
  CmeRenameDailyDiversion            = 521;
  CmeCreateDailyDiversion            = 522;
  CmeDeleteDailyDiversion            = 523;
  CmeImportDailyFlowDataFromCSVFile  = 524;
  CmeImportDailyInstreamFlowFile     = 525;
  CmeExportFlowDiversionRelationship = 526;

  CmeYRCDeleteChart                 = 527;

  CmeGenerateFlowDiversionRelation  = 528;
  CmeClearDailyFlowDataFromCSVFile  = 529;
  CmeClearDailyInstreamFlowFile     = 530;
  CmeClearFlowDiversionRelation     = 531;
  CmeGenerateWRYMData               = 532;
  CmeClearWRYMData                  = 533;

  CmeImportFile14                   = 536;
  CmeClearFile14                    = 537;
  CmeChangeListStationFilter        = 538;

  CmeStomsaFileNew                  = 539;
  CmeStomsaFileOpen                 = 540;
  CmeStomsaFileOpenParam            = 541;
  CmeStomsaFileSave                 = 542;
  CmeStomsaFileSaveAs               = 543;
  CmeStomsaFileSaveANS              = 544;
  CmeStomsaFileMerge                = 545;
  CmeStomsaFileClose                = 546;
  CmeStomsaFileExport               = 547;
  CmeStomsaFileImport               = 548;

  CmeCopyIFRFeature                 = 549;
  CmeCopyChannel                    = 550;
  CmeCopyPumpingFeature             = 551;
  CmeCopyPhysicalFlowConstraint     = 552;
  CmeCopyPowerPlant                 = 553;
  CmeCopyIrrigationArea             = 554;
  CmeCopyIrrigationBlock            = 555;

  CmeCopyWetland                    = 556;
  CmeCopyYMDemandCentre             = 557;
  CmeCopySFRSubCatchment            = 558;
  CmeCopyMine                       = 559;

  CmeStomsaSaveData                 = 560;
  CmeStomsaFilesAdded               = 601;
  CmeStomsaFilesRemoved             = 602;

  CmeCopyReservoirFromScenario          = 603;
  CmeCopyChannelFromScenario            = 605;
  CmeCopyIrrigationAreaFromScenario     = 606;
  CmeCopyIrrigationBlockFromScenario    = 607;
  CmeCopyPowerPlantFromScenario         = 608;
  CmeCopyWetlandFromScenario            = 609;
  CmeCopyYMDemandCentreFromScenario     = 610;
  CmeCopySFRSubCatchmentFromScenario    = 611;
  CmeCopyMineFromScenario               = 612;
  CmeCopyGroundWaterFromScenario        = 613;

  CmeRainIMSPatchROutput                = 614;
  CmeRainIMSClassROutput                = 615;
  CmeRainIMSSAWSNumbering               = 616;
  CmeRainIMSCLASSRAndPATCHRMethodology  = 617;

  CmeLoadHierarchyWizard                = 619;
  CmeBSPImportPOSDataset                = 620;
  CmePopulateAreaTree                   = 621;

  CmeBSPAddRecentDataset                = 622;
  CmeBSPSelectRecentDataset             = 623;

  CmeBSPAreaListWizard                  = 624;
  CmeBSPAreaTypeWizard                  = 625;
  CmeBSPApportionmentWizard             = 626;
  CmeBSPCovertDatasetWizard             = 627;
  CmeBSPDeriveDatasetWizard             = 628;
  CmeBSPLoadApportionmentWizard         = 629;
  CmeBSPCreateHierarchyWizard           = 630;

  CmeWRMFHelpContents                   = 900;
  CmeWRMFHelpReadMe                     = 901;
  CmeWRMFHelpReleaseNote                = 902;

  CmeOpenFile                           = 903;
  CmeExampleFile                        = 904;

  CmeHelpAbout                          = 905;
  CmeHelpWhatIsThis                     = 906;
  CmeSetHelpWhatIsThisOff               = 907;

  CmeWRMFReleaseNote                    = 908;
  CmeWRYMUserGuide                      = 909;
  CmeWRYMProceduralManual               = 910;
  CmeWRYMTrainingMaterial               = 911;
  CmeWRYMParameterTables                = 912;

  CmeWRPMReleaseNote                    = 913;
  CmeWRPMUserGuide                      = 914;
  CmeWRPMParameterTables                = 915;


  CmeHydrologyParameterTables           = 916;
  CmeSTOMSAUserGuide                    = 917;
  CmeIFRUserGuide                       = 918;
  CmeDailyDiversionUserGuide            = 919;


  CmeRainfallUserGuide                  = 920;
  CmeRainfallTrainingMaterial           = 921;
  CmeRainfallSummaryOfPatchROutput      = 922;
  CmeRainfallCLASSRAndPATCHRMethodology = 923;
  CmeRainfallSummaryOfClassROutput      = 924;
  CmeRainfallSAWSNumbering              = 925;
  CmeRWHUserGuide                       = 925;

  cmeCreateDDTSDam                      = 926;

  CmeCreateDamSedimentation             = 927;
  cmeDeleteDamSedimentation             = 928;
  cmeSaveDamSedimentation               = 929;
  CmeSediUserGuide                       = 930;


implementation

end.
