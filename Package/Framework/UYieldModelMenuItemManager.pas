//
//
//  UNIT      : Contains TYieldModelMenuItemManager Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/04
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYieldModelMenuItemManager;

interface

uses
  UAbstractObject,
  UAbstractComponent,
  UMenuItemManager,
  UYieldModelToolBar,
  UYieldModelLinkClasses,
//  UAppModulesConstructionGeneric,
  UDataModelMenuItemManager;

  type
  TYieldModelMenuItemManager = class(TDataModelMenuItemManager)
  protected
    FToolBar: TYieldModelToolBar;
    FIsResultsLoaded: boolean;
    procedure CreateToolBar; override;
    procedure DestroyMemberObjects; override;

    procedure SetCreateReservoir(AAction: TMenuSetAction);
    procedure SetDeleteReservoir(AAction: TMenuSetAction);
    procedure SetCopyReservoir(AAction: TMenuSetAction);
    procedure SetScenarioCopyReservoir(AAction: TMenuSetAction);

    procedure SetCreateNodeWithInflow(AAction: TMenuSetAction);
    procedure SetDeleteNodeWithInflow(AAction: TMenuSetAction);

    procedure SetCreateNodeWithoutInflow(AAction: TMenuSetAction);
    procedure SetDeleteNodeWithoutInflow(AAction: TMenuSetAction);

    procedure SetCreateChannel(AAction: TMenuSetAction);
    procedure SetDeleteChannel(AAction: TMenuSetAction);
    procedure SetConvertChannel(AAction: TMenuSetAction);
    procedure SetCopyChannel(AAction: TMenuSetAction);
    procedure SetScenarioCopyChannel(AAction: TMenuSetAction);

    procedure SetScenarioCopyIrrigationArea(AAction: TMenuSetAction);
    procedure SetScenarioCopyIrrigationBlock(AAction: TMenuSetAction);
    procedure SetScenarioCopyPowerPlant(AAction: TMenuSetAction);
    procedure SetScenarioCopyWetland(AAction: TMenuSetAction);
    procedure SetScenarioCopyYMDemandCentre(AAction: TMenuSetAction);
    procedure SetScenarioCopySFRSubCatchment(AAction: TMenuSetAction);
    procedure SetScenarioCopyMine(AAction: TMenuSetAction);
    procedure SetScenarioCopyGroundWater(AAction: TMenuSetAction);

    procedure SetCreateFeature(AAction: TMenuSetAction);

    procedure SetCreatePumpingFeature(AAction: TMenuSetAction);
    procedure SetCopyPumpingFeature(AAction: TMenuSetAction);
    procedure SetDeletePumpingFeature(AAction: TMenuSetAction);

    procedure SetCreateYMDemandCentreReturnFlowFeature(AAction: TMenuSetAction);
    procedure SetDeleteYMDemandCentreReturnFlowFeature(AAction: TMenuSetAction);

    procedure SetCreateSpecifiedInflowFeature(AAction: TMenuSetAction);

    procedure SetCreateIFRFeature(AAction: TMenuSetAction);
    procedure SetCopyIFRFeature(AAction: TMenuSetAction);
    procedure SetDeleteIFRFeature(AAction: TMenuSetAction);

    procedure SetCreatePhysicalFlowConstraint(AAction: TMenuSetAction);
    procedure SetCopyPhysicalFlowConstraint(AAction: TMenuSetAction);
    procedure SetDeletePhysicalFlowConstraint(AAction: TMenuSetAction);

    procedure SetCreateIrrigationArea(AAction: TMenuSetAction);
    procedure SetCopyIrrigationArea(AAction: TMenuSetAction);
    procedure SetDeleteIrrigationArea(AAction: TMenuSetAction);

    procedure SetCreateIrrigationBlock(AAction: TMenuSetAction);
    procedure SetCopyIrrigationBlock(AAction: TMenuSetAction);
    procedure SetDeleteIrrigationBlock(AAction: TMenuSetAction);

    procedure SetCreatePowerPlant(AAction: TMenuSetAction);
    procedure SetCopyPowerPlant(AAction: TMenuSetAction);
    procedure SetDeletePowerPlant(AAction: TMenuSetAction);

    procedure SetCreateMasterControlFeature(AAction: TMenuSetAction);
    procedure SetDeleteMasterControlFeature(AAction: TMenuSetAction);

    procedure SetCreateWaterDemandFeature(AAction: TMenuSetAction);
    procedure SetDeleteWaterDemandFeature(AAction: TMenuSetAction);

    procedure SetCreateWetland(AAction: TMenuSetAction);
    procedure SetCopyWetland(AAction: TMenuSetAction);
    procedure SetDeleteWetland(AAction: TMenuSetAction);

    procedure SetCreateYMDemandCentre(AAction: TMenuSetAction);
    procedure SetCopyYMDemandCentre(AAction: TMenuSetAction);
    procedure SetDeleteYMDemandCentre(AAction: TMenuSetAction);

    procedure SetCreateDroughtRestriction(AAction: TMenuSetAction);
    procedure SetDeleteDroughtRestriction(AAction: TMenuSetAction);

    procedure SetCreateSFRSubCatchment(AAction: TMenuSetAction);
    procedure SetCopySFRSubCatchment(AAction: TMenuSetAction);
    procedure SetDeleteSFRSubCatchment(AAction: TMenuSetAction);

    procedure SetCreateMine(AAction: TMenuSetAction);
    procedure SetCopyMine(AAction: TMenuSetAction);
    procedure SetDeleteMine(AAction: TMenuSetAction);

    procedure SetCreateWizardNewReservoir(AAction: TMenuSetAction);
    procedure SetWizardNewNodeWithInflow(AAction: TMenuSetAction);
    procedure SetWizardNewChannel(AAction: TMenuSetAction);
    procedure SetInvokeWizard(AAction: TMenuSetAction);
    procedure SetWizardRunYield(AAction: TMenuSetAction);

    //PLanning
    procedure SetCreateAllocDef(AAction: TMenuSetAction);
    procedure SetDeleteAllocDef(AAction: TMenuSetAction);
    procedure SetCreateSwitchDef(AAction: TMenuSetAction);
    procedure SetDeleteSwitchDef(AAction: TMenuSetAction);
    procedure SetCreateResTimeCntrl(AAction: TMenuSetAction);
    procedure SetDeleteResTimeCntrl(AAction: TMenuSetAction);
    procedure SetCreateResReplacement(AAction: TMenuSetAction);
    procedure SetCreateChannelTimeCntrl(AAction: TMenuSetAction);
    procedure SetDeleteChannelTimeCntrl(AAction: TMenuSetAction);
    procedure SetCreateChannelSwitchCntrl(AAction: TMenuSetAction);
    procedure SetDeleteChannelSwitchCntrl(AAction: TMenuSetAction);
    procedure SetCreateDisbenefitFunction(AAction: TMenuSetAction);
    procedure SetDeleteDisbenefitFunction(AAction: TMenuSetAction);
    procedure SetCreateReturnFlowChannel(AAction: TMenuSetAction);
    procedure SetDeleteReturnFlowChannel(AAction: TMenuSetAction);
    //Added by Karabo
    procedure SetCreateMultiChannelCurtailRule(AAction: TMenuSetAction);
    procedure SetDeleteMultiChannelCurtailRule(AAction: TMenuSetAction);

  public
    constructor Create(AAppModules: TAppModules; AIsNetworkLoaded, AIsGridLoaded, AIsGraphLoaded,
                AIsFileSelectionLoaded, AIsResultsLoaded: boolean); reintroduce;

    function Initialise: Boolean; override;
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;

    procedure AddMenuItems; override;
    procedure TabHasChanged (AGridTabSelected: boolean);
    procedure TreeNodeHasChanged (ADataType  : string;  AElementID : integer);

    procedure SetMenuValidateFiles(AAction: TMenuSetAction);
    procedure SetMenuImportFiles(AAction: TMenuSetAction);
    procedure SetMenuExportFiles(AAction: TMenuSetAction);
    procedure SetMenuClearModelData(AAction: TMenuSetAction);
    procedure SetMenuValidateModelData(AAction: TMenuSetAction);
    procedure SetMenuRunModel(AAction: TMenuSetAction);
    procedure SetMenuGenerateSysConfigFiles(AAction: TMenuSetAction);
    procedure SetCreateGroundWater(AAction: TMenuSetAction);
    procedure SetCopyGroundWater(AAction: TMenuSetAction);
    procedure SetDeleteGroundWater(AAction: TMenuSetAction);

    procedure SetExpertUserChecked(AChecked: boolean);
    procedure SetStandardUserChecked(AChecked: boolean);

    property ToolBar: TYieldModelToolBar read FToolBar;
  end;

implementation

uses
  SysUtils,
  UConstants,
  VoaimsCom_TLB,
  UMainMenuEventType,
  UYieldModelDataObject,
  UErrorHandlingOperations;

const
  CModel                        : array[0..0] of WideString = ('Model');
  CValidateFiles                : array[0..1] of WideString = ('Model','ValidateFiles');
  CExportFiles                  : array[0..1] of WideString = ('Model','ExportFiles');
  CImportFiles                  : array[0..1] of WideString = ('Model','ImportFiles');
  CClearModelData               : array[0..1] of WideString = ('Model','ClearModelData');
  CValidateModelData            : array[0..1] of WideString = ('Model','ValidateModelData');
  CGenerateSysConfigFiles       : array[0..1] of WideString = ('Model','GenerateSystemConfigDataFiles');
  CRunModel                     : array[0..1] of WideString = ('Model','RunModel');

  CScenarioCopy                 : array[0..1] of WideString = ('Data','ScenarioCopy');
  CScenarioCopyReservoir        : array[0..2] of WideString = ('Data','ScenarioCopy','ScenarioCopyReservoir');
  CScenarioCopyChannel          : array[0..2] of WideString = ('Data','ScenarioCopy','ScenarioCopyChannel');

  CScenarioCopyNetworkFeature   : array[0..2] of WideString = ('Data','ScenarioCopy','ScenarioCopyNetworkFeature');
  CScenarioCopyIrrigationArea   : array[0..3] of WideString = ('Data','ScenarioCopy','ScenarioCopyNetworkFeature','ScenarioCopyIrrigationArea');
  CScenarioCopyIrrigationBlock  : array[0..3] of WideString = ('Data','ScenarioCopy','ScenarioCopyNetworkFeature','ScenarioCopyIrrigationBlock');
  CScenarioCopyPowerPlant       : array[0..3] of WideString = ('Data','ScenarioCopy','ScenarioCopyNetworkFeature','ScenarioCopyPowerPlant');
  CScenarioCopyWetland          : array[0..3] of WideString = ('Data','ScenarioCopy','ScenarioCopyNetworkFeature','ScenarioCopyWetland');
  CScenarioCopyYMDemandCentre   : array[0..3] of WideString = ('Data','ScenarioCopy','ScenarioCopyNetworkFeature','ScenarioCopyYMDemandCentre');
  CScenarioCopySFRSubCatchment  : array[0..3] of WideString = ('Data','ScenarioCopy','ScenarioCopyNetworkFeature','ScenarioCopySFRSubCatchment');
  CScenarioCopyMine             : array[0..3] of WideString = ('Data','ScenarioCopy','ScenarioCopyNetworkFeature','ScenarioCopyMine');
  CScenarioCopyGroundWater      : array[0..3] of WideString = ('Data','ScenarioCopy','ScenarioCopyNetworkFeature','ScenarioCopyGroundWater');

  CReservoirs                   : array[0..1] of WideString = ('Data','Reservoirs');
  CCreateReservoir              : array[0..2] of WideString = ('Data','Reservoirs','CreateReservoir');
  CDeleteReservoir              : array[0..2] of WideString = ('Data','Reservoirs','DeleteReservoir');
  CCopyReservoir                : array[0..2] of WideString = ('Data','Reservoirs','CopyReservoir');
  CCreateResReplacement         : array[0..2] of WideString = ('Data','Reservoirs','CreateResReplacement');

  CNodes                        : array[0..1] of WideString = ('Data','Nodes');
  CCreateNodeWithInflow         : array[0..2] of WideString = ('Data','Nodes','CreateNodeWithInflow');
  CDeleteNodeWithInflow         : array[0..2] of WideString = ('Data','Nodes','DeleteNodeWithInflow');
  CCreateNodeWithoutInflow      : array[0..2] of WideString = ('Data','Nodes','CreateNodeWithoutInflow');
  CDeleteNodeWithoutInflow      : array[0..2] of WideString = ('Data','Nodes','DeleteNodeWithoutInflow');

  CChannels                     : array[0..1] of WideString = ('Data','Channels');
  CCreateChannel                : array[0..2] of WideString = ('Data','Channels','CreateChannel');
  CCopyChannel                  : array[0..2] of WideString = ('Data','Channels','CopyChannel');
  CDeleteChannel                : array[0..2] of WideString = ('Data','Channels','DeleteChannel');
  CConvertChannel               : array[0..2] of WideString = ('Data','Channels','ConvertChannel');

  CChannelFeatures              : array[0..1] of WideString = ('Data','ChannelFeatures');
  CCreateMinimumFlowFeature     : array[0..2] of WideString = ('Data','ChannelFeatures','CreateMinimumFlowFeature');
  CCreateMinMaxFlowFeature      : array[0..2] of WideString = ('Data','ChannelFeatures','CreateMinMaxFlowFeature');
  CCreatePumpingFeature         : array[0..2] of WideString = ('Data','ChannelFeatures','CreatePumpingFeature');
  CCopyPumpingFeature           : array[0..2] of WideString = ('Data','ChannelFeatures','CopyPumpingFeature');
  CDeletePumpingFeature         : array[0..2] of WideString = ('Data','ChannelFeatures','DeletePumpingFeature');
  CCreateYMDemandCentreReturnFlowFeature  : array[0..2] of WideString = ('Data','ChannelFeatures','CreateYMDemandCentreReturnFlowFeature');
  CDeleteYMDemandCentreReturnFlowFeature  : array[0..2] of WideString = ('Data','ChannelFeatures','DeleteYMDemandCentreReturnFlowFeature');
  CCreateLossFeature            : array[0..2] of WideString = ('Data','ChannelFeatures','CreateLossFeature');
  CCreateSpecifiedDemandFeature : array[0..2] of WideString = ('Data','ChannelFeatures','CreateSpecifiedDemandFeature');
  CCreateDiversionFeature       : array[0..2] of WideString = ('Data','ChannelFeatures','CreateDiversionFeature');
  CCreateSpecifiedInflowFeature : array[0..2] of WideString = ('Data','ChannelFeatures','CreateSpecifiedInflowFeature');
  CCreateIFRFeature             : array[0..2] of WideString = ('Data','ChannelFeatures','CreateIFRFeature');
  CCopyIFRFeature               : array[0..2] of WideString = ('Data','ChannelFeatures','CopyIFRFeature');
  CDeleteIFRFeature             : array[0..2] of WideString = ('Data','ChannelFeatures','DeleteIFRFeature');

  CCreatePhysicalFlowConstraint : array[0..2] of WideString = ('Data','ChannelFeatures','CreatePhysicalFlowConstraint');
  CCopyPhysicalFlowConstraint   : array[0..2] of WideString = ('Data','ChannelFeatures','CopyPhysicalFlowConstraint');
  CDeletePhysicalFlowConstraint : array[0..2] of WideString = ('Data','ChannelFeatures','DeletePhysicalFlowConstraint');

  CDroughtRestriction           : array[0..1] of WideString = ('Data','DroughtRestriction');
  CCreateDroughtRestriction     : array[0..2] of WideString = ('Data','DroughtRestriction','CreateDroughtRestriction');
  CDeleteDroughtRestriction     : array[0..2] of WideString = ('Data','DroughtRestriction','DeleteDroughtRestriction');

  CNetworkFeatures              : array[0..1] of WideString = ('Data','NetworkFeatures');
  CCreatePowerPlant             : array[0..2] of WideString = ('Data','NetworkFeatures','CreatePowerPlant');
  CCopyPowerPlant               : array[0..2] of WideString = ('Data','NetworkFeatures','CopyPowerPlant');
  CDeletePowerPlant             : array[0..2] of WideString = ('Data','NetworkFeatures','DeletePowerPlant');

  CCreateIrrigationArea         : array[0..2] of WideString = ('Data','NetworkFeatures','CreateIrrigationArea');
  CCopyIrrigationArea           : array[0..2] of WideString = ('Data','NetworkFeatures','CopyIrrigationArea');
  CDeleteIrrigationArea         : array[0..2] of WideString = ('Data','NetworkFeatures','DeleteIrrigationArea');

  CCreateIrrigationBlock        : array[0..2] of WideString = ('Data','NetworkFeatures','CreateIrrigationBlock');
  CCopyIrrigationBlock          : array[0..2] of WideString = ('Data','NetworkFeatures','CopyIrrigationBlock');
  CDeleteIrrigationBlock        : array[0..2] of WideString = ('Data','NetworkFeatures','DeleteIrrigationBlock');

  CCreateWetland                : array[0..2] of WideString = ('Data','NetworkFeatures','CreateWetland');
  CCopyWetland                  : array[0..2] of WideString = ('Data','NetworkFeatures','CopyWetland');
  CDeleteWetland                : array[0..2] of WideString = ('Data','NetworkFeatures','DeleteWetland');

  CCreateSFRSubCatchment        : array[0..2] of WideString = ('Data','NetworkFeatures','CreateSFRSubCatchment');
  CCopySFRSubCatchment          : array[0..2] of WideString = ('Data','NetworkFeatures','CopySFRSubCatchment');
  CDeleteSFRSubCatchment        : array[0..2] of WideString = ('Data','NetworkFeatures','DeleteSFRSubCatchment');

  CCreateYMDemandCentre         : array[0..2] of WideString = ('Data','NetworkFeatures','CreateYMDemandCentre');
  CCopyYMDemandCentre           : array[0..2] of WideString = ('Data','NetworkFeatures','CopyYMDemandCentre');
  CDeleteYMDemandCentre         : array[0..2] of WideString = ('Data','NetworkFeatures','DeleteYMDemandCentre');

  CCreateMine                   : array[0..2] of WideString = ('Data','NetworkFeatures','CreateMine');
  CCopyMine                     : array[0..2] of WideString = ('Data','NetworkFeatures','CopyMine');
  CDeleteMine                   : array[0..2] of WideString = ('Data','NetworkFeatures','DeleteMine');

  CCreateGroundWater            : array[0..2] of WideString = ('Data','NetworkFeatures','CreateGroundWater');
  CCopyGroundWater              : array[0..2] of WideString = ('Data','NetworkFeatures','CopyGroundWater');
  CDeleteGroundWater            : array[0..2] of WideString = ('Data','NetworkFeatures','DeleteGroundWater');

  CCreateMasterControlFeature   : array[0..2] of WideString = ('Data','ChannelFeatures','CreateMasterControlFeature');
  CDeleteMasterControlFeature   : array[0..2] of WideString = ('Data','ChannelFeatures','DeleteMasterControlFeature');
  CCreateWaterDemandFeature     : array[0..2] of WideString = ('Data','ChannelFeatures','CreateWaterDemandFeature');
  CDeleteWaterDemandFeature     : array[0..2] of WideString = ('Data','ChannelFeatures','DeleteWaterDemandFeature');

  CWizards                      : array[0..0] of WideString = ('Wizards');
  CWizardNewReservoir           : array[0..1] of WideString = ('Wizards','CreateReservoir');
  CWizardNewNodeWithInflow      : array[0..1] of WideString = ('Wizards','CreateNodeWithInflow');
  CWizardNewChannel             : array[0..1] of WideString = ('Wizards','CreateChannel');
  CWizardSeparator1             : array[0..1] of WideString = ('Wizards','WizardSeparator1');
  CWizardInvoke                 : array[0..1] of WideString = ('Wizards','InvokeWizard');
  CWizardSeparator2             : array[0..1] of WideString = ('Wizards','WizardSeparator2');
  CWizardRunYieldHistoric       : array[0..1] of WideString = ('Wizards','RunYieldHistoric');
  CWizardRunYieldStochastic     : array[0..1] of WideString = ('Wizards','RunYieldStochastic');
  CWizardRunYieldYRC            : array[0..1] of WideString = ('Wizards','RunYieldYRC');


//  CPlanningStudy                : array[0..1] of WideString = ('Data','Planning');

  CAllocationControl            : array[0..1] of WideString = ('Data','AllocationControl');
  CCreateAllocDef               : array[0..2] of WideString = ('Data','AllocationControl','CreateAllocDef');
  CDeleteAllocDef               : array[0..2] of WideString = ('Data','AllocationControl','DeleteAllocDef');

  CSwitchControl                : array[0..1] of WideString = ('Data','SwitchControl');
  CCreateSwitchDef              : array[0..2] of WideString = ('Data','SwitchControl','CreateSwitchDef');
  CDeleteSwitchDef              : array[0..2] of WideString = ('Data','SwitchControl','DeleteSwitchDef');
  CCreateChannelSwitchCntrl     : array[0..2] of WideString = ('Data','SwitchControl','CreateChannelSwitchCntrl');
  CDeleteChannelSwitchCntrl     : array[0..2] of WideString = ('Data','SwitchControl','DeleteChannelSwitchCntrl');

  CTimeControl                  : array[0..1] of WideString = ('Data','TimeControl');
  CCreateResTimeCntrl           : array[0..2] of WideString = ('Data','TimeControl','CreateResTimeCntrl');
  CDeleteResTimeCntrl           : array[0..2] of WideString = ('Data','TimeControl','DeleteResTimeCntrl');
  CCreateChannelTimeCntrl       : array[0..2] of WideString = ('Data','TimeControl','CreateChannelTimeCntrl');
  CDeleteChannelTimeCntrl       : array[0..2] of WideString = ('Data','TimeControl','DeleteChannelTimeCntrl');


  CDisbenefitFunction           : array[0..1] of WideString = ('Data','DisbenefitFunction');
  CCreateDisbenefitFunction     : array[0..2] of WideString = ('Data','DisbenefitFunction','CreateDisbenefitFunction');
  CDeleteDisbenefitFunction     : array[0..2] of WideString = ('Data','DisbenefitFunction','DeleteDisbenefitFunction');

  CCreateReturnFlowChannel      : array[0..2] of WideString = ('Data','Channels','CreateReturnFlowChannel');
  CDeleteReturnFlowChannel      : array[0..2] of WideString = ('Data','Channels','DeleteReturnFlowChannel');
  //Added by Karabo
  CCreateMultiChannelCurtailRule  : array[0..2] of WideString = ('Data','ChannelFeatures','CreateMultiChannelCurtailRestriction');
  CDeleteMultiChannelCurtailRule  : array[0..2] of WideString = ('Data','ChannelFeatures','DeleteMultiChannelCurtailRestriction');

  CViewStandardUser             : array[0..1] of WideString = ('View','ViewStandardUser');
  CViewExpertUser               : array[0..1] of WideString = ('View','ViewExpertUser');
  CViewModeSep                  : array[0..1] of WideString = ('View','ViewModeSep');

  CViewResults                  : array[0..1] of WideString = ('View','ViewResults');
  CViewTabSheetsSep             : array[0..1] of WideString = ('View','ViewTabSheetsSep1');

  CHelpWRMFReleaseNote          : array[0..1] of WideString = ('Help','HelpWRMFReleaseNote');
  CHelpWRYMUserGuide            : array[0..1] of WideString = ('Help','HelpWRYMUserGuide');
  CHelpWRYMProceduralManual     : array[0..1] of WideString = ('Help','HelpWRYMProceduralManual');
  CHelpWRYMTrainingMaterial     : array[0..1] of WideString = ('Help','HelpWRYMTrainingMaterial');
  CHelpWRYMParameterTables      : array[0..1] of WideString = ('Help','HelpWRYMParameterTables');

  CHelpWRPMReleaseNote          : array[0..1] of WideString = ('Help','HelpWRPMReleaseNote');
  CHelpWRPMUserGuide            : array[0..1] of WideString = ('Help','HelpWRPMUserGuide');
  CHelpWRPMParameterTables      : array[0..1] of WideString = ('Help','HelpWRPMParameterTables');

  CHelpHydrologyParameterTables : array[0..1] of WideString = ('Help','HelpHydrologyParameterTables');

{ TYieldModelMenuItemManager }

constructor TYieldModelMenuItemManager.Create(AAppModules: TAppModules;
  AIsNetworkLoaded, AIsGridLoaded, AIsGraphLoaded, AIsFileSelectionLoaded, AIsResultsLoaded: boolean);
const OPNAME = 'TYieldModelMenuItemManager.Create';
begin
  try
    FIsResultsLoaded := AIsResultsLoaded;
    inherited Create(AAppModules, AIsNetworkLoaded, AIsGridLoaded, AIsGraphLoaded, AIsFileSelectionLoaded);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.AddMenuItems;
const OPNAME = 'TYieldModelMenuItemManager.AddMenuItems';
begin
  try
    DeleteMenuItems;
    inherited AddMenuItems;

    AddMenuItemEntry(CModel,            600);
    AddMenuItemEntry(CValidateFiles,    610, CmeValidateFiles,nil);
    AddMenuItemEntry(CImportFiles,      620, CmeImportFiles,nil);
    AddMenuItemEntry(CExportFiles,      630, CmeExportFiles,nil);
    AddMenuItemEntry(CClearModelData,   640, CmeClearModelData,nil);
    AddMenuItemEntry(CValidateModelData,650, CmeValidateModelData);
    AddMenuItemEntry(CRunModel,         655, CmeRunModel);
    AddMenuItemEntry(CGenerateSysConfigFiles,  656, CmeGenerateSysConfigFiles);

    AddMenuItemEntry(CViewStandardUser, 660, CmeStandardUser);
    AddMenuItemEntry(CViewExpertUser,   670, CmeExpertUser);
    AddMenuItemEntry(CViewTabSheetsSep, 680, CmeSeparator);

    if FIsResultsLoaded then
      AddMenuItemEntry(CViewResults, 240, CmeViewResultGraphs);

    AddMenuItemEntry(CReservoirs,                   195);
    AddMenuItemEntry(CCreateReservoir,              200, CmeCreateReservoir, nil);
    AddMenuItemEntry(CDeleteReservoir,              220, CmeDeleteReservoir, nil);
    AddMenuItemEntry(CCopyReservoir,                225, CmeCopyReservoir, nil);

    AddMenuItemEntry(CNodes,                        235);
    AddMenuItemEntry(CCreateNodeWithInflow,         240, CmeCreateNodeWithInflow, nil);
    AddMenuItemEntry(CDeleteNodeWithInflow,         260, CmeDeleteNodeWithInflow, nil);
    AddMenuItemEntry(CCreateNodeWithoutInflow,      280, CmeCreateNodeWithoutInflow, nil);
    AddMenuItemEntry(CDeleteNodeWithoutInflow,      300, CmeDeleteNodeWithoutInflow, nil);

    AddMenuItemEntry(CChannels,                     315);
    AddMenuItemEntry(CCreateChannel,                320, CmeCreateChannel, nil);
    AddMenuItemEntry(CCopyChannel,                  320, CmeCopyChannel, nil);
    AddMenuItemEntry(CDeleteChannel,                340, CmeDeleteChannel, nil);
    AddMenuItemEntry(CConvertChannel,               360, CmeConvertChannel, nil);

    AddMenuItemEntry(CDroughtRestriction,           361);
    AddMenuItemEntry(CCreateDroughtRestriction,     362, CmeCreateDroughtRestriction, nil);
    AddMenuItemEntry(CDeleteDroughtRestriction,     363, CmeDeleteDroughtRestriction, nil);

    AddMenuItemEntry(CChannelFeatures,              375);
    AddMenuItemEntry(CCreateMasterControlFeature,   376, CmeCreateMasterControlFeature, nil);
    AddMenuItemEntry(CDeleteMasterControlFeature,   377, CmeDeleteMasterControlFeature, nil);
    AddMenuItemEntry(CCreateDiversionFeature,       378, CmeCreateDiversionFeature, nil);
    AddMenuItemEntry(CCreateMinimumFlowFeature,     379, CmeCreateMinimumFlowFeature, nil);
    AddMenuItemEntry(CCreateLossFeature,            380, CmeCreateLossFeature, nil);
    AddMenuItemEntry(CCreateMinMaxFlowFeature,      381, CmeCreateMinMaxFlowFeature, nil);
    AddMenuItemEntry(CCreatePumpingFeature,         382, CmeCreatePumpingFeature, nil);
    AddMenuItemEntry(CCopyPumpingFeature,           383, CmeCopyPumpingFeature, nil);
    AddMenuItemEntry(CDeletePumpingFeature,         384, CmeDeletePumpingFeature, nil);
    AddMenuItemEntry(CCreateSpecifiedInflowFeature, 385, CmeCreateSpecifiedInflowFeature, nil);
    AddMenuItemEntry(CCreateSpecifiedDemandFeature, 386, CmeCreateSpecifiedDemandFeature, nil);
    AddMenuItemEntry(CCreateIFRFeature,             387, CmeCreateIFRFeature, nil);
    AddMenuItemEntry(CCopyIFRFeature,               388, CmeCopyIFRFeature, nil);
    AddMenuItemEntry(CDeleteIFRFeature,             389, CmeDeleteIFRFeature, nil);
    AddMenuItemEntry(CCreateWaterDemandFeature,     390, CmeCreateWaterDemandFeature, nil);
    AddMenuItemEntry(CDeleteWaterDemandFeature,     391, CmeDeleteWaterDemandFeature, nil);
    AddMenuItemEntry(CCreatePhysicalFlowConstraint, 392, CmeCreatePhysicalFlowConstraint, nil);
    AddMenuItemEntry(CCopyPhysicalFlowConstraint,   393, CmeCopyPhysicalFlowConstraint, nil);
    AddMenuItemEntry(CDeletePhysicalFlowConstraint, 394, CmeDeletePhysicalFlowConstraint, nil);

    AddMenuItemEntry(CNetworkFeatures,              610);

    AddMenuItemEntry(CCreatePowerPlant,             620, CmeCreatePowerPlant, nil);
    AddMenuItemEntry(CCopyPowerPlant,               630, CmeCopyPowerPlant, nil);
    AddMenuItemEntry(CDeletePowerPlant,             640, CmeDeletePowerPlant, nil);

    AddMenuItemEntry(CCreateIrrigationArea,         660, CmeCreateIrrigationArea, nil);
    AddMenuItemEntry(CCopyIrrigationArea,           670, CmeCopyIrrigationArea, nil);
    AddMenuItemEntry(CDeleteIrrigationArea,         680, CmeDeleteIrrigationArea, nil);

    AddMenuItemEntry(CCreateIrrigationBlock,        780, CmeCreateIrrigationBlock, nil);
    AddMenuItemEntry(CCopyIrrigationBlock,          790, CmeCopyIrrigationBlock, nil);
    AddMenuItemEntry(CDeleteIrrigationBlock,        800, CmeDeleteIrrigationBlock, nil);

    AddMenuItemEntry(CCreateWetland,                820, CmeCreateWetland, nil);
    AddMenuItemEntry(CCopyWetland,                  830, CmeCopyWetland, nil);
    AddMenuItemEntry(CDeleteWetland,                840, CmeDeleteWetland, nil);

    AddMenuItemEntry(CCreateSFRSubCatchment,        860, CmeCreateSFRSubCatchment, nil);
    AddMenuItemEntry(CCopySFRSubCatchment,          870, CmeCopySFRSubCatchment, nil);
    AddMenuItemEntry(CDeleteSFRSubCatchment,        880, CmeDeleteSFRSubCatchment, nil);

    AddMenuItemEntry(CCreateYMDemandCentre,         890, CmeCreateYMDemandCentre, nil);
    AddMenuItemEntry(CCopyYMDemandCentre,           892, CmeCopyYMDemandCentre, nil);
    AddMenuItemEntry(CDeleteYMDemandCentre,         895, CmeDeleteYMDemandCentre, nil);

    AddMenuItemEntry(CCreateMine,                   900, CmeCreateMine, nil);
    AddMenuItemEntry(CCopyMine,                     901, CmeCopyMine, nil);
    AddMenuItemEntry(CDeleteMine,                   905, CmeDeleteMine, nil);

    AddMenuItemEntry(CScenarioCopy,                 950);
    AddMenuItemEntry(CScenarioCopyReservoir,        951, CmeCopyReservoirFromScenario, nil);
    AddMenuItemEntry(CScenarioCopyChannel,          952, CmeCopyChannelFromScenario, nil);

    AddMenuItemEntry(CScenarioCopyNetworkFeature,   953);
    AddMenuItemEntry(CScenarioCopyIrrigationArea,   954, CmeCopyIrrigationAreaFromScenario,nil);
    AddMenuItemEntry(CScenarioCopyIrrigationBlock,  955, CmeCopyIrrigationBlockFromScenario,nil);
    AddMenuItemEntry(CScenarioCopyPowerPlant,       956, CmeCopyPowerPlantFromScenario,nil);
    AddMenuItemEntry(CScenarioCopyWetland,          957, CmeCopyWetlandFromScenario,nil);
    AddMenuItemEntry(CScenarioCopyYMDemandCentre,   958, CmeCopyYMDemandCentreFromScenario,nil);
    AddMenuItemEntry(CScenarioCopySFRSubCatchment,  959, CmeCopySFRSubCatchmentFromScenario,nil);
    AddMenuItemEntry(CScenarioCopyMine,             960, CmeCopyMineFromScenario,nil);
    AddMenuItemEntry(CScenarioCopyGroundWater,      961, CmeCopyGroundWaterFromScenario,nil);

    AddMenuItemEntry(CCreateGroundWater,            962, CmeCreateGroundWater, nil);
    AddMenuItemEntry(CCopyGroundWater,              963, CmeCopyGroundWater, nil);
    AddMenuItemEntry(CDeleteGroundWater,            964, CmeDeleteGroundWater, nil);

    AddMenuItemEntry(CWizards,  850);
    AddMenuItemEntry(CWizardNewReservoir,       10, CmeWizardNewReservoir, nil);
    AddMenuItemEntry(CWizardNewNodeWithInflow,  20, CmeWizardNewNodeWithInflow, nil);
    AddMenuItemEntry(CWizardNewChannel,         30, CmeWizardNewChannel, nil);
    AddMenuItemEntry(CWizardSeparator1,         40, CmeSeparator);
    AddMenuItemEntry(CWizardInvoke,             50, CmeInvokeWizard);
    AddMenuItemEntry(CWizardSeparator2,         60, CmeSeparator);
    AddMenuItemEntry(CWizardRunYieldHistoric,   70, CmeWizardRunYieldHistoric);
    AddMenuItemEntry(CWizardRunYieldStochastic, 80, CmeWizardRunYieldStochastic);
    AddMenuItemEntry(CWizardRunYieldYRC,        90, CmeWizardRunYieldYRC);

    //PLanning
    if (FAppModules.StudyArea.ModelSubCode = CPlanning) then
    begin
      //Added by Karabo
      AddMenuItemEntry(CCreateMultiChannelCurtailRule, 395,CmeCreateMultiChannelCurtailmentRule);
      AddMenuItemEntry(CDeleteMultiChannelCurtailRule, 396,CmeDeleteMultiChannelCurtailmentRule);
      AddMenuItemEntry(CAllocationControl,        795);
      AddMenuItemEntry(CDisbenefitFunction,       796);
      AddMenuItemEntry(CTimeControl,              797);
      AddMenuItemEntry(CSwitchControl,            798);
      AddMenuItemEntry(CCreateAllocDef,           800, CmeCreateAllocDef, nil);
      AddMenuItemEntry(CDeleteAllocDef,           810, CmeDeleteAllocDef, nil);
      AddMenuItemEntry(CCreateSwitchDef,          820, CmeCreateSwitchDef, nil);
      AddMenuItemEntry(CDeleteSwitchDef,          830, CmeDeleteSwitchDef, nil);
      AddMenuItemEntry(CCreateResTimeCntrl,       840, CmeCreateResTimeCntrl, nil);
      AddMenuItemEntry(CDeleteResTimeCntrl,       850, CmeDeleteResTimeCntrl, nil);
      AddMenuItemEntry(CCreateResReplacement,     860, CmeCreateResReplacement, nil);
      AddMenuItemEntry(CCreateChannelTimeCntrl,   870, CmeCreateChannelTimeCntrl, nil);
      AddMenuItemEntry(CDeleteChannelTimeCntrl,   880, CmeDeleteChannelTimeCntrl, nil);
      AddMenuItemEntry(CCreateChannelSwitchCntrl, 890, CmeCreateChannelSwitchCntrl, nil);
      AddMenuItemEntry(CDeleteChannelSwitchCntrl, 900, CmeDeleteChannelSwitchCntrl, nil);
      AddMenuItemEntry(CCreateDisbenefitFunction, 910, CmeCreateDisbenefitFunction, nil);
      AddMenuItemEntry(CDeleteDisbenefitFunction, 920, CmeDeleteDisbenefitFunction, nil);
      AddMenuItemEntry(CCreateReturnFlowChannel,  365, CmeCreateReturnFlowChannel, nil);
      AddMenuItemEntry(CDeleteReturnFlowChannel,  370, CmeDeleteReturnFlowChannel, nil);

      AddMenuItemEntry(CHelpWRPMReleaseNote,          240, CmeWRPMReleaseNote);
      AddMenuItemEntry(CHelpWRPMUserGuide,            230, CmeWRPMUserGuide);
      AddMenuItemEntry(CHelpWRPMParameterTables,      240, CmeWRPMParameterTables);
      AddMenuItemEntry(CHelpHydrologyParameterTables, 250, CmeHydrologyParameterTables);
    end;

    if (FAppModules.StudyArea.ModelSubCode = CYield) then
    begin
      AddMenuItemEntry(CHelpWRMFReleaseNote,          240, CmeWRMFReleaseNote);
      AddMenuItemEntry(CHelpWRYMUserGuide,            241, CmeWRYMUserGuide);
      AddMenuItemEntry(CHelpWRYMProceduralManual,     242, CmeWRYMProceduralManual);
      AddMenuItemEntry(CHelpWRYMTrainingMaterial,     243, CmeWRYMTrainingMaterial);
      AddMenuItemEntry(CHelpWRYMParameterTables,      244, CmeWRYMParameterTables);
      AddMenuItemEntry(CHelpHydrologyParameterTables, 245, CmeHydrologyParameterTables);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.DestroyMemberObjects;
const OPNAME = 'TYieldModelMenuItemManager.DestroyMemberObjects';
begin
  try
    if Assigned(FAppModules.MainForm()) then
      FAppModules.MainForm.RemoveSystemChildToolBar(FToolBar)
    else
      FToolBar.Parent := nil;
    FreeAndNil(FToolBar);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelMenuItemManager.Initialise: Boolean;
const OPNAME = 'TYieldModelMenuItemManager.Initialise';
begin
  Result := inherited Initialise;
  try
    SetMenuValidateFiles(msDisable);
    SetMenuImportFiles(msDisable);
    SetMenuExportFiles(msDisable);
    SetMenuClearModelData(msDisable);
    SetMenuValidateModelData(msDisable);
    SetMenuGenerateSysConfigFiles(msDisable);
    FAppModules.MainForm.AddSystemChildToolBar(FToolBar);

    SetCreateReservoir(msEnable);
    SetDeleteReservoir(msDisable);
    SetCreateNodeWithInflow(msEnable);
    SetDeleteNodeWithInflow(msDisable);
    SetCreateNodeWithoutInflow(msEnable);
    SetDeleteNodeWithoutInflow(msDisable);
    SetCreateChannel(msEnable);
    SetCopyChannel(msDisable);
    SetDeleteChannel(msDisable);
    SetConvertChannel(msDisable);
    SetCreateFeature(msDisable);
    SetCreatePumpingFeature(msDisable);
    SetCopyPumpingFeature(msDisable);
    SetDeletePumpingFeature(msDisable);
    SetCreateSpecifiedInflowFeature(msDisable);
    SetCreateIFRFeature(msDisable);
    SetCopyIFRFeature(msDisable);
    SetDeleteIFRFeature(msDisable);
    SetCreatePhysicalFlowConstraint(msDisable);
    SetCopyPhysicalFlowConstraint(msDisable);
    SetDeletePhysicalFlowConstraint(msDisable);
     //Added by Karabo
    SetCreateMultiChannelCurtailRule(msDisable);
    SetDeleteMultiChannelCurtailRule(msDisable);

    SetCreateIrrigationArea(msEnable);
    SetCopyIrrigationArea(msDisable);
    SetDeleteIrrigationArea(msDisable);
    SetCreateIrrigationBlock(msEnable);

    SetCopyIrrigationBlock(msDisable);

    SetCreateGroundWater(msEnable);
    SetCopyGroundWater(msDisable);
    SetDeleteGroundWater(msDisable);

    SetDeleteIrrigationBlock(msDisable);
    SetCreateWetland(msEnable);
    SetCreateDroughtRestriction(msEnable);
    SetCopyWetland(msDisable);
    SetDeleteWetland(msDisable);
    SetDeleteDroughtRestriction(msDisable);
    SetCreateYMDemandCentre(msEnable);
    SetCopyYMDemandCentre(msDisable);
    SetDeleteYMDemandCentre(msDisable);
    SetCreateSFRSubCatchment(msEnable);
    SetCopySFRSubCatchment(msDisable);
    SetDeleteSFRSubCatchment(msDisable);
    SetCreateMine(msEnable);
    SetCopyMine(msDisable);
    SetDeleteMine(msDisable);
    SetCreatePowerPlant(msEnable);
    SetCopyPowerPlant(msDisable);
    SetDeletePowerPlant(msDisable);
    SetCreateMasterControlFeature(msDisable);
    SetDeleteMasterControlFeature(msDisable);
    SetCreateWaterDemandFeature(msDisable);
    SetDeleteWaterDemandFeature(msDisable);
    SetCreateWizardNewReservoir(msEnable);
    SetWizardNewNodeWithInflow(msEnable);
    SetWizardNewChannel(msEnable);
    SetCopyReservoir(msDisable);

    SetScenarioCopyReservoir(msEnable);
    SetScenarioCopyChannel(msEnable);

    SetScenarioCopyIrrigationArea(msEnable);
    SetScenarioCopyIrrigationBlock(msEnable);
    SetScenarioCopyPowerPlant(msEnable);
    SetScenarioCopyWetland(msEnable);
    SetScenarioCopyYMDemandCentre(msEnable);
    SetScenarioCopySFRSubCatchment(msEnable);
    SetScenarioCopyMine(msEnable);

    SetInvokeWizard(msDisable);
    SetWizardRunYield(msEnable);

    SetCreateAllocDef(msEnable);
    SetDeleteAllocDef(msDisable);
    SetCreateSwitchDef(msEnable);
    SetDeleteSwitchDef(msDisable);
    SetCreateResTimeCntrl(msDisable);
    SetDeleteResTimeCntrl(msDisable);
    SetCreateResReplacement(msDisable);
    SetCreateChannelTimeCntrl(msDisable);
    SetDeleteChannelTimeCntrl(msDisable);
    SetCreateChannelSwitchCntrl(msDisable);
    SetDeleteChannelSwitchCntrl(msDisable);
    SetCreateDisbenefitFunction(msDisable);
    SetDeleteDisbenefitFunction(msDisable);
    SetCreateReturnFlowChannel(msDisable);
    SetDeleteReturnFlowChannel(msDisable);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.CreateToolBar;
const OPNAME = 'TYieldModelMenuItemManager.CreateToolBar';
begin
  try
    FToolBar := TYieldModelToolBar.Create(nil,FAppModules);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelMenuItemManager.LanguageHasChanged: boolean;
const OPNAME = 'TYieldModelMenuItemManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    Result := Result and FToolBar.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelMenuItemManager.StudyHasChanged: Boolean;
const OPNAME = 'TYieldModelMenuItemManager.StudyHasChanged';
var LYieldModelDataObject: TYieldModelDataObject;
begin
  Result := inherited StudyHasChanged;
  try
    Initialise;
    if (FAppModules.StudyArea.ModelVersion <> '7') then
    begin
      FAppModules.SetMenuItem(CCreateIrrigationBlock, msHide);
      FAppModules.SetMenuItem(CCopyIrrigationBlock, msHide);
      FAppModules.SetMenuItem(CDeleteIrrigationBlock, msHide);
      FAppModules.SetMenuItem(CCreateWetland, msHide);
      FAppModules.SetMenuItem(CCopyWetland, msHide);
      FAppModules.SetMenuItem(CDeleteWetland, msHide);
      FAppModules.SetMenuItem(CCreateYMDemandCentre, msHide);
      FAppModules.SetMenuItem(CCopyYMDemandCentre, msHide);
      FAppModules.SetMenuItem(CDeleteYMDemandCentre, msHide);
      FAppModules.SetMenuItem(CDeleteSFRSubCatchment, msHide);
      FAppModules.SetMenuItem(CCreateSFRSubCatchment, msHide);
      FAppModules.SetMenuItem(CCopySFRSubCatchment, msHide);
      FAppModules.SetMenuItem(CCreateMine, msHide);
      FAppModules.SetMenuItem(CCopyMine, msHide);
      FAppModules.SetMenuItem(CDeleteMine, msHide);
      FAppModules.SetMenuItem(CDroughtRestriction, msHide);
      FAppModules.SetMenuItem(CCreateDroughtRestriction, msHide);
      FAppModules.SetMenuItem(CDeleteDroughtRestriction, msHide);
      FAppModules.SetMenuItem(CScenarioCopyIrrigationBlock, msHide);
      FAppModules.SetMenuItem(CScenarioCopyWetland, msHide);
      FAppModules.SetMenuItem(CScenarioCopyYMDemandCentre, msHide);
      FAppModules.SetMenuItem(CScenarioCopySFRSubCatchment, msHide);
      FAppModules.SetMenuItem(CScenarioCopyMine, msHide);
      FAppModules.SetMenuItem(CScenarioCopyGroundWater, msHide);
      FAppModules.SetMenuItem(CCreateGroundWater, msHide);
      FAppModules.SetMenuItem(CCopyGroundWater, msHide);
      FAppModules.SetMenuItem(CDeleteGroundWater, msHide);
    end else begin

      FAppModules.SetMenuItem(CCreateIrrigationBlock, msShow);
      FAppModules.SetMenuItem(CCopyIrrigationBlock, msShow);
      FAppModules.SetMenuItem(CDeleteIrrigationBlock, msShow);
      FAppModules.SetMenuItem(CCreateWetland, msShow);
      FAppModules.SetMenuItem(CCopyWetland, msShow);
      FAppModules.SetMenuItem(CDeleteWetland, msShow);
      FAppModules.SetMenuItem(CCreateYMDemandCentre, msShow);
      FAppModules.SetMenuItem(CCopyYMDemandCentre, msShow);
      FAppModules.SetMenuItem(CDeleteYMDemandCentre, msShow);
      FAppModules.SetMenuItem(CDeleteSFRSubCatchment, msShow);
      FAppModules.SetMenuItem(CCopySFRSubCatchment, msShow);
      FAppModules.SetMenuItem(CCreateSFRSubCatchment, msShow);
      FAppModules.SetMenuItem(CCreateMine, msShow);
      FAppModules.SetMenuItem(CCopyMine, msShow);
      FAppModules.SetMenuItem(CDeleteMine, msShow);
      FAppModules.SetMenuItem(CDroughtRestriction, msShow);
      FAppModules.SetMenuItem(CCreateDroughtRestriction, msShow);
      FAppModules.SetMenuItem(CDeleteDroughtRestriction, msShow);

      LYieldModelDataObject := TYieldModelDataObject(FAppModules.Model.ModelData);
      if LYieldModelDataObject <> nil then
      begin
        if LYieldModelDataObject.ImplementedNetworkFeatures.GroundWaterFeatureImplemented then
        begin
          FAppModules.SetMenuItem(CCreateGroundWater, msShow);
          FAppModules.SetMenuItem(CCopyGroundWater, msShow);
          FAppModules.SetMenuItem(CDeleteGroundWater, msShow);
        end
        else
        begin
          FAppModules.SetMenuItem(CCreateGroundWater, msHide);
          FAppModules.SetMenuItem(CCopyGroundWater, msHide);
          FAppModules.SetMenuItem(CDeleteGroundWater, msHide);
        end;
      end;

      FAppModules.SetMenuItem(CScenarioCopyIrrigationArea, msShow);
      FAppModules.SetMenuItem(CScenarioCopyPowerPlant, msShow);

      FAppModules.SetMenuItem(CScenarioCopyIrrigationBlock, msShow);
      FAppModules.SetMenuItem(CScenarioCopyWetland, msShow);
      FAppModules.SetMenuItem(CScenarioCopyYMDemandCentre, msShow);
      FAppModules.SetMenuItem(CScenarioCopySFRSubCatchment, msShow);
      FAppModules.SetMenuItem(CScenarioCopyMine, msShow);
      FAppModules.SetMenuItem(CScenarioCopyGroundWater, msShow);
    end;
    Result := FToolBar.StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetMenuExportFiles(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetMenuExportFiles';
begin
  try
    FToolBar.SetExportFilesState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CExportFiles, AAction, 'ExportFilesDisabled');
    else
      FAppModules.SetMenuItem(CExportFiles, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetMenuImportFiles(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetMenuImportFiles';
begin
  try
    if(AAction = msEnable) then
      if(not (FAppModules.User.UserRights in CUR_EditData)) or
        FAppModules.StudyArea.ScenarioLocked then
          AAction := msDisable;

    FToolBar.SetImportFilesState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CImportFiles, AAction, 'ImportFilesDisabled');
    else
      FAppModules.SetMenuItem(CImportFiles, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetMenuValidateFiles(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetMenuValidateFiles';
begin
  try
    FToolBar.SetValidateFilesState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CValidateFiles, AAction, 'ValidateFilesDisabled');
    else
      FAppModules.SetMenuItem(CValidateFiles, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetMenuClearModelData(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetMenuClearModelData';
begin
  try
    if(AAction = msEnable) then
      if(not (FAppModules.User.UserRights in CUR_EditData)) or
        FAppModules.StudyArea.ScenarioLocked then
          AAction := msDisable;
    FToolBar.SetClearModelDataState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CClearModelData, AAction, 'ClearFilesDisabled');
    else
      FAppModules.SetMenuItem(CClearModelData, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetMenuRunModel(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetMenuRunModel';
begin
  try
    FToolBar.SetRunModelState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CRunModel, AAction, 'RunModelDisabled');
    else
      FAppModules.SetMenuItem(CRunModel, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetExpertUserChecked(AChecked: boolean);
const OPNAME = 'TYieldModelMenuItemManager.SetExpertUserChecked';
begin
  try
    if AChecked then
    begin
      FAppModules.SetMenuItem(CViewExpertUser, msChecked, '');
      FAppModules.SetMenuItem(CViewStandardUser, msUnChecked);
    end
    else
    begin
     FAppModules.SetMenuItem(CViewExpertUser, msUnChecked);
     FAppModules.SetMenuItem(CViewStandardUser ,msChecked, '');
    end;

    if (FAppModules.User.UserType = utExpert) and (FAppModules.StudyArea.ModelVersion = '7') then
    begin
      FAppModules.SetMenuItem(CGenerateSysConfigFiles,msShow);
      FAppModules.SetMenuItem(CGenerateSysConfigFiles,msEnable);
    end else
    begin
      FAppModules.SetMenuItem(CGenerateSysConfigFiles,msHide);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetStandardUserChecked(AChecked: boolean);
const OPNAME = 'TYieldModelMenuItemManager.SetStandardUserChecked';
begin
  try
    if AChecked then
    begin
      FAppModules.SetMenuItem(CViewExpertUser, msUnChecked, '');
      FAppModules.SetMenuItem(CViewStandardUser, msChecked);
    end
    else
    begin
     FAppModules.SetMenuItem(CViewExpertUser, msChecked);
     FAppModules.SetMenuItem(CViewStandardUser ,msUnChecked, '');
    end;

    if (FAppModules.User.UserType = utExpert) and (FAppModules.StudyArea.ModelVersion = '7') then
    begin
      FAppModules.SetMenuItem(CGenerateSysConfigFiles,msShow);
      FAppModules.SetMenuItem(CGenerateSysConfigFiles,msEnable);
    end else
    begin
      FAppModules.SetMenuItem(CGenerateSysConfigFiles,msHide);
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetMenuValidateModelData(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetMenuValidateModelData';
begin
  try
    FToolBar.SetValidateModelDataClickState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CValidateModelData, AAction, 'ValidateModelDataDisabled');
    else
      FAppModules.SetMenuItem(CValidateModelData, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.TabHasChanged(AGridTabSelected: boolean);
const OPNAME = 'TYieldModelMenuItemManager.TabHasChanged';
var
  LAction               : TMenuSetAction;
  LYieldModelDataObject : TYieldModelDataObject;
begin
  try
    FToolBar.TabHasChanged(AGridTabSelected);

    if AGridTabSelected then
      LAction := msShow
     else
      LAction := msHide;

  FAppModules.SetMenuItem(CCreateReservoir, LAction);
  FAppModules.SetMenuItem(CDeleteReservoir, LAction);
  FAppModules.SetMenuItem(CCreateNodeWithInflow, LAction);
  FAppModules.SetMenuItem(CDeleteNodeWithInflow, LAction);
  FAppModules.SetMenuItem(CCreateNodeWithoutInflow, LAction);
  FAppModules.SetMenuItem(CDeleteNodeWithoutInflow, LAction);
  FAppModules.SetMenuItem(CCreateChannel, LAction);
  FAppModules.SetMenuItem(CCopyChannel, LAction);
  FAppModules.SetMenuItem(CCopyReservoir, LAction);
  FAppModules.SetMenuItem(CDeleteChannel, LAction);
  FAppModules.SetMenuItem(CConvertChannel, LAction);
  FAppModules.SetMenuItem(CCreateMinimumFlowFeature, LAction);
  FAppModules.SetMenuItem(CCreateMinMaxFlowFeature, LAction);
  FAppModules.SetMenuItem(CCreatePumpingFeature, LAction);
  FAppModules.SetMenuItem(CCopyPumpingFeature, LAction);
  FAppModules.SetMenuItem(CDeletePumpingFeature, LAction);
  FAppModules.SetMenuItem(CCreateLossFeature, LAction);
  FAppModules.SetMenuItem(CCreateSpecifiedDemandFeature, LAction);
  FAppModules.SetMenuItem(CCreateDiversionFeature, LAction);
  FAppModules.SetMenuItem(CCreateSpecifiedInflowFeature, LAction);
  FAppModules.SetMenuItem(CCreateIFRFeature, LAction);
  FAppModules.SetMenuItem(CCopyIFRFeature, LAction);
  FAppModules.SetMenuItem(CDeleteIFRFeature, LAction);
  FAppModules.SetMenuItem(CCreatePhysicalFlowConstraint, LAction);
  FAppModules.SetMenuItem(CCopyPhysicalFlowConstraint, LAction);
  FAppModules.SetMenuItem(CDeletePhysicalFlowConstraint, LAction);
  FAppModules.SetMenuItem(CCreateMultiChannelCurtailRule, LAction);
  FAppModules.SetMenuItem(CDeleteMultiChannelCurtailRule, LAction);
  FAppModules.SetMenuItem(CCreateIrrigationArea, LAction);
  FAppModules.SetMenuItem(CCopyIrrigationArea, LAction);
  FAppModules.SetMenuItem(CDeleteIrrigationArea, LAction);
  FAppModules.SetMenuItem(CCreatePowerPlant, LAction);
  FAppModules.SetMenuItem(CCopyPowerPlant, LAction);
  FAppModules.SetMenuItem(CDeletePowerPlant, LAction);
  FAppModules.SetMenuItem(CCreateMasterControlFeature, LAction);
  FAppModules.SetMenuItem(CDeleteMasterControlFeature, LAction);
  FAppModules.SetMenuItem(CCreateWaterDemandFeature, LAction);
  FAppModules.SetMenuItem(CDeleteWaterDemandFeature, LAction);

  FAppModules.SetMenuItem(CCopyIrrigationBlock, LAction);
  FAppModules.SetMenuItem(CCopyWetland, LAction);
  FAppModules.SetMenuItem(CCopyYMDemandCentre, LAction);
  FAppModules.SetMenuItem(CCopySFRSubCatchment, LAction);
  FAppModules.SetMenuItem(CCopyMine, LAction);

  if(FAppModules.StudyArea.ModelVersion <> '7') then
  begin
    FAppModules.SetMenuItem(CCreateIrrigationBlock, msHide);
    FAppModules.SetMenuItem(CCopyIrrigationBlock, msHide);
    FAppModules.SetMenuItem(CDeleteIrrigationBlock, msHide);
    FAppModules.SetMenuItem(CCreateWetland, msHide);
    FAppModules.SetMenuItem(CCopyWetland, msHide);
    FAppModules.SetMenuItem(CDeleteWetland, msHide);
    FAppModules.SetMenuItem(CCreateYMDemandCentre, msHide);
    FAppModules.SetMenuItem(CCopyYMDemandCentre, msHide);
    FAppModules.SetMenuItem(CDeleteYMDemandCentre, msHide);
    FAppModules.SetMenuItem(CCreateSFRSubCatchment, msHide);
    FAppModules.SetMenuItem(CCopySFRSubCatchment, msHide);
    FAppModules.SetMenuItem(CDeleteSFRSubCatchment, msHide);
    FAppModules.SetMenuItem(CCreateMine, msHide);
    FAppModules.SetMenuItem(CCopyMine, msHide);
    FAppModules.SetMenuItem(CDeleteMine, msHide);
    FAppModules.SetMenuItem(CDroughtRestriction, msHide);
    FAppModules.SetMenuItem(CCreateDroughtRestriction, msHide);
    FAppModules.SetMenuItem(CDeleteDroughtRestriction, msHide);
    FAppModules.SetMenuItem(CCreateGroundWater, msHide);
    FAppModules.SetMenuItem(CCopyGroundWater, msHide);
    FAppModules.SetMenuItem(CDeleteGroundWater, msHide);
  end
  else
  begin
    FAppModules.SetMenuItem(CCreateIrrigationBlock, LAction);
    FAppModules.SetMenuItem(CCopyIrrigationBlock, LAction);
    FAppModules.SetMenuItem(CDeleteIrrigationBlock, LAction);
    FAppModules.SetMenuItem(CCreateWetland, LAction);
    FAppModules.SetMenuItem(CCopyWetland, LAction);
    FAppModules.SetMenuItem(CDeleteWetland, LAction);
    FAppModules.SetMenuItem(CCreateYMDemandCentre, LAction);
    FAppModules.SetMenuItem(CCopyYMDemandCentre, LAction);
    FAppModules.SetMenuItem(CDeleteYMDemandCentre, LAction);
    FAppModules.SetMenuItem(CCreateSFRSubCatchment, LAction);
    FAppModules.SetMenuItem(CCopySFRSubCatchment, LAction);
    FAppModules.SetMenuItem(CDeleteSFRSubCatchment, LAction);
    FAppModules.SetMenuItem(CCreateMine, LAction);
    FAppModules.SetMenuItem(CCopyMine, LAction);
    FAppModules.SetMenuItem(CDeleteMine, LAction);
    FAppModules.SetMenuItem(CDroughtRestriction, LAction);
    FAppModules.SetMenuItem(CCreateDroughtRestriction, LAction);
    FAppModules.SetMenuItem(CDeleteDroughtRestriction, LAction);
    FAppModules.SetMenuItem(CCreateGroundWater, LAction);
    FAppModules.SetMenuItem(CCopyGroundWater, LAction);
    FAppModules.SetMenuItem(CDeleteGroundWater, LAction);


    LYieldModelDataObject := TYieldModelDataObject(FAppModules.Model.ModelData);
    if LYieldModelDataObject <> nil then
    begin
      if (TYieldModelDataObject(FAppModules.Model.ModelData).ImplementedNetworkFeatures.GroundWaterFeatureImplemented) then
      begin
        FAppModules.SetMenuItem(CCreateGroundWater, LAction);
        FAppModules.SetMenuItem(CCopyGroundWater, LAction);
        FAppModules.SetMenuItem(CDeleteGroundWater, LAction);
      end
      else
      begin
        FAppModules.SetMenuItem(CCreateGroundWater, msHide);
        FAppModules.SetMenuItem(CCopyGroundWater, msHide);
        FAppModules.SetMenuItem(CDeleteGroundWater, msHide);
      end;
    end;
  end;

  FAppModules.SetMenuItem(CWizards, LAction);
  FAppModules.SetMenuItem(CWizardNewReservoir, LAction);
  FAppModules.SetMenuItem(CWizardNewNodeWithInflow, LAction);
  FAppModules.SetMenuItem(CWizardNewChannel, LAction);
  FAppModules.SetMenuItem(CWizardSeparator1, LAction);
  FAppModules.SetMenuItem(CWizardInvoke, LAction);
  FAppModules.SetMenuItem(CWizardSeparator2, LAction);
  FAppModules.SetMenuItem(CWizardRunYieldHistoric, LAction);
  FAppModules.SetMenuItem(CWizardRunYieldStochastic, LAction);
  FAppModules.SetMenuItem(CWizardRunYieldYRC, LAction);

  FAppModules.SetMenuItem(CCreateAllocDef, LAction);
  FAppModules.SetMenuItem(CDeleteAllocDef, LAction);
  FAppModules.SetMenuItem(CCreateSwitchDef, LAction);
  FAppModules.SetMenuItem(CDeleteSwitchDef, LAction);
  FAppModules.SetMenuItem(CCreateResTimeCntrl, LAction);
  FAppModules.SetMenuItem(CDeleteResTimeCntrl, LAction);
  FAppModules.SetMenuItem(CCreateResReplacement, LAction);
  FAppModules.SetMenuItem(CCreateChannelTimeCntrl, LAction);
  FAppModules.SetMenuItem(CDeleteChannelTimeCntrl, LAction);
  FAppModules.SetMenuItem(CCreateChannelSwitchCntrl, LAction);
  FAppModules.SetMenuItem(CDeleteChannelSwitchCntrl, LAction);
  FAppModules.SetMenuItem(CCreateDisbenefitFunction, LAction);
  FAppModules.SetMenuItem(CDeleteDisbenefitFunction, LAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCreateReservoir(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCreateReservoir';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    FToolBar.SetCreateReservoir(AAction = msEnable);
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCreateReservoir, msDisable, 'ActionCreateReservoirDisabled')
    else
      FAppModules.SetMenuItem(CCreateReservoir, AAction)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCreateNodeWithInflow(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCreateNodeWithInflow';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    FToolBar.SetCreateNodeWithInflow(AAction = msEnable);
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCreateNodeWithInflow, msDisable, 'ActionCreateNodeWithInflowDisabled')
    else
      FAppModules.SetMenuItem(CCreateNodeWithInflow, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCreateNodeWithoutInflow(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCreateNodeWithoutInflow';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    FToolBar.SetCreateNodeWithoutInflow(AAction = msEnable);
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCreateNodeWithoutInflow, msDisable, 'ActionCreateNodeWithoutInflowDisabled')
    else
      FAppModules.SetMenuItem(CCreateNodeWithoutInflow, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetDeleteNodeWithInflow(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetDeleteNodeWithInflow';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    FToolBar.SetDeleteNodeWithInflow(AAction = msEnable);
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CDeleteNodeWithInflow, msDisable, 'ActionDeleteNodeWithInflowDisabled')
    else
      FAppModules.SetMenuItem(CDeleteNodeWithInflow, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetDeleteNodeWithoutInflow(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetDeleteNodeWithoutInflow';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    FToolBar.SetDeleteNodeWithoutInflow(AAction = msEnable);
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CDeleteNodeWithoutInflow, msDisable, 'ActionDeleteNodeWithoutInflowDisabled')
    else
      FAppModules.SetMenuItem(CDeleteNodeWithoutInflow, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetDeleteReservoir(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetDeleteReservoir';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    FToolBar.SetDeleteReservoir(AAction = msEnable);
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CDeleteReservoir, msDisable, 'ActionDeleteReservoirDisabled')
    else
      FAppModules.SetMenuItem(CDeleteReservoir, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCreateChannel(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCreateChannel';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    FToolBar.SetCreateChannel(AAction = msEnable);
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCreateChannel, msDisable, 'ActionCreateChannelDisabled')
    else
      FAppModules.SetMenuItem(CCreateChannel, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCopyChannel(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCopyChannel';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCopyChannel, msDisable, 'ActionCopyChannelDisabled')
    else
      FAppModules.SetMenuItem(CCopyChannel, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCopyReservoir(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCopyReservoir';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCopyReservoir, msDisable, 'ActionDeleteReservoirDisabled')
    else
      FAppModules.SetMenuItem(CCopyReservoir, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetDeleteChannel(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetDeleteChannel';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    FToolBar.SetDeleteChannel(AAction = msEnable);
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CDeleteChannel, msDisable, 'ActionDeleteChannelDisabled')
    else
      FAppModules.SetMenuItem(CDeleteChannel, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetConvertChannel(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetConvertChannel';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CConvertChannel, msDisable, 'ActionConvertChannelDisabled')
    else
      FAppModules.SetMenuItem(CConvertChannel, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCreateFeature(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCreateFeature';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msEnable) then
    begin
      FAppModules.SetMenuItem(CCreateMinimumFlowFeature,     msEnable);
      FAppModules.SetMenuItem(CCreateMinMaxFlowFeature,      msEnable);
      FAppModules.SetMenuItem(CCreateLossFeature,            msEnable);
      FAppModules.SetMenuItem(CCreateSpecifiedDemandFeature, msEnable);
      FAppModules.SetMenuItem(CCreateDiversionFeature,       msEnable);
    end
    else
    begin
      FAppModules.SetMenuItem(CCreateMinimumFlowFeature,     msDisable, 'ActionCreateFeatureDisabled');
      FAppModules.SetMenuItem(CCreateMinMaxFlowFeature,      msDisable, 'ActionCreateFeatureDisabled');
      FAppModules.SetMenuItem(CCreateLossFeature,            msDisable, 'ActionCreateFeatureDisabled');
      FAppModules.SetMenuItem(CCreateSpecifiedDemandFeature, msDisable, 'ActionCreateFeatureDisabled');
      FAppModules.SetMenuItem(CCreateDiversionFeature,       msDisable, 'ActionCreateFeatureDisabled');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCreatePumpingFeature(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCreatePumpingFeature';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCreatePumpingFeature, msDisable, 'ActionCreatePumpingFeatureDisabled')
    else
      FAppModules.SetMenuItem(CCreatePumpingFeature, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCopyPumpingFeature(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCopyPumpingFeature';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCopyPumpingFeature, msDisable, 'ActionCopyPumpingFeatureDisabled')
    else
      FAppModules.SetMenuItem(CCopyPumpingFeature, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetDeletePumpingFeature(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetDeletePumpingFeature';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CDeletePumpingFeature, msDisable, 'ActionDeletePumpingFeatureDisabled')
    else
      FAppModules.SetMenuItem(CDeletePumpingFeature, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCreateIFRFeature(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCreateIFRFeature';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCreateIFRFeature, msDisable, 'ActionCreateIFRFeatureDisabled')
    else
      FAppModules.SetMenuItem(CCreateIFRFeature, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TYieldModelMenuItemManager.SetCopyIFRFeature(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCopyIFRFeature';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCopyIFRFeature, msDisable, 'ActionCopyIFRFeatureDisabled')
    else
      FAppModules.SetMenuItem(CCopyIFRFeature, AAction)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCreateSpecifiedInflowFeature(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCreateSpecifiedInflowFeature';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCreateSpecifiedInflowFeature, msDisable, 'ActionCreateSpecifiedInflowFeatureDisabled')
    else
      FAppModules.SetMenuItem(CCreateSpecifiedInflowFeature, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetDeleteIFRFeature(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetDeleteIFRFeature';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CDeleteIFRFeature, msDisable, 'ActionDeleteIFRFeatureDisabled')
    else
      FAppModules.SetMenuItem(CDeleteIFRFeature, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCreatePhysicalFlowConstraint(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCreatePhysicalFlowConstraint';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCreatePhysicalFlowConstraint, msDisable, 'ActionCreatePhysicalFlowConstraintDisabled')
    else
      FAppModules.SetMenuItem(CCreatePhysicalFlowConstraint, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCopyPhysicalFlowConstraint(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCopyPhysicalFlowConstraint';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCopyPhysicalFlowConstraint, msDisable, 'ActionCopyPhysicalFlowConstraintDisabled')
    else
      FAppModules.SetMenuItem(CCopyPhysicalFlowConstraint, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetDeletePhysicalFlowConstraint(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetDeletePhysicalFlowConstraint';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CDeletePhysicalFlowConstraint, msDisable, 'ActionDeletePhysicalFlowConstraintDisabled')
    else
      FAppModules.SetMenuItem(CDeletePhysicalFlowConstraint, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;
procedure TYieldModelMenuItemManager.SetCreateMultiChannelCurtailRule(AAction: TMenuSetAction);
const OPNAME =  'TYieldModelMenuItemManager.SetCreateMultiChannelCurtailRule';
begin
  try
   if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
      if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCreateMultiChannelCurtailRule, msDisable)
    else
      FAppModules.SetMenuItem(CCreateMultiChannelCurtailRule, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetDeleteMultiChannelCurtailRule(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetDeleteMultiChannelCurtailRule';
begin
  try
   if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
      if (AAction = msDisable) then
      FAppModules.SetMenuItem(CDeleteMultiChannelCurtailRule, msDisable)
    else
      FAppModules.SetMenuItem(CDeleteMultiChannelCurtailRule, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCreateIrrigationArea(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCreateIrrigationArea';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCreateIrrigationArea, msDisable, 'ActionCreateIrrigationAreaDisabled')
    else
      FAppModules.SetMenuItem(CCreateIrrigationArea, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCopyIrrigationArea(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCopyIrrigationArea';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCopyIrrigationArea, msDisable, 'ActionCopyIrrigationAreaDisabled')
    else
      FAppModules.SetMenuItem(CCopyIrrigationArea, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetDeleteIrrigationArea(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetDeleteIrrigationArea';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CDeleteIrrigationArea, msDisable, 'ActionDeleteIrrigationAreaDisabled')
    else
      FAppModules.SetMenuItem(CDeleteIrrigationArea, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCreatePowerPlant(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCreatePowerPlant';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCreatePowerPlant, msDisable, 'ActionCreatePowerPlantDisabled')
    else
      FAppModules.SetMenuItem(CCreatePowerPlant, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCopyPowerPlant(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCopyPowerPlant';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCopyPowerPlant, msDisable, 'ActionCopyPowerPlantDisabled')
    else
      FAppModules.SetMenuItem(CCopyPowerPlant, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetDeletePowerPlant(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetDeletePowerPlant';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CDeletePowerPlant, msDisable, 'ActionDeletePowerPlantDisabled')
    else
      FAppModules.SetMenuItem(CDeletePowerPlant, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCreateMasterControlFeature(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCreateMasterControlFeature';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCreateMasterControlFeature, msDisable, 'ActionCreateMasterControlFeatureDisabled')
    else
      FAppModules.SetMenuItem(CCreateMasterControlFeature, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetDeleteMasterControlFeature(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetDeleteMasterControlFeature';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CDeleteMasterControlFeature, msDisable, 'ActionDeleteMasterControlFeatureDisabled')
    else
      FAppModules.SetMenuItem(CDeleteMasterControlFeature, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetInvokeWizard(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetInvokeWizard';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CWizardInvoke, msDisable, 'ActionInvokeWizardDisabled')
    else
      FAppModules.SetMenuItem(CWizardInvoke, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetWizardRunYield(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetWizardRunYield';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msEnable) then
    begin
      FAppModules.SetMenuItem(CWizardRunYieldHistoric, msEnable);
      FAppModules.SetMenuItem(CWizardRunYieldStochastic, msEnable);
      FAppModules.SetMenuItem(CWizardRunYieldYRC, msEnable);
    end
    else
    begin
      FAppModules.SetMenuItem(CWizardRunYieldHistoric, msDisable, '');
      FAppModules.SetMenuItem(CWizardRunYieldStochastic, msDisable, '');
      FAppModules.SetMenuItem(CWizardRunYieldYRC, msDisable, '');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCreateWaterDemandFeature(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCreateWaterDemandFeature';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCreateWaterDemandFeature, msDisable, 'ActionCreateWaterDemandFeatureDisabled')
    else
      FAppModules.SetMenuItem(CCreateWaterDemandFeature, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetDeleteWaterDemandFeature(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetDeleteWaterDemandFeature';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CDeleteWaterDemandFeature, msDisable, 'ActionDeleteWaterDemandFeatureDisabled')
    else
      FAppModules.SetMenuItem(CDeleteWaterDemandFeature, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCreateWizardNewReservoir(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCreateWizardNewReservoir';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CWizardNewReservoir, msDisable, 'ActionCreateReservoirDisabled')
    else
      FAppModules.SetMenuItem(CWizardNewReservoir, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetWizardNewNodeWithInflow(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetWizardNewNodeWithInflow';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CWizardNewNodeWithInflow, msDisable, 'ActionCreateNodeWithInflowDisabled')
    else
      FAppModules.SetMenuItem(CWizardNewNodeWithInflow, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetWizardNewChannel(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetWizardNewChannel';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CWizardNewChannel, msDisable, 'ActionCreateChannelDisabled')
    else
      FAppModules.SetMenuItem(CWizardNewChannel, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCreateAllocDef(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCreateAllocDef';
begin
  try
    if (AAction = msEnable) AND
       ((FAppModules.StudyArea.ModelCode <> CPlanning) OR
        (NOT(FAppModules.User.UserRights in CUR_EditData)) OR
        (FAppModules.StudyArea.ScenarioLocked)) then
      AAction := msDisable;
    FToolBar.SetCreateAllocDef(AAction = msEnable);
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCreateAllocDef, msDisable, 'ActionCreateAllocDefDisabled')
    else
      FAppModules.SetMenuItem(CCreateAllocDef, AAction)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetDeleteAllocDef(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetDeleteAllocDef';
begin
  try
    if (AAction = msEnable) AND
       ((FAppModules.StudyArea.ModelCode <> CPlanning) OR
        (NOT(FAppModules.User.UserRights in CUR_EditData)) OR
        (FAppModules.StudyArea.ScenarioLocked)) then
      AAction := msDisable;
    FToolBar.SetDeleteAllocDef(AAction = msEnable);
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CDeleteAllocDef, msDisable, 'ActionDeleteAllocDefDisabled')
    else
      FAppModules.SetMenuItem(CDeleteAllocDef, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCreateSwitchDef(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCreateSwitchDef';
begin
  try
    if (AAction = msEnable) AND
       ((FAppModules.StudyArea.ModelCode <> CPlanning) OR
        (NOT(FAppModules.User.UserRights in CUR_EditData)) OR
        (FAppModules.StudyArea.ScenarioLocked)) then
      AAction := msDisable;
    FToolBar.SetCreateSwitchDef(AAction = msEnable);
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCreateSwitchDef, msDisable, 'ActionCreateSwitchDefDisabled')
    else
      FAppModules.SetMenuItem(CCreateSwitchDef, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetDeleteSwitchDef(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetDeleteSwitchDef';
begin
  try
    if (AAction = msEnable) AND
       ((FAppModules.StudyArea.ModelCode <> CPlanning) OR
        (NOT(FAppModules.User.UserRights in CUR_EditData)) OR
        (FAppModules.StudyArea.ScenarioLocked)) then
      AAction := msDisable;
    FToolBar.SetDeleteSwitchDef(AAction = msEnable);
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CDeleteSwitchDef, msDisable, 'ActionDeleteSwitchDefDisabled')
    else
      FAppModules.SetMenuItem(CDeleteSwitchDef, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCreateResTimeCntrl(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCreateResTimeCntrl';
begin
  try
    if (AAction = msEnable) AND
       ((FAppModules.StudyArea.ModelCode <> CPlanning) OR
        (NOT(FAppModules.User.UserRights in CUR_EditData)) OR
        (FAppModules.StudyArea.ScenarioLocked)) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCreateResTimeCntrl, msDisable, 'ActionCreateResTimeCntrlDisabled')
    else
      FAppModules.SetMenuItem(CCreateResTimeCntrl, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetDeleteResTimeCntrl(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetDeleteResTimeCntrl';
begin
  try
    if (AAction = msEnable) AND
       ((FAppModules.StudyArea.ModelCode <> CPlanning) OR
        (NOT(FAppModules.User.UserRights in CUR_EditData)) OR
        (FAppModules.StudyArea.ScenarioLocked)) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CDeleteResTimeCntrl, msDisable, 'ActionDeleteResTimeCntrlDisabled')
    else
      FAppModules.SetMenuItem(CDeleteResTimeCntrl, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCreateResReplacement(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCreateResReplacement';
begin
  try
    if (AAction = msEnable) AND
       ((FAppModules.StudyArea.ModelCode <> CPlanning) OR
        (NOT(FAppModules.User.UserRights in CUR_EditData)) OR
        (FAppModules.StudyArea.ScenarioLocked)) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCreateResReplacement, msDisable, 'ActionCreateResReplacementDisabled')
    else
      FAppModules.SetMenuItem(CCreateResReplacement, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCreateChannelTimeCntrl(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCreateChannelTimeCntrl';
begin
  try
    if (AAction = msEnable) AND
       ((FAppModules.StudyArea.ModelCode <> CPlanning) OR
        (NOT(FAppModules.User.UserRights in CUR_EditData)) OR
        (FAppModules.StudyArea.ScenarioLocked)) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCreateChannelTimeCntrl, msDisable, 'ActionCreateChannelTimeCntrlDisabled')
    else
      FAppModules.SetMenuItem(CCreateChannelTimeCntrl, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetDeleteChannelTimeCntrl(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetDeleteChannelTimeCntrl';
begin
  try
    if (AAction = msEnable) AND
       ((FAppModules.StudyArea.ModelCode <> CPlanning) OR
        (NOT(FAppModules.User.UserRights in CUR_EditData)) OR
        (FAppModules.StudyArea.ScenarioLocked)) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CDeleteChannelTimeCntrl, msDisable, 'ActionDeleteChannelTimeCntrlDisabled')
    else
      FAppModules.SetMenuItem(CDeleteChannelTimeCntrl, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCreateChannelSwitchCntrl(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCreateChannelSwitchCntrl';
begin
  try
    if (AAction = msEnable) AND
       ((FAppModules.StudyArea.ModelCode <> CPlanning) OR
        (NOT(FAppModules.User.UserRights in CUR_EditData)) OR
        (FAppModules.StudyArea.ScenarioLocked)) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCreateChannelSwitchCntrl, msDisable, 'ActionCreateChannelSwitchCntrlDisabled')
    else
      FAppModules.SetMenuItem(CCreateChannelSwitchCntrl, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetDeleteChannelSwitchCntrl(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetDeleteChannelSwitchCntrl';
begin
  try
    if (AAction = msEnable) AND
       ((FAppModules.StudyArea.ModelCode <> CPlanning) OR
        (NOT(FAppModules.User.UserRights in CUR_EditData)) OR
        (FAppModules.StudyArea.ScenarioLocked)) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CDeleteChannelSwitchCntrl, msDisable, 'ActionDeleteChannelSwitchCntrlDisabled')
    else
      FAppModules.SetMenuItem(CDeleteChannelSwitchCntrl, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCreateDisbenefitFunction(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCreateDisbenefitFunction';
begin
  try
    if (AAction = msEnable) AND
       ((FAppModules.StudyArea.ModelCode <> CPlanning) OR
        (NOT(FAppModules.User.UserRights in CUR_EditData)) OR
        (FAppModules.StudyArea.ScenarioLocked)) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCreateDisbenefitFunction, msDisable, 'ActionCreateDisbenefitFunctionDisabled')
    else
      FAppModules.SetMenuItem(CCreateDisbenefitFunction, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetDeleteDisbenefitFunction(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetDeleteDisbenefitFunction';
begin
  try
    if (AAction = msEnable) AND
       ((FAppModules.StudyArea.ModelCode <> CPlanning) OR
        (NOT(FAppModules.User.UserRights in CUR_EditData)) OR
        (FAppModules.StudyArea.ScenarioLocked)) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CDeleteDisbenefitFunction, msDisable, 'ActionDeleteDisbenefitFunctionDisabled')
    else
      FAppModules.SetMenuItem(CDeleteDisbenefitFunction, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCreateReturnFlowChannel(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCreateReturnFlowChannel';
begin
  try
    if (AAction = msEnable) AND
       ((FAppModules.StudyArea.ModelCode <> CPlanning) OR
        (NOT(FAppModules.User.UserRights in CUR_EditData)) OR
        (FAppModules.StudyArea.ScenarioLocked)) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCreateReturnFlowChannel, msDisable, 'ActionCreateReturnFlowChannelDisabled')
    else
      FAppModules.SetMenuItem(CCreateReturnFlowChannel, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetDeleteReturnFlowChannel(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetDeleteReturnFlowChannel';
begin
  try
    if (AAction = msEnable) AND
       ((FAppModules.StudyArea.ModelCode <> CPlanning) OR
        (NOT(FAppModules.User.UserRights in CUR_EditData)) OR
        (FAppModules.StudyArea.ScenarioLocked)) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CDeleteReturnFlowChannel, msDisable, 'ActionDeleteReturnFlowChannelDisabled')
    else
      FAppModules.SetMenuItem(CDeleteReturnFlowChannel, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TYieldModelMenuItemManager.TreeNodeHasChanged (ADataType  : string;
                                                         AElementID : integer);
const OPNAME = 'TYieldModelMenuItemManager.TreeNodeHasChanged';
var
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  lMasterList    : IMasterControlFeatureList;
  lReservoirList : IReservoirDataList;
  lReservoir     : IReservoirData;
  lRunCon        : IRunConfigurationData;
begin
  try
    SetDeleteSwitchDef(msDisable);
    SetDeleteAllocDef(msDisable);

    SetDeleteReservoir(msDisable);
    SetDeleteNodeWithInflow(msDisable);
    SetDeleteNodeWithoutInflow(msDisable);
    SetCopyChannel(msDisable);
    SetDeleteChannel(msDisable);
    SetCopyIrrigationArea(msDisable);
    SetDeleteIrrigationArea(msDisable);
    SetCopyIrrigationBlock(msDisable);
    SetDeleteIrrigationBlock(msDisable);
    SetCopyWetland(msDisable);
    SetDeleteWetland(msDisable);
    SetCopyYMDemandCentre(msDisable);
    SetDeleteYMDemandCentre(msDisable);
    SetCopySFRSubCatchment(msDisable);
    SetDeleteSFRSubCatchment(msDisable);
    SetDeletePowerPlant(msDisable);
    SetCopyPowerPlant(msDisable);
    SetInvokeWizard(msDisable);

    SetConvertChannel(msDisable);
    SetCreateFeature(msDisable);
    SetCreateSpecifiedInflowFeature(msDisable);
    SetCreatePumpingFeature(msDisable);
    SetCopyPumpingFeature(msDisable);
    SetDeletePumpingFeature(msDisable);
    SetCreateIFRFeature(msDisable);
    SetCopyIFRFeature(msDisable);
    SetDeleteIFRFeature(msDisable);
    SetCreatePhysicalFlowConstraint(msDisable);
    SetCopyPhysicalFlowConstraint(msDisable);
    SetDeletePhysicalFlowConstraint(msDisable);
    SetCreateMasterControlFeature(msDisable);
    SetDeleteMasterControlFeature(msDisable);
    SetCreateWaterDemandFeature(msDisable);
    SetDeleteWaterDemandFeature(msDisable);

    SetCreateResTimeCntrl(msDisable);
    SetDeleteResTimeCntrl(msDisable);
    SetCreateResReplacement(msDisable);
    SetCreateChannelTimeCntrl(msDisable);
    SetDeleteChannelTimeCntrl(msDisable);
    SetCreateChannelSwitchCntrl(msDisable);
    SetDeleteChannelSwitchCntrl(msDisable);

    SetCreateDisbenefitFunction(msDisable);
    SetDeleteDisbenefitFunction(msDisable);
    SetCreateReturnFlowChannel(msDisable);
    SetDeleteReturnFlowChannel(msDisable);
    SetCopyReservoir(msDisable);

    SetCreateMine(msEnable);
    SetCopyMine(msDisable);
    SetDeleteMine(msDisable);


    SetDeleteDroughtRestriction(msDisable);
    SetCreateDroughtRestriction(msEnable);


    SetCopyGroundWater(msDisable);
    SetDeleteGroundWater(msDisable);
    SetCreateMultiChannelCurtailRule(msDisable);
    SetDeleteMultiChannelCurtailRule(msDisable);


    if (FAppModules.User.UserRights in CUR_EditData) AND
       (NOT FAppModules.StudyArea.ScenarioLocked) then
    begin
      if (ADataType = 'IRRIGATIONAREA') then
      begin
        SetDeleteIrrigationArea(msEnable);
        SetCopyIrrigationArea(msEnable);
        SetInvokeWizard(msEnable);
      end
      else
      if (ADataType = 'IRRIGATIONBLOCK') then
      begin
        SetDeleteIrrigationBlock(msEnable);
        SetCopyIrrigationBlock(msEnable);
        SetInvokeWizard(msEnable);
      end
      else
      if (ADataType = 'POWERPLANT') then
      begin
        SetDeletePowerPlant(msEnable);
        SetCopyPowerPlant(msEnable);
        SetInvokeWizard(msEnable);
      end
      else
      if (ADataType = 'NODESWITHINFLOW') then
      begin
        SetDeleteNodeWithInflow(msEnable);
        SetInvokeWizard(msEnable);
      end
      else
      if (ADataType = 'NODESWITHOUTINFLOW') AND (AElementID <> 0) then
      begin
        SetDeleteNodeWithoutInflow(msEnable);
      end
      else
      if (ADataType = 'ALLOCATIONDEFINITION') then
      begin
        SetDeleteAllocDef(msEnable);
      end
      else
      if (ADataType = 'SWITCHDEFINITION') then
      begin
        SetDeleteSwitchDef(msEnable);
      end
      else
      if (ADataType = 'WETLAND') then
      begin
        SetDeleteWetland(msEnable);
        SetCopyWetland(msEnable);
        SetInvokeWizard(msEnable);
      end
      else
      if (ADataType = 'YMDEMANDCENTRE') then
      begin
        SetDeleteYMDemandCentre(msEnable);
        SetCopyYMDemandCentre(msEnable);
        SetInvokeWizard(msEnable);
      end
      else
      if (ADataType = 'STREAMFLOWREDUCTION') then
      begin
        SetDeleteSFRSubCatchment(msEnable);
        SetCopySFRSubCatchment(msEnable);
        SetInvokeWizard(msEnable);
      end
      else
      if (ADataType = 'MINE') then
      begin
        SetDeleteMine(msEnable);
        SetCopyMine(msEnable);
      end
      else
      if (ADataType = 'RESTRICTIONS') then
      begin
        SetDeleteDroughtRestriction(msEnable);
      end
      else
      if (ADataType = 'GROUNDWATER') then
      begin
        SetDeleteGroundWater(msEnable);
        SetCopyGroundWater(msEnable);
      end
      else
      if (ADataType = 'RESERVOIR') then
      begin
        SetDeleteReservoir(msEnable);
        SetCopyReservoir(msEnable);
        SetInvokeWizard(msEnable);

        if (FAppModules.StudyArea.ModelSubCode = CPlanning) then
        begin
          lReservoirList := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList;
          lReservoir     := lReservoirList.ReservoirByIdentifier[AElementID];
          if (Assigned(lReservoir)) then
          begin
            if (lReservoir.TimeControl = nil) then
            begin
              SetCreateResTimeCntrl(msEnable);
              SetDeleteResTimeCntrl(msDisable);
              SetCreateResReplacement(msDisable);
            end
            else
            begin
              SetCreateResTimeCntrl(msDisable);
              if (lReservoir.TimeControl.BaseNodeNumber = lReservoir.ReservoirConfigurationData.ReservoirIdentifier) then
              begin
                SetCreateResReplacement(msEnable);
                if (lReservoir.TimeControl.Replacements = '') then
                  SetDeleteResTimeCntrl(msEnable)
                else
                begin
                  SetDeleteReservoir(msDisable);
                  SetDeleteReservoir(msDisable);
                  SetDeleteResTimeCntrl(msDisable);
                end;
              end
              else
                SetCreateResReplacement(msDisable);
            end;
          end;
        end;
      end
      else
      if (ADataType = 'CHANNEL') OR (ADataType = 'MASTERCONTROLCONFIGURATION') then
      begin
        SetInvokeWizard(msEnable);
        lMasterList  := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.MasterControlFeatureList;
        lChannelList := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList;
        lChannel     := lChannelList.ChannelByChannelNumber[AElementID];
        if (Assigned(lChannel)) then
        begin
          if (lChannel.TimeControl = nil) AND (lChannel.SwitchControlCount = 0) then
          begin
            if (lChannel.ChannelType = 2) then
              SetCreateChannelTimeCntrl(msDisable)
            else
              SetCreateChannelTimeCntrl(msEnable);

            if (lChannel.ChannelType = 2) then
              SetCreateChannelSwitchCntrl(msDisable)
            else
              SetCreateChannelSwitchCntrl(msEnable);

            SetDeleteChannelTimeCntrl(msDisable);
            SetDeleteChannelSwitchCntrl(msDisable);
          end
          else
          if (lChannel.SwitchControlCount > 0) then
          begin
            SetCreateChannelTimeCntrl(msDisable);
            SetCreateChannelSwitchCntrl(msEnable);
            SetDeleteChannelTimeCntrl(msDisable);
            SetDeleteChannelSwitchCntrl(msEnable);
          end
          else
          if (lChannel.TimeControl <> nil) then
          begin
            SetCreateChannelTimeCntrl(msDisable);
            SetCreateChannelSwitchCntrl(msDisable);
            SetDeleteChannelTimeCntrl(msEnable);
            SetDeleteChannelSwitchCntrl(msDisable);
          end;

          if (lChannel.MasterControlFeature = nil) then
          begin
            SetDeleteChannel(msEnable);
            SetCopyChannel(msEnable);
            SetCreateDisbenefitFunction(msDisable);
            SetDeleteDisbenefitFunction(msDisable);
            SetCreateReturnFlowChannel(msDisable);
            SetDeleteReturnFlowChannel(msDisable);

          end;
          if (NOT (lChannel.ChannelType in [2,12])) then
          begin
            SetCopyChannel(msEnable);
            SetConvertChannel(msEnable);
          end;

          // Master Control channel
          if (lChannel.MasterControlFeature <> nil) AND (lMasterList.MasterControlFeatureCount > 1) then
          begin
            SetConvertChannel(msEnable);
            SetDeleteMasterControlFeature(msEnable);
            if (lChannel.DisbenefitFunction <> nil) then
            begin
              SetDeleteDisbenefitFunction(msEnable);
              SetCreateDisbenefitFunction(msDisable);
            end
            else
            begin
              SetCreateDisbenefitFunction(msEnable);
              SetDeleteDisbenefitFunction(msDisable);
            end;
            if (lChannel.ReturnFlowChannel <> nil) then
            begin
              SetCreateReturnFlowChannel(msDisable);
              SetDeleteReturnFlowChannel(msEnable);
            end
            else
            if (lChannel.ReturnFlowChannel = nil) then
            begin
              SetCreateReturnFlowChannel(msEnable);
              SetDeleteReturnFlowChannel(msDisable);
            end;
          end;
          // Pumping channel
          if (lChannel.PumpingFeature <> nil) then
          begin
            SetDeletePumpingFeature(msEnable);
            SetCopyPumpingFeature(msEnable);
          end;
          // Water demand
          if (lChannel.WaterDemandFeature <> nil) then
            SetDeleteWaterDemandFeature(msEnable);
          // Min-max channel
          if (lChannel.MinMaxFlowConstraint <> nil) then
          begin
            if (lChannel.PumpingFeature = nil) AND
               (lChannel.PhysicalFlowConstraint = nil) then
            begin
              SetCreatePumpingFeature(msEnable);
              SetCreatePhysicalFlowConstraint(msEnable);

              if (lChannel.WaterDemandFeature = nil) then
                SetCreateWaterDemandFeature(msEnable);

              if (lChannel.IFRFeature = nil) then
                SetCreateIFRFeature(msEnable)
              else
                SetCreatePhysicalFlowConstraint(msDisable);

            end;
          end;

           //IFR Channel
          if (lChannel.IFRFeature <> nil) then
          begin
            SetDeleteIFRFeature(msEnable);
            SetCopyIFRFeature(msEnable);
          end;

            // PhysicalFlowContraint
          if (lChannel.PhysicalFlowConstraint <> nil) then
          begin
            SetDeletePhysicalFlowConstraint(msEnable);
            SetCopyPhysicalFlowConstraint(msEnable);
          end;

          // Specified Demand channel
          if (lChannel.SpecifiedDemandFeature <> nil) AND (lChannel.WaterDemandFeature = nil) then
            SetCreateWaterDemandFeature(msEnable);

          // General flow channel
          if (lChannel.ChannelType = 12) then
          begin
            if (lChannel.PhysicalFlowConstraint = nil) AND
               (lChannel.IFRFeature = nil) then
            begin
              SetCreateFeature(msEnable);
              if (lChannel.DownStreamNode <> nil) AND
                 (lChannel.DownStreamNode.ReservoirConfigurationData.NodeType <> ntNodeWithoutInflow) then
                SetCreateSpecifiedInflowFeature(msEnable);
              if (FAppModules.StudyArea.ModelCode = CPlanning) OR
                 (lMasterList.MasterControlFeatureCount < 2) then
                SetCreateMasterControlFeature(msEnable);
            end;
            if (lChannel.PhysicalFlowConstraint <> nil) then
            begin
              SetDeletePhysicalFlowConstraint(msEnable);
              SetCopyPhysicalFlowConstraint(msEnable);
            end
            else
              SetCreatePhysicalFlowConstraint(msEnable);
          end;
            if(FAppModules.StudyArea.ModelSubCode = CPlanning) then
            begin
            //Check for property
              LRunCon := (FAppModules.Model.ModelData as IYieldModelData).RunConfigurationData;
              if LRunCon <> nil then
              begin
                if (LChannel.MultiResChannelCurtailByChannelNo[LChannel.ChannelNumber] <> nil) then
                  SetDeleteMultiChannelCurtailRule(msEnable)
                else
                if (LRunCon.AllocationControlOption = 'I') and (LChannel.MultiResChannelCurtailByChannelNo[LChannel.ChannelNumber] = nil) then
                  SetCreateMultiChannelCurtailRule(msEnable);
              end;

            end;
        end;
      end;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCreateIrrigationBlock(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCreateIrrigationBlock';
begin
  try
    if(FAppModules.StudyArea.ModelVersion <> '7') then Exit;
    if (AAction = msEnable) AND ((NOT (FAppModules.User.UserRights in CUR_EditData) OR
       FAppModules.StudyArea.ScenarioLocked)) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCreateIrrigationBlock, msDisable, 'ActionCreateIrrigationBlockDisabled')
    else
      FAppModules.SetMenuItem(CCreateIrrigationBlock, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCopyIrrigationBlock(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCopyIrrigationBlock';
begin
  try
    if(FAppModules.StudyArea.ModelVersion <> '7') then Exit;
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCopyIrrigationBlock, msDisable, 'ActionCopyIrrigationBlockDisabled')
    else
      FAppModules.SetMenuItem(CCopyIrrigationBlock, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetDeleteIrrigationBlock(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetDeleteIrrigationBlock';
begin
  try
    if(FAppModules.StudyArea.ModelVersion <> '7') then Exit;
    if (AAction = msEnable) AND ((NOT (FAppModules.User.UserRights in CUR_EditData) OR
       FAppModules.StudyArea.ScenarioLocked)) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CDeleteIrrigationBlock, msDisable, 'ActionDeleteIrrigationBlockDisabled')
    else
      FAppModules.SetMenuItem(CDeleteIrrigationBlock, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCreateWetland(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCreateWetland';
begin
  try
    if(FAppModules.StudyArea.ModelVersion <> '7') then Exit;
    if (AAction = msEnable) AND ((NOT (FAppModules.User.UserRights in CUR_EditData) OR
       FAppModules.StudyArea.ScenarioLocked)) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCreateWetland, msDisable, 'ActionCreateWetlandDisabled')
    else
      FAppModules.SetMenuItem(CCreateWetland, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCopyWetland(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCopyWetland';
begin
  try
    if(FAppModules.StudyArea.ModelVersion <> '7') then Exit;
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCopyWetland, msDisable, 'ActionCopyWetlandDisabled')
    else
      FAppModules.SetMenuItem(CCopyWetland, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetDeleteWetland(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetDeleteWetland';
begin
  try
    if(FAppModules.StudyArea.ModelVersion <> '7') then Exit;
    if (AAction = msEnable) AND ((NOT (FAppModules.User.UserRights in CUR_EditData) OR
       FAppModules.StudyArea.ScenarioLocked)) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CDeleteWetland, msDisable, 'ActionDeleteWetlandDisabled')
    else
      FAppModules.SetMenuItem(CDeleteWetland, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCreateYMDemandCentre(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCreateYMDemandCentre';
begin
  try
    if(FAppModules.StudyArea.ModelVersion <> '7') then Exit;
    if (AAction = msEnable) and ((not (FAppModules.User.UserRights in CUR_EditData) or
       FAppModules.StudyArea.ScenarioLocked)) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCreateYMDemandCentre, msDisable, 'ActionCreateYMDemandCentreDisabled')
    else
      FAppModules.SetMenuItem(CCreateYMDemandCentre, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TYieldModelMenuItemManager.SetCopyYMDemandCentre(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCopyYMDemandCentre';
begin
  try
    if(FAppModules.StudyArea.ModelVersion <> '7') then Exit;
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCopyYMDemandCentre, msDisable, 'ActionCopyYMDemandCentreDisabled')
    else
      FAppModules.SetMenuItem(CCopyYMDemandCentre, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetDeleteYMDemandCentre(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetDeleteYMDemandCentre';
begin
  try
    if(FAppModules.StudyArea.ModelVersion <> '7') then Exit;
    if (AAction = msEnable) AND ((NOT (FAppModules.User.UserRights in CUR_EditData) OR
       FAppModules.StudyArea.ScenarioLocked)) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CDeleteYMDemandCentre, msDisable, 'ActionDeleteYMDemandCentreDisabled')
    else
      FAppModules.SetMenuItem(CDeleteYMDemandCentre, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCreateSFRSubCatchment(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCreateSFRSubCatchment';
begin
  try
    if(FAppModules.StudyArea.ModelVersion <> '7') then Exit;
    if (AAction = msEnable) AND ((NOT (FAppModules.User.UserRights in CUR_EditData) OR
       FAppModules.StudyArea.ScenarioLocked)) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCreateSFRSubCatchment, msDisable, 'ActionCreateSFRSubCatchmentDisabled')
    else
      FAppModules.SetMenuItem(CCreateSFRSubCatchment, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCopySFRSubCatchment(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCopySFRSubCatchment';
begin
  try
    if(FAppModules.StudyArea.ModelVersion <> '7') then Exit;

    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then

      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCopySFRSubCatchment, msDisable, 'ActionCopySFRSubCatchmentDisabled')
    else
      FAppModules.SetMenuItem(CCopySFRSubCatchment, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetDeleteSFRSubCatchment(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetDeleteSFRSubCatchment';
begin
  try
    if(FAppModules.StudyArea.ModelVersion <> '7') then Exit;
    if (AAction = msEnable) AND ((NOT (FAppModules.User.UserRights in CUR_EditData) OR
       FAppModules.StudyArea.ScenarioLocked)) then
      AAction := msDisable;
    if (AAction = msDisable) then
     FAppModules.SetMenuItem(CDeleteSFRSubCatchment, msDisable, 'ActionDeleteSFRSubCatchmentDisabled')
    else
      FAppModules.SetMenuItem(CDeleteSFRSubCatchment, AAction);
   except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCreateYMDemandCentreReturnFlowFeature(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCreateYMDemandCentreReturnFlowFeature';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCreatePumpingFeature, msDisable, 'ActionCreatePumpingFeatureDisabled')
    else
      FAppModules.SetMenuItem(CCreatePumpingFeature, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetDeleteYMDemandCentreReturnFlowFeature(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetDeleteYMDemandCentreReturnFlowFeature';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCreatePumpingFeature, msDisable, 'ActionDeleteYMDemandCentreReturnFlowDisabled')
    else
      FAppModules.SetMenuItem(CCreatePumpingFeature, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetMenuGenerateSysConfigFiles(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetMenuGenerateSysConfigFiles';
begin
  try
    case AAction of
      msDisable : FAppModules.SetMenuItem(CGenerateSysConfigFiles, AAction, 'GenerateSysConfigFilesDisabled');
    else
      FAppModules.SetMenuItem(CGenerateSysConfigFiles, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCreateMine(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCreateMine';
begin
  try
    if(FAppModules.StudyArea.ModelVersion <> '7') then Exit;
    if (AAction = msEnable) AND ((NOT (FAppModules.User.UserRights in CUR_EditData) OR
       FAppModules.StudyArea.ScenarioLocked)) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCreateMine, msDisable, 'ActionCreateMineDisabled')
    else
      FAppModules.SetMenuItem(CCreateMine, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCopyMine(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCopyMine';
begin
  try
    if(FAppModules.StudyArea.ModelVersion <> '7') then Exit;
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCopyMine, msDisable, 'ActionCopyMineDisabled')
    else
      FAppModules.SetMenuItem(CCopyMine, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TYieldModelMenuItemManager.SetDeleteMine(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetDeleteMine';
begin
  try
    if(FAppModules.StudyArea.ModelVersion <> '7') then Exit;
    if (AAction = msEnable) AND ((NOT (FAppModules.User.UserRights in CUR_EditData) OR
       FAppModules.StudyArea.ScenarioLocked)) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CDeleteMine, msDisable, 'ActionDeleteMineDisabled')
    else
      FAppModules.SetMenuItem(CDeleteMine, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCreateDroughtRestriction(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCreateDroughtRestriction';
var
  LYieldModelDataObject : TYieldModelDataObject;
begin
  try
    LYieldModelDataObject := TYieldModelDataObject(FAppModules.Model.ModelData);
    if(FAppModules.StudyArea.ModelVersion <> '7') then Exit;
    if (AAction = msEnable) AND
       (((NOT (FAppModules.User.UserRights in CUR_EditData) OR
         FAppModules.StudyArea.ScenarioLocked)) OR
         (LYieldModelDataObject.NetworkFeaturesData.CurtailmentAndDrought.DroughtRestrictionCount >= 20)) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCreateDroughtRestriction, msDisable, 'ActionCreateDroughtRestrictionDisabled')
    else
      FAppModules.SetMenuItem(CCreateDroughtRestriction, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetDeleteDroughtRestriction(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetDeleteDroughtRestriction';
begin
  try
    if(FAppModules.StudyArea.ModelVersion <> '7') then Exit;
    if (AAction = msEnable) AND ((NOT (FAppModules.User.UserRights in CUR_EditData) OR
       FAppModules.StudyArea.ScenarioLocked)) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CDeleteDroughtRestriction, msDisable, 'ActionDeleteDroughtRestrictionDisabled')
    else
      FAppModules.SetMenuItem(CDeleteDroughtRestriction, AAction)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetScenarioCopyReservoir(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetScenarioCopyReservoir';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CScenarioCopyReservoir, msDisable, 'ActionScenarioCopyReservoirDisabled')
    else
      FAppModules.SetMenuItem(CScenarioCopyReservoir, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetScenarioCopyChannel(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetScenarioCopyChannel';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CScenarioCopyChannel, msDisable, 'ActionScenarioCopyChannelDisabled')
    else
      FAppModules.SetMenuItem(CScenarioCopyChannel, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetScenarioCopyIrrigationArea(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetScenarioCopyIrrigationArea';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CScenarioCopyIrrigationArea, msDisable, 'ActionScenarioCopyIrrigationAreaDisabled')
    else
      FAppModules.SetMenuItem(CScenarioCopyIrrigationArea, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetScenarioCopyIrrigationBlock(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetScenarioCopyIrrigationBlock';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CScenarioCopyIrrigationBlock, msDisable, 'ActionScenarioCopyIrrigationBlockDisabled')
    else
      FAppModules.SetMenuItem(CScenarioCopyIrrigationBlock, AAction);
   except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetScenarioCopyPowerPlant(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetScenarioCopyPowerPlant';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CScenarioCopyPowerPlant, msDisable, 'ActionScenarioCopyPowerPlantDisabled')
    else
      FAppModules.SetMenuItem(CScenarioCopyPowerPlant, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetScenarioCopyWetland(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetScenarioCopyWetland';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CScenarioCopyWetland, msDisable, 'ActionScenarioCopyWetlandDisabled')
    else
      FAppModules.SetMenuItem(CScenarioCopyWetland, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetScenarioCopyYMDemandCentre(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetScenarioCopyYMDemandCentre';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CScenarioCopyYMDemandCentre, msDisable, 'ActionScenarioCopyYMDemandCentreDisabled')
    else
      FAppModules.SetMenuItem(CScenarioCopyYMDemandCentre, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetScenarioCopySFRSubCatchment(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetScenarioCopySFRSubCatchment';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CScenarioCopySFRSubCatchment, msDisable, 'ActionScenarioCopySFRSubCatchmentDisabled')
    else
      FAppModules.SetMenuItem(CScenarioCopySFRSubCatchment, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetScenarioCopyMine(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetScenarioCopyMine';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CScenarioCopyMine, msDisable, 'ActionScenarioCopyMineDisabled')
    else
      FAppModules.SetMenuItem(CScenarioCopyMine, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetScenarioCopyGroundWater(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetScenarioCopyGroundWater';
begin
  try
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CScenarioCopyGroundWater, msDisable, 'ActionScenarioCopyGroundWaterDisabled')
    else
      FAppModules.SetMenuItem(CScenarioCopyGroundWater, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCreateGroundWater(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCreateGroundWater';
begin
  try
    if(FAppModules.StudyArea.ModelVersion <> '7') then Exit;
    if (AAction = msEnable) AND ((NOT (FAppModules.User.UserRights in CUR_EditData) OR
       FAppModules.StudyArea.ScenarioLocked)) then
      AAction := msDisable;
    if (AAction = msDisable) then
     FAppModules.SetMenuItem(CCreateGroundWater, msDisable, 'ActionCreateGroundWaterDisabled')
    else
      FAppModules.SetMenuItem(CCreateGroundWater, AAction);
   except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetDeleteGroundWater(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetDeleteGroundWater';
begin
  try
    if(FAppModules.StudyArea.ModelVersion <> '7') then Exit;
    if (AAction = msEnable) AND ((NOT (FAppModules.User.UserRights in CUR_EditData) OR
       FAppModules.StudyArea.ScenarioLocked)) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CDeleteGroundWater, msDisable, 'ActionDeleteGroundWaterDisabled')
    else
      FAppModules.SetMenuItem(CDeleteGroundWater, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelMenuItemManager.SetCopyGroundWater(AAction: TMenuSetAction);
const OPNAME = 'TYieldModelMenuItemManager.SetCopyGroundWater';
begin
  try
    if(FAppModules.StudyArea.ModelVersion <> '7') then Exit;
    if (AAction = msEnable) AND
       (NOT (FAppModules.User.UserRights in CUR_EditData) OR FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable;
    if (AAction = msDisable) then
      FAppModules.SetMenuItem(CCopyGroundWater, msDisable, 'ActionCopyGroundWaterDisabled')
    else
      FAppModules.SetMenuItem(CCopyGroundWater, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
