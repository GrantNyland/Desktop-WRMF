//
//
//  UNIT      : Contains TYieldModelManager Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 04/12/2001
//  COPYRIGHT : Copyright © 2001 DWAF
//
//

unit UYieldModelManager;

interface

uses
  Classes,
  VCL.ComCtrls,

  VoaimsCom_TLB,
  UAbstractObject,
  UYieldModelDataObject,
  UYieldModelDataGUIManager,
  UMenuItemManager,
  UWRYMRunOptions,
  UFileNames,
  UWizardForm,
  UViewDataItem,
  USystemModelManager,
  UTabSheetManager,
  UVisioTabSheetManager,
  UGridActionObject,
  UDataViewerManager,
  UResultsTabSheetManager,
  UDataTabSheetManager,
  UFileselectionManager,
  UGenericModelLinkClasses,
  UFilesActionAbstractManager,
  UStringListOfStringLists,
  UAppModulesWithDatabase,
  UYieldModelIterationTracker,
  UChannelPenaltyStructureDataLoadAgent;

type
  TYieldModelManager = class(TSystemModelManager,IYieldModel)
  protected
    FEmptyStudy                 : boolean;
    FMineCreated                : boolean;
    FChannelConvert             : boolean;
    FModelMenuItemManager       : TMenuItemManager;
    FNetworkVisualiserManager   : TVisioTabSheetManager;
    FModelCapabilityManager     : TTabSheetManager;
    FOutputReviewManager        : TTabSheetManager;
    FOutputComparison           : TTabSheetManager;
    FYieldReliabilityCurveManager: TTabSheetManager;

    FFileEditManager            : TAbstractFileEditManager;
    FModelDataGUIManager        : TYieldModelDataGUIManager      ;
    FFileSelectionManager       : TFileselectionManager;
    FModelFilesActionManager    : TFilesActionAbstractManager;

    //FResultsTabSheetManager     : TResultsTabSheetManager;
    FDataTabSheetManager        : TDataTabSheetManager;
    FWizardForm                 : TWizardForm;
    FModelData                  : TYieldModelDataObject;//TInterfacedObject;
    FYieldModelIterationTracker : TYieldModelIterationTracker;
    FWRYMRunOptions             : TWRYMRunOptions;
    FLoadingData                : boolean;
    FTimeSeriesComparitorManager: TDataViewerManager;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    procedure CreateModelData; virtual;
    procedure CreateFileActionManager; virtual;
    procedure CreateFileSelectionManager; virtual;
    procedure CreateModelDatasetConstructor; virtual;
    procedure DeleteModelDatasetConstructor; virtual;
    procedure CreateNetworkVisualiserManager; virtual;
    procedure CreateOutputReviewManager; virtual;
    procedure CreateOutputComparison; virtual;

    //procedure CreateResultsTabSheetManager; virtual;
    procedure CreateDataTabSheetManager; virtual;
    procedure CreateTimeSeriesComparitor;virtual;
    procedure CreateYieldReliabilityCurveManager; virtual;

    procedure CreateModelTabSheetManagers; virtual;
    procedure CreateModelMenuItemManager; virtual;
    procedure CreateFileEditManager; virtual;
    procedure CreateModelCapabilityManager; virtual;
    procedure CreateModelGUIManager; virtual;

    procedure SetSystemMenuState; virtual;
    procedure DoLaunchDocumentViewer(ADocumentKey : integer);

    function ShowNetworkVisualiser: boolean; virtual;
    function ShowFileEditor: boolean; virtual;

    function ShowGridEditor: boolean; virtual;
    function EditGridEditor(AData: TObject): boolean; virtual;
    function ShowGraph: boolean; virtual;

    function SelectModelFileNames: boolean; virtual;
    function DisplayModelFileNames: boolean; virtual;
    function SelectAdditionalFileNames: boolean; virtual;
    function RefreshHydrologyFileNames: boolean; virtual;
    function StudyDataIsEmpty: boolean; virtual;

    function LoadModelData: boolean; virtual;
    function LoadMiningData: boolean; virtual;
    function LoadConfigurationData: boolean; virtual;
    function LoadReservoirData: boolean; virtual;
    function LoadChannelData: boolean; virtual;
    function LoadReservoirPenaltyStructureData: boolean; virtual;
    function LoadParameterData: boolean; virtual;
    function LoadOutputData: boolean; virtual;
    function LoadOutputComparisonData: boolean;
    function LoadStudyMetaData: boolean;
    function LoadIFRSiteData: boolean;
    function LoadFieldFileReferencesData: Boolean;virtual;

    function InitialiseYearsInAnalysisFromParamDFile: boolean; virtual;
    function ImportParamAndPathsFile: boolean; virtual;
    function AddTreeViewsSubNodes: boolean; virtual;
    function AddMineNodes(AMine: IMine): boolean; virtual;
    function AddOutputWetlandChannels(AWetland: IWetland): boolean; virtual;
    function AddOutputIrrigationBlockChannels(AIrrigationBlock: IIrrigationBlock): boolean; virtual;
    function AddGroundWater(AGroundWater: IGroundWater): boolean;
    function AddOutputPowerPlantChannels(APowerPlant: IPowerPlant): boolean; virtual;
    function AddOutputDemandCentreChannels(LDemandCentre: IYMDemandCentre): boolean; virtual;

    function ProcessCustomModelEvent(AData: TModelMenuData): boolean; override;
    function ProcessParameterChangeEvent : boolean; override;
    function ProcessMetaDataEvent : boolean; override;
    function DoResultGridAction(AAction: TGridAction): boolean; virtual;
    function DoShowResultGraphs: boolean; virtual;
    function DoRefreshFileHints: boolean; virtual;
    function RefreshModelData: boolean; virtual;

    function UpdateTestFieldFieldValue(AFieldName: String; ANewValue: String;AOldValue: String; AContextData: TStrings): boolean;
    function DeleteTabSheetTreeNode(ATreeNode : TTreeNode) : boolean;

    function GetReviewParentIDs(AParentIDsCommatext : string): string;
    function DoWizardReservoir (AData : TObject)      : boolean;
    function DoWizardNodeWithInflow (AData : TObject) : boolean;
    function DoWizardChannel (AData : TObject)        : boolean;
    function DoWizardIrrigationArea (AData : TObject) : boolean;
    function DoWizardPowerPlant (AData : TObject)     : boolean;
    function DoConvertToGeneralFlow (AChannelNumber: Integer): WordBool;
    function DoDeleteAllFeatures (AChannelNumber: Integer): WordBool;
    function GetKeyValue (AKeyName   : string; AKeyValues : string) : string;
    function SetBaseValueRunConfiguration (AFieldPropName : string;
                                           AKeyValues     : string;
                                           AFieldIndex    : string;
                                           ANewValue      : string) : boolean;
    function GetBaseValueRunConfiguration (AFieldPropName : string;
                                           AKeyValues     : string;
                                           AFieldIndex    : string) : string;
    function SetBaseValueReservoirPenalty (AFieldPropName : string;
                                           AKeyValues     : string;
                                           AFieldIndex    : string;
                                           ANewValue      : string) : boolean;
    function GetBaseValueReservoirPenalty (AFieldPropName : string;
                                           AKeyValues     : string;
                                           AFieldIndex    : string) : string;
    function SetBaseValueChannelPenalty (AFieldPropName : string;
                                         AKeyValues     : string;
                                         AFieldIndex    : string;
                                         ANewValue      : string) : boolean;
    function GetBaseValueChannelPenalty (AFieldPropName : string;
                                         AKeyValues     : string;
                                         AFieldIndex    : string) : string;
    function SetBaseValuePumpingFeature (AFieldPropName : string;
                                         AKeyValues     : string;
                                         AFieldIndex    : string;
                                         ANewValue      : string) : boolean;
    function GetBaseValuePumpingFeature (AFieldPropName : string;
                                         AKeyValues     : string;
                                         AFieldIndex    : string) : string;
    function SetBaseValueLossFeature (AFieldPropName : string;
                                      AKeyValues     : string;
                                      AFieldIndex    : string;
                                      ANewValue      : string) : boolean;
    function GetBaseValueLossFeature (AFieldPropName : string;
                                      AKeyValues     : string;
                                      AFieldIndex    : string) : string;
    function SetBaseValueMinimumFlowFeature (AFieldPropName : string;
                                             AKeyValues     : string;
                                             AFieldIndex    : string;
                                             ANewValue      : string) : boolean;
    function GetBaseValueMinimumFlowFeature (AFieldPropName : string;
                                             AKeyValues     : string;
                                             AFieldIndex    : string) : string;
    function SetBaseValueMinMaxFlowFeature (AFieldPropName : string;
                                            AKeyValues     : string;
                                            AFieldIndex    : string;
                                            ANewValue      : string) : boolean;
    function GetBaseValueMinMaxFlowFeature (AFieldPropName : string;
                                            AKeyValues     : string;
                                            AFieldIndex    : string) : string;
    function SetBaseValueIFRFeature (AFieldPropName : string;
                                     AKeyValues     : string;
                                     AFieldIndex    : string;
                                     ANewValue      : string) : boolean;
    function GetBaseValueIFRFeature (AFieldPropName : string;
                                     AKeyValues     : string;
                                     AFieldIndex    : string) : string;
    function GetYieldModelDataObject: TYieldModelDataObject;
    function GetUnderGroundNodeNumber(AMineNumber, AUnderGroundIdentifier: integer): integer;
    function GetIrrigationFeatureID(ANodeNumber: integer): integer;
    procedure ResetMinMaxChannelFlow(AIFRFeature : IIFRFeature);
    procedure ShowSplashScreen;

    property YieldModelDataObject : TYieldModelDataObject read GetYieldModelDataObject;
  public
    function ModelName: string; override;
    function ResetState: boolean; override;
    function Initialise: boolean; override;
    function ModelDataLoaded: boolean; override;
    function PopulateDataList(AModelDataChangeType : TModelDataChangeType; AList : TList) : boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function LanguageHasChanged: boolean; override;
    function ProcessEvent(AEventType: integer; AData: TObject): boolean; override;
    function EditFieldValue(AFieldName, AOldValue : string; AContextData: TStrings): boolean; override;
    function EditFieldValue(AFieldName, AOldValue, ADefaultValue: string; AContextData: TStrings): boolean; override;
    function DoesFieldHaveACustomEditor(AFieldName: String): boolean; override;
    procedure DoModelDataHasChanged(AChangeLevel:TModelDataChangeLevel;AChangeType:TModelDataChangeType;
              AChangeAction:TModelDataChangeAction); override;
    function GetFileLineType(AFileObject: TObject; ALineNumber: Integer): string; override;
    function CanApplicationClose: boolean; override;
    procedure ApplicationIsClosing; override;
    function IsGraphLoaded: boolean; override;
    function IsGridLoaded: boolean; override;
    function GetModelViewItems(AItems: TStringListOfStringLists): boolean; override;
    function CreateTabSheetTreeViewElement(AViewID,AParentIDsCommatext   : string;
                                        AWeighting,
                                        AParentWeighting  : integer;
                                        ACaption    : string;
                                        ABitmapName : string;
                                        ADataType   : string;
                                        ASelect     : boolean;
                                        ATreeViewSheetName:TTreeViewSheetName) : boolean; override;
    function DeleteTabSheetTreeViewElement (AViewID, AParentIDsCommatext : string;
                                            AWeighting,AParentWeighting  : integer) : boolean; override;
    function CanCopyToCLipboard: boolean; override;
    function CanExport: boolean; override;
    function CanPrint: boolean; override;
    procedure DoCopyToCLipboard; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;

    function ViewInputDialog(AParent : TObject; ACommaTextContextData : String; AOwner : TObject = nil): boolean; override;
    function ViewOutputDialog(AParent : TObject; ACommaTextContextData : String; AOwner : TObject = nil): boolean; override;
    function ViewInputPopupDialog(AParent : TObject; ACommaTextContextData : String;  AOwner : TObject = nil): boolean; override;
    function ViewOutputPopupDialog(AParent : TObject; ACommaTextContextData : String;  AOwner : TObject = nil): boolean; override;
    function ViewOutputComparisonDialog(AParent : TObject; ACommaTextContextData : String;  AOwner : TObject = nil): boolean; override;

    function ResetYieldModelData            : boolean;

    function DoValidateSingleFile: boolean;
    function DoValidateAllFiles: WordBool; safecall;
    function DoExportAllFiles: WordBool; safecall;
    function DoExportSingleFile: boolean;
    function DoImportAllFiles: WordBool; safecall;
    function DoImportSingleFile: boolean;
    function DoClearModelData: WordBool; safecall;
    function DoReadYRCDataFromDB(AEvent: boolean = False): boolean;
    function DoReadYRCDataFromFile: boolean;
    function DoSaveYRCDataToDB: boolean;
    function DoDeleteYRCDataFromDB: boolean;

    function DoValidateModelData: boolean;
    function DoGenerateSystemConfigDataFiles : Boolean;
    function DoRunModel: WordBool; safecall;
    function DoRunStorageVsYield(AReservoirNumber: Integer;
                                 const AStartingStorageCommaText: WideString;
                                 var AMinTargetDraftCommaText: WideString;
                                 var AMaxTargetDraftCommaText: WideString;
                                 var AYieldCommaText: WideString): WordBool; safecall;
    function DoExportAllFilesAndRunModel(ASilent: WordBool): WordBool; safecall;
    function GetYieldChannelYield: Double; safecall;
    function DoImportDemandFile(const AFileNAme: WideString): boolean;
    function DoImportDamLevelsFile(const AFileNAme: WideString): boolean;
    function DoImportHydrologyFile(const AFileNAme: WideString): boolean;
    function DoImportParamFile(const AFileNAme: WideString): boolean;
    function DoClearParamFile(const AFileNAme: WideString): boolean;

    function DoCreateReservoir: IReservoirData; safecall;
    function DoDeleteReservoir(AReservoirNumber: Integer): WordBool; safecall;
    function DoCreateNodeWithInflow: IReservoirData; safecall;
    function DoDeleteNodeWithInflow(ANodeNumber: Integer): WordBool; safecall;
    function DoCreateNodeWithoutInflow: IReservoirData; safecall;
    function DoDeleteNodeWithoutInflow(ANodeNumber: Integer): WordBool; safecall;
    function DoCreateChannel(AUpStreamNodeNumber: Integer; ADownStreamNodeNumber: Integer): IGeneralFlowChannel; safecall;
    function DoDeleteChannel(AChannelNumber: Integer): WordBool; safecall;
    function DoConvertChannel(AChannelNumber: Integer): WordBool; safecall;
    function DoCreateMinimumFlowFeature(AChannelNumber: Integer): IMinimumFlowConstraint; safecall;
    function DoCreateMinMaxFlowFeature(AChannelNumber: Integer): IMinMaxFlowConstraint; safecall;
    function DoCreatePumpingFeature(AChannelNumber: Integer): IPumpingFeature; safecall;
    function DoCreateLossFeature(AChannelNumber: Integer): ILossFeature; safecall;
    function DoCreateSpecifiedDemandFeature(AChannelNumber: Integer): ISpecifiedDemandFeature; safecall;
    function DoCreateDiversionFeature(AChannelNumber: Integer): IDiversionFeature; safecall;
    function DoCreateSpecifiedInflowFeature(AChannelNumber: Integer): ISpecifiedInflowFeature; safecall;
    function DoCreateIFRFeature(AChannelNumber: Integer; AIFRType : TIFRFeatureReferenceFlowType): IIFRFeature; safecall;
    function DoCreatePhysicalFlowConstraint(AChannelNumber: Integer): IPhysicalFlowConstraint; safecall;
    function DoCreateIrrigationArea: IIrrigationArea; safecall;
    function DoCreateIrrigationBlock: IIrrigationBlock; safecall;
    function DoCreateWetland: IWetland; safecall;
    function DoCreateYMDemandCentre: IYMDemandCentre; safecall;
    function DoCreateSFRSubCatchment: IStreamFlowReduction; safecall;
    function DoCreatePowerPlant: IPowerPlant; safecall;
    function DoCreateDroughtRestriction: IDroughtRestriction; safecall;

    function DoDeletePumpingFeature(AChannelNumber: Integer): WordBool; safecall;
    function DoDeleteIFRFeature(AChannelNumber: Integer): WordBool; safecall;
    function DoDeletePhysicalFlowConstraint(AChannelNumber: Integer): WordBool; safecall;
    function DoDeleteIrrigationArea(ANodeNumber: Integer): WordBool; safecall;
    function DoDeleteIrrigationBlock(ABlockNodeNumber: Integer): WordBool; safecall;
    function DoDeleteWetland(ANodeNumber: Integer): WordBool; safecall;
    function DoDeleteYMDemandCentre(ANodeNumber: Integer): WordBool; safecall;
    function DoDeleteSFRSubCatchment(AIdentifier: Integer): WordBool; safecall;
    function DoDeletePowerPlant(AFeatureID: Integer): WordBool; safecall;

    function DoCreateMine: IMine; safecall;
    function DoDeleteMine(AMineNumber: Integer): WordBool; safecall;
    function DoCreateOpenCast(AMineNumber: integer) : IOpenCast; safecall;
    function DoDeleteOpenCast(AMineNumber,AOpenCastIdentifier: integer) : WordBool; safecall;
    function DoCreateUnderGround(AMineNumber: integer) : IUnderground; safecall;
    function DoDeleteUnderGround(AMineNumber,AUnderGroundIdentifier: integer) : WordBool; safecall;
    function DoCreateSlurryDump(AMineNumber: integer)  : ISlurryDump; safecall;
    function DoDeleteSlurryDump(AMineNumber,ASlurryDumpIdentifier: integer) : WordBool; safecall;

    function DoCreateMasterControlFeature(AChannelNumber: Integer): IMasterControlFeature; safecall;
    function DoDeleteMasterControlFeature(AChannelNumber: Integer): WordBool; safecall;
    function DoCreateWaterDemandFeature(AChannelNumber: Integer): IWaterDemandFeature; safecall;
    function DoDeleteWaterDemandFeature(AChannelNumber: Integer): WordBool; safecall;
    function DoDeleteDroughtRestriction(AIdentifier: Integer): WordBool; safecall;
    function DoCreateGroundWater: IGroundWater; safecall;
    function DoDeleteGroundWater(AGroundWaterID: Integer): WordBool; safecall;

    function DoCreateChannelTariff(AChannelNr : integer): WordBool; virtual;
    function DoDeleteChannelTariff(AChannelNr : integer): WordBool; virtual;

    function DoWizardNewReservoir                     : WordBool; safecall;
    function DoWizardNewNodeWithInflow                : WordBool; safecall;
    function DoWizardNewChannel(const AUpDownNodeNumbers : WideString): WordBool; safecall;
    function DoInvokeWizard                           : WordBool; safecall;
    function DoWizardRunYieldHistoric                 : WordBool; safecall;
    function DoWizardRunYieldStochastic               : WordBool; safecall;
    function DoWizardRunYieldYRC                      : WordBool; safecall;

    function SetUserExpertMode(AExpert: boolean): boolean;

    function ModelData: TInterfacedObject; override;
    function Get_YieldModelData: IYieldModelData; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function EntityDescription (AFieldPropName : string;
                                AKeyValues     : string;
                                AFieldIndex    : string) : string; override;
    function SetBaseValue (AFieldPropName : string;
                           AKeyValues     : string;
                           AFieldIndex    : string;
                           ANewValue      : string) : boolean; override;
    function GetBaseValue (AFieldPropName : string;
                           AKeyValues     : string;
                           AFieldIndex    : string) : string; override;
    function HandleVNVEvent(const AVisioApp: IUnknown; const AVisioDoc: IUnknown;
                            AVisioEventCode: Integer; const ASourceObj: IUnknown;
                            AEventID: Integer; AEventSeqNum: Integer; const ASubjectObj: IUnknown;
                            AMoreInfo: OleVariant): WordBool; safecall;
    function ProcessVNVSpecial(const AParameter: WideString): WordBool; safecall;
    procedure OnTabHasChanged (ASender: TObject); override;
    property YieldModelData: IYieldModelData read Get_YieldModelData;
    function GetModelDataSetKey : string; override;
    function GetChangeListWhereClause : string; override;
    function ReadFirmYieldFromDebugFile: Double; safecall;
    function ImportOutputFiles: WordBool; safecall;

    function ViewInputConfigurationDialog(const AViewName: WideString): WordBool; safecall;
    function ViewInputMasterControlChannelDialog(AChannelNumber: Integer): WordBool; safecall;
    function ViewInputReservoirDialog(AResevoirNumber: Integer): WordBool; safecall;
    function ViewInputNodeWithInflowDialog(ANodeNumber: Integer): WordBool; safecall;
    function ViewInputNodeWithoutInflowDialog(ANodeNumber: Integer): WordBool; safecall;
    function ViewInputChannelDialog(AChannelNumber: Integer): WordBool; safecall;
    function ViewInputPowerPlantDialog(APowerPlantNumber: Integer): WordBool; safecall;
    function ViewInputIrrigationAreaDialog(AIrrigationAreaNumber: Integer): WordBool; safecall;
    function ViewInputIrrigationBlockDialog(AIrrigationBlockNodeNr: Integer): WordBool; safecall;
    function ViewInputWetlandDialog(AWetlandNr: Integer): WordBool; safecall;
    function ViewInputStreamFlowReductionDialog(AStreamFlowReductionNumber: Integer): WordBool; safecall;
    function ViewInputDemandCentreDialog(AYMDemandCentreNr: Integer): WordBool; safecall;
    function ViewInputMineDialog(AMineNodeNr: Integer): WordBool; safecall;
    function ViewInputMinePCDDamDialog(APCDDamNr: Integer): WordBool; safecall;
    function ViewInputMineUndergroundDamDialog(AUndergroundDamNr: Integer): WordBool; safecall;

    function ViewOutputMasterControlChannelDialog(AChannelNumber: Integer): WordBool; safecall;
    function ViewOutputReservoirDialog(AResevoirNumber: Integer): WordBool; safecall;
    function ViewOutputNodeWithInflowDialog(ANodeNumber: Integer): WordBool; safecall;
    function ViewOutputNodeWithoutInflowDialog(ANodeNumber: Integer): WordBool; safecall;
    function ViewOutputChannelDialog(AChannelNumber: Integer): WordBool; safecall;
    function ViewOutputPowerPlantDialog(APowerPlantNumber: Integer): WordBool; safecall;
    function ViewOutputIrrigationAreaDialog(AIrrigationAreaNumber: Integer): WordBool; safecall;
    function ViewOutputIrrigationBlockDialog(AIrrigationBlockNodeNr: Integer): WordBool; safecall;
    function ViewOutputWetlandDialog(AWetlandNr: Integer): WordBool; safecall;
    function ViewOutputStreamFlowReductionDialog(AStreamFlowReductionNumber: Integer): WordBool; safecall;
    function ViewOutputDemandCentreDialog(AYMDemandCentreNr: Integer): WordBool; safecall;
    function ViewOutputMineDialog(AMineNodeNr: Integer): WordBool; safecall;
    function ViewOutputMinePCDDamDialog(APCDDamNr: Integer): WordBool; safecall;
    function ViewOutputMineUndergroundDamDialog(AUndergroundDamNr: Integer): WordBool; safecall;
    function ViewInputGroundwaterDialog(AAquiferNodeNr: Integer): WordBool; safecall;
    function ViewOutputGroundwaterDialog(AAquiferNodeNr: Integer): WordBool; safecall;
    //function DoStationFilter(AChangeListID : integer) : WordBool;override;
    function Get_WRYMRunOptions: IWRYMRunOptions; safecall;
    function Get_YieldModelIterationTracker : IYieldModelIterationTracker; safecall;

    function DoCopyIFRFeature(AChannelNumber: Integer): IIFRFeature; safecall;
    function DoCopyChannel(AChannelNumber: Integer): IGeneralFlowChannel; safecall;
    function DoCopyMinimumFlowFeature(AChannelNumber: Integer): IMinimumFlowConstraint; safecall;
    function DoCopyMinMaxFlowFeature(AChannelNumber: Integer): IMinMaxFlowConstraint; safecall;
    function DoCopyLossFeature(AChannelNumber: Integer): ILossFeature; safecall;
    function DoCopySpecifiedDemandFeature(AChannelNumber: Integer): ISpecifiedDemandFeature; safecall;
    function DoCopyDiversionFeature(AChannelNumber: Integer): IDiversionFeature; safecall;
    function DoCopySpecifiedInflowFeature(AChannelNumber: Integer): ISpecifiedInflowFeature; safecall;
    function CopyPhysicalFlowConstraint(AChannelNumber: Integer): IPhysicalFlowConstraint; safecall;
    function DoCopyMasterControlFeature(AChannelNumber: Integer): IMasterControlFeature; safecall;
    function DoCopyPumpingFeature(AChannelNumber: Integer): IPumpingFeature; safecall;
    function DoCopyWaterDemandFeature(AChannelNumber: Integer): IWaterDemandFeature; safecall;
    function DoCopyPowerPlant(AChannelNumber: Integer): IPowerPlant; safecall;
    function DoCopyIrrigationArea(AFeatureID: Integer): IIrrigationArea; safecall;
    function DoCopyIrrigationBlock(AFeatureID: Integer): IIrrigationBlock; safecall;
    function DoCopyWetland(AWetlandID : integer):IWetland;safecall;
    function DoCopySFRSubCatchment(AStreamFlowReductionID: Integer): IStreamFlowReduction; safecall;
    function DoCopyYMDemandCentre(ANodeNumber: Integer): IYMDemandCentre; safecall;
    function DoCopyMine(AMineNumber: integer):IMine; safecall;
    function DoCopyGroundWater(AGroundWaterNumber: integer): IGroundWater; safecall;
    function DoCopyReservoir(AReservoirNumber: Integer): IReservoirData; safecall;
    function DoCopyReservoirFromScenario: WordBool;safecall;

    function DoCopyChannelFromScenario: WordBool;safecall;
    function DoCopyIrrigationAreaFromScenario: WordBool;safecall;
    function DoCopyPowerPlantFromScenario: WordBool;safecall;
    function DoCopyIrrigationBlockFromScenario: WordBool;safecall;
    function DoCopyWetlandFromScenario:WordBool;safecall;
    function DoCopyYMDemandCentreFromScenario:WordBool;safecall;
    function DoCopySFRFromScenario:WordBool;safecall;
    function DoCopyMineFromScenario:WordBool;safecall;
    function DoCopyGroundWaterFromScenario:WordBool;safecall;
    function StudyPropertiesCommaText: WideString; safecall;

    property YieldReliabilityCurveManager: TTabSheetManager read FYieldReliabilityCurveManager;
    property TimeSeriesComparitorManager: TDataViewerManager read FTimeSeriesComparitorManager;

  end;

implementation

uses
  windows,
  SysUtils,
  System.UITypes,
  VCL.Dialogs,
  VCL.Controls,
  VCL.Forms,

  UConstants,
  UUtilities,
  UChannelData,
  UReservoirData,
  UPowerPlants,

  UFilesActionYieldManager,
  UYieldFileSelectionManager,

  UDataSetType,
  UTreeViewTabSheet,
  USQLDatabaseLayer,
  UAbstractFileNamesObject,
  UMainMenuEventType,
  UStringFieldOperations,
  UReservoirDataLoadAgent,
  UOutputComparisonLoadAgent,
  UYieldModelDatasetConstructor,
  UYieldModelMenuItemManager,
  UReservoirPenaltyStructureDataLoadAgent,
  URunConfigurationDataLoadAgent,
  UChannelDataLoadAgent,
  UYRCModelDataLoadAgent,
  UNetworkFeaturesLoadAgent,
  UParameterDataLoadAgent,
  UYieldModelDataLoadAgent,
  UOutputDataLoadAgent,
  UMineLoadAgent,
  UIFRDataLoadAgent,
  UStudyMetaDataLoadAgent,
  UAbstractComponent,
  UYieldModelWizards,
  UNetworkFeaturesData,
  UDataFileObjects,
  VarUtils,
  UIsWordReaderInstalled,
  UWordDocumentLauncher,
  UIsAcrobatReaderInstalled,
  UPDFDocumentLauncher,
  UDynamicTreeViewTabSheet,
  UErrorHandlingOperations,
  UCurtailmentAndDrought,
  UIrrigationBlock,
  UWetland,
  UStreamFlowReduction,
  UYMDemandCentre,
  UCopyScenarioDataManager,
  UGroundWater,
  UStudyMetaData,
  UImportParamDialog,
  UYieldModelDataGUIForm,
  UCautionarySplashScreenValidator;

procedure TYieldModelManager.CreateMemberObjects;
const OPNAME = 'TYieldModelManager.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FLoadingData := False;
    FEmptyStudy  := False;
    FRefCount := 10000;
    FMineCreated := False;
    FChannelConvert := False;
    // Create the correct model data.
    CreateModelData;

    // Create these managers first(They may be used by other constructors).
    CreateModelDatasetConstructor;

    // Create the DLL based managers.
    CreateModelTabSheetManagers;

    // Create these managers last(They check if othe managers are created).
    CreateModelMenuItemManager;

    // General
    SetLength(FCustomFieldEditArray,0);

    // Create the model specific manager
    CreateModelGUIManager;

    // Create the application based managers.
    CreateFileActionManager;
    CreateFileSelectionManager;
    CreateModelCapabilityManager;

    FWizardForm := nil;

    FYieldModelIterationTracker := TYieldModelIterationTracker.Create(FAppModules);
    FWRYMRunOptions             := TWRYMRunOptions.Create(FAppModules);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelManager.DestroyMemberObjects;
const OPNAME = 'TYieldModelManager.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil(FModelMenuItemManager);
    FreeAndNil(FModelDataGUIManager);
    FreeAndNil(FModelData);
    FreeAndNil(FYieldModelIterationTracker);
    FreeAndNil(FWRYMRunOptions);
    SetLength(FCustomFieldEditArray,0);
    FCustomFieldEditArray := nil;
    DeleteModelDatasetConstructor;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager._AddRef: Integer;
const OPNAME = 'TYieldModelManager._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager._Release: Integer;
const OPNAME = 'TYieldModelManager._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.Initialise: boolean;
const OPNAME = 'TYieldModelManager.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := YieldModelDataObject.Initialise;
    Result := Result and FModelDataGUIManager.Initialise;
    if Assigned(FModelMenuItemManager) then
      Result := Result and FModelMenuItemManager.Initialise;
    if Assigned(FFileEditManager) then
      FFileEditManager.SetOnFileSave(DoValidateSingleFile);
    if Assigned(FModelCapabilityManager) then
      FModelCapabilityManager.Initialise;

    if Assigned(FYieldReliabilityCurveManager) then
      Result := Result and FYieldReliabilityCurveManager.Initialise;

    if Assigned(FTimeSeriesComparitorManager) then
      Result := Result and FTimeSeriesComparitorManager.Initialise;

    if Assigned(FAppModules.MainForm()) then
        FAppModules.MainForm.ApplyPreviousTabIndex;

    FYieldModelIterationTracker.Initialise;
    FWRYMRunOptions.Initialise;
    SetUserExpertMode(FAppModules.User.UserType = utExpert);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelManager.CreateModelTabSheetManagers;
const OPNAME = 'TYieldModelManager.CreateModelTabSheetManagers';
begin
  try
    CreateNetworkVisualiserManager;
    if not Assigned(FAppModules.MainForm()) then Exit;
    CreateDataTabSheetManager;
    CreateFileEditManager;
    CreateOutputReviewManager;
    CreateTimeSeriesComparitor;
    CreateYieldReliabilityCurveManager;
    CreateOutputComparison;

    //CreateResultsTabSheetManager;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelManager.CreateFileEditManager;
const OPNAME = 'TYieldModelManager.CreateFileEditManager';
begin
  try
    if not Assigned(FAppModules.MainForm()) then Exit;
    CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\FileEditor.dll',
      TAbstractAppObject(FFileEditManager), FAppModules, False, OPNAME);
    if Assigned(FFileEditManager) then
    begin
      FOwnedAppObjects.Add(FFileEditManager);
      FFileEditManager.TabSheet.PageControl := FAppModules.MainForm.PageControl;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelManager.CreateModelCapabilityManager;
const OPNAME = 'TYieldModelManager.CreateModelCapabilityManager';
begin
  try
    if not Assigned(FAppModules.MainForm()) then Exit;
    CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\ModelCapability.dll',
      TAbstractAppObject(FModelCapabilityManager), FAppModules, False, OPNAME);
    if Assigned(FModelCapabilityManager) then
    begin
      FOwnedAppObjects.Add(FModelCapabilityManager);
      FModelCapabilityManager.TabSheet.PageControl := FAppModules.MainForm.PageControl;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelManager.CreateNetworkVisualiserManager;
const OPNAME = 'TYieldModelManager.CreateNetworkVisualiserManager';
begin
  try
    CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\VisioNetworkVisualiser.dll',
      TAbstractAppObject(FNetworkVisualiserManager), FAppModules, False, OPNAME);
    if Assigned(FNetworkVisualiserManager) then
    begin
      FOwnedAppObjects.Add(FNetworkVisualiserManager);
      if Assigned(FAppModules.MainForm()) then
        FNetworkVisualiserManager.TabSheet.PageControl := FAppModules.MainForm.PageControl;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldModelManager.CreateOutputReviewManager;
const OPNAME = 'TYieldModelManager.CreateOutputReviewManager';
begin
  if not Assigned(FAppModules.MainForm()) then Exit;
  try
    CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\OutputReview.dll',
      TAbstractAppObject(FOutputReviewManager), FAppModules, False, OPNAME);
    if Assigned(FOutputReviewManager) then
    begin
      FOwnedAppObjects.Add(FOutputReviewManager);
      FOutputReviewManager.TabSheet.PageControl := FAppModules.MainForm.PageControl;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldModelManager.CreateOutputComparison;
const OPNAME = 'TYieldModelManager.CreateOutputComparison';
begin
  if not Assigned(FAppModules.MainForm()) then Exit;
  try
    CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\OutputComparison.dll',
      TAbstractAppObject(FOutputComparison), FAppModules, False, OPNAME);
    if Assigned(FOutputComparison) then
    begin
      FOwnedAppObjects.Add(FOutputComparison);
      FOutputComparison.TabSheet.PageControl := FAppModules.MainForm.PageControl;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TYieldModelManager.CreateModelData;
const OPNAME = 'TYieldModelManager.CreateModelData';
begin
  inherited;
  try
    FModelData := TYieldModelDataObject.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldModelManager.CreateFileActionManager;
const OPNAME = 'TYieldModelManager.CreateFileActionManager';
begin
  try
    FModelFilesActionManager := TFilesActionYieldManager.Create(FAppModules);
    if Assigned(FModelFilesActionManager) then
    begin
      FOwnedAppObjects.Add(FModelFilesActionManager);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldModelManager.CreateFileSelectionManager;
const OPNAME = 'TYieldModelManager.CreateFileSelectionManager';
begin
  try
    FFileSelectionManager := TYieldFileSelectionManager.Create(FAppModules);
    if Assigned(FFileSelectionManager) then
    begin
      FOwnedAppObjects.Add(FFileSelectionManager);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldModelManager.CreateDataTabSheetManager;
const OPNAME = 'TYieldModelManager.CreateDataTabSheetManager';
begin
  try
    if not Assigned(FAppModules.MainForm()) then Exit;
    FDataTabSheetManager := nil;
    if TDataTabSheetManager.CanDataTabBeCreated then
      FDataTabSheetManager := TDataTabSheetManager.Create(FAppModules);
    if Assigned(FDataTabSheetManager) then
    begin
      FOwnedAppObjects.Add(FDataTabSheetManager);
      FDataTabSheetManager.TabSheet.PageControl := FAppModules.MainForm.PageControl;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldModelManager.CreateYieldReliabilityCurveManager;
const OPNAME = 'TYieldModelManager.CreateYieldReliabilityCurveManager';
begin
  if not Assigned(FAppModules.MainForm()) then Exit;
  try
    CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\YieldReliabilityCurve.dll',
      TAbstractAppObject(FYieldReliabilityCurveManager), FAppModules, False, OPNAME);
    if Assigned(FYieldReliabilityCurveManager) then
    begin
      FOwnedAppObjects.Add(FYieldReliabilityCurveManager);
      FYieldReliabilityCurveManager.TabSheet.PageControl := FAppModules.MainForm.PageControl;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;
procedure TYieldModelManager.CreateTimeSeriesComparitor;
const OPNAME = 'TYieldModelManager.CreateTimeSeriesComparitor';
begin
  if not Assigned(FAppModules.MainForm()) then Exit;
  try
    if not FileExists(ExtractFilePath(ApplicationExeName) + 'bin\TimeSeriesComparitor.dll') then Exit;
    CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\TimeSeriesComparitor.dll',
      TAbstractAppObject(FTimeSeriesComparitorManager), FAppModules, False, OPNAME);
    if Assigned(FTimeSeriesComparitorManager) then
    begin
      FOwnedAppObjects.Add(FTimeSeriesComparitorManager);
      FTimeSeriesComparitorManager.TabSheet.PageControl := FAppModules.MainForm.PageControl;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;


{procedure TYieldModelManager.CreateResultsTabSheetManager;
const OPNAME = 'TYieldModelManager.CreateResultsTabSheetManager';
begin
  try
    if not Assigned(FAppModules.MainForm()) then Exit;
    FResultsTabSheetManager := nil;
    if TResultsTabSheetManager.CanResultsTabBeCreated then
      FResultsTabSheetManager := TResultsTabSheetManager.Create(FAppModules);
    if Assigned(FResultsTabSheetManager) then
    begin
      FOwnedAppObjects.Add(FResultsTabSheetManager);
      FResultsTabSheetManager.TabSheet.PageControl := FAppModules.MainForm.PageControl;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;
 }
procedure TYieldModelManager.CreateModelDatasetConstructor;
const OPNAME = 'TYieldModelManager.CreateModelDatasetConstructor';
begin
  try
    // Add the data set constructor.
    TSQLDatabaseLayer(FAppModules.Database).AddDataSetConstructor(TYieldModelDatasetConstructor.Create(FAppModules));

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelManager.CreateModelGUIManager;
const OPNAME = 'TYieldModelManager.CreateModelGUIManager';
begin
  try
    FModelDataGUIManager := TYieldModelDataGUIManager.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelManager.DeleteModelDatasetConstructor;
const OPNAME = 'TYieldModelManager.DeleteModelDatasetConstructor';
begin
  try
    TSQLDatabaseLayer(FAppModules.Database).DeleteDataSetConstructorsOfType(TYieldModelDatasetConstructor);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.LanguageHasChanged: boolean;
const OPNAME = 'TYieldModelManager.LanguageHasChanged';
begin
  if FLoadingData then
  begin
    Result := True;
    Exit;
  end;
  Result := inherited LanguageHasChanged;
  try
    if Result and Assigned(FModelMenuItemManager) then
        Result := FModelMenuItemManager.LanguageHasChanged;
    if Result and Assigned(FNetworkVisualiserManager) then
        Result := FNetworkVisualiserManager.LanguageHasChanged;
    if Result and Assigned(FOutputReviewManager) then
        Result := FOutputReviewManager.LanguageHasChanged;
    if Result and Assigned(FOutputComparison) then
        Result := FOutputComparison.LanguageHasChanged;

    if Result and Assigned(FTimeSeriesComparitorManager) then
        Result := FTimeSeriesComparitorManager.LanguageHasChanged;

    if Result and Assigned(FFileEditManager) then
        Result := FFileEditManager.LanguageHasChanged;
    {if Result and Assigned(FResultsTabSheetManager) then
        Result := FResultsTabSheetManager.LanguageHasChanged;
        }
    if Result and Assigned(FDataTabSheetManager) then
        Result := FDataTabSheetManager.LanguageHasChanged;
    if Result and Assigned(FModelFilesActionManager) then
        Result := FModelFilesActionManager.LanguageHasChanged;
    if Result and Assigned(FFileSelectionManager) then
        Result := FFileSelectionManager.LanguageHasChanged;

    if Assigned(FYieldReliabilityCurveManager) then
      Result := Result and FYieldReliabilityCurveManager.LanguageHasChanged;

    if Assigned(FTimeSeriesComparitorManager) then
      Result := Result and FTimeSeriesComparitorManager.LanguageHasChanged;

    if Result and Assigned(FModelCapabilityManager) then
        Result := FModelCapabilityManager.LanguageHasChanged;
    Result := Result and FModelDataGUIManager.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.ModelDataLoaded: boolean;
const OPNAME = 'TYieldModelManager.ModelDataLoaded';
begin
  Result := False;
  try
    Result := True;
   if Assigned(YieldModelDataObject.CastFileNamesObject) and
      Assigned(YieldModelDataObject.CastFileNamesObject.ConfigFileNames) and
      Assigned(YieldModelDataObject.CastFileNamesObject.ParamFileNames) and
      Assigned(YieldModelDataObject.CastFileNamesObject.DirectoryFileNames) and
      Assigned(YieldModelDataObject.CastFileNamesObject.ConfigFileNames.FileNameObject[00]) and
      Assigned(YieldModelDataObject.CastFileNamesObject.ConfigFileNames.FileNameObject[01]) and
      Assigned(YieldModelDataObject.CastFileNamesObject.ConfigFileNames.FileNameObject[02]) and
      Assigned(YieldModelDataObject.CastFileNamesObject.ConfigFileNames.FileNameObject[03]) and
      Assigned(YieldModelDataObject.CastFileNamesObject.ConfigFileNames.FileNameObject[04]) and
      Assigned(YieldModelDataObject.CastFileNamesObject.ConfigFileNames.FileNameObject[05]) and
      Assigned(YieldModelDataObject.CastFileNamesObject.ConfigFileNames.FileNameObject[06]) and
      Assigned(YieldModelDataObject.CastFileNamesObject.ConfigFileNames.FileNameObject[07]) and
      Assigned(YieldModelDataObject.CastFileNamesObject.ConfigFileNames.FileNameObject[08]) and
      Assigned(YieldModelDataObject.CastFileNamesObject.ConfigFileNames.FileNameObject[09]) and
      Assigned(YieldModelDataObject.CastFileNamesObject.ConfigFileNames.FileNameObject[10]) and
      Assigned(YieldModelDataObject.CastFileNamesObject.ConfigFileNames.FileNameObject[11]) and
      Assigned(YieldModelDataObject.CastFileNamesObject.ConfigFileNames.FileNameObject[12]) and
      Assigned(YieldModelDataObject.CastFileNamesObject.ConfigFileNames.FileNameObject[13]) and
      Assigned(YieldModelDataObject.CastFileNamesObject.ParamFileNames.FileNameObject[00]) and
      Assigned(YieldModelDataObject.CastFileNamesObject.DirectoryFileNames.FileNameObject[00]) then
   begin
     Result :=
              TFileNameObject(YieldModelDataObject.CastFileNamesObject.ConfigFileNames.FileNameObject[00]).SavedInDB or
              TFileNameObject(YieldModelDataObject.CastFileNamesObject.ConfigFileNames.FileNameObject[01]).SavedInDB or
              TFileNameObject(YieldModelDataObject.CastFileNamesObject.ConfigFileNames.FileNameObject[02]).SavedInDB or
              TFileNameObject(YieldModelDataObject.CastFileNamesObject.ConfigFileNames.FileNameObject[03]).SavedInDB or
              TFileNameObject(YieldModelDataObject.CastFileNamesObject.ConfigFileNames.FileNameObject[04]).SavedInDB or
              TFileNameObject(YieldModelDataObject.CastFileNamesObject.ConfigFileNames.FileNameObject[05]).SavedInDB or
              TFileNameObject(YieldModelDataObject.CastFileNamesObject.ConfigFileNames.FileNameObject[06]).SavedInDB or
              TFileNameObject(YieldModelDataObject.CastFileNamesObject.ConfigFileNames.FileNameObject[07]).SavedInDB or
              TFileNameObject(YieldModelDataObject.CastFileNamesObject.ConfigFileNames.FileNameObject[08]).SavedInDB or
              TFileNameObject(YieldModelDataObject.CastFileNamesObject.ConfigFileNames.FileNameObject[09]).SavedInDB or
              TFileNameObject(YieldModelDataObject.CastFileNamesObject.ConfigFileNames.FileNameObject[10]).SavedInDB or
              TFileNameObject(YieldModelDataObject.CastFileNamesObject.ConfigFileNames.FileNameObject[11]).SavedInDB or
              TFileNameObject(YieldModelDataObject.CastFileNamesObject.ConfigFileNames.FileNameObject[12]).SavedInDB or
              TFileNameObject(YieldModelDataObject.CastFileNamesObject.ParamFileNames.FileNameObject[00]).SavedInDB or
              TFileNameObject(YieldModelDataObject.CastFileNamesObject.DirectoryFileNames.FileNameObject[00]).SavedInDB;
   end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.ProcessEvent(AEventType: integer; AData: TObject): boolean;
const OPNAME = 'TYieldModelManager.ProcessEvent';
var
  LCanChange: boolean;
  lListObject : TStringList;
begin
  Result := True;
  try
    case AEventType of
      CmeWRMFReleaseNote,
      CmeWRYMUserGuide,
      CmeWRYMProceduralManual,
      CmeWRYMTrainingMaterial,
      CmeWRYMParameterTables,
      CmeWRPMReleaseNote,
      CmeWRPMUserGuide,
      CmeWRPMParameterTables,
      CmeHydrologyParameterTables     : DoLaunchDocumentViewer(AEventType);

      CmeReloadModelData              : RefreshModelData;
      CmeViewFileEdit                 : ShowFileEditor;
      CmeViewNetworkVisualiser        : ShowNetworkVisualiser;
      CmeViewEditGrid                 : ShowGridEditor;
      CmeEditGridEditor               : EditGridEditor(AData);
      CmeViewGraph                    : ShowGraph;
      CmeValidateFile                 : DoValidateSingleFile;
      CmeImportFile                   : DoImportSingleFile;
      CmeExportFile                   : DoExportSingleFile;
      CmeValidateFiles                : DoValidateAllFiles;
      CmeImportFiles                  : DoImportAllFiles;
      CmeExportFiles                  : DoExportAllFiles;
      CmeClearModelData               : DoClearModelData;
      CmeRunModel                     : DoRunModel;
      CmeValidateModelData            : DoValidateModelData;
      CmeGenerateSysConfigFiles       : DoGenerateSystemConfigDataFiles;
      CmeExportFilesAndRunModel       : DoExportAllFilesAndRunModel(AData <> nil);
      CmeYRCLoadFromDB                : DoReadYRCDataFromDB(True);
      CmeYRCLoadFromFile              : DoReadYRCDataFromFile;
      CmeYRCSaveChart                 : DoSaveYRCDataToDB;
      CmeYRCDeleteChart               : DoDeleteYRCDataFromDB;
      CmeViewResultGraphs             : DoShowResultGraphs;
      CmeRefreshFileHints             : DoRefreshFileHints;
      CmeResultPageControlChanged     : OnTabHasChanged(nil);
      CmeCreateReservoir              : DoCreateReservoir;
      CmeDeleteReservoir              : DoDeleteReservoir(-1);
      CmeCreateNodeWithInflow         : DoCreateNodeWithInflow;
      CmeDeleteNodeWithInflow         : DoDeleteNodeWithInflow(-1);
      CmeCreateNodeWithoutInflow      : DoCreateNodeWithoutInflow;
      CmeDeleteNodeWithoutInflow      : DoDeleteNodeWithoutInflow(-1);
      CmeExpertUser                   : SetUserExpertMode(True);
      CmeStandardUser                 : SetUserExpertMode(False);
      CmeCreateChannel                :
      begin
        if Assigned(AData) then
        begin
          lListObject
           := TStringList(AData);
          case lListObject.Count of
            0 : DoCreateChannel(0, 0);
            1 : DoCreateChannel(StrToInt(lListObject.Strings[0]), 0);
          else
            DoCreateChannel(StrToInt(lListObject.Strings[0]),StrToInt(lListObject.Strings[1]));
          end;
        end
        else
          DoCreateChannel(0, 0);
      end;
      CmeCopyChannel                  : DoCopyChannel(-1);
      CmeDeleteChannel                : DoDeleteChannel(-1);
      CmeConvertChannel               : DoConvertChannel(-1);
      CmeCreateMinimumFlowFeature     : DoCreateMinimumFlowFeature(-1);
      CmeCreateMinMaxFlowFeature      : DoCreateMinMaxFlowFeature(-1);
      CmeCreatePumpingFeature         : DoCreatePumpingFeature(-1);
      CmeCopyPumpingFeature           : DoCopyPumpingFeature(-1);
      CmeDeletePumpingFeature         : DoDeletePumpingFeature(-1);
      CmeCreateLossFeature            : DoCreateLossFeature(-1);
      CmeCreateSpecifiedDemandFeature : DoCreateSpecifiedDemandFeature(-1);
      CmeCreateDiversionFeature       : DoCreateDiversionFeature(-1);
      CmeCreateSpecifiedInflowFeature : DoCreateSpecifiedInflowFeature(-1);
      CmeCreateIFRFeature             : DoCreateIFRFeature(-1,ifrtNone);
      CmeCopyIFRFeature               : DoCopyIFRFeature(-1);
      CmeDeleteIFRFeature             : DoDeleteIFRFeature(-1);
      CmeCreatePhysicalFlowConstraint : DoCreatePhysicalFlowConstraint(-1);
      CmeCopyPhysicalFlowConstraint   : CopyPhysicalFlowConstraint(-1);
      CmeDeletePhysicalFlowConstraint : DoDeletePhysicalFlowConstraint(-1);
      CmeCreateIrrigationArea         : DoCreateIrrigationArea;
      CmeCopyIrrigationArea           : DoCopyIrrigationArea(-1);
      CmeCopyIrrigationBlock          : DoCopyIrrigationBlock(-1);
      CmeDeleteIrrigationArea         : DoDeleteIrrigationArea(-1);
      CmeCreateIrrigationBlock        : DoCreateIrrigationBlock;
      CmeDeleteIrrigationBlock        : DoDeleteIrrigationBlock(-1);
      CmeCreateWetland                : DoCreateWetland;
      CmeCopyWetland                  : DoCopyWetland(-1);
      CmeDeleteWetland                : DoDeleteWetland(-1);
      CmeCreateYMDemandCentre         : DoCreateYMDemandCentre;
      CmeCopyYMDemandCentre           : DoCopyYMDemandCentre(-1);
      CmeDeleteYMDemandCentre         : DoDeleteYMDemandCentre(-1);
      CmeCreateSFRSubCatchment        : DoCreateSFRSubCatchment;
      CmeCopySFRSubCatchment          : DoCopySFRSubCatchment(-1);
      CmeDeleteSFRSubCatchment        : DoDeleteSFRSubCatchment(-1);
      CmeCreatePowerPlant             : DoCreatePowerPlant;
      CmeCopyPowerPlant               : DoCopyPowerPlant(-1);
      CmeDeletePowerPlant             : DoDeletePowerPlant(-1);
      CmeCreateMasterControlFeature   : DoCreateMasterControlFeature(-1);
      CmeDeleteMasterControlFeature   : DoDeleteMasterControlFeature(-1);
      CmeCreateWaterDemandFeature     : DoCreateWaterDemandFeature(-1);
      CmeDeleteWaterDemandFeature     : DoDeleteWaterDemandFeature(-1);

      CmeCreateMine                   : DoCreateMine;
      CmeCopyMine                     : DoCopyMine(-1);
      CmeDeleteMine                   : DoDeleteMine(-1);

      CmeCreateDroughtRestriction     : DoCreateDroughtRestriction;
      CmeDeleteDroughtRestriction     : DoDeleteDroughtRestriction(-1);
      CmeCopyReservoir                : DoCopyReservoir(-1);

      CmeCreateGroundWater            : DoCreateGroundWater;
      CmeDeleteGroundWater            : DoDeleteGroundWater(-1);
      CmeCopyGroundWater              : DoCopyGroundWater(-1);

      CmeWizardNewReservoir           : DoWizardNewReservoir;
      CmeWizardNewNodeWithInflow      : DoWizardNewNodeWithInflow;
      CmeWizardNewChannel             :
        begin
          if Assigned(AData) then
            DoWizardNewChannel(TStringList(AData).CommaText)
          else
            DoWizardNewChannel('');
        end;
      CmeInvokeWizard                 : DoInvokeWizard;
      CmeWizardRunYieldHistoric       : DoWizardRunYieldHistoric;
      CmeWizardRunYieldStochastic     : DoWizardRunYieldStochastic;
      CmeWizardRunYieldYRC            : DoWizardRunYieldYRC;

      CmeCopyReservoirFromScenario       : DoCopyReservoirFromScenario;
      CmeCopyChannelFromScenario         : DoCopyChannelFromScenario;
      CmeCopyIrrigationAreaFromScenario  : DoCopyIrrigationAreaFromScenario;
      CmeCopyPowerPlantFromScenario      : DoCopyPowerPlantFromScenario;
      CmeCopyIrrigationBlockFromScenario : DoCopyIrrigationBlockFromScenario;
      CmeCopyWetlandFromScenario         : DoCopyWetlandFromScenario;
      CmeCopyYMDemandCentreFromScenario  : DoCopyYMDemandCentreFromScenario;
      CmeCopySFRSubCatchmentFromScenario : DoCopySFRFromScenario;
      CmeCopyMineFromScenario            : DoCopyMineFromScenario;
      CmeCopyGroundWaterFromScenario     : DoCopyGroundWaterFromScenario;

      CmeResultPageControlChanging    :
      begin
        LCanChange := True;
        OnTabChangeRequest(nil,LCanChange);
      end;
      CmeImportDemandFile             :
      begin
        if (AData <> nil) then
          DoImportDemandFile(TAbstractModelFileName(AData).FileName);
      end;
      CmeImportDamLevelsFile          :
      begin
        if (AData <> nil) then
          DoImportDamLevelsFile(TAbstractModelFileName(AData).FileName);
      end;
      CmeImportHydrologyFile          :
      begin
        if (AData <> nil) then
          DoImportHydrologyFile(TAbstractModelFileName(AData).FileName);
      end;
      CmeImportParamFile          :
      begin
        if (AData <> nil) then
          DoImportParamFile(TAbstractModelFileName(AData).FileName);
      end;
      CmeClearParamFile          :
      begin
        if (AData <> nil) then
          DoClearParamFile(TAbstractModelFileName(AData).FileName);
      end;


    else
      Result := inherited ProcessEvent(AEventType, AData);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoShowResultGraphs: boolean;
const OPNAME = 'TYieldModelManager.DoShowResultGraphs';
//var LCanChange: boolean;
begin
  Result := False;
  try
    {if not Assigned(FAppModules.MainForm()) then Exit;
    OnTabChangeRequest(nil, LCanChange);
    if Assigned(FResultsTabSheetManager) and LCanChange then
    begin
      FAppModules.MainForm.ActivePage := FResultsTabSheetManager.TabSheet;
      OnTabHasChanged(nil);
      Result := True;
    end;
    }
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.StudyHasChanged: boolean;
const OPNAME = 'TYieldModelManager.StudyHasChanged';
var
  LCursor:TCursor;
begin
  Result := False;
  try
    if FLoadingData then
    begin
      Result := True;
      Exit;
    end;

    //StartFunctionTimer;
    //ProcessFunctionCall(OPNAME);
    LCursor       := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    FEmptyStudy   := False;
    try
      Result := YieldModelDataObject.StudyHasChanged;;
      if StudyDataIsEmpty then
      begin
        ResetYieldModelData;
        FEmptyStudy := True;
      end;

      Result := LoadModelData;

      ShowSplashScreen;

      if FEmptyStudy then
         InitialiseYearsInAnalysisFromParamDFile;
      Result := Result and FModelDataGUIManager.StudyHasChanged;
      Result := Result and inherited StudyHasChanged;

      if Result and Assigned(FFileEditManager) then
        Result := FFileEditManager.StudyHasChanged;

      if Result  and Assigned(FModelMenuItemManager) then
        Result := FModelMenuItemManager.StudyHasChanged;

      if Result  and Assigned(FNetworkVisualiserManager)then
        Result := FNetworkVisualiserManager.StudyHasChanged;

      if Result  and Assigned(FOutputReviewManager)then
        Result := FOutputReviewManager.StudyHasChanged;
      if Result  and Assigned(FOutputComparison)then
        Result := FOutputComparison.StudyHasChanged;

      if Assigned(FTimeSeriesComparitorManager) then
        Result := Result and FTimeSeriesComparitorManager.StudyHasChanged;

      if Result and Assigned(FModelFilesActionManager) then
        Result := FModelFilesActionManager.StudyHasChanged;

      {if Result and Assigned(FResultsTabSheetManager) then
        Result := Result and FResultsTabSheetManager.StudyHasChanged;
       }
      if Result and Assigned(FDataTabSheetManager) then
        Result := Result and FDataTabSheetManager.StudyHasChanged;

      if Assigned(FFileEditManager) and Assigned(FFileEditManager.TabSheet) then
        FFileEditManager.TabSheet.StudyHasChanged;

      if Assigned(FYieldReliabilityCurveManager) then
        Result := Result and FYieldReliabilityCurveManager.StudyHasChanged;

      if Result and Assigned(FModelCapabilityManager) then
        Result := Result and FModelCapabilityManager.StudyHasChanged;


      if Assigned(FAppModules.MainForm()) then
      begin
        AddTreeViewsSubNodes;
        FAppModules.MainForm.ApplyPreviousTabIndex;
        OnTabHasChanged(nil);
        SetSystemMenuState;
      end;
      //SetUserExpertMode(YieldModelDataObject.UserType = utExpert);
    finally
      Screen.Cursor := LCursor;
      FEmptyStudy := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
  //ProcessFunctionReturn(OPNAME);
  //FunctionTimerCallTraceTable;
  //FunctionTimerFunctionCountTable;
end;

function TYieldModelManager.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TYieldModelManager.StudyDataHasChanged';
begin
  if Assigned(FModelDataGUIManager) and FModelDataGUIManager.PopupDialogShowing then
  begin
    Result := FModelDataGUIManager.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
    Exit;
  end;

  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if(AFieldName = 'ParamFile') then
      Result := Result and RefreshHydrologyFileNames;

    if Result and Assigned(FNetworkVisualiserManager) then
      Result := FNetworkVisualiserManager.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);

    if Result and Assigned(FOutputReviewManager) then
      Result := FOutputReviewManager.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
    if Result and Assigned(FOutputComparison) then
      Result := FOutputComparison.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);

    if Assigned(FTimeSeriesComparitorManager) then
      Result := Result and FTimeSeriesComparitorManager.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);

    if Result and Assigned(FFileEditManager) then
      Result := FFileEditManager.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);

    if Assigned(FModelDataGUIManager) then
      Result := Result and FModelDataGUIManager.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);

    if Assigned(FFileSelectionManager) then
      Result := Result and FFileSelectionManager.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);

    {if Assigned(FResultsTabSheetManager) then
      Result := Result and FResultsTabSheetManager.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
     }
    if Assigned(FDataTabSheetManager) then
      Result := Result and FDataTabSheetManager.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);

    if (Assigned(FWizardForm)) then
      Result := Result AND FWizardForm.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);

    if (Assigned(FModelCapabilityManager)) then
      Result := Result AND FModelCapabilityManager.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);

    if Assigned(FYieldReliabilityCurveManager) then
      Result := Result and FYieldReliabilityCurveManager.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);


  SetSystemMenuState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.ProcessParameterChangeEvent : boolean;
const OPNAME = 'TYieldModelManager.ProcessParameterChangeEvent';
begin
  Result := False;
  try
    Result := FModelDataGUIManager.ProcessParameterChangeEvent;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.ProcessMetaDataEvent : boolean;
const OPNAME = 'TYieldModelManager.ProcessMetaDataEvent';
begin
  Result := False;
  try
    Result := FModelDataGUIManager.ProcessMetaDataEvent;
    {if (NOT Result) and (FResultsTabSheetManager <> nil)then
      Result := FResultsTabSheetManager.ProcessMetaDataEvent;
      }
    if (NOT Result) and (FYieldReliabilityCurveManager <> nil) then
      Result := FYieldReliabilityCurveManager.ProcessMetaDataEvent;
    if (NOT Result) then
      ShowMessage(FAppModules.Language.GetString('Message.DataNotImpl'));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.ProcessCustomModelEvent(AData: TModelMenuData): boolean;
const OPNAME = 'TYieldModelManager.ProcessCustomModelEvent';
begin
  Result := False;
  try
    //We are broadcasting the events for now. We need to pass the tabsheet name to pass
    // the message to the correct manager

    if (not Result) and  Assigned(FNetworkVisualiserManager) then
      Result := FNetworkVisualiserManager.DoCustomTabSheetEvent(AData);

    if (not Result) and  Assigned(FOutputReviewManager) then
      Result := FOutputReviewManager.DoCustomTabSheetEvent(AData);

    if (not Result) and  Assigned(FOutputComparison) then
      Result := FOutputComparison.DoCustomTabSheetEvent(AData);

    if (not Result) and  Assigned(FTimeSeriesComparitorManager) then
      Result := FTimeSeriesComparitorManager.DoCustomTabSheetEvent(AData);


    if (not Result) and Assigned(FFileEditManager) then
      Result := FFileEditManager.DoCustomTabSheetEvent(AData);

    {if (not Result) and  Assigned(FResultsTabSheetManager) then
      Result :=  FResultsTabSheetManager.DoCustomTabSheetEvent(AData);
     }
    if (not Result) and  Assigned(FDataTabSheetManager) then
      Result :=  FDataTabSheetManager.DoCustomTabSheetEvent(AData);

    if (not Result) and Assigned(FYieldReliabilityCurveManager) then
      Result := FYieldReliabilityCurveManager.DoCustomTabSheetEvent(AData);

    if (not Result) and  Assigned(FModelCapabilityManager) then
      Result :=  FModelCapabilityManager.DoCustomTabSheetEvent(AData);

    if not Result then
      inherited ProcessCustomModelEvent(AData);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoResultGridAction( AAction: TGridAction): boolean;
const OPNAME = 'TYieldModelManager.DoResultGridAction';
var
  LAction: TGridActionObject;
begin
  Result := False;
  try
    LAction := TGridActionObject.Create(AAction);
    try
      //if Assigned(FResultsTabSheetManager) then FResultsTabSheetManager.DoGridAction(LAction);
      if Assigned(FDataTabSheetManager) then FDataTabSheetManager.DoGridAction(LAction);
    finally
      FreeAndNil(LAction);
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.EditFieldValue(AFieldName, AOldValue: string;
  AContextData: TStrings): boolean;
const OPNAME = 'TYieldModelManager.EditFieldValue';
begin
  Result := False;
  try
    Result := EditFieldValue(AFieldName, AOldValue, '', AContextData);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.EditFieldValue(AFieldName, AOldValue,
  ADefaultValue: string; AContextData: TStrings): boolean;
const OPNAME = 'TYieldModelManager.EditFieldValue';
var
  LFieldEditFunction : TFieldEditFunction;
begin
  Result := False;
  try
    LFieldEditFunction := GetFieldEditFunction(AFieldName);
    if Assigned(LFieldEditFunction) then
      Result := LFieldEditFunction(AFieldName, ADefaultValue, AContextData)
    else
      Result := inherited EditFieldValue(AFieldName, AOldValue, ADefaultValue, AContextData);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.UpdateTestFieldFieldValue(AFieldName,
  ANewValue, AOldValue: String; AContextData: TStrings): boolean;
const OPNAME = 'TYieldModelManager.UpdateTestFieldFieldValue';
begin
  Result := False;
  try
    ShowMessage(FAppModules.Language.GetString('Message.CustomEditor'));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoesFieldHaveACustomEditor(
  AFieldName: String): boolean;
const OPNAME = 'TYieldModelManager.DoesFieldHaveACustomEditor';
begin
  Result := false;
  try
    Result := Assigned(GetFieldEditFunction(AFieldName));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelManager.CreateModelMenuItemManager;
const OPNAME = 'TYieldModelManager.CreateModelMenuItemManager';
begin
  try

    if not Assigned(FAppModules.MainForm()) then Exit;
    FModelMenuItemManager :=
      TYieldModelMenuItemManager.Create(FAppModules,
        Assigned(FNetworkVisualiserManager),
        Assigned(FDataTabSheetManager) and Assigned(FDataTabSheetManager.GridEditorManager),
        Assigned(FDataTabSheetManager) and Assigned(FDataTabSheetManager.GraphManager),
        Assigned(FFileEditManager),
        Assigned(FTimeSeriesComparitorManager)
        {Assigned(FResultsTabSheetManager)});
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoValidateSingleFile: boolean;
const OPNAME = 'TYieldModelManager.DoValidateSingleFile';
begin
  Result := False;
  try
    Result := FModelFilesActionManager.DoValidateSingleFile;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoValidateAllFiles: WordBool;
const OPNAME = 'TYieldModelManager.DoValidateAllFiles';
begin
  Result := False;
  try
    Result := FModelFilesActionManager.DoValidateAllFiles;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoExportAllFiles: WordBool;
const OPNAME = 'TYieldModelManager.DoExportAllFiles';
begin
  Result := False;
  try
    if(FModelDataGUIManager <> nil) then
       FModelDataGUIManager.ExitCurrentEditControl;

    Result := FModelFilesActionManager.DoExportAllFiles;
    // Set System menu state.
    SetSystemMenuState;
    if Assigned(FFileEditManager) and Assigned(FFileEditManager.TabSheet) then
      FFileEditManager.TabSheet.StudyDataHasChanged(sdccExport,'FileNames','','');

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoImportAllFiles: WordBool;
const OPNAME = 'TYieldModelManager.DoImportAllFiles';
begin
  Result := False;
  try
    Result := FModelFilesActionManager.DoImportAllFiles;
    RefreshModelData

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoExportSingleFile: boolean;
const OPNAME = 'TYieldModelManager.DoExportSingleFile';
begin
  Result := False;
  try
    if(FModelDataGUIManager <> nil) then
       FModelDataGUIManager.ExitCurrentEditControl;

    Result := FModelFilesActionManager.DoExportSingleFile;
    // Set System menu state.
    SetSystemMenuState;
    if Assigned(FFileEditManager) and Assigned(FFileEditManager.TabSheet) then
      FFileEditManager.TabSheet.StudyDataHasChanged(sdccExport,'SingleFile','','');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoImportSingleFile: boolean;
const OPNAME = 'TYieldModelManager.DoImportSingleFile';
begin
  Result := False;
  try
    Result := FModelFilesActionManager.DoImportSingleFile;
    RefreshModelData
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoClearModelData: WordBool;
const OPNAME = 'TYieldModelManager.DoClearModelData';
begin
  Result := False;
  try
    Result := FModelFilesActionManager.DoClearModelData;
    RefreshModelData
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoExportAllFilesAndRunModel(ASilent: WordBool): WordBool;
const OPNAME = 'TYieldModelManager.DoExportAllFilesAndRunModel';
begin
  Result := False;
  try
    if(FModelDataGUIManager <> nil) then
       FModelDataGUIManager.ExitCurrentEditControl;

    if Asilent then
    begin
      FAppModules.GlobalData.SetStopOnFirstErr(False);
    end;

    Result := FModelFilesActionManager.DoExportAllFilesAndRunModel(ASilent);
    RefreshModelData
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoRunModel: WordBool;
const OPNAME = 'TYieldModelManager.DoRunModel';
begin
  Result := False;
  try
    Result := FModelFilesActionManager.DoRunModel;
    if Result then
      RefreshModelData
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.ImportOutputFiles: WordBool;
const OPNAME = 'TYieldModelManager.ImportOutputFiles';
begin
  Result := False;
  try
    Result := FModelFilesActionManager.ImportOutputFiles;
    RefreshModelData
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.DoValidateModelData: boolean;
const OPNAME = 'TYieldModelManager.DoValidateModelData';
begin
  Result := False;
  try
    Result := FModelFilesActionManager.DoValidateModelData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelManager.SetSystemMenuState;
const OPNAME = 'TYieldModelManager.SetSystemMenuState';
begin
  try
    if not Assigned(FAppModules.MainForm()) then Exit;
    if (YieldModelDataObject.CastFileNamesObject.FilesCount = 0) then
    begin
      TYieldModelMenuItemManager(FModelMenuItemManager).SetMenuValidateFiles(msDisable);
      TYieldModelMenuItemManager(FModelMenuItemManager).SetMenuImportFiles(msDisable);
      TYieldModelMenuItemManager(FModelMenuItemManager).SetMenuExportFiles(msDisable);
      TYieldModelMenuItemManager(FModelMenuItemManager).SetMenuClearModelData(msDisable);
      TYieldModelMenuItemManager(FModelMenuItemManager).SetMenuRunModel(msDisable);
    end
    else
    begin
      TYieldModelMenuItemManager(FModelMenuItemManager).SetMenuValidateFiles(msEnable);
      TYieldModelMenuItemManager(FModelMenuItemManager).SetMenuImportFiles(msEnable);
      TYieldModelMenuItemManager(FModelMenuItemManager).SetMenuRunModel(msEnable);
      if (YieldModelDataObject.CastFileNamesObject.FilesSavedInDatabaseCount = 0) then
      begin
        TYieldModelMenuItemManager(FModelMenuItemManager).SetMenuExportFiles(msDisable);
        TYieldModelMenuItemManager(FModelMenuItemManager).SetMenuClearModelData(msDisable);
      end
      else
      begin
        TYieldModelMenuItemManager(FModelMenuItemManager).SetMenuExportFiles(msEnable);
        TYieldModelMenuItemManager(FModelMenuItemManager).SetMenuClearModelData(msEnable);
      end;
    end;
    if(YieldModelData.NetworkElementData.ChannelList.ChannelCount > 0) or
      (YieldModelData.NetworkElementData.ReservoirList.ReservoirAndNodesCount > 1) then
    begin
      TYieldModelMenuItemManager(FModelMenuItemManager).SetMenuExportFiles(msEnable);
      TYieldModelMenuItemManager(FModelMenuItemManager).SetMenuValidateModelData(msEnable);
    end
    else
    begin
      TYieldModelMenuItemManager(FModelMenuItemManager).SetMenuExportFiles(msDisable);
      TYieldModelMenuItemManager(FModelMenuItemManager).SetMenuValidateModelData(msDisable);
    end;

    if (FAppModules.User.UserType = utExpert) and (FAppModules.StudyArea.ModelVersion = '7') then
    begin
      TYieldModelMenuItemManager(FModelMenuItemManager).SetMenuGenerateSysConfigFiles(msShow);
      TYieldModelMenuItemManager(FModelMenuItemManager).SetMenuGenerateSysConfigFiles(msEnable);
    end else
    begin
      TYieldModelMenuItemManager(FModelMenuItemManager).SetMenuGenerateSysConfigFiles(msHide);
    end;

    TYieldModelMenuItemManager(FModelMenuItemManager).SetExpertUserChecked(FAppModules.User.UserType = utExpert);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelManager.DoModelDataHasChanged(AChangeLevel: TModelDataChangeLevel; AChangeType: TModelDataChangeType;
  AChangeAction: TModelDataChangeAction);
const OPNAME = 'TYieldModelManager.DoModelDataHasChanged';
begin
  inherited;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.RefreshHydrologyFileNames: boolean;
const OPNAME = 'TYieldModelManager.RefreshHydrologyFileNames';
begin
  Result := False;
  try
    if not Assigned(FFileSelectionManager) then
    begin
      Result := True;
    end
    else
    begin
      Result := TYieldFileSelectionManager(FFileSelectionManager).ReselectHydrologyFileNames(
                YieldModelDataObject.CastFileNamesObject);
      // Populate the File Edit tabsheet treeview.
      if Result and
       Assigned(FFileEditManager) and
       Assigned(FFileEditManager.TabSheet) then
       begin
         Result := FFileSelectionManager.PopulateTreeView(TTreeViewTabSheet(FFileEditManager.TabSheet).TreeView,
                                                    YieldModelDataObject.CastFileNamesObject);
         FFileEditManager.TabSheet.LanguageHasChanged;
       end;
    end
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.SelectModelFileNames: boolean;
const OPNAME = 'TYieldModelManager.SelectModelFileNames';
var
  LDataFileObjects: TDataFileObjects;
begin
  Result := False;
  try
    if not Assigned(FFileSelectionManager) then
    begin
      Result := True;
    end
    else
    begin
      LDataFileObjects := TDataFileObjects.Create;
      try
        Result := FFileSelectionManager.PopulateFileNames(LDataFileObjects,YieldModelDataObject.CastFileNamesObject);
      finally
        FreeAndNil(LDataFileObjects);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.SelectAdditionalFileNames: boolean;
const OPNAME = 'TYieldModelManager.SelectAdditionalFileNames';
var
  LDataFileObjects: TDataFileObjects;
begin
  Result := False;
  try
    if not Assigned(FFileSelectionManager) then
    begin
      Result := True;
    end
    else
    begin
      LDataFileObjects := TDataFileObjects.Create;
      try
        Result := TYieldFileSelectionManager(FFileSelectionManager).PopulateSpecifiedDemandFileNames(
                  YieldModelDataObject.NetworkFeaturesData.SpecifiedDemandFeatureList,
                  YieldModelDataObject.CastFileNamesObject);
      finally
        FreeAndNil(LDataFileObjects);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DisplayModelFileNames: boolean;
const OPNAME = 'TYieldModelManager.DisplayModelFileNames';
begin
  Result := False;
  try
    if Assigned(FFileEditManager) and Assigned(FFileEditManager.TabSheet) then
       Result := FFileSelectionManager.PopulateTreeView(TTreeViewTabSheet(FFileEditManager.TabSheet).TreeView,
                 YieldModelDataObject.CastFileNamesObject);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.LoadModelData: boolean;
const OPNAME = 'TYieldModelManager.LoadModelData';
begin
  Result := False;
  try
    FLoadingData := True;
    try
      Result := SelectModelFileNames;
      Result := Result and ImportParamAndPathsFile;
      Result := Result and DoReadYRCDataFromDB(False);
      Result := Result and LoadMiningData;
      Result := Result and LoadReservoirData;
      Result := Result and LoadParameterData;
      Result := Result and LoadConfigurationData;
      Result := Result and LoadReservoirPenaltyStructureData;
      Result := Result and LoadChannelData;
      Result := Result and LoadOutputData;
      Result := Result and LoadOutputComparisonData;
      Result := Result and SelectAdditionalFileNames;
      Result := Result and DisplayModelFileNames;
      Result := Result and LoadStudyMetaData;
      Result := Result and LoadFieldFileReferencesData;
    finally
      FLoadingData := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.LoadMiningData: boolean;
const OPNAME = 'TYieldModelManager.LoadMiningData';
var LLoadAgent: TMineLoadAgent;
begin
  Result := False;
  try
    LLoadAgent := TMineLoadAgent.Create(FAppModules);
    try
      Result := LLoadAgent.ConstructData(YieldModelDataObject.CastNetworkFeaturesData.CastMineList,
                                         YieldModelDataObject.CastNetworkFeaturesData.CastMineSubCatchmentList);
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

function TYieldModelManager.LoadReservoirData: boolean;
const OPNAME = 'TYieldModelManager.LoadReservoirData';
var LLoadAgent: TReservoirDataLoadAgent;
begin
  Result := False;
  try
    LLoadAgent := TReservoirDataLoadAgent.Create(FAppModules);
    try
      if LLoadAgent.ConstructData(YieldModelDataObject.CastNetworkElementData.CastReservoirList,
                                  YieldModelDataObject.CastNetworkElementData.CastReservoirAreaGroupList) then
        Result := True;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

function TYieldModelManager.LoadConfigurationData: boolean;
const OPNAME = 'TYieldModelManager.LoadConfigurationData';
var LLoadAgent: TRunConfigurationDataLoadAgent;
begin
  Result := False;
  try
    LLoadAgent := TRunConfigurationDataLoadAgent.Create(FAppModules);
    try
      Result := LLoadAgent.ConstructData(YieldModelDataObject.CastRunConfigurationData,YieldModelDataObject.CastDataFilePaths);
      if Result then
      begin
        if(YieldModelDataObject.CastFileNamesObject.HydrologyFilesPath = '') and
          (YieldModelDataObject.CastDataFilePaths.HydrologyFilePath <> '') then
          YieldModelDataObject.CastFileNamesObject.PopulateHydrologyPaths(YieldModelDataObject.CastDataFilePaths.HydrologyFilePath);
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

function TYieldModelManager.LoadChannelData: boolean;
const OPNAME = 'TYieldModelManager.LoadChannelData';
var LLoadChannelAgent: TChannelDataLoadAgent;
    LLoadPenaltyAgent: TChannelPenaltyStructureDataLoadAgent;
    lLoadFeaturesAgent : TNetworkFeaturesLoadAgent;
begin
  Result := False;
  try
    LLoadChannelAgent  := TChannelDataLoadAgent.Create(FAppModules);
    LLoadPenaltyAgent  := TChannelPenaltyStructureDataLoadAgent.Create(FAppModules);
    lLoadFeaturesAgent := TNetworkFeaturesLoadAgent.Create(FAppModules);
    try

      Result := LLoadChannelAgent.ConstructData
                                   (YieldModelDataObject.CastNetworkElementData.CastChannelList,
                                    YieldModelDataObject.CastNetworkFeaturesData.CastMasterControlFeatureList,
                                    YieldModelDataObject.CastNetworkFeaturesData.CastMinimumFlowConstraintList,
                                    YieldModelDataObject.CastNetworkFeaturesData.CastSpecifiedDemandFeatureList,
                                    YieldModelDataObject.CastNetworkFeaturesData.CastMinMaxFlowConstraintList,
                                    YieldModelDataObject.CastNetworkFeaturesData.CastPumpingFeatureList,
                                    YieldModelDataObject.CastNetworkFeaturesData.CastLossFeatureList);

      Result := Result AND lLoadFeaturesAgent.ConstructData
                                   (YieldModelDataObject.CastNetworkElementData.CastChannelList,
                                    YieldModelDataObject.CastNetworkFeaturesData.CastDiversionFeatureList,
                                    YieldModelDataObject.CastNetworkFeaturesData.CastPhysicalFlowConstraintList,
                                    YieldModelDataObject.CastNetworkFeaturesData.CastIFRFeatureList,
                                    YieldModelDataObject.CastNetworkFeaturesData.CastIrrigationAreaList,
                                    YieldModelDataObject.CastNetworkFeaturesData.CastIrrigationBlockList,
                                    YieldModelDataObject.CastNetworkFeaturesData.CastWetlandList,
                                    YieldModelDataObject.CastNetworkFeaturesData.CastYMDemandCentreList,
                                    YieldModelDataObject.CastNetworkFeaturesData.CastPowerPlantList,
                                    YieldModelDataObject.CastNetworkFeaturesData.CastSpecifiedInflowDataList,
                                    YieldModelDataObject.CastNetworkFeaturesData.CastWaterDemandConfiguration,
                                    YieldModelDataObject.CastNetworkFeaturesData.CastWaterDemandFeatureList,
                                    YieldModelDataObject.CastNetworkFeaturesData.CastChannelAreaList,
                                    YieldModelDataObject.CastNetworkFeaturesData.CastStreamFlowReductionList,
                                    YieldModelDataObject.CastNetworkFeaturesData.CastCurtailmentAndDrought,
                                    YieldModelDataObject.CastNetworkFeaturesData.CastGroundWaterList);

      Result := Result and LLoadPenaltyAgent.ConstructData
                                   (YieldModelDataObject.CastNetworkElementData.CastChannelPenaltyList);

      Result := Result and
                LLoadPenaltyAgent.PopulateChannelPenaltyStructures
                                   (YieldModelDataObject.CastNetworkElementData.CastChannelList,
                                    YieldModelDataObject.CastNetworkElementData.CastChannelPenaltyList);

      Result := Result and
                LLoadChannelAgent.LoadOutputChannelData
                                   (YieldModelDataObject.CastNetworkElementData.CastChannelList);

      Result := Result and LoadIFRSiteData;


    finally
      LLoadPenaltyAgent.Free;
      LLoadChannelAgent.Free;
      lLoadFeaturesAgent.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

function TYieldModelManager.LoadReservoirPenaltyStructureData: boolean;
const OPNAME = 'TYieldModelManager.LoadReservoirPenaltyStructureData';
var LLoadAgent: TReservoirPenaltyStructureDataLoadAgent;
begin
  Result := False;
  try
    LLoadAgent := TReservoirPenaltyStructureDataLoadAgent.Create(FAppModules);
    try
      if LLoadAgent.ConstructData(YieldModelDataObject.CastNetworkElementData.CastReservoirPenaltyStructureList) then
        Result := True;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

function TYieldModelManager.DoReadYRCDataFromDB(AEvent: boolean = False): boolean;
const OPNAME = 'TYieldModelManager.DoReadYRCDataFromDB';
var
  LDataLoadAgent: TYRCModelDataLoadAgent;
begin
  Result := False;
  try
    LDataLoadAgent := TYRCModelDataLoadAgent.Create(FAppModules);
    try
      Result := LDataLoadAgent.ReadYRCDataFromDB(YieldModelDataObject.CastYRCGraphDataObject);
      if Result and AEvent {and Assigned(FResultsTabSheetManager)} then
        FYieldReliabilityCurveManager.DoCustomTabSheetEvent(TModelMenuData.Create(meYRCChartDataLoaded));
    finally
      LDataLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoReadYRCDataFromFile: boolean;
const OPNAME = 'TYieldModelManager.DoReadYRCDataFromFile';
var
  LDataLoadAgent: TYRCModelDataLoadAgent;
begin
  Result := False;
  try
    LDataLoadAgent := TYRCModelDataLoadAgent.Create(FAppModules);
    try
      Result := LDataLoadAgent.ReadYRCDataFromFile(YieldModelDataObject.CastYRCGraphDataObject,
                YieldModelDataObject.CastFileNamesObject);
      if Result {and Assigned(FResultsTabSheetManager) }then
        FYieldReliabilityCurveManager.DoCustomTabSheetEvent(TModelMenuData.Create(meYRCChartDataLoaded));
    finally
      LDataLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoSaveYRCDataToDB: boolean;
const OPNAME = 'TYieldModelManager.DoSaveYRCDataToDB';
var
  LDataLoadAgent: TYRCModelDataLoadAgent;
begin
  Result := False;
  try
    LDataLoadAgent := TYRCModelDataLoadAgent.Create(FAppModules);
    try
      Result := LDataLoadAgent.SaveYRCDataToDB(YieldModelDataObject.CastYRCGraphDataObject);
      if Result then
        StudyDataHasChanged(sdccSaveData,'YRCData','','');
    finally
      LDataLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoDeleteYRCDataFromDB: boolean;
const OPNAME = 'TYieldModelManager.DoDeleteYRCDataFromDB';
var
  LDataLoadAgent: TYRCModelDataLoadAgent;
begin
  Result := False;
  try
    LDataLoadAgent := TYRCModelDataLoadAgent.Create(FAppModules);
    try
      Result := LDataLoadAgent.DeleteYRCDataFromDB(YieldModelDataObject.CastYRCGraphDataObject);
      if Result then
        StudyDataHasChanged(sdccDelete,'YRCData','','');
    finally
      LDataLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.GetFileLineType(AFileObject: TObject; ALineNumber: Integer): string;
const OPNAME = 'TYieldModelManager.GetFileLineType';
begin
  Result := '';
  try
    if Assigned(AFileObject) and Assigned(ModelData()) and Assigned(YieldModelDataObject.CastFilesLineTypes) then
       Result := YieldModelDataObject.CastFilesLineTypes.GetFileLineType(TFileNameObject(AFileObject),ALineNumber);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoRefreshFileHints: boolean;
const OPNAME = 'TYieldModelManager.DoRefreshFileHints';
begin
  Result := False;
  try
   Result := FModelFilesActionManager.DoRefreshFileHints;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelManager.ApplicationIsClosing;
const OPNAME = 'TYieldModelManager.ApplicationIsClosing';
begin
  inherited ApplicationIsClosing;
  try
    // We need a list of Managers here - VGN 20030214
    if Assigned(FNetworkVisualiserManager) and Assigned(FNetworkVisualiserManager.TabSheet) then
      FNetworkVisualiserManager.TabSheet.ApplicationIsClosing;

    if Assigned(FOutputReviewManager) and Assigned(FOutputReviewManager.TabSheet) then
      FOutputReviewManager.TabSheet.ApplicationIsClosing;
    if Assigned(FOutputComparison) and Assigned(FOutputComparison.TabSheet) then
      FOutputComparison.TabSheet.ApplicationIsClosing;
    if Assigned(FTimeSeriesComparitorManager) and Assigned(FTimeSeriesComparitorManager.TabSheet) then
      FTimeSeriesComparitorManager.TabSheet.ApplicationIsClosing;


    {if Assigned(FResultsTabSheetManager) and Assigned(FResultsTabSheetManager.TabSheet) then
      FResultsTabSheetManager.TabSheet.ApplicationIsClosing;
     }
    if Assigned(FDataTabSheetManager) and Assigned(FDataTabSheetManager.TabSheet) then
      FDataTabSheetManager.TabSheet.ApplicationIsClosing;

    if Assigned(FFileEditManager) and Assigned(FFileEditManager.TabSheet) then
      FFileEditManager.TabSheet.ApplicationIsClosing;

    if Assigned(FModelCapabilityManager) and Assigned(FModelCapabilityManager.TabSheet) then
      FModelCapabilityManager.TabSheet.ApplicationIsClosing;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.CanApplicationClose: boolean;
const OPNAME = 'TYieldModelManager.CanApplicationClose';
begin
  Result := inherited CanApplicationClose;
  try
    // We need a list of Managers here - VGN 20030214
    if Assigned(FNetworkVisualiserManager) and Assigned(FNetworkVisualiserManager.TabSheet) then
      Result := FNetworkVisualiserManager.TabSheet.CanApplicationClose;

    if Assigned(FOutputReviewManager) and Assigned(FOutputReviewManager.TabSheet) then
      Result := FOutputReviewManager.TabSheet.CanApplicationClose;
    if Assigned(FOutputComparison) and Assigned(FOutputComparison.TabSheet) then
      Result := FOutputComparison.TabSheet.CanApplicationClose;
    if Assigned(FTimeSeriesComparitorManager) and Assigned(FTimeSeriesComparitorManager.TabSheet) then
      Result := FTimeSeriesComparitorManager.TabSheet.CanApplicationClose;


    {if Assigned(FResultsTabSheetManager) and Assigned(FResultsTabSheetManager.TabSheet) then
      Result := FResultsTabSheetManager.TabSheet.CanApplicationClose;
     }
    if Assigned(FDataTabSheetManager) and Assigned(FDataTabSheetManager.TabSheet) then
      Result := FDataTabSheetManager.TabSheet.CanApplicationClose;

    if Assigned(FFileEditManager) and Assigned(FFileEditManager.TabSheet) then
      Result := FFileEditManager.TabSheet.CanApplicationClose;

    if Assigned(FModelCapabilityManager) and Assigned(FModelCapabilityManager.TabSheet) then
      Result := FModelCapabilityManager.TabSheet.CanApplicationClose;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.IsGraphLoaded: boolean;
const OPNAME = 'TYieldModelManager.IsGraphLoaded';
begin
  Result := false;
  try
    Result :=  Assigned(FDataTabSheetManager) and Assigned(FDataTabSheetManager.GraphManager);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.IsGridLoaded: boolean;
const OPNAME = 'TYieldModelManager.IsGridLoaded';
begin
  Result := false;
  try
    Result :=  Assigned(FDataTabSheetManager) and Assigned(FDataTabSheetManager.GridEditorManager);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.ViewInputDialog(AParent : TObject; ACommaTextContextData : String; AOwner: TObject = nil): boolean;
const OPNAME = 'TYieldModelManager.ViewInputDialog';
var
  lDataList   : TStringList;
  lViewType   : string;
  lIdentifier : integer;
begin
  Result := FALSE;
  try
    Result := FModelDataGUIManager.ViewInputDialog(TWincontrol(AParent), ACommaTextContextData, TWincontrol(AOwner));
    if ({Result AND }Assigned(FModelMenuItemManager)) then
    begin
      lDataList := TStringList.Create;
      try
        lDataList.CommaText := ACommaTextContextData;
        lViewType   := UpperCase(Trim(lDataList.Values['VIEWNAME']));
        lIdentifier := StrToInt(lDataList.Values['MODELELEMENTID']);
        (FModelMenuItemManager as TYieldModelMenuItemManager).TreeNodeHasChanged(lViewType, lIdentifier);
      finally
        FreeAndNil(lDataList);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.ViewInputPopupDialog(AParent: TObject; ACommaTextContextData: String; AOwner: TObject): boolean;
const OPNAME = 'TYieldModelManager.ViewInputPopupDialog';
begin
  Result := FALSE;
  try
    Result := FModelDataGUIManager.ViewInputPopupDialog(TWincontrol(AParent), ACommaTextContextData, TWincontrol(AOwner));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.ViewOutputDialog(AParent: TObject; ACommaTextContextData: String; AOwner: TObject): boolean;
const OPNAME = 'TYieldModelManager.ViewOutputDialog';
begin
  Result := FALSE;
  try
    Result := FModelDataGUIManager.ViewOutputDialog(TWincontrol(AParent), ACommaTextContextData, TWincontrol(AOwner));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.ViewOutputPopupDialog(AParent: TObject; ACommaTextContextData: String; AOwner: TObject): boolean;
const OPNAME = 'TYieldModelManager.ViewOutputPopupDialog';
begin
  Result := FALSE;
  try
    Result := FModelDataGUIManager.ViewOutputPopupDialog(TWincontrol(AParent), ACommaTextContextData, TWincontrol(AOwner));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.ViewOutputComparisonDialog(AParent: TObject; ACommaTextContextData: String; AOwner: TObject): boolean;
const OPNAME = 'TYieldModelManager.ViewOutputComparisonDialog';
begin
  Result := FALSE;
  try
    Result := FModelDataGUIManager.ViewOutputComparisonDialog(TWincontrol(AParent), ACommaTextContextData, TWincontrol(AOwner));
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TYieldModelManager.PopulateDataList(AModelDataChangeType: TModelDataChangeType; AList: TList): boolean;
const OPNAME = 'TYieldModelManager.PopulateDataList';
var
  LIndex : integer;
begin
  Result := False;
  try
    if ModelDataLoaded and Assigned(AList) then
    begin
      if AModelDataChangeType = mdctReservior then
        for LIndex := 0 to YieldModelDataObject.NetworkElementData.ReservoirList.ReservoirCount - 1 do
          AList.Add(YieldModelDataObject.CastNetworkElementData.CastReservoirList.CastReservoirByIndex[LIndex]);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.LoadParameterData: boolean;
const OPNAME = 'TYieldModelManager.LoadParameterData';
var LLoadAgent: TParameterDataLoadAgent;
begin
  Result := False;
  try
    LLoadAgent := TParameterDataLoadAgent.Create(FAppModules);
    try
      if LLoadAgent.ConstructData(YieldModelDataObject.CastCastParameterData) then
        Result := True;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

function TYieldModelManager.LoadOutputData: boolean;
const OPNAME = 'TYieldModelManager.LoadOutputData';
var LLoadAgent: TOutputDataLoadAgent;
begin
  Result := False;
  try
    LLoadAgent := TOutputDataLoadAgent.Create(FAppModules);
    try
      if LLoadAgent.ConstructData(YieldModelDataObject.OutputData,YieldModelDataObject) then
        Result := True;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

function TYieldModelManager.LoadOutputComparisonData: boolean;
const OPNAME = 'TYieldModelManager.LoadOutputComparisonData';
var LLoadAgent: TOutputComparisonLoadAgent;
begin
  Result := False;
  try
    LLoadAgent := TOutputComparisonLoadAgent.Create(FAppModules);
    try
      if LLoadAgent.ConstructData(YieldModelDataObject) then
        Result := True;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

function TYieldModelManager.LoadStudyMetaData: boolean;
const OPNAME = 'TYieldModelManager.LoadStudyMetaData';
var
  LLoadAgent : TStudyMetaDataLoadAgent;
begin
  Result := False;
  try
    LLoadAgent := TStudyMetaDataLoadAgent.Create(FAppModules);
    try
      if LLoadAgent.ConstructData(YieldModelDataObject.StudyMetaDataList) then
        Result := True;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

function TYieldModelManager.LoadFieldFileReferencesData: Boolean;
const OPNAME = 'TYieldModelManager.LoadFieldFileReferencesData';
begin
  Result :=False;
  try
    Result := FAppModules.FieldProperties.LoadFieldFileReferencesData(ModelName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.RefreshModelData: boolean;
const OPNAME = 'TYieldModelManager.RefreshModelData';
begin
  Result :=False;
  try
    Initialise;
    StudyHasChanged;
    LanguageHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.GetYieldChannelYield: Double;
const OPNAME = 'TYieldModelManager.GetYieldChannelYield';
var
  LTargetYield: double;
  LResult : Boolean;
begin
  Result := 0.0;;
  try
    LResult := TFilesActionYieldManager(FModelFilesActionManager).GetYieldChannelYield(Result);
    if LResult then
    begin
      LTargetYield := YieldModelData.RunConfigurationData.TargetYieldByIndex[1];
      Result := LTargetYield - (LTargetYield * Result);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.ShowNetworkVisualiser: boolean;
const OPNAME = 'TYieldModelManager.ShowNetworkVisualiser';
var LCanChange: boolean;
begin
  Result := False;
  try
    if not Assigned(FAppModules.MainForm()) then Exit;
    OnTabChangeRequest(nil, LCanChange);
    if Assigned(FNetworkVisualiserManager) and Assigned(FNetworkVisualiserManager.TabSheet) and LCanChange then
    begin
      FAppModules.MainForm.ActivePage := FNetworkVisualiserManager.TabSheet;
      OnTabHasChanged(nil);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.ShowFileEditor: boolean;
const OPNAME = 'TYieldModelManager.ShowFileEditor';
var LCanChange: boolean;
begin
  Result := False;
  try
    if not Assigned(FAppModules.MainForm()) then Exit;
    if (FAppModules.User.UserType <> utExpert) then Exit;
    if Assigned(FFileEditManager) and Assigned(FFileEditManager.TabSheet) then
    begin
      OnTabChangeRequest(nil, LCanChange);
      if LCanChange then
      begin
        FAppModules.MainForm.ActivePage := FFileEditManager.TabSheet;
        OnTabHasChanged(nil);
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.ShowGridEditor: boolean;
const OPNAME = 'TYieldModelManager.ShowGridEditor';
var LCanChange: boolean;
begin
  Result := False;
  try
    if not Assigned(FAppModules.MainForm()) then Exit;
    OnTabChangeRequest(nil, LCanChange);
    if Assigned(FDataTabSheetManager) and Assigned(FDataTabSheetManager.GridEditorManager) and LCanChange then
    begin
      FAppModules.MainForm.ActivePage := FDataTabSheetManager.TabSheet;
      FDataTabSheetManager.ShowGridEditor;
      OnTabHasChanged(nil);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.EditGridEditor(AData: TObject): boolean;
const OPNAME = 'TYieldModelManager.EditGridEditor';
var LCanChange: boolean;
begin
  Result := False;
  try
    if not Assigned(FAppModules.MainForm()) then Exit;
    OnTabChangeRequest(nil, LCanChange);
    if Assigned(FDataTabSheetManager) and Assigned(FDataTabSheetManager.GridEditorManager) and LCanChange then
    begin
      FAppModules.MainForm.ActivePage := FDataTabSheetManager.TabSheet;
      FDataTabSheetManager.EditGridEditor(AData);
      OnTabHasChanged(nil);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.ShowGraph: boolean;
const OPNAME = 'TYieldModelManager.ShowGraph';
var LCanChange: boolean;
begin
  Result := False;
  try
    if not Assigned(FAppModules.MainForm()) then Exit;
    OnTabChangeRequest(nil, LCanChange);
    if Assigned(FDataTabSheetManager) and Assigned(FDataTabSheetManager.GridEditorManager) and LCanChange then
    begin
      FAppModules.MainForm.ActivePage := FDataTabSheetManager.TabSheet;
      FDataTabSheetManager.ShowGraph;
      OnTabHasChanged(nil);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.ResetState: boolean;
const OPNAME = 'TYieldModelManager.ResetState';
begin
    Result := inherited ResetState;
  try
    if Assigned(FFileEditManager) then
      Result := Result and FFileEditManager.ResetState;

    if Assigned(FNetworkVisualiserManager)  and
       Assigned(FNetworkVisualiserManager.TabSheet) then
      Result := Result and FNetworkVisualiserManager.ResetState;

    if Assigned(FOutputReviewManager) then
      Result := Result and FOutputReviewManager.ResetState;

    if Assigned(FOutputComparison) then
      Result := Result and FOutputComparison.ResetState;
    if Assigned(FTimeSeriesComparitorManager) then
      Result := Result and FTimeSeriesComparitorManager.ResetState;



    {if Result and Assigned(FResultsTabSheetManager) then
      Result := Result and FResultsTabSheetManager.ResetState;
     }
    if Result and Assigned(FDataTabSheetManager) then
      Result := Result and FDataTabSheetManager.ResetState;

    if Assigned(FModelDataGUIManager) then
      Result := Result and FModelDataGUIManager.ResetState;

    if Assigned(FYieldReliabilityCurveManager) then
      FYieldReliabilityCurveManager.ResetState;

    if Assigned(FModelCapabilityManager) then
      Result := Result and FModelCapabilityManager.ResetState;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.StudyDataIsEmpty: boolean;
const OPNAME = 'TYieldModelManager.StudyDataIsEmpty';
var LLoadAgent: TYieldModelDataLoadAgent;
begin
  Result := False;
  try
    LLoadAgent := TYieldModelDataLoadAgent.Create(FAppModules);
    try
      Result := LLoadAgent.StudyDataIsEmpty;
    finally
       LLoadAgent.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

function TYieldModelManager.ResetYieldModelData: boolean;
const OPNAME = 'TYieldModelManager.ResetYieldModelData';
var LLoadAgent: TYieldModelDataLoadAgent;
begin
  Result := False;
  try
    LLoadAgent := TYieldModelDataLoadAgent.Create(FAppModules);
    try
      Result := LLoadAgent.ResetYieldModelData;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

function TYieldModelManager.DoCreateReservoir: IReservoirData;
const OPNAME = 'TYieldModelManager.DoCreateReservoir';
begin
  Result := nil;
  try
    Result :=  YieldModelDataObject.CastNetworkElementData.CastReservoirList.CreateReservoir(ntReservoir);
    if (Result <> nil) then
    begin
      CreateTabSheetTreeViewElement('AReservoir',
                                 'AReservoir',
                                 Result.ReservoirConfigurationData.ReservoirIdentifier, tvnidAReservoir,
                                 Result.ReservoirConfigurationData.ReservoirName,
                                 'RESERVOIR',
                                 'RESERVOIR',
                                  True,tvsnAll);
      StudyDataHasChanged(sdccAdd, 'ReservoirName', '0',IntToStr(Result.ReservoirConfigurationData.ReservoirIdentifier));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoDeleteReservoir(AReservoirNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.DoDeleteReservoir';
var
  LNode            : TTreeNode;
  LDataObject      : TViewDataTreeNodeData;
begin
  Result := False;
  try
    lNode := nil;
    if (AReservoirNumber <= 0) then
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          AReservoirNumber := LDataObject.ViewDataNode.Weighting;
        end;
      end;
    end;
    if (AReservoirNumber > 0) then
    begin
      Result := YieldModelDataObject.CastNetworkElementData.CastReservoirList.DeleteReservoir(AReservoirNumber);
      if (LNode <> nil) then
      begin
        if Result then
          DeleteTabSheetTreeNode(lNode);
        SetSystemMenuState;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCreateNodeWithInflow: IReservoirData;
const OPNAME = 'TYieldModelManager.DoCreateNodeWithInflow';
begin
  Result := nil;
  try
    Result :=  YieldModelDataObject.CastNetworkElementData.CastReservoirList.CreateNodeWithInflow(ntNodeWithInflow);
    if (Result <> nil) then
    begin
      CreateTabSheetTreeViewElement('NodesWithInflow',
                                 'NodesHeading,NodesWithInflow',
                                 Result.ReservoirConfigurationData.ReservoirIdentifier, tvnidNodesWithInflow,
                                 Result.ReservoirConfigurationData.ReservoirName,
                                 '',
                                 'NODEWITHINFLOW',
                                 True,tvsnAll);
      StudyDataHasChanged(sdccAdd, 'ReservoirName', '0',IntToStr(Result.ReservoirConfigurationData.ReservoirIdentifier));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoDeleteNodeWithInflow(ANodeNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.DoDeleteNodeWithInflow';
var
  LNode            : TTreeNode;
  LDataObject      : TViewDataTreeNodeData;
begin
  Result := False;
  try
    if(ANodeNumber > 0) then
      Result := YieldModelDataObject.CastNetworkElementData.CastReservoirList.DeleteNodeWithInflow(ANodeNumber)
    else
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          ANodeNumber := LDataObject.ViewDataNode.Weighting;
          Result := YieldModelDataObject.CastNetworkElementData.CastReservoirList.DeleteNodeWithInflow(ANodeNumber);
          if Result then
            DeleteTabSheetTreeNode(LNode);
          SetSystemMenuState;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCreateNodeWithoutInflow: IReservoirData;
const OPNAME = 'TYieldModelManager.DoCreateNodeWithoutInflow';
begin
  Result := nil;
  try
    Result :=  YieldModelDataObject.CastNetworkElementData.CastReservoirList.CreateNodeWithoutInflow(ntNodeWithoutInflow);
    if (Result <> nil) then
    begin
      CreateTabSheetTreeViewElement('NodesWithoutInFlow',
                                 'NodesHeading,NodesWithoutInFlow',
                                 Result.ReservoirConfigurationData.ReservoirIdentifier,tvnidNodesWithoutInFlow,
                                 Result.ReservoirConfigurationData.ReservoirName,
                                 '',
                                 'NODEWITHOUTINFLOW',
                                 True,tvsnAll);
      StudyDataHasChanged(sdccAdd, 'ReservoirName', '0',IntToStr(Result.ReservoirConfigurationData.ReservoirIdentifier));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoDeleteNodeWithoutInflow(ANodeNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.DoDeleteNodeWithoutInflow';
var
  LNode            : TTreeNode;
  LDataObject      : TViewDataTreeNodeData;
begin
  Result := False;
  try
    if(ANodeNumber > 0) then
      Result := YieldModelDataObject.CastNetworkElementData.CastReservoirList.DeleteNodeWithoutInflow(ANodeNumber)
    else
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          ANodeNumber := LDataObject.ViewDataNode.Weighting;
          Result := YieldModelDataObject.CastNetworkElementData.CastReservoirList.DeleteNodeWithoutInflow(ANodeNumber);
          if Result then
            DeleteTabSheetTreeNode(LNode);
          SetSystemMenuState;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCreateChannel (AUpStreamNodeNumber: Integer; ADownStreamNodeNumber: Integer): IGeneralFlowChannel;
const OPNAME = 'TYieldModelManager.DoCreateChannel';
begin
  Result := Nil;
  try
    Result := YieldModelDataObject.
                  NetworkElementData.ChannelList.CreateChannel;
    if (Result <> nil) then
    begin
      Result.UpStreamNodeNumber   := AUpStreamNodeNumber;
      Result.DownStreamNodeNumber := ADownStreamNodeNumber;
      CreateTabSheetTreeViewElement('ChannelDetails12',
                                 'ChannelHeading,ChannelDetails12',
                                 Result.ChannelNumber,tvnidChannelDetails12,
                                 Result.ChannelName,
                                 'GENERALCHANNEL',
                                 'CHANNEL',
                                 True,tvsnAll);
      StudyDataHasChanged(sdccAdd, 'ChannelName', '0',IntToStr(Result.ChannelNumber));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCreateChannelTariff(AChannelNr: integer): WordBool;
const OPNAME = 'TYieldModelManager.DoCreateChannelTariff';
begin
  Result := False;
  try
    //Do nothing here. Only the planning model needs to implement this virtual method
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoDeleteChannelTariff(AChannelNr: integer): WordBool;
const OPNAME = 'TYieldModelManager.DoDeleteChannelTariff';
begin
  Result := False;
  try
    //Do nothing here. Only the planning model needs to implement this virtual method
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoDeleteChannel(AChannelNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.DoDeleteChannel';
var
  LNode          : TTreeNode;
  LDataObject    : TViewDataTreeNodeData;
begin
  Result := False;
  try
    if (AChannelNumber > 0) then
    begin
      Result := DoDeleteAllFeatures(AChannelNumber);
      Result := Result and YieldModelDataObject.CastNetworkElementData.
                CastChannelList.RemoveChannelWithNumber(AChannelNumber)
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          AChannelNumber := LDataObject.ViewDataNode.Weighting;
          Result := DoDeleteAllFeatures(AChannelNumber);
          Result := Result and YieldModelDataObject.CastNetworkElementData.
                   CastChannelList.RemoveChannelWithNumber(AChannelNumber);

          if (Result) then
            DeleteTabSheetTreeNode(lNode);
          SetSystemMenuState;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoConvertChannel(AChannelNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.DoConvertChannel';
var
  LNode       : TTreeNode;
  LDataObject : TViewDataTreeNodeData;
  lChannel    : TGeneralFlowChannel;
begin
  Result := False;
  try
    if (AChannelNumber > 0) then
    begin
      Result := DoConvertToGeneralFlow(AChannelNumber);
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject    := TViewDataTreeNodeData((LNode.Data));
          AChannelNumber := LDataObject.ViewDataNode.Weighting;
          Result         := DoConvertToGeneralFlow(AChannelNumber);
          if Result then
          begin
            lChannel       := YieldModelDataObject.CastNetworkElementData.
                                CastChannelList.CastChannelByChannelNumber[AChannelNumber];
            DeleteTabSheetTreeNode(lNode);
            CreateTabSheetTreeViewElement('ChannelDetails12',
                                       'ChannelHeading,ChannelDetails12',
                                       AChannelNumber,tvnidChannelDetails12,
                                       lChannel.ChannelName,
                                       'GENERALCHANNEL',
                                       'CHANNEL',
                                        True,tvsnAll);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoConvertToGeneralFlow (AChannelNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.DoConvertToGeneralFlow';
var
  lChannel : TGeneralFlowChannel;
  LMessage : string;
begin
  Result := FALSE;
  try
    if (AChannelNumber > 0) then
    begin
      lChannel := YieldModelDataObject.CastNetworkElementData.
                    CastChannelList.CastChannelByChannelNumber[AChannelNumber];
      if (lChannel <> nil) then
      begin
        Result := TRUE;
        if (lChannel.MinimumFlowConstraintID <> 0) then
        begin
          LMessage := Format('Minimum Flow Channel Number [%d] will be converted into a General flow Channel, do you want to continue?',[AChannelNumber]);
          if MessageDlg(LMessage,mtConfirmation,[mbOK, mbCancel], 0) = mrOK then
          begin
            if (NOT YieldModelDataObject.CastNetworkFeaturesData.CastMinimumFlowConstraintList.
                RemoveMinimumFlowConstraintWithID(lChannel.MinimumFlowConstraintID)) then
              Result := FALSE;
          end
          else
            Result := False;
        end;

        if (lChannel.LossFeatureID <> 0) then
        begin
          LMessage := Format('Loss Channel Number [%d] will be converted into a General flow Channel, do you want to continue?',[AChannelNumber]);
          if MessageDlg(LMessage,mtConfirmation,[mbOK, mbCancel], 0) = mrOK then
          begin
            if (NOT YieldModelDataObject.CastNetworkFeaturesData.CastLossFeatureList.
                RemoveLossFeatureWithID(lChannel.LossFeatureID)) then
            Result := FALSE;
          end
          else
            Result := False;
        end;

        if (lChannel.PumpingFeatureID <> 0) then
        begin
          LMessage := Format('Pumping Channel Number [%d] will be converted into a General flow Channel, do you want to continue?',[AChannelNumber]);
          if MessageDlg(LMessage,mtConfirmation,[mbOK, mbCancel], 0) = mrOK then
          begin
            if (NOT YieldModelDataObject.CastNetworkFeaturesData.CastPumpingFeatureList.
                 RemovePumpingFeatureWithID(lChannel.PumpingFeatureID)) then
            Result := FALSE;
          end
          else
            Result := False;

          if Result then
          begin
            if (NOT YieldModelDataObject.CastNetworkFeaturesData.CastMinMaxFlowConstraintList.
                    RemoveMinMaxFlowConstraintWithID(lChannel.MinMaxFlowConstraintID)) then
                Result := FALSE;
          end;
        end;

        if ((lChannel.MinMaxFlowConstraintID <> 0)  and (lChannel.PumpingFeatureID = 0) and
            (lChannel.IFRFeatureID = 0)) then
        begin
          if (NOT YieldModelDataObject.CastNetworkFeaturesData.CastMinMaxFlowConstraintList.
                    RemoveMinMaxFlowConstraintWithID(lChannel.MinMaxFlowConstraintID)) then
          Result := FALSE;
        end;

        if (lChannel.SpecifiedDemandFeatureID <> 0) then
        begin
          LMessage := Format('Specified demand Channel Number [%d] will be converted into a General flow Channel, do you want to continue?',[AChannelNumber]);
          if MessageDlg(LMessage,mtConfirmation,[mbOK, mbCancel], 0) = mrOK then
          begin
            if (NOT YieldModelDataObject.
                    CastNetworkFeaturesData.CastSpecifiedDemandFeatureList.
                    RemoveSpecifiedDemandFeatureWithID(lChannel.SpecifiedDemandFeatureID)) then
            Result := FALSE;
          end
          else
            Result := False;
        end;

        if (lChannel.DiversionFeatureID <> 0) then
        begin
          LMessage := Format('Diversion Channel Number [%d] will be converted into a General flow Channel, do you want to continue?',[AChannelNumber]);
          if MessageDlg(LMessage,mtConfirmation,[mbOK, mbCancel], 0) = mrOK then
          begin
            if (NOT YieldModelDataObject.CastNetworkFeaturesData.CastDiversionFeatureList.
                      RemoveDiversionFeatureWithID(lChannel.DiversionFeatureID)) then
            Result := FALSE;
          end
          else
            Result := False;
        end;

        if (lChannel.PhysicalFlowConstraintID <> 0) then
        begin
          LMessage := Format('Physical Flow Channel Number [%d] will be converted into a General flow Channel, do you want to continue?',[AChannelNumber]);
          if MessageDlg(LMessage,mtConfirmation,[mbOK, mbCancel], 0) = mrOK then
          begin
            if (NOT YieldModelDataObject.
                    CastNetworkFeaturesData.CastPhysicalFlowConstraintList.
                      RemovePhysicalFlowConstraintWithID(lChannel.PhysicalFlowConstraintID)) then
            Result := FALSE;
          end
          else
            Result := False;
        end;

        if (lChannel.IFRFeatureID <> 0) then
        begin
          LMessage := Format('IFR Channel Number [%d] will be converted into a General flow Channel, do you want to continue?',[AChannelNumber]);
          if MessageDlg(LMessage,mtConfirmation,[mbOK, mbCancel], 0) = mrOK then
          begin
            if (NOT YieldModelDataObject.CastNetworkFeaturesData.CastIFRFeatureList.
                      RemoveIFRFeatureWithID(lChannel.IFRFeatureID)) then
            Result := FALSE;
          end
          else
            Result := False;

          if Result then
          begin
            if (NOT YieldModelDataObject.CastNetworkFeaturesData.CastMinMaxFlowConstraintList.
                    RemoveMinMaxFlowConstraintWithID(lChannel.MinMaxFlowConstraintID)) then
                Result := FALSE;
          end;
        end;

        if (lChannel.SpecifiedInflowFeatureID <> 0) then
        begin
          LMessage := Format('Specified Inflow Channel Number [%d] will be converted into a General flow Channel, do you want to continue?',[AChannelNumber]);
          if MessageDlg(LMessage,mtConfirmation,[mbOK, mbCancel], 0) = mrOK then
          begin
            if (NOT YieldModelDataObject.
                    CastNetworkFeaturesData.CastSpecifiedInflowDataList.
                      RemoveSpecifiedInflowFeatureWithID(lChannel.SpecifiedInflowFeatureID)) then
            Result := FALSE;
          end
          else
            Result := False;
        end;

        if(lChannel.MasterControlFeatureID <> 0) then
        begin
          LMessage := Format('Master Control Channel Number [%d] will be converted into a General flow Channel, do you want to continue?',[AChannelNumber]);
          if MessageDlg(LMessage,mtConfirmation,[mbOK, mbCancel], 0) = mrOK then
          begin
            if (NOT YieldModelDataObject.
                    CastNetworkFeaturesData.CastMasterControlFeatureList.
                      RemoveMasterControlFeatureWithID(lChannel.MasterControlFeatureID)) then
            Result := FALSE;
            Result := YieldModelDataObject.NetworkFeaturesData.WaterDemandConfiguration.DeleteWaterUseOutputProportion(lChannel.ChannelNumber);
          end
          else
            Result := False;
        end;

        if Result then
          lChannel.DeleteAllFeatures;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoDeleteAllFeatures(AChannelNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.DoDeleteAllFeatures';
var
  lChannel : TGeneralFlowChannel;
begin
  Result := FALSE;
  try
    if (AChannelNumber > 0) then
    begin
      lChannel := YieldModelDataObject.CastNetworkElementData.
                    CastChannelList.CastChannelByChannelNumber[AChannelNumber];
      if (lChannel <> nil) then
      begin
        Result := TRUE;
        if (lChannel.MinimumFlowConstraintID <> 0) then
        begin
          if (NOT YieldModelDataObject.
                    CastNetworkFeaturesData.CastMinimumFlowConstraintList.
                      RemoveMinimumFlowConstraintWithID(lChannel.MinimumFlowConstraintID)) then
            Result := FALSE;
        end;
        if (lChannel.LossFeatureID <> 0) then
        begin
          if (NOT YieldModelDataObject.
                    CastNetworkFeaturesData.CastLossFeatureList.
                      RemoveLossFeatureWithID(lChannel.LossFeatureID)) then
            Result := FALSE;
        end;
        if (lChannel.PumpingFeatureID <> 0) then
        begin
          if (NOT YieldModelDataObject.
                    CastNetworkFeaturesData.CastPumpingFeatureList.
                      RemovePumpingFeatureWithID(lChannel.PumpingFeatureID)) then
            Result := FALSE;
        end;
        if (lChannel.MinMaxFlowConstraintID <> 0) then
        begin
          if (NOT YieldModelDataObject.
                    CastNetworkFeaturesData.CastMinMaxFlowConstraintList.
                      RemoveMinMaxFlowConstraintWithID(lChannel.MinMaxFlowConstraintID)) then
            Result := FALSE;
        end;
        if (lChannel.SpecifiedDemandFeatureID <> 0) then
        begin
          if (NOT YieldModelDataObject.
                    CastNetworkFeaturesData.CastSpecifiedDemandFeatureList.
                      RemoveSpecifiedDemandFeatureWithID(lChannel.SpecifiedDemandFeatureID)) then
            Result := FALSE;
        end;
        if (lChannel.DiversionFeatureID <> 0) then
        begin
          if (NOT YieldModelDataObject.
                    CastNetworkFeaturesData.CastDiversionFeatureList.
                      RemoveDiversionFeatureWithID(lChannel.DiversionFeatureID)) then
            Result := FALSE;
        end;
        if (lChannel.PhysicalFlowConstraintID <> 0) then
        begin
          if (NOT YieldModelDataObject.
                    CastNetworkFeaturesData.CastPhysicalFlowConstraintList.
                      RemovePhysicalFlowConstraintWithID(lChannel.PhysicalFlowConstraintID)) then
            Result := FALSE;
        end;
        if (lChannel.IFRFeatureID <> 0) then
        begin
          if (NOT YieldModelDataObject.
                    CastNetworkFeaturesData.CastIFRFeatureList.
                      RemoveIFRFeatureWithID(lChannel.IFRFeatureID)) then
            Result := FALSE;
        end;
        if (lChannel.SpecifiedInflowFeatureID <> 0) then
        begin
          if (NOT YieldModelDataObject.
                    CastNetworkFeaturesData.CastSpecifiedInflowDataList.
                      RemoveSpecifiedInflowFeatureWithID(lChannel.SpecifiedInflowFeatureID)) then
            Result := FALSE;
        end;
        lChannel.DeleteAllFeatures;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TYieldModelManager.DoCreateMinimumFlowFeature(AChannelNumber: Integer): IMinimumFlowConstraint;
const OPNAME = 'TYieldModelManager.DoCreateMinimumFlowFeature';
var
  LNode          : TTreeNode;
  LDataObject    : TViewDataTreeNodeData;
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  LMessage       : string;
begin
  Result := nil;
  try
    begin
      LMessage := '';
      if(AChannelNumber > 0) then
      begin
        LMessage := Format('Channel Number [%d] will be converted into a minimum flow channel, do you want to continue?',[AChannelNumber]);
        if MessageDlg(LMessage,mtConfirmation,[mbOK, mbCancel], 0) = mrOK then
        begin
          lChannelList := YieldModelDataObject.
                           NetworkElementData.ChannelList;
          lChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
          if (lChannel <> nil) then
          begin
            Result := YieldModelDataObject.
                        NetworkFeaturesData.MinimumFlowConstraintList.
                          CreateMinimumFlowConstraint;
            if (Result <> nil) then
            begin
              lChannel.MinimumFlowConstraint := Result;
              Result.Channel                 := lChannel;
              lChannel.ChannelType           := 6;
            end;
          end;
        end;
      end
      else
      begin
        if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
        begin
          LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
          if (Assigned(LNode) AND Assigned(LNode.Data)) then
          begin
            LDataObject := TViewDataTreeNodeData((LNode.Data));
            AChannelNumber := LDataObject.ViewDataNode.Weighting;
            LMessage := Format('Channel Number [%d] will be converted into a minimum flow channel, do you want to continue?',[AChannelNumber]);
            if MessageDlg(LMessage,mtConfirmation,[mbOK, mbCancel], 0) = mrOK then
            begin
              lChannelList := YieldModelDataObject.
                                NetworkElementData.ChannelList;
              lChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
              if (lChannel <> nil) then
              begin
                Result := YieldModelDataObject.
                              NetworkFeaturesData.MinimumFlowConstraintList.
                                CreateMinimumFlowConstraint;
                if (Result <> nil) then
                begin
                  lChannel.MinimumFlowConstraint := Result;
                  Result.Channel               := lChannel;
                  lChannel.ChannelType           := 6;
                end;
              end;
              DeleteTabSheetTreeNode(lNode);
              CreateTabSheetTreeViewElement('ChannelDetails6',
                                         'ChannelHeading,ChannelDetails6',
                                         lChannel.ChannelNumber,tvnidChannelDetails6,
                                         lChannel.ChannelName,
                                         'MINFLOWCHANNEL',
                                         'CHANNEL',
                                          True,tvsnAll);
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCreateMinMaxFlowFeature(AChannelNumber: Integer): IMinMaxFlowConstraint;
const OPNAME = 'TYieldModelManager.DoCreateMinMaxFlowFeature';
var
  LNode        : TTreeNode;
  LDataObject  : TViewDataTreeNodeData;
  lChannelList :  IChannelList;
  lChannel     : IGeneralFlowChannel;
  LMessage     : string;
begin
  Result := nil;
  try
    begin
      if not FMineCreated then
      begin
        LMessage := 'selected channel will be converted into a Min-Max Flow channel ,do you want to continue?';
        if MessageDlg(LMessage,mtConfirmation,[mbOK, mbCancel], 0) = mrOK then
          FChannelConvert := True
        else
          FChannelConvert := False;
      end
      else
        FChannelConvert := True;


      if FChannelConvert then
      begin
        if(AChannelNumber > 0) then
        begin
          begin
            lChannelList := YieldModelDataObject.
                              NetworkElementData.ChannelList;
            lChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
            if (lChannel <> nil) then
            begin
              Result := YieldModelDataObject.
                            NetworkFeaturesData.MinMaxFlowConstraintList.
                              CreateMinMaxFlowConstraint;
              if (Result <> nil) then
              begin
                lChannel.MinMaxFlowConstraint := Result;
                lChannel.ChannelType          := 8;
                Result.Channel                := lChannel;
                Result.CreateFlowConstraints;
                FMineCreated := True;
              end;
            end;
          end;
        end
        else
        begin
          if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
          begin
            LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
            if (Assigned(LNode) AND Assigned(LNode.Data)) then
            begin
              LDataObject := TViewDataTreeNodeData((LNode.Data));
              AChannelNumber := LDataObject.ViewDataNode.Weighting;
              lChannelList := YieldModelDataObject.NetworkElementData.ChannelList;
              lChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
              if (lChannel <> nil) then
              begin
                Result := YieldModelDataObject.
                              NetworkFeaturesData.MinMaxFlowConstraintList.
                                CreateMinMaxFlowConstraint;
                if (Result <> nil) then
                begin
                  lChannel.MinMaxFlowConstraint := Result;
                  lChannel.ChannelType          := 8;
                  Result.Channel                := lChannel;
                  Result.CreateFlowConstraints;
                  FMineCreated := True;
                end;
              end;
              DeleteTabSheetTreeNode(lNode);
              CreateTabSheetTreeViewElement('ChannelDetails8',
                                         'ChannelHeading,ChannelDetails8',
                                         lChannel.ChannelNumber,tvnidChannelDetails8,
                                         lChannel.ChannelName,
                                         'MINMAXCHANNEL',
                                         'CHANNEL',
                                          True,tvsnAll);
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCreatePumpingFeature(AChannelNumber: Integer): IPumpingFeature;
const OPNAME = 'TYieldModelManager.DoCreatePumpingFeature';
var
  LNode          : TTreeNode;
  LDataObject    : TViewDataTreeNodeData;
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  LMessage       : string;
begin
  Result := nil;
  try
    begin
      if(AChannelNumber > 0) then
      begin
        LMessage := Format('Channel Number [%d] will be converted into a pumping channel ,do you want to continue?',[AChannelNumber]);
        if MessageDlg(LMessage,mtConfirmation,[mbOK, mbCancel], 0) = mrOK then
        begin
          lChannelList := YieldModelDataObject.
                            NetworkElementData.ChannelList;
          lChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
          if (lChannel <> nil) then
          begin
            Result := YieldModelDataObject.
                          NetworkFeaturesData.PumpingFeatureList.
                            CreatePumpingFeature;
            if (Result <> nil) then
            begin
              lChannel.PumpingFeature   := Result;
              lChannel.ChannelType      := 9;
              Result.Channel            := lChannel;
            end;
          end;
        end;
      end
      else
      begin
        if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
        begin
          LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
          if (Assigned(LNode) AND Assigned(LNode.Data)) then
          begin
            LDataObject := TViewDataTreeNodeData((LNode.Data));
            AChannelNumber := LDataObject.ViewDataNode.Weighting;
            LMessage := Format('Channel Number [%d] will be converted into a pumping channel ,do you want to continue?',[AChannelNumber]);
            if MessageDlg(LMessage,mtConfirmation,[mbOK, mbCancel], 0) = mrOK then
            begin
              lChannelList := YieldModelDataObject.NetworkElementData.ChannelList;
              lChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
              if (lChannel <> nil) then
              begin
                Result := YieldModelDataObject.
                              NetworkFeaturesData.PumpingFeatureList.
                                CreatePumpingFeature;
                if (Result <> nil) then
                begin
                  lChannel.PumpingFeature   := Result;
                  lChannel.ChannelType      := 9;
                  Result.Channel            := lChannel;
                end;
              end;
              DeleteTabSheetTreeNode(lNode);
              CreateTabSheetTreeViewElement('ChannelDetails9',
                                         'ChannelHeading,ChannelDetails9',
                                         lChannel.ChannelNumber,tvnidChannelDetails9,
                                         lChannel.ChannelName,
                                         'PUMPINGCHANNEL',
                                         'CHANNEL',
                                          True,tvsnAll);
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCreateLossFeature(AChannelNumber: Integer): ILossFeature;
const OPNAME = 'TYieldModelManager.DoCreateLossFeature';
var
  LNode          : TTreeNode;
  LDataObject    : TViewDataTreeNodeData;
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  LMessage       : string;
begin
  Result := nil;
  try
    begin
      if(AChannelNumber > 0) then
      begin
        LMessage := format('Channel Number [%d] will be converted into a loss channel, do you want to continue?',[AChannelNumber]);
        if MessageDlg(LMessage,mtConfirmation,[mbOK, mbCancel], 0) = mrOK then
        begin
          lChannelList := YieldModelDataObject.NetworkElementData.ChannelList;
          lChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
          if (lChannel <> nil) then
          begin
            Result := YieldModelDataObject.
                          NetworkFeaturesData.LossFeatureList.CreateLossFeature;
            if (Result <> nil) then
            begin
              lChannel.LossFeature := Result;
              Result.Channel       := lChannel;
              lChannel.ChannelType := 7;
            end;
          end;
        end;
      end
      else
      begin
        if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
        begin
          LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
          if (Assigned(LNode) AND Assigned(LNode.Data)) then
          begin
            LDataObject := TViewDataTreeNodeData((LNode.Data));
            AChannelNumber := LDataObject.ViewDataNode.Weighting;
            LMessage := format('Channel Number [%d] will be converted into a loss channel, do you want to continue?',[AChannelNumber]);
            if MessageDlg(LMessage,mtConfirmation,[mbOK, mbCancel], 0) = mrOK then
            begin
              lChannelList := YieldModelDataObject.NetworkElementData.ChannelList;
              lChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
              if (lChannel <> nil) then
              begin
                Result := YieldModelDataObject.
                              NetworkFeaturesData.LossFeatureList.
                                CreateLossFeature;
                if (Result <> nil) then
                begin
                  lChannel.LossFeature := Result;
                  Result.Channel       := lChannel;
                  lChannel.ChannelType := 7;
                end;
              end;
              DeleteTabSheetTreeNode(lNode);
              CreateTabSheetTreeViewElement('ChannelDetails7',
                                         'ChannelHeading,ChannelDetails7',
                                         lChannel.ChannelNumber,tvnidChannelDetails7,
                                         lChannel.ChannelName,
                                         'LOSSCHANNEL',
                                         'CHANNEL',
                                          True,tvsnAll);
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCreateSpecifiedDemandFeature(AChannelNumber: Integer): ISpecifiedDemandFeature;
const OPNAME = 'TYieldModelManager.DoCreateSpecifiedDemandFeature';
var
  LNode          : TTreeNode;
  LDataObject    : TViewDataTreeNodeData;
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  LMessage       : string;
begin
  Result := nil;
  try
    begin
      if(AChannelNumber > 0) then
      begin
        LMessage := Format('Channel Number [%d] will be converted into a specified demand channel, do you want to continue?',[AChannelNumber]);
        if MessageDlg(LMessage,mtConfirmation,[mbOK, mbCancel], 0) = mrOK then
        begin
          lChannelList := YieldModelDataObject.
                            NetworkElementData.ChannelList;
          lChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
          if (lChannel <> nil) then
          begin
            Result := YieldModelDataObject.
                          CastNetworkFeaturesData.CastSpecifiedDemandFeatureList.
                            CreateSpecifiedDemandFeature;
            if (Result <> nil) then
            begin
              lChannel.SpecifiedDemandFeature := Result;
              Result.Channel                  := lChannel;
              lChannel.ChannelType            := 11;
            end;
          end;
        end;
      end
      else
      begin
        if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
        begin
          LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
          if (Assigned(LNode) AND Assigned(LNode.Data)) then
          begin
            LDataObject := TViewDataTreeNodeData((LNode.Data));
            AChannelNumber := LDataObject.ViewDataNode.Weighting;
            LMessage := Format('Channel Number [%d] will be converted into a specified demand channel, do you want to continue?',[AChannelNumber]);
            if MessageDlg(LMessage,mtConfirmation,[mbOK, mbCancel], 0) = mrOK then
            begin
              lChannelList := YieldModelDataObject.NetworkElementData.ChannelList;
              lChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
              if (lChannel <> nil) then
              begin
                Result := YieldModelDataObject.
                              CastNetworkFeaturesData.CastSpecifiedDemandFeatureList.
                                CreateSpecifiedDemandFeature;
                if (Result <> nil) then
                begin
                  lChannel.SpecifiedDemandFeature := Result;
                  Result.Channel                  := lChannel;
                  lChannel.ChannelType            := 11;
                end;
              end;
              DeleteTabSheetTreeNode(lNode);
              CreateTabSheetTreeViewElement('ChannelDetails11',
                                         'ChannelHeading,ChannelDetails11',
                                         lChannel.ChannelNumber,tvnidChannelDetails11,
                                         lChannel.ChannelName,
                                         'DEMANDCHANNEL',
                                         'CHANNEL',True,tvsnAll);
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCreateDiversionFeature(AChannelNumber: Integer): IDiversionFeature;
const OPNAME = 'TYieldModelManager.DoCreateDiversionFeature';
var
  LNode          : TTreeNode;
  LDataObject    : TViewDataTreeNodeData;
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  LMessage       : string;
begin
  Result := nil;
  try
    begin
      if(AChannelNumber > 0) then
      begin
        LMessage := Format('Channel Number [%d] will be converted into a diversion flow channel, do you want to continue?',[AChannelNumber]);
        if MessageDlg(LMessage,mtConfirmation,[mbOK, mbCancel], 0) = mrOK then
        begin
          lChannelList := YieldModelDataObject.
                            NetworkElementData.ChannelList;
          lChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
          if (lChannel <> nil) then
          begin
            Result := YieldModelDataObject.
                          NetworkFeaturesData.DiversionFeatureList.
                            CreateDiversionFeature;
            if (Result <> nil) then
            begin
              lChannel.DiversionFeature := Result;
              Result.Channel            := lChannel;
              lChannel.ChannelType      := 5;
            end;
          end;
        end;
      end
      else
      begin
        if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
        begin
          LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
          if (Assigned(LNode) AND Assigned(LNode.Data)) then
          begin
            LDataObject := TViewDataTreeNodeData((LNode.Data));
            AChannelNumber := LDataObject.ViewDataNode.Weighting;
            LMessage := Format('Channel Number [%d] will be converted into a diversion flow channel, do you want to continue?',[AChannelNumber]);
            if MessageDlg(LMessage,mtConfirmation,[mbOK, mbCancel], 0) = mrOK then
            begin
              lChannelList := YieldModelDataObject.NetworkElementData.ChannelList;
              lChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
              if (lChannel <> nil) then
              begin
                Result := YieldModelDataObject.
                              NetworkFeaturesData.DiversionFeatureList.
                                CreateDiversionFeature;
                if (Result <> nil) then
                begin
                  lChannel.DiversionFeature := Result;
                  Result.Channel            := lChannel;
                  lChannel.ChannelType      := 5;
                end;
              end;
              DeleteTabSheetTreeNode(lNode);
              CreateTabSheetTreeViewElement('ChannelDetails5',
                                         'ChannelHeading,ChannelDetails5',
                                         lChannel.ChannelNumber,tvnidChannelDetails5,
                                         lChannel.ChannelName,
                                         'DIVERSIONCHANNEL',
                                         'CHANNEL',True,tvsnAll);
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCreateSpecifiedInflowFeature(AChannelNumber: Integer): ISpecifiedInflowFeature;
const OPNAME = 'TYieldModelManager.DoCreateSpecifiedInflowFeature';
var
  LNode          : TTreeNode;
  LDataObject    : TViewDataTreeNodeData;
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  LMessage       : string;
begin
  Result := nil;
  try
    begin
      if(AChannelNumber > 0) then
      begin
        LMessage := Format('Channel Number [%d] will be converted into a specified inflow channel, do you want to continue?',[AChannelNumber]);
        if MessageDlg(LMessage,mtConfirmation,[mbOK, mbCancel], 0) = mrOK then
        begin
          lChannelList := YieldModelDataObject.
                            NetworkElementData.ChannelList;
          lChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
          if (lChannel <> nil) then
          begin
            Result := YieldModelDataObject.
                          NetworkFeaturesData.SpecifiedInflowFeatureList.
                            CreateSpecifiedInflowFeature;
            if (Result <> nil) then
            begin
              lChannel.SpecifiedInflowFeature := Result;
              Result.Channel                  := lChannel;
              lChannel.ChannelType            := 10;
            end;
          end;
        end;
      end
      else
      begin
        if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
        begin
          LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
          if (Assigned(LNode) AND Assigned(LNode.Data)) then
          begin
            LDataObject := TViewDataTreeNodeData((LNode.Data));
            AChannelNumber := LDataObject.ViewDataNode.Weighting;
            LMessage := Format('Channel Number [%d] will be converted into a specified inflow channel, do you want to continue?',[AChannelNumber]);
            if MessageDlg(LMessage,mtConfirmation,[mbOK, mbCancel], 0) = mrOK then
            begin
              lChannelList := YieldModelDataObject.NetworkElementData.ChannelList;
              lChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
              if (lChannel <> nil) then
              begin
                Result := YieldModelDataObject.
                              NetworkFeaturesData.SpecifiedInflowFeatureList.
                                CreateSpecifiedInflowFeature;
                if (Result <> nil) then
                begin
                  lChannel.SpecifiedInflowFeature := Result;
                  Result.Channel                  := lChannel;
                  lChannel.ChannelType            := 10;
                end;
              end;
              DeleteTabSheetTreeNode(lNode);
              CreateTabSheetTreeViewElement('ChannelDetails10',
                                         'ChannelHeading,ChannelDetails10',
                                         lChannel.ChannelNumber,tvnidChannelDetails10,
                                         lChannel.ChannelName,
                                         'INFLOWCHANNEL',
                                         'CHANNEL',True,tvsnAll);
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TYieldModelManager.DoCreateIFRFeature(AChannelNumber: Integer; AIFRType : TIFRFeatureReferenceFlowType): IIFRFeature;
const OPNAME = 'TYieldModelManager.DoCreateIFRFeature';
var
  LNode          : TTreeNode;
  LDataObject    : TViewDataTreeNodeData;
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  LMsg           : string;
  LMesgDlgResult : word;
  LMessage       : string;
begin
  Result := nil;
  try
    if(AIFRType = ifrtNone) then
    begin
      LMsg := FAppModules.Language.GetString('Message.IFRType');
      LMesgDlgResult := WRMFMessageDialog(LMsg,mtConfirmation,mbYesNoCancel,
                        [FAppModules.Language.GetString('TIFRFeatureDialog.MonthlyRefenceFlow'),
                         FAppModules.Language.GetString('TIFRFeatureDialog.AnnualRefenceFlow')]);
      if (LMesgDlgResult = mrCancel) then Exit;
      if (LMesgDlgResult = mrYes) then
        AIFRType := ifrtMonthly
      else
        AIFRType := ifrrftAnnual;
    end;

    if(AChannelNumber > 0) then
    begin
      LMessage := Format('Channel Number [%d] will be converted into a IFR channel, do you want to continue?',[AChannelNumber]);
      if MessageDlg(LMessage,mtConfirmation,[mbOK, mbCancel], 0) = mrOK then
      begin
        lChannelList := YieldModelDataObject.
                        NetworkElementData.ChannelList;
        lChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
        if (lChannel <> nil) then
        begin
          Result := YieldModelDataObject. NetworkFeaturesData.IFRFeatureList.CreateIFRFeature(AIFRType);
          if (Result <> nil) then
          begin
            lChannel.IFRFeature       := Result;
            Result.Channel            := lChannel;
            ResetMinMaxChannelFlow(Result);
          end;
        end;
      end;
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          AChannelNumber := LDataObject.ViewDataNode.Weighting;
          LMessage := Format('Channel Number [%d] will be converted into aa IFR channel, do you want to continue?',[AChannelNumber]);
          if MessageDlg(LMessage,mtConfirmation,[mbOK, mbCancel], 0) = mrOK then
          begin
            lChannelList := YieldModelDataObject.NetworkElementData.ChannelList;
            lChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
            if (lChannel <> nil) then
            begin
              Result := YieldModelDataObject.NetworkFeaturesData.IFRFeatureList.CreateIFRFeature(AIFRType);
              if (Result <> nil) then
              begin
                lChannel.IFRFeature       := Result;
                Result.Channel            := lChannel;
                ResetMinMaxChannelFlow(Result);
              end;
            end;
            DeleteTabSheetTreeNode(lNode);
            CreateTabSheetTreeViewElement('ChannelDetails18',
                                       'ChannelHeading,ChannelDetails18',
                                       lChannel.ChannelNumber,tvnidChannelDetails18,
                                       lChannel.ChannelName,
                                       'IFRCHANNEL',
                                       'CHANNEL',True,tvsnAll);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
(*
function TYieldModelManager.DoCreateAnnualIFRFeature(AChannelNumber: Integer): IIFRFeature;
const OPNAME = 'TYieldModelManager.DoCreateAnnualIFRFeature';
{var
  LNode          : TTreeNode;
  LDataObject    : TViewDataTreeNodeData;
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  }
begin
  Result := nil;
  try
    {if(AChannelNumber > 0) then
    begin
      lChannelList := YieldModelDataObject.
                        NetworkElementData.ChannelList;
      lChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
      if (lChannel <> nil) then
      begin
        Result := YieldModelDataObject.NetworkFeaturesData.IFRFeatureList.CreateAnnualIFRFeature;
        if (Result <> nil) then
        begin
          lChannel.IFRFeature       := Result;
          Result.Channel            := lChannel;
        end;
      end;
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          AChannelNumber := LDataObject.ViewDataNode.Weighting;
          lChannelList := YieldModelDataObject.
                            NetworkElementData.ChannelList;
          lChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
          if (lChannel <> nil) then
          begin
            Result := YieldModelDataObject.
                          NetworkFeaturesData.IFRFeatureList.CreateIFRFeature;
            if (Result <> nil) then
            begin
              lChannel.IFRFeature       := Result;
              Result.Channel            := lChannel;
            end;
          end;
          DeleteTabSheetTreeNode(lNode);
          CreateTabSheetTreeViewElement('ChannelDetails18',
                                     'ChannelHeading,ChannelDetails18',
                                     lChannel.ChannelNumber,tvnidChannelDetails18,
                                     lChannel.ChannelName,
                                     'IFRCHANNEL',
                                     'CHANNEL');
        end;
      end;
    end;
    }
  except on E: Exception do HandleError(E, OPNAME) end;
end;
*)

function TYieldModelManager.DoCreatePhysicalFlowConstraint(AChannelNumber: Integer): IPhysicalFlowConstraint;
const OPNAME = 'TYieldModelManager.DoCreatePhysicalFlowConstraint';
var
  LNode          : TTreeNode;
  LDataObject    : TViewDataTreeNodeData;
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  LMessage       : string;
begin
  Result := nil;
  try
    begin
      lChannelList := YieldModelDataObject.
                        NetworkElementData.ChannelList;
      lChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
      if (lChannel <> nil) then
      begin
        LMessage :=  Format('Channel Number [%d] will be converted into a physical flow constraint channel, do you want to continue?',[AChannelNumber]);
        if MessageDlg(LMessage,mtConfirmation,[mbOK, mbCancel], 0) = mrOK then
        begin
          Result := YieldModelDataObject.CastNetworkFeaturesData.CastPhysicalFlowConstraintList.
                          CreatePhysicalFlowConstraint(AChannelNumber);
          if (Result <> nil) then
          begin
            lChannel.PhysicalFlowConstraint := Result;
  //          lChannel.ChannelType            := 19;
          end;
        end;
      end
      else
      begin
        if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
        begin
          LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
          if (Assigned(LNode) AND Assigned(LNode.Data)) then
          begin
            LDataObject := TViewDataTreeNodeData((LNode.Data));
            AChannelNumber := LDataObject.ViewDataNode.Weighting;
            LMessage :=  Format('Channel Number [%d] will be converted into a physical flow constraint channel, do you want to continue?',[AChannelNumber]);
            if MessageDlg(LMessage,mtConfirmation,[mbOK, mbCancel], 0) = mrOK then
            begin
              lChannelList := YieldModelDataObject.NetworkElementData.ChannelList;
              lChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
              if (lChannel <> nil) then
              begin
              Result := YieldModelDataObject.CastNetworkFeaturesData.
                              CastPhysicalFlowConstraintList.CreatePhysicalFlowConstraint(AChannelNumber);
                if (Result <> nil) then
                begin
                  lChannel.PhysicalFlowConstraint := Result;
    //              lChannel.ChannelType            := 19;
                end;
              end;
              DeleteTabSheetTreeNode(lNode);
              CreateTabSheetTreeViewElement('ChannelDetails19',
                                         'ChannelHeading,ChannelDetails19',
                                         lChannel.ChannelNumber,tvnidChannelDetails19,
                                         lChannel.ChannelName,
                                         'PFCCHANNEL',
                                         'CHANNEL',True,tvsnAll);
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCreateIrrigationArea: IIrrigationArea;
const OPNAME = 'TYieldModelManager.DoCreateIrrigationArea';
//var
//  lNode : IReservoirData;
begin
  Result := nil;
  try
    Result := YieldModelDataObject.
               NetworkFeaturesData.IrrigationAreaList.CreateIrrigationArea;
    {lNode := YieldModelDataObject.
               NetworkElementData.ReservoirList.IrrigationNodeByIdentifier
                 [Result.ConsumptiveChannel.UpStreamNodeNumber];
    if (lNode <> nil) then
      CreateTabSheetTreeViewElement('NodesWithoutInFlow',
                                 'NodesHeading,NodesWithoutInFlow',
                                 lNode.ReservoirConfigurationData.ReservoirIdentifier,tvnidNodesWithoutInFlow,
                                 LNode.ReservoirConfigurationData.ReservoirName,
                                 '',
                                 'NODEWITHOUTINFLOW', FALSE,tvsnAll);}
    CreateTabSheetTreeViewElement('IrrigationAreas',
                               'FeaturesHeading,IrrigationAreas',
                               Result.IrrigationNodeNumber,tvnidIrrigationAreas,
                               Result.FeatureName,
                               'IRRIGATIONCHANNEL',
                               'IRRIGATIONAREA',True,tvsnAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCreatePowerPlant: IPowerPlant;
const OPNAME = 'TYieldModelManager.DoCreatePowerPlant';
begin
  Result := nil;
  try
    Result := YieldModelDataObject.
                     NetworkFeaturesData.PowerPlantList.CreatePowerPlant;
    CreateTabSheetTreeViewElement('PowerPlants',
                               'FeaturesHeading,PowerPlants',
                               Result.FeatureID,tvnidPowerPlants,
                               Result.FeatureName,
                               'POWERCHANNEL',
                               'POWERPLANT',True,tvsnAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCreateMasterControlFeature(AChannelNumber: Integer): IMasterControlFeature;
const OPNAME = 'TYieldModelManager.DoCreateMasterControlFeature';
var
  LNode          : TTreeNode;
  LDataObject    : TViewDataTreeNodeData;
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  LMessage       : string;
begin
  Result := nil;
  try
    if(AChannelNumber > 0) then
    begin
      LMessage :=  Format('Channel Number [%d] will be converted into a master control channel ,do you want to continue?',[AChannelNumber]);
      if MessageDlg(LMessage,mtConfirmation,[mbOK, mbCancel], 0) = mrOK then
      begin
        lChannelList := YieldModelDataObject.
                          NetworkElementData.ChannelList;
        lChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
        if (lChannel <> nil) then
        begin
          Result := YieldModelDataObject.
                        NetworkFeaturesData.MasterControlFeatureList.
                          CreateMasterControlFeature(lChannel.ChannelNumber);
          if (Result <> nil) then
          begin
            YieldModelDataObject.NetworkFeaturesData.WaterDemandConfiguration.CreateNewWaterUseOutputProportion(AChannelNumber);
            DoCreateChannelTariff(lChannel.ChannelNumber);
            lChannel.MasterControlFeature := Result;
            Result.Channel              := lChannel;
            lChannel.ChannelType          := 2;
          end;
        end;
      end;
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          AChannelNumber := LDataObject.ViewDataNode.Weighting;
          LMessage :=  Format('Channel Number [%d] will be converted into a master control channel ,do you want to continue?',[AChannelNumber]);
          if MessageDlg(LMessage,mtConfirmation,[mbOK, mbCancel], 0) = mrOK then
          begin
            lChannelList := YieldModelDataObject.NetworkElementData.ChannelList;
            lChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
            if (lChannel <> nil) then
            begin
              Result := YieldModelDataObject.
                            NetworkFeaturesData.MasterControlFeatureList.
                              CreateMasterControlFeature(lChannel.ChannelNumber);
              if (Result <> nil) then
              begin
                YieldModelDataObject.NetworkFeaturesData.WaterDemandConfiguration.CreateNewWaterUseOutputProportion(AChannelNumber);
                DoCreateChannelTariff(lChannel.ChannelNumber);
                lChannel.MasterControlFeature := Result;
                Result.Channel              := lChannel;
                lChannel.ChannelType          := 2;
              end;
            end;
            DeleteTabSheetTreeNode(LNode);
            CreateTabSheetTreeViewElement('MasterControlConfiguration',
                                       'ParametersHeading,MasterControlConfiguration',
                                       lChannel.ChannelNumber,tvnidMasterControlConfiguration,
                                       lChannel.ChannelName,
                                       '',
                                       'CHANNEL',True,tvsnAll);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoDeletePumpingFeature(AChannelNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.DoDeletePumpingFeature';
var
  LNode       : TTreeNode;
  LDataObject : TViewDataTreeNodeData;
  lChannel    : TGeneralFlowChannel;
  lFeatureID  : integer;
begin
  Result := FALSE;
  try
    if (AChannelNumber > 0) then
    begin
      lChannel     := YieldModelDataObject.CastNetworkElementData.
                        CastChannelList.CastChannelByChannelNumber[AChannelNumber];
      if (lChannel <> nil) then
      begin
        lFeatureID := lChannel.PumpingFeatureID;
        Result     := YieldModelDataObject.CastNetworkFeaturesData.
                        PumpingFeatureList.RemovePumpingFeatureWithID(lFeatureID);
        Result     := Result and lChannel.DeletePumpingFeature;
        if Result then
          lChannel.ChannelType := 8;
      end;
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject    := TViewDataTreeNodeData((LNode.Data));
          AChannelNumber := LDataObject.ViewDataNode.Weighting;
          lChannel       := YieldModelDataObject.CastNetworkElementData.
                              CastChannelList.CastChannelByChannelNumber[AChannelNumber];
          if (lChannel <> nil) then
          begin
            lFeatureID := lChannel.PumpingFeatureID;
            Result     := YieldModelDataObject.CastNetworkFeaturesData.
                            PumpingFeatureList.RemovePumpingFeatureWithID(lFeatureID);
            Result     := Result and lChannel.DeletePumpingFeature;
            if Result then
              lChannel.ChannelType := 8;
          end;
          if Result then
          begin
            DeleteTabSheetTreeNode(lNode);
            CreateTabSheetTreeViewElement('ChannelDetails8',
                                       'ChannelHeading,ChannelDetails8',
                                       lChannel.ChannelNumber,tvnidChannelDetails8,
                                       lChannel.ChannelName,
                                       'MINMAXCHANNEL',
                                       'CHANNEL',True,tvsnAll);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoDeleteIFRFeature(AChannelNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.DoDeleteIFRFeature';
var
  LNode        : TTreeNode;
  LDataObject  : TViewDataTreeNodeData;
  lChannel     : TGeneralFlowChannel;
  lFeatureID   : integer;
begin
  Result := FALSE;
  try
    if (AChannelNumber > 0) then
    begin
      lChannel     := YieldModelDataObject.CastNetworkElementData.
                        CastChannelList.CastChannelByChannelNumber[AChannelNumber];
      if (lChannel <> nil) then
      begin
        lFeatureID := lChannel.IFRFeatureID;
        Result     := YieldModelDataObject.CastNetworkFeaturesData.
                        IFRFeatureList.RemoveIFRFeatureWithID(lFeatureID);
        Result     := Result AND lChannel.DeleteIFRFeature;
      end;
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject    := TViewDataTreeNodeData((LNode.Data));
          AChannelNumber := LDataObject.ViewDataNode.Weighting;
          lChannel       := YieldModelDataObject.CastNetworkElementData.
                              CastChannelList.CastChannelByChannelNumber[AChannelNumber];
          if (lChannel <> nil) then
          begin
            lFeatureID           := lChannel.IFRFeatureID;
            Result               := YieldModelDataObject.CastNetworkFeaturesData.
                                   IFRFeatureList.RemoveIFRFeatureWithID(lFeatureID);
            Result               := Result and lChannel.DeleteIFRFeature;
            lChannel.ChannelType := 8;
          end;
          DeleteTabSheetTreeNode(lNode);
          CreateTabSheetTreeViewElement('ChannelDetails8',
                                     'ChannelHeading,ChannelDetails8',
                                     lChannel.ChannelNumber,tvnidChannelDetails8,
                                     lChannel.ChannelName,
                                     'MINMAXCHANNEL',
                                     'CHANNEL',True,tvsnAll);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
(*
function TYieldModelManager.DoDeleteAnnualIFRFeature(AChannelNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.DoDeleteAnnualIFRFeature';
var
  LNode        : TTreeNode;
  LDataObject  : TViewDataTreeNodeData;
  lChannel     : TGeneralFlowChannel;
  lFeatureID   : integer;
begin
  Result := FALSE;
  try
    if (AChannelNumber > 0) then
    begin
      lChannel     := YieldModelDataObject.CastNetworkElementData.
                        CastChannelList.CastChannelByChannelNumber[AChannelNumber];
      if (lChannel <> nil) then
      begin
        lFeatureID := lChannel.IFRFeatureID;
        Result     := YieldModelDataObject.CastNetworkFeaturesData.IFRFeatureList.RemoveAnnualIFRFeatureWithID(lFeatureID);
        Result     := Result AND lChannel.DeleteIFRFeature;
      end;
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject    := TViewDataTreeNodeData((LNode.Data));
          AChannelNumber := LDataObject.ViewDataNode.Weighting;
          lChannel       := YieldModelDataObject.CastNetworkElementData.
                              CastChannelList.CastChannelByChannelNumber[AChannelNumber];
          if (lChannel <> nil) then
          begin
            lFeatureID           := lChannel.IFRFeatureID;
            Result               := YieldModelDataObject.CastNetworkFeaturesData.
                                   IFRFeatureList.RemoveIFRFeatureWithID(lFeatureID);
            Result               := Result and lChannel.DeleteIFRFeature;
            lChannel.ChannelType := 8;
          end;
          DeleteTabSheetTreeNode(lNode);
          CreateTabSheetTreeViewElement('ChannelDetails8',
                                     'ChannelHeading,ChannelDetails8',
                                     lChannel.ChannelNumber,tvnidChannelDetails8,
                                     lChannel.ChannelName,
                                     'MINMAXCHANNEL',
                                     'CHANNEL');
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
*)
function TYieldModelManager.DoDeletePhysicalFlowConstraint(AChannelNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.DoDeletePhysicalFlowConstraint';
var
  LNode        : TTreeNode;
  LDataObject  : TViewDataTreeNodeData;
  lChannel     : TGeneralFlowChannel;
  lFeatureID   : integer;
  LMessage     : string;
begin
  Result := FALSE;
  try
    if (AChannelNumber > 0) then
    begin
      LMessage :=  Format('Do you want to delete channel number [%d]?',[AChannelNumber]);
      if MessageDlg(LMessage,mtConfirmation,[mbOK, mbCancel], 0) = mrOK then
      begin
        lChannel     := YieldModelDataObject.CastNetworkElementData.
                        CastChannelList.CastChannelByChannelNumber[AChannelNumber];
        if (lChannel <> nil) then
        begin
          lFeatureID := lChannel.PhysicalFlowConstraintID;
          Result     := YieldModelDataObject.CastNetworkFeaturesData.
                        PhysicalFlowConstraintList.RemovePhysicalFlowConstraintWithID(lFeatureID);
          Result     := Result AND lChannel.DeletePhysicalFlowConstraint;
        end;
      end;
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject    := TViewDataTreeNodeData((LNode.Data));
          AChannelNumber := LDataObject.ViewDataNode.Weighting;
          LMessage :=  Format('Do you want to delete channel number [%d]?',[AChannelNumber]);
          if MessageDlg(LMessage,mtConfirmation,[mbOK, mbCancel], 0) = mrOK then
          begin
            lChannel       := YieldModelDataObject.CastNetworkElementData.
                                CastChannelList.CastChannelByChannelNumber[AChannelNumber];
            if (lChannel <> nil) then
            begin
              lFeatureID := lChannel.PhysicalFlowConstraintID;
              Result     := YieldModelDataObject.CastNetworkFeaturesData.
                              PhysicalFlowConstraintList.RemovePhysicalFlowConstraintWithID(lFeatureID);
              Result     := Result and lChannel.DeletePhysicalFlowConstraint;
              lChannel.ChannelType := 8;
            end;
            DeleteTabSheetTreeNode(lNode);
            CreateTabSheetTreeViewElement('ChannelDetails8',
                                       'ChannelHeading,ChannelDetails8',
                                       lChannel.ChannelNumber,tvnidChannelDetails8,
                                       lChannel.ChannelName,
                                       'MINMAXCHANNEL',
                                       'CHANNEL',True,tvsnAll);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.GetIrrigationFeatureID(ANodeNumber: integer): integer;
const OPNAME = 'TYieldModelManager.GetIrrigationFeatureID';
var
  LIrrigationArea: IIrrigationArea;
begin
  Result := NullInteger;
  try
    LIrrigationArea := YieldModelDataObject.CastNetworkFeaturesData.CastIrrigationAreaList.IrrigationAreaByNodeNumber[ANodeNumber];
    if(LIrrigationArea <> nil) then
      Result := LIrrigationArea.FeatureID;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoDeleteIrrigationArea(ANodeNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.DoDeleteIrrigationArea';
var
  lNode                 : TTreeNode;
  lDataObject           : TViewDataTreeNodeData;
  LFeatureID            : integer;
  lConsumptiveNodeNr    : integer;
  lDiversionChannelNr   : integer;
  lConsumptiveChannelNr : integer;
  lReturnFlowChannelNr  : integer;
begin
  Result := FALSE;
  try
    if (ANodeNumber > 0) then
    begin
      LFeatureID := GetIrrigationFeatureID(ANodeNumber);
      YieldModelDataObject.CastNetworkFeaturesData.
        CastIrrigationAreaList.GetChannelNumbers(LFeatureID, lDiversionChannelNr,
          lConsumptiveChannelNr, lReturnFlowChannelNr,lConsumptiveNodeNr);

      Result := DoDeleteChannel(lDiversionChannelNr);
      Result := Result AND DoDeleteChannel(lConsumptiveChannelNr);
      Result := Result AND DoDeleteChannel(lReturnFlowChannelNr);
      Result := Result AND YieldModelDataObject.
                             CastNetworkElementData.CastReservoirList.
                               DeleteIrrigationNode(lConsumptiveNodeNr);;
      Result := Result AND YieldModelDataObject.
                             CastNetworkFeaturesData.CastIrrigationAreaList.
                               RemoveIrrigationAreaWithID(LFeatureID);
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          ANodeNumber  := LDataObject.ViewDataNode.Weighting;
          LFeatureID := GetIrrigationFeatureID(ANodeNumber);

          YieldModelDataObject.CastNetworkFeaturesData.
            CastIrrigationAreaList.GetChannelNumbers(LFeatureID, lDiversionChannelNr,
              lConsumptiveChannelNr, lReturnFlowChannelNr, lConsumptiveNodeNr);

          Result := DoDeleteChannel(lDiversionChannelNr);
          Result := Result AND DoDeleteChannel(lConsumptiveChannelNr);
          Result := Result AND DoDeleteChannel(lReturnFlowChannelNr);
          Result := Result AND YieldModelDataObject.
                                 CastNetworkElementData.CastReservoirList.
                                   DeleteIrrigationNode(lConsumptiveNodeNr);;
          Result := Result AND YieldModelDataObject.
                                 CastNetworkFeaturesData.CastIrrigationAreaList.
                                   RemoveIrrigationAreaWithID(LFeatureID);
          if (Result) then
          begin
            DeleteTabSheetTreeViewElement('NodesWithoutInFlow','NodesHeading,NodesWithoutInFlow', lConsumptiveNodeNr,10010);
            DeleteTabSheetTreeNode(lNode);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoDeletePowerPlant(AFeatureID: Integer): WordBool;
const OPNAME = 'TYieldModelManager.DoDeletePowerPlant';
var
  LNode            : TTreeNode;
  LDataObject      : TViewDataTreeNodeData;
  LPowerChannellNr : integer;
  LSpillChannelNr  : integer;
begin
  Result := False;
  try
    if (AFeatureID > 0) then
    begin
      YieldModelDataObject.CastNetworkFeaturesData.
        CastPowerPlantList.GetChannelNumbers(AFeatureID, LPowerChannellNr, LSpillChannelNr);

      Result := DoDeleteChannel(LPowerChannellNr);
      Result := Result AND DoDeleteChannel(LSpillChannelNr);
      Result := Result AND YieldModelDataObject.
                             NetworkFeaturesData.PowerPlantList.RemovePowerPlantWithID(AFeatureID);
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          AFeatureID  := LDataObject.ViewDataNode.Weighting;

          YieldModelDataObject.CastNetworkFeaturesData.
            CastPowerPlantList.GetChannelNumbers(AFeatureID, LPowerChannellNr, LSpillChannelNr);

          Result := DoDeleteChannel(LPowerChannellNr);
          Result := Result AND DoDeleteChannel(LSpillChannelNr);
          Result := Result AND YieldModelDataObject.
                                 NetworkFeaturesData.PowerPlantList.RemovePowerPlantWithID(AFeatureID);
          Result  := Result and DeleteTabSheetTreeNode(LNode);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCreateDroughtRestriction: IDroughtRestriction;
const OPNAME = 'TYieldModelManager.DoCreateDroughtRestriction';
begin
  Result := nil;
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then Exit;
    Result := YieldModelDataObject.CastNetworkFeaturesData.
              CastCurtailmentAndDrought.CreateDroughtRestriction;
    if (Result <> nil) then
    begin
      CreateTabSheetTreeViewElement('DroughtRestrictions',
                                 'ParametersHeading,DroughtRestrictions',
                                 (Result as IDroughtRestriction).Identifier, tvnidDroughtRestriction,
                                 (Result as IDroughtRestriction).DroughtRestrictionName,
                                  'DroughtRestrictions',
                                  'DroughtRestrictions',True,tvsnAll);
      StudyDataHasChanged(sdccAdd,'DroughtRestrictionName',  '0' , IntToStr((Result as IDroughtRestriction).Identifier));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoDeleteDroughtRestriction(AIdentifier: Integer): WordBool;
const OPNAME = 'TYieldModelManager.DoDeleteDroughtRestriction';
var
  lNode       : TTreeNode;
  lDataObject : TViewDataTreeNodeData;
begin
  Result  := FALSE;
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then Exit;
    if (AIdentifier > 0) then
    begin
      Result := YieldModelDataObject.NetworkFeaturesData.CurtailmentAndDrought.RemoveChannelCurtailment(AIdentifier);
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          AIdentifier  := LDataObject.ViewDataNode.Weighting;
          Result := YieldModelDataObject.CastNetworkFeaturesData.CastCurtailmentAndDrought.RemoveDroughtRestriction(AIdentifier);
          if (Result) then
            DeleteTabSheetTreeNode(lNode);
          SetSystemMenuState;
        end;
      end;
    end;
    if Result then
      StudyDataHasChanged(sdccDelete, 'DroughtRestrictionName', '0',IntToStr(AIdentifier));

  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TYieldModelManager.DoDeleteMasterControlFeature(AChannelNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.DoDeleteMasterControlFeature';
var
  LNode          : TTreeNode;
  LDataObject    : TViewDataTreeNodeData;
  lFeatureID     : integer;
  lChannel       : TGeneralFlowChannel;
begin
  Result := FALSE;
  try
    if (AChannelNumber > 0) then
    begin
      lChannel     := YieldModelDataObject.CastNetworkElementData.
                        CastChannelList.CastChannelByChannelNumber[AChannelNumber];
      if (lChannel <> nil) then
      begin
        lFeatureID := lChannel.MasterControlFeatureID;
        Result     := YieldModelDataObject.CastNetworkFeaturesData.
                        MasterControlFeatureList.RemoveMasterControlFeatureWithID(lFeatureID);
        Result     := Result and lChannel.DeleteMasterControlFeature;
        YieldModelDataObject.NetworkFeaturesData.WaterDemandConfiguration.DeleteWaterUseOutputProportion(AChannelNumber);
        DoDeleteChannelTariff(lChannel.ChannelNumber);

        if Result then
          lChannel.ChannelType := 12;
      end;
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject    := TViewDataTreeNodeData((LNode.Data));
          AChannelNumber := LDataObject.ViewDataNode.Weighting;
          lChannel       := YieldModelDataObject.CastNetworkElementData.
                              CastChannelList.CastChannelByChannelNumber[AChannelNumber];
          if (lChannel <> nil) then
          begin
            lFeatureID := lChannel.MasterControlFeatureID;
            Result     := YieldModelDataObject.CastNetworkFeaturesData.
                            MasterControlFeatureList.RemoveMasterControlFeatureWithID(lFeatureID);
            Result     := Result and lChannel.DeleteMasterControlFeature;
            YieldModelDataObject.NetworkFeaturesData.WaterDemandConfiguration.DeleteWaterUseOutputProportion(AChannelNumber);
            DoDeleteChannelTariff(lChannel.ChannelNumber);
            if Result then
              lChannel.ChannelType := 12;
          end;
          if Result then
          begin
            DeleteTabSheetTreeNode(lNode);
            CreateTabSheetTreeViewElement('ChannelDetails12',
                                       'ChannelHeading,ChannelDetails12',
                                       lChannel.ChannelNumber,tvnidChannelDetails12,
                                       lChannel.ChannelName,
                                       'GENERALCHANNEL',
                                       'CHANNEL',True,tvsnAll);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.AddTreeViewsSubNodes: boolean;
const OPNAME = 'TYieldModelManager.AddTreeViewsSubNodes';
var
  LIndex           : integer;
  LMine            : IMine;
  LWetland         : IWetland;
  LIrrigationBlock : IIrrigationBlock;
  LGroundWater     : IGroundWater;
  LPowerPlant      : IPowerPlant;
  LYMDemandCentre  : IYMDemandCentre;
begin
  Result := FALSE;
  try
    for LIndex := 0 to YieldModelDataObject.NetworkFeaturesData.MineList.MineCount-1 do
    begin
      LMine := YieldModelDataObject.NetworkFeaturesData.MineList.MineByIndex[LIndex];
      if(LMine <> nil) then
      begin
        AddMineNodes(LMine);
      end;
    end;

   for LIndex := 0 to YieldModelDataObject.NetworkFeaturesData.WetlandList.WetLandCount-1 do
    begin
      LWetland := YieldModelDataObject.NetworkFeaturesData.WetlandList.WetlandByIndex[LIndex];
      if(LWetland <> nil) then
      begin
        AddOutputWetlandChannels(LWetland);
      end;
    end;

    for LIndex := 0 to YieldModelDataObject.NetworkFeaturesData.IrrigationBlockList.IrrigationBlockCount-1 do
    begin
      LIrrigationBlock := YieldModelDataObject.NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByIndex[LIndex];
      if(LIrrigationBlock <> nil) then
      begin
        AddOutputIrrigationBlockChannels(LIrrigationBlock);
      end;
    end;

    for LIndex := 0 to YieldModelDataObject.NetworkFeaturesData.GroundWaterList.GroundWaterCount-1 do
    begin
      LGroundWater := YieldModelDataObject.NetworkFeaturesData.GroundWaterList.GroundWaterByIndex[LIndex];
      if(LGroundWater <> nil) then
      begin
        AddGroundWater(LGroundWater);
      end;
    end;

    for LIndex := 0 to YieldModelDataObject.NetworkFeaturesData.PowerPlantList.PowerPlantCount-1 do
    begin
      LPowerPlant := YieldModelDataObject.NetworkFeaturesData.PowerPlantList.PowerPlantByIndex[LIndex];
      if(LPowerPlant <> nil) then
      begin
        AddOutputPowerPlantChannels(LPowerPlant);
      end;
    end;

    for LIndex := 0 to YieldModelDataObject.NetworkFeaturesData.YMDemandCentreList.YMDemandCentreCount-1 do
    begin
      LYMDemandCentre := YieldModelDataObject.NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByIndex[LIndex];
      if(LYMDemandCentre <> nil) then
      begin
        AddOutputDemandCentreChannels(LYMDemandCentre);
      end;
    end;



    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.AddMineNodes(AMine: IMine): boolean;
const OPNAME = 'TYieldModelManager.AddMineNodes';
var
  LReservoirNumber,
  LCount : integer;
  LReservoir   : IReservoirData;
  LUnderground:IUnderground;
  LReservoirName,
  LParentIDsCommatext : string;
begin
  Result := FALSE;
  try
    if(AMine = nil) then
      Exit;

    if((FDataTabSheetManager <> nil) and (FDataTabSheetManager.GridEditorManager <> nil)) or
      ((FOutputReviewManager <> nil) and (FOutputReviewManager.TabSheet <> nil)) then
    begin
      LReservoir := AMine.PolutionControlDam;
      if(LReservoir <> nil) then
      begin
        //Add  Mine PCD
        LParentIDsCommatext := 'FeaturesHeading,Mine,Mine';
        LReservoirNumber    := LReservoir.ReservoirConfigurationData.ReservoirIdentifier;
        LReservoirName      := LReservoir.ReservoirConfigurationData.ReservoirName;
        CreateTabSheetTreeViewElement('PCDDam',
                                    LParentIDsCommatext,
                                    LReservoirNumber,
                                    AMine.NodeNumber,
                                    LReservoirName,
                                   'RESERVOIR',
                                   'PCDDAM',
                                    False,tvsnAll);

        if(AMine.RiverChannel <> nil) then
        begin
          LParentIDsCommatext := 'FeaturesHeading,Mine,Mine,PCDDam';
          LReservoirNumber    := LReservoir.ReservoirConfigurationData.ReservoirIdentifier;
          CreateTabSheetTreeViewElement('ChannelDetails12',
                                      LParentIDsCommatext,
                                      AMine.RiverChannel.ChannelNumber,
                                      LReservoirNumber,
                                      AMine.RiverChannel.ChannelName,
                                     'CHANNEL',
                                     'CHANNEL',
                                      False,tvsnOutput);
        end;

        if(AMine.PCDChannel <> nil) then
        begin
          LParentIDsCommatext := 'FeaturesHeading,Mine,Mine,PCDDam';
          LReservoirNumber    := LReservoir.ReservoirConfigurationData.ReservoirIdentifier;
          CreateTabSheetTreeViewElement('ChannelDetails12',
                                      LParentIDsCommatext,
                                      AMine.PCDChannel.ChannelNumber,
                                      LReservoirNumber,
                                      AMine.PCDChannel.ChannelName,
                                     'CHANNEL',
                                     'CHANNEL',
                                      False,tvsnOutput);
        end;

        for LCount := 0 to AMine.UndergroundCount-1 do
        begin
          LUnderground := AMine.UnderGroundByIndex[LCount];
          if(LUnderground <> nil) then
          begin
            LReservoir := LUnderground.UndergroundDam;
            if(LReservoir <> nil) then
            begin
              //Add  Mine Undeground dam
              LParentIDsCommatext := 'FeaturesHeading,Mine,Mine';
              LReservoirNumber    := LReservoir.ReservoirConfigurationData.ReservoirIdentifier;
              LReservoirName      := LReservoir.ReservoirConfigurationData.ReservoirName;
              CreateTabSheetTreeViewElement('UndegroundDam',
                                          LParentIDsCommatext,
                                          LReservoirNumber,
                                          AMine.NodeNumber,
                                          LReservoirName,
                                         'RESERVOIR',
                                         'UNDEGROUNDDAM',
                                          False,tvsnAll);

              if(LUnderground.ChannelToUnderGroundDam <> nil) then
              begin
                LParentIDsCommatext := 'FeaturesHeading,Mine,Mine,UndegroundDam';
                LReservoirNumber    := LReservoir.ReservoirConfigurationData.ReservoirIdentifier;
                CreateTabSheetTreeViewElement('ChannelDetails12',
                                            LParentIDsCommatext,
                                            LUnderground.ChannelToUnderGroundDam.ChannelNumber,
                                            LReservoirNumber,
                                            LUnderground.ChannelToUnderGroundDam.ChannelName,
                                           'CHANNEL',
                                           'CHANNEL',
                                            False,tvsnOutput);
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.AddOutputWetlandChannels(AWetland: IWetland): boolean;
const OPNAME = 'TYieldModelManager.AddOutputWetlandChannels';
var
  LChannelNumber      : integer;
  LInflowChannel      : IGeneralFlowChannel;
  LOutFlowChannel     : IGeneralFlowChannel;
  LChannelName        : string;
  LParentIDsCommatext : string;
begin
  Result := FALSE;
  try
    if(AWetland = nil) then
      Exit;

    if((FDataTabSheetManager <> nil) and (FDataTabSheetManager.GridEditorManager <> nil)) or
      ((FOutputReviewManager <> nil) and (FOutputReviewManager.TabSheet <> nil)) then
    begin
      LInflowChannel := AWetland.InflowChannel;
      if(LInflowChannel <> nil) then
      begin
        //Add Inflow Channel
        LParentIDsCommatext := 'ReviewFeaturesHeading,Wetland,Wetland';
        LChannelNumber      := LInflowChannel.ChannelNumber;
        LChannelName        := LInflowChannel.ChannelName;
        CreateTabSheetTreeViewElement('ChannelDetails8',
                                    LParentIDsCommatext,
                                    LChannelNumber,
                                    AWetland.NodeNumber,
                                    LChannelName,
                                   'CHANNEL',
                                   'CHANNEL',
                                    False,tvsnAll);
      end;

      LOutFlowChannel := AWetland.OutflowChannel;
      if(LOutFlowChannel <> nil) then
      begin
        //Add Outflow Channel
        LParentIDsCommatext := 'ReviewFeaturesHeading,Wetland,Wetland';
        LChannelNumber      := LOutFlowChannel.ChannelNumber;
        LChannelName        := LOutFlowChannel.ChannelName;
        CreateTabSheetTreeViewElement('ChannelDetails8',
                                    LParentIDsCommatext,
                                    LChannelNumber,
                                    AWetland.NodeNumber,
                                    LChannelName,
                                   'CHANNEL',
                                   'CHANNEL',
                                    False,tvsnAll);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.AddOutputIrrigationBlockChannels(AIrrigationBlock: IIrrigationBlock): boolean;
const OPNAME = 'TYieldModelManager.AddOutputIrrigationBlockChannels';
var
  LChannelNumber      : integer;
  LDiversionChannel   : IGeneralFlowChannel;
  LReturnFlowChannel  : IGeneralFlowChannel;
  LChannelName        : string;
  LParentIDsCommatext : string;
begin
  Result := FALSE;
  try
    if(AIrrigationBlock = nil) then
      Exit;

    if((FDataTabSheetManager <> nil) and (FDataTabSheetManager.GridEditorManager <> nil)) or
      ((FOutputReviewManager <> nil) and (FOutputReviewManager.TabSheet <> nil)) then
    begin
      LDiversionChannel := AIrrigationBlock.DiversionChannel;
      if(LDiversionChannel <> nil) then
      begin
        //Add Diversion Channel
        LParentIDsCommatext := 'ReviewFeaturesHeading,IrrigationBlock,IrrigationBlock';
        LChannelNumber      := LDiversionChannel.ChannelNumber;
        LChannelName        := LDiversionChannel.ChannelName;
        CreateTabSheetTreeViewElement('IrrigationBlock',
                                    LParentIDsCommatext,
                                    LChannelNumber,
                                    AIrrigationBlock.BlockNodeNumber,
                                    LChannelName,
                                   'IRRIGATIONBLOCK',
                                   'IRRIGATIONBLOCK',
                                    False,tvsnAll);
      end;

      LReturnFlowChannel := AIrrigationBlock.ReturnFlowChannel;
      if(LReturnFlowChannel <> nil) then
      begin
        //Add Return Flow Channel
        LParentIDsCommatext := 'ReviewFeaturesHeading,IrrigationBlock,IrrigationBlock';
        LChannelNumber      := LReturnFlowChannel.ChannelNumber;
        LChannelName        := LReturnFlowChannel.ChannelName;
        CreateTabSheetTreeViewElement('IrrigationBlock',
                                    LParentIDsCommatext,
                                    LChannelNumber,
                                    AIrrigationBlock.BlockNodeNumber,
                                    LChannelName,
                                   'IRRIGATIONBLOCK',
                                   'IRRIGATIONBLOCK',
                                    False,tvsnAll);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.AddOutputDemandCentreChannels(LDemandCentre: IYMDemandCentre): boolean;
const OPNAME = 'TYieldModelManager.AddOutputDemandCentreChannels';
var
  LIndex              : integer;
  LChannelNumber      : integer;
  LChannel            : IGeneralFlowChannel;
  LChannelName        : string;
  LParentIDsCommatext : string;
begin
  Result := FALSE;
  try
    if(LDemandCentre = nil) then
      Exit;

    if((FDataTabSheetManager <> nil) and (FDataTabSheetManager.GridEditorManager <> nil)) or
      ((FOutputReviewManager <> nil) and (FOutputReviewManager.TabSheet <> nil)) then
    begin
      LChannel := LDemandCentre.ReclaimationChannel;
      if(LChannel <> nil) then
      begin
        //Add Reclaimation Channel
        LParentIDsCommatext := 'ReviewFeaturesHeading,YMDemandCentre,YMDemandCentre';
        LChannelNumber      := LChannel.ChannelNumber;
        LChannelName        := LChannel.ChannelName;
        CreateTabSheetTreeViewElement('YMDemandCentre',
                                    LParentIDsCommatext,
                                    LChannelNumber,
                                    LDemandCentre.NodeNumber,
                                    LChannelName,
                                   'YMDEMANDCENTRE',
                                   'YMDEMANDCENTRE',
                                    False,tvsnOutput);
      end;

      for LIndex := 0 to LDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureCount-1 do
      begin
        LChannel := LDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureByIndex[LIndex].Channel;
        if(LChannel <> nil) then
        begin
          //Add ReturnFlow Channel
          LParentIDsCommatext := 'ReviewFeaturesHeading,YMDemandCentre,YMDemandCentre';
          LChannelNumber      := LChannel.ChannelNumber;
          LChannelName        := LChannel.ChannelName;
          CreateTabSheetTreeViewElement('YMDemandCentre',
                                      LParentIDsCommatext,
                                      LChannelNumber,
                                      LDemandCentre.NodeNumber,
                                      LChannelName,
                                     'YMDEMANDCENTRE',
                                     'YMDEMANDCENTRE',
                                      False,tvsnOutput);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.AddOutputPowerPlantChannels(APowerPlant: IPowerPlant): boolean;
const OPNAME = 'TYieldModelManager.AddOutputPowerPlantChannels';
var
  LChannelNumber      : integer;
  LChannel            : IGeneralFlowChannel;
  LChannelName        : string;
  LParentIDsCommatext : string;
begin
  Result := FALSE;
  try
    if(APowerPlant = nil) then
      Exit;

    if((FDataTabSheetManager <> nil) and (FDataTabSheetManager.GridEditorManager <> nil)) or
      ((FOutputReviewManager <> nil) and (FOutputReviewManager.TabSheet <> nil)) then
    begin
      LChannel := APowerPlant.PowerChannel;
      if(LChannel <> nil) then
      begin
        //Add Power Channel
        LParentIDsCommatext := 'ReviewFeaturesHeading,PowerPlants,PowerPlants';
        LChannelNumber      := LChannel.ChannelNumber;
        LChannelName        := LChannel.ChannelName;
        CreateTabSheetTreeViewElement('PowerPlants',
                                    LParentIDsCommatext,
                                    LChannelNumber,
                                    APowerPlant.FeatureID,
                                    LChannelName,
                                   'POWERPLANTS',
                                   'POWERPLANTS',
                                    False,tvsnOutput);
      end;

      LChannel := APowerPlant.SpillChannel;
      if(LChannel <> nil) then
      begin
        //Add Power Channel
        LParentIDsCommatext := 'ReviewFeaturesHeading,PowerPlants,PowerPlants';
        LChannelNumber      := LChannel.ChannelNumber;
        LChannelName        := LChannel.ChannelName;
        CreateTabSheetTreeViewElement('PowerPlants',
                                    LParentIDsCommatext,
                                    LChannelNumber,
                                    APowerPlant.FeatureID,
                                    LChannelName,
                                   'POWERPLANTS',
                                   'POWERPLANTS',
                                    False,tvsnOutput);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.GetReviewParentIDs(AParentIDsCommatext: string): string;
const OPNAME = 'TYieldModelManager.GetReviewParentIDs';
var
  LIndex : integer;
  LIDsContainer: TStringList;
begin
  Result := AParentIDsCommatext;
  try
    LIDsContainer := TStringList.Create;
    try
      LIDsContainer.CommaText := AParentIDsCommatext;
      for LIndex := 0 to LIDsContainer.Count-1 do
      begin
       if(Pos('Review',LIDsContainer[LIndex]) <> 1) then
         LIDsContainer[LIndex] := 'Review' + LIDsContainer[LIndex];
      end;
      Result := LIDsContainer.CommaText;
    finally
      LIDsContainer.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.CreateTabSheetTreeViewElement(AViewID     : string;
                                       AParentIDsCommatext   : string;
                                       AWeighting,
                                       AParentWeighting  : integer;
                                       ACaption    : string;
                                       ABitmapName : string;
                                       ADataType   : string;
                                       ASelect     : boolean;
                                       ATreeViewSheetName:TTreeViewSheetName) : boolean;
const OPNAME = 'TYieldModelManager.CreateTabSheetTreeViewElement';
var
  LTreeViewTabSheet : TDynamicTreeViewTabSheet;
begin
  Result := FALSE;
  try
    if (FAppModules.MainForm = nil)  then
      Result := True
    else
    begin
      if(ATreeViewSheetName in [tvsnAll,tvsnData]) then
      begin
        if(FDataTabSheetManager <> nil) and (FDataTabSheetManager.GridEditorManager <> nil) then
        begin
          LTreeViewTabSheet := TDynamicTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet);
          LTreeViewTabSheet.AddAndSelectElement(AViewID,AParentIDsCommatext,AWeighting,AParentWeighting,
                                                ACaption,ABitmapName,ADataType,ASelect);
        end;
      end;

      if(ATreeViewSheetName in [tvsnAll,tvsnOutput]) then
      begin
        if(FOutputReviewManager <> nil) and (FOutputReviewManager.TabSheet <> nil) then
        begin
          LTreeViewTabSheet := TDynamicTreeViewTabSheet(FOutputReviewManager.TabSheet);
          AParentIDsCommatext := GetReviewParentIDs(AParentIDsCommatext);
          LTreeViewTabSheet.AddAndSelectElement('Review'+AViewID,AParentIDsCommatext,AWeighting,AParentWeighting,
                                                ACaption,ABitmapName,ADataType,False);
        end;
      end;

      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DeleteTabSheetTreeViewElement(AViewID, AParentIDsCommatext : string; AWeighting,AParentWeighting  : integer) : boolean;
const OPNAME = 'TYieldModelManager.DeleteTabSheetTreeViewElement';
var
  LTreeViewTabSheet : TDynamicTreeViewTabSheet;
begin
  Result := FALSE;
  try
    if (FAppModules.MainForm = nil)  then
      Result := True
    else
    begin
      if(FDataTabSheetManager <> nil) and (FDataTabSheetManager.GridEditorManager <> nil) then
      begin
        LTreeViewTabSheet := TDynamicTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet);
        LTreeViewTabSheet.DeleteTreeNode(AViewID,AParentIDsCommatext,AWeighting,AParentWeighting);
      end;

      if(FOutputReviewManager <> nil) and (FOutputReviewManager.TabSheet <> nil) then
      begin
        LTreeViewTabSheet := TDynamicTreeViewTabSheet(FOutputReviewManager.TabSheet);
        AParentIDsCommatext := GetReviewParentIDs(AParentIDsCommatext);
        LTreeViewTabSheet.DeleteTreeNode('Review'+AViewID,AParentIDsCommatext,AWeighting,AParentWeighting);
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DeleteTabSheetTreeNode (ATreeNode : TTreeNode) : boolean;
const OPNAME = 'TYieldModelManager.DeleteTabSheetTreeNode';
var
  LViewID,
  LParentIDsCommatext : string;
  LParentIDs          : TStringList;
  LWeighting,
  LParentWeighting    : integer;
  LViewDataNode : TViewDataNode;
  LParentNode   : TTreeNode;
  LTreeNodeData : TViewDataTreeNodeData;
begin
  Result := FALSE;
  try
    if(ATreeNode = nil) then
      Exit;

    if (FAppModules.MainForm = nil)  then
      Result := True
    else
    begin
      if Assigned(ATreeNode.Data) then
      begin
        LTreeNodeData := TViewDataTreeNodeData(ATreeNode.Data);
        LViewDataNode := LTreeNodeData.ViewDataNode;
        if(LViewDataNode <> nil) then
        begin
          LViewID             := LViewDataNode.ViewID;
          LWeighting          := LViewDataNode.Weighting;
          LParentIDs          := TStringList.Create;
          LParentWeighting    := NullInteger;
          try
            LParentNode := ATreeNode.Parent;
            while (LParentNode <> nil) do
            begin
              LTreeNodeData := TViewDataTreeNodeData(LParentNode.Data);
              LViewDataNode := LTreeNodeData.ViewDataNode;
              if(LViewDataNode <> nil) then
              begin
                LParentIDs.Insert(0,LViewDataNode.ViewID);
                if(LParentWeighting = NullInteger) then
                  LParentWeighting := LViewDataNode.Weighting;
              end;
              LParentNode := LParentNode.Parent;
            end;
            LParentIDsCommatext := LParentIDs.CommaText;
          finally
            LParentIDs.Free;
          end;
          Result := DeleteTabSheetTreeViewElement(LViewID,LParentIDsCommatext,LWeighting,LParentWeighting);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoInvokeWizard : WordBool;
const OPNAME = 'TYieldModelManager.DoInvokeWizard';
var
  lNode       : TTreeNode;
  lDataObject : TViewDataTreeNodeData;
  lNumber     : integer;
  lListObject : TStringList;
begin
  Result := FALSE;
  try
    if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
    begin
      lNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
      if (Assigned(lNode) AND Assigned(lNode.Data)) then
      begin
        try
          lListObject := TStringList.Create;
          lDataObject := TViewDataTreeNodeData(lNode.Data);
          lNumber     := lDataObject.ViewDataNode.Weighting;
          lListObject.Add(IntToStr(lNumber));
          if (lDataObject.ViewDataNode.DataType = 'RESERVOIR') then
            DoWizardReservoir(lListObject)
          else
          if (LDataObject.ViewDataNode.DataType = 'NODEWITHINFLOW') then
            DoWizardNodeWithInflow(lListObject)
          else
          if (LDataObject.ViewDataNode.DataType = 'CHANNEL') then
            DoWizardChannel(lListObject)
          else
          if (LDataObject.ViewDataNode.DataType = 'IRRIGATIONAREA') then
            DoWizardIrrigationArea(lListObject)
          else
          if (LDataObject.ViewDataNode.DataType = 'POWERPLANT') then
            DoWizardPowerPlant(lListObject);
        finally
         FreeAndNil(lListObject);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoWizardReservoir(AData : TObject) : boolean;
const OPNAME = 'TYieldModelManager.DoWizardReservoir';
var
  lListObject      : TStringList;
  lReservoir       : IReservoirData;
  lReservoirNumber : integer;
  lNode            : TTreeNode;
begin
  Result := FALSE;
  try
    if (Assigned(AData)) then
    begin
      lListObject := TStringList(AData);
      lReservoirNumber := StrToInt(lListObject.Strings[0]);
      lReservoir := YieldModelData.NetworkElementData.ReservoirList.ReservoirByIdentifier[lReservoirNumber];
      if (lReservoir <> nil) then
      begin
        try
          FWizardForm := TWizardForm.CreateWithoutDFM(nil, FAppModules);
          FWizardForm.SetFormCaption('Reservoir Wizard: ' + lReservoir.ReservoirConfigurationData.ReservoirName);
          FWizardForm.LanguageHasChanged;
          TYieldModelWizard(FWizardForm.Wizard).LoadReservoirWizard(FWizardForm.ValidatorParent, lReservoirNumber);
          FWizardForm.PopulateWizard;
          FWizardForm.ShowModal;
          Result := TRUE;
        finally
          FreeAndNil(FWizardForm);
          if (FAppModules.MainForm <> nil)  then
          begin
            LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
            TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).ReselectTreeNode(lNode);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoWizardNewReservoir: WordBool;
const OPNAME = 'TYieldModelManager.DoWizardNewReservoir';
var
  lReservoir       : IReservoirData;
  lReservoirNumber : integer;
  lListObject      : TStringList;
begin
  Result := FALSE;
  try
    lReservoir :=  YieldModelDataObject.CastNetworkElementData.CastReservoirList.CreateReservoir(ntReservoir);
    if (lReservoir <> nil) then
    begin
      lReservoirNumber := lReservoir.ReservoirConfigurationData.ReservoirIdentifier;
      try
        lListObject := TStringList.Create;
        lListObject.Add(IntToStr(lReservoirNumber));
        Result := DoWizardReservoir(lListObject);
      finally
        FreeAndNil(lListObject);
        CreateTabSheetTreeViewElement('AReservoir',
                                   'AReservoir',
                                   lReservoirNumber,tvnidAReservoir,
                                   lReservoir.ReservoirConfigurationData.ReservoirName,
                                   'RESERVOIR',
                                   'RESERVOIR',True,tvsnAll);
        StudyDataHasChanged(sdccAdd, 'ReservoirName', '0', IntToStr(lReservoirNumber));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoWizardNodeWithInflow(AData : TObject) : boolean;
const OPNAME = 'TYieldModelManager.DoWizardNodeWithInflow';
var
  lListObject : TStringList;
  lNode       : IReservoirData;
  lNodeNumber : integer;
  lTreeNode   : TTreeNode;
begin
  Result := FALSE;
  try
    if (Assigned(AData)) then
    begin
      lListObject := TStringList(AData);
      lNodeNumber := StrToInt(lListObject.Strings[0]);
      lNode := YieldModelData.NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[lNodeNumber];
      if (Assigned(lNode)) then
      begin
        try
          FWizardForm := TWizardForm.CreateWithoutDFM(nil, FAppModules);
          FWizardForm.SetFormCaption('Node With Inflow Wizard: ' + lNode.ReservoirConfigurationData.ReservoirName);
          FWizardForm.LanguageHasChanged;
          TYieldModelWizard(FWizardForm.Wizard).LoadNodeWizard(FWizardForm.ValidatorParent, lNodeNumber);
          FWizardForm.PopulateWizard;
          FWizardForm.ShowModal;
          Result := TRUE;
        finally
          FreeAndNil(FWizardForm);
          if (FAppModules.MainForm <> nil)  then
          begin
            lTreeNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
            TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).ReselectTreeNode(lTreeNode);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoWizardNewNodeWithInflow: WordBool;
const OPNAME = 'TYieldModelManager.DoWizardNewNodeWithInflow';
var
  lNode       : IReservoirData;
  lNodeNumber : integer;
  lListObject : TStringList;
begin
  Result := FALSE;
  try
    lNode :=  YieldModelDataObject.CastNetworkElementData.CastReservoirList.CreateNodeWithInflow(ntNodeWithInflow);
    if (lNode <> nil) then
    begin
      lNodeNumber := lNode.ReservoirConfigurationData.ReservoirIdentifier;
      try
        lListObject := TStringList.Create;
        lListObject.Add(IntToStr(lNodeNumber));
        Result := DoWizardNodeWithInflow(lListObject);
      finally
        FreeAndNil(lListObject);
        CreateTabSheetTreeViewElement('NodesWithInflow',
                                   'NodesHeading,NodesWithInflow',
                                   lNodeNumber,tvnidNodesWithInflow,
                                   lNode.ReservoirConfigurationData.ReservoirName,
                                   '',
                                   'NODEWITHINFLOW',True,tvsnAll);
        StudyDataHasChanged(sdccAdd, 'ReservoirName', '0', IntToStr(lNodeNumber));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoWizardChannel(AData : TObject) : boolean;
const OPNAME = 'TYieldModelManager.DoWizardChannel';
var
  lListObject    : TStringList;
  lChannel       : IGeneralFlowChannel;
  lChannelNumber : integer;
  lNode          : TTreeNode;
begin
  Result := FALSE;
  try
    if (Assigned(AData)) then
    begin
      lListObject := TStringList(AData);
      lChannelNumber := StrToInt(lListObject.Strings[0]);
      lChannel := YieldModelData.NetworkElementData.ChannelList.ChannelByChannelNumber[lChannelNumber];
      if (Assigned(lChannel)) then
      begin
        try
          FWizardForm := TWizardForm.CreateWithoutDFM(nil, FAppModules);
          FWizardForm.SetFormCaption('Channel Wizard: ' + lChannel.ChannelName);
          FWizardForm.LanguageHasChanged;
          TYieldModelWizard(FWizardForm.Wizard).LoadChannelWizard(FWizardForm.ValidatorParent, lChannel.ChannelNumber);
          FWizardForm.PopulateWizard;
          FWizardForm.ShowModal;
          Result := TRUE;
        finally
          FreeAndNil(FWizardForm);
          if (FAppModules.MainForm <> nil)  then
          begin
            LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
            TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).ReselectTreeNode(LNode);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoWizardNewChannel(const AUpDownNodeNumbers: WideString): WordBool;
const OPNAME = 'TYieldModelManager.DoWizardNewChannel';
var
  lChannel       : IGeneralFlowChannel;
  lChannelNumber : integer;
  lListObject    : TStringList;
begin
  Result := False;
  try
    lChannel := YieldModelDataObject.
                NetworkElementData.ChannelList.CreateChannel;
    try
      if (lChannel <> nil) then
      begin
        lChannelNumber := lChannel.ChannelNumber;
        lListObject := TStringList.Create;
        try
          if(Trim(AUpDownNodeNumbers) <> '') then
            lListObject.CommaText := AUpDownNodeNumbers;
          if(lListObject.Count > 0) then
            lChannel.UpStreamNodeNumber   := StrToInt(lListObject.Strings[0]);
          if(lListObject.Count > 1) then
           lChannel.DownStreamNodeNumber := StrToInt(lListObject.Strings[1]);

          lListObject.Insert(0, IntToStr(lChannelNumber));
          Result := DoWizardChannel(lListObject);
        finally
          FreeAndNil(lListObject);
          CreateTabSheetTreeViewElement('ChannelDetails12',
                                     'ChannelHeading,ChannelDetails12',
                                     lChannelNumber,tvnidChannelDetails12,
                                     lChannel.ChannelName,
                                     'GENERALCHANNEL',
                                     'CHANNEL',True,tvsnAll);
          StudyDataHasChanged(sdccAdd, 'ChannelName', '0', IntToStr(lChannelNumber));
        end;
      end;
    finally
      lChannel := nil
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoWizardIrrigationArea(AData : TObject) : boolean;
const OPNAME = 'TYieldModelManager.DoWizardIrrigationArea';
var
  lListObject : TStringList;
  lFeature    : IIrrigationArea;
  lFeatureID  : integer;
  lNode       : TTreeNode;
begin
  Result := FALSE;
  try
    if (Assigned(AData)) then
    begin
      lListObject := TStringList(AData);
      lFeatureID  := StrToInt(lListObject.Strings[0]);
      lFeature    := YieldModelData.NetworkFeaturesData.IrrigationAreaList.IrrigationAreaByNodeNumber[lFeatureID];
      if (Assigned(lFeature)) then
      begin
        try
          FWizardForm := TWizardForm.CreateWithoutDFM(nil, FAppModules);
          FWizardForm.SetFormCaption('Irrigation Area Wizard: ' + lFeature.FeatureName);
          FWizardForm.LanguageHasChanged;
          TYieldModelWizard(FWizardForm.Wizard).LoadIrrigationAreaWizard(FWizardForm.ValidatorParent, lFeature.FeatureID);
          FWizardForm.PopulateWizard;
          FWizardForm.ShowModal;
          Result := TRUE;
        finally
          FreeAndNil(FWizardForm);
          if (FAppModules.MainForm <> nil)  then
          begin
            LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
            TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).ReselectTreeNode(LNode);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoWizardPowerPlant(AData : TObject) : boolean;
const OPNAME = 'TYieldModelManager.DoWizardPowerPlant';
var
  lListObject : TStringList;
  lFeature    : IPowerPlant;
  lFeatureID  : integer;
  lNode       : TTreeNode;
begin
  Result := FALSE;
  try
    if (Assigned(AData)) then
    begin
      lListObject := TStringList(AData);
      lFeatureID  := StrToInt(lListObject.Strings[0]);
      lFeature    := YieldModelData.NetworkFeaturesData.PowerPlantList.PowerPlantByID[lFeatureID];
      if (Assigned(lFeature)) then
      begin
        try
          FWizardForm := TWizardForm.CreateWithoutDFM(nil, FAppModules);
          FWizardForm.SetFormCaption('Power Plant Wizard: ' + lFeature.FeatureName);
          FWizardForm.LanguageHasChanged;
          TYieldModelWizard(FWizardForm.Wizard).LoadPowerPlantWizard(FWizardForm.ValidatorParent, lFeature.FeatureID);
          FWizardForm.PopulateWizard;
          FWizardForm.ShowModal;
          Result := TRUE;
        finally
          FreeAndNil(FWizardForm);
          if (FAppModules.MainForm <> nil)  then
          begin
            LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
            TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).ReselectTreeNode(LNode);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoWizardRunYieldHistoric : WordBool;
const OPNAME = 'TYieldModelManager.DoWizardRunYieldHistoric';
begin
  Result := False;
  try
    try
      FWizardForm := TWizardForm.CreateWithoutDFM(nil, FAppModules);
      FWizardForm.SetFormCaption('Run Yield Model Historic Wizard');
      FWizardForm.LanguageHasChanged;
      TYieldModelWizard(FWizardForm.Wizard).LoadYieldRunHistoricWizard(FWizardForm.ValidatorParent);
      FWizardForm.PopulateWizard;
      FWizardForm.ShowModal;
      Result := TRUE;
    finally
      FreeAndNil(FWizardForm);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoWizardRunYieldStochastic : WordBool;
const OPNAME = 'TYieldModelManager.DoWizardRunYieldStochastic';
begin
  Result := False;
  try
    try
      FWizardForm := TWizardForm.CreateWithoutDFM(nil, FAppModules);
      FWizardForm.SetFormCaption('Run Yield Model Stochastic Wizard');
      FWizardForm.LanguageHasChanged;
      TYieldModelWizard(FWizardForm.Wizard).LoadYieldRunStochasticWizard(FWizardForm.ValidatorParent);
      FWizardForm.PopulateWizard;
      FWizardForm.ShowModal;
      Result := TRUE;
    finally
      FreeAndNil(FWizardForm);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoWizardRunYieldYRC : WordBool;
const OPNAME = 'TYieldModelManager.DoWizardRunYieldYRC';
begin
  Result := False;
  try
    try
      FWizardForm := TWizardForm.CreateWithoutDFM(nil, FAppModules);
      FWizardForm.SetFormCaption('Run Yield Model YRC Wizard');
      FWizardForm.LanguageHasChanged;
      TYieldModelWizard(FWizardForm.Wizard).LoadYieldRunYRCWizard(FWizardForm.ValidatorParent);
      FWizardForm.PopulateWizard;
      FWizardForm.ShowModal;
      Result := TRUE;
    finally
      FreeAndNil(FWizardForm);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.ModelName: string;
const OPNAME = 'TYieldModelManager.ModelName';
begin
  Result := '';
  try
    Result := CYield;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.Get_YieldModelData: IYieldModelData;
const OPNAME = 'TYieldModelManager.Get_YieldModelData';
begin
  Result := nil;
  try
    Result := YieldModelDataObject;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TYieldModelManager.Validate';
begin
  Result := False;
  try
    Result := YieldModelData.Validate(AErrors,AContext);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.ModelData: TInterfacedObject;
const OPNAME = 'TYieldModelManager.ModelData';
begin
  Result := nil;
  try
    Result := YieldModelDataObject;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.GetYieldModelDataObject: TYieldModelDataObject;
const OPNAME = 'TYieldModelManager.GetYieldModelDataObject';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FModelData);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCreateWaterDemandFeature (AChannelNumber: Integer): IWaterDemandFeature;
const OPNAME = 'TYieldModelManager.DoCreateWaterDemandFeature';
var
  LNode          : TTreeNode;
  LDataObject    : TViewDataTreeNodeData;
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  lFeature       : IWaterDemandFeature;
begin
  Result := nil;
  try
    lChannel := nil;
    lNode    := nil;
    if (AChannelNumber > 0) then
    begin
      lChannelList := YieldModelDataObject.
                        NetworkElementData.ChannelList;
      lChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          AChannelNumber := LDataObject.ViewDataNode.Weighting;
          lChannelList := YieldModelDataObject.
                            NetworkElementData.ChannelList;
          lChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
        end;
      end;
    end;
    if (lChannel <> nil) then
    begin
      lFeature := YieldModelDataObject.
                    NetworkFeaturesData.WaterDemandFeatureList.CreateWaterDemandFeature;
      if (lFeature <> nil) then
      begin
        lChannel.WaterDemandFeature := lFeature;
        lFeature.Channel            := lChannel;
        YieldModelDataObject.NetworkFeaturesData.WaterDemandConfiguration.CreateNewWaterUseOutputProportion(AChannelNumber);
        Result                      := lFeature;
      end;
    end;

    if(lNode <> nil) then
      TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).ReselectTreeNode(LNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoDeleteWaterDemandFeature(AChannelNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.DoDeleteWaterDemandFeature';
var
  LNode        : TTreeNode;
  LDataObject  : TViewDataTreeNodeData;
  lChannel     : TGeneralFlowChannel;
  lFeatureID   : integer;
begin
  Result := FALSE;
  try
    if (AChannelNumber > 0) then
    begin
      lChannel     := YieldModelDataObject.CastNetworkElementData.
                        CastChannelList.CastChannelByChannelNumber[AChannelNumber];
      if (lChannel <> nil) then
      begin
        lFeatureID := lChannel.WaterDemandFeature.FeatureID;
        Result     := YieldModelDataObject.CastNetworkFeaturesData.
                      WaterDemandFeatureList.RemoveWaterDemandFeatureWithID(lFeatureID);
        Result     := Result AND lChannel.DeleteWaterDemandFeature;
        YieldModelDataObject.NetworkFeaturesData.WaterDemandConfiguration.DeleteWaterUseOutputProportion(AChannelNumber);
      end;
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject    := TViewDataTreeNodeData((LNode.Data));
          AChannelNumber := LDataObject.ViewDataNode.Weighting;
          lChannel       := YieldModelDataObject.CastNetworkElementData.
                              CastChannelList.CastChannelByChannelNumber[AChannelNumber];
          if (lChannel <> nil) then
          begin
            lFeatureID := lChannel.WaterDemandFeature.FeatureID;
            Result     := YieldModelDataObject.CastNetworkFeaturesData.
                          WaterDemandFeatureList.RemoveWaterDemandFeatureWithID(lFeatureID);
            Result     := Result and lChannel.DeleteWaterDemandFeature;
            YieldModelDataObject.NetworkFeaturesData.WaterDemandConfiguration.DeleteWaterUseOutputProportion(AChannelNumber);
          end;

          if Result then
            TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).ReselectTreeNode(LNode);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.CanCopyToCLipboard: boolean;
const OPNAME = 'TYieldModelManager.CanCopyToCLipboard';
begin
  Result := False;
  try
    if Assigned(FModelDataGUIManager) then
      Result := FModelDataGUIManager.CanCopyToCLipboard;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.CanExport: boolean;
const OPNAME = 'TYieldModelManager.CanExport';
begin
  Result := False;
  try
    if Assigned(FModelDataGUIManager) then
      Result := FModelDataGUIManager.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.CanPrint: boolean;
const OPNAME = 'TYieldModelManager.CanPrint';
begin
  Result := False;
  try
    if Assigned(FModelDataGUIManager) then
      Result := FModelDataGUIManager.CanPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelManager.DoCopyToCLipboard;
const OPNAME = 'TYieldModelManager.DoCopyToCLipboard';
begin
  try
    if Assigned(FModelDataGUIManager) then
      FModelDataGUIManager.DoCopyToCLipboard;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelManager.DoExport(AFileName: string = '');
const OPNAME = 'TYieldModelManager.DoExport';
begin
  try
    if Assigned(FModelDataGUIManager) then
      FModelDataGUIManager.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelManager.DoPrint;
const OPNAME = 'TYieldModelManager.DoPrint';
begin
  try
    if Assigned(FModelDataGUIManager) then
      FModelDataGUIManager.DoPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.SetUserExpertMode(AExpert: boolean): boolean;
const OPNAME = 'TYieldModelManager.SetUserExpertMode';
begin
  Result := False;
  try
    if AExpert then
      FAppModules.User.SetUserType(utExpert)
    else
      FAppModules.User.SetUserType(utStandard);


    if Assigned(FFileEditManager) then
    begin
      if Assigned(FAppModules.MainForm()) then
      begin
        if Assigned(FAppModules.MainForm.PageControl) then
        begin
          if AExpert then
          begin
            FFileEditManager.TabSheet.TabVisible := True;
            FFileEditManager.DoCustomTabSheetEvent(TModelMenuData.Create(meShowFileEditTab));
          end
          else
          begin
            if(FAppModules.Mainform.PageControl.ActivePageIndex =  FFileEditManager.TabSheet.PageIndex) then
            begin
              if(FAppModules.Mainform.PageControl.PageCount > 1) then
                 FAppModules.Mainform.PageControl.SelectNextPage(false);
            end;
            FFileEditManager.DoCustomTabSheetEvent(TModelMenuData.Create(meHideFileEditTab));
            FFileEditManager.TabSheet.TabVisible := False;
          end;
        end;
        TYieldModelMenuItemManager(FModelMenuItemManager).SetExpertUserChecked(AExpert);
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.GetKeyValue (AKeyName   : string; AKeyValues : string) : string;
const OPNAME = 'TYieldModelManager.GetKeyValue';
var
  lPos : integer;
begin
  Result := '';
  try
    lPos := Pos(AKeyName, AKeyValues);
    if (lPos > 0) then
    begin
      AKeyValues := Copy(AKeyValues, lPos, Length(AKeyValues) - lPos + 1);
      lPos       := Pos('=', AKeyValues);
      if (lPos > 0) then
      begin
        AKeyValues := Copy(AKeyValues, lPos+1, Length(AKeyValues) - lPos);
        lPos       := Pos(',', AKeyValues);
        if (lPos > 0) then
          Result := Copy(AKeyValues, 1, lPos - 1)
        else
          Result := AKeyValues;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.EntityDescription (AFieldPropName : string;
                                               AKeyValues     : string;
                                               AFieldIndex    : string) : string;
const OPNAME = 'TYieldModelManager.EntityDescription';
var
  lReservoir          : IReservoirData;
  lChannel            : IGeneralFlowChannel;
  lChannelPenalty     : IChannelPenalty;
  lPumpingFeature     : IPumpingFeature;
  lLossFeature        : ILossFeature;
  lMinimumFeature     : IMinimumFlowConstraint;
  lMinMaxFeature      :  IMinMaxFlowConstraint;
  lDiversionFeature   : IDiversionFeature;
  lIFRFeature         : IIFRFeature;
  lPhysFlowConstraint : IPhysicalFlowConstraint;
  lMasterFeature      : IMasterControlFeature;
  lSpecInflowFeature  : ISpecifiedInflowFeature;
  lSpecDemandFeature  : ISpecifiedDemandFeature;
  lIrrigationArea     : IIrrigationArea;
  lIrrigationBlock    : IIrrigationBlock;
  lWetland            : IWetland;
  lYMDemandCentre     : IYMDemandCentre;
  lPowerPlant         : IPowerPlant;
  lFieldProperty      : TAbstractFieldProperty;
  lKeyValue           : string;
  lItemID             : integer;
begin
  Result := inherited EntityDescription(AFieldPropName, AKeyValues, AFieldIndex);
  try
    if (AFieldPropName <> '') then
    begin
      lFieldProperty := FAppModules.FieldProperties.FieldProperty(Trim(AFieldPropName));
      if (lFieldProperty <> nil) then
      begin
        if (lFieldProperty.DataClassName = 'TTimeSeriesComparitorChart') OR
           (lFieldProperty.DataClassName = 'TTimeSeriesComparitorView') then
        begin
          {Result := FResultsTabSheetManager.TimeSeriesComparitorManager.
                      TabSheet.EntityDescription(AFieldPropName, AKeyValues, AFieldIndex);}
           FTimeSeriesComparitorManager.TabSheet.EntityDescription(AFieldPropName, AKeyValues, AFieldIndex);
        end
        else
        if (lFieldProperty.DataClassName = 'TYRCObject') then
        begin
          Result := FYieldReliabilityCurveManager.TabSheet.EntityDescription(AFieldPropName, AKeyValues, AFieldIndex);
        end
        else
        if (lFieldProperty.DataClassName = 'TRunConfigurationData') then
        begin
          Result := 'Run Configuration';
        end
        else
        if (lFieldProperty.DataClassName = 'TReservoirPenalty') then
        begin
          if (AFieldPropName = 'ReservoirPenalty') then
            Result := 'Penalty Structure' + AFieldIndex
          else
            Result := '';
        end
        else
        if (lFieldProperty.DataClassName = 'TChannelPenalty') then
        begin
          lKeyValue := GetKeyValue('Identifier', AKeyValues);
          if (lKeyValue <> '') then
          begin
            lItemID := StrToInt(lKeyValue);
            lChannelPenalty := YieldModelDataObject.NetworkElementData.
                                 ChannelPenaltyList.ChannelPenaltyByIdentifier[lItemID];
            if (lChannelPenalty <> nil) then
              Result := lChannelPenalty.ChannelPenaltyName;
          end;
        end
        else
        if (lFieldProperty.DataClassName = 'TPumpingFeature') then
        begin
          lKeyValue := GetKeyValue('Identifier', AKeyValues);
          if (lKeyValue <> '') then
          begin
            lItemID := StrToInt(lKeyValue);
            lPumpingFeature := YieldModelDataObject.NetworkFeaturesData.
                                 PumpingFeatureList.PumpingFeatureByID[lItemID];
            if (lPumpingFeature <> nil) then
            begin
              Result := lPumpingFeature.FeatureName;
              lChannel := YieldModelDataObject.NetworkElementData.ChannelList.
                            ChannelByChannelNumber[lPumpingFeature.Channel.ChannelNumber];
              if (lChannel <> nil) then
                Result := Result + ' (on ' + lChannel.ChannelName + ')';
            end;
          end;
        end
        else
        if (lFieldProperty.DataClassName = 'TLossFeature') then
        begin
          lKeyValue := GetKeyValue('Identifier', AKeyValues);
          if (lKeyValue <> '') then
          begin
            lItemID := StrToInt(lKeyValue);
            lLossFeature := YieldModelDataObject.NetworkFeaturesData.
                              LossFeatureList.LossFeatureByID[lItemID];
            if (lLossFeature <> nil) then
            begin
              Result := lLossFeature.FeatureName;
              lChannel := YieldModelDataObject.NetworkElementData.ChannelList.
                            ChannelByChannelNumber[lLossFeature.Channel.ChannelNumber];
              if (lChannel <> nil) then
                Result := Result + ' (on ' + lChannel.ChannelName + ')';
            end;
          end;
        end
        else
        if (lFieldProperty.DataClassName = 'TMinimumFlowConstraint') then
        begin
          lKeyValue := GetKeyValue('Identifier', AKeyValues);
          if (lKeyValue <> '') then
          begin
            lItemID := StrToInt(lKeyValue);
            lMinimumFeature := YieldModelDataObject.NetworkFeaturesData.
                                 MinimumFlowConstraintList.MinimumFlowConstraintByID[lItemID];
            if (lMinimumFeature <> nil) then
            begin
              Result := lMinimumFeature.FeatureName;
              lChannel := YieldModelDataObject.NetworkElementData.ChannelList.
                            ChannelByChannelNumber[lMinimumFeature.Channel.ChannelNumber];
              if (lChannel <> nil) then
                Result := Result + ' (on ' + lChannel.ChannelName + ')';
            end;
          end;
        end
        else
        if (lFieldProperty.DataClassName = 'TMinMaxFlowConstraint') then
        begin
          lKeyValue := GetKeyValue('Identifier', AKeyValues);
          if (lKeyValue <> '') then
          begin
            lItemID := StrToInt(lKeyValue);
            lMinMaxFeature := YieldModelDataObject.NetworkFeaturesData.
                                MinMaxFlowConstraintList.MinMaxFlowConstraintByID[lItemID];
            if (lMinMaxFeature <> nil) then
            begin
              Result := lMinMaxFeature.FeatureName;
              lChannel := YieldModelDataObject.NetworkElementData.ChannelList.
                            ChannelByChannelNumber[lMinMaxFeature.Channel.ChannelNumber];
              if (lChannel <> nil) then
                Result := Result + ' (on ' + lChannel.ChannelName + ')';
            end;
          end;
        end
        else
        if (lFieldProperty.DataClassName = 'TDiversionFeature') then
        begin
          lKeyValue := GetKeyValue('Identifier', AKeyValues);
          if (lKeyValue <> '') then
          begin
            lItemID := StrToInt(lKeyValue);
            lDiversionFeature := YieldModelDataObject.NetworkFeaturesData.
                                   DiversionFeatureList.DiversionFeatureByID[lItemID];
            if (lDiversionFeature <> nil) then
            begin
              Result := lDiversionFeature.FeatureName;
              lChannel := YieldModelDataObject.NetworkElementData.ChannelList.
                            ChannelByChannelNumber[lDiversionFeature.Channel.ChannelNumber];
              if (lChannel <> nil) then
                Result := Result + ' (on ' + lChannel.ChannelName + ')';
            end;
          end;
        end
        else
        if (lFieldProperty.DataClassName = 'TIFRFeature') then
        begin
          lKeyValue := GetKeyValue('Identifier', AKeyValues);
          if (lKeyValue <> '') then
          begin
            lItemID := StrToInt(lKeyValue);
            lIFRFeature := YieldModelDataObject.NetworkFeaturesData.
                             IFRFeatureList.MonthlyIFRFeatureByID[lItemID];
            if (lIFRFeature = nil) then
              lIFRFeature := YieldModelDataObject.NetworkFeaturesData.
                             IFRFeatureList.AnnualIFRFeatureByID[lItemID];
            if (lIFRFeature <> nil) then
            begin
              Result := lIFRFeature.FeatureName;
              lChannel := YieldModelDataObject.NetworkElementData.ChannelList.
                            ChannelByChannelNumber[lIFRFeature.Channel.ChannelNumber];
              if (lChannel <> nil) then
                Result := Result + ' (on ' + lChannel.ChannelName + ')';
            end;
          end;
        end
        else
        if (lFieldProperty.DataClassName = 'TPhysicalFlowConstraint') then
        begin
          lKeyValue := GetKeyValue('Identifier', AKeyValues);
          if (lKeyValue <> '') then
          begin
            lItemID := StrToInt(lKeyValue);
            lPhysFlowConstraint := YieldModelDataObject.NetworkFeaturesData.
                                     PhysicalFlowConstraintList.PhysicalFlowConstraintByID[lItemID];
            if (lPhysFlowConstraint <> nil) then
            begin
              Result := lPhysFlowConstraint.FeatureName;
              lChannel := YieldModelDataObject.NetworkElementData.ChannelList.
                            ChannelByChannelNumber[lPhysFlowConstraint.ChannelNumber];
              if (lChannel <> nil) then
                Result := Result + ' (on ' + lChannel.ChannelName + ')';
            end;
          end;
        end
        else
        if (lFieldProperty.DataClassName = 'TMasterControlFeature') then
        begin
          lKeyValue := GetKeyValue('Identifier', AKeyValues);
          if (lKeyValue <> '') then
          begin
            lItemID := StrToInt(lKeyValue);
            lMasterFeature := YieldModelDataObject.NetworkFeaturesData.
                                 MasterControlFeatureList.MasterControlFeatureByID[lItemID];
            if (lMasterFeature <> nil) then
            begin
              Result := lMasterFeature.FeatureName;
              lChannel := YieldModelDataObject.NetworkElementData.ChannelList.
                            ChannelByChannelNumber[lMasterFeature.Channel.ChannelNumber];
              if (lChannel <> nil) then
                Result := Result + ' (on ' + lChannel.ChannelName + ')';
            end;
          end;
        end
        else
        if (lFieldProperty.DataClassName = 'TGeneralFlowChannel') then
        begin
          lKeyValue := GetKeyValue('Identifier', AKeyValues);
          if (lKeyValue <> '') then
          begin
            lItemID := StrToInt(lKeyValue);
            lChannel := YieldModelDataObject.NetworkElementData.
                                 ChannelList.ChannelByIdentifier[lItemID];
            if (lChannel <> nil) then
              Result := lChannel.ChannelName;
          end;
        end
        else
        if (lFieldProperty.DataClassName = 'TReservoirData') then
        begin
          lKeyValue := GetKeyValue('Identifier', AKeyValues);
          if (lKeyValue <> '') then
          begin
            lItemID := StrToInt(lKeyValue);
            lReservoir := YieldModelDataObject.NetworkElementData.
                                 ReservoirList.ReservoirOrNodeByID[lItemID];
            if (lReservoir <> nil) then
              Result := lReservoir.ReservoirConfigurationData.ReservoirName;
          end;
        end
        else
        if (lFieldProperty.DataClassName = 'TSpecifiedInflowFeature') then
        begin
          lKeyValue := GetKeyValue('Identifier', AKeyValues);
          if (lKeyValue <> '') then
          begin
            lItemID := StrToInt(lKeyValue);
            lSpecInflowFeature := YieldModelDataObject.NetworkFeaturesData.
                                    SpecifiedInflowFeatureList.SpecifiedInflowFeatureByID[lItemID];
            if (lSpecInflowFeature <> nil) then
            begin
              Result := lSpecInflowFeature.FeatureName;
              lChannel := YieldModelDataObject.NetworkElementData.ChannelList.
                            ChannelByChannelNumber[lSpecInflowFeature.Channel.ChannelNumber];
              if (lChannel <> nil) then
                Result := Result + ' (on ' + lChannel.ChannelName + ')';
            end;
          end;
        end
        else
        if (lFieldProperty.DataClassName = 'TSpecifiedDemandFeature') then
        begin
          lKeyValue := GetKeyValue('Identifier', AKeyValues);
          if (lKeyValue <> '') then
          begin
            lItemID := StrToInt(lKeyValue);
            lSpecDemandFeature := YieldModelDataObject.NetworkFeaturesData.
                                    SpecifiedDemandFeatureList.SpecifiedDemandFeatureByID[lItemID];
            if (lSpecDemandFeature <> nil) then
            begin
              Result := lSpecDemandFeature.FeatureName;
              lChannel := YieldModelDataObject.NetworkElementData.ChannelList.
                            ChannelByChannelNumber[lSpecDemandFeature.Channel.ChannelNumber];
              if (lChannel <> nil) then
                Result := Result + ' (on ' + lChannel.ChannelName + ')';
            end;
          end;
        end
        else
        if (lFieldProperty.DataClassName = 'TIrrigationArea') then
        begin
          lKeyValue := GetKeyValue('Identifier', AKeyValues);
          if (lKeyValue <> '') then
          begin
            lItemID := StrToInt(lKeyValue);
            lIrrigationArea := YieldModelDataObject.NetworkFeaturesData.
                                 IrrigationAreaList.IrrigationAreaByNodeNumber[lItemID];
            if (lIrrigationArea <> nil) then
              Result := lIrrigationArea.FeatureName;
          end;
        end
        else
        if (lFieldProperty.DataClassName = 'TIrrigationBlock') then
        begin
          lKeyValue := GetKeyValue('Identifier', AKeyValues);
          if (lKeyValue <> '') then
          begin
            lItemID := StrToInt(lKeyValue);
            lIrrigationBlock := YieldModelDataObject.NetworkFeaturesData.
                                 IrrigationBlockList.IrrigationBlockByBlockNodeNumber[lItemID];
            if (lIrrigationBlock <> nil) then
              Result := lIrrigationBlock.BlockName;
          end;
        end
        else
        if (lFieldProperty.DataClassName = 'TWetland') then
        begin
          lKeyValue := GetKeyValue('Identifier', AKeyValues);
          if (lKeyValue <> '') then
          begin
            lItemID   := StrToInt(lKeyValue);
            lWetland  := YieldModelDataObject.NetworkFeaturesData.WetlandList.WetlandByID[lItemID];
            if (lWetland <> nil) then
              Result := lWetland.Name;
          end;
        end
        else
        if (lFieldProperty.DataClassName = 'TYMDemandCentre') then
        begin
          lKeyValue := GetKeyValue('Identifier', AKeyValues);
          if (lKeyValue <> '') then
          begin
            lItemID         := StrToInt(lKeyValue);
            lYMDemandCentre := YieldModelDataObject.NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[lItemID];
            if (lYMDemandCentre <> nil) then
              Result := lYMDemandCentre.Name;
          end;
        end
        else
        if (lFieldProperty.DataClassName = 'TPowerPlant') then
        begin
          lKeyValue := GetKeyValue('Identifier', AKeyValues);
          if (lKeyValue <> '') then
          begin
            lItemID := StrToInt(lKeyValue);
            lPowerPlant := YieldModelDataObject.NetworkFeaturesData.
                             PowerPlantList.PowerPlantByID[lItemID];
            if (lPowerPlant <> nil) then
              Result := lPowerPlant.FeatureName;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.SetBaseValue (AFieldPropName : string;
                                          AKeyValues     : string;
                                          AFieldIndex    : string;
                                          ANewValue      : string) : boolean;
const OPNAME = 'TYieldModelManager.SetBaseValue';
var
  lFieldProperty  : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    if (AFieldPropName <> '') then
    begin
      lFieldProperty := FAppModules.FieldProperties.FieldProperty(Trim(AFieldPropName));
      if (lFieldProperty <> nil) then
      begin
        if (lFieldProperty.DataClassName = 'TRunConfigurationData') then
          Result := SetBaseValueRunConfiguration(AFieldPropName, AKeyValues, AFieldIndex, ANewValue)
        else
        if (lFieldProperty.DataClassName = 'TReservoirPenalty') then
          Result := SetBaseValueReservoirPenalty(AFieldPropName, AKeyValues, AFieldIndex, ANewValue)
        else
        if (lFieldProperty.DataClassName = 'TChannelPenalty') then
          Result := SetBaseValueChannelPenalty(AFieldPropName, AKeyValues, AFieldIndex, ANewValue)
        else
        if (lFieldProperty.DataClassName = 'TPumpingFeature') then
          Result := SetBaseValuePumpingFeature(AFieldPropName, AKeyValues, AFieldIndex, ANewValue)
        else
        if (lFieldProperty.DataClassName = 'TLossFeature') then
          Result := SetBaseValueLossFeature(AFieldPropName, AKeyValues, AFieldIndex, ANewValue)
        else
        if (lFieldProperty.DataClassName = 'TMinimumFlowConstraint') then
          Result := SetBaseValueMinimumFlowFeature(AFieldPropName, AKeyValues, AFieldIndex, ANewValue)
        else
        if (lFieldProperty.DataClassName = 'TMinMaxFlowConstraint') then
          Result := SetBaseValueMinMaxFlowFeature(AFieldPropName, AKeyValues, AFieldIndex, ANewValue)
        else
        if (lFieldProperty.DataClassName = 'TIFRFeature') then
          Result := SetBaseValueIFRFeature(AFieldPropName, AKeyValues, AFieldIndex, ANewValue)
        ;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.SetBaseValuePumpingFeature (AFieldPropName : string;
                                                        AKeyValues     : string;
                                                        AFieldIndex    : string;
                                                        ANewValue      : string) : boolean;
const OPNAME = 'TYieldModelManager.SetBaseValuePumpingFeature';
var
  lPumpingFeature : IPumpingFeature;
  lKeyValue       : string;
  lItemID         : integer;
begin
  Result := FALSE;
  try
    lKeyValue := GetKeyValue('Identifier', AKeyValues);
    if (lKeyValue <> '') then
    begin
      lItemID := StrToInt(lKeyValue);
      lPumpingFeature := YieldModelDataObject.NetworkFeaturesData.
                           PumpingFeatureList.PumpingFeatureByID[lItemID];
      if (lPumpingFeature <> nil) then
      begin
        if (AFieldPropName = 'PumpingHead') then
        begin
          try
            lPumpingFeature.PumpingHead := StrToFloat(ANewValue);
            Result := TRUE;
          except
          end;
        end
        else if (AFieldPropName = 'PumpEfficiency') then
        begin
          try
            lPumpingFeature.PumpingEfficiency := StrToFloat(ANewValue);
            Result := TRUE;
          except
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.SetBaseValueLossFeature (AFieldPropName : string;
                                                     AKeyValues     : string;
                                                     AFieldIndex    : string;
                                                     ANewValue      : string) : boolean;
const OPNAME = 'TYieldModelManager.SetBaseValueLossFeature';
var
  lLossFeature    : ILossFeature;
  lKeyValue       : string;
  lItemID         : integer;
  lMonth          : integer;
begin
  Result := FALSE;
  try
    lKeyValue := GetKeyValue('Identifier', AKeyValues);
    if (lKeyValue <> '') then
    begin
      lItemID := StrToInt(lKeyValue);
      lLossFeature := YieldModelDataObject.NetworkFeaturesData.
                        LossFeatureList.LossFeatureByID[lItemID];
      if (lLossFeature <> nil) then
      begin
        if (AFieldPropName = 'WaterLoss') then
        begin
          try
            lMonth := StrToInt(AFieldIndex);
            lLossFeature.WaterLossByMonth[lMonth] := StrToFloat(ANewValue);
            Result := TRUE;
          except
          end;
        end
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.SetBaseValueMinimumFlowFeature (AFieldPropName : string;
                                                            AKeyValues     : string;
                                                            AFieldIndex    : string;
                                                            ANewValue      : string) : boolean;
const OPNAME = 'TYieldModelManager.SetBaseValueMinimumFlowFeature';
var
  lFeature    : IMinimumFlowConstraint;
  lKeyValue   : string;
  lItemID     : integer;
  lMonth      : integer;
begin
  Result := FALSE;
  try
    lKeyValue := GetKeyValue('Identifier', AKeyValues);
    if (lKeyValue <> '') then
    begin
      lItemID := StrToInt(lKeyValue);
      lFeature := YieldModelDataObject.NetworkFeaturesData.
                    MinimumFlowConstraintList.MinimumFlowConstraintByID[lItemID];
      if (lFeature <> nil) then
      begin
        if (AFieldPropName = 'MinFlowDemand') then
        begin
          try
            lMonth := StrToInt(AFieldIndex);
            lFeature.MinimumFlowDemandByMonth[lMonth] := StrToFloat(ANewValue);
            Result := TRUE;
          except
          end;
        end
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.SetBaseValueMinMaxFlowFeature (AFieldPropName : string;
                                                           AKeyValues     : string;
                                                           AFieldIndex    : string;
                                                           ANewValue      : string) : boolean;
const OPNAME = 'TYieldModelManager.SetBaseValueMinMaxFlowFeature';
var
  lFeature    : IMinMaxFlowConstraint;
  lKeyValue   : string;
  lItemID     : integer;
  lDim1Idx    : integer;
  lDim2Idx    : integer;
begin
  Result := FALSE;
  try
    lKeyValue := GetKeyValue('Identifier', AKeyValues);
    if (lKeyValue <> '') then
    begin
      lItemID := StrToInt(lKeyValue);
      lFeature := YieldModelDataObject.NetworkFeaturesData.
                    MinMaxFlowConstraintList.MinMaxFlowConstraintByID[lItemID];
      if (lFeature <> nil) then
      begin
        if (AFieldPropName = 'FlowConstraints') then
        begin
          try
            FAppModules.Changes.GetIndexes(AFieldIndex, lDim1Idx, lDim2Idx);
            lFeature.FlowConstraintByArcMonth[lDim1Idx,lDim2Idx] := StrToFloat(ANewValue);
            Result := TRUE;
          except
          end;
        end
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.GetBaseValue (AFieldPropName : string;
                                          AKeyValues     : string;
                                          AFieldIndex    : string) : string;
const OPNAME = 'TYieldModelManager.GetBaseValue';
var
  lFieldProperty  : TAbstractFieldProperty;
begin
  Result := '';
  try
    if (AFieldPropName <> '') then
    begin
      lFieldProperty := FAppModules.FieldProperties.FieldProperty(Trim(AFieldPropName));
      if (lFieldProperty <> nil) then
      begin
        if (lFieldProperty.DataClassName = 'TRunConfigurationData') then
          Result := GetBaseValueRunConfiguration(AFieldPropName, AKeyValues, AFieldIndex)
        else
        if (lFieldProperty.DataClassName = 'TReservoirPenalty') then
          Result := GetBaseValueReservoirPenalty(AFieldPropName, AKeyValues, AFieldIndex)
        else
        if (lFieldProperty.DataClassName = 'TChannelPenalty') then
          Result := GetBaseValueChannelPenalty(AFieldPropName, AKeyValues, AFieldIndex)
        else
        if (lFieldProperty.DataClassName = 'TPumpingFeature') then
          Result := GetBaseValuePumpingFeature(AFieldPropName, AKeyValues, AFieldIndex)
        else
        if (lFieldProperty.DataClassName = 'TLossFeature') then
          Result := GetBaseValueLossFeature(AFieldPropName, AKeyValues, AFieldIndex)
        else
        if (lFieldProperty.DataClassName = 'TMinimumFlowConstraint') then
          Result := GetBaseValueMinimumFlowFeature(AFieldPropName, AKeyValues, AFieldIndex)
        else
        if (lFieldProperty.DataClassName = 'TMinMaxFlowConstraint') then
          Result := GetBaseValueMinMaxFlowFeature(AFieldPropName, AKeyValues, AFieldIndex)
        else
        if (lFieldProperty.DataClassName = 'TIFRFeature') then
          Result := GetBaseValueIFRFeature(AFieldPropName, AKeyValues, AFieldIndex)
        ;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.GetBaseValuePumpingFeature (AFieldPropName : string;
                                                        AKeyValues     : string;
                                                        AFieldIndex    : string) : string;
const OPNAME = 'TYieldModelManager.GetBaseValuePumpingFeature';
var
  lPumpingFeature : IPumpingFeature;
  lKeyValue       : string;
  lItemID         : integer;
begin
  Result := '';
  try
    lKeyValue := GetKeyValue('Identifier', AKeyValues);
    if (lKeyValue <> '') then
    begin
      lItemID := StrToInt(lKeyValue);
      lPumpingFeature := YieldModelDataObject.NetworkFeaturesData.
                           PumpingFeatureList.PumpingFeatureByID[lItemID];
      if (lPumpingFeature <> nil) then
        Result := lPumpingFeature.GetBaseValue(AFieldPropName, AFieldIndex);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.GetBaseValueLossFeature (AFieldPropName : string;
                                                     AKeyValues     : string;
                                                     AFieldIndex    : string) : string;
const OPNAME = 'TYieldModelManager.GetBaseValueLossFeature';
var
  lLossFeature    : ILossFeature;
  lKeyValue       : string;
  lItemID         : integer;
begin
  Result := '';
  try
    lKeyValue := GetKeyValue('Identifier', AKeyValues);
    if (lKeyValue <> '') then
    begin
      lItemID := StrToInt(lKeyValue);
      lLossFeature := YieldModelDataObject.NetworkFeaturesData.
                        LossFeatureList.LossFeatureByID[lItemID];
      if (lLossFeature <> nil) then
        Result := lLossFeature.GetBaseValue(AFieldPropName, AFieldIndex);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.GetBaseValueMinimumFlowFeature (AFieldPropName : string;
                                                            AKeyValues     : string;
                                                            AFieldIndex    : string) : string;
const OPNAME = 'TYieldModelManager.GetBaseValueMinimumFlowFeature';
var
  lFeature    : IMinimumFlowConstraint;
  lKeyValue   : string;
  lItemID     : integer;
begin
  Result := '';
  try
    lKeyValue := GetKeyValue('Identifier', AKeyValues);
    if (lKeyValue <> '') then
    begin
      lItemID := StrToInt(lKeyValue);
      lFeature := YieldModelDataObject.NetworkFeaturesData.
                    MinimumFlowConstraintList.MinimumFlowConstraintByID[lItemID];
      if (lFeature <> nil) then
        Result := lFeature.GetBaseValue(AFieldPropName, AFieldIndex);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.GetBaseValueMinMaxFlowFeature (AFieldPropName : string;
                                                           AKeyValues     : string;
                                                           AFieldIndex    : string) : string;
const OPNAME = 'TYieldModelManager.GetBaseValueMinMaxFlowFeature';
var
  lFeature    : IMinMaxFlowConstraint;
  lKeyValue   : string;
  lItemID     : integer;
begin
  Result := '';
  try
    lKeyValue := GetKeyValue('Identifier', AKeyValues);
    if (lKeyValue <> '') then
    begin
      lItemID := StrToInt(lKeyValue);
      lFeature := YieldModelDataObject.NetworkFeaturesData.
                    MinMaxFlowConstraintList.MinMaxFlowConstraintByID[lItemID];
      if (lFeature <> nil) then
        Result := lFeature.GetBaseValue(AFieldPropName, AFieldIndex);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.GetBaseValueIFRFeature (AFieldPropName : string;
                                                    AKeyValues     : string;
                                                    AFieldIndex    : string): string;
const OPNAME = 'TYieldModelManager.GetBaseValueIFRFeature';
var
  lFeature    : IIFRFeature;
  lKeyValue   : string;
  lItemID     : integer;
begin
  Result := '';
  try
    lKeyValue := GetKeyValue('Identifier', AKeyValues);
    if (lKeyValue <> '') then
    begin
      lItemID := StrToInt(lKeyValue);
      lFeature := YieldModelDataObject.NetworkFeaturesData.
                    IFRFeatureList.MonthlyIFRFeatureByID[lItemID];
      if (lFeature = nil) then
        lFeature := YieldModelDataObject.NetworkFeaturesData.
                    IFRFeatureList.AnnualIFRFeatureByID[lItemID];

      if (lFeature <> nil) then
        Result := lFeature.GetBaseValue(AFieldPropName, AFieldIndex);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.SetBaseValueIFRFeature (AFieldPropName : string;
                                                    AKeyValues     : string;
                                                    AFieldIndex    : string;
                                                    ANewValue      : string) : boolean;
const OPNAME = 'TYieldModelManager.SetBaseValueIFRFeature';
var
  lFeature    : IIFRFeature;
  lKeyValue   : string;
  lItemID     : integer;
  lDim1Idx    : integer;
  lDim2Idx    : integer;
begin
  Result := FALSE;
  try
    lKeyValue := GetKeyValue('Identifier', AKeyValues);
    if (lKeyValue <> '') then
    begin
      lItemID := StrToInt(lKeyValue);
      lFeature := YieldModelDataObject.NetworkFeaturesData.
                    IFRFeatureList.MonthlyIFRFeatureByID[lItemID];
      if (lFeature = nil) then
      lFeature := YieldModelDataObject.NetworkFeaturesData.
                    IFRFeatureList.AnnualIFRFeatureByID[lItemID];

      if (lFeature <> nil) then
      begin
        if (AFieldPropName = 'LagInMonthsCount') then
        begin
          try
            FAppModules.Changes.GetIndexes(AFieldIndex, lDim1Idx, lDim2Idx);
            lFeature.LagMonths := StrToInt(ANewValue);
            Result := TRUE;
          except
          end;
        end
        else
        if (AFieldPropName = 'IFRVariables') then
        begin
          try
            FAppModules.Changes.GetIndexes(AFieldIndex, lDim1Idx, lDim2Idx);
            lFeature.InflowByIndexAndMonth[lDim1Idx, lDim2Idx] := StrToFloat(ANewValue);
            Result := TRUE;
          except
          end;
        end
        else
        if (AFieldPropName = 'IFRReleaseVariables') then
        begin
          try
            FAppModules.Changes.GetIndexes(AFieldIndex, lDim1Idx, lDim2Idx);
            lFeature.ReleaseByIndexAndMonth[lDim1Idx, lDim2Idx] := StrToFloat(ANewValue);
            Result := TRUE;
          except
          end;
        end
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.GetBaseValueChannelPenalty(AFieldPropName : string;
                                                       AKeyValues     : string;
                                                       AFieldIndex    : string) : string;
const OPNAME = 'TYieldModelManager.GetBaseValueChannelPenalty';
var
  lPenalty  : IChannelPenalty;
  lKeyValue : string;
  lItemID   : integer;
begin
  Result := '';
  try
    lKeyValue := GetKeyValue('Identifier', AKeyValues);
    if (lKeyValue <> '') then
    begin
      lItemID := StrToInt(lKeyValue);
      lPenalty := YieldModelDataObject.NetworkElementData.
                    ChannelPenaltyList.ChannelPenaltyByIdentifier[lItemID];
      if (lPenalty <> nil) then
        Result := lPenalty.GetBaseValue(AFieldPropName, AFieldIndex);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.SetBaseValueChannelPenalty (AFieldPropName : string;
                                                        AKeyValues     : string;
                                                        AFieldIndex    : string;
                                                        ANewValue      : string) : boolean;
const OPNAME = 'TYieldModelManager.SetBaseValueChannelPenalty';
var
  lPenalty  : IChannelPenalty;
  lKeyValue : string;
  lItemID   : integer;
  lArc      : integer;
begin
  Result := FALSE;
  try
    lKeyValue := GetKeyValue('Identifier', AKeyValues);
    if (lKeyValue <> '') then
    begin
      lItemID := StrToInt(lKeyValue);
      lPenalty := YieldModelDataObject.NetworkElementData.
                    ChannelPenaltyList.ChannelPenaltyByIdentifier[lItemID];
      if (lPenalty <> nil) then
      begin
        if (AFieldPropName = 'Penalty') then
        begin
          try
            lArc := StrToInt(AFieldIndex);
            lPenalty.ChannelPenaltyValueByIndex[lArc] := StrToFloat(ANewValue);
            Result := TRUE;
          except
          end;
        end
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.GetBaseValueReservoirPenalty(AFieldPropName : string;
                                                         AKeyValues     : string;
                                                         AFieldIndex    : string) : string;
const OPNAME = 'TYieldModelManager.GetBaseValueReservoirPenalty';
var
  lPenalty  : IReservoirPenalty;
  lZoneID   : string;
  lIndex    : integer;
begin
  Result := '';
  try
    lZoneID := GetKeyValue('Identifier', AKeyValues);
    if (lZoneID <> '') AND (AFieldIndex <> '') then
    begin
      try
        lIndex := StrToInt(AFieldIndex);
        lPenalty := YieldModelDataObject.NetworkElementData.
                      ReservoirPenaltyStructureList.ReservoirPenaltyByIndex[lIndex-1];
        if (lPenalty <> nil) then
          Result := lPenalty.GetBaseValue(AFieldPropName, lZoneID);
      except
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.SetBaseValueReservoirPenalty (AFieldPropName : string;
                                                          AKeyValues     : string;
                                                          AFieldIndex    : string;
                                                          ANewValue      : string) : boolean;
const OPNAME = 'TYieldModelManager.SetBaseValueReservoirPenalty';
var
  lPenalty  : IReservoirPenalty;
  lZoneID   : string;
  lZoneIdx  : integer;
  lIndex    : integer;
begin
  Result := FALSE;
  try
    if (AFieldPropName = 'ReservoirPenalty') then
    begin
      lZoneID := GetKeyValue('Identifier', AKeyValues);
      if (lZoneID <> '') AND (AFieldIndex <> '') then
      begin
        try
          lIndex   := StrToInt(AFieldIndex);
          lZoneIdx := StrToInt(lZoneID);
          lPenalty := YieldModelDataObject.NetworkElementData.
                        ReservoirPenaltyStructureList.ReservoirPenaltyByIndex[lIndex-1];
          if (lPenalty <> nil) then
          begin
            lPenalty.ReservoirPenaltyValueByIndex[lZoneIdx] := StrToFloat(ANewValue);
            Result := TRUE;
          end;
        except
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.SetBaseValueRunConfiguration (AFieldPropName : string;
                                                          AKeyValues     : string;
                                                          AFieldIndex    : string;
                                                          ANewValue      : string) : boolean;
const OPNAME = 'TYieldModelManager.SetBaseValueRunConfiguration';
var
  lRunConfig : IRunConfigurationData;
  lIndex     : integer;
begin
  Result := FALSE;
  try
    lRunConfig := YieldModelDataObject.RunConfigurationData;
    if (lRunConfig <> nil) then
    begin
      if (AFieldPropName = 'TYield') then
      begin
        try
          lIndex := StrToInt(AFieldIndex);
          lRunConfig.TargetYieldByIndex[lIndex] := StrToFloat(ANewValue);
          Result := TRUE;
        except
        end;
      end
      else
      if (AFieldPropName = 'MYield') then
      begin
        try
          lIndex := StrToInt(AFieldIndex);
          lRunConfig.MaximumYieldByIndex[lIndex] := StrToFloat(ANewValue);
          Result := TRUE;
        except
        end;
      end
      else
      if (AFieldPropName = 'TPower') then
      begin
        try
          lIndex := StrToInt(AFieldIndex);
          lRunConfig.TargetPowerByIndex[lIndex] := StrToFloat(ANewValue);
          Result := TRUE;
        except
        end;
      end
      else
      if (AFieldPropName = 'SummaryLevel') then
      begin
        try
          lRunConfig.OutputSummaryLevel := StrToInt(ANewValue);
          Result := TRUE;
        except
        end;
      end
      else
      if (AFieldPropName = 'DebugLevel') then
      begin
        try
          lRunConfig.DebugLevel := StrToInt(ANewValue);
          Result := TRUE;
        except
        end;
      end
      else
      if (AFieldPropName = 'LimitOpt') then
      begin
        try
          lRunConfig.LimitOption := ANewValue = '1';
          Result := TRUE;
        except
        end;
      end
      else
      if (AFieldPropName = 'SummaryOut') then
      begin
        try
          lRunConfig.CreateDataFile := ANewValue = '1';
          Result := TRUE;
        except
        end;
      end
      else
     if (AFieldPropName = 'StoreYield') then
      begin
        try
          lRunConfig.CreateYieldFile := ANewValue = '1';
          Result := TRUE;
        except
        end;
      end
      else
      if (AFieldPropName = 'PlotOpt') then
      begin
        try
          lRunConfig.CreatePlotFile := ANewValue = 'Y';
          Result := TRUE;
        except
        end;
      end
      else
      if (AFieldPropName = 'MultPeriodOpt') then
      begin
        try
          lRunConfig.MultiplePeriodLengths := ANewValue = '1';
          Result := TRUE;
        except
        end;
      end;
      if (AFieldPropName = 'ReduceSeqOpt') then
      begin
        try
          lRunConfig.ReduceSequences := ANewValue = '1';
          Result := TRUE;
        except
        end;
      end;
      if (AFieldPropName = 'CalcHistoryOpt') then
      begin
        try
          lRunConfig.CalculateHistoricFirmYield := StrToInt(ANewValue);
          Result := TRUE;
        except
        end;
      end
      else
      if (AFieldPropName = 'StartType') then
      begin
        try
          lRunConfig.GeneratedFlowFlag := StrToInt(ANewValue);
          Result := TRUE;
        except
        end;
      end
      else
      if (AFieldPropName = 'Title1') then
      begin
        lRunConfig.YieldRunTitle1 := ANewValue;
        Result := TRUE;
      end
      else
      if (AFieldPropName = 'Title2') then
      begin
        lRunConfig.YieldRunTitle2 := ANewValue;
        Result := TRUE;
      end
      else
      if (AFieldPropName = 'Title3') then
      begin
        lRunConfig.YieldRunTitle3 := ANewValue;
        Result := TRUE;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.GetBaseValueRunConfiguration(AFieldPropName : string;
                                                         AKeyValues     : string;
                                                         AFieldIndex    : string) : string;
const OPNAME = 'TYieldModelManager.GetBaseValueRunConfiguration';
var
  lRunConfig : IRunConfigurationData;
begin
  Result := '';
  try
    lRunConfig := YieldModelDataObject.RunConfigurationData;
    if (lRunConfig <> nil) then
      Result := lRunConfig.GetBaseValue(AFieldPropName, AFieldIndex);
  except on E: Exception do HandleError(E, OPNAME); end;
end;
function TYieldModelManager.InitialiseYearsInAnalysisFromParamDFile: boolean;
const OPNAME = 'TYieldModelManager.InitialiseYearsInAnalysisFromParamDFile';
var
  LNumberOfYears: integer;
  LStartYearGregorian: integer;
  LParamFileName: string;
  LFileObject: TAbstractModelFileName;
begin
  Result := False;
  try
    LFileObject := YieldModelDataObject.CastFileNamesObject.ParamFileNames.FileNameObject[0];
    if(LFileObject <> nil) then
    begin
      LParamFileName := LFileObject.FileName;
      if GetAnalysisYearsFromParamDataFile(LParamFileName,LNumberOfYears,LStartYearGregorian) then
      begin
        YieldModelDataObject.RunConfigurationData.StartYearOther       := LStartYearGregorian;
        YieldModelDataObject.RunConfigurationData.YearsInAnalysis      := LNumberOfYears;
        YieldModelDataObject.RunConfigurationData.PeriodsInAnalysis    := LNumberOfYears * 12;
        YieldModelDataObject.RunConfigurationData.EndDebugPeriod       := YieldModelDataObject.RunConfigurationData.PeriodsInAnalysis;
        Result := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.DoImportDemandFile(const AFileNAme: WideString): boolean;
const OPNAME = 'TYieldModelManager.DoImportDemandFile';
begin
  Result := False;
  try
    Result := TFilesActionYieldManager(FModelFilesActionManager).ImportDemandFile(AFileNAme);
    if Result then
    begin
      if Assigned(FFileEditManager) and Assigned(FFileEditManager.TabSheet) then
      begin
        FFileSelectionManager.PopulateTreeView(TTreeViewTabSheet(FFileEditManager.TabSheet).TreeView,
          YieldModelDataObject.CastFileNamesObject);
        FFileEditManager.TabSheet.LanguageHasChanged;
      end;
   end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.DoImportDamLevelsFile(const AFileNAme: WideString): boolean;
const OPNAME = 'TYieldModelManager.DoImportDamLevelsFile';
begin
  Result := False;
  try
    Result := TFilesActionYieldManager(FModelFilesActionManager).ImportDamWaterLevelsFile(AFileNAme);
    if Result then
    begin
      if Assigned(FFileEditManager) and Assigned(FFileEditManager.TabSheet) then
      begin
        FFileSelectionManager.PopulateTreeView(TTreeViewTabSheet(FFileEditManager.TabSheet).TreeView,
          YieldModelDataObject.CastFileNamesObject);
        FFileEditManager.TabSheet.LanguageHasChanged;
      end;
   end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.DoImportHydrologyFile(const AFileNAme: WideString): boolean;
const OPNAME = 'TYieldModelManager.DoImportHydrologyFile';
begin
  Result := False;
  try
    Result := TFilesActionYieldManager(FModelFilesActionManager).ImportHydrologyFile(AFileNAme);
    if Result then
    begin
      if Assigned(FFileEditManager) and Assigned(FFileEditManager.TabSheet) then
      begin
        FFileSelectionManager.PopulateTreeView(TTreeViewTabSheet(FFileEditManager.TabSheet).TreeView,
          YieldModelDataObject.CastFileNamesObject);
        FFileEditManager.TabSheet.LanguageHasChanged;
      end;
   end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TYieldModelManager.DoClearParamFile(const AFileNAme: WideString): boolean;
const OPNAME = 'TYieldModelManager.DoClearParamFile';
begin
  Result := False;
  try
    Result := TFilesActionYieldManager(FModelFilesActionManager).ClearParamFile(AFileNAme);
    if Result then
    begin
      if Assigned(FFileEditManager) and Assigned(FFileEditManager.TabSheet) then
      begin
        FFileSelectionManager.PopulateTreeView(TTreeViewTabSheet(FFileEditManager.TabSheet).TreeView,
          YieldModelDataObject.CastFileNamesObject);
        FFileEditManager.TabSheet.LanguageHasChanged;
      end;
   end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.DoImportParamFile(const AFileNAme: WideString): boolean;
const OPNAME = 'TYieldModelManager.DoImportParamFile';
begin
  Result := False;
  try
    Result := TFilesActionYieldManager(FModelFilesActionManager).ImportParamFile(AFileNAme);
    if Result then
    begin
      if Assigned(FFileEditManager) and Assigned(FFileEditManager.TabSheet) then
      begin
        FFileSelectionManager.PopulateTreeView(TTreeViewTabSheet(FFileEditManager.TabSheet).TreeView,
          YieldModelDataObject.CastFileNamesObject);
        FFileEditManager.TabSheet.LanguageHasChanged;
      end;
   end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.ImportParamAndPathsFile: boolean;
const OPNAME = 'TYieldModelManager.ImportParamAndPathsFile';
var
  LPathsFile,
  LParamFile : TAbstractModelFileName;
  LDialog    : TfrmImportParam;
begin
  Result := False;
  try
    LParamFile := YieldModelDataObject.CastFileNamesObject.ParamFileNames.FileNameObject[0];
    LPathsFile := YieldModelDataObject.CastFileNamesObject.DirectoryFileNames.FileNameObject[0];
    if(LParamFile <> nil) and (not LParamFile.SavedInDB) and
       FileExists(LParamFile.FileName) then
    begin
      LDialog := TfrmImportParam.Create(nil);
      try
        LDialog.lblMessage.Caption := #10#13 + FAppModules.Language.GetString('ParamFile.ImportFileTakesTime');
        LDialog.ShowModal;
        TFilesActionYieldManager(FModelFilesActionManager).ImportParamFile(LParamFile.FileName);
        if LDialog.chkboxImportHyrology.Checked then
          TFilesActionYieldManager(FModelFilesActionManager).ImportAllHydrologyFiles;
      finally
        LDialog.Free;
      end;
    end;

    if(LPathsFile <> nil) and (not LPathsFile.SavedInDB) and
       FileExists(LPathsFile.FileName) then
       TFilesActionYieldManager(FModelFilesActionManager).ImportPathsFile(LPathsFile.FileName);

    Result := True;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.HandleVNVEvent(const AVisioApp,
  AVisioDoc: IInterface; AVisioEventCode: Integer;
  const ASourceObj: IInterface; AEventID, AEventSeqNum: Integer;
  const ASubjectObj: IInterface; AMoreInfo: OleVariant): WordBool;
const OPNAME = 'TYieldModelManager.HandleVNVEvent';
begin
  Result := False;
  try
    if(FNetworkVisualiserManager <> nil) then
       Result := FNetworkVisualiserManager.HandleVNVEvent(AVisioApp,AVisioDoc,AVisioEventCode,
                ASourceObj, AEventID, AEventSeqNum,ASubjectObj,AMoreInfo);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.ProcessVNVSpecial (const AParameter: WideString): WordBool;
const OPNAME = 'TYieldModelManager.ProcessVNVSpecial';
begin
  Result := FALSE;
  try
    if (FNetworkVisualiserManager <> nil) then
      Result := FNetworkVisualiserManager.ProcessVNVSpecial(AParameter);
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TYieldModelManager.GetModelViewItems(AItems: TStringListOfStringLists): boolean;
const OPNAME = 'TYieldModelManager.GetModelViewItems';
begin
  Result := False;
  try
    Result := YieldModelDataObject.GetModelViewItems(AItems);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldModelManager.OnTabHasChanged(ASender: TObject);
const OPNAME = 'TYieldModelManager.OnTabHasChanged';
begin
  try
    if (Assigned(FModelMenuItemManager)) then
    begin
      if ((Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) and
         (FDataTabSheetManager.TabSheet = FAppModules.MainForm.PageControl.ActivePage)) then
      begin
        TYieldModelMenuItemManager(FModelMenuItemManager).TabHasChanged(TRUE);
      end
      else
        TYieldModelMenuItemManager(FModelMenuItemManager).TabHasChanged(FALSE);
    end;
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.GetModelDataSetKey : string;
const OPNAME = 'TYieldModelManager.GetModelDataSetKey';
begin
  Result := '';
  try
    Result := 'Model='         + QuotedStr(FAppModules.StudyArea.ModelCode) + ',' +
              'StudyAreaName=' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ',' +
              'SubArea='       + QuotedStr(FAppModules.StudyArea.SubAreaCode) + ',' +
              'Scenario='      + QuotedStr(FAppModules.StudyArea.ScenarioCode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCreateIrrigationBlock: IIrrigationBlock;
const OPNAME = 'TYieldModelManager.DoCreateIrrigationBlock';
var
  LBlockType : integer;
//  LBlockIndex : integer;
begin
  Result := nil;
  try
    if(FAppModules.StudyArea.ModelVersion <> '7')  then Exit;
   // LBlockIndex := SelectionRadio('Select irrigation block type','Please specify the irrigation block type you would like to create.','1,2,4',-1);
    LBlockType  := 1;
    {case LBlockIndex of
      0: LBlockType := 1;
      1: LBlockType := 2;
      2: LBlockType := 4;
    end;
    if not (LBlockType in [1,2,4]) then Exit;
     }
    Result := YieldModelDataObject.CastNetworkFeaturesData.CastIrrigationBlockList.CreateIrrigationBlock(LBlockType);
    if (Result <> nil) then
    begin
      CreateTabSheetTreeViewElement('IrrigationBlock',
                                 'FeaturesHeading,IrrigationBlock',
                                 (Result as IIrrigationBlock).BlockNodeNumber,tvnidIrrigationBlock,
                                 (Result as IIrrigationBlock).BlockName,
                                 'IRRIGATIONBLOCK',
                                 'IRRIGATIONBLOCK',True,tvsnAll);
      StudyDataHasChanged(sdccAdd, 'IrrigationBlockName', '0',IntToStr((Result as IIrrigationBlock).BlockNodeNumber));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoDeleteIrrigationBlock(ABlockNodeNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.DoDeleteIrrigationBlock';
var
  lNode       : TTreeNode;
  lDataObject : TViewDataTreeNodeData;
begin
  Result := FALSE;
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then Exit;
    if (ABlockNodeNumber > 0) then
    begin
      Result := YieldModelDataObject.CastNetworkFeaturesData.CastIrrigationBlockList.RemoveIrrigationBlock(ABlockNodeNumber);
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) and Assigned(LNode.Data)) then
        begin
          LDataObject   := TViewDataTreeNodeData((LNode.Data));
          ABlockNodeNumber  := LDataObject.ViewDataNode.Weighting;
          Result := YieldModelDataObject.CastNetworkFeaturesData.CastIrrigationBlockList.RemoveIrrigationBlock(ABlockNodeNumber);
          if (Result) then
            DeleteTabSheetTreeNode(lNode);
          SetSystemMenuState;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCreateWetland: IWetland;
const OPNAME = 'TYieldModelManager.DoCreateWetland';
begin
  Result := nil;
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then Exit;
    Result := YieldModelDataObject.CastNetworkFeaturesData.CastWetlandList.CreateWetland;
    if (Result <> nil) then
    begin
      CreateTabSheetTreeViewElement('Wetland',
                                 'FeaturesHeading,Wetland',
                                 (Result as IWetland).NodeNumber, tvnidWetland,
                                 (Result as IWetland).Name,
                                 'WETLAND',
                                 'WETLAND',True,tvsnAll);
      StudyDataHasChanged(sdccAdd, 'WetlandName', '0',IntToStr((Result as IWetland).Identifier));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoDeleteWetland(ANodeNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.DoDeleteWetland';
var
  lNode                 : TTreeNode;
  lDataObject           : TViewDataTreeNodeData;
begin
  Result  := FALSE;
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then Exit;
    if (ANodeNumber > 0) then
    begin
      Result := YieldModelDataObject.CastNetworkFeaturesData.CastWetlandList.RemoveWetland(ANodeNumber);
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          ANodeNumber  := LDataObject.ViewDataNode.Weighting;
          Result := YieldModelDataObject.CastNetworkFeaturesData.CastWetlandList.RemoveWetland(ANodeNumber);
          if (Result) then
            DeleteTabSheetTreeNode(lNode);
          SetSystemMenuState;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelManager.DoLaunchDocumentViewer(ADocumentKey: integer);
const OPNAME = 'TModelRainfallDataManager.DoLaunchDocumentViewer';
begin
  try
    if IsAcrobatReaderInstalled then
    begin
      if(ADocumentKey = CmeWRMFReleaseNote) then
        TPDFDocumentLauncher.Launch(ExtractFilePath(ApplicationExeName) + 'help\WRYM Release Note.pdf');
      if(ADocumentKey = CmeWRYMUserGuide) then
        TPDFDocumentLauncher.Launch(ExtractFilePath(ApplicationExeName) + 'help\WRYM User Guide.pdf');
      if(ADocumentKey = CmeWRYMProceduralManual) then
      TPDFDocumentLauncher.Launch(ExtractFilePath(ApplicationExeName)   + 'help\WRYM Procedural Manual.pdf');
      if(ADocumentKey = CmeWRYMTrainingMaterial) then
      TPDFDocumentLauncher.Launch(ExtractFilePath(ApplicationExeName)   + 'help\WRYM Training Material.pdf');
      if(ADocumentKey = CmeWRYMParameterTables) then
        TPDFDocumentLauncher.Launch(ExtractFilePath(ApplicationExeName) + 'help\WRYM Files Parameter Tables.pdf');

      if(ADocumentKey = CmeWRPMReleaseNote) then
        TPDFDocumentLauncher.Launch(ExtractFilePath(ApplicationExeName) + 'help\WRPM Release Note.pdf');
      if(ADocumentKey = CmeWRPMUserGuide) then
        TPDFDocumentLauncher.Launch(ExtractFilePath(ApplicationExeName) + 'help\WRPM User Guide.pdf');
      if(ADocumentKey = CmeWRPMParameterTables) then
        TPDFDocumentLauncher.Launch(ExtractFilePath(ApplicationExeName) + 'help\WRPM Files Parameter Tables.pdf');
      if(ADocumentKey = CmeHydrologyParameterTables) then
        TPDFDocumentLauncher.Launch(ExtractFilePath(ApplicationExeName) + 'help\Hydrology Files Parameter Tables.pdf');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCreateYMDemandCentre: IYMDemandCentre;
const OPNAME = 'TYieldModelManager.DoCreateYMDemandCentre';
begin
  Result := nil;
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then Exit;
    Result := YieldModelDataObject.CastNetworkFeaturesData.CastYMDemandCentreList.CreateYMDemandCentre;
    if (Result <> nil) then
    begin
      if(Result.SupplyChannelByIndex[0] <> nil) then
        CreateTabSheetTreeViewElement('ChannelDetails12',
                                   'ChannelHeading,ChannelDetails12',
                                   Result.SupplyChannelByIndex[0].ChannelNumber,tvnidChannelDetails12,
                                   Result.SupplyChannelByIndex[0].ChannelName,
                                   'GENERALCHANNEL',
                                   'CHANNEL',
                                   True,tvsnAll);
      CreateTabSheetTreeViewElement('YMDemandCentre',
                                 'FeaturesHeading,YMDemandCentre',
                                 (Result as IYMDemandCentre).NodeNumber,tvnidYMDemandCentre,
                                 (Result as IYMDemandCentre).Name,
                                 'YMDEMANDCENTRE',
                                 'YMDEMANDCENTRE',True,tvsnAll);
      StudyDataHasChanged(sdccAdd, 'YMDemandCentreName', '0',IntToStr((Result as IYMDemandCentre).Identifier));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoDeleteYMDemandCentre(ANodeNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.DoDeleteYMDemandCentre';
var
  lNode       : TTreeNode;
  lDataObject : TViewDataTreeNodeData;
begin
  Result  := FALSE;
  try
    if (FAppModules.StudyArea.ModelVersion <> '7') then
      Exit;

    if (ANodeNumber > 0) then
    begin
      Result := YieldModelDataObject.CastNetworkFeaturesData.CastYMDemandCentreList.RemoveYMDemandCentre(ANodeNumber);
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) and Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) and Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          ANodeNumber := LDataObject.ViewDataNode.Weighting;
          Result := YieldModelDataObject.CastNetworkFeaturesData.CastYMDemandCentreList.RemoveYMDemandCentre(ANodeNumber);
          if (Result) then
            DeleteTabSheetTreeNode(lNode);
          SetSystemMenuState;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCreateSFRSubCatchment: IStreamFlowReduction;
const OPNAME = 'TYieldModelManager.DoCreateSFRSubCatchment';
begin
  Result := nil;
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then Exit;
    Result := YieldModelDataObject.CastNetworkFeaturesData.CastStreamFlowReductionList.CreateStreamFlowReduction;
    if (Result <> nil) then
    begin
      CreateTabSheetTreeViewElement('StreamFlowReduction',
                                 'FeaturesHeading,StreamFlowReduction',
                                  Result.Identifier,tvnidStreamFlowReduction,
                                  Result.SFRName,
                                 '',
                                 'STREAMFLOWREDUCTION',True,tvsnAll);
      StudyDataHasChanged(sdccAdd, 'StreamFlowReduction', '0',IntToStr((Result as IStreamFlowReduction).Identifier));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoDeleteSFRSubCatchment(AIdentifier: Integer): WordBool;
const OPNAME = 'TYieldModelManager.DoDeleteSFRSubCatchment';
var
  lNode       : TTreeNode;
  lDataObject : TViewDataTreeNodeData;
begin
  Result  := FALSE;
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then Exit;
    if (AIdentifier > 0) then
    begin
      Result := YieldModelDataObject.CastNetworkFeaturesData.CastStreamFlowReductionList.RemoveStreamFlowReduction(AIdentifier);
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) and Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) and Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          AIdentifier := LDataObject.ViewDataNode.Weighting;
          Result := YieldModelDataObject.CastNetworkFeaturesData.CastStreamFlowReductionList.RemoveStreamFlowReduction(AIdentifier);
          if (Result) then
            DeleteTabSheetTreeNode(lNode);
          SetSystemMenuState;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.GetChangeListWhereClause: string;
const OPNAME = 'TYieldModelManager.GetChangeListWhereClause';
begin
  Result := '';
  try
    Result := 'Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ' AND ' +
              'StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ' AND ' +
              'SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ' AND ' +
              'Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoGenerateSystemConfigDataFiles: Boolean;
const OPNAME = 'TYieldModelManager.DoGenerateSystemConfigDataFiles';
begin
  Result := False;
  try
    Result := FModelFilesActionManager.DoGenerateSystemConfigurationDataFiles;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.LoadIFRSiteData: boolean;
const OPNAME = 'TYieldModelManager.LoadIFRSiteData';
var
  LAgent : TIFRDataLoadAgent;
begin
  Result := False;
  try
    LAgent := TIFRDataLoadAgent.Create(FAppModules);
    try
      Result := LAgent.LoadDataFromDB(YieldModelDataObject.CastNetworkFeaturesData.CastIFRSiteList);
    finally
      LAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{function TYieldModelManager.DoStationFilter( AChangeListID: integer): WordBool;
const OPNAME = 'TYieldModelManager.DoStationFilter';
begin
  Result := True;
  try //Do not insert code here
    Result := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

{function TYieldModelManager.ReadFirmYieldFromDebugFile: Double;
const OPNAME = 'TYieldModelManager.ReadFirmYieldFromDebugFile';
var
  LIndex         : integer;
  LFirmYieldLine : string;
  LDebugFileName,
  LFileName      : TAbstractModelFileName;
  LFileContents  : TStringlist;
begin
  Result := NullFloat;
  try
    LDebugFileName := nil;
    for LIndex := 0 to YieldModelDataObject.CastFileNamesObject.CastOutputFileNames.Count-1 do
    begin
      LFileName := YieldModelDataObject.CastFileNamesObject.CastOutputFileNames.FileNameObject[LIndex];
      if(Pos('DBG.OUT',UpperCase(LFileName.FileName)) > 0) then
      begin
        LDebugFileName := LFileName;
        Break;
      end;
    end;
    if(LDebugFileName <> nil) and FileExists(LDebugFileName.FileName) then
    begin
      LFileContents := TStringlist.Create;
      try
        LFileContents.LoadFromFile(LDebugFileName.FileName);
        LFirmYieldLine := '';
        for LIndex := 0 to LFileContents.Count-1 do
        begin
          if(Pos('PREVIOUS YIELD WITHOUT FAILURES',LFileContents[LIndex]) > 0) then
            LFirmYieldLine := LFileContents[LIndex];
        end;

        if(LFirmYieldLine <> '') then
        begin
          LIndex := Pos('=',LFirmYieldLine);
          if(LIndex <> 0) then
          begin
            LFirmYieldLine := Copy(LFirmYieldLine,LIndex+1,Length(LFirmYieldLine));
            LFirmYieldLine := Trim(LFirmYieldLine);
            Result := StrToFloatDef(LFirmYieldLine,NullFloat);
          end;
        end;
      finally
        LFileContents.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

function TYieldModelManager.DoCreateMine: IMine;
const OPNAME = 'TYieldModelManager.DoCreateMine';
begin
  Result := nil;
  try
    FMineCreated := True;
    if (FAppModules.StudyArea.ModelVersion <> '7')  then Exit;
    Result := YieldModelDataObject.CastNetworkFeaturesData.CastMineList.CreateMine;
    if (Result <> nil) then
    begin
      CreateTabSheetTreeViewElement('Mine',
                                 'FeaturesHeading,Mine',
                                 (Result as IMine).NodeNumber,tvnidMine,
                                 (Result as IMine).MineName,
                                 'Mine',
                                 'Mine',True,tvsnAll);
      AddMineNodes(Result);
      StudyDataHasChanged(sdccAdd, 'Mine', '0',IntToStr((Result as IMine).NodeNumber));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoDeleteMine(AMineNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.DoDeleteMine';
var
  lNode        : TTreeNode;
  lDataObject  : TViewDataTreeNodeData;
begin
  Result  := FALSE;
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then Exit;
    if (AMineNumber > 0) then
    begin
      Result := YieldModelDataObject.CastNetworkFeaturesData.CastMineList.RemoveMine(AMineNumber);
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject     := TViewDataTreeNodeData((LNode.Data));
          AMineNumber := LDataObject.ViewDataNode.Weighting;
          Result := YieldModelDataObject.CastNetworkFeaturesData.CastMineList.RemoveMine(AMineNumber);
          if (Result) then
            DeleteTabSheetTreeNode(lNode);
          SetSystemMenuState;
        end;
      end;
    end;
    if Result then
      StudyDataHasChanged(sdccDelete, 'Mine', '0',IntToStr(AMineNumber));

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCreateOpenCast(AMineNumber: integer): IOpenCast;
const OPNAME = 'TYieldModelManager.DoCreateOpenCast';
var
  LMine        : IMine;
begin
  Result := nil;
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then Exit;
    if(AMineNumber > 0) then
    begin
      LMine := YieldModelDataObject.CastNetworkFeaturesData.CastMineList.MineByNodeNumber[AMineNumber];
      if(LMine <> nil) then
      begin
        Result := LMine.CreateOpenCast;
        if (Result <> nil) then
          StudyDataHasChanged(sdccAdd, 'OpenCast', '0',IntToStr(Result.Identifier));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoDeleteOpenCast(AMineNumber,AOpenCastIdentifier: integer): WordBool;
const OPNAME = 'TYieldModelManager.DoDeleteOpenCast';
var
  LMine        : IMine;
begin
  Result := False;
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then Exit;
    if (AMineNumber > 0) and (AOpenCastIdentifier > 0) then
    begin
      LMine := YieldModelDataObject.CastNetworkFeaturesData.CastMineList.MineByNodeNumber[AMineNumber];
      if(LMine <> nil) then
      begin
        Result := LMine.RemoveOpenCast(AOpenCastIdentifier);
        if Result then
          StudyDataHasChanged(sdccDelete, 'OpenCast', '0',IntToStr(AOpenCastIdentifier));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCreateUnderGround(AMineNumber: integer): IUnderground;
const OPNAME = 'TYieldModelManager.DoCreateUnderGround';
var
  LMine        : IMine;
begin
  Result := nil;
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then Exit;
    if(AMineNumber > 0) then
    begin
      LMine := YieldModelDataObject.CastNetworkFeaturesData.CastMineList.MineByNodeNumber[AMineNumber];
      if(LMine <> nil) then
      begin
        Result := LMine.CreateUnderGround;
        if(Result <> nil) then
        begin
          if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
          begin
            if (Result.UndergroundDam <> nil) then
              CreateTabSheetTreeViewElement('UndegroundDam',
                                         'FeaturesHeading,Mine,Mine',
                                         Result.UndergroundDam.ReservoirConfigurationData.ReservoirIdentifier,
                                         AMineNumber,
                                         Result.UndergroundDam.ReservoirConfigurationData.ReservoirName,
                                         'RESERVOIR',
                                         'UNDEGROUNDDAM',
                                         False,tvsnAll);
          end;
          StudyDataHasChanged(sdccAdd, 'UndegroundDam', '0',IntToStr(Result.Identifier));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.GetUnderGroundNodeNumber(AMineNumber,AUnderGroundIdentifier: integer): integer;
const OPNAME = 'TYieldModelManager.GetUnderGroundNodeNumber';
var
  LMine : IMine;
begin
  Result := 0;
  try
    LMine := YieldModelDataObject.CastNetworkFeaturesData.CastMineList.MineByNodeNumber[AMineNumber];
    if(LMine <> nil) then
      if(LMine.UnderGroundByIdentifier[AUnderGroundIdentifier] <> nil)  and
        (LMine.UnderGroundByIdentifier[AUnderGroundIdentifier].UndergroundDam <> nil) then
        Result := LMine.UnderGroundByIdentifier[AUnderGroundIdentifier].UndergroundDam.ReservoirConfigurationData.ReservoirIdentifier;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelManager.ResetMinMaxChannelFlow(AIFRFeature : IIFRFeature);
const OPNAME = 'TYieldModelManager.ResetMinMaxChannelFlow';
var
  LMinMaxFlowConstraint : IMinMaxFlowConstraint;
  LMinMaxFlowField : TAbstractFieldProperty;
  LMonth,
  LArc : integer;
  LChannel : IGeneralFlowChannel;
  LChannelPenaltyList : IChannelPenaltyList;
  LPenaltyStructure : IChannelPenalty;
begin
  try
    if (AIFRFeature <> nil) and (AIFRFeature.Channel <> nil) then
    begin
      LMinMaxFlowField := FAppModules.FieldProperties.FieldProperty('FlowConstraints');
      if not Assigned(LMinMaxFlowField) then
        raise Exception.Create('Field (FlowConstraints) not found in field properties');
      LChannel := AIFRFeature.Channel;
      if (LChannel <> nil) then
      begin
        LMinMaxFlowConstraint := LChannel.MinMaxFlowConstraint;
        if (LMinMaxFlowConstraint <> nil) then
        begin
          if (LChannel.ChannelPenalty <> nil) then
          begin
            for LArc := 1 to LChannel.ChannelPenalty.ChannelPenaltyArcCount do
            begin
              for LMonth := LMinMaxFlowField.ArrayLowDimTwo to LMinMaxFlowField.ArrayHighDimTwo do
              begin
                LMinMaxFlowConstraint.FlowConstraintByArcMonth[LArc, LMonth] := 0.0;
                if (FAppModules.Model.ModelName = CPlanning) then
                  LMinMaxFlowConstraint.DistributionByArcMonth[LArc, LMonth] := 0.0;
              end;
            end;
          end
          else
          if (LChannel.ChannelPenalty = nil) then
          begin
            LChannelPenaltyList := YieldModelDataObject.CastNetworkElementData.CastChannelPenaltyList;
            LPenaltyStructure := LChannelPenaltyList.CreateChannelPenalty;
            LChannel.ChannelPenalty := LPenaltyStructure;
            LMinMaxFlowConstraint.CreateFlowConstraints;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TYieldModelManager.DoDeleteUnderGround(AMineNumber,AUnderGroundIdentifier: integer): WordBool;
const OPNAME = 'TYieldModelManager.DoDeleteUnderGround';
var
  LMine : IMine;
  LResevoirNumber : integer;
begin
  Result := False;
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then Exit;
    if (AMineNumber > 0) and (AUnderGroundIdentifier > 0) then
    begin
      LMine := YieldModelDataObject.CastNetworkFeaturesData.CastMineList.MineByNodeNumber[AMineNumber];
      if(LMine <> nil) then
      begin
        LResevoirNumber := GetUnderGroundNodeNumber(AMineNumber,AUnderGroundIdentifier);
        Result := LMine.RemoveUnderGround(AUnderGroundIdentifier);
        if Result then
        begin
          if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
          begin
            if(LResevoirNumber <> 0) then
              DeleteTabSheetTreeViewElement('UndegroundDam',
                                            'FeaturesHeading,Mine,Mine',
                                            LResevoirNumber,
                                            AMineNumber);
          end;
          StudyDataHasChanged(sdccDelete, 'UndegroundDam', '0',IntToStr(AUnderGroundIdentifier));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCreateSlurryDump(AMineNumber: integer): ISlurryDump;
const OPNAME = 'TYieldModelManager.DoCreateSlurryDump';
var
  LMine        : IMine;
begin
  Result := nil;
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then Exit;
    if(AMineNumber > 0) then
    begin
      LMine := YieldModelDataObject.CastNetworkFeaturesData.CastMineList.MineByNodeNumber[AMineNumber];
      if(LMine <> nil) then
      begin
        Result := LMine.CreateSlurryDump;
        if (Result <> nil) then
          StudyDataHasChanged(sdccAdd, 'Slurry', '0',IntToStr(Result.Identifier));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoDeleteSlurryDump(AMineNumber,ASlurryDumpIdentifier: integer): WordBool;
const OPNAME = 'TYieldModelManager.DoDeleteSlurryDump';
var
  LMine        : IMine;
begin
  Result := False;
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then Exit;
    if (AMineNumber > 0) and (ASlurryDumpIdentifier > 0) then
    begin
      LMine := YieldModelDataObject.CastNetworkFeaturesData.CastMineList.MineByNodeNumber[AMineNumber];
      if(LMine <> nil) then
      begin
        Result := LMine.RemoveSlurryDump(ASlurryDumpIdentifier);
        if Result then
          StudyDataHasChanged(sdccDelete, 'Slurry', '0',IntToStr(ASlurryDumpIdentifier));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.ViewInputChannelDialog(AChannelNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.ViewInputChannelDialog';
var
  LContextData : string;
begin
  Result := False;
  try
    if (YieldModelData.NetworkElementData.ChannelList.ChannelByChannelNumber[AChannelNumber] = nil) then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.ChannelNotExist')+IntToStr(AChannelNumber)+FAppModules.Language.GetString('Message.ScenarioNotExist'));
      Exit;
    end;

    LContextData := ViewModelDataContextDataCommaText(mdvnChannel,AChannelNumber);
    Result := FModelDataGUIManager.ViewInputPopupDialog(nil, LContextData, nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.ViewInputConfigurationDialog(const AViewName: WideString): WordBool;
const OPNAME = 'TYieldModelManager.ViewInputConfigurationDialog';
var
  LContextData : string;
begin
  Result := False;
  try
    if(AViewName = mdvnRunTitle)                   or (AViewName = mdvnMetaData) or
      (AViewName = mdvnRunConfiguration)           or (AViewName = mdvnOutputConfiguration) or
      (AViewName = mdvnMasterControlConfiguration) or (AViewName = mdvnCatchmentProportions) or
      (AViewName = mdvnChannelPenalties)           or (AViewName = mdvnReservoirPenalty) or
      (AViewName = mdvnChannelArea)                or (AViewName = mdvnReconciliationAnalysis) then
    begin
      LContextData := ViewModelDataContextDataCommaText(AViewName,NullInteger);
      Result := FModelDataGUIManager.ViewInputPopupDialog(nil, LContextData, nil);
    end
    else
    begin
      ShowMessage('View named ('+ AViewName + ') not yet implemented');
      Exit;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.ViewInputDemandCentreDialog(AYMDemandCentreNr: Integer): WordBool;
const OPNAME = 'TYieldModelManager.ViewInputDemandCentreDialog';
var
  LContextData : string;
begin
  Result := False;
  try
    if (YieldModelData.NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByNodeNumber[AYMDemandCentreNr] = nil) then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.DemandCentreNotExist')+IntToStr(AYMDemandCentreNr)+FAppModules.Language.GetString('Message.ScenarioNotExist'));
      Exit;
    end;

    LContextData := ViewModelDataContextDataCommaText(mdvnYMDemandCentre,AYMDemandCentreNr);
    Result := FModelDataGUIManager.ViewInputPopupDialog(nil, LContextData, nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.ViewInputIrrigationAreaDialog(AIrrigationAreaNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.ViewInputIrrigationAreaDialog';
var
  LContextData : string;
begin
  Result := False;
  try
    if (YieldModelData.NetworkFeaturesData.IrrigationAreaList.IrrigationAreaByNodeNumber[AIrrigationAreaNumber] = nil) then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.IrrigationAreaNotExist')+IntToStr(AIrrigationAreaNumber)+FAppModules.Language.GetString('Message.ScenarioNotExist'));
      Exit;
    end;

    LContextData := ViewModelDataContextDataCommaText(mdvnIrrigationArea,AIrrigationAreaNumber);
    Result := FModelDataGUIManager.ViewInputPopupDialog(nil, LContextData, nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.ViewInputIrrigationBlockDialog(AIrrigationBlockNodeNr: Integer): WordBool;
const OPNAME = 'TYieldModelManager.ViewInputIrrigationBlockDialog';
var
  LContextData : string;
begin
  Result := False;
  try
    if (YieldModelData.NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[AIrrigationBlockNodeNr] = nil) then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.IrrigationBlockNotExist')+IntToStr(AIrrigationBlockNodeNr)+FAppModules.Language.GetString('Message.ScenarioNotExist'));
      Exit;
    end;

    LContextData := ViewModelDataContextDataCommaText(mdvnIrrigationBlock,AIrrigationBlockNodeNr);
    Result := FModelDataGUIManager.ViewInputPopupDialog(nil, LContextData, nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.ViewInputMasterControlChannelDialog(AChannelNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.ViewInputMasterControlChannelDialog';
var
  LContextData : string;
begin
  Result := False;
  try
    if (YieldModelData.NetworkElementData.ChannelList.ChannelByChannelNumber[AChannelNumber] = nil) then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.ChannelNotExist')+IntToStr(AChannelNumber)+FAppModules.Language.GetString('Message.ScenarioNotExist'));
      Exit;
    end;

    LContextData := ViewModelDataContextDataCommaText(mdvnChannel,AChannelNumber);
    Result := FModelDataGUIManager.ViewInputPopupDialog(nil, LContextData, nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.ViewInputMineDialog(AMineNodeNr: Integer): WordBool;
const OPNAME = 'TYieldModelManager.ViewInputMineDialog';
var
  LContextData : string;
begin
  Result := False;
  try
    if (YieldModelData.NetworkFeaturesData.MineList.MineByNodeNumber[AMineNodeNr] = nil) then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.MineNotExist')+IntToStr(AMineNodeNr)+FAppModules.Language.GetString('Message.ScenarioNotExist'));
      Exit;
    end;

    LContextData := ViewModelDataContextDataCommaText(mdvnMine,AMineNodeNr);
    Result := FModelDataGUIManager.ViewInputPopupDialog(nil, LContextData, nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.ViewInputMinePCDDamDialog(APCDDamNr: Integer): WordBool;
const OPNAME = 'TYieldModelManager.ViewInputMinePCDDamDialog';
var
  LContextData : string;
begin
  Result := False;
  try
    if (YieldModelData.NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[APCDDamNr] = nil) then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.ReservoirNotExist')+IntToStr(APCDDamNr)+FAppModules.Language.GetString('Message.ScenarioNotExist'));
      Exit;
    end;

    LContextData := ViewModelDataContextDataCommaText(mdvnPCDDam,APCDDamNr);
    Result := FModelDataGUIManager.ViewInputPopupDialog(nil, LContextData, nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.ViewInputMineUndergroundDamDialog(AUndergroundDamNr: Integer): WordBool;
const OPNAME = 'TYieldModelManager.ViewInputMineUndergroundDamDialog';
var
  LContextData : string;
begin
  Result := False;
  try
    if (YieldModelData.NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[AUndergroundDamNr] = nil) then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.ReservoirNotExist')+IntToStr(AUndergroundDamNr)+FAppModules.Language.GetString('Message.ScenarioNotExist'));
      Exit;
    end;

    LContextData := ViewModelDataContextDataCommaText(mdvnUndegroundDam,AUndergroundDamNr);
    Result := FModelDataGUIManager.ViewInputPopupDialog(nil, LContextData, nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.ViewInputNodeWithInflowDialog(ANodeNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.ViewInputNodeWithInflowDialog';
var
  LContextData : string;
begin
  Result := False;
  try
    if (YieldModelData.NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[ANodeNumber] = nil) then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.NodeNotExist')+IntToStr(ANodeNumber)+FAppModules.Language.GetString('Message.ScenarioNotExist'));
      Exit;
    end;

    LContextData := ViewModelDataContextDataCommaText(mdvnNodesWithInflow,ANodeNumber);
    Result := FModelDataGUIManager.ViewInputPopupDialog(nil, LContextData, nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.ViewInputNodeWithoutInflowDialog(ANodeNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.ViewInputNodeWithoutInflowDialog';
var
  LContextData : string;
begin
  Result := False;
  try
    if (YieldModelData.NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[ANodeNumber] = nil) then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.ReservoirNotExist')+IntToStr(ANodeNumber)+FAppModules.Language.GetString('Message.ScenarioNotExist'));
      Exit;
    end;

    LContextData := ViewModelDataContextDataCommaText(mdvnNodesWithoutInFlow,ANodeNumber);
    Result := FModelDataGUIManager.ViewInputPopupDialog(nil, LContextData, nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.ViewInputPowerPlantDialog(APowerPlantNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.ViewInputPowerPlantDialog';
var
  LContextData : string;
begin
  Result := False;
  try
    if (YieldModelData.NetworkFeaturesData.PowerPlantList.PowerPlantByID[APowerPlantNumber] = nil) then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.PowerPlantNotExist')+IntToStr(APowerPlantNumber)+FAppModules.Language.GetString('Message.ScenarioNotExist'));
      Exit;
    end;

    LContextData := ViewModelDataContextDataCommaText(mdvnPowerPlant,APowerPlantNumber);
    Result := FModelDataGUIManager.ViewInputPopupDialog(nil, LContextData, nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.ViewInputReservoirDialog(AResevoirNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.ViewInputReservoirDialog';
var
  LContextData : string;
  LReservoir : IReservoirData;
begin
  Result := False;
  try
    LReservoir := YieldModelData.NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[AResevoirNumber];
    if (LReservoir = nil) then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.ReservoirNotExist')+IntToStr(AResevoirNumber)+FAppModules.Language.GetString('Message.ScenarioNotExist'));
    end
    else
    begin
      if(LReservoir.ReservoirConfigurationData.NodeType = ntMinePolutionControlDam) then
        LContextData := ViewModelDataContextDataCommaText(mdvnPCDDam,AResevoirNumber)
      else if(LReservoir.ReservoirConfigurationData.NodeType = ntMineUndergroundDam) then
        LContextData := ViewModelDataContextDataCommaText(mdvnUndegroundDam,AResevoirNumber)
      else
        LContextData := ViewModelDataContextDataCommaText(mdvnReservoir,AResevoirNumber);
      Result := FModelDataGUIManager.ViewInputPopupDialog(nil, LContextData, nil);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.ViewInputStreamFlowReductionDialog(AStreamFlowReductionNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.ViewInputStreamFlowReductionDialog';
var
  LContextData : string;
begin
  Result := False;
  try
    if (YieldModelData.NetworkFeaturesData.StreamFlowReductionList.StreamFlowReductionByID[AStreamFlowReductionNumber] = nil) then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.SFRACentreNotExist')+IntToStr(AStreamFlowReductionNumber)+FAppModules.Language.GetString('Message.ScenarioNotExist'));
      Exit;
    end;

    LContextData := ViewModelDataContextDataCommaText(mdvnStreamFlowReduction,AStreamFlowReductionNumber);
    Result := FModelDataGUIManager.ViewInputPopupDialog(nil, LContextData, nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.ViewInputWetlandDialog(AWetlandNr: Integer): WordBool;
const OPNAME = 'TYieldModelManager.ViewInputWetlandDialog';
var
  LContextData : string;
begin
  Result := False;
  try
    if (YieldModelData.NetworkFeaturesData.WetlandList.WetlandByNodeNumber[AWetlandNr] = nil) then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.WetlandNotExist')+IntToStr(AWetlandNr)+FAppModules.Language.GetString('Message.ScenarioNotExist'));
      Exit;
    end;

    LContextData := ViewModelDataContextDataCommaText(mdvnWetland,AWetlandNr);
    Result := FModelDataGUIManager.ViewInputPopupDialog(nil, LContextData, nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;


//__________________________________________________________________________________________________________________

function TYieldModelManager.ViewOutputChannelDialog(AChannelNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.ViewOutputChannelDialog';
var
  LContextData : string;
begin
  Result := False;
  try
    if (YieldModelData.NetworkElementData.ChannelList.ChannelByChannelNumber[AChannelNumber] = nil) then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.ChannelNotExist')+IntToStr(AChannelNumber)+FAppModules.Language.GetString('Message.ScenarioNotExist'));
      Exit;
    end;

    LContextData := ViewModelDataContextDataCommaText(mdvnChannel,AChannelNumber);
    Result := FModelDataGUIManager.ViewOutputPopupDialog(nil, LContextData, nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.ViewOutputDemandCentreDialog(AYMDemandCentreNr: Integer): WordBool;
const OPNAME = 'TYieldModelManager.ViewOutputDemandCentreDialog';
var
  LContextData : string;
begin
  Result := False;
  try
    if (YieldModelData.NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByNodeNumber[AYMDemandCentreNr] = nil) then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.DemandCentreNotExist')+IntToStr(AYMDemandCentreNr)+FAppModules.Language.GetString('Message.ScenarioNotExist'));
      Exit;
    end;

    LContextData := ViewModelDataContextDataCommaText(mdvnYMDemandCentre,AYMDemandCentreNr);
    Result := FModelDataGUIManager.ViewOutputPopupDialog(nil, LContextData, nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.ViewOutputIrrigationAreaDialog(AIrrigationAreaNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.ViewOutputIrrigationAreaDialog';
var
  LContextData : string;
begin
  Result := False;
  try
    if (YieldModelData.NetworkFeaturesData.IrrigationAreaList.IrrigationAreaByNodeNumber[AIrrigationAreaNumber] = nil) then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.IrrigationAreaNotExist')+IntToStr(AIrrigationAreaNumber)+FAppModules.Language.GetString('Message.ScenarioNotExist'));
      Exit;
    end;

    LContextData := ViewModelDataContextDataCommaText(mdvnIrrigationArea,AIrrigationAreaNumber);
    Result := FModelDataGUIManager.ViewOutputPopupDialog(nil, LContextData, nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.ViewOutputIrrigationBlockDialog(AIrrigationBlockNodeNr: Integer): WordBool;
const OPNAME = 'TYieldModelManager.ViewOutputIrrigationBlockDialog';
var
  LContextData : string;
begin
  Result := False;
  try
    if (YieldModelData.NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[AIrrigationBlockNodeNr] = nil) then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.IrrigationBlockNotExist')+IntToStr(AIrrigationBlockNodeNr)+FAppModules.Language.GetString('Message.ScenarioNotExist'));
      Exit;
    end;

    LContextData := ViewModelDataContextDataCommaText(mdvnIrrigationBlock,AIrrigationBlockNodeNr);
    Result := FModelDataGUIManager.ViewOutputPopupDialog(nil, LContextData, nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.ViewOutputMasterControlChannelDialog(AChannelNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.ViewOutputMasterControlChannelDialog';
var
  LContextData : string;
begin
  Result := False;
  try
    if (YieldModelData.NetworkElementData.ChannelList.ChannelByChannelNumber[AChannelNumber] = nil) then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.ChannelNotExist')+IntToStr(AChannelNumber)+FAppModules.Language.GetString('Message.ScenarioNotExist'));
      Exit;
    end;

    LContextData := ViewModelDataContextDataCommaText(mdvnChannel,AChannelNumber);
    Result := FModelDataGUIManager.ViewOutputPopupDialog(nil, LContextData, nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.ViewOutputMineDialog(AMineNodeNr: Integer): WordBool;
const OPNAME = 'TYieldModelManager.ViewOutputMineDialog';
var
  LContextData : string;
begin
  Result := False;
  try
    if (YieldModelData.NetworkFeaturesData.MineList.MineByNodeNumber[AMineNodeNr] = nil) then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.MineNotExist')+IntToStr(AMineNodeNr)+FAppModules.Language.GetString('Message.ScenarioNotExist'));
      Exit;
    end;

    LContextData := ViewModelDataContextDataCommaText(mdvnMine,AMineNodeNr);
    Result := FModelDataGUIManager.ViewOutputPopupDialog(nil, LContextData, nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.ViewOutputMinePCDDamDialog(APCDDamNr: Integer): WordBool;
const OPNAME = 'TYieldModelManager.ViewOutputMinePCDDamDialog';
var
  LContextData : string;
begin
  Result := False;
  try
    if (YieldModelData.NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[APCDDamNr] = nil) then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.ReservoirNotExist')+IntToStr(APCDDamNr)+FAppModules.Language.GetString('Message.ScenarioNotExist'));
      Exit;
    end;

    LContextData := ViewModelDataContextDataCommaText(mdvnPCDDam,APCDDamNr);
    Result := FModelDataGUIManager.ViewOutputPopupDialog(nil, LContextData, nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.ViewOutputMineUndergroundDamDialog(AUndergroundDamNr: Integer): WordBool;
const OPNAME = 'TYieldModelManager.ViewOutputMineUndergroundDamDialog';
var
  LContextData : string;
begin
  Result := False;
  try
    if (YieldModelData.NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[AUndergroundDamNr] = nil) then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.ReservoirNotExist')+IntToStr(AUndergroundDamNr)+FAppModules.Language.GetString('Message.ScenarioNotExist'));
      Exit;
    end;

    LContextData := ViewModelDataContextDataCommaText(mdvnUndegroundDam,AUndergroundDamNr);
    Result := FModelDataGUIManager.ViewOutputPopupDialog(nil, LContextData, nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.ViewOutputNodeWithInflowDialog(ANodeNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.ViewOutputNodeWithInflowDialog';
var
  LContextData : string;
begin
  Result := False;
  try
    if (YieldModelData.NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[ANodeNumber] = nil) then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.NodeNotExist')+IntToStr(ANodeNumber)+FAppModules.Language.GetString('Message.ScenarioNotExist'));
      Exit;
    end;

    LContextData := ViewModelDataContextDataCommaText(mdvnNodesWithInflow,ANodeNumber);
    Result := FModelDataGUIManager.ViewOutputPopupDialog(nil, LContextData, nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.ViewOutputNodeWithoutInflowDialog(ANodeNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.ViewOutputNodeWithoutInflowDialog';
var
  LContextData : string;
begin
  Result := False;
  try
    if (YieldModelData.NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[ANodeNumber] = nil) then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.ReservoirNotExist')+IntToStr(ANodeNumber)+FAppModules.Language.GetString('Message.ScenarioNotExist'));
      Exit;
    end;

    LContextData := ViewModelDataContextDataCommaText(mdvnNodesWithoutInFlow,ANodeNumber);
    Result := FModelDataGUIManager.ViewOutputPopupDialog(nil, LContextData, nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.ViewOutputPowerPlantDialog(APowerPlantNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.ViewOutputPowerPlantDialog';
var
  LContextData : string;
begin
  Result := False;
  try
    if (YieldModelData.NetworkFeaturesData.PowerPlantList.PowerPlantByID[APowerPlantNumber] = nil) then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.PowerPlantNotExist')+IntToStr(APowerPlantNumber)+FAppModules.Language.GetString('Message.ScenarioNotExist'));
      Exit;
    end;

    LContextData := ViewModelDataContextDataCommaText(mdvnPowerPlant,APowerPlantNumber);
    Result := FModelDataGUIManager.ViewOutputPopupDialog(nil, LContextData, nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.ViewOutputReservoirDialog(AResevoirNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.ViewOutputReservoirDialog';
var
  LContextData : string;
begin
  Result := False;
  try
    if (YieldModelData.NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[AResevoirNumber] = nil) then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.ReservoirNotExist')+IntToStr(AResevoirNumber)+FAppModules.Language.GetString('Message.ScenarioNotExist'));
      Exit;
    end;

    LContextData := ViewModelDataContextDataCommaText(mdvnReservoir,AResevoirNumber);
    Result := FModelDataGUIManager.ViewOutputPopupDialog(nil, LContextData, nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.ViewOutputStreamFlowReductionDialog(AStreamFlowReductionNumber: Integer): WordBool;
const OPNAME = 'TYieldModelManager.ViewOutputStreamFlowReductionDialog';
var
  LContextData : string;
begin
  Result := False;
  try
    if (YieldModelData.NetworkFeaturesData.StreamFlowReductionList.StreamFlowReductionByID[AStreamFlowReductionNumber] = nil) then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.SFRACentreNotExist')+IntToStr(AStreamFlowReductionNumber)+FAppModules.Language.GetString('Message.ScenarioNotExist'));
      Exit;
    end;

    LContextData := ViewModelDataContextDataCommaText(mdvnStreamFlowReduction,AStreamFlowReductionNumber);
    Result := FModelDataGUIManager.ViewOutputPopupDialog(nil, LContextData, nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.ViewOutputWetlandDialog(AWetlandNr: Integer): WordBool;
const OPNAME = 'TYieldModelManager.ViewOutputWetlandDialog';
var
  LContextData : string;
begin
  Result := False;
  try
    if (YieldModelData.NetworkFeaturesData.WetlandList.WetlandByNodeNumber[AWetlandNr] = nil) then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.WetlandNotExist')+IntToStr(AWetlandNr)+FAppModules.Language.GetString('Message.ScenarioNotExist'));
      Exit;
    end;

    LContextData := ViewModelDataContextDataCommaText(mdvnWetland,AWetlandNr);
    Result := FModelDataGUIManager.ViewOutputPopupDialog(nil, LContextData, nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.Get_WRYMRunOptions: IWRYMRunOptions;
const OPNAME = 'TYieldModelManager.Get_WRYMRunOptions';
begin
  Result := nil;
  try
    Result := FWRYMRunOptions;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.Get_YieldModelIterationTracker: IYieldModelIterationTracker;
const OPNAME = 'TYieldModelManager.Get_YieldModelIterationTracker';
begin
  Result := nil;
  try
    Result := FYieldModelIterationTracker;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCopyIFRFeature(AChannelNumber: Integer): IIFRFeature;
const OPNAME = 'TYieldModelManager.DoCopyIFRFeature';
var
  LNode          : TTreeNode;
  LDataObject    : TViewDataTreeNodeData;
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  LChannelCopy   : IGeneralFlowChannel;
begin
  Result := nil;
  try
    if(AChannelNumber > 0) then
    begin
      lChannelList := YieldModelDataObject.
                      NetworkElementData.ChannelList;
      lChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
      if (lChannel <> nil) then
      begin
        if LChannel.IFRFeature <> nil then
        begin
          LChannelCopy := YieldModelDataObject.NetworkElementData.ChannelList.CopyChannel(LChannel.ChannelNumber);
          if (LChannelCopy <> nil) then
          begin
            Result := LChannelCopy.IFRFeature;
          end;
        end;
      end;
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          AChannelNumber := LDataObject.ViewDataNode.Weighting;
          lChannelList := YieldModelDataObject.
                            NetworkElementData.ChannelList;
          lChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
          if (lChannel <> nil) then
          begin
            if LChannel.IFRFeature <> nil then
            begin
              LChannelCopy := YieldModelDataObject.NetworkElementData.ChannelList.CopyChannel(LChannel.ChannelNumber);
              if (LChannelCopy <> nil) then
              begin
                Result := LChannelCopy.IFRFeature;
              end;
              CreateTabSheetTreeViewElement('ChannelDetails18',
                         'ChannelHeading,ChannelDetails18',
                         LChannelCopy.ChannelNumber,tvnidChannelDetails18,
                         LChannelCopy.ChannelName,
                         'IFRCHANNEL',
                         'CHANNEL',True,tvsnAll);
            end;
          end;
        end;
      end;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.CopyPhysicalFlowConstraint(AChannelNumber: Integer): IPhysicalFlowConstraint;
const OPNAME = 'TYieldModelManager.CopyPhysicalFlowConstraint';
var
  LNode          : TTreeNode;
  LDataObject    : TViewDataTreeNodeData;
  lChannelList   : IChannelList;
  LChannel       : IGeneralFlowChannel;
  LChannelCopy   : IGeneralFlowChannel;
begin
  Result := nil;
  try
    if AChannelNumber > 0 then
    begin
     LChannelList := YieldModelDataObject.
                      NetworkElementData.ChannelList;
      LChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
      if (LChannel <> nil) then
      begin
        LChannelCopy := YieldModelDataObject.NetworkElementData.ChannelList.CopyChannel(AChannelNumber);
        if (LChannelCopy <> nil) then
        begin
          Result := LChannelCopy.PhysicalFlowConstraint;
        end;
      end;
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          AChannelNumber := LDataObject.ViewDataNode.Weighting;
          LChannelList := YieldModelDataObject.
                            NetworkElementData.ChannelList;
          LChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
          if (LChannel <> nil) then
          begin
            LChannelCopy := YieldModelDataObject.NetworkElementData.ChannelList.CopyChannel(AChannelNumber);
            if (LChannelCopy <> nil) then
            begin
              Result := LChannelCopy.PhysicalFlowConstraint;
              CreateTabSheetTreeViewElement('ChannelDetails19',
                                     'ChannelHeading,ChannelDetails19',
                                     lChannel.ChannelNumber,tvnidChannelDetails19,
                                     lChannel.ChannelName,
                                     'PFCCHANNEL',
                                     'CHANNEL',True,tvsnAll);

            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCopyChannel(AChannelNumber: Integer): IGeneralFlowChannel;
const OPNAME = 'TYieldModelManager.DoCopyChannel';
var
  LNode          : TTreeNode;
  LDataObject    : TViewDataTreeNodeData;
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  LChannelCopy   : IGeneralFlowChannel;
begin
  Result := nil;
  try
    if(AChannelNumber > 0) then
    begin
      lChannelList := YieldModelDataObject.
                      NetworkElementData.ChannelList;
      lChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
      if (lChannel <> nil) then
      begin
        case LChannel.ChannelType of
        2 : DoCopyMasterControlFeature(-1);
        12 :
          begin
            if LChannel <> nil then
            begin
              LChannelCopy := YieldModelDataObject.NetworkElementData.ChannelList.CopyChannel(LChannel.ChannelNumber);
              if (LChannelCopy <> nil) then
              begin
                Result := LChannelCopy;
              end;
            end;
          end;

        end;
      end;
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          AChannelNumber := LDataObject.ViewDataNode.Weighting;
          lChannelList := YieldModelDataObject.
                            NetworkElementData.ChannelList;
          lChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
          if (LChannel <> nil) then
          begin
            case LChannel.ChannelType of
            2 : DoCopyMasterControlFeature(-1);
            //3 : DoCopyPowerPlant(-1);
            5 : DoCopyDiversionFeature(-1);
            6 : DoCopyMinimumFlowFeature(-1);
            7 : DoCopyLossFeature(-1);
            8 : DoCopyMinMaxFlowFeature(-1);
            9 : DoCopyPumpingFeature(-1);
            10: DoCopySpecifiedInflowFeature(-1);
            11: DoCopySpecifiedDemandFeature(-1);
            12:
              begin
                LChannelCopy := YieldModelDataObject.NetworkElementData.ChannelList.CopyChannel(LChannel.ChannelNumber);
                if (LChannelCopy <> nil) then
                begin
                  Result := LChannelCopy;
                  CreateTabSheetTreeViewElement('ChannelDetails12',
                                 'ChannelHeading,ChannelDetails12',
                                 Result.ChannelNumber,tvnidChannelDetails12,
                                 Result.ChannelName,
                                 'GENERALCHANNEL',
                                 'CHANNEL',
                                 True,tvsnAll);
                end;
              end;

            end;

          end;
        end;
      end;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCopyDiversionFeature(AChannelNumber: Integer): IDiversionFeature;
const OPNAME = 'TYieldModelManager.DoCopyDiversionFeature';
var
  LNode          : TTreeNode;
  LDataObject    : TViewDataTreeNodeData;
  lChannelList   : IChannelList;
  LChannel       : IGeneralFlowChannel;
  LChannelCopy   : IGeneralFlowChannel;
begin
  Result := nil;
  try
    if AChannelNumber > 0 then
    begin
     LChannelList := YieldModelDataObject.
                      NetworkElementData.ChannelList;
      LChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
      if (LChannel <> nil) then
      begin
        LChannelCopy := YieldModelDataObject.NetworkElementData.ChannelList.CopyChannel(AChannelNumber);
        if (LChannelCopy <> nil) then
        begin
          Result := LChannelCopy.DiversionFeature;
        end;
      end;
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          AChannelNumber := LDataObject.ViewDataNode.Weighting;
          LChannelList := YieldModelDataObject.
                            NetworkElementData.ChannelList;
          LChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
          if (LChannel <> nil) then
          begin
            LChannelCopy := YieldModelDataObject.NetworkElementData.ChannelList.CopyChannel(AChannelNumber);
            if (LChannelCopy <> nil) then
            begin
              Result := LChannelCopy.DiversionFeature;
              CreateTabSheetTreeViewElement('ChannelDetails5',
                                         'ChannelHeading,ChannelDetails5',
                                         LChannelCopy.ChannelNumber,tvnidChannelDetails5,
                                         LChannelCopy.ChannelName,
                                         'DIVERSIONCHANNEL',
                                         'CHANNEL',True,tvsnAll);

            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TYieldModelManager.DoCopyLossFeature(AChannelNumber: Integer): ILossFeature;
const OPNAME = 'TYieldModelManager.DoCopyLossFeature';
var
  LNode          : TTreeNode;
  LDataObject    : TViewDataTreeNodeData;
  lChannelList   : IChannelList;
  LChannel       : IGeneralFlowChannel;
  LChannelCopy   : IGeneralFlowChannel;
begin
  Result := nil;
  try
    if AChannelNumber > 0 then
    begin
     LChannelList := YieldModelDataObject.
                      NetworkElementData.ChannelList;
      LChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
      if (LChannel <> nil) then
      begin
        LChannelCopy := YieldModelDataObject.NetworkElementData.ChannelList.CopyChannel(AChannelNumber);
        if (LChannelCopy <> nil) then
        begin
          Result := LChannelCopy.LossFeature;
        end;
      end;
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          AChannelNumber := LDataObject.ViewDataNode.Weighting;
          LChannelList := YieldModelDataObject.
                            NetworkElementData.ChannelList;
          LChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
          if (LChannel <> nil) then
          begin
            LChannelCopy := YieldModelDataObject.NetworkElementData.ChannelList.CopyChannel(AChannelNumber);
            if (LChannelCopy <> nil) then
            begin
              Result := LChannelCopy.LossFeature;
              CreateTabSheetTreeViewElement('ChannelDetails7',
                                         'ChannelHeading,ChannelDetails7',
                                          LChannelCopy.ChannelNumber,tvnidChannelDetails7,
                                          LChannelCopy.ChannelName,
                                          'LOSSCHANNEL',
                                          'CHANNEL',
                                           True,tvsnAll);

            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TYieldModelManager.DoCopyMasterControlFeature(AChannelNumber: Integer): IMasterControlFeature;
const OPNAME = 'TYieldModelManager.DoCopyMasterControlFeature';
var
  LNode          : TTreeNode;
  LDataObject    : TViewDataTreeNodeData;
  lChannelList   : IChannelList;
  LChannel       : IGeneralFlowChannel;
  LChannelCopy   : IGeneralFlowChannel;
begin
  Result := nil;
  try
    if AChannelNumber > 0 then
    begin
     LChannelList := YieldModelDataObject.
                      NetworkElementData.ChannelList;
      LChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
      if (LChannel <> nil) then
      begin
        LChannelCopy := YieldModelDataObject.NetworkElementData.ChannelList.CopyChannel(AChannelNumber);
        if (LChannelCopy <> nil) then
        begin
          Result := LChannelCopy.MasterControlFeature;
        end;
      end;
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          AChannelNumber := LDataObject.ViewDataNode.Weighting;
          LChannelList := YieldModelDataObject.
                            NetworkElementData.ChannelList;
          LChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
          if (LChannel <> nil) then
          begin
            LChannelCopy := YieldModelDataObject.NetworkElementData.ChannelList.CopyChannel(AChannelNumber);
            if (LChannelCopy <> nil) then
            begin
              Result := LChannelCopy.MasterControlFeature;
              CreateTabSheetTreeViewElement('MasterControlConfiguration',
                         'ParametersHeading,MasterControlConfiguration',
                         LChannelCopy.ChannelNumber,tvnidMasterControlConfiguration,
                         LChannelCopy.ChannelName,
                         '',
                         'CHANNEL',True,tvsnAll);

            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCopyMinimumFlowFeature(AChannelNumber: Integer): IMinimumFlowConstraint;
const OPNAME = 'TYieldModelManager.DoCopyMinimumFlowFeature';
var
  LNode          : TTreeNode;
  LDataObject    : TViewDataTreeNodeData;
  lChannelList   : IChannelList;
  LChannel       : IGeneralFlowChannel;
  LChannelCopy   : IGeneralFlowChannel;
begin
  Result := nil;
  try
    if AChannelNumber > 0 then
    begin
     LChannelList := YieldModelDataObject.
                      NetworkElementData.ChannelList;
      LChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
      if (LChannel <> nil) then
      begin
        LChannelCopy := YieldModelDataObject.NetworkElementData.ChannelList.CopyChannel(AChannelNumber);
        if (LChannelCopy <> nil) then
        begin
          Result := LChannelCopy.MinimumFlowConstraint;
        end;
      end;
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          AChannelNumber := LDataObject.ViewDataNode.Weighting;
          LChannelList := YieldModelDataObject.
                            NetworkElementData.ChannelList;
          LChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
          if (LChannel <> nil) then
          begin
            LChannelCopy := YieldModelDataObject.NetworkElementData.ChannelList.CopyChannel(AChannelNumber);
            if (LChannelCopy <> nil) then
            begin
              Result := LChannelCopy.MinimumFlowConstraint;
              CreateTabSheetTreeViewElement('ChannelDetails6',
                                     'ChannelHeading,ChannelDetails6',
                                     LChannelCopy.ChannelNumber,tvnidChannelDetails6,
                                     LChannelCopy.ChannelName,
                                     'MINFLOWCHANNEL',
                                     'CHANNEL',
                                      True,tvsnAll);
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TYieldModelManager.DoCopyMinMaxFlowFeature(AChannelNumber: Integer): IMinMaxFlowConstraint;
const OPNAME = 'TYieldModelManager.DoCopyMinMaxFlowFeature';
var
  LNode          : TTreeNode;
  LDataObject    : TViewDataTreeNodeData;
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  LChannelCopy   : IGeneralFlowChannel;
begin
  Result := nil;
  try
    if(AChannelNumber > 0) then
    begin
      lChannelList := YieldModelDataObject.
                      NetworkElementData.ChannelList;
      lChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
      if (lChannel <> nil) then
      begin
        if LChannel.MinMaxFlowConstraint <> nil then
        begin
          LChannelCopy := YieldModelDataObject.NetworkElementData.ChannelList.CopyChannel(LChannel.ChannelNumber);
          if (LChannelCopy <> nil) then
          begin
            Result := LChannelCopy.MinMaxFlowConstraint;
          end;
        end;
      end;
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          AChannelNumber := LDataObject.ViewDataNode.Weighting;
          lChannelList := YieldModelDataObject.
                            NetworkElementData.ChannelList;
          lChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
          if (lChannel <> nil) then
          begin
            if LChannel.MinMaxFlowConstraint <> nil then
            begin
              LChannelCopy := YieldModelDataObject.NetworkElementData.ChannelList.CopyChannel(LChannel.ChannelNumber);
              if (LChannelCopy <> nil) then
              begin
                Result := LChannelCopy.MinMaxFlowConstraint;
              end;
              CreateTabSheetTreeViewElement('ChannelDetails8',
                                     'ChannelHeading,ChannelDetails8',
                                     LChannelCopy.ChannelNumber,tvnidChannelDetails8,
                                     LChannelCopy.ChannelName,
                                     'MINMAXCHANNEL',
                                     'CHANNEL',
                                      True,tvsnAll);
            end;
          end;
        end;
      end;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCopyPumpingFeature(AChannelNumber: Integer): IPumpingFeature;
const OPNAME = 'TYieldModelManager.DoCopyPumpingFeature';
var
  LNode          : TTreeNode;
  LDataObject    : TViewDataTreeNodeData;
  lChannelList   : IChannelList;
  LChannel       : IGeneralFlowChannel;
  LChannelCopy   : IGeneralFlowChannel;
begin
  Result := nil;
  try
    if(AChannelNumber > 0) then
    begin
      lChannelList := YieldModelDataObject.
                      NetworkElementData.ChannelList;
      lChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
      if (lChannel <> nil) then
      begin
        if LChannel.PumpingFeature <> nil then
        begin
          LChannelCopy := YieldModelDataObject.NetworkElementData.ChannelList.CopyChannel(LChannel.ChannelNumber);
          if (LChannelCopy <> nil) then
          begin
            Result := LChannelCopy.PumpingFeature;
          end;
        end;
      end;
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          AChannelNumber := LDataObject.ViewDataNode.Weighting;
          lChannelList := YieldModelDataObject.
                            NetworkElementData.ChannelList;
          LChannel     := LChannelList.ChannelByChannelNumber[AChannelNumber];
          if (LChannel <> nil) then
          begin
            if LChannel.PumpingFeature <> nil then
            begin
              LChannelCopy := YieldModelDataObject.NetworkElementData.ChannelList.CopyChannel(LChannel.ChannelNumber);
              if (LChannelCopy <> nil) then
              begin
                Result := LChannelCopy.PumpingFeature;
              end;
              CreateTabSheetTreeViewElement('ChannelDetails9',
                                     'ChannelHeading,ChannelDetails9',
                                     LChannelCopy.ChannelNumber,tvnidChannelDetails9,
                                     LChannelCopy.ChannelName,
                                     'PUMPINGCHANNEL',
                                     'CHANNEL',
                                      True,tvsnAll);
            end;
          end;
        end;
      end;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCopySpecifiedDemandFeature(AChannelNumber: Integer): ISpecifiedDemandFeature;
const OPNAME = 'TYieldModelManager.DoCopySpecifiedDemandFeature';
var
  LNode          : TTreeNode;
  LDataObject    : TViewDataTreeNodeData;
  LChannelList   : IChannelList;
  LChannel       : IGeneralFlowChannel;
  LChannelCopy   : IGeneralFlowChannel;
begin
  Result := nil;
  try
    if(AChannelNumber > 0) then
    begin
      LChannelList := YieldModelDataObject.
                      NetworkElementData.ChannelList;
      LChannel     := LChannelList.ChannelByChannelNumber[AChannelNumber];
      if (lChannel <> nil) then
      begin
        if LChannel.SpecifiedDemandFeature <> nil then
        begin
          LChannelCopy := YieldModelDataObject.NetworkElementData.ChannelList.CopyChannel(LChannel.ChannelNumber);
          if (LChannelCopy <> nil) then
          begin
            Result := LChannelCopy.SpecifiedDemandFeature;
          end;
        end;
      end;
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          AChannelNumber := LDataObject.ViewDataNode.Weighting;
          lChannelList := YieldModelDataObject.
                            NetworkElementData.ChannelList;
          LChannel     := LChannelList.ChannelByChannelNumber[AChannelNumber];
          if (LChannel <> nil) then
          begin
            if LChannel.SpecifiedDemandFeature <> nil then
            begin
              LChannelCopy := YieldModelDataObject.NetworkElementData.ChannelList.CopyChannel(LChannel.ChannelNumber);
              if (LChannelCopy <> nil) then
              begin
                Result := LChannelCopy.SpecifiedDemandFeature;
              end;
              CreateTabSheetTreeViewElement('ChannelDetails11',
                                     'ChannelHeading,ChannelDetails11',
                                     LChannelCopy.ChannelNumber,tvnidChannelDetails11,
                                     LChannelCopy.ChannelName,
                                     'DEMANDCHANNEL',
                                     'CHANNEL',True,tvsnAll);
            end;
          end;
        end;
      end;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCopySpecifiedInflowFeature(AChannelNumber: Integer): ISpecifiedInflowFeature;
const OPNAME = 'TYieldModelManager.DoCopySpecifiedInflowFeature';
var
  LNode          : TTreeNode;
  LDataObject    : TViewDataTreeNodeData;
  lChannelList   : IChannelList;
  LChannel       : IGeneralFlowChannel;
  LChannelCopy   : IGeneralFlowChannel;
begin
  Result := nil;
  try
    if(AChannelNumber > 0) then
    begin
      lChannelList := YieldModelDataObject.
                      NetworkElementData.ChannelList;
      lChannel     := lChannelList.ChannelByChannelNumber[AChannelNumber];
      if (lChannel <> nil) then
      begin
        if LChannel.SpecifiedInflowFeature <> nil then
        begin
          LChannelCopy := YieldModelDataObject.NetworkElementData.ChannelList.CopyChannel(LChannel.ChannelNumber);
          if (LChannelCopy <> nil) then
          begin
            Result := LChannelCopy.SpecifiedInflowFeature;
          end;
        end;
      end;
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          AChannelNumber := LDataObject.ViewDataNode.Weighting;
          lChannelList := YieldModelDataObject.
                            NetworkElementData.ChannelList;
          LChannel     := LChannelList.ChannelByChannelNumber[AChannelNumber];
          if (LChannel <> nil) then
          begin
            if LChannel.SpecifiedInflowFeature <> nil then
            begin
              LChannelCopy := YieldModelDataObject.NetworkElementData.ChannelList.CopyChannel(LChannel.ChannelNumber);
              if (LChannelCopy <> nil) then
              begin
                Result := LChannelCopy.SpecifiedInflowFeature;
              end;
              CreateTabSheetTreeViewElement('ChannelDetails10',
                                     'ChannelHeading,ChannelDetails10',
                                     LChannelCopy.ChannelNumber,tvnidChannelDetails10,
                                     LChannelCopy.ChannelName,
                                     'INFLOWCHANNEL',
                                     'CHANNEL',True,tvsnAll);
            end;
          end;
        end;
      end;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCopyWaterDemandFeature(AChannelNumber: Integer): IWaterDemandFeature;
const OPNAME = 'TYieldModelManager.DoCopyWaterDemandFeature';
begin

end;

function TYieldModelManager.DoCopyPowerPlant(AChannelNumber: Integer): IPowerPlant;
const OPNAME = 'TYieldModelManager.DoCopyPowerPlant';
var
  LPowerPlant : IPowerPlant;
  LPowerPlantCopy : IPowerPlant;
  LFeatureID : integer;
  LNode          : TTreeNode;
  LDataObject    : TViewDataTreeNodeData;
begin
  Result := nil;
  try
    if AChannelNumber > 0 then
    begin
      LPowerPlant := YieldModelDataObject.NetworkFeaturesData.PowerPlantList.PowerPlantByID[AChannelNumber];
      LPowerPlantCopy := YieldModelDataObject.NetworkFeaturesData.PowerPlantList.CreatePowerPlant;
      Result := YieldModelDataObject.NetworkFeaturesData.PowerPlantList.CopyPowerPlant(LPowerPlantCopy.FeatureID,AChannelNumber);
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) and Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) and Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          LFeatureID := LDataObject.ViewDataNode.Weighting;
          LPowerPlant := YieldModelDataObject.NetworkFeaturesData.PowerPlantList.PowerPlantByID[LFeatureID];
          LPowerPlantCopy := YieldModelDataObject.NetworkFeaturesData.PowerPlantList.CreatePowerPlant;
          Result := YieldModelDataObject.NetworkFeaturesData.PowerPlantList.CopyPowerPlant(LPowerPlantCopy.FeatureID,LFeatureID);
          CreateTabSheetTreeViewElement('PowerPlants',
                                   'FeaturesHeading,PowerPlants',
                                   Result.FeatureID,tvnidPowerPlants,
                                   Result.FeatureName,
                                   'POWERCHANNEL',
                                   'POWERPLANT',True,tvsnAll);
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TYieldModelManager.DoCopyIrrigationArea(AFeatureID: Integer): IIrrigationArea;
const OPNAME = 'TYieldModelManager.DoCopyIrrigationArea';
var
  LFeatureID : integer;
  LNode : TTreeNode;
  LDataObject : TViewDataTreeNodeData;
  LReservoirNode : IReservoirData;
begin
  Result := nil;
  try
    if AFeatureID > 0 then
    begin
      Result := YieldModelDataObject.NetworkFeaturesData.IrrigationAreaList.CopyCreate(AFeatureID)
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) and Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) and Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          LFeatureID := LDataObject.ViewDataNode.Weighting;
          Result := YieldModelDataObject.NetworkFeaturesData.IrrigationAreaList.CopyCreate(LFeatureID);
        end;
        if Result <> nil then
        begin
          LReservoirNode := YieldModelDataObject.NetworkElementData.ReservoirList.IrrigationNodeByIdentifier
                            [Result.ConsumptiveChannel.UpStreamNodeNumber];
          if (LReservoirNode <> nil) then
            CreateTabSheetTreeViewElement('NodesWithoutInFlow',
                                     'NodesHeading,NodesWithoutInFlow',
                                     LReservoirNode.ReservoirConfigurationData.ReservoirIdentifier,tvnidNodesWithoutInFlow,
                                     LReservoirNode.ReservoirConfigurationData.ReservoirName,
                                     '',
                                     'NODEWITHOUTINFLOW', FALSE,tvsnAll);

          CreateTabSheetTreeViewElement('IrrigationAreas',
                                   'FeaturesHeading,IrrigationAreas',
                                   Result.IrrigationNodeNumber,tvnidIrrigationAreas,
                                   Result.FeatureName,
                                   'IRRIGATIONCHANNEL',
                                   'IRRIGATIONAREA',True,tvsnAll);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCopyIrrigationBlock(AFeatureID: Integer): IIrrigationBlock;
const OPNAME = 'TYieldModelManager.DoCopyIrrigationBlock';
var
  LFeatureID : integer;
  LNode : TTreeNode;
  LDataObject : TViewDataTreeNodeData;
begin
  Result := nil;
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then Exit;

    if AFeatureID > 0 then
    begin
      Result := YieldModelDataObject.CastNetworkFeaturesData.CastIrrigationBlockList.CopyCreate(AFeatureID);
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) and Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) and Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          LFeatureID := LDataObject.ViewDataNode.Weighting;
          Result := YieldModelDataObject.NetworkFeaturesData.IrrigationBlockList.CopyCreate(LFeatureID);
        end;
        if (Result <> nil) then
        begin
          CreateTabSheetTreeViewElement('IrrigationBlock',
                                     'FeaturesHeading,IrrigationBlock',
                                     (Result as IIrrigationBlock).BlockNodeNumber,tvnidIrrigationBlock,
                                     (Result as IIrrigationBlock).BlockName,
                                     'IRRIGATIONBLOCK',
                                     'IRRIGATIONBLOCK',True,tvsnAll);
          StudyDataHasChanged(sdccAdd, 'IrrigationBlockName', '0',IntToStr((Result as IIrrigationBlock).BlockNodeNumber));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCopyWetland(AWetlandID : integer):IWetland;safecall;
const OPNAME = 'TYieldModelManager.DoCopyWetland';
var
  LWetlandID : integer;
  LNode : TTreeNode;
  LDataObject : TViewDataTreeNodeData;
begin
  Result := nil;
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then Exit;
    if AWetlandID > 0 then
      Result := YieldModelDataObject.CastNetworkFeaturesData.CastWetlandList.CopyCreate(AWetlandID)
    else
    begin
      if (Assigned(FDataTabSheetManager) and Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) and Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          LWetlandID := LDataObject.ViewDataNode.Weighting;
          Result := YieldModelDataObject.NetworkFeaturesData.WetlandList.CopyCreate(LWetlandID);
        end;
        if (Result <> nil) then
        begin
          CreateTabSheetTreeViewElement('Wetland',
                                     'FeaturesHeading,Wetland',
                                     (Result as IWetland).NodeNumber, tvnidWetland,
                                     (Result as IWetland).Name,
                                     'WETLAND',
                                     'WETLAND',True,tvsnAll);
          StudyDataHasChanged(sdccAdd, 'WetlandName', '0',IntToStr((Result as IWetland).Identifier));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCopySFRSubCatchment(AStreamFlowReductionID: Integer): IStreamFlowReduction; safecall;
const OPNAME = 'TYieldModelManager.DoCopySFRSubCatchment';
var
  LStreamFlowReductionID : integer;
  LNode : TTreeNode;
  LDataObject : TViewDataTreeNodeData;
begin
  Result := nil;
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then Exit;
    if AStreamFlowReductionID > 0 then
      Result := YieldModelDataObject.CastNetworkFeaturesData.CastStreamFlowReductionList.CopyCreate(AStreamFlowReductionID)
    else
    begin
      if (Assigned(FDataTabSheetManager) and Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) and Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          LStreamFlowReductionID := LDataObject.ViewDataNode.Weighting;
          Result := YieldModelDataObject.CastNetworkFeaturesData.CastStreamFlowReductionList.CopyCreate(LStreamFlowReductionID);
        end;
        if (Result <> nil) then
        begin
          CreateTabSheetTreeViewElement('StreamFlowReduction',
                                     'FeaturesHeading,StreamFlowReduction',
                                      Result.Identifier,tvnidStreamFlowReduction,
                                      Result.SFRName,
                                     '',
                                     'STREAMFLOWREDUCTION',True,tvsnAll);
          StudyDataHasChanged(sdccAdd, 'StreamFlowReduction', '0',IntToStr((Result as IStreamFlowReduction).Identifier));
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TYieldModelManager.DoCopyYMDemandCentre(ANodeNumber: Integer): IYMDemandCentre;
const OPNAME = 'TYieldModelManager.DoCopyYMDemandCentre';
var
  LNodeNumber : integer;
  LNode : TTreeNode;
  LDataObject : TViewDataTreeNodeData;
begin
  Result := nil;
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then Exit;
    if ANodeNumber > 0 then
      Result := YieldModelDataObject.CastNetworkFeaturesData.CastYMDemandCentreList.CopyCreate(ANodeNumber)
    else
    begin
      if (Assigned(FDataTabSheetManager) and Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) and Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          LNodeNumber := LDataObject.ViewDataNode.Weighting;
          Result := YieldModelDataObject.CastNetworkFeaturesData.CastYMDemandCentreList.CopyCreate(LNodeNumber);
        end;
        if (Result <> nil) then
        begin
          if(Result.SupplyChannelByIndex[0] <> nil) then
            CreateTabSheetTreeViewElement('ChannelDetails12',
                                       'ChannelHeading,ChannelDetails12',
                                       Result.SupplyChannelByIndex[0].ChannelNumber,tvnidChannelDetails12,
                                       Result.SupplyChannelByIndex[0].ChannelName,
                                       'GENERALCHANNEL',
                                       'CHANNEL',
                                       True,tvsnAll);
          CreateTabSheetTreeViewElement('YMDemandCentre',
                                     'FeaturesHeading,YMDemandCentre',
                                     (Result as IYMDemandCentre).NodeNumber,tvnidYMDemandCentre,
                                     (Result as IYMDemandCentre).Name,
                                     'YMDEMANDCENTRE',
                                     'YMDEMANDCENTRE',True,tvsnAll);
          StudyDataHasChanged(sdccAdd, 'YMDemandCentreName', '0',IntToStr((Result as IYMDemandCentre).Identifier));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCopyMine(AMineNumber: integer): IMine;
const OPNAME = 'TYieldModelManager.DoCopyMine';
var
  LMineNumber : integer;
  LNode : TTreeNode;
  LDataObject : TViewDataTreeNodeData;
begin
  Result := nil;
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then Exit;
    if AMineNumber > 0 then
      Result := YieldModelDataObject.CastNetworkFeaturesData.CastMineList.CopyCreate(AMineNumber)
    else
    begin
      if (Assigned(FDataTabSheetManager) and Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) and Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          LMineNumber := LDataObject.ViewDataNode.Weighting;
          Result := YieldModelDataObject.CastNetworkFeaturesData.CastMineList.CopyCreate(LMineNumber);
        end;
      end;
      if (Result <> nil) then
      begin

        CreateTabSheetTreeViewElement('Mine',
                                   'FeaturesHeading,Mine',
                                   (Result as IMine).NodeNumber,tvnidMine,
                                   (Result as IMine).MineName,
                                   'Mine',
                                   'Mine',True,tvsnAll);
        AddMineNodes(Result);
        StudyDataHasChanged(sdccAdd, 'Mine', '0',IntToStr((Result as IMine).NodeNumber));
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TYieldModelManager.DoCopyReservoir(AReservoirNumber: Integer): IReservoirData;
const OPNAME = 'TYieldModelManager.DoCopyReservoir';
var
  LReservoirNr   : integer;
  LNode          : TTreeNode;
  LDataObject    : TViewDataTreeNodeData;
begin
  Result := nil;
  try
    if AReservoirNumber > 0 then
    begin
      Result := YieldModelDataObject.NetworkElementData.ReservoirList.CopyCreate(AReservoirNumber)
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) and Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) and Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          LReservoirNr := LDataObject.ViewDataNode.Weighting;
          Result := YieldModelDataObject.NetworkElementData.ReservoirList.CopyCreate(LReservoirNr);
        end;
        if Result <> nil then
        begin
          CreateTabSheetTreeViewElement('AReservoir',
                               'AReservoir',
                               Result.ReservoirConfigurationData.ReservoirIdentifier, tvnidAReservoir,
                               Result.ReservoirConfigurationData.ReservoirName,
                               'RESERVOIR',
                               'RESERVOIR',
                               True,tvsnAll);
          StudyDataHasChanged(sdccAdd, 'ReservoirName', '0',IntToStr(Result.ReservoirConfigurationData.ReservoirIdentifier));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCopyReservoirFromScenario: WordBool;
const OPNAME = 'TYieldModelManager.DoCopyReservoirFromScenario';
var
  LCopyManager : TCopyScenarioDataManager;
begin
  Result := False;
  try
    LCopyManager := TCopyScenarioDataManager.Create(FAppModules);
    try
      Result := LCopyManager.CopyReservoirFromScenario;
      if Result then
        RefreshModelData
    finally
      LCopyManager.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCopyChannelFromScenario: WordBool;
const OPNAME = 'TYieldModelManager.DoCopyChannelFromScenario';
var
  LCopyManager : TCopyScenarioDataManager;
begin
  Result := False;
  try
    LCopyManager := TCopyScenarioDataManager.Create(FAppModules);
    try
      Result := LCopyManager.CopyChannelFromScenario;
      if Result then
        RefreshModelData
    finally
      LCopyManager.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCopyIrrigationAreaFromScenario: WordBool;
const OPNAME = 'TYieldModelManager.DoCopyIrrigationAreaFromScenario';
var
  LCopyManager : TCopyScenarioDataManager;
begin
  Result := False;
  try
    LCopyManager := TCopyScenarioDataManager.Create(FAppModules);
    try
      Result := LCopyManager.CopyIrrigationAreaFromScenario;
      if Result then
        RefreshModelData
    finally
      LCopyManager.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCopyPowerPlantFromScenario: WordBool;
const OPNAME = 'TYieldModelManager.DoCopyPowerPlantFromScenario';
var
  LCopyManager : TCopyScenarioDataManager;
begin
  Result := False;
  try
    LCopyManager := TCopyScenarioDataManager.Create(FAppModules);
    try
      Result := LCopyManager.CopyPowerPlantFromScenario;
      if Result then
        RefreshModelData
    finally
      LCopyManager.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCopyIrrigationBlockFromScenario: WordBool;
const OPNAME = 'TYieldModelManager.DoCopyIrrigationBlockFromScenario';
var
  LCopyManager : TCopyScenarioDataManager;
begin
  Result := False;
  try
    LCopyManager := TCopyScenarioDataManager.Create(FAppModules);
    try
      Result := LCopyManager.CopyIrrigationBlockFromScenario;
      if Result then
        RefreshModelData
    finally
      LCopyManager.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCopyWetlandFromScenario:WordBool;
const OPNAME = 'TYieldModelManager.DoCopyWetlandFromScenario';
var
  LCopyManager : TCopyScenarioDataManager;
begin
  Result := False;
  try
    LCopyManager := TCopyScenarioDataManager.Create(FAppModules);
    try
      Result := LCopyManager.CopyWetlandFromScenario;
      if Result then
        RefreshModelData
    finally
      LCopyManager.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCopyYMDemandCentreFromScenario:WordBool;
const OPNAME = 'TYieldModelManager.DoCopyYMDemandCentreFromScenario';
var
  LCopyManager : TCopyScenarioDataManager;
begin
  Result := False;
  try
    LCopyManager := TCopyScenarioDataManager.Create(FAppModules);
    try
      Result := LCopyManager.CopyYMDemandCentreFromScenario;
      if Result then
        RefreshModelData
    finally
      LCopyManager.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCopySFRFromScenario:WordBool;
const OPNAME = 'TYieldModelManager.DoCopySFRFromScenario';
var
  LCopyManager : TCopyScenarioDataManager;
begin
  Result := False;
  try
    LCopyManager := TCopyScenarioDataManager.Create(FAppModules);
    try
      Result := LCopyManager.CopySFRFromScenario;
      if Result then
        RefreshModelData
    finally
      LCopyManager.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCopyMineFromScenario:WordBool;
const OPNAME = 'TYieldModelManager.DoCopyMineFromScenario';
var
  LCopyManager : TCopyScenarioDataManager;
begin
  Result := False;
  try
    LCopyManager := TCopyScenarioDataManager.Create(FAppModules);
    try
      Result := LCopyManager.CopyMineFromScenario;
      if Result then
        RefreshModelData
    finally
      LCopyManager.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCopyGroundWaterFromScenario: WordBool;
const OPNAME = 'TYieldModelManager.DoCopyGroundWaterFromScenario';
var
  LCopyManager : TCopyScenarioDataManager;
begin
  Result := False;
  try
    LCopyManager := TCopyScenarioDataManager.Create(FAppModules);
    try
      Result := LCopyManager.CopyGroundWaterFromScenario;
      if Result then
        RefreshModelData
    finally
      LCopyManager.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.ReadFirmYieldFromDebugFile: Double;
const OPNAME = 'TYieldModelManager.ReadFirmYieldFromDebugFile';
begin
  Result := NullFloat;
  try
    Result := TFilesActionYieldManager(FFileSelectionManager).ReadFirmYieldFromDebugFile;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoRunStorageVsYield(AReservoirNumber: Integer; const AStartingStorageCommaText: WideString;
         var AMinTargetDraftCommaText: WideString; var AMaxTargetDraftCommaText: WideString;
         var AYieldCommaText: WideString): WordBool; safecall;
const OPNAME = 'TYieldModelManager.DoRunStorageVsYield';
begin
  Result := False;
  try
    Result := TFilesActionYieldManager(FModelFilesActionManager).DoRunStorageVsYield(AReservoirNumber,
              AStartingStorageCommaText, AMinTargetDraftCommaText, AMaxTargetDraftCommaText, AYieldCommaText)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.AddGroundWater(AGroundWater: IGroundWater): boolean;
const OPNAME = 'TYieldModelManager.AddGroundWater';
var
  LReservoirNumber    : integer;
  LReservoir          : IReservoirData;
  LReservoirName      : string;
begin
  Result := FALSE;
  try
    if(AGroundWater = nil) then
      Exit;

    if((FDataTabSheetManager <> nil) and (FDataTabSheetManager.GridEditorManager <> nil)) or
      ((FOutputReviewManager <> nil) and (FOutputReviewManager.TabSheet <> nil)) then
    begin
      LReservoir := AGroundWater.AquiferNode;
      if(LReservoir <> nil) then
      begin
        //Add  AquiferNode
        LReservoirNumber    := LReservoir.ReservoirConfigurationData.ReservoirIdentifier;
        LReservoirName      := LReservoir.ReservoirConfigurationData.ReservoirName;
        CreateTabSheetTreeViewElement('AquiferNode',
                                   'FeaturesHeading,GroundWater,GroundWater',
                                   LReservoirNumber,
                                   AGroundWater.Identifier,
                                   LReservoirName,
                                   'RESERVOIR',
                                   'AQUIFERNODE',
                                   False,tvsnAll);
       end;

      //Add  BaseFlowNode
      LReservoir := AGroundWater.BaseFlowNode;
      if(LReservoir <> nil) then
      begin
        LReservoirNumber    := LReservoir.ReservoirConfigurationData.ReservoirIdentifier;
        LReservoirName      := LReservoir.ReservoirConfigurationData.ReservoirName;
        CreateTabSheetTreeViewElement('BaseFlowNode',
                                   'FeaturesHeading,GroundWater,GroundWater',
                                    LReservoirNumber,
                                    AGroundWater.Identifier,
                                    LReservoirName,
                                   'RESERVOIR',
                                   'BASEFLOWNODE',
                                    False,tvsnAll);
      end;

      //Add AbstractionNode
      LReservoir := AGroundWater.AbstractionNode;
      if(LReservoir <> nil) then
      begin
        LReservoirNumber    := LReservoir.ReservoirConfigurationData.ReservoirIdentifier;
        LReservoirName      := LReservoir.ReservoirConfigurationData.ReservoirName;
        CreateTabSheetTreeViewElement('AbstractionNode',
                                   'FeaturesHeading,GroundWater,GroundWater',
                                    LReservoirNumber,
                                    AGroundWater.Identifier,
                                    LReservoirName,
                                   'RESERVOIR',
                                   'ABSTRACTIONNODE',
                                    False,tvsnAll);
      end;

      //Add CollectionNode
      LReservoir := AGroundWater.CollectionNode;
      if(LReservoir <> nil) then
      begin
        LReservoirNumber    := LReservoir.ReservoirConfigurationData.ReservoirIdentifier;
        LReservoirName      := LReservoir.ReservoirConfigurationData.ReservoirName;
        CreateTabSheetTreeViewElement('CollectionNode',
                                   'FeaturesHeading,GroundWater,GroundWater',
                                    LReservoirNumber,
                                    AGroundWater.Identifier,
                                    LReservoirName,
                                   'RESERVOIR',
                                   'COLLECTIONNODE',
                                    False,tvsnAll);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCreateGroundWater: IGroundWater;
const OPNAME = 'TYieldModelManager.DoCreateGroundWater';
begin
  Result := nil;
  try
    if (FAppModules.StudyArea.ModelVersion <> '7') or
       (not FModelData.ImplementedNetworkFeatures.GroundWaterFeatureImplemented) then Exit;
    Result := YieldModelDataObject.CastNetworkFeaturesData.CastGroundWaterList.CreateGroundWater;
    if (Result <> nil) then
    begin
      CreateTabSheetTreeViewElement('GroundWater',
                                 'FeaturesHeading,GroundWater',
                                 (Result as IGroundWater).Identifier,tvnidGroundWater,
                                 (Result as IGroundWater).Name,
                                 'GroundWater',
                                 'GroundWater',True,tvsnAll);
      AddGroundWater(Result);
      StudyDataHasChanged(sdccAdd, 'GroundWater', '0',IntToStr((Result as IGroundWater).Identifier));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoDeleteGroundWater(AGroundWaterID: Integer): WordBool;
const OPNAME = 'TYieldModelManager.DoDeleteGroundWater';
var
  lNode        : TTreeNode;
  lDataObject  : TViewDataTreeNodeData;
begin
  Result  := FALSE;
  try
    if (FAppModules.StudyArea.ModelVersion <> '7') or
       (not FModelData.ImplementedNetworkFeatures.GroundWaterFeatureImplemented) then Exit;
    if (AGroundWaterID > 0) then
    begin
      Result := YieldModelDataObject.CastNetworkFeaturesData.CastGroundWaterList.RemoveGroundWater(AGroundWaterID);
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject     := TViewDataTreeNodeData((LNode.Data));
          AGroundWaterID := LDataObject.ViewDataNode.Weighting;
          Result := YieldModelDataObject.CastNetworkFeaturesData.CastGroundWaterList.RemoveGroundWater(AGroundWaterID);
          if (Result) then
            DeleteTabSheetTreeNode(lNode);
          SetSystemMenuState;
        end;
      end;
    end;
    if Result then
      StudyDataHasChanged(sdccDelete, 'GroundWater', '0',IntToStr(AGroundWaterID));

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelManager.DoCopyGroundWater(AGroundWaterNumber: integer): IGroundWater;
const OPNAME = 'TYieldModelManager.DoCopyGroundWater';
var
  LGroundWaterNumber : integer;
  LNode              : TTreeNode;
  LDataObject        : TViewDataTreeNodeData;
begin
  Result := nil;
  try
    if (FAppModules.StudyArea.ModelVersion <> '7') or
       (not FModelData.ImplementedNetworkFeatures.GroundWaterFeatureImplemented) then Exit;

    if (AGroundWaterNumber > 0) then
      Result := YieldModelDataObject.CastNetworkFeaturesData.CastGroundWaterList.CopyGroundwater(AGroundWaterNumber)
    else
    begin
      if (Assigned(FDataTabSheetManager) and Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) and Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          LGroundWaterNumber := LDataObject.ViewDataNode.Weighting;
          Result := YieldModelDataObject.CastNetworkFeaturesData.CastGroundWaterList.CopyGroundwater(LGroundWaterNumber);
        end;
      end;
      if (Result <> nil) then
      begin

        CreateTabSheetTreeViewElement('GroundWater',
                                   'FeaturesHeading,GroundWater',
                                   (Result as IGroundWater).Identifier,tvnidGroundWater,
                                   (Result as IGroundWater).Name,
                                   'GroundWater',
                                   'GroundWater',True,tvsnAll);
        StudyDataHasChanged(sdccAdd, 'GroundWater', '0',IntToStr((Result as IGroundWater).Identifier));
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TYieldModelManager.ViewInputGroundwaterDialog(AAquiferNodeNr: Integer): WordBool;
const OPNAME = 'TYieldModelManager.ViewInputGroundwaterDialog';
var
  LContextData : string;
begin
  Result := False;
  try
    if (YieldModelData.NetworkFeaturesData.GroundWaterList.GroundWaterByNodeNumber[AAquiferNodeNr] = nil) then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.GroundwaterNotExist')+IntToStr(AAquiferNodeNr)+FAppModules.Language.GetString('Message.ScenarioNotExist'));
      Exit;
    end;

    LContextData := ViewModelDataContextDataCommaText(mdvnGroundWater,YieldModelData.NetworkFeaturesData.GroundWaterList.GroundWaterByNodeNumber[AAquiferNodeNr].Identifier);
    Result := FModelDataGUIManager.ViewInputPopupDialog(nil, LContextData, nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.ViewOutputGroundwaterDialog(AAquiferNodeNr: Integer): WordBool;
const OPNAME = 'TYieldModelManager.ViewOutputGroundwaterDialog';
var
  LContextData : string;
begin
  Result := False;
  try
    if (YieldModelData.NetworkFeaturesData.GroundWaterList.GroundWaterByNodeNumber[AAquiferNodeNr] = nil) then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.GroundwaterNotExist')+IntToStr(AAquiferNodeNr)+FAppModules.Language.GetString('Message.ScenarioNotExist'));
      Exit;
    end;

    LContextData := ViewModelDataContextDataCommaText(mdvnGroundWater,AAquiferNodeNr);
    Result := FModelDataGUIManager.ViewOutputPopupDialog(nil, LContextData, nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelManager.StudyPropertiesCommaText: WideString;
const OPNAME = 'TYieldModelManager.StudyPropertiesCommaText';
var
  LData : TStringList;
begin
  Result := '';
  try
    LData := TStringList.Create;
    try
      LData.Add('Model');
      LData.Add('StudyAreaName');
      LData.Add('SubArea');
      LData.Add('Scenario');
      LData.Values['Model'] := FAppModules.StudyArea.ModelCode;
      LData.Values['StudyAreaName'] := FAppModules.StudyArea.StudyAreaCode;
      LData.Values['SubArea'] := FAppModules.StudyArea.SubAreaCode;
      LData.Values['Scenario'] := FAppModules.StudyArea.ScenarioCode;
      Result := LData.CommaText;
    finally
      LData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldModelManager.ShowSplashScreen;
const OPNAME = 'TYieldModelManager.ShowSplashScreen';
var
  LIndex             : integer;
  LShowAgain         : string;
  LIssueForm         : TYieldModelDataGUIForm;
  LOldCursor         : TCursor;
  LDialogValidator   : TCautionarySplashScreenValidator;
  LCount             : integer;
  LStudyMetaDataList : TStudyMetaDataList;
  LStudyMetaData     : TStudyMetaData;
begin
  try
    LShowAgain := FAppModules.ViewIni.ReadString(ClassName,'ShowScreenAgain','');
    if(LShowAgain <> '') then
    begin
      if not StrToBool(LShowAgain) then
      begin
        LStudyMetaDataList := TYieldModelDataObject(FAppModules.Model.ModelData).StudyMetaDataList;
        if(LStudyMetaDataList.StudyMetaDataCount > 0) then
        begin
          LOldCursor       := Screen.Cursor;
          Screen.Cursor    := crHourGlass;
          LIssueForm       := TYieldModelDataGUIForm.CreateWithoutDFM(nil,FAppModules);
          LDialogValidator := TCautionarySplashScreenValidator.Create(LIssueForm,FAppModules);
          try
            LIssueForm.Initialise;
            LIssueForm.StudyHasChanged;
            LIssueForm.LanguageHasChanged;
            LIssueForm.AddModelDataDialogValidator(LDialogValidator);

            LDialogValidator.LanguageHasChanged;
            LDialogValidator.CautionarySplashScreenDialog.CheckBoxPanel.Visible := True;
            LIssueForm.BtnOk.Caption     := FAppModules.Language.GetString('TSCChartLegendDialog.BtnOkCaption');
            LIssueForm.BtnCancel.Caption := FAppModules.Language.GetString('TSCChartLegendDialog.BtnCancelCaption');

            LCount := 0;
            LDialogValidator.CautionarySplashScreenDialog.IssueStringGrid.RowCount := LDialogValidator.CautionarySplashScreenDialog.IssueStringGrid.RowCount +
                                                                                      LStudyMetaDataList.StudyMetaDataCount;
            for LIndex := 0 to LStudyMetaDataList.StudyMetaDataCount - 1 do
            begin
              LCount := LCount + 1;
              LStudyMetaData := LStudyMetaDataList.CastStudyMetaDataByIndex[LIndex];
              LDialogValidator.CautionarySplashScreenDialog.IssueStringGrid.Cells[0,LCount]    := LStudyMetaData.StudyAreaName;
              LDialogValidator.CautionarySplashScreenDialog.IssueStringGrid.Cells[1,LCount]    := LStudyMetaData.ImportedBy;
              LDialogValidator.CautionarySplashScreenDialog.IssueStringGrid.Cells[2,LCount]    := LStudyMetaData.ErrorType;
              LDialogValidator.CautionarySplashScreenDialog.IssueStringGrid.Cells[3,LCount]    := LStudyMetaData.ErrorDescription;
              LDialogValidator.CautionarySplashScreenDialog.IssueStringGrid.Cells[4,LCount]    := LStudyMetaData.CorrectiveAction;
              LDialogValidator.CautionarySplashScreenDialog.IssueStringGrid.RowHeights[LCount] := 70;
            end;
            LDialogValidator.CautionarySplashScreenDialog.IssueStringGrid.FixedRows := 1;

            Screen.Cursor := LOldCursor;
            LIssueForm.ShowModal;
          finally
            FreeAndNil(LIssueForm);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

