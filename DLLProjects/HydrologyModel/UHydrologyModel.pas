//
//  Contains THydrologyModel Class, the manager for the Hydrology model
//
unit UHydrologyModel;

interface

uses
  VCL.Forms, Classes,
  UHostDlg,
  XMLIntf,

  UXMLAgent,
  UAbstractObject,
  USystemModelManager,
  UMenuItemManager,
  UDataTabSheetManager,
  UVisioTabSheetManager,
  UNetwork,
  HydrologyCom_TLB,

  UReservoirPropertiesDlg,
  UReservoirVolumeAreaDlg,
  UPanDataDlg,
  UInflowRoutesDlg,
  UOutflowRoutesDlg,
  URunOffPropertiesDlg,
  URunOffSlavesDlg,
  URunOffPitmanDlg,
  URunOffAfforestationDlg,
  URunOffAlienVegetationDlg,
  URunOffPavedAreaDlg,
  URunOffGroundWaterAbstractionDlg,
  URunOffOutflowRoutesDlg,
  URunOffHughesDlg,
  URunOffSamiDlg,
  UChannelPropertiesDlg,
  UIrrigationPropertiesDlg,
  UIrrigationWQTDlg,
  UIrrigationFactorsDlg,
  UIrrigationAreaDlg,
  UIrrigationAllocationGrowthDlg,
  UIrrigationReturnFlowDlg,
  UIrrigationEfficiencyDlg,
  UIrrigationCropsDlg,

  UMinePropertiesDlg,
  UMinePlantAreaDlg,
  UMineSectionsDlg,
  UMineOpencastPitPropertiesDlg,
  UMineOpencastPitWorkingsDlg,
  UMineOpencastPitDisturbedDlg,
  UMineOpencastPitRehabilitatedDlg,
  UMineOpencastPitEvaporationDlg,
  UMineOpencastPitDecantDlg,
  UMineOpencastPitSeepageDlg,
  UMineOpencastPitRechargeFactorsDlg,
  UMineOpencastPitWorkingsQSLDDlg,
  UMineOpencastPitSeepDecantQSLDDlg,
  UMineUndergroundPropertiesDlg,
  UMineUndergroundBoardPillarDlg,
  UMineUndergroundHighExtractionDlg,
  UMineUndergroundRechargeFactorsDlg,
  UMineUndergroundQSLDDlg,
  UMineSlurryDumpPropertiesDlg,
  UMineSlurryDumpAreaDlg,
  UMineSlurryDumpRechargeFactorsDlg,
  UMineSlurryDumpQSLDDlg,

  UNetworkRouteDlg,
  UObservationPointDlg,
  UHydroTimeSeriesDlg,
  UNetworkSequenceDlg,

  UHydroOutputDlg,
  UHydroDBManager,
  UGenericModelLinkClasses;

type
  TTreeViewSheetName   = (tvsnAll,tvsnData,tvsnOutput);
  THydrologyModel = class(TSystemModelManager,IHydrologyModel)
  protected
    FEmptyStudy                 : boolean;
    FModelMenuItemManager       : TMenuItemManager;
    FHydroNVManager             : TVisioTabSheetManager;
    FDataTabSheetManager        : TDataTabSheetManager;
    FHydroDBManager             : THydroDBManager;
    FNetwork                    : TNetwork;
    FMayChangeNetwork           : Boolean;
    FLoadingData                : boolean;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Get_Network: INetwork; safecall;
    function Get_MayChangeNetwork: WordBool; safecall;
    procedure Set_MayChangeNetwork(Value: WordBool); safecall;
    function ShowObservationPointDialog(ARouteNo: Integer): WordBool; safecall;
    function ShowNetworkRouteDialog(ARouteNo: Integer): WordBool; safecall;
    function ShowReservoirModuleDialog(AModuleNo: Integer): WordBool; safecall;
    function ShowRunOffModuleDialog(AModuleNo: Integer): WordBool; safecall;
    function ShowChannelModuleDialog(AModuleNo: Integer): WordBool; safecall;
    function ShowIrrigationModuleDialog(AModuleNo: Integer): WordBool; safecall;
    function ShowMineModuleDialog(AModuleNo: Integer): WordBool; safecall;
    function ShowMineSectionsDialog(AModuleNo: Integer): WordBool; safecall;
    function ShowMineOpencastPitDialog (AModuleNo  : Integer;
                                        ASectionNo : Integer): WordBool; safecall;
    function ShowMineUndergroundSectionDialog (AModuleNo  : Integer;
                                               ASectionNo : Integer): WordBool; safecall;
    function ShowMineSlurryDumpDialog (AModuleNo  : Integer;
                                       ASectionNo : Integer): WordBool; safecall;
    function ShowNetworkSequenceDialog(AModuleID: Integer): WordBool; safecall;
    function ShowHydroOutputDialog (const AElementType    : WideString;
                                    const AElementSubType : WideString;
                                    AElementNo            : Integer;
                                    ASubElementID         : Integer;
                                    AResultTypeID         : Integer): WordBool; safecall;
    function RefreshOutputDlg(const AXMLOut: WideString): WordBool; safecall;
    procedure FreeOutputDlg; safecall;
    function UpdateNetworkData(const AXMLOut: WideString): WordBool; safecall;

    property Network: INetwork read Get_Network;
    property MayChangeNetwork: WordBool read Get_MayChangeNetwork write Set_MayChangeNetwork;

    procedure CreateHydroNVManager; virtual;
    procedure CreateModelTabSheetManagers; virtual;
    procedure CreateModelMenuItemManager; virtual;
    procedure CreateDataTabSheetManager; virtual;
    procedure SetSystemMenuState; virtual;

    function ShowNetworkVisualiser: boolean; virtual;

    function ShowGridEditor: boolean; virtual;
    function EditGridEditor(AData: TObject): boolean; virtual;
    function ShowGraph: boolean; virtual;

    function LoadModelData: boolean; virtual;
    function ProcessCustomModelEvent(AData: TModelMenuData): boolean; override;
    function RefreshModelData: boolean; virtual;

    function AddObservationPointDialog(AHostDialog : THostDlg; AObsPoint : IObservationPoint) : Boolean;
    function AddNetworkRouteDialog(AHostDialog : THostDlg; ARoute : INetworkRoute) : Boolean;
    function AddReservoirPropertiesDialog(AHostDialog : THostDlg; AModule: INetworkModule): Boolean;
    function AddReservoirVolumeAreaDialog(AHostDialog : THostDlg; AModule: INetworkModule): Boolean;
    function AddPanDataDialog(AHostDialog : THostDlg; AModule: INetworkModule): Boolean;
    function AddInflowRoutesDialog(AHostDialog : THostDlg; AModule: INetworkModule): Boolean;
    function AddOutflowRoutesDialog(AHostDialog : THostDlg; AModule: INetworkModule): Boolean;
    function AddChannelPropertiesDialog(AHostDialog : THostDlg; AModule: INetworkModule): Boolean;
    function AddRunOffPropertiesDialog(AHostDialog : THostDlg; AModule: INetworkModule): Boolean;
    function AddRunOffSlavesDialog(AHostDialog : THostDlg; AModule: INetworkModule): Boolean;
    function AddRunOffPitmanDialog(AHostDialog : THostDlg; AModule: INetworkModule): Boolean;
    function AddRunOffHughesDialog(AHostDialog : THostDlg; AModule: INetworkModule): Boolean;
    function AddRunOffSamiDialog(AHostDialog : THostDlg; AModule: INetworkModule): Boolean;
    function AddRunOffAfforestationDialog(AHostDialog : THostDlg; AModule: INetworkModule): Boolean;
    function AddRunOffAlienVegetationDialog(AHostDialog : THostDlg; AModule: INetworkModule): Boolean;
    function AddRunOffPavedAreaDialog(AHostDialog : THostDlg; AModule: INetworkModule): Boolean;
    function AddRunOffGroundWaterAbstractionDialog(AHostDialog : THostDlg; AModule: INetworkModule): Boolean;
    function AddRunOffOutflowRoutesDialog(AHostDialog : THostDlg; AModule: INetworkModule): Boolean;
    function AddIrrigationPropertiesDialog(AHostDialog : THostDlg; AModule: INetworkModule): Boolean;
    function AddIrrigationWQTDialog(AHostDialog : THostDlg; AModule: INetworkModule): Boolean;
    function AddIrrigationFactorsDialog(AHostDialog : THostDlg; AModule: INetworkModule): Boolean;
    function AddIrrigationAreaDialog(AHostDialog : THostDlg; AModule: INetworkModule): Boolean;
    function AddIrrigationAllocationGrowthDialog(AHostDialog : THostDlg; AModule: INetworkModule): Boolean;
    function AddIrrigationReturnFlowDialog(AHostDialog : THostDlg; AModule: INetworkModule): Boolean;
    function AddIrrigationEfficiencyDialog(AHostDialog : THostDlg; AModule: INetworkModule): Boolean;
    function AddIrrigationCropsDialog(AHostDialog : THostDlg; AModule: INetworkModule): Boolean;
    function AddMinePropertiesDialog(AHostDialog : THostDlg; AModule: INetworkModule): Boolean;
    function AddMinePlantAreaDialog(AHostDialog : THostDlg; AModule: INetworkModule): Boolean;
    function AddMineOpencastPitPropertiesDialog(AHostDialog : THostDlg; AModule : INetworkModule; ASectionNo : Integer): Boolean;
    function AddMineOpencastPitWorkingsDialog(AHostDialog : THostDlg; AModule : INetworkModule; ASectionNo : Integer): Boolean;
    function AddMineOpencastPitDisturbedDialog(AHostDialog : THostDlg; AModule : INetworkModule; ASectionNo : Integer): Boolean;
    function AddMineOpencastPitRehabilitatedDialog(AHostDialog : THostDlg; AModule : INetworkModule; ASectionNo : Integer): Boolean;
    function AddMineOpencastPitEvaporationDialog(AHostDialog : THostDlg; AModule : INetworkModule; ASectionNo : Integer): Boolean;
    function AddMineOpencastPitDecantDialog(AHostDialog : THostDlg; AModule : INetworkModule; ASectionNo : Integer): Boolean;
    function AddMineOpencastPitSeepageDialog(AHostDialog : THostDlg; AModule : INetworkModule; ASectionNo : Integer): Boolean;
    function AddMineOpencastPitRechargeFactorsDialog(AHostDialog : THostDlg; AModule : INetworkModule; ASectionNo : Integer): Boolean;
    function AddMineOpencastPitWorkingsQSLDDialog(AHostDialog : THostDlg; AModule : INetworkModule; ASectionNo : Integer): Boolean;
    function AddMineOpencastPitSeepDecantQSLDDialog(AHostDialog : THostDlg; AModule : INetworkModule; ASectionNo : Integer): Boolean;
    function AddMineUndergroundPropertiesDialog(AHostDialog : THostDlg; AModule : INetworkModule; ASectionNo : Integer): Boolean;
    function AddMineUndergroundBoardPillarDialog(AHostDialog : THostDlg; AModule : INetworkModule; ASectionNo : Integer): Boolean;
    function AddMineUndergroundHighExtractionDialog(AHostDialog : THostDlg; AModule : INetworkModule; ASectionNo : Integer): Boolean;
    function AddMineUndergroundRechargeFactorsDialog(AHostDialog : THostDlg; AModule : INetworkModule; ASectionNo : Integer): Boolean;
    function AddMineUndergroundQSLDDialog(AHostDialog : THostDlg; AModule : INetworkModule; ASectionNo : Integer): Boolean;
    function AddMineSlurryDumpPropertiesDialog(AHostDialog : THostDlg; AModule : INetworkModule; ASectionNo : Integer): Boolean;
    function AddMineSlurryDumpAreaDialog(AHostDialog : THostDlg; AModule : INetworkModule; ASectionNo : Integer): Boolean;
    function AddMineSlurryDumpRechargeFactorsDialog(AHostDialog : THostDlg; AModule : INetworkModule; ASectionNo : Integer): Boolean;
    function AddMineSlurryDumpQSLDDialog(AHostDialog : THostDlg; AModule : INetworkModule; ASectionNo : Integer): Boolean;
    function AddHydroTimeSeriesDialog (AHostDialog : THostDlg; ATimeSeries : ITimeSeries; ATitle : String): Boolean;
  private
    FHostWidthReservoir            : Integer;
    FHostHeightReservoir           : Integer;
    FHostWidthChannel              : Integer;
    FHostHeightChannel             : Integer;
    FHostWidthRunOff               : Integer;
    FHostHeightRunOff              : Integer;
    FHostWidthIrrigation           : Integer;
    FHostHeightIrrigation          : Integer;
    FHostWidthMine                 : Integer;
    FHostHeightMine                : Integer;
    FHostWidthNetworkRoute         : Integer;
    FHostHeightNetworkRoute        : Integer;
    FHostWidthMineOpencastPit      : Integer;
    FHostHeightMineOpencastPit     : Integer;
    FHostWidthMineUnderground      : Integer;
    FHostHeightMineUnderground     : Integer;
    FHostWidthMineSlurryDump       : Integer;
    FHostHeightMineSlurryDump      : Integer;
    FHostWidthObservationPoint     : Integer;
    FHostHeightObservationPoint    : Integer;
    FHostWidthHydroOutput          : Integer;
    FHostHeightHydroOutput         : Integer;

    FReservoirPropertiesDlg        : TReservoirPropertiesDlg;
    FReservoirVolumeAreaDlg        : TReservoirVolumeAreaDlg;
    FPanDataDlg                    : TPanDataDlg;
    FInflowRoutesDlg               : TInflowRoutesDlg;
    FOutflowRoutesDlg              : TOutflowRoutesDlg;
    FNetworkRouteDlg               : TNetworkRouteDlg;
    FObservationPointDlg           : TObservationPointDlg;
    FHydroTimeSeriesDlg            : THydroTimeSeriesDlg;

    FRunOffPropertiesDlg             : TRunOffPropertiesDlg;
    FRunOffSlavesDlg                 : TRunOffSlavesDlg;
    FRunOffPitmanDlg                 : TRunOffPitmanDlg;
    FRunOffAfforestationDlg          : TRunOffAfforestationDlg;
    FRunOffAlienVegetationDlg        : TRunOffAlienVegetationDlg;
    FRunOffPavedAreaDlg              : TRunOffPavedAreaDlg;
    FRunOffGroundWaterAbstractionDlg : TRunOffGroundWaterAbstractionDlg;
    FRunOffOutflowRoutesDlg          : TRunOffOutflowRoutesDlg;
    FRunOffHughesDlg                 : TRunOffHughesDlg;
    FRunOffSamiDlg                   : TRunOffSamiDlg;

    FChannelPropertiesDlg            : TChannelPropertiesDlg;

    FIrrigationPropertiesDlg         : TIrrigationPropertiesDlg;
    FIrrigationWQTDlg                : TIrrigationWQTDlg;
    FIrrigationFactorsDlg            : TIrrigationFactorsDlg;
    FIrrigationAreaDlg               : TIrrigationAreaDlg;
    FIrrigationAllocationGrowthDlg   : TIrrigationAllocationGrowthDlg;
    FIrrigationReturnFlowDlg         : TIrrigationReturnFlowDlg;
    FIrrigationEfficiencyDlg         : TIrrigationEfficiencyDlg;
    FIrrigationCropsDlg              : TIrrigationCropsDlg;

    FMinePropertiesDlg                 : TMinePropertiesDlg;
    FMinePlantAreaDlg                  : TMinePlantAreaDlg;
    FMineSectionsDlg                   : TMineSectionsDlg;
    FMineOpencastPitPropertiesDlg      : TMineOpencastPitPropertiesDlg;
    FMineOpencastPitWorkingsDlg        : TMineOpencastPitWorkingsDlg;
    FMineOpencastPitDisturbedDlg       : TMineOpencastPitDisturbedDlg;
    FMineOpencastPitRehabilitatedDlg   : TMineOpencastPitRehabilitatedDlg;
    FMineOpencastPitEvaporationDlg     : TMineOpencastPitEvaporationDlg;
    FMineOpencastPitDecantDlg          : TMineOpencastPitDecantDlg;
    FMineOpencastPitSeepageDlg         : TMineOpencastPitSeepageDlg;
    FMineOpencastPitRechargeFactorsDlg : TMineOpencastPitRechargeFactorsDlg;
    FMineOpencastPitWorkingsQSLDDlg    : TMineOpencastPitWorkingsQSLDDlg;
    FMineOpencastPitSeepDecantQSLDDlg  : TMineOpencastPitSeepDecantQSLDDlg;
    FMineUndergroundPropertiesDlg      : TMineUndergroundPropertiesDlg;
    FMineUndergroundBoardPillarDlg     : TMineUndergroundBoardPillarDlg;
    FMineUndergroundHighExtractionDlg  : TMineUndergroundHighExtractionDlg;
    FMineUndergroundRechargeFactorsDlg : TMineUndergroundRechargeFactorsDlg;
    FMineUndergroundQSLDDlg            : TMineUndergroundQSLDDlg;
    FMineSlurryDumpPropertiesDlg       : TMineSlurryDumpPropertiesDlg;
    FMineSlurryDumpAreaDlg             : TMineSlurryDumpAreaDlg;
    FMineSlurryDumpRechargeFactorsDlg  : TMineSlurryDumpRechargeFactorsDlg;
    FMineSlurryDumpQSLDDlg             : TMineSlurryDumpQSLDDlg;

    FNetworkSequenceDlg                : TNetworkSequenceDlg;
    FHydroOutputDlg                    : THydroOutputDlg;


    procedure RefreshDlgXML (AXMLAgent  : TXMLAgent;
                             AModule    : INetworkModule;
                             ASectionNo : Integer);
    procedure RefreshRouteDlgXML (AXMLAgent  : TXMLAgent;
                                  ARoute     : INetworkRoute);
    procedure RefreshObsPointDlgXML (AXMLAgent  : TXMLAgent;
                                     AObsPoint  : IObservationPoint);
    procedure RefreshTimeSeriesDlgXML (AXMLAgent   : TXMLAgent;
                                       ATimeSeries : ITimeSeries;
                                       ATitle      : String);
    procedure RefreshOutputDlgXML (AXMLAgent       : TXMLAgent;
                                   AElementType    : WideString;
                                   AElementSubType : WideString;
                                   AElementNo      : Integer;
                                   AElementID      : Integer;
                                   ASubElementID   : Integer;
                                   AResultTypeID   : Integer);
    function UpdateReservoirProperties (ARootNode : IXMLNode): WordBool;
    function UpdateReservoirVolumeArea (ARootNode : IXMLNode): WordBool;
    function UpdatePanData (ARootNode : IXMLNode): WordBool;
    function UpdateInflowRoutes (ARootNode : IXMLNode): WordBool;
    function UpdateOutflowRoutes (ARootNode : IXMLNode): WordBool;

    function UpdateRunOffProperties (ARootNode : IXMLNode): WordBool;
    function UpdateRunOffSlaves (ARootNode : IXMLNode): WordBool;
    function UpdateRunOffPitman (ARootNode : IXMLNode): WordBool;
    function UpdateRunOffAfforestation (ARootNode : IXMLNode): WordBool;
    function UpdateRunOffAlienVegetation (ARootNode : IXMLNode): WordBool;
    function UpdateRunOffPavedArea (ARootNode : IXMLNode): WordBool;
    function UpdateRunOffGroundWaterAbstraction (ARootNode : IXMLNode): WordBool;
    function UpdateRunOffOutflowRoutes (ARootNode : IXMLNode): WordBool;
    function UpdateRunOffHughes (ARootNode : IXMLNode): WordBool;
    function UpdateRunOffSami (ARootNode : IXMLNode): WordBool;

    function UpdateChannelProperties (ARootNode : IXMLNode): WordBool;

    function UpdateIrrigationProperties (ARootNode : IXMLNode): WordBool;
    function UpdateIrrigationWQT (ARootNode : IXMLNode): WordBool;
    function UpdateIrrigationFactors (ARootNode : IXMLNode): WordBool;
    function UpdateIrrigationArea (ARootNode : IXMLNode): WordBool;
    function UpdateIrrigationAllocationGrowth (ARootNode : IXMLNode): WordBool;
    function UpdateIrrigationReturnFlow (ARootNode : IXMLNode): WordBool;
    function UpdateIrrigationEfficiency (ARootNode : IXMLNode): WordBool;
    function UpdateIrrigationCrops (ARootNode : IXMLNode): WordBool;

    function UpdateMineProperties (ARootNode : IXMLNode): WordBool;
    function UpdateMinePlantArea (ARootNode : IXMLNode): WordBool;
    function UpdateMineSections (ARootNode : IXMLNode): WordBool;
    function UpdateMineOpencastPitProperties (ARootNode : IXMLNode): WordBool;
    function UpdateMineOpencastPitWorkings (ARootNode : IXMLNode): WordBool;
    function UpdateMineOpencastPitDisturbed (ARootNode : IXMLNode): WordBool;
    function UpdateMineOpencastPitRehabilitated (ARootNode : IXMLNode): WordBool;
    function UpdateMineOpencastPitEvaporation (ARootNode : IXMLNode): WordBool;
    function UpdateMineOpencastPitDecant (ARootNode : IXMLNode): WordBool;
    function UpdateMineOpencastPitSeepage (ARootNode : IXMLNode): WordBool;
    function UpdateMineOpencastPitRechargeFactors (ARootNode : IXMLNode): WordBool;
    function UpdateMineOpencastPitWorkingsQSLD (ARootNode : IXMLNode): WordBool;
    function UpdateMineOpencastPitSeepDecantQSLD (ARootNode : IXMLNode): WordBool;
    function UpdateMineUndergroundProperties (ARootNode : IXMLNode): WordBool;
    function UpdateMineUndergroundBoardPillar (ARootNode : IXMLNode): WordBool;
    function UpdateMineUndergroundHighExtraction (ARootNode : IXMLNode): WordBool;
    function UpdateMineUndergroundRechargeFactors (ARootNode : IXMLNode): WordBool;
    function UpdateMineUndergroundQSLD (ARootNode : IXMLNode): WordBool;
    function UpdateMineSlurryDumpProperties (ARootNode : IXMLNode): WordBool;
    function UpdateMineSlurryDumpArea (ARootNode : IXMLNode): WordBool;
    function UpdateMineSlurryDumpRechargeFactors (ARootNode : IXMLNode): WordBool;
    function UpdateMineSlurryDumpQSLD (ARootNode : IXMLNode): WordBool;

    function UpdateObservationPoint (ARootNode : IXMLNode): WordBool;
    function UpdateNetworkRoute (ARootNode : IXMLNode): WordBool;
    function UpdateNetworkSequence (ARootNode : IXMLNode): WordBool;
    function ChangeModuleSequenceNumber (AModule   : INetworkModule;
                                         ANewValue : Integer): WordBool;
  public
    function ModelName: string; override;
    function ModelData: TInterfacedObject; override;
    function ResetState: boolean; override;
    function Initialise: boolean; override;
    function ModelDataLoaded: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged (AContext   : TChangeContext;
                                  AFieldName : String;
                                  AOldValue  : String;
                                  ANewValue  : String): boolean; override;
    function LanguageHasChanged: boolean; override;
    function ProcessEvent (AEventType : integer;
                           AData      : TObject): boolean; override;
    procedure DoModelDataHasChanged (AChangeLevel  : TModelDataChangeLevel;
                                     AChangeType   : TModelDataChangeType;
                                     AChangeAction : TModelDataChangeAction); override;
    function CanApplicationClose: boolean; override;
    procedure ApplicationIsClosing; override;
    function IsGraphLoaded: boolean; override;
    function IsGridLoaded: boolean; override;

    function CanCopyToCLipboard: boolean; override;

    procedure OnTabHasChanged (ASender: TObject); override;
    function GetModelDataSetKey : string; override;
    function GetChangeListWhereClause : string; override;
    function HandleVNVEvent (const AVisioApp   : IUnknown;
                             const AVisioDoc   : IUnknown;
                             AVisioEventCode   : Integer;
                             const ASourceObj  : IUnknown;
                             AEventID          : Integer;
                             AEventSeqNum      : Integer;
                             const ASubjectObj : IUnknown;
                             AMoreInfo         : OleVariant): WordBool; safecall;
    function ProcessVNVSpecial(const AParameter : WideString): WordBool; safecall;
    function CreateNewNetwork (const ANetworkCode     : WideString;
                               AVersionNo             : Integer;
                               const AInputDir        : WideString;
                               const AOutputDir       : WideString;
                               const ADebugRequired   : WideString;
                               ADebugStartPeriod      : Integer;
                               ADebudEndPeriod        : Integer;
                               const ASummaryRequired : WideString;
                               ASimulationStartYear   : Integer;
                               ASimulationEndYear     : Integer;
                               AReadOnly              : Integer;
                               var ANetworkID         : Integer;
                               var AErrorMsg          : WideString): WordBool; safecall;
    function DeleteNetwork (ANetworkID: Integer; var AErrorMsg      : WideString): WordBool; safecall;
    function CopyNetwork (ANetworkID: Integer; const ANewNetworkCode: WideString;
                            var AErrorMsg      : WideString): WordBool; safecall;
    function ExportNetwork (ANetworkID: Integer; const ADirectory   : WideString;
                            var AErrorMsg      : WideString): WordBool; safecall;
    function ImportNetwork (const ADirectory : WideString;
                            ANetworkID: Integer;  var AErrorMsg    : WideString): WordBool; safecall;
  end;

implementation

uses
  SysUtils,
  Windows,
  VCL.ComCtrls,
  VCL.Controls,

  UModule,
  UReservoirModule,
  UChannelModule,
  URunOffModule,
  UIrrigationModule,
  UMineModule,
  UNetworkRoute,
  UObservationPoint,

  UHydroDBAgent,
  UMainMenuEventType,
  UHydrologyModelMenuItemManager,
  UUtilities,
  UErrorHandlingOperations;

procedure THydrologyModel.CreateMemberObjects;
const OPNAME = 'THydrologyModel.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FLoadingData    := False;
    FEmptyStudy     := False;
    FRefCount       := 10000;
    
    // Create the correct model data.
    FMayChangeNetwork := TRUE; // RianaChangeStructure
    FNetwork          := TNetwork.Create;
    GHydroDBAgent     := THydroDBAgent.Create;
    FHydroDBManager   := THydroDBManager.Create(FAppModules);

    FHostWidthReservoir        := 500;
    FHostHeightReservoir       := 370;
    FHostWidthChannel          := 770;
    FHostHeightChannel         := 440;
    FHostWidthRunOff           := 755;
    FHostHeightRunOff          := 505;
    FHostWidthIrrigation       := 800;
    FHostHeightIrrigation      := 460;
    FHostWidthMine             := 660;
    FHostHeightMine            := 380;
    FHostWidthNetworkRoute     := 380;
    FHostHeightNetworkRoute    := 230;
    FHostWidthMineOpencastPit  := 730;
    FHostHeightMineOpencastPit := 400;
    FHostWidthMineUnderground  := 730;
    FHostHeightMineUnderground := 400;
    FHostWidthMineSlurryDump   := 730;
    FHostHeightMineSlurryDump  := 400;
    FHostWidthObservationPoint := 380;
    FHostHeightObservationPoint:= 230;
    FHostWidthHydroOutput      := 760;
    FHostHeightHydroOutput     := 460;

    // Create the DLL based managers.
    CreateModelTabSheetManagers;

    // Create these managers last(They check if othe managers are created).
    CreateModelMenuItemManager;

    // General
    SetLength(FCustomFieldEditArray,0);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyModel.DestroyMemberObjects;
const OPNAME = 'THydrologyModel.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil(FModelMenuItemManager);
    FreeAndNil(FNetwork);
    FreeAndNil(FHydroDBManager);
    FreeAndNil(GHydroDBAgent);

    SetLength(FCustomFieldEditArray,0);
    FCustomFieldEditArray := nil;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel._AddRef: Integer;
const OPNAME = 'THydrologyModel._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydrologyModel._Release: Integer;
const OPNAME = 'THydrologyModel._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydrologyModel.Initialise: boolean;
const OPNAME = 'THydrologyModel.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := FNetwork.Initialise;
    if Assigned(FModelMenuItemManager) then
      Result := Result and FModelMenuItemManager.Initialise;
    if Assigned(FAppModules.MainForm()) then
        FAppModules.MainForm.ApplyPreviousTabIndex;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyModel.CreateModelTabSheetManagers;
const OPNAME = 'THydrologyModel.CreateModelTabSheetManagers';
begin
  try
    CreateHydroNVManager;
    if not Assigned(FAppModules.MainForm()) then Exit;
    CreateDataTabSheetManager;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyModel.CreateHydroNVManager;
const OPNAME = 'THydrologyModel.CreateHydroNVManager';
begin
  try
    CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\HydroNetworkVisualiser.dll',
      TAbstractAppObject(FHydroNVManager), FAppModules, False, OPNAME);
    if Assigned(FHydroNVManager) then
    begin
      FOwnedAppObjects.Add(FHydroNVManager);
      if Assigned(FAppModules.MainForm()) then
        FHydroNVManager.TabSheet.PageControl := FAppModules.MainForm.PageControl;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydrologyModel.CreateDataTabSheetManager;
const OPNAME = 'THydrologyModel.CreateDataTabSheetManager';
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

function THydrologyModel.LanguageHasChanged: boolean;
const OPNAME = 'THydrologyModel.LanguageHasChanged';
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
    if Result and Assigned(FHydroNVManager) then
        Result := FHydroNVManager.LanguageHasChanged;
    if Result and Assigned(FDataTabSheetManager) then
        Result := FDataTabSheetManager.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.ModelDataLoaded: boolean;
const OPNAME = 'THydrologyModel.ModelDataLoaded';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.ProcessEvent (AEventType : integer;
                                       AData      : TObject): boolean;
const OPNAME = 'THydrologyModel.ProcessEvent';
var
  LCanChange: boolean;
begin
  Result := True;
  try
    case AEventType of
      CmeReloadModelData              : RefreshModelData;
      CmeViewNetworkVisualiser        : ShowNetworkVisualiser;
      CmeViewEditGrid                 : ShowGridEditor;
      CmeEditGridEditor               : EditGridEditor(AData);
      CmeViewGraph                    : ShowGraph;
      CmeResultPageControlChanging    :
      begin
        LCanChange := True;
        OnTabChangeRequest(nil,LCanChange);
      end;
    else
      Result := inherited ProcessEvent(AEventType, AData);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.HandleVNVEvent (const AVisioApp   : IUnknown;
                                         const AVisioDoc   : IUnknown;
                                         AVisioEventCode   : Integer;
                                         const ASourceObj  : IUnknown;
                                         AEventID          : Integer;
                                         AEventSeqNum      : Integer;
                                         const ASubjectObj : IUnknown;
                                         AMoreInfo         : OleVariant): WordBool;
const OPNAME = 'THydrologyModel.HandleVNVEvent';
begin
  Result := False;
  try
    if(FHydroNVManager <> nil) then
       Result := FHydroNVManager.HandleVNVEvent(AVisioApp,AVisioDoc,AVisioEventCode,
                ASourceObj, AEventID, AEventSeqNum,ASubjectObj,AMoreInfo);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydrologyModel.ProcessVNVSpecial (const AParameter: WideString): WordBool;
const OPNAME = 'THydrologyModel.ProcessVNVSpecial';
begin
  Result := FALSE;
  try
    if (FHydroNVManager <> nil) then
      Result := FHydroNVManager.ProcessVNVSpecial(AParameter);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydrologyModel.StudyHasChanged: boolean;
const OPNAME = 'THydrologyModel.StudyHasChanged';
begin

  if FLoadingData then
  begin
    Result := True;
    Exit;
  end;
  Result := False;
  try
    FEmptyStudy := False;
    try

      Result := LoadModelData;
//      Result := Result and inherited StudyHasChanged; //RianaHydro Error

      if Result  and Assigned(FHydroNVManager)then
        Result := FHydroNVManager.StudyHasChanged;

      if Result and Assigned(FDataTabSheetManager) then
        Result := Result and FDataTabSheetManager.StudyHasChanged;

      if Assigned(FAppModules.MainForm()) then
      begin
//        AddTreeViewsSubNodes;
        FAppModules.MainForm.ApplyPreviousTabIndex;
        OnTabHasChanged(nil);
        SetSystemMenuState;
      end;
    finally
      FEmptyStudy := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'THydrologyModel.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if Result and Assigned(FHydroNVManager) then
      Result := FHydroNVManager.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
    if Assigned(FDataTabSheetManager) then
      Result := Result and FDataTabSheetManager.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);

    SetSystemMenuState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.ProcessCustomModelEvent(AData: TModelMenuData): boolean;
const OPNAME = 'THydrologyModel.ProcessCustomModelEvent';
begin
  Result := False;
  try
    //We are broadcasting the events for now. We need to pass the tabsheet name to pass
    // the message to the correct manager

    if (not Result) and  Assigned(FHydroNVManager) then
      Result := FHydroNVManager.DoCustomTabSheetEvent(AData);

    if (not Result) and  Assigned(FDataTabSheetManager) then
      Result :=  FDataTabSheetManager.DoCustomTabSheetEvent(AData);

    if not Result then
      inherited ProcessCustomModelEvent(AData);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyModel.CreateModelMenuItemManager;
const OPNAME = 'THydrologyModel.CreateModelMenuItemManager';
begin
  try

    if not Assigned(FAppModules.MainForm()) then Exit;
    FModelMenuItemManager :=
      THydrologyModelMenuItemManager.Create(FAppModules,
        Assigned(FHydroNVManager),
        Assigned(FDataTabSheetManager) and Assigned(FDataTabSheetManager.GridEditorManager),
        Assigned(FDataTabSheetManager) and Assigned(FDataTabSheetManager.GraphManager));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyModel.SetSystemMenuState;
const OPNAME = 'THydrologyModel.SetSystemMenuState';
begin
  try
    if not Assigned(FAppModules.MainForm()) then Exit;

//    THydrologyModelMenuItemManager(FModelMenuItemManager).SetExpertUserChecked(FAppModules.User.UserType = utExpert);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyModel.DoModelDataHasChanged (AChangeLevel  : TModelDataChangeLevel;
                                                 AChangeType   : TModelDataChangeType;
                                                 AChangeAction : TModelDataChangeAction);
const OPNAME = 'THydrologyModel.DoModelDataHasChanged';
begin
  inherited;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.LoadModelData: boolean;
const OPNAME = 'THydrologyModel.LoadModelData';
begin
  Result := False;
  try
    FLoadingData := True;
    try
      Result := FNetwork.LoadNetworkWithCode(FAppModules.StudyArea.ScenarioCode);
    finally
      FLoadingData := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyModel.ApplicationIsClosing;
const OPNAME = 'THydrologyModel.ApplicationIsClosing';
begin
  inherited ApplicationIsClosing;
  try
    // We need a list of Managers here - VGN 20030214
    if Assigned(FHydroNVManager) and Assigned(FHydroNVManager.TabSheet) then
      FHydroNVManager.TabSheet.ApplicationIsClosing;

    if Assigned(FDataTabSheetManager) and Assigned(FDataTabSheetManager.TabSheet) then
      FDataTabSheetManager.TabSheet.ApplicationIsClosing;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.CanApplicationClose: boolean;
const OPNAME = 'THydrologyModel.CanApplicationClose';
begin
  Result := inherited CanApplicationClose;
  try
    // We need a list of Managers here - VGN 20030214
    if Assigned(FHydroNVManager) and Assigned(FHydroNVManager.TabSheet) then
      Result := FHydroNVManager.TabSheet.CanApplicationClose;

    if Assigned(FDataTabSheetManager) and Assigned(FDataTabSheetManager.TabSheet) then
      Result := FDataTabSheetManager.TabSheet.CanApplicationClose;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.IsGraphLoaded: boolean;
const OPNAME = 'THydrologyModel.IsGraphLoaded';
begin
  Result := false;
  try
    Result :=  Assigned(FDataTabSheetManager) and Assigned(FDataTabSheetManager.GraphManager);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.IsGridLoaded: boolean;
const OPNAME = 'THydrologyModel.IsGridLoaded';
begin
  Result := false;
  try
    Result :=  Assigned(FDataTabSheetManager) and Assigned(FDataTabSheetManager.GridEditorManager);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.RefreshModelData: boolean;
const OPNAME = 'THydrologyModel.RefreshModelData';
begin
  Result :=False;
  try
    Initialise;
    StudyHasChanged;
    LanguageHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.ShowNetworkVisualiser: boolean;
const OPNAME = 'THydrologyModel.ShowNetworkVisualiser';
var LCanChange: boolean;
begin
  Result := False;
  try
    if not Assigned(FAppModules.MainForm()) then Exit;
    OnTabChangeRequest(nil, LCanChange);
    if Assigned(FHydroNVManager) and Assigned(FHydroNVManager.TabSheet) and LCanChange then
    begin
      FAppModules.MainForm.ActivePage := FHydroNVManager.TabSheet;
      OnTabHasChanged(nil);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.ShowGridEditor: boolean;
const OPNAME = 'THydrologyModel.ShowGridEditor';
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

function THydrologyModel.EditGridEditor(AData: TObject): boolean;
const OPNAME = 'THydrologyModel.EditGridEditor';
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

function THydrologyModel.ShowGraph: boolean;
const OPNAME = 'THydrologyModel.ShowGraph';
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

function THydrologyModel.ResetState: boolean;
const OPNAME = 'THydrologyModel.ResetState';
begin
    Result := inherited ResetState;
  try
    if Assigned(FHydroNVManager)  and
       Assigned(FHydroNVManager.TabSheet) then
      Result := Result and FHydroNVManager.ResetState;
    if Result and Assigned(FDataTabSheetManager) then
      Result := Result and FDataTabSheetManager.ResetState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.ModelName: string;
const OPNAME = 'THydrologyModel.ModelName';
begin
  Result := '';
  try
    Result := CHydrology;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.ModelData: TInterfacedObject;
const OPNAME = 'THydrologyModel.ModelData';
begin
  Result := nil;
  try
    Result := FNetwork;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.CanCopyToCLipboard: boolean;
const OPNAME = 'THydrologyModel.CanCopyToCLipboard';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyModel.OnTabHasChanged(ASender: TObject);
const OPNAME = 'THydrologyModel.OnTabHasChanged';
begin
  try
    if (Assigned(FModelMenuItemManager)) then
    begin
      if ((Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) and
         (FDataTabSheetManager.TabSheet = FAppModules.MainForm.PageControl.ActivePage)) then
      begin
        THydrologyModelMenuItemManager(FModelMenuItemManager).TabHasChanged(TRUE);
      end
      else
        THydrologyModelMenuItemManager(FModelMenuItemManager).TabHasChanged(FALSE);
    end;    
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydrologyModel.GetModelDataSetKey : string;
const OPNAME = 'THydrologyModel.GetModelDataSetKey';
begin
  Result := '';
  try
    Result := 'Model='         + QuotedStr(FAppModules.StudyArea.ModelCode) + ',' +
              'StudyAreaName=' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ',' +
              'SubArea='       + QuotedStr(FAppModules.StudyArea.SubAreaCode) + ',' +
              'Scenario='      + QuotedStr(FAppModules.StudyArea.ScenarioCode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.GetChangeListWhereClause: string;
const OPNAME = 'THydrologyModel.GetChangeListWhereClause';
begin
  Result := '';
  try
    Result := 'Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ' AND ' +
              'StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ' AND ' +
              'SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ' AND ' +
              'Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.Get_Network: INetwork;
const OPNAME = 'THydrologyModel.Get_Network';
begin
  Result := nil;
  try
    Result := FNetwork;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.Get_MayChangeNetwork: WordBool;
const OPNAME = 'THydrologyModel.Get_MayChangeNetwork';
begin
  Result := FALSE;
  try
    Result := FMayChangeNetwork;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyModel.Set_MayChangeNetwork(Value: WordBool);
const OPNAME = 'THydrologyModel.Set_MayChangeNetwork';
begin
  try
    FMayChangeNetwork := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.ShowObservationPointDialog (ARouteNo: Integer): WordBool;
const OPNAME = 'THydrologyModel.ShowObservationPointDialog';
var
  LHostDialog : THostDlg;
  LObsPoint   : IObservationPoint;
begin
  Result := FALSE;
  try
    LObsPoint := Network.ObservationPointAgent.ObservationPointByRouteNo[ARouteNo];
    LHostDialog := THostDlg.Create(nil);
    LHostDialog.ClientHeight := FHostHeightObservationPoint;
    LHostDialog.ClientWidth  := FHostWidthObservationPoint;
    LHostDialog.Caption := 'Observation Point ' + LObsPoint.Name;
    try
      if (AddObservationPointDialog(LHostDialog, LObsPoint) AND
          AddHydroTimeSeriesDialog(LHostDialog, LObsPoint.FlowData, LObsPoint.Name + ' Observed Flow Data')) then

        LHostDialog.ShowModal;
    finally
      FHostHeightObservationPoint := LHostDialog.ClientHeight;
      FHostWidthObservationPoint  := LHostDialog.ClientWidth;
      FreeAndNil(LHostDialog);
      FObservationPointDlg := nil;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddHydroTimeSeriesDialog (AHostDialog : THostDlg;
                                                   ATimeSeries : ITimeSeries;
                                                   ATitle      : String): Boolean;
const OPNAME = 'THydrologyModel.AddHydroTimeSeriesDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'HydroTimeSeries';
    LTabSheet.Caption     := ATitle;

    FHydroTimeSeriesDlg                   := THydroTimeSeriesDlg.Create(AHostDialog);
    FHydroTimeSeriesDlg.Parent            := LTabSheet;
    FHydroTimeSeriesDlg.Align             := alClient;
    FHydroTimeSeriesDlg.FHydrologyModel   := Self;
    FHydroTimeSeriesDlg.FMayChangeNetwork := FMayChangeNetwork;
    FHydroTimeSeriesDlg.FXMLAgent.FHydrologyModel := Self;

    RefreshTimeSeriesDlgXML(FHydroTimeSeriesDlg.FXMLAgent, ATimeSeries, ATitle);
    FHydroTimeSeriesDlg.Show;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddObservationPointDialog (AHostDialog : THostDlg;
                                                    AObsPoint   : IObservationPoint) : Boolean;
const OPNAME = 'THydrologyModel.AddObservationPointDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'ObservationPoint';
    LTabSheet.Caption     := 'Observation Point';

    FObservationPointDlg                   := TObservationPointDlg.Create(AHostDialog);
    FObservationPointDlg.Parent            := LTabSheet;
    FObservationPointDlg.Align             := alClient;
    FObservationPointDlg.FHydrologyModel   := Self;
    FObservationPointDlg.FMayChangeNetwork := FMayChangeNetwork;
    FObservationPointDlg.FXMLAgent.FHydrologyModel := Self;

    RefreshObsPointDlgXML(FObservationPointDlg.FXMLAgent, AObsPoint);
    FObservationPointDlg.Show;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.ShowNetworkRouteDialog (ARouteNo: Integer): WordBool;
const OPNAME = 'THydrologyModel.ShowNetworkRouteDialog';
var
  LHostDialog : THostDlg;
  LRoute      : INetworkRoute;
begin
  Result := FALSE;
  try
    LRoute := Network.NetworkRouteAgent.NetworkRouteByRouteNo[ARouteNo];
    LHostDialog := THostDlg.Create(nil);
    LHostDialog.ClientHeight := FHostHeightNetworkRoute;
    LHostDialog.ClientWidth  := FHostWidthNetworkRoute;
    LHostDialog.Caption := 'Network Route ' + IntToStr(ARouteNo);
    try
      if (AddNetworkRouteDialog(LHostDialog, LRoute)) then
        LHostDialog.ShowModal;
    finally
      FHostHeightNetworkRoute := LHostDialog.ClientHeight;
      FHostWidthNetworkRoute  := LHostDialog.ClientWidth;
      FreeAndNil(LHostDialog);
      FNetworkRouteDlg := nil;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddNetworkRouteDialog (AHostDialog : THostDlg;
                                                ARoute      : INetworkRoute) : Boolean;
const OPNAME = 'THydrologyModel.AddNetworkRouteDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'NetworkRoute';
    LTabSheet.Caption     := 'Network Route';

    FNetworkRouteDlg                   := TNetworkRouteDlg.Create(AHostDialog);
    FNetworkRouteDlg.Parent            := LTabSheet;
    FNetworkRouteDlg.Align             := alClient;
    FNetworkRouteDlg.FHydrologyModel   := Self;
    FNetworkRouteDlg.FMayChangeNetwork := FMayChangeNetwork;
    FNetworkRouteDlg.FXMLAgent.FHydrologyModel := Self;

    RefreshRouteDlgXML(FNetworkRouteDlg.FXMLAgent, ARoute);
    FNetworkRouteDlg.Show;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.ShowReservoirModuleDialog (AModuleNo: Integer): WordBool;
const OPNAME = 'THydrologyModel.ShowReservoirModuleDialog';
var
  LReservoirModule : IReservoirModule;
  LHostDialog      : THostDlg;
begin
  Result := FALSE;
  try
    LReservoirModule := Network.ReservoirModuleAgent.ReservoirModuleByNumber[AModuleNo];
    LHostDialog := THostDlg.Create(nil);
    LHostDialog.ClientHeight := FHostHeightReservoir;
    LHostDialog.ClientWidth  := FHostWidthReservoir;
    LHostDialog.Caption := 'Reservoir Module RV' + IntToStr(AModuleNo);

    try
      if (AddReservoirPropertiesDialog(LHostDialog, LReservoirModule)) AND
         (AddReservoirVolumeAreaDialog(LHostDialog, LReservoirModule)) AND
         (AddPanDataDialog(LHostDialog, LReservoirModule)) AND
         (AddInflowRoutesDialog(LHostDialog, LReservoirModule)) AND
         (AddOutflowRoutesDialog(LHostDialog, LReservoirModule)) then
        LHostDialog.ShowModal;
    finally
      FHostHeightReservoir := LHostDialog.ClientHeight;
      FHostWidthReservoir  := LHostDialog.ClientWidth;
      FreeAndNil(LHostDialog);
      FReservoirPropertiesDlg := nil;
      FReservoirVolumeAreaDlg := nil;
      FPanDataDlg             := nil;
      FInflowRoutesDlg        := nil;
      FOutflowRoutesDlg       := nil;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddReservoirPropertiesDialog (AHostDialog : THostDlg; AModule : INetworkModule) : Boolean;
const OPNAME = 'THydrologyModel.AddReservoirPropertiesDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'ReservoirProperties';
    LTabSheet.Caption     := 'Properties';

    FReservoirPropertiesDlg                   := TReservoirPropertiesDlg.Create(AHostDialog);
    FReservoirPropertiesDlg.Parent            := LTabSheet;
    FReservoirPropertiesDlg.Align             := alClient;
    FReservoirPropertiesDlg.FHydrologyModel   := Self;
    FReservoirPropertiesDlg.FMayChangeNetwork := FMayChangeNetwork;
    FReservoirPropertiesDlg.FXMLAgent.FHydrologyModel := Self;

    RefreshDlgXML(FReservoirPropertiesDlg.FXMLAgent, AModule, 0);
    FReservoirPropertiesDlg.Show;
    Result := TRUE;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyModel.RefreshDlgXML (AXMLAgent  : TXMLAgent;
                                         AModule    : INetworkModule;
                                         ASectionNo : Integer);
const OPNAME = 'THydrologyModel.RefreshDlgXML';
var
  LAgentXMLDocIn   : IXMLDocument;
  LAgentXMLDocOut  : IXMLDocument;
  LDialogXMLDocIn  : IXMLDocument;
  LDialogXMLDocOut : IXMLDocument;
begin
  try
    LAgentXMLDocIn   := AXMLAgent.InXMLDocument(AModule, ASectionNo);
    LAgentXMLDocOut  := AXMLAgent.OutXMLDocument(AModule, ASectionNo);
    LDialogXMLDocIn  := AXMLAgent.DlgXMLDocumentIn;
    LDialogXMLDocOut := AXMLAgent.DlgXMLDocumentOut;

    LAgentXMLDocOut.Active := TRUE;
    LDialogXMLDocIn.XML.Clear;
    LDialogXMLDocOut.XML.Clear;
    LDialogXMLDocIn.XML.Text  := LAgentXMLDocIn.XML.Text;
    LDialogXMLDocOut.XML.Text := LAgentXMLDocOut.XML.Text;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyModel.RefreshRouteDlgXML (AXMLAgent  : TXMLAgent;
                                              ARoute     : INetworkRoute);
const OPNAME = 'THydrologyModel.RefreshRouteDlgXML';
var
  LAgentXMLDocIn   : IXMLDocument;
  LAgentXMLDocOut  : IXMLDocument;
  LDialogXMLDocIn  : IXMLDocument;
  LDialogXMLDocOut : IXMLDocument;
begin
  try
    LAgentXMLDocIn   := AXMLAgent.InXMLDocument(ARoute);
    LAgentXMLDocOut  := AXMLAgent.OutXMLDocument(ARoute);
    LDialogXMLDocIn  := AXMLAgent.DlgXMLDocumentIn;
    LDialogXMLDocOut := AXMLAgent.DlgXMLDocumentOut;

    LAgentXMLDocOut.Active := TRUE;
    LDialogXMLDocIn.XML.Clear;
    LDialogXMLDocOut.XML.Clear;
    LDialogXMLDocIn.XML.Text  := LAgentXMLDocIn.XML.Text;
    LDialogXMLDocOut.XML.Text := LAgentXMLDocOut.XML.Text;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyModel.RefreshObsPointDlgXML (AXMLAgent  : TXMLAgent;
                                                 AObsPoint  : IObservationPoint);
const OPNAME = 'THydrologyModel.RefreshObsPointDlgXML';
var
  LAgentXMLDocIn   : IXMLDocument;
  LAgentXMLDocOut  : IXMLDocument;
  LDialogXMLDocIn  : IXMLDocument;
  LDialogXMLDocOut : IXMLDocument;
begin
  try
    LAgentXMLDocIn   := AXMLAgent.InXMLDocument(AObsPoint);
    LAgentXMLDocOut  := AXMLAgent.OutXMLDocument(AObsPoint);
    LDialogXMLDocIn  := AXMLAgent.DlgXMLDocumentIn;
    LDialogXMLDocOut := AXMLAgent.DlgXMLDocumentOut;

    LAgentXMLDocOut.Active := TRUE;
    LDialogXMLDocIn.XML.Clear;
    LDialogXMLDocOut.XML.Clear;
    LDialogXMLDocIn.XML.Text  := LAgentXMLDocIn.XML.Text;
    LDialogXMLDocOut.XML.Text := LAgentXMLDocOut.XML.Text;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyModel.RefreshTimeSeriesDlgXML (AXMLAgent   : TXMLAgent;
                                                   ATimeSeries : ITimeSeries;
                                                   ATitle      : String);
const OPNAME = 'THydrologyModel.RefreshTimeSeriesDlgXML';
var
  LAgentXMLDocIn   : IXMLDocument;
  LDialogXMLDocIn  : IXMLDocument;
begin
  try
    LAgentXMLDocIn   := AXMLAgent.InXMLDocument(ATimeSeries, ATitle);
    LDialogXMLDocIn  := AXMLAgent.DlgXMLDocumentIn;

    LDialogXMLDocIn.XML.Clear;
    LDialogXMLDocIn.XML.Text  := LAgentXMLDocIn.XML.Text;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyModel.RefreshOutputDlgXML (AXMLAgent       : TXMLAgent;
                                               AElementType    : WideString;
                                               AElementSubType : WideString;
                                               AElementNo      : Integer;
                                               AElementID      : Integer;
                                               ASubElementID   : Integer;
                                               AResultTypeID   : Integer);
const OPNAME = 'THydrologyModel.RefreshOutputDlgXML';
var
  LAgentXMLDocIn   : IXMLDocument;
  LAgentXMLDocOut  : IXMLDocument;
  LDialogXMLDocIn  : IXMLDocument;
  LDialogXMLDocOut : IXMLDocument;
begin
  try
    LAgentXMLDocIn   := AXMLAgent.InXMLDocument(AElementType, AElementSubType, AElementNo, AElementID, ASubElementID, AResultTypeID);
    LAgentXMLDocOut  := AXMLAgent.OutXMLDocument(AElementType, AElementSubType, AElementNo, AElementID, ASubElementID, AResultTypeID);
    LDialogXMLDocIn  := AXMLAgent.DlgXMLDocumentIn;
    LDialogXMLDocOut := AXMLAgent.DlgXMLDocumentOut;

    LAgentXMLDocOut.Active := TRUE;
    LDialogXMLDocIn.XML.Clear;
    LDialogXMLDocOut.XML.Clear;
    LDialogXMLDocIn.XML.Text  := LAgentXMLDocIn.XML.Text;
    LDialogXMLDocOut.XML.Text := LAgentXMLDocOut.XML.Text;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddReservoirVolumeAreaDialog (AHostDialog : THostDlg; AModule : INetworkModule) : Boolean;
const OPNAME = 'THydrologyModel.AddReservoirVolumeAreaDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'ReservoirVolumeArea';
    LTabSheet.Caption     := 'Volume/surface area data';

    FReservoirVolumeAreaDlg                    := TReservoirVolumeAreaDlg.Create(AHostDialog);
    FReservoirVolumeAreaDlg.Parent             := LTabSheet;
    FReservoirVolumeAreaDlg.Align              := alClient;
    FReservoirVolumeAreaDlg.FHydrologyModel    := Self;
    FReservoirVolumeAreaDlg.FMayChangeNetwork  := FMayChangeNetwork;
    FReservoirVolumeAreaDlg.FXMLAgent.FHydrologyModel := Self;

    RefreshDlgXML(FReservoirVolumeAreaDlg.FXMLAgent, AModule, 0);
    FReservoirVolumeAreaDlg.Show;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddPanDataDialog (AHostDialog : THostDlg; AModule : INetworkModule) : Boolean;
const OPNAME = 'THydrologyModel.AddPanDataDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'PanData';
    LTabSheet.Caption     := 'Pan';

    FPanDataDlg                   := TPanDataDlg.Create(AHostDialog);
    FPanDataDlg.Parent            := LTabSheet;
    FPanDataDlg.Align             := alClient;
    FPanDataDlg.FHydrologyModel   := Self;
    FPanDataDlg.FMayChangeNetwork := FMayChangeNetwork;
    FPanDataDlg.FXMLAgent.FHydrologyModel := Self;

    RefreshDlgXML(FPanDataDlg.FXMLAgent, AModule, 0);
    FPanDataDlg.Show;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddInflowRoutesDialog (AHostDialog : THostDlg; AModule : INetworkModule) : Boolean;
const OPNAME = 'THydrologyModel.AddInflowRoutesDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'InflowRoutes';
    LTabSheet.Caption     := 'Inflow Routes';

    FInflowRoutesDlg                   := TInflowRoutesDlg.Create(AHostDialog);
    FInflowRoutesDlg.Parent            := LTabSheet;
    FInflowRoutesDlg.Align             := alClient;
    FInflowRoutesDlg.FHydrologyModel   := Self;
    FInflowRoutesDlg.FMayChangeNetwork := FMayChangeNetwork;
    FInflowRoutesDlg.FXMLAgent.FHydrologyModel := Self;

    RefreshDlgXML(FInflowRoutesDlg.FXMLAgent, AModule, 0);
    FInflowRoutesDlg.Show;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddOutflowRoutesDialog (AHostDialog : THostDlg; AModule : INetworkModule) : Boolean;
const OPNAME = 'THydrologyModel.AddOutflowRoutesDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'OutflowRoutes';
    LTabSheet.Caption     := 'Outflow Routes';

    FOutflowRoutesDlg                    := TOutflowRoutesDlg.Create(AHostDialog);
    FOutflowRoutesDlg.Parent             := LTabSheet;
    FOutflowRoutesDlg.Align              := alClient;
    FOutflowRoutesDlg.FHydrologyModel    := Self;
    FOutflowRoutesDlg.FMayChangeNetwork  := FMayChangeNetwork;
    FOutflowRoutesDlg.FXMLAgent.FHydrologyModel := Self;

    FOutflowRoutesDlg.Show;
    RefreshDlgXML(FOutflowRoutesDlg.FXMLAgent, AModule, 0);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.ShowChannelModuleDialog (AModuleNo: Integer): WordBool;
const OPNAME = 'THydrologyModel.ShowChannelModuleDialog';
var
  LChannelModule : IChannelModule;
  LHostDialog    : THostDlg;
begin
  Result := FALSE;
  try
    LChannelModule := Network.ChannelModuleAgent.ChannelModuleByNumber[AModuleNo];
    LHostDialog := THostDlg.Create(nil);
    LHostDialog.ClientHeight := FHostHeightChannel;
    LHostDialog.ClientWidth  := FHostWidthChannel;
    LHostDialog.Caption := 'Channel Module CR' + IntToStr(AModuleNo);
    try
      if (AddChannelPropertiesDialog(LHostDialog, LChannelModule)) AND
         (AddPanDataDialog(LHostDialog, LChannelModule)) AND
         (AddInflowRoutesDialog(LHostDialog, LChannelModule)) AND
         (AddOutflowRoutesDialog(LHostDialog, LChannelModule)) then
        LHostDialog.ShowModal;
    finally
      FHostHeightChannel := LHostDialog.ClientHeight;
      FHostWidthChannel  := LHostDialog.ClientWidth;
      FreeAndNil(LHostDialog);
      FChannelPropertiesDlg := nil;
      FPanDataDlg           := nil;
      FInflowRoutesDlg      := nil;
      FOutflowRoutesDlg     := nil;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddChannelPropertiesDialog (AHostDialog : THostDlg; AModule : INetworkModule) : Boolean;
const OPNAME = 'THydrologyModel.AddChannelPropertiesDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'ChannelProperties';
    LTabSheet.Caption     := 'Properties';

    FChannelPropertiesDlg                   := TChannelPropertiesDlg.Create(AHostDialog);
    FChannelPropertiesDlg.Parent            := LTabSheet;
    FChannelPropertiesDlg.Align             := alClient;
    FChannelPropertiesDlg.FHydrologyModel   := Self;
    FChannelPropertiesDlg.FMayChangeNetwork := FMayChangeNetwork;
    FChannelPropertiesDlg.FXMLAgent.FHydrologyModel := Self;

    RefreshDlgXML(FChannelPropertiesDlg.FXMLAgent, AModule, 0);
    FChannelPropertiesDlg.Show;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.ShowRunOffModuleDialog (AModuleNo: Integer): WordBool;
const OPNAME = 'THydrologyModel.ShowRunOffModuleDialog';
var
  LRunOffModule : IRunOffModule;
  LResult       : Boolean;
  LHostDialog   : THostDlg;
begin
  Result := FALSE;
  try
    LResult := FALSE;
    LRunOffModule := Network.RunOffModuleAgent.RunOffModuleByNumber[AModuleNo];
    LHostDialog := THostDlg.Create(nil);
    LHostDialog.ClientHeight := FHostHeightRunOff;
    LHostDialog.ClientWidth  := FHostWidthRunOff;
    LHostDialog.Caption := 'RunOff Module RU' + IntToStr(AModuleNo);
    try
      LResult := (AddRunOffPropertiesDialog(LHostDialog, LRunOffModule)) AND
                 (AddRunOffSlavesDialog(LHostDialog, LRunOffModule)) AND
                 (AddPanDataDialog(LHostDialog, LRunOffModule)) AND
                 (AddRunOffOutflowRoutesDialog(LHostDialog, LRunOffModule)) AND
                 (AddRunOffPitmanDialog(LHostDialog, LRunOffModule));
      if (LRunOffModule.HughesModel <> nil) then
        LResult := LResult AND AddRunOffHughesDialog(LHostDialog, LRunOffModule);
      if (LRunOffModule.SamiModel <> nil) then
        LResult := LResult AND AddRunOffSamiDialog(LHostDialog, LRunOffModule);
      if (LRunOffModule.Afforestation <> nil) then
        LResult := LResult AND AddRunOffAfforestationDialog(LHostDialog, LRunOffModule);
      if (LRunOffModule.AlienVegetation <> nil) then
        LResult := LResult AND AddRunOffAlienVegetationDialog(LHostDialog, LRunOffModule);
      if (LRunOffModule.PavedArea <> nil) then
        LResult := LResult AND AddRunOffPavedAreaDialog(LHostDialog, LRunOffModule);
      if (LRunOffModule.GroundWaterAbstraction <> nil) then
        LResult := LResult AND AddRunOffGroundWaterAbstractionDialog(LHostDialog, LRunOffModule);

      if (LResult) then
        LHostDialog.ShowModal;
    finally
      FHostHeightRunOff := LHostDialog.ClientHeight;
      FHostWidthRunOff  := LHostDialog.ClientWidth;
      FreeAndNil(LHostDialog);
      FRunOffPropertiesDlg             := nil;
      FRunOffSlavesDlg                 := nil;
      FRunOffPitmanDlg                 := nil;
      FRunOffOutflowRoutesDlg          := nil;
      FRunOffHughesDlg                 := nil;
      FRunOffSamiDlg                   := nil;
      FPanDataDlg                      := nil;
      FRunOffAfforestationDlg          := nil;
      FRunOffAlienVegetationDlg        := nil;
      FRunOffPavedAreaDlg              := nil;
      FRunOffGroundWaterAbstractionDlg := nil;
      Result := LResult;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddRunOffPropertiesDialog (AHostDialog : THostDlg; AModule : INetworkModule) : Boolean;
const OPNAME = 'THydrologyModel.AddRunOffPropertiesDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'RunOffProperties';
    LTabSheet.Caption     := 'Properties';

    FRunOffPropertiesDlg                   := TRunOffPropertiesDlg.Create(AHostDialog);
    FRunOffPropertiesDlg.Parent            := LTabSheet;
    FRunOffPropertiesDlg.Align             := alClient;
    FRunOffPropertiesDlg.FHydrologyModel   := Self;
    FRunOffPropertiesDlg.FMayChangeNetwork := FMayChangeNetwork;
    FRunOffPropertiesDlg.FXMLAgent.FHydrologyModel := Self;

    RefreshDlgXML(FRunOffPropertiesDlg.FXMLAgent, AModule, 0);
    FRunOffPropertiesDlg.Show;
    Result := TRUE;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddRunOffSlavesDialog (AHostDialog : THostDlg; AModule  : INetworkModule) : Boolean;
const OPNAME = 'THydrologyModel.AddRunOffSlavesDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'RunOffSlaves';
    LTabSheet.Caption     := 'Slaves';

    FRunOffSlavesDlg                   := TRunOffSlavesDlg.Create(AHostDialog);
    FRunOffSlavesDlg.Parent            := LTabSheet;
    FRunOffSlavesDlg.Align             := alClient;
    FRunOffSlavesDlg.FHydrologyModel   := Self;
    FRunOffSlavesDlg.FMayChangeNetwork := FMayChangeNetwork;
    FRunOffSlavesDlg.FXMLAgent.FHydrologyModel := Self;

    RefreshDlgXML(FRunOffSlavesDlg.FXMLAgent, AModule, 0);
    FRunOffSlavesDlg.Show;
    Result := TRUE;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddRunOffPitmanDialog (AHostDialog : THostDlg; AModule : INetworkModule) : Boolean;
const OPNAME = 'THydrologyModel.AddRunOffPitmanDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'RunOffPitman';
    LTabSheet.Caption     := 'Pitman model';

    FRunOffPitmanDlg                   := TRunOffPitmanDlg.Create(AHostDialog);
    FRunOffPitmanDlg.Parent            := LTabSheet;
    FRunOffPitmanDlg.Align             := alClient;
    FRunOffPitmanDlg.FHydrologyModel   := Self;
    FRunOffPitmanDlg.FMayChangeNetwork := FMayChangeNetwork;
    FRunOffPitmanDlg.FXMLAgent.FHydrologyModel := Self;

    RefreshDlgXML(FRunOffPitmanDlg.FXMLAgent, AModule, 0);
    FRunOffPitmanDlg.Show;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddRunOffAfforestationDialog (AHostDialog : THostDlg; AModule : INetworkModule) : Boolean;
const OPNAME = 'THydrologyModel.AddRunOffAfforestationDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'RunOffAfforestation';
    LTabSheet.Caption     := 'Afforestation';

    FRunOffAfforestationDlg                   := TRunOffAfforestationDlg.Create(AHostDialog);
    FRunOffAfforestationDlg.Parent            := LTabSheet;
    FRunOffAfforestationDlg.Align             := alClient;
    FRunOffAfforestationDlg.FHydrologyModel   := Self;
    FRunOffAfforestationDlg.FMayChangeNetwork := FMayChangeNetwork;
    FRunOffAfforestationDlg.FXMLAgent.FHydrologyModel := Self;

    FRunOffAfforestationDlg.Show;
    RefreshDlgXML(FRunOffAfforestationDlg.FXMLAgent, AModule, 0);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddRunOffAlienVegetationDialog (AHostDialog : THostDlg; AModule : INetworkModule) : Boolean;
const OPNAME = 'THydrologyModel.AddRunOffAlienVegetationDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'RunOffAlienVegetation';
    LTabSheet.Caption     := 'Alien vegetation';

    FRunOffAlienVegetationDlg                   := TRunOffAlienVegetationDlg.Create(AHostDialog);
    FRunOffAlienVegetationDlg.Parent            := LTabSheet;
    FRunOffAlienVegetationDlg.Align             := alClient;
    FRunOffAlienVegetationDlg.FHydrologyModel   := Self;
    FRunOffAlienVegetationDlg.FMayChangeNetwork := FMayChangeNetwork;
    FRunOffAlienVegetationDlg.FXMLAgent.FHydrologyModel := Self;

    RefreshDlgXML(FRunOffAlienVegetationDlg.FXMLAgent, AModule, 0);
    FRunOffAlienVegetationDlg.Show;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddRunOffPavedAreaDialog (AHostDialog : THostDlg; AModule : INetworkModule) : Boolean;
const OPNAME = 'THydrologyModel.AddRunOffPavedAreaDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'RunOffPavedArea';
    LTabSheet.Caption     := 'Paved Areas';

    FRunOffPavedAreaDlg                   := TRunOffPavedAreaDlg.Create(AHostDialog);
    FRunOffPavedAreaDlg.Parent            := LTabSheet;
    FRunOffPavedAreaDlg.Align             := alClient;
    FRunOffPavedAreaDlg.FHydrologyModel   := Self;
    FRunOffPavedAreaDlg.FMayChangeNetwork := FMayChangeNetwork;
    FRunOffPavedAreaDlg.FXMLAgent.FHydrologyModel := Self;

    FRunOffPavedAreaDlg.Show;
    RefreshDlgXML(FRunOffPavedAreaDlg.FXMLAgent, AModule, 0);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddRunOffGroundWaterAbstractionDialog (AHostDialog : THostDlg; AModule : INetworkModule) : Boolean;
const OPNAME = 'THydrologyModel.AddRunOffGroundWaterAbstractionDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'RunOffGroundWaterAbstraction';
    LTabSheet.Caption     := 'Groundwater Abstraction';

    FRunOffGroundWaterAbstractionDlg                   := TRunOffGroundWaterAbstractionDlg.Create(AHostDialog);
    FRunOffGroundWaterAbstractionDlg.Parent            := LTabSheet;
    FRunOffGroundWaterAbstractionDlg.Align             := alClient;
    FRunOffGroundWaterAbstractionDlg.FHydrologyModel   := Self;
    FRunOffGroundWaterAbstractionDlg.FMayChangeNetwork := FMayChangeNetwork;
    FRunOffGroundWaterAbstractionDlg.FXMLAgent.FHydrologyModel := Self;

    FRunOffGroundWaterAbstractionDlg.Show;
    RefreshDlgXML(FRunOffGroundWaterAbstractionDlg.FXMLAgent, AModule, 0);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddRunOffOutflowRoutesDialog (AHostDialog : THostDlg; AModule : INetworkModule) : Boolean;
const OPNAME = 'THydrologyModel.AddRunOffOutflowRoutesDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'RunOffOutflowRoutes';
    LTabSheet.Caption     := 'Outflow routes';

    FRunOffOutflowRoutesDlg                   := TRunOffOutflowRoutesDlg.Create(AHostDialog);
    FRunOffOutflowRoutesDlg.Parent            := LTabSheet;
    FRunOffOutflowRoutesDlg.Align             := alClient;
    FRunOffOutflowRoutesDlg.FHydrologyModel   := Self;
    FRunOffOutflowRoutesDlg.FMayChangeNetwork := FMayChangeNetwork;
    FRunOffOutflowRoutesDlg.FXMLAgent.FHydrologyModel := Self;

    FRunOffOutflowRoutesDlg.Show;
    RefreshDlgXML(FRunOffOutflowRoutesDlg.FXMLAgent, AModule, 0);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddRunOffHughesDialog (AHostDialog : THostDlg; AModule : INetworkModule) : Boolean;
const OPNAME = 'THydrologyModel.AddRunOffHughesDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'RunOffHughes';
    LTabSheet.Caption     := 'Hughes model';

    FRunOffHughesDlg                   := TRunOffHughesDlg.Create(AHostDialog);
    FRunOffHughesDlg.Parent            := LTabSheet;
    FRunOffHughesDlg.Align             := alClient;
    FRunOffHughesDlg.FHydrologyModel   := Self;
    FRunOffHughesDlg.FMayChangeNetwork := FMayChangeNetwork;
    FRunOffHughesDlg.FXMLAgent.FHydrologyModel := Self;

    FRunOffHughesDlg.Show;
    RefreshDlgXML(FRunOffHughesDlg.FXMLAgent, AModule, 0);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddRunOffSamiDialog (AHostDialog : THostDlg; AModule : INetworkModule) : Boolean;
const OPNAME = 'THydrologyModel.AddRunOffSamiDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'RunOffSami';
    LTabSheet.Caption     := 'Sami model';

    FRunOffSamiDlg                   := TRunOffSamiDlg.Create(AHostDialog);
    FRunOffSamiDlg.Parent            := LTabSheet;
    FRunOffSamiDlg.Align             := alClient;
    FRunOffSamiDlg.FHydrologyModel   := Self;
    FRunOffSamiDlg.FMayChangeNetwork := FMayChangeNetwork;
    FRunOffSamiDlg.FXMLAgent.FHydrologyModel := Self;

    FRunOffSamiDlg.Show;
    RefreshDlgXML(FRunOffSamiDlg.FXMLAgent, AModule, 0);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.ShowIrrigationModuleDialog (AModuleNo: Integer): WordBool;
const OPNAME = 'THydrologyModel.ShowIrrigationModuleDialog';
var
  LIrrigationModule : IIrrigationModule;
  LHostDialog       : THostDlg;
begin
  Result := FALSE;
  try
    LIrrigationModule := Network.IrrigationModuleAgent.IrrigationModuleByNumber[AModuleNo];
    LHostDialog := THostDlg.Create(nil);
    LHostDialog.ClientHeight := FHostHeightIrrigation;
    LHostDialog.ClientWidth  := FHostWidthIrrigation;
    LHostDialog.Caption := 'Irrigation Module RR' + IntToStr(AModuleNo);
    try
      if ((AddIrrigationPropertiesDialog(LHostDialog, LIrrigationModule)) AND
          (AddIrrigationWQTDialog(LHostDialog, LIrrigationModule)) AND
          (AddIrrigationFactorsDialog(LHostDialog, LIrrigationModule)) AND
          (AddIrrigationAreaDialog(LHostDialog, LIrrigationModule)) AND
          (AddIrrigationAllocationGrowthDialog(LHostDialog, LIrrigationModule)) AND
          (AddIrrigationReturnFlowDialog(LHostDialog, LIrrigationModule)) AND
          (AddIrrigationEfficiencyDialog(LHostDialog, LIrrigationModule)) AND
          (AddIrrigationCropsDialog(LHostDialog, LIrrigationModule)) AND
          (AddPanDataDialog(LHostDialog, LIrrigationModule))) then
        LHostDialog.ShowModal;
    finally
      FHostHeightIrrigation := LHostDialog.ClientHeight;
      FHostWidthIrrigation  := LHostDialog.ClientWidth;
      FreeAndNil(LHostDialog);
      FIrrigationPropertiesDlg       := nil;
      FIrrigationWQTDlg              := nil;
      FIrrigationFactorsDlg          := nil;
      FIrrigationAreaDlg             := nil;
      FIrrigationAllocationGrowthDlg := nil;
      FIrrigationReturnFlowDlg       := nil;
      FIrrigationEfficiencyDlg       := nil;
      FIrrigationCropsDlg            := nil;
      FPanDataDlg                    := nil;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddIrrigationPropertiesDialog (AHostDialog : THostDlg; AModule : INetworkModule) : Boolean;
const OPNAME = 'THydrologyModel.AddIrrigationPropertiesDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'IrrigationProperties';
    LTabSheet.Caption     := 'Properties';

    FIrrigationPropertiesDlg                   := TIrrigationPropertiesDlg.Create(AHostDialog);
    FIrrigationPropertiesDlg.Parent            := LTabSheet;
    FIrrigationPropertiesDlg.Align             := alClient;
    FIrrigationPropertiesDlg.FHydrologyModel   := Self;
    FIrrigationPropertiesDlg.FMayChangeNetwork := FMayChangeNetwork;
    FIrrigationPropertiesDlg.FXMLAgent.FHydrologyModel := Self;

    FIrrigationPropertiesDlg.Show;
    RefreshDlgXML(FIrrigationPropertiesDlg.FXMLAgent, AModule, 0);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddIrrigationWQTDialog (AHostDialog : THostDlg; AModule : INetworkModule) : Boolean;
const OPNAME = 'THydrologyModel.AddIrrigationWQTDialog';
var
  LTabSheet         : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'IrrigationWQT';
    LTabSheet.Caption     := 'WQT';

    FIrrigationWQTDlg                   := TIrrigationWQTDlg.Create(AHostDialog);
    FIrrigationWQTDlg.Parent            := LTabSheet;
    FIrrigationWQTDlg.Align             := alClient;
    FIrrigationWQTDlg.FHydrologyModel   := Self;
    FIrrigationWQTDlg.FMayChangeNetwork := FMayChangeNetwork;
    FIrrigationWQTDlg.FXMLAgent.FHydrologyModel := Self;

    FIrrigationWQTDlg.Show;
    RefreshDlgXML(FIrrigationWQTDlg.FXMLAgent, AModule, 0);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddIrrigationFactorsDialog (AHostDialog : THostDlg; AModule : INetworkModule) : Boolean;
const OPNAME = 'THydrologyModel.AddIrrigationFactorsDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'IrrigationFactors';
    LTabSheet.Caption     := 'Factors';

    FIrrigationFactorsDlg                   := TIrrigationFactorsDlg.Create(AHostDialog);
    FIrrigationFactorsDlg.Parent            := LTabSheet;
    FIrrigationFactorsDlg.Align             := alClient;
    FIrrigationFactorsDlg.FHydrologyModel   := Self;
    FIrrigationFactorsDlg.FMayChangeNetwork := FMayChangeNetwork;
    FIrrigationFactorsDlg.FXMLAgent.FHydrologyModel := Self;

    FIrrigationFactorsDlg.Show;
    RefreshDlgXML(FIrrigationFactorsDlg.FXMLAgent, AModule, 0);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddIrrigationAreaDialog (AHostDialog : THostDlg; AModule : INetworkModule) : Boolean;
const OPNAME = 'THydrologyModel.AddIrrigationAreaDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'IrrigationArea';
    LTabSheet.Caption     := 'Area';

    FIrrigationAreaDlg                   := TIrrigationAreaDlg.Create(AHostDialog);
    FIrrigationAreaDlg.Parent            := LTabSheet;
    FIrrigationAreaDlg.Align             := alClient;
    FIrrigationAreaDlg.FHydrologyModel   := Self;
    FIrrigationAreaDlg.FMayChangeNetwork := FMayChangeNetwork;
    FIrrigationAreaDlg.FXMLAgent.FHydrologyModel := Self;

    FIrrigationAreaDlg.Show;
    RefreshDlgXML(FIrrigationAreaDlg.FXMLAgent, AModule, 0);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddIrrigationAllocationGrowthDialog (AHostDialog : THostDlg; AModule : INetworkModule) : Boolean;
const OPNAME = 'THydrologyModel.AddIrrigationAllocationGrowthDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'IrrigationAllocationGrowth';
    LTabSheet.Caption     := 'Allocation growth';

    FIrrigationAllocationGrowthDlg                   := TIrrigationAllocationGrowthDlg.Create(AHostDialog);
    FIrrigationAllocationGrowthDlg.Parent            := LTabSheet;
    FIrrigationAllocationGrowthDlg.Align             := alClient;
    FIrrigationAllocationGrowthDlg.FHydrologyModel   := Self;
    FIrrigationAllocationGrowthDlg.FMayChangeNetwork := FMayChangeNetwork;
    FIrrigationAllocationGrowthDlg.FXMLAgent.FHydrologyModel := Self;

    FIrrigationAllocationGrowthDlg.Show;
    RefreshDlgXML(FIrrigationAllocationGrowthDlg.FXMLAgent, AModule, 0);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddIrrigationReturnFlowDialog (AHostDialog : THostDlg; AModule : INetworkModule) : Boolean;
const OPNAME = 'THydrologyModel.AddIrrigationReturnFlowDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'IrrigationReturnFlow';
    LTabSheet.Caption     := 'Return flow';

    FIrrigationReturnFlowDlg                   := TIrrigationReturnFlowDlg.Create(AHostDialog);
    FIrrigationReturnFlowDlg.Parent            := LTabSheet;
    FIrrigationReturnFlowDlg.Align             := alClient;
    FIrrigationReturnFlowDlg.FHydrologyModel   := Self;
    FIrrigationReturnFlowDlg.FMayChangeNetwork := FMayChangeNetwork;
    FIrrigationReturnFlowDlg.FXMLAgent.FHydrologyModel := Self;

    FIrrigationReturnFlowDlg.Show;
    RefreshDlgXML(FIrrigationReturnFlowDlg.FXMLAgent, AModule, 0);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddIrrigationEfficiencyDialog (AHostDialog : THostDlg; AModule : INetworkModule) : Boolean;
const OPNAME = 'THydrologyModel.AddIrrigationEfficiencyDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'IrrigationEfficiency';
    LTabSheet.Caption     := 'Efficiency';

    FIrrigationEfficiencyDlg                   := TIrrigationEfficiencyDlg.Create(AHostDialog);
    FIrrigationEfficiencyDlg.Parent            := LTabSheet;
    FIrrigationEfficiencyDlg.Align             := alClient;
    FIrrigationEfficiencyDlg.FHydrologyModel   := Self;
    FIrrigationEfficiencyDlg.FMayChangeNetwork := FMayChangeNetwork;
    FIrrigationEfficiencyDlg.FXMLAgent.FHydrologyModel := Self;

    FIrrigationEfficiencyDlg.Show;
    RefreshDlgXML(FIrrigationEfficiencyDlg.FXMLAgent, AModule, 0);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddIrrigationCropsDialog (AHostDialog : THostDlg; AModule : INetworkModule) : Boolean;
const OPNAME = 'THydrologyModel.AddIrrigationCropsDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'IrrigationCrops';
    LTabSheet.Caption     := 'Crops';

    FIrrigationCropsDlg                   := TIrrigationCropsDlg.Create(AHostDialog);
    FIrrigationCropsDlg.Parent            := LTabSheet;
    FIrrigationCropsDlg.Align             := alClient;
    FIrrigationCropsDlg.FHydrologyModel   := Self;
    FIrrigationCropsDlg.FMayChangeNetwork := FMayChangeNetwork;
    FIrrigationCropsDlg.FXMLAgent.FHydrologyModel := Self;

    FIrrigationCropsDlg.Show;
    RefreshDlgXML(FIrrigationCropsDlg.FXMLAgent, AModule, 0);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.ShowMineModuleDialog (AModuleNo: Integer): WordBool;
const OPNAME = 'THydrologyModel.ShowMineModuleDialog';
var
  LMineModule : IMineModule;
  LHostDialog : THostDlg;
begin
  Result := FALSE;
  try
    LMineModule := Network.MineModuleAgent.MineModuleByNumber[AModuleNo];
    LHostDialog := THostDlg.Create(nil);
    LHostDialog.ClientHeight := FHostHeightMine;
    LHostDialog.ClientWidth  := FHostWidthMine;
    LHostDialog.Caption := 'Mine Module MM' + IntToStr(AModuleNo);
    try
      if ((AddMinePropertiesDialog(LHostDialog, LMineModule)) AND
          (AddMinePlantAreaDialog(LHostDialog, LMineModule)) AND
          (AddPanDataDialog(LHostDialog, LMineModule))) then
        LHostDialog.ShowModal;
    finally
      FHostHeightMine := LHostDialog.ClientHeight;
      FHostWidthMine  := LHostDialog.ClientWidth;
      FreeAndNil(LHostDialog);
      FMinePropertiesDlg := nil;
      FMinePlantAreaDlg  := nil;
      FPanDataDlg        := nil;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddMinePropertiesDialog (AHostDialog : THostDlg; AModule : INetworkModule) : Boolean;
const OPNAME = 'THydrologyModel.AddMinePropertiesDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'MineProperties';
    LTabSheet.Caption     := 'Properties';

    FMinePropertiesDlg                   := TMinePropertiesDlg.Create(AHostDialog);
    FMinePropertiesDlg.Parent            := LTabSheet;
    FMinePropertiesDlg.Align             := alClient;
    FMinePropertiesDlg.FHydrologyModel   := Self;
    FMinePropertiesDlg.FMayChangeNetwork := FMayChangeNetwork;
    FMinePropertiesDlg.FXMLAgent.FHydrologyModel := Self;

    RefreshDlgXML(FMinePropertiesDlg.FXMLAgent, AModule, 0);
    FMinePropertiesDlg.Show;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.ShowMineSectionsDialog (AModuleNo: Integer): WordBool;
const OPNAME = 'THydrologyModel.ShowMineSectionsDialog';
var
  LMineModule : IMineModule;
begin
  Result := FALSE;
  try
    LMineModule := Network.MineModuleAgent.MineModuleByNumber[AModuleNo];

    FMineSectionsDlg                   := TMineSectionsDlg.Create(nil);
    FMineSectionsDlg.FHydrologyModel   := Self;
    FMineSectionsDlg.FMayChangeNetwork := FMayChangeNetwork;
    FMineSectionsDlg.FXMLAgent.FHydrologyModel := Self;
    
    RefreshDlgXML(FMineSectionsDlg.FXMLAgent, LMineModule, 0);
    try
      FMineSectionsDlg.ShowModal;
    finally
      FMineSectionsDlg := nil;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddMinePlantAreaDialog (AHostDialog : THostDlg; AModule : INetworkModule) : Boolean;
const OPNAME = 'THydrologyModel.AddMinePlantAreaDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'MinePlantArea';
    LTabSheet.Caption     := 'Plant area growth';

    FMinePlantAreaDlg                   := TMinePlantAreaDlg.Create(AHostDialog);
    FMinePlantAreaDlg.Parent            := LTabSheet;
    FMinePlantAreaDlg.Align             := alClient;
    FMinePlantAreaDlg.FHydrologyModel   := Self;
    FMinePlantAreaDlg.FMayChangeNetwork := FMayChangeNetwork;
    FMinePlantAreaDlg.FXMLAgent.FHydrologyModel := Self;

    RefreshDlgXML(FMinePlantAreaDlg.FXMLAgent, AModule, 0);
    FMinePlantAreaDlg.Show;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.ShowMineOpencastPitDialog (AModuleNo  : Integer;
                                                    ASectionNo : Integer): WordBool;
const OPNAME = 'THydrologyModel.ShowMineOpencastPitDialog';
var
  LMineModule  : IMineModule;
  LOpencastPit : IOpencastPit;
  LHostDialog  : THostDlg;
begin
  Result := FALSE;
  try
    LMineModule  := Network.MineModuleAgent.MineModuleByNumber[AModuleNo];
    LOpencastPit := LMineModule.OpencastPitBySectionNo[ASectionNo];
    LHostDialog := THostDlg.Create(nil);
    LHostDialog.ClientHeight := FHostHeightMineOpencastPit;
    LHostDialog.ClientWidth  := FHostWidthMineOpencastPit;
    LHostDialog.Caption := 'Opencast pit: ' + LOpencastPit.SectionName + ' (Mine Module MM' + IntToStr(AModuleNo) + ')';
    try
      if ((AddMineOpencastPitPropertiesDialog(LHostDialog, LMineModule, ASectionNo))      AND
          (AddMineOpencastPitWorkingsDialog(LHostDialog, LMineModule, ASectionNo))        AND
          (AddMineOpencastPitDisturbedDialog(LHostDialog, LMineModule, ASectionNo))       AND
          (AddMineOpencastPitRehabilitatedDialog(LHostDialog, LMineModule, ASectionNo))   AND
          (AddMineOpencastPitEvaporationDialog(LHostDialog, LMineModule, ASectionNo))     AND
          (AddMineOpencastPitDecantDialog(LHostDialog, LMineModule, ASectionNo))          AND
          (AddMineOpencastPitSeepageDialog(LHostDialog, LMineModule, ASectionNo))         AND
          (AddMineOpencastPitRechargeFactorsDialog(LHostDialog, LMineModule, ASectionNo)) AND
          (AddMineOpencastPitWorkingsQSLDDialog(LHostDialog, LMineModule, ASectionNo))    AND
          (AddMineOpencastPitSeepDecantQSLDDialog(LHostDialog, LMineModule, ASectionNo))) then
        LHostDialog.ShowModal;
    finally
      FHostHeightMineOpencastPit := LHostDialog.ClientHeight;
      FHostWidthMineOpencastPit  := LHostDialog.ClientWidth;
      FreeAndNil(LHostDialog);
      FMineOpencastPitPropertiesDlg      := nil;
      FMineOpencastPitWorkingsDlg        := nil;
      FMineOpencastPitDisturbedDlg       := nil;
      FMineOpencastPitRehabilitatedDlg   := nil;
      FMineOpencastPitEvaporationDlg     := nil;
      FMineOpencastPitDecantDlg          := nil;
      FMineOpencastPitSeepageDlg         := nil;
      FMineOpencastPitRechargeFactorsDlg := nil;
      FMineOpencastPitWorkingsQSLDDlg    := nil;
      FMineOpencastPitSeepDecantQSLDDlg  := nil;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddMineOpencastPitPropertiesDialog(AHostDialog : THostDlg;
                                                            AModule     : INetworkModule;
                                                            ASectionNo  : Integer): Boolean;
const OPNAME = 'THydrologyModel.AddMineOpencastPitPropertiesDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'MineOpencastPitProperties';
    LTabSheet.Caption     := 'Properties';

    FMineOpencastPitPropertiesDlg                    := TMineOpencastPitPropertiesDlg.Create(AHostDialog);
    FMineOpencastPitPropertiesDlg.Parent             := LTabSheet;
    FMineOpencastPitPropertiesDlg.Align              := alClient;
    FMineOpencastPitPropertiesDlg.FHydrologyModel    := Self;
    FMineOpencastPitPropertiesDlg.FMayChangeNetwork  := FMayChangeNetwork;
    FMineOpencastPitPropertiesDlg.FXMLAgent.FHydrologyModel := Self;

    RefreshDlgXML(FMineOpencastPitPropertiesDlg.FXMLAgent, AModule, ASectionNo);
    FMineOpencastPitPropertiesDlg.Show;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddMineOpencastPitWorkingsDialog(AHostDialog : THostDlg;
                                                          AModule     : INetworkModule;
                                                          ASectionNo  : Integer): Boolean;
const OPNAME = 'THydrologyModel.AddMineOpencastPitWorkingsDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'MineOpencastPitWorkings';
    LTabSheet.Caption     := 'Workings area growth';

    FMineOpencastPitWorkingsDlg                   := TMineOpencastPitWorkingsDlg.Create(AHostDialog);
    FMineOpencastPitWorkingsDlg.Parent            := LTabSheet;
    FMineOpencastPitWorkingsDlg.Align             := alClient;
    FMineOpencastPitWorkingsDlg.FHydrologyModel   := Self;
    FMineOpencastPitWorkingsDlg.FMayChangeNetwork := FMayChangeNetwork;
    FMineOpencastPitWorkingsDlg.FXMLAgent.FHydrologyModel := Self;

    RefreshDlgXML(FMineOpencastPitWorkingsDlg.FXMLAgent, AModule, ASectionNo);
    FMineOpencastPitWorkingsDlg.Show;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddMineOpencastPitDisturbedDialog(AHostDialog : THostDlg;
                                                           AModule     : INetworkModule;
                                                           ASectionNo  : Integer): Boolean;
const OPNAME = 'THydrologyModel.AddMineOpencastPitDisturbedDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'MineOpencastPitDisturbed';
    LTabSheet.Caption     := 'Disturbed area growth';

    FMineOpencastPitDisturbedDlg                   := TMineOpencastPitDisturbedDlg.Create(AHostDialog);
    FMineOpencastPitDisturbedDlg.Parent            := LTabSheet;
    FMineOpencastPitDisturbedDlg.Align             := alClient;
    FMineOpencastPitDisturbedDlg.FHydrologyModel   := Self;
    FMineOpencastPitDisturbedDlg.FMayChangeNetwork := FMayChangeNetwork;
    FMineOpencastPitDisturbedDlg.FXMLAgent.FHydrologyModel := Self;

    RefreshDlgXML(FMineOpencastPitDisturbedDlg.FXMLAgent, AModule, ASectionNo);
    FMineOpencastPitDisturbedDlg.Show;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddMineOpencastPitRehabilitatedDialog (AHostDialog : THostDlg;
                                                                AModule     : INetworkModule;
                                                                ASectionNo  : Integer): Boolean;
const OPNAME = 'THydrologyModel.AddMineOpencastPitRehabilitatedDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'MineOpencastPitRehabilitated';
    LTabSheet.Caption     := 'Rehabilitated area growth';

    FMineOpencastPitRehabilitatedDlg                   := TMineOpencastPitRehabilitatedDlg.Create(AHostDialog);
    FMineOpencastPitRehabilitatedDlg.Parent            := LTabSheet;
    FMineOpencastPitRehabilitatedDlg.Align             := alClient;
    FMineOpencastPitRehabilitatedDlg.FHydrologyModel   := Self;
    FMineOpencastPitRehabilitatedDlg.FMayChangeNetwork := FMayChangeNetwork;
    FMineOpencastPitRehabilitatedDlg.FXMLAgent.FHydrologyModel := Self;

    RefreshDlgXML(FMineOpencastPitRehabilitatedDlg.FXMLAgent, AModule, ASectionNo);
    FMineOpencastPitRehabilitatedDlg.Show;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddMineOpencastPitEvaporationDialog (AHostDialog : THostDlg;
                                                              AModule     : INetworkModule;
                                                              ASectionNo  : Integer): Boolean;
const OPNAME = 'THydrologyModel.AddMineOpencastPitEvaporationDialog';
var
  LTabSheet  : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'MineOpencastPitEvaporation';
    LTabSheet.Caption     := 'Pit evaporation area growth';

    FMineOpencastPitEvaporationDlg                    := TMineOpencastPitEvaporationDlg.Create(AHostDialog);
    FMineOpencastPitEvaporationDlg.Parent             := LTabSheet;
    FMineOpencastPitEvaporationDlg.Align              := alClient;
    FMineOpencastPitEvaporationDlg.FHydrologyModel    := Self;
    FMineOpencastPitEvaporationDlg.FMayChangeNetwork  := FMayChangeNetwork;
    FMineOpencastPitEvaporationDlg.FXMLAgent.FHydrologyModel := Self;

    RefreshDlgXML(FMineOpencastPitEvaporationDlg.FXMLAgent, AModule, ASectionNo);
    FMineOpencastPitEvaporationDlg.Show;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddMineOpencastPitDecantDialog (AHostDialog : THostDlg;
                                                         AModule     : INetworkModule;
                                                         ASectionNo  : Integer): Boolean;
const OPNAME = 'THydrologyModel.AddMineOpencastPitDecantDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'MineOpencastPitDecant';
    LTabSheet.Caption     := 'Inspoils decant growth';

    FMineOpencastPitDecantDlg                   := TMineOpencastPitDecantDlg.Create(AHostDialog);
    FMineOpencastPitDecantDlg.Parent            := LTabSheet;
    FMineOpencastPitDecantDlg.Align             := alClient;
    FMineOpencastPitDecantDlg.FHydrologyModel   := Self;
    FMineOpencastPitDecantDlg.FMayChangeNetwork := FMayChangeNetwork;
    FMineOpencastPitDecantDlg.FXMLAgent.FHydrologyModel := Self;

    RefreshDlgXML(FMineOpencastPitDecantDlg.FXMLAgent, AModule, ASectionNo);
    FMineOpencastPitDecantDlg.Show;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddMineOpencastPitSeepageDialog (AHostDialog : THostDlg;
                                                          AModule     : INetworkModule;
                                                          ASectionNo  : Integer): Boolean;
const OPNAME = 'THydrologyModel.AddMineOpencastPitSeepageDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'MineOpencastPitSeepage';
    LTabSheet.Caption     := 'Inspoils seepage growth';

    FMineOpencastPitSeepageDlg                   := TMineOpencastPitSeepageDlg.Create(AHostDialog);
    FMineOpencastPitSeepageDlg.Parent            := LTabSheet;
    FMineOpencastPitSeepageDlg.Align             := alClient;
    FMineOpencastPitSeepageDlg.FHydrologyModel   := Self;
    FMineOpencastPitSeepageDlg.FMayChangeNetwork := FMayChangeNetwork;
    FMineOpencastPitSeepageDlg.FXMLAgent.FHydrologyModel := Self;

    RefreshDlgXML(FMineOpencastPitSeepageDlg.FXMLAgent, AModule, ASectionNo);
    FMineOpencastPitSeepageDlg.Show;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddMineOpencastPitRechargeFactorsDialog (AHostDialog : THostDlg;
                                                                  AModule     : INetworkModule;
                                                                  ASectionNo  : Integer): Boolean;
const OPNAME = 'THydrologyModel.AddMineOpencastPitRechargeFactorsDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'MineOpencastPitRechargeFactors';
    LTabSheet.Caption     := 'Monthly recharge factors';

    FMineOpencastPitRechargeFactorsDlg                   := TMineOpencastPitRechargeFactorsDlg.Create(AHostDialog);
    FMineOpencastPitRechargeFactorsDlg.Parent            := LTabSheet;
    FMineOpencastPitRechargeFactorsDlg.Align             := alClient;
    FMineOpencastPitRechargeFactorsDlg.FHydrologyModel   := Self;
    FMineOpencastPitRechargeFactorsDlg.FMayChangeNetwork := FMayChangeNetwork;
    FMineOpencastPitRechargeFactorsDlg.FXMLAgent.FHydrologyModel := Self;

    RefreshDlgXML(FMineOpencastPitRechargeFactorsDlg.FXMLAgent, AModule, ASectionNo);
    FMineOpencastPitRechargeFactorsDlg.Show;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddMineOpencastPitWorkingsQSLDDialog (AHostDialog : THostDlg;
                                                               AModule     : INetworkModule;
                                                               ASectionNo  : Integer): Boolean;
const OPNAME = 'THydrologyModel.AddMineOpencastPitWorkingsQSLDDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'MineOpencastPitWorkingsQSLD';
    LTabSheet.Caption     := 'Workings area Q vs SLD';

    FMineOpencastPitWorkingsQSLDDlg                   := TMineOpencastPitWorkingsQSLDDlg.Create(AHostDialog);
    FMineOpencastPitWorkingsQSLDDlg.Parent            := LTabSheet;
    FMineOpencastPitWorkingsQSLDDlg.Align             := alClient;
    FMineOpencastPitWorkingsQSLDDlg.FHydrologyModel   := Self;
    FMineOpencastPitWorkingsQSLDDlg.FMayChangeNetwork := FMayChangeNetwork;
    FMineOpencastPitWorkingsQSLDDlg.FXMLAgent.FHydrologyModel := Self;

    RefreshDlgXML(FMineOpencastPitWorkingsQSLDDlg.FXMLAgent, AModule, ASectionNo);
    FMineOpencastPitWorkingsQSLDDlg.Show;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddMineOpencastPitSeepDecantQSLDDialog (AHostDialog : THostDlg;
                                                                 AModule     : INetworkModule;
                                                                 ASectionNo  : Integer): Boolean;
const OPNAME = 'THydrologyModel.AddMineOpencastPitSeepDecantQSLDDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'MineOpencastPitSeepDecantQSLD';
    LTabSheet.Caption     := 'Seep and Decant Q vs SLD';

    FMineOpencastPitSeepDecantQSLDDlg                   := TMineOpencastPitSeepDecantQSLDDlg.Create(AHostDialog);
    FMineOpencastPitSeepDecantQSLDDlg.Parent            := LTabSheet;
    FMineOpencastPitSeepDecantQSLDDlg.Align             := alClient;
    FMineOpencastPitSeepDecantQSLDDlg.FHydrologyModel   := Self;
    FMineOpencastPitSeepDecantQSLDDlg.FMayChangeNetwork := FMayChangeNetwork;
    FMineOpencastPitSeepDecantQSLDDlg.FXMLAgent.FHydrologyModel := Self;

    RefreshDlgXML(FMineOpencastPitSeepDecantQSLDDlg.FXMLAgent, AModule, ASectionNo);
    FMineOpencastPitSeepDecantQSLDDlg.Show;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.ShowMineUndergroundSectionDialog (AModuleNo  : Integer;
                                                           ASectionNo : Integer): WordBool;
const OPNAME = 'THydrologyModel.ShowMineUndergroundSectionDialog';
var
  LMineModule  : IMineModule;
  LUnderground : IUndergroundSection;
  LHostDialog  : THostDlg;
begin
  Result := FALSE;
  try
    LMineModule  := Network.MineModuleAgent.MineModuleByNumber[AModuleNo];
    LUnderground := LMineModule.UndergroundSectionBySectionNo[ASectionNo];
    LHostDialog  := THostDlg.Create(nil);
    LHostDialog.ClientHeight := FHostHeightMineUnderground;
    LHostDialog.ClientWidth  := FHostWidthMineUnderground;
    LHostDialog.Caption := 'Underground section: ' + LUnderground.SectionName + ' (Mine Module MM' + IntToStr(AModuleNo) + ')';
    try
      if ((AddMineUndergroundPropertiesDialog(LHostDialog, LMineModule, ASectionNo))      AND
          (AddMineUndergroundBoardPillarDialog(LHostDialog, LMineModule, ASectionNo))     AND
          (AddMineUndergroundHighExtractionDialog(LHostDialog, LMineModule, ASectionNo))  AND
          (AddMineUndergroundRechargeFactorsDialog(LHostDialog, LMineModule, ASectionNo)) AND
          (AddMineUndergroundQSLDDialog(LHostDialog, LMineModule, ASectionNo))) then
        LHostDialog.ShowModal;
    finally
      FreeAndNil(LHostDialog);
      FMineUndergroundPropertiesDlg      := nil;
      FMineUndergroundBoardPillarDlg     := nil;
      FMineUndergroundHighExtractionDlg  := nil;
      FMineUndergroundRechargeFactorsDlg := nil;
      FMineUndergroundQSLDDlg            := nil;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddMineUndergroundPropertiesDialog(AHostDialog : THostDlg;
                                                            AModule     : INetworkModule;
                                                            ASectionNo  : Integer): Boolean;
const OPNAME = 'THydrologyModel.AddMineUndergroundPropertiesDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'MineUndergroundProperties';
    LTabSheet.Caption     := 'Properties';

    FMineUndergroundPropertiesDlg                   := TMineUndergroundPropertiesDlg.Create(AHostDialog);
    FMineUndergroundPropertiesDlg.Parent            := LTabSheet;
    FMineUndergroundPropertiesDlg.Align             := alClient;
    FMineUndergroundPropertiesDlg.FHydrologyModel   := Self;
    FMineUndergroundPropertiesDlg.FMayChangeNetwork := FMayChangeNetwork;
    FMineUndergroundPropertiesDlg.FXMLAgent.FHydrologyModel := Self;

    RefreshDlgXML(FMineUndergroundPropertiesDlg.FXMLAgent, AModule, ASectionNo);
    FMineUndergroundPropertiesDlg.Show;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddMineUndergroundBoardPillarDialog(AHostDialog : THostDlg;
                                                             AModule     : INetworkModule;
                                                             ASectionNo  : Integer): Boolean;
const OPNAME = 'THydrologyModel.AddMineUndergroundBoardPillarDialog';
var
  LTabSheet  : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'MineUndergroundBoardPillar';
    LTabSheet.Caption     := 'Board and Pillar';

    FMineUndergroundBoardPillarDlg                    := TMineUndergroundBoardPillarDlg.Create(AHostDialog);
    FMineUndergroundBoardPillarDlg.Parent             := LTabSheet;
    FMineUndergroundBoardPillarDlg.Align              := alClient;
    FMineUndergroundBoardPillarDlg.FHydrologyModel    := Self;
    FMineUndergroundBoardPillarDlg.FMayChangeNetwork  := FMayChangeNetwork;
    FMineUndergroundBoardPillarDlg.FXMLAgent.FHydrologyModel := Self;

    RefreshDlgXML(FMineUndergroundBoardPillarDlg.FXMLAgent, AModule, ASectionNo);
    FMineUndergroundBoardPillarDlg.Show;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddMineUndergroundHighExtractionDialog (AHostDialog : THostDlg;
                                                                 AModule     : INetworkModule;
                                                                 ASectionNo  : Integer): Boolean;
const OPNAME = 'THydrologyModel.AddMineUndergroundHighExtractionDialog';
var
  LTabSheet  : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'MineUndergroundHighExtraction';
    LTabSheet.Caption     := 'High extraction';

    FMineUndergroundHighExtractionDlg := TMineUndergroundHighExtractionDlg.Create(AHostDialog);
    FMineUndergroundHighExtractionDlg.Parent            := LTabSheet;
    FMineUndergroundHighExtractionDlg.Align             := alClient;
    FMineUndergroundHighExtractionDlg.FHydrologyModel   := Self;
    FMineUndergroundHighExtractionDlg.FMayChangeNetwork := FMayChangeNetwork;
    FMineUndergroundHighExtractionDlg.FXMLAgent.FHydrologyModel := Self;

    RefreshDlgXML(FMineUndergroundHighExtractionDlg.FXMLAgent, AModule, ASectionNo);
    FMineUndergroundHighExtractionDlg.Show;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddMineUndergroundRechargeFactorsDialog (AHostDialog : THostDlg;
                                                                  AModule     : INetworkModule;
                                                                  ASectionNo  : Integer): Boolean;
const OPNAME = 'THydrologyModel.AddMineUndergroundRechargeFactorsDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'MineUndergroundRechargeFactors';
    LTabSheet.Caption     := 'Recharge factors';

    FMineUndergroundRechargeFactorsDlg                   := TMineUndergroundRechargeFactorsDlg.Create(AHostDialog);
    FMineUndergroundRechargeFactorsDlg.Parent            := LTabSheet;
    FMineUndergroundRechargeFactorsDlg.Align             := alClient;
    FMineUndergroundRechargeFactorsDlg.FHydrologyModel   := Self;
    FMineUndergroundRechargeFactorsDlg.FMayChangeNetwork := FMayChangeNetwork;
    FMineUndergroundRechargeFactorsDlg.FXMLAgent.FHydrologyModel := Self;

    RefreshDlgXML(FMineUndergroundRechargeFactorsDlg.FXMLAgent, AModule, ASectionNo);
    FMineUndergroundRechargeFactorsDlg.Show;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddMineUndergroundQSLDDialog (AHostDialog : THostDlg;
                                                       AModule     : INetworkModule;
                                                       ASectionNo  : Integer): Boolean;
const OPNAME = 'THydrologyModel.AddMineUndergroundQSLDDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'MineUndergroundQSLD';
    LTabSheet.Caption     := 'Q vs SLD';

    FMineUndergroundQSLDDlg                   := TMineUndergroundQSLDDlg.Create(AHostDialog);
    FMineUndergroundQSLDDlg.Parent            := LTabSheet;
    FMineUndergroundQSLDDlg.Align             := alClient;
    FMineUndergroundQSLDDlg.FHydrologyModel   := Self;
    FMineUndergroundQSLDDlg.FMayChangeNetwork := FMayChangeNetwork;
    FMineUndergroundQSLDDlg.FXMLAgent.FHydrologyModel := Self;

    RefreshDlgXML(FMineUndergroundQSLDDlg.FXMLAgent, AModule, ASectionNo);
    FMineUndergroundQSLDDlg.Show;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.ShowMineSlurryDumpDialog (AModuleNo  : Integer;
                                                   ASectionNo : Integer): WordBool;
const OPNAME = 'THydrologyModel.ShowMineSlurryDumpDialog';
var
  LMineModule  : IMineModule;
  LSlurryDump  : ISlurryDump;
  LHostDialog  : THostDlg;
begin
  Result := FALSE;
  try
    LMineModule  := Network.MineModuleAgent.MineModuleByNumber[AModuleNo];
    LSlurryDump := LMineModule.SlurryDumpBySectionNo[ASectionNo];
    LHostDialog := THostDlg.Create(nil);
    LHostDialog.ClientHeight := FHostHeightMineUnderground;
    LHostDialog.ClientWidth  := FHostWidthMineUnderground;
    LHostDialog.Caption := 'Slurry dump : ' + LSlurryDump.SectionName + ' (Mine Module MM' + IntToStr(AModuleNo) + ')';
    try
      if ((AddMineSlurryDumpPropertiesDialog(LHostDialog, LMineModule, ASectionNo))      AND
          (AddMineSlurryDumpAreaDialog(LHostDialog, LMineModule, ASectionNo))            AND
          (AddMineSlurryDumpRechargeFactorsDialog(LHostDialog, LMineModule, ASectionNo)) AND
          (AddMineSlurryDumpQSLDDialog(LHostDialog, LMineModule, ASectionNo))) then
        LHostDialog.ShowModal;
    finally
      FreeAndNil(LHostDialog);
      FMineSlurryDumpPropertiesDlg      := nil;
      FMineSlurryDumpAreaDlg            := nil;
      FMineSlurryDumpRechargeFactorsDlg := nil;
      FMineSlurryDumpQSLDDlg            := nil;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddMineSlurryDumpPropertiesDialog (AHostDialog : THostDlg;
                                                            AModule     : INetworkModule;
                                                            ASectionNo  : Integer): Boolean;
const OPNAME = 'THydrologyModel.AddMineSlurryDumpPropertiesDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'MineSlurryDumpProperties';
    LTabSheet.Caption     := 'Properties';

    FMineSlurryDumpPropertiesDlg := TMineSlurryDumpPropertiesDlg.Create(AHostDialog);
    FMineSlurryDumpPropertiesDlg.Parent             := LTabSheet;
    FMineSlurryDumpPropertiesDlg.Align              := alClient;
    FMineSlurryDumpPropertiesDlg.FHydrologyModel    := Self;
    FMineSlurryDumpPropertiesDlg.FMayChangeNetwork  := FMayChangeNetwork;
    FMineSlurryDumpPropertiesDlg.FXMLAgent.FHydrologyModel := Self;

    RefreshDlgXML(FMineSlurryDumpPropertiesDlg.FXMLAgent, AModule, ASectionNo);
    FMineSlurryDumpPropertiesDlg.Show;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddMineSlurryDumpAreaDialog(AHostDialog : THostDlg;
                                                     AModule     : INetworkModule;
                                                     ASectionNo  : Integer): Boolean;
const OPNAME = 'THydrologyModel.AddMineSlurryDumpAreaDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'MineSlurryDumpArea';
    LTabSheet.Caption     := 'Area growth data';

    FMineSlurryDumpAreaDlg                   := TMineSlurryDumpAreaDlg.Create(AHostDialog);
    FMineSlurryDumpAreaDlg.Parent            := LTabSheet;
    FMineSlurryDumpAreaDlg.Align             := alClient;
    FMineSlurryDumpAreaDlg.FHydrologyModel   := Self;
    FMineSlurryDumpAreaDlg.FMayChangeNetwork := FMayChangeNetwork;
    FMineSlurryDumpAreaDlg.FXMLAgent.FHydrologyModel := Self;

    RefreshDlgXML(FMineSlurryDumpAreaDlg.FXMLAgent, AModule, ASectionNo);
    FMineSlurryDumpAreaDlg.Show;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddMineSlurryDumpRechargeFactorsDialog (AHostDialog : THostDlg;
                                                                 AModule     : INetworkModule;
                                                                 ASectionNo  : Integer): Boolean;
const OPNAME = 'THydrologyModel.AddMineSlurryDumpRechargeFactorsDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'MineSlurryDumpRechargeFactors';
    LTabSheet.Caption     := 'Recharge factors';

    FMineSlurryDumpRechargeFactorsDlg := TMineSlurryDumpRechargeFactorsDlg.Create(AHostDialog);
    FMineSlurryDumpRechargeFactorsDlg.Parent            := LTabSheet;
    FMineSlurryDumpRechargeFactorsDlg.Align             := alClient;
    FMineSlurryDumpRechargeFactorsDlg.FHydrologyModel   := Self;
    FMineSlurryDumpRechargeFactorsDlg.FMayChangeNetwork := FMayChangeNetwork;
    FMineSlurryDumpRechargeFactorsDlg.FXMLAgent.FHydrologyModel := Self;

    RefreshDlgXML(FMineSlurryDumpRechargeFactorsDlg.FXMLAgent, AModule, ASectionNo);
    FMineSlurryDumpRechargeFactorsDlg.Show;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.AddMineSlurryDumpQSLDDialog(AHostDialog : THostDlg;
                                                     AModule     : INetworkModule;
                                                     ASectionNo  : Integer): Boolean;
const OPNAME = 'THydrologyModel.AddMineSlurryDumpQSLDDialog';
var
  LTabSheet : TTabSheet;
begin
  Result := FALSE;
  try
    LTabSheet             := TTabSheet.Create(AHostDialog);
    LTabSheet.PageControl := AHostDialog.PgcHost;
    LTabSheet.Name        := 'MineSlurryDumpQSLD';
    LTabSheet.Caption     := 'Q vs SLD';

    FMineSlurryDumpQSLDDlg                   := TMineSlurryDumpQSLDDlg.Create(AHostDialog);
    FMineSlurryDumpQSLDDlg.Parent            := LTabSheet;
    FMineSlurryDumpQSLDDlg.Align             := alClient;
    FMineSlurryDumpQSLDDlg.FHydrologyModel   := Self;
    FMineSlurryDumpQSLDDlg.FMayChangeNetwork := FMayChangeNetwork;
    FMineSlurryDumpQSLDDlg.FXMLAgent.FHydrologyModel := Self;

    RefreshDlgXML(FMineSlurryDumpQSLDDlg.FXMLAgent, AModule, ASectionNo);
    FMineSlurryDumpQSLDDlg.Show;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateNetworkData (const AXMLOut: WideString): WordBool;
const OPNAME = 'THydrologyModel.UpdateNetworkData';
var
  LXMLAgent   : TXMLAgent;
  LXMLDocOut  : IXMLDocument;
  LRootNode   : IXMLNode;
  LModuleType : String;
  LModuleID   : String;
  LSection    : String;
begin
  try
    LXMLAgent := TXMLAgent.Create;
    LXMLAgent.FHydrologyModel := Self;
    try
      LXMLDocOut  := LXMLAgent.CreateXMLDocument(AXMLOut);
      LRootNode   := LXMLDocOut.DocumentElement;
      LModuleType := LRootNode.ChildNodes['ModuleType'].Text;
      LModuleID   := LRootNode.ChildNodes['Identifier'].Text;
      LSection    := LRootNode.ChildNodes['Section'].Text;
      if ((LModuleType = 'RV') OR (LModuleType = 'CR')) then
      begin
        if (LSection = 'InflowRoutes') then
          Result := UpdateInflowRoutes(LRootNode)
        else if (LSection = 'OutflowRoutes') then
          Result := UpdateOutflowRoutes(LRootNode);
      end;
      if ((LModuleType = 'RV') OR (LModuleType = 'RU') OR (LModuleType = 'CR') OR
          (LModuleType = 'MM') OR (LModuleType = 'RR')) then
      begin
        if (LSection = 'PanData') then
          Result := UpdatePanData(LRootNode)
        else if (LSection = 'ChangeNetworkSequence') then
          Result := UpdateNetworkSequence(LRootNode);
      end;
      if (LModuleType = 'RV') then
      begin
        if (LSection = 'ReservoirProperties') then
          Result := UpdateReservoirProperties(LRootNode)
        else if (LSection = 'ReservoirVolumeArea') then
          Result := UpdateReservoirVolumeArea(LRootNode);
      end
      else if (LModuleType = 'RU') then
      begin
        if (LSection = 'RunOffProperties') then
          Result := UpdateRunOffProperties(LRootNode)
        else if (LSection = 'RunOffSlaves') then
          Result := UpdateRunOffSlaves(LRootNode)
        else if (LSection = 'RunOffPitman') then
          Result := UpdateRunOffPitman(LRootNode)
        else if (LSection = 'RunOffAfforestation') then
          Result := UpdateRunOffAfforestation(LRootNode)
        else if (LSection = 'RunOffAlienVegetation') then
          Result := UpdateRunOffAlienVegetation(LRootNode)
        else if (LSection = 'RunOffPavedArea') then
          Result := UpdateRunOffPavedArea(LRootNode)
        else if (LSection = 'RunOffGroundWaterAbstraction') then
          Result := UpdateRunOffGroundWaterAbstraction(LRootNode)
        else if (LSection = 'RunOffOutflowRoutes') then
          Result := UpdateRunOffOutflowRoutes(LRootNode)
        else if (LSection = 'RunOffHughes') then
          Result := UpdateRunOffHughes(LRootNode)
        else if (LSection = 'RunOffSami') then
          Result := UpdateRunOffSami(LRootNode);
      end
      else if (LModuleType = 'CR') then
      begin
        if (LSection = 'ChannelProperties') then
          Result := UpdateChannelProperties(LRootNode);
      end
      else if (LModuleType = 'RR') then
      begin
        if (LSection = 'IrrigationProperties') then
          Result := UpdateIrrigationProperties(LRootNode)
        else if (LSection = 'IrrigationWQT') then
          Result := UpdateIrrigationWQT(LRootNode)
        else if (LSection = 'IrrigationFactors') then
          Result := UpdateIrrigationFactors(LRootNode)
        else if (LSection = 'IrrigationArea') then
          Result := UpdateIrrigationArea(LRootNode)
        else if (LSection = 'IrrigationAllocationGrowth') then
          Result := UpdateIrrigationAllocationGrowth(LRootNode)
        else if (LSection = 'IrrigationReturnFlow') then
          Result := UpdateIrrigationReturnFlow(LRootNode)
        else if (LSection = 'IrrigationEfficiency') then
          Result := UpdateIrrigationEfficiency(LRootNode)
        else if (LSection = 'IrrigationCrops') then
          Result := UpdateIrrigationCrops(LRootNode);
      end
      else if (LModuleType = 'MM') then
      begin
        if (LSection = 'MineProperties') then
          Result := UpdateMineProperties(LRootNode)
        else if (LSection = 'MinePlantArea') then
          Result := UpdateMinePlantArea(LRootNode)
        else if (LSection = 'MineSections') then
          Result := UpdateMineSections(LRootNode)
        else if (LSection = 'MineOpencastPitProperties') then
          Result := UpdateMineOpencastPitProperties(LRootNode)
        else if (LSection = 'MineOpencastPitWorkings') then
          Result := UpdateMineOpencastPitWorkings(LRootNode)
        else if (LSection = 'MineOpencastPitDisturbed') then
          Result := UpdateMineOpencastPitDisturbed(LRootNode)
        else if (LSection = 'MineOpencastPitRehabilitated') then
          Result := UpdateMineOpencastPitRehabilitated(LRootNode)
        else if (LSection = 'MineOpencastPitEvaporation') then
          Result := UpdateMineOpencastPitEvaporation(LRootNode)
        else if (LSection = 'MineOpencastPitDecant') then
          Result := UpdateMineOpencastPitDecant(LRootNode)
        else if (LSection = 'MineOpencastPitSeepage') then
          Result := UpdateMineOpencastPitSeepage(LRootNode)
        else if (LSection = 'MineOpencastPitRechargeFactors') then
          Result := UpdateMineOpencastPitRechargeFactors(LRootNode)
        else if (LSection = 'MineOpencastPitWorkingsQSLD') then
          Result := UpdateMineOpencastPitWorkingsQSLD(LRootNode)
        else if (LSection = 'MineOpencastPitSeepDecantQSLD') then
          Result := UpdateMineOpencastPitSeepDecantQSLD(LRootNode)
        else if (LSection = 'MineUndergroundProperties') then
          Result := UpdateMineUndergroundProperties(LRootNode)
        else if (LSection = 'MineUndergroundBoardPillar') then
          Result := UpdateMineUndergroundBoardPillar(LRootNode)
        else if (LSection = 'MineUndergroundHighExtraction') then
          Result := UpdateMineUndergroundHighExtraction(LRootNode)
        else if (LSection = 'MineUndergroundRechargeFactors') then
          Result := UpdateMineUndergroundRechargeFactors(LRootNode)
        else if (LSection = 'MineUndergroundQSLD') then
          Result := UpdateMineUndergroundQSLD(LRootNode)
        else if (LSection = 'MineSlurryDumpProperties') then
          Result := UpdateMineSlurryDumpProperties(LRootNode)
        else if (LSection = 'MineSlurryDumpArea') then
          Result := UpdateMineSlurryDumpArea(LRootNode)
        else if (LSection = 'MineSlurryDumpRechargeFactors') then
          Result := UpdateMineSlurryDumpRechargeFactors(LRootNode)
        else if (LSection = 'MineSlurryDumpQSLD') then
          Result := UpdateMineSlurryDumpQSLD(LRootNode)
      end
      else if (LModuleType = 'RQ'{'Route'}) then
      begin
        if (LSection = 'NetworkRoute') then
          Result := UpdateNetworkRoute(LRootNode);
      end
      else if (LModuleType = 'Obs') then
      begin
        if (LSection = 'ObservationPoint') then
          Result := UpdateObservationPoint(LRootNode);
      end;
    finally
      LXMLAgent.Free;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateInflowRoutes (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateInflowRoutes';
var
  LModuleType : String;
  LModuleID   : Integer;
  LReservoir  : TReservoirModule;
  LChannel    : TChannelModule;
begin
  Result := FALSE;
  try
    LModuleType := ARootNode.ChildNodes['ModuleType'].Text;
    LModuleID   := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    if (LModuleType = 'RV') then
    begin
      LReservoir := FNetwork.FindReservoirModuleAgent.FindReservoirModuleByID(LModuleID);
      Result     := LReservoir.UpdateInflowRoutesData(ARootNode);
      if (FInflowRoutesDlg <> nil) then
        RefreshDlgXML(FInflowRoutesDlg.FXMLAgent, LReservoir, 0);
      Result := TRUE;
    end
    else if (LModuleType = 'CR') then
    begin
      LChannel := FNetwork.FindChannelModuleAgent.FindChannelModuleByID(LModuleID);
      Result   := LChannel.UpdateInflowRoutesData(ARootNode);
      if (FInflowRoutesDlg <> nil) then
        RefreshDlgXML(FInflowRoutesDlg.FXMLAgent, LChannel, 0);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateOutflowRoutes (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateOutflowRoutes';
var
  LModuleType : String;
  LModuleID   : Integer;
  LReservoir  : TReservoirModule;
  LChannel    : TChannelModule;
begin
  Result := FALSE;
  try
    LModuleType := ARootNode.ChildNodes['ModuleType'].Text;
    LModuleID   := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    if (LModuleType = 'RV') then
    begin
      LReservoir := FNetwork.FindReservoirModuleAgent.FindReservoirModuleByID(LModuleID);
      Result     := LReservoir.UpdateOutflowRoutesData(ARootNode);
      if (FOutflowRoutesDlg <> nil) then
        RefreshDlgXML(FOutflowRoutesDlg.FXMLAgent, LReservoir, 0);
      Result := TRUE;
    end
    else if (LModuleType = 'CR') then
    begin
      LChannel := FNetwork.FindChannelModuleAgent.FindChannelModuleByID(LModuleID);
      Result   := LChannel.UpdateOutflowRoutesData(ARootNode);
      if (FOutflowRoutesDlg <> nil) then
        RefreshDlgXML(FOutflowRoutesDlg.FXMLAgent, LChannel, 0);
      Result := TRUE;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdatePanData (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdatePanData';
var
  LModuleID : Integer;
  LModule   : TNetworkModule;
begin
  Result := FALSE;
  try
    LModuleID  := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LModule    := FNetwork.FindModuleWithID(LModuleID);
    Result     := LModule.UpdatePanData(ARootNode);
    if (FPanDataDlg <> nil) then
      RefreshDlgXML(FPanDataDlg.FXMLAgent, LModule, 0);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateReservoirProperties (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateReservoirProperties';
var
  LModuleID   : Integer;
  LReservoir  : TReservoirModule;
begin
  Result := FALSE;
  try
    LModuleID  := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LReservoir := FNetwork.FindReservoirModuleAgent.FindReservoirModuleByID(LModuleID);
    Result     := LReservoir.UpdatePropertiesData(ARootNode);
    if (FReservoirPropertiesDlg <> nil) then
    RefreshDlgXML(FReservoirPropertiesDlg.FXMLAgent, LReservoir, 0);
    StudyDataHasChanged(sdccEdit, 'RV', IntToStr(LReservoir.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateReservoirVolumeArea (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateReservoirVolumeArea';
var
  LModuleID   : Integer;
  LReservoir  : TReservoirModule;
begin
  Result := FALSE;
  try
    LModuleID  := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LReservoir := FNetwork.FindReservoirModuleAgent.FindReservoirModuleByID(LModuleID);
    Result     := LReservoir.UpdateVolumeAreaData(ARootNode);
    if (FReservoirVolumeAreaDlg <> nil) then
      RefreshDlgXML(FReservoirVolumeAreaDlg.FXMLAgent, LReservoir, 0);
    StudyDataHasChanged(sdccEdit, 'RV', IntToStr(LReservoir.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateNetworkRoute (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateNetworkRoute';
var
  LRouteNo      : Integer;
  LNetworkRoute : TNetworkRoute;
begin
  Result := FALSE;
  try
    LRouteNo  := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LNetworkRoute := FNetwork.FindNetworkRouteAgent.FindNetworkRouteByNo(LRouteNo);
    Result        := LNetworkRoute.UpdatePropertiesData(ARootNode);
    if (FNetworkRouteDlg <> nil) then
      RefreshRouteDlgXML(FNetworkRouteDlg.FXMLAgent, LNetworkRoute);
    StudyDataHasChanged(sdccEdit, 'RQ'{'Route'}, IntToStr(LNetworkRoute.RouteNo), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateObservationPoint (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateObservationPoint';
var
  LRouteNo  : Integer;
  LObsPoint : TObservationPoint;
begin
  Result := FALSE;
  try
    LRouteNo  := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LObsPoint := FNetwork.FindObservationPointAgent.FindObservationPointByRouteNo(LRouteNo);
    Result    := LObsPoint.UpdatePropertiesData(ARootNode);
    if (FObservationPointDlg <> nil) then
      RefreshObsPointDlgXML(FObservationPointDlg.FXMLAgent, LObsPoint);
    if (FHydroTimeSeriesDlg <> nil) then
    begin
      RefreshTimeSeriesDlgXML(FHydroTimeSeriesDlg.FXMLAgent, LObsPoint.FlowData, LObsPoint.Name + ' Observed Flow Data');
      FHydroTimeSeriesDlg.PopulateGrid;
    end;
    StudyDataHasChanged(sdccEdit, 'Obs', IntToStr(LObsPoint.RouteNo), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateRunOffProperties (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateRunOffProperties';
var
  LModuleID : Integer;
  LRunOff   : TRunOffModule;
begin
  Result := FALSE;
  try
    LModuleID  := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LRunOff    := FNetwork.FindRunOffModuleAgent.FindRunOffModuleByID(LModuleID);
    Result     := LRunOff.UpdatePropertiesData(ARootNode);
    if (FRunOffPropertiesDlg <> nil) then
      RefreshDlgXML(FRunOffPropertiesDlg.FXMLAgent, LRunOff, 0);
    StudyDataHasChanged(sdccEdit, 'RU', IntToStr(LRunOff.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateRunOffSlaves (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateRunOffSlaves';
var
  LModuleID : Integer;
  LRunOff   : TRunOffModule;
begin
  Result := FALSE;
  try
    LModuleID  := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LRunOff    := FNetwork.FindRunOffModuleAgent.FindRunOffModuleByID(LModuleID);
    Result     := LRunOff.UpdateSlavesData(ARootNode);
    if (FRunOffSlavesDlg <> nil) then
      RefreshDlgXML(FRunOffSlavesDlg.FXMLAgent, LRunOff, 0);
    StudyDataHasChanged(sdccEdit, 'RU', IntToStr(LRunOff.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateRunOffPitman (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateRunOffPitman';
var
  LModuleID : Integer;
  LRunOff   : TRunOffModule;
begin
  Result := FALSE;
  try
    LModuleID  := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LRunOff    := FNetwork.FindRunOffModuleAgent.FindRunOffModuleByID(LModuleID);
    Result     := LRunOff.UpdatePitmanData(ARootNode);
    if (FRunOffPitmanDlg <> nil) then
      RefreshDlgXML(FRunOffPitmanDlg.FXMLAgent, LRunOff, 0);
    StudyDataHasChanged(sdccEdit, 'RU', IntToStr(LRunOff.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateRunOffAfforestation (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateRunOffAfforestation';
var
  LModuleID : Integer;
  LRunOff   : TRunOffModule;
begin
  Result := FALSE;
  try
    LModuleID  := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LRunOff    := FNetwork.FindRunOffModuleAgent.FindRunOffModuleByID(LModuleID);
    Result     := LRunOff.UpdateAfforestationData(ARootNode);
    if (FRunOffAfforestationDlg <> nil) then
      RefreshDlgXML(FRunOffAfforestationDlg.FXMLAgent, LRunOff, 0);
    StudyDataHasChanged(sdccEdit, 'RU', IntToStr(LRunOff.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateRunOffAlienVegetation (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateRunOffAlienVegetation';
var
  LModuleID : Integer;
  LRunOff   : TRunOffModule;
begin
  Result := FALSE;
  try
    LModuleID  := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LRunOff    := FNetwork.FindRunOffModuleAgent.FindRunOffModuleByID(LModuleID);
    Result     := LRunOff.UpdateAlienVegetationData(ARootNode);
    if (FRunOffAlienVegetationDlg <> nil) then
      RefreshDlgXML(FRunOffAlienVegetationDlg.FXMLAgent, LRunOff, 0);
    StudyDataHasChanged(sdccEdit, 'RU', IntToStr(LRunOff.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateRunOffPavedArea (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateRunOffPavedArea';
var
  LModuleID : Integer;
  LRunOff   : TRunOffModule;
begin
  Result := FALSE;
  try
    LModuleID  := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LRunOff    := FNetwork.FindRunOffModuleAgent.FindRunOffModuleByID(LModuleID);
    Result     := LRunOff.UpdatePavedAreaData(ARootNode);
    if (FRunOffPavedAreaDlg <> nil) then
      RefreshDlgXML(FRunOffPavedAreaDlg.FXMLAgent, LRunOff, 0);
    StudyDataHasChanged(sdccEdit, 'RU', IntToStr(LRunOff.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateRunOffGroundWaterAbstraction (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateRunOffGroundWaterAbstraction';
var
  LModuleID : Integer;
  LRunOff   : TRunOffModule;
begin
  Result := FALSE;
  try
    LModuleID  := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LRunOff    := FNetwork.FindRunOffModuleAgent.FindRunOffModuleByID(LModuleID);
    Result     := LRunOff.UpdateGroundWaterAbstractionData(ARootNode);
    if (FRunOffGroundWaterAbstractionDlg <> nil) then
      RefreshDlgXML(FRunOffGroundWaterAbstractionDlg.FXMLAgent, LRunOff, 0);
    StudyDataHasChanged(sdccEdit, 'RU', IntToStr(LRunOff.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateRunOffOutflowRoutes (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateRunOffOutflowRoutes';
var
  LModuleID : Integer;
  LRunOff   : TRunOffModule;
begin
  Result := FALSE;
  try
    LModuleID  := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LRunOff    := FNetwork.FindRunOffModuleAgent.FindRunOffModuleByID(LModuleID);
    Result     := LRunOff.UpdateOutflowRoutesData(ARootNode);
    if (FRunOffOutflowRoutesDlg <> nil) then
      RefreshDlgXML(FRunOffOutflowRoutesDlg.FXMLAgent, LRunOff, 0);
    StudyDataHasChanged(sdccEdit, 'RU', IntToStr(LRunOff.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateRunOffHughes (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateRunOffHughes';
var
  LModuleID : Integer;
  LRunOff   : TRunOffModule;
begin
  Result := FALSE;
  try
    LModuleID  := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LRunOff    := FNetwork.FindRunOffModuleAgent.FindRunOffModuleByID(LModuleID);
    Result     := LRunOff.UpdateHughesData(ARootNode);
    if (FRunOffHughesDlg <> nil) then
      RefreshDlgXML(FRunOffHughesDlg.FXMLAgent, LRunOff, 0);
    StudyDataHasChanged(sdccEdit, 'RU', IntToStr(LRunOff.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateRunOffSami (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateRunOffSami';
var
  LModuleID : Integer;
  LRunOff   : TRunOffModule;
begin
  Result := FALSE;
  try
    LModuleID  := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LRunOff    := FNetwork.FindRunOffModuleAgent.FindRunOffModuleByID(LModuleID);
    Result     := LRunOff.UpdateSamiData(ARootNode);
    if (FRunOffSamiDlg <> nil) then
      RefreshDlgXML(FRunOffSamiDlg.FXMLAgent, LRunOff, 0);
    StudyDataHasChanged(sdccEdit, 'RU', IntToStr(LRunOff.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateChannelProperties (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateChannelProperties';
var
  LModuleID : Integer;
  LChannel  : TChannelModule;
begin
  Result := FALSE;
  try
    LModuleID  := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LChannel   := FNetwork.FindChannelModuleAgent.FindChannelModuleByID(LModuleID);
    Result     := LChannel.UpdatePropertiesData(ARootNode);
    if (FChannelPropertiesDlg <> nil) then
      RefreshDlgXML(FChannelPropertiesDlg.FXMLAgent, LChannel, 0);
    StudyDataHasChanged(sdccEdit, 'CR', IntToStr(LChannel.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateIrrigationProperties (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateIrrigationProperties';
var
  LModuleID   : Integer;
  LIrrigation : TIrrigationModule;
begin
  Result := FALSE;
  try
    LModuleID   := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LIrrigation := FNetwork.FindIrrigationModuleAgent.FindIrrigationModuleByID(LModuleID);
    Result      := LIrrigation.UpdatePropertiesData(ARootNode);
    if (FIrrigationPropertiesDlg <> nil) then
      RefreshDlgXML(FIrrigationPropertiesDlg.FXMLAgent, LIrrigation, 0);
    StudyDataHasChanged(sdccEdit, 'RR', IntToStr(LIrrigation.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateIrrigationWQT (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateIrrigationWQT';
var
  LModuleID   : Integer;
  LIrrigation : TIrrigationModule;
begin
  Result := FALSE;
  try
    LModuleID   := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LIrrigation := FNetwork.FindIrrigationModuleAgent.FindIrrigationModuleByID(LModuleID);
    Result      := LIrrigation.UpdateWQTData(ARootNode);
    if (FIrrigationWQTDlg <> nil) then
      RefreshDlgXML(FIrrigationWQTDlg.FXMLAgent, LIrrigation, 0);
    StudyDataHasChanged(sdccEdit, 'RR', IntToStr(LIrrigation.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateIrrigationFactors (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateIrrigationFactors';
var
  LModuleID   : Integer;
  LIrrigation : TIrrigationModule;
begin
  Result := FALSE;
  try
    LModuleID   := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LIrrigation := FNetwork.FindIrrigationModuleAgent.FindIrrigationModuleByID(LModuleID);
    Result      := LIrrigation.UpdateFactorsData(ARootNode);
    if (FIrrigationFactorsDlg <> nil) then
      RefreshDlgXML(FIrrigationFactorsDlg.FXMLAgent, LIrrigation, 0);
    StudyDataHasChanged(sdccEdit, 'RR', IntToStr(LIrrigation.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateIrrigationArea (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateIrrigationArea';
var
  LModuleID   : Integer;
  LIrrigation : TIrrigationModule;
begin
  Result := FALSE;
  try
    LModuleID   := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LIrrigation := FNetwork.FindIrrigationModuleAgent.FindIrrigationModuleByID(LModuleID);
    Result      := LIrrigation.UpdateAreaData(ARootNode);
    if (FIrrigationAreaDlg <> nil) then
      RefreshDlgXML(FIrrigationAreaDlg.FXMLAgent, LIrrigation, 0);
    StudyDataHasChanged(sdccEdit, 'RR', IntToStr(LIrrigation.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateIrrigationAllocationGrowth (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateIrrigationAllocationGrowth';
var
  LModuleID   : Integer;
  LIrrigation : TIrrigationModule;
begin
  Result := FALSE;
  try
    LModuleID   := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LIrrigation := FNetwork.FindIrrigationModuleAgent.FindIrrigationModuleByID(LModuleID);
    Result      := LIrrigation.UpdateAllocationGrowthData(ARootNode);
    if (FIrrigationAllocationGrowthDlg <> nil) then
      RefreshDlgXML(FIrrigationAllocationGrowthDlg.FXMLAgent, LIrrigation, 0);
    StudyDataHasChanged(sdccEdit, 'RR', IntToStr(LIrrigation.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateIrrigationReturnFlow (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateIrrigationReturnFlow';
var
  LModuleID   : Integer;
  LIrrigation : TIrrigationModule;
begin
  Result := FALSE;
  try
    LModuleID   := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LIrrigation := FNetwork.FindIrrigationModuleAgent.FindIrrigationModuleByID(LModuleID);
    Result      := LIrrigation.UpdateReturnFlowData(ARootNode);
    if (FIrrigationReturnFlowDlg <> nil) then
      RefreshDlgXML(FIrrigationReturnFlowDlg.FXMLAgent, LIrrigation, 0);
    StudyDataHasChanged(sdccEdit, 'RR', IntToStr(LIrrigation.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateIrrigationEfficiency (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateIrrigationEfficiency';
var
  LModuleID   : Integer;
  LIrrigation : TIrrigationModule;
begin
  Result := FALSE;
  try
    LModuleID   := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LIrrigation := FNetwork.FindIrrigationModuleAgent.FindIrrigationModuleByID(LModuleID);
    Result      := LIrrigation.UpdateEfficiencyData(ARootNode);
    if (FIrrigationEfficiencyDlg <> nil) then
      RefreshDlgXML(FIrrigationEfficiencyDlg.FXMLAgent, LIrrigation, 0);
    StudyDataHasChanged(sdccEdit, 'RR', IntToStr(LIrrigation.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateIrrigationCrops (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateIrrigationCrops';
var
  LModuleID   : Integer;
  LIrrigation : TIrrigationModule;
begin
  Result := FALSE;
  try
    LModuleID   := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LIrrigation := FNetwork.FindIrrigationModuleAgent.FindIrrigationModuleByID(LModuleID);
    Result      := LIrrigation.UpdateCropsData(ARootNode);
    if (FIrrigationCropsDlg <> nil) then
      RefreshDlgXML(FIrrigationCropsDlg.FXMLAgent, LIrrigation, 0);
    StudyDataHasChanged(sdccEdit, 'RR', IntToStr(LIrrigation.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateMineProperties (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateMineProperties';
var
  LModuleID : Integer;
  LMine     : TMineModule;
begin
  Result := FALSE;
  try
    LModuleID := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LMine     := FNetwork.FindMineModuleAgent.FindMineModuleByID(LModuleID);
    Result    := LMine.UpdatePropertiesData(ARootNode);
    if (FMinePropertiesDlg <> nil) then
      RefreshDlgXML(FMinePropertiesDlg.FXMLAgent, LMine, 0);
    StudyDataHasChanged(sdccEdit, 'MM', IntToStr(LMine.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateMinePlantArea (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateMinePlantArea';
var
  LModuleID : Integer;
  LMine     : TMineModule;
begin
  Result := FALSE;
  try
    LModuleID := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LMine     := FNetwork.FindMineModuleAgent.FindMineModuleByID(LModuleID);
    Result    := LMine.UpdatePlantAreaData(ARootNode);
    if (FMinePlantAreaDlg <> nil) then
      RefreshDlgXML(FMinePlantAreaDlg.FXMLAgent, LMine, 0);
    StudyDataHasChanged(sdccEdit, 'MM', IntToStr(LMine.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateMineSections (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateMineSections';
var
  LModuleID : Integer;
  LMine     : TMineModule;
begin
  Result := FALSE;
  try
    LModuleID := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LMine     := FNetwork.FindMineModuleAgent.FindMineModuleByID(LModuleID);
    Result    := LMine.UpdateSections(ARootNode);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateMineOpencastPitProperties (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateMineOpencastPitProperties';
var
  LModuleID    : Integer;
  LSectionNo   : Integer;
  LMine        : TMineModule;
  LOpencastPit : TOpencastPit;
begin
  Result := FALSE;
  try
    LModuleID    := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LSectionNo   := StrToInt(ARootNode.ChildNodes['SectionNo'].Text);
    LMine        := FNetwork.FindMineModuleAgent.FindMineModuleByID(LModuleID);
    LOpencastPit := LMine.FindOpencastPitBySectionNo(LSectionNo);
    Result       := LOpencastPit.UpdatePropertiesData(ARootNode);
    if (FMineOpencastPitPropertiesDlg <> nil) then
      RefreshDlgXML(FMineOpencastPitPropertiesDlg.FXMLAgent, LMine, LSectionNo);
    StudyDataHasChanged(sdccEdit, 'MM', IntToStr(LMine.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateMineOpencastPitWorkings (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateMineOpencastPitWorkings';
var
  LModuleID    : Integer;
  LSectionNo   : Integer;
  LMine        : TMineModule;
  LOpencastPit : TOpencastPit;
begin
  Result := FALSE;
  try
    LModuleID    := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LSectionNo   := StrToInt(ARootNode.ChildNodes['SectionNo'].Text);
    LMine        := FNetwork.FindMineModuleAgent.FindMineModuleByID(LModuleID);
    LOpencastPit := LMine.FindOpencastPitBySectionNo(LSectionNo);
    Result       := LOpencastPit.UpdateWorkingsAreaData(ARootNode);
    if (FMineOpencastPitWorkingsDlg <> nil) then
      RefreshDlgXML(FMineOpencastPitWorkingsDlg.FXMLAgent, LMine, LSectionNo);
    StudyDataHasChanged(sdccEdit, 'MM', IntToStr(LMine.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateMineOpencastPitDisturbed (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateMineOpencastPitDisturbed';
var
  LModuleID    : Integer;
  LSectionNo   : Integer;
  LMine        : TMineModule;
  LOpencastPit : TOpencastPit;
begin
  Result := FALSE;
  try
    LModuleID    := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LSectionNo   := StrToInt(ARootNode.ChildNodes['SectionNo'].Text);
    LMine        := FNetwork.FindMineModuleAgent.FindMineModuleByID(LModuleID);
    LOpencastPit := LMine.FindOpencastPitBySectionNo(LSectionNo);
    Result       := LOpencastPit.UpdateDisturbedAreaData(ARootNode);
    if (FMineOpencastPitDisturbedDlg <> nil) then
      RefreshDlgXML(FMineOpencastPitDisturbedDlg.FXMLAgent, LMine, LSectionNo);
    StudyDataHasChanged(sdccEdit, 'MM', IntToStr(LMine.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateMineOpencastPitRehabilitated (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateMineOpencastPitRehabilitated';
var
  LModuleID    : Integer;
  LSectionNo   : Integer;
  LMine        : TMineModule;
  LOpencastPit : TOpencastPit;
begin
  Result := FALSE;
  try
    LModuleID    := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LSectionNo   := StrToInt(ARootNode.ChildNodes['SectionNo'].Text);
    LMine        := FNetwork.FindMineModuleAgent.FindMineModuleByID(LModuleID);
    LOpencastPit := LMine.FindOpencastPitBySectionNo(LSectionNo);
    Result       := LOpencastPit.UpdateRehabilitatedAreaData(ARootNode);
    if (FMineOpencastPitRehabilitatedDlg <> nil) then
      RefreshDlgXML(FMineOpencastPitRehabilitatedDlg.FXMLAgent, LMine, LSectionNo);
    StudyDataHasChanged(sdccEdit, 'MM', IntToStr(LMine.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateMineOpencastPitEvaporation (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateMineOpencastPitEvaporation';
var
  LModuleID    : Integer;
  LSectionNo   : Integer;
  LMine        : TMineModule;
  LOpencastPit : TOpencastPit;
begin
  Result := FALSE;
  try
    LModuleID    := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LSectionNo   := StrToInt(ARootNode.ChildNodes['SectionNo'].Text);
    LMine        := FNetwork.FindMineModuleAgent.FindMineModuleByID(LModuleID);
    LOpencastPit := LMine.FindOpencastPitBySectionNo(LSectionNo);
    Result       := LOpencastPit.UpdateEvaporationAreaData(ARootNode);
    if (FMineOpencastPitEvaporationDlg <> nil) then
      RefreshDlgXML(FMineOpencastPitEvaporationDlg.FXMLAgent, LMine, LSectionNo);
    StudyDataHasChanged(sdccEdit, 'MM', IntToStr(LMine.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateMineOpencastPitDecant (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateMineOpencastPitDecant';
var
  LModuleID    : Integer;
  LSectionNo   : Integer;
  LMine        : TMineModule;
  LOpencastPit : TOpencastPit;
begin
  Result := FALSE;
  try
    LModuleID    := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LSectionNo   := StrToInt(ARootNode.ChildNodes['SectionNo'].Text);
    LMine        := FNetwork.FindMineModuleAgent.FindMineModuleByID(LModuleID);
    LOpencastPit := LMine.FindOpencastPitBySectionNo(LSectionNo);
    Result       := LOpencastPit.UpdateInspoilsDecantData(ARootNode);
    if (FMineOpencastPitDecantDlg <> nil) then
      RefreshDlgXML(FMineOpencastPitDecantDlg.FXMLAgent, LMine, LSectionNo);
    StudyDataHasChanged(sdccEdit, 'MM', IntToStr(LMine.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateMineOpencastPitSeepage (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateMineOpencastPitSeepage';
var
  LModuleID    : Integer;
  LSectionNo   : Integer;
  LMine        : TMineModule;
  LOpencastPit : TOpencastPit;
begin
  Result := FALSE;
  try
    LModuleID    := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LSectionNo   := StrToInt(ARootNode.ChildNodes['SectionNo'].Text);
    LMine        := FNetwork.FindMineModuleAgent.FindMineModuleByID(LModuleID);
    LOpencastPit := LMine.FindOpencastPitBySectionNo(LSectionNo);
    Result       := LOpencastPit.UpdateInspoilsSeepageData(ARootNode);
    if (FMineOpencastPitSeepageDlg <> nil) then
      RefreshDlgXML(FMineOpencastPitSeepageDlg.FXMLAgent, LMine, LSectionNo);
    StudyDataHasChanged(sdccEdit, 'MM', IntToStr(LMine.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateMineOpencastPitRechargeFactors (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateMineOpencastPitRechargeFactors';
var
  LModuleID    : Integer;
  LSectionNo   : Integer;
  LMine        : TMineModule;
  LOpencastPit : TOpencastPit;
begin
  Result := FALSE;
  try
    LModuleID    := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LSectionNo   := StrToInt(ARootNode.ChildNodes['SectionNo'].Text);
    LMine        := FNetwork.FindMineModuleAgent.FindMineModuleByID(LModuleID);
    LOpencastPit := LMine.FindOpencastPitBySectionNo(LSectionNo);
    Result       := LOpencastPit.UpdateRechargeFactorsData(ARootNode);
    if (FMineOpencastPitRechargeFactorsDlg <> nil) then
      RefreshDlgXML(FMineOpencastPitRechargeFactorsDlg.FXMLAgent, LMine, LSectionNo);
    StudyDataHasChanged(sdccEdit, 'MM', IntToStr(LMine.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateMineOpencastPitWorkingsQSLD (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateMineOpencastPitWorkingsQSLD';
var
  LModuleID    : Integer;
  LSectionNo   : Integer;
  LMine        : TMineModule;
  LOpencastPit : TOpencastPit;
begin
  Result := FALSE;
  try
    LModuleID    := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LSectionNo   := StrToInt(ARootNode.ChildNodes['SectionNo'].Text);
    LMine        := FNetwork.FindMineModuleAgent.FindMineModuleByID(LModuleID);
    LOpencastPit := LMine.FindOpencastPitBySectionNo(LSectionNo);
    Result       := LOpencastPit.UpdateWorkingsQSLDData(ARootNode);
    if (FMineOpencastPitWorkingsQSLDDlg <> nil) then
      RefreshDlgXML(FMineOpencastPitWorkingsQSLDDlg.FXMLAgent, LMine, LSectionNo);
    StudyDataHasChanged(sdccEdit, 'MM', IntToStr(LMine.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateMineOpencastPitSeepDecantQSLD (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateMineOpencastPitSeepDecantQSLD';
var
  LModuleID    : Integer;
  LSectionNo   : Integer;
  LMine        : TMineModule;
  LOpencastPit : TOpencastPit;
begin
  Result := FALSE;
  try
    LModuleID    := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LSectionNo   := StrToInt(ARootNode.ChildNodes['SectionNo'].Text);
    LMine        := FNetwork.FindMineModuleAgent.FindMineModuleByID(LModuleID);
    LOpencastPit := LMine.FindOpencastPitBySectionNo(LSectionNo);
    Result       := LOpencastPit.UpdateSeepDecantQSLDData(ARootNode);
    if (FMineOpencastPitSeepDecantQSLDDlg <> nil) then
      RefreshDlgXML(FMineOpencastPitSeepDecantQSLDDlg.FXMLAgent, LMine, LSectionNo);
    StudyDataHasChanged(sdccEdit, 'MM', IntToStr(LMine.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateMineUndergroundProperties (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateMineUndergroundProperties';
var
  LModuleID    : Integer;
  LSectionNo   : Integer;
  LMine        : TMineModule;
  LUnderground : TUndergroundSection;
begin
  Result := FALSE;
  try
    LModuleID    := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LSectionNo   := StrToInt(ARootNode.ChildNodes['SectionNo'].Text);
    LMine        := FNetwork.FindMineModuleAgent.FindMineModuleByID(LModuleID);
    LUnderground := LMine.FindUndergroundSectionBySectionNo(LSectionNo);
    Result       := LUnderground.UpdatePropertiesData(ARootNode);
    if (FMineUndergroundPropertiesDlg <> nil) then
      RefreshDlgXML(FMineUndergroundPropertiesDlg.FXMLAgent, LMine, LSectionNo);
    StudyDataHasChanged(sdccEdit, 'MM', IntToStr(LMine.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateMineUndergroundBoardPillar (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateMineUndergroundBoardPillar';
var
  LModuleID    : Integer;
  LSectionNo   : Integer;
  LMine        : TMineModule;
  LUnderground : TUndergroundSection;
begin
  Result := FALSE;
  try
    LModuleID    := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LSectionNo   := StrToInt(ARootNode.ChildNodes['SectionNo'].Text);
    LMine        := FNetwork.FindMineModuleAgent.FindMineModuleByID(LModuleID);
    LUnderground := LMine.FindUndergroundSectionBySectionNo(LSectionNo);
    Result       := LUnderground.UpdateBoardPillarData(ARootNode);
    if (FMineUndergroundBoardPillarDlg <> nil) then
      RefreshDlgXML(FMineUndergroundBoardPillarDlg.FXMLAgent, LMine, LSectionNo);
    StudyDataHasChanged(sdccEdit, 'MM', IntToStr(LMine.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateMineUndergroundHighExtraction (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateMineUndergroundHighExtraction';
var
  LModuleID    : Integer;
  LSectionNo   : Integer;
  LMine        : TMineModule;
  LUnderground : TUndergroundSection;
begin
  Result := FALSE;
  try
    LModuleID    := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LSectionNo   := StrToInt(ARootNode.ChildNodes['SectionNo'].Text);
    LMine        := FNetwork.FindMineModuleAgent.FindMineModuleByID(LModuleID);
    LUnderground := LMine.FindUndergroundSectionBySectionNo(LSectionNo);
    Result       := LUnderground.UpdateHighExtractionData(ARootNode);
    if (FMineUndergroundHighExtractionDlg <> nil) then
      RefreshDlgXML(FMineUndergroundHighExtractionDlg.FXMLAgent, LMine, LSectionNo);
    StudyDataHasChanged(sdccEdit, 'MM', IntToStr(LMine.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateMineUndergroundRechargeFactors (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateMineUndergroundRechargeFactors';
var
  LModuleID    : Integer;
  LSectionNo   : Integer;
  LMine        : TMineModule;
  LUnderground : TUndergroundSection;
begin
  Result := FALSE;
  try
    LModuleID    := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LSectionNo   := StrToInt(ARootNode.ChildNodes['SectionNo'].Text);
    LMine        := FNetwork.FindMineModuleAgent.FindMineModuleByID(LModuleID);
    LUnderground := LMine.FindUndergroundSectionBySectionNo(LSectionNo);
    Result       := LUnderground.UpdateMonthlyData(ARootNode);
    if (FMineUndergroundRechargeFactorsDlg <> nil) then
      RefreshDlgXML(FMineUndergroundRechargeFactorsDlg.FXMLAgent, LMine, LSectionNo);
    StudyDataHasChanged(sdccEdit, 'MM', IntToStr(LMine.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateMineUndergroundQSLD (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateMineUndergroundQSLD';
var
  LModuleID    : Integer;
  LSectionNo   : Integer;
  LMine        : TMineModule;
  LUnderground : TUndergroundSection;
begin
  Result := FALSE;
  try
    LModuleID    := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LSectionNo   := StrToInt(ARootNode.ChildNodes['SectionNo'].Text);
    LMine        := FNetwork.FindMineModuleAgent.FindMineModuleByID(LModuleID);
    LUnderground := LMine.FindUndergroundSectionBySectionNo(LSectionNo);
    Result       := LUnderground.UpdateQSLDData(ARootNode);
    if (FMineUndergroundQSLDDlg <> nil) then
      RefreshDlgXML(FMineUndergroundQSLDDlg.FXMLAgent, LMine, LSectionNo);
    StudyDataHasChanged(sdccEdit, 'MM', IntToStr(LMine.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateMineSlurryDumpProperties (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateMineSlurryDumpProperties';
var
  LModuleID    : Integer;
  LSectionNo   : Integer;
  LMine        : TMineModule;
  LSlurryDump  : TSlurryDump;
begin
  Result := FALSE;
  try
    LModuleID    := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LSectionNo   := StrToInt(ARootNode.ChildNodes['SectionNo'].Text);
    LMine        := FNetwork.FindMineModuleAgent.FindMineModuleByID(LModuleID);
    LSlurryDump  := LMine.FindSlurryDumpSectionBySectionNo(LSectionNo);
    Result       := LSlurryDump.UpdatePropertiesData(ARootNode);
    if (FMineSlurryDumpPropertiesDlg <> nil) then
      RefreshDlgXML(FMineSlurryDumpPropertiesDlg.FXMLAgent, LMine, LSectionNo);
    StudyDataHasChanged(sdccEdit, 'MM', IntToStr(LMine.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateMineSlurryDumpArea (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateMineSlurryDumpArea';
var
  LModuleID    : Integer;
  LSectionNo   : Integer;
  LMine        : TMineModule;
  LSlurryDump  : TSlurryDump;
begin
  Result := FALSE;
  try
    LModuleID    := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LSectionNo   := StrToInt(ARootNode.ChildNodes['SectionNo'].Text);
    LMine        := FNetwork.FindMineModuleAgent.FindMineModuleByID(LModuleID);
    LSlurryDump  := LMine.FindSlurryDumpSectionBySectionNo(LSectionNo);
    Result       := LSlurryDump.UpdateGrowthData(ARootNode);
    if (FMineSlurryDumpAreaDlg <> nil) then
      RefreshDlgXML(FMineSlurryDumpAreaDlg.FXMLAgent, LMine, LSectionNo);
    StudyDataHasChanged(sdccEdit, 'MM', IntToStr(LMine.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateMineSlurryDumpRechargeFactors (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateMineSlurryDumpRechargeFactors';
var
  LModuleID    : Integer;
  LSectionNo   : Integer;
  LMine        : TMineModule;
  LSlurryDump  : TSlurryDump;
begin
  Result := FALSE;
  try
    LModuleID    := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LSectionNo   := StrToInt(ARootNode.ChildNodes['SectionNo'].Text);
    LMine        := FNetwork.FindMineModuleAgent.FindMineModuleByID(LModuleID);
    LSlurryDump  := LMine.FindSlurryDumpSectionBySectionNo(LSectionNo);
    Result       := LSlurryDump.UpdateMonthlyData(ARootNode);
    if (FMineSlurryDumpRechargeFactorsDlg <> nil) then
      RefreshDlgXML(FMineSlurryDumpRechargeFactorsDlg.FXMLAgent, LMine, LSectionNo);
    StudyDataHasChanged(sdccEdit, 'MM', IntToStr(LMine.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateMineSlurryDumpQSLD (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateMineSlurryDumpQSLD';
var
  LModuleID    : Integer;
  LSectionNo   : Integer;
  LMine        : TMineModule;
  LSlurryDump  : TSlurryDump;
begin
  Result := FALSE;
  try
    LModuleID    := StrToInt(ARootNode.ChildNodes['Identifier'].Text);
    LSectionNo   := StrToInt(ARootNode.ChildNodes['SectionNo'].Text);
    LMine        := FNetwork.FindMineModuleAgent.FindMineModuleByID(LModuleID);
    LSlurryDump  := LMine.FindSlurryDumpSectionBySectionNo(LSectionNo);
    Result       := LSlurryDump.UpdateQSLDData(ARootNode);
    if (FMineSlurryDumpQSLDDlg <> nil) then
      RefreshDlgXML(FMineSlurryDumpQSLDDlg.FXMLAgent, LMine, LSectionNo);
    StudyDataHasChanged(sdccEdit, 'MM', IntToStr(LMine.ModuleNumber), '');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.ShowHydroOutputDialog (const AElementType    : WideString;
                                                const AElementSubType : WideString;
                                                AElementNo            : Integer;
                                                ASubElementID         : Integer;
                                                AResultTypeID         : Integer): WordBool;
const OPNAME = 'THydrologyModel.ShowHydroOutputDialog';
var
  LNetworkModule : INetworkModule;
  LNetworkRoute  : INetworkRoute;
  LElementID     : Integer;
begin
  Result := FALSE;
  try
    LNetworkModule := nil;
    LNetworkRoute  := nil;
    if (AElementType = 'RQ') then
      LNetworkRoute := FNetwork.NetworkRouteAgent.NetworkRouteByRouteNo[AElementNo]
    else if (AElementType = 'RU') then
      LNetworkModule := FNetwork.RunOffModuleAgent.RunOffModuleByNumber[AElementNo]
    else if (AElementType = 'RV') then
      LNetworkModule := FNetwork.ReservoirModuleAgent.ReservoirModuleByNumber[AElementNo]
    else if (AElementType = 'CR') then
      LNetworkModule := FNetwork.ChannelModuleAgent.ChannelModuleByNumber[AElementNo]
    else if (AElementType = 'RR') then
      LNetworkModule := FNetwork.IrrigationModuleAgent.IrrigationModuleByNumber[AElementNo]
    else if (AElementType = 'MM') then
      LNetworkModule := FNetwork.MineModuleAgent.MineModuleByNumber[AElementNo];

    if ((LNetworkModule <> nil) OR (LNetworkRoute <> nil)) then
    begin
      if (AElementType = 'RQ') then
        LElementID := LNetworkRoute.RouteID
      else
        LElementID := LNetworkModule.ModuleID;
      FHydroOutputDlg := THydroOutputDlg.Create(nil);
      FHydroOutputDlg.ClientHeight      := FHostHeightHydroOutput;
      FHydroOutputDlg.ClientWidth       := FHostWidthHydroOutput;
      FHydroOutputDlg.FHydrologyModel   := Self;
      FHydroOutputDlg.FMayChangeNetwork := FMayChangeNetwork;
      FHydroOutputDlg.FXMLAgent.FHydrologyModel := Self;

      RefreshOutputDlgXML(FHydroOutputDlg.FXMLAgent, AElementType, AElementSubType, AElementNo, LElementID, ASubElementID, AResultTypeID);
      FHydroOutputDlg.PopulateForm;
      FHydroOutputDlg.Show;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyModel.FreeOutputDlg;
const OPNAME = 'THydrologyModel.FreeOutputDlg';
begin
  try
    FHostHeightHydroOutput := FHydroOutputDlg.ClientHeight;
    FHostWidthHydroOutput  := FHydroOutputDlg.ClientWidth;
    FHydroOutputDlg := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.RefreshOutputDlg (const AXMLOut: WideString): WordBool;
const OPNAME = 'THydrologyModel.RefreshOutputDlg';
var
  LXMLAgent       : TXMLAgent;
  LXMLDocOut      : IXMLDocument;
  LRootNode       : IXMLNode;
  LElementType    : String;
  LElementSubType : String;
  LElementNo      : Integer;
  LElementID      : Integer;
  LSubElementID   : Integer;
  LResultTypeID   : Integer;
begin
  try
    LXMLAgent := TXMLAgent.Create;
    LXMLAgent.FHydrologyModel := Self;
    try
      LXMLDocOut      := LXMLAgent.CreateXMLDocument(AXMLOut);
      LRootNode       := LXMLDocOut.DocumentElement;
      LElementType    := LRootNode.ChildNodes['ElementType'].Text;
      LElementSubType := LRootNode.ChildNodes['ElementSubType'].Text;
      LElementNo      := StrToInt(LRootNode.ChildNodes['ElementNo'].Text);
      LElementID      := StrToInt(LRootNode.ChildNodes['ElementID'].Text);
      LSubElementID   := StrToInt(LRootNode.ChildNodes['SubElementID'].Text);
      LResultTypeID   := StrToInt(LRootNode.ChildNodes['ResultTypeID'].Text);
      RefreshOutputDlgXML(FHydroOutputDlg.FXMLAgent, LElementType, LElementSubType, LElementNo, LElementID, LSubElementID, LResultTypeID);
      FHydroOutputDlg.PopulateForm;
    finally
      LXMLAgent.Free;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.ShowNetworkSequenceDialog (AModuleID: Integer): WordBool;
const OPNAME = 'THydrologyModel.ShowNetworkSequenceDialog';
var
  LNetworkModule : INetworkModule;
begin
  Result := FALSE;
  try
    LNetworkModule := Network.ModuleByID[AModuleID];

    FNetworkSequenceDlg := TNetworkSequenceDlg.Create(nil);
    FNetworkSequenceDlg.FHydrologyModel   := Self;
    FNetworkSequenceDlg.FMayChangeNetwork := FMayChangeNetwork;
    FNetworkSequenceDlg.FXMLAgent.FHydrologyModel := Self;
    FNetworkSequenceDlg.FModuleID                 := AModuleID;

    RefreshDlgXML(FNetworkSequenceDlg.FXMLAgent, LNetworkModule, 0);
    try
      FNetworkSequenceDlg.ShowModal;
    finally
      FreeAndNil(FNetworkSequenceDlg);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.ChangeModuleSequenceNumber (AModule   : INetworkModule;
                                                     ANewValue : Integer): WordBool;
const OPNAME = 'THydrologyModel.ChangeModuleSequenceNumber';
begin
  Result := FALSE;
  try
    AModule.NetworkSequence := ANewValue;
    Result := StudyDataHasChanged(sdccEdit, AModule.ModuleType, IntToStr(AModule.ModuleNumber), IntToStr(ANewValue));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.UpdateNetworkSequence (ARootNode : IXMLNode): WordBool;
const OPNAME = 'THydrologyModel.UpdateNetworkSequence';
var
  LSectionNode      : IXMLNode;
  LDataListNode     : IXMLNode;
  LNode             : IXMLNode;
  LIndex            : Integer;
  LModuleID         : Integer;
  LNetworkSequence  : Integer;
  LModule           : INetworkModule;
begin
  Result := FALSE;
  try
    LSectionNode  := ARootNode.ChildNodes['ChangeNetworkSequence'];
    LDataListNode := LSectionNode.ChildNodes['DataList'];
    for LIndex := 1 to LDataListNode.ChildNodes.Count do
    begin
      LNode            := LDataListNode.ChildNodes.Get(LIndex-1);
      LModuleID        := StrToInt(LNode.ChildNodes['ModuleID'].Text);
      LNetworkSequence := StrToInt(LNode.ChildNodes['NetworkSequence'].Text);
      LModule          := FNetwork.ModuleByID[LModuleID];
      if ((LModule <> nil) AND (LModule.NetworkSequence <> LNetworkSequence)) then
        ChangeModuleSequenceNumber(LModule, LNetworkSequence);
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.CreateNewNetwork (const ANetworkCode     : WideString;
                                           AVersionNo             : Integer;
                                           const AInputDir        : WideString;
                                           const AOutputDir       : WideString;
                                           const ADebugRequired   : WideString;
                                           ADebugStartPeriod      : Integer;
                                           ADebudEndPeriod        : Integer;
                                           const ASummaryRequired : WideString;
                                           ASimulationStartYear   : Integer;
                                           ASimulationEndYear     : Integer;
                                           AReadOnly              : Integer;
                                           var ANetworkID         : Integer;
                                           var AErrorMsg          : WideString): WordBool;
const OPNAME = 'THydrologyModel.CreateNewNetwork';
begin
  Result := FALSE;
  try
    AErrorMsg := '';
    Result := FHydroDBManager.CreateNewNetwork
                (ANetworkCode, AVersionNo, AInputDir, AOutputDir, ADebugRequired,
                 ADebugStartPeriod, ADebudEndPeriod, ASummaryRequired,
                 ASimulationStartYear, ASimulationEndYear, AReadOnly, ANetworkID, AErrorMsg);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.DeleteNetwork (ANetworkID: Integer; var AErrorMsg      : WideString): WordBool;
const OPNAME = 'THydrologyModel.DeleteNetwork';
begin
  Result := FALSE;
  try
    AErrorMsg := '';
    Result := FHydroDBManager.DeleteNetwork(ANetworkID, AErrorMsg);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.CopyNetwork (ANetworkID: Integer;
                                      const ANewNetworkCode : WideString;
                                      var AErrorMsg         : WideString): WordBool;
const OPNAME = 'THydrologyModel.CopyNetwork';
begin
  Result := FALSE;
  try
    AErrorMsg := '';
    Result := FHydroDBManager.CopyNetwork(ANetworkID, ANewNetworkCode, AErrorMsg);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.ExportNetwork (ANetworkID: Integer;
                                        const ADirectory   : WideString;
                                        var AErrorMsg      : WideString): WordBool;
const OPNAME = 'THydrologyModel.ExportNetwork';
begin
  Result := FALSE;
  try
    AErrorMsg := '';
    Result := FHydroDBManager.ExportNetwork(ANetworkID, ADirectory, AErrorMsg);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModel.ImportNetwork (const ADirectory : WideString;
                                        ANetworkID: Integer;
                                        var AErrorMsg    : WideString): WordBool;
const OPNAME = 'THydrologyModel.ImportNetwork';
begin
  Result := FALSE;
  try
    AErrorMsg := '';
    Result := FHydroDBManager.ImportNetwork(ADirectory, ANetworkID, AErrorMsg);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.

