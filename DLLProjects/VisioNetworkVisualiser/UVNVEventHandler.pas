{******************************************************************************}
{*  UNIT      : Contains the class TVNVEventHandler.                          *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/09/01                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UVNVEventHandler;

interface

uses
  VCL.Forms,
  Classes,
  StdVCL,
  Contnrs,
  VCL.ExtCtrls,
  StrUtils,
  Visio_TLB,
  VisOcx_TLB,
  VoaimsCom_TLB,
  GisViewerX41Control_TLB,
  UOKCancelForm,
  UVNVDialogs,
  UAbstractObject,
  UNetworkVisualiserData;

const
  mtWRYMReservoir       = 'Reservoir (WRYM)';
  mtWRYMNode            = 'Node (WRYM)';
  mtWRYMChannel         = 'Channel (WRYM)';
  mtWRYMInflow          = 'Inflow (WRYM)';
  mtWRYMText            = 'Text (WRYM)';
  mtWRYMResPenalty      = 'Reservoir Penalty (WRYM)';
  mtWRYMResPenaltyExt   = 'Extended Reservoir Penalty (WRYM)';
  mtWRYMChanPenalty     = 'Channel Penalty (WRYM)';
  mtWRYMOutputSelector  = 'Output Selector (WRYM)';
  mtWRYMResInflow       = 'Reservoir Inflow (WRYM)';
  mtWRYMNodeInflow      = 'Node Inflow (WRYM)';
  mtWRYMIrrBlock        = 'Irrigation Block';
  mtWRYMIrrArea         = 'Irrigation Area';
  mtWRYMWetland         = 'Wetland';
  mtWRYMSubCatchment    = 'SFRA Sub Catchment';
  mtWRYMSubCatchmentTxt = 'SFRA Text.';
  mtWRYMDemandCentre    = 'Demand Centre';
  mtWRYMReclamPlant     = 'Reclamation Plant Loss Feature';
  mtWRYMMine            = 'Mine';
  mtWRYMGroundWater     = 'Ground Water';
  mtWRYMPowerPlant      = 'Power Plant (WRYM)';
  mtModel               = 'Model';
  mtMultiAction         = 'Multi-Action';
  mtLayersAction        = 'Layer-Action';
  CThisVersion          = '2.14.1';

  CPowerArray          : array [1..3] of string = ('CV1 to PD',
                                                   'V2 to CV1',
                                                   'CV2/V3 to CV3/INF');
  CIrrigationArray     : array [1..2] of string = ('RET/IRR to RET/IRR',
                                                   'V6/V5/V4 to NIRR/RET/IRR');
  CIrrAreaDiversionArray : array [1..2] of string = ('IRR to IRR',
                                                     '0 to IRR');
  CIrrAreaReturnArray  : array [1..2] of string = ('RET to RET',
                                                   '0 to RET');
  CIrrAreaConsumptiveArray  : array [1..1] of string = ('0 to IRR');
  CDiversionArray      : array [1..2] of string = ('QDIV to QDIV',
                                                   'V7 to QDIV');
  CLossArray           : array [1..2] of string = ('QLOSS to QLOSS',
                                                   'V9 to QLOSS');
  CIrrBlockDiversionArray : array [1..2] of string = ('IRR to IRR',
                                                      '0 to IRR');
  CIrrBlockReturnArray : array [1..2] of string = ('RET to RET',
                                                   '0 to RET');
  CPumpingArray        : array [1..5] of string = ('MPMP4 to MPMP5',
                                                   'MPMP3 to MPMP4',
                                                   'MPMP2 to MPMP3',
                                                   'MPMP1 to MPMP2',
                                                   'V11 to MPMP1');
  CInflowArray         : array [1..2] of string = ('QINF to QINF',
                                                   'V12 to QINF');

  cWRYMStencilName  = 'WRYMStencil.vss';
  cWRPMStencilName  = 'WRPMStencil.vss';
  cPinX             = 'PinX';
  cPinY             = 'PinY';
  cBeginX           = 'BeginX';
  cBeginY           = 'BeginY';
  cEndX             = 'EndX';
  cEndY             = 'EndY';
  cVersion          = 'Prop.Version.Value';
  cVersionLbl       = 'Prop.Version.Label';
  cVersionInv       = 'Prop.Version.Invisible';
  cNumber           = 'Prop.Number.Value';
  cResNr            = 'Prop.ReservoirNumber.Value';
  cNodeID           = 'Prop.NodeID.Value';
  cSubCatchmentID   = 'Prop.SubCatchment.Value';
  cChannelID        = 'Prop.ChannelID.Value';
  cResID            = 'Prop.ReservoirID.Value';
  cName             = 'Prop.Name.Value';
  cParentMine       = 'Prop.ParentMine.Value';
  cParentGroundWater = 'Prop.ParentGroundWater.Value';
  cRow1Menu         = 'Actions.Row_1.Menu';
  cRow1Action       = 'Actions.Row_1.Action';
  cTextType         = 'Prop.Type.Value';
  cTextLbl          = 'Prop.Type.Label';
  cTextInv          = 'Prop.Type.Invisible';
  cParent           = 'Prop.ParentShapeName.Value';
  cPenalty          = 'Prop.Penalty.Value';
  cParentXDiff      = 'Prop.ParentXDiff.Value';
  cParentYDiff      = 'Prop.ParentYDiff.Value';
  cParentXMoved     = 'Prop.ParentXMoved.Value';
  cParentYMoved     = 'Prop.ParentYMoved.Value';
  cPermanent        = 'Prop.Permanent.Value';
  cPermanentLabel   = 'Prop.Permanent.Label';
  cPermanentType    = 'Prop.Permanent.Type';
  cDownStreamNode   = 'Prop.DownStreamNode.Value';
  cUpStreamNode     = 'Prop.UpStreamNode.Value';
  cChannelType      = 'Prop.ChannelType.Value';
  cColourChanged    = 'Prop.ColourChanged.Value';
  cColourChangedInv = 'Prop.ColourChanged.Invisible';
  cShapeType        = 'Prop.ShapeType.Value';
  cShapeIndex       = 'Prop.ShapeIndex.Value';
  cRowIndex         = 'Prop.RowIndex.Value';
  cColIndex         = 'Prop.ColIndex.Value';
  cPenaltyNumber    = 'Prop.PenaltyNumber.Value';
  cCatchmentNumber  = 'Prop.CatchmentNumber.Value';
  cCatchmentLbl     = 'Prop.CatchmentNumber.Label';
  cDeletePrevention = 'LockDelete';
  cHeight           = 'Height';
  cWidth            = 'Width';
  cPermanentColor        = 'Prop.PermanentColor.Value';
  cPermanentColorLabel   = 'Prop.PermanentColor.Label';
  cPermanentColorType    = 'Prop.PermanentColor.Type';

  cGISScaleFactor   = 4;

  cGISWidth         = 994;
  cGISHeight        = 663;
  OutputTextTypeSet = [nvtElevation, nvtVolume, nvtNetBasinRunoff,nvtRainfall, nvtEvaporation, nvtPercFull,
                       nvtChannelFlow, nvtAvgElevation, nvtAvgVolume, nvtAvgNetBasinRunoff,nvtAvgRainfall,
                       nvtAvgEvaporation, nvtAvgChannelFlow,nvtReservoirStorageChange,nvtTimePeriod];

type TGISCalcLonOrLat = (gisCalcLon, gisCalcLat, gisCalcBoth);

type TSFRADialogType = (sfrdtCreateNew, sfrdtViewProperties);

type
  TVNVEventHandler = class(TAbstractAppObject)
  protected
    FDrawing                : TDrawing;
    FVisioApp               : IVApplication;
    FVisioToolbar           : IVToolbarSet;
    FVisioDoc               : IVDocument;
    FWRYMStencil            : IVDocument;
    FVisioEventCode         : integer;
    FOutputSelector         : IVShape;
    FReservoirList          : TStringList;
    FMineList               : TStringList;
    FGroundWaterList        : TStringList;
    FIrrigationList         : TStringList;
    FIrrigationAreaList     : TStringList;
    FWetlandList            : TStringList;
    FNodeList               : TStringList;
    FSubCatchmentList       : TStringList;
    FChannelList            : TStringList;
    FDemandCentreList       : TStringList;
    FResPenaltyList         : TStringList;
    FChanPenaltyList        : TStringList;
    FOutputList             : TStringList;
    FDoNotRecalcCoordList   : TStringList;
    FPowerPlantList         : TStringList;

    FBufferReservoirList    : TStringList;
    FBufferNodeHasSFRAList  : TStringList;
    FBufferIrrBlockList     : TStringList;
    FBufferIrrAreaList      : TStringList;
    FBufferWetlandList      : TStringList;
    FBufferNodeList         : TStringList;
    FBufferSubCatchmentList : TStringList;
    FBufferChannelList      : TStringList;
    FBufferResPenaltyList   : TStringList;
    FBufferChanPenaltyList  : TStringList;
    FBufferTextList         : TStringList;
    FBufferInflowList       : TStringList;
    FBufferDemandCentreList : TStringList;
    FBufferMineList         : TStringList;
    FBufferGroundWaterList  : TStringList;
    FBufferPowerPlantList   : TStringList;
    
    FReservoirExistNew      : integer;
    FMineExistNew           : integer;
    FGroundWaterExistNew    : integer;
    FSubCatchmentExistNew   : integer;
    FIrrigationExistNew     : integer;
    FIrrigationAreaExistNew : integer;
    FWetlandExistNew        : integer;
    FDemandCentreExistNew   : integer;
    FNodeWithInflow         : integer;
    FNodeExistNew           : integer;
    FChannelExistNew        : integer;
    FPowerPlantExistNew     : integer;
    FNodeDuplicate          : integer;
    FSubCatchmentDuplicate  : integer;
    FReservoirDuplicate     : integer;
    FMineDuplicate          : integer;
    FGroundWaterDuplicate   : integer;
    FWetlandDuplicate       : integer;
    FDemandCentreDuplicate  : integer;
    FIrrigationDuplicate    : integer;
    FIrrigationAreaDuplicate: integer;
    FChannelDuplicate       : integer;
    FResPenaltyDuplicate    : integer;
    FChanPenaltyDuplicate   : integer;
    FPowerPlantDuplicate    : integer;
    FLoadCase               : integer;
    FSequence               : integer;
    FMonthIndex             : integer;
    FUnits                  : TOutputUnits;
    FValueType              : TOutputValueType;
    FTimeStep               : TOutputTimeStep;
    FHighLight              : WordBool;
    FDisplayMonth           : integer;
    FGISForm                : TOKCancelForm;
    FGISViewer              : TGisViewerX;

    FMapWidth,
    FMapHeight,
    FPageWidth,
    FPageHeight             : Double;
    FBlockStudyDataChanges  : boolean;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure CreateGISViewer;
    procedure SetStencilPath;
    procedure SetCurrentStencil;
    procedure OnDocumentOpened (ASender    : TObject; const ADoc : IVDocument);

    procedure InitialiseDrawing(const ADoc : IVDocument);
    procedure RefreshDocument (const ADoc : IVDocument);
    procedure RefreshReservoir (AShape     : IVShape; AElementID : integer);
    procedure RefreshAllReservoirs;
    procedure RefreshIrrigationBlock (AShape     : IVShape; AElementID : integer);
    procedure RefreshIrrigationArea (AShape : IVShape; AElementID : integer);
    procedure RefreshWetland (AShape     : IVShape; AElementID : integer);
    procedure RefreshDemandCentre (AShape     : IVShape; AElementID : integer);
    procedure RefreshDemandCentreChannels(AShape  :  IVShape; ADemandCentre  :  IYMDemandCentre);
    procedure RefreshNode (AShape     : IVShape; AElementID : integer);
    procedure RefreshSubCatchment (AShape     : IVShape; AElementID : integer);
    procedure RefreshInflow (var AShape     : IVShape; AElementID : integer);
    procedure RefreshChannel (AShape     : IVShape; AElementID : integer);
    procedure RefreshReservoirPenalty (var AShape     : IVShape;  AElementID : integer);
    procedure RefreshChannelPenalty (var AShape     : IVShape; AElementID : integer);
    procedure RefreshOutputSelector (AShape     : IVShape);
    procedure RefreshText (AShape : IVShape);
    procedure RefreshInflowText (AShape     : IVShape; AElementID : integer);
    procedure RefreshDemandFileText (AShape     : IVShape; AElementID : integer);
    procedure RefreshMine (AShape     : IVShape; AElementID : integer);
    procedure RefreshGroundWater (AShape     : IVShape; AElementID : integer);
    procedure RefreshPowerPlant (AShape     : IVShape; AElementID : integer);

    procedure CreatePolutionControlDam (AMineShape     : IVShape; AMineNodeNumber,AReservoirNumber,AChannelNumber : integer);
    procedure DeletePolutionControlDam (AMineShape     : IVShape;  AMineNodeNumber,AReservoirNumber,AChannelNumber : integer);

    procedure GISCalculateLonLatOfShape (AShape     : IVShape; AElementID : integer; AGisCalcLonOrLat : TGISCalcLonOrLat);
    procedure GISSetXYPosFromLonLat (AShape     : IVShape; AElementID : integer);
    procedure GISPopulateDoNotRecalcCoordsList(AElementID : Integer);
    function  Get_ReservoirFromShape(AShape : IVShape; AElementID : integer):IReservoirData;

    procedure UpgradeDocument (const ADoc : IVDocument);
    function UpgradeReservoir (AShape : IVShape) : IVShape;
    function UpgradeMineReservoir (AShape : IVShape) : IVShape;
    function UpgradeGroundWaterAquifer (AShape : IVShape) : IVShape;
    function UpgradeSubCatchment(AShape : IVShape) : IVShape;
    function UpgradeInflow (AShape : IVShape) : IVShape;
    function UpgradeNode (AShape : IVShape) : IVShape;
    function UpgradeChannel (AShape : IVShape) : IVShape;
    function UpgradeReservoirPenalty (AShape : IVShape) : IVShape;
    function UpgradeChannelPenalty (AShape : IVShape) : IVShape;
    function UpgradeText (AShape : IVShape) : IVShape;

    procedure OnShapeAdded (ASender      : TObject; const AShape : IVShape);
    procedure ReservoirShapeAdded (const AShape : IVShape; AElementID : Integer = -1; ADeleteLock : Integer = 0; AParentMineID : Integer = 0);
    procedure SFRSubCatchmentShapeAdded  (const AShape : IVShape);
    procedure NodeShapeAdded (const AShape : IVShape; AElementID : Integer = -1);
    procedure ChannelShapeAdded (const AShape : IVShape; AElementID : Integer = -1);
    procedure ReservoirPenaltyShapeAdded (const AShape : IVShape);
    procedure ChannelPenaltyShapeAdded (const AShape : IVShape);
    procedure OutputSelectorShapeAdded (const AShape : IVShape);
    procedure InflowShapeAdded (const AShape : IVShape);
    procedure TextShapeAdded (const AShape : IVShape);
    procedure IrrigationBlockShapeAdded (const AShape : IVShape);
    procedure IrrigationAreaShapeAdded (const AShape : IVShape);
    procedure WetlandShapeAdded (const AShape : IVShape);
    procedure DemandCentreShapeAdded (const AShape : IVShape);
    procedure MineShapeAdded (const AShape : IVShape);
    procedure GroundWaterShapeAdded (const AShape : IVShape);
    procedure ModelShapeAdded (const AShape : IVShape);
    procedure PowerPlantShapeAdded(const AShape: IVShape);
    
    procedure GetOutputTextShape(AContainer : TInterfaceList;ATextType:TVNVTextType);
    function GetOutputTextShapePerParent(AContainer : TInterfaceList; AParentShape : IVShape):IVShape;
    procedure GetOutputTextShapesByParent(AContainer : TInterfaceList; AParentShape : IVShape;ATextType:TVNVTextType);

    function AddNewLayer(var ALayersInterface : IVLayers;var ALayer : IVLayer):boolean;
    function AddShapeToLayer(const ALayersInterface : IVLayers;const ALayer : IVLayer;const AShape: IVShape):boolean;
    function AddTextShapeToLayer(const ALayersInterface : IVLayers;const ALayer : IVLayer;const AShape: IVShape):boolean;
    function ClearShapeFromLayers(AShape : IVShape):boolean;
    function ClearTextShapesFromLayers(AShape : IVShape):boolean;

    function AddAllReservoirPenaltyLayer: boolean;
    function ClearAllReservoirPenaltiesFromLayer: boolean;

    function AddAllChannelPenaltyLayer: boolean;
    function ClearAllChannelPenaltyLayer: boolean;

    function AddAllChannelAndReservoirPenaltyLayer: boolean;
    function ClearAllChannelAndReservoirPenaltyLayer: boolean;

    function AddAllAverageChannelFlowsToALayer: boolean;
    function ClearAllAverageChannelFlowsFromLayers: boolean;

    function AddAllReservoirToALayer: boolean;
    function ClearAllReservoirFromLayers: boolean;

    function AddAllWetlandsToALayer: boolean;
    function ClearAllWetlandsFromLayers: boolean;

    function AddAllIrrigationBlockToALayer: boolean;
    function ClearAllIrrigationBlocksFromLayers: boolean;

    function AddAllNodesWithoutInflowToALayer: boolean;
    function ClearAllNodesWithoutInflowFromLayers: boolean;

    function AddAllNodesWithInflowToALayer: boolean;
    function ClearAllNodesWithInflowfromLayers: boolean;

    function AddAllMinesToALayer: boolean;
    function ClearAllMinesFromLayers: boolean;

    function AddAllDemandCentresToALayer: boolean;
    function ClearAllDemandCentresFromLayers: boolean;

    function AddAllIrrigationAreasToALayer: boolean;
    function ClearAllIrrigationAreasFromLayers: boolean;

    function AddAllPowerPlantsToALayer: boolean;
    function ClearAllPowerPlantsFromLayers: boolean;

    function AddAllNodesToDrawing : boolean;
    function AddAllChannelsToDrawing : boolean;
    function AddAllReservoirPenaltyToDrawing : boolean;
    function AddAllChannelPenaltyToDrawing : boolean;
    function DeleteAllReservoirPenaltyFromDrawing : boolean;
    function DeleteAllChannelPenaltyFromDrawing : boolean;

    function AddAllChannelOutputTextLabels(ATextType:TVNVTextType) : boolean;
    function AddAllReservoirOutputTextLabels(ATextType:TVNVTextType) : boolean;
    function DeleteAllChannelOutputTextLabels(ATextType:TVNVTextType) : boolean;
    function DeleteAllReservoirOutputTextLabels(ATextType:TVNVTextType) : boolean;
    function DeleteAllOutputTextLabels(ATextType:TVNVTextType) : boolean;
    procedure DeleteOutputTextLabels(const AShape : IVShape);
    procedure DeleteRelatedOutputTextLabels(const AShape : IVShape; ANumber: integer);
    procedure DoDeleteRelatedOutputTextLabels(const AShape : IVShape; ANumber: integer;AContainer: TInterfaceList;ATextType:TVNVTextType);
    procedure DeleteReservoirsRelatedOutputTextLabels(ANumber,ACount,AIndex: integer;ATextType:TVNVTextType);
    procedure DeleteChannelRelatedOutputTextLabels(ANumber,ACount,AIndex: integer;ATextType:TVNVTextType);
    procedure ReAlignOutputTextLabels(AContainer:TInterfaceList;AIndex:integer;const AShape : IVShape);

    function SetAllOutputLabelsPermanent : boolean;
    procedure AddUndergroundShape (const AShape : IVShape; AUnderground : IUnderground; AParentMineID : Integer = 0);
    function AddSFRASubCatchmentText(AParent  : IVShape; ASFRs : String) : IVShape;

    procedure SnapChannelToNodes (const AShape : IVShape; AChannelNr   : integer; AMineConnNodeNumber : integer = 3);
    procedure AddConnection (ASourceShape : IVShape; ATargetShape : IVShape);
    function AddIrrigationChannel(AIrrBlockShape : IVShape;AChannel : IGeneralFlowChannel) : IVShape;
    function AddIrrigationAreaChannel(AIrrAreaShape : IVShape;AChannel : IGeneralFlowChannel) : IVShape;
    function AddWetlandChannel(AWetlandShape : IVShape;AChannel : IGeneralFlowChannel) : IVShape;
    function AddMineChannel(AMineShape: IVShape; AChannel: IGeneralFlowChannel;
                                        AMineConnNodeNumber : integer = -1; ADeleteLock : integer = 0): IVShape;
    function AddGroundWaterChannels(AAquiferShape: IVShape; AGroundWater: IGroundWater): boolean;
    function AddGroundWaterNodes(AAquiferShape: IVShape; AGroundWater: IGroundWater): boolean;
    function AddTextLabel (AShape : IVShape; AType  : TVNVTextType) : IVShape;
    function AddInflowSymbol (AShape     : IVShape; AElementID : integer) : IVShape;

    function AddDemandCentreReclamationPlantChannel(ADemandCentreShape : IVShape;AChannel : IGeneralFlowChannel) : IVShape;
    function AddDemandCentreConsumptiveUseChannel(ADemandCentreShape : IVShape; AChannel : IGeneralFlowChannel) : IVShape;
    function AddDemandCentreReturnFlowChannels(ADemandCentreShape : IVShape;AChannel : IGeneralFlowChannel) : IVShape;
    function AddDemandCentreSupplyChannels(AShape  :  IVShape; AChannel : IGeneralFlowChannel) : IVShape;
    function AddPowerPlantChannel(APowerplantShape : IVShape;AChannel : IGeneralFlowChannel): IVShape;

    procedure OnShapeDelete (ASender      : TObject; const AShape : IVShape);
    procedure DeleteAllTextShapes (const AParentShape : IVShape);
    procedure DeleteAllShapesWithParent (const AParentShape : IVShape);
    procedure ReservoirShapeDeleted (const AShape : IVShape);
    procedure SubCatchmentShapeDeleted (const AShape : IVShape);
    procedure NodeShapeDeleted (const AShape : IVShape);
    procedure ChannelShapeDeleted (const AShape : IVShape);
    procedure ReservoirPenaltyShapeDeleted (const AShape : IVShape);
    procedure ChannelPenaltyShapeDeleted (const AShape : IVShape);
    procedure InflowShapeDeleted (const AShape : IVShape);
    procedure IrrigationBlockShapeDeleted (const AShape : IVShape);
    procedure IrrigationAreaShapeDeleted (const AShape : IVShape);
    procedure WetlandShapeDeleted (const AShape : IVShape);
    procedure DemandCentreShapeDeleted (const AShape : IVShape);
    procedure MineShapeDeleted (const AShape : IVShape);
    procedure GroundWaterShapeDeleted (const AShape : IVShape);
    procedure DeleteUndergroundShape(AUnderground: integer; AUndergroundChannel: integer; ADBDelete: integer = 1);
    procedure DeleteConnection (ASourceShape : IVShape;  ATargetShape : IVShape);
    procedure PowerPlantShapeDeleted(const AShape : IVShape);
    function GetUnderGroundIdentifier(AMineIdentifier,AUnderGroundIdentifier: integer): integer;
    function GetPCDIdentifier(AMineIdentifier : integer): integer;

    procedure RemoveDeletedDemandCentreChannels(AElementID  :  Integer);
    function DeleteReservoir (ANumber : integer; ADBDelete : integer = -1) : WordBool;
    function DeleteNode (ANumber : integer; ADBDelete : integer = -1) : WordBool;
    function DeleteSubCatchment (ANumber : integer) : WordBool;
    function DeleteChannel (ANumber : integer; ADBDelete : integer = -1) : WordBool;
    function DeleteIrrigationBlock (ANumber : integer) : WordBool;
    function DeleteIrrigationArea (ANumber : integer) : WordBool;
    function DeleteWetland (ANumber : integer) : WordBool;
    function DeleteDemandCentre(ANumber : integer) : WordBool;
    function DeleteMine(ANumber : integer; ADBDelete : integer = -1) : WordBool;
    function DeleteGroundWater(AAquiferNodeNumber : integer; ADBDelete : integer = -1) : WordBool;
    function DeletePowerPlant(ANumber : integer) : WordBool;

    procedure FindTextShapesWithParent (AParentName : string; AShapesList : TStringList);
    procedure FindShapesWithParent (AParentName : string; AShapesList : TStringList);
    procedure FindDemandCentreChannelsWithParent (ADemandCentreNodeNr : integer; AShapesList : TStringList);
    procedure FindChannelsWithParent (AParentNumber : integer; AShapesList : TStringList);
    function FindChannelUpShape (AChannel : IGeneralFlowChannel) : IVShape;
    function FindChannelDownShape (AChannel : IGeneralFlowChannel) : IVShape;
    function FindShapeWithPropertyValue (AMasterName : string; AProperty   : string; AValue : string) : IVShape;
    function FindShapeWithParent (AMasterName : string; AParentName : string) : IVShape;
    function FindChannelShapeByChannelNumber(AChannelNumber: integer): IVShape;
    function FindInflowShapeByReservoirNumber(AReservoirNumber: integer): IVShape;
    function FindSFRAShapeByReservoirNumber(AReservoirNumber: integer): IVShape;
    function FindNodeShapeBySubCatchmentNumber(ASubCatchmentNumber: integer): IVShape;
    function FindNodeShapeByNodeNumber(ANodeNumber: integer): IVShape;
    function FindTextWithParent (AParentName : string) : IVShape;
    function FindSFRATextWithParent (AParentName : string) : IVShape;
    function FindTextTypeWithParent (ATextType   : TVNVTextType; AParentName : string) : IVShape;
    function FindShapeWithName (AShapeName : string) : IVShape;
    function FindReservoirWithNumber (AReservoirNumber : Integer) : IVShape;
    function FindReservoirPenaltyWithNumber(AElementID : integer) : IVShape;
    function FindChannelPenaltyWithNumber(AElementID : integer) : IVShape;
    function FindMineWithNumber (AMineNumber : integer ) : IVShape;
    function FindGroundWaterWithNumber (AAquiferNodeNumber : integer ) : IVShape;
    function FindDemandCentreByNode (AElementID : integer) : IVShape;
    procedure FindReservoirByParentProperty (AElementid : integer; AShapesList : TStringList);
    function FindWetlandByElementID(AElementID : integer) : IVShape;
    function FindIrrBlockByElementID(AElementID : integer) : IVShape;
    function FindPowerPlantByElementID(AElementID : integer) : IVShape;

    function ShowReservoirsDialog : integer;
    function ShowSubCatchmentDialog(ANodeNr : integer; ASFRADialogType : TSFRADialogType) : integer;
    function ShowNodesDialog : integer;
    function ShowChannelsDialog : integer;
    function ShowIrrigationBlockDialog : integer;
    function ShowIrrigationAreaDialog : integer;
    function ShowWetlandDialog : integer;
    function ShowDemandCentreDialog : integer;
    function ShowTextDialog (var AElementType : string; var AElementNr : integer; var ATextType: TVNVTextType) : boolean;
    function ShowReservoirPenaltiesDialog : integer;
    function ShowChannelPenaltiesDialog : integer;
    function ShowMinesDialog : integer;
    function ShowGroundWaterDialog : integer;
    function ShowDroughtRestrictionDialog(AUsedRestrictionIDs: WideString): integer;
    function ShowPowerPlantDialog : integer;

    procedure PositionChanged (ACell  : IVCell;  AShape : IVShape);
    procedure ColourChanged (ACell  : IVCell; AShape : IVShape);
    procedure NodePositionChanged (ACell  : IVCell; AShape : IVShape);
    procedure ChannelPositionChanged (ACell  : IVCell; AShape : IVShape);
    procedure InflowPositionChanged (ACell  : IVCell; AShape : IVShape);
    procedure SFRASubCatchmentPositionChanged(ACell : IVCell; AShape : IVShape);
    procedure TextPositionChanged (ACell  : IVCell; AShape : IVShape);
    procedure IrrigationBlockPositionChanged (ACell  : IVCell; AShape : IVShape);
    procedure IrrigationAreaPositionChanged (ACell  : IVCell; AShape : IVShape);
    procedure WetlandPositionChanged (ACell  : IVCell; AShape : IVShape);
    procedure DemandCentrePositionChanged (ACell  : IVCell; AShape : IVShape);
    procedure PowerPlantPositionChanged(ACell : IVCell; AShape : IVShape);
    function SplitChannel (ANumber : integer) : WordBool;

    procedure NodeHasChanged (AShapeName : string; ANumber    : integer);
    procedure ReservoirHasChanged (AShapeName : string; ANumber    : integer);
    procedure MineHasChanged (AShapeName : string; ANumber : integer);
    procedure GroundWaterHasChanged (AShapeName : string; AAquiferNodeNumber: integer);
    procedure IrrigationBlockHasChanged (AShapeName : string;ANumber    : integer);
    procedure IrrigationAreaHasChanged (AShapeName : string;ANumber    : integer);
    procedure WetlandHasChanged (AShapeName : string; ANumber    : integer);
    procedure DemandCentreHasChanged (AShapeName : string; ANumber    : integer);
    procedure ReservoirPenaltyHasChanged (AShapeName : string; ANumber    : integer);
    procedure ChannelHasChanged (AShapeName : string; ANumber    : integer);
    procedure ChannelPenaltyHasChanged (AShapeName : string; ANumber    : integer);
    procedure NodeCatchmentHasChanged (AShapeName : string; ANumber    : integer);
    procedure PowerPlantHasChanged(AShapeName: string;ANumber: integer);

    function ShapeIsSelected (ASelection : IVSelection; AShapeName : string) : Boolean;
    function ChannelTypeName (AChannel : IGeneralFlowChannel) : string;
    function FindMaster (AMasterName : WideString) : IVMaster;
    function UnQuote (AString : string) : string;
    function UnMM (AString : string) : string;

    procedure CheckUpgradeInflow (AShape : IVShape);
    procedure CheckUpgradeReservoir (AShape : IVShape);
    procedure CheckUpgradeMineReservoir (AShape : IVShape);
    procedure CheckUpgradeGroundWaterAquifer (AShape : IVShape);
    procedure CheckUpgradeIrrBlock (AShape : IVShape);
    procedure CheckUpgradeIrrArea  (AShape : IVShape);
    procedure CheckUpgradeWetland (AShape : IVShape);
    procedure CheckUpgradeDemandCentre (AShape : IVShape);
    procedure CheckUpgradeNode (AShape : IVShape);
    procedure CheckUpgradeChannel (AShape : IVShape);
    procedure CheckUpgradeResPenalty (AShape : IVShape);
    procedure CheckUpgradeChanPenalty (AShape : IVShape);
    procedure CheckUpgradeText (AShape : IVShape);

    function CheckExistingWithCoords(AShape : IVShape; AElementID : integer) : Boolean;
    function GetFirstSelectedShape(AShapeName : string):IVShape;

    function ProcessVNVToggleGIS: boolean;
    function ProcessVNVDoubleClicked (AShapeName : string; ANumber    : integer) : WordBool;
    function ProcessVNVRightClicked (AShapeName : string;  ANumber    : integer) : WordBool;
    function ProcessVNVReservoirRightClicked (AShapeName : string; ANumber    : integer) : WordBool;
    function ProcessVNVSubCatchmentRightClicked (AShapeName : string; ANumber    : integer) : WordBool;
    function ProcessVNVNodeRightClicked (AShapeName : string; ANumber    : integer) : WordBool;
    function ProcessVNVChannelRightClicked (AShapeName : string; ANumber    : integer) : WordBool;
    function ProcessVNVIrrigationBlockRightClicked (AShapeName : string; ANumber    : integer) : WordBool;
    function ProcessVNVIrrigationAreaRightClicked (AShapeName : string; ANumber    : integer) : WordBool;
    function ProcessVNVWetlandRightClicked (AShapeName : string; ANumber    : integer) : WordBool;
    function ProcessVNVDemandCentreRightClicked (AShapeName : string; ANumber    : integer) : WordBool;
    function ProcessVNVModelRightClicked (AShapeName : string; ANumber    : integer) : WordBool;
    function ProcessVNVMineRightClicked (AShapeName : string; ANumber : integer) : WordBool;
    function ProcessVNVGroundWaterRightClicked (AShapeName : string; ANumber : integer) : WordBool;
    function ProcessVNVTextRightClicked (AShapeName : string; ANumber : integer) : WordBool;
    function ProcessVNVReservoirPenaltyRightClicked (AShapeName : string; ANumber : integer) : WordBool;
    function ProcessVNVChannelPenaltyRightClicked (AShapeName : string; ANumber : integer) : WordBool;
    function ProcessVNVMultiActionRightClicked (AShapeName : string; ANumber : integer) : WordBool;
    function ProcessVNVLayerRightClicked (AShapeName : string; ANumber : integer) : WordBool;
    function ProcessVNVPowerPlantRightClicked(AShapeName : string; ANumber : integer) : WordBool;

    function ViewCurtialment(AShapeName : string; AChannelNumber : integer) : WordBool;
    function ViewChannelDroughtRestriction (AShapeName : string; AChannelNumber : integer) : WordBool;
    function ViewReservoirDroughtRestriction (AShapeName : string; AReservoirNumber : integer) : WordBool;
    function ViewMineProperties(AShapeName : string; AMineNodeNumber : integer) : WordBool;

    function GetGISPage: IVPage;
    function ShowGIS: boolean;
    function HideGIS: boolean;
    function MayChangeScenario : Boolean;
    function TextTypeToStr (AType : TVNVTextType) : string;
    function StrToTextType (ATypeStr : string) : TVNVTextType;
    function ChannelPenaltyStr (AChannel  : IGeneralFlowChannel; ATimeStep : integer; ALoadCase : integer;
                                AArcNr    : integer) : string;
    function FindNumber(AComponentName  : String)  :  String;
    function GISConvertLonLatToXY(AGISViewer: TGisViewerX; ALon, ALat: double; var AX, AY: double): boolean;
    procedure GetSelectionData;
    procedure GenerateDemandCentreChannelList(var AChannelList : TStringList; ADemandCentre  : IYMDemandCentre);

    function MaxLossFactor(AChannel : IGeneralFlowChannel): string;
    function MaxDiversionFlow(AChannel : IGeneralFlowChannel): string;
  public
    function HandleVNVEvent (AVisioApp       : IUnknown;
                             AVisioDoc       : IUnknown;
                             AVisioEventCode : integer;
                             ASourceObj      : IUnknown;
                             AEventID        : Integer;
                             AEventSeqNum    : Integer;
                             ASubjectObj     : IUnknown;
                             AMoreInfo       : OleVariant): boolean;
    function ProcessVNVSpecial(const AParameter: WideString): boolean;
    function StudyDataHasChanged (AContext   : TChangeContext;
                                  AFieldName : string;
                                  AOldValue  : string;
                                  ANewValue  : string): boolean; override;
  end;

implementation

uses
  System.UITypes,
  Math,
  SysUtils,
  VCL.Dialogs,
  VCL.Controls,
  VCL.Menus,
  VCL.Graphics,

  VCLTee.Chart,
  VCLTee.TeeProcs,
  VCLTee.Series,
  VCLTee.TEEngine,

  UConstants,
  UUtilities,
  UVNVShapeMenus,
  UVNVProgressDialog,
  UNetworkVisualiserDataSQLAgent,
  UErrorHandlingOperations;

{ TVNVEventHandler }

procedure TVNVEventHandler.CreateMemberObjects;
const OPNAME = 'TVNVEventHandler.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FOutputSelector        := nil;
    FGISViewer             := nil;
    FGISForm               := nil;
    FBlockStudyDataChanges := False;
    FDrawing               := TDrawing.Create(FAppModules);

    FReservoirList          := TStringList.Create;
    FSubCatchmentList       := TStringList.Create;
    FNodeList               := TStringList.Create;
    FChannelList            := TStringList.Create;
    FResPenaltyList         := TStringList.Create;
    FChanPenaltyList        := TStringList.Create;
    FOutputList             := TStringList.Create;
    FIrrigationList         := TStringList.Create;
    FIrrigationAreaList     := TStringList.Create;
    FWetlandList            := TStringList.Create;
    FDemandCentreList       := TStringList.Create;
    FDoNotRecalcCoordList   := TStringList.Create;
    FMineList               := TStringList.Create;
    FGroundWaterList        := TStringList.Create;
    FPowerPlantList         := TStringList.Create;

    FBufferReservoirList    := TStringList.Create;
    FBufferNodeHasSFRAList  := TStringList.Create;
    FBufferSubCatchmentList := TStringList.Create;
    FBufferNodeList         := TStringList.Create;
    FBufferChannelList      := TStringList.Create;
    FBufferResPenaltyList   := TStringList.Create;
    FBufferChanPenaltyList  := TStringList.Create;
    FBufferTextList         := TStringList.Create;
    FBufferInflowList       := TStringList.Create;
    FBufferIrrBlockList     := TStringList.Create;
    FBufferIrrAreaList      := TStringList.Create;
    FBufferWetlandList      := TStringList.Create;
    FBufferDemandCentreList := TStringList.Create;
    FBufferMineList         := TStringList.Create;
    FBufferGroundWaterList  := TStringList.Create;
    FBufferPowerPlantList   := TStringList.Create;

    FNodeExistNew            := 1;
    FReservoirExistNew       := 1;
    FSubCatchmentExistNew    := 1;
    FChannelExistNew         := 1;
    FIrrigationExistNew      := 1;
    FIrrigationAreaExistNew  := 1;
    FWetlandExistNew         := 1;
    FDemandCentreExistNew    := 1;
    FPowerPlantExistNew      := 1;

    FNodeWithInflow          := 1;

    FNodeDuplicate           := 0;
    FReservoirDuplicate      := 0;
    FSubCatchmentDuplicate   := 0;
    FChannelDuplicate        := 0;
    FResPenaltyDuplicate     := 0;
    FChanPenaltyDuplicate    := 0;
    FIrrigationDuplicate     := 0;
    FIrrigationAreaDuplicate := 0;
    FWetlandDuplicate        := 0;
    FDemandCentreDuplicate   := 0;
    FPowerPlantDuplicate     := 0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.DestroyMemberObjects;
const OPNAME = 'TVNVEventHandler.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil(FGISViewer);
    FreeAndNil(FGISForm);

    FreeAndNil(FDrawing);
    FreeAndNil(FReservoirList);
    FreeAndNil(FSubCatchmentList);
    FreeAndNil(FNodeList);
    FreeAndNil(FChannelList);
    FreeAndNil(FResPenaltyList);
    FreeAndNil(FChanPenaltyList);
    FreeAndNil(FOutputList);
    FreeAndNil(FIrrigationList);
    FreeAndNil(FIrrigationAreaList);
    FreeAndNil(FWetlandList);
    FreeAndNil(FDemandCentreList);
    FreeAndNil(FDoNotRecalcCoordList);
    FreeAndNil(FMineList);
    FreeAndNil(FGroundWaterList);
    FreeAndNil(FPowerPlantList);

    FreeAndNil(FBufferReservoirList);
    FreeAndNil(FBufferNodeHasSFRAList);
    FreeAndNil(FSubCatchmentList);
    FreeAndNil(FBufferNodeList);
    FreeAndNil(FBufferChannelList);
    FreeAndNil(FBufferResPenaltyList);
    FreeAndNil(FBufferChanPenaltyList);
    FreeAndNil(FBufferTextList);
    FreeAndNil(FBufferInflowList);
    FreeAndNil(FBufferIrrBlockList);
    FreeAndNil(FBufferIrrAreaList);
    FreeandNil(FBufferWetlandList);
    FreeandNil(FBufferDemandCentreList);
    FreeAndNil(FBufferMineList);
    FreeAndNil(FBufferGroundWaterList);
    FreeAndNil(FBufferPowerPlantList);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.CreateGISViewer;
const OPNAME = 'TVNVEventHandler.CreateGISViewer';
begin
  try
    FGISForm          := TOKCancelForm.CreateWithoutDFM(nil, FAppModules);
    FGISViewer        := TGisViewerX.Create(FGISForm);
    FGISViewer.Parent := FGISForm;
    FGISForm.SendToBack;
    with FGISViewer do
    begin
      Display_StatusBar  := True;
      Display_DataTab    := False;
      Display_ToolBar    := True;
      Display_Legend     := True;
      Display_ScrollBars := False;

      Width := cGISWidth;
      Height := cGISHeight;
      AxBorderStyle := 0;
      Map_SetFilePathCovers(GISCoversDirectory);

    // Load the view settings for the study selection mode.
      Chart_ClearAll;
      Control_Initialize;
      Visible := True;
    end; //with

  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TVNVEventHandler.HandleVNVEvent (AVisioApp       : IUnknown;
                                          AVisioDoc       : IUnknown;
                                          AVisioEventCode : integer;
                                          ASourceObj      : IUnknown;
                                          AEventID        : Integer;
                                          AEventSeqNum    : Integer;
                                          ASubjectObj     : IUnknown;
                                          AMoreInfo       : OleVariant): boolean;
const OPNAME = 'TVNVEventHandler.HandleVNVEvent';
const visEvtAddShape      = -32704;
const visEvtAddConnection = -32512;
var
  lShape         : IVShape;
  lTargetShape   : IVShape;
  lSourceShape   : IVShape;
  lCell          : IVCell;
begin
  Result := FALSE;
  try
    FVisioApp := AVisioApp as IVApplication;
    FVisioDoc := AVisioDoc as IVDocument;
    FVisioEventCode := AVisioEventCode;

    case AVisioEventCode of

      visEvtCodeDocOpen :
        OnDocumentOpened(Self, FVisioDoc);
      visEvtShape + visEvtAdd,
      visEvtAddShape :
      begin
        lShape := ASubjectObj as IVShape;
        OnShapeAdded(Self, lShape);
      end;
      visEvtCodeShapeDelete :
      begin
      end;
      visEvtDel + visEvtShape :
      begin
        lShape := ASubjectObj as IVShape;
        OnShapeDelete(Self, lShape);
      end;
      visEvtAdd + visEvtConnect,
      visEvtAddConnection :
      begin
        lTargetShape := (ASubjectObj as IVConnects).ToSheet;
        lSourceShape := (ASubjectObj as IVConnects).FromSheet;
        if (Pos(PChar(mtWRYMChannel), lSourceShape.Name) > 0) then
          AddConnection(lSourceShape, lTargetShape);
      end;
      visEvtDel + visEvtConnect:
      begin
        lTargetShape := (ASubjectObj as IVConnects).ToSheet;
        lSourceShape := (ASubjectObj as IVConnects).FromSheet;
        if (Pos(PChar(mtWRYMChannel), lSourceShape.Name) > 0) then
          DeleteConnection(lSourceShape, lTargetShape);
      end;
      visEvtMod + visEvtCell:
      begin
        lCell := ASubjectObj as IVCell;
        if (lCell.Name = cPinX)   OR (lCell.Name = cPinY) OR
           (lCell.Name = cBeginX) OR (lCell.Name = cBeginY) OR
           (lCell.Name = cEndX)   OR (lCell.Name = cEndY) then
          PositionChanged(lCell, lCell.Shape)
        else
        if (lCell.Name = 'LineColor') AND
           ((Pos(PChar(mtWRYMChannel), lCell.Shape.Name) > 0) OR
            (Pos(PChar(mtWRYMChanPenalty), lCell.Shape.Name) > 0)) then
          ColourChanged(lCell, lCell.Shape);
      end;
    else
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.CheckUpgradeReservoir (AShape : IVShape);
const OPNAME = 'TVNVEventHandler.CheckUpgradeReservoir';
begin
  try
    { Upgrade to 2.12 }
    if (AShape.CellExists[cVersion, 0] = 0) then
    begin
      if (AShape.CellExists[cResNr, 0] = -1) then
        FBufferReservoirList.Add(AShape.Name);
    end
    else
    { Upgrade to 2.12.00 }
    if (AShape.Cells[cVersion].Formula = '"2.12"') OR
       (AShape.Cells[cVersion].Formula = '""') OR
       (AShape.Cells[cVersion].Formula = '') then
    begin
      FBufferReservoirList.Add(AShape.Name);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.UpgradeReservoir (AShape : IVShape) : IVShape;
const OPNAME = 'TVNVEventHandler.UpgradeReservoir';
var
  lElementID   : integer;
  lXVal        : double;
  lYVal        : double;
  lXPin        : double;
  lYPin        : double;
  lMaster      : IVMaster;
  lInflowShape : IVShape;
  lTxtLabel    : IVShape;
  lShape       : IVShape;
  lIndex       : integer;
begin
  Result := nil;
  try
    { Upgrade to 2.12 }
    if (AShape.CellExists[cVersion, 0] = 0) then
    begin
      lTxtLabel := FindTextWithParent(AShape.Name);
      if (lTxtLabel <> nil) then
        lTxtLabel.Delete;
      lTxtLabel := nil;
      lElementID := StrToIntDef(Trim(UnQuote(AShape.Cells[cResNr].Formula)),0);
      AShape.XYToPage(0, 0, lXVal, lYVal);
      lXPin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinX].ResultStr[visMillimeters])),0);
      lYPin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinY].ResultStr[visMillimeters])),0);
      AShape.Delete;
      lMaster := FindMaster(mtWRYMReservoir);
      if (lMaster <> nil) then
      begin
        lShape := FVisioApp.ActivePage.Drop(lMaster, lXVal, lYVal);
        lShape.Cells[cNumber].Formula  := '"' + IntToStr(lElementID) + '"';
        lShape.Cells[cPinX].Formula    := '"' + FloatToStrF(lXPin, ffFixed, 12, 2) + 'mm"';
        lShape.Cells[cPinY].Formula    := '"' + FloatToStrF(lYPin, ffFixed, 12, 2) + 'mm"';
        lShape.Cells[cVersion].Formula := '"2.12"';
        lInflowShape := AddInflowSymbol(lShape, lElementID);
        lTxtLabel := AddTextLabel(lShape, nvtName);
        RefreshText(lTxtLabel);
      end;
      Result := lShape;
    end;
    { Upgrade to 2.12.00 }
    if (AShape.CellExists[cVersion, 0] <> 0) AND
       ((AShape.Cells[cVersion].Formula = '"2.12"') OR
        (AShape.Cells[cVersion].Formula = '""') OR
        (AShape.Cells[cVersion].Formula = '')) then
    begin
      if (AShape.SectionExists[visSectionAction, 0] = 0) then
      begin
        AShape.AddSection(visSectionAction);
        AShape.AddRow(visSectionAction, visRowAction, visTagDefault);
      end;
      lIndex := AShape.RowCount[visSectionAction];
      while (lIndex > 1) do
      begin
        AShape.DeleteRow(visSectionAction, lIndex-1);
        lIndex := AShape.RowCount[visSectionAction];
      end;
      AShape.Cells[cRow1Menu].Formula   := '"' + FAppModules.Language.GetString('VNV.ReservoirOptions') + '"';
      AShape.Cells[cRow1Action].Formula := 'RUNMACRO("ThisDocument.ProcessVNVSpecial")';
      AShape.Cells[cVersion].Formula    := '"2.12.00"';
      Result := AShape;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.UpgradeSubCatchment (AShape : IVShape) : IVShape;
const OPNAME = 'TVNVEventHandler.UpgradeSubCatchment';
var
  lElementID   : integer;
  lXVal        : double;
  lYVal        : double;
  lXPin        : double;
  lYPin        : double;
  lMaster      : IVMaster;
  lInflowShape : IVShape;
  lTxtLabel    : IVShape;
  lShape       : IVShape;
  lIndex       : integer;
begin
  Result := nil;
  try
    { Upgrade to 2.12 }
    if (AShape.CellExists[cVersion, 0] = 0) then
    begin
      lTxtLabel := FindTextWithParent(AShape.Name);
      if (lTxtLabel <> nil) then
        lTxtLabel.Delete;
      lTxtLabel := nil;
      lElementID := StrToIntDef(Trim(UnQuote(AShape.Cells[cResNr].Formula)),0);
      AShape.XYToPage(0, 0, lXVal, lYVal);
      lXPin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinX].ResultStr[visMillimeters])),0);
      lYPin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinY].ResultStr[visMillimeters])),0);
      AShape.Delete;
      lMaster := FindMaster(mtWRYMReservoir);
      if (lMaster <> nil) then
      begin
        lShape := FVisioApp.ActivePage.Drop(lMaster, lXVal, lYVal);
        lShape.Cells[cNumber].Formula  := '"' + IntToStr(lElementID) + '"';
        lShape.Cells[cPinX].Formula    := '"' + FloatToStrF(lXPin, ffFixed, 12, 2) + 'mm"';
        lShape.Cells[cPinY].Formula    := '"' + FloatToStrF(lYPin, ffFixed, 12, 2) + 'mm"';
        lShape.Cells[cVersion].Formula := '"2.12"';
        lInflowShape := AddInflowSymbol(lShape, lElementID);
        lTxtLabel := AddTextLabel(lShape, nvtName);
        RefreshText(lTxtLabel);
      end;
      Result := lShape;
    end;
    { Upgrade to 2.12.00 }
    if (AShape.CellExists[cVersion, 0] <> 0) AND
       ((AShape.Cells[cVersion].Formula = '"2.12"') OR
        (AShape.Cells[cVersion].Formula = '""') OR
        (AShape.Cells[cVersion].Formula = '')) then
    begin
      if (AShape.SectionExists[visSectionAction, 0] = 0) then
      begin
        AShape.AddSection(visSectionAction);
        AShape.AddRow(visSectionAction, visRowAction, visTagDefault);
      end;
      lIndex := AShape.RowCount[visSectionAction];
      while (lIndex > 1) do
      begin
        AShape.DeleteRow(visSectionAction, lIndex-1);
        lIndex := AShape.RowCount[visSectionAction];
      end;
      AShape.Cells[cRow1Action].Formula := 'RUNMACRO("ThisDocument.ProcessVNVSpecial")';
      AShape.Cells[cVersion].Formula    := '"2.12.00"'; 
      Result := AShape;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TVNVEventHandler.CheckUpgradeInflow (AShape : IVShape);
const OPNAME = 'TVNVEventHandler.CheckUpgradeInflow';
begin
  try
    { Upgrade to 2.14.1 }
    if (AShape.Cells[cVersion].Formula <> '"2.14.1"') then
    begin
      FBufferInflowList.Add(AShape.Name);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.UpgradeInflow (AShape : IVShape) : IVShape;
const OPNAME = 'TVNVEventHandler.UpgradeInflow';
var
  lElementID      : integer;
  lYieldModelData : IYieldModelData;
  lReservoir      : IReservoirData;
begin
  Result := nil;
  try
    { Upgrade to 2.14.1 }
    if (AShape.Cells[cVersion].Formula <> '"2.14.1"') then
    begin
      if (AShape.CellExists[cCatchmentNumber, 0] = 0) then
      begin
        AShape.AddNamedRow(visSectionProp, 'CatchmentNumber', visTagDefault);
        AShape.Cells[cCatchmentNumber].Formula := '"0"';
        AShape.Cells[cCatchmentLbl].Formula    := '"Cathment number"';
      end;
      
      lElementID      := StrToIntDef(Trim(UnQuote(AShape.Cells[cNumber].Formula)),0);
      lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
      lReservoir      := lYieldModelData.NetworkElementData.ReservoirList.ReservoirByIdentifier[lElementID];
      if (lReservoir <> nil) then
        AShape.Cells[cCatchmentNumber].Formula := '"' + IntToStr(lReservoir.ReservoirConfigurationData.CatchmentRef) + '"';

      AShape.Cells[cVersion].Formula    := '"2.14.1"';
      Result := AShape;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.CheckUpgradeNode (AShape : IVShape);
const OPNAME = 'TVNVEventHandler.CheckUpgradeNode';
begin
  try
    { Upgrade to 2.12 }
    if (AShape.CellExists[cVersion, 0] = 0) then
    begin
      if (AShape.CellExists[cNodeID, 0] = -1) then
        FBufferNodeList.Add(AShape.Name);
    end
    else
    { Upgrade to 2.12.00 }
    if (AShape.Cells[cVersion].Formula = '"2.12"') OR
       (AShape.Cells[cVersion].Formula = '""') OR
       (AShape.Cells[cVersion].Formula = '') then
      FBufferNodeList.Add(AShape.Name);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.UpgradeNode (AShape : IVShape) : IVShape;
const OPNAME = 'TVNVEventHandler.UpgradeNode';
var
  lElementID   : integer;
  lXVal        : double;
  lYVal        : double;
  lXPin        : double;
  lYPin        : double;
  lMaster      : IVMaster;
  lYieldModel  : IYieldModel;
  lNode        : IReservoirData;
  lTxtLabel    : IVShape;
  lShape       : IVShape;
  lIndex       : integer;
begin
  Result := nil;
  try
    { Upgrade to 2.12 }
    if (AShape.CellExists[cVersion, 0] = 0) then
    begin
      lTxtLabel := FindTextWithParent(AShape.Name);
      if (lTxtLabel <> nil) then
        lTxtLabel.Delete;
      lTxtLabel := nil;
      lElementID := StrToIntDef(Trim(UnQuote(AShape.Cells[cNodeID].Formula)),0);
      AShape.XYToPage(0, 0, lXVal, lYVal);
      lXPin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinX].ResultStr[visMillimeters])),0);
      lYPin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinY].ResultStr[visMillimeters])),0);
      AShape.Delete;
      lMaster := FindMaster(mtWRYMNode);
      if (lMaster <> nil) then
      begin
        lShape := FVisioApp.ActivePage.Drop(lMaster, lXVal, lYVal);
        lShape.Cells[cNumber].Formula  := '"' + IntToStr(lElementID) + '"';
        lShape.Cells[cPinX].Formula    := '"' + FloatToStrF(lXPin, ffFixed, 12, 2) + 'mm"';
        lShape.Cells[cPinY].Formula    := '"' + FloatToStrF(lYPin, ffFixed, 12, 2) + 'mm"';
        lShape.Cells[cVersion].Formula := '"2.12"';
        lYieldModel := FAppModules.Model as IYieldModel;
        lNode := (lYieldModel.YieldModelData as IYieldModelData).NetworkElementData.
                   ReservoirList.ReservoirOrNodeByIdentifier[lElementID];
        lTxtLabel := AddTextLabel(lShape, nvtName);
        RefreshText(lTxtLabel);
        if (lNode <> nil) AND (lNode.ReservoirConfigurationData.NodeType = ntNodeWithInflow) then
          AddInflowSymbol(lShape, lElementID);
      end;
      Result := lShape;
    end;
    { Upgrade to 2.12.00 }
    if (AShape.CellExists[cVersion, 0] <> 0) AND
       ((AShape.Cells[cVersion].Formula = '"2.12"') OR
        (AShape.Cells[cVersion].Formula = '""') OR
        (AShape.Cells[cVersion].Formula = '')) then
    begin
      if (AShape.SectionExists[visSectionAction, 0] = 0) then
      begin
        AShape.AddSection(visSectionAction);
        AShape.AddRow(visSectionAction, visRowAction, visTagDefault);
      end;
      lIndex := AShape.RowCount[visSectionAction];
      while (lIndex > 1) do
      begin
        AShape.DeleteRow(visSectionAction, lIndex-1);
        lIndex := AShape.RowCount[visSectionAction];
      end;
      AShape.Cells[cRow1Menu].Formula   := '"' + FAppModules.Language.GetString('VNV.ViewNodeProperties') + '"';
      AShape.Cells[cRow1Action].Formula := 'RUNMACRO("ThisDocument.ProcessVNVSpecial")';
      AShape.Cells[cVersion].Formula    := '"2.12.00"';
      Result := AShape;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.FindMaster (AMasterName : WideString) : IVMaster;
const OPNAME = 'TVNVEventHandler.FindMaster';
var
  lMaster  : IVMaster;
  lIndex   : integer;
begin
  try
    Result := nil;
    if (FWRYMStencil = nil) then
      SetCurrentStencil;

    if (FWRYMStencil <> nil) then
    begin
      lIndex := 1;
      while ((Result = nil) AND (lIndex <= FWRYMStencil.Masters.Count)) do
      begin
        lMaster := FWRYMStencil.Masters.Item[lIndex];
        if (lMaster.Name = AMasterName) then
          Result := lMaster
        else
          lIndex := lIndex + 1;
      end;
    end
    else
    begin
      if(FVisioApp.Documents.Item[cWRYMStencilName] <> nil) then
        Result := FVisioApp.Documents.Item[cWRYMStencilName].Masters.ItemU[AMasterName];
      if(Result = nil) then
      begin
        if(FVisioApp.Documents.Item[cWRPMStencilName] <> nil) then
          Result := FVisioApp.Documents.Item[cWRPMStencilName].Masters.ItemU[AMasterName];
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.CheckUpgradeChannel (AShape : IVShape);
const OPNAME = 'TVNVEventHandler.CheckUpgradeChannel';
begin
  try
    { Upgrade to 2.12 }
    if (AShape.CellExists[cVersion, 0] = 0) then
    begin
      if (AShape.CellExists[cChannelID, 0] = -1) then
        FBufferChannelList.Add(AShape.Name);
    end
    else
    { Upgrade to 2.12.00 }
    if (AShape.Cells[cVersion].Formula = '"2.12"') OR
       (AShape.Cells[cVersion].Formula = '""') OR
       (AShape.Cells[cVersion].Formula = '') then
      FBufferChannelList.Add(AShape.Name);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.UpgradeChannel (AShape : IVShape) : IVShape;
const OPNAME = 'TVNVEventHandler.UpgradeChannel';
var
  lElementID   : integer;
  lXVal        : double;
  lYVal        : double;
  lXBegin      : double;
  lXEnd        : double;
  lYBegin      : double;
  lYEnd        : double;
  lMaster      : IVMaster;
  lTxtLabel    : IVShape;
  lShape       : IVShape;
  lIndex       : integer;
begin
  Result := nil;
  try
    { Upgrade to 2.12 }
    if (AShape.CellExists[cVersion, 0] = 0) then
    begin
      lTxtLabel := FindTextWithParent(AShape.Name);
      if (lTxtLabel <> nil) then
        lTxtLabel.Delete;
      lTxtLabel := nil;
      lElementID := StrToIntDef(Trim(UnQuote(AShape.Cells[cChannelID].Formula)),0);
      AShape.XYToPage(0, 0, lXVal, lYVal);
      lXBegin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cBeginX].ResultStr[visMillimeters])),0);
      lXEnd   := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cEndX].ResultStr[visMillimeters])),0);
      lYBegin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cBeginY].ResultStr[visMillimeters])),0);
      lYEnd   := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cEndY].ResultStr[visMillimeters])),0);
      AShape.Delete;
      lMaster := FindMaster(mtWRYMChannel);
      if (lMaster <> nil) then
      begin
        lShape := FVisioApp.ActivePage.Drop(lMaster, lXVal, lYVal);
        lShape.Cells[cNumber].Formula  := '"' + IntToStr(lElementID) + '"';
        lShape.Cells[cBeginX].Formula  := '"' + FloatToStrF(lXBegin, ffFixed, 12, 2) + 'mm"';
        lShape.Cells[cEndX].Formula    := '"' + FloatToStrF(lXEnd, ffFixed, 12, 2) + 'mm"';
        lShape.Cells[cBeginY].Formula  := '"' + FloatToStrF(lYBegin, ffFixed, 12, 2) + 'mm"';
        lShape.Cells[cEndY].Formula    := '"' + FloatToStrF(lYEnd, ffFixed, 12, 2) + 'mm"';
        lShape.Cells[cVersion].Formula := '"2.12"';
        lTxtLabel := AddTextLabel(lShape, nvtName);
        RefreshText(lTxtLabel);
      end;
      Result := lShape;
    end;
    { Upgrade to 2.12.00 }
    if (AShape.CellExists[cVersion, 0] <> 0) AND
       ((AShape.Cells[cVersion].Formula = '"2.12"') OR
        (AShape.Cells[cVersion].Formula = '""') OR
        (AShape.Cells[cVersion].Formula = '')) then
    begin
      if (AShape.SectionExists[visSectionAction, 0] = 0) then
      begin
        AShape.AddSection(visSectionAction);
        AShape.AddRow(visSectionAction, visRowAction, visTagDefault);
      end;
      lIndex := AShape.RowCount[visSectionAction];
      while (lIndex > 1) do
      begin
        AShape.DeleteRow(visSectionAction, lIndex-1);
        lIndex := AShape.RowCount[visSectionAction];
      end;
      AShape.Cells[cRow1Menu].Formula   := '"' + FAppModules.Language.GetString('VNV.ChannelOptions') + '"';
      AShape.Cells[cRow1Action].Formula := 'RUNMACRO("ThisDocument.ProcessVNVSpecial")';
      AShape.Cells[cVersion].Formula    := '"2.12.00"';
      Result := AShape;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.CheckUpgradeResPenalty (AShape : IVShape);
const OPNAME = 'TVNVEventHandler.CheckUpgradeResPenalty';
begin
  try
    if (AShape.CellExists[cNumber, 0] = 0) then
    begin
      if (AShape.CellExists[cResID, 0] = -1) then
        FBufferResPenaltyList.Add(AShape.Name);
    end;
    if (AShape.CellExists[cPermanent, 0] = 0) then
    begin
      FBufferResPenaltyList.Add(AShape.Name);
    end
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.UpgradeReservoirPenalty (AShape : IVShape) : IVShape;
const OPNAME = 'TVNVEventHandler.UpgradeReservoirPenalty';
var
  lElementID : string;
  lMaster    : IVMaster;
  lXVal      : double;
  lYVal      : double;
  lXPin      : double;
  lYPin      : double;
  lShape     : IVShape;
  lTxtLabel  : IVShape;
begin
  Result := nil;
  try
    lShape := AShape;
    if (AShape.CellExists[cNumber, 0] = 0) then
    begin
      lTxtLabel := FindTextWithParent(AShape.Name);
      if (lTxtLabel <> nil) then
        lTxtLabel.Delete;
      lTxtLabel := nil;
      lElementID := UnQuote(AShape.Cells[cResID].Formula);
      AShape.XYToPage(0, 0, lXVal, lYVal);
      lXPin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinX].ResultStr[visMillimeters])),0);
      lYPin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinY].ResultStr[visMillimeters])),0);
      AShape.Delete;
      lMaster := FindMaster(mtWRYMResPenalty); //mtWRYMResPenaltyExt not done
      if (lMaster <> nil) then
      begin
        lShape := FVisioApp.ActivePage.Drop(lMaster, lXVal, lYVal);
        lShape.Cells[cNumber].Formula  := '"' + lElementID + '"';
        lShape.Cells[cPinX].Formula    := '"' + FloatToStrF(lXPin, ffFixed, 12, 2) + 'mm"';
        lShape.Cells[cPinY].Formula    := '"' + FloatToStrF(lYPin, ffFixed, 12, 2) + 'mm"';
        lShape.Cells[cVersion].Formula := '"2.12"';
      end;
    end;

    if (lShape.CellExists[cPermanent, 0] = 0) then
    begin
      lShape.AddNamedRow(visSectionProp, 'Permanent', visTagDefault);
      lShape.Cells[cPermanentType].Formula     := '"0"';
      lShape.Cells[cPermanentLabel].Formula    := '"Permanent"';
      lShape.Cells[cPermanent].Formula         := '"FALSE"';

      if (AShape.CellExists[cRow1Menu, 0] = 0) then
      begin
        AShape.AddNamedRow(visSectionAction, 'Row_1', visTagDefault);
        AShape.Cells[cRow1Menu].Formula   := '"' + FAppModules.Language.GetString('VNV.ViewReservoirPenaltyProperties') + '"';
        AShape.Cells[cRow1Action].Formula := 'RUNMACRO("ThisDocument.ProcessVNVSpecial 1")';
      end;
    end;
    Result := lShape;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.CheckUpgradeText (AShape : IVShape);
const OPNAME = 'TVNVEventHandler.CheckUpgradeText';
var
  lTempStr : string;
begin
  try
    if (AShape.CellExists[cParent, 0] = -1) AND
       (Trim(UnQuote(AShape.Cells[cParent].Formula)) <> '') AND
       (AShape.Cells[cVersion].Formula <> '"2.13"') then
    begin
      lTempStr := UnQuote(AShape.Cells[cParent].Formula);
      if (Trim(lTempStr) <> '') then
        FBufferTextList.Add(AShape.Name);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.UpgradeText (AShape : IVShape) : IVShape;
const OPNAME = 'TVNVEventHandler.UpgradeText';
var
  lParentShape : IVShape;
  lParentName  : string;
  lIndex       : integer;
  lTextType    : TVNVTextType;
begin
  try
    //Do not upgrade output text labels.
    lTextType := nvtNone;
    if (AShape.CellExists[cTextType, 1] <> 0) AND
       (AShape.CellExists[cNumber, 1] <> 0) then
    begin
      lTextType  := TVNVTextType(StrToIntDef(UnQuote(AShape.Cells[cTextType].Formula),0));
    end;

    if(lTextType in OutputTextTypeSet) then
    begin
      if (AShape.CellExists[cPermanent, 0] = 0) then
      begin
        AShape.AddNamedRow(visSectionProp, 'Permanent', visTagDefault);
        AShape.Cells[cPermanentType].Formula     := '"0"';
        AShape.Cells[cPermanentLabel].Formula    := '"Permanent"';
        AShape.Cells[cPermanent].Formula         := '"FALSE"';

        if (AShape.CellExists[cRow1Menu, 0] = 0) then
        begin
          AShape.AddNamedRow(visSectionAction, 'Row_1', visTagDefault);
          AShape.Cells[cRow1Menu].Formula   := '"' + FAppModules.Language.GetString('VNV.ViewTextProperties') + '"';
          AShape.Cells[cRow1Action].Formula := 'RUNMACRO("ThisDocument.ProcessVNVSpecial 1")';
        end;
      end;
    end
    else
    begin
      if (AShape.CellExists[cTextType, 0] <> 0) then
      begin
        lIndex := AShape.CellsRowIndex[cTextType];
        AShape.DeleteRow(visSectionProp, lIndex);
      end;
      lParentName  := UnQuote(AShape.Cells[cParent].Formula);
      lParentShape := FindShapeWithName(lParentName);
      if (lParentShape = nil) then
      begin
        AShape.Cells[cParent].Formula := '""';
      end
      else
      begin
        // Add Number property
        if (AShape.CellExists[cNumber, 0] = 0) then
          AShape.AddNamedRow(visSectionProp, 'Number', visTagDefault);
        if (lParentShape <> nil) then
          AShape.Cells[cNumber].Formula := lParentShape.Cells[cNumber].Formula
        else
          AShape.Cells[cNumber].Formula := '"-1"';

        // Add TextType property
        if (AShape.CellExists[cTextType, 0] = 0) then
          AShape.AddNamedRow(visSectionProp, 'Type', visTagDefault);

        if (Pos(PChar(mtWRYMInflow), lParentShape.Name) > 0) then
        begin
          AShape.Cells[cTextType].Formula := '"' + IntToStr(Ord(nvtInflow)) + '"';
          AShape.Cells[cTextLbl].Formula  := '"' + TextTypeToStr(nvtInflow) + '"';
        end
        else
        begin
          AShape.Cells[cTextType].Formula := '"' + IntToStr(Ord(nvtName)) + '"';
          AShape.Cells[cTextLbl].Formula  := '"' + TextTypeToStr(nvtName) + '"';
        end;
        AShape.Cells[cTextInv].Formula  := '"TRUE"';
        AShape.Cells[cVersion].Formula := '"2.13"';
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
 end;

procedure TVNVEventHandler.CheckUpgradeChanPenalty (AShape : IVShape);
const OPNAME = 'TVNVEventHandler.CheckUpgradeChanPenalty';
begin
  try
    if (AShape.CellExists[cNumber, 0] = 0) then
    begin
      if (AShape.CellExists[cChannelID, 0] = -1) then
        FBufferChanPenaltyList.Add(AShape.Name);
    end;
    if (AShape.CellExists[cPermanent, 0] = 0) then
    begin
        FBufferChanPenaltyList.Add(AShape.Name);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.UpgradeChannelPenalty (AShape : IVShape) : IVShape;
const OPNAME = 'TVNVEventHandler.UpgradeChannelPenalty';
var
  lElementID : string;
  lMaster    : IVMaster;
  lXVal      : double;
  lYVal      : double;
  lXPin      : double;
  lYPin      : double;
  lShape     : IVShape;
  lTxtLabel  : IVShape;
begin
  Result := nil;
  try
    lShape := AShape;
    if (AShape.CellExists[cNumber, 0] = 0) then
    begin
      lTxtLabel := FindTextWithParent(AShape.Name);
      if (lTxtLabel <> nil) then
        lTxtLabel.Delete;
      lTxtLabel := nil;
      lElementID := UnQuote(AShape.Cells[cChannelID].Formula);
      AShape.XYToPage(0, 0, lXVal, lYVal);
      lXPin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinX].ResultStr[visMillimeters])),0);
      lYPin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinY].ResultStr[visMillimeters])),0);
      AShape.Delete;
      lMaster := FindMaster(mtWRYMChanPenalty);
      if (lMaster <> nil) then
      begin
        lShape := FVisioApp.ActivePage.Drop(lMaster, lXVal, lYVal);
        lShape.Cells[cNumber].Formula  := '"' + lElementID + '"';
        lShape.Cells[cPinX].Formula    := '"' + FloatToStrF(lXPin, ffFixed, 12, 2) + 'mm"';
        lShape.Cells[cPinY].Formula    := '"' + FloatToStrF(lYPin, ffFixed, 12, 2) + 'mm"';
        lShape.Cells[cVersion].Formula := '"2.12"';
      end;
    end;
    if (lShape.CellExists[cPermanent, 0] = 0) then
    begin
      lShape.AddNamedRow(visSectionProp, 'Permanent', visTagDefault);
      lShape.Cells[cPermanentType].Formula     := '"0"';
      lShape.Cells[cPermanentLabel].Formula    := '"Permanent"';
      lShape.Cells[cPermanent].Formula         := '"FALSE"';

      if (AShape.CellExists[cRow1Menu, 0] = 0) then
      begin
        AShape.AddNamedRow(visSectionAction, 'Row_1', visTagDefault);
        AShape.Cells[cRow1Menu].Formula   := '"' + FAppModules.Language.GetString('VNV.ViewChannelPenaltyProperties') + '"';
        AShape.Cells[cRow1Action].Formula := 'RUNMACRO("ThisDocument.ProcessVNVSpecial 1")';
      end;
    end;
    Result := lShape;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.OnDocumentOpened (ASender    : TObject;
                                             const ADoc : IVDocument);
const OPNAME = 'TVNVEventHandler.OnDocumentOpened';
begin
  try
    FReservoirList.Clear;
    FMineList.Clear;
    FGroundWaterList.Clear;
    FSubCatchmentList.Clear;
    FNodeList.Clear;
    FChannelList.Clear;
    FResPenaltyList.Clear;
    FChanPenaltyList.Clear;
    FOutputList.Clear;
    FOutputSelector := nil;
    FIrrigationList.Clear;
    FIrrigationAreaList.Clear;
    FDoNotRecalcCoordList.Clear;
    FPowerPlantList.Clear;
    SetStencilPath;
    FWRYMStencil := nil;
    SetCurrentStencil;

    InitialiseDrawing(ADoc);
    GetSelectionData;
    UpgradeDocument(ADoc);
    RefreshDocument(ADoc);


    if FDrawing.GISMode then
      ShowGIS
    else
      HideGIS;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.SetStencilPath;
const OPNAME = 'TVNVEventHandler.SetStencilPath';
var
  lWRYMPath    : string;
  lWRPMPath    : string;
  lStencilPaths : string;
  lVNVPath      : string;
  lMsg          : string;
begin
  try
    lStencilPaths := Trim(FVisioApp.StencilPaths);
    lVNVPath      := NetworkDiagramsPath;
    lWRYMPath    := lVNVPath + cWRYMStencilName;
    lWRPMPath    := lVNVPath + cWRPMStencilName;
    lVNVPath      := Copy(lVNVPath, 1, Length(lVNVPath) - 1);

    if (Pos(UpperCase(lVNVPath), UpperCase(lStencilPaths)) = 0) then
    begin
      if (FileExists(lWRYMPath)) or (FileExists(lWRPMPath)) then
      begin
        if (lStencilPaths = '') then
          FVisioApp.StencilPaths := lVNVPath
        else
          FVisioApp.StencilPaths := lStencilPaths + ';' + lVNVPath;
      end
      else
      begin
        lMsg := FAppModules.Language.GetString('VNV.StencilDoesNotExist');
        lMsg := Format(lMsg, [lWRYMPath+' or '+lWRPMPath]);
        MessageDlg(lMsg, mtError, [mbOK], 0);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.SetCurrentStencil;
const OPNAME = 'TVNVEventHandler.SetCurrentStencil';
var
  lIndex       : integer;
  lDocument    : IVDocument;
begin
  try
    lIndex       := 1;
    while ((FWRYMStencil = nil) AND (lIndex <= FVisioApp.Documents.Count)) do
    begin
      lDocument := FVisioApp.Documents.Item[lIndex];
      if (Trim(UpperCase(lDocument.Name)) = UpperCase(cWRYMStencilName)) or
         (Trim(UpperCase(lDocument.Name)) = UpperCase(cWRPMStencilName))then
        FWRYMStencil := lDocument
      else
        lIndex := lIndex + 1;
    end;
   except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.UpgradeDocument (const ADoc : IVDocument);
const OPNAME = 'TVNVEventHandler.UpgradeDocument';
var
  //lPageIdx     : integer;
  lShapeIdx    : integer;
  lPage        : IVPage;
  lShape       : IVShape;
  lShapeName   : string;
  lVersion     : string;
begin
  try
    FBufferReservoirList.Clear;
    FBufferNodeHasSFRAList.Clear;
    FBufferSubCatchmentList.Clear;
    FBufferNodeList.Clear;
    FBufferChannelList.Clear;
    FBufferResPenaltyList.Clear;
    FBufferChanPenaltyList.Clear;
    FBufferTextList.Clear;
    FBufferInflowList.Clear;
    FBufferIrrBlockList.Clear;
    FBufferIrrAreaList.Clear;
    FBufferMineList.Clear;
    FBufferGroundWaterList.Clear;

    ADoc.Protection[0] := ADoc.Protection[0] - visProtectShapes;
    //for lPageIdx := 1 to ADoc.Pages.Count  do
    //begin
      lPage := ADoc.Pages.Item[1];
      lShapeIdx := 1;
      while (lShapeIdx <= lPage.Shapes.Count)  do
      begin
        lShape := lPage.Shapes.Item[lShapeIdx];
        { Delete all old inflow symbols. }
        if (Pos(PChar(mtWRYMResInflow), lShape.Name) > 0) OR
           (Pos(PChar(mtWRYMNodeInflow), lShape.Name) > 0) then
        begin
          lShape.Delete;
          lShapeIdx := lShapeIdx - 1;
        end
        else
        if (Pos(PChar(mtWRYMInflow), lShape.Name) > 0) then
          CheckUpgradeInflow(lShape)
        else
        if (Pos(PChar(mtWRYMReservoir), lShape.Name) > 0) then
          CheckUpgradeReservoir(lShape)
        else
        if (Pos(PChar(mtWRYMMine), lShape.Name) > 0) then
          CheckUpgradeMineReservoir(lShape)
        else
        if (Pos(PChar(mtWRYMGroundWater), lShape.Name) > 0) then
          CheckUpgradeGroundWaterAquifer(lShape)
        else
        if (Pos(PChar(mtWRYMNode), lShape.Name) > 0) then
          CheckUpgradeNode(lShape)
        else
        if (Pos(PChar(mtWRYMChannel), lShape.Name) > 0) then
          CheckUpgradeChannel(lShape)
        else
        if (Pos(PChar(mtWRYMResPenalty), lShape.Name) > 0) or (Pos(PChar(mtWRYMResPenaltyExt), lShape.Name) > 0) then
          CheckUpgradeResPenalty(lShape)
        else
        if (Pos(PChar(mtWRYMChanPenalty), lShape.Name) > 0) then
          CheckUpgradeChanPenalty(lShape)
        else
        if (Pos(PChar(mtWRYMText), lShape.Name) > 0) then
          CheckUpgradeText(lShape)
        else
        if (Pos(PChar(mtWRYMIrrBlock), lShape.Name) > 0) then
          CheckUpgradeIrrBlock(lShape)
        else
        if (Pos(PChar(mtWRYMIrrArea), lShape.Name) > 0) then
          CheckUpgradeIrrArea(lShape)
        else
         if (Pos(PChar(mtWRYMWetland), lShape.Name) > 0) then
          CheckUpgradeWetland(lShape)
        else
         if (Pos(PChar(mtWRYMDemandCentre), lShape.Name) > 0) then
          CheckUpgradeDemandCentre(lShape)
        else
         if (Pos(PChar(mtWRYMGroundWater), lShape.Name) > 0) then
          CheckUpgradeGroundWaterAquifer(lShape);
        lShapeIdx := lShapeIdx + 1;
      end;
    //end;

    ADoc.Protection[0] := ADoc.Protection[0] + visProtectShapes;

    for lShapeIdx := 0 to FBufferReservoirList.Count - 1 do
    begin
      lShapeName := FBufferReservoirList.Strings[lShapeIdx];
      lShape     := FindShapeWithName(lShapeName);
      if (lShape <> nil) then
        UpgradeReservoir(lShape);
    end;
    FBufferReservoirList.Clear;

    for lShapeIdx := 0 to FBufferMineList.Count - 1 do
    begin
      lShapeName := FBufferMineList.Strings[lShapeIdx];
      lShape     := FindShapeWithName(lShapeName);
      if (lShape <> nil) then
        UpgradeMineReservoir(lShape);
    end;
    FBufferMineList.Clear;

    for lShapeIdx := 0 to FBufferGroundWaterList.Count - 1 do
    begin
      lShapeName := FBufferGroundWaterList.Strings[lShapeIdx];
      lShape     := FindShapeWithName(lShapeName);
      if (lShape <> nil) then
        UpgradeGroundWaterAquifer(lShape);
    end;
    FBufferGroundWaterList.Clear;

    for lShapeIdx := 0 to FBufferNodeList.Count - 1 do
    begin
      lShapeName := FBufferNodeList.Strings[lShapeIdx];
      lShape     := FindShapeWithName(lShapeName);
      if (lShape <> nil) then
        UpgradeNode(lShape);
    end;
    FBufferNodeList.Clear;

    for lShapeIdx := 0 to FBufferChannelList.Count - 1 do
    begin
      lShapeName := FBufferChannelList.Strings[lShapeIdx];
      lShape     := FindShapeWithName(lShapeName);
      if (lShape <> nil) then
        UpgradeChannel(lShape);
    end;
    FBufferChannelList.Clear;

    for lShapeIdx := 0 to FBufferResPenaltyList.Count - 1 do
    begin
      lShapeName := FBufferResPenaltyList.Strings[lShapeIdx];
      lShape     := FindShapeWithName(lShapeName);
      if (lShape <> nil) then
        UpgradeReservoirPenalty(lShape);
    end;
    FBufferResPenaltyList.Clear;

    for lShapeIdx := 0 to FBufferChanPenaltyList.Count - 1 do
    begin
      lShapeName := FBufferChanPenaltyList.Strings[lShapeIdx];
      lShape     := FindShapeWithName(lShapeName);
      if (lShape <> nil) then
        UpgradeChannelPenalty(lShape);
    end;
    FBufferChanPenaltyList.Clear;

    for lShapeIdx := 0 to FBufferTextList.Count - 1 do
    begin
      lShapeName := FBufferTextList.Strings[lShapeIdx];
      lShape     := FindShapeWithName(lShapeName);
      if (lShape <> nil) then
        UpgradeText(lShape);
    end;
    FBufferTextList.Clear;

    for lShapeIdx := 0 to FBufferInflowList.Count - 1 do
    begin
      lShapeName := FBufferInflowList.Strings[lShapeIdx];
      lShape     := FindShapeWithName(lShapeName);
      if (lShape <> nil) then
        UpgradeInflow(lShape);
    end;
    FBufferInflowList.Clear;

    { Check that all version properties are set }
    //for lPageIdx := 1 to ADoc.Pages.Count  do
    //begin
      lPage := ADoc.Pages.Item[1];
      for lShapeIdx := 1 to lPage.Shapes.Count  do
      begin
        lShape := lPage.Shapes.Item[lShapeIdx];
        if (Pos(PChar('(WRYM)'), lShape.Name) > 0) then
        begin
          if (lShape.CellExists[cVersion, 0] = 0) then
          begin
            lShape.AddNamedRow(visSectionProp, 'Version', visTagDefault);
            lShape.Cells[cVersionLbl].Formula := '"Version"';
            lShape.Cells[cVersion].Formula    := '"' + CThisVersion + '"';
            lShape.Cells[cVersionInv].Formula := '"TRUE"';
            MessageDlg(FAppModules.Language.GetString('VisioNetwork.MsgAddedVersion') + lShape.Name, mtInformation, [mbOK], 0);
          end
          else
          begin
            lShape.Cells[cVersionLbl].Formula := '"Version"';
            lVersion := lShape.Cells[cVersion].Formula;
            if (lShape.Cells[cVersion].Formula = '""') OR
               (lShape.Cells[cVersion].Formula = '') then
            begin
              lShape.Cells[cVersion].Formula := '"' + CThisVersion + '"';
              MessageDlg(FAppModules.Language.GetString('VisioNetwork.MsgSetVersion') + lShape.Name, mtInformation, [mbOK], 0);
            end
          end;
        end;

        // Do not refresh colours
        if (lShape.CellExists[cPermanentColor, 0] = 0) then
        begin
          lShape.AddNamedRow(visSectionProp, 'PermanentColor', visTagDefault);
          lShape.Cells[cPermanentColorType].Formula     := '"0"';
          lShape.Cells[cPermanentColorLabel].Formula    := '"PermanentColor"';
          lShape.Cells[cPermanentColor].Formula         := '"FALSE"';
        end;
      end;
    //end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.RefreshDocument (const ADoc : IVDocument);
const OPNAME = 'TVNVEventHandler.RefreshDocument';
var
  //lPageIdx     : integer;
  lShapeIdx    : integer;
  lPage        : IVPage;
  lShape       : IVShape;
  lElementID   : integer;
  lProgressDlg : TVNVProgressDialog;
  lCount       : integer;
  lTextType    : TVNVTextType;
begin
  try
    FReservoirList.Clear;
    FMineList.Clear;
    FGroundWaterList.Clear;
    FSubCatchmentList.Clear;
    FNodeList.Clear;
    FChannelList.Clear;
    FResPenaltyList.Clear;
    FChanPenaltyList.Clear;
    FOutputList.Clear;
    FOutputSelector := nil;
    FIrrigationList.Clear;
    FIrrigationAreaList.Clear;
    FPowerPlantList.Clear;

  	lProgressDlg := TVNVProgressDialog.CreateWithoutDFM(nil, FAppModules);
    try
      lProgressDlg.LanguageHasChanged;
      if (lProgressDlg <> nil) then
        lProgressDlg.Show;

      lCount := 0;
      //for lPageIdx := 1 to ADoc.Pages.Count  do
      //begin
        lPage := ADoc.Pages.Item[1];
        lCount := lCount + lPage.Shapes.Count;
      //end;
      if (lProgressDlg <> nil) then
        lProgressDlg.SetMaximum(lCount);

      //for lPageIdx := 1 to ADoc.Pages.Count  do
      //begin
        //if(lPageIdx > ADoc.Pages.Count) then Break;

        lPage := ADoc.Pages.Item[1];
        for lShapeIdx := 1 to lPage.Shapes.Count  do
        begin
          if(lShapeIdx > lPage.Shapes.Count) then Break;

          if (lProgressDlg <> nil) then
            lProgressDlg.UpdateProgress(lShapeIdx);
          lShape := lPage.Shapes.Item[lShapeIdx];
          if (Pos(PChar(mtWRYMOutputSelector), lShape.Name) > 0) then
          begin
            FOutputSelector := lShape;
            RefreshOutputSelector(FOutputSelector);
          end
          else
          if (Pos(PChar(mtWRYMReservoir), lShape.Name) > 0) then
          begin
            lElementID := StrToIntDef(UnQuote(lShape.Cells[cNumber].Formula),0);
            FReservoirList.Add(IntToStr(lElementID));
            RefreshReservoir(lShape, lElementID);
          end
          else
          if (Pos(PChar(mtWRYMMine), lShape.Name) > 0) then
          begin
            lElementID := StrToIntDef(UnQuote(lShape.Cells[cNumber].Formula),0);
            FMineList.Add(IntToStr(lElementID));
            RefreshMine(lShape, lElementID);
          end
          else
          if (Pos(PChar(mtWRYMGroundWater), lShape.Name) > 0) then
          begin
            lElementID := StrToIntDef(UnQuote(lShape.Cells[cNumber].Formula),0);
            FGroundWaterList.Add(IntToStr(lElementID));
            RefreshGroundWater(lShape, lElementID);
          end
          else
          if (Pos(PChar(mtWRYMSubCatchment), lShape.Name) > 0) then
          begin
            lElementID := StrToIntDef(UnQuote(lShape.Cells[cNumber].Formula),0);
            RefreshSubCatchment(lShape, lElementID);
          end
          else
          if (Pos(PChar(mtWRYMNode), lShape.Name) > 0) then
          begin
            lElementID := StrToIntDef(UnQuote(lShape.Cells[cNumber].Formula),0);
            FNodeList.Add(IntToStr(lElementID));
            RefreshNode(lShape, lElementID);
          end
          else
          if (Pos(PChar(mtWRYMChannel), lShape.Name) > 0) then
          begin
            lElementID := StrToIntDef(UnQuote(lShape.Cells[cNumber].Formula),0);
            FChannelList.Add(IntToStr(lElementID));
            RefreshChannel(lShape, lElementID);
            SnapChannelToNodes(lShape, lElementID);
          end
          else
          if (Pos(PChar(mtWRYMInflow), lShape.Name) > 0) then
          begin
            lElementID := StrToIntDef(UnQuote(lShape.Cells[cNumber].Formula),0);
            RefreshInflow(lShape, lElementID)
          end
          else
          if (Pos(PChar(mtWRYMResPenalty), lShape.Name) > 0) or (Pos(PChar(mtWRYMResPenaltyExt), lShape.Name) > 0) then
          begin
            lElementID := StrToIntDef(UnQuote(lShape.Cells[cNumber].Formula),0);
            FResPenaltyList.Add(IntToStr(lElementID));
            RefreshReservoirPenalty(lShape, lElementID);
          end
          else
          if (Pos(PChar(mtWRYMChanPenalty), lShape.Name) > 0) then
          begin
            lElementID := StrToIntDef(UnQuote(lShape.Cells[cNumber].Formula),0);
            FChanPenaltyList.Add(IntToStr(lElementID));
            RefreshChannelPenalty(lShape, lElementID);
          end
          else
          if (Pos(PChar(mtWRYMText), lShape.Name) > 0) then
          begin
            if (lShape.CellExists[cTextType, 1] <> 0) AND
               (lShape.CellExists[cNumber, 1] <> 0) then
            begin
              lTextType  := TVNVTextType(StrToIntDef(UnQuote(lShape.Cells[cTextType].Formula),0));
              if (lTextType in OutputTextTypeSet) then
                FOutputList.Add(lShape.Name);
              RefreshText(lShape);
            end;
          end
          else
          if (Pos(PChar(mtWRYMIrrBlock), lShape.Name) > 0) then
          begin
            lElementID := StrToIntDef(UnQuote(lShape.Cells[cNumber].Formula),0);
            FIrrigationList.Add(IntToStr(lElementID));
            RefreshIrrigationBlock(lShape, lElementID);
          end
          else
          if (Pos(PChar(mtWRYMIrrArea), lShape.Name) > 0) then
          begin
            lElementID := StrToIntDef(UnQuote(lShape.Cells[cNumber].Formula),0);
            FIrrigationAreaList.Add(IntToStr(lElementID));
            RefreshIrrigationArea(lShape, lElementID);
          end
          else
          if (Pos(PChar(mtWRYMWetland), lShape.Name) > 0) then
          begin
            lElementID := StrToIntDef(UnQuote(lShape.Cells[cNumber].Formula),0);
            FWetlandList.Add(IntToStr(lElementID));
            RefreshWetland(lShape, lElementID);
          end
          else
          if (Pos(PChar(mtWRYMDemandCentre), lShape.Name) > 0) then
          begin
            lElementID := StrToIntDef(UnQuote(lShape.Cells[cNumber].Formula),0);
            FDemandCentreList.Add(IntToStr(lElementID));
          end
          else
          if (Pos(PChar(mtWRYMGroundWater), lShape.Name) > 0) then
          begin
            lElementID := StrToIntDef(UnQuote(lShape.Cells[cNumber].Formula),0);
            FGroundWaterList.Add(IntToStr(lElementID));
            RefreshGroundWater(lShape, lElementID);
          end
          else
          if (Pos(PChar(mtWRYMPowerPlant), lShape.Name) > 0) then
          begin
            lElementID := StrToIntDef(UnQuote(lShape.Cells[cNumber].Formula),0);
            FPowerPlantList.Add(IntToStr(lElementID));
            RefreshPowerPlant(lShape, lElementID);
          end;

        end;
      //end;

      //Demand Centres
      for lShapeIdx := 0 to FDemandCentreList.Count - 1 do
      begin
        lElementId := StrToIntDef(FDemandCentreList.Strings[lShapeIdx], -1);
        lShape := FindDemandCentreByNode(lElementId);
        if (lShape <> nil) then
          DemandCentreHasChanged(lShape.Name, lElementID);
      end;

    finally
      if (lProgressDlg <> nil) then
      begin
        lProgressDlg.Hide;
        FreeAndNil(lProgressDlg);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.OnShapeAdded (ASender      : TObject;
                                         const AShape : IVShape);
const OPNAME = 'TVNVEventHandler.OnShapeAdded';
begin
  try
    FBlockStudyDataChanges := True;
    try
      if (AShape.Master <> nil) then
      begin
        if (Pos(PChar(mtWRYMReservoir), AShape.Name) > 0) then
          ReservoirShapeAdded(AShape)
        else
        if (Pos(PChar(mtWRYMSubCatchment), AShape.Name) > 0) then
          SFRSubCatchmentShapeAdded(AShape)
        else
        if (Pos(PChar(mtWRYMNode), AShape.Name) > 0) then
          NodeShapeAdded(AShape)
        else
        if (Pos(PChar(mtWRYMChannel), AShape.Name) > 0) then
          ChannelShapeAdded(AShape)
        else
        if (Pos(PChar(mtWRYMChanPenalty), AShape.Name) > 0) then
          ChannelPenaltyShapeAdded(AShape)
        else
        if (Pos(PChar(mtWRYMResPenalty), AShape.Name) > 0) or (Pos(PChar(mtWRYMResPenaltyExt), AShape.Name) > 0) then
          ReservoirPenaltyShapeAdded(AShape)
        else
        if (Pos(PChar(mtWRYMInflow), AShape.Name) > 0) then
          InflowShapeAdded(AShape)
        else
        if (Pos(PChar(mtWRYMText), AShape.Name) > 0) then
          TextShapeAdded(AShape)
        else
        if (Pos(PChar(mtWRYMOutputSelector), AShape.Name) > 0) then
          OutputSelectorShapeAdded(AShape)
        else
        if (Pos(PChar(mtWRYMIrrBlock), AShape.Name) > 0) then
          IrrigationBlockShapeAdded(AShape)
        else
        if (Pos(PChar(mtWRYMIrrArea), AShape.Name) > 0) then
          IrrigationAreaShapeAdded(AShape)
        else
        if (Pos(PChar(mtWRYMWetland), AShape.Name) > 0) then
          WetlandShapeAdded(AShape)
        else
        if (Pos(PChar(mtWRYMDemandCentre), AShape.Name) > 0) then
          DemandCentreShapeAdded(AShape)
        else
        if (Pos(PChar(mtWRYMMine), AShape.Name) > 0) then
          MineShapeAdded(AShape)
        else
        if (Pos(PChar(mtWRYMGroundWater), AShape.Name) > 0) then
          GroundWaterShapeAdded(AShape)
        else
        if (Pos(PChar(mtModel), AShape.Name) > 0) then
          ModelShapeAdded(AShape)
        else
        if (Pos(PChar(mtWRYMPowerPlant), AShape.Name) > 0) then
           PowerPlantShapeAdded(AShape);

      end;
    finally
      FBlockStudyDataChanges := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.ReservoirShapeAdded(const AShape: IVShape; AElementID : Integer = -1; ADeleteLock : Integer = 0; AParentMineID : Integer = 0);
const OPNAME = 'TVNVEventHandler.ReservoirShapeAdded';
var
  lElementID  : integer;
  lTxtLabel   : IVShape;
begin
  try
    lElementID := AElementID;
    if (FBufferReservoirList.Count > 0) AND (lElementID = -1) then
      { Upgraded reservoir }
      FBufferReservoirList.Delete(0)
    else
    begin
      if (AShape.CellExists[cNumber, 0] <> 0) AND (UnQuote(AShape.Cells[cNumber].Formula) <> '') then
      begin
        { Copied reservoir }
        lElementID  := StrToIntDef(UnQuote(AShape.Cells[cNumber].Formula),0);
      end
      else
      begin
        { New reservoir - from stencil }
        if(lElementID <= 0) then
        begin
          lElementID := ShowReservoirsDialog;
        end;
        if (lElementID <> -1) then
        begin
          AShape.Cells[cNumber].Formula  := '"' + IntToStr(lElementID) + '"';
          AShape.Cells[cVersion].Formula := '"' + CThisVersion + '"';
          AShape.Cells[cDeletePrevention].Formula := '"' + IntToStr(ADeleteLock) + '"';
          AShape.Cells[cParentMine].Formula := '"' + IntToStr(AParentMineID) + '"';
        end;
      end;

      if (lElementID = -1) then
        AShape.Delete
      else
      begin
        AShape.Text := IntToStr(lElementID);
        RefreshReservoir(AShape, lElementID);
        FReservoirList.Add(IntToStr(lElementID));

        if (FDrawing.GISMode) then
          if CheckExistingWithCoords(AShape, lElementID) then
          begin
            GISPopulateDoNotRecalcCoordsList(lElementID);
            GISSetXYPosFromLonLat(AShape, lElementID)
          end;

        lTxtLabel := AddTextLabel(AShape, nvtName);
        RefreshText(lTxtLabel);

      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.RefreshReservoir (AShape     : IVShape; AElementID : integer);
const OPNAME = 'TVNVEventHandler.RefreshReservoir';
var
  lReservoir  : IReservoirData;
  lResName    : string;
  lKeepColor  : boolean;
  lSFRS       : String;
  lShape2     : IVShape;
begin
  try
    if(AShape = nil) or (AElementID = 0) or (AElementID = NullInteger) then Exit;

    lKeepColor := (AShape.CellExists[cPermanentColor, 0] <> 0)  and (AShape.Cells[cPermanentColor].Formula = '"TRUE"');
    lReservoir := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[AElementID];
    if (lReservoir = nil) then
    begin
      if not lKeepColor then
        AShape.CellsSRC[visSectionObject, visRowFill, visFillForegnd].FormulaU := IntToStr(visRed);
    end
    else
    begin
      if not lKeepColor then
        AShape.CellsSRC[visSectionObject, visRowFill, visFillForegnd].FormulaU := IntToStr(visCyan);
      FVisioApp.ActiveWindow.Windows.ItemFromID[visWinIDCustProp].Visible := FALSE;
      lResName := lReservoir.ReservoirConfigurationData.ReservoirName;
      AShape.Cells[cName].Formula := '"' + lResName + '"';;
      if (lReservoir.ReservoirPenaltyStructureData <> nil) then
        AShape.Cells[cPenalty].Formula := '"' + IntToStr(lReservoir.ReservoirPenaltyStructureData.ReservoirPenaltyID) + '"'
      else
        AShape.Cells[cPenalty].Formula := '""';
      if (lReservoir.ReservoirConfigurationData.StatusIndicator = 0) then
        AShape.CellsSRC[visSectionObject, visRowLine, visLinePattern].FormulaU := '10';


      lSFRs := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.StreamFlowReductionList.StreamFlowReductionIDsPerInflowNode(AElementID);

      if (lSFRs = '') then
      begin
        lShape2 := FindSFRAShapeByReservoirNumber(AElementID);
        if lShape2 <> nil then
        begin
          FBufferSubCatchmentList.Add(UnQuote(lShape2.Cells[cNumber].Formula));
          lShape2.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
          lShape2.Delete;
        end;
        lShape2 := FindInflowShapeByReservoirNumber(AElementID);
        if lShape2 = nil then
          AddInflowSymbol(AShape,AElementID);
      end
      else
      begin
        lShape2 := FindInflowShapeByReservoirNumber(AElementID);
        if lShape2 <> nil then
        begin
          FBufferInflowList.Add(lShape2.Name);
          lShape2.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
          lShape2.Delete;
        end;
        lShape2 := FindSFRAShapeByReservoirNumber(AElementID);
        if lShape2 = nil then
          AddInflowSymbol(AShape,AElementID);
      end;
      AShape.Cells[cRow1Action].Formula := 'RUNMACRO("ThisDocument.ProcessVNVSpecial 1")';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.RefreshInflow (var AShape : IVShape; AElementID : integer);
const OPNAME = 'TVNVEventHandler.RefreshInflow';
var
  lNode         : IReservoirData;
  lTxtShape     : IVShape;
  lYieldModel   : IYieldModel;
  //lYieldModelData : IYieldModelData;
  //lReservoir      : IReservoirData;
  //lShape          : IVShape;
  //lSFRS           : String;
begin
  try
    if(AShape = nil) or (AElementID = 0) or (AElementID = NullInteger) then Exit;
    lYieldModel := FAppModules.Model as IYieldModel;
    lNode := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.
               ReservoirList.ReservoirOrNodeByIdentifier[AElementID];
    if (lNode = nil) then
    begin
      if (AShape.CellsSRCExists[visSectionObject, visRowFill, visFillForegnd, 0] = -1) then
        AShape.CellsSRC[visSectionObject, visRowFill, visFillForegnd].FormulaU := IntToStr(visRed);
    end
    else
    begin
      //check sfra
      {lSFRS := '';
      lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
      lReservoir      := lYieldModelData.NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[AElementID];
      if (lReservoir <> nil) then
      begin
        lSFRs := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.StreamFlowReductionList.
                 StreamFlowReductionIDsPerInflowNode(lReservoir.ReservoirConfigurationData.ReservoirIdentifier);
        lShape := FindNodeShapeBySubCatchmentNumber(AElementID);
        if lShape <> nil then
        begin
          if (lSFRS <> '') and (Pos(mtWRYMSubCatchment, AShape.Name) <> 0) then
          begin
             FBufferSubCatchmentList.Add(IntToStr(AElementID));
             AShape.Delete;
             AShape := AddInflowSymbol(lShape, AElementID);
          end
          else
          begin
            if (Pos(mtWRYMInflow, AShape.Name) <> 0) then
            begin
             FBufferInflowList.Add(AShape.Name);
             AShape.Delete;
             AShape := AddInflowSymbol(lShape, AElementID);
            end
          end;
        end;
      end;}

      FVisioApp.ActiveWindow.Windows.ItemFromID[visWinIDCustProp].Visible := FALSE;
//      lTxtShape := FindTextWithParent(AShape.Name);
      lTxtShape := FindTextWithParent(AShape.Name);
      if lTxtShape = nil then
        lTxtShape := AddTextLabel(AShape, nvtInflow);

      if ((lTxtShape <> nil) and (lTxtShape.Cells[cTextType].Formula = '"' + IntToStr(Ord(nvtName)) + '"')) then
      begin
        lTxtShape.Cells[cTextType].Formula := '"' + IntToStr(Ord(nvtInflow)) + '"';
        lTxtShape.Cells[cTextLbl].Formula  := '"' + TextTypeToStr(nvtInflow) + '"';
      end;
//      lTxtShape := FindTextTypeWithParent(nvtInflow, AShape.Name);
      lTxtShape := FindTextTypeWithParent(nvtInflow, AShape.Name);
      if (lTxtShape <> nil) then
        RefreshInflowText(lTxtShape, AElementID);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.RefreshSubCatchment (AShape     : IVShape;
                                                AElementID : integer);
const OPNAME = 'TVNVEventHandler.RefreshSubCatchment';
var
  lYieldModelData : IYieldModelData;
  lReservoir      : IReservoirData;
  lShape          : IVShape;
  lSFRTxtShape    : IVShape;
  lSFRs           : string;
begin
  try
    if (AElementID = -1) then
      exit;
    lSFRTxtShape := nil;
    lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
    lReservoir      := lYieldModelData.NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[AElementID];
    if (lReservoir <> nil) then
    begin
      lSFRs := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.StreamFlowReductionList.StreamFlowReductionIDsPerInflowNode(AElementID);
      lShape := FindNodeShapeBySubCatchmentNumber(AElementID);
      FBufferNodeHasSFRAList.Add(lShape.Name);
      lSFRTxtShape := FindShapeWithName(mtWRYMSubCatchmentTxt + FindNumber(AShape.Name));
      if lSFRTxtShape <> nil then
        lSFRTxtShape.Delete;
      lSFRTxtShape := AddSFRASubCatchmentText(AShape, lSFRs);

      //FVisioApp.ActiveWindow.Windows.ItemFromID[visWinIDCustProp].Visible := FALSE;
      AShape.Cells[cRow1Action].Formula := 'RUNMACRO("ThisDocument.ProcessVNVSpecial 1")';
   end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.AddTextLabel (AShape : IVShape; AType  : TVNVTextType) : IVShape;
const OPNAME = 'TVNVEventHandler.AddTextLabel';
var
  lXParent    : Double;
  lYParent    : Double;
  lXChild     : Double;
  lYChild     : Double;
  lShape      : IVShape;
  lMaster     : IVMaster;
  lYShift     : double;
  lXShift     : double;
begin
  Result := nil;
  try
    if (AShape <> nil) then
    begin
      AShape.XYToPage(0, 0, lXParent, lYParent);
      lXShift := 0;
      lYShift := 0;
      if (Pos(PChar(mtWRYMReservoir), AShape.Name) > 0) then
        lYShift := - 0.1
      else
      if (Pos(PChar(mtWRYMMine), AShape.Name) > 0) then
        lYShift := - 0.1
      else
      if (Pos(PChar(mtWRYMGroundWater), AShape.Name) > 0) then
        lYShift := - 0.1
      else
      if (Pos(PChar(mtWRYMSubCatchment), AShape.Name) > 0) then
        lYShift := 0.5
      else
      if (Pos(PChar(mtWRYMIrrBlock), AShape.Name) > 0) then
        lYShift :=  - 0.6
      else
      if (Pos(PChar(mtWRYMIrrArea), AShape.Name) > 0) then
        lYShift :=  - 0.6
      else
      if (Pos(PChar(mtWRYMWetland), AShape.Name) > 0) then
        lYShift :=  - 0.1
      else
      if (Pos(PChar(mtWRYMDemandCentre), AShape.Name) > 0) then
        lYShift :=  - 0.1
      else
      if (Pos(PChar(mtWRYMNode), AShape.Name) > 0) then
        lYShift := - 0.1
      else
      if (Pos(PChar(mtWRYMChannel), AShape.Name) > 0) then
      begin
        if (AType = nvtName) then
          lYShift := - 0.1
        else
        if (AType = nvtDemandFile) then
          lYShift := + 0.25;
      end
      else
      if (Pos(PChar(mtWRYMInflow), AShape.Name) > 0) then
        lYShift := 0.5
      else
      if (Pos(PChar(mtWRYMSubCatchmentTxt), AShape.Name) > 0) then
        lYShift := 0.5
      else
      if (Pos(PChar(mtWRYMGroundWater), AShape.Name) > 0) then
        lYShift := - 0.1;


      lMaster := FindMaster(mtWRYMText);
      if (lMaster <> nil) then
      begin
        lShape := FVisioApp.ActivePage.Drop(lMaster, lXParent + lXShift, lYParent + lYShift);
        FBufferTextList.Add(lShape.Name);
        FVisioApp.ActiveWindow.DeselectAll;
        FVisioApp.ActiveWindow.Select(AShape, visSelect);
        FVisioApp.ActiveWindow.Select(lShape, visSelect);
        FVisioApp.ActiveWindow.Selection.Align(visHorzAlignCenter, visVertAlignNone, False);
        FVisioApp.ActiveWindow.DeselectAll;

        lXParent := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinX].ResultStr[visMillimeters])),0);
        lYParent := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinY].ResultStr[visMillimeters])),0);
        lXChild  := StrToFloatDef(UnMM(UnQuote(lShape.Cells[cPinX].Formula)),0);
        lYChild  := StrToFloatDef(UnMM(UnQuote(lShape.Cells[cPinY].Formula)),0);

        lShape.Cells[cParent].Formula       := '"' + AShape.Name + '"';
        lShape.Cells[cParentXDiff].Formula  := '"' + FloatToStrF(lXParent - lXChild, ffFixed, 12, 2) + '"';
        lShape.Cells[cParentYDiff].Formula  := '"' + FloatToStrF(lYParent - lYChild, ffFixed, 12, 2) + '"';
        lShape.Cells[cParentXMoved].Formula := '"' + IntToStr(0) + '"';
        lShape.Cells[cParentYMoved].Formula := '"' + IntToStr(0) + '"';
        lShape.Cells[cVersion].Formula      := '"' + CThisVersion + '"';

        if (Integer(AType) >= 0) then
        begin
          lShape.AddNamedRow(visSectionProp, 'Type', visTagDefault);
          lShape.Cells[cTextType].Formula := '"' + IntToStr(Ord(AType)) + '"';
          lShape.Cells[cTextLbl].Formula  := '"' + TextTypeToStr(AType) + '"';
          lShape.Cells[cTextInv].Formula  := '"TRUE"';
          if (AType = nvtDemandFile) then
            lShape.CellsSRC[visSectionCharacter, visRowCharacter, visCharacterColor].FormulaU := IntToStr(visRed);
        end;

        if (AType = nvtSFRASubCatchment) then
            lShape.CellsSRC[visSectionCharacter, visRowCharacter, visCharacterColor].FormulaU := IntToStr(visGreen);

        lShape.AddNamedRow(visSectionProp, 'Number', visTagDefault);
        lShape.Cells[cNumber].Formula := AShape.Cells[cNumber].Formula;

        Result := lShape;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.TextTypeToStr (AType : TVNVTextType) : string;
const OPNAME = 'TVNVEventHandler.TextTypeToStr';
begin
  Result := '';
  try
    case AType of
      nvtGeneral           : Result := FAppModules.Language.GetString('VisioNetwork.General');
      nvtDemandFile        : Result := FAppModules.Language.GetString('VisioNetwork.DemandFile');
      nvtName              : Result := FAppModules.Language.GetString('TSCCaptionSelector.Grid_Caption1');
      nvtInflow            : Result := FAppModules.Language.GetString('VisioNetwork.Inflow');
      nvtElevation         : Result := FAppModules.Language.GetString('VisioNetwork.Elevation');
      nvtVolume            : Result := FAppModules.Language.GetString('VisioNetwork.Volume');
      nvtPercFull          : Result := FAppModules.Language.GetString('VisioNetwork.PercentageFull');
      nvtChannelFlow       : Result := FAppModules.Language.GetString('VisioNetwork.ChannelFlow');
      nvtNetBasinRunoff    : Result := FAppModules.Language.GetString('VisioNetwork.NetBasinRunoff');
      nvtRainfall          : Result := FAppModules.Language.GetString('VisioNetwork.Rainfall');
      nvtEvaporation       : Result := FAppModules.Language.GetString('VisioNetwork.Evaporation');
      nvtSFRASubCatchment  : Result := FAppModules.Language.GetString('VisioNetwork.SFRASubCatchment');
      nvtAvgChannelFlow    : Result := FAppModules.Language.GetString('VisioNetwork.AvgChannelFlow');
      nvtAvgElevation      : Result := FAppModules.Language.GetString('VisioNetwork.AvgElevation');
      nvtAvgVolume         : Result := FAppModules.Language.GetString('VisioNetwork.AvgVolume');
      nvtAvgNetBasinRunoff : Result := FAppModules.Language.GetString('VisioNetwork.AvgNetBasinRunoff');
      nvtAvgRainfall       : Result := FAppModules.Language.GetString('VisioNetwork.AvgRainfall');
      nvtAvgEvaporation    : Result := FAppModules.Language.GetString('VisioNetwork.AvgEvaporation');
      nvtReservoirStorageChange    : Result := FAppModules.Language.GetString('VisioNetwork.ReservoirStorageChange');
    else
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.StrToTextType (ATypeStr : string) : TVNVTextType;
const OPNAME = 'TVNVEventHandler.StrToTextType';
begin
  Result := nvtGeneral;
  try
    if (ATypeStr = 'General') then
      Result := nvtGeneral
    else
    if (ATypeStr = 'DemandFile') then
      Result := nvtDemandFile
    else
    if (ATypeStr = 'Name') then
      Result := nvtName
    else
    if (ATypeStr = 'Inflow') then
      Result := nvtInflow
    else
    if (ATypeStr = 'Elevation') then
      Result := nvtElevation
    else
    if (ATypeStr = 'Volume') then
      Result := nvtVolume
    else
    if (ATypeStr = 'PercentageFull') then
      Result := nvtPercFull
    else
    if (ATypeStr = 'ChannelFlow') then
      Result := nvtChannelFlow
    else
    if (ATypeStr = 'NetBasinRunoff') then
      Result := nvtNetBasinRunoff
    else
    if (ATypeStr = 'Rainfall') then
      Result := nvtRainfall
    else
    if (ATypeStr = 'Evaporation') then
      Result := nvtEvaporation
    else
    if (ATypeStr = 'SFRASubCatchment') then
      Result := nvtSFRASubCatchment
    else
    if (ATypeStr = 'AvgElevation') then
      Result := nvtAvgElevation
    else
    if (ATypeStr = 'AvgVolume') then
      Result := nvtAvgVolume
    else
    if (ATypeStr = 'AvgNetBasinRunoff') then
      Result := nvtAvgNetBasinRunoff
    else
    if (ATypeStr = 'AvgRainfall') then
      Result := nvtAvgRainfall
    else
    if (ATypeStr = 'AvgEvaporation') then
      Result := nvtAvgEvaporation
    else
    if (ATypeStr = 'AvgChannelFlow') then
      Result := nvtAvgChannelFlow;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.NodeShapeAdded(const AShape: IVShape; AElementID : Integer = -1);
const OPNAME = 'TVNVEventHandler.NodeShapeAdded';
var
  lYieldModel : IYieldModel;
  lNode       : IReservoirData;
  lTxtLabel   : IVShape;
begin
  try
    if (FBufferNodeList.Count > 0) AND (AElementID = -1) then
      { Upgraded node }
      FBufferNodeList.Delete(0)
    else
    begin
      if (AShape.CellExists[cNumber, 0] <> 0) AND
         (UnQuote(AShape.Cells[cNumber].Formula) <> '') then
      begin
        { Copied node }
        AElementID := StrToIntDef(UnQuote(AShape.Cells[cNumber].Formula),0);
      end
      else
      begin
        { New node - from stencil }
        if(AElementID <= 0) then
        begin
          AElementID := ShowNodesDialog;
        end;
        if (AElementID <> -1) then
        begin
          AShape.Cells[cNumber].Formula  := '"' + IntToStr(AElementID) + '"';
          AShape.Cells[cVersion].Formula := '"' + CThisVersion + '"';
        end;
      end;

      AShape.Text := IntToStr(AElementID);
      if (AElementID = -1) then
        AShape.Delete
      else
      begin
        RefreshNode(AShape, AElementID);
        lYieldModel := FAppModules.Model as IYieldModel;
        lNode := (lYieldModel.YieldModelData as IYieldModelData).NetworkElementData.
                   ReservoirList.ReservoirOrNodeByIdentifier[AElementID];

        if (lNode <> nil) AND (lNode.ReservoirConfigurationData.NodeType = ntNodeWithInflow) then
          AddInflowSymbol(AShape, AElementID);
        FNodeList.Add(IntToStr(AElementID));

        if (FDrawing.GISMode) then
          if CheckExistingWithCoords(AShape, AElementID) then
          begin
            GISPopulateDoNotRecalcCoordsList(AElementID);
            GISSetXYPosFromLonLat(AShape, AElementID)
          end;

        lTxtLabel := AddTextLabel(AShape, nvtName);
        RefreshText(lTxtLabel);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.RefreshNode (AShape     : IVShape;
                                        AElementID : integer);
const OPNAME = 'TVNVEventHandler.RefreshNode';
var
  lNode     : IReservoirData;
  lNodeName : string;
  lKeepColor: boolean;
begin
  try
    if(AShape = nil) or (AElementID = 0) or (AElementID = NullInteger) then Exit;

    lKeepColor := (AShape.CellExists[cPermanentColor, 0] <> 0)  and (AShape.Cells[cPermanentColor].Formula = '"TRUE"');
    lNode      := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[AElementID];
    if (lNode = nil) then
    begin
      if not lKeepColor then
        AShape.CellsSRC[visSectionObject, visRowFill, visFillForegnd].FormulaU := IntToStr(visRed);
    end
    else
    begin
      if not lKeepColor then
        AShape.CellsSRC[visSectionObject, visRowFill, visFillForegnd].FormulaU := IntToStr(visGreen);
      FVisioApp.ActiveWindow.Windows.ItemFromID[visWinIDCustProp].Visible := FALSE;
      lNodeName := lNode.ReservoirConfigurationData.ReservoirName;
      AShape.Cells[cName].Formula       := '"' + lNodeName + '"';
      AShape.Cells[cRow1Action].Formula := 'RUNMACRO("ThisDocument.ProcessVNVSpecial 1")';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.AddInflowSymbol (AShape : IVShape; AElementID : integer) : IVShape;
const OPNAME = 'TVNVEventHandler.AddInflowSymbol';
var
  lYieldModelData : IYieldModelData;
  lReservoir      : IReservoirData;
  lXParent        : Double;
  lYParent        : Double;
  lXChild         : Double;
  lYChild         : Double;
  lShape          : IVShape;
  lSFRTxtShape    : IVShape;
  lMaster         : IVMaster;
  lXShift         : Double;
  lYShift         : Double;
  lSFRS           : String;
begin
  Result := nil;
  try
    if (AShape <> nil) then
    begin
      AShape.XYToPage(0, 0, lXParent, lYParent);
      if (Pos(PChar(mtWRYMNode), AShape.Name) > 0) then
      begin
        lXShift := 0.84;
        lYShift := 0.48;
      end
      else
      if (Pos(PChar(mtWRYMWetland), AShape.Name) > 0) then
      begin
        lXShift := 0.95;
        lYShift := 0.55;
      end
      else
      begin
        lXShift := 0.68;
        lYShift := 0.5;
      end;

      lSFRS := '';
      lSFRTxtShape := nil;
      if ((Pos(PChar(mtWRYMReservoir), AShape.Name) > 0) or
          (Pos(PChar(mtWRYMWetland), AShape.Name) > 0) or
          (Pos(PChar(mtWRYMNode), AShape.Name) > 0) or
          (Pos(PChar(mtWRYMGroundWater), AShape.Name) > 0)) then
      begin
        lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
        lReservoir      := lYieldModelData.NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[AElementID];
        if (lReservoir <> nil) then
        begin
          lSFRs := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.StreamFlowReductionList.StreamFlowReductionIDsPerInflowNode(lReservoir.ReservoirConfigurationData.ReservoirIdentifier);
          if lSFRS <> '' then
          begin
            FBufferNodeHasSFRAList.Add(AShape.Name);
            lMaster := FindMaster(mtWRYMSubCatchment);
          end
          else
          begin
            lMaster := FindMaster(mtWRYMInflow);
          end; // if
        end; //if (lReservoir <> nil)
      end;

      if (lMaster <> nil) then
      begin
        lShape := FVisioApp.ActivePage.Drop(lMaster, lXParent + lXShift, lYParent + lYShift);
        if Pos(PChar(mtWRYMSubCatchment), lMaster.Name) > 0 then
        begin
          lShape.Name :=  mtWRYMSubCatchment + IntToStr(AElementID);
          FBufferSubCatchmentList.Add(lShape.Name)
        end
        else
        begin
          lShape.Name :=  mtWRYMInflow + IntToStr(AElementID);
          FBufferInflowList.Add(lShape.Name);
        end;

        lXParent := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinX].ResultStr[visMillimeters])),0);
        lYParent := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinY].ResultStr[visMillimeters])),0);
        lXChild  := StrToFloatDef(UnMM(UnQuote(lShape.Cells[cPinX].Formula)),0);
        lYChild  := StrToFloatDef(UnMM(UnQuote(lShape.Cells[cPinY].Formula)),0);

        lShape.Cells[cNumber].Formula       := '"' + IntToStr(AElementID) + '"';
        lShape.Cells[cParent].Formula       := '"' + AShape.Name + '"';
        lShape.Cells[cParentXDiff].Formula  := '"' + FloatToStrF(lXParent - lXChild, ffFixed, 12, 2) + '"';
        lShape.Cells[cParentYDiff].Formula  := '"' + FloatToStrF(lYParent - lYChild, ffFixed, 12, 2) + '"';
        lShape.Cells[cParentXMoved].Formula := '"' + IntToStr(0) + '"';
        lShape.Cells[cParentYMoved].Formula := '"' + IntToStr(0) + '"';
        lShape.Cells[cVersion].Formula      := '"' + CThisVersion + '"';

        lShape.AddNamedRow(visSectionProp, 'CatchmentNumber', visTagDefault);
        lShape.Cells[cCatchmentNumber].Formula := '"0"';
        lShape.Cells[cCatchmentLbl].Formula    := '"Cathment number"';
        lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
        lReservoir      := lYieldModelData.NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[AElementID];
        if (lReservoir <> nil) then
        begin
          lShape.Cells[cCatchmentNumber].Formula := '"' + IntToStr(lReservoir.ReservoirConfigurationData.CatchmentRef) + '"';
          if (lSFRTxtShape <> nil) then
            lShape.Cells[cName].Formula       := '"' + FAppModules.Language.GetString('VNV.SFRASubCatchmentFor') + lReservoir.ReservoirConfigurationData.ReservoirName + '"';
        end;

        if(lSFRS <> '') then
          RefreshSubCatchment(lShape,AElementID)
        else
          RefreshInflow(lShape,AElementID);
        FVisioApp.ActiveWindow.DeselectAll;
        Result := lShape;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TVNVEventHandler.ChannelShapeAdded(const AShape: IVShape; AElementID : Integer = -1);
const OPNAME = 'TVNVEventHandler.ChannelShapeAdded';
var
  lElementID  : integer;
  lTxtLabel   : IVShape;
  lChannel    : IGeneralFlowChannel;
  lFileLabel  : IVShape;

begin
  try
    lElementID := AElementID;
    if (FBufferChannelList.Count > 0) AND (lElementID = -1) then
      { Upgraded channel }
      FBufferChannelList.Delete(0)
    else
    begin
      if (AShape.CellExists[cNumber, 0] <> 0) AND
         (UnQuote(AShape.Cells[cNumber].Formula) <> '') then
      begin
        { Copied channel }
        lElementID := StrToIntDef(UnQuote(AShape.Cells[cNumber].Formula),0);
      end
      else
      begin
        { New channel - from stencil }
        if(lElementID <= 0) then
        begin
          lElementID := ShowChannelsDialog;
        end;

        if (lElementID <> -1) then
        begin
          AShape.Cells[cNumber].Formula  := '"' + IntToStr(lElementID) + '"';
          AShape.Cells[cVersion].Formula := '"' + CThisVersion + '"';
        end;
      end;

      if (lElementID = -1) then
        AShape.Delete
      else
      begin
        AShape.Text := IntToStr(lElementID);
        RefreshChannel(AShape, lElementID);
        lChannel := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.
                      ChannelList.ChannelByChannelNumber[lElementID];
        if ((lChannel <> nil) AND (lChannel.SpecifiedDemandFeature <> nil)) then
        begin
          lFileLabel := AddTextLabel(AShape, nvtDemandFile);
          if (lFileLabel <> nil) then
          RefreshText(lFileLabel);
        end;
        SnapChannelToNodes(AShape, lElementID);
        lTxtLabel := AddTextLabel(AShape, nvtName);
        RefreshText(lTxtLabel);
        FChannelList.Add(IntToStr(lElementID));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.SnapChannelToNodes (const AShape : IVShape;
                                               AChannelNr   : integer; AMineConnNodeNumber : integer = 3);
const OPNAME = 'TVNVEventHandler.SnapChannelToNodes';
var
  lYieldModel     : IYieldModel;
  lChannel        : IGeneralFlowChannel;
  lChannelCell    : IVCell;
  lOtherCell      : IVCell;
  lDownShape      : IVShape;
  lUpShape        : IVShape;
  lShape          : IVShape;
  lIndex          : integer;
  lIsConnected    : Boolean;
  lConnect        : IVConnect;
  lUpNodeNumber   : integer;
  lDownNodeNumber : integer;
  lNumber         : integer;
begin
  try
    lYieldModel := FAppModules.Model as IYieldModel;
    lChannel    := (lYieldModel.YieldModelData as IYieldModelData).NetworkElementData.
                     ChannelList.ChannelByChannelNumber[AChannelNr];

    if (lChannel <> nil) then
    begin
      lUpNodeNumber   := lChannel.UpStreamNodeNumber;
      lDownNodeNumber := lChannel.DownStreamNodeNumber;
      if (lDownNodeNumber <> 0) then
      begin
        lIsConnected := FALSE;
        lIndex       := 1;
        while ((NOT lIsConnected) AND (lIndex <= AShape.Connects.Count)) do
        begin
          lConnect := AShape.Connects[lIndex];
          if (lConnect.FromPart = visEnd) then
          begin
            lShape := FindShapeWithName(lConnect.ToSheet.Name);
            if (lShape <> nil) AND
               ((Pos(PChar(mtWRYMReservoir), lShape.Name) > 0) OR
                (Pos(PChar(mtWRYMIrrBlock), lShape.Name) > 0) OR
                (Pos(PChar(mtWRYMIrrArea), lShape.Name) > 0) OR
                (Pos(PChar(mtWRYMWetland), lShape.Name) > 0) OR
                (Pos(PChar(mtWRYMDemandCentre), lShape.Name) > 0) OR
                (Pos(PChar(mtWRYMNode), lShape.Name) > 0) OR
                (Pos(PChar(mtWRYMMine), lShape.Name) > 0) OR
                (Pos(PChar(mtWRYMGroundWater), lShape.Name) > 0) OR
                (Pos(PChar(mtWRYMPowerPlant), lShape.Name) > 0)) then
            begin
              lNumber := StrToIntDef(Trim(UnQuote(lShape.Cells[cNumber].Formula)),0);
              if (lNumber = lDownNodeNumber) then
              begin
                if(lChannel.ChannelType = 14) then
                begin
                  if (Pos(PChar(mtWRYMIrrBlock), lShape.Name) > 0) then
                    lIsConnected := TRUE;
                end
                else
                if(lChannel.ChannelType = 4) then
                begin
                    if (Pos(PChar(mtWRYMIrrArea), lShape.Name) > 0) then
                      lIsConnected := TRUE;
                end
                else
                if(lChannel.ChannelType = 16) then
                begin
                  if (Pos(PChar(mtWRYMWetland), lShape.Name) > 0) then
                    lIsConnected := TRUE;
                end
                else
                  lIsConnected := TRUE;
              end;
            end;
          end;
          lIndex := lIndex + 1;
        end;
        if (NOT lIsConnected) then
        begin
          lDownShape := FindChannelDownShape(lChannel);
          if (lDownShape <> nil) then
          begin
            if (Pos(PChar(mtWRYMReservoir), lDownShape.Name) > 0) or
               (Pos(PChar(mtWRYMGroundWater), lDownShape.Name) > 0) then
            begin
              if AMineConnNodeNumber = -1 then
                lOtherCell := lDownShape.CellsSRC[7, 3, 0]
              else
                lOtherCell := lDownShape.CellsSRC[7, 2, 0]
            end
            else if(Pos(PChar(mtWRYMIrrBlock), lDownShape.Name) > 0)then
              lOtherCell := lDownShape.CellsSRC[7, 3, 0]
            else if(Pos(PChar(mtWRYMIrrArea), lDownShape.Name) > 0)then
              lOtherCell := lDownShape.CellsSRC[7, 3, 0]
            else if(Pos(PChar(mtWRYMWetland), lDownShape.Name) > 0)then
              lOtherCell := lDownShape.CellsSRC[7, 3, 0]
            else if(Pos(PChar(mtWRYMMine), lDownShape.Name) > 0)then
            begin
              if AMineConnNodeNumber = 1 then
                lOtherCell := lDownShape.CellsSRC[7, 1, 0]
              else if AMineConnNodeNumber = 2 then
                lOtherCell := lDownShape.CellsSRC[7, 2, 0]
              else if AMineConnNodeNumber = 3 then
                lOtherCell := lDownShape.CellsSRC[7, 3, 0]
              else if AMineConnNodeNumber = 0 then
                lOtherCell := lDownShape.CellsSRC[7, 0, 0]
            end
            else
              lOtherCell := lDownShape.CellsSRC[7, 2, 0];
            lChannelCell := AShape.CellsU[cEndX];
            lChannelCell.GlueTo(lOtherCell);
           end;
        end;
      end;

      if (lUpNodeNumber <> 0) then
      begin
        lIsConnected := FALSE;
        lIndex       := 1;
        while ((NOT lIsConnected) AND (lIndex <= AShape.Connects.Count)) do
        begin
          lConnect := AShape.Connects[lIndex];
          if (lConnect.FromPart = visBegin) then
          begin
            lShape := FindShapeWithName(lConnect.ToSheet.Name);
            if (lShape <> nil) AND
               ((Pos(PChar(mtWRYMReservoir), lShape.Name) > 0) OR
                (Pos(PChar(mtWRYMIrrBlock), lShape.Name) > 0) OR
                (Pos(PChar(mtWRYMIrrArea), lShape.Name) > 0) OR
                (Pos(PChar(mtWRYMWetland), lShape.Name) > 0) OR
                (Pos(PChar(mtWRYMDemandCentre), lShape.Name) > 0) OR
                (Pos(PChar(mtWRYMNode), lShape.Name) > 0) OR
                (Pos(PChar(mtWRYMGroundWater), lShape.Name) > 0) OR
                (Pos(PChar(mtWRYMMine), lShape.Name) > 0) OR
                (Pos(PChar(mtWRYMPowerPlant), lShape.Name) > 0))   then
            begin
              lNumber := StrToIntDef(Trim(UnQuote(lShape.Cells[cNumber].Formula)),0);
              if (lNumber = lUpNodeNumber) then
              begin
                if(lChannel.ChannelType = 15) then
                begin
                  if (Pos(PChar(mtWRYMIrrBlock), lShape.Name) > 0) then
                    lIsConnected := TRUE;
                end
                else
                if(lChannel.ChannelType = 4) then
                begin
                    if (Pos(PChar(mtWRYMIrrArea), lShape.Name) > 0) then
                      lIsConnected := TRUE;
                end
                else
                if(lChannel.ChannelType = 17) then
                begin
                  if (Pos(PChar(mtWRYMWetland), lShape.Name) > 0) then
                    lIsConnected := TRUE;
                end
                else
                  lIsConnected := TRUE;
              end;
            end;
          end;
          lIndex := lIndex + 1;
        end;
        if (NOT lIsConnected) then
        begin
          lUpShape := FindChannelUpShape(lChannel);
          if (lUpShape <> nil) then
          begin
            if (Pos(PChar(mtWRYMReservoir), lUpShape.Name) > 0) or
               (Pos(PChar(mtWRYMGroundWater), lUpShape.Name) > 0) then
            begin
              if AMineConnNodeNumber = -1 then
                lOtherCell := lUpShape.CellsSRC[7, 3, 0]
              else lOtherCell := lUpShape.CellsSRC[7, 2, 0]
            end
            else if(Pos(PChar(mtWRYMIrrBlock), lUpShape.Name) > 0)then
              lOtherCell := lUpShape.CellsSRC[7, 3, 0]
            else if(Pos(PChar(mtWRYMIrrArea), lUpShape.Name) > 0)then
              if lChannel.ChannelSubType = 2 then lOtherCell := lUpShape.CellsSRC[7, 2, 0]
              else lOtherCell := lUpShape.CellsSRC[7, 3, 0]
            else if(Pos(PChar(mtWRYMWetland), lUpShape.Name) > 0)then
              lOtherCell := lUpShape.CellsSRC[7, 3, 0]
            else if(Pos(PChar(mtWRYMPowerPlant), lUpShape.Name) > 0)then
              if lChannel.ChannelSubType = 1 then lOtherCell := lUpShape.CellsSRC[7, 2, 0]
              else lOtherCell := lUpShape.CellsSRC[7, 3, 0] 
            else if(Pos(PChar(mtWRYMMine), lUpShape.Name) > 0)then
            begin
              if AMineConnNodeNumber = 1 then
                lOtherCell := lUpShape.CellsSRC[7, 1, 0]
              else if AMineConnNodeNumber = 2 then
                lOtherCell := lUpShape.CellsSRC[7, 2, 0]
              else if AMineConnNodeNumber = 3 then
                lOtherCell := lUpShape.CellsSRC[7, 3, 0]
              else if AMineConnNodeNumber = 0 then
                lOtherCell := lUpShape.CellsSRC[7, 0, 0]
            end
            else if(Pos(PChar(mtWRYMDemandCentre), lUpShape.Name) > 0)then
            begin
              if lChannel.ChannelType = 21 then //Reclamation
                lOtherCell := lUpShape.CellsSRC[7, 0, 0]
              else
              if lChannel.ChannelType = 20 then //Return Flow
                lOtherCell := lUpShape.CellsSRC[7, 1, 0]
              else
                lOtherCell := lUpShape.CellsSRC[7, 3, 0]
            end
            else
              lOtherCell := lUpShape.CellsSRC[7, 2, 0];
            lChannelCell := AShape.CellsU[cBeginX];
            lChannelCell.GlueTo(lOtherCell);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.FindChannelUpShape (AChannel : IGeneralFlowChannel) : IVShape;
const OPNAME = 'TVNVEventHandler.FindChannelUpShape';
var
  lUpStreamNode : string;
begin
  Result := nil;
  try
    if (AChannel <> nil) then
    begin
      lUpStreamNode := IntToStr(AChannel.UpStreamNodeNumber);
      //if (AChannel.ChannelType = 15) then
      if (Result = nil) then
        Result := FindShapeWithPropertyValue(mtWRYMIrrBlock, cNumber, lUpStreamNode);
      //if (AChannel.ChannelType = 17) then
      if (Result = nil) then
        Result := FindShapeWithPropertyValue(mtWRYMWetland, cNumber, lUpStreamNode);
      //if (AChannel.ChannelType = 4) then
      if (Result = nil) then
          Result := FindShapeWithPropertyValue(mtWRYMIrrArea, cNumber, lUpStreamNode);
      if (Result = nil) then
        Result := FindShapeWithPropertyValue(mtWRYMReservoir, cNumber, lUpStreamNode);
      if (Result = nil) then
        Result := FindShapeWithPropertyValue(mtWRYMNode, cNumber, lUpStreamNode);
      if (Result = nil) then
        Result := FindShapeWithPropertyValue(mtWRYMDemandCentre, cNumber, lUpStreamNode);
      if (Result = nil) then
        Result := FindShapeWithPropertyValue(mtWRYMMine, cNumber, lUpStreamNode);
      if (Result = nil) then
        Result := FindShapeWithPropertyValue(mtWRYMGroundWater, cNumber, lUpStreamNode);
      if (Result = nil) then
        Result := FindShapeWithPropertyValue(mtWRYMGroundWater, cNumber, lUpStreamNode);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.FindChannelDownShape (AChannel : IGeneralFlowChannel) : IVShape;
const OPNAME = 'TVNVEventHandler.FindChannelDownShape';
var
  lDownStreamNode : string;
begin
  Result := nil;
  try
    if (AChannel <> nil) then
    begin
      lDownStreamNode := IntToStr(AChannel.DownStreamNodeNumber);
      //if (AChannel.ChannelType = 14) then
      if (Result = nil) then
        Result := FindShapeWithPropertyValue(mtWRYMIrrBlock, cNumber, lDownStreamNode);
      //if (AChannel.ChannelType = 16) then
      if (Result = nil) then
        Result := FindShapeWithPropertyValue(mtWRYMWetland, cNumber, lDownStreamNode);
      //if (AChannel.ChannelType = 4) then
      if (Result = nil) then
          Result := FindShapeWithPropertyValue(mtWRYMIrrArea, cNumber, lDownStreamNode);
      if (Result = nil) then
        Result := FindShapeWithPropertyValue(mtWRYMReservoir, cNumber, lDownStreamNode);
      if (Result = nil) then
        Result := FindShapeWithPropertyValue(mtWRYMNode, cNumber, lDownStreamNode);
      if (Result = nil) then
        Result := FindShapeWithPropertyValue(mtWRYMDemandCentre, cNumber, lDownStreamNode);
      if (Result = nil) then
        Result := FindShapeWithPropertyValue(mtWRYMMine, cNumber, lDownStreamNode);
      if (Result = nil) then
        Result := FindShapeWithPropertyValue(mtWRYMGroundWater, cNumber, lDownStreamNode);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.RefreshChannel (AShape     : IVShape; AElementID : integer);
const OPNAME = 'TVNVEventHandler.RefreshChannel';
var
  lChannel       : IGeneralFlowChannel;
  lChannelName   : string;
  lColourChanged : boolean;
begin
  try
    if(AShape = nil) or (AElementID = 0) or (AElementID = NullInteger) then Exit;
    lChannel := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[AElementID];
    if (lChannel = nil) then
    begin
      AShape.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
      AShape.CellsSRC[visSectionObject, visRowLine, visLineColor].FormulaU := IntToStr(visRed)
    end
    else
    begin
      FVisioApp.ActiveWindow.Windows.ItemFromID[visWinIDCustProp].Visible := FALSE;
      lChannelName := lChannel.ChannelName;

      AShape.Cells[cName].Formula           := '"' + lChannelName + '"';
      AShape.Cells[cDownStreamNode].Formula := '"' + IntToStr(lChannel.DownStreamNodeNumber) + '"';
      AShape.Cells[cUpStreamNode].Formula   := '"' + IntToStr(lChannel.UpStreamNodeNumber) + '"';
      AShape.Cells[cPenalty].Formula        := '"' + IntToStr(lChannel.ChannelPenaltyNumber) + '"';
      AShape.Cells[cChannelType].Formula    := '"' + ChannelTypeName(lChannel) + '"';
      if (AShape.CellExists[cColourChanged, 0] = 0) then
      begin
        AShape.AddNamedRow(visSectionProp, 'ColourChanged', visTagDefault);
        AShape.Cells[cColourChanged].Formula    := '"N"';
        AShape.Cells[cColourChangedInv].Formula := '"TRUE"';
        lColourChanged := FALSE;
      end
      else
      begin
        lColourChanged := Trim(UnQuote(AShape.Cells[cColourChanged].Formula)) = 'Y';
      end;
      if (NOT lColourChanged) then
      begin
        if (lChannel.IFRFeature <> nil) then
          AShape.CellsSRC[visSectionObject, visRowLine, visLineColor].FormulaU := IntToStr(visDarkGreen) {3}
        else
        begin
          case lChannel.ChannelType of
            2 :
              AShape.CellsSRC[visSectionObject, visRowLine, visLineColor].FormulaU := IntToStr(visDarkYellow); {5}
            4 :
              AShape.CellsSRC[visSectionObject, visRowLine, visLineColor].FormulaU := IntToStr(visDarkGreen); {3}
            8	, 11 :
              AShape.CellsSRC[visSectionObject, visRowLine, visLineColor].FormulaU := IntToStr(visRed); {2}
            5, 6, 7, 9, 10 :
              AShape.CellsSRC[visSectionObject, visRowLine, visLineColor].FormulaU := IntToStr(visPurple); {12}
          else
            AShape.CellsSRC[visSectionObject, visRowLine, visLineColor].FormulaU := IntToStr(visBlack); {255}
          end;
        end;
      end;
      AShape.Cells[cRow1Action].Formula := 'RUNMACRO("ThisDocument.ProcessVNVSpecial 1")';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function  TVNVEventHandler.FindShapeWithPropertyValue (AMasterName : string;
                                                       AProperty   : string;
                                                       AValue      : string) : IVShape;
const OPNAME = 'TVNVEventHandler.FindShapeWithPropertyValue';
var
  lPageIndex  : Integer;
  lShapeIndex : Integer;
  lPage       : IVPage;
  lShape      : IVShape;
  lExists     : Boolean;
  lTempStr    : string;
begin
  Result := nil;
  try
    lExists := FALSE;
    lPageIndex := 1;
    while (NOT lExists) AND (lPageIndex <= FVisioApp.ActiveDocument.Pages.Count) do
    begin
      lPage := FVisioApp.ActiveDocument.Pages[lPageIndex];
      lShapeIndex := 1;
      while (NOT lExists) AND (lShapeIndex <= lPage.Shapes.Count) do
      begin
        lShape := lPage.Shapes[lShapeIndex];
        if ((lShape <> nil) AND (lShape.Master <> nil) AND
            (Pos(PChar(AMasterName), lShape.Master.Name) > 0)) then
        begin
          lTempStr := UnQuote(lShape.Cells[AProperty].Formula);
          if (lTempStr = AValue) then
          begin
            lExists := TRUE;
            Result  := lShape;
          end;
        end;
        lShapeIndex := lShapeIndex + 1;
      end;
      lPageIndex := lPageIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.FindTextWithParent (AParentName : string) : IVShape;
const OPNAME = 'TVNVEventHandler.FindTextWithParent';
var
  lPageIdx        : integer;
  lShapeIdx       : integer;
  lShape          : IVShape;
  lPage           : IVPage;
  lExists         : Boolean;
  lTempStr        : string;
begin
  Result := nil;
  try
    lExists  := FALSE;
    lPageIdx := 1;
    while (NOT lExists) AND (lPageIdx <= FVisioApp.ActiveDocument.Pages.Count) do
    begin
      lPage := FVisioApp.ActiveDocument.Pages[lPageIdx];
      lShapeIdx := 1;
      while (NOT lExists) AND (lShapeIdx <= lPage.Shapes.Count) do
      begin
        lShape := lPage.Shapes[lShapeIdx];
        if ((lShape <> nil) AND (Pos(PChar(mtWRYMText), lShape.Name) > 0)) then
        begin
          if (lShape.CellExists[cParent, 1] <> 0) then
          begin
            lTempStr := UnQuote(lShape.Cells[cParent].Formula);
            if (lTempStr = AParentName) then
            begin
              lExists := TRUE;
              Result  := lShape;
            end;
          end;
        end;
        lShapeIdx := lShapeIdx + 1;
      end;
      lPageIdx := lPageIdx + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.FindSFRATextWithParent (AParentName : string) : IVShape;
const OPNAME = 'TVNVEventHandler.FindSFRATextWithParent';
var
  lPageIdx        : integer;
  lShapeIdx       : integer;
  lShape          : IVShape;
  lPage           : IVPage;
  lExists         : Boolean;
  lTempStr        : string;
begin
  Result := nil;
  try
    lExists  := FALSE;
    lPageIdx := 1;
    while (NOT lExists) AND (lPageIdx <= FVisioApp.ActiveDocument.Pages.Count) do
    begin
      lPage := FVisioApp.ActiveDocument.Pages[lPageIdx];
      lShapeIdx := 1;
      while (NOT lExists) AND (lShapeIdx <= lPage.Shapes.Count) do
      begin
        lShape := lPage.Shapes[lShapeIdx];
        if ((lShape <> nil) AND (Pos(PChar(mtWRYMSubCatchmentTxt + FindNumber(AParentName)), lShape.Name) > 0)) then
        begin
          if (lShape.CellExists[cParent, 1] <> 0) then
          begin
            lTempStr := UnQuote(lShape.Cells[cParent].Formula);
            if (lTempStr = AParentName) then
            begin
              lExists := TRUE;
              Result  := lShape;
            end;
          end;
        end;
        lShapeIdx := lShapeIdx + 1;
      end;
      lPageIdx := lPageIdx + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TVNVEventHandler.FindTextShapesWithParent (AParentName : string;
                                                     AShapesList : TStringList);
const OPNAME = 'TVNVEventHandler.FindTextShapesWithParent';
var
  lPageIdx        : integer;
  lShapeIdx       : integer;
  lShape          : IVShape;
  lPage           : IVPage;
  lTempStr        : string;
begin
  try
    AShapesList.Clear;
    lPageIdx := 1;
    while (lPageIdx <= FVisioApp.ActiveDocument.Pages.Count) do
    begin
      lPage := FVisioApp.ActiveDocument.Pages[lPageIdx];
      lShapeIdx := 1;
      while (lShapeIdx <= lPage.Shapes.Count) do
      begin
        lShape := lPage.Shapes[lShapeIdx];
        if ((lShape <> nil) AND ((Pos(PChar(mtWRYMText), lShape.Name) > 0) or (Pos(PChar(mtWRYMSubCatchmentTxt), lShape.Name) > 0))) then
        begin
          if (lShape.CellExists[cParent, 1] <> 0) then
          begin
            lTempStr := UnQuote(lShape.Cells[cParent].Formula);
            if (lTempStr = AParentName) then
              AShapesList.Add(lShape.Name);
          end;
        end;
        lShapeIdx := lShapeIdx + 1;
      end;
      lPageIdx := lPageIdx + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.FindShapesWithParent (AParentName : string;
                                                 AShapesList : TStringList);
const OPNAME = 'TVNVEventHandler.FindShapesWithParent';
var
  lPageIdx        : integer;
  lShapeIdx       : integer;
  lShape          : IVShape;
  lPage           : IVPage;
  lTempStr        : string;
begin
  try
    AShapesList.Clear;
    lPageIdx := 1;
    while (lPageIdx <= FVisioApp.ActiveDocument.Pages.Count) do
    begin
      lPage := FVisioApp.ActiveDocument.Pages[lPageIdx];
      lShapeIdx := 1;
      while (lShapeIdx <= lPage.Shapes.Count) do
      begin
        lShape := lPage.Shapes[lShapeIdx];
        if ((lShape <> nil)) then
        begin
          if (lShape.CellExists[cParent, 1] <> 0) then
          begin
            lTempStr := UnQuote(lShape.Cells[cParent].Formula);
            if (lTempStr = AParentName) then
              AShapesList.Add(lShape.Name);
          end;
        end;
        lShapeIdx := lShapeIdx + 1;
      end;
      lPageIdx := lPageIdx + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TVNVEventHandler.FindTextTypeWithParent (ATextType   : TVNVTextType;
                                                  AParentName : string) : IVShape;
const OPNAME = 'TVNVEventHandler.FindTextTypeWithParent';
var
  lPageIdx        : integer;
  lShapeIdx       : integer;
  lShape          : IVShape;
  lPage           : IVPage;
  lExists         : Boolean;
  lParentStr      : string;
  lTextType       : TVNVTextType;
begin
  Result := nil;
  try
    lExists     := FALSE;
    lPageIdx    := 1;
    while (NOT lExists) AND (lPageIdx <= FVisioApp.ActiveDocument.Pages.Count) do
    begin
      lPage := FVisioApp.ActiveDocument.Pages[lPageIdx];
      lShapeIdx := 1;
      while (NOT lExists) AND (lShapeIdx <= lPage.Shapes.Count) do
      begin
        lShape := lPage.Shapes[lShapeIdx];
        if ((lShape <> nil) AND (Pos(PChar(mtWRYMText), lShape.Name) > 0)) then
        begin
          if (lShape.CellExists[cParent, 1] <> 0) AND
             (lShape.CellExists[cTextType, 1] <> 0) then
          begin
            lParentStr := UnQuote(lShape.Cells[cParent].Formula);
            lTextType  := TVNVTextType(StrToIntDef(UnQuote(lShape.Cells[cTextType].Formula),0));
            if (lParentStr = AParentName) AND (lTextType = ATextType) then
            begin
              lExists := TRUE;
              Result  := lShape;
            end;
          end;
        end;
        lShapeIdx := lShapeIdx + 1;
      end;
      lPageIdx := lPageIdx + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.FindShapeWithParent (AMasterName : string;
                                               AParentName : string) : IVShape;
const OPNAME = 'TVNVEventHandler.FindShapeWithParent';
var
  lPageIdx        : integer;
  lShapeIdx       : integer;
  lShape          : IVShape;
  lPage           : IVPage;
  lExists         : Boolean;
  lTempStr        : string;
begin
  Result := nil;
  try
    lExists  := FALSE;
    lPageIdx := 1;
    while (NOT lExists) AND (lPageIdx <= FVisioApp.ActiveDocument.Pages.Count) do
    begin
      lPage := FVisioApp.ActiveDocument.Pages[lPageIdx];
      lShapeIdx := 1;
      while (NOT lExists) AND (lShapeIdx <= lPage.Shapes.Count) do
      begin
        lShape := lPage.Shapes[lShapeIdx];
        if ((lShape <> nil) AND (lShape.Master <> nil) AND
            (Pos(PChar(AMasterName), lShape.Master.Name) > 0)) then
        begin
          if (lShape.CellExists[cParent, 1] <> 0) then
          begin
            lTempStr := UnQuote(lShape.Cells[cParent].Formula);
            if (lTempStr = AParentName) then
            begin
              lExists := TRUE;
              Result  := lShape;
            end;
          end;
        end;
        lShapeIdx := lShapeIdx + 1;
      end;
      lPageIdx := lPageIdx + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.FindInflowShapeByReservoirNumber(AReservoirNumber: integer): IVShape;
const OPNAME = 'TVNVEventHandler.FindInflowShapeByReservoirNumber';
var
  LNumber,
  lPageIdx        : integer;
  lShapeIdx       : integer;
  lShape          : IVShape;
  lPage           : IVPage;
  lExists         : Boolean;
  lTempStr        : string;
begin
  Result := nil;
  try
    lExists  := FALSE;
    lPageIdx := 1;
    while (NOT lExists) AND (lPageIdx <= FVisioApp.ActiveDocument.Pages.Count) do
    begin
      lPage := FVisioApp.ActiveDocument.Pages[lPageIdx];
      lShapeIdx := 1;
      while (NOT lExists) AND (lShapeIdx <= lPage.Shapes.Count) do
      begin
        lShape := lPage.Shapes[lShapeIdx];
        if ((lShape <> nil) AND (lShape.Master <> nil) AND (Pos(PChar(mtWRYMInflow), lShape.Master.Name) > 0)) then
        begin
          lTempStr := UnQuote(lShape.Cells[cNumber].Formula);
          LNumber  := StrToIntDef(lTempStr,-1);
          if (LNumber = AReservoirNumber) then
          begin
            lExists := TRUE;
            Result  := lShape;
          end;
        end;
        lShapeIdx := lShapeIdx + 1;
      end;
      lPageIdx := lPageIdx + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.FindSFRAShapeByReservoirNumber(AReservoirNumber: integer): IVShape;
const OPNAME = 'TVNVEventHandler.FindSFRAShapeByReservoirNumber';
var
  LNumber,
  lPageIdx        : integer;
  lShapeIdx       : integer;
  lShape          : IVShape;
  lPage           : IVPage;
  lExists         : Boolean;
  lTempStr        : string;
begin
  Result := nil;
  try
    lExists  := FALSE;
    lPageIdx := 1;
    while (NOT lExists) AND (lPageIdx <= FVisioApp.ActiveDocument.Pages.Count) do
    begin
      lPage := FVisioApp.ActiveDocument.Pages[lPageIdx];
      lShapeIdx := 1;
      while (NOT lExists) AND (lShapeIdx <= lPage.Shapes.Count) do
      begin
        lShape := lPage.Shapes[lShapeIdx];
        if ((lShape <> nil) AND (lShape.Master <> nil) AND ((Pos(PChar(mtWRYMSubCatchment), lShape.Master.Name) > 0))) then
        begin
          lTempStr := UnQuote(lShape.Cells[cNumber].Formula);
          LNumber  := StrToIntDef(lTempStr,-1);
          if (LNumber = AReservoirNumber) then
          begin
            lExists := TRUE;
            Result  := lShape;
          end;
        end;
        lShapeIdx := lShapeIdx + 1;
      end;
      lPageIdx := lPageIdx + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.FindNodeShapeBySubCatchmentNumber(ASubCatchmentNumber: integer): IVShape;
const OPNAME = 'TVNVEventHandler.FindNodeShapeBySubCatchmentNumber';
var
  LNumber,
  lPageIdx        : integer;
  lShapeIdx       : integer;
  lShape          : IVShape;
  lPage           : IVPage;
  lExists         : Boolean;
  lTempStr        : string;
begin
  Result := nil;
  try
    lExists  := FALSE;
    lPageIdx := 1;
    while (NOT lExists) AND (lPageIdx <= FVisioApp.ActiveDocument.Pages.Count) do
    begin
      lPage := FVisioApp.ActiveDocument.Pages[lPageIdx];
      lShapeIdx := 1;
      while (NOT lExists) AND (lShapeIdx <= lPage.Shapes.Count) do
      begin
        lShape := lPage.Shapes[lShapeIdx];
        if ((lShape <> nil) AND (lShape.Master <> nil) AND
            ((Pos(PChar(mtWRYMReservoir), lShape.Master.Name) > 0) OR
            (Pos(PChar(mtWRYMNode), lShape.Master.Name) > 0))) then
        begin
          lTempStr := UnQuote(lShape.Cells[cNumber].Formula);
          LNumber  := StrToIntDef(lTempStr,-1);
          if (LNumber = ASubCatchmentNumber) then
          begin
            lExists := TRUE;
            Result  := lShape;
          end;
        end;
        lShapeIdx := lShapeIdx + 1;
      end;
      lPageIdx := lPageIdx + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TVNVEventHandler.FindChannelShapeByChannelNumber(AChannelNumber: integer): IVShape;
const OPNAME = 'TVNVEventHandler.FindChannelShapeByChannelNumber';
var
  LNumber,
  lPageIdx        : integer;
  lShapeIdx       : integer;
  lShape          : IVShape;
  lPage           : IVPage;
  lExists         : Boolean;
  lTempStr        : string;
begin
  Result := nil;
  try
    lExists  := FALSE;
    lPageIdx := 1;
    while (NOT lExists) AND (lPageIdx <= FVisioApp.ActiveDocument.Pages.Count) do
    begin
      lPage := FVisioApp.ActiveDocument.Pages[lPageIdx];
      lShapeIdx := 1;
      while (NOT lExists) AND (lShapeIdx <= lPage.Shapes.Count) do
      begin
        lShape := lPage.Shapes[lShapeIdx];
        if ((lShape <> nil) AND (lShape.Master <> nil) AND
            (Pos(PChar(mtWRYMChannel), lShape.Master.Name) > 0)) then
        begin
          lTempStr := UnQuote(lShape.Cells[cNumber].Formula);
          LNumber  := StrToIntDef(lTempStr,-1);
          if (LNumber = AChannelNumber) then
          begin
            lExists := TRUE;
            Result  := lShape;
          end;
        end;
        lShapeIdx := lShapeIdx + 1;
      end;
      lPageIdx := lPageIdx + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.FindShapeWithName (AShapeName : string) : IVShape;
const OPNAME = 'TVNVEventHandler.FindShapeWithName';
var
  lPageIdx        : integer;
  lShapeIdx       : integer;
  lShape          : IVShape;
  lPage           : IVPage;
  lFound          : Boolean;
begin
  Result := nil;
  try
    lFound  := FALSE;
    lPageIdx := 1;
    while (NOT lFound) AND (lPageIdx <= FVisioApp.ActiveDocument.Pages.Count) do
    begin
      lPage := FVisioApp.ActiveDocument.Pages[lPageIdx];
      lShapeIdx := 1;
      while (NOT lFound) AND (lShapeIdx <= lPage.Shapes.Count) do
      begin
        lShape := lPage.Shapes[lShapeIdx];
        if (lShape.Name = AShapeName) then
        begin
          lFound := TRUE;
          Result := lShape;
        end;
        lShapeIdx := lShapeIdx + 1;
      end;
      lPageIdx := lPageIdx + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.FindDemandCentreByNode (AElementID : integer) : IVShape;
const OPNAME = 'TVNVEventHandler.FindDemandCentreByNode';
var
  lElementID,
  lIndex             :  integer;
  lShape             :  IVShape;
begin
  try
    Result := nil;
    for LIndex := 1 to FVisioApp.ActivePage.Shapes.Count do
    begin
      LShape := FVisioApp.ActivePage.Shapes[LIndex];
      if (Pos(PChar(mtWRYMDemandCentre), LShape.Name) > 0) then
      begin
        lElementID := StrToIntDef(UnQuote(LShape.Cells[cNumber].Formula), -1);//StrToInt(lFormula);
        if (lElementID = AElementID) then
        begin
          Result := lShape;
          break;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ShowReservoirsDialog : integer;
const OPNAME = 'TVNVEventHandler.ShowReservoirsDialog';
var
  LReservoirsDlg : TVNVReservoirDialog;
  lStrTemp       : string;
begin
  Result := -1;
  try
  	LReservoirsDlg := TVNVReservoirDialog.CreateWithoutDFM(nil, FAppModules);
    LReservoirsDlg.LanguageHasChanged;
    try
      lStrTemp := FReservoirList.CommaText;
	    Result   := LReservoirsDlg.ShowReservoirsDialog(lStrTemp, FReservoirExistNew, FReservoirDuplicate);
    finally
      LReservoirsDlg.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVEventHandler.ShowSubCatchmentDialog(ANodeNr  :  Integer; ASFRADialogType : TSFRADialogType) : integer;
const OPNAME = 'TVNVEventHandler.ShowSubCatchmentDialog';
var
  lShowSubCatchmentDlg : TVNVSubCatchmentDialog; 
  lStrTemp             : string;
  lBoolProp            : Boolean;
begin
  Result := -1;
  try
    lShowSubCatchmentDlg := TVNVSubCatchmentDialog.CreateWithoutDFM(nil, FAppModules);
    lShowSubCatchmentDlg.LanguageHasChanged;
    try
      lStrTemp := FSubCatchmentList.CommaText;
      lBoolProp := False;
      Case ASFRADialogType of
        sfrdtCreateNew      : lBoolProp := True;
        sfrdtViewProperties : lBoolProp := False;
      end;
      Result   := lShowSubCatchmentDlg.ShowSubCatchmentDialog(lStrTemp, FSubCatchmentExistNew, FSubCatchmentDuplicate, ANodeNr, lBoolProp);
    finally
      lShowSubCatchmentDlg.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVEventHandler.ShowIrrigationBlockDialog: integer;
const OPNAME = 'TVNVEventHandler.ShowIrrigationBlockDialog';
var
  LIrrigationBlockDlg : TVNVIrrigationBlockDialog;
  lStrTemp    : string;
begin
  Result := -1;
  try
  	LIrrigationBlockDlg := TVNVIrrigationBlockDialog.CreateWithoutDFM(nil, FAppModules);
    LIrrigationBlockDlg.LanguageHasChanged;
    try
      lStrTemp := FIrrigationList.CommaText;
	    Result   := LIrrigationBlockDlg.ShowIrrigationBlockDialog(lStrTemp, FIrrigationExistNew, FIrrigationDuplicate);
    finally
      LIrrigationBlockDlg.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVEventHandler.ShowWetlandDialog: integer;
const OPNAME = 'TVNVEventHandler.ShowWetlandDialog';
var
  LWetlandDlg : TVNVWetlandDialog;
  lStrTemp    : string;
begin
  Result := -1;
  try
  	LWetlandDlg := TVNVWetlandDialog.CreateWithoutDFM(nil, FAppModules);
    LWetlandDlg.LanguageHasChanged;
    try
      lStrTemp := FWetlandList.CommaText;
	    Result   := LWetlandDlg.ShowWetlandDialog(lStrTemp, FWetlandExistNew, FWetlandDuplicate);
    finally
      LWetlandDlg.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVEventHandler.ShowDemandCentreDialog : integer;
const OPNAME = 'TVNVEventHandler.ShowDemandCentreDialog';
var
  lDemandCentreDlg : TVNVDemandCentreDialog;
  lStrTemp         : string;
begin
  Result := -1;
  try
    lDemandCentreDlg := TVNVDemandCentreDialog.CreateWithoutDFM(nil, FAppModules);
    lDemandCentreDlg.LanguageHasChanged;
    try
      lStrTemp := FDemandCentreList.CommaText;
      Result   := lDemandCentreDlg.ShowDemandCentreDialog(lStrTemp, FDemandCentreExistNew, FDemandCentreDuplicate);
    finally
      lDemandCentreDlg.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVEventHandler.ShowNodesDialog : integer;
const OPNAME = 'TVNVEventHandler.ShowNodesDialog';
var
  LNodesDlg : TVNVNodesDialog;
  lStrTemp  : string;
begin
  Result := -1;
  try
  	LNodesDlg := TVNVNodesDialog.CreateWithoutDFM(nil, FAppModules);
    LNodesDlg.LanguageHasChanged;
    try
      lStrTemp := FNodeList.CommaText;
		  Result   := LNodesDlg.ShowNodesDialog(lStrTemp, FNodeWithInflow, FNodeExistNew, FNodeDuplicate);
    finally
      LNodesDlg.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVEventHandler.ShowChannelsDialog : integer;
const OPNAME = 'TVNVEventHandler.ShowChannelsDialog';
var
  LChannelsDlg : TVNVChannelsDialog;
  lStrTemp     : string;
begin
  Result := -1;
  try
  	LChannelsDlg := TVNVChannelsDialog.CreateWithoutDFM(nil, FAppModules);
    LChannelsDlg.LanguageHasChanged;
    try
      lStrTemp := FChannelList.CommaText;
	  	Result   := LChannelsDlg.ShowChannelsDialog(lStrTemp, FChannelExistNew, FChannelDuplicate);
    finally
      LChannelsDlg.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVEventHandler.ShowPowerPlantDialog : integer;
const OPNAME = 'TVNVEventHandler.ShowPowerPlantDialog';
var
  LPowerPlantDlg : TVNVPowerPlantDialog;
  lStrTemp     : string;
begin
  Result := -1;
  try
  	LPowerPlantDlg := TVNVPowerPlantDialog.CreateWithoutDFM(nil, FAppModules);
    LPowerPlantDlg.LanguageHasChanged;
    try
      lStrTemp := FPowerPlantList.CommaText;
	  	Result   := LPowerPlantDlg.ShowPowerPlantDialog(lStrTemp, FPowerPlantExistNew, FPowerPlantDuplicate);
    finally
      LPowerPlantDlg.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVEventHandler.OnShapeDelete (ASender      : TObject;
                                          const AShape : IVShape);
const OPNAME = 'TVNVEventHandler.OnShapeDelete';
begin
  try
    if (Pos(PChar(mtWRYMReservoir), AShape.Name) > 0) then
      ReservoirShapeDeleted(AShape)
    else
    if (Pos(PChar(mtWRYMMine), AShape.Name) > 0) then
      MineShapeDeleted(AShape)
    else
    if (Pos(PChar(mtWRYMSubCatchment), AShape.Name) > 0) then
      SubCatchmentShapeDeleted(AShape)
    else
    if (Pos(PChar(mtWRYMNode), AShape.Name) > 0) then
      NodeShapeDeleted(AShape)
    else
    if (Pos(PChar(mtWRYMChannel), AShape.Name) > 0) then
      ChannelShapeDeleted(AShape)
    else
    if (Pos(PChar(mtWRYMResPenalty), AShape.Name) > 0) or (Pos(PChar(mtWRYMResPenaltyExt), AShape.Name) > 0) then
      ReservoirPenaltyShapeDeleted(AShape)
    else
    if (Pos(PChar(mtWRYMChanPenalty), AShape.Name) > 0) then
      ChannelPenaltyShapeDeleted(AShape)
    else
    if (Pos(PChar(mtWRYMInflow), AShape.Name) > 0) then
      InflowShapeDeleted(AShape)
    else
    if (Pos(PChar(mtWRYMIrrBlock), AShape.Name) > 0) then
      IrrigationBlockShapeDeleted(AShape)
    else
    if (Pos(PChar(mtWRYMIrrArea), AShape.Name) > 0) then
      IrrigationAreaShapeDeleted(AShape)
    else
    if (Pos(PChar(mtWRYMWetland), AShape.Name) > 0) then
      WetlandShapeDeleted(AShape)
    else
    if (Pos(PChar(mtWRYMDemandCentre), AShape.Name) > 0) then
      DemandCentreShapeDeleted(AShape)
    else
    if (Pos(PChar(mtWRYMOutputSelector), AShape.Name) > 0) then
      FOutputSelector := nil
    else
    if (Pos(PChar(mtWRYMGroundWater), AShape.Name) > 0) then
      GroundWaterShapeDeleted(AShape)
    else
    if (Pos(PChar(mtWRYMPowerPlant), AShape.Name) > 0) then
      PowerPlantShapeDeleted(AShape);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ShapeIsSelected (ASelection : IVSelection;
                                           AShapeName : string) : Boolean;
const OPNAME = 'TVNVEventHandler.ShapeIsSelected';
var
  lIndex  : integer;
  lShape  : IVShape;
begin
  Result := False;
  try
    if (ASelection <> nil) AND (ASelection.Count >= 1) then
    begin
      lIndex := 1;
      while ((NOT Result) AND (lIndex <= ASelection.Count)) do
      begin
        lShape := ASelection.Item[lIndex];
        if (lShape.Name = AShapeName) then
          Result := True
        else
          lIndex := lIndex + 1;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.DeleteAllTextShapes (const AParentShape : IVShape);
const OPNAME = 'TVNVEventHandler.DeleteAllTextShapes';
var
  lTextShape  : IVShape;
  lShapesLst  : TStringList;
  lIndex      : integer;
  lShapeName  : string;
  lSelection  : IVSelection;
begin
  try
    lSelection := FVisioApp.ActiveWindow.Selection;
    lShapesLst := TStringList.Create;
    try
      FindTextShapesWithParent(AParentShape.Name, lShapesLst);
      for lIndex := 0 to lShapesLst.Count - 1 do
      begin
        lShapeName := lShapesLst.Strings[lIndex];
        lTextShape  := FindShapeWithName(lShapeName);
        if (lTextShape <> nil) AND (NOT ShapeIsSelected(lSelection, lTextShape.Name)) then
          lTextShape.Delete;
      end;
    finally
      FreeAndNil(lShapesLst);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVEventHandler.DeleteAllShapesWithParent (const AParentShape : IVShape);
const OPNAME = 'TVNVEventHandler.DeleteAllShapesWithParent';
var
  lTextShape  : IVShape;
  lShapesLst  : TStringList;
  lIndex      : integer;
  lShapeName  : string;
  lSelection  : IVSelection;
begin
  try
    lSelection := FVisioApp.ActiveWindow.Selection;
    lShapesLst := TStringList.Create;
    try
      FindShapesWithParent(AParentShape.Name, lShapesLst);
      for lIndex := 0 to lShapesLst.Count - 1 do
      begin
        lShapeName := lShapesLst.Strings[lIndex];
        lTextShape  := FindShapeWithName(lShapeName);
        if (lTextShape <> nil) AND (NOT ShapeIsSelected(lSelection, lTextShape.Name)) then
          lTextShape.Delete;
      end;
    finally
      FreeAndNil(lShapesLst);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVEventHandler.DeleteOutputTextLabels(const AShape : IVShape);
const OPNAME = 'TVNVEventHandler.DeleteOutputTextLabels';
begin
  try
    if AShape <> nil then
      AShape.Delete;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVEventHandler.DeleteRelatedOutputTextLabels(const AShape : IVShape; ANumber: integer);
const OPNAME = 'TVNVEventHandler.DeleteRelatedOutputTextLabels';
var
  LIndex     : Integer;
  LContainer : TInterfaceList;
  LTextType  : TVNVTextType;
  LShape     : IVShape;
begin
  try
    LContainer   := TInterfaceList.Create;
    try
      LTextType := TVNVTextType(StrToIntDef(UnQuote(AShape.Cells[cTextType].Formula),0));
      GetOutputTextShape(LContainer,LTextType);
      for LIndex := 0 to LContainer.Count-1 do
      begin
        LShape :=   IVShape(LContainer[LIndex]);
        if (UnQuote(IVShape(LContainer[LIndex]).Cells[cPinX].Formula) =
          UnQuote(AShape.Cells[cPinX].Formula)) and
          (UnQuote(IVShape(LContainer[LIndex]).Cells[cPinY].Formula) =
          UnQuote(AShape.Cells[cPinY].Formula)) then
        begin
          DoDeleteRelatedOutputTextLabels(LShape,ANumber,LContainer,LTextType);
          Break;
        end;
      end;
    finally
      FreeAndNil(LContainer);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVEventHandler.DoDeleteRelatedOutputTextLabels(const AShape : IVShape; ANumber: integer;AContainer: TInterfaceList;ATextType:TVNVTextType);
const OPNAME = 'TVNVEventHandler.DoDeleteRelatedOutputTextLabels';
var
  LYieldModelData : IYieldModelData;
  LReservoirData  : IReservoirData;
  LChannel        : IGeneralFlowChannel;
  LChannelShape,
  LReservoirShape : IVShape;
  LContainer      : TInterfaceList;
  LCount,
  LIndex          : integer;
begin
  try
    LYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
    LContainer := TInterfaceList.Create;
    try
      LReservoirData := LYieldModelData.NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[ANumber];
      if LReservoirData <> nil then
      begin
        LReservoirShape  := FindReservoirWithNumber(LReservoirData.ReservoirConfigurationData.ReservoirIdentifier);
        if(LReservoirShape = nil) then
          LReservoirShape  := FindNodeShapeByNodeNumber(LReservoirData.ReservoirConfigurationData.ReservoirIdentifier);
      end;
      LChannel := LYieldModelData.NetworkElementData.ChannelList.ChannelByChannelNumber[ANumber];
      if LChannel <> nil then
        LChannelShape  := FindChannelShapeByChannelNumber(LChannel.ChannelNumber);

      if(LChannelShape <> nil) then
      begin
        GetOutputTextShapesByParent(LContainer,LChannelShape,ATextType);
        if LContainer.Count>0 then
        begin
          LCount := LContainer.Count;
          LIndex := LContainer.IndexOf(AShape);
          if (LIndex>=0) then
            DeleteChannelRelatedOutputTextLabels(ANumber,LCount,LIndex,ATextType);
        end;
      end
      else
      if(LReservoirShape <> nil) then
      begin
        GetOutputTextShapesByParent(LContainer,LReservoirShape,ATextType);
        if LContainer.Count>0 then
        begin
          LCount := LContainer.Count;
          LIndex := LContainer.IndexOf(AShape);
          if (LIndex>=0) then
            DeleteReservoirsRelatedOutputTextLabels(ANumber,LCount,LIndex,ATextType);
        end;
      end;
    finally
      FreeAndNil(LContainer);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVEventHandler.DeleteChannelRelatedOutputTextLabels(ANumber,ACount,AIndex: integer;ATextType:TVNVTextType);
const OPNAME = 'TVNVEventHandler.DeleteReservoirsRelatedOutputTextLabels';
var
  LYieldModelData : IYieldModelData;
  LChannel        : IGeneralFlowChannel;
  LCount,
  LContainerIndex,
  LIndex          : integer;
  LChannelShape   : IVShape;
  LContainer      : TInterfaceList;
begin
  try
    LYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
    LContainer := TInterfaceList.Create;
    try
      for LIndex := 0 to lYieldModelData.NetworkElementData.ChannelList.ChannelCount-1 do
      begin
        LChannel := lYieldModelData.NetworkElementData.ChannelList.ChannelByIndex[LIndex];
        if (LChannel.MasterControlFeature = nil) and (UpperCase(LChannel.SummaryOutputRequired) <> 'Y')  then
          Continue;
        LChannelShape  := FindChannelShapeByChannelNumber(LChannel.ChannelNumber);
        if(LChannelShape <> nil) then
        begin
          GetOutputTextShapesByParent(LContainer,LChannelShape,ATextType);
          LContainerIndex := -1;
          LCount := LContainer.Count;
          while LContainerIndex <= LContainer.Count-1 do
          begin
            LContainerIndex := LContainerIndex+1;
            if (LContainerIndex>=0)and (LContainerIndex = AIndex) and
              (LCount = ACount) then
            begin
              IVShape(LContainer[LContainerIndex]).Delete;
              LContainer.Delete(LContainerIndex);
              ReAlignOutputTextLabels(LContainer,LContainerIndex,LChannelShape);
            end;
          end;
        end;
      end;
    finally
      FreeandNil(LContainer);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TVNVEventHandler.DeleteReservoirsRelatedOutputTextLabels(ANumber,ACount,AIndex: integer;ATextType:TVNVTextType);
const OPNAME = 'TVNVEventHandler.DeleteReservoirsRelatedOutputTextLabels';
var
  LYieldModelData : IYieldModelData;
  LReservoirData  : IReservoirData;
  LCount,
  LContainerIndex,
  LIndex          : integer;
  LReservoirShape : IVShape;
  LContainer      : TInterfaceList;
begin
  try
    LYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
    LContainer := TInterfaceList.Create;
    try
      for LIndex := 0 to lYieldModelData.NetworkElementData.ReservoirList.ReservoirAndNodesCount-1 do
      begin
        LReservoirData := lYieldModelData.NetworkElementData.ReservoirList.ReservoirOrNodeByIndex[LIndex];

        LReservoirShape  := FindReservoirWithNumber(LReservoirData.ReservoirConfigurationData.ReservoirIdentifier);
        if(LReservoirShape = nil) then
          LReservoirShape  := FindNodeShapeByNodeNumber(LReservoirData.ReservoirConfigurationData.ReservoirIdentifier);
        if(LReservoirShape <> nil) then
        begin
          if(LReservoirData.ReservoirConfigurationData.CatchmentRef = 0)  then
            Continue;
          GetOutputTextShapesByParent(LContainer,LReservoirShape,ATextType);
          LContainerIndex := -1;
          LCount := LContainer.Count;

          while LContainerIndex <= LContainer.Count-1 do
          begin
            LContainerIndex := LContainerIndex+1;
            if (LContainerIndex>=0)and (LContainerIndex = AIndex) and
              (LCount = ACount) then
            begin
              IVShape(LContainer[LContainerIndex]).Delete;
              LContainer.Delete(LContainerIndex);
              ReAlignOutputTextLabels(LContainer,LContainerIndex,LReservoirShape);
            end;
          end;
        end;
      end;
    finally
      FreeandNil(LContainer);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVEventHandler.GetOutputTextShapesByParent(AContainer : TInterfaceList; AParentShape : IVShape;ATextType:TVNVTextType);
const OPNAME = 'TVNVEventHandler.GetOutputTextShapesByParent';
var
  LPageIdx        : integer;
  LShapeIdx       : integer;
  LShape          : IVShape;
  LPage           : IVPage;
begin
  try
    AContainer.Clear;
    for LPageIdx := 1 to FVisioApp.ActiveDocument.Pages.Count do
    begin
      LPage := FVisioApp.ActiveDocument.Pages[lPageIdx];
      for LShapeIdx := 1 to LPage.Shapes.Count do
      begin
        LShape := LPage.Shapes[lShapeIdx];
        if (LShape.CellExists[cTextType, 0] <> 0) then
        begin
          if(LShape.Cells[cNumber].Formula = AParentShape.Cells[cNumber].Formula) and
            (LShape.Cells[cTextType].Formula = '"' + IntToStr(Ord(ATextType)) + '"') then
            AContainer.Add(lShape);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.ReAlignOutputTextLabels(AContainer:TInterfaceList;AIndex:integer;const AShape : IVShape);
const OPNAME = 'TVNVEventHandler.ReAlignOutputTextLabels';
var
  LCurrentShape,
  LPreviousShape : IVShape;
  LXPin : double;
  LYPin : double;
  LIndex : integer;
begin
  try
    if (AIndex<=AContainer.Count-1) then
    begin
      for LIndex := 0 to AContainer.Count-1 do
      begin
        if LIndex > 0 then
          LPreviousShape := IVShape(AContainer[LIndex-1])
        else
          LPreviousShape := AShape;
        LCurrentShape := IVShape(AContainer[LIndex]);
        LXPin := StrToFloatDef(UnMM(UnQuote(LPreviousShape.Cells[cPinX].ResultStr[visMillimeters])),0);
        LYPin := StrToFloatDef(UnMM(UnQuote(LPreviousShape.Cells[cPinY].ResultStr[visMillimeters])),0);
        LYPin := LYPin + 3;
        LCurrentShape.Cells[cPinX].Formula    := '"' + FloatToStrF(LXPin, ffFixed, 12, 2) + 'mm"';
        LCurrentShape.Cells[cPinY].Formula    := '"' + FloatToStrF(LYPin, ffFixed, 12, 2) + 'mm"';
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVEventHandler.ReservoirShapeDeleted (const AShape: IVShape);
const OPNAME = 'TVNVEventHandler.ReservoirShapeDeleted';
var
  lShape          : IVShape;
  lSelection      : IVSelection;
  lElementID      : string;
  lElementIdx     : integer;
  lInflowTxt      : IVShape;

begin
  try
    lElementID := UnQuote(AShape.Cells[cNumber].Formula);
    lElementIdx := FReservoirList.IndexOf(lElementID);
    if (lElementIdx >= 0) then
    begin
      FReservoirList.Delete(lElementIdx);
      lSelection := FVisioApp.ActiveWindow.Selection;

      { Remove all penalties for this reservoir}
      lShape := FindShapeWithParent(mtWRYMResPenalty, AShape.Name);
      if (lShape <> nil) then
      begin
        if (NOT ShapeIsSelected(lSelection, lShape.Name)) then
          lShape.Delete;
      end
      else
      begin
        lShape := FindShapeWithParent(mtWRYMResPenaltyExt, AShape.Name);
        if (lShape <> nil) then
        begin
          if (NOT ShapeIsSelected(lSelection, lShape.Name)) then
            lShape.Delete;
        end;
      end;

      {Remove all inflows for this reservoir}
      if (FBufferNodeHasSFRAList.IndexOf(AShape.Name) >= 0) then
      begin
        lShape := FindShapeWithParent(mtWRYMSubCatchment, AShape.Name);
        FBufferNodeHasSFRAList.Delete(FBufferNodeHasSFRAList.IndexOf(AShape.Name));
        lInflowTxt := FindShapeWithParent(mtWRYMText, AShape.Name);
        if lInflowTxt <> nil then
          begin
            lInflowTxt.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
            lInflowTxt.Delete;
          end;

        lInflowTxt := FindShapeWithParent(mtWRYMText, AShape.Name);
        if lInflowTxt <> nil then
          begin
            lInflowTxt.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
            lInflowTxt.Delete;
          end;

      end
      else
        lShape := FindShapeWithParent(mtWRYMInflow, AShape.Name);

      if (lShape <> nil) then
      begin
        lInflowTxt := FindShapeWithParent(mtWRYMText, lShape.Name);
        if (lInflowTxt <> nil) then
        begin
          if (NOT ShapeIsSelected(lSelection, lInflowTxt.Name)) then
            begin
              lInflowTxt.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
              lInflowTxt.Delete;
            end;
        end;
        if (NOT ShapeIsSelected(lSelection, lShape.Name)) then
        begin
          lShape.Cells[cParent].Formula := '""';
          lShape.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
          lShape.Delete;
        end;
      end;

      DeleteAllShapesWithParent(AShape);

      {Remove all text annotations for this reservoir}
      DeleteAllTextShapes(AShape);

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.SubCatchmentShapeDeleted (const AShape: IVShape);
const OPNAME = 'TVNVEventHandler.SubCatchmentShapeDeleted';
var
  lParentShape : IVShape;
  lShape      : IVShape;
  lSelection  : IVSelection;
  lMsg,
  lParentName,
  lElementID  : string;
  lElementIdx : integer;
  lInflowTxt  : IVShape;
begin
  try
    if (AShape = nil) then
      exit;

    lElementID := UnQuote(AShape.Cells[cNumber].Formula);
    lElementIdx := FBufferSubCatchmentList.IndexOf(lElementID);
    lParentName  := UnQuote(AShape.Cells[cParent].Formula);
    if (lElementIdx < 0) then
    begin
      if (lParentName <> '') then
      begin
        lParentShape := FindShapeWithName(lParentName);
        if (lParentShape <> nil) then
        lMsg := FAppModules.Language.GetString('VNV.MayNotDeleteInflow');
        MessageDlg(lMsg, mtError, [mbOK], 0);
        AddInflowSymbol(lParentShape, StrToIntDef(lElementID,0));
      end
    end
    else
    begin
      FBufferSubCatchmentList.Delete(lElementIdx);
      {Remove text}
      lInflowTxt := FindSFRATextWithParent(lParentName);
      if (lInflowTxt <> nil) then
      begin
        lInflowTxt.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
        lInflowTxt.Delete;
      end;
      {Remove all inflows for this SubCatchment}
      lSelection := FVisioApp.ActiveWindow.Selection;
      lShape := FindShapeWithParent(mtWRYMSubCatchment, AShape.Name);
      if (lShape <> nil) then
      begin
        if (NOT ShapeIsSelected(lSelection, lShape.Name)) then
        begin
          lShape.Cells[cParent].Formula := '""';
          lShape.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
          lShape.Delete;
        end;
      end;
      {Remove all text annotations for this SubCatchment}
      DeleteAllTextShapes(AShape);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TVNVEventHandler.NodeShapeDeleted(const AShape: IVShape);
const OPNAME = 'TVNVEventHandler.NodeShapeDeleted';
var
  lShape      : IVShape;
  lSelection  : IVSelection;
  lElementID  : string;
  lElementIdx : integer;
  lInflowTxt  : IVShape;
begin
  try
    lElementID  := UnQuote(AShape.Cells[cNumber].Formula);
    lElementIdx := FNodeList.IndexOf(lElementID);
    if (lElementIdx >= 0) then
    begin
      FNodeList.Delete(lElementIdx);
      lSelection := FVisioApp.ActiveWindow.Selection;

      {Remove all inflows for this reservoir}
      if (FBufferNodeHasSFRAList.IndexOf(AShape.Name) >= 0) then
      begin
        lShape := FindShapeWithParent(mtWRYMSubCatchment, AShape.Name);
        FBufferNodeHasSFRAList.Delete(FBufferNodeHasSFRAList.IndexOf(AShape.Name));
        lInflowTxt := FindShapeWithParent(mtWRYMText, AShape.Name);
        if lInflowTxt <> nil then
          begin
            lInflowTxt.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
            lInflowTxt.Delete;
          end;
      end
      else
        lShape := FindShapeWithParent(mtWRYMInflow, AShape.Name);

      if (lShape <> nil) then
      begin
        lInflowTxt := FindShapeWithParent(mtWRYMText, lShape.Name);
        if (lInflowTxt <> nil) then
        begin
          if (NOT ShapeIsSelected(lSelection, lInflowTxt.Name)) then
            begin
              lInflowTxt.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
              lInflowTxt.Delete;
            end;
        end;
        if (NOT ShapeIsSelected(lSelection, lShape.Name)) then
        begin
          lShape.Cells[cParent].Formula := '""';
          lShape.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
          lShape.Delete;
        end;
      end;

      DeleteAllShapesWithParent(AShape);
      
      {Remove all text annotations for this node}
      DeleteAllTextShapes(AShape);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.ChannelShapeDeleted(const AShape: IVShape);
const OPNAME = 'TVNVEventHandler.ChannelShapeDeleted';
var
  lShape      : IVShape;
  lSelection  : IVSelection;
  lElementID  : string;
  lElementIdx : integer;
begin
  try
    lElementID  := UnQuote(AShape.Cells[cNumber].Formula);
    lElementIdx := FChannelList.IndexOf(lElementID);
    if (lElementIdx >= 0) then
    begin
      FChannelList.Delete(lElementIdx);
      lSelection := FVisioApp.ActiveWindow.Selection;
      {Remove all penalties for this channel}
      lShape := FindShapeWithParent(mtWRYMChanPenalty, AShape.Name);
      if (lShape <> nil) then
      begin
        if (NOT ShapeIsSelected(lSelection, lShape.Name)) then
          lShape.Delete;
      end;
      {Remove all text annotations for this channel}
      DeleteAllTextShapes(AShape);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.IrrigationBlockShapeDeleted( const AShape: IVShape);
const OPNAME = 'TVNVEventHandler.IrrigationBlockShapeDeleted';
var
  lShape      : IVShape;
  lSelection  : IVSelection;
  lElementID  : string;
  lElementIdx : integer;
  lIrrigationBlock : IIrrigationBlock;
 begin
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then
    begin
      MessageDlg(FAppModules.Language.GetString('VNV.IrrigationBlockNoModelSupport'), mtError, [mbOK], 0);
      Exit;
    end;

    lElementID  := UnQuote(AShape.Cells[cNumber].Formula);
    lElementIdx := FIrrigationList.IndexOf(lElementID);
    if (lElementIdx >= 0) then
    begin
      FIrrigationList.Delete(lElementIdx);
      lSelection := FVisioApp.ActiveWindow.Selection;

      lIrrigationBlock := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[StrToIntDef(lElementID,-1)];
      if (lIrrigationBlock <> nil) then
      begin
        {Remove inflow channel for this irrigation block}
        if(lIrrigationBlock.DiversionChannel <> nil) then
        begin
          lShape := FindChannelShapeByChannelNumber(lIrrigationBlock.DiversionChannel.ChannelNumber);
          if (lShape <> nil) then
          begin
            if (NOT ShapeIsSelected(lSelection, lShape.Name)) then
              lShape.Delete;
          end;
        end;

        {Remove return flow channel for this irrigation block}
        if(lIrrigationBlock.ReturnFlowChannel <> nil) then
        begin
          lShape := FindChannelShapeByChannelNumber(lIrrigationBlock.ReturnFlowChannel.ChannelNumber);
          if (lShape <> nil) then
          begin
            if (NOT ShapeIsSelected(lSelection, lShape.Name)) then
              lShape.Delete;
          end;
        end;
      end;

      {Remove all text annotations for this irrigation block}
      DeleteAllTextShapes(AShape);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.WetlandShapeDeleted( const AShape: IVShape);
const OPNAME = 'TVNVEventHandler.WetlandShapeDeleted';
var
  lShape      : IVShape;
  lSelection  : IVSelection;
  lElementID  : string;
  lReservoirID: string;
  lElementIdx : integer;
  lWetland    : IWetland;
  lInflowTxt  : IVShape;
begin
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then
    begin
      MessageDlg(FAppModules.Language.GetString('VNV.WetlandNoModelSupport'), mtError, [mbOK], 0);
      Exit;
    end;

    lElementID  := UnQuote(AShape.Cells[cNumber].Formula);
    lElementIdx := FWetlandList.IndexOf(lElementID);
    if (lElementIdx >= 0) then
    begin
      FWetlandList.Delete(lElementIdx);
      lSelection := FVisioApp.ActiveWindow.Selection;

      lWetland := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.WetlandList.WetlandByNodeNumber[StrToIntDef(lElementID,-1)];
      if (lWetland <> nil) then
      begin
        {Remove all inflows for this node}
        if(lWetland.ReservoirDetails <> nil) then
        begin
          lReservoirID := IntToStr(lWetland.ReservoirDetails.ReservoirConfigurationData.ReservoirIdentifier);
          if(FReservoirList.IndexOf(lReservoirID) >= 0) then
            FReservoirList.Delete(FReservoirList.IndexOf(lReservoirID));
        end;
        lShape := FindShapeWithParent(mtWRYMInflow, AShape.Name);
        if (lShape <> nil) then
        begin
          lInflowTxt := FindShapeWithParent(mtWRYMText, lShape.Name);
          if (lInflowTxt <> nil) then
          begin
            if (NOT ShapeIsSelected(lSelection, lInflowTxt.Name)) then
              begin
                lInflowTxt.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
                lInflowTxt.Delete;
              end;
          end;
          if (NOT ShapeIsSelected(lSelection, lShape.Name)) then
          begin
            lShape.Cells[cParent].Formula := '""';
            lShape.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
            lShape.Delete;
          end;
        end;

        {Remove inflow channel for this Wetland}
        if(lWetland.InflowChannel <> nil) then
        begin
          lShape := FindChannelShapeByChannelNumber(lWetland.InflowChannel.ChannelNumber);
          if (lShape <> nil) then
          begin
            if (NOT ShapeIsSelected(lSelection, lShape.Name)) then
              begin
                lShape.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
                lShape.Delete;
              end;
          end;
        end;

        {Remove return flow channel for this Wetland}
        if(lWetland.OutflowChannel <> nil) then
        begin
          lShape := FindChannelShapeByChannelNumber(lWetland.OutflowChannel.ChannelNumber);
          if (lShape <> nil) then
          begin
            if (NOT ShapeIsSelected(lSelection, lShape.Name)) then
              lShape.Delete;
          end;
        end;
      end;

      { Remove all penalties for this reservoir}
      lShape := FindShapeWithParent(mtWRYMResPenalty, AShape.Name);
      if (lShape <> nil) then
      begin
        if (NOT ShapeIsSelected(lSelection, lShape.Name)) then
          lShape.Delete;
      end
      else
      begin
        lShape := FindShapeWithParent(mtWRYMResPenaltyExt, AShape.Name);
        if (lShape <> nil) then
        begin
          if (NOT ShapeIsSelected(lSelection, lShape.Name)) then
            lShape.Delete;
        end;
      end;

      {Remove all text annotations for this Wetland}
      DeleteAllShapesWithParent(AShape);
      DeleteAllTextShapes(AShape);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.DemandCentreShapeDeleted( const AShape: IVShape);
const OPNAME = 'TVNVEventHandler.DemandCentreShapeDeleted';
var
  lShape       : IVShape;
  lSelection   : IVSelection;
  lElementID   : string;
  lElementIdx  : integer;
  lDemandCentre: IYMDemandCentre;
  lIdx         : integer;
begin
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then
    begin
      MessageDlg(FAppModules.Language.GetString('VNV.DemandCentreNoModelSupport'), mtError, [mbOK], 0);
      Exit;
    end;

    lElementID  := UnQuote(AShape.Cells[cNumber].Formula);
    lElementIdx := FDemandCentreList.IndexOf(lElementID);
    if (lElementIdx >= 0) then
    begin
      FDemandCentreList.Delete(lElementIdx);
      lSelection := FVisioApp.ActiveWindow.Selection;

      lDemandCentre := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByNodeNumber[StrToIntDef(lElementID,-1)];
      if (lDemandCentre <> nil) then
      begin
        //Remove the Consumptive Channel
        if (lDemandCentre.ConsumptiveUseChannel <> nil) then
        begin
          lShape := FindChannelShapeByChannelNumber(lDemandCentre.ConsumptiveUseChannel.ChannelNumber);
          if lShape <> nil then
            lShape.Delete;
        end;

        //Remove the Reclamation Channel
        if lDemandCentre.ReclaimationChannel <> nil then
        begin
          lShape := FindChannelShapeByChannelNumber(lDemandCentre.ReclaimationChannelNr);
          if lShape <> nil then
            lShape.Delete;
        end;

        //Remove the Return Flow Channel
        for lIdx := 0 to lDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureCount - 1 do
        begin
          lShape := FindChannelShapeByChannelNumber(lDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureByIndex[lIdx].ChannelNr);
          if lShape <> nil then
            lShape.Delete;
        end;

        //Remove the Supply Channel
        for lIdx := 0 to lDemandCentre.SupplyChannelCount - 1 do
        begin
          lShape := FindChannelShapeByChannelNumber(lDemandCentre.SupplyChannelByIndex[lIdx].ChannelNumber);
          if lShape <> nil then
            lShape.Delete;
        end;

      end;
      {Remove all text annotations for this DemandCentre}
      DeleteAllTextShapes(AShape);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TVNVEventHandler.ReservoirPenaltyShapeDeleted (const AShape: IVShape);
const OPNAME = 'TVNVEventHandler.ReservoirPenaltyShapeDeleted';
var
  lElementID  : string;
  lElementIdx : integer;
begin
  try
    if (AShape.CellExists[cNumber, 0] = -1) then
    begin
      lElementID := UnQuote(AShape.Cells[cNumber].Formula);
      lElementIdx := FResPenaltyList.IndexOf(lElementID);
      if (lElementIdx >= 0) then
        FResPenaltyList.Delete(lElementIdx);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.ChannelPenaltyShapeDeleted (const AShape: IVShape);
const OPNAME = 'TVNVEventHandler.ChannelPenaltyShapeDeleted';
var
  lElementID  : string;
  lElementIdx : integer;
begin
  try
    if (AShape.CellExists[cNumber, 0] = -1) then
    begin
      lElementID := UnQuote(AShape.Cells[cNumber].Formula);
      lElementIdx := FChanPenaltyList.IndexOf(lElementID);
      if (lElementIdx >= 0) then
        FChanPenaltyList.Delete(lElementIdx);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.UnQuote (AString : string) : string;
const OPNAME = 'TVNVEventHandler.UnQuote';
begin
  Result := AString;
  try
    if (Copy(AString, 1, 1) = '"') then
      AString := Copy(AString, 2, Length(AString) - 1);
    if (Copy(AString, Length(AString), 1) = '"') then
      AString := Copy(AString, 1, Length(AString) - 1);
    Result := AString;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.UnMM (AString : string) : string;
const OPNAME = 'TVNVEventHandler.UnMM';
var
  lPos : integer;
begin
  Result := AString;
  try
    lPos := Pos('mm', AString);
    if (lPos > 0) then
      AString := Trim(Copy(AString, 1, lPos - 1));
    Result := AString;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.AddConnection (ASourceShape : IVShape;
                                          ATargetShape : IVShape);
const OPNAME = 'TVNVEventHandler.AddConnection';
var
  lIndex         : Integer;
  lConnect       : IVConnect;
  lNodeName      : string;
  lChannelID     : integer;
  lChannel       : IGeneralFlowChannel;
  lOldUpNodeNr   : integer;
  lOldDownNodeNr : integer;
  lNewNodeNr     : integer;
  lMessage       : string;
  lUp            : Boolean;
  lChange        : Boolean;
  lChannelList   : TStringList;
  lDemandCentre  : IYMDemandCentre;
  lYieldModelData: IYieldModelData;
begin
  try
    if (ATargetShape.Master <> nil) AND
       ((Pos(PChar(mtWRYMReservoir), ATargetShape.Name) > 0) OR
        (Pos(PChar(mtWRYMIrrBlock), ATargetShape.Name) > 0) OR
        (Pos(PChar(mtWRYMIrrArea), ATargetShape.Name) > 0) OR
        (Pos(PChar(mtWRYMWetland), ATargetShape.Name) > 0) OR
        (Pos(PChar(mtWRYMDemandCentre), ATargetShape.Name) > 0) OR
        (Pos(PChar(mtWRYMNode), ATargetShape.Name) > 0) OR
        (Pos(PChar(mtWRYMMine), ATargetShape.Name) > 0) OR
        (Pos(PChar(mtWRYMGroundWater), ATargetShape.Name) > 0)) then
    begin
      lNodeName  := UnQuote(ATargetShape.Cells[cName].Formula);

      lChannelID := StrToIntDef(UnQuote(ASourceShape.Cells[cNumber].Formula),0);
      lChannel   := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.
                      ChannelList.ChannelByChannelNumber[lChannelID];
      if (lChannel <> nil) then
      begin

        // jkw can be optimised
        //Can not connect channels to demand centre. Has to do it through the interface.
        if (Pos(PChar(mtWRYMDemandCentre), ATargetShape.Name) > 0) then
        begin
          lChannelList := TStringList.Create;
          lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
          lDemandCentre := lYieldModelData.NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByNodeNumber[StrToIntDef(UnQuote(ATargetShape.Cells[cNumber].Formula), -1)];
          GenerateDemandCentreChannelList(lChannelList, lDemandCentre);
          lIndex := lChannelList.IndexOf(IntToStr(lChannel.ChannelNumber));
          FreeAndNil(lChannelList);
          if (lIndex = -1) then
          begin
            MessageDlg('You can not drag and drop channels to the Demand Centre. To add/remove different channel types, right click on the demand centre, and select the channel type from the menu', mtError, [mbOK], 0);
            exit;
          end;
        end;

        lOldUpNodeNr   := lChannel.UpStreamNodeNumber;
        lOldDownNodeNr := lChannel.DownStreamNodeNumber;
        for lIndex := 1 to ASourceShape.Connects.Count do
        begin
          lConnect := ASourceShape.Connects[lIndex];
          if (lConnect.ToSheet.Name = ATargetShape.Name) then
          begin
            lUp        := lConnect.FromPart = visBegin;
            lNewNodeNr := StrToIntDef(UnQuote(ATargetShape.Text),0);
            lChange    := (lUp AND (lNewNodeNr <> lOldUpNodeNr)) OR
                          ((NOT lUp) AND (lNewNodeNr <> lOldDownNodeNr));
            if (lChange) AND (MayChangeScenario) then
            begin
              if (lUp) then
              begin
                lMessage := FAppModules.Language.GetString('VNV.ChangeUpstreamNode');
                lMessage := Format(lMessage, [lChannelID, lChannel.UpStreamNodeNumber, lNewNodeNr]);
              end
              else
              begin
                lMessage := FAppModules.Language.GetString('VNV.ChangeDownstreamNode');
                lMessage := Format(lMessage, [lChannelID, lChannel.DownStreamNodeNumber, lNewNodeNr]);
              end;
              if (MessageDlg(lMessage, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
              begin
                if (lUp) then
                  lChannel.UpStreamNodeNumber := lNewNodeNr
                else
                  lChannel.DownStreamNodeNumber := lNewNodeNr;
              end;
              RefreshChannel(ASourceShape, lChannelID);
              SnapChannelToNodes(ASourceShape, lChannelID);
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.DeleteConnection (ASourceShape : IVShape; ATargetShape : IVShape);
const OPNAME = 'TVNVEventHandler.DeleteConnection';
var
  lIndex         : Integer;
  lConnect       : IVConnect;
  lNewNodeNr     : integer;
  lMessage       : string;
  lNodeName      : string;
  lChannelID     : integer;
  lChannel       : IGeneralFlowChannel;
  lUpConnect     : Boolean;
  lDownConnect   : Boolean;
  lUpShape       : IVShape;
  lDownShape     : IVShape;
begin
  try
    if (ATargetShape.Master <> nil) AND
       ((Pos(PChar(mtWRYMReservoir), ATargetShape.Name) > 0) OR
        (Pos(PChar(mtWRYMIrrBlock), ATargetShape.Name) > 0) OR
        (Pos(PChar(mtWRYMIrrArea), ATargetShape.Name) > 0) OR
        (Pos(PChar(mtWRYMWetland), ATargetShape.Name) > 0) OR
        (Pos(PChar(mtWRYMDemandCentre), ATargetShape.Name) > 0) OR
        (Pos(PChar(mtWRYMNode), ATargetShape.Name) > 0) OR
        (Pos(PChar(mtWRYMMine), ATargetShape.Name) > 0) OR
        (Pos(PChar(mtWRYMGroundWater), ATargetShape.Name) > 0)) then
    begin
      lNodeName  := UnQuote(ATargetShape.Cells[cName].Formula);
      lChannelID := StrToIntDef(UnQuote(ASourceShape.Cells[cNumber].Formula),0);
      lChannel   := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.
                      ChannelList.ChannelByChannelNumber[lChannelID];
      if (lChannel <> nil) then
      begin
        lUpConnect     := FALSE;
        lDownConnect   := FALSE;
        for lIndex := 1 to ASourceShape.Connects.Count do
        begin
          lConnect := ASourceShape.Connects[lIndex];
          if (lConnect.FromPart = visBegin) then
            lUpConnect := TRUE
          else
          if (lConnect.FromPart = visEnd) then
            lDownConnect := TRUE;
        end;
        if (NOT lUpConnect) then
        begin
          lUpShape := FindChannelUpShape(lChannel);
          if (lUpShape <> nil) AND (MayChangeScenario) then
          begin
            lNewNodeNr := 0;
            lMessage := FAppModules.Language.GetString('VNV.ChangeUpstreamNode');
            lMessage := Format(lMessage, [lChannelID, lChannel.UpStreamNodeNumber, lNewNodeNr]);
            if (MessageDlg(lMessage, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
              lChannel.UpStreamNodeNumber := lNewNodeNr;
            RefreshChannel(ASourceShape, lChannelID);
            SnapChannelToNodes(ASourceShape, lChannelID);
          end;
        end;
        if (NOT lDownConnect) then
        begin
          lDownShape := FindChannelDownShape(lChannel);
          if (lDownShape <> nil) AND (MayChangeScenario) then
          begin
            lNewNodeNr := 0;
            lMessage := FAppModules.Language.GetString('VNV.ChangeDownstreamNode');
            lMessage := Format(lMessage, [lChannelID, lChannel.DownStreamNodeNumber, lNewNodeNr]);
            if (MessageDlg(lMessage, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
              lChannel.DownStreamNodeNumber := lNewNodeNr;
            RefreshChannel(ASourceShape, lChannelID);
            SnapChannelToNodes(ASourceShape, lChannelID);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.PowerPlantShapeDeleted( const AShape: IVShape);
const OPNAME = 'TVNVEventHandler.PowerPlantShapeDeleted';
var
  lShape      : IVShape;
  lSelection  : IVSelection;
  lElementID  : string;
  lChannelID : string;
  lElementIdx : integer;
  lPowerPlant : IPowerPlant;
begin
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then
    begin
      MessageDlg(FAppModules.Language.GetString('VNV.PowerPlantNoModelSupport'), mtError, [mbOK], 0);
      Exit;
    end;

    lElementID  := UnQuote(AShape.Cells[cNumber].Formula);
    lElementIdx := FPowerPlantList.IndexOf(lElementID);
    if (lElementIdx >= 0) then
    begin
      FPowerPlantList.Delete(lElementIdx);
      lSelection := FVisioApp.ActiveWindow.Selection;

      lPowerPlant := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.PowerPlantList.PowerPlantByID[StrToIntDef(lElementID,-1)];
      if (lPowerPlant <> nil) then
      begin
        lChannelID := IntToStr(lPowerPlant.FeatureID);
        {Remove spill channel for this Power Plant}
        if(lPowerPlant.SpillChannel <> nil) then
        begin
          lShape := FindChannelShapeByChannelNumber(lPowerPlant.SpillChannel.ChannelNumber);
          if (lShape <> nil) then
          begin
            if (NOT ShapeIsSelected(lSelection, lShape.Name)) then
              begin
                lShape.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
                lShape.Delete;
              end;
          end;
        end;

        {Remove poer channel for this Power Plant}
        if(lPowerPlant.PowerChannel <> nil) then
        begin
          lShape := FindChannelShapeByChannelNumber(lPowerPlant.PowerChannel.ChannelNumber);
          if (lShape <> nil) then
          begin
            if (NOT ShapeIsSelected(lSelection, lShape.Name)) then
            begin
              DeleteAllTextShapes(AShape);
              lShape.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
              lShape.Delete;
            end;
          end;
        end;

        { Remove all penalties for this reservoir}
        lShape := FindShapeWithParent(mtWRYMResPenalty, AShape.Name);
        if (lShape <> nil) then
        begin
          if (NOT ShapeIsSelected(lSelection, lShape.Name)) then
            lShape.Delete;
        end
        else
        begin
          lShape := FindShapeWithParent(mtWRYMResPenaltyExt, AShape.Name);
          if (lShape <> nil) then
          begin
            if (NOT ShapeIsSelected(lSelection, lShape.Name)) then
              lShape.Delete;
          end;
        end;
        {Remove all text annotations for this Power Plant}
        DeleteAllTextShapes(AShape);
        DeleteAllShapesWithParent(AShape);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ChannelTypeName (AChannel : IGeneralFlowChannel) : string;
const OPNAME = 'TVNVEventHandler.ChannelTypeName';
begin
  Result := '';
  try
    case AChannel.ChannelType of
      2	 : Result := FAppModules.Language.GetString('VNV.MasterControl');
      3	 :
        case AChannel.ChannelSubType of
          1 : Result := FAppModules.Language.GetString('VNV.PowerChannel');
          2 : Result := FAppModules.Language.GetString('VNV.PowerSpill');
        else
        end;
      4	 :
        case AChannel.ChannelSubType of
          1 : Result := FAppModules.Language.GetString('VNV.IrrigationDiversion');
          2 : Result := FAppModules.Language.GetString('VNV.IrrigationConsumption');
          3 : Result := FAppModules.Language.GetString('VNV.IrrigationReturnFlow');
        else
        end;
      5	 : Result := FAppModules.Language.GetString('VNV.Diversion');
      6	 : Result := FAppModules.Language.GetString('VNV.MinimumFlow');
      7	 : Result := FAppModules.Language.GetString('VNV.Loss');
      8	 : Result := FAppModules.Language.GetString('VNV.MinMax');
      9	 : Result := FAppModules.Language.GetString('VNV.Pumping');
      10 : Result := FAppModules.Language.GetString('VNV.SpecifiedInflow');
      11 : Result := FAppModules.Language.GetString('VNV.SpecifiedDemand');
      12 : Result := FAppModules.Language.GetString('VNV.GeneralFlow');
      14 : Result := FAppModules.Language.GetString('VNV.IrrigationDiversion');
      15 : Result := FAppModules.Language.GetString('VNV.IrrigationReturnFlow');
      20 : Result := FAppModules.Language.GetString('VNV.DemandCentreReturnFlowChannel');
      21 : Result := FAppModules.Language.GetString('VNV.DemandCentreReclamationChannel');
    else
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.ReservoirPenaltyShapeAdded(const AShape: IVShape);
const OPNAME = 'TVNVEventHandler.ReservoirPenaltyShapeAdded';
var
  lElementID  : integer;
  LShape      : IVShape;
begin
  try
    if (FBufferResPenaltyList.Count > 0) then
      { Upgraded reservoir penalty }
      FBufferResPenaltyList.Delete(0)
    else
    begin
      { Upgrade reservoir penalty }
      UpgradeReservoirPenalty(AShape);

      if (AShape.CellExists[cNumber, 0] <> 0) AND
         (UnQuote(AShape.Cells[cNumber].Formula) <> '') then
      begin
        { Copied reservoir penalty }
        lElementID := StrToIntDef(UnQuote(AShape.Cells[cNumber].Formula),0);
      end
      else
      begin
        { New reservoir penalty - from stencil }
        lElementID := ShowReservoirPenaltiesDialog;
        if (lElementID <> -1) then
        begin
          AShape.Cells[cNumber].Formula  := '"' + IntToStr(lElementID) + '"';
          AShape.Cells[cVersion].Formula := '"' + CThisVersion + '"';
        end;
      end;
      if (lElementID = -1) then
        AShape.Delete
      else
      begin
        LShape := AShape;
        RefreshReservoirPenalty(LShape, lElementID);
        if(LShape <> nil) then
          FResPenaltyList.Add(IntToStr(lElementID));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ShowReservoirPenaltiesDialog : integer;
const OPNAME = 'TVNVEventHandler.ShowReservoirPenaltiesDialog';
var
  LReservoirsDlg : TVNVReservoirPenaltyDialog;
  lResTemp       : string;
  lPenTemp       : string;
  lPenaltyNodes  : TStringList;
begin
  Result := -1;
  try
    lPenaltyNodes  := TStringList.Create;
  	LReservoirsDlg := TVNVReservoirPenaltyDialog.CreateWithoutDFM(nil, FAppModules);
    LReservoirsDlg.LanguageHasChanged;
    try
      lPenaltyNodes.AddStrings(FReservoirList);
      lPenaltyNodes.AddStrings(FWetlandList);
      lResTemp := lPenaltyNodes.CommaText;
      lPenTemp := FResPenaltyList.CommaText;
	    Result   := LReservoirsDlg.ShowReservoirPenaltiesDialog(lResTemp, lPenTemp, FResPenaltyDuplicate);
    finally
      lPenaltyNodes.Free;
      LReservoirsDlg.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVEventHandler.RefreshReservoirPenalty (var AShape     : IVShape;
                                                    AElementID : integer);
const OPNAME = 'TVNVEventHandler.RefreshReservoirPenalty';
var
  lReservoir       : IReservoirData;
  lResShape        : IVShape;
  lResPenaltyNr    : integer;
  lResPenalty      : IReservoirPenalty;
  lYieldModelData  : IYieldModelData;
  lDone            : boolean;
  lZoneCount       : integer;
  lColCount        : integer;
  lZoneIdx         : integer;
  lShapeIdx        : integer;
  lRowIdx          : integer;
  lColIdx          : integer;
  lResPenaltyList  : IReservoirPenaltyList;
  lSubShape        : IVShape;
  lRectShape       : IVShape;
  lLabelShape      : IVShape;
  lElevationShape  : IVShape;
  lVolumeShape     : IVShape;
  lAreaShape       : IVShape;
  lTextShape       : IVShape;
  lHorLineShape    : IVShape;
  lVerLineShape1   : IVShape;
  lVerLineShape2   : IVShape;
  lVerLineShape3   : IVShape;
  lVerLineShape4   : IVShape;
  lSubType         : string;
  lDefaultWidth    : integer;
  lTopY            : integer;
  lSelection       : IVSelection;
  lHorisontalLines : array of IVShape;
  lTextValues      : array of Array of IVShape;
  lValue           : double;
  lTextStr         : string;
  lTotalHeight     : integer;
  lRectHeight      : integer;
  lColWidths       : array of integer;
  lColPinX         : array of integer;
  lTotalWidth      : integer;
  lWidth           : integer;
  lPenaltyProp     : TAbstractFieldProperty;
  lLevelProp       : TAbstractFieldProperty;
begin
  try
    //Do not refresh Reservoir Penalty that have been made permanent
    if (AShape.CellExists[cPermanent, 0] <> 0)  and (AShape.Cells[cPermanent].Formula = '"TRUE"')then Exit;
    if(AShape = nil) or (AElementID = 0) or (AElementID = NullInteger) then Exit;
    lDone     := FALSE;
    lColCount := 3;
    if(Pos(PChar(mtWRYMResPenaltyExt), AShape.Name) > 0) then
      lColCount := 5;

    lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
    lResPenaltyList := lYieldModelData.NetworkElementData.ReservoirPenaltyStructureList;
    lReservoir      := lYieldModelData.NetworkElementData.ReservoirList.ReservoirByIdentifier[AElementID];
    if (lReservoir <> nil) then
    begin
      if (lReservoir.ReservoirPenaltyStructureData = nil) then
        ShowMessage('Channel penalty not yet specified')
      else
      begin
        lResPenaltyNr := lReservoir.ReservoirPenaltyStructureData.ReservoirPenaltyID;
        lResPenalty   := lResPenaltyList.ReservoirPenaltyByIdentifier[lResPenaltyNr];
        lResShape     := nil;
        if(lReservoir.ReservoirConfigurationData.NodeType = ntWetlandNode) then
          lResShape     := FindShapeWithPropertyValue(mtWRYMWetland, cNumber, IntToStr(AElementID))
        else
          lResShape     := FindShapeWithPropertyValue(mtWRYMReservoir, cNumber, IntToStr(AElementID));
        if (lResPenalty <> nil) AND (lResShape <> nil) then
        begin
          FVisioApp.ActiveWindow.Windows.ItemFromID[visWinIDCustProp].Visible := FALSE;
          lZoneCount := lResPenaltyList.PenaltyZoneCount;
          SetLength(lHorisontalLines, lZoneCount);
          SetLength(lTextValues, lZoneCount, lColCount);
          SetLength(lColWidths, lColCount);
          SetLength(lColPinX, lColCount);
          for lColIdx := 0 to lColCount - 1 do
          begin
            lColWidths[lColIdx] := 2;
            lColPinX[lColIdx]   := (lColIdx * 10) + 1;
          end;
          for lZoneIdx := 0 to lZoneCount - 1 do
          begin
            lHorisontalLines[lZoneIdx] := nil;
            for lColIdx := 0 to lColCount - 1 do
              lTextValues[lZoneIdx, lColIdx] := nil;
          end;

          lSelection      := FVisioApp.ActiveWindow.Selection;
          lHorLineShape   := nil;
          lTextShape      := nil;
          lVerLineShape1  := nil;
          lVerLineShape2  := nil;
          lVerLineShape3  := nil;
          lVerLineShape4  := nil;
          lLabelShape     := nil;
          lElevationShape := nil;
          lVolumeShape    := nil;
          lAreaShape      := nil;
          for lShapeIdx := 1 to AShape.Shapes.Count do
          begin
            lSubShape := AShape.Shapes.Item[lShapeIdx];
            lSubType  := UnQuote(lSubShape.Cells[cShapeType].Formula);
            if (lSubType = 'Label') then
              lLabelShape := lSubShape
            else if (lSubType = 'Elevation') then
              lElevationShape := lSubShape
            else if (lSubType = 'Volume') then
              lVolumeShape := lSubShape
            else if (lSubType = 'Area') then
              lAreaShape := lSubShape
            else if (lSubType = 'Rectangle') then
              lRectShape := lSubShape
            else if (lSubType = 'HorisontalLine') then
            begin
              lZoneIdx := StrToIntDef(UnQuote(lSubShape.Cells[cShapeIndex].Formula),0);
              if (lZoneIdx < lZoneCount) then
              begin
                if (lHorLineShape = nil) then
                  lHorLineShape := lSubShape;
                lHorisontalLines[lZoneIdx] := lSubShape;
              end
              else
                lSubShape.Delete;
            end
            else if (lSubType = 'Text') then
            begin
              lRowIdx := StrToIntDef(UnQuote(lSubShape.Cells[cRowIndex].Formula),0);
              lColIdx := StrToIntDef(UnQuote(lSubShape.Cells[cColIndex].Formula),0);
              if (lRowIdx < lZoneCount) AND (lColIdx < lColCount) then
              begin
                if (lTextShape = nil) then
                  lTextShape := lSubShape;
                lTextValues[lRowIdx, lColIdx] := lSubShape;
              end
              else
                lSubShape.Delete;
            end
            else if (lSubType = 'VerticalLine1') then
              lVerLineShape1 := lSubShape
            else if (lSubType = 'VerticalLine2') then
              lVerLineShape2 := lSubShape
            else if (lSubType = 'VerticalLine3') then
              lVerLineShape3 := lSubShape
            else if (lSubType = 'VerticalLine4') then
              lVerLineShape4 := lSubShape;
          end;

          lDone         := TRUE;
          lDefaultWidth := 30;
          lTotalHeight  := 4 * (lZoneCount + 1);
          lRectHeight   := 4 * lZoneCount;

          AShape.Cells[cParent].Formula        := '"' + lResShape.Name          + '"';
          AShape.Cells[cHeight].Formula        := '"' + IntToStr(lTotalHeight)  + ' mm"';
          if (lDone) then
          begin
            if (lRectShape <> nil) then
            begin
              lRectShape.Cells[cHeight].Formula := '"' + IntToStr(lRectHeight)     + ' mm"';
              lRectShape.Cells[cPinY].Formula   := '"' + FloatToStr(lRectHeight/2) + ' mm"';
            end
            else
              lDone := FALSE;
          end;
          if (lDone) then
          begin
            if (lElevationShape <> nil) then
              lElevationShape.Text := FAppModules.Language.GetString('TField.Elevation');
            if (lVolumeShape <> nil) then
              lVolumeShape.Text    := FAppModules.Language.GetString('TField.Volume');
            if (lAreaShape <> nil) then
              lAreaShape.Text      := FAppModules.Language.GetString('TField.Area');

            if (lLabelShape <> nil) then
            begin
              lLabelShape.Text := IntToStr(AElementID) + ' - [' + IntToStr(lResPenaltyNr) + ']';
              lLabelShape.Cells[cPinY].Formula  := '"' + IntToStr(lRectHeight + 2) + ' mm"';
              lLabelShape.Cells[cPinX].Formula  := '"7.5 mm"';
              lLabelShape.Cells[cWidth].Formula := '"15 mm"';
            end
            else
              lDone := FALSE;
          end;
          if (lDone) then
          begin
            if (lHorLineShape <> nil) then
            begin
              for lZoneIdx := 1 to lZoneCount - 1 do
              begin
                lTopY := lZoneIdx * 4;
                lSubShape := lHorisontalLines[lZoneIdx];
                if (lSubShape = nil) then
                begin
                  lSubShape := lHorLineShape.Duplicate;
                  lSubShape.Cells[cShapeIndex].Formula := '"' + IntToStr(lZoneIdx) + '"';
                  lHorisontalLines[lZoneIdx] := lSubShape;
                end;
                lSubShape.Cells[cBeginY].Formula := '"' + IntToStr(lTopY)   + ' mm"';
                lSubShape.Cells[cEndY].Formula   := '"' + IntToStr(lTopY)   + ' mm"';
                lSubShape.Cells[cBeginX].Formula := '"' + IntToStr(0)       + ' mm"';
                lSubShape.Cells[cEndX].Formula   := '"' + IntToStr(lDefaultWidth) + ' mm"';
              end;
            end
            else
              lDone := FALSE;
          end;
          if (lDone) then
          begin
            if (lTextShape <> nil) then
            begin
              lPenaltyProp := FAppModules.FieldProperties.FieldProperty('ReservoirPenalty');
              //lLevelProp   := FAppModules.FieldProperties.FieldProperty('ReservoirLev');
              for lRowIdx := 0 to lZoneCount - 1 do
              begin
                lTopY := (lZoneCount - lRowIdx) * 4;
                for lColIdx := 0 to lColCount - 1 do
                begin
                  lSubShape := lTextValues[lRowIdx, lColIdx];
                  if (lSubShape = nil) then
                  begin
                    lSubShape := lTextShape.Duplicate;
                    lSubShape.Cells[cRowIndex].Formula := '"' + IntToStr(lRowIdx) + '"';
                    lSubShape.Cells[cColIndex].Formula := '"' + IntToStr(lColIdx) + '"';
                    lTextValues[lRowIdx, lColIdx] := lSubShape;
                  end;
                  if (lColIdx = 0) then
                    lSubShape.Characters.ParaProps[visHorzAlign] := visAlignLeft
                  else
                    lSubShape.Characters.ParaProps[visHorzAlign] := visAlignRight;
                  lSubShape.Cells[cPinX].Formula := '"' + IntToStr((lColIdx * 10) + 5) + ' mm"';
                  lSubShape.Cells[cPinY].Formula := '"' + IntToStr(lTopY - 2) + ' mm"';
                  case lColIdx of
                    0 :
                      begin
                        lTextStr := lResPenaltyList.ReservoirPenaltyZoneByIndex[lRowIdx].ZoneName;
                      end;
                    1 :
                      begin
                        lValue   := lResPenalty.ReservoirPenaltyValueByIndex[lRowIdx+1];
                        lTextStr := Format(lPenaltyProp.FormatStringGrid, [lValue]);
                      end;
                    2 :
                      begin
                        lLevelProp   := FAppModules.FieldProperties.FieldProperty('SurfaceElevation');
                        if (lRowIdx = 0) then
                          lValue := lReservoir.ReservoirZoneElevationsData.FullSupplyLevel.Elevation
                        else if (lRowIdx = lZoneCount - 2) then
                          lValue := lReservoir.ReservoirZoneElevationsData.DeadStorageLevel.Elevation
                        else if (lRowIdx = lZoneCount - 1) then
                          lValue := lReservoir.ReservoirZoneElevationsData.BottomOfReservoir.Elevation
                        else
                        begin
                          lReservoir.ReservoirZoneElevationsData.DrawDownLevelByIndex[lRowIdx-1].CalculateAvarageElevation;
                          lValue := lReservoir.ReservoirZoneElevationsData.DrawDownLevelByIndex[lRowIdx-1].AverageElevations;
                        end;
                        lTextStr := Format(lLevelProp.FormatStringGrid, [lValue]);
                      end;
                    3 :
                      begin
                        lLevelProp   := FAppModules.FieldProperties.FieldProperty('Volume');
                        if (lRowIdx = 0) then
                          lValue := lReservoir.GetReservoirVolumeByElevation(lReservoir.ReservoirZoneElevationsData.FullSupplyLevel.Elevation)
                        else if (lRowIdx = lZoneCount - 2) then
                          lValue := lReservoir.GetReservoirVolumeByElevation(lReservoir.ReservoirZoneElevationsData.DeadStorageLevel.Elevation)
                        else if (lRowIdx = lZoneCount - 1) then
                          lValue := lReservoir.GetReservoirVolumeByElevation(lReservoir.ReservoirZoneElevationsData.BottomOfReservoir.Elevation)
                        else
                        begin
                          lReservoir.ReservoirZoneElevationsData.DrawDownLevelByIndex[lRowIdx-1].CalculateAvarageElevation;
                          lValue := lReservoir.GetReservoirVolumeByElevation(lReservoir.ReservoirZoneElevationsData.DrawDownLevelByIndex[lRowIdx-1].AverageElevations);
                        end;
                        lTextStr := Format(lLevelProp.FormatStringGrid, [lValue]);
                      end;
                    4 :
                      begin
                        lLevelProp   := FAppModules.FieldProperties.FieldProperty('Area');
                        if (lRowIdx = 0) then
                          lValue := lReservoir.GetReservoirSurfaceAreaByElevation(lReservoir.ReservoirZoneElevationsData.FullSupplyLevel.Elevation)
                        else if (lRowIdx = lZoneCount - 2) then
                          lValue := lReservoir.GetReservoirSurfaceAreaByElevation(lReservoir.ReservoirZoneElevationsData.DeadStorageLevel.Elevation)
                        else if (lRowIdx = lZoneCount - 1) then
                          lValue := lReservoir.GetReservoirSurfaceAreaByElevation(lReservoir.ReservoirZoneElevationsData.BottomOfReservoir.Elevation)
                        else
                        begin
                          lReservoir.ReservoirZoneElevationsData.DrawDownLevelByIndex[lRowIdx-1].CalculateAvarageElevation;
                          lValue := lReservoir.GetReservoirSurfaceAreaByElevation(lReservoir.ReservoirZoneElevationsData.DrawDownLevelByIndex[lRowIdx-1].AverageElevations);
                        end;
                        lTextStr := Format(lLevelProp.FormatStringGrid, [lValue]);
                      end;

                  else
                  end;
                  lSubShape.Text := lTextStr;
                  if (Length(lTextStr) > lColWidths[lColIdx]) then
                    lColWidths[lColIdx] := Length(lTextStr);
                end;
              end;
              lTotalWidth := 0;
              for lColIdx := 0 to lColCount - 1 do
              begin
                if (lColIdx = 0) then
                  lColWidths[lColIdx] := lColWidths[lColIdx] * 2
                else
                  lColWidths[lColIdx] := Ceil(lColWidths[lColIdx] * 1.5);
                lColPinX[lColIdx]   := Ceil(lTotalWidth + lColWidths[lColIdx] / 2.0);
                lTotalWidth         := lTotalWidth + lColWidths[lColIdx];
              end;
              if (lTotalWidth > 0) then
              begin
                AShape.Cells[cWidth].Formula     := '"' + IntToStr(lTotalWidth) + ' mm"';
                lRectShape.Cells[cWidth].Formula := '"' + IntToStr(lTotalWidth) + ' mm"';
                if (lVerLineShape1 <> nil) then
                begin
                  lWidth := lColWidths[0];
                  lVerLineShape1.Cells[cBeginX].Formula := '"' + IntToStr(lWidth) + ' mm"';
                  lVerLineShape1.Cells[cEndX].Formula   := '"' + IntToStr(lWidth) + ' mm"';
                  lVerLineShape1.Cells[cBeginY].Formula := '"' + IntToStr(lRectHeight) + ' mm"';
                  lVerLineShape1.Cells[cEndY].Formula   := '"' + IntToStr(0)     + ' mm"';
                end;
                if (lVerLineShape2 <> nil) then
                begin
                  lWidth := lColWidths[0]+ lColWidths[1];
                  lVerLineShape2.Cells[cBeginX].Formula := '"' + IntToStr(lWidth) + ' mm"';
                  lVerLineShape2.Cells[cEndX].Formula   := '"' + IntToStr(lWidth) + ' mm"';
                  lVerLineShape2.Cells[cBeginY].Formula := '"' + IntToStr(lRectHeight) + ' mm"';
                  lVerLineShape2.Cells[cEndY].Formula   := '"' + IntToStr(0)     + ' mm"';
                end;
                if (lVerLineShape3 <> nil) then
                begin
                  lWidth := lColWidths[0]+ lColWidths[1]+ lColWidths[2];
                  lVerLineShape3.Cells[cBeginX].Formula := '"' + IntToStr(lWidth) + ' mm"';
                  lVerLineShape3.Cells[cEndX].Formula   := '"' + IntToStr(lWidth) + ' mm"';
                  lVerLineShape3.Cells[cBeginY].Formula := '"' + IntToStr(lRectHeight) + ' mm"';
                  lVerLineShape3.Cells[cEndY].Formula   := '"' + IntToStr(0)     + ' mm"';
                end;
                if (lVerLineShape4 <> nil) then
                begin
                  lWidth := lColWidths[0]+ lColWidths[1]+ lColWidths[2]+ lColWidths[3];
                  lVerLineShape4.Cells[cBeginX].Formula := '"' + IntToStr(lWidth) + ' mm"';
                  lVerLineShape4.Cells[cEndX].Formula   := '"' + IntToStr(lWidth) + ' mm"';
                  lVerLineShape4.Cells[cBeginY].Formula := '"' + IntToStr(lRectHeight) + ' mm"';
                  lVerLineShape4.Cells[cEndY].Formula   := '"' + IntToStr(0)     + ' mm"';
                end;
                for lRowIdx := 0 to lZoneCount - 1 do
                begin
                  for lColIdx := 0 to lColCount - 1 do
                  begin
                    lSubShape := lTextValues[lRowIdx, lColIdx];
                    lSubShape.Cells[cWidth].Formula := '"' + IntToStr(lColWidths[lColIdx]) + ' mm"';
                    lSubShape.Cells[cPinX].Formula  := '"' + IntToStr(lColPinX[lColIdx])   + ' mm"';
                  end;
                end;
                for lZoneIdx := 1 to lZoneCount - 1 do
                begin
                  lSubShape := lHorisontalLines[lZoneIdx];
                  lSubShape.Cells[cEndX].Formula := '"' + IntToStr(lTotalWidth) + ' mm"';
                end;
              end;
            end;
          end;
        end;
      end;

    end;
    if (NOT lDone) then
    begin
      AShape.Delete;
      AShape := nil;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.RefreshChannelPenalty (var AShape     : IVShape;
                                                  AElementID : integer);
const OPNAME = 'TVNVEventHandler.RefreshChannelPenalty';
var
  lChannel         : IGeneralFlowChannel;
  lChanShape       : IVShape;
  lChanPenaltyNr   : integer;
  lChanPenalty     : IChannelPenalty;
  lYieldModelData  : IYieldModelData;
  lDone            : boolean;
  lArcCount        : integer;
  lColCount        : integer;
  lArcIdx          : integer;
  lShapeIdx        : integer;
  lRowIdx          : integer;
  lColIdx          : integer;
  lChanPenaltyList : IChannelPenaltyList;
  lSubShape        : IVShape;
  lRectShape       : IVShape;
  lLabelShape      : IVShape;
  lLabel1Shape     : IVShape;
  lTextShape       : IVShape;
  lHorLineShape    : IVShape;
  lVerLineShape    : IVShape;
  lSubType         : string;
  lDefaultWidth    : integer;
  lTopY            : integer;
  lSelection       : IVSelection;
  lHorisontalLines : array of IVShape;
  lTextValues      : array of array of IVShape;
  lValue           : double;
  lTextStr         : string;
  lTotalHeight     : integer;
  lRectHeight      : integer;
  lColWidths       : array of integer;
  lColPinX         : array of integer;
  lTotalWidth      : integer;
  lWidth           : integer;
  lPenaltyProp     : TAbstractFieldProperty;
  lColour          : integer;
  lColourChanged   : boolean;
begin
  try
    //Do not refresh Channel Penalty that have been made permanent
    if (AShape.CellExists[cPermanent, 0] <> 0)  and (AShape.Cells[cPermanent].Formula = '"TRUE"')then Exit;
    if(AShape = nil) or (AElementID = 0) or (AElementID = NullInteger) then Exit;
    lDone     := FALSE;
    lColCount := 2;
    lYieldModelData  := FAppModules.Model.ModelData as IYieldModelData;
    lChanPenaltyList := lYieldModelData.NetworkElementData.ChannelPenaltyList;
    lChannel         := lYieldModelData.NetworkElementData.ChannelList.ChannelByChannelNumber[AElementID];
    if (lChannel <> nil) then
    begin
      if (AShape.CellExists[cColourChanged, 0] = 0) then
      begin
        AShape.AddNamedRow(visSectionProp, 'ColourChanged', visTagDefault);
        AShape.Cells[cColourChanged].Formula    := '"N"';
        AShape.Cells[cColourChangedInv].Formula := '"TRUE"';
      end;
      if (lChannel.ChannelPenalty = nil) then
        ShowMessage('Channel penalty not yet specified')
      else
      begin
        lChanPenaltyNr := lChannel.ChannelPenaltyNumber;
        lChanPenalty   := lChannel.ChannelPenalty;
        lChanShape     := FindShapeWithPropertyValue(mtWRYMChannel, cNumber, IntToStr(AElementID));
        if (lChanPenalty <> nil) AND (lChanShape <> nil) then
        begin
          FVisioApp.ActiveWindow.Windows.ItemFromID[visWinIDCustProp].Visible := FALSE;
          lArcCount := lChanPenalty.ChannelPenaltyArcCount;
          SetLength(lHorisontalLines, lArcCount);
          SetLength(lTextValues, lArcCount, lColCount);
          SetLength(lColWidths, lColCount);
          SetLength(lColPinX, lColCount);
          for lColIdx := 0 to lColCount - 1 do
          begin
            lColWidths[lColIdx] := 2;
            lColPinX[lColIdx]   := (lColIdx * 10) + 1;
          end;
          for lArcIdx := 0 to lArcCount - 1 do
          begin
            lHorisontalLines[lArcIdx] := nil;
            for lColIdx := 0 to lColCount - 1 do
              lTextValues[lArcIdx, lColIdx] := nil;
          end;

          lColourChanged := Trim(UnQuote(AShape.Cells[cColourChanged].Formula)) = 'Y';
          if (lChannel.IFRFeature <> nil) then
            lColour := visDarkGreen
          else
          begin
            case lChannel.ChannelType of
              2 : lColour := visDarkYellow;
              4 : lColour := visDarkGreen;
              8	, 11 : lColour := visRed;
              5, 6, 7, 9, 10 : lColour := visPurple;
            else
              lColour := visBlack;
            end;
          end;
          lSelection := FVisioApp.ActiveWindow.Selection;
          lHorLineShape := nil;
          lTextShape    := nil;
          lLabel1Shape  := nil;
          lLabelShape   := nil;
          for lShapeIdx := 1 to AShape.Shapes.Count do
          begin
            lSubShape := AShape.Shapes.Item[lShapeIdx];
            lSubType  := UnQuote(lSubShape.Cells[cShapeType].Formula);
            if (lSubType = 'Label') then
              lLabelShape := lSubShape
            else if (lSubType = 'Label1') then
              lLabel1Shape := lSubShape
            else if (lSubType = 'Rectangle') then
              lRectShape := lSubShape
            else if (lSubType = 'HorisontalLine') then
            begin
              lArcIdx := StrToIntDef(UnQuote(lSubShape.Cells[cShapeIndex].Formula),0);
              if (lArcIdx = 1) then
                lHorLineShape := lSubShape;
              if (lArcIdx < lArcCount) then
              begin
                lHorisontalLines[lArcIdx] := lSubShape;
              end
              else if (lArcIdx <> 1) then
                lSubShape.Delete;
            end
            else if (lSubType = 'Text') then
            begin
              lRowIdx := StrToIntDef(UnQuote(lSubShape.Cells[cRowIndex].Formula),0);
              lColIdx := StrToIntDef(UnQuote(lSubShape.Cells[cColIndex].Formula),0);
              if (lRowIdx < lArcCount) AND (lColIdx < lColCount) then
              begin
                if (lTextShape = nil) then
                  lTextShape := lSubShape;
                lTextValues[lRowIdx, lColIdx] := lSubShape;
              end
              else
                lSubShape.Text := 'Undefined';
                //lSubShape.Delete;
            end
            else if (lSubType = 'VerticalLine1') then
              lVerLineShape := lSubShape;
          end;

          lDone         := TRUE;
          lDefaultWidth := 30;
          lTotalHeight  := 4 * (lArcCount + 1);
          lRectHeight   := 4 * lArcCount;
          AShape.Cells[cPenaltyNumber].Formula := '"' + IntToStr(lChanPenaltyNr) + '"';
          AShape.Cells[cParent].Formula        := '"' + lChanShape.Name          + '"';
          AShape.Cells[cHeight].Formula        := '"' + IntToStr(lTotalHeight)   + ' mm"';
          if (lDone) then
          begin
            if (lRectShape <> nil) then
            begin
              lRectShape.Cells[cHeight].Formula := '"' + IntToStr(lRectHeight)     + ' mm"';
              lRectShape.Cells[cPinY].Formula   := '"' + FloatToStr(lRectHeight/2) + ' mm"';
              if (NOT lColourChanged) then
                lRectShape.CellsSRC[visSectionObject, visRowLine, visLineColor].FormulaU := IntToStr(lColour);
            end
            else
              lDone := FALSE;
          end;
          if (lDone) then
          begin
            if (lLabelShape <> nil) then
            begin
              lLabelShape.Text := IntToStr(AElementID) + ' - [' + IntToStr(lChanPenaltyNr) + ']';
              lLabelShape.Cells[cPinY].Formula  := '"' + IntToStr(lRectHeight + 2) + ' mm"';
              lLabelShape.Cells[cPinX].Formula  := '"7.5 mm"';
              lLabelShape.Cells[cWidth].Formula := '"15 mm"';
              if (NOT lColourChanged) then
                lLabelShape.CellsSRC[visSectionCharacter, visRowCharacter, visCharacterColor].FormulaU := IntToStr(lColour);
            end
            else
              lDone := FALSE;
            if (lLabel1Shape <> nil) then
            begin
              lLabel1Shape.Text := '';
              if(lChannel.LossFeature <> nil) then
              begin
                lLabel1Shape.Text := MaxLossFactor(lChannel);
              end;
              if(lChannel <> nil) and (lChannel.DiversionFeature <> nil) and (lChannel.DiversionFeature.DiversionType in [2,4]) then
              begin
                lLabel1Shape.Text := MaxDiversionFlow(lChannel);
              end;

              if (NOT lColourChanged) then
                lLabel1Shape.CellsSRC[visSectionCharacter, visRowCharacter, visCharacterColor].FormulaU := IntToStr(lColour);
            end;
            //else
            //  lDone := FALSE;
          end;
          if (lDone) then
          begin
            if (lHorLineShape <> nil) then
            begin
              if (NOT lColourChanged) then
                lHorLineShape.CellsSRC[visSectionObject, visRowLine, visLineColor].FormulaU := IntToStr(lColour);
              for lArcIdx := 1 to lArcCount - 1 do
              begin
                lTopY := lArcIdx * 4;
                lSubShape := lHorisontalLines[lArcIdx];
                if (lSubShape = nil) then
                begin
                  lSubShape := lHorLineShape.Duplicate;
                  lSubShape.Cells[cShapeIndex].Formula := '"' + IntToStr(lArcIdx) + '"';
                  lHorisontalLines[lArcIdx] := lSubShape;
                end;
                lSubShape.Cells[cBeginY].Formula := '"' + IntToStr(lTopY)   + ' mm"';
                lSubShape.Cells[cEndY].Formula   := '"' + IntToStr(lTopY)   + ' mm"';
                lSubShape.Cells[cBeginX].Formula := '"' + IntToStr(0)       + ' mm"';
                lSubShape.Cells[cEndX].Formula   := '"' + IntToStr(lDefaultWidth) + ' mm"';
                if (NOT lColourChanged) then
                  lSubShape.CellsSRC[visSectionObject, visRowLine, visLineColor].FormulaU := IntToStr(lColour);
              end;
            end
            else
              lDone := FALSE;
          end;
          if (lDone) then
          begin
            if (lTextShape <> nil) then
            begin
              lPenaltyProp := FAppModules.FieldProperties.FieldProperty('Penalty');
              for lRowIdx := 0 to lArcCount - 1 do
              begin
                lTopY := (lArcCount - lRowIdx) * 4;
                for lColIdx := 0 to lColCount - 1 do
                begin
                  lSubShape := lTextValues[lRowIdx, lColIdx];
                  if (lSubShape = nil) then
                  begin
                    lSubShape := lTextShape.Duplicate;
                    lSubShape.Cells[cRowIndex].Formula := '"' + IntToStr(lRowIdx) + '"';
                    lSubShape.Cells[cColIndex].Formula := '"' + IntToStr(lColIdx) + '"';
                    lTextValues[lRowIdx, lColIdx] := lSubShape;
                  end;
                  if (lColIdx = 0) then
                    lSubShape.Characters.ParaProps[visHorzAlign] := visAlignLeft
                  else
                    lSubShape.Characters.ParaProps[visHorzAlign] := visAlignRight;
                  lSubShape.Cells[cPinX].Formula := '"' + IntToStr((lColIdx * 10) + 5) + ' mm"';
                  lSubShape.Cells[cPinY].Formula := '"' + IntToStr(lTopY - 2) + ' mm"';
                  if (NOT lColourChanged) then
                    lSubShape.CellsSRC[visSectionCharacter, visRowCharacter, visCharacterColor].FormulaU := IntToStr(lColour);
                  case lColIdx of
                    0 : lTextStr := ChannelPenaltyStr(lChannel, FMonthIndex, FLoadCase, lRowIdx + 1);
                    1 : begin
                          lValue   := lChanPenalty.ChannelPenaltyValueByIndex[lRowIdx+1];
                          lTextStr := Format(lPenaltyProp.FormatStringGrid, [lValue]);
                        end;
                  else
                  end;
                  lSubShape.Text := lTextStr;
                  if (Length(lTextStr) > lColWidths[lColIdx]) then
                    lColWidths[lColIdx] := Length(lTextStr);
                end;
              end;
              lTotalWidth := 0;
              for lColIdx := 0 to lColCount - 1 do
              begin
                if (lColIdx = 0) then
                  lColWidths[lColIdx] := Ceil(lColWidths[lColIdx] * 1.6)
                else
                  lColWidths[lColIdx] := Ceil(lColWidths[lColIdx] * 1.3);
                lColPinX[lColIdx]   := Ceil(lTotalWidth + lColWidths[lColIdx] / 2.0);
                lTotalWidth         := lTotalWidth + lColWidths[lColIdx];
              end;
              if (lTotalWidth > 0) then
              begin
                AShape.Cells[cWidth].Formula     := '"' + IntToStr(lTotalWidth) + ' mm"';
                lRectShape.Cells[cWidth].Formula := '"' + IntToStr(lTotalWidth) + ' mm"';
                lRectShape.Cells[cPinX].Formula  := '"' + FloatToStr(lTotalWidth/2) + ' mm"';
                if (lVerLineShape <> nil) then
                begin
                  lWidth := lColWidths[0];
                  lVerLineShape.Cells[cBeginX].Formula := '"' + IntToStr(lWidth) + ' mm"';
                  lVerLineShape.Cells[cEndX].Formula   := '"' + IntToStr(lWidth) + ' mm"';
                  lVerLineShape.Cells[cBeginY].Formula := '"' + IntToStr(lRectHeight) + ' mm"';
                  lVerLineShape.Cells[cEndY].Formula   := '"' + IntToStr(0)     + ' mm"';
                  if (NOT lColourChanged) then
                    lVerLineShape.CellsSRC[visSectionObject, visRowLine, visLineColor].FormulaU := IntToStr(lColour);
                end;
                for lRowIdx := 0 to lArcCount - 1 do
                begin
                  for lColIdx := 0 to lColCount - 1 do
                  begin
                    lSubShape := lTextValues[lRowIdx, lColIdx];
                    lSubShape.Cells[cWidth].Formula := '"' + IntToStr(lColWidths[lColIdx]) + ' mm"';
                    lSubShape.Cells[cPinX].Formula  := '"' + IntToStr(lColPinX[lColIdx])   + ' mm"';
                  end;
                end;
                for lArcIdx := 1 to lArcCount - 1 do
                begin
                  lSubShape := lHorisontalLines[lArcIdx];
                  lSubShape.Cells[cEndX].Formula := '"' + IntToStr(lTotalWidth) + ' mm"';
                end;
                if (lArcCount = 1) then
                  lHorLineShape.Cells[cEndX].Formula := '"' + IntToStr(lTotalWidth) + ' mm"';
              end;
            end;
          end;
        end;
      end;
    end;
    if (NOT lDone) then
    begin
      AShape.Delete;
      AShape := nil;
    end;
   
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ChannelPenaltyStr (AChannel  : IGeneralFlowChannel;
                                             ATimeStep : integer;
                                             ALoadCase : integer;
                                             AArcNr    : integer) : string;
const OPNAME = 'TVNVEventHandler.ChannelPenaltyStr';
var
  lValDouble       : double;
  lValStr          : WideString;
  lTextStr         : string;
  lYieldModelData  : IYieldModelData;
  lTargetYield     : double;
  lMaxYield        : double;
  lFactor          : double;
  lMonthDays       : double;
  lMonthNr         : integer;
begin
  Result := '';
  try
    if (AChannel <> nil) then
    begin
      lMonthNr := ATimeStep mod 12;
      if (lMonthNr = 0) then
        lMonthNr := 12;
      lTextStr := '';
      case AChannel.ChannelType of
        2  : { Master Control / Yield Channel }
          begin
            lYieldModelData  := FAppModules.Model.ModelData as IYieldModelData;
            lTargetYield     := lYieldModelData.RunConfigurationData.TargetYieldByIndex[ALoadCase];
            lMaxYield        := lYieldModelData.RunConfigurationData.MaximumYieldByIndex[ALoadCase];
            lFactor          := AChannel.MasterControlFeature.FactorByMonth[lMonthNr];
            if (AArcNr = 1) then
            begin
              lValDouble := lTargetYield * lFactor * 1000000 / 365.25 / 86400.0;
              lTextStr   := FormatFloat('######0.000', lValDouble) + ' to ';
              lValDouble := lMaxYield * lFactor * 1000000 / 365.25 / 86400.0;
              lTextStr   := lTextStr + FormatFloat('######0.000', lValDouble);
            end
            else
            begin
              lValDouble := lTargetYield * lFactor * 1000000 / 365.25 / 86400.0;
              lTextStr   := '0 to ' + FormatFloat('######0.000', lValDouble);
            end;
          end;
        3  : lTextStr := CPowerArray[AArcNr];
        4  : { Irrigation Area}
             begin
               case AChannel.ChannelSubType of
               1 : lTextStr := CIrrAreaDiversionArray[AArcNr];
               2 : lTextStr := CIrrAreaConsumptiveArray[AArcNr];
               3 : lTextStr := CIrrAreaReturnArray[AArcNr];
               end;
             end;
        5  : lTextStr := CDiversionArray[AArcNr];
        6  : { Minimum Flow Channel }
          begin
            lValDouble := AChannel.MinimumFlowConstraint.MinimumFlowDemandByMonth[lMonthNr];
            if (AArcNr = 1) then
              lTextStr := FormatFloat('######0.000', lValDouble) + ' to inf'
            else
              lTextStr := '0 to ' + FormatFloat('######0.000', lValDouble);
          end;
        7  : lTextStr := CLossArray[AArcNr];
        8  : { Min Max Flow Feature }
          begin
            if (AArcNr = AChannel.ChannelPenalty.ChannelPenaltyArcCount) then
            begin
              lValDouble := AChannel.MinMaxFlowConstraint.FlowConstraintByArcMonth[AArcNr, lMonthNr];
              lTextStr := '0 to ' + FormatFloat('######0.000', lValDouble);
            end
            else
            begin
              lValDouble := AChannel.MinMaxFlowConstraint.FlowConstraintByArcMonth[AArcNr+1, lMonthNr];
              lTextStr   := FormatFloat('######0.000', lValDouble) + ' to ';
              lValDouble := AChannel.MinMaxFlowConstraint.FlowConstraintByArcMonth[AArcNr, lMonthNr];
              lTextStr   := lTextStr + FormatFloat('######0.000', lValDouble);
            end;
          end;
        9  : lTextStr := CPumpingArray[AArcNr];
        10 : lTextStr := CInflowArray[AArcNr];
        11 : { Specified Demand Channel }
          begin
            if (AChannel.SpecifiedDemandFeature.GetMonthlyDemand(ATimeStep, lValStr)) then
            begin
              lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
              lValDouble := StrToFloatDef(lValStr,0);
              lMonthDays := lYieldModelData.RunConfigurationData.MonthDaysByIndex[lMonthNr];
              lValDouble := lValDouble * 1000000 / (lMonthDays * 86400.0);
              lValStr    := FormatFloat('######0.000', lValDouble);
              if (AArcNr = 1) then
                lTextStr := lValStr + ' to ' + lValStr
              else
                lTextStr := '0 to ' + lValStr;
            end
            else
            begin
              if (AArcNr = 1) then
                lTextStr := '?? to ??'
              else
                lTextStr := '0 to ??';
            end;
          end;
        12 : { General Flow Channel }
             lTextStr := '0 to inf';
        14 : {Irrigation Block - Diversion Channel}
             lTextStr := CIrrBlockDiversionArray [AArcNr];
        15 : {Irrigation Block - Return Channel}
             lTextStr := CIrrBlockReturnArray[AArcNr];
      else
      end;
      Result := lTextStr;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.RefreshOutputSelector (AShape : IVShape);
const OPNAME = 'TVNVEventHandler.RefreshOutputSelector';
var
  lYieldModelData  : IYieldModelData;
  lShapeIdx        : integer;
  lSubShape        : IVShape;
  lSubType         : string;
begin
  try
    lYieldModelData  := FAppModules.Model.ModelData as IYieldModelData;
    for lShapeIdx := 1 to AShape.Shapes.Count do
    begin
      lSubShape := AShape.Shapes.Item[lShapeIdx];
      if (lSubShape.CellExists[cShapeType, 0] <> 0) then
      begin
        lSubType  := UnQuote(lSubShape.Cells[cShapeType].Formula);
        if (lSubType = 'LoadCase') then
          lSubShape.Text := IntToStr(FLoadCase)
        else if (lSubType = 'Sequence') then
          lSubShape.Text := IntToStr(FSequence)
        else if (lSubType = 'Month') then
          lSubShape.Text := IntToStr(FMonthIndex);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.RefreshText (AShape : IVShape);
const OPNAME = 'TVNVEventHandler.RefreshText';
var
  lElementID        : integer;
  lTextType         : TVNVTextType;
  lParentName       : string;
  lParentShape      : IVShape;
  lDataStr          : WideString;
  lDataErrors       : WideString;
  lDataValue        : double;
  lYieldModelData   : IYieldModelData;
  lChannel          : IGeneralFlowChannel;
  lReservoir        : IReservoirData;
  LWetland          : IWetland;

  lTextStr          : string;
  lFullSupplyLevel  : double;
  lDeadStorageLevel : double;
  lElevation        : double;
  lFull             : boolean;
  lFail             : Boolean;
  lTargetYield      : double;
  lFactor           : double;
  lMonthNr          : integer;
  lDemandValue      : double;
  lMonthDays        : double;
  lDemandStr        : WideString;
  lCanBeFull        : boolean;

  LContainer        : TStringList;
  LRow,LCol         : integer;
begin
  try
    //Do not refresh label that have been made permanent
    if (AShape.CellExists[cPermanent, 0] <> 0)  and (AShape.Cells[cPermanent].Formula = '"TRUE"')then Exit;
    lParentName := '';
    lTextStr    := '';
    lDataStr    := '';
    if (AShape.CellExists[cNumber, 0] = -1) AND
       (AShape.CellExists[cParent, 0] = -1) AND
       (AShape.CellExists[cTextType, 0] = -1) then
    begin
      lElementID   := StrToIntDef(UnQuote(AShape.Cells[cNumber].Formula),0);
      lParentName  := UnQuote(AShape.Cells[cParent].Formula);
      lTextType    := TVNVTextType(StrToIntDef(UnQuote(AShape.Cells[cTextType].Formula),0));
      lParentShape := FindShapeWithName(lParentName);
      if (lParentShape <> nil) then
      begin
        if (lTextType = nvtName) then
          AShape.Text := UnQuote(lParentShape.Cells[cName].Formula)
        else
        if (lTextType = nvtInflow) then
          RefreshInflowText(AShape, lElementID)
        else
        if (lTextType = nvtDemandFile) then
          RefreshDemandFileText(AShape, lElementID)
        else
        if (lTextType in OutputTextTypeSet) then
        begin
          AShape.CellsSRC[visSectionCharacter, visRowCharacter, visCharacterColor].FormulaU := IntToStr(visBlue);
          lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
          lTextStr := FAppModules.Language.GetString('VisioNetwork.StringNoValue');
          lFail    := FALSE;
          if(lTextType = nvtTimePeriod) then
          begin
            lTextStr :=  DateToStr(lYieldModelData.OutputData.GetSelection.AverageStartDate) + ' - '+
                         DateToStr(lYieldModelData.OutputData.GetSelection.AverageEndDate);
            AShape.Text := lTextStr;
          end
          else if(lTextType in [nvtChannelFlow,nvtAvgChannelFlow]) then
          begin
            lChannel := lYieldModelData.NetworkElementData.ChannelList.
                          ChannelByChannelNumber[lElementID];
            if (lChannel <> nil) then
            begin
              if(lTextType = nvtChannelFlow) then
              begin
                if(FLoadCase = 0) or (FSequence = 0) or (FMonthIndex = 0) then
                begin
                  ShowMessage('Please add the Output Selector on the diagram and make the neccesary selection.');
                  AShape.Text := 'Output selection invalid';
                  Exit;
                end;
                lYieldModelData.OutputData.SummaryOutputData.GetBlockDataByElementID(lDataStr,btMonthlyAverageChannelFlow, lElementID, lDataErrors);
                if(lDataStr <> '') then
                begin
                  LContainer  := TStringList.Create;
                  try
                    LContainer.CommaText := lDataStr;
                    LRow := FMonthIndex div 12;
                    LCol := FMonthIndex mod 12;
                    if (LCol = 0) then
                      LCol := 12;

                    if(LRow < LContainer.Count) then
                    begin
                       LContainer.CommaText := LContainer[LRow];
                       lDataStr := LContainer[LCol];
                    end;
                  finally
                    LContainer.Free;
                  end;
                end;

              end;
              if(lTextType = nvtAvgChannelFlow) then
                lYieldModelData.OutputData.SummaryOutputData.GetBlockAverageDataByElementID(lDataStr,btMonthlyAverageChannelFlow, lElementID, lDataErrors);

              if(lDataStr <> '') then
              begin
                lMonthNr := FMonthIndex mod 12;
                if (lMonthNr = 0) then
                  lMonthNr := 12;

                if (FUnits = ouPerSecond) then
                begin
                  lTextStr := lDataStr + ' ' + FAppModules.Language.GetString('VisioNetwork.Stringm3');
                end
                else
                if (FUnits = ouMcmPerMonthOrYear) then
                begin
                  lDataValue   := StrToFloatDef(lDataStr,0);
                  lMonthDays   := lYieldModelData.RunConfigurationData.MonthDaysByIndex[lMonthNr];
                  lDataValue   := (lDataValue * 86400.0 * lMonthDays) / 1000000;
                  lDataStr     := FormatFloat('#####0.000', lDataValue);
                  lTextStr := lDataStr + ' ' + FAppModules.Language.GetString('LabelText.Millionm3');
                end
                else
                if (FUnits = ouMegaLitersPerDay) then
                begin
                  lDataValue   := StrToFloatDef(lDataStr,0);
                  lDataValue   := (lDataValue *  86400.0) /1000.0;
                  lDataStr     := FormatFloat('#####0.000', lDataValue);
                  lTextStr := lDataStr + ' ' + FAppModules.Language.GetString('MasterControl.MegaL');
                end
                else
                  lTextStr := lDataStr;

                if (lChannel.MasterControlFeature <> nil) then
                begin
                  lDataValue   := StrToFloatDef(lDataStr,0);
                  lTargetYield := lYieldModelData.RunConfigurationData.TargetYieldByIndex[FLoadCase];
                  lFactor      := lChannel.MasterControlFeature.FactorByMonth[lMonthNr];
                  lDemandValue := Round((lTargetYield * lFactor * 1000000 / 365.25 / 86400.0) * 1000) / 1000;
                  lFail        := lDemandValue > lDataValue;
                end
                else
                if (lChannel.MinMaxFlowConstraint <> nil) then
                begin
                  lDataValue   := StrToFloatDef(lDataStr,0);
                  lDemandValue := lChannel.MinMaxFlowConstraint.FlowConstraintByArcMonth[1, lMonthNr];
                  lFail        := lDemandValue > lDataValue;
                end
                else
                if (lChannel.SpecifiedDemandFeature <> nil) then
                begin
                  if (lChannel.SpecifiedDemandFeature.GetMonthlyDemand(FTimeStep, lDemandStr)) then
                  begin
                    lDataValue   := StrToFloatDef(lDataStr,0);
                    lDemandValue := StrToFloatDef(lDemandStr,0);
                    lMonthDays   := lYieldModelData.RunConfigurationData.MonthDaysByIndex[lMonthNr];
                    lDemandValue := Round((lDemandValue * 1000000 / (lMonthDays * 86400.0)) * 1000) / 1000;
                    lFail        := lDemandValue > lDataValue;
                  end;
                end;
                if (lFail) then
                begin
                  lTextStr := lDataStr + ' ' + FAppModules.Language.GetString('VisioNetwork.StringFail');
                  AShape.CellsSRC[visSectionCharacter, visRowCharacter, visCharacterColor].FormulaU := IntToStr(visRed);
                end;
                if(lTextType = nvtAvgChannelFlow) and (lTextStr <> '') then
                  lTextStr := 'Avg ' + lTextStr;
              end;
            end;
            AShape.Text := lTextStr;
          end
          else
          begin
            lReservoir := lYieldModelData.NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[lElementID];
            if (lReservoir = nil) then
            begin
              LWetland := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.WetlandList.WetlandByNodeNumber[lElementID];
              if (LWetland <> nil) then
                lReservoir := LWetland.ReservoirDetails;
            end;

            if (lReservoir <> nil) then
            begin
              if (lTextType in [nvtNetBasinRunoff,nvtAvgNetBasinRunoff]) then
              begin
                if(lTextType = nvtNetBasinRunoff) then
                  lYieldModelData.OutputData.SummaryOutputData.GetBlockDataByElementID(lDataStr,btNetBasinRunoffIntoResArea, lElementID, lDataErrors);
                if(lTextType = nvtAvgNetBasinRunoff) then
                  lYieldModelData.OutputData.SummaryOutputData.GetBlockAverageDataByElementID(lDataStr,btNetBasinRunoffIntoResArea, lElementID, lDataErrors);

                if(lDataStr <> '') then
                  lTextStr := lDataStr + ' ' + FAppModules.Language.GetString('VisioNetwork.Stringm3')
              end;

              lYieldModelData.OutputData.SummaryOutputData.GetBlockDataByElementID(lDataStr,btMonthEndReservoirElevation, lElementID, lDataErrors);
              if(lDataStr <> '') then
              begin
                lCanBeFull   := True;
                lElevation        := StrToFloatDef(lDataStr,0);
                lFullSupplyLevel  := lReservoir.ReservoirZoneElevationsData.FullSupplyLevel.Elevation;
                lDeadStorageLevel := lReservoir.ReservoirZoneElevationsData.DeadStorageLevel.Elevation;
                lFull             := lElevation >= lFullSupplyLevel;
                lFail             := lElevation <= lDeadStorageLevel;
                if(lTextType in [nvtElevation,nvtAvgElevation]) then
                begin
                  if(lTextType = nvtAvgElevation) then
                    lYieldModelData.OutputData.SummaryOutputData.GetBlockAverageDataByElementID(lDataStr,btMonthEndReservoirElevation, lElementID, lDataErrors);
                  if(lDataStr <> '') then
                    lTextStr := lDataStr + ' m '
                end
                else
                if (lTextType in [nvtVolume,nvtAvgVolume,nvtReservoirStorageChange]) then
                begin
                  if(lTextType = nvtVolume) then
                    lYieldModelData.OutputData.SummaryOutputData.GetBlockDataByElementID(lDataStr,btMonthEndReservoirVolume, lElementID, lDataErrors);
                  if(lTextType = nvtAvgVolume) then
                    lYieldModelData.OutputData.SummaryOutputData.GetBlockAverageDataByElementID(lDataStr,btMonthEndReservoirVolume, lElementID, lDataErrors);
                  if(lTextType = nvtReservoirStorageChange) then
                    lYieldModelData.OutputData.SummaryOutputData.GetPeriodChangeDataByElementID(lDataStr,btMonthEndReservoirVolume, lElementID, lDataErrors);
                  if(lDataStr <> '') then
                    lTextStr := lDataStr + ' ' + FAppModules.Language.GetString('VisioNetwork.StringMcm')
                end
                else
                if (lTextType in [nvtRainfall,nvtAvgRainfall]) then
                begin
                  if(lTextType = nvtRainfall) then
                    lYieldModelData.OutputData.SummaryOutputData.GetBlockDataByElementID(lDataStr,btRainfallOnReservoirSurface, lElementID, lDataErrors);
                  if(lTextType = nvtAvgRainfall) then
                    lYieldModelData.OutputData.SummaryOutputData.GetBlockAverageDataByElementID(lDataStr,btRainfallOnReservoirSurface, lElementID, lDataErrors);

                  lCanBeFull   := False;
                  if(lDataStr <> '') then
                    lTextStr := lDataStr + ' ' + FAppModules.Language.GetString('VisioNetwork.Stringm3') + ' Rain';
                end
                else
                if (lTextType in [nvtEvaporation,nvtAvgEvaporation]) then
                begin
                  if(lTextType = nvtEvaporation) then
                    lYieldModelData.OutputData.SummaryOutputData.GetBlockDataByElementID(lDataStr,btGrossEvaporationLossFromReservoir, lElementID, lDataErrors);
                  if(lTextType = nvtAvgEvaporation) then
                    lYieldModelData.OutputData.SummaryOutputData.GetBlockAverageDataByElementID(lDataStr,btGrossEvaporationLossFromReservoir, lElementID, lDataErrors);

                  lCanBeFull   := False;
                  if(lDataStr <> '') then
                    lTextStr := lDataStr + ' ' + FAppModules.Language.GetString('VisioNetwork.Stringm3') + ' Evap';
                end
                else
                if (lTextType = nvtPercFull) AND
                   (lYieldModelData.OutputData.SummaryOutputData.GetBlockDataByElementID(lDataStr,btMonthEndReservoirVolume, lElementID, lDataErrors)) then
                begin
                  lDataValue := StrToFloatDef(lDataStr,0);
                  lDataValue := lDataValue / lReservoir.ReservoirConfigurationData.VolumeWhenFull * 100;
                  lTextStr   := FormatFloat('#####0.000', lDataValue) + ' % ';
                end;

                if lCanBeFull then
                begin
                  if lFull then
                    lTextStr := lTextStr + ' ' + FAppModules.Language.GetString('VisioNetwork.StringFull')
                  else if lFail then
                  begin
                    lTextStr := lTextStr + ' ' + FAppModules.Language.GetString('VisioNetwork.StringEmpty');
                    AShape.CellsSRC[visSectionCharacter, visRowCharacter, visCharacterColor].FormulaU := IntToStr(visRed);
                  end;
                end;
                if(lTextStr <> '') and (lTextType in [nvtAvgElevation, nvtAvgVolume, nvtAvgNetBasinRunoff,nvtAvgRainfall, nvtAvgEvaporation]) then
                  lTextStr := 'Avg ' + lTextStr;
              end;
            end;
            AShape.Text := lTextStr;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.RefreshInflowText (AShape     : IVShape;
                                              AElementID : integer);
const OPNAME = 'TVNVEventHandler.RefreshInflowText';
var
  lNode         : IReservoirData;
  lInflowPerc   : Double;
  lCatchmentRef : integer;
  lYieldModel   : IYieldModel;
  lParamRef     : IParamReference;
  lFileName     : string;
begin
  try
    if(AShape = nil) or (AElementID = 0) or (AElementID = NullInteger) then Exit;
    lYieldModel := FAppModules.Model as IYieldModel;

    lNode := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.
               ReservoirList.ReservoirOrNodeByIdentifier[AElementID];


    if (lNode <> nil) then
    begin
//      FVisioApp.ActiveWindow.Windows.ItemFromID[visWinIDCustProp].Visible := TRUE;
      lInflowPerc   := lNode.ReservoirConfigurationData.DrainageScale;
      lCatchmentRef := lNode.ReservoirConfigurationData.CatchmentRef;
      lFileName := '';
      if (lCatchmentRef <> 0) then
      begin
        lParamRef   := (lYieldModel.YieldModelData as IYieldModelData).ParamSetup.ReferenceDataByCatchNumber[lCatchmentRef];
        if lParamRef <> nil then
        begin
          lFileName   := lParamRef.FileReference;
          lFileName   := ExtractFileName(lFileName);
          AShape.Text := FormatFloat('##0.###', lInflowPerc) + '% x [' + IntToStr(lParamRef.CatchReference) + '] ' + lFileName;          
        end
        else
          AShape.Text := FAppModules.Language.GetString('VNV.Unassigned')
      end
      else
        AShape.Text := FAppModules.Language.GetString('VNV.Unassigned');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.RefreshDemandFileText (AShape     : IVShape;
                                                  AElementID : integer);
const OPNAME = 'TVNVEventHandler.RefreshDemandFileText';
var
  lChannel   : IGeneralFlowChannel;
begin
  try
    if(AShape = nil) or (AElementID = 0) or (AElementID = NullInteger) then Exit;
    lChannel := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.
                  ChannelList.ChannelByChannelNumber[AElementID];
    if ((lChannel <> nil) AND (lChannel.SpecifiedDemandFeature <> nil)) then
      AShape.Text := ExtractFileName(lChannel.SpecifiedDemandFeature.SpecifiedDemandFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.TextShapeAdded(const AShape: IVShape);
const OPNAME = 'TVNVEventHandler.TextShapeAdded';
var
  lElementNr   : integer;
  lElementType : string;
  lTextType    : TVNVTextType;
  lParentShape : IVShape;
  lXParent     : Double;
  lYParent     : Double;
  lXChild      : Double;
  lYChild      : Double;
  lIndex       : integer;
begin
  try
    if (FBufferTextList.Count > 0) AND (FBufferTextList.IndexOf(AShape.Name) >= 0) then
    begin
      lIndex := FBufferTextList.IndexOf(AShape.Name);
      FBufferTextList.Delete(lIndex);
    end
    else
    if (AShape.CellExists[cParent, 0] <> 0) AND
       (Trim(UnQuote(AShape.Cells[cParent].Formula)) <> '') then
    begin
      AShape.Delete;
    end
    else
    begin
      if (ShowTextDialog(lElementType, lElementNr, lTextType)) then
      begin
        if (lElementNr >= 0) AND (lElementType <> '') then
        begin
          if (lTextType = nvtInflow) then
            lParentShape := FindShapeWithParent(mtWRYMInflow, lParentShape.Name)
          else
            lParentShape := FindShapeWithPropertyValue(lElementType, cNumber, IntToStr(lElementNr));
          if (lParentShape <> nil) then
          begin
            if (AShape.CellExists[cParent, 0] = -1) then
            begin
              lXParent := StrToFloatDef(UnMM(UnQuote(lParentShape.Cells[cPinX].ResultStr[visMillimeters])),0);
              lYParent := StrToFloatDef(UnMM(UnQuote(lParentShape.Cells[cPinY].ResultStr[visMillimeters])),0);
              lXChild  := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinX].Formula)),0);
              lYChild  := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinY].Formula)),0);

              AShape.Cells[cParent].Formula := '"' + lParentShape.Name + '"';
              AShape.Cells[cParentXDiff].Formula  := '"' + FloatToStrF(lXParent - lXChild, ffFixed, 12, 2) + '"';
              AShape.Cells[cParentYDiff].Formula  := '"' + FloatToStrF(lYParent - lYChild, ffFixed, 12, 2) + '"';
              AShape.Cells[cParentXMoved].Formula := '"' + IntToStr(0) + '"';
              AShape.Cells[cParentYMoved].Formula := '"' + IntToStr(0) + '"';
              AShape.Cells[cVersion].Formula      := '"' + CThisVersion + '"';

              AShape.AddNamedRow(visSectionProp, 'Type', visTagDefault);
              AShape.Cells[cTextType].Formula := '"' + IntToStr(Ord(lTextType)) + '"';
              AShape.Cells[cTextLbl].Formula  := '"' + TextTypeToStr(lTextType) + '"';
              AShape.Cells[cTextInv].Formula  := '"TRUE"';

              if (lTextType = nvtDemandFile) then
                AShape.CellsSRC[visSectionCharacter, visRowCharacter, visCharacterColor].FormulaU := IntToStr(visRed);

              AShape.AddNamedRow(visSectionProp, 'Number', visTagDefault);
              AShape.Cells[cNumber].Formula := '"' + IntToStr(lElementNr) + '"';

              if (lTextType in OutputTextTypeSet) then
                FOutputList.Add(AShape.Name);

              RefreshText(AShape);
            end;
          end;
        end;
      end
      else
        AShape.Delete;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ShowTextDialog (var AElementType : string;
                                          var AElementNr   : integer;
                                          var ATextType    : TVNVTextType) : boolean;
const OPNAME = 'TVNVEventHandler.ShowTextDialog';
var
  lDialog   : TVNVTextDialog;
begin
  Result := FALSE;
  try
  	lDialog := TVNVTextDialog.CreateWithoutDFM(nil, FAppModules);
    lDialog.LanguageHasChanged;
    try
	    Result := lDialog.ShowTextDialog
                          (FReservoirList.CommaText,
                           FNodeList.CommaText,
                           FChannelList.CommaText,
                           FWetlandList.CommaText,
                           AElementNr, AElementType, ATextType);
    finally
      lDialog.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.ChannelPenaltyShapeAdded(const AShape: IVShape);
const OPNAME = 'TVNVEventHandler.ChannelPenaltyShapeAdded';
var
  lElementID  : integer;
  LShape      : IVShape;
begin
  try
    if (FBufferChanPenaltyList.Count > 0) then
      { Upgraded channel penalty }
      FBufferChanPenaltyList.Delete(0)
    else
    begin
      { Upgrade channel penalty }
      UpgradeChannelPenalty(AShape);

      if (AShape.CellExists[cNumber, 0] <> 0) AND
         (UnQuote(AShape.Cells[cNumber].Formula) <> '') then
      begin
        { Copied channel penalty }
        lElementID := StrToIntDef(UnQuote(AShape.Cells[cNumber].Formula),0);
      end
      else
      begin
        { New channel penalty - from stencil }
        lElementID := ShowChannelPenaltiesDialog;
        if (lElementID <> -1) then
        begin
          AShape.Cells[cNumber].Formula  := '"' + IntToStr(lElementID) + '"';
          AShape.Cells[cVersion].Formula := '"' + CThisVersion + '"';
        end;
      end;
      if (lElementID = -1) then
        AShape.Delete
      else
      begin
        LShape := AShape;
        RefreshChannelPenalty(LShape, lElementID);
        if(LShape <> nil) then
          FChanPenaltyList.Add(IntToStr(lElementID));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.OutputSelectorShapeAdded(const AShape: IVShape);
const OPNAME = 'TVNVEventHandler.OutputSelectorShapeAdded';
var
  lMessage    : string;
begin
  try
   if (FOutputSelector <> nil) then
    begin
      lMessage := FAppModules.Language.GetString('VNV.MultiOutputSelectorErr');
      MessageDlg(lMessage, mtError, [mbOK], 0);
      AShape.Delete;
    end
    else
    begin
      FOutputSelector := AShape;
      AShape.Cells[cNumber].Formula    := '"0"';
      AShape.AddNamedRow(visSectionProp, 'Version', visTagDefault);
      AShape.Cells[cVersionLbl].Formula := '"Version"';
      AShape.Cells[cVersion].Formula    := '"' + CThisVersion + '"';
      AShape.Cells[cVersionInv].Formula := '"TRUE"';
      RefreshOutputSelector(AShape);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.InflowShapeAdded (const AShape: IVShape);
const OPNAME = 'TVNVEventHandler.InflowShapeAdded';
var
  lMsg    : string;
  lIndex  : integer;
begin
  try
    if (FBufferInflowList.Count > 0) AND (FBufferInflowList.IndexOf(AShape.Name) >= 0) then
    begin
      lIndex := FBufferInflowList.IndexOf(AShape.Name);
      FBufferInflowList.Delete(lIndex);
    end
    else
    begin
      if (AShape.CellExists[cParent, 0] = 0) OR
         (Trim(UnQuote(AShape.Cells[cParent].Formula)) = '') then
      begin
        lMsg := FAppModules.Language.GetString('VNV.MayNotAddInflow');
        MessageDlg(lMsg, mtError, [mbOK], 0);
      end
      else
        AShape.Cells[cParent].Formula := '""';
      AShape.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
      AShape.Delete;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TVNVEventHandler.SFRSubCatchmentShapeAdded(const AShape: IVShape);
  function SFRDroppedOnReservoirOrNodeNumber(const AShape: IVShape): integer;
  const OPNAME = 'TVNVEventHandler.SFRDroppedOnReservoirOrNodeNumber';
  var
    X1, X2,
    Y1, Y2      : Integer;
    XDiff,
    YDiff       : Integer;
    LShape      : IVShape;
    lIndex      : integer;
  begin
    Result := -1;
    try

      if (AShape.CellExists[cParent, 0] = 0) OR
         (Trim(UnQuote(AShape.Cells[cParent].Formula)) = '') then
      begin
        //check which node/reservoir selected
        for LIndex := 1 to FVisioApp.ActivePage.Shapes.Count do
        begin
          LShape := FVisioApp.ActivePage.Shapes[LIndex];
          if ((Pos(PChar(mtWRYMReservoir), LShape.Name) > 0) or
               (Pos(PChar(mtWRYMNode), LShape.Name) > 0)) then
         begin
        //            lElementID := StrToInt(UnQuote(LShape.Cells[cNumber].Formula));//StrToInt(lFormula);
            X1 := Round(StrToFloat(UnMM(UnQuote(LShape.Cells[cPinX].Formula))));
            X2 := Round(StrToFloat(UnMM(UnQuote(AShape.Cells[cPinX].Formula))));

            Y1 := Round(StrToFloat(UnMM(UnQuote(LShape.Cells[cPinY].Formula))));
            Y2 := Round(StrToFloat(UnMM(UnQuote(AShape.Cells[cPinY].Formula))));

            XDiff := abs(X1 - X2);
            YDiff := abs(Y1 - Y2);

            if ((XDiff <= 10) and (YDiff <= 10)) then
            begin
              Result := StrToIntDef(UnQuote(LShape.Cells[cNumber].Formula), -1);;
              break;
            end;
          end;
        end;
      end;
    except on E: Exception do HandleError(E, OPNAME) end;
  end;
const OPNAME = 'TVNVEventHandler.SFRSubCatchmentShapeAdded';
var
  LIndex,
  lElementID  : integer;
  LShape      : IVShape;
  LReservoirNumber : integer;
  LSFRCommaText : String;
  lYieldModel : IYieldModel;
begin
  try
    if (FBufferSubCatchmentList.Count > 0) AND (FBufferSubCatchmentList.IndexOf(AShape.Name) >= 0) then
    begin
      lIndex := FBufferSubCatchmentList.IndexOf(AShape.Name);
      FBufferSubCatchmentList.Delete(lIndex);
      Exit;
    end;

    if (FAppModules.StudyArea.ModelVersion <> '7')  then
    begin
      MessageDlg(FAppModules.Language.GetString('VNV.SFRANoModelSupport'), mtError, [mbOK], 0);
      AShape.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
      AShape.Delete;
      Exit;
    end;
    LReservoirNumber := SFRDroppedOnReservoirOrNodeNumber(AShape);
    if(LReservoirNumber < 0) then
    begin
      MessageDlg(FAppModules.Language.GetString('VNV.SFRADragDropError'), mtError, [mbOK], 0);
      AShape.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
      AShape.Delete;
      Exit;
    end;

    lYieldModel   := (FAppModules.Model as IYieldModel);
    LSFRCommaText := lYieldModel.YieldModelData.NetworkFeaturesData.StreamFlowReductionList.StreamFlowReductionIDsPerInflowNode(LReservoirNumber);
    if(LSFRCommaText <> '') then
    begin
//      MessageDlg(FAppModules.Language.GetString('VNV.ReservoirAlreadyHasSFR'), mtError, [mbOK], 0);
      AShape.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
//      AShape.Delete;
      Exit;
    end;

    lElementID    := ShowSubCatchmentDialog(LReservoirNumber, sfrdtCreateNew);
    if (lElementID <> -1) then
    begin
      lYieldModel.ViewInputStreamFlowReductionDialog(lElementID);
      LShape := FindReservoirWithNumber(LReservoirNumber);
      AddInflowSymbol(LShape,LReservoirNumber);
    end;
    
    AShape.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
    AShape.Delete;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.InflowShapeDeleted (const AShape : IVShape);
const OPNAME = 'TVNVEventHandler.InflowShapeDeleted';
var
  lParentShape : IVShape;
  lMsg         : string;
  lElementID   : integer;
  lInflowTxt   : IVShape;
begin
  try
    lParentShape := FindShapeWithName(UnQuote(AShape.Cells[cParent].Formula));
    if (lParentShape <> nil) then
    begin
      if (FBufferInflowList.IndexOf(AShape.Name) = -1) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.MayNotDeleteInflow');
        MessageDlg(lMsg, mtError, [mbOK], 0);
        lElementID := StrToIntDef(Trim(UnQuote(AShape.Cells[cNumber].Formula)),0);
        AddInflowSymbol(lParentShape, lElementID);
      end
      else
      begin

        lInflowTxt := FindShapeWithParent(mtWRYMText, AShape.Name);
        if (lInflowTxt <> nil) then
          begin
            lInflowTxt.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
            lInflowTxt.Delete;
          end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ShowChannelPenaltiesDialog : integer;
const OPNAME = 'TVNVEventHandler.ShowChannelPenaltiesDialog';
var
  lDialog   : TVNVChannelPenaltyDialog;
  lChanTemp : string;
  lPenTemp  : string;
begin
  Result := -1;
  try
    lDialog := TVNVChannelPenaltyDialog.CreateWithoutDFM(nil, FAppModules);
    lDialog.LanguageHasChanged;
    try
      lChanTemp := FChannelList.CommaText;
      lPenTemp  := FChanPenaltyList.CommaText;
      Result    := lDialog.ShowChannelPenaltiesDialog(lChanTemp, lPenTemp, FChanPenaltyDuplicate);
    finally
      lDialog.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVEventHandler.ColourChanged (ACell  : IVCell;
                                          AShape : IVShape);
const OPNAME = 'TVNVEventHandler.ColourChanged';
begin
  try
    if (AShape.CellExists[cColourChanged, 0] = -1) then
      AShape.Cells[cColourChanged].Formula := '"Y"';
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVEventHandler.PositionChanged (ACell  : IVCell;
                                            AShape : IVShape);
const OPNAME = 'TVNVEventHandler.PositionChanged';
begin
  try
    if (AShape.Master <> nil) then
    begin
      if (Pos(PChar(mtWRYMReservoir), AShape.Name) > 0) then
        NodePositionChanged(ACell, AShape)
      else
      if (Pos(PChar(mtWRYMSubCatchment), AShape.Name) > 0) then
        SFRASubCatchmentPositionChanged(ACell, AShape)
      else
      if (Pos(PChar(mtWRYMNode), AShape.Name) > 0) then
        NodePositionChanged(ACell, AShape)
      else
      if (Pos(PChar(mtWRYMChannel), AShape.Name) > 0) then
        ChannelPositionChanged(ACell, AShape)
      else
      if (Pos(PChar(mtWRYMInflow), AShape.Name) > 0) then
        InflowPositionChanged(ACell, AShape)
      else
      if (Pos(PChar(mtWRYMIrrBlock), AShape.Name) > 0) then
        IrrigationBlockPositionChanged(ACell, AShape)
      else
      if (Pos(PChar(mtWRYMIrrArea), AShape.Name) > 0) then
        IrrigationAreaPositionChanged(ACell, AShape)
      else
      if (Pos(PChar(mtWRYMWetland), AShape.Name) > 0) then
        WetlandPositionChanged(ACell, AShape)
      else
      if (Pos(PChar(mtWRYMText), AShape.Name) > 0) then
        TextPositionChanged(ACell, AShape)
      else
      if (Pos(PChar(mtWRYMDemandCentre), AShape.Name) > 0) then
        DemandCentrePositionChanged(ACell, AShape)
      else
      if (Pos(PChar(mtWRYMMine), AShape.Name) > 0) then
        NodePositionChanged(ACell, AShape)
      else
      if (Pos(PChar(mtWRYMGroundWater), AShape.Name) > 0) then
        NodePositionChanged(ACell, AShape);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVEventHandler.TextPositionChanged (ACell  : IVCell;
                                                AShape : IVShape);
const OPNAME = 'TVNVEventHandler.TextPositionChanged';
var
  lParentName  : string;
  lParentShape : IVShape;
  lTextPin     : double;
  lParentPin   : double;
  lDiff        : double;
  lParentMoved : integer;
begin
  try
    lParentName := UnQuote(AShape.Cells[cParent].Formula);
    lParentShape := FindShapeWithName(lParentName);
    if (lParentShape <> nil) then
    begin
      if (Pos(mtWRYMSubCatchment, lParentName) > 0) then
      begin
        if (ACell.Name = cPinX) then
        begin
          lParentMoved := StrToIntDef(UnQuote(AShape.Cells[cParentXMoved].Formula),0);
          if (lParentMoved = 1) then
            AShape.Cells[cParentXMoved].Formula := '"' + IntToStr(0) + '"'
          else
          begin
            lTextPin   := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinX].Formula)),0);
            lParentPin := StrToFloatDef(UnMM(UnQuote(lParentShape.Cells[cPinX].ResultStr[visMillimeters])),0);
            lDiff      := lParentPin - lTextPin;
            AShape.Cells[cParentXDiff].Formula := '"' + FloatToStrF(lDiff, ffFixed, 12, 2) + '"';
          end;
        end
        else
        if (ACell.Name = cPinY) then
        begin
          lParentMoved := StrToIntDef(UnQuote(AShape.Cells[cParentYMoved].Formula),0);
          if (lParentMoved = 1) then
            AShape.Cells[cParentYMoved].Formula := '"' + IntToStr(0) + '"'
          else
          begin
            lTextPin   := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinY].Formula)),0);
            lParentPin := StrToFloatDef(UnMM(UnQuote(lParentShape.Cells[cPinY].ResultStr[visMillimeters])),0);
            lDiff      := lParentPin - lTextPin;
            AShape.Cells[cParentYDiff].Formula := '"' + FloatToStrF(lDiff, ffFixed, 12, 2) + '"';
          end;
        end;
      end
      else
      begin
        if (ACell.Name = cPinX) then
        begin
          lParentMoved := StrToIntDef(UnQuote(AShape.Cells[cParentXMoved].Formula),0);
          if (lParentMoved = 1) then
            AShape.Cells[cParentXMoved].Formula := '"' + IntToStr(0) + '"'
          else
          begin
            lTextPin   := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinX].Formula)),0);
            lParentPin := StrToFloatDef(UnMM(UnQuote(lParentShape.Cells[cPinX].ResultStr[visMillimeters])),0);
            lDiff      := lParentPin - lTextPin;
            AShape.Cells[cParentXDiff].Formula := '"' + FloatToStrF(lDiff, ffFixed, 12, 2) + '"';
          end;
        end
        else
        if (ACell.Name = cPinY) then
        begin
          lParentMoved := StrToIntDef(UnQuote(AShape.Cells[cParentYMoved].Formula),0);
          if (lParentMoved = 1) then
            AShape.Cells[cParentYMoved].Formula := '"' + IntToStr(0) + '"'
          else
          begin
            lTextPin   := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinY].Formula)),0);
            lParentPin := StrToFloatDef(UnMM(UnQuote(lParentShape.Cells[cPinY].ResultStr[visMillimeters])),0);
            lDiff      := lParentPin - lTextPin;
            AShape.Cells[cParentYDiff].Formula := '"' + FloatToStrF(lDiff, ffFixed, 12, 2) + '"';
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVEventHandler.ChannelPositionChanged (ACell  : IVCell;
                                                   AShape : IVShape);
const OPNAME = 'TVNVEventHandler.ChannelPositionChanged';
var
  lTxtShape    : IVShape;
  lXDiff       : double;
  lYDiff       : double;
  lXParent     : double;
  lYParent     : double;
  lShapesLst   : TStringList;
  lIndex       : integer;
  lShapeName   : string;
begin
  try
    lShapesLst := TStringList.Create;
    try
      FindTextShapesWithParent(AShape.Name, lShapesLst);
      for lIndex := 0 to lShapesLst.Count - 1 do
      begin
        lShapeName := lShapesLst.Strings[lIndex];
        lTxtShape  := FindShapeWithName(lShapeName);
        if (lTxtShape <> nil) then
        begin
          if (ACell.Name = cBeginX) OR (ACell.Name = cEndX) then
          begin
            lXDiff := StrToFloatDef(UnQuote(lTxtShape.Cells[cParentXDiff].Formula),0);
            lXParent := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinX].ResultStr[visMillimeters])),0);
            lTxtShape.Cells[cParentXMoved].Formula := '"' + IntToStr(1) + '"';
            lTxtShape.Cells[cPinX].Formula := '"' + FloatToStrF(lXParent - lXDiff, ffFixed, 12, 2) + 'mm"';
          end
          else
          if (ACell.Name = cBeginY) OR (ACell.Name = cEndY) then
          begin
            lYDiff := StrToFloatDef(UnQuote(lTxtShape.Cells[cParentYDiff].Formula),0);
            lYParent := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinY].ResultStr[visMillimeters])),0);
            lTxtShape.Cells[cParentYMoved].Formula := '"' + IntToStr(1) + '"';
            lTxtShape.Cells[cPinY].Formula := '"' + FloatToStrF(lYParent - lYDiff, ffFixed, 12, 2) + 'mm"';
          end;
        end;
      end;
    finally
      FreeAndNil(lShapesLst);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVEventHandler.IrrigationBlockPositionChanged(ACell: IVCell; AShape: IVShape);
const OPNAME = 'TVNVEventHandler.IrrigationBlockPositionChanged';
var
  lTxtShape    : IVShape;
  lDiff        : double;
  lParentPin   : double;
  lElementID   : integer;
  //lShapePin    : double;
  //lParentName  : string;
  //lParentShape : IVShape;
  //lParentMoved : integer;
begin
  try
    {lParentName := UnQuote(AShape.Cells[cParent].Formula);
    lParentShape := FindShapeWithName(lParentName);
    if (lParentShape <> nil) then
    begin
      if (ACell.Name = cPinX) then
      begin
        lParentMoved := StrToIntDef(UnQuote(AShape.Cells[cParentXMoved].Formula),0);
        if (lParentMoved = 1) then
          AShape.Cells[cParentXMoved].Formula := '"' + IntToStr(0) + '"'
        else
        begin
          lShapePin  := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinX].Formula)),0);
          lParentPin := StrToFloatDef(UnMM(UnQuote(lParentShape.Cells[cPinX].Formula)),0);
          lDiff      := lParentPin - lShapePin;
          AShape.Cells[cParentXDiff].Formula := '"' + FloatToStrF(lDiff, ffFixed, 12, 2) + '"';
        end;
      end
      else
      if (ACell.Name = cPinY) then
      begin
        lParentMoved := StrToIntDef(UnQuote(AShape.Cells[cParentYMoved].Formula),0);
        if (lParentMoved = 1) then
          AShape.Cells[cParentYMoved].Formula := '"' + IntToStr(0) + '"'
        else
        begin
          lShapePin  := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinY].Formula)),0);
          lParentPin := StrToFloatDef(UnMM(UnQuote(lParentShape.Cells[cPinY].Formula)),0);
          lDiff      := lParentPin - lShapePin;
          AShape.Cells[cParentYDiff].Formula := '"' + FloatToStrF(lDiff, ffFixed, 12, 2) + '"';
        end;
      end;
    end;}
    lTxtShape := FindShapeWithParent(mtWRYMText, AShape.Name);
    if (lTxtShape <> nil) then
    begin
      if (ACell.Name = cPinX) then
      begin
        lDiff      := StrToFloatDef(UnQuote(lTxtShape.Cells[cParentXDiff].Formula),0);
        lParentPin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinX].Formula)),0);
        lTxtShape.Cells[cParentXMoved].Formula := '"' + IntToStr(1) + '"';
        lTxtShape.Cells[cPinX].Formula := '"' + FloatToStrF(lParentPin - lDiff, ffFixed, 12, 2) + 'mm"';
        if (FDrawing.GISMode) then
        begin
          lElementID := StrToIntDef(UnQuote(AShape.Cells[cNumber].Formula),0);
          GISCalculateLonLatOfShape(AShape,lElementID,gisCalcLon);
        end;
      end
      else
      if (ACell.Name = cPinY) then
      begin
        lDiff      := StrToFloatDef(UnQuote(lTxtShape.Cells[cParentYDiff].Formula),0);
        lParentPin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinY].Formula)),0);
        lTxtShape.Cells[cParentYMoved].Formula := '"' + IntToStr(1) + '"';
        lTxtShape.Cells[cPinY].Formula := '"' + FloatToStrF(lParentPin - lDiff, ffFixed, 12, 2) + 'mm"';
        if (FDrawing.GISMode) then
        begin
          lElementID := StrToIntDef(UnQuote(AShape.Cells[cNumber].Formula),0);
          GISCalculateLonLatOfShape(AShape,lElementID,gisCalcLat);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVEventHandler.WetlandPositionChanged(ACell: IVCell; AShape: IVShape);
const OPNAME = 'TVNVEventHandler.WetlandPositionChanged';
var
  lInflowShape : IVShape;
  lTxtShape    : IVShape;
  lDiff        : double;
  lParentPin   : double;
  lElementID   : integer;
  //lShapePin    : double;
  //lParentName  : string;
  //lParentShape : IVShape;
  //lParentMoved : integer;
begin
  try
    {lParentName := UnQuote(AShape.Cells[cParent].Formula);
    lParentShape := FindShapeWithName(lParentName);
    if (lParentShape <> nil) then
    begin
      if (ACell.Name = cPinX) then
      begin
        lParentMoved := StrToIntDef(UnQuote(AShape.Cells[cParentXMoved].Formula),0);
        if (lParentMoved = 1) then
          AShape.Cells[cParentXMoved].Formula := '"' + IntToStr(0) + '"'
        else
        begin
          lShapePin  := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinX].Formula)),0);
          lParentPin := StrToFloatDef(UnMM(UnQuote(lParentShape.Cells[cPinX].Formula)),0);
          lDiff      := lParentPin - lShapePin;
          AShape.Cells[cParentXDiff].Formula := '"' + FloatToStrF(lDiff, ffFixed, 12, 2) + '"';
        end;
      end
      else
      if (ACell.Name = cPinY) then
      begin
        lParentMoved := StrToIntDef(UnQuote(AShape.Cells[cParentYMoved].Formula),0);
        if (lParentMoved = 1) then
          AShape.Cells[cParentYMoved].Formula := '"' + IntToStr(0) + '"'
        else
        begin
          lShapePin  := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinY].Formula)),0,);
          lParentPin := StrToFloatDef(UnMM(UnQuote(lParentShape.Cells[cPinY].Formula)),0);
          lDiff      := lParentPin - lShapePin;
          AShape.Cells[cParentYDiff].Formula := '"' + FloatToStrF(lDiff, ffFixed, 12, 2) + '"';
        end;
      end;
    end;}

    lInflowShape := FindShapeWithParent(mtWRYMInflow, AShape.Name);
    if (lInflowShape <> nil) then
    begin
      if (ACell.Name = cPinX) then
      begin
        lDiff      := StrToFloatDef(UnQuote(lInflowShape.Cells[cParentXDiff].Formula),0);
        lParentPin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinX].Formula)),0);
        lInflowShape.Cells[cParentXMoved].Formula := '"' + IntToStr(1) + '"';
        lInflowShape.Cells[cPinX].Formula := '"' + FloatToStrF(lParentPin - lDiff, ffFixed, 12, 2) + 'mm"';
      end
      else
      if (ACell.Name = cPinY) then
      begin
        lDiff      := StrToFloatDef(UnQuote(lInflowShape.Cells[cParentYDiff].Formula),0);
        lParentPin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinY].Formula)),0);
        lInflowShape.Cells[cParentYMoved].Formula := '"' + IntToStr(1) + '"';
        lInflowShape.Cells[cPinY].Formula := '"' + FloatToStrF(lParentPin - lDiff, ffFixed, 12, 2) + 'mm"';
      end;
    end;

    lTxtShape := FindShapeWithParent(mtWRYMText, AShape.Name);
    if (lTxtShape <> nil) then
    begin
      if (ACell.Name = cPinX) then
      begin
        lDiff      := StrToFloatDef(UnQuote(lTxtShape.Cells[cParentXDiff].Formula),0);
        lParentPin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinX].Formula)),0);
        lTxtShape.Cells[cParentXMoved].Formula := '"' + IntToStr(1) + '"';
        lTxtShape.Cells[cPinX].Formula := '"' + FloatToStrF(lParentPin - lDiff, ffFixed, 12, 2) + 'mm"';
        if (FDrawing.GISMode) then
        begin
          lElementID := StrToIntDef(UnQuote(AShape.Cells[cNumber].Formula),0);
          GISCalculateLonLatOfShape(AShape,lElementID,gisCalcLon);
        end;
      end
      else
      if (ACell.Name = cPinY) then
      begin
        lDiff      := StrToFloatDef(UnQuote(lTxtShape.Cells[cParentYDiff].Formula),0);
        lParentPin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinY].Formula)),0);
        lTxtShape.Cells[cParentYMoved].Formula := '"' + IntToStr(1) + '"';
        lTxtShape.Cells[cPinY].Formula := '"' + FloatToStrF(lParentPin - lDiff, ffFixed, 12, 2) + 'mm"';
        if (FDrawing.GISMode) then
        begin
          lElementID := StrToIntDef(UnQuote(AShape.Cells[cNumber].Formula),0);
          GISCalculateLonLatOfShape(AShape,lElementID,gisCalcLat);
        end;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TVNVEventHandler.DemandCentrePositionChanged(ACell: IVCell; AShape: IVShape);
const OPNAME = 'TVNVEventHandler.DemandCentrePositionChanged';
var
  lTxtShape    : IVShape;
  lDiff        : double;
  lParentPin   : double;
  lElementID   : integer;
begin
  try
    lTxtShape := FindShapeWithParent(mtWRYMText, AShape.Name);
    if (lTxtShape <> nil) then
    begin
      if (ACell.Name = cPinX) then
      begin
        lDiff      := StrToFloatDef(UnQuote(lTxtShape.Cells[cParentXDiff].Formula),0);
        lParentPin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinX].Formula)),0);
        lTxtShape.Cells[cParentXMoved].Formula := '"' + IntToStr(1) + '"';
        lTxtShape.Cells[cPinX].Formula := '"' + FloatToStrF(lParentPin - lDiff, ffFixed, 12, 2) + 'mm"';
        if (FDrawing.GISMode) then
        begin
          lElementID := StrToIntDef(UnQuote(AShape.Cells[cNumber].Formula),0);
          GISCalculateLonLatOfShape(AShape,lElementID,gisCalcLon);
        end;
      end
      else
      if (ACell.Name = cPinY) then
      begin
        lDiff      := StrToFloatDef(UnQuote(lTxtShape.Cells[cParentYDiff].Formula),0);
        lParentPin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinY].Formula)),0);
        lTxtShape.Cells[cParentYMoved].Formula := '"' + IntToStr(1) + '"';
        lTxtShape.Cells[cPinY].Formula := '"' + FloatToStrF(lParentPin - lDiff, ffFixed, 12, 2) + 'mm"';
        if (FDrawing.GISMode) then
        begin
          lElementID := StrToIntDef(UnQuote(AShape.Cells[cNumber].Formula),0);
          GISCalculateLonLatOfShape(AShape,lElementID,gisCalcLat);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TVNVEventHandler.NodePositionChanged (ACell  : IVCell;
                                                AShape : IVShape);
const OPNAME = 'TVNVEventHandler.NodePositionChanged';
var
  lTxtShape    : IVShape;
  lInflowShape : IVShape;
  lDiff        : double;
  lParentPin   : double;
  lShapesLst   : TStringList;
  lIndex       : integer;
  lShapeName   : string;
  lElementID   :  integer;
//  lFormula     : string;
begin
  try
    lShapesLst := TStringList.Create;
    try
      FindTextShapesWithParent(AShape.Name, lShapesLst);
      for lIndex := 0 to lShapesLst.Count - 1 do
      begin
        lShapeName := lShapesLst.Strings[lIndex];
        lTxtShape  := FindShapeWithName(lShapeName);
        if (lTxtShape <> nil) then
        begin
          if (ACell.Name = cPinX) then
          begin
            lDiff      := StrToFloatDef(UnQuote(lTxtShape.Cells[cParentXDiff].Formula),0);
            lParentPin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinX].Formula)),0);
            lTxtShape.Cells[cParentXMoved].Formula := '"' + IntToStr(1) + '"';
            lTxtShape.Cells[cPinX].Formula := '"' + FloatToStrF(lParentPin - lDiff, ffFixed, 12, 2) + 'mm"';
          end
          else
          if (ACell.Name = cPinY) then
          begin
            lDiff      := StrToFloatDef(UnQuote(lTxtShape.Cells[cParentYDiff].Formula),0);
            lParentPin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinY].Formula)),0);
            lTxtShape.Cells[cParentYMoved].Formula := '"' + IntToStr(1) + '"';
            lTxtShape.Cells[cPinY].Formula := '"' + FloatToStrF(lParentPin - lDiff, ffFixed, 12, 2) + 'mm"';
          end;
        end;
      end
    finally
      FreeAndNil(lShapesLst);
    end;

    if (FBufferNodeHasSFRAList.IndexOf(AShape.Name) >= 0 ) then
      lInflowShape := FindShapeWithParent(mtWRYMSubCatchment, AShape.Name)
    else
      lInflowShape := FindShapeWithParent(mtWRYMInflow, AShape.Name);
    if (lInflowShape <> nil) then
    begin
      if (ACell.Name = cPinX) then
      begin
        lDiff      := StrToFloatDef(UnQuote(lInflowShape.Cells[cParentXDiff].Formula),0);
        lParentPin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinX].Formula)),0);
        lInflowShape.Cells[cParentXMoved].Formula := '"' + IntToStr(1) + '"';
        lInflowShape.Cells[cPinX].Formula := '"' + FloatToStrF(lParentPin - lDiff, ffFixed, 12, 2) + 'mm"';
      end
      else
      if (ACell.Name = cPinY) then
      begin
        lDiff      := StrToFloatDef(UnQuote(lInflowShape.Cells[cParentYDiff].Formula),0);
        lParentPin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinY].Formula)),0);
        lInflowShape.Cells[cParentYMoved].Formula := '"' + IntToStr(1) + '"';
        lInflowShape.Cells[cPinY].Formula := '"' + FloatToStrF(lParentPin - lDiff, ffFixed, 12, 2) + 'mm"';
      end;
    end;

    if (FVisioApp.ActivePage <> nil) then
    begin
        if (FDrawing.GISMode) then
        begin
//         lFormula := MidStr(AShape.Cells[cNumber].Formula, 2, length(AShape.Cells[cNumber].Formula)-2);
         lElementID := StrToIntDef(UnQuote(AShape.Cells[cNumber].Formula), -1); //StrToInt(lFormula);
         if (ACell.Name = cPinX) then
           GISCalculateLonLatOfShape (AShape, lElementID, gisCalcLon)
         else
           if (ACell.Name = cPinY) then
             GISCalculateLonLatOfShape (AShape, lElementID, gisCalcLat)
        end;

    end; //if (FVisioApp.ActivePage <> nil)

  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TVNVEventHandler.SFRASubCatchmentPositionChanged (ACell  : IVCell;
                                                            AShape : IVShape);
const OPNAME = 'TVNVEventHandler.SFRASubCatchmentPositionChanged';
var
  lTxtShape    : IVShape;
  lDiff        : double;
  lParentPin   : double;
  lShapePin    : double;
  lParentName  : string;
  lParentShape : IVShape;
  lParentMoved : integer;
begin
  try
    if (AShape = nil) then
      exit;

    lParentName := UnQuote(AShape.Cells[cParent].Formula);
    lParentShape := FindShapeWithName(lParentName);
    if (lParentShape <> nil) then
    begin
      if (ACell.Name = cPinX) then
      begin
        lParentMoved := StrToIntDef(UnQuote(AShape.Cells[cParentXMoved].Formula),0);
        if (lParentMoved = 1) then
          AShape.Cells[cParentXMoved].Formula := '"' + IntToStr(0) + '"'
        else
        begin
          lShapePin  := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinX].Formula)),0);
          lParentPin := StrToFloatDef(UnMM(UnQuote(lParentShape.Cells[cPinX].Formula)),0);
          lDiff      := lParentPin - lShapePin;
          AShape.Cells[cParentXDiff].Formula := '"' + FloatToStrF(lDiff, ffFixed, 12, 2) + '"';
        end;
      end
      else
      if (ACell.Name = cPinY) then
      begin
        lParentMoved := StrToIntDef(UnQuote(AShape.Cells[cParentYMoved].Formula),0);
        if (lParentMoved = 1) then
          AShape.Cells[cParentYMoved].Formula := '"' + IntToStr(0) + '"'
        else
        begin
          lShapePin  := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinY].Formula)),0);
          lParentPin := StrToFloatDef(UnMM(UnQuote(lParentShape.Cells[cPinY].Formula)),0);
          lDiff      := lParentPin - lShapePin;
          AShape.Cells[cParentYDiff].Formula := '"' + FloatToStrF(lDiff, ffFixed, 12, 2) + '"';
        end;
      end;
    end;

    lTxtShape := FindShapeWithParent(mtWRYMText, AShape.Name);
    if (lTxtShape <> nil) then
    begin
      if (ACell.Name = cPinX) then
      begin
        lDiff      := StrToFloatDef(UnQuote(lTxtShape.Cells[cParentXDiff].Formula),0);
        lParentPin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinX].Formula)),0);
        lTxtShape.Cells[cParentXMoved].Formula := '"' + IntToStr(1) + '"';
        lTxtShape.Cells[cPinX].Formula := '"' + FloatToStrF(lParentPin - lDiff, ffFixed, 12, 2) + 'mm"';
      end
      else
      if (ACell.Name = cPinY) then
      begin
        lDiff      := StrToFloatDef(UnQuote(lTxtShape.Cells[cParentYDiff].Formula),0);
        lParentPin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinY].Formula)),0);
        lTxtShape.Cells[cParentYMoved].Formula := '"' + IntToStr(1) + '"';
        lTxtShape.Cells[cPinY].Formula := '"' + FloatToStrF(lParentPin - lDiff, ffFixed, 12, 2) + 'mm"';
      end;
    end;

    //SFRA Sub Catchment Text
    lTxtShape := FindSFRATextWithParent(lParentName);
    if (lTxtShape <> nil) then
    begin
      if (ACell.Name = cPinX) then
      begin
        lDiff      := StrToFloatDef(UnQuote(lTxtShape.Cells[cParentXDiff].Formula),0);
        lParentPin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinX].Formula)),0);
        lTxtShape.Cells[cParentXMoved].Formula := '"' + IntToStr(1) + '"';
        lTxtShape.Cells[cPinX].Formula := '"' + FloatToStrF(lParentPin - lDiff + 25, ffFixed, 12, 2) + 'mm"';
      end
      else
      if (ACell.Name = cPinY) then
      begin
        lDiff      := StrToFloatDef(UnQuote(lTxtShape.Cells[cParentYDiff].Formula),0);
        lParentPin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinY].Formula)),0);
        lTxtShape.Cells[cParentYMoved].Formula := '"' + IntToStr(1) + '"';
        lTxtShape.Cells[cPinY].Formula := '"' + FloatToStrF(lParentPin - lDiff, ffFixed, 12, 2) + 'mm"';
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;



procedure TVNVEventHandler.InflowPositionChanged (ACell  : IVCell;
                                                  AShape : IVShape);
const OPNAME = 'TVNVEventHandler.InflowPositionChanged';
var
  lTxtShape    : IVShape;
  lDiff        : double;
  lParentPin   : double;
  lShapePin    : double;
  lParentName  : string;
  lParentShape : IVShape;
  lParentMoved : integer;
begin
  try
    lParentName := UnQuote(AShape.Cells[cParent].Formula);
    lParentShape := FindShapeWithName(lParentName);
    if (lParentShape <> nil) then
    begin
      if (ACell.Name = cPinX) then
      begin
        lParentMoved := StrToIntDef(UnQuote(AShape.Cells[cParentXMoved].Formula),0);
        if (lParentMoved = 1) then
          AShape.Cells[cParentXMoved].Formula := '"' + IntToStr(0) + '"'
        else
        begin
          lShapePin  := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinX].Formula)),0);
          lParentPin := StrToFloatDef(UnMM(UnQuote(lParentShape.Cells[cPinX].Formula)),0);
          lDiff      := lParentPin - lShapePin;
          AShape.Cells[cParentXDiff].Formula := '"' + FloatToStrF(lDiff, ffFixed, 12, 2) + '"';
        end;
      end
      else
      if (ACell.Name = cPinY) then
      begin
        lParentMoved := StrToIntDef(UnQuote(AShape.Cells[cParentYMoved].Formula),0);
        if (lParentMoved = 1) then
          AShape.Cells[cParentYMoved].Formula := '"' + IntToStr(0) + '"'
        else
        begin
          lShapePin  := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinY].Formula)),0);
          lParentPin := StrToFloatDef(UnMM(UnQuote(lParentShape.Cells[cPinY].Formula)),0);
          lDiff      := lParentPin - lShapePin;
          AShape.Cells[cParentYDiff].Formula := '"' + FloatToStrF(lDiff, ffFixed, 12, 2) + '"';
        end;
      end;
    end;
    lTxtShape := FindShapeWithParent(mtWRYMText, AShape.Name);
    if (lTxtShape <> nil) then
    begin
      if (ACell.Name = cPinX) then
      begin
        lDiff      := StrToFloatDef(UnQuote(lTxtShape.Cells[cParentXDiff].Formula),0);
        lParentPin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinX].Formula)),0);
        lTxtShape.Cells[cParentXMoved].Formula := '"' + IntToStr(1) + '"';
        lTxtShape.Cells[cPinX].Formula := '"' + FloatToStrF(lParentPin - lDiff, ffFixed, 12, 2) + 'mm"';
      end
      else
      if (ACell.Name = cPinY) then
      begin
        lDiff      := StrToFloatDef(UnQuote(lTxtShape.Cells[cParentYDiff].Formula),0);
        lParentPin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinY].Formula)),0);
        lTxtShape.Cells[cParentYMoved].Formula := '"' + IntToStr(1) + '"';
        lTxtShape.Cells[cPinY].Formula := '"' + FloatToStrF(lParentPin - lDiff, ffFixed, 12, 2) + 'mm"';
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TVNVEventHandler.ProcessVNVSpecial (const AParameter: WideString): boolean;
const OPNAME = 'TVNVEventHandler.ProcessVNVSpecial';
var
  lParameter : string;
  lFunction  : string;
  lShapeName : string;
  lNumber    : integer;
  lPos       : integer;
begin
  Result := FALSE;
  try
    lParameter := AParameter;
    lPos := Pos(',', lParameter);
    if (lPos > 0) then
    begin
      lFunction  := Copy(lParameter, 1, lPos - 1);
      lFunction  := Trim(lFunction);
      lParameter := Copy(lParameter, lPos + 1, Length(lParameter) - lPos);
      lPos       := Pos(',', lParameter);
      if (lPos > 0) then
      begin
        lShapeName := Copy(lParameter, 1, lPos - 1);
        lParameter := Copy(lParameter, lPos + 1, Length(lParameter) - lPos);
        lNumber    := StrToIntDef(UnQuote(lParameter),0);
        if (lFunction = 'DoubleClicked') then
          ProcessVNVDoubleClicked(lShapeName, lNumber)
        else
        if (lFunction = '1') then
          ProcessVNVRightClicked(lShapeName, lNumber)
        else
        if (lFunction = '2') then
          ProcessVNVToggleGIS
        else
          MessageDlg(lFunction + ' '+ FAppModules.Language.GetString('VisioNetwork.MshNotImplemented'), mtError, [mbOK], 0);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ProcessVNVRightClicked (AShapeName : string;
                                                  ANumber    : integer) : WordBool;
const OPNAME = 'TVNVEventHandler.ProcessVNVRightClicked';
var
  lYieldModelData : IYieldModelData;
begin
  Result := FALSE;
  try
    lYieldModelData := (FAppModules.Model.ModelData as IYieldModelData);
    if (Pos(mtWRYMReservoir, AShapeName) > 0) then
    begin
      if (lYieldModelData.NetworkElementData.ReservoirList.ReservoirByIdentifier[ANumber] = nil) then
        MessageDlg(FAppModules.Language.GetString('Message.ReservoirNotExist') +
                    IntToStr(ANumber) +
                    FAppModules.Language.GetString('Message.ScenarioNotExist'), mtError, [mbOK], 0)
      else
        Result := ProcessVNVReservoirRightClicked(AShapeName, ANumber);
    end
    else if (Pos(mtWRYMMine, AShapeName) > 0) then
    begin
      if (lYieldModelData.NetworkFeaturesData.MineList.MineByNodeNumber[ANumber] = nil) then
        MessageDlg(FAppModules.Language.GetString('Message.MineNotExist') +
                    IntToStr(ANumber) +
                    FAppModules.Language.GetString('Message.ScenarioNotExist'), mtError, [mbOK], 0)
      else
        Result := ProcessVNVMineRightClicked(AShapeName, ANumber);
    end
    else if (Pos(mtWRYMGroundWater, AShapeName) > 0) then
    begin
      if (lYieldModelData.NetworkFeaturesData.GroundWaterList.GroundWaterByNodeNumber[ANumber] = nil) then
        MessageDlg(FAppModules.Language.GetString('Message.GroundWaterNotExist') +
                    IntToStr(ANumber) +
                    FAppModules.Language.GetString('Message.ScenarioNotExist'), mtError, [mbOK], 0)
      else
        Result := ProcessVNVGroundWaterRightClicked(AShapeName, ANumber);
    end
    else if (Pos(mtWRYMNode, AShapeName) > 0) then
    begin
      if (lYieldModelData.NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[ANumber] = nil) then
        MessageDlg(FAppModules.Language.GetString('Message.NodeNotExist') +
                    IntToStr(ANumber) +
                    FAppModules.Language.GetString('Message.ScenarioNotExist'), mtError, [mbOK], 0)
      else
        Result := ProcessVNVNodeRightClicked(AShapeName, ANumber);
    end
    else if (Pos(mtWRYMChannel, AShapeName) > 0) then
    begin
      if (lYieldModelData.NetworkElementData.ChannelList.ChannelByChannelNumber[ANumber] = nil) then
        MessageDlg(FAppModules.Language.GetString('Message.ChannelNotExist') +
                    IntToStr(ANumber) +
                    FAppModules.Language.GetString('Message.ScenarioNotExist'), mtError, [mbOK], 0)
      else
        Result := ProcessVNVChannelRightClicked(AShapeName, ANumber);
    end
    else if (Pos(mtWRYMIrrBlock, AShapeName) > 0) then
    begin
      if (lYieldModelData.NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[ANumber] = nil) then
        MessageDlg(FAppModules.Language.GetString('Message.IrrigationBlockNotExist') +
                   IntToStr(ANumber) +
                     FAppModules.Language.GetString('Message.ScenarioNotExist'), mtError, [mbOK], 0)
    else
        Result := ProcessVNVIrrigationBlockRightClicked(AShapeName, ANumber);
    end
    else if (Pos(mtWRYMIrrArea, AShapeName) > 0) then
    begin
      if (lYieldModelData.NetworkFeaturesData.IrrigationAreaList.IrrigationAreaByNodeNumber[ANumber] = nil) then
        MessageDlg(FAppModules.Language.GetString('Message.IrrigationAreaNotExist') +
                   IntToStr(ANumber) +
                     FAppModules.Language.GetString('Message.ScenarioNotExist'), mtError, [mbOK], 0)
    else
        Result := ProcessVNVIrrigationAreaRightClicked(AShapeName, ANumber);
    end
    else if (Pos(mtWRYMWetland, AShapeName) > 0) then
    begin
      if (lYieldModelData.NetworkFeaturesData.WetlandList.WetlandByNodeNumber[ANumber] = nil) then
        MessageDlg(FAppModules.Language.GetString('Message.WetlandNotExist') +
                   IntToStr(ANumber) +
                     FAppModules.Language.GetString('Message.ScenarioNotExist'), mtError, [mbOK], 0)
    else
        Result := ProcessVNVWetlandRightClicked(AShapeName, ANumber);
    end
    else if (Pos(mtWRYMSubCatchment, AShapeName) > 0) then
    begin
        //jkw check if subcatchment area exists
        Result := ProcessVNVSubCatchmentRightClicked(AShapeName, ANumber);
    end
    else if (Pos(mtWRYMDemandCentre, AShapeName) > 0) then
    begin
      if (lYieldModelData.NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByNodeNumber[ANumber] = nil) then
        MessageDlg(FAppModules.Language.GetString('Message.DemandCentreNotExist') +
                   IntToStr(ANumber) +
                     FAppModules.Language.GetString('Message.ScenarioNotExist'), mtError, [mbOK], 0)
      else
        Result := ProcessVNVDemandCentreRightClicked(AShapeName, ANumber);
    end
    else if (Pos(mtModel, AShapeName) > 0) then
    begin
        Result := ProcessVNVModelRightClicked(AShapeName, ANumber);
    end
    else if (Pos(mtWRYMText, AShapeName) > 0) then
    begin
        Result := ProcessVNVTextRightClicked(AShapeName, ANumber);
    end
    else if (Pos(mtWRYMResPenalty, AShapeName) > 0) then
    begin
        Result := ProcessVNVReservoirPenaltyRightClicked(AShapeName, ANumber);
    end
    else if (Pos(mtWRYMChanPenalty, AShapeName) > 0) then
    begin
        Result := ProcessVNVChannelPenaltyRightClicked(AShapeName, ANumber);
    end
    else if (Pos(mtMultiAction, AShapeName) > 0) then
    begin
        Result := ProcessVNVMultiActionRightClicked(AShapeName, ANumber);
    end
    else if (Pos(mtLayersAction, AShapeName) > 0) then
    begin
      Result := ProcessVNVLayerRightClicked(AShapeName, ANumber);
    end 
    else if (Pos(mtWRYMPowerPlant, AShapeName) > 0) then
    begin
      if (lYieldModelData.NetworkFeaturesData.PowerPlantList.PowerPlantByID[ANumber] = nil) then
        MessageDlg(FAppModules.Language.GetString('Message.PowerPlantNotExist') +
                    IntToStr(ANumber) +
                    FAppModules.Language.GetString('Message.ScenarioNotExist'), mtError, [mbOK], 0)
      else
        Result := ProcessVNVPowerPlantRightClicked(AShapeName, ANumber);
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ProcessVNVReservoirRightClicked (AShapeName : string; ANumber    : integer) : WordBool;
const OPNAME = 'TVNVEventHandler.ProcessVNVReservoirRightClicked';
var
  lYieldModel     : IYieldModel;
  lOptionList     : TStringList;
  lMenuDlg        : TVNVShapeMenuDlg;
  lOption         : integer;
  LPermanentColor : boolean;
  lShape          : IVShape;
begin
  Result := FALSE;
  try
    lYieldModel := (FAppModules.Model as IYieldModel);
    lOptionList := TStringList.Create;
    try
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.MenuCancelOption'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.ViewReservoirProperties'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.ViewReservoirOutputDataGrid'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.DeleteReservoirFromScenario'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('MenuCaption.DroughtRestriction'),TObject(1));
      LPermanentColor := True;
      lShape := GetFirstSelectedShape(mtWRYMReservoir);
      if(lShape <> nil) and (lShape.CellExists[cPermanentColor, 0] <> 0) then
      begin
        LPermanentColor := (lShape.Cells[cPermanentColor].Formula = '"TRUE"');
        if LPermanentColor then
          lOptionList.AddObject(FAppModules.Language.GetString('VNV.PermanentColorOff'),TObject(1))
        else
          lOptionList.AddObject(FAppModules.Language.GetString('VNV.PermanentColorOn'),TObject(1));
      end;

      lMenuDlg := TVNVShapeMenuDlg.Create(nil);
      try
        lOption := lMenuDlg.ShowShapeMenuDialog(lOptionList);
        case lOption of
          1 : begin
                Result := lYieldModel.ViewInputReservoirDialog(ANumber);
                if (Result) then
                  ReservoirHasChanged(AShapeName, ANumber);
              end;
          2 : Result := lYieldModel.ViewOutputReservoirDialog(ANumber);
          3 : Result := DeleteReservoir(ANumber);
          4 : Result := ViewReservoirDroughtRestriction(AShapeName,ANumber);
          5 : begin
                if LPermanentColor then
                  lShape.Cells[cPermanentColor].Formula         := '"FALSE"'
                else
                  lShape.Cells[cPermanentColor].Formula         := '"TRUE"';
                Result := True;
              end;
        else
        end;
      finally
        lMenuDlg.Free;
      end;
    finally
      FreeAndNil(lOptionList);
    end
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ProcessVNVIrrigationBlockRightClicked(AShapeName: string; ANumber: integer): WordBool;
const OPNAME = 'TVNVEventHandler.ProcessVNVIrrigationBlockRightClicked';
var
  lYieldModel : IYieldModel;
  lOptionList : TStringList;
  lMenuDlg    : TVNVShapeMenuDlg;
  lOption     : integer;
  lIrrigationBlock : IIrrigationBlock;
  LPermanentColor  : boolean;
  lShape           : IVShape;
begin
  Result := FALSE;
  try
    lIrrigationBlock := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[ANumber];
    if (lIrrigationBlock <> nil) then
    begin
      lYieldModel := (FAppModules.Model as IYieldModel);
      lOptionList := TStringList.Create;
      try
        lOptionList.AddObject(FAppModules.Language.GetString('VNV.MenuCancelOption'),TObject(1));
        lOptionList.AddObject(FAppModules.Language.GetString('VNV.ViewIrrigationBlockProperties'),TObject(1));
        lOptionList.AddObject(FAppModules.Language.GetString('VNV.DeleteIrrigationBlockFromScenario'),TObject(1));
        LPermanentColor := True;
        lShape := GetFirstSelectedShape(mtWRYMIrrBlock);
        if(lShape <> nil) and (lShape.CellExists[cPermanentColor, 0] <> 0) then
        begin
          LPermanentColor := (lShape.Cells[cPermanentColor].Formula = '"TRUE"');
          if LPermanentColor then
            lOptionList.AddObject(FAppModules.Language.GetString('VNV.PermanentColorOff'),TObject(1))
          else
            lOptionList.AddObject(FAppModules.Language.GetString('VNV.PermanentColorOn'),TObject(1));
        end;

        lMenuDlg := TVNVShapeMenuDlg.Create(nil);
        try
          lOption := lMenuDlg.ShowShapeMenuDialog(lOptionList);
          case lOption of
            1 : begin
                  Result := lYieldModel.ViewInputIrrigationBlockDialog(ANumber);
                  if (Result) then
                    IrrigationBlockHasChanged(AShapeName, ANumber);
                end;
            2 : Result := DeleteIrrigationBlock(ANumber);
            3 : begin
                  if LPermanentColor then
                    lShape.Cells[cPermanentColor].Formula         := '"FALSE"'
                  else
                    lShape.Cells[cPermanentColor].Formula         := '"TRUE"';
                  Result := True;
                end;
          end;
        finally
          lMenuDlg.Free;
        end;
      finally
        FreeAndNil(lOptionList);
      end
    end
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ProcessVNVWetlandRightClicked(AShapeName: string; ANumber: integer): WordBool;
const OPNAME = 'TVNVEventHandler.ProcessVNVWetlandRightClicked';
var
  lYieldModel : IYieldModel;
  lOptionList : TStringList;
  lMenuDlg    : TVNVShapeMenuDlg;
  lOption     : integer;
  LWetland    : IWetland;
  LPermanentColor  : boolean;
  lShape           : IVShape;
begin
  Result := FALSE;
  try
    LWetland := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.WetlandList.WetlandByNodeNumber[ANumber];
    if (LWetland <> nil) then
    begin
      lYieldModel := (FAppModules.Model as IYieldModel);
      lOptionList := TStringList.Create;
      try
        lOptionList.AddObject(FAppModules.Language.GetString('VNV.MenuCancelOption'),TObject(1));
        lOptionList.AddObject(FAppModules.Language.GetString('VNV.ViewWetlandProperties'),TObject(1));
        lOptionList.AddObject(FAppModules.Language.GetString('VNV.ViewWetlandOutput'),TObject(1));
        lOptionList.AddObject(FAppModules.Language.GetString('VNV.DeleteWetlandFromScenario'),TObject(1));
        LPermanentColor := True;
        lShape := GetFirstSelectedShape(mtWRYMWetland);
        if(lShape <> nil) and (lShape.CellExists[cPermanentColor, 0] <> 0) then
        begin
          LPermanentColor := (lShape.Cells[cPermanentColor].Formula = '"TRUE"');
          if LPermanentColor then
            lOptionList.AddObject(FAppModules.Language.GetString('VNV.PermanentColorOff'),TObject(1))
          else
            lOptionList.AddObject(FAppModules.Language.GetString('VNV.PermanentColorOn'),TObject(1));
        end;

        lMenuDlg := TVNVShapeMenuDlg.Create(nil);
        try
          lOption := lMenuDlg.ShowShapeMenuDialog(lOptionList);
        finally
          lMenuDlg.Free;
        end;
      finally
        FreeAndNil(lOptionList);
      end;
      case lOption of
        1 : begin
              Result := lYieldModel.ViewInputWetlandDialog(ANumber);
              if (Result) then
                WetlandHasChanged(AShapeName, ANumber);
            end;
        2 : Result := lYieldModel.ViewOutputWetlandDialog(ANumber);
        3 : Result := DeleteWetland(ANumber);
        4 : begin
              if LPermanentColor then
                lShape.Cells[cPermanentColor].Formula         := '"FALSE"'
              else
                lShape.Cells[cPermanentColor].Formula         := '"TRUE"';
              Result := True;
            end;
      end;
    end
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ProcessVNVDemandCentreRightClicked(AShapeName: string; ANumber: integer): WordBool;
const OPNAME = 'TVNVEventHandler.ProcessVNVDemandCentreRightClicked';
var
  lYieldModel   : IYieldModel;
  lOptionList   : TStringList;
  lMenuDlg      : TVNVShapeMenuDlg;
  lOption       : integer;
  lDemandCentre : IYMDemandCentre;
  lYMDCChannels : TStringList;
  LPermanentColor : boolean;
  lShape          : IVShape;
begin
  Result := FALSE;
  try
    lYMDCChannels := TStringList.Create;  
    lDemandCentre := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByNodeNumber[ANumber];

    if (lDemandCentre <> nil) then
    begin
      lYieldModel := (FAppModules.Model as IYieldModel);
      lOptionList := TStringList.Create;
      try
        lOptionList.AddObject(FAppModules.Language.GetString('VNV.MenuCancelOption'),TObject(1));
        lOptionList.AddObject(FAppModules.Language.GetString('VNV.ViewDemandCentreProperties'),TObject(1));
        lOptionList.AddObject(FAppModules.Language.GetString('VNV.DeleteDemandCentreFromScenario'),TObject(1));
        LPermanentColor := True;
        lShape := GetFirstSelectedShape(mtWRYMDemandCentre);
        if(lShape <> nil) and (lShape.CellExists[cPermanentColor, 0] <> 0) then
        begin
          LPermanentColor := (lShape.Cells[cPermanentColor].Formula = '"TRUE"');
          if LPermanentColor then
            lOptionList.AddObject(FAppModules.Language.GetString('VNV.PermanentColorOff'),TObject(1))
          else
            lOptionList.AddObject(FAppModules.Language.GetString('VNV.PermanentColorOn'),TObject(1));
        end;

        lMenuDlg := TVNVShapeMenuDlg.Create(nil);
        try
          lOption := lMenuDlg.ShowShapeMenuDialog(lOptionList);
          case lOption of
            1 : begin
                  Result := lYieldModel.ViewInputDemandCentreDialog(ANumber);
                  if (Result) then
                  begin
                    DemandCentreHasChanged(AShapeName, ANumber);
                  end;
                end;
            2 :begin
                 if MessageDlg(FAppModules.Language.GetString('VNV.DeleteDemandCentreFromScenario') + '?', mtConfirmation, [mbOK, mbCancel], 0) = mrOK then
                 begin
                   lShape := FindDemandCentreByNode(ANumber);
                   if (lShape <> nil) then
                     lShape.Delete;
                   Result := lYieldModel.DoDeleteYMDemandCentre(ANumber);
                 end
               end;
            3 : begin
                  if LPermanentColor then
                    lShape.Cells[cPermanentColor].Formula         := '"FALSE"'
                  else
                    lShape.Cells[cPermanentColor].Formula         := '"TRUE"';
                  Result := True;
                end;

          else
          end;
        finally
          lMenuDlg.Free;
        end;
      finally
        FreeAndNil(lYMDCChannels);
        FreeAndNil(lOptionList);
      end
    end
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TVNVEventHandler.ProcessVNVSubCatchmentRightClicked(AShapeName: string; ANumber: integer): WordBool;
const OPNAME = 'TVNVEventHandler.ProcessVNVSubCatchmentRightClicked';
var
  lYieldModel   : IYieldModel;
  lOptionList   : TStringList;
  lMenuDlg      : TVNVShapeMenuDlg;
  lOption       : integer;
  lElementID    : integer;
  LShape        : IVShape;
begin
  Result := True;
  try
    lOptionList := TStringList.Create;
    lMenuDlg := TVNVShapeMenuDlg.Create(nil);
    try
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.MenuCancelOption'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.ViewInflowChannelProperties'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.ViewSFRASubCatchmentProperties'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.DeleteSFRAFromScenario'),TObject(1));
      lOption := lMenuDlg.ShowShapeMenuDialog(lOptionList);
    finally
      FreeAndNil(lOptionList);
      lMenuDlg.Free;
    end;

    lYieldModel := (FAppModules.Model as IYieldModel);
    case lOption of
      1 : begin
            Result := lYieldModel.ViewInputConfigurationDialog(mdvnCatchmentProportions);
            if (Result) then
              NodeCatchmentHasChanged(AShapeName, ANumber);
              LShape := FindShapeWithName(AShapeName);
              RefreshSubCatchment(LShape, ANumber);
           end;
      2 : begin
            lElementID := ShowSubCatchmentDialog(ANumber, sfrdtViewProperties);
            if (lElementID <> -1) then
            begin
              Result := lYieldModel.ViewInputStreamFlowReductionDialog(lElementID);
              if Result then
              begin
                LShape := FindReservoirWithNumber(ANumber);
                if(LShape <> nil) then
                  ReservoirHasChanged(LShape.Name,ANumber);
              end;
            end;
          end;
      3 : begin
            lElementID := ShowSubCatchmentDialog(ANumber, sfrdtViewProperties);
            if (lElementID <> -1) then
            begin
              Result := DeleteSubCatchment(lElementID);
            end;
          end;
      else
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.DeleteReservoir (ANumber : integer; ADBDelete : integer = -1) : WordBool;
const OPNAME = 'TVNVEventHandler.DeleteReservoir';
var
  lShape : IVShape;
begin
  Result := FALSE;
  try
    lShape := FindShapeWithPropertyValue(mtWRYMReservoir, cNumber, IntToStr(ANumber));
    if (lShape <> nil) AND (MayChangeScenario) then
    begin
      lShape.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
      lShape.Delete;
      if ADBDelete = -1 then
        Result := (FAppModules.Model as IYieldModel).DoDeleteReservoir(ANumber);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.DeleteSubCatchment(ANumber: integer): WordBool;
  function Get_SubCatchmentReservoirNumber(SFRID: integer): integer;
  const OPNAME = 'TVNVEventHandler.DeleteSubCatchment';
  var
    lSubCatchment    : IStreamFlowReduction;
  begin
    Result := NullInteger;
    try
      lSubCatchment := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.StreamFlowReductionList.StreamFlowReductionByID[ANumber];
      if(lSubCatchment <> nil) then
        Result := lSubCatchment.InflowNodeNumber;;
    except on E: Exception do HandleError(E, OPNAME) end;
  end;
const OPNAME = 'TVNVEventHandler.DeleteSubCatchment';
var
  lShape : IVShape;
  lYieldModelData  : IYieldModelData;
  LReservoirNumber : integer;
begin
  Result := FALSE;
  try
    lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
    LReservoirNumber := Get_SubCatchmentReservoirNumber(ANumber);
    if(LReservoirNumber >= 0) then
    begin
      lShape := FindShapeWithPropertyValue(mtWRYMSubCatchment, cNumber, IntToStr(LReservoirNumber));
      if (lShape <> nil) AND (MayChangeScenario) then
      begin
        Result := (FAppModules.Model as IYieldModel).DoDeleteSFRSubCatchment(ANumber);
        if Result then
        begin
          FBufferSubCatchmentList.Add(IntToStr(LReservoirNumber));
          lShape.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
          lShape.Delete;
          lShape := FindShapeWithPropertyValue(mtWRYMReservoir, cNumber, IntToStr(LReservoirNumber));
          if (lShape <> nil) then
          begin
            AddInflowSymbol(lShape, LReservoirNumber);
            RefreshReservoir(lShape, LReservoirNumber);
          end;
          end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.DeleteIrrigationBlock(ANumber: integer): WordBool;
const OPNAME = 'TVNVEventHandler.DeleteIrrigationBlock';
var
  lShape : IVShape;
begin
  Result := FALSE;
  try
    lShape := FindShapeWithPropertyValue(mtWRYMIrrBlock, cNumber, IntToStr(ANumber));
    if (lShape <> nil) AND (MayChangeScenario) then
    begin
      lShape.Delete;
      Result := (FAppModules.Model as IYieldModel).DoDeleteIrrigationBlock(ANumber);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.DeleteWetland(ANumber: integer): WordBool;
const OPNAME = 'TVNVEventHandler.DeleteWetland';
var
  lShape : IVShape;
begin
  Result := FALSE;
  try
    lShape := FindShapeWithPropertyValue(mtWRYMWetland, cNumber, IntToStr(ANumber));
    if (lShape <> nil) AND (MayChangeScenario) then
    begin
      lShape.Delete;
      Result := (FAppModules.Model as IYieldModel).DoDeleteWetland(ANumber);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.DeleteDemandCentre(ANumber: integer): WordBool;
const OPNAME = 'TVNVEventHandler.DeleteDemandCentre';
var
  lShape : IVShape;
begin
  Result := FALSE;
  try
    lShape := FindShapeWithPropertyValue(mtWRYMDemandCentre, cNumber, IntToStr(ANumber));
    if (lShape <> nil) AND (MayChangeScenario) then
    begin
      lShape.Delete;
      Result := (FAppModules.Model as IYieldModel).DoDeleteYMDemandCentre(ANumber)
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ProcessVNVTextRightClicked(AShapeName: string; ANumber: integer): WordBool;
const OPNAME = 'TVNVEventHandler.ProcessVNVTextRightClicked';
var
  lOptionList     : TStringList;
  lMenuDlg        : TVNVShapeMenuDlg;
  lOption         : integer;
  LPermanentOn    : boolean;
  lShape          : IVShape;
begin
  Result := FALSE;
  try
    lShape := GetFirstSelectedShape(mtWRYMText);
    if(lShape <> nil) and (lShape.CellExists[cPermanent, 0] <> 0) then
    begin
      LPermanentOn := (lShape.Cells[cPermanent].Formula = '"TRUE"');
      lOptionList := TStringList.Create;
      try
        lOptionList.AddObject(FAppModules.Language.GetString('VNV.MenuCancelOption'),TObject(1));
        if LPermanentOn then
          lOptionList.AddObject(FAppModules.Language.GetString('VNV.RemovePermenant'),TObject(1))
        else
          lOptionList.AddObject(FAppModules.Language.GetString('VNV.SetPermenant'),TObject(1));
        lOptionList.AddObject(FAppModules.Language.GetString('VNV.DeleteText'),TObject(1));
        lOptionList.AddObject(FAppModules.Language.GetString('VNV.DeleteRelatedText'),TObject(1));

        lMenuDlg := TVNVShapeMenuDlg.Create(nil);
        try
          lOption := lMenuDlg.ShowShapeMenuDialog(lOptionList);
          if (lOption = 1) then
          begin
            if LPermanentOn then
            begin
              lShape.Cells[cPermanent].Formula         := '"FALSE"';
              RefreshText(lShape);
            end
            else
              lShape.Cells[cPermanent].Formula         := '"TRUE"';
          end;
          if (lOption = 2) then
            DeleteOutputTextLabels(lShape);
          if (lOption = 3) then
            DeleteRelatedOutputTextLabels(lShape,ANumber);
          
        finally
          lMenuDlg.Free;
        end;
      finally
        FreeAndNil(lOptionList);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ProcessVNVChannelPenaltyRightClicked(AShapeName: string; ANumber: integer): WordBool;
const OPNAME = 'TVNVEventHandler.ProcessVNVChannelPenaltyRightClicked';
var
  lOptionList     : TStringList;
  lMenuDlg        : TVNVShapeMenuDlg;
  lOption         : integer;
  LPermanentOn    : boolean;
  lShape          : IVShape;
  lYieldModel     : IYieldModel;
begin
  Result := FALSE;
  try
    lShape := GetFirstSelectedShape(mtWRYMChanPenalty);
    lYieldModel := (FAppModules.Model as IYieldModel);
    if(lShape <> nil) and (lYieldModel <> nil) and (lShape.CellExists[cPermanent, 0] <> 0) then
    begin
      LPermanentOn := (lShape.Cells[cPermanent].Formula = '"TRUE"');
      lOptionList := TStringList.Create;
      try
        lOptionList.AddObject(FAppModules.Language.GetString('VNV.MenuCancelOption'),TObject(1));
        lOptionList.AddObject(FAppModules.Language.GetString('VNV.ViewChannelPenalties'),TObject(1));

        if LPermanentOn then
          lOptionList.AddObject(FAppModules.Language.GetString('VNV.RemovePermenant'),TObject(1))
        else
          lOptionList.AddObject(FAppModules.Language.GetString('VNV.SetPermenant'),TObject(1));

        lOptionList.AddObject(FAppModules.Language.GetString('VNV.DeleteText'),TObject(1));
        lOptionList.AddObject(FAppModules.Language.GetString('VNV.DeleteRelatedText'),TObject(1));
        lMenuDlg := TVNVShapeMenuDlg.Create(nil);
        try
          lOption := lMenuDlg.ShowShapeMenuDialog(lOptionList);
          if (lOption = 1) then
            Result := lYieldModel.ViewInputConfigurationDialog(mdvnChannelPenalties);

          if (lOption = 2) then
          begin
            if LPermanentOn then
            begin
              lShape.Cells[cPermanent].Formula         := '"FALSE"';
              RefreshText(lShape);
            end
            else
              lShape.Cells[cPermanent].Formula         := '"TRUE"';
          end;
          if (lOption = 3) then
            DeleteOutputTextLabels(lShape);
          if (lOption = 4) then
            DeleteRelatedOutputTextLabels(lShape,ANumber);
        finally
          lMenuDlg.Free;
        end;
      finally
        FreeAndNil(lOptionList);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ProcessVNVReservoirPenaltyRightClicked(AShapeName: string; ANumber: integer): WordBool;
const OPNAME = 'TVNVEventHandler.ProcessVNVTextRightClicked';
var
  lOptionList     : TStringList;
  lMenuDlg        : TVNVShapeMenuDlg;
  lOption         : integer;
  LPermanentOn    : boolean;
  lShape          : IVShape;
  lYieldModel : IYieldModel;
begin
  Result := FALSE;
  try

    lShape := GetFirstSelectedShape(mtWRYMResPenalty);
    lYieldModel := (FAppModules.Model as IYieldModel);
    if(lShape <> nil) and (lYieldModel <> nil) and (lShape.CellExists[cPermanent, 0] <> 0) then
    begin
      LPermanentOn := (lShape.Cells[cPermanent].Formula = '"TRUE"');
      lOptionList := TStringList.Create;
      try
        lOptionList.AddObject(FAppModules.Language.GetString('VNV.MenuCancelOption'),TObject(1));
        lOptionList.AddObject(FAppModules.Language.GetString('VNV.ViewReservoirPenalty'),TObject(1));
        if LPermanentOn then
          lOptionList.AddObject(FAppModules.Language.GetString('VNV.RemovePermenant'),TObject(1))
        else
          lOptionList.AddObject(FAppModules.Language.GetString('VNV.SetPermenant'),TObject(1));

        lMenuDlg := TVNVShapeMenuDlg.Create(nil);
        try
          lOption := lMenuDlg.ShowShapeMenuDialog(lOptionList);
          if (lOption = 1) then
            Result := lYieldModel.ViewInputConfigurationDialog(mdvnReservoirPenalty);

          if (lOption = 2) then
          begin
            if LPermanentOn then
            begin
              lShape.Cells[cPermanent].Formula         := '"FALSE"';
              RefreshText(lShape);
            end
            else
              lShape.Cells[cPermanent].Formula         := '"TRUE"';
          end;
        finally
          lMenuDlg.Free;
        end;
      finally
        FreeAndNil(lOptionList);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TVNVEventHandler.ProcessVNVNodeRightClicked (AShapeName : string;
                                                      ANumber    : integer) : WordBool;
const OPNAME = 'TVNVEventHandler.ProcessVNVNodeRightClicked';
var
  lOptionList     : TStringList;
  lMenuDlg        : TVNVShapeMenuDlg;
  lOption         : integer;
  lYieldModel     : IYieldModel;
  LPermanentColor : boolean;
  lShape          : IVShape;
begin
  Result := FALSE;
  try
    lYieldModel := FAppModules.Model as IYieldModel;
    lOptionList := TStringList.Create;
    try
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.MenuCancelOption'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.ViewNodeProperties'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.ViewNodeOutput'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.DeleteNodeFromScenario'),TObject(1));
      LPermanentColor := True;
      lShape := GetFirstSelectedShape(mtWRYMNode);
      if(lShape <> nil) and (lShape.CellExists[cPermanentColor, 0] <> 0) then
      begin
        LPermanentColor := (lShape.Cells[cPermanentColor].Formula = '"TRUE"');
        if LPermanentColor then
          lOptionList.AddObject(FAppModules.Language.GetString('VNV.PermanentColorOff'),TObject(1))
        else
          lOptionList.AddObject(FAppModules.Language.GetString('VNV.PermanentColorOn'),TObject(1));
      end;

      lMenuDlg := TVNVShapeMenuDlg.Create(nil);
      try
        lOption := lMenuDlg.ShowShapeMenuDialog(lOptionList);
        case lOption of
          1 : begin
                if (lYieldModel.YieldModelData as IYieldModelData).NetworkElementData.ReservoirList.NodeHasInflow(ANumber) then
                  Result := lYieldModel.ViewInputNodeWithInflowDialog(ANumber)
                else
                  Result := lYieldModel.ViewInputNodeWithoutInflowDialog(ANumber);
                if (Result) then
                  NodeHasChanged(AShapeName, ANumber);
              end;
          2: begin
               if (lYieldModel.YieldModelData as IYieldModelData).NetworkElementData.ReservoirList.NodeHasInflow(ANumber) then
                 Result := lYieldModel.ViewOutputNodeWithInflowDialog(ANumber)
               else
                 Result := lYieldModel.ViewOutputNodeWithoutInflowDialog(ANumber);
             end;
          3: Result := DeleteNode(ANumber);
          4 : begin
                if LPermanentColor then
                  lShape.Cells[cPermanentColor].Formula         := '"FALSE"'
                else
                  lShape.Cells[cPermanentColor].Formula         := '"TRUE"';
                Result := True;
              end;
        end;
      finally
        lMenuDlg.Free;
      end;
    finally
      FreeAndNil(lOptionList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.DeleteNode (ANumber : integer; ADBDelete : integer = -1) : WordBool;
const OPNAME = 'TVNVEventHandler.DeleteNode';
var
  lShape       : IVShape;
  lInflowShape : IVShape;
begin
  Result := FALSE;
  try
    lShape := FindShapeWithPropertyValue(mtWRYMNode, cNumber, IntToStr(ANumber));
    if (lShape <> nil) AND (MayChangeScenario) then
    begin
      if ADBDelete = -1 then
      begin
        lInflowShape := FindShapeWithParent(mtWRYMInflow, lShape.Name);
        if (lInflowShape <> nil) then
          Result := (FAppModules.Model as IYieldModel).DoDeleteNodeWithInflow(ANumber)
        else
          Result := (FAppModules.Model as IYieldModel).DoDeleteNodeWithoutInflow(ANumber);
      end;
      lShape.Delete;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ProcessVNVChannelRightClicked (AShapeName : string;
                                                         ANumber    : integer) : WordBool;
const OPNAME = 'TVNVEventHandler.ProcessVNVChannelRightClicked';
var
  lYieldModel                : IYieldModel;
  lOptionList                : TStringList;
  lMenuDlg                   : TVNVShapeMenuDlg;
  lOption                    : integer;
  lOptionTxt                 : string;
  lChannel                   : IGeneralFlowChannel;
  lProperties                : string;
  lOutput                    : string;
  lCreateMinMax              : string;
  lCreateLoss                : string;
  lCreateMinimum             : string;
  lCreateDiversion           : string;
  lCreatePumping             : string;
  lCreateInflow              : string;
  lCreateSpecifiedDemand     : string;
  lCreatePhysical            : string;
  lCreateIFR                 : string;
  lCreateWaterDemand         : string;
  lCreateMaster              : string;
  lDeleteMinMax              : string;
  lDeleteLoss                : string;
  lDeleteMinimum             : string;
  lDeleteDiversion           : string;
  lDeletePumping             : string;
  lDeleteInflow              : string;
  lDeleteSpecifiedDemand     : string;
  lDeletePhysical            : string;
  lDeleteIFR                 : string;
  lDeleteWaterDemand         : string;
  lDeleteMaster              : string;
  lDeleteChannel             : string;
  lSplit                     : string;
  lCancel                    : string;
  lCurtailment               : string;
  lDroughtRestriction        : string;
  lMayChange                 : boolean;
  lChannelHasChanged         : boolean;
  lShape                     : IVShape;
  LEnabled                   : integer;
begin
  Result := FALSE;
  try
    lMayChange := (FAppModules.User.UserRights in CUR_EditData) AND (NOT FAppModules.StudyArea.ScenarioLocked);

    lYieldModel := (FAppModules.Model as IYieldModel);
    lChannel    := lYieldModel.YieldModelData.NetworkElementData.ChannelList.ChannelByChannelNumber[ANumber];
    lOptionList := TStringList.Create;
    try
      lProperties             := FAppModules.Language.GetString('VNV.ViewChannelProperties');
      lOutput                 := FAppModules.Language.GetString('VNV.ViewChannelOutput');

      lCreateMinMax           := FAppModules.Language.GetString('MenuCaption.CreateMinMaxFlowFeature');
      lCreateLoss             := FAppModules.Language.GetString('MenuCaption.CreateLossFeature');
      lCreateMinimum          := FAppModules.Language.GetString('MenuCaption.CreateMinimumFlowFeature');
      lCreateDiversion        := FAppModules.Language.GetString('MenuCaption.CreateDiversionFeature');
      lCreatePumping          := FAppModules.Language.GetString('MenuCaption.CreatePumpingFeature');
      lCreateSpecifiedDemand  := FAppModules.Language.GetString('MenuCaption.CreateSpecifiedDemandFeature');
      lCreateInflow           := FAppModules.Language.GetString('MenuCaption.CreateSpecifiedInflowFeature');
      lCreatePhysical         := FAppModules.Language.GetString('MenuCaption.CreatePhysicalFlowConstraint');
      lCreateIFR              := FAppModules.Language.GetString('MenuCaption.CreateIFRFeature');
      lCreateWaterDemand      := FAppModules.Language.GetString('MenuCaption.CreateWaterDemandFeature');
      lCreateMaster           := FAppModules.Language.GetString('MenuCaption.CreateMasterControlFeature');

      lDeleteMinMax           := FAppModules.Language.GetString('MenuCaption.DeleteMinMaxFlowFeature');
      lDeleteLoss             := FAppModules.Language.GetString('MenuCaption.DeleteLossFeature');
      lDeleteMinimum          := FAppModules.Language.GetString('MenuCaption.DeleteMinimumFlowFeature');
      lDeleteDiversion        := FAppModules.Language.GetString('MenuCaption.DeleteDiversionFeature');
      lDeletePumping          := FAppModules.Language.GetString('MenuCaption.DeletePumpingFeature');
      lDeleteSpecifiedDemand  := FAppModules.Language.GetString('MenuCaption.DeleteSpecifiedDemandFeature');
      lDeleteInflow           := FAppModules.Language.GetString('MenuCaption.DeleteSpecifiedInflowFeature');
      lDeletePhysical         := FAppModules.Language.GetString('MenuCaption.DeletePhysicalFlowConstraint');
      lDeleteIFR              := FAppModules.Language.GetString('MenuCaption.DeleteIFRFeature');
      lDeleteWaterDemand      := FAppModules.Language.GetString('MenuCaption.DeleteWaterDemandFeature');
      lDeleteMaster           := FAppModules.Language.GetString('MenuCaption.DeleteMasterControlFeature');
      lCurtailment            := FAppModules.Language.GetString('MenuCaption.Curtailment');
      lDroughtRestriction     := FAppModules.Language.GetString('MenuCaption.DroughtRestriction');

      lDeleteChannel          := FAppModules.Language.GetString('VNV.DeleteChannelFromScenario');
      lSplit                  := FAppModules.Language.GetString('VNV.SplitChannel');
      lCancel                 := FAppModules.Language.GetString('VNV.MenuCancelOption');

      { Add items to pop-up menu depending on channeltype }
      lOptionList.AddObject(lCancel,TObject(1));
      lOptionList.AddObject(lProperties,TObject(1));
      lOptionList.AddObject(lOutput,TObject(1));

      if (lMayChange) then
      begin
        case lChannel.ChannelType of
         2 :  begin
                LEnabled := Ord(lYieldModel.YieldModelData.NetworkFeaturesData.MasterControlFeatureList.MasterControlFeatureCount > 1);
                lOptionList.AddObject(lDeleteMaster,TObject(LEnabled));
              end;
         5 : lOptionList.AddObject(lDeleteDiversion,TObject(1));
         6 : lOptionList.AddObject(lDeleteMinimum,TObject(1));
         7 : lOptionList.AddObject(lDeleteLoss,TObject(1));
         8 : // Min-max channel
            begin
              LEnabled := Ord((lChannel.PhysicalFlowConstraint = nil) AND (lChannel.IFRFeature = nil) AND (lChannel.WaterDemandFeature = nil));
              lOptionList.AddObject(lDeleteMinMax,TObject(LEnabled));
              LEnabled := Ord((lChannel.IFRFeature = nil) AND (lChannel.PhysicalFlowConstraint = nil));
              lOptionList.AddObject(lCreatePumping,TObject(LEnabled));
              lOptionList.AddObject(lCreateIFR,TObject(LEnabled));
              lOptionList.AddObject(lCreatePhysical,TObject(LEnabled));
              LEnabled := Ord(lChannel.WaterDemandFeature = nil);
              lOptionList.AddObject(lCreateWaterDemand,TObject(LEnabled));
            end;
         9 : lOptionList.AddObject(lDeletePumping,TObject(1));
        10 : lOptionList.AddObject(lDeleteInflow,TObject(1));
        11 : // Specified Demand channel
            begin
              LEnabled := Ord((lChannel.WaterDemandFeature = nil));
              lOptionList.AddObject(lDeleteSpecifiedDemand,TObject(LEnabled));
              lOptionList.AddObject(lCreateWaterDemand,TObject(LEnabled));
            end;
        12 : // General flow channel
            begin
              LEnabled := Ord(lChannel.PhysicalFlowConstraint = nil);
              lOptionList.AddObject(lCreateMinimum,TObject(LEnabled));
              lOptionList.AddObject(lCreateMinMax,TObject(LEnabled));
              lOptionList.AddObject(lCreateLoss,TObject(LEnabled));
              lOptionList.AddObject(lCreateSpecifiedDemand,TObject(LEnabled));
              lOptionList.AddObject(lCreateDiversion,TObject(LEnabled));
              LEnabled := Ord((lChannel.DownStreamNode <> nil) AND
                          (not (lChannel.DownStreamNode.ReservoirConfigurationData.NodeType in NodesWithoutInflowSet)));
              lOptionList.AddObject(lCreateInflow,TObject(LEnabled));
              lOptionList.AddObject(lCreatePhysical,TObject(LEnabled));
              LEnabled := Ord(lYieldModel.YieldModelData.NetworkFeaturesData.MasterControlFeatureList.MasterControlFeatureCount < 2);
              lOptionList.AddObject(lCreateMaster,TObject(LEnabled));
            end;
        else
        end;
        LEnabled := Ord(lChannel.PhysicalFlowConstraint <> nil);
        lOptionList.AddObject(lDeletePhysical,TObject(LEnabled));
        LEnabled := Ord(lChannel.IFRFeature <> nil);
        lOptionList.AddObject(lDeleteIFR,TObject(LEnabled));
        LEnabled := Ord(lChannel.WaterDemandFeature <> nil);
        lOptionList.AddObject(lDeleteWaterDemand,TObject(LEnabled));

        lOptionList.AddObject(lCurtailment,TObject(1));
        lOptionList.AddObject(lDroughtRestriction,TObject(1));
      end;

      LEnabled := Ord(lChannel.ChannelType <> 2);
      lOptionList.AddObject(lDeleteChannel,TObject(LEnabled));
      lOptionList.AddObject(lSplit,TObject(1));

      lChannel := nil;
      lChannelHasChanged := FALSE;
      lMenuDlg := TVNVShapeMenuDlg.Create(nil);
      try
        lOption := lMenuDlg.ShowShapeMenuDialog(lOptionList);
        lOptionTxt := '';
        if(lOption >= 0) and (lOption < lOptionList.Count) then
          lOptionTxt := lOptionList.Strings[lOption];
        {Process option selected from pop-up menu}
        if (lOptionTxt = lProperties) then
        begin
          lChannelHasChanged := TRUE;
          Result := lYieldModel.ViewInputChannelDialog(ANumber);
        end
        else
        if (lOptionTxt = lOutput) then
          Result := lYieldModel.ViewOutputChannelDialog(ANumber)
        else
        if (lOptionTxt = lDeleteChannel) then
          Result := DeleteChannel(ANumber)
        else
        if (lOptionTxt = lSplit) then
          Result := SplitChannel(ANumber)
        else
        if (lOptionTxt = lCreateMinimum) then
        begin
          lChannelHasChanged := TRUE;
          lYieldModel.DoCreateMinimumFlowFeature(ANumber);
        end
        else
        if (lOptionTxt = lCreateMinMax) then
        begin
          lChannelHasChanged := TRUE;
          lYieldModel.DoCreateMinMaxFlowFeature(ANumber);
        end
        else
        if (lOptionTxt = lCreateLoss) then
        begin
          lChannelHasChanged := TRUE;
          lYieldModel.DoCreateLossFeature(ANumber);
        end
        else
        if (lOptionTxt = lCreatePumping) then
        begin
          lChannelHasChanged := TRUE;
          lYieldModel.DoCreatePumpingFeature(ANumber);
        end
        else
        if (lOptionTxt = lCreateDiversion) then
        begin
          lChannelHasChanged := TRUE;
          lYieldModel.DoCreateDiversionFeature(ANumber);
        end
        else
        if (lOptionTxt = lCreateInflow) then
        begin
          lChannelHasChanged := TRUE;
          lYieldModel.DoCreateSpecifiedInflowFeature(ANumber);
        end
        else
        if (lOptionTxt = lCreateSpecifiedDemand) then
        begin
          lChannelHasChanged := TRUE;
          lYieldModel.DoCreateSpecifiedDemandFeature(ANumber);
        end
        else
        if (lOptionTxt = lCreatePhysical) then
        begin
          lChannelHasChanged := TRUE;
          lYieldModel.DoCreatePhysicalFlowConstraint(ANumber);
        end
        else
        if (lOptionTxt = lCreateIFR) then
        begin
          lChannelHasChanged := TRUE;
          lYieldModel.DoCreateIFRFeature(ANumber,ifrtNone);
        end
        else
        if (lOptionTxt = lCreateWaterDemand) then
        begin
          lChannelHasChanged := TRUE;
          lYieldModel.DoCreateWaterDemandFeature(ANumber);
        end
        else
        if (lOptionTxt = lCreateMaster) then
        begin
          lChannelHasChanged := TRUE;
          lYieldModel.DoCreateMasterControlFeature(ANumber);
        end
        else
        if (lOptionTxt = lDeleteMinMax)  OR (lOptionTxt = lDeleteLoss) OR
           (lOptionTxt = lDeleteMinimum) OR (lOptionTxt = lDeleteDiversion) OR
           (lOptionTxt = lDeleteInflow)  OR (lOptionTxt = lDeleteSpecifiedDemand) then
        begin
          lChannelHasChanged := TRUE;
          lYieldModel.DoConvertChannel(ANumber);
        end
        else
        if (lOptionTxt = lDeletePumping) then
        begin
          lChannelHasChanged := TRUE;
          lYieldModel.DoDeletePumpingFeature(ANumber);
        end
        else
        if (lOptionTxt = lDeleteIFR) then
        begin
          lChannelHasChanged := TRUE;
          lYieldModel.DoDeleteIFRFeature(ANumber);
        end
        else
        if (lOptionTxt = lDeletePhysical) then
        begin
          lChannelHasChanged := TRUE;
          lYieldModel.DoDeletePhysicalFlowConstraint(ANumber);
        end
        else
        if (lOptionTxt = lDeleteMaster) then
        begin
          lChannelHasChanged := TRUE;
          lYieldModel.DoDeleteMasterControlFeature(ANumber);
        end
        else
        if (lOptionTxt = lDeleteWaterDemand) then
        begin
          lChannelHasChanged := TRUE;
          lYieldModel.DoDeleteWaterDemandFeature(ANumber);
        end
        else
        if (lOptionTxt = lCurtailment) then
        begin
          ViewCurtialment(AShapeName,ANumber);
        end
        else
        if (lOptionTxt = lDroughtRestriction) then
        begin
          ViewChannelDroughtRestriction(AShapeName,ANumber);
        end;

        lYieldModel := nil;
        if (lChannelHasChanged) then
        begin
          if (lOptionTxt <> lProperties) then
          begin
            lShape := FindShapeWithName(AShapeName);
            if (lShape <> nil) then
              lShape.Cells[cColourChanged].Formula := '"N"';
          end;
          ChannelHasChanged(AShapeName, ANumber);
        end;
      finally
        lMenuDlg.Free;
      end;
    finally
      FreeAndNil(lOptionList);
    end
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.DeleteChannel (ANumber : integer; ADBDelete : integer = -1) : WordBool;
const OPNAME = 'TVNVEventHandler.DeleteChannel';
var
  lShape : IVShape;
begin
  Result := FALSE;
  try
    lShape := FindShapeWithPropertyValue(mtWRYMChannel, cNumber, IntToStr(ANumber));
    if (lShape <> nil) AND (MayChangeScenario) then
    begin
      lShape.Delete;
      if ADBDelete = -1 then
        Result := (FAppModules.Model as IYieldModel).DoDeleteChannel(ANumber);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.SplitChannel (ANumber : integer) : WordBool;
const OPNAME = 'TVNVEventHandler.SplitChannel';
var
  lShape        : IVShape;
  lNewChannelNr : integer;
  lNewNodeNr    : integer;
  lDownNodeNr   : integer;
  lChannel      : IGeneralFlowChannel;
  lNewChannel   : IGeneralFlowChannel;
  lNewNode      : IReservoirData;
  lXPin         : double;
  lYPin         : double;
  lMaster       : IVMaster;
  lNewShape     : IVShape;
  lXBegin       : double;
  lXEnd         : double;
  lYBegin       : double;
  lYEnd         : double;
  lXVal         : double;
  lYVal         : double;
begin
  Result := FALSE;
  try
    lShape := FindShapeWithPropertyValue(mtWRYMChannel, cNumber, IntToStr(ANumber));
    if (lShape <> nil) AND (MayChangeScenario) then
    begin
      lChannel := (FAppModules.Model as IYieldModel).YieldModelData.NetworkElementData.
                    ChannelList.ChannelByChannelNumber[ANumber];
      if (lChannel <> nil) then
      begin
        lNewNode := (FAppModules.Model as IYieldModel).DoCreateNodeWithoutInflow;
        if (lNewNode <> nil) then
        begin
          lNewNodeNr  := lNewNode.ReservoirConfigurationData.ReservoirIdentifier;
          lDownNodeNr := lChannel.DownStreamNodeNumber;
          lNewChannel := (FAppModules.Model as IYieldModel).DoCreateChannel(lNewNodeNr, lDownNodeNr);
          if (lNewChannel <> nil) then
          begin
            lNewChannelNr := lNewChannel.ChannelNumber;
            lChannel.DownStreamNodeNumber := lNewNodeNr;
            lXPin   := StrToFloatDef(UnMM(UnQuote(lShape.Cells[cPinX].ResultStr[visMillimeters])),0);
            lYPin   := StrToFloatDef(UnMM(UnQuote(lShape.Cells[cPinY].ResultStr[visMillimeters])),0);
            lXBegin := StrToFloatDef(UnMM(UnQuote(lShape.Cells[cBeginX].ResultStr[visMillimeters])),0);
            lXEnd   := StrToFloatDef(UnMM(UnQuote(lShape.Cells[cEndX].ResultStr[visMillimeters])),0);
            lYBegin := StrToFloatDef(UnMM(UnQuote(lShape.Cells[cBeginY].ResultStr[visMillimeters])),0);
            lYEnd   := StrToFloatDef(UnMM(UnQuote(lShape.Cells[cEndY].ResultStr[visMillimeters])),0);
            lShape.XYToPage(0, 0, lXVal, lYVal);
            // Drop node shape
            lMaster := FindMaster(mtWRYMNode);
            if (lMaster <> nil) then
            begin
              lNewShape := FVisioApp.ActivePage.Drop(lMaster, lXVal, lYVal);
              lNewShape.Cells[cNumber].Formula  := '"' + IntToStr(lNewNodeNr) + '"';
              lNewShape.Cells[cPinX].Formula    := '"' + FloatToStrF(lXPin, ffFixed, 12, 2) + 'mm"';
              lNewShape.Cells[cPinY].Formula    := '"' + FloatToStrF(lYPin, ffFixed, 12, 2) + 'mm"';
              lNewShape.Cells[cVersion].Formula := '"' + CThisVersion + '"';
            end;
            // Drop channel shape
            lMaster := FindMaster(mtWRYMChannel);
            if (lMaster <> nil) then
            begin
              lNewShape := FVisioApp.ActivePage.Drop(lMaster, lXVal, lYVal);
              lNewShape.Cells[cNumber].Formula  := '"' + IntToStr(lNewChannelNr) + '"';
              lNewShape.Cells[cBeginX].Formula  := '"' + FloatToStrF(lXBegin, ffFixed, 12, 2) + 'mm"';
              lNewShape.Cells[cEndX].Formula    := '"' + FloatToStrF(lXEnd, ffFixed, 12, 2) + 'mm"';
              lNewShape.Cells[cBeginY].Formula  := '"' + FloatToStrF(lYBegin, ffFixed, 12, 2) + 'mm"';
              lNewShape.Cells[cEndY].Formula    := '"' + FloatToStrF(lYEnd, ffFixed, 12, 2) + 'mm"';
              lNewShape.Cells[cVersion].Formula := '"' + CThisVersion + '"';
              lNewShape.BringToFront;
            end;
            SnapChannelToNodes(lShape, ANumber);
            SnapChannelToNodes(lNewShape, lNewChannelNr);
            Result := TRUE;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.NodeHasChanged (AShapeName : string;
                                           ANumber    : integer);
const OPNAME = 'TVNVEventHandler.NodeHasChanged';
var
  lShape      : IVShape;
  lShapesLst  : TStringList;
  lIndex      : integer;
  lShapeName  : string;
begin
  try
    lShape := FindShapeWithName(AShapeName);
    if (lShape <> nil) then
      RefreshNode(lShape, ANumber);

    lShape := FindShapeWithParent(mtWRYMInflow, AShapeName);
    if (lShape <> nil) then
    begin
      RefreshInflow(lShape, ANumber);
    end;

    lShapesLst := TStringList.Create;
    try
      FindTextShapesWithParent(AShapeName, lShapesLst);
      for lIndex := 0 to lShapesLst.Count - 1 do
      begin
        lShapeName := lShapesLst.Strings[lIndex];
        lShape     := FindShapeWithName(lShapeName);
        if (lShape <> nil) then
          RefreshText(lShape);
      end;
    finally
      FreeAndNil(lShapesLst);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.ReservoirHasChanged (AShapeName : string;
                                                ANumber    : integer);
const OPNAME = 'TVNVEventHandler.ReservoirHasChanged';
var
  lShape      : IVShape;
  lShapesLst  : TStringList;
  lIndex      : integer;
  lShapeName  : string;
begin
  try
    lShape := FindShapeWithName(AShapeName);
    if (lShape <> nil) then
      RefreshReservoir(lShape, ANumber);

    lShape := FindShapeWithParent(mtWRYMResPenalty, AShapeName);
    if (lShape <> nil) then
      RefreshReservoirPenalty(lShape, ANumber)
    else
    begin
      lShape := FindShapeWithParent(mtWRYMResPenaltyExt, AShapeName);
      if (lShape <> nil) then
        RefreshReservoirPenalty(lShape, ANumber)
    end;

    lShape := FindInflowShapeByReservoirNumber(ANumber);
    if (lShape <> nil) then
      RefreshInflow(lShape, ANumber);
    lShape := FindSFRAShapeByReservoirNumber(ANumber);
    if (lShape <> nil) then
      RefreshInflow(lShape, ANumber);

    lShapesLst := TStringList.Create;
    try
      FindTextShapesWithParent(AShapeName, lShapesLst);
      for lIndex := 0 to lShapesLst.Count - 1 do
      begin
        lShapeName := lShapesLst.Strings[lIndex];
        lShape     := FindShapeWithName(lShapeName);
        if (lShape <> nil) then
          RefreshText(lShape);
      end;
    finally
      FreeAndNil(lShapesLst);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.ReservoirPenaltyHasChanged (AShapeName : string;
                                                       ANumber    : integer);
const OPNAME = 'TVNVEventHandler.ReservoirPenaltyHasChanged';
var
  lShape       : IVShape;
  lPenaltyNr   : integer;
  lPageIndex   : integer;
  lShapeIndex  : integer;
  lPage        : IVPage;
  lReservoirNr : integer;
  lOtherNr     : integer;
begin
  try
    lShape := FindShapeWithName(AShapeName);
    if (lShape <> nil) then
    begin
      lPenaltyNr :=0;
      if (Pos(mtWRYMReservoir, AShapeName) > 0) then
        lPenaltyNr := StrToIntDef(UnQuote(lShape.Cells[cPenalty].Formula),0)
      else
      if (Pos(mtWRYMResPenalty, AShapeName) > 0) or (Pos(mtWRYMResPenaltyExt, AShapeName) > 0)then
        lPenaltyNr := StrToIntDef(UnQuote(lShape.Cells[cPenaltyNumber].Formula),0);
      if (lPenaltyNr <> 0) then
      begin
        for lPageIndex := 1 to FVisioApp.ActiveDocument.Pages.Count do
        begin
          lPage := FVisioApp.ActiveDocument.Pages[lPageIndex];
          for lShapeIndex := 1 to lPage.Shapes.Count  do
          begin
            lShape := lPage.Shapes.Item[lShapeIndex];
            if (Pos(PChar(mtWRYMResPenalty), lShape.Name) > 0) or (Pos(PChar(mtWRYMResPenaltyExt), lShape.Name) > 0)then
            begin
              lReservoirNr := StrToIntDef(UnQuote(lShape.Cells[cNumber].Formula),0);
              lOtherNr     := StrToIntDef(UnQuote(lShape.Cells[cPenaltyNumber].Formula),0);
              if (lOtherNr = lPenaltyNr) then
                RefreshReservoirPenalty(lShape, lReservoirNr);
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.ChannelHasChanged (AShapeName : string;
                                              ANumber    : integer);
const OPNAME = 'TVNVEventHandler.ChannelHasChanged';
var
  lShape      : IVShape;
  lShapesLst  : TStringList;
  lIndex      : integer;
  lShapeName  : string;
begin
  try
    lShape := FindShapeWithName(AShapeName);
    if (lShape <> nil) then
      RefreshChannel(lShape, ANumber);

    lShape := FindShapeWithParent(mtWRYMChanPenalty, AShapeName);
    if (lShape <> nil) then
      RefreshChannelPenalty(lShape, ANumber);

    lShapesLst := TStringList.Create;
    try
      FindTextShapesWithParent(AShapeName, lShapesLst);
      for lIndex := 0 to lShapesLst.Count - 1 do
      begin
        lShapeName := lShapesLst.Strings[lIndex];
        lShape     := FindShapeWithName(lShapeName);
        if (lShape <> nil) then
          RefreshText(lShape);
      end;
    finally
      FreeAndNil(lShapesLst);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.ChannelPenaltyHasChanged (AShapeName : string;
                                                     ANumber    : integer);
const OPNAME = 'TVNVEventHandler.ChannelPenaltyHasChanged';
var
  lShape      : IVShape;
  lPenaltyNr  : integer;
  lPageIndex  : integer;
  lShapeIndex : integer;
  lPage       : IVPage;
  lChannelNr  : integer;
  lOtherNr    : integer;
begin
  try
    lShape := FindShapeWithName(AShapeName);
    if (lShape <> nil) then
    begin
      lPenaltyNr := 0;
      if (Pos(mtWRYMChannel, AShapeName) > 0) then
        lPenaltyNr := StrToIntDef(UnQuote(lShape.Cells[cPenalty].Formula),0)
      else
      if (Pos(mtWRYMChanPenalty, AShapeName) > 0) then
        lPenaltyNr := StrToIntDef(UnQuote(lShape.Cells[cPenaltyNumber].Formula),0);

      if (lPenaltyNr <> 0) then
      begin
        for lPageIndex := 1 to FVisioApp.ActiveDocument.Pages.Count do
        begin
          lPage := FVisioApp.ActiveDocument.Pages[lPageIndex];
          for lShapeIndex := 1 to lPage.Shapes.Count  do
          begin
            lShape := lPage.Shapes.Item[lShapeIndex];
            if (Pos(PChar(mtWRYMChanPenalty), lShape.Name) > 0) then
            begin
              lChannelNr := StrToIntDef(UnQuote(lShape.Cells[cNumber].Formula),0);
              lOtherNr   := StrToIntDef(UnQuote(lShape.Cells[cPenaltyNumber].Formula),0);
              if (lOtherNr = lPenaltyNr) then
                RefreshChannelPenalty(lShape, lChannelNr);
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.NodeCatchmentHasChanged (AShapeName : string; ANumber    : integer);
const OPNAME = 'TVNVEventHandler.NodeCatchmentHasChanged';
var
  lShape       : IVShape;
  lCatchmentNr : integer;
  lPageIndex   : integer;
  lShapeIndex  : integer;
  lPage        : IVPage;
  lNodeNr      : integer;
  lOtherNr     : integer;
begin
  try
    lShape := FindShapeWithName(AShapeName);
    if (lShape <> nil) then
    begin
      lCatchmentNr := StrToIntDef(UnQuote(lShape.Cells[cCatchmentNumber].Formula),0);

      for lPageIndex := 1 to FVisioApp.ActiveDocument.Pages.Count do
      begin
        lPage := FVisioApp.ActiveDocument.Pages[lPageIndex];
        for lShapeIndex := 1 to lPage.Shapes.Count  do
        begin
          lShape := lPage.Shapes.Item[lShapeIndex];
          if (Pos(PChar(mtWRYMInflow), lShape.Name) > 0) then
          begin
            lNodeNr  := StrToIntDef(UnQuote(lShape.Cells[cNumber].Formula),0);
            lOtherNr := StrToIntDef(UnQuote(lShape.Cells[cCatchmentNumber].Formula),0);
            if (lOtherNr = lCatchmentNr) then
              RefreshInflow(lShape, lNodeNr);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.PowerPlantHasChanged(AShapeName: string;ANumber: integer);
const OPNAME = 'TVNVEventHandler.PowerPlantHasChanged';
var
  lShape      : IVShape;
  lShapesLst  : TStringList;
  lIndex      : integer;
  lShapeName  : string;
  lPowerPlant : IPowerPlant;
begin
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then
    begin
      MessageDlg(FAppModules.Language.GetString('VNV.PowerPlantNoModelSupport'), mtError, [mbOK], 0);
      Exit;
    end;

    lPowerPlant := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.PowerPlantList.PowerPlantByID[ANumber];
    if (lPowerPlant <> nil) then
    begin
      if(lPowerPlant.PowerChannel <> nil) then
      begin
        lShape := FindChannelShapeByChannelNumber(lPowerPlant.PowerChannel.ChannelNumber);
        if (lShape <> nil) then
        begin
          SnapChannelToNodes(lShape, lPowerPlant.PowerChannel.ChannelNumber);
          ChannelHasChanged(lShape.Name,lPowerPlant.PowerChannel.ChannelNumber);
        end;
      end;
      if(lPowerPlant.SpillChannel <> nil) then
      begin
        lShape := FindChannelShapeByChannelNumber(lPowerPlant.SpillChannel.ChannelNumber);
        if (lShape <> nil) then
        begin
          SnapChannelToNodes(lShape, lPowerPlant.SpillChannel.ChannelNumber);
          ChannelHasChanged(lShape.Name,lPowerPlant.SpillChannel.ChannelNumber);
        end;
      end;
    end;

    lShape := FindShapeWithName(AShapeName);
    if (lShape <> nil) then
      RefreshPowerPlant(lShape, ANumber);

    lShapesLst := TStringList.Create;
    try
      FindTextShapesWithParent(AShapeName, lShapesLst);
      for lIndex := 0 to lShapesLst.Count - 1 do
      begin
        lShapeName := lShapesLst.Strings[lIndex];
        lShape     := FindShapeWithName(lShapeName);
        if (lShape <> nil) then
          RefreshText(lShape);
      end;
    finally
      FreeAndNil(lShapesLst);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ProcessVNVDoubleClicked (AShapeName : string;
                                                   ANumber    : integer) : WordBool;
const OPNAME = 'TVNVEventHandler.ProcessVNVDoubleClicked';
var
  lYieldModel   : IYieldModel;
  LSequenceType : string;
  lElementID    : integer;
begin
  Result := FALSE;
  try
    lYieldModel   := (FAppModules.Model as IYieldModel);
    LSequenceType  := lYieldModel.YieldModelData.RunConfigurationData.RunSequenceType;

    if (Pos(mtWRYMReservoir, AShapeName) > 0) then
    begin
      Result := lYieldModel.ViewInputReservoirDialog(ANumber);
      if (Result) then
        ReservoirHasChanged(AShapeName, ANumber);
    end
    else if (Pos(mtWRYMMine, AShapeName) > 0) then
    begin
      try
        Result := ViewMineProperties(AShapeName,ANumber);
        //Result := lYieldModel.ViewInputMineDialog(ANumber);
        if (Result) then
          MineHasChanged(AShapeName, ANumber);
      except
      end;
    end
    else if (Pos(mtWRYMGroundWater, AShapeName) > 0) then
    begin
        Result := lYieldModel.ViewInputGroundwaterDialog(ANumber);
        if (Result) then
          GroundWaterHasChanged(AShapeName, ANumber);
    end
    else if (Pos(mtWRYMNode, AShapeName) > 0) then
    begin
      if (lYieldModel.YieldModelData as IYieldModelData).NetworkElementData.ReservoirList.NodeHasInflow(ANumber) then
        Result := lYieldModel.ViewInputNodeWithInflowDialog(ANumber)
      else
        Result := lYieldModel.ViewInputNodeWithoutInflowDialog(ANumber);
      if (Result) then
        NodeHasChanged(AShapeName, ANumber);
    end
    else if (Pos(mtWRYMChannel, AShapeName) > 0) then
    begin
      Result := lYieldModel.ViewInputChannelDialog(ANumber);
      if (Result) then
        ChannelHasChanged(AShapeName, ANumber);
    end
    else if (Pos(mtWRYMResPenalty, AShapeName) > 0) or (Pos(mtWRYMResPenaltyExt, AShapeName) > 0) then
    begin
      Result := lYieldModel.ViewInputConfigurationDialog(mdvnReservoirPenalty);
      if (Result) then
        ReservoirPenaltyHasChanged(AShapeName, ANumber);
    end
    else if (Pos(mtWRYMChanPenalty, AShapeName) > 0) then
    begin
      Result := lYieldModel.ViewInputConfigurationDialog(mdvnChannelPenalties);
      if (Result) then
        ChannelPenaltyHasChanged(AShapeName, ANumber);
    end
    else if (Pos(mtWRYMInflow, AShapeName) > 0) then
    begin
      Result := lYieldModel.ViewInputConfigurationDialog(mdvnCatchmentProportions);
      if (Result) then
        NodeCatchmentHasChanged(AShapeName, ANumber);
    end
    else if (Pos(mtWRYMIrrBlock, AShapeName) > 0) then
    begin
      Result := lYieldModel.ViewInputIrrigationBlockDialog(ANumber);
      if (Result) then
        IrrigationBlockHasChanged(AShapeName, ANumber);
    end
    else if (Pos(mtWRYMIrrArea, AShapeName) > 0) then
    begin
      Result := lYieldModel.ViewInputIrrigationAreaDialog(ANumber);
      if (Result) then
        IrrigationAreaHasChanged(AShapeName, ANumber);
    end
    else if (Pos(mtWRYMWetland, AShapeName) > 0) then
    begin
      Result := lYieldModel.ViewInputWetlandDialog(ANumber);
      if (Result) then
        WetlandHasChanged(AShapeName, ANumber);
    end
    else if (Pos(mtWRYMOutputSelector, AShapeName) > 0) then
    begin
      Result := (lYieldModel.YieldModelData as IYieldModelData).OutputData.
                   ShowDataSelectionDialog(NullInteger,votNone,osdNetworkVisualiser,btNone,ovtNone);
      if (FOutputSelector <> nil) then         
        RefreshOutputSelector(FOutputSelector)
    end
    else if (Pos(mtWRYMSubCatchment, AShapeName) > 0) then
    begin
      lElementID := ShowSubCatchmentDialog(ANumber, sfrdtViewProperties);

      if (lElementID <> -1) then
      begin
        Result := lYieldModel.ViewInputStreamFlowReductionDialog(lElementID);
        RefreshAllReservoirs;
        //RefreshSubCatchment(FindShapeWithName(AShapeName), ANumber);
      end;
    end
    else if (Pos(mtWRYMDemandCentre, AShapeName) > 0) then
    begin
        Result := lYieldModel.ViewInputDemandCentreDialog(ANumber);
        if (Result) then
          DemandCentreHasChanged(AShapeName, ANumber);
    end
    else if (Pos(mtWRYMPowerPlant, AShapeName) > 0) then
    begin
        Result := lYieldModel.ViewInputPowerPlantDialog(ANumber);
        if (Result) then
          PowerPlantHasChanged(AShapeName, ANumber);
    end
    else if (Pos(mtModel, AShapeName) > 0) then
    begin
      Result := lYieldModel.ViewInputConfigurationDialog(mdvnRunConfiguration);
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.MayChangeScenario : Boolean;
const OPNAME = 'TVNVEventHandler.MayChangeScenario';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    if (NOT (FAppModules.User.UserRights in CUR_EditData)) then
    begin
      lMessage := FAppModules.Language.GetString('VNV.NoUserRights');
      MessageDlg(lMessage, mtError, [mbOK], 0);
    end
    else if (FAppModules.StudyArea.ScenarioLocked) then
    begin
      lMessage := FAppModules.Language.GetString('VNV.ScenarioLocked');
      MessageDlg(lMessage, mtError, [mbOK], 0);
    end
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.StudyDataHasChanged (AContext   : TChangeContext;
                                               AFieldName : string;
                                               AOldValue  : string;
                                               ANewValue  : string): boolean;
const OPNAME = 'TVNVEventHandler.StudyDataHasChanged';
var
  lIndex     : integer;
  lShape     : IVShape;
  lShapeName : string;
  lElementID : integer;
begin
  if FBlockStudyDataChanges  then
  begin
    Result := True;
    Exit;
  end;

  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if(AContext in [sdccDelete,sdccAdd]) then
    begin
      //RefreshDocument(FVisioApp.ActiveDocument);
    end
    else
    if(AContext = sdccEdit) then
    begin
      if(AFieldName = 'UpNodeNumber') then Exit;
      if(AFieldName = 'DownNodeNumber') then Exit;

      GetSelectionData;
      if (FOutputSelector <> nil) then
        RefreshOutputSelector(FOutputSelector);
      for lIndex := 0 to FOutputList.Count - 1 do
      begin
        lShapeName := FOutputList.Strings[lIndex];
        lShape     := FindShapeWithName(lShapeName);
        if (lShape <> nil) then
          RefreshText(lShape);
      end;
      for lIndex := 0 to FChanPenaltyList.Count - 1 do
      begin
        lElementID := StrToIntDef(FChanPenaltyList.Strings[lIndex],0);
        lShape     := FindShapeWithPropertyValue(mtWRYMChanPenalty, cNumber, IntToStr(lElementID));
        if (lShape <> nil) then
          RefreshChannelPenalty(lShape, lElementID);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.GetSelectionData;
const OPNAME = 'TVNVEventHandler.GetSelectionData';
var
  LDataselection : IOutputDataSelection;
begin
  try
    LDataselection := (FAppModules.Model.ModelData as IYieldModelData).OutputData.GetSelection;

    if (LDataselection <> nil) then
    begin
      FLoadCase     := LDataSelection.LoadCase;
      FSequence     := LDataSelection.Sequence;
      FMonthIndex   := LDataSelection.Month;
      FUnits        := LDataSelection.Units;
      FValueType    := LDataSelection.ValueType;
      FTimeStep     := LDataSelection.TimeStep;
      FHighLight    := LDataSelection.Highlight;
      FDisplayMonth := LDataSelection.DisplayMonth;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TVNVEventHandler.CheckUpgradeIrrBlock(AShape: IVShape);
const OPNAME = 'TVNVEventHandler.CheckUpgradeIrrBlock';
begin
  try
    //FBufferIrrBlockList.Add(AShape.Name);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.CheckUpgradeWetland(AShape: IVShape);
const OPNAME = 'TVNVEventHandler.CheckUpgradeWetland';
begin
  try
    //FBufferIrrBlockList.Add(AShape.Name);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.CheckUpgradeDemandCentre(AShape: IVShape);
const OPNAME = 'TVNVEventHandler.CheckUpgradeDemandCentre';
begin
  try
    //FBufferIrrBlockList.Add(AShape.Name);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TVNVEventHandler.RefreshIrrigationBlock(AShape: IVShape;AElementID: integer);
const OPNAME = 'TVNVEventHandler.RefreshIrrigationBlock';
var
  lIrrigationBlock : IIrrigationBlock;
  lIrrigationName  : string;
  lKeepColor       : boolean;
begin
  try
    if(AShape = nil) or (AElementID = 0) or (AElementID = NullInteger) then Exit;
    if (FAppModules.StudyArea.ModelVersion <> '7')  then
    begin
      //MessageDlg(FAppModules.Language.GetString('VNV.IrrigationBlockNoModelSupport'));
      Exit;
    end;

    lKeepColor := (AShape.CellExists[cPermanentColor, 0] <> 0)  and (AShape.Cells[cPermanentColor].Formula = '"TRUE"');
    lIrrigationBlock := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[AElementID];
    if (lIrrigationBlock = nil) then
    begin
      if not lKeepColor then
        AShape.CellsSRC[visSectionObject, visRowFill, visFillForegnd].FormulaU := IntToStr(visRed);
    end
    else
    begin
      if not lKeepColor then
        AShape.CellsSRC[visSectionObject, visRowFill, visFillForegnd].FormulaU := IntToStr(visYellow);
      //FVisioApp.ActiveWindow.Windows.ItemFromID[visWinIDCustProp].Visible := TRUE;
      lIrrigationName := lIrrigationBlock.BlockName;
      AShape.Cells[cName].Formula := '"' + lIrrigationName + '"';;
      AShape.Cells[cRow1Action].Formula := 'RUNMACRO("ThisDocument.ProcessVNVSpecial 1")';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.RefreshWetland(AShape: IVShape;AElementID: integer);
const OPNAME = 'TVNVEventHandler.RefreshWetland';
var
  lWetland     : IWetland;
  lWetlandName : string;
  lKeepColor   : boolean;
begin
  try
    if(AShape = nil) or (AElementID = 0) or (AElementID = NullInteger) then Exit;
    if (FAppModules.StudyArea.ModelVersion <> '7')  then
    begin
      //MessageDlg(FAppModules.Language.GetString('VNV.WetlandNoModelSupport'));
      Exit;
    end;

    lKeepColor := (AShape.CellExists[cPermanentColor, 0] <> 0)  and (AShape.Cells[cPermanentColor].Formula = '"TRUE"');
    lWetland := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.WetlandList.WetlandByNodeNumber[AElementID];
    if (lWetland = nil) then
    begin
      if not lKeepColor then
        AShape.CellsSRC[visSectionObject, visRowFill, visFillForegnd].FormulaU := IntToStr(visRed);
    end
    else
    begin
      if not lKeepColor then
        AShape.CellsSRC[visSectionObject, visRowFill, visFillForegnd].FormulaU := IntToStr(visCyan);
      //FVisioApp.ActiveWindow.Windows.ItemFromID[visWinIDCustProp].Visible := TRUE;
      lWetlandName := lWetland.Name;
      AShape.Cells[cName].Formula := '"' + lWetlandName + '"';;
      AShape.Cells[cRow1Action].Formula := 'RUNMACRO("ThisDocument.ProcessVNVSpecial 1")';
      //FWetlandList.Add(IntToStr(lWetland.ReservoirDetails.ReservoirConfigurationData.ReservoirIdentifier));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.RefreshDemandCentre(AShape: IVShape;AElementID: integer);
const OPNAME = 'TVNVEventHandler.RefreshDemandCentre';
var
  lDemandCentre      : IYMDemandCentre;
begin
  try
    if(AShape = nil) or (AElementID = 0) or (AElementID = NullInteger) then Exit;
    if (FAppModules.StudyArea.ModelVersion <> '7')  then  Exit;

    lDemandCentre := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByNodeNumber[AElementID];         // YMDemandCentreByID[AElementID];

    RemoveDeletedDemandCentreChannels(AElementID);

    RefreshDemandCentreChannels(AShape, lDemandCentre);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.IrrigationBlockShapeAdded(const AShape: IVShape);
const OPNAME = 'TVNVEventHandler.IrrigationBlockShapeAdded';
var
  LIrrigationBlockNodeNumber  : integer;
  lTxtLabel                   : IVShape;
  lYieldModelData             : IYieldModelData;
  lIrrBlock                   : IIrrigationBlock;
begin
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then
    begin
      MessageDlg(FAppModules.Language.GetString('VNV.IrrigationBlockNoModelSupport'), mtError, [mbOK], 0);
      Exit;
    end;
    if (FBufferIrrBlockList.Count > 0) then
      { Upgraded Irrigation }
      FBufferIrrBlockList.Delete(0)
    else
    begin
      if (AShape.CellExists[cNumber, 0] <> 0) AND (UnQuote(AShape.Cells[cNumber].Formula) <> '') then
      begin
        { Copied Irrigation }
        LIrrigationBlockNodeNumber  := StrToIntDef(UnQuote(AShape.Cells[cNumber].Formula),0);
      end
      else
      begin
        { New Irrigation - from stencil }
        LIrrigationBlockNodeNumber := ShowIrrigationBlockDialog;
        if (LIrrigationBlockNodeNumber <> -1) then
          AShape.Cells[cNumber].Formula  := '"' + IntToStr(LIrrigationBlockNodeNumber) + '"';
      end;
      AShape.Text := IntToStr(LIrrigationBlockNodeNumber);
      if (LIrrigationBlockNodeNumber = -1) then
        AShape.Delete
      else
      begin
        RefreshIrrigationBlock(AShape, LIrrigationBlockNodeNumber);
        lTxtLabel := AddTextLabel(AShape, nvtName);
        RefreshText(lTxtLabel);

        lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
        lIrrBlock        := lYieldModelData.NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[LIrrigationBlockNodeNumber];
        if(lIrrBlock <> nil) then
        begin
          if(lIrrBlock.DiversionChannel <> nil) then
          begin
            AddIrrigationChannel(AShape,lIrrBlock.DiversionChannel);
          end;
          if(lIrrBlock.ReturnFlowChannel <> nil) then
          begin
            AddIrrigationChannel(AShape,lIrrBlock.ReturnFlowChannel);
          end;
        end;
        FIrrigationList.Add(IntToStr(LIrrigationBlockNodeNumber));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.AddIrrigationChannel(AIrrBlockShape : IVShape;AChannel : IGeneralFlowChannel): IVShape;
const OPNAME = 'TVNVEventHandler.AddIrrigationChannel';
var
  lXVal,
  lYVal        : double;
  lMaster      : IVMaster;
  lTxtLabel    : IVShape;
  LChannelShape: IVShape;
begin
  Result := nil;
  try
    if(AChannel = nil) then Exit;
    AIrrBlockShape.XYToPage(0, 0, lXVal, lYVal);
    lMaster := FindMaster(mtWRYMChannel);
    if (lMaster <> nil) then
    begin
      LChannelShape := FVisioApp.ActivePage.Drop(lMaster, lXVal, lYVal);
      LChannelShape.Text := IntToStr(AChannel.ChannelNumber);
      FBufferChannelList.Add(IntToStr(AChannel.ChannelNumber));
      LChannelShape.Cells[cNumber].Formula  := '"' + IntToStr(AChannel.ChannelNumber) + '"';
      LChannelShape.Cells[cVersion].Formula := '"' + CThisVersion + '"';
      RefreshChannel(LChannelShape,AChannel.ChannelNumber);
      SnapChannelToNodes(LChannelShape, AChannel.ChannelNumber);
      lTxtLabel := AddTextLabel(LChannelShape, nvtName);
      RefreshText(lTxtLabel);
      FChannelList.Add(IntToStr(AChannel.ChannelNumber));
      Result := LChannelShape;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.IrrigationBlockHasChanged(AShapeName: string;ANumber: integer);
const OPNAME = 'TVNVEventHandler.IrrigationBlockHasChanged';
var
  lShape      : IVShape;
  lShapesLst  : TStringList;
  lIndex      : integer;
  lShapeName  : string;
  lIrrigationBlock : IIrrigationBlock;
begin
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then
    begin
      MessageDlg(FAppModules.Language.GetString('VNV.IrrigationBlockNoModelSupport'), mtError, [mbOK], 0);
      Exit;
    end;

    lIrrigationBlock := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[ANumber];
    if (lIrrigationBlock <> nil) then
    begin
      if(lIrrigationBlock.DiversionChannel <> nil) then
      begin
        lShape := FindChannelShapeByChannelNumber(lIrrigationBlock.DiversionChannel.ChannelNumber);
        if (lShape <> nil) then
        begin
          SnapChannelToNodes(lShape, lIrrigationBlock.DiversionChannel.ChannelNumber);
          ChannelHasChanged(lShape.Name,lIrrigationBlock.DiversionChannel.ChannelNumber);
        end;
      end;
      if(lIrrigationBlock.ReturnFlowChannel <> nil) then
      begin
        lShape := FindChannelShapeByChannelNumber(lIrrigationBlock.ReturnFlowChannel.ChannelNumber);
        if (lShape <> nil) then
        begin
          SnapChannelToNodes(lShape, lIrrigationBlock.ReturnFlowChannel.ChannelNumber);
          ChannelHasChanged(lShape.Name,lIrrigationBlock.ReturnFlowChannel.ChannelNumber);
        end;
      end;
    end;


    lShape := FindShapeWithName(AShapeName);
    if (lShape <> nil) then
      RefreshIrrigationBlock(lShape, ANumber);

    lShapesLst := TStringList.Create;
    try
      FindTextShapesWithParent(AShapeName, lShapesLst);
      for lIndex := 0 to lShapesLst.Count - 1 do
      begin
        lShapeName := lShapesLst.Strings[lIndex];
        lShape     := FindShapeWithName(lShapeName);
        if (lShape <> nil) then
          RefreshText(lShape);
      end;
    finally
      FreeAndNil(lShapesLst);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.WetlandShapeAdded(const AShape: IVShape);
const OPNAME = 'TVNVEventHandler.WetlandShapeAdded';
var
  LWetlandID  : integer;
  lTxtLabel   : IVShape;
  lYieldModelData : IYieldModelData;
  lWetland       : IWetland;
begin
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then
    begin
      MessageDlg(FAppModules.Language.GetString('VNV.WetlandNoModelSupport'), mtError, [mbOK], 0);
      Exit;
    end;

    if (FBufferWetlandList.Count > 0) then
      { Upgraded Wetland }
      FBufferWetlandList.Delete(0)
    else
    begin
      if (AShape.CellExists[cNumber, 0] <> 0) AND (UnQuote(AShape.Cells[cNumber].Formula) <> '') then
      begin
        { Copied Wetland }
        LWetlandID  := StrToIntDef(UnQuote(AShape.Cells[cNumber].Formula),0);
      end
      else
      begin
        { New Wetland - from stencil }
        LWetlandID := ShowWetlandDialog;
        if (LWetlandID <> -1) then
          AShape.Cells[cNumber].Formula  := '"' + IntToStr(LWetlandID) + '"';
      end;
      AShape.Text := IntToStr(LWetlandID);
      if (LWetlandID = -1) then
        AShape.Delete
      else
      begin
        RefreshWetland(AShape, LWetlandID);
        lTxtLabel := AddTextLabel(AShape, nvtName);
        RefreshText(lTxtLabel);

        lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
        lWetland        := lYieldModelData.NetworkFeaturesData.WetlandList.WetlandByNodeNumber[LWetlandID];
        if(lWetland <> nil) then
        begin
          if(lWetland.ReservoirDetails <> nil) then
            AddInflowSymbol(AShape, lWetland.ReservoirDetails.ReservoirConfigurationData.ReservoirIdentifier);
          if(lWetland.InflowChannel <> nil) then
          begin
            AddWetlandChannel(AShape,lWetland.InflowChannel);
          end;
          if(lWetland.OutflowChannel <> nil) then
          begin
            AddWetlandChannel(AShape,lWetland.OutflowChannel);
          end;
          FWetlandList.Add(IntToStr(LWetlandID));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TVNVEventHandler.AddWetlandChannel(AWetlandShape : IVShape;AChannel : IGeneralFlowChannel): IVShape;
const OPNAME = 'TVNVEventHandler.AddWetlandChannel';
var
  lXVal,
  lYVal        : double;
  lMaster      : IVMaster;
  lTxtLabel    : IVShape;
  LChannelShape: IVShape;
begin
  Result := nil;
  try
    if(AChannel = nil) then Exit;
    AWetlandShape.XYToPage(0, 0, lXVal, lYVal);
    lMaster := FindMaster(mtWRYMChannel);
    if (lMaster <> nil) then
    begin
      LChannelShape := FVisioApp.ActivePage.Drop(lMaster, lXVal, lYVal);
      LChannelShape.Text := IntToStr(AChannel.ChannelNumber);
      FBufferChannelList.Add(IntToStr(AChannel.ChannelNumber));
      LChannelShape.Cells[cNumber].Formula  := '"' + IntToStr(AChannel.ChannelNumber) + '"';
      LChannelShape.Cells[cVersion].Formula := '"' + CThisVersion + '"';
      RefreshChannel(LChannelShape,AChannel.ChannelNumber);
      SnapChannelToNodes(LChannelShape, AChannel.ChannelNumber);
      lTxtLabel := AddTextLabel(LChannelShape, nvtName);
      RefreshText(lTxtLabel);
      FChannelList.Add(IntToStr(AChannel.ChannelNumber));
      Result := LChannelShape;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.WetlandHasChanged(AShapeName: string;ANumber: integer);
const OPNAME = 'TVNVEventHandler.WetlandHasChanged';
var
  lShape      : IVShape;
  lShapesLst  : TStringList;
  lIndex      : integer;
  lShapeName  : string;
  lWetland    : IWetland;
begin
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then
    begin
      MessageDlg(FAppModules.Language.GetString('VNV.WetlandNoModelSupport'), mtError, [mbOK], 0);
      Exit;
    end;

    lWetland := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.WetlandList.WetlandByNodeNumber[ANumber];
    if (lWetland <> nil) then
    begin
      if(lWetland.InflowChannel <> nil) then
      begin
        lShape := FindChannelShapeByChannelNumber(lWetland.InflowChannel.ChannelNumber);
        if (lShape <> nil) then
        begin
          SnapChannelToNodes(lShape, lWetland.InflowChannel.ChannelNumber);
          ChannelHasChanged(lShape.Name,lWetland.InflowChannel.ChannelNumber);
        end;
      end;
      if(lWetland.OutflowChannel <> nil) then
      begin
        lShape := FindChannelShapeByChannelNumber(lWetland.OutflowChannel.ChannelNumber);
        if (lShape <> nil) then
        begin
          SnapChannelToNodes(lShape, lWetland.OutflowChannel.ChannelNumber);
          ChannelHasChanged(lShape.Name,lWetland.OutflowChannel.ChannelNumber);
        end;
      end;
    end;

    lShape := FindShapeWithName(AShapeName);
    if (lShape <> nil) then
      RefreshWetland(lShape, ANumber);

    lShapesLst := TStringList.Create;
    try
      FindTextShapesWithParent(AShapeName, lShapesLst);
      for lIndex := 0 to lShapesLst.Count - 1 do
      begin
        lShapeName := lShapesLst.Strings[lIndex];
        lShape     := FindShapeWithName(lShapeName);
        if (lShape <> nil) then
          RefreshText(lShape);
      end;
    finally
      FreeAndNil(lShapesLst);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.DemandCentreHasChanged(AShapeName: string;ANumber: integer);
const OPNAME = 'TVNVEventHandler.DemandCentreHasChanged';
var
  lShape        : IVShape;
  lDemandCentre : IYMDemandCentre;
  lKeepColor    : boolean;
begin
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then
    begin
      MessageDlg(FAppModules.Language.GetString('VNV.DemandCentreNoModelSupport'), mtError, [mbOK], 0);
      Exit;
    end;

    lDemandCentre := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByNodeNumber[ANumber];
    if (lDemandCentre = nil) then
    begin
       lShape := FindShapeWithName(AShapeName);
       if (lShape <> nil) then
       begin
         lKeepColor := (lShape.CellExists[cPermanentColor, 0] <> 0)  and (lShape.Cells[cPermanentColor].Formula = '"TRUE"');
         if not lKeepColor then
           lShape.CellsSRC[visSectionObject, visRowFill, visFillForegnd].FormulaU := IntToStr(visRed);
       end;
    end;


    lShape := FindShapeWithName(AShapeName);
    if (lShape <> nil) then
    begin
      RefreshDemandCentre(lShape, ANumber);
      lShape := FindTextWithParent(lShape.Name);
      if (lShape <> nil) then
          RefreshText(lShape);
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.DemandCentreShapeAdded(const AShape: IVShape);
const OPNAME = 'TVNVEventHandler.DemandCentreShapeAdded';
var
  lDemandCentreID : integer;
  lTxtLabel       : IVShape;
begin
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then
    begin
      MessageDlg(FAppModules.Language.GetString('VNV.DemandCentreNoModelSupport'), mtError, [mbOK], 0);
      Exit;
    end;

    if (FBufferDemandCentreList.Count > 0) then
      { Upgraded Demand Centre }
      FBufferDemandCentreList.Delete(0)
    else
    begin
      if (AShape.CellExists[cNumber, 0] <> 0) AND (UnQuote(AShape.Cells[cNumber].Formula) <> '') then
      begin
        { Copied Demand Centre }
        lDemandCentreID  := StrToIntDef(UnQuote(AShape.Cells[cNumber].Formula),0);
      end
      else
      begin
        { New Demand Centre - from stencil }
        lDemandCentreID := ShowDemandCentreDialog;
        if (lDemandCentreID <> -1) then
          AShape.Cells[cNumber].Formula  := '"' + IntToStr(lDemandCentreID) + '"';
      end;
      AShape.Text := IntToStr(lDemandCentreID); 
      if (lDemandCentreID = -1) then
        AShape.Delete
      else
      begin
        RefreshDemandCentre(AShape, lDemandCentreID);

        lTxtLabel := AddTextLabel(AShape, nvtName);
        RefreshText(lTxtLabel);

        FDemandCentreList.Add(IntToStr(lDemandCentreID));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.InitialiseDrawing(const ADoc : IVDocument);
const OPNAME = 'TVNVEventHandler.InitialiseDrawing';
var
  LGroupName,
  LDrawingName,
  LFileName: string;
  LPos: integer;
  LSQLAgent : TNetworkVisualiserDataSQLAgent;
begin
  try
    LGroupName   := '';
    LDrawingName := '';

    LFileName := ADoc.Name;
    Delete(LFileName, Pos('.', LFileName), Length(LFileName));
    LPos := Pos('-', LFileName);
    if LPos > 0 then
    begin
      LGroupName := Copy(LFileName, 1, LPos-1);
      LDrawingName := Copy(LFileName, LPos+1, Length(LFileName));
    end;
    LSQLAgent := TNetworkVisualiserDataSQLAgent.Create(FAppModules);
    try
      LSQLAgent.PopulateDrawing(FDrawing,LGroupName,LDrawingName);
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ProcessVNVToggleGIS: boolean;
const OPNAME = 'TVNVEventHandler.ProcessVNVToggleGIS';
var
  LSQLAgent  : TNetworkVisualiserDataSQLAgent;
begin
  Result := False;
  try
    if (FDrawing.DrawingGroupID <> 0) and (FDrawing.DrawingID <> 0) then
    begin
      FDrawing.GISMode := not FDrawing.GISMode;
      LSQLAgent := TNetworkVisualiserDataSQLAgent.Create(FAppModules);
      try
        LSQLAgent.UpdateDrawingGIS(FDrawing);
        if FDrawing.GISMode then
          ShowGIS
        else
          HideGIS;
      finally
        LSQLAgent.Free;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.GetGISPage: IVPage;
const OPNAME = 'TVNVEventHandler.GetGISPage';
var
  LIndex: integer;
begin
  Result := nil;
  try
    for LIndex := 1 to FVisioDoc.Pages.Count do
    begin
      if (FVisioDoc.Pages.Item[LIndex].Name = 'GIS') then
      begin
         Result := FVisioDoc.Pages.Item[LIndex];
         Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ShowGIS: boolean;
const OPNAME = 'TVNVEventHandler.ShowGIS';
      cVisioDrPage = 1;

{var
  LIndex          : integer;
  LCurrentPage,
  LGISPage        : IVPage;
  LFileName       : string;

  LImg            : TImage;
  LImgW,
  LImgH           : Double;

  LShape          : IVShape;
  lElementID      : integer;
  LXYCoord        : istrings; }

begin
  Result := False;
  try
    {LGISPage := GetGISPage;
    if(LGISPage <> nil) then
    begin
      LCurrentPage := FVisioDoc.Pages.Item[1];
      LCurrentPage.BackPageFromName := LGISPage.Name;
    end;}

    {if FDrawing.GISMode  and (FVisioApp.ActivePage <> nil) and (FVisioApp.ActivePage.Background = 0) then
    begin
      if(FGISViewer = nil) then
        CreateGISViewer;
      LCurrentPage :=  FVisioApp.ActivePage;

      // The True parameter indicated that the local file path
      // should be used and not the path contained in the glf

      LFileName := ExtractFilePath(ApplicationExeName) + 'Network Diagrams\'+
      ChopCharacters(FAppModules.StudyArea.StudyAreaCode)+'\'+ChopCharacters(FAppModules.StudyArea.SubAreaCode)
      +'\'+ChopCharacters(FAppModules.StudyArea.ScenarioCode)+'\'+FAppModules.StudyArea.ScenarioCode+'.glf';
      UpdateGISShapeFilePath(LFileName);
      FGISViewer.Layers_LoadLayout(LFileName,False);
      FGISViewer.Control_RefreshAll;
      Application.ProcessMessages;

      FGISViewer.Extent_Set(FAppModules.StudyArea.TopLeftCoord,FAppModules.StudyArea.TopRightCoord, FAppModules.StudyArea.BottomLeftCoord, FAppModules.StudyArea.BottomRightCoord);
      FGISViewer.Visible := True;
      FGISForm.SendToBack;
      FGISForm.Show;
      FGISForm.Hide;

      LFileName := ExtractFilePath(ApplicationExeName) + 'Logs\GIS.bmp';
      FGISViewer.Output_ExportMap(1, LFileName, cGISScaleFactor);
      Application.ProcessMessages;

      LImg := nil;
      LImg := TImage.Create(nil);
      LImg.AutoSize := True;
      LImg.Picture.LoadFromFile(LFileName);
      LImgW := LImg.Width / cGISScaleFactor;
      LImgH := LImg.Height / cGISScaleFactor;
      FreeAndNil(LImg);

      LGISPage := GetGISPage;
      if(LGISPage <>  nil) then
      begin
        for LIndex := 1 to LGISPage.Shapes.Count do
          LGISPage.Shapes.Item[LIndex].Delete;
      end
      else
        LGISPage := FVisioDoc.Pages.Add;
      LGISPage.Name := 'GIS';
      LGISPage.Background := 1;
      LGISPage.Import(LFileName);

      if ((LImgW < 10) or (LImgH < 10)) then
      begin
        MessageDlg(FAppModules.Language.GetString('GISMapError'), mtError, [mbOk], 0);
        exit;
      end;

      LGISPage.PageSheet.Cells['PageWidth'].Formula := FloatToStr(LImgW) + ' mm';
      LGISPage.PageSheet.Cells['PageHeight'].Formula := FloatToStr(LImgH) + ' mm';

      //set the properties for the background image
      LGISPage.Shapes.Item[1].Cells['Width'].Formula := FloatToStr(LImgW) + ' mm';
      LGISPage.Shapes.Item[1].Cells['Height'].Formula := FloatToStr(LImgH) + ' mm';

      LGISPage.Shapes.Item[1].Cells['PinX'].Formula := FloatToStr(LImgW/2) + ' mm';
      LGISPage.Shapes.Item[1].Cells['PinY'].Formula :=  FloatToStr(LImgH/2) + ' mm';

      LCurrentPage.PageSheet.Cells['PageWidth'].Formula := FloatToStr(LImgW) + ' mm';
      LCurrentPage.PageSheet.Cells['PageHeight'].Formula := FloatToStr(LImgH) + ' mm';

      LCurrentPage.BackPage := LGISPage.Name;
      FVisioApp.ActiveWindow.Page := LCurrentPage.Name;

      FPageWidth := LImgW;
      FPageHeight := LImgH;

      // Load the view settings for the study selection mode.
      LXYCoord := FGISViewer.Map_LonLatToXY(FAppModules.StudyArea.BottomLeftCoord,FAppModules.StudyArea.BottomRightCoord);
      FMapWidth := StrToFloat(LXYCoord.Item[0]);
      FMapHeight := StrToFloat(LXYCoord.Item[1]);

      FDoNotRecalcCoordList.Clear;

      for LIndex := 1 to FVisioApp.ActivePage.Shapes.Count do
      begin
        LShape := FVisioApp.ActivePage.Shapes[LIndex];
        if ((Pos(PChar(mtWRYMReservoir), LShape.Name) > 0) or
            (Pos(PChar(mtWRYMGroundWater), LShape.Name) > 0) OR
             (Pos(PChar(mtWRYMNode), LShape.Name) > 0)) then
        begin
          lElementID := StrToIntDef(UnQuote(LShape.Cells[cNumber].Formula),-1);
          GISPopulateDoNotRecalcCoordsList(lElementID);
          GISSetXYPosFromLonLat(LShape, lElementID);
        end;
      end;
    end }
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.HideGIS: boolean;
const OPNAME = 'TVNVEventHandler.HideGIS';
begin
  Result := False;
  try
    {FDoNotRecalcCoordList.Clear;
    FVisioApp.ActivePage.BackPage := '';
    }Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TVNVEventHandler.GISConvertLonLatToXY(AGISViewer : TGisViewerX;ALon,ALat: double; var AX,AY:double): boolean;
const OPNAME = 'TVNVEventHandler.GISConvertLonLatToXY';
var
  pGISCoord  : IStrings;
  m_LO       : double;
  m_Ellipse  : integer;

begin
  Result := False;
  try
    AX := 0.0;
    AY := 0.0;
    if(AGISViewer <> nil) then
    begin
      pGISCoord := AGISViewer.Map_LOXYExtent;
      m_LO      := AGISViewer.Map_GetLO;
      m_Ellipse := AGISViewer.Map_GetEllipsoid();

      pGISCoord := nil;
      pGISCoord := AGISViewer.Map_ConvertLonLatToXY(ALon, ALat, m_LO, TxEllipsoid(m_Ellipse));
      AX := -1.0 * StrToFloat(pGISCoord.Item[1]);
      AY := StrToFloat(pGISCoord.Item[0]);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.Get_ReservoirFromShape(AShape: IVShape; AElementID: integer): IReservoirData;
const OPNAME = 'TVNVEventHandler.Get_ReservoirFromShape';
var
  lYieldModelData   : IYieldModelData;
begin
  Result := nil;
  try
    lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
    if (Pos(PChar(mtWRYMReservoir), AShape.Name) > 0) then
      Result := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[AElementID]
    else
    if (Pos(PChar(mtWRYMNode), AShape.Name) > 0) then
      Result := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[AElementID]
    else
    if (Pos(PChar(mtWRYMIrrBlock), AShape.Name) > 0) then
      Result := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[AElementID]
    else
    if (Pos(PChar(mtWRYMIrrArea), AShape.Name) > 0) then
      Result := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[AElementID]
    else
    if (Pos(PChar(mtWRYMWetland), AShape.Name) > 0) then
      Result := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[AElementID]
    else
    if (Pos(PChar(mtWRYMDemandCentre), AShape.Name) > 0) then
      Result := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[AElementID]
    else
    if (Pos(PChar(mtWRYMMine), AShape.Name) > 0) then
      Result := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[AElementID]
    else
    if (Pos(PChar(mtWRYMGroundWater), AShape.Name) > 0) then
      Result := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[AElementID];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.GISCalculateLonLatOfShape (AShape     : IVShape;
                                                      AElementID : integer;
                                                      AGisCalcLonOrLat : TGISCalcLonOrLat);

const OPNAME = 'TVNVEventHandler.GISCalculateLonLatOfShape';
var
  lReservoirOrNode : IReservoirData;
  lElementIdx      : Integer;
  lPinX,
  lPinY,
  lXFactor,
  lYFactor,
  lXCoord,
  lYCoord          : double;
  LonLatCoord      : istrings;
  lKeepColor       : boolean;
begin
  try
    if (AElementID = -1) then
      exit;
    lElementIdx := FDoNotRecalcCoordList.IndexOf(IntToStr(AElementID));
    if (lElementIdx > -1) then
    begin
      FDoNotRecalcCoordList.Delete(lElementIdx);
    end
    else
    begin
      //get the reservoir or node
      lReservoirOrNode := Get_ReservoirFromShape(AShape,AElementID);
      if (lReservoirOrNode <> nil) then
      begin
        LXCoord := lReservoirOrNode.ReservoirConfigurationData.XCoord;
        LYCoord := lReservoirOrNode.ReservoirConfigurationData.YCoord;
        if ((FDrawing.GISMode) and (FGISViewer <> nil)) then
        begin
          //the fomula will return a string with <space>mm appended to it. remove the mm part
          lPinX := StrToFloat(Copy(AShape.Cells[cPinX].Formula, 1, Pos(' ', AShape.Cells[cPinX].Formula)));
          lPinY := StrToFloat(Copy(AShape.Cells[cPinY].Formula, 1, Pos(' ', AShape.Cells[cPinY].Formula)));
          lPinY := FPageHeight - lPinY;  //flip it around a, as in visio y=0 is in the left bottom cornerl

          lXFactor := (lPinX / FPageWidth) * FMapWidth;
          lYFactor := (lPinY / FPageHeight) * FMapHeight;

          LonLatCoord := FGISViewer.Map_XYToLonLat(Round(lXFactor), Round(lYFactor));

          LXCoord := StrToFloat(LonLatCoord.Item[0]);
          LYCoord := StrToFloat(LonLatCoord.Item[1]);

          lKeepColor := (AShape.CellExists[cPermanentColor, 0] <> 0)  and (AShape.Cells[cPermanentColor].Formula = '"TRUE"');
          if (Pos(PChar(mtWRYMReservoir), AShape.Name) > 0) and (not lKeepColor)then
              AShape.CellsSRC[visSectionObject, visRowFill, visFillForegnd].FormulaU := IntToStr(visCyan)
          else
          if (Pos(PChar(mtWRYMGroundWater), AShape.Name) > 0) and (not lKeepColor) then
              AShape.CellsSRC[visSectionObject, visRowFill, visFillForegnd].FormulaU := IntToStr(visBlue)
          else
            if (Pos(PChar(mtWRYMNode), AShape.Name) > 0) and (not lKeepColor) then
              AShape.CellsSRC[visSectionObject, visRowFill, visFillForegnd].FormulaU := IntToStr(visGreen);
        end;

        if (AGISCalcLonOrLat = gisCalcLon) or (AGISCalcLonOrLat = gisCalcBoth)  then
          lReservoirOrNode.ReservoirConfigurationData.XCoord := LXCoord;

        if (AGISCalcLonOrLat = gisCalcLat) or (AGISCalcLonOrLat = gisCalcBoth)  then
            lReservoirOrNode.ReservoirConfigurationData.YCoord := LYCoord;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TVNVEventHandler.GISSetXYPosFromLonLat(AShape     : IVShape;
                                                 AElementID : integer);
const OPNAME = 'TVNVEventHandler.GISSetXYPosFromLonLat';
//function calculate and positions the node or reservoir's xy position on the screen, according to the lon lat coords
var
  lYieldModelData : IYieldModelData;
  lReservoir      : IReservoirData;

  lXFactor,
  lYFactor        :  double;

  LMapXYCoord     : istrings;
  lKeepColor      : boolean;
begin
  try
    if (AElementID = -1) then
      Exit;
    //get the reservoir or node
    lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
    lReservoir := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[AElementID];
    lKeepColor := (AShape.CellExists[cPermanentColor, 0] <> 0)  and (AShape.Cells[cPermanentColor].Formula = '"TRUE"');

    if (lReservoir <> nil) then
    begin
      if (lReservoir.ReservoirConfigurationData.XCoord <> 0) and
         (lReservoir.ReservoirConfigurationData.YCoord <> 0) then
      begin
        LMapXYCoord := FGISViewer.Map_LonLatToXY(lReservoir.ReservoirConfigurationData.XCoord, lReservoir.ReservoirConfigurationData.YCoord);

        if (LMapXYCoord <> nil) and (LMapXYCoord.Count = 2) then
        begin
          lXFactor := (FPageWidth / FMapWidth) * StrToFloat(LMapXYCoord.Item[0]);
          lYFactor := (FPageHeight / FMapHeight) * StrToFloat(LMapXYCoord.Item[1]);

          if (LXFactor < 0) or (LYFactor < 0) or
             (StrToIntDef(LMapXYCoord.Item[0], -1) > FMapWidth) or (StrToIntDef(LMapXYCoord.Item[1], -1) > FMapHeight) then
          begin
            if not lKeepColor then
              AShape.CellsSRC[visSectionObject, visRowFill, visFillForegnd].FormulaU := IntToStr(visDarkYellow);
          end
          else
          begin
              AShape.Cells[cPinX].Formula := FloatToStr(lXFactor) + ' mm';
              AShape.Cells[cPinY].Formula := FloatToStr(FPageHeight - lYFactor) + ' mm';  //flip it arounda, as in visio y=0 is in the left bottom cornerl
          end;
        end; // if (LMapXYCoord <> nil) and (LMapXYCoord.Count = 2) then
      end
      else
      begin
        if not lKeepColor then
          AShape.CellsSRC[visSectionObject, visRowFill, visFillForegnd].FormulaU := IntToStr(visDarkYellow);  //x=0 y=0
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TVNVEventHandler.CheckExistingWithCoords(AShape     : IVShape;
                                                  AElementID : integer) : Boolean;
const OPNAME = 'TVNVEventHandler.CheckExistingWithCoords';
//
var
  lYieldModelData : IYieldModelData;
  lReservoir      : IReservoirData;
  lKeepColor      : boolean;
begin
  Result := False;
  try
    //get the reservoir or node
    lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
    lReservoir := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[AElementID];
    lKeepColor := (AShape.CellExists[cPermanentColor, 0] <> 0)  and (AShape.Cells[cPermanentColor].Formula = '"TRUE"');

    if (lReservoir <> nil) then
    begin
      if (lReservoir.ReservoirConfigurationData.XCoord <> 0) and
         (lReservoir.ReservoirConfigurationData.YCoord <> 0) then
        Result := True
      else
      begin
        GISCalculateLonLatOfShape(AShape,AElementID,gisCalcBoth);
        if (lReservoir.ReservoirConfigurationData.XCoord <> 0) and
           (lReservoir.ReservoirConfigurationData.YCoord <> 0) then
          Result := True;
        if (not Result) and (not lKeepColor) then
            AShape.CellsSRC[visSectionObject, visRowFill, visFillForegnd].FormulaU := IntToStr(visDarkYellow);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.GISPopulateDoNotRecalcCoordsList(AElementID : Integer);
const OPNAME = 'TVNVEventHandler.GISPopulateDoNotRecalcCoordsList';
var
  lIDx  :  Integer;
begin
  try
    if AElementID = -1 then
      exit;

    lIDx  := FDoNotRecalcCoordList.IndexOf(IntToStr(AElementID));
    while lIDx <> -1 do  //delete existing
    begin
      FDoNotRecalcCoordList.Delete(lIDx);
      lIDx  := FDoNotRecalcCoordList.IndexOf(IntToStr(AElementID));
    end;
    FDoNotRecalcCoordList.Add(IntToStr(AElementID));  //x
    FDoNotRecalcCoordList.Add(IntToStr(AElementID));  //y
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.AddSFRASubCatchmentText(AParent  : IVShape; ASFRs : String) : IVShape;
const OPNAME = 'TVNVEventHandler.AddSFRASubCatchmentText';
var
  lXParent    : Double;
  lYParent    : Double;
  lXChild     : Double;
  lYChild     : Double;
  lShape      : IVShape;
  lMaster     : IVMaster;
  lYShift     : double;
  lXShift     : double;

  lSFRItems   : TStringList;
  ARes        : String;
  AIndex      : Integer;

  lSFRList    : IStreamFlowReductionList;
  lSFR        : IStreamFlowReduction;
begin
  Result := nil;
  try
    if (AParent <> nil) then
    begin
      AParent.XYToPage(0, 0, lXParent, lYParent);
      //lYShift := 0.3;
      //lXShift := 1.6;
      lXShift := 0.3;
      lYShift := 0.5;
      lMaster := FindMaster(mtWRYMText);
      if (lMaster <> nil) then
      begin
        lShape := FVisioApp.ActivePage.Drop(lMaster, lXParent + lXShift, lYParent + lYShift);
        lShape.Name := mtWRYMSubCatchmentTxt + FindNumber(AParent.Name);
        FBufferTextList.Add(lShape.Name);
        lShape.Name := mtWRYMSubCatchmentTxt + FindNumber(AParent.Name);
        FVisioApp.ActiveWindow.DeselectAll;
        FVisioApp.ActiveWindow.Select(AParent, visSelect);
        FVisioApp.ActiveWindow.Select(AParent, visSelect);
        FVisioApp.ActiveWindow.Selection.Align(visHorzAlignCenter, visVertAlignNone, False);
        FVisioApp.ActiveWindow.DeselectAll;

        lXParent := StrToFloatDef(UnMM(UnQuote(AParent.Cells[cPinX].ResultStr[visMillimeters])),0);
        lYParent := StrToFloatDef(UnMM(UnQuote(AParent.Cells[cPinY].ResultStr[visMillimeters])),0);
        lXChild  := StrToFloatDef(UnMM(UnQuote(AParent.Cells[cPinX].Formula)),0);
        lYChild  := StrToFloatDef(UnMM(UnQuote(AParent.Cells[cPinY].Formula)),0);


        lShape.Cells[cParent].Formula       := '"' + AParent.Name + '"';
        lShape.Cells[cParentXDiff].Formula  := '"' + FloatToStrF(lXParent - lXChild, ffFixed, 12, 2) + '"';
        lShape.Cells[cParentYDiff].Formula  := '"' + FloatToStrF(lYParent - lYChild, ffFixed, 12, 2) + '"';
        lShape.Cells[cParentXMoved].Formula := '"' + IntToStr(0) + '"';
        lShape.Cells[cParentYMoved].Formula := '"' + IntToStr(0) + '"';
        lShape.Cells[cVersion].Formula      := '"' + CThisVersion + '"';

        lSFRItems := TStringList.Create;
        lSFRItems.Clear;
        lSFRItems.CommaText := ASFRs;

        lSFRList := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.StreamFlowReductionList;

        ARes := '';
        for AIndex := 0 to lSFRItems.Count - 1 do
        begin
          lSFR := lSFRList.StreamFlowReductionByID[StrToIntDef(lSFRItems.Strings[AIndex], -1)];
          if (lSFR <> nil) then
            ARes := ARes + lSFR.SFRDescription + ' - ' + FloatToStr(lSFR.CoveredArea) + 'sq km.';
          if AIndex < lSFRItems.Count - 1 then
            ARes := ARes + #13#10;
        end;

        lShape.Text := ARes + #13#10 + ' '; //jkw dodgy
        lShape.CellsSRC[visSectionCharacter, visRowCharacter, visCharacterColor].FormulaU := IntToStr(visDarkGreen);

        FreeAndNil(lSFRItems);

//        RefreshInflow(lShape, AElementID);
        FVisioApp.ActiveWindow.DeselectAll;
        Result := lShape;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.FindNumber(AComponentName  : String)  :  String;
const OPNAME = 'TVNVEventHandler.FindNumber';
var
  LIndex,
  lNrPos    :  Integer;
  LNum      : string;
begin
  Result := '';
  try
    lNrPos := Pos('.', AComponentName);
    if(lNrPos > 0) then
      Result := Copy(AComponentName, lNrPos +1, 3)
    else
    begin
      LNum := '';
      for LIndex := Length(AComponentName) downto 1 do
      begin
        if CharInSet(AComponentName[LIndex],['0','1','2','3','4','5','6','7','8','9']) then
          LNum := AComponentName[LIndex] + LNum
        else
          Break;
      end;
      Result := LNum;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.AddDemandCentreReclamationPlantChannel(ADemandCentreShape : IVShape;AChannel : IGeneralFlowChannel) : IVShape;
const OPNAME = 'TVNVEventHandler.AddDemandCentreReclamationPlantChannel';
var
  lXVal,
  lYVal        : double;
  lMaster      : IVMaster;
  lTxtLabel    : IVShape;
  LChannelShape: IVShape;
  LWidth       : Real;
begin
  Result := nil;
  try
    if(AChannel = nil) then Exit;
    ADemandCentreShape.XYToPage(0, 0, lXVal, lYVal);
    lMaster := FindMaster(mtWRYMChannel);
    if (lMaster <> nil) then
    begin
      LChannelShape := FVisioApp.ActivePage.Drop(lMaster, lXVal, lYVal);
      LChannelShape.Text := IntToStr(AChannel.ChannelNumber);
      lWidth := StrToFloat(UnMM(UnQuote(LChannelShape.Cells[cEndX].Formula))) + 30;
      LChannelShape.Cells[cEndX].Formula := FloatToStr(lWidth) + ' mm';

      FBufferChannelList.Add(IntToStr(AChannel.ChannelNumber));
      LChannelShape.Cells[cNumber].Formula  := '"' + IntToStr(AChannel.ChannelNumber) + '"';
      LChannelShape.Cells[cVersion].Formula := '"' + CThisVersion + '"';
      RefreshChannel(LChannelShape,AChannel.ChannelNumber);
      SnapChannelToNodes(LChannelShape, AChannel.ChannelNumber);
      lTxtLabel := AddTextLabel(LChannelShape, nvtName);
      RefreshText(lTxtLabel);
      //lTxtLabel.Text := FAppModules.Language.GetString('VNV.DemandCentreReclamationChannel') + ' ' + lTxtLabel.Text;

      FChannelList.Add(IntToStr(AChannel.ChannelNumber));
      Result := LChannelShape;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.AddDemandCentreConsumptiveUseChannel(ADemandCentreShape : IVShape;AChannel : IGeneralFlowChannel) : IVShape;
const OPNAME = 'TVNVEventHandler.AddDemandCentreConsumptiveUseChannel';
var
  lXVal,
  lYVal        : double;
  lMaster      : IVMaster;
  lTxtLabel    : IVShape;
  LChannelShape: IVShape;
  LWidth       : Real;  
begin
  Result := nil;
  try
    if(AChannel = nil) then Exit;
    ADemandCentreShape.XYToPage(0, 0, lXVal, lYVal);
    lMaster := FindMaster(mtWRYMChannel);
    if (lMaster <> nil) then
    begin
      LChannelShape := FVisioApp.ActivePage.Drop(lMaster, lXVal, lYVal);
      LChannelShape.Text := IntToStr(AChannel.ChannelNumber);
      lWidth := StrToFloat(UnMM(UnQuote(LChannelShape.Cells[cBeginX].Formula))) - 30;
      LChannelShape.Cells[cEndX].Formula := FloatToStr(lWidth) + ' mm';

      lWidth := StrToFloat(UnMM(UnQuote(LChannelShape.Cells[cEndY].Formula))) + 7;
      LChannelShape.Cells[cEndY].Formula := FloatToStr(lWidth) + ' mm';

      FBufferChannelList.Add(IntToStr(AChannel.ChannelNumber));
      LChannelShape.Cells[cNumber].Formula  := '"' + IntToStr(AChannel.ChannelNumber) + '"';
      LChannelShape.Cells[cVersion].Formula := '"' + CThisVersion + '"';
      RefreshChannel(LChannelShape,AChannel.ChannelNumber);
      SnapChannelToNodes(LChannelShape, AChannel.ChannelNumber);
      lTxtLabel := AddTextLabel(LChannelShape, nvtName);
      RefreshText(lTxtLabel);
      //lTxtLabel.Text := FAppModules.Language.GetString('VNV.DemandCentreConsumptiveChannel') + ' ' +lTxtLabel.Text;
      FChannelList.Add(IntToStr(AChannel.ChannelNumber));
      Result := LChannelShape;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.AddDemandCentreReturnFlowChannels(ADemandCentreShape : IVShape;AChannel : IGeneralFlowChannel) : IVShape;
const OPNAME = 'TVNVEventHandler.AddDemandCentreReturnFlowChannels';
var
  lXVal,
  lYVal        : double;
  lMaster      : IVMaster;
  lTxtLabel    : IVShape;
  LChannelShape: IVShape;
  LWidth       : Real;

  lStartY      : Real;
  lStartX      : Real;
begin
  Result := nil;
  try
    if (AChannel = nil) then  exit;
    ADemandCentreShape.XYToPage(0, 0, lXVal, lYVal);

    lStartY := 42;
    lStartX := 40;
    lMaster := FindMaster(mtWRYMChannel);
    if (lMaster <> nil) then
    begin
      LChannelShape := FVisioApp.ActivePage.Drop(lMaster, lXVal, lYVal);
      LChannelShape.Text := IntToStr(AChannel.ChannelNumber);
      lStartY := lStartY - 4;
      lStartX := lStartX + 4;
      lWidth := StrToFloat(UnMM(UnQuote(LChannelShape.Cells[cBeginX].Formula))) + lStartX;
      LChannelShape.Cells[cEndX].Formula := FloatToStr(lWidth) + ' mm';

      lWidth := StrToFloat(UnMM(UnQuote(LChannelShape.Cells[cEndY].Formula))) + lStartY;
      LChannelShape.Cells[cEndY].Formula := FloatToStr(lWidth) + ' mm';

      FBufferChannelList.Add(IntToStr(AChannel.ChannelNumber));
      LChannelShape.Cells[cNumber].Formula  := '"' + IntToStr(AChannel.ChannelNumber) + '"';
      LChannelShape.Cells[cVersion].Formula := '"' + CThisVersion + '"';
      RefreshChannel(LChannelShape,AChannel.ChannelNumber);
      SnapChannelToNodes(LChannelShape, AChannel.ChannelNumber);
      lTxtLabel := AddTextLabel(LChannelShape, nvtName);
      RefreshText(lTxtLabel);
      //lTxtLabel.Text := FAppModules.Language.GetString('VNV.DemandCentreReturnFlowChannel') + ' ' +lTxtLabel.Text;
      FChannelList.Add(IntToStr(AChannel.ChannelNumber));
      Result := LChannelShape;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.RefreshDemandCentreChannels(AShape  :  IVShape; ADemandCentre  :  IYMDemandCentre);
const OPNAME = 'TVNVEventHandler.RefreshDemandCentreChannels';
var
  lShape         : IVShape;
  lIdx           : integer;
  LChannelNumber : integer;
  LChannel       : IGeneralFlowChannel;
begin
  try
    if (ADemandCentre = nil) then
      exit;

    //Refresh the Consumptive Channel
    if (ADemandCentre.ConsumptiveUseChannel <> nil) then
    begin
      LChannelNumber := ADemandCentre.ConsumptiveUseChannel.ChannelNumber;
      lShape := FindChannelShapeByChannelNumber(LChannelNumber);
      if lShape <> nil then
        RefreshChannel(lShape,LChannelNumber)
      else
        lShape := AddDemandCentreConsumptiveUseChannel(AShape, ADemandCentre.ConsumptiveUseChannel);
    end;

    //Refresh the Reclamation Channel
    if (ADemandCentre.ReclaimationPlantExists) and (ADemandCentre.ReclaimationChannel <> nil) then
    begin
      LChannelNumber := ADemandCentre.ReclaimationChannel.ChannelNumber;
      lShape := FindChannelShapeByChannelNumber(LChannelNumber);
      if lShape <> nil then
        RefreshChannel(lShape,LChannelNumber)
      else
        lShape := AddDemandCentreReclamationPlantChannel(AShape, ADemandCentre.ReclaimationChannel);
    end;

    //Refresh the Return Flow Channel
    for lIdx := 0 to ADemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureCount - 1 do
    begin
      LChannel       := ADemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureByIndex[lIdx].Channel;
      if(LChannel <> nil) then
      begin
        LChannelNumber := LChannel.ChannelNumber;
        lShape := FindChannelShapeByChannelNumber(LChannelNumber);
        if lShape <> nil then
          RefreshChannel(lShape,LChannelNumber)
        else
          lShape := AddDemandCentreReturnFlowChannels(AShape, LChannel);
      end;
    end;

    //Refresh the Supply Chain Channel
    for lIdx := 0 to ADemandCentre.SupplyChannelCount - 1 do
    begin
      LChannel       := ADemandCentre.SupplyChannelByIndex[lIdx];
      if(LChannel <> nil) then
      begin
        LChannelNumber := LChannel.ChannelNumber;
        lShape := FindChannelShapeByChannelNumber(LChannelNumber);
        if lShape <> nil then
          RefreshChannel(lShape,LChannelNumber)
        else
          lShape := AddDemandCentreSupplyChannels(AShape, LChannel);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.GenerateDemandCentreChannelList(var AChannelList : TStringList; ADemandCentre  : IYMDemandCentre);
const OPNAME = 'TVNVEventHandler.GenerateDemandCentreChannelList';
var
  lIdx  :  Integer;
begin
  try
    AChannelList.Clear;

    if (ADemandCentre <> nil) then
    begin
      //Consumptive Channel
      if (ADemandCentre.ConsumptiveUseChannel <> nil) then
        AChannelList.Add(IntToStr(ADemandCentre.ConsumptiveUseChannel.ChannelNumber));

      //Reclamation Channel
      if (ADemandCentre.ReclaimationPlantExists) then
        if ADemandCentre.ReclaimationChannel <> nil then
            AChannelList.Add(IntToStr(ADemandCentre.ReclaimationChannelNr));

      //Return Flow Channel
      if (ADemandCentre.ReturnFlowFeatureList <> nil) then
        for lIdx := 0 to ADemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureCount - 1 do
          AChannelList.Add(IntToStr(ADemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureByIndex[lIdx].ChannelNr));

      //Supply Chain
      for lIdx := 0 to ADemandCentre.SupplyChannelCount - 1 do
        AChannelList.Add(IntToStr(ADemandCentre.SupplyChannelByIndex[lIdx].ChannelNumber));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.AddDemandCentreSupplyChannels(AShape  :  IVShape; AChannel : IGeneralFlowChannel) : IVShape;
const OPNAME = 'TVNVEventHandler.AddDemandCentreSupplyChannels';
var
  lXVal,
  lYVal        : double;
  lMaster      : IVMaster;
  lTxtLabel    : IVShape;
  LChannelShape: IVShape;
  LWidth       : Real;

  lStartY      : Real;
  lStartX      : Real;
begin
  Result := nil;
  try
    AShape.XYToPage(0, 0, lXVal, lYVal);

    lStartY := -20;
    lStartX := 0;

    lMaster := FindMaster(mtWRYMChannel);
    if (lMaster <> nil) then
    begin
      LChannelShape := FVisioApp.ActivePage.Drop(lMaster, lXVal, lYVal);
      LChannelShape.Text := IntToStr(AChannel.ChannelNumber);
      lStartY := lStartY - 4;
      lStartX := lStartX + 4;
      lWidth := StrToFloat(UnMM(UnQuote(LChannelShape.Cells[cBeginX].Formula))) + lStartX;
      LChannelShape.Cells[cBeginX].Formula := FloatToStr(lWidth) + ' mm';

      lWidth := StrToFloat(UnMM(UnQuote(LChannelShape.Cells[cEndY].Formula))) + lStartY;
      LChannelShape.Cells[cBeginY].Formula := FloatToStr(lWidth) + ' mm';

      FBufferChannelList.Add(IntToStr(AChannel.ChannelNumber));
      LChannelShape.Cells[cNumber].Formula  := '"' + IntToStr(AChannel.ChannelNumber) + '"';
      LChannelShape.Cells[cVersion].Formula := '"' + CThisVersion + '"';
      RefreshChannel(LChannelShape,AChannel.ChannelNumber);
      SnapChannelToNodes(LChannelShape, AChannel.ChannelNumber);
      lTxtLabel := AddTextLabel(LChannelShape, nvtName);
      RefreshText(lTxtLabel);
      //lTxtLabel.Text := FAppModules.Language.GetString('VNV.DemandCentreSupplyChannel') + ' ' +lTxtLabel.Text;
      FChannelList.Add(IntToStr(AChannel.ChannelNumber));
      Result := LChannelShape;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.AddPowerPlantChannel(APowerplantShape : IVShape;AChannel : IGeneralFlowChannel): IVShape;
const OPNAME = 'TVNVEventHandler.AddPowerPlantChannel';
var
  lXVal,
  lYVal        : double;
  lMaster      : IVMaster;
  lTxtLabel    : IVShape;
  LChannelShape: IVShape;
begin
  Result := nil;
  try
    if(AChannel = nil) then Exit;
    APowerplantShape.XYToPage(0, 0, lXVal, lYVal);
    lMaster := FindMaster(mtWRYMChannel);
    if (lMaster <> nil) then
    begin
      if AChannel.ChannelSubType = 1 then LChannelShape := FVisioApp.ActivePage.Drop(lMaster, lXVal, lYVal+0.2)
      else if AChannel.ChannelSubType = 2 then LChannelShape := FVisioApp.ActivePage.Drop(lMaster, lXVal, lYVal+0.3)
      else LChannelShape := FVisioApp.ActivePage.Drop(lMaster, lXVal, lYVal);

      LChannelShape.Text := IntToStr(AChannel.ChannelNumber);
      FBufferChannelList.Add(IntToStr(AChannel.ChannelNumber));
      LChannelShape.Cells[cNumber].Formula  := '"' + IntToStr(AChannel.ChannelNumber) + '"';
      LChannelShape.Cells[cVersion].Formula := '"' + CThisVersion + '"';
      RefreshChannel(LChannelShape,AChannel.ChannelNumber);
      SnapChannelToNodes(LChannelShape, AChannel.ChannelNumber);
      lTxtLabel := AddTextLabel(LChannelShape, nvtName);
      RefreshText(lTxtLabel);
      FChannelList.Add(IntToStr(AChannel.ChannelNumber));
      Result := LChannelShape;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.FindDemandCentreChannelsWithParent (ADemandCentreNodeNr : integer;
                                                               AShapesList : TStringList);
const OPNAME = 'TVNVEventHandler.FindDemandCentreChannelsWithParent';
var
  lPageIdx          : integer;
  lShapeIdx         : integer;
  lShape            : IVShape;
  lPage             : IVPage;
  lDownStreamNodeNr,
  lUpStreamNodeNr   : integer;
begin
  try
    AShapesList.Clear;
    lPageIdx := 1;
    while (lPageIdx <= FVisioApp.ActiveDocument.Pages.Count) do
    begin
      lPage := FVisioApp.ActiveDocument.Pages[lPageIdx];
      lShapeIdx := 1;
      while (lShapeIdx <= lPage.Shapes.Count) do
      begin
        lShape := lPage.Shapes[lShapeIdx];
        if ((lShape <> nil) AND (Pos(PChar(mtWRYMChannel), lShape.Name) > 0)) then
        begin
          lDownStreamNodeNr := StrToIntDef(UnQuote(lShape.Cells[cDownStreamNode].Formula), -1);
          lUpStreamNodeNr := StrToIntDef(UnQuote(lShape.Cells[cUpStreamNode].Formula), -1);
          if ((lDownStreamNodeNr = ADemandCentreNodeNr) or (lUpStreamNodeNr = ADemandCentreNodeNr)) then
              AShapesList.Add(UnQuote(lShape.Cells[cNumber].Formula));
        end;
        lShapeIdx := lShapeIdx + 1;
      end;
      lPageIdx := lPageIdx + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.RemoveDeletedDemandCentreChannels(AElementID  :  Integer);
const OPNAME = 'TVNVEventHandler.RemoveDeletedDemandCentreChannels';
var
  LChannelNumber    : integer;
  lIdx              : integer;
  lShape            : IVShape;
  lShapeName        : string;
  lChannelList      : TStringList;
  lChannel          : IGeneralFlowChannel;
begin
  try
    lChannelList := TStringList.Create;
    try
      lChannelList.Clear;
      FindDemandCentreChannelsWithParent(AElementID, lChannelList);
      for lIdx := 0 to lChannelList.Count - 1 do
      begin
        LChannelNumber := StrToIntDef(lChannelList.Strings[lIdx], -1);
        lChannel       := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.
                          ChannelList.ChannelByChannelNumber[LChannelNumber];
        if(lChannel <> nil) then
        begin
          if(lChannel.UpStreamNodeNumber = AElementID) then Continue;
          if(lChannel.DownStreamNodeNumber = AElementID) then Continue;
        end;

        lShape := FindChannelShapeByChannelNumber(LChannelNumber);
        if (lShape <> nil) then
        begin
          lShapeName := lShape.Name;
          lShape.Delete;
          lShape := FindShapeWithParent(mtWRYMText, lShapeName);  //Delete the Channel Label
          if (lShape <> nil) then
            lShape.Delete;
        end;
      end;
    finally
      FreeAndNil(lChannelList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.MineShapeAdded(const AShape: IVShape);
const OPNAME = 'TVNVEventHandler.MineShapeAdded';
var
  lElementID        : Integer;
  lTxtLabel         : IVShape;
  lYieldModelData   : IYieldModelData;
  lMine             : IMine;
  LCount            : Integer;
  LUnderground      : IUnderground;
begin
  try
    if (FBufferMineList.Count > 0) then
      { Upgraded Mine }
      FBufferMineList.Delete(0)
    else
    begin
      if (AShape.CellExists[cNumber, 0] <> 0) AND (UnQuote(AShape.Cells[cNumber].Formula) <> '') then
      begin
        { Copied Mine }
        lElementID  := StrToIntDef(UnQuote(AShape.Cells[cNumber].Formula),0);
      end
      else
      begin
        { New Mine - from stencil }
        lElementID := ShowMinesDialog;
        if (lElementID <> -1) then
        begin
          AShape.Cells[cNumber].Formula  := '"' + IntToStr(lElementID) + '"';
          AShape.Cells[cVersion].Formula := '"' + CThisVersion + '"';
        end;
      end;
      AShape.Text := IntToStr(lElementID);
      if (lElementID = -1) then
        AShape.Delete
      else
      begin
        RefreshMine(AShape, lElementID);
        FMineList.Add(IntToStr(lElementID));

        if (FDrawing.GISMode) then
          if CheckExistingWithCoords(AShape, lElementID) then
          begin
            GISPopulateDoNotRecalcCoordsList(lElementID);
            GISSetXYPosFromLonLat(AShape, lElementID)
          end;

        lTxtLabel := AddTextLabel(AShape, nvtName);
        RefreshText(lTxtLabel);

        lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
        lMine := lYieldModelData.NetworkFeaturesData.MineList.MineByNodeNumber[lElementID];

        AddMineChannel(AShape,lMine.RiverChannel,3);

        if lMine.PolutionControlDamExists then
          CreatePolutionControlDam(AShape,lMine.NodeNumber, lMine.PolutionControlDam.ReservoirConfigurationData.ReservoirIdentifier,lMine.PCDChannelNumber);

        for LCount := 0 to lMine.UndergroundCount-1 do
        begin
          LUnderground  := lMine.UnderGroundByIndex[LCount];
          AddUndergroundShape(AShape,LUnderground,lElementID);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ShowMinesDialog: integer;
const OPNAME = 'TVNVEventHandler.ShowMinesDialog';
var
  LMinesDlg  : TVNVMineDialog;
  lStrTemp   : string;
begin
  Result := -1;
  try
  	LMinesDlg := TVNVMineDialog.CreateWithoutDFM(nil, FAppModules);
    LMinesDlg.LanguageHasChanged;
    try
      lStrTemp := FMineList.CommaText;
	    Result   := LMinesDlg.ShowMinesDialog(lStrTemp, FMineExistNew, FMineDuplicate);
    finally
      LMinesDlg.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVEventHandler.RefreshMine(AShape: IVShape; AElementID: integer);
const OPNAME = 'TVNVEventHandler.RefreshMine';
var
  lMine      : IMine;
  lMineName  : string;
  lKeepColor : boolean;
begin
  try
    if(AShape = nil) or (AElementID = 0) or (AElementID = NullInteger) then Exit;

    lKeepColor := (AShape.CellExists[cPermanentColor, 0] <> 0)  and (AShape.Cells[cPermanentColor].Formula = '"TRUE"');
    lMine      := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.MineList.MineByNodeNumber[AElementID];
    if (lMine = nil) then
    begin
      if not lKeepColor then
        AShape.CellsSRC[visSectionObject, visRowFill, visFillForegnd].FormulaU := IntToStr(visRed);
    end
    else
    begin
      FVisioApp.ActiveWindow.Windows.ItemFromID[visWinIDCustProp].Visible := FALSE;
      lMineName := lMine.MineName;
      AShape.Cells[cName].Formula       := '"' + lMineName + '"';
      AShape.Cells[cRow1Action].Formula := 'RUNMACRO("ThisDocument.ProcessVNVSpecial 1")';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.AddMineChannel(AMineShape: IVShape; AChannel: IGeneralFlowChannel;
                                        AMineConnNodeNumber : integer = -1; ADeleteLock : integer = 0): IVShape;
const OPNAME = 'TVNVEventHandler.AddMineChannel';
var
  lXVal,
  lYVal        : double;
  lMaster      : IVMaster;
  lTxtLabel    : IVShape;
  LChannelShape: IVShape;
begin
  Result := nil;
  try
    if(AChannel = nil) then Exit;
    AMineShape.XYToPage(-1, 0, lXVal, lYVal);
    lMaster := FindMaster(mtWRYMChannel);
    if (lMaster <> nil) then
    begin
      LChannelShape := FVisioApp.ActivePage.Drop(lMaster, lXVal, lYVal);
      LChannelShape.Text := IntToStr(AChannel.ChannelNumber);
      FBufferChannelList.Add(IntToStr(AChannel.ChannelNumber));
      LChannelShape.Cells[cNumber].Formula  := '"' + IntToStr(AChannel.ChannelNumber) + '"';
      LChannelShape.Cells[cVersion].Formula := '"' + CThisVersion + '"';
      LChannelShape.Cells[cDeletePrevention].Formula := '"' + IntToStr(ADeleteLock) + '"';
      RefreshChannel(LChannelShape,AChannel.ChannelNumber);
      SnapChannelToNodes(LChannelShape, AChannel.ChannelNumber, AMineConnNodeNumber);
      lTxtLabel := AddTextLabel(LChannelShape, nvtName);
      RefreshText(lTxtLabel);
      FChannelList.Add(IntToStr(AChannel.ChannelNumber));
      Result := LChannelShape;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ProcessVNVModelRightClicked(AShapeName: string; ANumber: integer): WordBool;
const OPNAME = 'TVNVEventHandler.ProcessVNVModelRightClicked';
var
  lYieldModel : IYieldModel;
  lOptionList : TStringList;
  lMenuDlg    : TVNVShapeMenuDlg;
  lOption     : integer;
begin
  Result := FALSE;
  try
    lYieldModel := (FAppModules.Model as IYieldModel);
    lOptionList := TStringList.Create;
    try
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.MenuCancelOption'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.ViewRunConfiguration'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.ViewOutputConfiguration'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.ViewCatchmentProportions'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.ViewChannelPenalties'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.ViewReservoirPenalty'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.ViewChannelArea'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.ViewReconciliationAnalysis'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.ViewMetaData'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.RunModel'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.ImportOutputFiles'),TObject(1));
      lMenuDlg := TVNVShapeMenuDlg.Create(nil);
      try
        lOption := lMenuDlg.ShowShapeMenuDialog(lOptionList);
        case lOption of
          1 : Result := lYieldModel.ViewInputConfigurationDialog(mdvnRunConfiguration);
          2 : Result := lYieldModel.ViewInputConfigurationDialog(mdvnOutputConfiguration);
          3 : Result := lYieldModel.ViewInputConfigurationDialog(mdvnCatchmentProportions);
          4 : Result := lYieldModel.ViewInputConfigurationDialog(mdvnChannelPenalties);
          5 : Result := lYieldModel.ViewInputConfigurationDialog(mdvnReservoirPenalty);
          6 : Result := lYieldModel.ViewInputConfigurationDialog(mdvnChannelArea);
          7 : Result := lYieldModel.ViewInputConfigurationDialog(mdvnReconciliationAnalysis);
          8 : Result := lYieldModel.ViewInputConfigurationDialog(mdvnMetaData);
          9 : Result := lYieldModel.DoRunModel;
          10 : Result := lYieldModel.ImportOutputFiles;
        end;
      finally
        lMenuDlg.Free;
      end;
    finally
      FreeAndNil(lOptionList);
    end
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ProcessVNVMultiActionRightClicked(AShapeName: string; ANumber: integer): WordBool;
const OPNAME = 'TVNVEventHandler.ProcessVNVMultiActionRightClicked';
var
  lYieldModel : IYieldModel;
  lOptionList : TStringList;
  lMenuDlg    : TVNVShapeMenuDlg;
  lOption     : integer;
begin
  Result := FALSE;
  try
    lYieldModel := (FAppModules.Model as IYieldModel);
    lOptionList := TStringList.Create;
    try
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.MenuCancelOption'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.AddAllNodes'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.AddAllChannels'),TObject(1));

      lOptionList.AddObject(FAppModules.Language.GetString('VNV.AddAllAverageChannelFlow'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.DeleteAllAverageChannelFlow'),TObject(1));

      lOptionList.AddObject(FAppModules.Language.GetString('VNV.AddAllChangeInVolume'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.DeleteAllChangeInVolume'),TObject(1));

      lOptionList.AddObject(FAppModules.Language.GetString('VNV.AddAllAverageVolume'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.DeleteAllAverageVolume'),TObject(1));

      lOptionList.AddObject(FAppModules.Language.GetString('VNV.AddAllAverageElevation'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.DeleteAllAverageElevation'),TObject(1));

      lOptionList.AddObject(FAppModules.Language.GetString('VNV.AddAllAverageEvaporation'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.DeleteAllAverageEvaporation'),TObject(1));

      lOptionList.AddObject(FAppModules.Language.GetString('VNV.AddAllAverageRainfall'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.DeleteAllAverageRainfall'),TObject(1));

      lOptionList.AddObject(FAppModules.Language.GetString('VNV.AddAllNetBasinRunoff'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.DeleteAllNetBasinRunoff'),TObject(1));

      lOptionList.AddObject(FAppModules.Language.GetString('VNV.AddAllAvgNetBasinRunoff'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.DeleteAllAvgNetBasinRunoff'),TObject(1));

      lOptionList.AddObject(FAppModules.Language.GetString('VNV.SetAllOutputLabelsPermanent'),TObject(1));

      lOptionList.AddObject(FAppModules.Language.GetString('VNV.AddAllReservoirPenalty'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.DeleteAllReservoirPenalty'),TObject(1));
      
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.AddAllChannelPenalty'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.DeleteAllChannelPenalty'),TObject(1));

      lOptionList.AddObject(FAppModules.Language.GetString('VNV.AddAllPowerPlants'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.DeleteAllPowerPlants'),TObject(1));

      lOptionList.AddObject(FAppModules.Language.GetString('LabelCaption.Refresh'),TObject(1));
      lMenuDlg := TVNVShapeMenuDlg.Create(nil);
      try
        lOption := lMenuDlg.ShowShapeMenuDialog(lOptionList);
        case lOption of
          1 : Result := AddAllNodesToDrawing;
          2 : Result := AddAllChannelsToDrawing;

          3 : Result := AddAllChannelOutputTextLabels(nvtAvgChannelFlow);
          4 : Result := DeleteAllChannelOutputTextLabels(nvtAvgChannelFlow);

          5 : Result := AddAllReservoirOutputTextLabels(nvtReservoirStorageChange);
          6 : Result := DeleteAllReservoirOutputTextLabels(nvtReservoirStorageChange);

          7 : Result := AddAllReservoirOutputTextLabels(nvtAvgVolume);
          8 : Result := DeleteAllOutputTextLabels(nvtAvgVolume);

          9 : Result := AddAllReservoirOutputTextLabels(nvtAvgElevation);
          10 : Result := DeleteAllOutputTextLabels(nvtAvgElevation);

          11 : Result := AddAllReservoirOutputTextLabels(nvtAvgEvaporation);
          12 : Result := DeleteAllOutputTextLabels(nvtAvgEvaporation);

         13 : Result := AddAllReservoirOutputTextLabels(nvtAvgRainfall);
         14 : Result := DeleteAllOutputTextLabels(nvtAvgRainfall);

         15 : Result := AddAllReservoirOutputTextLabels(nvtNetBasinRunoff);
         16 : Result := DeleteAllReservoirOutputTextLabels(nvtNetBasinRunoff);

         17 : Result := AddAllReservoirOutputTextLabels(nvtAvgNetBasinRunoff);
         18 : Result := DeleteAllReservoirOutputTextLabels(nvtAvgNetBasinRunoff);

         19 : Result := SetAllOutputLabelsPermanent;

         20 : Result := AddAllReservoirPenaltyToDrawing;
         21 : Result := DeleteAllReservoirPenaltyFromDrawing;
         
         22 : Result := AddAllChannelPenaltyToDrawing;
         23 : Result := DeleteAllChannelPenaltyFromDrawing;

         24 : Result := AddAllChannelPenaltyToDrawing;
         25 : Result := DeleteAllChannelPenaltyFromDrawing;

         26 :
         begin
           RefreshDocument(FVisioDoc);
           Result := True;
         end;
        end;
      finally
        lMenuDlg.Free;
      end;
    finally
      FreeAndNil(lOptionList);
    end
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ProcessVNVLayerRightClicked(AShapeName: string; ANumber: integer): WordBool;
const OPNAME = 'TVNVEventHandler.ProcessVNVLayerRightClicked';
var
  lYieldModel : IYieldModel;
  lOptionList : TStringList;
  lMenuDlg    : TVNVShapeMenuDlg;
  lOption     : integer;
begin
  Result := FALSE;
  try
    lYieldModel := (FAppModules.Model as IYieldModel);
    lOptionList := TStringList.Create;
    try
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.MenuCancelOption'),TObject(1));

      lOptionList.AddObject(FAppModules.Language.GetString('VNV.AddAllReservoirPenaltyLayer'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.ClearAllReservoirPenaltiesFromLayer'),TObject(1));

      lOptionList.AddObject(FAppModules.Language.GetString('VNV.AddAllChannelPenaltyLayer'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.ClearAllChannelPenaltyLayer'),TObject(1));

      lOptionList.AddObject(FAppModules.Language.GetString('VNV.AddAllChannelAndReservoirPenaltyLayer'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.ClearAllChannelAndReservoirPenaltyLayer'),TObject(1));

      lOptionList.AddObject(FAppModules.Language.GetString('VNV.AddAllAverageChannelFlowsToALayer'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.ClearAllAverageChannelFlowsFromLayers'),TObject(1));

      lOptionList.AddObject(FAppModules.Language.GetString('VNV.AddAllReservoirToALayer'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.ClearAllReservoirFromLayers'),TObject(1));

      lOptionList.AddObject(FAppModules.Language.GetString('VNV.AddAllWetlandsToALayer'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.ClearAllWetlandsFromLayers'),TObject(1));

      lOptionList.AddObject(FAppModules.Language.GetString('VNV.AddAllIrrigationBlocksToALayer'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.ClearAllIrrigationBlocksFromLayers'),TObject(1));

      lOptionList.AddObject(FAppModules.Language.GetString('VNV.AddAllNodesWithoutInflowToALayer'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.ClearAllNodesWithoutInflowFromLayers'),TObject(1));

      lOptionList.AddObject(FAppModules.Language.GetString('VNV.AddAllNodesWithInflowToALayer'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.ClearAllNodesWithInflowfromLayers'),TObject(1));

      lOptionList.AddObject(FAppModules.Language.GetString('VNV.AddAllMinesToALayer'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.ClearAllMinesFromLayers'),TObject(1));


      lOptionList.AddObject(FAppModules.Language.GetString('VNV.AddAllDemandCentresToALayer'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.ClearAllDemandCentresFromLayers'),TObject(1));

      lOptionList.AddObject(FAppModules.Language.GetString('VNV.AddAllIrrigationAreasToALayer'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.ClearAllIrrigationAreasFromLayers'),TObject(1));

      lOptionList.AddObject(FAppModules.Language.GetString('VNV.AddAllPowerPlantsToALayer'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.ClearAllPowerPlantsFromLayers'),TObject(1));


      lMenuDlg := TVNVShapeMenuDlg.Create(nil);
      try
        lOption := lMenuDlg.ShowShapeMenuDialog(lOptionList);
        case lOption of
           1 : Result := AddAllReservoirPenaltyLayer;
           2 : Result := ClearAllReservoirPenaltiesFromLayer;

           3 : Result := AddAllChannelPenaltyLayer;
           4 : Result := ClearAllChannelPenaltyLayer;

           5 : Result := AddAllChannelAndReservoirPenaltyLayer;
           6 : Result := ClearAllChannelAndReservoirPenaltyLayer;

           7 : Result := AddAllAverageChannelFlowsToALayer;
           8 : Result := ClearAllAverageChannelFlowsFromLayers;

           9 : Result := AddAllReservoirToALayer;
          10 : Result := ClearAllReservoirFromLayers;

          11 : Result := AddAllWetlandsToALayer;
          12 : Result := ClearAllWetlandsFromLayers;

          13 : Result := AddAllIrrigationBlockToALayer;
          14 : Result := ClearAllIrrigationBlocksFromLayers;

          15 : Result := AddAllNodesWithoutInflowToALayer;
          16 : Result := ClearAllNodesWithoutInflowFromLayers;

          17 : Result := AddAllNodesWithInflowToALayer;
          18 : Result := ClearAllNodesWithInflowfromLayers;

          19 : Result := AddAllMinesToALayer;
          20 : Result := ClearAllMinesFromLayers;

          21 : Result := AddAllDemandCentresToALayer;
          22 : Result := ClearAllDemandCentresFromLayers;

          23 : Result := AddAllIrrigationAreasToALayer;
          24 : Result := ClearAllIrrigationAreasFromLayers;

          25 : Result := AddAllPowerPlantsToALayer;
          26 : Result := ClearAllPowerPlantsFromLayers;  

        end;
      finally
        lMenuDlg.Free;
      end;
    finally
      FreeAndNil(lOptionList);
    end
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TVNVEventHandler.ModelShapeAdded(const AShape: IVShape);
const OPNAME = 'TVNVEventHandler.ModelShapeAdded';
begin
  try
    AShape.Cells[cNumber].Formula  := '"' + IntToStr(0) + '"';
    AShape.Cells[cVersion].Formula := '"' + CThisVersion + '"';
    AShape.Cells[cName].Formula    := '"' + 'Model' + '"';;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.DeleteMine(ANumber: integer; ADBDelete : integer = -1) : WordBool;
const OPNAME = 'TVNVEventHandler.DeleteMine';
var
  lShape : IVShape;
begin
  Result := FALSE;
  try
    lShape := FindShapeWithPropertyValue(mtWRYMMine, cNumber, IntToStr(ANumber));
    if (lShape <> nil) AND (MayChangeScenario) then
    begin
      lShape.Delete;
      if ADBDelete = -1 then
        Result := (FAppModules.Model as IYieldModel).DoDeleteMine(ANumber);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.MineShapeDeleted (const AShape : IVShape);
const OPNAME = 'TVNVEventHandler.MineShapeDeleted';
var
  lElementID          : string;
  lElementIdx         : integer;
  LCount              : integer;
  LMine               : IMine;
  lYieldModelData     : IYieldModelData;
  LUnderground        : integer;
  LUndergroundChannel : integer;
  LPCD                : integer;
  LPCDChannel         : integer;
  LRiverChannel       : integer;
  LReservoir          : IVShape;
  LChannel            : IVShape;
  LUGChannel          : IVShape;

begin
  try
    lElementID := UnQuote(AShape.Cells[cNumber].Formula);
    lElementIdx := FMineList.IndexOf(lElementID);

    if (lElementIdx >= 0) then
    begin

      lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
      LMine := lYieldModelData.NetworkFeaturesData.MineList.MineByNodeNumber[StrToIntDef(UnQuote(AShape.Cells[cNumber].Formula),0)];

      if LMine <> nil then
      begin
        for LCount := 0 to LMine.UndergroundCount-1 do
        begin
          LUnderground := GetUnderGroundIdentifier(LMine.NodeNumber,LCount);
          LUndergroundChannel := LMine.UnderGroundByIndex[LCount].ChannelNumberToUGDam;
          LUGChannel := FindChannelShapeByChannelNumber(LUndergroundChannel);
          LUGChannel.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
          DeleteUndergroundShape(LUnderground, LUndergroundChannel);
        end;

        LPCD := GetPCDIdentifier(LMine.NodeNumber);
        LPCDChannel := LMine.PCDChannelNumber;

        LReservoir := FindReservoirWithNumber(LPCD);
        if(LReservoir <> nil) then
        begin
          LReservoir.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
          DeleteReservoir(LPCD,1);
        end;

        LChannel := FindChannelShapeByChannelNumber(LPCDChannel);
        if(LChannel <> nil) then
        begin
          LChannel.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
          DeleteChannel(LPCDChannel,1);
        end;

        LRiverChannel := LMine.RiverChannel.ChannelNumber;
        DeleteChannel(LRiverChannel,1);

        DeleteAllTextShapes(AShape);

        DeleteAllShapesWithParent(AShape);

        FMineList.Delete(lElementIdx);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ProcessVNVMineRightClicked(AShapeName: string; ANumber: integer): WordBool;
const OPNAME = 'TVNVEventHandler.ProcessVNVMineRightClicked';
var
  lOptionList : TStringList;
  lMenuDlg    : TVNVShapeMenuDlg;
  lOption     : integer;
  LPermanentColor   : boolean;
  lShape            : IVShape;
begin
  Result := FALSE;
  try
    lOptionList := TStringList.Create;
    try
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.MenuCancelOption'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.ViewMineProperties'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.DeleteMineFromScenario'),TObject(1));
      LPermanentColor := True;
      lShape := GetFirstSelectedShape(mtWRYMMine);
      if(lShape <> nil) and (lShape.CellExists[cPermanentColor, 0] <> 0) then
      begin
        LPermanentColor := (lShape.Cells[cPermanentColor].Formula = '"TRUE"');
        if LPermanentColor then
          lOptionList.AddObject(FAppModules.Language.GetString('VNV.PermanentColorOff'),TObject(1))
        else
          lOptionList.AddObject(FAppModules.Language.GetString('VNV.PermanentColorOn'),TObject(1));
      end;

      lMenuDlg := TVNVShapeMenuDlg.Create(nil);
      try
        lOption := lMenuDlg.ShowShapeMenuDialog(lOptionList);
        case lOption of
          1 : begin
                try
                  Result := ViewMineProperties(AShapeName,ANumber);
                  if (Result) then
                    MineHasChanged(AShapeName, ANumber);
                except
                end;
              end;
          2 : Result := DeleteMine(ANumber);
          3 : begin
                if LPermanentColor then
                  lShape.Cells[cPermanentColor].Formula         := '"FALSE"'
                else
                  lShape.Cells[cPermanentColor].Formula         := '"TRUE"';
                Result := True;
              end;
        else
        end;
      finally
        lMenuDlg.Free;
      end;
    finally
      FreeAndNil(lOptionList);
    end
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.MineHasChanged(AShapeName: string; ANumber: integer);
const OPNAME = 'TVNVEventHandler.MineHasChanged';
var
  lShape              : IVShape;
  lShapesLst          : TStringList;
  lIndex              : integer;
  lShapeName          : string;
  lReservoirShapeList : TStringList;
  lUGReservoirList    : TStringList;
  LChannelList        : TStringList;
  LMine               : IMine;
  LYieldModelData     : IYieldModelData;
  LCount              : integer;
  LChannelCount       : integer;
  LUnderground        : IUnderground;
  LChannel            : IVShape;
  LMainShape          : IVShape;
  LDeleteUnderground  : Boolean;

begin
  try
    lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
    LMine := LYieldModelData.NetworkFeaturesData.MineList.MineByNodeNumber[ANumber];

    Try
      lShape := FindShapeWithName(AShapeName);
      if (lShape <> nil) then
      begin
        LMainShape := lShape;
        RefreshMine(lShape, ANumber);

        lUGReservoirList := TStringList.Create;
        lUGReservoirList.Add(IntToStr(LMine.PolutionControlDam.ReservoirConfigurationData.ReservoirIdentifier));
        for LCount := 0 to lMine.UndergroundCount-1 do
        begin
            LUnderground  := lMine.UnderGroundByIndex[LCount];

            lShape := FindReservoirWithNumber(LUnderground.UndergroundDam.ReservoirConfigurationData.ReservoirIdentifier);
            if (lShape <> nil) then
              RefreshReservoir(lShape, ANumber)
            else
              AddUndergroundShape(LMainShape,LUnderground,ANumber);

            lUGReservoirList.Add(IntToStr(LUnderground.UndergroundDam.ReservoirConfigurationData.ReservoirIdentifier));
        end;
      end;

      lReservoirShapeList := TStringList.Create;
      FindReservoirByParentProperty(ANumber,lReservoirShapeList);
      for lIndex := 0 to lReservoirShapeList.Count - 1 do
      begin
        lShapeName := lReservoirShapeList.Strings[lIndex];
        lShape     := FindShapeWithName(lShapeName);

        LDeleteUnderground := True;
        for LCount := 0 to lUGReservoirList.Count - 1 do
        begin
          if lUGReservoirList.Strings[LCount] = UnQuote(lShape.Cells[cNumber].Formula) then
            LDeleteUnderground := False;
        end;

        if LDeleteUnderground then
        begin
          try
            LChannelList := TStringList.Create;
            FindChannelsWithParent(StrToIntDef(UnQuote(lShape.Cells[cNumber].Formula),0),LChannelList);
            for LChannelCount := 0 to LChannelList.Count - 1 do
            begin
              LChannel := FindChannelShapeByChannelNumber(StrToIntDef(lChannelList.Strings[LChannelCount], -1));
              if LChannel <> Nil then
              begin
                if (UnQuote(LChannel.Cells[cDownStreamNode].Formula) = UnQuote(lShape.Cells[cNumber].Formula))
                  and (StrToIntDef(UnQuote(LChannel.Cells[cUpStreamNode].Formula),0) = ANumber) then
                    begin
                      LChannel.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
                      DeleteChannel(StrToIntDef(lChannelList.Strings[LChannelCount], -1));
                    end;
              end;
            end;

            DeleteReservoir(StrToIntDef(UnQuote(lShape.Cells[cNumber].Formula),0),1);
          finally
            FreeAndNil(LChannelList);
          end;
        end;
      end;

    finally
      FreeAndNil(lReservoirShapeList);
      FreeAndNil(lUGReservoirList);
    end;

    lShapesLst := TStringList.Create;
    try
      FindTextShapesWithParent(AShapeName, lShapesLst);
      for lIndex := 0 to lShapesLst.Count - 1 do
      begin
        lShapeName := lShapesLst.Strings[lIndex];
        lShape     := FindShapeWithName(lShapeName);
        if (lShape <> nil) then
          RefreshText(lShape);
      end;
    finally
      FreeAndNil(lShapesLst);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.AddUndergroundShape(const AShape: IVShape; AUnderground : IUnderground; AParentMineID : Integer = 0);
const OPNAME = 'TVNVEventHandler.AddUndergroundShape';
var
   lMaster            : IVMaster;
   lXVal              : Double;
   lYVal              : Double;
   LUndergroundShape  : IVShape;
begin
  try
   lMaster := FindMaster(mtWRYMReservoir);
   if(lMaster <> nil) then
     begin
       AShape.XYToPage(0, -0.5, lXVal, lYVal);
       FBufferReservoirList.Add(AUnderground.UndergroundDam.ReservoirConfigurationData.ReservoirName);
       LUndergroundShape := FVisioApp.ActivePage.Drop(lMaster, LXVal, LYVal);
       ReservoirShapeAdded(LUndergroundShape,AUnderground.UndergroundDam.ReservoirConfigurationData.ReservoirIdentifier,1,AParentMineID);
       AddMineChannel(LUndergroundShape,AUnderground.ChannelToUnderGroundDam,0,1);
     end;
   except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.DeleteUndergroundShape(AUnderground: integer; AUndergroundChannel: integer; ADBDelete: integer = 1);
const OPNAME = 'TVNVEventHandler.DeleteUndergroundShape';
begin
  try
   begin
       FBufferReservoirList.Add(IntToStr(AUnderground));
       DeleteReservoir(AUnderground,ADBDelete);
       FBufferReservoirList.Clear;
       FBufferChannelList.Add(IntToStr(AUndergroundChannel));
       DeleteChannel(AUndergroundChannel,ADBDelete);
       FBufferChannelList.Clear;
   end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.FindReservoirWithNumber(AReservoirNumber: Integer): IVShape;
const OPNAME = 'TVNVEventHandler.FindReservoirWithNumber';
var
  LNumber,
  lPageIdx        : integer;
  lShapeIdx       : integer;
  lShape          : IVShape;
  lPage           : IVPage;
  lExists         : Boolean;
  lTempStr        : string;
begin
  Result := nil;
  try
    lExists  := FALSE;
    lPageIdx := 1;
    while (NOT lExists) AND (lPageIdx <= FVisioApp.ActiveDocument.Pages.Count) do
    begin
      lPage := FVisioApp.ActiveDocument.Pages[lPageIdx];
      lShapeIdx := 1;
      while (NOT lExists) AND (lShapeIdx <= lPage.Shapes.Count) do
      begin
        lShape := lPage.Shapes[lShapeIdx];
        if ((lShape <> nil) AND (lShape.Master <> nil) AND
            ((Pos(PChar(mtWRYMReservoir), lShape.Master.Name) > 0))) then
        begin
          lTempStr := UnQuote(lShape.Cells[cNumber].Formula);
          LNumber  := StrToIntDef(lTempStr,-1);
          if (LNumber = AReservoirNumber) then
          begin
            lExists := TRUE;
            Result  := lShape;
          end;
        end;
        lShapeIdx := lShapeIdx + 1;
      end;
      lPageIdx := lPageIdx + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.FindNodeShapeByNodeNumber(ANodeNumber: integer): IVShape;
const OPNAME = 'TVNVEventHandler.FindNodeShapeByNodeNumber';
var
  LNumber,
  lPageIdx        : integer;
  lShapeIdx       : integer;
  lShape          : IVShape;
  lPage           : IVPage;
  lExists         : Boolean;
  lTempStr        : string;
begin
  Result := nil;
  try
    lExists  := FALSE;
    lPageIdx := 1;
    while (NOT lExists) AND (lPageIdx <= FVisioApp.ActiveDocument.Pages.Count) do
    begin
      lPage := FVisioApp.ActiveDocument.Pages[lPageIdx];
      lShapeIdx := 1;
      while (NOT lExists) AND (lShapeIdx <= lPage.Shapes.Count) do
      begin
        lShape := lPage.Shapes[lShapeIdx];
        if ((lShape <> nil) AND (lShape.Master <> nil) AND
            ((Pos(PChar(mtWRYMNode), lShape.Master.Name) > 0))) then
        begin
          lTempStr := UnQuote(lShape.Cells[cNumber].Formula);
          LNumber  := StrToIntDef(lTempStr,-1);
          if (LNumber = ANodeNumber) then
          begin
            lExists := TRUE;
            Result  := lShape;
          end;
        end;
        lShapeIdx := lShapeIdx + 1;
      end;
      lPageIdx := lPageIdx + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.FindChannelsWithParent(AParentNumber: integer; AShapesList: TStringList);
const OPNAME = 'TVNVEventHandler.FindChannelsWithParent';
var
  lPageIdx          : integer;
  lShapeIdx         : integer;
  lShape            : IVShape;
  lPage             : IVPage;
  lDownStreamNodeNr,
  lUpStreamNodeNr   : integer;
  lChannelNumber    : integer;
begin
  try
    AShapesList.Clear;
    lPageIdx := 1;
    while (lPageIdx <= FVisioApp.ActiveDocument.Pages.Count) do
    begin
      lPage := FVisioApp.ActiveDocument.Pages[lPageIdx];
      lShapeIdx := 1;
      while (lShapeIdx <= lPage.Shapes.Count) do
      begin
        lShape := lPage.Shapes[lShapeIdx];
        if ((lShape <> nil) AND (Pos(PChar(mtWRYMChannel), lShape.Name) > 0)) then
        begin
          lDownStreamNodeNr := StrToIntDef(UnQuote(lShape.Cells[cDownStreamNode].Formula), -1);
          lUpStreamNodeNr := StrToIntDef(UnQuote(lShape.Cells[cUpStreamNode].Formula), -1);
          lChannelNumber := StrToIntDef(UnQuote(lShape.Cells[cNumber].Formula),0);
          if ((lDownStreamNodeNr = AParentNumber) or (lUpStreamNodeNr = AParentNumber)) then
              AShapesList.Add(IntToStr(lChannelNumber));
        end;
        lShapeIdx := lShapeIdx + 1;
      end;
      lPageIdx := lPageIdx + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.FindMineWithNumber(AMineNumber: integer): IVShape;
const OPNAME = 'TVNVEventHandler.FindMineWithNumber';
var
  LNumber,
  lPageIdx        : integer;
  lShapeIdx       : integer;
  lShape          : IVShape;
  lPage           : IVPage;
  lExists         : Boolean;
  lTempStr        : string;
begin
  Result := nil;
  try
    lExists  := FALSE;
    lPageIdx := 1;
    while (NOT lExists) AND (lPageIdx <= FVisioApp.ActiveDocument.Pages.Count) do
    begin
      lPage := FVisioApp.ActiveDocument.Pages[lPageIdx];
      lShapeIdx := 1;
      while (NOT lExists) AND (lShapeIdx <= lPage.Shapes.Count) do
      begin
        lShape := lPage.Shapes[lShapeIdx];
        if ((lShape <> nil) AND (lShape.Master <> nil) AND
            ((Pos(PChar(mtWRYMMine), lShape.Master.Name) > 0))) then
        begin
          lTempStr := UnQuote(lShape.Cells[cNumber].Formula);
          LNumber  := StrToIntDef(lTempStr,-1);
          if (LNumber = AMineNumber) then
          begin
            lExists := TRUE;
            Result  := lShape;
          end;
        end;
        lShapeIdx := lShapeIdx + 1;
      end;
      lPageIdx := lPageIdx + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.FindChannelPenaltyWithNumber(AElementID : integer) : IVShape;
const OPNAME = 'TVNVEventHandler.FindChannelPenaltyWithNumber';
var
  LNumber,
  lPageIdx        : integer;
  lShapeIdx       : integer;
  lShape          : IVShape;
  lPage           : IVPage;
  lExists         : Boolean;
  lTempStr        : string;
begin
  Result := nil;
  try
    lExists  := FALSE;
    lPageIdx := 1;
    while (NOT lExists) AND (lPageIdx <= FVisioApp.ActiveDocument.Pages.Count) do
    begin
      lPage := FVisioApp.ActiveDocument.Pages[lPageIdx];
      lShapeIdx := 1;
      while (NOT lExists) AND (lShapeIdx <= lPage.Shapes.Count) do
      begin
        lShape := lPage.Shapes[lShapeIdx];
        if ((lShape <> nil) AND (lShape <> nil) AND
            (Pos(PChar(mtWRYMChanPenalty), lShape.Name) > 0)) then
        begin
          lTempStr := UnQuote(lShape.Cells[cNumber].Formula);
          LNumber  := StrToIntDef(lTempStr,-1);
          if (LNumber = AElementID) then
          begin
            lExists := TRUE;
            Result  := lShape;
          end;
        end;
        lShapeIdx := lShapeIdx + 1;
      end;
      lPageIdx := lPageIdx + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.FindReservoirPenaltyWithNumber(AElementID : integer) : IVShape;
const OPNAME = 'TVNVEventHandler.FindReservoirPenaltyWithNumber';
var
  LNumber,
  lPageIdx        : integer;
  lShapeIdx       : integer;
  lShape          : IVShape;
  lPage           : IVPage;
  lExists         : Boolean;
  lTempStr        : string;
begin
  Result := nil;
  try
    lExists  := FALSE;
    lPageIdx := 1;
    while (NOT lExists) AND (lPageIdx <= FVisioApp.ActiveDocument.Pages.Count) do
    begin
      lPage := FVisioApp.ActiveDocument.Pages[lPageIdx];
      lShapeIdx := 1;
      while (NOT lExists) AND (lShapeIdx <= lPage.Shapes.Count) do
      begin
        lShape := lPage.Shapes[lShapeIdx];
        if ((lShape <> nil) AND (lShape <> nil) AND
            ((Pos(PChar(mtWRYMResPenalty), lShape.Name) > 0))or
            (Pos(PChar(mtWRYMResPenaltyExt), lShape.Name) > 0)) then
        begin
          lTempStr := UnQuote(lShape.Cells[cNumber].Formula);
          LNumber  := StrToIntDef(lTempStr,-1);
          if (LNumber = AElementID) then
          begin
            lExists := TRUE;
            Result  := lShape;
          end;
        end;
        lShapeIdx := lShapeIdx + 1;
      end;
      lPageIdx := lPageIdx + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



procedure TVNVEventHandler.RefreshIrrigationArea(AShape: IVShape; AElementID: integer);
const OPNAME = 'TVNVEventHandler.RefreshIrrigationArea';
var
  lIrrigationArea      : IIrrigationArea;
  lIrrigationAreaName  : string;
  lKeepColor           : boolean;
begin
  try
    if(AShape = nil) or (AElementID = 0) or (AElementID = NullInteger) then Exit;
    if (FAppModules.StudyArea.ModelVersion <> '7')  then
    begin
      //MessageDlg(FAppModules.Language.GetString('VNV.IrrigationAreaNoModelSupport'));
      Exit;
    end;

    lKeepColor      := (AShape.CellExists[cPermanentColor, 0] <> 0)  and (AShape.Cells[cPermanentColor].Formula = '"TRUE"');
    lIrrigationArea := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.IrrigationAreaList.IrrigationAreaByNodeNumber[AElementID];
    if (lIrrigationArea = nil) then
    begin
      if not lKeepColor then
        AShape.CellsSRC[visSectionObject, visRowFill, visFillForegnd].FormulaU := IntToStr(visRed);
    end
    else
    begin
      if not lKeepColor then
        AShape.CellsSRC[visSectionObject, visRowFill, visFillForegnd].FormulaU := IntToStr(visGreen);
      lIrrigationAreaName := lIrrigationArea.FeatureName;
      AShape.Cells[cName].Formula := '"' + lIrrigationAreaName + '"';;
      AShape.Cells[cRow1Action].Formula := 'RUNMACRO("ThisDocument.ProcessVNVSpecial 1")';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.IrrigationAreaShapeAdded(const AShape: IVShape);
const OPNAME = 'TVNVEventHandler.IrrigationAreaShapeAdded';
var
  LIrrigationAreaID  : integer;
  lTxtLabel          : IVShape;
  lYieldModelData    : IYieldModelData;
  lIrrArea           : IIrrigationArea;
begin
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then
    begin
      MessageDlg(FAppModules.Language.GetString('VNV.IrrigationAreaNoModelSupport'), mtError, [mbOK], 0);
      Exit;
    end;
    if (FBufferIrrAreaList.Count > 0) then
      { Upgraded Irrigation }
      FBufferIrrAreaList.Delete(0)
    else
    begin
      if (AShape.CellExists[cNumber, 0] <> 0) AND (UnQuote(AShape.Cells[cNumber].Formula) <> '') then
      begin
        { Copied Irrigation }
        LIrrigationAreaID  := StrToIntDef(UnQuote(AShape.Cells[cNumber].Formula),0);
      end
      else
      begin
        { New Irrigation - from stencil }
        LIrrigationAreaID := ShowIrrigationAreaDialog;
        if (LIrrigationAreaID <> -1) then
          AShape.Cells[cNumber].Formula  := '"' + IntToStr(LIrrigationAreaID) + '"';
      end;
      AShape.Text := IntToStr(LIrrigationAreaID);
      if (LIrrigationAreaID = -1) then
        AShape.Delete
      else
      begin
        RefreshIrrigationArea(AShape, LIrrigationAreaID);
        lTxtLabel := AddTextLabel(AShape, nvtName);
        RefreshText(lTxtLabel);

        lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
        lIrrArea        := lYieldModelData.NetworkFeaturesData.IrrigationAreaList.IrrigationAreaByNodeNumber[LIrrigationAreaID];
        if(lIrrArea <> nil) then
        begin
          if(lIrrArea.DiversionChannel <> nil) then
          begin
            AddIrrigationAreaChannel(AShape,lIrrArea.DiversionChannel);
          end;
          if(lIrrArea.ConsumptiveChannel <> nil) then
          begin
            AddIrrigationAreaChannel(AShape,lIrrArea.ConsumptiveChannel);
          end;
          if(lIrrArea.ReturnFlowChannel <> nil) then
          begin
            AddIrrigationAreaChannel(AShape,lIrrArea.ReturnFlowChannel);
          end;
        end;
        FIrrigationAreaList.Add(IntToStr(LIrrigationAreaID));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.AddIrrigationAreaChannel(AIrrAreaShape: IVShape; AChannel: IGeneralFlowChannel): IVShape;
const OPNAME = 'TVNVEventHandler.AddIrrigationAreaChannel';
var
  lXVal,
  lYVal        : double;
  lMaster      : IVMaster;
  lTxtLabel    : IVShape;
  LChannelShape: IVShape;
begin
  Result := nil;
  try
    if(AChannel = nil) then Exit;
    AIrrAreaShape.XYToPage(0, 0, lXVal, lYVal);
    lMaster := FindMaster(mtWRYMChannel);
    if (lMaster <> nil) then
    begin
      if AChannel.ChannelSubType = 1 then LChannelShape := FVisioApp.ActivePage.Drop(lMaster, lXVal, lYVal+0.2)
      else if AChannel.ChannelSubType = 2 then LChannelShape := FVisioApp.ActivePage.Drop(lMaster, lXVal, lYVal+0.3)
      else if AChannel.ChannelSubType = 3 then LChannelShape := FVisioApp.ActivePage.Drop(lMaster, lXVal, lYVal+0.5)
      else LChannelShape := FVisioApp.ActivePage.Drop(lMaster, lXVal, lYVal);
      FBufferChannelList.Add(IntToStr(AChannel.ChannelNumber));
      LChannelShape.Text := IntToStr(AChannel.ChannelNumber);
      LChannelShape.Cells[cNumber].Formula  := '"' + IntToStr(AChannel.ChannelNumber) + '"';
      LChannelShape.Cells[cVersion].Formula := '"' + CThisVersion + '"';
      LChannelShape.Cells[cDeletePrevention].Formula := '"' + '1' + '"';
      RefreshChannel(LChannelShape,AChannel.ChannelNumber);
      SnapChannelToNodes(LChannelShape, AChannel.ChannelNumber);
      lTxtLabel := AddTextLabel(LChannelShape, nvtName);
      RefreshText(lTxtLabel);
      FChannelList.Add(IntToStr(AChannel.ChannelNumber));
      Result := LChannelShape;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.IrrigationAreaShapeDeleted( const AShape: IVShape);
const OPNAME = 'TVNVEventHandler.IrrigationAreaShapeDeleted';
var
  lShape          : IVShape;
  lSelection      : IVSelection;
  lElementID      : string;
  lElementIdx     : integer;
  lIrrigationArea : IIrrigationArea;
begin
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then
    begin
      MessageDlg(FAppModules.Language.GetString('VNV.IrrigationAreaNoModelSupport'), mtError, [mbOK], 0);
      Exit;
    end;

    lElementID  := UnQuote(AShape.Cells[cNumber].Formula);
    lElementIdx := FIrrigationAreaList.IndexOf(lElementID);
    if (lElementIdx >= 0) then
    begin
      FIrrigationAreaList.Delete(lElementIdx);
      lSelection := FVisioApp.ActiveWindow.Selection;

      lIrrigationArea := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.IrrigationAreaList.IrrigationAreaByNodeNumber[StrToIntDef(lElementID,-1)];
      if (lIrrigationArea <> nil) then
      begin
        {Remove diversion channel for this irrigation area}
        if(lIrrigationArea.DiversionChannel <> nil) then
        begin
          lShape := FindChannelShapeByChannelNumber(lIrrigationArea.DiversionChannel.ChannelNumber);
          if (lShape <> nil) then
          begin
            if (NOT ShapeIsSelected(lSelection, lShape.Name)) then
            begin
              lShape.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
              lShape.Delete;
            end;
          end;
        end;

        {Remove return flow channel for this irrigation area}
        if(lIrrigationArea.ReturnFlowChannel <> nil) then
        begin
          lShape := FindChannelShapeByChannelNumber(lIrrigationArea.ReturnFlowChannel.ChannelNumber);
          if (lShape <> nil) then
          begin
            if (NOT ShapeIsSelected(lSelection, lShape.Name)) then
            begin
              lShape.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
              lShape.Delete;
            end;
          end;
        end;

        {Remove consumptive channel for this irrigation area}
        if(lIrrigationArea.ConsumptiveChannel <> nil) then
        begin
          lShape := FindChannelShapeByChannelNumber(lIrrigationArea.ConsumptiveChannel.ChannelNumber);
          if (lShape <> nil) then
          begin
            if (NOT ShapeIsSelected(lSelection, lShape.Name)) then
            begin
              lShape.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
              lShape.Delete;
            end;
          end;
        end;
      end;

      {Remove all text annotations for this irrigation area}
      DeleteAllTextShapes(AShape);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.DeleteIrrigationArea(ANumber: integer): WordBool;
const OPNAME = 'TVNVEventHandler.DeleteIrrigationArea';
var
  lShape : IVShape;
begin
  Result := FALSE;
  try
    lShape := FindShapeWithPropertyValue(mtWRYMIrrArea, cNumber, IntToStr(ANumber));
    if (lShape <> nil) AND (MayChangeScenario) then
    begin
      lShape.Delete;
      Result := (FAppModules.Model as IYieldModel).DoDeleteIrrigationArea(ANumber);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ShowIrrigationAreaDialog: integer;
const OPNAME = 'TVNVEventHandler.ShowIrrigationAreaDialog';
var
  LIrrigationAreaDlg : TVNVIrrigationAreaDialog;
  lStrTemp    : string;
begin
  Result := -1;
  try
  	LIrrigationAreaDlg := TVNVIrrigationAreaDialog.CreateWithoutDFM(nil, FAppModules);
    LIrrigationAreaDlg.LanguageHasChanged;
    try
      lStrTemp := FIrrigationAreaList.CommaText;
	    Result   := LIrrigationAreaDlg.ShowIrrigationAreaDialog(lStrTemp, FIrrigationAreaExistNew, FIrrigationAreaDuplicate);
    finally
      LIrrigationAreaDlg.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVEventHandler.IrrigationAreaPositionChanged(ACell: IVCell; AShape: IVShape);
const OPNAME = 'TVNVEventHandler.IrrigationAreaPositionChanged';
var
  lTxtShape    : IVShape;
  lDiff        : double;
  lParentPin   : double;
  lElementID   : integer;
  //lShapePin    : double;
  //lParentName  : string;
  //lParentShape : IVShape;
  //lParentMoved : integer;
begin
  try
    {lParentName := UnQuote(AShape.Cells[cParent].Formula);
    lParentShape := FindShapeWithName(lParentName);
    if (lParentShape <> nil) then
    begin
      if (ACell.Name = cPinX) then
      begin
        lParentMoved := StrToIntDef(UnQuote(AShape.Cells[cParentXMoved].Formula),0);
        if (lParentMoved = 1) then
          AShape.Cells[cParentXMoved].Formula := '"' + IntToStr(0) + '"'
        else
        begin
          lShapePin  := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinX].Formula)),0);
          lParentPin := StrToFloatDef(UnMM(UnQuote(lParentShape.Cells[cPinX].Formula)),0);
          lDiff      := lParentPin - lShapePin;
          AShape.Cells[cParentXDiff].Formula := '"' + FloatToStrF(lDiff, ffFixed, 12, 2) + '"';
        end;
      end
      else
      if (ACell.Name = cPinY) then
      begin
        lParentMoved := StrToIntDef(UnQuote(AShape.Cells[cParentYMoved].Formula),0);
        if (lParentMoved = 1) then
          AShape.Cells[cParentYMoved].Formula := '"' + IntToStr(0) + '"'
        else
        begin
          lShapePin  := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinY].Formula)),0);
          lParentPin := StrToFloatDef(UnMM(UnQuote(lParentShape.Cells[cPinY].Formula)),0);
          lDiff      := lParentPin - lShapePin;
          AShape.Cells[cParentYDiff].Formula := '"' + FloatToStrF(lDiff, ffFixed, 12, 2) + '"';
        end;
      end;
    end;}
    lTxtShape := FindShapeWithParent(mtWRYMText, AShape.Name);
    if (lTxtShape <> nil) then
    begin
      if (ACell.Name = cPinX) then
      begin
        lDiff      := StrToFloatDef(UnQuote(lTxtShape.Cells[cParentXDiff].Formula),0);
        lParentPin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinX].Formula)),0);
        lTxtShape.Cells[cParentXMoved].Formula := '"' + IntToStr(1) + '"';
        lTxtShape.Cells[cPinX].Formula := '"' + FloatToStrF(lParentPin - lDiff, ffFixed, 12, 2) + 'mm"';
        if (FDrawing.GISMode) then
        begin
          lElementID := StrToIntDef(UnQuote(AShape.Cells[cNumber].Formula),0);
          GISCalculateLonLatOfShape(AShape,lElementID,gisCalcLon);
        end;
      end
      else
      if (ACell.Name = cPinY) then
      begin
        lDiff      := StrToFloatDef(UnQuote(lTxtShape.Cells[cParentYDiff].Formula),0);
        lParentPin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinY].Formula)),0);
        lTxtShape.Cells[cParentYMoved].Formula := '"' + IntToStr(1) + '"';
        lTxtShape.Cells[cPinY].Formula := '"' + FloatToStrF(lParentPin - lDiff, ffFixed, 12, 2) + 'mm"';
        if (FDrawing.GISMode) then
        begin
          lElementID := StrToIntDef(UnQuote(AShape.Cells[cNumber].Formula),0);
          GISCalculateLonLatOfShape(AShape,lElementID,gisCalcLat);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVEventHandler.IrrigationAreaHasChanged(AShapeName: string; ANumber: integer);
const OPNAME = 'TVNVEventHandler.IrrigationAreaHasChanged';
var
  lShape           : IVShape;
  lShapesLst       : TStringList;
  lIndex           : integer;
  lShapeName       : string;
  lIrrigationArea  : IIrrigationArea;
begin
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then
    begin
      MessageDlg(FAppModules.Language.GetString('VNV.IrrigationAreaNoModelSupport'), mtError, [mbOK], 0);
      Exit;
    end;

    lIrrigationArea := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.IrrigationAreaList.IrrigationAreaByNodeNumber[ANumber];
    if (lIrrigationArea <> nil) then
    begin
      if(lIrrigationArea.DiversionChannel <> nil) then
      begin
        lShape := FindChannelShapeByChannelNumber(lIrrigationArea.DiversionChannel.ChannelNumber);
        if (lShape <> nil) then
        begin
          SnapChannelToNodes(lShape, lIrrigationArea.DiversionChannel.ChannelNumber);
          ChannelHasChanged(lShape.Name,lIrrigationArea.DiversionChannel.ChannelNumber);
        end;
      end;
      if(lIrrigationArea.ReturnFlowChannel <> nil) then
      begin
        lShape := FindChannelShapeByChannelNumber(lIrrigationArea.ReturnFlowChannel.ChannelNumber);
        if (lShape <> nil) then
        begin
          SnapChannelToNodes(lShape, lIrrigationArea.ReturnFlowChannel.ChannelNumber);
          ChannelHasChanged(lShape.Name,lIrrigationArea.ReturnFlowChannel.ChannelNumber);
        end;
      end;
      if(lIrrigationArea.ConsumptiveChannel <> nil) then
      begin
        lShape := FindChannelShapeByChannelNumber(lIrrigationArea.ConsumptiveChannel.ChannelNumber);
        if (lShape <> nil) then
        begin
          SnapChannelToNodes(lShape, lIrrigationArea.ConsumptiveChannel.ChannelNumber);
          ChannelHasChanged(lShape.Name,lIrrigationArea.ConsumptiveChannel.ChannelNumber);
        end;
      end;
    end;

    lShape := FindShapeWithName(AShapeName);
    if (lShape <> nil) then
      RefreshIrrigationArea(lShape, ANumber);

    lShapesLst := TStringList.Create;
    try
      FindTextShapesWithParent(AShapeName, lShapesLst);
      for lIndex := 0 to lShapesLst.Count - 1 do
      begin
        lShapeName := lShapesLst.Strings[lIndex];
        lShape     := FindShapeWithName(lShapeName);
        if (lShape <> nil) then
          RefreshText(lShape);
      end;
    finally
      FreeAndNil(lShapesLst);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.CheckUpgradeIrrArea(AShape: IVShape);
const OPNAME = 'TVNVEventHandler.CheckUpgradeIrrArea';
begin
  try
    //FBufferIrrAreaList.Add(AShape.Name);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ProcessVNVIrrigationAreaRightClicked(AShapeName: string; ANumber: integer): WordBool;
const OPNAME = 'TVNVEventHandler.ProcessVNVIrrigationAreaRightClicked';
var
  lYieldModel     : IYieldModel;
  lOptionList     : TStringList;
  lMenuDlg        : TVNVShapeMenuDlg;
  lOption         : integer;
  lIrrigationArea : IIrrigationArea;
  LPermanentColor : boolean;
  lShape          : IVShape;
begin
  Result := FALSE;
  try
    lIrrigationArea := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.IrrigationAreaList.IrrigationAreaByNodeNumber[ANumber];
    if (lIrrigationArea <> nil) then
    begin
      lYieldModel := (FAppModules.Model as IYieldModel);
      lOptionList := TStringList.Create;
      try
        lOptionList.AddObject(FAppModules.Language.GetString('VNV.MenuCancelOption'),TObject(1));
        lOptionList.AddObject(FAppModules.Language.GetString('VNV.ViewIrrigationAreaProperties'),TObject(1));
        lOptionList.AddObject(FAppModules.Language.GetString('VNV.DeleteIrrigationAreaFromScenario'),TObject(1));
        LPermanentColor := True;
        lShape := GetFirstSelectedShape(mtWRYMIrrArea);
        if(lShape <> nil) and (lShape.CellExists[cPermanentColor, 0] <> 0) then
        begin
          LPermanentColor := (lShape.Cells[cPermanentColor].Formula = '"TRUE"');
          if LPermanentColor then
            lOptionList.AddObject(FAppModules.Language.GetString('VNV.PermanentColorOff'),TObject(1))
          else
            lOptionList.AddObject(FAppModules.Language.GetString('VNV.PermanentColorOn'),TObject(1));
        end;

        lMenuDlg := TVNVShapeMenuDlg.Create(nil);
        try
          lOption := lMenuDlg.ShowShapeMenuDialog(lOptionList);
          case lOption of
            1 : begin
                  Result := lYieldModel.ViewInputIrrigationAreaDialog(ANumber);
                  if (Result) then
                    IrrigationAreaHasChanged(AShapeName, ANumber);
                end;
            2 : Result := DeleteIrrigationArea(ANumber);
            3 : begin
                  if LPermanentColor then
                    lShape.Cells[cPermanentColor].Formula         := '"FALSE"'
                  else
                    lShape.Cells[cPermanentColor].Formula         := '"TRUE"';
                  Result := True;
                end;
          end;
        finally
          lMenuDlg.Free;
        end;
      finally
        FreeAndNil(lOptionList);
      end
    end
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.CheckUpgradeMineReservoir(AShape: IVShape);
const OPNAME = 'TVNVEventHandler.CheckUpgradeMineReservoir';
begin
  try
    FBufferMineList.Add(AShape.Name);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.GetPCDIdentifier(AMineIdentifier : integer): integer;
const OPNAME = 'TVNVEventHandler.GetPCDIdentifier';
var
  LMine               : IMine;
  lYieldModelData     : IYieldModelData;
begin
  Result := 0;
  try
    lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
    LMine := lYieldModelData.NetworkFeaturesData.MineList.MineByNodeNumber[AMineIdentifier];
    if(LMine <> nil) then
      if(LMine.PolutionControlDam <> nil) then
        Result := LMine.PolutionControlDam.ReservoirConfigurationData.ReservoirIdentifier;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.GetUnderGroundIdentifier(AMineIdentifier,AUnderGroundIdentifier: integer): integer;
const OPNAME = 'TVNVEventHandler.GetUnderGroundIdentifier';
var
  LMine               : IMine;
  lYieldModelData     : IYieldModelData;
begin
  Result := 0;
  try
    lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
    LMine := lYieldModelData.NetworkFeaturesData.MineList.MineByNodeNumber[AMineIdentifier];
    if(LMine <> nil) then
      if(LMine.UnderGroundByIndex[AUnderGroundIdentifier] <> nil)  and
        (LMine.UnderGroundByIndex[AUnderGroundIdentifier].UndergroundDam <> nil) then
        Result := LMine.UnderGroundByIndex[AUnderGroundIdentifier].UndergroundDam.ReservoirConfigurationData.ReservoirIdentifier;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.UpgradeMineReservoir(AShape: IVShape): IVShape;
const OPNAME = 'TVNVEventHandler.UpgradeMineReservoir';
var
  lElementID        : integer;
  lYieldModelData   : IYieldModelData;
  lMine             : IMine;
  LCount            : Integer;
  LUnderground      : Integer;
  LReservoirShape   : IVShape;
  LPCD              : Integer;
begin
  Result := nil;
  try
    lElementID  := StrToIntDef(UnQuote(AShape.Cells[cNumber].Formula),0);

    lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
    lMine := lYieldModelData.NetworkFeaturesData.MineList.MineByNodeNumber[lElementID];

    LPCD := GetPCDIdentifier(LMine.NodeNumber);
    LReservoirShape := FindReservoirWithNumber(LPCD);
    if LReservoirShape <> nil then
      LReservoirShape.Cells[cParentMine].Formula := '"' + IntToStr(lElementID) + '"';

    for LCount := 0 to lMine.UndergroundCount-1 do
    begin
      LUnderground := GetUnderGroundIdentifier(LMine.NodeNumber,LCount);
      LReservoirShape := FindReservoirWithNumber(LUnderground);
      if LReservoirShape <> nil then
        LReservoirShape.Cells[cParentMine].Formula := '"' + IntToStr(lElementID) + '"';
    end;

    Result := AShape;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.FindReservoirByParentProperty(AElementid : integer; AShapesList : TStringList);
const OPNAME = 'TVNVEventHandler.FindReservoirByParentProperty';
var
  lPageIdx        : integer;
  lShapeIdx       : integer;
  lShape          : IVShape;
  lPage           : IVPage;
  lTempStr        : string;
begin
  try
    AShapesList.Clear;
    lPageIdx := 1;
    while (lPageIdx <= FVisioApp.ActiveDocument.Pages.Count) do
    begin
      lPage := FVisioApp.ActiveDocument.Pages[lPageIdx];
      lShapeIdx := 1;
      while (lShapeIdx <= lPage.Shapes.Count) do
      begin
        lShape := lPage.Shapes[lShapeIdx];
        if ((lShape <> nil) AND (Pos(PChar(mtWRYMReservoir), lShape.Name) > 0)) then
        begin
          lTempStr := UnQuote(lShape.Cells[cParentMine].Formula);
            if (lTempStr = IntToStr(AElementid)) then
              AShapesList.Add(lShape.Name);
        end;
        lShapeIdx := lShapeIdx + 1;
      end;
      lPageIdx := lPageIdx + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.FindWetlandByElementID(AElementID : integer) : IVShape;
const OPNAME = 'TVNVEventHandler.FindWetlandByElementID';
var
  lPageIdx        : integer;
  lShapeIdx       : integer;
  lShape          : IVShape;
  lPage           : IVPage;
  lTempStr        : string;
  LNumber         : integer;
begin
  try
    lPageIdx := 1;
    while (lPageIdx <= FVisioApp.ActiveDocument.Pages.Count) do
    begin
      lPage := FVisioApp.ActiveDocument.Pages[lPageIdx];
      lShapeIdx := 1;
      while (lShapeIdx <= lPage.Shapes.Count) do
      begin
        lShape := lPage.Shapes[lShapeIdx];
        if ((lShape <> nil) AND (Pos(PChar(mtWRYMWetland), lShape.Name) > 0)) then
        begin
          lTempStr := UnQuote(lShape.Cells[cNumber].Formula);
          LNumber  := StrToIntDef(lTempStr,-1);
          if (LNumber = AElementID) then
            Result  := lShape;
        end;
        lShapeIdx := lShapeIdx + 1;
      end;
      lPageIdx := lPageIdx + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TVNVEventHandler.FindIrrBlockByElementID(AElementID : integer) : IVShape;
const OPNAME = 'TVNVEventHandler.FindIrrBlockByElementID';
var
  lPageIdx        : integer;
  lShapeIdx       : integer;
  lShape          : IVShape;
  lPage           : IVPage;
  lTempStr        : string;
  LNumber         : integer;
begin
  try
    lPageIdx := 1;
    while (lPageIdx <= FVisioApp.ActiveDocument.Pages.Count) do
    begin
      lPage := FVisioApp.ActiveDocument.Pages[lPageIdx];
      lShapeIdx := 1;
      while (lShapeIdx <= lPage.Shapes.Count) do
      begin
        lShape := lPage.Shapes[lShapeIdx];
        if ((lShape <> nil) AND (Pos(PChar(mtWRYMIrrBlock), lShape.Name) > 0)) then
        begin
          lTempStr := UnQuote(lShape.Cells[cNumber].Formula);
          LNumber  := StrToIntDef(lTempStr,-1);
          if (LNumber = AElementID) then
            Result  := lShape;
        end;
        lShapeIdx := lShapeIdx + 1;
      end;
      lPageIdx := lPageIdx + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.FindPowerPlantByElementID(AElementID : integer) : IVShape;
const OPNAME = 'TVNVEventHandler.FindPowerPlantByElementID';
var
  lPageIdx        : integer;
  lShapeIdx       : integer;
  lShape          : IVShape;
  lPage           : IVPage;
  lTempStr        : string;
  LNumber         : integer;
begin
  try
    lPageIdx := 1;
    while (lPageIdx <= FVisioApp.ActiveDocument.Pages.Count) do
    begin
      lPage := FVisioApp.ActiveDocument.Pages[lPageIdx];
      lShapeIdx := 1;
      while (lShapeIdx <= lPage.Shapes.Count) do
      begin
        lShape := lPage.Shapes[lShapeIdx];
        if ((lShape <> nil) AND (Pos(PChar(mtWRYMPowerPlant), lShape.Name) > 0)) then
        begin
          lTempStr := UnQuote(lShape.Cells[cNumber].Formula);
          LNumber  := StrToIntDef(lTempStr,-1);
          if (LNumber = AElementID) then
            Result  := lShape;
        end;
        lShapeIdx := lShapeIdx + 1;
      end;
      lPageIdx := lPageIdx + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ViewCurtialment(AShapeName: string; AChannelNumber: integer): WordBool;
const OPNAME = 'TVNVEventHandler.ViewCurtialment';
var
  LViewContext: string;
begin
  Result := False;
  try
    LViewContext := ViewModelDataContextDataCommaText(mdvnCurtailments,AChannelNumber);
    FAppModules.Model.ViewInputPopupDialog(nil,LViewContext,nil);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ViewChannelDroughtRestriction(AShapeName: string; AChannelNumber: integer): WordBool;
const OPNAME = 'TVNVEventHandler.ViewChannelDroughtRestriction';
var
  lYieldModelData        : IYieldModelData;
  LCurtailmentAndDrought : ICurtailmentAndDrought;
  LDroughtRestriction    : IDroughtRestriction;
  LViewContext           : string;
  LIdentifiers           : string;
  LDroughtRestrictionID  : Integer;
begin
  Result := False;
  try
    lYieldModelData        := FAppModules.Model.ModelData as IYieldModelData;
    LCurtailmentAndDrought := lYieldModelData.NetworkFeaturesData.CurtailmentAndDrought;
    LIdentifiers           := LCurtailmentAndDrought.DroughtRestrictionByChannelNumber[AChannelNumber];
    LDroughtRestrictionID  := ShowDroughtRestrictionDialog(LIdentifiers);
    LDroughtRestriction    := LCurtailmentAndDrought.DroughtRestrictionByID[LDroughtRestrictionID];
    if(LDroughtRestriction <> nil) then
    begin
      if(LDroughtRestriction.ChannelCount = 0) then
        LDroughtRestriction.ChannelNumbers := IntToStr(AChannelNumber)
      else
        LDroughtRestriction.ChannelNumbers := LDroughtRestriction.ChannelNumbers + ',' + IntToStr(AChannelNumber);
      LViewContext := ViewModelDataContextDataCommaText(mdvnRestrictions,LDroughtRestrictionID);
      FAppModules.Model.ViewInputPopupDialog(nil,LViewContext,nil);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ViewReservoirDroughtRestriction( AShapeName: string; AReservoirNumber: integer): WordBool;
const OPNAME = 'TVNVEventHandler.ViewReservoirDroughtRestriction';
var
  lYieldModelData        : IYieldModelData;
  LCurtailmentAndDrought : ICurtailmentAndDrought;
  LDroughtRestriction    : IDroughtRestriction;
  LViewContext           : string;
  LIdentifiers           : string;
  LDroughtRestrictionID  : Integer;
begin
  Result := False;
  try
    lYieldModelData        := FAppModules.Model.ModelData as IYieldModelData;
    LCurtailmentAndDrought := lYieldModelData.NetworkFeaturesData.CurtailmentAndDrought;
    LIdentifiers           := LCurtailmentAndDrought.DroughtRestrictionByReservoirNumber[AReservoirNumber];
    LDroughtRestrictionID  := ShowDroughtRestrictionDialog(LIdentifiers);
    LDroughtRestriction    := LCurtailmentAndDrought.DroughtRestrictionByID[LDroughtRestrictionID];
    if(LDroughtRestriction <> nil) then
    begin
      if(LDroughtRestriction.ReservoirCount = 0) then
        LDroughtRestriction.ReservoirNumbers := IntToStr(AReservoirNumber)
      else
        LDroughtRestriction.ReservoirNumbers := LDroughtRestriction.ReservoirNumbers + ',' + IntToStr(AReservoirNumber);

      LViewContext := ViewModelDataContextDataCommaText(mdvnRestrictions,LDroughtRestrictionID);
      FAppModules.Model.ViewInputPopupDialog(nil,LViewContext,nil);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ShowDroughtRestrictionDialog(AUsedRestrictionIDs : WideString) : integer;
const OPNAME = 'TVNVEventHandler.ShowDroughtRestrictionDialog';
var
  LDroughtRestrictionDialog : TVNVDroughtRestrictionDialog;
  LDroughtRestrictionCount,
  LUseExisting     : integer;
  LShowAll         : integer;
begin
  Result := -1;
  try
  	LDroughtRestrictionDialog := TVNVDroughtRestrictionDialog.CreateWithoutDFM(nil, FAppModules);
    LDroughtRestrictionDialog.LanguageHasChanged;
    try
      LDroughtRestrictionCount := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.CurtailmentAndDrought.DroughtRestrictionCount;
      if(LDroughtRestrictionCount = 0) then
        LUseExisting := 0
      else
        LUseExisting := 1;

      LShowAll := 0;
      if(LDroughtRestrictionCount > 0) then
      begin
        if (AUsedRestrictionIDs = '') then
          LShowAll := 1
        else
          LShowAll := 0;
      end;
	    Result   := LDroughtRestrictionDialog.ShowDroughtRestrictionDialog(AUsedRestrictionIDs, LUseExisting, LShowAll);
    finally
      LDroughtRestrictionDialog.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

//DSR---------------------------------------------------------------------------------------------------------------

function TVNVEventHandler.ShowGroundWaterDialog: integer;
const OPNAME = 'TVNVEventHandler.ShowGroundWaterDialog';
var
  LGroundWaterDlg  : TVNVGroundWaterDialog;
  lStrTemp   : string;
begin
  Result := -1;
  try
  	LGroundWaterDlg := TVNVGroundWaterDialog.CreateWithoutDFM(nil, FAppModules);
    LGroundWaterDlg.LanguageHasChanged;
    try
      lStrTemp := FGroundWaterList.CommaText;
	    Result   := LGroundWaterDlg.ShowGroundWaterDialog(lStrTemp, FGroundWaterExistNew, FGroundWaterDuplicate);
    finally
      LGroundWaterDlg.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVEventHandler.RefreshGroundWater(AShape: IVShape; AElementID: integer);
const OPNAME = 'TVNVEventHandler.RefreshGroundWater';
var
  lReservoir   : IReservoirData;
  lResName     : string;
  lGroundWater : IGroundWater;
  lKeepColor   : boolean;
begin
  try
    if(AShape = nil) or (AElementID = 0) or (AElementID = NullInteger) then Exit;

    lKeepColor := (AShape.CellExists[cPermanentColor, 0] <> 0)  and (AShape.Cells[cPermanentColor].Formula = '"TRUE"');
    lGroundWater := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.GroundWaterList.GroundWaterByNodeNumber[AElementID];
    if (lGroundWater = nil) then
    begin
      if not lKeepColor then
        AShape.CellsSRC[visSectionObject, visRowFill, visFillForegnd].FormulaU := IntToStr(visRed);
    end
    else
    begin
      lReservoir := lGroundWater.AquiferNode;
      if (lReservoir = nil) then
      begin
        if not lKeepColor then
          AShape.CellsSRC[visSectionObject, visRowFill, visFillForegnd].FormulaU := IntToStr(visRed);
      end
      else
      begin
        if not lKeepColor then
          AShape.CellsSRC[visSectionObject, visRowFill, visFillForegnd].FormulaU := IntToStr(visCyan);
        FVisioApp.ActiveWindow.Windows.ItemFromID[visWinIDCustProp].Visible := FALSE;
        lResName := lReservoir.ReservoirConfigurationData.ReservoirName;
        AShape.Cells[cName].Formula := '"' + lResName + '"';;
        if (lReservoir.ReservoirPenaltyStructureData <> nil) then
          AShape.Cells[cPenalty].Formula := '"' + IntToStr(lReservoir.ReservoirPenaltyStructureData.ReservoirPenaltyID) + '"'
        else
          AShape.Cells[cPenalty].Formula := '""';
        if (lReservoir.ReservoirConfigurationData.StatusIndicator = 0) then
          AShape.CellsSRC[visSectionObject, visRowLine, visLinePattern].FormulaU := '10';
        AShape.Cells[cRow1Action].Formula := 'RUNMACRO("ThisDocument.ProcessVNVSpecial 1")';

      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
procedure TVNVEventHandler.RefreshPowerPlant(AShape: IVShape;  AElementID: integer);
const OPNAME = 'TVNVEventHandler.RefreshPowerPlant';
var
  lPowerPlant  : IPowerPlant;
  lPowerPlantName : string;
  lKeepColor   : boolean;
begin
  try
    if(AShape = nil) or (AElementID = 0) or (AElementID = NullInteger) then Exit;

    if (FAppModules.StudyArea.ModelVersion <> '7')  then Exit;

    lKeepColor := (AShape.CellExists[cPermanentColor, 0] <> 0)  and (AShape.Cells[cPermanentColor].Formula = '"TRUE"');
    lPowerPlant := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.PowerPlantList.PowerPlantByID[AElementID];
    if (lPowerPlant = nil) then
    begin
      if not lKeepColor then
        AShape.CellsSRC[visSectionObject, visRowFill, visFillForegnd].FormulaU := IntToStr(visRed);
    end
    else
    begin
      if not lKeepColor then
        AShape.CellsSRC[visSectionObject, visRowFill, visFillForegnd].FormulaU := IntToStr(visBlue);

      lPowerPlantName := lPowerPlant.FeatureName;
      AShape.Cells[cName].Formula := '"' + lPowerPlantName + '"';;
      AShape.Cells[cRow1Action].Formula := 'RUNMACRO("ThisDocument.ProcessVNVSpecial 1")';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.GroundWaterShapeAdded(const AShape: IVShape);
const OPNAME = 'TVNVEventHandler.GroundWaterShapeAdded';
var
  lAquiferNodeNumber : Integer;
  lTxtLabel          : IVShape;
  lYieldModelData    : IYieldModelData;
  lGroundWater       : IGroundWater;
begin
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then
    begin
      MessageDlg(FAppModules.Language.GetString('VNV.GroundWaterNoModelSupport'), mtError, [mbOK], 0);
      Exit;
    end;
    if(not (FAppModules.Model.ModelData as IYieldModelData).ImplementedNetworkFeatures.GroundWaterFeatureImplemented) then
    begin
      MessageDlg(FAppModules.Language.GetString('VNV.GroundWaterNoModelSupport'), mtError, [mbOK], 0);
      Exit;
    end;

    if (FBufferGroundWaterList.Count > 0) then
      { Upgraded Ground Water }
      FBufferGroundWaterList.Delete(0)
    else
    begin
      if (AShape.CellExists[cNumber, 0] <> 0) AND (UnQuote(AShape.Cells[cNumber].Formula) <> '') then
      begin
        { Copied Ground Water }
        lAquiferNodeNumber  := StrToIntDef(UnQuote(AShape.Cells[cNumber].Formula),0);
      end
      else
      begin
        { New Ground Water - from stencil }
        lAquiferNodeNumber := ShowGroundWaterDialog;
        if (lAquiferNodeNumber <> -1) then
        begin
          lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
          lGroundWater := lYieldModelData.NetworkFeaturesData.GroundWaterList.GroundWaterByNodeNumber[lAquiferNodeNumber];
          AShape.Cells[cNumber].Formula  := '"' + IntToStr(lAquiferNodeNumber) + '"';
          AShape.Cells[cParentGroundWater].Formula := '"' + IntToStr(lGroundWater.Identifier) + '"';
          AShape.Cells[cVersion].Formula := '"' + CThisVersion + '"';
        end;
      end;
      AShape.Text := IntToStr(lAquiferNodeNumber);
      if (lAquiferNodeNumber = -1) then
        AShape.Delete
      else
      begin
        RefreshGroundWater(AShape, lAquiferNodeNumber);
        FGroundWaterList.Add(IntToStr(lAquiferNodeNumber));

        if (FDrawing.GISMode) then
          if CheckExistingWithCoords(AShape, lAquiferNodeNumber) then
          begin
            GISPopulateDoNotRecalcCoordsList(lAquiferNodeNumber);
            GISSetXYPosFromLonLat(AShape, lAquiferNodeNumber)
          end;

        lTxtLabel := AddTextLabel(AShape, nvtName);
        RefreshText(lTxtLabel);

        lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
        lGroundWater := lYieldModelData.NetworkFeaturesData.GroundWaterList.GroundWaterByNodeNumber[lAquiferNodeNumber];
        if(lGroundWater <> nil) then
        begin
          AddGroundWaterNodes(AShape,lGroundWater);
          AddGroundWaterChannels(AShape,lGroundWater);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.AddGroundWaterNodes(AAquiferShape: IVShape; AGroundWater : IGroundWater): boolean;
const OPNAME = 'TVNVEventHandler.AddGroundWaterNodes';
var
  lXVal,
  lYVal        : double;
  lMaster      : IVMaster;
  lTxtLabel    : IVShape;
  lShape       : IVShape;
  LNodeNumber  : integer;
begin
  Result := False;
  try
    if(AGroundWater = nil) or (AAquiferShape = nil) then Exit;

    if(AGroundWater.BaseFlowNode = nil) or
      (AGroundWater.AbstractionNode = nil) or
      (AGroundWater.CollectionNode = nil) then  Exit;

    AAquiferShape.XYToPage(0, 0, lXVal, lYVal);

    lMaster := FindMaster(mtWRYMNode);
    if (lMaster <> nil) then
    begin
      LNodeNumber := AGroundWater.CollectionNode.ReservoirConfigurationData.ReservoirIdentifier;
      FNodeList.Add(IntToStr(LNodeNumber));
      lShape := FVisioApp.ActivePage.Drop(lMaster, lXVal+2.5, lYVal+0.2);
      lShape.Cells[cNumber].Formula  := '"' + IntToStr(LNodeNumber) + '"';
      lShape.Cells[cVersion].Formula := '"2.12"';
      lShape.Cells[cDeletePrevention].Formula := '"' + IntToStr(1) + '"';
      lTxtLabel := AddTextLabel(lShape, nvtName);
      RefreshText(lTxtLabel);
    end;

    lMaster := FindMaster(mtWRYMNode);
    if (lMaster <> nil) then
    begin
      LNodeNumber := AGroundWater.BaseFlowNode.ReservoirConfigurationData.ReservoirIdentifier;
      FNodeList.Add(IntToStr(LNodeNumber));
      lShape := FVisioApp.ActivePage.Drop(lMaster, lXVal+0.2, lYVal-1);
      lShape.Cells[cNumber].Formula  := '"' + IntToStr(LNodeNumber) + '"';
      lShape.Cells[cVersion].Formula := '"2.12"';
      lShape.Cells[cDeletePrevention].Formula := '"' + IntToStr(1) + '"';
      lTxtLabel := AddTextLabel(lShape, nvtName);
      RefreshText(lTxtLabel);
    end;

    lMaster := FindMaster(mtWRYMNode);
    if (lMaster <> nil) then
    begin
      LNodeNumber := AGroundWater.AbstractionNode.ReservoirConfigurationData.ReservoirIdentifier;
      FNodeList.Add(IntToStr(LNodeNumber));
      lShape := FVisioApp.ActivePage.Drop(lMaster, lXVal+1.5, lYVal-1);
      lShape.Cells[cNumber].Formula  := '"' + IntToStr(LNodeNumber) + '"';
      lShape.Cells[cVersion].Formula := '"2.12"';
      lShape.Cells[cDeletePrevention].Formula := '"' + IntToStr(1) + '"';
      lTxtLabel := AddTextLabel(lShape, nvtName);
      RefreshText(lTxtLabel);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.AddGroundWaterChannels(AAquiferShape: IVShape; AGroundWater : IGroundWater): boolean;
const OPNAME = 'TVNVEventHandler.AddGroundWaterChannels';
var
  lXVal,
  lYVal        : double;
  lMaster      : IVMaster;
  lTxtLabel    : IVShape;
  LChannelShape: IVShape;
begin
  Result := False;
  try
    if(AGroundWater = nil) or (AAquiferShape = nil) then Exit;

    if(AGroundWater.AquiferInflowChannel = nil) or
      (AGroundWater.AquiferExcessInterflowChannel = nil) or
      (AGroundWater.GroundWaterBaseflowChannel = nil) or
      (AGroundWater.AbstractionFromAquiferChannel = nil) or
      (AGroundWater.AbstractionFromBaseFlowChannel = nil) or
      (AGroundWater.OutflowToDownstreamAquiferChannel = nil) or
      (AGroundWater.SurfaceRunoffAndSoilInterflowChannel = nil) then  Exit;

    AAquiferShape.XYToPage(-1, 0, lXVal, lYVal);

    lMaster := FindMaster(mtWRYMChannel);
    if (lMaster <> nil) then
    begin
      LChannelShape := FVisioApp.ActivePage.Drop(lMaster, lXVal, lYVal);
      LChannelShape.Text := IntToStr(AGroundWater.AquiferInflowChannel.ChannelNumber);
      FBufferChannelList.Add(IntToStr(AGroundWater.AquiferInflowChannel.ChannelNumber));
      LChannelShape.Cells[cNumber].Formula  := '"' + IntToStr(AGroundWater.AquiferInflowChannel.ChannelNumber) + '"';
      LChannelShape.Cells[cVersion].Formula := '"' + CThisVersion + '"';
      LChannelShape.Cells[cDeletePrevention].Formula := '"' + IntToStr(1) + '"';
      RefreshChannel(LChannelShape,AGroundWater.AquiferInflowChannel.ChannelNumber);
      SnapChannelToNodes(LChannelShape, AGroundWater.AquiferInflowChannel.ChannelNumber, -1);
      lTxtLabel := AddTextLabel(LChannelShape, nvtName);
      RefreshText(lTxtLabel);
      FChannelList.Add(IntToStr(AGroundWater.AquiferInflowChannel.ChannelNumber));
    end;

    lMaster := FindMaster(mtWRYMChannel);
    if (lMaster <> nil) then
    begin
      LChannelShape := FVisioApp.ActivePage.Drop(lMaster, lXVal, lYVal);
      LChannelShape.Text := IntToStr(AGroundWater.AquiferExcessInterflowChannel.ChannelNumber);
      FBufferChannelList.Add(IntToStr(AGroundWater.AquiferExcessInterflowChannel.ChannelNumber));
      LChannelShape.Cells[cNumber].Formula  := '"' + IntToStr(AGroundWater.AquiferExcessInterflowChannel.ChannelNumber) + '"';
      LChannelShape.Cells[cVersion].Formula := '"' + CThisVersion + '"';
      LChannelShape.Cells[cDeletePrevention].Formula := '"' + IntToStr(1) + '"';
      RefreshChannel(LChannelShape,AGroundWater.AquiferExcessInterflowChannel.ChannelNumber);
      SnapChannelToNodes(LChannelShape, AGroundWater.AquiferExcessInterflowChannel.ChannelNumber, -1);
      lTxtLabel := AddTextLabel(LChannelShape, nvtName);
      RefreshText(lTxtLabel);
      FChannelList.Add(IntToStr(AGroundWater.AquiferExcessInterflowChannel.ChannelNumber));
    end;

    lMaster := FindMaster(mtWRYMChannel);
    if (lMaster <> nil) then
    begin
      LChannelShape := FVisioApp.ActivePage.Drop(lMaster, lXVal, lYVal);
      LChannelShape.Text := IntToStr(AGroundWater.GroundWaterBaseflowChannel.ChannelNumber);
      FBufferChannelList.Add(IntToStr(AGroundWater.GroundWaterBaseflowChannel.ChannelNumber));
      LChannelShape.Cells[cNumber].Formula  := '"' + IntToStr(AGroundWater.GroundWaterBaseflowChannel.ChannelNumber) + '"';
      LChannelShape.Cells[cVersion].Formula := '"' + CThisVersion + '"';
      LChannelShape.Cells[cDeletePrevention].Formula := '"' + IntToStr(1) + '"';
      RefreshChannel(LChannelShape,AGroundWater.GroundWaterBaseflowChannel.ChannelNumber);
      SnapChannelToNodes(LChannelShape, AGroundWater.GroundWaterBaseflowChannel.ChannelNumber, -1);
      lTxtLabel := AddTextLabel(LChannelShape, nvtName);
      RefreshText(lTxtLabel);
      FChannelList.Add(IntToStr(AGroundWater.GroundWaterBaseflowChannel.ChannelNumber));
    end;

    lMaster := FindMaster(mtWRYMChannel);
    if (lMaster <> nil) then
    begin
      LChannelShape := FVisioApp.ActivePage.Drop(lMaster, lXVal, lYVal);
      LChannelShape.Text := IntToStr(AGroundWater.AbstractionFromAquiferChannel.ChannelNumber);
      FBufferChannelList.Add(IntToStr(AGroundWater.AbstractionFromAquiferChannel.ChannelNumber));
      LChannelShape.Cells[cNumber].Formula  := '"' + IntToStr(AGroundWater.AbstractionFromAquiferChannel.ChannelNumber) + '"';
      LChannelShape.Cells[cVersion].Formula := '"' + CThisVersion + '"';
      LChannelShape.Cells[cDeletePrevention].Formula := '"' + IntToStr(1) + '"';
      RefreshChannel(LChannelShape,AGroundWater.AbstractionFromAquiferChannel.ChannelNumber);
      SnapChannelToNodes(LChannelShape, AGroundWater.AbstractionFromAquiferChannel.ChannelNumber, -1);
      lTxtLabel := AddTextLabel(LChannelShape, nvtName);
      RefreshText(lTxtLabel);
      FChannelList.Add(IntToStr(AGroundWater.AbstractionFromAquiferChannel.ChannelNumber));
    end;

    lMaster := FindMaster(mtWRYMChannel);
    if (lMaster <> nil) then
    begin
      LChannelShape := FVisioApp.ActivePage.Drop(lMaster, lXVal, lYVal);
      LChannelShape.Text := IntToStr(AGroundWater.AbstractionFromBaseFlowChannel.ChannelNumber);
      FBufferChannelList.Add(IntToStr(AGroundWater.AbstractionFromBaseFlowChannel.ChannelNumber));
      LChannelShape.Cells[cNumber].Formula  := '"' + IntToStr(AGroundWater.AbstractionFromBaseFlowChannel.ChannelNumber) + '"';
      LChannelShape.Cells[cVersion].Formula := '"' + CThisVersion + '"';
      LChannelShape.Cells[cDeletePrevention].Formula := '"' + IntToStr(1) + '"';
      RefreshChannel(LChannelShape,AGroundWater.AbstractionFromBaseFlowChannel.ChannelNumber);
      SnapChannelToNodes(LChannelShape, AGroundWater.AbstractionFromBaseFlowChannel.ChannelNumber, -1);
      lTxtLabel := AddTextLabel(LChannelShape, nvtName);
      RefreshText(lTxtLabel);
      FChannelList.Add(IntToStr(AGroundWater.AbstractionFromBaseFlowChannel.ChannelNumber));
    end;

    lMaster := FindMaster(mtWRYMChannel);
    if (lMaster <> nil) then
    begin
      LChannelShape := FVisioApp.ActivePage.Drop(lMaster, lXVal, lYVal);
      LChannelShape.Text := IntToStr(AGroundWater.OutflowToDownstreamAquiferChannel.ChannelNumber);
      FBufferChannelList.Add(IntToStr(AGroundWater.OutflowToDownstreamAquiferChannel.ChannelNumber));
      LChannelShape.Cells[cNumber].Formula  := '"' + IntToStr(AGroundWater.OutflowToDownstreamAquiferChannel.ChannelNumber) + '"';
      LChannelShape.Cells[cVersion].Formula := '"' + CThisVersion + '"';
      LChannelShape.Cells[cDeletePrevention].Formula := '"' + IntToStr(1) + '"';
      RefreshChannel(LChannelShape,AGroundWater.OutflowToDownstreamAquiferChannel.ChannelNumber);
      SnapChannelToNodes(LChannelShape, AGroundWater.OutflowToDownstreamAquiferChannel.ChannelNumber, -1);
      lTxtLabel := AddTextLabel(LChannelShape, nvtName);
      RefreshText(lTxtLabel);
      FChannelList.Add(IntToStr(AGroundWater.OutflowToDownstreamAquiferChannel.ChannelNumber));
    end;

    lMaster := FindMaster(mtWRYMChannel);
    if (lMaster <> nil) then
    begin
      LChannelShape := FVisioApp.ActivePage.Drop(lMaster, lXVal, lYVal);
      LChannelShape.Text := IntToStr(AGroundWater.SurfaceRunoffAndSoilInterflowChannel.ChannelNumber);
      FBufferChannelList.Add(IntToStr(AGroundWater.SurfaceRunoffAndSoilInterflowChannel.ChannelNumber));
      LChannelShape.Cells[cNumber].Formula  := '"' + IntToStr(AGroundWater.SurfaceRunoffAndSoilInterflowChannel.ChannelNumber) + '"';
      LChannelShape.Cells[cVersion].Formula := '"' + CThisVersion + '"';
      LChannelShape.Cells[cDeletePrevention].Formula := '"' + IntToStr(1) + '"';
      RefreshChannel(LChannelShape,AGroundWater.SurfaceRunoffAndSoilInterflowChannel.ChannelNumber);
      SnapChannelToNodes(LChannelShape, AGroundWater.SurfaceRunoffAndSoilInterflowChannel.ChannelNumber, -1);
      lTxtLabel := AddTextLabel(LChannelShape, nvtName);
      RefreshText(lTxtLabel);
      FChannelList.Add(IntToStr(AGroundWater.SurfaceRunoffAndSoilInterflowChannel.ChannelNumber));
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.DeleteGroundWater(AAquiferNodeNumber: integer; ADBDelete : integer = -1) : WordBool;
const OPNAME = 'TVNVEventHandler.DeleteGroundWater';
var
  lShape : IVShape;
  lYieldModelData     : IYieldModelData;
  lGroundWater        : IGroundWater;
begin
  Result := FALSE;
  try
    lShape := FindShapeWithPropertyValue(mtWRYMGroundWater, cNumber, IntToStr(AAquiferNodeNumber));
    if (lShape <> nil) AND (MayChangeScenario) then
    begin
      lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
      lGroundWater    := lYieldModelData.NetworkFeaturesData.GroundWaterList.GroundWaterByNodeNumber[AAquiferNodeNumber];
      if(lGroundWater <> nil) then
      begin
        lShape.Delete;
        if ADBDelete = -1 then
          Result := (FAppModules.Model as IYieldModel).DoDeleteGroundWater(lGroundWater.Identifier);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.GroundWaterShapeDeleted (const AShape : IVShape);
const OPNAME = 'TVNVEventHandler.GroundWaterShapeDeleted';
var
  lAquiferNodeNumber  : Integer;
  lAquiferNodeIdx     : integer;
  lYieldModelData     : IYieldModelData;
  lGroundWater        : IGroundWater;
  LChannelNumber      : integer;
  LNodeNumber         : integer;
  LNodeShape          : IVShape;
  LChannelShape       : IVShape;
begin
  try
    lAquiferNodeNumber := StrToInt(UnQuote(AShape.Cells[cNumber].Formula));
    lAquiferNodeIdx    := FGroundWaterList.IndexOf(IntToStr(lAquiferNodeNumber));

    if (lAquiferNodeIdx >= 0) then
    begin

      lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
      lGroundWater    := lYieldModelData.NetworkFeaturesData.GroundWaterList.GroundWaterByNodeNumber[lAquiferNodeNumber];
      if(lGroundWater <> nil) then
      begin
        LChannelNumber := lGroundWater.AquiferInflowChannelNr;
        LChannelShape := FindChannelShapeByChannelNumber(LChannelNumber);
        if(LChannelShape <> nil) then
        begin
          LChannelShape.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
          DeleteChannel(LChannelNumber,1);
        end;

        LChannelNumber := lGroundWater.AquiferExcessInterflowChannelNr;
        LChannelShape := FindChannelShapeByChannelNumber(LChannelNumber);
        if(LChannelShape <> nil) then
        begin
          LChannelShape.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
          DeleteChannel(LChannelNumber,1);
        end;

        LChannelNumber := lGroundWater.GroundWaterBaseflowChannelNr;
        LChannelShape := FindChannelShapeByChannelNumber(LChannelNumber);
        if(LChannelShape <> nil) then
        begin
          LChannelShape.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
          DeleteChannel(LChannelNumber,1);
        end;

        LChannelNumber := lGroundWater.AbstractionFromAquiferChannelNr;
        LChannelShape := FindChannelShapeByChannelNumber(LChannelNumber);
        if(LChannelShape <> nil) then
        begin
          LChannelShape.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
          DeleteChannel(LChannelNumber,1);
        end;

        LChannelNumber := lGroundWater.AbstractionFromBaseflowChannelNr;
        LChannelShape := FindChannelShapeByChannelNumber(LChannelNumber);
        if(LChannelShape <> nil) then
        begin
          LChannelShape.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
          DeleteChannel(LChannelNumber,1);
        end;

        LChannelNumber := lGroundWater.OutflowToDownstreamAquiferChannelNr;
        LChannelShape := FindChannelShapeByChannelNumber(LChannelNumber);
        if(LChannelShape <> nil) then
        begin
          LChannelShape.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
          DeleteChannel(LChannelNumber,1);
        end;

        LChannelNumber := lGroundWater.SurfaceRunoffAndSoilInterflowChannelNr;
        LChannelShape := FindChannelShapeByChannelNumber(LChannelNumber);
        if(LChannelShape <> nil) then
        begin
          LChannelShape.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
          DeleteChannel(LChannelNumber,1);
        end;

        LNodeNumber := lGroundWater.AbstractionNodeNr;
        LNodeShape := FindNodeShapeByNodeNumber(LNodeNumber);
        if(LNodeShape <> nil) then
        begin
          LNodeShape.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
          DeleteNode(LNodeNumber,1);
        end;

        LNodeNumber := lGroundWater.BaseFlowNodeNr;
        LNodeShape := FindNodeShapeByNodeNumber(LNodeNumber);
        if(LNodeShape <> nil) then
        begin
          LNodeShape.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
          DeleteNode(LNodeNumber,1);
        end;

        LNodeNumber := lGroundWater.CollectionNodeNr;
        LNodeShape := FindNodeShapeByNodeNumber(LNodeNumber);
        if(LNodeShape <> nil) then
        begin
          LNodeShape.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
          DeleteNode(LNodeNumber,1);
        end;

        DeleteAllTextShapes(AShape);

        DeleteAllShapesWithParent(AShape);

        FGroundWaterList.Delete(lAquiferNodeIdx);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ProcessVNVGroundWaterRightClicked(AShapeName: string; ANumber: integer): WordBool;
const OPNAME = 'TVNVEventHandler.ProcessVNVGroundWaterRightClicked';
var
  lYieldModel : IYieldModel;
  lOptionList : TStringList;
  lMenuDlg    : TVNVShapeMenuDlg;
  lOption     : integer;
  LPermanentColor : boolean;
  lShape          : IVShape;
begin
  Result := FALSE;
  try
    lYieldModel := (FAppModules.Model as IYieldModel);
    lOptionList := TStringList.Create;
    try
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.MenuCancelOption'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.ViewGroundWaterProperties'),TObject(1));
      lOptionList.AddObject(FAppModules.Language.GetString('VNV.DeleteGroundWaterFromScenario'),TObject(1));
      LPermanentColor := True;
      lShape := GetFirstSelectedShape(mtWRYMGroundWater);
      if(lShape <> nil) and (lShape.CellExists[cPermanentColor, 0] <> 0) then
      begin
        LPermanentColor := (lShape.Cells[cPermanentColor].Formula = '"TRUE"');
        if LPermanentColor then
          lOptionList.AddObject(FAppModules.Language.GetString('VNV.PermanentColorOff'),TObject(1))
        else
          lOptionList.AddObject(FAppModules.Language.GetString('VNV.PermanentColorOn'),TObject(1));
      end;

      lMenuDlg := TVNVShapeMenuDlg.Create(nil);
      try
        lOption := lMenuDlg.ShowShapeMenuDialog(lOptionList);
        case lOption of
          1 : begin
                Result := lYieldModel.ViewInputGroundwaterDialog(ANumber);
                if (Result) then
                  GroundWaterHasChanged(AShapeName, ANumber);
              end;
          2 : Result := DeleteGroundWater(ANumber,-1);
          3 : begin
                if LPermanentColor then
                  lShape.Cells[cPermanentColor].Formula         := '"FALSE"'
                else
                  lShape.Cells[cPermanentColor].Formula         := '"TRUE"';
                Result := True;
              end;
        else
        end;
      finally
        lMenuDlg.Free;
      end;
    finally
      FreeAndNil(lOptionList);
    end
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.GroundWaterHasChanged(AShapeName: string; AAquiferNodeNumber: integer);
const OPNAME = 'TVNVEventHandler.GroundWaterHasChanged';
var
  lShape          : IVShape;
begin
  try
    lShape := FindGroundWaterWithNumber(AAquiferNodeNumber);
    if(lShape <> nil) then
      RefreshGroundWater(lShape,AAquiferNodeNumber);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.CheckUpgradeGroundWaterAquifer(AShape: IVShape);
const OPNAME = 'TVNVEventHandler.CheckUpgradeGroundWaterAquifer';
begin
  try
    FBufferGroundWaterList.Add(AShape.Name);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.FindGroundWaterWithNumber(AAquiferNodeNumber: integer): IVShape;
const OPNAME = 'TVNVEventHandler.FindGroundWaterWithNumber';
var
  LNumber,
  lPageIdx        : integer;
  lShapeIdx       : integer;
  lShape          : IVShape;
  lPage           : IVPage;
  lExists         : Boolean;
  lTempStr        : string;
begin
  Result := nil;
  try
    lExists  := FALSE;
    lPageIdx := 1;
    while (NOT lExists) AND (lPageIdx <= FVisioApp.ActiveDocument.Pages.Count) do
    begin
      lPage := FVisioApp.ActiveDocument.Pages[lPageIdx];
      lShapeIdx := 1;
      while (NOT lExists) AND (lShapeIdx <= lPage.Shapes.Count) do
      begin
        lShape := lPage.Shapes[lShapeIdx];
        if ((lShape <> nil) AND (lShape.Master <> nil) AND
            ((Pos(PChar(mtWRYMGroundWater), lShape.Master.Name) > 0))) then
        begin
          lTempStr := UnQuote(lShape.Cells[cNumber].Formula);
          LNumber  := StrToIntDef(lTempStr,-1);
          if (LNumber = AAquiferNodeNumber) then
          begin
            lExists := TRUE;
            Result  := lShape;
          end;
        end;
        lShapeIdx := lShapeIdx + 1;
      end;
      lPageIdx := lPageIdx + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.UpgradeGroundWaterAquifer(AShape: IVShape): IVShape;
const OPNAME = 'TVNVEventHandler.UpgradeGroundWaterAquifer';
var
  lAquiferNodeNumber  : Integer;
  lYieldModelData     : IYieldModelData;
  lGroundWater        : IGroundWater;
begin
  Result := nil;
  try
    lAquiferNodeNumber  := StrToIntDef(UnQuote(AShape.Cells[cNumber].Formula),0);

    lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
    lGroundWater    := lYieldModelData.NetworkFeaturesData.GroundWaterList.GroundWaterByNodeNumber[lAquiferNodeNumber];
    if(lGroundWater <> nil) then
    begin
      AShape.Cells[cParentGroundWater].Formula := '"' + IntToStr(lGroundWater.Identifier) + '"';
      AShape.Cells[cVersion].Formula           := '"' + CThisVersion + '"';
    end;

    Result := AShape;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.AddAllChannelsToDrawing : boolean;
const OPNAME = 'TVNVEventHandler.AddAllChannelsToDrawing';
var
  LIndex          : Integer;
  LContainer      : TStringList;
  lYieldModelData : IYieldModelData;
  LChannelNumber  : string;
  lMaster      : IVMaster;
  LChannelShape: IVShape;
begin
  Result := False;
  try
    LContainer := TStringList.Create;
    try
      LContainer.Sorted := True;
      LContainer.Duplicates := dupIgnore;

      lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
      for LIndex := 0 to lYieldModelData.NetworkElementData.ChannelList.ChannelCount-1 do
      begin
        LChannelNumber := IntToStr(lYieldModelData.NetworkElementData.ChannelList.ChannelByIndex[LIndex].ChannelNumber);
        if(FChannelList.IndexOf(LChannelNumber) < 0) then
          LContainer.Add(LChannelNumber);
      end;

      for LIndex := 0 to LContainer.Count-1 do
      begin
        lMaster := FindMaster(mtWRYMChannel);

        if (lMaster <> nil) then
        begin
          LChannelNumber := LContainer[LIndex];
          FBufferChannelList.Add(LChannelNumber);
          LChannelShape  := FVisioApp.ActivePage.Drop(lMaster, 0, 0);
          ChannelShapeAdded(LChannelShape,StrToInt(LChannelNumber));
        end;
      end;
      Result := True;
    finally
      LContainer.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.AddShapeToLayer(const ALayersInterface : IVLayers;const ALayer : IVLayer;const AShape: IVShape):boolean;
const OPNAME = 'TVNVEventHandler.AddNewLayer';
var
  LIndex : integer;
begin
  Result := False;
  try
    if (ALayersInterface <> nil) and (ALayer <> nil) and (AShape <> nil) then
    begin
      if (AShape.LayerCount>0) then
      begin
        for LIndex := 1 to ALayersInterface.Count do
        if ((Trim(UpperCase(ALayersInterface.Item[LIndex].Name)) = 'CATEGORIES') or
           (Trim(UpperCase(ALayersInterface.Item[LIndex].Name)) = 'CONNECTOR') or
           (Trim(UpperCase(ALayersInterface.Item[LIndex].Name)) = 'LABELS') or
           (Trim(UpperCase(ALayersInterface.Item[LIndex].Name)) = 'GIS BMP') or
           (Trim(UpperCase(ALayersInterface.Item[LIndex].Name)) = 'FLOWCHART')) then
          ALayersInterface.Item[LIndex].Remove(AShape,0);
      end;
      ALayer.Add(AShape,0);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.AddTextShapeToLayer(const ALayersInterface : IVLayers;const ALayer : IVLayer;const AShape: IVShape):boolean;
const OPNAME = 'TVNVEventHandler.AddTextShapeToLayer';
var
  LShapesLst : TStringList;
  LIndex : integer;
  LTextShape : IVShape;
begin
  Result := False;
  try
    if (ALayersInterface <> nil) and (ALayer <> nil) and (AShape <> nil) then
    begin
      LShapesLst := TStringList.Create;
      try
        FindTextShapesWithParent(AShape.Name, LShapesLst);
        for LIndex := 0 to LShapesLst.Count - 1 do
        begin
          LTextShape  := FindShapeWithName(LShapesLst.Strings[LIndex]);
          if (LTextShape <> nil) then
            AddShapeToLayer(ALayersInterface,ALayer,LTextShape);
        end;
      finally
        FreeAndNil(LShapesLst);
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.AddNewLayer(var ALayersInterface : IVLayers;var ALayer : IVLayer):boolean;
const OPNAME = 'TVNVEventHandler.AddNewLayer';
var
  LLayers : IVLayers;
  LPage : IVPage;
  LCount : integer;
  LLayerExist : boolean;
  LLayerName : WideString;
  LExistingLayerName : WideString;
begin
  Result := False;
  try
    LLayerName := InputBox(FAppModules.Language.GetString('VNV.EnterNewLayerName'),
                  FAppModules.Language.GetString('VNV.LayerName'),'');
    if Trim(LLayerName) <> '' then
    begin
      LPage := FVisioApp.ActivePage;
      if LPage <> nil then
      begin
        LLayers := LPage.Layers;
        ALayersInterface := LLayers;
        LLayerExist := False;
        for LCount := 1 to LLayers.Count do
        begin
          LExistingLayerName := LLayers.ItemU[LCount].Name;
          if LLayerName = LExistingLayerName then
            LLayerExist := True;
        end;
        if not LLayerExist then
          ALayer := LLayers.Add(LLayerName)
        else
          ShowMessage('Layer: '+LLayerName+' Already Exist.');
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.AddAllReservoirPenaltyLayer: boolean;
const OPNAME = 'TVNVEventHandler.AddAllReservoirPenaltyLayer';
var
  LShape : IVShape;
  LElementID : integer;
  LLayers : IVLayers;
  LLayer : IVLayer;
  LCount : integer;
begin
  Result := False;
  try
    if AddNewLayer(LLayers,LLayer) then
    begin
      for LCount := 0 to FResPenaltyList.Count-1 do
      begin
        LElementID := StrToIntDef(FResPenaltyList[LCount],0);
        LShape := FindReservoirPenaltyWithNumber(LElementID);
        if LShape <> nil then
          AddShapeToLayer(LLayers,LLayer,LShape);
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ClearAllReservoirPenaltiesFromLayer: boolean;
const OPNAME = 'TVNVEventHandler.ClearAllReservoirPenaltiesFromLayer';
var
  LShape : IVShape;
  LElementID : integer;
  LCount : integer;
begin
  Result := False;
  try
    for LCount := 0 to FResPenaltyList.Count-1 do
    begin
      LElementID := StrToIntDef(FResPenaltyList[LCount],0);
      LShape := FindReservoirPenaltyWithNumber(LElementID);
      if LShape <> nil then
        ClearShapeFromLayers(LShape);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ClearShapeFromLayers(AShape : IVShape):boolean;
const OPNAME = 'TVNVEventHandler.ClearShapeFromLayers';
var
  LLayers : IVLayers;
  LLayer : IVLayer;
  LPage : IVPage;
  LIndex : integer;
  LCount : integer;
begin
  Result := False;
  try
    if (AShape <> nil) then
    begin
      if (AShape.LayerCount>0) then
      begin
        LPage := FVisioApp.ActivePage;
        if LPage <> nil then
        begin
          LLayers := LPage.Layers;
          if LLayers <> nil then
          begin
            LCount := 1;
            while LCount<=AShape.LayerCount do
            begin
              if LCount>AShape.LayerCount then Break;
              for LIndex := 1 to LLayers.Count do
              begin
                LLayer := nil;
                LLayer := LLayers.Item[LIndex];
                if (AShape.LayerCount>0) then
                begin
                  if LLayer <> nil then
                  begin
                    if((Trim(UpperCase(LLayer.Name)) = Trim(UpperCase(AShape.Layer[LCount].Name))) and
                       (Trim(UpperCase(LLayer.Name)) <> 'CATEGORIES') and
                       (Trim(UpperCase(LLayer.Name)) <> 'CONNECTOR') and
                       (Trim(UpperCase(LLayer.Name)) <> 'LABELS') and
                       (Trim(UpperCase(LLayer.Name)) <> 'GIS BMP') and
                       (Trim(UpperCase(LLayer.Name)) <> 'FLOWCHART')) then
                     begin
                       LLayers.Item[LIndex].Remove(AShape,0);
                       LLayers.Item[LIndex].Delete(0);
                       Break;
                     end;
                  end;
                end;
              end;
              LCount := LCount+1;
            end;

          end;
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ClearTextShapesFromLayers(AShape : IVShape):boolean;
const OPNAME = 'TVNVEventHandler.ClearTextShapesFromLayers';
var
  LShapesLst : TStringList;
  LIndex : integer;
  LTextShape : IVShape;
begin
  Result := False;
  try
    LShapesLst := TStringList.Create;
    try
      FindTextShapesWithParent(AShape.Name, LShapesLst);
      for LIndex := 0 to LShapesLst.Count - 1 do
      begin
        LTextShape  := FindShapeWithName(LShapesLst.Strings[LIndex]);
        if (LTextShape <> nil) then
          ClearShapeFromLayers(LTextShape);
      end;
    finally
      FreeAndNil(LShapesLst);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TVNVEventHandler.AddAllReservoirToALayer: boolean;
const OPNAME = 'TVNVEventHandler.AddAllReservoirToALayer';
var
  LShape : IVShape;
  LCount : integer;
  LElementID : integer;
  LInflowShape : IVShape;
  LSFRShape : IVShape;
  LLayers : IVLayers;
  LLayer : IVLayer;
begin
  Result := False;
  try
    if AddNewLayer(LLayers,LLayer) then
    begin
      for LCount := 0 to FReservoirList.Count-1 do
      begin
        LElementID := StrToIntDef(FReservoirList[LCount],0);
        LShape := FindReservoirWithNumber(LElementID);
        if LShape <> nil then
        begin
          AddShapeToLayer(LLayers,LLayer,LShape);
          AddTextShapeToLayer(LLayers,LLayer,LShape);
          if (FBufferNodeHasSFRAList.IndexOf(LShape.Name) >= 0) then
          begin
            LSFRShape := FindShapeWithParent(mtWRYMSubCatchment, LShape.Name);
            if LSFRShape <> nil then
            begin
              AddShapeToLayer(LLayers,LLayer,LSFRShape);
              AddTextShapeToLayer(LLayers,LLayer,LSFRShape);
            end;
          end;
        end;
        LInflowShape := FindInflowShapeByReservoirNumber(LElementID);
        if LInflowShape <> nil then
        begin
          AddShapeToLayer(LLayers,LLayer,LInflowShape);
          AddTextShapeToLayer(LLayers,LLayer,LInflowShape);
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ClearAllReservoirFromLayers: boolean;
const OPNAME = 'TVNVEventHandler.ClearAllReservoirFromLayers';
var
  LShape : IVShape;
  LCount : integer;
  LElementID : integer;
  LInflowShape : IVShape;
  LSFRShape : IVShape;
begin
  Result := False;
  try
    for LCount := 0 to FReservoirList.Count-1 do
    begin
      LElementID := StrToIntDef(FReservoirList[LCount],0);
      LShape := FindReservoirWithNumber(LElementID);
      if LShape <> nil then
      begin
        ClearShapeFromLayers(LShape);
        ClearTextShapesFromLayers(LShape);
        if (FBufferNodeHasSFRAList.IndexOf(LShape.Name) >= 0) then
        begin
          LSFRShape := FindShapeWithParent(mtWRYMSubCatchment, LShape.Name);
          if LSFRShape <> nil then
          begin
            ClearShapeFromLayers(LSFRShape);
            ClearTextShapesFromLayers(LSFRShape);
          end;
        end;
      end;
      LInflowShape := FindInflowShapeByReservoirNumber(LElementID);
      if LInflowShape <> nil then
      begin
        ClearShapeFromLayers(LInflowShape);
        ClearTextShapesFromLayers(LInflowShape);
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.AddAllChannelPenaltyLayer: boolean;
const OPNAME = 'TVNVEventHandler.AddAllChannelPenaltyLayer';
var
  LShape : IVShape;
  LCount : integer;
  LElementID : integer;
  LLayers : IVLayers;
  LLayer : IVLayer;
begin
  Result := False;
  try
    if AddNewLayer(LLayers,LLayer) then
    begin
      for LCount := 0 to FChanPenaltyList.Count-1 do
      begin
        LElementID := StrToIntDef(FChanPenaltyList[LCount],0);
        LShape := FindChannelPenaltyWithNumber(LElementID);
        if LShape <> nil then
          AddShapeToLayer(LLayers,LLayer,LShape);
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ClearAllChannelPenaltyLayer: boolean;
const OPNAME = 'TVNVEventHandler.ClearAllChannelPenaltyLayer';
var
  LShape : IVShape;
  LCount : integer;
  LElementID : integer;
begin
  Result := False;
  try
    for LCount := 0 to FChanPenaltyList.Count-1 do
    begin
      LElementID := StrToIntDef(FChanPenaltyList[LCount],0);
      LShape := FindChannelPenaltyWithNumber(LElementID);
      if LShape <> nil then
        ClearShapeFromLayers(LShape);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TVNVEventHandler.AddAllChannelAndReservoirPenaltyLayer: boolean;
const OPNAME = 'TVNVEventHandler.AddAllChannelAndReservoirPenaltyLayer';
var
  LShape : IVShape;
  LCount : integer;
  LElementID : integer;
  LLayers : IVLayers;
  LLayer : IVLayer;
begin
  Result := False;
  try
    if AddNewLayer(LLayers,LLayer) then
    begin
      for LCount := 0 to FResPenaltyList.Count-1 do
      begin
        LElementID := StrToIntDef(FResPenaltyList[LCount],0);
        LShape := FindReservoirPenaltyWithNumber(LElementID);
        if LShape <> nil then
          AddShapeToLayer(LLayers,LLayer,LShape);
      end;

      for LCount := 0 to FChanPenaltyList.Count-1 do
      begin
        LElementID := StrToIntDef(FChanPenaltyList[LCount],0);
        LShape := FindChannelPenaltyWithNumber(LElementID);
        if LShape <> nil then
          AddShapeToLayer(LLayers,LLayer,LShape);
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ClearAllChannelAndReservoirPenaltyLayer:boolean;
const OPNAME = 'TVNVEventHandler.ClearAllChannelAndReservoirPenaltyLayer';
var
  LShape : IVShape;
  LCount : integer;
  LElementID : integer;
begin
  Result := False;
  try
    for LCount := 0 to FResPenaltyList.Count-1 do
    begin
      LElementID := StrToIntDef(FResPenaltyList[LCount],0);
      LShape := FindReservoirPenaltyWithNumber(LElementID);
      if LShape <> nil then
        ClearShapeFromLayers(LShape);
    end;

    for LCount := 0 to FChanPenaltyList.Count-1 do
    begin
      LElementID := StrToIntDef(FChanPenaltyList[LCount],0);
      LShape := FindChannelPenaltyWithNumber(LElementID);
      if LShape <> nil then
        ClearShapeFromLayers(LShape);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.AddAllAverageChannelFlowsToALayer: boolean;
const OPNAME = 'TVNVEventHandler.AddAllAverageChannelFlowsToALayer';
var
  LShape : IVShape;
  LCount : integer;
  LContainer : TInterfaceList;
  LLayers : IVLayers;
  LLayer : IVLayer;
begin
  Result := False;
  try
    if AddNewLayer(LLayers,LLayer) then
    begin
      LContainer := TInterfaceList.Create;
      try
        LContainer.Clear;
        GetOutputTextShape(LContainer,nvtAvgChannelFlow);
        for LCount := 0 to LContainer.Count-1 do
        begin
          LShape := IVShape(LContainer[LCount]);
          if LShape <> nil then
            AddShapeToLayer(LLayers,LLayer,LShape);
        end;
      finally
        FreeAndNil(LContainer);
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ClearAllAverageChannelFlowsFromLayers: boolean;
const OPNAME = 'TVNVEventHandler.ClearAllAverageChannelFlowsFromLayers';
var
  LShape : IVShape;
  LCount : integer;
  LContainer : TInterfaceList;
begin
  Result := False;
  try
    LContainer := TInterfaceList.Create;
    try
      LContainer.Clear;
      GetOutputTextShape(LContainer,nvtAvgChannelFlow);
      for LCount := 0 to LContainer.Count-1 do
      begin
        LShape := IVShape(LContainer[LCount]);
        if LShape <> nil then
          ClearShapeFromLayers(LShape);
      end;
    finally
      FreeAndNil(LContainer);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TVNVEventHandler.AddAllWetlandsToALayer: boolean;
const OPNAME = 'TVNVEventHandler.AddAllWetlandsToALayer';
var
  LShape : IVShape;
  LCount : integer;
  LElementID : integer;
  LWetland    : IWetland;
  LInflowShape : IVShape;
  LInflowChannelShape: IVShape;
  LPenaltyShape: IVShape;
  LOutflowChannelShape: IVShape;
  LLayers : IVLayers;
  LLayer : IVLayer;
begin
  Result := False;
  try
    if AddNewLayer(LLayers,LLayer) then
    begin
      for LCount := 0 to FWetlandList.Count-1 do
      begin
        LElementID := StrToIntDef(FWetlandList[LCount],-1);
        LShape := FindWetlandByElementID(LElementID);
        if LShape <> nil then
        begin
          AddShapeToLayer(LLayers,LLayer,LShape);
          AddTextShapeToLayer(LLayers,LLayer,LShape);

          LWetland := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.WetlandList.WetlandByNodeNumber[LElementID];
          if (LWetland <> nil) then
          begin
            LInflowShape := FindShapeWithParent(mtWRYMInflow, LShape.Name);
            if (LInflowShape <> nil) then
            begin
              AddShapeToLayer(LLayers,LLayer,LInflowShape);
              AddTextShapeToLayer(LLayers,LLayer,LInflowShape);
            end;
            if(LWetland.InflowChannel <> nil) then
            begin
              LInflowChannelShape := FindChannelShapeByChannelNumber(lWetland.InflowChannel.ChannelNumber);
              if (LInflowChannelShape <> nil) then
              begin
                AddShapeToLayer(LLayers,LLayer,LInflowChannelShape);
                AddTextShapeToLayer(LLayers,LLayer,LInflowChannelShape);
              end;
            end;
            if(LWetland.OutflowChannel <> nil) then
            begin
              LOutflowChannelShape := FindChannelShapeByChannelNumber(LWetland.OutflowChannel.ChannelNumber);
              if (LOutflowChannelShape <> nil) then
              begin
                AddShapeToLayer(LLayers,LLayer,LOutflowChannelShape);
                AddTextShapeToLayer(LLayers,LLayer,LOutflowChannelShape);
              end;
              LPenaltyShape := FindShapeWithParent(mtWRYMResPenalty, LShape.Name);
              if (LPenaltyShape <> nil) then
                AddShapeToLayer(LLayers,LLayer,LPenaltyShape)
              else
              begin
                LPenaltyShape := FindShapeWithParent(mtWRYMResPenaltyExt, LShape.Name);
                if (LPenaltyShape <> nil) then
                  AddShapeToLayer(LLayers,LLayer,LPenaltyShape)
              end;
            end;
          end;
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ClearAllWetlandsFromLayers:boolean;
const OPNAME = 'TVNVEventHandler.ClearAllWetlandsFromLayers';
var
  LShape : IVShape;
  LCount : integer;
  LElementID : integer;
  LWetland    : IWetland;
  LInflowShape : IVShape;
  LInflowChannelShape: IVShape;
  LPenaltyShape: IVShape;
  LOutflowChannelShape: IVShape;
begin
  Result := False;
  try
    for LCount := 0 to FWetlandList.Count-1 do
    begin
      LElementID := StrToIntDef(FWetlandList[LCount],-1);
      LShape := FindWetlandByElementID(LElementID);
      if LShape <> nil then
      begin
        ClearShapeFromLayers(LShape);
        ClearTextShapesFromLayers(LShape);
        LWetland := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.WetlandList.WetlandByNodeNumber[LElementID];
        if (LWetland <> nil) then
        begin
          LInflowShape := FindShapeWithParent(mtWRYMInflow, LShape.Name);
          if (LInflowShape <> nil) then
          begin
            ClearShapeFromLayers(LInflowShape);
            ClearTextShapesFromLayers(LInflowShape);
          end;
          if(LWetland.InflowChannel <> nil) then
          begin
            LInflowChannelShape := FindChannelShapeByChannelNumber(lWetland.InflowChannel.ChannelNumber);
            if (LInflowChannelShape <> nil) then
            begin
              ClearShapeFromLayers(LInflowChannelShape);
              ClearTextShapesFromLayers(LInflowChannelShape);
            end;
          end;
          if(LWetland.OutflowChannel <> nil) then
          begin
            LOutflowChannelShape := FindChannelShapeByChannelNumber(LWetland.OutflowChannel.ChannelNumber);
            if (LOutflowChannelShape <> nil) then
            begin
              ClearShapeFromLayers(LOutflowChannelShape);
              ClearTextShapesFromLayers(LOutflowChannelShape);
            end;
            LPenaltyShape := FindShapeWithParent(mtWRYMResPenalty, LShape.Name);
            if (LPenaltyShape <> nil) then
              ClearShapeFromLayers(LPenaltyShape)
            else
            begin
              LPenaltyShape := FindShapeWithParent(mtWRYMResPenaltyExt, LShape.Name);
              if (LPenaltyShape <> nil) then
                ClearShapeFromLayers(LPenaltyShape);
            end;
          end;
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TVNVEventHandler.AddAllIrrigationBlockToALayer: boolean;
const OPNAME = 'TVNVEventHandler.AddAllIrrigationBlockToALayer';
var
  LShape : IVShape;
  LCount : integer;
  LElementID : integer;
  LIrrigationBlock : IIrrigationBlock;
  LDiversionChannelShape,
  LReturnFlowChannelShape : IVShape;
  LLayers : IVLayers;
  LLayer : IVLayer;
begin
  Result := False;
  try
    if AddNewLayer(LLayers,LLayer) then
    begin
      for LCount := 0 to FIrrigationList.Count-1 do
      begin
        LElementID := StrToIntDef(FIrrigationList[LCount],-1);
        LShape := FindIrrBlockByElementID(LElementID);
        if LShape <> nil then
        begin
          AddShapeToLayer(LLayers,LLayer,LShape);
          AddTextShapeToLayer(LLayers,LLayer,LShape);
          LIrrigationBlock := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[lElementID];
          if (LIrrigationBlock <> nil) then
          begin
            if(LIrrigationBlock.DiversionChannel <> nil) then
            begin
              LDiversionChannelShape := FindChannelShapeByChannelNumber(LIrrigationBlock.DiversionChannel.ChannelNumber);
              if (LDiversionChannelShape <> nil) then
              begin
                AddShapeToLayer(LLayers,LLayer,LDiversionChannelShape);
                AddTextShapeToLayer(LLayers,LLayer,LDiversionChannelShape);
              end;
            end;
            if(LIrrigationBlock.ReturnFlowChannel <> nil) then
            begin
              LReturnFlowChannelShape := FindChannelShapeByChannelNumber(LIrrigationBlock.ReturnFlowChannel.ChannelNumber);
              if (LReturnFlowChannelShape <> nil) then
              begin
                AddShapeToLayer(LLayers,LLayer,LReturnFlowChannelShape);
                AddTextShapeToLayer(LLayers,LLayer,LReturnFlowChannelShape);
              end;
            end;
          end;
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ClearAllIrrigationBlocksFromLayers: boolean;
const OPNAME = 'TVNVEventHandler.ClearAllIrrigationBlocksFromLayers';
var
  LShape : IVShape;
  LCount : integer;
  LElementID : integer;
  LIrrigationBlock : IIrrigationBlock;
  LDiversionChannelShape,
  LReturnFlowChannelShape : IVShape;
begin
  Result := False;
  try
    for LCount := 0 to FIrrigationList.Count-1 do
    begin
      LElementID := StrToIntDef(FIrrigationList[LCount],-1);
      LShape := FindIrrBlockByElementID(LElementID);
      if LShape <> nil then
      begin
        ClearShapeFromLayers(LShape);
        ClearTextShapesFromLayers(LShape);
        LIrrigationBlock := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[lElementID];
        if (LIrrigationBlock <> nil) then
        begin
          if(LIrrigationBlock.DiversionChannel <> nil) then
          begin
            LDiversionChannelShape := FindChannelShapeByChannelNumber(LIrrigationBlock.DiversionChannel.ChannelNumber);
            if (LDiversionChannelShape <> nil) then
            begin
              ClearShapeFromLayers(LDiversionChannelShape);
              ClearTextShapesFromLayers(LDiversionChannelShape);
            end;
          end;
          if(LIrrigationBlock.ReturnFlowChannel <> nil) then
          begin
            LReturnFlowChannelShape := FindChannelShapeByChannelNumber(LIrrigationBlock.ReturnFlowChannel.ChannelNumber);
            if (LReturnFlowChannelShape <> nil) then
            begin
              ClearShapeFromLayers(LReturnFlowChannelShape);
              ClearTextShapesFromLayers(LReturnFlowChannelShape);
            end;
          end;
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.AddAllNodesWithoutInflowToALayer: boolean;
const OPNAME = 'TVNVEventHandler.AddAllNodesWithoutInflowToALayer';
var
  LSFRShape,
  LShape : IVShape;
  LElementID : integer;
  LLayers : IVLayers;
  LLayer : IVLayer;
  LCount : integer;
  LYieldModelData : IYieldModelData;
  LNode : IReservoirData;
begin
  Result := False;
  try
    if AddNewLayer(LLayers,LLayer) then
    begin
      LYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
      if LYieldModelData <> nil then
      begin
        for LCount := 0 to FNodeList.Count-1 do
        begin
          LElementID := StrToIntDef(FNodeList[LCount],0);
          LNode := LYieldModelData.NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[LElementID];
          if(LNode = nil) then Continue;
          if(LNode.ReservoirConfigurationData.NodeType =  ntNodeWithoutInflow) then
          begin
            LShape := FindNodeShapeByNodeNumber(LElementID);
            if LShape <> nil then
            begin
              AddShapeToLayer(LLayers,LLayer,LShape);
              AddTextShapeToLayer(LLayers,LLayer,LShape);
              if (FBufferNodeHasSFRAList.IndexOf(LShape.Name) >= 0) then
              begin
                LSFRShape := FindShapeWithParent(mtWRYMSubCatchment, LShape.Name);
                if LSFRShape <> nil then
                begin
                  AddShapeToLayer(LLayers,LLayer,LSFRShape);
                  AddTextShapeToLayer(LLayers,LLayer,LSFRShape);
                end;
              end;
            end;
          end;
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ClearAllNodesWithoutInflowFromLayers: boolean;
const OPNAME = 'TVNVEventHandler.ClearAllNodesWithoutInflowFromLayers';
var
  LSFRShape,
  LShape : IVShape;
  LElementID : integer;
  LCount : integer;
  LYieldModelData : IYieldModelData;
  LNode : IReservoirData;
begin
  Result := False;
  try
    LYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
    if LYieldModelData <> nil then
    begin
      for LCount := 0 to FNodeList.Count-1 do
      begin
        LElementID := StrToIntDef(FNodeList[LCount],0);
        LNode := LYieldModelData.NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[LElementID];
        if(LNode = nil) then Continue;
        if(LNode.ReservoirConfigurationData.NodeType =  ntNodeWithoutInflow) then
        begin
          LShape := FindNodeShapeByNodeNumber(LElementID);
          if LShape <> nil then
          begin
            ClearShapeFromLayers(LShape);
            ClearTextShapesFromLayers(LShape);
            if (FBufferNodeHasSFRAList.IndexOf(LShape.Name) >= 0) then
            begin
              LSFRShape := FindShapeWithParent(mtWRYMSubCatchment, LShape.Name);
              if LSFRShape <> nil then
              begin
                ClearShapeFromLayers(LSFRShape);
                ClearTextShapesFromLayers(LSFRShape);
              end;
            end;
          end;
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TVNVEventHandler.AddAllNodesWithInflowToALayer: boolean;
const OPNAME = 'TVNVEventHandler.AddAllNodesWithInflowToALayer';
var
  LInflowShape,
  LSFRShape,
  LShape : IVShape;
  LElementID : integer;
  LLayers : IVLayers;
  LLayer : IVLayer;
  LCount : integer;
  LYieldModelData : IYieldModelData;
  LNode : IReservoirData;
begin
  Result := False;
  try
    if AddNewLayer(LLayers,LLayer) then
    begin
      LYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
      if LYieldModelData <> nil then
      begin
        for LCount := 0 to FNodeList.Count-1 do
        begin
          LElementID := StrToIntDef(FNodeList[LCount],0);
          LNode := LYieldModelData.NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[LElementID];
          if(LNode = nil) then Continue;
          if(LNode.ReservoirConfigurationData.NodeType =  ntNodeWithInflow) then
          begin
            LShape := FindNodeShapeByNodeNumber(LElementID);
            if LShape <> nil then
            begin
              AddShapeToLayer(LLayers,LLayer,LShape);
              AddTextShapeToLayer(LLayers,LLayer,LShape);
              LInflowShape := FindShapeWithParent(mtWRYMInflow, LShape.Name);
              if (LInflowShape <> nil) then
              begin
                AddShapeToLayer(LLayers,LLayer,LInflowShape);
                AddTextShapeToLayer(LLayers,LLayer,LInflowShape);
              end;
              if (FBufferNodeHasSFRAList.IndexOf(LShape.Name) >= 0) then
              begin
                LSFRShape := FindShapeWithParent(mtWRYMSubCatchment, LShape.Name);
                if LSFRShape <> nil then
                begin
                  AddShapeToLayer(LLayers,LLayer,LSFRShape);
                  AddTextShapeToLayer(LLayers,LLayer,LSFRShape);
                end;
              end;
            end;
          end;
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ClearAllNodesWithInflowfromLayers: boolean;
const OPNAME = 'TVNVEventHandler.ClearAllNodesWithInflowfromLayers';
var
  LInflowShape,
  LSFRShape,
  LShape : IVShape;
  LElementID : integer;
  LCount : integer;
  LYieldModelData : IYieldModelData;
  LNode : IReservoirData;
begin
  Result := False;
  try
    LYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
    if LYieldModelData <> nil then
    begin
      for LCount := 0 to FNodeList.Count-1 do
      begin
        LElementID := StrToIntDef(FNodeList[LCount],0);
        LNode := LYieldModelData.NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[LElementID];
        if(LNode = nil) then Continue;
        if(LNode.ReservoirConfigurationData.NodeType =  ntNodeWithInflow) then
        begin
          LShape := FindNodeShapeByNodeNumber(LElementID);
          if LShape <> nil then
          begin
            ClearShapeFromLayers(LShape);
            ClearTextShapesFromLayers(LShape);
            LInflowShape := FindShapeWithParent(mtWRYMInflow, LShape.Name);
            if (LInflowShape <> nil) then
            begin
              ClearShapeFromLayers(LInflowShape);
              ClearTextShapesFromLayers(LInflowShape);
            end;
            if (FBufferNodeHasSFRAList.IndexOf(LShape.Name) >= 0) then
            begin
              LSFRShape := FindShapeWithParent(mtWRYMSubCatchment, LShape.Name);
              if LSFRShape <> nil then
              begin
                ClearShapeFromLayers(LSFRShape);
                ClearTextShapesFromLayers(LSFRShape);
              end;
            end;
          end;
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TVNVEventHandler.AddAllMinesToALayer: boolean;
const OPNAME = 'TVNVEventHandler.AddAllMinesToALayer';
var
  LLayers : IVLayers;
  LLayer : IVLayer;
  LCount : integer;
  LElementID : integer;
  LMine               : IMine;
  LYieldModelData     : IYieldModelData;
  LUnderground        : integer;
  LUndergroundChannel : integer;
  LPCD                : integer;
  LPCDChannel         : integer;
  LRiverChannel       : integer;
  LRiverChannelShape  : IVShape;
  LReservoirShape     : IVShape;
  LUGChannelShape     : IVShape;
  LUndergroundShape   : IVShape;
  LChannelShape       : IVShape;
  LInflowShape,
  LSFRShape,
  LMineShape          : IVShape;
  LIndex              : integer;
begin
  Result := False;
  try
    if AddNewLayer(LLayers,LLayer) then
    begin
      LYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
      for LCount := 0 to FMineList.Count-1 do
      begin
        LElementID := StrToIntDef(FMineList[LCount],0);
        LMine := LYieldModelData.NetworkFeaturesData.MineList.MineByNodeNumber[LElementID];
        if LMine <> nil then
        begin
          for LIndex := 0 to LMine.UndergroundCount-1 do
          begin
            LUnderground := GetUnderGroundIdentifier(LMine.NodeNumber,LIndex);
            LUndergroundChannel := LMine.UnderGroundByIndex[LIndex].ChannelNumberToUGDam;
            LUGChannelShape := FindChannelShapeByChannelNumber(LUndergroundChannel);
            if LUGChannelShape <> nil then
            begin
              AddShapeToLayer(LLayers,LLayer,LUGChannelShape);
              AddTextShapeToLayer(LLayers,LLayer,LUGChannelShape);
            end;
            LUndergroundShape := FindShapeWithPropertyValue(mtWRYMReservoir, cNumber, IntToStr(LUnderground));
            if LUndergroundShape <> nil then
            begin
              AddShapeToLayer(LLayers,LLayer,LUndergroundShape);
              AddTextShapeToLayer(LLayers,LLayer,LUndergroundShape);
              if (FBufferNodeHasSFRAList.IndexOf(LUndergroundShape.Name) >= 0) then
              begin
                LSFRShape := FindShapeWithParent(mtWRYMSubCatchment, LUndergroundShape.Name);
                if LSFRShape <> nil then
                begin
                  AddShapeToLayer(LLayers,LLayer,LSFRShape);
                  AddTextShapeToLayer(LLayers,LLayer,LSFRShape);
                end;
              end;

              LInflowShape := FindShapeWithParent(mtWRYMInflow,LUndergroundShape.Name);
              if LInflowShape <> nil then
              begin
                AddShapeToLayer(LLayers,LLayer,LInflowShape);
                AddTextShapeToLayer(LLayers,LLayer,LInflowShape);
              end;
            end;
          end;
          LPCD := GetPCDIdentifier(LMine.NodeNumber);
          LPCDChannel := LMine.PCDChannelNumber;
          LReservoirShape := FindReservoirWithNumber(LPCD);

          if(LReservoirShape <> nil) then
          begin
            AddShapeToLayer(LLayers,LLayer,LReservoirShape);
            AddTextShapeToLayer(LLayers,LLayer,LReservoirShape);
            if (FBufferNodeHasSFRAList.IndexOf(LReservoirShape.Name) >= 0) then
            begin
              LSFRShape := FindShapeWithParent(mtWRYMSubCatchment, LReservoirShape.Name);
              if LSFRShape <> nil then
              begin
                AddShapeToLayer(LLayers,LLayer,LSFRShape);
                AddTextShapeToLayer(LLayers,LLayer,LSFRShape);
              end;
            end;

            LInflowShape := FindShapeWithParent(mtWRYMInflow,LReservoirShape.Name);
            if LInflowShape <> nil then
            begin
              AddShapeToLayer(LLayers,LLayer,LInflowShape);
              AddTextShapeToLayer(LLayers,LLayer,LInflowShape);
            end;
          end;

          LChannelShape := FindChannelShapeByChannelNumber(LPCDChannel);
          if(LChannelShape <> nil) then
          begin
            AddShapeToLayer(LLayers,LLayer,LChannelShape);
            AddTextShapeToLayer(LLayers,LLayer,LChannelShape);
          end;
          LRiverChannel := LMine.RiverChannel.ChannelNumber;
          LRiverChannelShape := FindShapeWithPropertyValue(mtWRYMChannel, cNumber, IntToStr(LRiverChannel));
          if LRiverChannelShape <> nil then
          begin
            AddShapeToLayer(LLayers,LLayer,LRiverChannelShape);
            AddTextShapeToLayer(LLayers,LLayer,LRiverChannelShape);
          end;
          LMineShape := FindMineWithNumber(LElementID);
          if LMineShape <> nil then
          begin
            AddShapeToLayer(LLayers,LLayer,LMineShape);
            AddTextShapeToLayer(LLayers,LLayer,LMineShape);
          end
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ClearAllMinesFromLayers: boolean;
const OPNAME = 'TVNVEventHandler.ClearAllMinesFromLayers';
var
  LCount : integer;
  LElementID : integer;
  LMine               : IMine;
  LYieldModelData     : IYieldModelData;
  LUnderground        : integer;
  LUndergroundChannel : integer;
  LPCD                : integer;
  LPCDChannel         : integer;
  LRiverChannel       : integer;
  LRiverChannelShape  : IVShape;
  LReservoirShape     : IVShape;
  LUGChannelShape     : IVShape;
  LUndergroundShape   : IVShape;
  LChannelShape       : IVShape;
  LInflowShape,
  LSFRShape,
  LMineShape          : IVShape;
  LIndex              : integer;
begin
  Result := False;
  try
    LYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
    for LCount := 0 to FMineList.Count-1 do
    begin
      LElementID := StrToIntDef(FMineList[LCount],0);
      LMine := LYieldModelData.NetworkFeaturesData.MineList.MineByNodeNumber[LElementID];
      if LMine <> nil then
      begin
        for LIndex := 0 to LMine.UndergroundCount-1 do
        begin
          LUnderground := GetUnderGroundIdentifier(LMine.NodeNumber,LIndex);
          LUndergroundChannel := LMine.UnderGroundByIndex[LIndex].ChannelNumberToUGDam;
          LUGChannelShape := FindChannelShapeByChannelNumber(LUndergroundChannel);
          if LUGChannelShape <> nil then
          begin
            ClearShapeFromLayers(LUGChannelShape);
            ClearTextShapesFromLayers(LUGChannelShape);
          end;
          LUndergroundShape := FindShapeWithPropertyValue(mtWRYMReservoir, cNumber, IntToStr(LUnderground));
          if LUndergroundShape <> nil then
          begin
            ClearShapeFromLayers(LUndergroundShape);
            ClearTextShapesFromLayers(LUndergroundShape);
            if (FBufferNodeHasSFRAList.IndexOf(LUndergroundShape.Name) >= 0) then
            begin
              LSFRShape := FindShapeWithParent(mtWRYMSubCatchment, LUndergroundShape.Name);
              if LSFRShape <> nil then
              begin
                ClearShapeFromLayers(LSFRShape);
                ClearTextShapesFromLayers(LSFRShape);
              end;
            end;

            LInflowShape := FindShapeWithParent(mtWRYMInflow,LUndergroundShape.Name);
            if LInflowShape <> nil then
            begin
              ClearShapeFromLayers(LInflowShape);
              ClearTextShapesFromLayers(LInflowShape);
            end;
          end;
        end;
        LPCD := GetPCDIdentifier(LMine.NodeNumber);
        LPCDChannel := LMine.PCDChannelNumber;
        LReservoirShape := FindReservoirWithNumber(LPCD);

        if(LReservoirShape <> nil) then
        begin
          ClearShapeFromLayers(LReservoirShape);
          ClearTextShapesFromLayers(LReservoirShape);
          if (FBufferNodeHasSFRAList.IndexOf(LReservoirShape.Name) >= 0) then
          begin
            LSFRShape := FindShapeWithParent(mtWRYMSubCatchment, LReservoirShape.Name);
            if LSFRShape <> nil then
            begin
              ClearShapeFromLayers(LSFRShape);
              ClearTextShapesFromLayers(LSFRShape);
            end;
          end;
          LInflowShape := FindShapeWithParent(mtWRYMInflow,LReservoirShape.Name);
          if LInflowShape <> nil then
          begin
            ClearShapeFromLayers(LInflowShape);
            ClearTextShapesFromLayers(LInflowShape);
          end;
        end;
        LChannelShape := FindChannelShapeByChannelNumber(LPCDChannel);
        if(LChannelShape <> nil) then
        begin
          ClearShapeFromLayers(LChannelShape);
          ClearTextShapesFromLayers(LChannelShape);
        end;
        LRiverChannel := LMine.RiverChannel.ChannelNumber;
        LRiverChannelShape := FindShapeWithPropertyValue(mtWRYMChannel, cNumber, IntToStr(LRiverChannel));
        if LRiverChannelShape <> nil then
        begin
          ClearShapeFromLayers(LRiverChannelShape);
          ClearTextShapesFromLayers(LRiverChannelShape);
        end;
        LMineShape := FindMineWithNumber(LElementID);
        if LMineShape <> nil then
        begin
          ClearShapeFromLayers(LMineShape);
          ClearTextShapesFromLayers(LMineShape);
        end
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TVNVEventHandler.AddAllDemandCentresToALayer: boolean;
const OPNAME = 'TVNVEventHandler.AddAllDemandCentresToALayer';
var
  LCount : integer;
  LElementID : integer;
  LDemandCentre: IYMDemandCentre;
  LReclaimationChannelShape,
  LConsumptiveUseChannelShape,
  LDemandCentreShape,
  LSupplyChannelShape,
  LReturnFlowFeatureShape : IVShape;
  LLayers : IVLayers;
  LLayer : IVLayer;
  LIndex : integer;
begin
  Result := False;
  try
    if AddNewLayer(LLayers,LLayer) then
    begin
      for LCount := 0 to FDemandCentreList.Count-1 do
      begin
        LElementID := StrToIntDef(FDemandCentreList[LCount],-1);
        LDemandCentre := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByNodeNumber[LElementID];
        if (LDemandCentre <> nil) then
        begin
          if (LDemandCentre.ConsumptiveUseChannel <> nil) then
          begin
            LConsumptiveUseChannelShape := FindChannelShapeByChannelNumber(LDemandCentre.ConsumptiveUseChannel.ChannelNumber);
            if LConsumptiveUseChannelShape <> nil then
            begin
              AddShapeToLayer(LLayers,LLayer,LConsumptiveUseChannelShape);
              AddTextShapeToLayer(LLayers,LLayer,LConsumptiveUseChannelShape);
            end;
          end;
          if LDemandCentre.ReclaimationChannel <> nil then
          begin
            LReclaimationChannelShape := FindChannelShapeByChannelNumber(LDemandCentre.ReclaimationChannelNr);
            if LReclaimationChannelShape <> nil then
            begin
              AddShapeToLayer(LLayers,LLayer,LReclaimationChannelShape);
              AddTextShapeToLayer(LLayers,LLayer,LReclaimationChannelShape);
            end;
          end;
          for LIndex := 0 to LDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureCount - 1 do
          begin
            LReturnFlowFeatureShape := FindChannelShapeByChannelNumber(LDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureByIndex[LIndex].ChannelNr);
            if LReturnFlowFeatureShape <> nil then
            begin
              AddShapeToLayer(LLayers,LLayer,LReturnFlowFeatureShape);
              AddTextShapeToLayer(LLayers,LLayer,LReturnFlowFeatureShape);
            end;
          end;
          for LIndex := 0 to LDemandCentre.SupplyChannelCount - 1 do
          begin
            LSupplyChannelShape := FindChannelShapeByChannelNumber(LDemandCentre.SupplyChannelByIndex[LIndex].ChannelNumber);
            if LSupplyChannelShape <> nil then
            begin
              AddShapeToLayer(LLayers,LLayer,LSupplyChannelShape);
              AddTextShapeToLayer(LLayers,LLayer,LSupplyChannelShape);
            end
          end;
          LDemandCentreShape := FindDemandCentreByNode(LElementID);
          if LDemandCentreShape <> nil then
          begin
            AddShapeToLayer(LLayers,LLayer,LDemandCentreShape);
            AddTextShapeToLayer(LLayers,LLayer,LDemandCentreShape);
          end;
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TVNVEventHandler.ClearAllDemandCentresFromLayers: boolean;
const OPNAME = 'TVNVEventHandler.ClearAllDemandCentresFromLayers';
var
  LCount : integer;
  LElementID : integer;
  LDemandCentre: IYMDemandCentre;
  LReclaimationChannelShape,
  LConsumptiveUseChannelShape,
  LDemandCentreShape,
  LSupplyChannelShape,
  LReturnFlowFeatureShape : IVShape;
  LIndex : integer;
begin
  Result := False;
  try
    for LCount := 0 to FDemandCentreList.Count-1 do
    begin
      LElementID := StrToIntDef(FDemandCentreList[LCount],-1);
      LDemandCentre := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByNodeNumber[LElementID];
      if (LDemandCentre <> nil) then
      begin
        if (LDemandCentre.ConsumptiveUseChannel <> nil) then
        begin
          LConsumptiveUseChannelShape := FindChannelShapeByChannelNumber(LDemandCentre.ConsumptiveUseChannel.ChannelNumber);
          if LConsumptiveUseChannelShape <> nil then
          begin
            ClearShapeFromLayers(LConsumptiveUseChannelShape);
            ClearTextShapesFromLayers(LConsumptiveUseChannelShape);
          end;
        end;
        if LDemandCentre.ReclaimationChannel <> nil then
        begin
          LReclaimationChannelShape := FindChannelShapeByChannelNumber(LDemandCentre.ReclaimationChannelNr);
          if LReclaimationChannelShape <> nil then
          begin
            ClearShapeFromLayers(LReclaimationChannelShape);
            ClearTextShapesFromLayers(LReclaimationChannelShape);
          end;
        end;
        for LIndex := 0 to LDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureCount - 1 do
        begin
          LReturnFlowFeatureShape := FindChannelShapeByChannelNumber(LDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureByIndex[LIndex].ChannelNr);
          if LReturnFlowFeatureShape <> nil then
          begin
            ClearShapeFromLayers(LReturnFlowFeatureShape);
            ClearTextShapesFromLayers(LReturnFlowFeatureShape);
          end;
        end;
        for LIndex := 0 to LDemandCentre.SupplyChannelCount - 1 do
        begin
          LSupplyChannelShape := FindChannelShapeByChannelNumber(LDemandCentre.SupplyChannelByIndex[LIndex].ChannelNumber);
          if LSupplyChannelShape <> nil then
          begin
            ClearShapeFromLayers(LSupplyChannelShape);
            ClearTextShapesFromLayers(LSupplyChannelShape);
          end
        end;
        LDemandCentreShape := FindDemandCentreByNode(LElementID);
        if LDemandCentreShape <> nil then
        begin
          ClearShapeFromLayers(LDemandCentreShape);
          ClearTextShapesFromLayers(LDemandCentreShape);
        end;
      end;
    end;
   Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TVNVEventHandler.AddAllIrrigationAreasToALayer: boolean;
const OPNAME = 'TVNVEventHandler.AddAllIrrigationAreasToALayer';
var
  LLayers : IVLayers;
  LLayer : IVLayer;
  LCount : integer;
  LElementID : integer;
  LIrrigationArea : IIrrigationArea;
  LDiversionChannelShape : IVShape;
  LReturnFlowChannelShape : IVShape;
  LConsumptiveChannelShape : IVShape;
  LIrrigationAreaShape : IVShape;
begin
  Result := False;
  try
    if AddNewLayer(LLayers,LLayer) then
    begin
      for LCount := 0 to FIrrigationAreaList.Count-1 do
      begin
        LElementID := StrToIntDef(FIrrigationAreaList[LCount],-1);
        LIrrigationArea := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.IrrigationAreaList.IrrigationAreaByNodeNumber[lElementID];
        if LIrrigationArea <> nil then
        begin
          if(LIrrigationArea.DiversionChannel <> nil) then
          begin
            LDiversionChannelShape := FindChannelShapeByChannelNumber(LIrrigationArea.DiversionChannel.ChannelNumber);
            if (LDiversionChannelShape <> nil) then
            begin
              AddShapeToLayer(LLayers,LLayer,LDiversionChannelShape);
              AddTextShapeToLayer(LLayers,LLayer,LDiversionChannelShape);
            end;
          end;
          if(LIrrigationArea.ReturnFlowChannel <> nil) then
          begin
            LReturnFlowChannelShape := FindChannelShapeByChannelNumber(LIrrigationArea.ReturnFlowChannel.ChannelNumber);
            if (LReturnFlowChannelShape <> nil) then
            begin
              AddShapeToLayer(LLayers,LLayer,LReturnFlowChannelShape);
              AddTextShapeToLayer(LLayers,LLayer,LReturnFlowChannelShape);
            end;
          end;
          if(LIrrigationArea.ConsumptiveChannel <> nil) then
          begin
            LConsumptiveChannelShape := FindChannelShapeByChannelNumber(LIrrigationArea.ConsumptiveChannel.ChannelNumber);
            if (LConsumptiveChannelShape <> nil) then
            begin
              AddShapeToLayer(LLayers,LLayer,LConsumptiveChannelShape);
              AddTextShapeToLayer(LLayers,LLayer,LConsumptiveChannelShape);
            end;
          end;
          LIrrigationAreaShape := FindShapeWithPropertyValue(mtWRYMIrrArea, cNumber, IntToStr(LElementID));
          if (LIrrigationAreaShape <> nil) then
          begin
            AddShapeToLayer(LLayers,LLayer,LIrrigationAreaShape);
            AddTextShapeToLayer(LLayers,LLayer,LIrrigationAreaShape);
          end;
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ClearAllIrrigationAreasFromLayers : boolean;
const OPNAME = 'TVNVEventHandler.ClearAllIrrigationAreasFromLayers';
var
  LCount : integer;
  LElementID : integer;
  LIrrigationArea : IIrrigationArea;
  LDiversionChannelShape : IVShape;
  LReturnFlowChannelShape : IVShape;
  LConsumptiveChannelShape : IVShape;
  LIrrigationAreaShape : IVShape;
begin
  Result := False;
  try
    for LCount := 0 to FIrrigationAreaList.Count-1 do
    begin
      LElementID := StrToIntDef(FIrrigationAreaList[LCount],-1);
      LIrrigationArea := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.IrrigationAreaList.IrrigationAreaByNodeNumber[lElementID];
      if LIrrigationArea <> nil then
      begin
        if(LIrrigationArea.DiversionChannel <> nil) then
        begin
          LDiversionChannelShape := FindChannelShapeByChannelNumber(LIrrigationArea.DiversionChannel.ChannelNumber);
          if (LDiversionChannelShape <> nil) then
          begin
            ClearShapeFromLayers(LDiversionChannelShape);
            ClearTextShapesFromLayers(LDiversionChannelShape);
          end;
        end;
        if(LIrrigationArea.ReturnFlowChannel <> nil) then
        begin
          LReturnFlowChannelShape := FindChannelShapeByChannelNumber(LIrrigationArea.ReturnFlowChannel.ChannelNumber);
          if (LReturnFlowChannelShape <> nil) then
          begin
            ClearShapeFromLayers(LReturnFlowChannelShape);
            ClearTextShapesFromLayers(LReturnFlowChannelShape);
          end;
        end;
        if(LIrrigationArea.ConsumptiveChannel <> nil) then
        begin
          LConsumptiveChannelShape := FindChannelShapeByChannelNumber(LIrrigationArea.ConsumptiveChannel.ChannelNumber);
          if (LConsumptiveChannelShape <> nil) then
          begin
            ClearShapeFromLayers(LConsumptiveChannelShape);
            ClearTextShapesFromLayers(LConsumptiveChannelShape);
          end;
        end;
        LIrrigationAreaShape := FindShapeWithPropertyValue(mtWRYMIrrArea, cNumber, IntToStr(LElementID));
        if (LIrrigationAreaShape <> nil) then
        begin
          ClearShapeFromLayers(LIrrigationAreaShape);
          ClearTextShapesFromLayers(LIrrigationAreaShape);
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TVNVEventHandler.AddAllNodesToDrawing : boolean;
const OPNAME = 'TVNVEventHandler.AddAllNodesToDrawing';
var
  LIndex          : Integer;
  LContainer      : TStringList;
  LYieldModelData : IYieldModelData;
  LMaster         : IVMaster;
  LNodeShape      : IVShape;
  LNodeNumber     : string;
  LNode           : IReservoirData;
  LXPos           : Double;
  LYPos           : Double;
  LItemHeight     : Double;
  LItemWidth      : Double;
  LPageHeight     : Double;
  LPageWidth      : Double;
  LMessageShown   : boolean; 
begin
  Result := False;
  try
    LContainer := TStringList.Create;
    try
      LContainer.Sorted := True;
      LContainer.Duplicates := dupIgnore;

      lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
      for LIndex := 0 to lYieldModelData.NetworkElementData.ReservoirList.ReservoirAndNodesCount-1 do
      begin
        LNodeNumber := IntToStr(lYieldModelData.NetworkElementData.ReservoirList.ReservoirOrNodeByIndex[LIndex].ReservoirConfigurationData.ReservoirIdentifier);
        if(FReservoirList.IndexOf(LNodeNumber) < 0) and (FNodeList.IndexOf(LNodeNumber) < 0) and (LNodeNumber <> '0')then
          LContainer.Add(LNodeNumber);
      end;

      LMessageShown   := False;

      LItemHeight     := 1.2; //inches
      LItemWidth      := 1.5; //inches
      LPageHeight     := FVisioApp.ActivePage.PageSheet.Cells['PageHeight'].Result[visInches];;
      LPageWidth      := FVisioApp.ActivePage.PageSheet.Cells['PageWidth'].Result[visInches];;

      LXPos := LItemWidth/2.0;
      LYPos := LPageHeight -LItemHeight;
      for LIndex := 0 to LContainer.Count-1 do
      begin
        LNodeNumber := LContainer[LIndex];
        LNode := lYieldModelData.NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[StrToInt(LNodeNumber)];
        if(LNode = nil) then Continue;

        if(LNode.ReservoirConfigurationData.NodeType =  ntReservoir) then
        begin
          lMaster := FindMaster(mtWRYMReservoir);
          if (lMaster <> nil) then
          begin
            FBufferReservoirList.Add(LNode.ReservoirConfigurationData.ReservoirName);
            LNodeShape  := FVisioApp.ActivePage.Drop(lMaster, LXPos, LYPos);
            ReservoirShapeAdded(LNodeShape,StrToInt(LNodeNumber));
          end;
        end
        else
        if(LNode.ReservoirConfigurationData.NodeType =  ntNodeWithInflow) then
        begin
          lMaster := FindMaster(mtWRYMNode);
          if (lMaster <> nil) then
          begin
            FBufferNodeList.Add(LNode.ReservoirConfigurationData.ReservoirName);
            LNodeShape  := FVisioApp.ActivePage.Drop(lMaster, LXPos, LYPos+0.1);
            NodeShapeAdded(LNodeShape,StrToInt(LNodeNumber));
          end;
        end
        else
        if(LNode.ReservoirConfigurationData.NodeType =  ntNodeWithoutInflow) then
        begin
          lMaster := FindMaster(mtWRYMNode);
          if (lMaster <> nil) then
          begin
            FBufferNodeList.Add(LNode.ReservoirConfigurationData.ReservoirName);
            LNodeShape  := FVisioApp.ActivePage.Drop(lMaster, LXPos, LYPos+0.1);
            NodeShapeAdded(LNodeShape,StrToInt(LNodeNumber));
          end;
        end;

        LYPos := LYPos -(LItemHeight);
        if(LYPos <= (LItemHeight/2.0)) then
        begin
          LXPos := LXPos + (LItemWidth/2.0);
          LYPos := LPageHeight -(LItemHeight);
        end;
        if(LXPos > LPageWidth) then
        begin
          if not LMessageShown then
          begin
            ShowMessage('Some items are added beyond the page width. Encrease the page size to see all items.');
            LMessageShown := True;
          end;
        end;
      end;
      Result := True;
    finally
      LContainer.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.MaxLossFactor( AChannel: IGeneralFlowChannel): string;
const OPNAME = 'TVNVEventHandler.MaxLossFactor';
var
  LIndex    : Integer;
  LMaxValue : double;
begin
  Result := '';
  try
    if(AChannel <> nil) and (AChannel.LossFeature <> nil) then
    begin
      LMaxValue := AChannel.LossFeature.WaterLossByMonth[1];
      for LIndex := 2 to 12 do
      begin
        if(LMaxValue < AChannel.LossFeature.WaterLossByMonth[LIndex]) then
          LMaxValue := AChannel.LossFeature.WaterLossByMonth[LIndex];
      end;
      Result := Format(FAppModules.FieldProperties.FieldProperty('WaterLoss').FormatStringGrid, [LMaxValue]);
      Result := 'Loss ' + Result;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.MaxDiversionFlow(AChannel: IGeneralFlowChannel): string;
const OPNAME = 'TVNVEventHandler.MaxDiversionFlow';
var
  LIndex    : Integer;
  LMaxValue : double;
begin
  Result := '';
  try
    if(AChannel <> nil) and (AChannel.DiversionFeature <> nil) and (AChannel.DiversionFeature.DiversionType in [2,4]) then
    begin
      LMaxValue := AChannel.DiversionFeature.DivertedFlowByIndex[1];
      for LIndex := 2 to 12 do
      begin
        if(LMaxValue < AChannel.DiversionFeature.DivertedFlowByIndex[LIndex]) then
          LMaxValue := AChannel.DiversionFeature.DivertedFlowByIndex[LIndex];
      end;
      Result := Format(FAppModules.FieldProperties.FieldProperty('ActualDivertedFlow').FormatStringGrid, [LMaxValue]);
      Result := 'Div. ' + Result;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.GetFirstSelectedShape(AShapeName: string): IVShape;
const OPNAME = 'TVNVEventHandler.GetFirstSelectedShape';
var
  LIndex    : Integer;
  lSelection  : IVSelection;
  lShape  : IVShape;
begin
  Result := nil;
  try
    lSelection := FVisioApp.ActiveWindow.Selection;
    for lIndex := 1  to lSelection.Count do
    begin
      lShape := lSelection.Item[lIndex];
      if (Pos(PChar(AShapeName), lShape.Name) > 0) then
      begin
        Result := lShape;
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.GetOutputTextShapePerParent(AContainer : TInterfaceList; AParentShape: IVShape): IVShape;
const OPNAME = 'TVNVEventHandler.GetOutputTextShapePerParent';
var
  LIndex : Integer;
  LShape : IVShape;
begin
  Result := nil;
  try
    if (AParentShape.CellExists[cNumber, 0] = 0) then  Exit;
    for LIndex := AContainer.Count-1 downto 0 do
    begin
      LShape := IVShape(AContainer.Items[LIndex]);
      if(LShape.Cells[cNumber].Formula = AParentShape.Cells[cNumber].Formula) then
      begin
        Result := LShape;
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.GetOutputTextShape( AContainer: TInterfaceList;ATextType:TVNVTextType);
const OPNAME = 'TVNVEventHandler.GetOutputTextShape';
var
  lPageIdx        : integer;
  lShapeIdx       : integer;
  lShape          : IVShape;
  lPage           : IVPage;
begin
  try
    AContainer.Clear;
    for lPageIdx := 1 to FVisioApp.ActiveDocument.Pages.Count do
    begin
      lPage := FVisioApp.ActiveDocument.Pages[lPageIdx];
      for lShapeIdx := 1 to lPage.Shapes.Count do
      begin
        lShape := lPage.Shapes[lShapeIdx];
        if (lShape.CellExists[cTextType, 0] <> 0) then
        begin
          if(lShape.Cells[cTextType].Formula = '"' + IntToStr(Ord(ATextType)) + '"') then
           AContainer.Add(lShape);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.CreatePolutionControlDam(AMineShape: IVShape; AMineNodeNumber, AReservoirNumber, AChannelNumber: integer);
const OPNAME = 'TVNVEventHandler.CreatePolutionControlDam';
var
  lYieldModelData : IYieldModelData;
  lMine           : IMine;
  LPCDShape       : IVShape;
  lMaster         : IVMaster;
  lXVal           : double;
  lYVal           : double;
begin
  try
    lYieldModelData   := FAppModules.Model.ModelData as IYieldModelData;
    lMine := lYieldModelData.NetworkFeaturesData.MineList.MineByNodeNumber[AMineNodeNumber];
    if(lMine <> nil) and lMine.PolutionControlDamExists then
    begin
      lMaster := FindMaster(mtWRYMReservoir);
      if(lMaster <> nil) then
      begin
        AMineShape.XYToPage(1, 0, lXVal, lYVal);
        FBufferReservoirList.Add(lMine.PolutionControlDam.ReservoirConfigurationData.ReservoirName);
        LPCDShape := FVisioApp.ActivePage.Drop(lMaster, LXVal, LYVal);
        ReservoirShapeAdded(LPCDShape,lMine.PolutionControlDam.ReservoirConfigurationData.ReservoirIdentifier,1,lMine.NodeNumber);
        AddMineChannel(LPCDShape,lMine.PCDChannel,1,1);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.DeletePolutionControlDam(AMineShape: IVShape; AMineNodeNumber, AReservoirNumber, AChannelNumber: integer);
const OPNAME = 'TVNVEventHandler.DeletePolutionControlDam';
var
  LReservoir          : IVShape;
  LChannel            : IVShape;
begin
  try
    LReservoir := FindReservoirWithNumber(AReservoirNumber);
    if(LReservoir <> nil) then
    begin
      LReservoir.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
      DeleteReservoir(AReservoirNumber,1);
    end;

    LChannel := FindChannelShapeByChannelNumber(AChannelNumber);
    if(LChannel <> nil) then
    begin
      LChannel.Cells[cDeletePrevention].Formula := '"' + '0' + '"';
      DeleteChannel(AChannelNumber,1);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ViewMineProperties(AShapeName: string; AMineNodeNumber: integer): WordBool;
const OPNAME = 'TVNVEventHandler.ViewMineProperties';
var
  lYieldModel       : IYieldModel;
  lYieldModelData   : IYieldModelData;
  lMine             : IMine;
  lShape            : IVShape;
  lPCDExist         : boolean;
  lPCDNodeNumber    : integer;
  lPCDChannelNumber : integer;

begin
  Result := False;
  try
    lPCDExist         := False;
    lPCDNodeNumber    := 0;
    lPCDChannelNumber := 0;
    lYieldModel := (FAppModules.Model as IYieldModel);
    lYieldModelData   := FAppModules.Model.ModelData as IYieldModelData;
    lMine := lYieldModelData.NetworkFeaturesData.MineList.MineByNodeNumber[AMineNodeNumber];
    if(lMine <> nil) then
    begin
      lPCDExist := lMine.PolutionControlDamExists;
      if lPCDExist then
      begin
        lPCDNodeNumber    := lMine.PolutionControlDam.ReservoirConfigurationData.ReservoirIdentifier;
        lPCDChannelNumber := lMine.PCDChannelNumber;
      end;
    end;

    Result := lYieldModel.ViewInputMineDialog(AMineNodeNumber);

    lMine := lYieldModelData.NetworkFeaturesData.MineList.MineByNodeNumber[AMineNodeNumber];
    if(lMine <> nil) then
    begin
      if lPCDExist and (not lMine.PolutionControlDamExists) then
      begin
        DeletePolutionControlDam(nil,AMineNodeNumber,lPCDNodeNumber,lPCDChannelNumber);
      end
      else if (not lPCDExist) and lMine.PolutionControlDamExists then
      begin
        lPCDNodeNumber    := lMine.PolutionControlDam.ReservoirConfigurationData.ReservoirIdentifier;
        lPCDChannelNumber := lMine.PCDChannelNumber;
        lShape := GetFirstSelectedShape(mtWRYMMine);
        if(lShape <> nil) then
          CreatePolutionControlDam(lShape,AMineNodeNumber,lPCDNodeNumber,lPCDChannelNumber);
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.RefreshAllReservoirs;
const OPNAME = 'TVNVEventHandler.UpgradeDocument';
var
  lYieldModel       : IYieldModel;
  lYieldModelData   : IYieldModelData;
  lShape            : IVShape;
  LIndex,
  LReservoirNumber  : integer;
  lReservoir      : IReservoirData;
begin
  try
    lYieldModel := (FAppModules.Model as IYieldModel);
    lYieldModelData   := FAppModules.Model.ModelData as IYieldModelData;


    for LIndex := 0 to lYieldModelData.NetworkElementData.ReservoirList.ReservoirCount-1 do
    begin
      lReservoir := lYieldModelData.NetworkElementData.ReservoirList.ReservoirByIndex[LIndex];
      if(lReservoir <> nil) then
      begin
        LReservoirNumber := lReservoir.ReservoirConfigurationData.ReservoirIdentifier;
        lShape := FindReservoirWithNumber(LReservoirNumber);
        if(lShape <> nil) then
          ReservoirHasChanged(lShape.Name,LReservoirNumber);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
//__________________________________________________________________________________________________________________
function TVNVEventHandler.AddAllChannelOutputTextLabels(ATextType:TVNVTextType): boolean;
const OPNAME = 'TVNVEventHandler.AddAllChannelOutputTextLabels';
var
  LIndex          : Integer;
  lYieldModelData : IYieldModelData;
  LChannel        : IGeneralFlowChannel;

  LChannelShape   : IVShape;
  LPreviousShape  : IVShape;
  LCurrentShape   : IVShape;

  lXPin        : double;
  lYPin        : double;
  LContainer   : TInterfaceList;
begin
  Result := False;
  try
    LContainer   := TInterfaceList.Create;
    try
      GetOutputTextShape(LContainer,ATextType);
      lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
      for LIndex := 0 to lYieldModelData.NetworkElementData.ChannelList.ChannelCount-1 do
      begin
        LChannel := lYieldModelData.NetworkElementData.ChannelList.ChannelByIndex[LIndex];
        if (LChannel.MasterControlFeature = nil) and (UpperCase(LChannel.SummaryOutputRequired) <> 'Y')  then
          Continue;

        LChannelShape  := FindChannelShapeByChannelNumber(LChannel.ChannelNumber);
        if(LChannelShape <> nil) then
        begin
          LPreviousShape := GetOutputTextShapePerParent(LContainer,LChannelShape);
          LCurrentShape := AddTextLabel(LChannelShape,ATextType);
          if(LCurrentShape <> nil) then
          begin
            FOutputList.Add(LCurrentShape.Name);

            LCurrentShape.Cells[cParent].Formula := '"' + LChannelShape.Name + '"';
            LCurrentShape.Cells[cTextType].Formula := '"' + IntToStr(Ord(ATextType)) + '"';
            LCurrentShape.Cells[cTextLbl].Formula  := '"' + TextTypeToStr(ATextType) + '"';
            LCurrentShape.Cells[cTextInv].Formula  := '"TRUE"';
            LCurrentShape.Cells[cNumber].Formula := '"' + IntToStr(LChannel.ChannelNumber) + '"';
            RefreshText(LCurrentShape);
            LCurrentShape.Cells[cPermanent].Formula := '"TRUE"';

            if(LPreviousShape <> nil) then
            begin
              LCurrentShape.CellsSRC[visSectionCharacter, visRowCharacter, visCharacterColor].FormulaU := IntToStr(visRed);
              //Place it above the old one
              lXPin := StrToFloatDef(UnMM(UnQuote(LPreviousShape.Cells[cPinX].ResultStr[visMillimeters])),0);
              lYPin := StrToFloatDef(UnMM(UnQuote(LPreviousShape.Cells[cPinY].ResultStr[visMillimeters])),0);
              lYPin := lYPin + 3;
              LCurrentShape.Cells[cPinX].Formula    := '"' + FloatToStrF(lXPin, ffFixed, 12, 2) + 'mm"';
              LCurrentShape.Cells[cPinY].Formula    := '"' + FloatToStrF(lYPin, ffFixed, 12, 2) + 'mm"';
            end
            else
            begin
              //Place wher the channel number would have been
              lXPin := StrToFloatDef(UnMM(UnQuote(LChannelShape.Cells[cPinX].ResultStr[visMillimeters])),0);
              lYPin := StrToFloatDef(UnMM(UnQuote(LChannelShape.Cells[cPinY].ResultStr[visMillimeters])),0);
              lYPin := lYPin + 3;
              LCurrentShape.Cells[cPinX].Formula    := '"' + FloatToStrF(lXPin, ffFixed, 12, 2) + 'mm"';
              LCurrentShape.Cells[cPinY].Formula    := '"' + FloatToStrF(lYPin, ffFixed, 12, 2) + 'mm"';
            end;
          end;
        end;
      end;
      Result := True;
    finally
      LContainer.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.AddAllReservoirOutputTextLabels(ATextType:TVNVTextType) : boolean;
const OPNAME = 'TVNVEventHandler.AddAllReservoirOutputTextLabels';
var
  LIndex          : Integer;
  lYieldModelData : IYieldModelData;
  LReservoirData  : IReservoirData;

  LReservoirShape   : IVShape;
  LPreviousShape  : IVShape;
  LCurrentShape   : IVShape;

  lXPin        : double;
  lYPin        : double;
  LContainer   : TInterfaceList;
begin
  Result := False;
  try
    LContainer   := TInterfaceList.Create;
    try
      GetOutputTextShape(LContainer,ATextType);
      lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
      for LIndex := 0 to lYieldModelData.NetworkElementData.ReservoirList.ReservoirAndNodesCount-1 do
      begin
        LReservoirData := lYieldModelData.NetworkElementData.ReservoirList.ReservoirOrNodeByIndex[LIndex];
        if(LReservoirData.ReservoirConfigurationData.CatchmentRef = 0)  then
          Continue;

        LReservoirShape  := FindReservoirWithNumber(LReservoirData.ReservoirConfigurationData.ReservoirIdentifier);
        if(LReservoirShape = nil) then
          LReservoirShape  := FindNodeShapeByNodeNumber(LReservoirData.ReservoirConfigurationData.ReservoirIdentifier);
        if(LReservoirShape <> nil) then
        begin
          LPreviousShape := GetOutputTextShapePerParent(LContainer,LReservoirShape);
          LCurrentShape := AddTextLabel(LReservoirShape,ATextType);
          if(LCurrentShape <> nil) then
          begin
            FOutputList.Add(LCurrentShape.Name);

            LCurrentShape.Cells[cParent].Formula := '"' + LReservoirShape.Name + '"';
            LCurrentShape.Cells[cTextType].Formula := '"' + IntToStr(Ord(ATextType)) + '"';
            LCurrentShape.Cells[cTextLbl].Formula  := '"' + TextTypeToStr(ATextType) + '"';
            LCurrentShape.Cells[cTextInv].Formula  := '"TRUE"';
            LCurrentShape.Cells[cNumber].Formula := '"' + IntToStr(LReservoirData.ReservoirConfigurationData.ReservoirIdentifier) + '"';
            RefreshText(LCurrentShape);
            LCurrentShape.Cells[cPermanent].Formula := '"TRUE"';

            if(LPreviousShape <> nil) then
            begin
              LCurrentShape.CellsSRC[visSectionCharacter, visRowCharacter, visCharacterColor].FormulaU := IntToStr(visRed);
              //Place it above the old one
              lXPin := StrToFloatDef(UnMM(UnQuote(LPreviousShape.Cells[cPinX].ResultStr[visMillimeters])),0);
              lYPin := StrToFloatDef(UnMM(UnQuote(LPreviousShape.Cells[cPinY].ResultStr[visMillimeters])),0);
              lYPin := lYPin + 3;
              LCurrentShape.Cells[cPinX].Formula    := '"' + FloatToStrF(lXPin, ffFixed, 12, 2) + 'mm"';
              LCurrentShape.Cells[cPinY].Formula    := '"' + FloatToStrF(lYPin, ffFixed, 12, 2) + 'mm"';
            end
            else
            begin
              //Place where the channel number would have been
              lXPin := StrToFloatDef(UnMM(UnQuote(LReservoirShape.Cells[cPinX].ResultStr[visMillimeters])),0);
              lYPin := StrToFloatDef(UnMM(UnQuote(LReservoirShape.Cells[cPinY].ResultStr[visMillimeters])),0);
              lYPin := lYPin + 3;
              LCurrentShape.Cells[cPinX].Formula    := '"' + FloatToStrF(lXPin, ffFixed, 12, 2) + 'mm"';
              LCurrentShape.Cells[cPinY].Formula    := '"' + FloatToStrF(lYPin, ffFixed, 12, 2) + 'mm"';
            end;
          end;
        end;
      end;
      Result := True;
    finally
      LContainer.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.DeleteAllOutputTextLabels(ATextType:TVNVTextType) : boolean;
const OPNAME = 'TVNVEventHandler.DeleteAllOutputTextLabels';
var
  LIndex     : Integer;
  LContainer : TInterfaceList;
begin
  Result := False;
  try
    LContainer   := TInterfaceList.Create;
    try
      GetOutputTextShape(LContainer,ATextType);
      for LIndex := 0 to LContainer.Count-1 do
        IVShape(LContainer[LIndex]).Delete;
    finally
      FreeAndNil(LContainer);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TVNVEventHandler.DeleteAllChannelOutputTextLabels(ATextType:TVNVTextType) : boolean;
const OPNAME = 'TVNVEventHandler.DeleteAllChannelOutputTextLabels';
begin
  Result := False;
  try
    Result := DeleteAllOutputTextLabels(ATextType);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.DeleteAllReservoirOutputTextLabels(ATextType:TVNVTextType) : boolean;
const OPNAME = 'TVNVEventHandler.DeleteAllReservoirOutputTextLabels';
begin
  Result := False;
  try
    Result := DeleteAllOutputTextLabels(ATextType);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TVNVEventHandler.SetAllOutputLabelsPermanent: boolean;
const OPNAME = 'TVNVEventHandler.GetOutputTextShape';
var
  lPageIdx        : integer;
  lShapeIdx       : integer;
  lShape          : IVShape;
  lPage           : IVPage;
  lTextType       : TVNVTextType;
begin
  Result := False;
  try
    for lPageIdx := 1 to FVisioApp.ActiveDocument.Pages.Count do
    begin
      lPage := FVisioApp.ActiveDocument.Pages[lPageIdx];
      for lShapeIdx := 1 to lPage.Shapes.Count do
      begin
        lShape := lPage.Shapes[lShapeIdx];
        if (lShape.CellExists[cTextType, 0] <> 0) then
        begin
          lTextType  := TVNVTextType(StrToIntDef(UnQuote(lShape.Cells[cTextType].Formula),0));
          if(lTextType in OutputTextTypeSet) and (lShape.CellExists[cPermanent, 0] <> 0) then
            lShape.Cells[cPermanent].Formula         := '"TRUE"';
        end;
      end;
    end;
    Result := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.AddAllChannelPenaltyToDrawing: boolean;
const OPNAME = 'TVNVEventHandler.AddAllReservoirPenaltyToDrawing';
var
  LChannelShape : IVShape;
  LPenaltyShape   : IVShape;
  LListIndex      : integer;
  LElementID      : integer;
  lMaster         : IVMaster;
  lXParent        : Double;
  lYParent        : Double;
  lYPin           : double;
  lXPin           : double;

  lXBegin         : double;
  lXEnd           : double;
  lYBegin         : double;
  lYEnd           : double;
begin
  Result := False;
  try
    for LListIndex := 0 to FChannelList.Count-1 do
    begin
      LElementID := StrToIntDef(FChannelList[LListIndex],0);
      LChannelShape := FindChannelShapeByChannelNumber(LElementID);
      if(LChannelShape <> nil) then
      begin
        LPenaltyShape := FindChannelPenaltyWithNumber(LElementID);
        if(LPenaltyShape = nil) then
        begin
          lMaster := FindMaster(mtWRYMChanPenalty);
          if (lMaster <> nil) then
          begin
            LChannelShape.XYToPage(0, 0, lXParent, lYParent);
            lXBegin := StrToFloatDef(UnMM(UnQuote(LChannelShape.Cells[cBeginX].ResultStr[visMillimeters])),0);
            lXEnd   := StrToFloatDef(UnMM(UnQuote(LChannelShape.Cells[cEndX].ResultStr[visMillimeters])),0);
            lYBegin := StrToFloatDef(UnMM(UnQuote(LChannelShape.Cells[cBeginY].ResultStr[visMillimeters])),0);
            lYEnd   := StrToFloatDef(UnMM(UnQuote(LChannelShape.Cells[cEndY].ResultStr[visMillimeters])),0);
            lXPin   := lXBegin + ((lXEnd - lXBegin)/2.0);
            lYPin   := lYBegin + ((lYEnd - lYBegin)/2.0);
            LPenaltyShape := FVisioApp.ActivePage.Drop(lMaster, lXParent, lYParent);
            LPenaltyShape.Cells[cNumber].Formula  := '"' + IntToStr(lElementID) + '"';
            LPenaltyShape.Cells[cPinX].Formula    := '"' + FloatToStrF(lXPin, ffFixed, 12, 2) + 'mm"';
            LPenaltyShape.Cells[cPinY].Formula    := '"' + FloatToStrF(lYPin, ffFixed, 12, 2) + 'mm"';
          end;
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.AddAllReservoirPenaltyToDrawing: boolean;
const OPNAME = 'TVNVEventHandler.AddAllReservoirPenaltyToDrawing';
var
  LReservoirShape : IVShape;
  LPenaltyShape   : IVShape;
  LListIndex      : integer;
  LElementID      : integer;
  lMaster         : IVMaster;
  lXParent        : Double;
  lYParent        : Double;
  lYShift         : double;
  lXShift         : double;
begin
  Result := False;
  try
    for LListIndex := 0 to FReservoirList.Count-1 do
    begin
      LElementID := StrToIntDef(FReservoirList[LListIndex],0);
      LReservoirShape := FindReservoirWithNumber(LElementID);
      if(LReservoirShape <> nil) then
      begin
        LPenaltyShape := FindReservoirPenaltyWithNumber(LElementID);
        if(LPenaltyShape = nil) then
        begin
          lMaster := FindMaster(mtWRYMResPenalty);
          if (lMaster <> nil) then
          begin
            LReservoirShape.XYToPage(0, 0, lXParent, lYParent);
            lXShift := -0.5;
            lYShift := 1.0;
            LPenaltyShape := FVisioApp.ActivePage.Drop(lMaster, lXParent + lXShift, lYParent + lYShift);
            LPenaltyShape.Cells[cNumber].Formula  := '"' + IntToStr(lElementID) + '"';
          end;
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.DeleteAllChannelPenaltyFromDrawing: boolean;
const OPNAME = 'TVNVEventHandler.AddAllReservoirPenaltyToDrawing';
var
  LPenaltyShape   : IVShape;
  LListIndex      : integer;
  LElementID      : integer;
begin
  Result := False;
  try
    for LListIndex := 0 to FChannelList.Count-1 do
    begin
      LElementID := StrToIntDef(FChannelList[LListIndex],0);
      LPenaltyShape := FindChannelPenaltyWithNumber(LElementID);
      if(LPenaltyShape <> nil) then
        LPenaltyShape.Delete;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.DeleteAllReservoirPenaltyFromDrawing: boolean;
const OPNAME = 'TVNVEventHandler.AddAllReservoirPenaltyToDrawing';
var
  LPenaltyShape : IVShape;
  LListIndex      : integer;
  LElementID      : integer;
begin
  Result := False;
  try
    for LListIndex := 0 to FReservoirList.Count-1 do
    begin
      LElementID := StrToIntDef(FReservoirList[LListIndex],0);
      LPenaltyShape := FindReservoirPenaltyWithNumber(LElementID);
      if(LPenaltyShape <> nil) then
        LPenaltyShape.Delete;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.PowerPlantShapeAdded(const AShape: IVShape);
const OPNAME = 'TVNVEventHandler.PowerPlantShapeAdded';
var
  lPowerPlantID  : integer;
  lTxtLabel   : IVShape;
  lYieldModelData : IYieldModelData;
  lPowerPlant    : IPowerPlant;
begin
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then
    begin
      MessageDlg(FAppModules.Language.GetString('VNV.PowerPlantNoModelSupport'), mtError, [mbOK], 0);
      Exit;
    end;

    if (FBufferPowerPlantList.Count > 0) then
//    if (FPowerPlantList.Count > 0) then
      { Upgraded Power Plant }
//      FPowerPlantList.Delete(0)
      FBufferPowerPlantList.Delete(0)
    else
    begin
      if (AShape.CellExists[cNumber, 0] <> 0) AND (UnQuote(AShape.Cells[cNumber].Formula) <> '') then
      begin
        { Copied Power Plant }
        lPowerPlantID  := StrToIntDef(UnQuote(AShape.Cells[cNumber].Formula),0);
      end
      else
      begin
        { New Power Plant - from stencil }
        lPowerPlantID := ShowPowerPlantDialog;
        if (lPowerPlantID <> -1) then
          AShape.Cells[cNumber].Formula  := '"' + IntToStr(lPowerPlantID) + '"';
      end;
      AShape.Text := IntToStr(lPowerPlantID);
      if (lPowerPlantID = -1) then
        AShape.Delete
      else
      begin
        RefreshPowerPlant(AShape, lPowerPlantID);
        lTxtLabel := AddTextLabel(AShape, nvtName);
        RefreshText(lTxtLabel);

        lYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
        lPowerPlant     := lYieldModelData.NetworkFeaturesData.PowerPlantList.PowerPlantByID[lPowerPlantID];
        if(lPowerPlant <> nil) then
        begin
          if(lPowerPlant.PowerChannel <> nil) then
            AddPowerPlantChannel(AShape, lPowerPlant.PowerChannel);
          if(lPowerPlant.SpillChannel <> nil) then
            AddPowerPlantChannel(AShape, lPowerPlant.SpillChannel);
          FPowerPlantList.Add(IntToStr(lPowerPlantID));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVNVEventHandler.PowerPlantPositionChanged(ACell: IVCell; AShape: IVShape);
const OPNAME = 'TVNVEventHandler.PowerPlantPositionChanged';
var
  lTxtShape    : IVShape;
  lDiff        : double;
  lParentPin   : double;
  lElementID   : integer;
begin
  try
    lTxtShape := FindShapeWithParent(mtWRYMText, AShape.Name);
    if (lTxtShape <> nil) then
    begin
      if (ACell.Name = cPinX) then
      begin
        lDiff      := StrToFloatDef(UnQuote(lTxtShape.Cells[cParentXDiff].Formula),0);
        lParentPin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinX].Formula)),0);
        lTxtShape.Cells[cParentXMoved].Formula := '"' + IntToStr(1) + '"';
        lTxtShape.Cells[cPinX].Formula := '"' + FloatToStrF(lParentPin - lDiff, ffFixed, 12, 2) + 'mm"';
        if (FDrawing.GISMode) then
        begin
          lElementID := StrToIntDef(UnQuote(AShape.Cells[cNumber].Formula),0);
          GISCalculateLonLatOfShape(AShape,lElementID,gisCalcLon);
        end;
      end
      else
      if (ACell.Name = cPinY) then
      begin
        lDiff      := StrToFloatDef(UnQuote(lTxtShape.Cells[cParentYDiff].Formula),0);
        lParentPin := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinY].Formula)),0);
        lTxtShape.Cells[cParentYMoved].Formula := '"' + IntToStr(1) + '"';
        lTxtShape.Cells[cPinY].Formula := '"' + FloatToStrF(lParentPin - lDiff, ffFixed, 12, 2) + 'mm"';
        if (FDrawing.GISMode) then
        begin
          lElementID := StrToIntDef(UnQuote(AShape.Cells[cNumber].Formula),0);
          GISCalculateLonLatOfShape(AShape,lElementID,gisCalcLat);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVEventHandler.AddAllPowerPlantsToALayer: boolean;
const OPNAME = 'TVNVEventHandler.AddAllPowerPlantsToALayer';
var
  LShape : IVShape;
  LCount : integer;
  LElementID : integer;
  LPowerPlant : IPowerPlant;
  LPowerChannelShape,
  LSpillChannelShape : IVShape;
  LLayers : IVLayers;
  LLayer : IVLayer;
begin
  Result := False;
  try
    if AddNewLayer(LLayers,LLayer) then
    begin
      for LCount := 0 to FPowerPlantList.Count-1 do
      begin
        LElementID := StrToIntDef(FPowerPlantList[LCount],-1);
        LShape := FindPowerPlantByElementID(LElementID);
        if LShape <> nil then
        begin
          AddShapeToLayer(LLayers,LLayer,LShape);
          AddTextShapeToLayer(LLayers,LLayer,LShape);
          LPowerPlant := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.PowerPlantList.PowerPlantByID[lElementID];
          if (LPowerPlant <> nil) then
          begin
            if(LPowerPlant.PowerChannel <> nil) then
            begin
              LPowerChannelShape := FindChannelShapeByChannelNumber(LPowerPlant.PowerChannel.ChannelNumber);
              if (LPowerChannelShape <> nil) then
              begin
                AddShapeToLayer(LLayers,LLayer,LPowerChannelShape);
                AddTextShapeToLayer(LLayers,LLayer,LPowerChannelShape);
              end;
            end;
            if(LPowerPlant.SpillChannel <> nil) then
            begin
              LSpillChannelShape := FindChannelShapeByChannelNumber(LPowerPlant.SpillChannel.ChannelNumber);
              if (LSpillChannelShape <> nil) then
              begin
                AddShapeToLayer(LLayers,LLayer,LSpillChannelShape);
                AddTextShapeToLayer(LLayers,LLayer,LSpillChannelShape);
              end;
            end;
          end;
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.ClearAllPowerPlantsFromLayers: boolean;
const OPNAME = 'TVNVEventHandler.ClearAllPowerPlantsFromLayers';
var
  LShape : IVShape;
  LCount : integer;
  LElementID : integer;
  LPowerPlant : IPowerPlant;
  LPowerChannelShape,
  LSpillChannelShape : IVShape;
begin
  Result := False;
  try
    for LCount := 0 to FPowerPlantList.Count-1 do
    begin
      LElementID := StrToIntDef(FPowerPlantList[LCount],-1);
      LShape := FindPowerPlantByElementID(LElementID);
      if LShape <> nil then
      begin
        ClearShapeFromLayers(LShape);
        ClearTextShapesFromLayers(LShape);
        LPowerPlant := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.PowerPlantList.PowerPlantByID[lElementID];
        if (LPowerPlant <> nil) then
        begin
          if(LPowerPlant.PowerChannel <> nil) then
          begin
            LPowerChannelShape := FindChannelShapeByChannelNumber(LPowerPlant.PowerChannel.ChannelNumber);
            if (LPowerChannelShape <> nil) then
            begin
              ClearShapeFromLayers(LPowerChannelShape);
              ClearTextShapesFromLayers(LPowerChannelShape);
            end;
          end;
          if(LPowerPlant.SpillChannel <> nil) then
          begin
            LSpillChannelShape := FindChannelShapeByChannelNumber(LPowerPlant.SpillChannel.ChannelNumber);
            if (LSpillChannelShape <> nil) then
            begin
              ClearShapeFromLayers(LSpillChannelShape);
              ClearTextShapesFromLayers(LSpillChannelShape);
            end;
          end;
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVNVEventHandler.DeletePowerPlant(ANumber: integer): WordBool;
const OPNAME = 'TVNVEventHandler.DeletePowerPlant';
var
  lShape : IVShape;
begin
  Result := FALSE;
  try
    lShape := FindShapeWithPropertyValue(mtWRYMPowerPlant, cNumber, IntToStr(ANumber));
    if (lShape <> nil) AND (MayChangeScenario) then
    begin
      lShape.Delete;
      Result := (FAppModules.Model as IYieldModel).DoDeletePowerPlant(ANumber);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;

end;

function TVNVEventHandler.ProcessVNVPowerPlantRightClicked(AShapeName: string; ANumber    : integer): WordBool;
const OPNAME = 'TVNVEventHandler.ProcessVNVPowerPlantRightClicked';
var
  lYieldModel : IYieldModel;
  lOptionList : TStringList;
  lMenuDlg    : TVNVShapeMenuDlg;
  lOption     : integer;
  lPowerPlant : IPowerPlant;
  LPermanentColor  : boolean;
  lShape           : IVShape;
begin
  Result := FALSE;
  try
    lPowerPlant := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.PowerPlantList.PowerPlantByID[ANumber];
    if (lPowerPlant <> nil) then
    begin
      lYieldModel := (FAppModules.Model as IYieldModel);
      lOptionList := TStringList.Create;
      try
        lOptionList.AddObject(FAppModules.Language.GetString('VNV.MenuCancelOption'),TObject(1));
        lOptionList.AddObject(FAppModules.Language.GetString('VNV.ViewPowerPlantProperties'),TObject(1));
        lOptionList.AddObject(FAppModules.Language.GetString('VNV.ViewPowerPlantOutput'),TObject(1));
        lOptionList.AddObject(FAppModules.Language.GetString('VNV.DeletePowerPlantFromScenario'),TObject(1));
        LPermanentColor := True;
        lShape := GetFirstSelectedShape(mtWRYMPowerPlant);
        if(lShape <> nil) and (lShape.CellExists[cPermanentColor, 0] <> 0) then
        begin
          LPermanentColor := (lShape.Cells[cPermanentColor].Formula = '"TRUE"');
          if LPermanentColor then
            lOptionList.AddObject(FAppModules.Language.GetString('VNV.PermanentColorOff'),TObject(1))
          else
            lOptionList.AddObject(FAppModules.Language.GetString('VNV.PermanentColorOn'),TObject(1));
        end;

        lMenuDlg := TVNVShapeMenuDlg.Create(nil);
        try
          lOption := lMenuDlg.ShowShapeMenuDialog(lOptionList);
        finally
          lMenuDlg.Free;
        end;
      finally
        FreeAndNil(lOptionList);
      end;
      case lOption of
        1 : begin
              Result := lYieldModel.ViewInputPowerPlantDialog(ANumber);
              if (Result) then
                PowerPlantHasChanged(AShapeName, ANumber);
            end;
        2 : Result := lYieldModel.ViewOutputPowerPlantDialog(ANumber);
        3 : Result := DeletePowerPlant(ANumber);
        4 : begin
              if LPermanentColor then
                lShape.Cells[cPermanentColor].Formula         := '"FALSE"'
              else
                lShape.Cells[cPermanentColor].Formula         := '"TRUE"';
              Result := True;
            end;
      end;
    end
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

