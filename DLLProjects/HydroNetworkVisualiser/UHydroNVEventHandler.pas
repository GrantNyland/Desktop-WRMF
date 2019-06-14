{******************************************************************************}
{* UNIT : Contains the class THydroNVEventHandler.                            *}
{*        (Network Visualiser for Hydrology model)                            *}
{******************************************************************************}

unit UHydroNVEventHandler;

interface

uses
  VCL.Forms,
  Classes,
  StdVCL,
  VCL.ExtCtrls,
  StrUtils,
  XMLIntf,

  Visio_TLB,
  VisOcx_TLB,
  VoaimsCom_TLB,
  HydrologyCom_TLB,
  GisViewerX41Control_TLB,
  UHydroNVDrawing,
  UHydroNVOutputSelectorDlg,
  UHydroNVXMLAgent,
  UGISForm,
  UAbstractObject;

const
  mtHydroServer          = 'Server Connection';
  mtHydroNetwork         = 'Network (Hydro)';
  mtHydroReservoir       = 'Reservoir (Hydro)';
  mtHydroChannel         = 'Channel Reach (Hydro)';
  mtHydroRunOff          = 'RunOff (Hydro)';
  mtHydroMine            = 'Mine (Hydro)';
  mtHydroIrrBlock        = 'Irrigation Block (Hydro)';
  mtHydroRoute           = 'Route (Hydro)';
  mtHydroObsPoint        = 'Observation Point (Hydro)';
  mtHydroText            = 'Text (Hydro)';
  mtHydroOutputSelector  = 'Output Selector (Hydro)';

  CThisVersion           = '1.1.0';

  cStencilName          = 'HydroStencil.vss';
  cAngle                = 'Angle';
  cPinX                 = 'PinX';
  cPinY                 = 'PinY';
  cBeginX               = 'BeginX';
  cBeginY               = 'BeginY';
  cEndX                 = 'EndX';
  cEndY                 = 'EndY';
  cVersion              = 'Prop.Version.Value';
  cVersionLbl           = 'Prop.Version.Label';
  cVersionInv           = 'Prop.Version.Invisible';
  cNumber               = 'Prop.Number.Value';
  cElementSubType       = 'Prop.ElementSubType.Value';
  cElementSubTypeLbl    = 'Prop.ElementSubType.Label';
  cElementSubTypeInv    = 'Prop.ElementSubType.Invisible';
  cSectionNo            = 'Prop.SectionNo.Value';
  cSectionNoLbl         = 'Prop.SectionNo.Label';
  cSectionNoInv         = 'Prop.SectionNo.Invisible';
  cDuplicate            = 'Prop.Duplicate.Value';
  cNetworkSequence      = 'Prop.NetworkSequence.Value';
  cModuleID             = 'Prop.ModuleID.Value';
  cRouteID              = 'Prop.RouteID.Value';
//  cRouteNo              = 'Prop.RouteNo.Value';
  cName                 = 'Prop.Name.Value';
  cFlowFile             = 'Prop.FlowFile.Value';
  cRow1Action           = 'Actions.Row_1.Action';
  cTextType             = 'Prop.TextType.Value';
  cTextTypeLbl          = 'Prop.TextType.Label';
  cTextTypeInv          = 'Prop.TextType.Invisible';
  cOldTextType          = 'Prop.Type.Value';
  cOldTextLbl           = 'Prop.Type.Label';
  cOldTextInv           = 'Prop.Type.Invisible';
  cParent               = 'Prop.ParentShapeName.Value';
  cParentXDiff          = 'Prop.ParentXDiff.Value';
  cParentYDiff          = 'Prop.ParentYDiff.Value';
  cParentMoved          = 'Prop.ParentMoved.Value';
  cParentXMoved         = 'Prop.ParentXMoved.Value';
  cParentYMoved         = 'Prop.ParentYMoved.Value';
  cPercFromStart        = 'Prop.PercFromStart.Value';
  cSink                 = 'Prop.Sink.Value';
  cSource               = 'Prop.Source.Value';
  cColourChanged        = 'Prop.ColourChanged.Value';
  cShapeType            = 'Prop.ShapeType.Value';
  cInterval             = 'Prop.Interval.Value';

  cGISScaleFactor   = 4;
  cGISWidth         = 994;
  cGISHeight        = 663;
  OutputTextTypeSet = [nvtRQSimulatedRouteFlow, nvtRQDemands, nvtRQShortages,
                       nvtRUNetCatchmentRunOff, nvtRUTotalSurfaceRunOff, nvtRUGroundWaterOutFlow,
                       nvtRUPavedAreaFlow, nvtRUPitmanS, nvtRUAquiferStorage, nvtRUGroundWaterRecharge,
                       nvtRUWeightedPitmanS, nvtRUGroundWaterBaseFlow, nvtRUInterflow,
                       nvtCRWetlandUpstreamFlow, nvtCRWetlandInFlow, nvtCRWetlandStorage, nvtCRWetlandReturnFlow,
                       nvtRVReservoirStorage,
                       nvtMMPlantRunOff,
                       nvtMMUGUpStreamAreaRunOff, nvtMMUGRecharge, nvtMMUGBoardPillarRunOff, nvtMMUGHighExtractionRunOff,
                       nvtMMSDSurfaceRunOff, nvtMMSDSeepage, nvtMMSDPCDInFlow, nvtMMSDPCDStorage, nvtMMSDPCDSpillage,
                       nvtMMOCDisturbedAreaRunOff, nvtMMOCDisturbedAreaRecharge, nvtMMOCWorkingAreaRunOff,
                       nvtMMOCDisturbedWorkingsRecharge, nvtMMOCDisturbedWorkingsRunOff, nvtMMOCDisturbedWorkingsSeepage,
                       nvtMMOCDisturbedWorkingsDecant, nvtMMOCPCDSpillage, nvtMMOCPCDWaterBalance,
                       nvtMMOCPCDMonthStartStorage, nvtMMOCPCDMonthEndStorage, nvtMMOCInspoilStorage];

type TGISCalcLonOrLat  = (gisCalcLon, gisCalcLat, gisCalcBoth);
type THydroNVShapeType = (stHydroNone, stHydroServer, stHydroNetwork, stHydroReservoir, stHydroChannel, stHydroRunOff,
                          stHydroMine, stHydroIrrBlock, stHydroRoute, stHydroObsPoint, stHydroText, stHydroOutputSelector);

type
  THydroNVEventHandler = class(TAbstractAppObject)
  protected
    FVisioApp               : IVApplication;
    FVisioDoc               : IVDocument;
    FHydroStencil           : IVDocument;
    FVisioToolbar           : IVToolbarSet;
    FVisioEventCode         : integer;
    FGISViewer              : TGisViewerX;
    FGISForm                : TGISForm;
    FSystemFlag             : boolean;

    FDrawing                : THydroNVDrawing;
    FMapWidth               : Double;
    FMapHeight              : Double;
    FPageWidth              : Double;
    FPageHeight             : Double;
    FNoRecalcLonLatList     : TStringList;

    FOutputSelector         : IVShape;
    FInterval               : Integer;
    FIntervalSet            : Boolean;
    
    FReservoirList          : TStringList;
    FRunOffList             : TStringList;
    FChannelList            : TStringList;
    FIrrBlockList           : TStringList;
    FMineList               : TStringList;
    FRouteList              : TStringList;
    FObsPointList           : TStringList;
    FOutputTextList         : TStringList;

    FDuplicateReservoirList : TStringList;
    FDuplicateRunOffList    : TStringList;
    FDuplicateChannelList   : TStringList;
    FDuplicateIrrBlockList  : TStringList;
    FDuplicateMineList      : TStringList;
    FDuplicateRouteList     : TStringList;
    FDuplicateObsPointList  : TStringList;

    FBufferReservoirList    : TStringList;
    FBufferRunOffList       : TStringList;
    FBufferChannelList      : TStringList;
    FBufferIrrBlockList     : TStringList;
    FBufferMineList         : TStringList;
    FBufferRouteList        : TStringList;
    FBufferTextList         : TStringList;
    FBufferObsPointList     : TStringList;

    FMayChangeStructure     : Boolean;
    FNVOutputSelectorDlg    : THydroNVOutputSelectorDlg;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnDocumentOpened (ASender    : TObject; const ADoc : IVDocument);
    procedure SetStencilPath;
    procedure InitialiseDrawing(const ADoc : IVDocument);
    procedure UpgradeDocument (const ADoc : IVDocument);
    procedure RefreshDocument (const ADoc : IVDocument);

    function ShowGIS : boolean;
    function HideGIS : boolean;
    procedure CreateGISViewer;
    function GetGISPage: IVPage;
    procedure PositionShapeOnGIS (AShape: IVShape; AElementNo : Integer);
    procedure GISSetXYPosFromLonLat(AShape     : IVShape;
                                    AElementNo : integer);
    procedure GISCalculateLonLatOfShape (AShape     : IVShape;
                                         AElementNo : integer;
                                         AGisCalcLonOrLat : TGISCalcLonOrLat);
    procedure GISPopulateNoRecalcLonLatList (AShape     : IVShape;
                                             AElementNo : Integer);
    function GISConvertLonLatToXY(AGISViewer: TGisViewerX; ALon, ALat: double; var AX, AY: double): boolean;

    procedure CheckUpgradeReservoir (AShape : IVShape);
    procedure CheckUpgradeRunOff (AShape : IVShape);
    procedure CheckUpgradeChannel (AShape : IVShape);
    procedure CheckUpgradeIrrBlock (AShape : IVShape);
    procedure CheckUpgradeMine (AShape : IVShape);
    procedure CheckUpgradeRoute (AShape : IVShape);
    procedure CheckUpgradeObsPoint (AShape : IVShape);
    procedure CheckUpgradeText (AShape : IVShape);
    procedure CheckUpgradeOutputSelector (AShape : IVShape);

    function UpgradeReservoir (AShape : IVShape) : IVShape;
    function UpgradeRunOff (AShape : IVShape) : IVShape;
    function UpgradeChannel (AShape : IVShape) : IVShape;
    function UpgradeIrrBlock (AShape : IVShape) : IVShape;
    function UpgradeMine (AShape : IVShape) : IVShape;
    function UpgradeRoute (AShape : IVShape) : IVShape;
    function UpgradeObsPoint (AShape : IVShape) : IVShape;
    function UpgradeText (AShape : IVShape) : IVShape;
    function UpgradeOutputSelector (AShape : IVShape) : IVShape;

    procedure RefreshShape (AShape : IVShape);
    procedure RefreshReservoir (AShape : IVShape; AElementNo : integer);
    procedure RefreshRunOff (AShape : IVShape; AElementNo : integer);
    procedure RefreshChannel (AShape : IVShape; AElementNo : integer);
    procedure RefreshIrrBlock (AShape : IVShape; AElementNo : integer);
    procedure RefreshMine (AShape : IVShape; AElementNo : integer);
    procedure RefreshRoute (AShape : IVShape; AElementNo : integer);
    procedure RefreshObsPoint (AShape : IVShape; AElementNo : integer);
    procedure RefreshOutputSelector (AShape : IVShape);
    procedure RefreshText (AShape : IVShape);
    procedure RefreshObsFlowFileText (AShape : IVShape; AElementNo : integer);
    procedure RefreshNetworkText (AShape : IVShape);
    procedure RefreshAllNetworkText;
    procedure RefreshAllOutputText;
    procedure RefreshTextShapesWithParent (AParentShape : IVShape);

    procedure OnShapeAdded (ASender : TObject; const AShape : IVShape);
    procedure OnShapeDelete (ASender : TObject; const AShape : IVShape);
    procedure AddConnection (ASourceShape : IVShape; ATargetShape : IVShape);
    procedure DeleteConnection (ASourceShape : IVShape;  ATargetShape : IVShape);
    procedure PositionChanged (ACell  : IVCell;  AShape : IVShape);
    procedure ColourChanged (ACell  : IVCell; AShape : IVShape);

    procedure ReservoirShapeAdded (const AShape : IVShape; AElementNo : Integer = -1; ADeleteLock : Integer = 0);
    procedure RunOffShapeAdded (const AShape : IVShape; AElementNo : Integer = -1);
    procedure ChannelShapeAdded (const AShape : IVShape; AElementNo : Integer = -1);
    procedure IrrBlockShapeAdded (const AShape : IVShape; AElementNo : Integer = -1);
    procedure MineShapeAdded (const AShape : IVShape; AElementNo : Integer = -1);
    procedure RouteShapeAdded (const AShape : IVShape; AElementNo : Integer = -1);
    procedure ObsPointShapeAdded (const AShape : IVShape; AElementNo : Integer = -1; ADeleteLock : Integer = 0);
    procedure TextShapeAdded(const AShape: IVShape);
    procedure OutputSelectorShapeAdded(const AShape: IVShape);

    procedure ReservoirShapeDeleted (const AShape : IVShape);
    procedure RunOffShapeDeleted (const AShape : IVShape);
    procedure ChannelShapeDeleted (const AShape : IVShape);
    procedure IrrBlockShapeDeleted (const AShape : IVShape);
    procedure MineShapeDeleted (const AShape : IVShape);
    procedure RouteShapeDeleted (const AShape : IVShape);
    procedure ObsPointShapeDeleted (const AShape : IVShape);

    procedure NetworkModulePositionChanged (ACell : IVCell; AShape : IVShape);
    procedure NetworkLinePositionChanged (ACell : IVCell; AShape : IVShape);
    procedure ReservoirPositionChanged (ACell : IVCell; AShape : IVShape);
    procedure RunOffPositionChanged (ACell : IVCell; AShape : IVShape);
    procedure ChannelPositionChanged (ACell : IVCell; AShape : IVShape);
    procedure IrrBlockPositionChanged (ACell : IVCell; AShape : IVShape);
    procedure MinePositionChanged (ACell : IVCell; AShape : IVShape);
    procedure RoutePositionChanged (ACell : IVCell; AShape : IVShape);
    procedure ObsPointPositionChanged (ACell : IVCell; AShape : IVShape);
    procedure TextPositionChanged (ACell : IVCell; AShape : IVShape);

    function DeleteReservoirFromNetwork (ANumber : Integer) : WordBool;
    function DeleteChannelFromNetwork (ANumber : Integer) : WordBool;
    function DeleteRunOffFromNetwork (ANumber : Integer) : WordBool;
    function DeleteIrrigationFromNetwork (ANumber : Integer) : WordBool;
    function DeleteMineFromNetwork (ANumber : Integer) : WordBool;
    function DeleteRouteFromNetwork (ANumber : Integer) : WordBool;
    function DeleteObservationPointFromNetwork (ANumber : Integer) : WordBool;

    procedure SnapAddedRouteToModules (const AShape : IVShape; ARouteNr : integer);
    procedure SnapRoutesToAddedModule (const AShape : IVShape; ATypedNumber : String);
    function FindRouteSinkShape (ARoute : INetworkRoute) : IVShape;
    function FindRouteSourceShape (ARoute : INetworkRoute) : IVShape;
    procedure PositionObsPointOnRoute (const AShape : IVShape; const ARouteShape : IVShape);

    function ShowSelectReservoirsDialog : integer;
    function ShowSelectRunOffsDialog : integer;
    function ShowSelectChannelsDialog : integer;
    function ShowSelectIrrBlocksDialog : integer;
    function ShowSelectMinesDialog : integer;
    function ShowSelectRoutesDialog : integer;
    function ShowSelectObsPointsDialog : integer;
    function ShowSelectTextDialog (var AElementType    : string;
                                   var AElementNo      : Integer;
                                   var ATextType       : THydroNVTextType;
                                   var AElementSubType : THydroNVElementSubType;
                                   var ASectionNo      : Integer) : boolean;
    function ShowNVOutputSelectorDlg (var AInterval : Integer) : Boolean;

    procedure GenerateNetworkTextXML (AXMLDocument : IXMLDocument;
                                      AXMLAgent    : THydroNVXMLAgent);
    procedure GenerateReservoirTextXML (AXMLDocument : IXMLDocument;
                                        AXMLAgent    : THydroNVXMLAgent);
    procedure GenerateRunOffTextXML (AXMLDocument : IXMLDocument;
                                     AXMLAgent    : THydroNVXMLAgent);
    procedure GenerateChannelTextXML (AXMLDocument : IXMLDocument;
                                      AXMLAgent    : THydroNVXMLAgent);
    procedure GenerateIrrBlockTextXML (AXMLDocument : IXMLDocument;
                                       AXMLAgent    : THydroNVXMLAgent);
    procedure GenerateMineTextXML (AXMLDocument : IXMLDocument;
                                   AXMLAgent    : THydroNVXMLAgent);
    procedure GenerateNetworkRouteTextXML (AXMLDocument : IXMLDocument;
                                           AXMLAgent    : THydroNVXMLAgent);
    procedure GenerateObsPointTextXML (AXMLDocument : IXMLDocument;
                                       AXMLAgent    : THydroNVXMLAgent);
    procedure PopulateAllReservoirLists (AIDList : TStringList; ANameList : TStringList; AInList : TStringList);
    procedure PopulateAllRunOffLists (AIDList : TStringList; ANameList : TStringList; AInList : TStringList);
    procedure PopulateAllChannelLists (AIDList : TStringList; ANameList : TStringList; AInList : TStringList);
    procedure PopulateAllIrrBlockLists (AIDList : TStringList; ANameList : TStringList; AInList : TStringList);
    procedure PopulateAllMineLists (AIDList : TStringList; ANameList : TStringList; AInList : TStringList);
    procedure PopulateAllRouteLists (AIDList : TStringList; ANameList : TStringList; AInList : TStringList);
    procedure PopulateAllObsPointLists (AIDList   : TStringList;
                                        ANameList : TStringList;
                                        ANewList  : TStringList;
                                        AInList   : TStringList);

    function FindServerShape : IVShape;
    function FindShapeWithParent (AMasterName : string; AParentName : string) : IVShape;
    procedure FindTextShapesWithParent (AParentName : string; AShapesList : TStringList);
    function FindShapeWithName (AShapeName : string) : IVShape;
    function FindShapeWithPropertyValue (AMasterName : string; AProperty : string; AValue : string) : IVShape;
    procedure RenameDuplicateShapes (AMasterName : string; ANumber : string; ADupNo : Integer);
    procedure DeleteDuplicateShapes (AMasterName : string; ANumber : string);
    procedure FindObsPointsOnRoute(ARouteNo : integer; AShapesList: TStringList);

    function ShapeIsSelected (ASelection : IVSelection; AShapeName : string) : Boolean;
    procedure DeleteAllTextShapes (const AParentShape : IVShape);

    function UnQuote (AString : string) : string;
    function UnMM (AString : string) : string;
    function UnDeg (AString : string) : string;
    function TextTypeToStr (AType : THydroNVTextType) : string;
    function ElementSubTypeToStr (AType : THydroNVElementSubType) : string;
    function IsShapeDuplicate (AShape : IVShape) : Boolean;
    
    function ProcessHydroNVToggleGIS : boolean;
    function ProcessHydroNVDoubleClicked (AShapeName : string; ANumber : integer) : WordBool;
    function ProcessHydroNVRightClicked (AShapeName : string; ANumber : integer) : WordBool;
    function ProcessHydroNVObservationPointRightClicked (AShapeName : string; ANumber : integer) : WordBool;
    function ProcessHydroNVNetworkRouteRightClicked (AShapeName : string; ANumber : integer) : WordBool;
    function ProcessHydroNVReservoirModuleRightClicked (AShapeName : string; ANumber : integer) : WordBool;
    function ProcessHydroNVRunOffModuleRightClicked (AShapeName : string; ANumber : integer) : WordBool;
    function ProcessHydroNVChannelModuleRightClicked (AShapeName : string; ANumber : integer) : WordBool;
    function ProcessHydroNVIrrigationModuleRightClicked (AShapeName : string; ANumber : integer) : WordBool;
    function ProcessHydroNVMineModuleRightClicked (AShapeName : string; ANumber : integer) : WordBool;
    procedure FirstIntervalClick(Sender: TObject);
    procedure Minus12IntervalsClick(Sender: TObject);
    procedure PreviousIntervalClick(Sender: TObject);
    procedure NextIntervalClick(Sender: TObject);
    procedure Plus12IntervalsClick(Sender: TObject);
    procedure LastIntervalClick(Sender: TObject);
  private
    function GetShapeElementNoAsString (AShape : IVShape) : String;
    function GetShapeElementNo (AShape : IVShape; ADefault : Integer) : Integer;
    procedure SetShapeElementNo (AShape : IVShape; AValue : Integer);
    function GetShapeModuleID (AShape : IVShape) : Integer;
    procedure SetShapeModuleID (AShape : IVShape; AValue : Integer);
//    function GetShapeRouteNo (AShape : IVShape) : Integer;
    function GetShapeRouteID (AShape : IVShape) : Integer;
    procedure SetShapeRouteID (AShape : IVShape; AValue : Integer);
//    function GetShapeNetworkSequence (AShape : IVShape) : Integer;
    procedure SetShapeNetworkSequence (AShape : IVShape; AValue : Integer);
    function GetShapeDuplicateNoAsString (AShape : IVShape) : String;
    function GetShapeDuplicateNo (AShape : IVShape) : Integer;
    procedure SetShapeDuplicateNo (AShape : IVShape; AValue : Integer);
    function GetShapeHydroNVTextType (AShape : IVShape) : THydroNVTextType;
    procedure SetShapeHydroNVTextType (AShape : IVShape; ATextType : THydroNVTextType);
    function GetShapeHydroNVElementSubType (AShape : IVShape) : THydroNVElementSubType;
    procedure SetShapeHydroNVElementSubType (AShape : IVShape; AElementSubType : THydroNVElementSubType);
    function GetShapeSectionNo (AShape : IVShape) : Integer;
    procedure SetShapeSectionNo (AShape : IVShape; AValue : Integer);
    function GetShapeVersion (AShape : IVShape) : String;
    procedure SetShapeVersion (AShape : IVShape; AVersion : String);
    procedure SetShapeColour (AShape : IVShape; AColour : Integer);
    procedure SetShapeLinePattern (AShape : IVShape; APattern : Integer);
    procedure SetShapeActionProcessVNVSpecial (AShape : IVShape);
    function GetShapeName (AShape : IVShape) : String;
    procedure SetShapeName (AShape : IVShape; AName : String);
    function GetShapePinX (AShape : IVShape) : Double;
    procedure SetShapePinX (AShape : IVShape; AValue : Double);
    function GetShapePinY (AShape : IVShape) : Double;
    procedure SetShapePinY (AShape : IVShape; AValue : Double);
    function GetShapeParent (AShape : IVShape) : String;
    procedure SetShapeParent (AShape : IVShape; AParent : String);
    function GetShapeParentXDiff (AShape : IVShape) : Double;
    procedure SetShapeParentXDiff (AShape : IVShape; AValue : Double);
    function GetShapeParentYDiff (AShape : IVShape) : Double;
    procedure SetShapeParentYDiff (AShape : IVShape; AValue : Double);
    function GetShapeParentXMoved (AShape : IVShape) : Integer;
    procedure SetShapeParentXMoved (AShape : IVShape; AValue : Integer);
    function GetShapeParentYMoved (AShape : IVShape) : Integer;
    procedure SetShapeParentYMoved (AShape : IVShape; AValue : Integer);
    function GetShapeParentMoved (AShape : IVShape) : Integer;
    procedure SetShapeParentMoved (AShape : IVShape; AValue : Integer);
    function GetShapePercFromStart (AShape : IVShape) : Double;
    procedure SetShapePercFromStart (AShape : IVShape; AValue : Double);
    function GetRouteShapeSource (AShape : IVShape) : String;
    procedure SetRouteShapeSource (AShape : IVShape; AValue : String);
    function GetRouteShapeSink (AShape : IVShape) : String;
    procedure SetRouteShapeSink (AShape : IVShape; AValue : String);
    procedure AddElementToDuplicateList (ADuplicateList : TStringList; AElementNoStr : String);
    procedure RemoveElementFromDuplicateList (ADuplicateList : TStringList; AElementNoStr : String);
    procedure ClearElementFromDuplicateList (ADuplicateList : TStringList; AElementNoStr : String);
    function TotalElementsInDuplicateList (ADuplicateList : TStringList) : Integer;
    function CountElementInDuplicateList (ADuplicateList : TStringList; AElementNoStr : String) : Integer;
    function HydroNVShapeType (AShape : IVShape) : THydroNVShapeType;
    function GetMasterName (AModuleType : String) : String;
    function DetermineNetworkSequence (AModuleID : Integer): Boolean;
{    procedure PopulateTextTypeLists (ATypeIDList   : TStringList;
                                     ATypeNameList : TStringList;
                                     ATypeSet      : THydroNVTextTypeSet);}
    function GetOutputTextLabel (AParentShapeType : THydroNVShapeType;
                                 AElementNo       : Integer;
                                 ATextType        : THydroNVTextType;
                                 AElementSubType  : THydroNVElementSubType;
                                 ASectionNo       : Integer) : String;
    function GetOutputValue (AParentShapeType : THydroNVShapeType;
                             AElementID       : Integer;
                             ATextType        : THydroNVTextType;
                             AElementSubType  : THydroNVElementSubType;
                             ASectionNo       : Integer;
                             var AValue       : Double) : Boolean;
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
  Math,
  SysUtils,
  System.UITypes,
  VCL.Dialogs,
  VCL.Controls,
  VCL.Menus,
  VCL.Graphics,

  UUtilities,
  UHydroNVProgressDlg,
  UHydroNVSelectElementDlg,
  UHydroNVSelectTextDlg,
  UHydroNVShapemenuDlg,
  UErrorHandlingOperations;

{* THydroNVEventHandler *******************************************************}

procedure THydroNVEventHandler.CreateMemberObjects;
const OPNAME = 'THydroNVEventHandler.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FSystemFlag            := FALSE;
    FOutputSelector        := nil;
    FGISViewer             := nil;
    FGISForm               := nil;
    FDrawing               := THydroNVDrawing.Create;
    FNoRecalcLonLatList    := TStringList.Create;
    FInterval              := 0;
    FIntervalSet           := FALSE;
    
    FReservoirList    := TStringList.Create;
    FRunOffList       := TStringList.Create;
    FChannelList      := TStringList.Create;
    FIrrBlockList     := TStringList.Create;
    FMineList         := TStringList.Create;
    FRouteList        := TStringList.Create;
    FObsPointList     := TStringList.Create;
    FOutputTextList   := TStringList.Create;
    
    FDuplicateReservoirList := TStringList.Create;
    FDuplicateRunOffList    := TStringList.Create;
    FDuplicateChannelList   := TStringList.Create;
    FDuplicateIrrBlockList  := TStringList.Create;
    FDuplicateMineList      := TStringList.Create;
    FDuplicateRouteList     := TStringList.Create;
    FDuplicateObsPointList  := TStringList.Create;

    FBufferReservoirList    := TStringList.Create;
    FBufferRunOffList       := TStringList.Create;
    FBufferChannelList      := TStringList.Create;
    FBufferIrrBlockList     := TStringList.Create;
    FBufferMineList         := TStringList.Create;
    FBufferRouteList        := TStringList.Create;
    FBufferObsPointList     := TStringList.Create;
    FBufferTextList         := TStringList.Create;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.DestroyMemberObjects;
const OPNAME = 'THydroNVEventHandler.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil(FGISViewer);
    FreeAndNil(FGISForm);
    FreeAndNil(FDrawing);
    FreeAndNil(FNoRecalcLonLatList);

    FreeAndNil(FReservoirList);
    FreeAndNil(FRunOffList);
    FreeAndNil(FChannelList);
    FreeAndNil(FIrrBlockList);
    FreeAndNil(FMineList);
    FreeAndNil(FRouteList);
    FreeAndNil(FObsPointList);
    FreeAndNil(FOutputTextList);
    
    FreeAndNil(FDuplicateReservoirList);
    FreeAndNil(FDuplicateRunOffList);
    FreeAndNil(FDuplicateChannelList);
    FreeAndNil(FDuplicateIrrBlockList);
    FreeAndNil(FDuplicateMineList);
    FreeAndNil(FDuplicateRouteList);
    FreeAndNil(FDuplicateObsPointList);

    FreeAndNil(FBufferReservoirList);
    FreeAndNil(FBufferRunOffList);
    FreeAndNil(FBufferChannelList);
    FreeAndNil(FBufferIrrBlockList);
    FreeAndNil(FBufferMineList);
    FreeAndNil(FBufferRouteList);
    FreeAndNil(FBufferObsPointList);
    FreeAndNil(FBufferTextList);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.InitialiseDrawing(const ADoc : IVDocument);
const OPNAME = 'THydroNVEventHandler.InitialiseDrawing';
var
  LServerShape    : IVShape;
  LDrawing        : IHydroNVDrawing;
  LDrawingName    : String;
  LHydrologyModel : IHydrologyModel;
  LPos            : Integer;
begin
  try
    LServerShape := FindServerShape;
    if (LServerShape <> nil) then
    begin
      LDrawingName := ADoc.Name;
      LPos := Pos('.', LDrawingName);
      if (LPos > 0) then
        LDrawingName := Copy(LDrawingName, 1, LPos-1);
      LHydrologyModel := FAppModules.Model as IHydrologyModel;
      LDrawing   := LHydrologyModel.Network.HydroNVDrawingAgent.HydroNVDrawingByName[LDrawingName];
//      FMayChangeStructure := FALSE; // RianaChangeStructure
      FMayChangeStructure := LHydrologyModel.MayChangeNetwork;

      if (LDrawing <> nil) then
        FDrawing.Populate(LDrawing.NetworkID,
                          LDrawing.DrawingID,
                          LDrawing.DrawingName,
                          LDrawing.GISDrawing,
                          LDrawing.ReadOnly);
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.CreateGISViewer;
const OPNAME = 'THydroNVEventHandler.CreateGISViewer';
begin
  try
    FGISForm          := TGISForm.Create(nil);
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
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.HandleVNVEvent (AVisioApp       : IUnknown;
                                              AVisioDoc       : IUnknown;
                                              AVisioEventCode : integer;
                                              ASourceObj      : IUnknown;
                                              AEventID        : Integer;
                                              AEventSeqNum    : Integer;
                                              ASubjectObj     : IUnknown;
                                              AMoreInfo       : OleVariant): boolean;
const OPNAME = 'THydroNVEventHandler.HandleVNVEvent';
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
        if (Pos(PChar(mtHydroRoute), lSourceShape.Name) > 0) then
          AddConnection(lSourceShape, lTargetShape);
      end;
      visEvtDel + visEvtConnect:
      begin
        lTargetShape := (ASubjectObj as IVConnects).ToSheet;
        lSourceShape := (ASubjectObj as IVConnects).FromSheet;
        if (Pos(PChar(mtHydroRoute), lSourceShape.Name) > 0) then
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
           (Pos(PChar(mtHydroRoute), lCell.Shape.Name) > 0) then
          ColourChanged(lCell, lCell.Shape);
      end;
    else
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.ProcessVNVSpecial (const AParameter: WideString): boolean;
const OPNAME = 'THydroNVEventHandler.ProcessVNVSpecial';
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
          ProcessHydroNVDoubleClicked(lShapeName, lNumber)
        else
        if (lFunction = '1') then
          ProcessHydroNVRightClicked(lShapeName, lNumber)
        else
        if (lFunction = '2') then
          ProcessHydroNVToggleGIS
        else
          MessageDlg(lFunction + ' '+ FAppModules.Language.GetString('VisioNetwork.MshNotImplemented'), mtError, [mbOK], 0);
      end;
    end; 
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.OnDocumentOpened (ASender    : TObject;
                                                 const ADoc : IVDocument);
const OPNAME = 'THydroNVEventHandler.OnDocumentOpened';
var
  lIndex       : integer;
  lDocument    : IVDocument;
begin
  try
    FReservoirList.Clear;
    FRunOffList.Clear;
    FChannelList.Clear;
    FIrrBlockList.Clear;
    FMineList.Clear;
    FRouteList.Clear;
    FObsPointList.Clear;
    FOutputTextList.Clear;
    
    FDuplicateReservoirList.Clear;
    FDuplicateRunOffList.Clear;
    FDuplicateChannelList.Clear;
    FDuplicateIrrBlockList.Clear;
    FDuplicateMineList.Clear;
    FDuplicateRouteList.Clear;
    FDuplicateObsPointList.Clear;

    FNoRecalcLonLatList.Clear;

    FOutputSelector := nil;
    
    SetStencilPath;

    FHydroStencil := nil;
    lIndex       := 1;
    while ((FHydroStencil = nil) AND (lIndex <= FVisioApp.Documents.Count)) do
    begin
      lDocument := FVisioApp.Documents.Item[lIndex];
      if (Trim(UpperCase(lDocument.Name)) = UpperCase(cStencilName)) then
        FHydroStencil := lDocument
      else
        lIndex := lIndex + 1;
    end;

    InitialiseDrawing(ADoc);
//    GetSelectionData;
    UpgradeDocument(ADoc);
    RefreshDocument(ADoc);

    if (FDrawing.GISDrawing = 1) then
      ShowGIS
    else
      HideGIS;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.SetStencilPath;
const OPNAME = 'THydroNVEventHandler.SetStencilPath';
var
  lWRIMSPath    : string;
  lStencilPaths : string;
  lVNVPath      : string;
  lMsg          : string;
begin
  try
    lStencilPaths := Trim(FVisioApp.StencilPaths);
    lVNVPath      := NetworkDiagramsPath;
    lWRIMSPath    := lVNVPath + cStencilName;
    lVNVPath      := Copy(lVNVPath, 1, Length(lVNVPath) - 1);

    if (Pos(UpperCase(lVNVPath), UpperCase(lStencilPaths)) = 0) then
    begin
      if (FileExists(lWRIMSPath)) then
      begin
        if (lStencilPaths = '') then
          FVisioApp.StencilPaths := lVNVPath
        else
          FVisioApp.StencilPaths := lStencilPaths + ';' + lVNVPath;
      end
      else
      begin
        lMsg := FAppModules.Language.GetString('VNV.StencilDoesNotExist');
        lMsg := Format(lMsg, [lWRIMSPath]);
        MessageDlg(lMsg, mtError, [mbOK], 0);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.UpgradeDocument (const ADoc : IVDocument);
const OPNAME = 'THydroNVEventHandler.UpgradeDocument';
var
  lPageIdx     : integer;
  lShapeIdx    : integer;
  lPage        : IVPage;
  lShape       : IVShape;
  lShapeName   : string;
  lVersion     : string;
  LShapeType   : THydroNVShapeType;
begin
  try
    FBufferReservoirList.Clear;
    FBufferRunOffList.Clear;
    FBufferChannelList.Clear;
    FBufferIrrBlockList.Clear;
    FBufferMineList.Clear;
    FBufferRouteList.Clear;
    FBufferObsPointList.Clear;
    FBufferTextList.Clear;

    ADoc.Protection[0] := ADoc.Protection[0] - visProtectShapes;
    for lPageIdx := 1 to ADoc.Pages.Count  do
    begin
      lPage := ADoc.Pages.Item[lPageIdx];
      lShapeIdx := 1;
      while (lShapeIdx <= lPage.Shapes.Count)  do
      begin
        lShape := lPage.Shapes.Item[lShapeIdx];
        LShapeType := HydroNVShapeType(lShape);
        case LShapeType of
          stHydroReservoir      : CheckUpgradeReservoir(lShape);
          stHydroRunOff         : CheckUpgradeRunOff(lShape);
          stHydroChannel        : CheckUpgradeChannel(lShape);
          stHydroIrrBlock       : CheckUpgradeIrrBlock(lShape);
          stHydroMine           : CheckUpgradeMine(lShape);
          stHydroRoute          : CheckUpgradeRoute(lShape);
          stHydroObsPoint       : CheckUpgradeObsPoint(lShape);
          stHydroText           : CheckUpgradeText(lShape);
          stHydroOutputSelector : CheckUpgradeOutputSelector(lShape);
        else
        end;
        lShapeIdx := lShapeIdx + 1;
      end;
    end;

    ADoc.Protection[0] := ADoc.Protection[0] + visProtectShapes;

    for lShapeIdx := 0 to FBufferReservoirList.Count - 1 do
    begin
      lShapeName := FBufferReservoirList.Strings[lShapeIdx];
      lShape     := FindShapeWithName(lShapeName);
      if (lShape <> nil) then
        UpgradeReservoir(lShape);
    end;
    FBufferReservoirList.Clear;

    for lShapeIdx := 0 to FBufferRunOffList.Count - 1 do
    begin
      lShapeName := FBufferRunOffList.Strings[lShapeIdx];
      lShape     := FindShapeWithName(lShapeName);
      if (lShape <> nil) then
        UpgradeRunOff(lShape);
    end;
    FBufferRunOffList.Clear;

    for lShapeIdx := 0 to FBufferChannelList.Count - 1 do
    begin
      lShapeName := FBufferChannelList.Strings[lShapeIdx];
      lShape     := FindShapeWithName(lShapeName);
      if (lShape <> nil) then
        UpgradeChannel(lShape);
    end;
    FBufferChannelList.Clear;

    for lShapeIdx := 0 to FBufferIrrBlockList.Count - 1 do
    begin
      lShapeName := FBufferIrrBlockList.Strings[lShapeIdx];
      lShape     := FindShapeWithName(lShapeName);
      if (lShape <> nil) then
        UpgradeIrrBlock(lShape);
    end;
    FBufferIrrBlockList.Clear;

    for lShapeIdx := 0 to FBufferMineList.Count - 1 do
    begin
      lShapeName := FBufferMineList.Strings[lShapeIdx];
      lShape     := FindShapeWithName(lShapeName);
      if (lShape <> nil) then
        UpgradeMine(lShape);
    end;
    FBufferMineList.Clear;

    for lShapeIdx := 0 to FBufferRouteList.Count - 1 do
    begin
      lShapeName := FBufferRouteList.Strings[lShapeIdx];
      lShape     := FindShapeWithName(lShapeName);
      if (lShape <> nil) then
        UpgradeRoute(lShape);
    end;
    FBufferRouteList.Clear;

    for lShapeIdx := 0 to FBufferObsPointList.Count - 1 do
    begin
      lShapeName := FBufferObsPointList.Strings[lShapeIdx];
      lShape     := FindShapeWithName(lShapeName);
      if (lShape <> nil) then
        UpgradeObsPoint(lShape);
    end;
    FBufferObsPointList.Clear;

    for lShapeIdx := 0 to FBufferTextList.Count - 1 do
    begin
      lShapeName := FBufferTextList.Strings[lShapeIdx];
      lShape     := FindShapeWithName(lShapeName);
      if (lShape <> nil) then
        UpgradeText(lShape);
    end;
    FBufferTextList.Clear;

    { Check that all version properties are set }
    for lPageIdx := 1 to ADoc.Pages.Count  do
    begin
      lPage := ADoc.Pages.Item[lPageIdx];
      for lShapeIdx := 1 to lPage.Shapes.Count  do
      begin
        lShape := lPage.Shapes.Item[lShapeIdx];
        if (Pos(PChar('(Hydro)'), lShape.Name) > 0) then
        begin
          if (lShape.CellExists[cVersion, 0] = 0) then
          begin
            lShape.AddNamedRow(visSectionProp, 'Version', visTagDefault);
            lShape.Cells[cVersionLbl].Formula := '"Version"';
            SetShapeVersion(lShape, CThisVersion);
            lShape.Cells[cVersionInv].Formula := '"TRUE"';
            MessageDlg('Added Version ' + lShape.Name, mtInformation, [mbOK], 0);
          end
          else
          begin
            lShape.Cells[cVersionLbl].Formula := '"Version"';
            lVersion := GetShapeVersion(lShape);
            if (lVersion = '""') OR (lVersion = '') then
            begin
              SetShapeVersion(lShape, CThisVersion);
              MessageDlg('Set Version ' + lShape.Name, mtInformation, [mbOK], 0);
            end
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.RefreshDocument (const ADoc : IVDocument);
const OPNAME = 'THydroNVEventHandler.RefreshDocument';
var
  lPageIdx     : integer;
  lShapeIdx    : integer;
  lPage        : IVPage;
  lShape       : IVShape;
//  LElementNo   : integer;
  lProgressDlg : THydroNVProgressDlg;
  lCount       : integer;
//  LShapeType   : THydroNVShapeType;
begin
  try
    FReservoirList.Clear;
    FRunOffList.Clear;
    FChannelList.Clear;
    FIrrBlockList.Clear;
    FMineList.Clear;
    FRouteList.Clear;
    FObsPointList.Clear;
    FOutputTextList.Clear;
    
    FDuplicateReservoirList.Clear;
    FDuplicateRunOffList.Clear;
    FDuplicateChannelList.Clear;
    FDuplicateIrrBlockList.Clear;
    FDuplicateMineList.Clear;
    FDuplicateRouteList.Clear;
    FDuplicateObsPointList.Clear;

    FOutputSelector := nil;

  	lProgressDlg := THydroNVProgressDlg.Create(nil);
    try
      if (lProgressDlg <> nil) then
        lProgressDlg.Show;

      lCount := 0;
      for lPageIdx := 1 to ADoc.Pages.Count  do
      begin
        lPage := ADoc.Pages.Item[lPageIdx];
        lCount := lCount + lPage.Shapes.Count;
      end;
      if (lProgressDlg <> nil) then
        lProgressDlg.SetMaximum(lCount);

      for lPageIdx := 1 to ADoc.Pages.Count  do
      begin
        if(lPageIdx > ADoc.Pages.Count) then Break;
        
        lPage := ADoc.Pages.Item[lPageIdx];
        for lShapeIdx := 1 to lPage.Shapes.Count  do
        begin
          if(lShapeIdx > lPage.Shapes.Count) then Break;

          if (lProgressDlg <> nil) then
            lProgressDlg.UpdateProgress(lShapeIdx);
          lShape := lPage.Shapes.Item[lShapeIdx];

          if (Pos(PChar(mtHydroOutputSelector), lShape.Name) > 0) then
            FOutputSelector := lShape;

          RefreshShape(lShape);
        end;
      end;
      RefreshAllNetworkText;
    finally
      if (lProgressDlg <> nil) then
      begin
        lProgressDlg.Hide;
        FreeAndNil(lProgressDlg);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.RefreshShape (AShape : IVShape);
const OPNAME = 'THydroNVEventHandler.RefreshShape';
var
  LElementNo   : integer;
  LShapeType   : THydroNVShapeType;
begin
  try
    LShapeType := HydroNVShapeType(AShape);
    LElementNo := 0;
    if (NOT (LShapeType in [stHydroNone, stHydroText, stHydroServer, stHydroOutputSelector])) then
      LElementNo := GetShapeElementNo(AShape, 0);

    case LShapeType of
      stHydroReservoir : RefreshReservoir(AShape, LElementNo);
      stHydroRunOff    : RefreshRunOff(AShape, LElementNo);
      stHydroChannel   : RefreshChannel(AShape, LElementNo);
      stHydroIrrBlock  : RefreshIrrBlock(AShape, LElementNo);
      stHydroMine      : RefreshMine(AShape, LElementNo);
      stHydroRoute     :  begin
                            RefreshRoute(AShape, LElementNo);
                            SnapAddedRouteToModules(AShape, LElementNo);
                          end;
      stHydroObsPoint  : RefreshObsPoint(AShape, LElementNo);
      stHydroText      :  begin
                            if (AShape.CellExists[cTextType, 1] <> 0) AND
                               (AShape.CellExists[cNumber, 1] <> 0) then
                            begin
                              RefreshText(AShape);
                            end;
                          end;
      stHydroOutputSelector  : RefreshOutputSelector(AShape);
      else
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.StudyDataHasChanged (AContext   : TChangeContext;
                                                   AFieldName : string;
                                                   AOldValue  : string;
                                                   ANewValue  : string): boolean;
const OPNAME = 'THydroNVEventHandler.StudyDataHasChanged';
var
  LParentShape : IVShape;
  LMasterName  : String;
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (AContext = sdccEdit) then
    begin
      LMasterName  := GetMasterName(AFieldName);
      LParentShape := FindShapeWithPropertyValue(LMasterName, cNumber, AOldValue);
      if (LParentShape <> nil) then
      begin
        RefreshShape(LParentShape);
        RefreshTextShapesWithParent(LParentShape);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.RefreshTextShapesWithParent (AParentShape : IVShape);
const OPNAME = 'THydroNVEventHandler.RefreshTextShapesWithParent';
var
  LShape       : IVShape;
  LShapesList  : TStringList;
  LIndex       : Integer;
  LShapeName   : String;
begin
  try
    LShapesList := TStringList.Create;
    try
      FindTextShapesWithParent(AParentShape.Name, LShapesList);
      for LIndex := 0 to LShapesList.Count - 1 do
      begin
        LShapeName := LShapesList.Strings[LIndex];
        LShape     := FindShapeWithName(LShapeName);
        if (LShape <> nil) then
          RefreshText(LShape);
      end;
    finally
      FreeAndNil(LShapesList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.ShowGIS : boolean;
const OPNAME = 'THydroNVEventHandler.ShowGIS';
      cVisioDrPage = 1;
var
  LIndex          : integer;
  LCurrentPage    : IVPage;
  LGISPage        : IVPage;
  LFileName       : string;
  LImg            : TImage;
  LImgW           : Double;
  LImgH           : Double;
  LShape          : IVShape;
  LElementNo      : integer;
  LXYCoord        : istrings;
  LShapeType      : THydroNVShapeType;
begin
  Result := False;
  try
    if (FDrawing.GISDrawing = 1)  and (FVisioApp.ActivePage <> nil) and (FVisioApp.ActivePage.Background = 0) then
    begin
      if(FGISViewer = nil) then
        CreateGISViewer;
      LCurrentPage :=  FVisioApp.ActivePage;

      // The True parameter indicated that the local file path
      // should be used and not the path contained in the glf

      LFileName := GetAppDataLocalDir + '\Network Diagrams\'+    //ExtractFilePath(ApplicationExeName) + 'Network Diagrams\'+
      ChopCharacters(FAppModules.StudyArea.StudyAreaCode) +
//      '\'+ChopCharacters(FAppModules.StudyArea.SubAreaCode) +
      '\'+ChopCharacters(FAppModules.StudyArea.ScenarioCode) +
      '\'+FAppModules.StudyArea.ScenarioCode + '.glf';
      UpdateGISShapeFilePath(LFileName);
      FGISViewer.Layers_LoadLayout(LFileName,False);
      FGISViewer.Control_RefreshAll;
      Application.ProcessMessages;

//      LHydrologyModel := FAppModules.Model as IHydrologyModel;
//      FGISViewer.Extent_Set(LHydrologyModel.Network.MinLongitude, LHydrologyModel.Network.MaxLatitude, LHydrologyModel.Network.MaxLongitude, LHydrologyModel.Network.MinLatitude);
      FGISViewer.Extent_Set(FAppModules.StudyArea.TopLeftCoord,FAppModules.StudyArea.TopRightCoord, FAppModules.StudyArea.BottomLeftCoord, FAppModules.StudyArea.BottomRightCoord);
      FGISViewer.Visible := True;
      FGISForm.SendToBack;
      FGISForm.Show;
      FGISForm.Hide;

      LFileName := GetAppDataLocalDir+'\Logs\GIS.bmp';  //ExtractFilePath(ApplicationExeName) + 'Logs\GIS.bmp';
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

      FNoRecalcLonLatList.Clear;

      for LIndex := 1 to FVisioApp.ActivePage.Shapes.Count do
      begin
        LShape := FVisioApp.ActivePage.Shapes[LIndex];
        LShapeType := HydroNVShapeType(LShape);
        if (LShapeType in [stHydroReservoir, stHydroChannel, stHydroRunOff, stHydroMine, stHydroIrrBlock]) then
        begin
          LElementNo := GetShapeElementNo(LShape, -1);
          GISPopulateNoRecalcLonLatList(LShape, LElementNo);
          GISSetXYPosFromLonLat(LShape, LElementNo);
        end;
      end;
    end
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.HideGIS: boolean;
const OPNAME = 'THydroNVEventHandler.HideGIS';
begin
  Result := False;
  try
    FNoRecalcLonLatList.Clear;
    FVisioApp.ActivePage.BackPage := '';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.GetGISPage: IVPage;
const OPNAME = 'THydroNVEventHandler.GetGISPage';
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

procedure THydroNVEventHandler.GISSetXYPosFromLonLat(AShape     : IVShape;
                                                     AElementNo : integer);
const OPNAME = 'THydroNVEventHandler.GISSetXYPosFromLonLat';
//function calculate and positions the module's xy position on the screen, according to the lon lat coords
var
  LModule         : IModule;
  lXFactor        : double;
  lYFactor        : double;
  LMapXYCoord     : istrings;
  LHydrologyModel : IHydrologyModel;
  LShapeType      : THydroNVShapeType;
begin
  try
    if (AElementNo = -1) OR (IsShapeDuplicate(AShape)) then
      Exit;
    LShapeType := HydroNVShapeType(AShape);
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    case LShapeType of
      stHydroReservoir : LModule := LHydrologyModel.Network.ReservoirModuleAgent.ReservoirModuleByNumber[AElementNo];
      stHydroChannel   : LModule := LHydrologyModel.Network.ChannelModuleAgent.ChannelModuleByNumber[AElementNo];
      stHydroRunOff    : LModule := LHydrologyModel.Network.RunOffModuleAgent.RunOffModuleByNumber[AElementNo];
      stHydroMine      : LModule := LHydrologyModel.Network.MineModuleAgent.MineModuleByNumber[AElementNo];
      stHydroIrrBlock  : LModule := LHydrologyModel.Network.IrrigationModuleAgent.IrrigationModuleByNumber[AElementNo];
    else
      LModule := nil;
    end;  
    if (LModule <> nil) then
    begin
      if ((LModule.Longitude <> 0) AND (LModule.Latitude <> 0)) then
      begin
        LMapXYCoord := FGISViewer.Map_LonLatToXY(LModule.Longitude, LModule.Latitude);
        if (LMapXYCoord <> nil) AND (LMapXYCoord.Count = 2) then
        begin
          lXFactor := (FPageWidth / FMapWidth) * StrToFloat(LMapXYCoord.Item[0]);
          lYFactor := (FPageHeight / FMapHeight) * StrToFloat(LMapXYCoord.Item[1]);
          if (LXFactor < 0) or (LYFactor < 0) OR
             (StrToIntDef(LMapXYCoord.Item[0], -1) > FMapWidth) or (StrToIntDef(LMapXYCoord.Item[1], -1) > FMapHeight) then
          begin
            SetShapeColour(AShape, visDarkYellow);
          end
          else
          begin
            SetShapePinX(AShape, lXFactor);
            SetShapePinY(AShape, FPageHeight - lYFactor);  //flip it around, as in visio y=0 is in the left bottom corner
          end;
        end;
      end
      else
        SetShapeColour(AShape, visDarkYellow);  //x=0 y=0
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.GISCalculateLonLatOfShape (AShape           : IVShape;
                                                          AElementNo       : integer;
                                                          AGisCalcLonOrLat : TGISCalcLonOrLat);

const OPNAME = 'THydroNVEventHandler.GISCalculateLonLatOfShape';
var
  LModule         : IModule;
  LElementIndex   : Integer;
  LPinX           : Double;
  lPinY           : Double;
  lXFactor        : Double;
  lYFactor        : Double;
  lXCoord         : Double;
  lYCoord         : Double;
  LonLatCoord     : istrings;
  LHydrologyModel : IHydrologyModel;
  LShapeType      : THydroNVShapeType;
begin
  try
    if (AElementNo = -1) OR (IsShapeDuplicate(AShape)) then
      exit;
    LShapeType := HydroNVShapeType(AShape);
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    case LShapeType of
      stHydroReservoir : LModule := LHydrologyModel.Network.ReservoirModuleAgent.ReservoirModuleByNumber[AElementNo];
      stHydroChannel   : LModule := LHydrologyModel.Network.ChannelModuleAgent.ChannelModuleByNumber[AElementNo];
      stHydroRunOff    : LModule := LHydrologyModel.Network.RunOffModuleAgent.RunOffModuleByNumber[AElementNo];
      stHydroMine      : LModule := LHydrologyModel.Network.MineModuleAgent.MineModuleByNumber[AElementNo];
      stHydroIrrBlock  : LModule := LHydrologyModel.Network.IrrigationModuleAgent.IrrigationModuleByNumber[AElementNo];
    else
      LModule := nil;
    end;
    LElementIndex := FNoRecalcLonLatList.IndexOf(IntToStr(AElementNo));
    if (LElementIndex > -1) then
    begin
      FNoRecalcLonLatList.Delete(LElementIndex);
    end
    else
    begin
      if (LModule <> nil) then
      begin
        LXCoord := LModule.Longitude;
        LYCoord := LModule.Latitude;
        if ((FDrawing.GISDrawing = 1) and (FGISViewer <> nil)) then
        begin
          lPinX := GetShapePinX(AShape);
          lPinY := GetShapePinY(AShape);
          lPinY := FPageHeight - lPinY;  //flip it around a, as in visio y=0 is in the left bottom cornerl

          lXFactor := (lPinX / FPageWidth) * FMapWidth;
          lYFactor := (lPinY / FPageHeight) * FMapHeight;

          LonLatCoord := FGISViewer.Map_XYToLonLat(Round(lXFactor), Round(lYFactor));

          LXCoord := StrToFloat(LonLatCoord.Item[0]);
          LYCoord := StrToFloat(LonLatCoord.Item[1]);

          SetShapeColour(AShape, visWhite);
        end;

        if (AGISCalcLonOrLat = gisCalcLon) or (AGISCalcLonOrLat = gisCalcBoth)  then
          LModule.Longitude := LXCoord;

        if (AGISCalcLonOrLat = gisCalcLat) or (AGISCalcLonOrLat = gisCalcBoth)  then
          LModule.Latitude := LYCoord;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.GISConvertLonLatToXY(AGISViewer : TGisViewerX;ALon,ALat: double; var AX,AY:double): boolean;
const OPNAME = 'THydroNVEventHandler.GISConvertLonLatToXY';
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

procedure THydroNVEventHandler.GISPopulateNoRecalcLonLatList (AShape     : IVShape;
                                                              AElementNo : Integer);
const OPNAME = 'THydroNVEventHandler.GISPopulateNoRecalcLonLatList';
var
  lIDx  :  Integer;
begin
  try
    if (AElementNo = -1) OR (IsShapeDuplicate(AShape)) then
      exit;

    lIDx  := FNoRecalcLonLatList.IndexOf(IntToStr(AElementNo));
    while lIDx <> -1 do  //delete existing
    begin
      FNoRecalcLonLatList.Delete(lIDx);
      lIDx  := FNoRecalcLonLatList.IndexOf(IntToStr(AElementNo));
    end;
    FNoRecalcLonLatList.Add(IntToStr(AElementNo));  //x
    FNoRecalcLonLatList.Add(IntToStr(AElementNo));  //y
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.CheckUpgradeReservoir (AShape : IVShape);
const OPNAME = 'THydroNVEventHandler.CheckUpgradeReservoir';
begin
  try
    { Upgrade to 1.1.0 }
    if (AShape.Cells[cVersion].Formula = '"1.0.0"') then
    begin
      FBufferReservoirList.Add(AShape.Name);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.CheckUpgradeRunOff (AShape : IVShape);
const OPNAME = 'THydroNVEventHandler.CheckUpgradeRunOff';
begin
  try
    { Upgrade to 1.1.0 }
    if (AShape.Cells[cVersion].Formula = '"1.0.0"') then
    begin
      FBufferRunOffList.Add(AShape.Name);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.CheckUpgradeChannel (AShape : IVShape);
const OPNAME = 'THydroNVEventHandler.CheckUpgradeChannel';
begin
  try
    { Upgrade to 1.1.0 }
    if (AShape.Cells[cVersion].Formula = '"1.0.0"') then
    begin
      FBufferChannelList.Add(AShape.Name);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.CheckUpgradeIrrBlock (AShape : IVShape);
const OPNAME = 'THydroNVEventHandler.CheckUpgradeIrrBlock';
begin
  try
    { Upgrade to 1.1.0 }
    if (AShape.Cells[cVersion].Formula = '"1.0.0"') then
    begin
      FBufferIrrBlockList.Add(AShape.Name);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.CheckUpgradeMine (AShape : IVShape);
const OPNAME = 'THydroNVEventHandler.CheckUpgradeMine';
begin
  try
    { Upgrade to 1.1.0 }
    if (AShape.Cells[cVersion].Formula = '"1.0.0"') then
    begin
      FBufferMineList.Add(AShape.Name);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.CheckUpgradeRoute (AShape : IVShape);
const OPNAME = 'THydroNVEventHandler.CheckUpgradeRoute';
begin
  try
    { Upgrade to 1.1.0 }
    if (AShape.Cells[cVersion].Formula = '"1.0.0"') then
    begin
      FBufferRouteList.Add(AShape.Name);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.CheckUpgradeObsPoint (AShape : IVShape);
const OPNAME = 'THydroNVEventHandler.CheckUpgradeObsPoint';
begin
  try
    { Upgrade to 1.1.0 }
    if (AShape.Cells[cVersion].Formula = '"1.0.0"') then
    begin
      FBufferObsPointList.Add(AShape.Name);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.CheckUpgradeText (AShape : IVShape);
const OPNAME = 'THydroNVEventHandler.CheckUpgradeText';
begin
  try
    { Upgrade to 1.1.0 }
    if (AShape.Cells[cVersion].Formula = '"1.0.0"') then
    begin
      FBufferTextList.Add(AShape.Name);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.CheckUpgradeOutputSelector (AShape : IVShape);
const OPNAME = 'THydroNVEventHandler.CheckUpgradeOutputSelector';
var
  LVersion : String;
begin
  try
    LVersion := GetShapeVersion(AShape);
    if (LVersion <> CThisVersion) then
      UpgradeOutputSelector(AShape);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.UpgradeReservoir (AShape : IVShape) : IVShape;
const OPNAME = 'THydroNVEventHandler.UpgradeReservoir';
begin
  Result := nil;
  try
    { Upgrade to 1.1.0 }
    if (AShape.Cells[cVersion].Formula = '"1.0.0"') then
      SetShapeVersion(AShape, CThisVersion);
    Result := AShape;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.UpgradeRunOff (AShape : IVShape) : IVShape;
const OPNAME = 'THydroNVEventHandler.UpgradeRunOff';
begin
  Result := nil;
  try
    { Upgrade to 1.1.0 }
    if (AShape.Cells[cVersion].Formula = '"1.0.0"') then
      SetShapeVersion(AShape, CThisVersion);
    Result := AShape;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.UpgradeChannel (AShape : IVShape) : IVShape;
const OPNAME = 'THydroNVEventHandler.UpgradeChannel';
begin
  Result := nil;
  try
    { Upgrade to 1.1.0 }
    if (AShape.Cells[cVersion].Formula = '"1.0.0"') then
      SetShapeVersion(AShape, CThisVersion);
    Result := AShape;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.UpgradeIrrBlock (AShape : IVShape) : IVShape;
const OPNAME = 'THydroNVEventHandler.UpgradeIrrBlock';
begin
  Result := nil;
  try
    { Upgrade to 1.1.0 }
    if (AShape.Cells[cVersion].Formula = '"1.0.0"') then
      SetShapeVersion(AShape, CThisVersion);
    Result := AShape;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.UpgradeMine (AShape : IVShape) : IVShape;
const OPNAME = 'THydroNVEventHandler.UpgradeMine';
begin
  Result := nil;
  try
    { Upgrade to 1.1.0 }
    if (AShape.Cells[cVersion].Formula = '"1.0.0"') then
      SetShapeVersion(AShape, CThisVersion);
    Result := AShape;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.UpgradeRoute (AShape : IVShape) : IVShape;
const OPNAME = 'THydroNVEventHandler.UpgradeRoute';
var
  LRouteNo        : Integer;
  LHydrologyModel : IHydrologyModel;
  LNetworkRoute   : INetworkRoute;
begin
  Result := nil;
  try
    { Upgrade to 1.1.0 }
    if (AShape.Cells[cVersion].Formula = '"1.0.0"') then
    begin
      if (AShape.CellExists[cRouteID, 0] = 0) then
      begin
        LRouteNo := GetShapeElementNo(AShape, 0);
        LHydrologyModel := FAppModules.Model as IHydrologyModel;
        LNetworkRoute   := LHydrologyModel.Network.NetworkRouteAgent.NetworkRouteByRouteNo[LRouteNo];
        AShape.AddNamedRow(visSectionProp, 'RouteID', visTagDefault);
        SetShapeRouteID(AShape, LNetworkRoute.RouteID);
      end;
      SetShapeVersion(AShape, CThisVersion);
    end;
    Result := AShape;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.UpgradeObsPoint (AShape : IVShape) : IVShape;
const OPNAME = 'THydroNVEventHandler.UpgradeObsPoint';
begin
  Result := nil;
  try
    { Upgrade to 1.1.0 }
    if (AShape.Cells[cVersion].Formula = '"1.0.0"') then
      SetShapeVersion(AShape, CThisVersion);
    Result := AShape;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.UpgradeText (AShape : IVShape) : IVShape;
const OPNAME = 'THydroNVEventHandler.UpgradeText';
var
  LRowIndex : Integer;
  LValue    : String;
  LLabel    : String;
  LTextType : THydroNVTextType;
begin
  try
    { Upgrade to 1.1.0 }
    if (AShape.Cells[cVersion].Formula = '"1.0.0"') then
    begin
      if (AShape.CellExists[cOldTextType, 0] <> 0) then
      begin
        LRowIndex := AShape.CellsRowIndex[cOldTextType];
        LValue    := AShape.CellsSRC[visSectionProp, LRowIndex, 0].FormulaU;
        LTextType := THydroNVTextType(StrToInt(UnQuote(LValue)));
        LLabel    := AShape.CellsSRC[visSectionProp, LRowIndex, visCustPropsLabel].Formula;
        AShape.DeleteRow(visSectionProp, LRowIndex);
        AShape.AddNamedRow(visSectionProp, 'TextType', visTagDefault);
        SetShapeHydroNVTextType(AShape, LTextType);
        AShape.AddNamedRow(visSectionProp, 'ElementSubType', visTagDefault);
        SetShapeHydroNVElementSubType(AShape, subNone);
        AShape.AddNamedRow(visSectionProp, 'SectionNo', visTagDefault);
        SetShapeSectionNo(AShape, 0);
      end;
      SetShapeVersion(AShape, CThisVersion);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.UpgradeOutputSelector (AShape : IVShape) : IVShape;
const OPNAME = 'THydroNVEventHandler.UpgradeOutputSelector';
begin
  Result := nil;
  try
    { Upgrade to 1.1.0 }
    if (AShape.Cells[cVersion].Formula = '"1.0.0"') then
      SetShapeVersion(AShape, CThisVersion);
    Result := AShape;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.IsShapeDuplicate (AShape : IVShape) : Boolean;
const OPNAME = 'THydroNVEventHandler.IsShapeDuplicate';
begin
  Result := FALSE;
  try
//    Result := UnQuote(AShape.Cells[cDuplicate].Formula) <> '0';
    Result := GetShapeDuplicateNoAsString(AShape) <> '0';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.RefreshReservoir (AShape : IVShape; AElementNo : integer);
const OPNAME = 'THydroNVEventHandler.RefreshReservoir';
var
  LReservoir      : IReservoirModule;
  LResName        : string;
  LHydrologyModel : IHydrologyModel;
begin
  try
    if (AShape = nil) or (AElementNo = 0) or (AElementNo = -1) then Exit;
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    LReservoir := LHydrologyModel.Network.ReservoirModuleAgent.ReservoirModuleByNumber[AElementNo];
    if (lReservoir = nil) then
      SetShapeColour(AShape, visRed)
    else
    begin
      FVisioApp.ActiveWindow.Windows.ItemFromID[visWinIDCustProp].Visible := TRUE;
      lResName := lReservoir.ReservoirName;
      SetShapeModuleID(AShape, lReservoir.ModuleID);
      SetShapeNetworkSequence(AShape, lReservoir.NetworkSequence);
      SetShapeActionProcessVNVSpecial(AShape);
      if (lReservoir.Active <> 'Y') then
        SetShapeLinePattern(AShape, 10)
      else
        SetShapeLinePattern(AShape, 1);
      if (IsShapeDuplicate(AShape)) then
      begin
        AddElementToDuplicateList(FDuplicateReservoirList, IntToStr(AElementNo));
        SetShapeName(AShape, lResName + ' (Copy ' + GetShapeDuplicateNoAsString(AShape) + ')');
        SetShapeLinePattern(AShape, 23);
        SetShapeColour(AShape, visGray20);
      end
      else
      begin
        if (FReservoirList.IndexOf(IntToStr(AElementNo)) < 0) then
          FReservoirList.Add(IntToStr(AElementNo));
        SetShapeName(AShape, lResName);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.RefreshRunOff (AShape : IVShape; AElementNo : integer);
const OPNAME = 'THydroNVEventHandler.RefreshRunOff';
var
  LRunOff         : IRunOffModule;
  LRunOffName     : string;
  LHydrologyModel : IHydrologyModel;
begin
  try
    if(AShape = nil) or (AElementNo = 0) or (AElementNo = -1) then Exit;
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    lRunOff := LHydrologyModel.Network.RunOffModuleAgent.RunOffModuleByNumber[AElementNo];
    if (lRunOff = nil) then
      SetShapeColour(AShape, visRed)
    else
    begin
      FVisioApp.ActiveWindow.Windows.ItemFromID[visWinIDCustProp].Visible := TRUE;
      lRunOffName := lRunOff.RunOffName;
      SetShapeModuleID(AShape, LRunOff.ModuleID);
      SetShapeNetworkSequence(AShape, lRunOff.NetworkSequence);
      SetShapeActionProcessVNVSpecial(AShape);
      if (LRunOff.Active <> 'Y') then
        SetShapeLinePattern(AShape, 10)
      else
        SetShapeLinePattern(AShape, 1);
      if (IsShapeDuplicate(AShape)) then
      begin
        AddElementToDuplicateList(FDuplicateRunOffList, IntToStr(AElementNo));
        SetShapeName(AShape, lRunOffName + ' (Copy ' + GetShapeDuplicateNoAsString(AShape) + ')');
        SetShapeLinePattern(AShape, 23);
        SetShapeColour(AShape, visGray20);
      end
      else
      begin
        if (FRunOffList.IndexOf(IntToStr(AElementNo)) < 0) then
          FRunOffList.Add(IntToStr(AElementNo));
        SetShapeName(AShape, lRunOffName);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.RefreshChannel (AShape : IVShape; AElementNo : integer);
const OPNAME = 'THydroNVEventHandler.RefreshChannel';
var
  LChannel        : IChannelModule;
  LChannelName    : string;
  LHydrologyModel : IHydrologyModel;
begin
  try
    if(AShape = nil) or (AElementNo = 0) or (AElementNo = -1) then Exit;
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    lChannel := LHydrologyModel.Network.ChannelModuleAgent.ChannelModuleByNumber[AElementNo];
    if (lChannel = nil) then
      SetShapeColour(AShape, visRed)
    else
    begin
      FVisioApp.ActiveWindow.Windows.ItemFromID[visWinIDCustProp].Visible := TRUE;
      lChannelName := lChannel.ChannelName;
      SetShapeModuleID(AShape, LChannel.ModuleID);
      SetShapeNetworkSequence(AShape, lChannel.NetworkSequence);
      SetShapeActionProcessVNVSpecial(AShape);
      if (LChannel.Active <> 'Y') then
        SetShapeLinePattern(AShape, 10)
      else
        SetShapeLinePattern(AShape, 1);
      if (IsShapeDuplicate(AShape)) then
      begin
        AddElementToDuplicateList(FDuplicateChannelList, IntToStr(AElementNo));
        SetShapeName(AShape, lChannelName + ' (Copy ' + GetShapeDuplicateNoAsString(AShape) + ')');
        SetShapeLinePattern(AShape, 23);
        SetShapeColour(AShape, visGray20);
      end
      else
      begin
        if (FChannelList.IndexOf(IntToStr(AElementNo)) < 0) then
          FChannelList.Add(IntToStr(AElementNo));
        SetShapeName(AShape, lChannelName);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.RefreshIrrBlock (AShape : IVShape; AElementNo : integer);
const OPNAME = 'THydroNVEventHandler.RefreshIrrBlock';
var
  lIrrBlock       : IIrrigationModule;
  lIrrBlockName   : string;
  LHydrologyModel : IHydrologyModel;
begin
  try
    if(AShape = nil) or (AElementNo = 0) or (AElementNo = -1) then Exit;
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    lIrrBlock := LHydrologyModel.Network.IrrigationModuleAgent.IrrigationModuleByNumber[AElementNo];
    if (lIrrBlock = nil) then
      SetShapeColour(AShape, visRed)
    else
    begin
      FVisioApp.ActiveWindow.Windows.ItemFromID[visWinIDCustProp].Visible := TRUE;
      lIrrBlockName := lIrrBlock.IrrigationName;
      SetShapeModuleID(AShape, lIrrBlock.ModuleID);
      SetShapeNetworkSequence(AShape, lIrrBlock.NetworkSequence);
      SetShapeActionProcessVNVSpecial(AShape);
      if (lIrrBlock.Active <> 'Y') then
        SetShapeLinePattern(AShape, 10)
      else
        SetShapeLinePattern(AShape, 1);
      if (IsShapeDuplicate(AShape)) then
      begin
        AddElementToDuplicateList(FDuplicateIrrBlockList, IntToStr(AElementNo));
        SetShapeName(AShape, lIrrBlockName + ' (Copy ' + GetShapeDuplicateNoAsString(AShape) + ')');
        SetShapeLinePattern(AShape, 23);
        SetShapeColour(AShape, visGray20);
      end
      else
      begin
        if (FIrrBlockList.IndexOf(IntToStr(AElementNo)) < 0) then
          FIrrBlockList.Add(IntToStr(AElementNo));
        SetShapeName(AShape, lIrrBlockName);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.RefreshMine (AShape : IVShape; AElementNo : integer);
const OPNAME = 'THydroNVEventHandler.RefreshMine';
var
  lMine           : IMineModule;
  lMineName       : string;
  LHydrologyModel : IHydrologyModel;
begin
  try
    if(AShape = nil) or (AElementNo = 0) or (AElementNo = -1) then Exit;
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    lMine := LHydrologyModel.Network.MineModuleAgent.MineModuleByNumber[AElementNo];
    if (lMine = nil) then
      SetShapeColour(AShape, visRed)
    else
    begin
      FVisioApp.ActiveWindow.Windows.ItemFromID[visWinIDCustProp].Visible := TRUE;
      lMineName := lMine.MineName;
      SetShapeModuleID(AShape, lMine.ModuleID);
      SetShapeNetworkSequence(AShape, lMine.NetworkSequence);
      SetShapeActionProcessVNVSpecial(AShape);
      if (lMine.Active <> 'Y') then
        SetShapeLinePattern(AShape, 10)
      else
        SetShapeLinePattern(AShape, 1);
      if (IsShapeDuplicate(AShape)) then
      begin
        AddElementToDuplicateList(FDuplicateMineList, IntToStr(AElementNo));
        SetShapeName(AShape, lMineName + ' (Copy ' + GetShapeDuplicateNoAsString(AShape) + ')');
        SetShapeLinePattern(AShape, 23);
        SetShapeColour(AShape, visGray20);
      end
      else
      begin
        if (FMineList.IndexOf(IntToStr(AElementNo)) < 0) then
          FMineList.Add(IntToStr(AElementNo));
        SetShapeName(AShape, lMineName);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.RefreshRoute (AShape : IVShape; AElementNo : integer);
const OPNAME = 'THydroNVEventHandler.RefreshRoute';
var
  LRoute            : INetworkRoute;
  LRouteName        : string;
  LSinkModuleID     : Integer;
  LSourceModuleID   : Integer;
  LSinkModuleText   : String;
  LSourceModuleText : String;
  LHydrologyModel   : IHydrologyModel;
begin
  try
    if(AShape = nil) or (AElementNo = 0) or (AElementNo = -1) then Exit;
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    LRoute := LHydrologyModel.Network.NetworkRouteAgent.NetworkRouteByRouteNo[AElementNo];
    if (LRoute = nil) then
      SetShapeColour(AShape, visRed)
    else
    begin
      FVisioApp.ActiveWindow.Windows.ItemFromID[visWinIDCustProp].Visible := TRUE;
      LRouteName := 'Route ' + IntToStr(LRoute.RouteNo);
      if (IsShapeDuplicate(AShape)) then
      begin
        AddElementToDuplicateList(FDuplicateRouteList, IntToStr(AElementNo));
        SetShapeName(AShape, LRouteName + ' (Copy ' + GetShapeDuplicateNoAsString(AShape) + ')');
        SetShapeLinePattern(AShape, 23);
        SetShapeColour(AShape, visGray20);
      end
      else
      begin
        if (FRouteList.IndexOf(IntToStr(AElementNo)) < 0) then
          FRouteList.Add(IntToStr(AElementNo));
        SetShapeName(AShape, LRouteName);
      end;
//      LSinkSeqNo      := LRoute.SinkModuleNo;
//      LSinkModuleNo   := LHydrologyModel.Network.ModuleTextBySequenceNumber[LSinkSeqNo];
//      LSourceSeqNo    := LRoute.SourceModuleNo;
//      LSourceModuleNo := LHydrologyModel.Network.ModuleTextBySequenceNumber[LSourceSeqNo];
      LSinkModuleID     := LRoute.SinkModuleID;
      LSinkModuleText   := LHydrologyModel.Network.ModuleTextByID[LSinkModuleID];
      LSourceModuleID   := LRoute.SourceModuleID;
      LSourceModuleText := LHydrologyModel.Network.ModuleTextByID[LSourceModuleID];

      SetRouteShapeSink(AShape, LSinkModuleText);
      SetRouteShapeSource(AShape, LSourceModuleText);
      SetShapeActionProcessVNVSpecial(AShape);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.RefreshObsPoint (AShape : IVShape; AElementNo : integer);
const OPNAME = 'THydroNVEventHandler.RefreshObsPoint';
var
  LObsPoint       : IObservationPoint;
  LObsPointName   : string;
  LFlowFileName   : string;
  LHydrologyModel : IHydrologyModel;
begin
  try
    if (AShape = nil) or (AElementNo = 0) or (AElementNo = -1) then Exit;
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    LObsPoint := LHydrologyModel.Network.ObservationPointAgent.ObservationPointByRouteNo[AElementNo];
    if (LObsPoint = nil) then
      SetShapeColour(AShape, visRed)
    else
    begin
      FVisioApp.ActiveWindow.Windows.ItemFromID[visWinIDCustProp].Visible := TRUE;
      LObsPointName := LObsPoint.Name;
      LFlowFileName := LObsPoint.FlowDataFileName;
      if (IsShapeDuplicate(AShape)) then
      begin
        AddElementToDuplicateList(FDuplicateObsPointList, IntToStr(AElementNo));
        SetShapeName(AShape, LObsPointName + ' (Copy ' + GetShapeDuplicateNoAsString(AShape) + ')');
      end
      else
      begin
        if (FObsPointList.IndexOf(IntToStr(AElementNo)) < 0) then
          FObsPointList.Add(IntToStr(AElementNo));
        SetShapeName(AShape, LObsPointName);
      end;
      AShape.Cells[cFlowFile].Formula := '"' + LFlowFileName + '"';
      SetShapeActionProcessVNVSpecial(AShape);
      SetShapeElementNo(AShape, AElementNo);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.RefreshOutputSelector (AShape : IVShape);
const OPNAME = 'THydroNVEventHandler.RefreshOutputSelector';
var
  lShapeIdx        : integer;
  lSubShape        : IVShape;
  lSubType         : string;
begin
  try
    if (NOT FIntervalSet) then
    begin
      FInterval := StrToIntDef(UnQuote(AShape.Cells[CInterval].Formula),0);
      FIntervalSet := TRUE;
    end;

    AShape.Cells[CInterval].Formula  := '"' + IntToStr(FInterval) + '"';
    for lShapeIdx := 1 to AShape.Shapes.Count do
    begin
      lSubShape := AShape.Shapes.Item[lShapeIdx];
      if (lSubShape.CellExists[cShapeType, 0] <> 0) then
      begin
        lSubType  := UnQuote(lSubShape.Cells[cShapeType].Formula);
        if (lSubType = 'Interval') then
          lSubShape.Text := IntToStr(FInterval);
      end;
    end;
    RefreshAllOutputText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.RefreshAllOutputText;
const OPNAME = 'THydroNVEventHandler.RefreshAllOutputText';
var
  LIndex      : Integer;
  LShapeName  : String;
  LShape      : IVShape;
begin
  try
    for LIndex := 0 to FOutputTextList.Count - 1 do
    begin
      LShapeName := FOutputTextList.Strings[LIndex];
      LShape     := FindShapeWithName(LShapeName);
      if (LShape <> nil) then
        RefreshText(LShape);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.RefreshText (AShape : IVShape);
const OPNAME = 'THydroNVEventHandler.RefreshText';
var
  LTextType         : THydroNVTextType;
  LParentName       : string;
  LParentShape      : IVShape;
  LElementNo        : Integer;
  LElementSubType   : THydroNVElementSubType;
  LSectionNo        : Integer;
  LParentShapeType  : THydroNVShapeType;
  LLabel            : String;
  LElementID        : Integer;
  LValue            : Double;
begin
  try
    LParentName := '';
    if (AShape.CellExists[cNumber, 0] = -1) AND
       (AShape.CellExists[cParent, 0] = -1) AND
       (AShape.CellExists[cTextType, 0] = -1) then
    begin
      LElementNo       := GetShapeElementNo(AShape, 0);
      LParentName      := GetShapeParent(AShape);
      LTextType        := GetShapeHydroNVTextType(AShape);
      LElementSubType  := GetShapeHydroNVElementSubType(AShape);
      LSectionNo       := GetShapeSectionNo(AShape);
      LParentShape     := FindShapeWithName(LParentName);
      LParentShapeType := HydroNVShapeType(LParentShape);
      if (LParentShapeType = stHydroRoute) then
        LElementID     := GetShapeRouteID(LParentShape)
      else if (LParentShapeType = stHydroObsPoint) then
        LElementID     := 0
      else
        LElementID     := GetShapeModuleID(LParentShape);

      if (LTextType in OutputTextTypeSet) then
        if (FOutputTextList.IndexOf(AShape.Name) < 0) then
          FOutputTextList.Add(AShape.Name);

      if (LParentShape <> nil) then
      begin
        if (LTextType = nvtName) then
          AShape.Text := GetShapeName(lParentShape)
        else if (LTextType = nvtObsFlowFile) then
          RefreshObsFlowFileText(AShape, LElementNo)
        else if (LTextType in OutputTextTypeSet) then
        begin
          LLabel := GetOutputTextLabel(LParentShapeType, LElementNo, LTextType, LElementSubType, LSectionNo);
          if (GetOutputValue(LParentShapeType, LElementID, LTextType, LElementSubType, LSectionNo, LValue)) then
            AShape.Text := LLabel + ' =' + Format('%6.2f', [LValue])
          else
            AShape.Text := LLabel + ' [No value]';
        end;
      end
      else if (LParentName = mtHydroNetwork) then
      begin
        RefreshNetworkText(AShape);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.GetOutputTextLabel (AParentShapeType : THydroNVShapeType;
                                                  AElementNo       : Integer;
                                                  ATextType        : THydroNVTextType;
                                                  AElementSubType  : THydroNVElementSubType;
                                                  ASectionNo       : Integer) : String;
const OPNAME = 'THydroNVEventHandler.GetOutputTextLabel';
begin
  Result := '';
  try
    case AParentShapeType of
      stHydroReservoir : Result := 'RV' + IntToStr(AElementNo) + ': ' + TextTypeToStr(ATextType);
      stHydroRunOff    : Result := 'RU' + IntToStr(AElementNo) + ': ' + TextTypeToStr(ATextType);
      stHydroChannel   : Result := 'CR' + IntToStr(AElementNo) + ': ' + TextTypeToStr(ATextType);
      stHydroRoute     : Result := 'RQ' + IntToStr(AElementNo) + ': ' + TextTypeToStr(ATextType);
      stHydroMine      :
        begin
          case AElementSubType of
            subPlantArea   : Result := 'MM' + IntToStr(AElementNo) + ': ' + TextTypeToStr(ATextType);
            subOpencast    : Result := 'MM' + IntToStr(AElementNo) + '- OC' + IntToStr(ASectionNo) + ': ' + TextTypeToStr(ATextType);
            subUnderground : Result := 'MM' + IntToStr(AElementNo) + '- UG' + IntToStr(ASectionNo) + ': ' + TextTypeToStr(ATextType);
            subSlurryDump  : Result := 'MM' + IntToStr(AElementNo) + '- SD' + IntToStr(ASectionNo) + ': ' + TextTypeToStr(ATextType);
          else
            Result := 'MM' + IntToStr(AElementNo) + ': ' + TextTypeToStr(ATextType);
          end;
        end;
    else
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.GetOutputValue (AParentShapeType : THydroNVShapeType;
                                              AElementID       : Integer;
                                              ATextType        : THydroNVTextType;
                                              AElementSubType  : THydroNVElementSubType;
                                              ASectionNo       : Integer;
                                              var AValue       : Double) : Boolean;
const OPNAME = 'THydroNVEventHandler.GetOutputValue';
var
  LHydrologyModel : IHydrologyModel;
  LHydroOutput    : IHydroOutput;
begin
  Result := FALSE;
  try
    AValue := 0;
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    case AParentShapeType of
      stHydroReservoir :
        LHydroOutput := LHydrologyModel.Network.HydroOutputAgent.HydroOutput['RV', '', AElementID, ASectionNo, Ord(ATextType)];
      stHydroRunOff    :
        LHydroOutput := LHydrologyModel.Network.HydroOutputAgent.HydroOutput['RU', '', AElementID, ASectionNo, Ord(ATextType)];
      stHydroChannel   :
        LHydroOutput := LHydrologyModel.Network.HydroOutputAgent.HydroOutput['CR', '', AElementID, ASectionNo, Ord(ATextType)];
      stHydroRoute     :
        LHydroOutput := LHydrologyModel.Network.HydroOutputAgent.HydroOutput['RQ', '', AElementID, ASectionNo, Ord(ATextType)];
      stHydroMine      :
        begin
          case AElementSubType of
            subPlantArea   :
              LHydroOutput := LHydrologyModel.Network.HydroOutputAgent.HydroOutput['MM', '', AElementID, ASectionNo, Ord(ATextType)];
            subOpencast    :
              LHydroOutput := LHydrologyModel.Network.HydroOutputAgent.HydroOutput['MM', 'OC', AElementID, ASectionNo, Ord(ATextType)];
            subUnderground :
              LHydroOutput := LHydrologyModel.Network.HydroOutputAgent.HydroOutput['MM', 'UG', AElementID, ASectionNo, Ord(ATextType)];
            subSlurryDump  :
              LHydroOutput := LHydrologyModel.Network.HydroOutputAgent.HydroOutput['MM', 'SD', AElementID, ASectionNo, Ord(ATextType)];
          else
          end;
        end;
    else
    end;
    if (LHydroOutput <> nil) then
    begin
      AValue := LHydroOutput.Data[FInterval];
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.RefreshAllNetworkText;
const OPNAME = 'THydroNVEventHandler.RefreshAllNetworkText';
var
  LShapesList : TStringList;
  LIndex      : Integer;
  LShapeName  : String;
  LShape      : IVShape;
begin
  try
    LShapesList := TStringList.Create;
    try
      FindTextShapesWithParent(mtHydroNetwork, LShapesList);
      for LIndex := 0 to LShapesList.Count - 1 do
      begin
        LShapeName := LShapesList.Strings[LIndex];
        LShape     := FindShapeWithName(LShapeName);
        if (LShape <> nil) then
          RefreshNetworkText(LShape);
      end;
    finally
      FreeAndNil(LShapesList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.RefreshNetworkText (AShape : IVShape);
const OPNAME = 'THydroNVEventHandler.RefreshNetworkText';
var
  LNetwork        : INetwork;
  LHydrologyModel : IHydrologyModel;
  LDuplicateStr   : String;
  LCompleteStr    : String;
  LTextType       : THydroNVTextType;
  LElementNoStr   : String;
  LIndex          : Integer;
  LCount          : Integer;
  LCountIndex     : Integer;
begin
  try
    if (AShape = nil) then Exit;
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    LNetwork        := LHydrologyModel.Network;
    LTextType       := GetShapeHydroNVTextType(AShape);
    if (LTextType = nvtName) then
      AShape.Text := 'NETWORK: ' + LNetwork.NetworkCode
    else if (LTextType = nvtDupCounts) then
    begin
      LDuplicateStr := 'Count of DUPLICATE Network Elements: ';
      if (FDuplicateReservoirList.Count > 0) then
        LDuplicateStr := LDuplicateStr + 'Reservoirs (' + IntToStr(TotalElementsInDuplicateList(FDuplicateReservoirList)) + '), ';
      if (FDuplicateRunOffList.Count > 0) then
        LDuplicateStr := LDuplicateStr + 'RunOffs (' + IntToStr(TotalElementsInDuplicateList(FDuplicateRunOffList)) + '), ';
      if (FDuplicateChannelList.Count > 0) then
        LDuplicateStr := LDuplicateStr + 'Channels (' + IntToStr(TotalElementsInDuplicateList(FDuplicateChannelList)) + '), ';
      if (FDuplicateIrrBlockList.Count > 0) then
        LDuplicateStr := LDuplicateStr + 'Irrigation Blocks (' + IntToStr(TotalElementsInDuplicateList(FDuplicateIrrBlockList)) + '), ';
      if (FDuplicateMineList.Count > 0) then
        LDuplicateStr := LDuplicateStr + 'Mines (' + IntToStr(TotalElementsInDuplicateList(FDuplicateMineList)) + '), ';
      if (FDuplicateRouteList.Count > 0) then
        LDuplicateStr := LDuplicateStr + 'Network Routes (' + IntToStr(TotalElementsInDuplicateList(FDuplicateRouteList)) + '), ';
      if (FDuplicateObsPointList.Count > 0) then
        LDuplicateStr := LDuplicateStr + 'Observation Points (' + IntToStr(TotalElementsInDuplicateList(FDuplicateObsPointList)) + '), ';
      AShape.Text := LDuplicateStr;
    end
    else if (LTextType = nvtDupElements) then
    begin
      LDuplicateStr := 'DUPLICATE Network Elements: ';
      LIndex := 0;
      while (LIndex < FDuplicateReservoirList.Count) do
      begin
        LElementNoStr := FDuplicateReservoirList.Names[LIndex];
        LCount        := StrToInt(FDuplicateReservoirList.ValueFromIndex[LIndex]);
        for LCountIndex := 1 to LCount do
          LDuplicateStr := LDuplicateStr + 'RV' + LElementNoStr + ' (Copy ' + IntToStr(LCountIndex) + '), ';
        LIndex := LIndex + 1;
      end;
      LIndex := 0;
      while (LIndex < FDuplicateRunOffList.Count) do
      begin
        LElementNoStr := FDuplicateRunOffList.Names[LIndex];
        LCount        := StrToInt(FDuplicateRunOffList.ValueFromIndex[LIndex]);
        for LCountIndex := 1 to LCount do
          LDuplicateStr := LDuplicateStr + 'RU' + LElementNoStr + ' (Copy ' + IntToStr(LCountIndex) + '), ';
        LIndex := LIndex + 1;
      end;
      LIndex := 0;
      while (LIndex < FDuplicateChannelList.Count) do
      begin
        LElementNoStr := FDuplicateChannelList.Names[LIndex];
        LCount        := StrToInt(FDuplicateChannelList.ValueFromIndex[LIndex]);
        for LCountIndex := 1 to LCount do
          LDuplicateStr := LDuplicateStr + 'CR' + LElementNoStr + ' (Copy ' + IntToStr(LCountIndex) + '), ';
        LIndex := LIndex + 1;
      end;
      for LIndex := 0 to FDuplicateIrrBlockList.Count - 1 do
      begin
        LElementNoStr := FDuplicateIrrBlockList.Names[LIndex];
        LCount        := StrToInt(FDuplicateIrrBlockList.ValueFromIndex[LIndex]);
        for LCountIndex := 1 to LCount do
          LDuplicateStr := LDuplicateStr + 'RR' + LElementNoStr + ' (Copy ' + IntToStr(LCountIndex) + '), ';
      end;
      for LIndex := 0 to FDuplicateMineList.Count - 1 do
      begin
        LElementNoStr := FDuplicateMineList.Names[LIndex];
        LCount        := StrToInt(FDuplicateMineList.ValueFromIndex[LIndex]);
        for LCountIndex := 1 to LCount do
          LDuplicateStr := LDuplicateStr + 'MM' + LElementNoStr + ' (Copy ' + IntToStr(LCountIndex) + '), ';
      end;
      for LIndex := 0 to FDuplicateRouteList.Count - 1 do
      begin
        LElementNoStr := FDuplicateRouteList.Names[LIndex];
        LCount        := StrToInt(FDuplicateRouteList.ValueFromIndex[LIndex]);
        for LCountIndex := 1 to LCount do
          LDuplicateStr := LDuplicateStr + 'Route ' + LElementNoStr + ' (Copy ' + IntToStr(LCountIndex) + '), ';
      end;
      for LIndex := 0 to FDuplicateObsPointList.Count - 1 do
      begin
        LElementNoStr := FDuplicateObsPointList.Names[LIndex];
        LCount        := StrToInt(FDuplicateObsPointList.ValueFromIndex[LIndex]);
        for LCountIndex := 1 to LCount do
          LDuplicateStr := LDuplicateStr + 'Obs ' + LElementNoStr + ' (Copy ' + IntToStr(LCountIndex) + '), ';
      end;

      AShape.Text := LDuplicateStr;
    end
    else if (LTextType = nvtOutCounts) then
    begin
      LCompleteStr := 'Count of OUTSTANDING Network Elements: ';
      if ((LHydrologyModel.Network.ReservoirModuleAgent.ReservoirModuleCount > 0) AND
          (LHydrologyModel.Network.ReservoirModuleAgent.ReservoirModuleCount <> FReservoirList.Count)) then
        LCompleteStr := LCompleteStr + 'Reservoirs (' +
                        IntToStr(LHydrologyModel.Network.ReservoirModuleAgent.ReservoirModuleCount - FReservoirList.Count) + '), ';
      if ((LHydrologyModel.Network.RunOffModuleAgent.RunOffModuleCount > 0) AND
          (LHydrologyModel.Network.RunOffModuleAgent.RunOffModuleCount <> FRunOffList.Count)) then
        LCompleteStr := LCompleteStr + 'RunOffs (' +
                        IntToStr(LHydrologyModel.Network.RunOffModuleAgent.RunOffModuleCount - FRunOffList.Count) + '), ';
      if ((LHydrologyModel.Network.ChannelModuleAgent.ChannelModuleCount > 0) AND
          (LHydrologyModel.Network.ChannelModuleAgent.ChannelModuleCount <> FChannelList.Count)) then
        LCompleteStr := LCompleteStr + 'Channels (' +
                        IntToStr(LHydrologyModel.Network.ChannelModuleAgent.ChannelModuleCount - FChannelList.Count) + '), ';
      if ((LHydrologyModel.Network.IrrigationModuleAgent.IrrigationModuleCount > 0) AND
          (LHydrologyModel.Network.IrrigationModuleAgent.IrrigationModuleCount <> FIrrBlockList.Count)) then
        LCompleteStr := LCompleteStr + 'Irrigation Blocks (' +
                        IntToStr(LHydrologyModel.Network.IrrigationModuleAgent.IrrigationModuleCount - FIrrBlockList.Count) + '), ';
      if ((LHydrologyModel.Network.MineModuleAgent.MineModuleCount > 0) AND
          (LHydrologyModel.Network.MineModuleAgent.MineModuleCount <> FMineList.Count)) then
        LCompleteStr := LCompleteStr + 'Mines (' +
                        IntToStr(LHydrologyModel.Network.MineModuleAgent.MineModuleCount - FMineList.Count) + '), ';
      if ((LHydrologyModel.Network.NetworkRouteAgent.NetworkRouteCount > 0) AND
          (LHydrologyModel.Network.NetworkRouteAgent.NetworkRouteCount <> FRouteList.Count)) then
        LCompleteStr := LCompleteStr + 'Network Routes (' +
                        IntToStr(LHydrologyModel.Network.NetworkRouteAgent.NetworkRouteCount - FRouteList.Count) + '), ';
      if ((LHydrologyModel.Network.ObservationPointAgent.ObservationPointCount > 0) AND
          (LHydrologyModel.Network.ObservationPointAgent.ObservationPointCount <> FObsPointList.Count)) then
        LCompleteStr := LCompleteStr + 'Observation Points (' +
                        IntToStr(LHydrologyModel.Network.ObservationPointAgent.ObservationPointCount - FObsPointList.Count) + '), ';
      AShape.Text := LCompleteStr;
    end
    else if (LTextType = nvtOutElements) then
    begin
      LCompleteStr := 'OUTSTANDING Network Elements: ';
      if (LHydrologyModel.Network.ReservoirModuleAgent.ReservoirModuleCount <> FReservoirList.Count) then
      begin
        for LIndex := 0 to LHydrologyModel.Network.ReservoirModuleAgent.ReservoirModuleCount - 1 do
        begin
          LElementNoStr := IntToStr(LHydrologyModel.Network.ReservoirModuleAgent.ReservoirModuleByIndex[LIndex].ModuleNumber);
          if (FReservoirList.IndexOf(LElementNoStr) < 0) then
            LCompleteStr := LCompleteStr + 'RV' + LElementNoStr + ', ';
        end;
      end;
      if (LHydrologyModel.Network.RunOffModuleAgent.RunOffModuleCount <> FRunOffList.Count) then
      begin
        for LIndex := 0 to LHydrologyModel.Network.RunOffModuleAgent.RunOffModuleCount - 1 do
        begin
          LElementNoStr := IntToStr(LHydrologyModel.Network.RunOffModuleAgent.RunOffModuleByIndex[LIndex].ModuleNumber);
          if (FRunOffList.IndexOf(LElementNoStr) < 0) then
            LCompleteStr := LCompleteStr + 'RU' + LElementNoStr + ', ';
        end;
      end;
      if (LHydrologyModel.Network.ChannelModuleAgent.ChannelModuleCount <> FChannelList.Count) then
      begin
        for LIndex := 0 to LHydrologyModel.Network.ChannelModuleAgent.ChannelModuleCount - 1 do
        begin
          LElementNoStr := IntToStr(LHydrologyModel.Network.ChannelModuleAgent.ChannelModuleByIndex[LIndex].ModuleNumber);
          if (FChannelList.IndexOf(LElementNoStr) < 0) then
            LCompleteStr := LCompleteStr + 'CR' + LElementNoStr + ', ';
        end;
      end;
      if (LHydrologyModel.Network.IrrigationModuleAgent.IrrigationModuleCount <> FIrrBlockList.Count) then
      begin
        for LIndex := 0 to LHydrologyModel.Network.IrrigationModuleAgent.IrrigationModuleCount - 1 do
        begin
          LElementNoStr := IntToStr(LHydrologyModel.Network.IrrigationModuleAgent.IrrigationModuleByIndex[LIndex].ModuleNumber);
          if (FIrrBlockList.IndexOf(LElementNoStr) < 0) then
            LCompleteStr := LCompleteStr + 'RR' + LElementNoStr + ', ';
        end;
      end;
      if (LHydrologyModel.Network.MineModuleAgent.MineModuleCount <> FMineList.Count) then
      begin
        for LIndex := 0 to LHydrologyModel.Network.MineModuleAgent.MineModuleCount - 1 do
        begin
          LElementNoStr := IntToStr(LHydrologyModel.Network.MineModuleAgent.MineModuleByIndex[LIndex].ModuleNumber);
          if (FMineList.IndexOf(LElementNoStr) < 0) then
            LCompleteStr := LCompleteStr + 'MM' + LElementNoStr + ', ';
        end;
      end;
      if (LHydrologyModel.Network.NetworkRouteAgent.NetworkRouteCount <> FRouteList.Count) then
      begin
        for LIndex := 0 to LHydrologyModel.Network.NetworkRouteAgent.NetworkRouteCount - 1 do
        begin
          LElementNoStr := IntToStr(LHydrologyModel.Network.NetworkRouteAgent.NetworkRouteByIndex[LIndex].RouteNo);
          if (FRouteList.IndexOf(LElementNoStr) < 0) then
            LCompleteStr := LCompleteStr + 'Route ' + LElementNoStr + ', ';
        end;
      end;
      if (LHydrologyModel.Network.ObservationPointAgent.ObservationPointCount <> FObsPointList.Count) then
      begin
        for LIndex := 0 to LHydrologyModel.Network.ObservationPointAgent.ObservationPointCount - 1 do
        begin
          LElementNoStr := IntToStr(LHydrologyModel.Network.ObservationPointAgent.ObservationPointByIndex[LIndex].RouteNo);
          if (FObsPointList.IndexOf(LElementNoStr) < 0) then
            LCompleteStr := LCompleteStr + 'Obs ' + LElementNoStr + ', ';
        end;
      end;

      AShape.Text := LCompleteStr;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.RefreshObsFlowFileText (AShape     : IVShape;
                                                       AElementNo : integer);
const OPNAME = 'THydroNVEventHandler.RefreshObsFlowFileText';
var
  LObsPoint       : IObservationPoint;
  LHydrologyModel : IHydrologyModel;
begin
  try
    if (AShape = nil) or (AElementNo = 0) then Exit;
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    LObsPoint := LHydrologyModel.Network.ObservationPointAgent.ObservationPointByRouteNo[AElementNo];
    if (LObsPoint <> nil) then
      AShape.Text := LObsPoint.FlowDataFileName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.OnShapeAdded (ASender      : TObject;
                                             const AShape : IVShape);
const OPNAME = 'THydroNVEventHandler.OnShapeAdded';
var
  LShapeType : THydroNVShapeType;
begin
  try
    if (AShape.Master <> nil) then
    begin
      LShapeType := HydroNVShapeType(AShape);
      case LShapeType of
        stHydroReservoir      : ReservoirShapeAdded(AShape);
        stHydroRunOff         : RunOffShapeAdded(AShape);
        stHydroChannel        : ChannelShapeAdded(AShape);
        stHydroIrrBlock       : IrrBlockShapeAdded(AShape);
        stHydroMine           : MineShapeAdded(AShape);
        stHydroRoute          : RouteShapeAdded(AShape);
        stHydroObsPoint       : ObsPointShapeAdded(AShape);
        stHydroText           : TextShapeAdded(AShape);
        stHydroOutputSelector : OutputSelectorShapeAdded(AShape);
      else
      end;
      RefreshAllNetworkText;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.OnShapeDelete (ASender      : TObject;
                                              const AShape : IVShape);
const OPNAME = 'THydroNVEventHandler.OnShapeDelete';
var
  LShapeType : THydroNVShapeType;
begin
  try
    if (NOT FSystemFlag) then
    begin
      LShapeType := HydroNVShapeType(AShape);
      case LShapeType of
        stHydroReservoir      : ReservoirShapeDeleted(AShape);
        stHydroRunOff         : RunOffShapeDeleted(AShape);
        stHydroChannel        : ChannelShapeDeleted(AShape);
        stHydroIrrBlock       : IrrBlockShapeDeleted(AShape);
        stHydroMine           : MineShapeDeleted(AShape);
        stHydroRoute          : RouteShapeDeleted(AShape);
        stHydroObsPoint       : ObsPointShapeDeleted(AShape);
        stHydroOutputSelector : FOutputSelector := nil
      else
      end;
    end;
    RefreshAllNetworkText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.AddConnection (ASourceShape : IVShape;
                                              ATargetShape : IVShape);
const OPNAME = 'THydroNVEventHandler.AddConnection';
var
  LIndex             : Integer;
  LConnect           : IVConnect;
  LRouteNo           : integer;
  LRoute             : INetworkRoute;
  LOldSourceModuleID : integer;
  LOldSinkModuleID   : integer;
  LOldSourceText     : String;
  LOldSinkText       : String;
  LNewNodeNr         : integer;
  LNewNodeText       : String;
  LMessage           : string;
  LUp                : Boolean;
  LChange            : Boolean;
  LHydrologyModel    : IHydrologyModel;
begin
  try
    if (ATargetShape.Master <> nil) AND
       (HydroNVShapeType(ATargetShape) in [stHydroReservoir, stHydroRunOff, stHydroChannel, stHydroIrrBlock, stHydroMine]) then
    begin
      LRouteNo        := GetShapeElementNo(ASourceShape, 0);
      LHydrologyModel := FAppModules.Model as IHydrologyModel;
      LRoute          := LHydrologyModel.Network.NetworkRouteAgent.NetworkRouteByRouteNo[LRouteNo];
      if (LRoute <> nil) then
      begin
        LOldSourceModuleID := LRoute.SourceModuleID;
        LOldSinkModuleID   := LRoute.SinkModuleID;
        LOldSourceText     := LHydrologyModel.Network.ModuleTextByID[LOldSourceModuleID];
        LOldSinkText       := LHydrologyModel.Network.ModuleTextByID[LOldSinkModuleID];
        for lIndex := 1 to ASourceShape.Connects.Count do
        begin
          lConnect := ASourceShape.Connects[lIndex];
          if (lConnect.ToSheet.Name = ATargetShape.Name) then
          begin
            LUp          := lConnect.FromPart = visBegin;
            LNewNodeNr   := GetShapeModuleID(ATargetShape);
            LNewNodeText := LHydrologyModel.Network.ModuleTextByID[LNewNodeNr];
            LChange      := (LUp AND (LNewNodeNr <> LOldSourceModuleID)) OR
                            ((NOT LUp) AND (LNewNodeNr <> LOldSinkModuleID));
            if (LChange) then
            begin
              if (NOT FMayChangeStructure) then
              begin
                LMessage := 'Changes to structure of network are nor allowed.';
                ShowMessage(LMessage);
              end
              else
              begin
                if (LUp) then
                begin
                  LMessage := 'This action will change the source of route %d from %s to %s. ' +
                              'This change will not only be applied to the diagram but ' +
                              ' also to the network data. Are you sure you want to continue?';
                  LMessage := Format(LMessage, [LRouteNo, LOldSourceText, LNewNodeText]);
                end
                else
                begin
                  LMessage := 'This action will change the sink of route %d from %s to %s. ' +
                              'This change will not only be applied to the diagram but ' +
                              ' also to the network data. Are you sure you want to continue?';
                  LMessage := Format(LMessage, [LRouteNo, LOldSinkText, LNewNodeText]);
                end;
                if (MessageDlg(LMessage, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
                begin
                  if (LUp) then
                    LHydrologyModel.Network.SetRouteSource(LRouteNo, LNewNodeNr)
                  else
                    LHydrologyModel.Network.SetRouteSink(LRouteNo, LNewNodeNr)
                end;
              end;
              RefreshRoute(ASourceShape, LRouteNo);
              SnapAddedRouteToModules(ASourceShape, LRouteNo);
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.DeleteConnection (ASourceShape : IVShape;
                                                 ATargetShape : IVShape);
const OPNAME = 'THydroNVEventHandler.DeleteConnection';
var
  LIndex              : Integer;
  LConnect            : IVConnect;
  LRouteNo            : integer;
  LRoute              : INetworkRoute;
  LUpConnect          : Boolean;
  LDownConnect        : Boolean;
  LUpShape            : IVShape;
  LDownShape          : IVShape;
  LHydrologyModel     : IHydrologyModel;
  LOldSourceModuleID  : integer;
  LOldSinkModuleID    : integer;
  LOldSourceText      : String;
  LOldSinkText        : String;
  LMessage            : String;
begin
  try
    if (ATargetShape.Master <> nil) AND
       (HydroNVShapeType(ATargetShape) in [stHydroReservoir, stHydroRunOff, stHydroChannel, stHydroIrrBlock, stHydroMine]) then
    begin
      LRouteNo        := GetShapeElementNo(ASourceShape, 0);
      LHydrologyModel := FAppModules.Model as IHydrologyModel;
      LRoute          := LHydrologyModel.Network.NetworkRouteAgent.NetworkRouteByRouteNo[LRouteNo];
      if (LRoute <> nil) then
      begin
        LOldSourceModuleID := LRoute.SourceModuleID;
        LOldSinkModuleID   := LRoute.SinkModuleID;
        LOldSourceText     := LHydrologyModel.Network.ModuleTextByID[LOldSourceModuleID];
        LOldSinkText       := LHydrologyModel.Network.ModuleTextByID[LOldSinkModuleID];
        LUpConnect         := FALSE;
        LDownConnect       := FALSE;
        for LIndex := 1 to ASourceShape.Connects.Count do
        begin
          LConnect := ASourceShape.Connects[lIndex];
          if (LConnect.FromPart = visBegin) then
            LUpConnect := TRUE
          else
          if (LConnect.FromPart = visEnd) then
            LDownConnect := TRUE;
        end;
        if (NOT LUpConnect) then
        begin
          LUpShape := FindRouteSourceShape(LRoute);
          if (LUpShape <> nil) then
          begin
            if (NOT FMayChangeStructure) then
            begin
              LMessage := 'Changes to structure of network are nor allowed.';
              ShowMessage(LMessage);
            end
            else
            begin
              LMessage := 'This action will change the source of route %d from %s to 0. ' +
                          'This change will not only be applied to the diagram but ' +
                          ' also to the network data. Are you sure you want to continue?';
              LMessage := Format(LMessage, [LRouteNo, LOldSourceText]);
              if (MessageDlg(LMessage, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
                LHydrologyModel.Network.SetRouteSource(LRouteNo, 0);
            end;
            RefreshRoute(ASourceShape, LRouteNo);
            SnapAddedRouteToModules(ASourceShape, LRouteNo);
          end;
        end;
        if (NOT LDownConnect) then
        begin
          LDownShape := FindRouteSinkShape(LRoute);
          if (LDownShape <> nil) then
          begin
            if (NOT FMayChangeStructure) then
            begin
              LMessage := 'Changes to structure of network are nor allowed.';
              ShowMessage(LMessage);
            end
            else
            begin
              LMessage := 'This action will change the sink of route %d from %s to 0. ' +
                          'This change will not only be applied to the diagram but ' +
                          ' also to the network data. Are you sure you want to continue?';
              LMessage := Format(LMessage, [LRouteNo, LOldSinkText]);
              if (MessageDlg(LMessage, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
                LHydrologyModel.Network.SetRouteSink(LRouteNo, 0);
            end;
            RefreshRoute(ASourceShape, LRouteNo);
            SnapAddedRouteToModules(ASourceShape, LRouteNo);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.ColourChanged (ACell  : IVCell;
                                              AShape : IVShape);
const OPNAME = 'THydroNVEventHandler.ColourChanged';
begin
  try
    if (AShape.CellExists[cColourChanged, 0] = -1) then
      AShape.Cells[cColourChanged].Formula := '"Y"';
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVEventHandler.PositionChanged (ACell  : IVCell;
                                                AShape : IVShape);
const OPNAME = 'THydroNVEventHandler.PositionChanged';
var
  LShapeType : THydroNVShapeType;
begin
  try
    if (AShape.Master <> nil) then
    begin
      LShapeType := HydroNVShapeType(AShape);
      case LShapeType of
        stHydroReservoir : ReservoirPositionChanged(ACell, AShape);
        stHydroRunOff    : RunOffPositionChanged(ACell, AShape);
        stHydroChannel   : ChannelPositionChanged(ACell, AShape);
        stHydroIrrBlock  : IrrBlockPositionChanged(ACell, AShape);
        stHydroMine      : MinePositionChanged(ACell, AShape);
        stHydroRoute     : RoutePositionChanged(ACell, AShape);
        stHydroObsPoint  : ObsPointPositionChanged(ACell, AShape);
        stHydroText      : TextPositionChanged(ACell, AShape);
      else
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVEventHandler.NetworkModulePositionChanged (ACell  : IVCell;
                                                             AShape : IVShape);
const OPNAME = 'THydroNVEventHandler.NetworkModulePositionChanged';
var
  lTxtShape    : IVShape;
  lDiff        : double;
  lParentPin   : double;
  lShapesLst   : TStringList;
  lIndex       : integer;
  lShapeName   : string;
  LElementNo   :  integer;
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
            lDiff      := GetShapeParentXDiff(lTxtShape);
            lParentPin := GetShapePinX(AShape);
            SetShapeParentXMoved(lTxtShape, 1);
            SetShapePinX(lTxtShape, lParentPin - lDiff);
          end
          else
          if (ACell.Name = cPinY) then
          begin
            lDiff      := GetShapeParentYDiff(lTxtShape);
            lParentPin := GetShapePinY(AShape);
            SetShapeParentYMoved(lTxtShape, 1);
            SetShapePinY(lTxtShape, lParentPin - lDiff);
          end;
        end;
      end
    finally
      FreeAndNil(lShapesLst);
    end;

    if (FVisioApp.ActivePage <> nil) then
    begin
      if (FDrawing.GISDrawing = 1) then
      begin
        LElementNo := GetShapeElementNo(AShape, -1);
        if (ACell.Name = cPinX) then
          GISCalculateLonLatOfShape (AShape, LElementNo, gisCalcLon)
        else if (ACell.Name = cPinY) then
          GISCalculateLonLatOfShape (AShape, LElementNo, gisCalcLat)
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVEventHandler.NetworkLinePositionChanged (ACell  : IVCell;
                                                           AShape : IVShape);
const OPNAME = 'THydroNVEventHandler.NetworkLinePositionChanged';
var
  lXDiff       : double;
  lYDiff       : double;
  lXParent     : double;
  lYParent     : double;
  lTxtShape    : IVShape;
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
            lXDiff := GetShapeParentXDiff(lTxtShape);
            lXParent := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinX].ResultStr[visMillimeters])),0);
            SetShapeParentXMoved(lTxtShape, 1);
            SetShapePinX(lTxtShape, lXParent - lXDiff);
          end
          else
          if (ACell.Name = cBeginY) OR (ACell.Name = cEndY) then
          begin
            lYDiff := GetShapeParentYDiff(lTxtShape);
            lYParent := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinY].ResultStr[visMillimeters])),0);
            SetShapeParentYMoved(lTxtShape, 1);
            SetShapePinY(lTxtShape, lYParent - lYDiff);
          end;
        end;
      end
    finally
      FreeAndNil(lShapesLst);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVEventHandler.ReservoirPositionChanged (ACell  : IVCell;
                                                         AShape : IVShape);
const OPNAME = 'THydroNVEventHandler.ReservoirPositionChanged';
begin
  try
    NetworkModulePositionChanged(ACell, AShape);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVEventHandler.RunOffPositionChanged (ACell  : IVCell;
                                                      AShape : IVShape);
const OPNAME = 'THydroNVEventHandler.RunOffPositionChanged';
begin
  try
    NetworkModulePositionChanged(ACell, AShape);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVEventHandler.ChannelPositionChanged (ACell  : IVCell;
                                                       AShape : IVShape);
const OPNAME = 'THydroNVEventHandler.ChannelPositionChanged';
begin
  try
    NetworkModulePositionChanged(ACell, AShape);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVEventHandler.IrrBlockPositionChanged (ACell  : IVCell;
                                                        AShape : IVShape);
const OPNAME = 'THydroNVEventHandler.IrrBlockPositionChanged';
begin
  try
    NetworkModulePositionChanged(ACell, AShape);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVEventHandler.MinePositionChanged (ACell  : IVCell;
                                                    AShape : IVShape);
const OPNAME = 'THydroNVEventHandler.MinePositionChanged';
begin
  try
    NetworkModulePositionChanged(ACell, AShape);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVEventHandler.FindObsPointsOnRoute (ARouteNo : integer; AShapesList: TStringList);
const OPNAME = 'THydroNVEventHandler.FindObsPointsOnRoute';
var
  lPageIdx   : integer;
  lShapeIdx  : integer;
  lShape     : IVShape;
  lPage      : IVPage;
  LRouteNo   : Integer;
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
        if ((lShape <> nil) AND (Pos(PChar(mtHydroObsPoint), lShape.Name) > 0)) then
        begin
          LRouteNo := GetShapeElementNo(lShape, -1);
          if (LRouteNo = ARouteNo) then
              AShapesList.Add(lShape.Name);
        end;
        lShapeIdx := lShapeIdx + 1;
      end;
      lPageIdx := lPageIdx + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.RoutePositionChanged (ACell  : IVCell;
                                                     AShape : IVShape);
const OPNAME = 'THydroNVEventHandler.RoutePositionChanged';
var
  lShapesLst     : TStringList;
  LRouteNo       : Integer;
  lObsPointShape : IVShape;
  lIndex         : integer;
  lShapeName     : string;
begin
  try
    NetworkLinePositionChanged(ACell, AShape);
    lShapesLst := TStringList.Create;
    try
      LRouteNo := GetShapeElementNo(AShape, 0);
      FindObsPointsOnRoute(LRouteNo, lShapesLst);
      for lIndex := 0 to lShapesLst.Count - 1 do
      begin
        lShapeName      := lShapesLst.Strings[lIndex];
        lObsPointShape  := FindShapeWithName(lShapeName);
        if (lObsPointShape <> nil) then
        begin
          SetShapeParentMoved(lObsPointShape, 1);
          PositionObsPointOnRoute(lObsPointShape, AShape);
        end;
      end;
    finally
      lShapesLst.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVEventHandler.ObsPointPositionChanged (ACell  : IVCell;
                                                        AShape : IVShape);
const OPNAME = 'THydroNVEventHandler.ObsPointPositionChanged';
var
  LRouteNo       : Integer;
  LRouteShape    : IVShape;
  LParentMoved   : Integer;
  LObsM          : Double;
  LObsC          : Double;
  LRouteBeginX   : Double;
  LRouteEndX     : Double;
  LRouteBeginY   : Double;
  LRouteEndY     : Double;
  LRouteM        : Double;
  LRouteC        : Double;
  LCrossX        : Double;
  LObsPinX       : Double;
  LObsPinY       : Double;
  LPercFromStart : Double;
begin
  try
    LParentMoved := GetShapeParentMoved(AShape);
    if (lParentMoved = 1) then
      SetShapeParentMoved(AShape,0)
    else
    begin
      LRouteNo    := GetShapeElementNo(AShape, 0);
      LRouteShape := FindShapeWithPropertyValue(mtHydroRoute, cNumber, IntToStr(LRouteNo));
      if (LRouteShape <> nil) then
      begin
        LRouteBeginX := StrToFloatDef(UnMM(UnQuote(LRouteShape.Cells[cBeginX].ResultStr[visMillimeters])),0);
        LRouteEndX   := StrToFloatDef(UnMM(UnQuote(LRouteShape.Cells[cEndX].ResultStr[visMillimeters])),0);
        LRouteBeginY := StrToFloatDef(UnMM(UnQuote(LRouteShape.Cells[cBeginY].ResultStr[visMillimeters])),0);
        LRouteEndY   := StrToFloatDef(UnMM(UnQuote(LRouteShape.Cells[cEndY].ResultStr[visMillimeters])),0);
        LObsPinX     := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinX].ResultStr[visMillimeters])),0);
        LObsPinY     := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinY].ResultStr[visMillimeters])),0);

        if (LRouteEndX = LRouteBeginX) then
          LPercFromStart := (LObsPinY - LRouteBeginY) / (LRouteEndY - LRouteBeginY)
        else if (LRouteEndY = LRouteBeginY) then
          LPercFromStart := (LObsPinX - LRouteBeginX) / (LRouteEndX - LRouteBeginX)
        else
        begin
          LRouteM  := (LRouteEndY - LRouteBeginY) / (LRouteEndX - LRouteBeginX);
          LRouteC  := LRouteBeginY - (LRouteM * LRouteBeginX);
          LObsM    := -1 / LRouteM;
          LObsC    := LObsPinY - (LObsM * LObsPinX);
          LCrossX  := (LRouteC - LObsC) / (LObsM - LRouteM);
          LPercFromStart := (LCrossX - LRouteBeginX) / (LRouteEndX - LRouteBeginX);
        end;
        SetShapePercFromStart(AShape, LPercFromStart);
        PositionObsPointOnRoute(AShape, LRouteShape);
      end;
    end;
    NetworkModulePositionChanged(ACell, AShape);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVEventHandler.TextPositionChanged (ACell  : IVCell;
                                                    AShape : IVShape);
const OPNAME = 'THydroNVEventHandler.TextPositionChanged';
var
  lParentName  : string;
  lParentShape : IVShape;
  lTextPin     : double;
  lParentPin   : double;
  lDiff        : double;
  lParentMoved : integer;
begin
  try
    lParentName := GetShapeParent(AShape);
    lParentShape := FindShapeWithName(lParentName);
    if (lParentShape <> nil) then
    begin
      if (ACell.Name = cPinX) then
      begin
        lParentMoved := GetShapeParentXMoved(AShape);
        if (lParentMoved = 1) then
          SetShapeParentXMoved(AShape, 0)
        else
        begin
          lTextPin   := GetShapePinX(AShape);
          lParentPin := StrToFloatDef(UnMM(UnQuote(lParentShape.Cells[cPinX].ResultStr[visMillimeters])),0);
          lDiff      := lParentPin - lTextPin;
          SetShapeParentXDiff(AShape, lDiff);
        end;
      end
      else
      if (ACell.Name = cPinY) then
      begin
        lParentMoved := GetShapeParentYMoved(AShape);
        if (lParentMoved = 1) then
          SetShapeParentYMoved(AShape, 0)
        else
        begin
          lTextPin   := GetShapePinY(AShape);
          lParentPin := StrToFloatDef(UnMM(UnQuote(lParentShape.Cells[cPinY].ResultStr[visMillimeters])),0);
          lDiff      := lParentPin - lTextPin;
          SetShapeParentYDiff(AShape, lDiff);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVEventHandler.PositionShapeOnGIS (AShape: IVShape; AElementNo : Integer);
const OPNAME = 'THydroNVEventHandler.PositionShapeOnGIS';
var
  LModule         : IModule;
  LHydrologyModel : IHydrologyModel;
  LModuleID       : Integer;
begin
  try
    if (FDrawing.GISDrawing = 1) AND (NOT IsShapeDuplicate(AShape)) then
    begin
      LHydrologyModel := FAppModules.Model as IHydrologyModel;
//      LSequenceNumber := GetShapeNetworkSequence(AShape);
//      LModule := LHydrologyModel.Network.ModuleBySequenceNumber[LSequenceNumber];
      LModuleID := GetShapeModuleID(AShape);
      LModule   := LHydrologyModel.Network.ModuleByID[LModuleID];
      if ((LModule.Longitude <> 0) AND (LModule.Latitude <> 0)) then
      begin
        GISPopulateNoRecalcLonLatList(AShape, AElementNo);
        GISSetXYPosFromLonLat(AShape, AElementNo)
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVEventHandler.ReservoirShapeAdded(const AShape: IVShape; AElementNo : Integer = -1; ADeleteLock : Integer = 0);
const OPNAME = 'THydroNVEventHandler.ReservoirShapeAdded';
var
  LElementNo      : integer;
  LCount          : Integer;
  LHydrologyModel : IHydrologyModel;
  LReservoir      : IReservoirModule;
begin
  try
    LElementNo := AElementNo;
    if (FBufferReservoirList.Count > 0) AND (LElementNo = -1) then
      // Upgraded reservoir
      FBufferReservoirList.Delete(0)
    else
    begin
      if (AShape.CellExists[cNumber, 0] <> 0) AND (GetShapeElementNoAsString(AShape) <> '') then
        // Copied reservoir
        LElementNo := GetShapeElementNo(AShape, 0)
      else if (LElementNo <= 0) then
        // New reservoir - from stencil
        LElementNo := ShowSelectReservoirsDialog;
      if (LElementNo = -1) then
        AShape.Delete
      else
      begin
        if (LElementNo = 0) then
        begin
          LHydrologyModel := FAppModules.Model as IHydrologyModel;
          LReservoir      := LHydrologyModel.Network.ReservoirModuleAgent.CreateNewReservoirModule(LHydrologyModel.Network.NetworkID);
          LElementNo      := LReservoir.ModuleNumber;
        end;

        AShape.Text := 'RV' + IntToStr(LElementNo);
        SetShapeElementNo(AShape, LElementNo);
        SetShapeVersion(AShape, CThisVersion);
        if (FReservoirList.IndexOf(IntToStr(LElementNo)) >= 0) then
        begin
          LCount := CountElementInDuplicateList(FDuplicateReservoirList, IntToStr(LElementNo));
          SetShapeDuplicateNo(AShape, LCount + 1);
        end;
        RefreshReservoir(AShape, LElementNo);
        if (FDrawing.GISDrawing = 1) then
          PositionShapeOnGIS(AShape, LElementNo);
        if (NOT IsShapeDuplicate(AShape)) then
          SnapRoutesToAddedModule(AShape, AShape.Text);  
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.ReservoirShapeDeleted (const AShape: IVShape);
const OPNAME = 'THydroNVEventHandler.ReservoirShapeDeleted';
var
  lSelection      : IVSelection;
  LElementNo      : string;
  LElementIndex   : integer;
  LDupNo          : Integer;
begin
  try
    LElementNo := GetShapeElementNoAsString(AShape);
    if (IsShapeDuplicate(AShape)) then
    begin
      LDupNo := GetShapeDuplicateNo(AShape);
      RemoveElementFromDuplicateList(FDuplicateReservoirList, LElementNo);
      RenameDuplicateShapes(mtHydroReservoir, LElementNo, LDupNo);
    end
    else
    begin
      // Delete all the duplicates of the shape first
      DeleteDuplicateShapes(mtHydroReservoir, LElementNo);
      ClearElementFromDuplicateList(FDuplicateReservoirList, LElementNo);
      LElementIndex := FReservoirList.IndexOf(LElementNo);
      if (LElementIndex >= 0) then
        FReservoirList.Delete(LElementIndex);
    end;
    lSelection := FVisioApp.ActiveWindow.Selection;
    {Remove all text annotations for this reservoir}
    DeleteAllTextShapes(AShape);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.RunOffShapeAdded(const AShape: IVShape; AElementNo : Integer = -1);
const OPNAME = 'THydroNVEventHandler.RunOffShapeAdded';
var
  LElementNo      : integer;
  LCount          : Integer;
  LHydrologyModel : IHydrologyModel;
  LRunOff         : IRunOffModule;
begin
  try
    LElementNo := AElementNo;
    if (FBufferRunOffList.Count > 0) AND (LElementNo = -1) then
      // Upgraded RunOff
      FBufferRunOffList.Delete(0)
    else
    begin
      if (AShape.CellExists[cNumber, 0] <> 0) AND (GetShapeElementNoAsString(AShape) <> '') then
        // Copied RunOff
        LElementNo  := GetShapeElementNo(AShape, 0)
      else if (LElementNo <= 0) then
        // New RunOff - from stencil
        LElementNo := ShowSelectRunOffsDialog;
      if (LElementNo = -1) then
        AShape.Delete
      else
      begin
        if (LElementNo = 0) then
        begin
          LHydrologyModel := FAppModules.Model as IHydrologyModel;
          LRunOff         := LHydrologyModel.Network.RunOffModuleAgent.CreateNewRunOffModule(LHydrologyModel.Network.NetworkID);
          LElementNo      := LRunOff.ModuleNumber;
        end;

        AShape.Text := 'RU' + IntToStr(LElementNo);
        SetShapeElementNo(AShape, LElementNo);
        SetShapeVersion(AShape, CThisVersion);
        if (FRunOffList.IndexOf(IntToStr(LElementNo)) >= 0) then
        begin
          LCount := CountElementInDuplicateList(FDuplicateRunOffList, IntToStr(LElementNo));
          SetShapeDuplicateNo(AShape, LCount + 1);
        end;
        RefreshRunOff(AShape, LElementNo);
        if (FDrawing.GISDrawing = 1) then
          PositionShapeOnGIS(AShape, LElementNo);
        if (NOT IsShapeDuplicate(AShape)) then
          SnapRoutesToAddedModule(AShape, AShape.Text);  
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.RunOffShapeDeleted (const AShape: IVShape);
const OPNAME = 'THydroNVEventHandler.RunOffShapeDeleted';
var
  lSelection      : IVSelection;
  LElementNo      : string;
  LElementIndex   : integer;
  LDupNo          : Integer;
begin
  try
    LElementNo := GetShapeElementNoAsString(AShape);
    if (IsShapeDuplicate(AShape)) then
    begin
      LDupNo := GetShapeDuplicateNo(AShape);
      RemoveElementFromDuplicateList(FDuplicateRunOffList, LElementNo);
      RenameDuplicateShapes(mtHydroRunOff, LElementNo, LDupNo);
    end
    else
    begin
      DeleteDuplicateShapes(mtHydroRunOff, LElementNo);
      ClearElementFromDuplicateList(FDuplicateRunOffList, LElementNo);
      LElementIndex := FRunOffList.IndexOf(LElementNo);
      if (LElementIndex >= 0) then
        FRunOffList.Delete(LElementIndex);
    end;
    lSelection := FVisioApp.ActiveWindow.Selection;
    DeleteAllTextShapes(AShape);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.ChannelShapeAdded(const AShape: IVShape; AElementNo : Integer = -1);
const OPNAME = 'THydroNVEventHandler.ChannelShapeAdded';
var
  LElementNo      : integer;
  LCount          : Integer;
  LHydrologyModel : IHydrologyModel;
  LChannelReach   : IChannelModule;
begin
  try
    LElementNo := AElementNo;
    if (FBufferChannelList.Count > 0) AND (LElementNo = -1) then
      // Upgraded Channel
      FBufferChannelList.Delete(0)
    else
    begin
      if (AShape.CellExists[cNumber, 0] <> 0) AND (GetShapeElementNoAsString(AShape) <> '') then
        // Copied Channel
        LElementNo  := GetShapeElementNo(AShape, 0)
      else if (LElementNo <= 0) then
        // New Channel - from stencil
        LElementNo := ShowSelectChannelsDialog;
      if (LElementNo = -1) then
        AShape.Delete
      else
      begin
        if (LElementNo = 0) then
        begin
          LHydrologyModel := FAppModules.Model as IHydrologyModel;
          LChannelReach   := LHydrologyModel.Network.ChannelModuleAgent.CreateNewChannelModule(LHydrologyModel.Network.NetworkID);
          LElementNo      := LChannelReach.ModuleNumber;
        end;

        AShape.Text := 'CR' + IntToStr(LElementNo);
        SetShapeElementNo(AShape, LElementNo);
        SetShapeVersion(AShape, CThisVersion);
        if (FChannelList.IndexOf(IntToStr(LElementNo)) >= 0) then
        begin
          LCount := CountElementInDuplicateList(FDuplicateChannelList, IntToStr(LElementNo));
          SetShapeDuplicateNo(AShape, LCount + 1);
        end;
        RefreshChannel(AShape, LElementNo);
        if (FDrawing.GISDrawing = 1) then
          PositionShapeOnGIS(AShape, LElementNo);
        if (NOT IsShapeDuplicate(AShape)) then
          SnapRoutesToAddedModule(AShape, AShape.Text);  
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.ChannelShapeDeleted (const AShape: IVShape);
const OPNAME = 'THydroNVEventHandler.ChannelShapeDeleted';
var
  lSelection      : IVSelection;
  LElementNo      : string;
  LElementIndex   : integer;
  LDupNo          : Integer;
begin
  try
    LElementNo := GetShapeElementNoAsString(AShape);
    if (IsShapeDuplicate(AShape)) then
    begin
      LDupNo := GetShapeDuplicateNo(AShape);
      RemoveElementFromDuplicateList(FDuplicateChannelList, LElementNo);
      RenameDuplicateShapes(mtHydroChannel, LElementNo, LDupNo);
    end
    else
    begin
      DeleteDuplicateShapes(mtHydroChannel, LElementNo);
      ClearElementFromDuplicateList(FDuplicateChannelList, LElementNo);
      LElementIndex := FChannelList.IndexOf(LElementNo);
      if (LElementIndex >= 0) then
        FChannelList.Delete(LElementIndex);
    end;
    lSelection := FVisioApp.ActiveWindow.Selection;
    DeleteAllTextShapes(AShape);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.IrrBlockShapeAdded(const AShape: IVShape; AElementNo : Integer = -1);
const OPNAME = 'THydroNVEventHandler.IrrBlockShapeAdded';
var
  LElementNo      : integer;
  LCount          : Integer;
  LHydrologyModel : IHydrologyModel;
  LIrrigation     : IIrrigationModule;
begin
  try
    LElementNo := AElementNo;
    if (FBufferIrrBlockList.Count > 0) AND (LElementNo = -1) then
      // Upgraded IrrBlock
      FBufferIrrBlockList.Delete(0)
    else
    begin
      if (AShape.CellExists[cNumber, 0] <> 0) AND (GetShapeElementNoAsString(AShape) <> '') then
        // Copied IrrBlock
        LElementNo  := GetShapeElementNo(AShape, 0)
      else if (LElementNo <= 0) then
        // New IrrBlock - from stencil
        LElementNo := ShowSelectIrrBlocksDialog;
      if (LElementNo = -1) then
        AShape.Delete
      else
      begin
        if (LElementNo = 0) then
        begin
          LHydrologyModel := FAppModules.Model as IHydrologyModel;
          LIrrigation     := LHydrologyModel.Network.IrrigationModuleAgent.CreateNewIrrigationModule(LHydrologyModel.Network.NetworkID);
          LElementNo      := LIrrigation.ModuleNumber;
        end;

        AShape.Text := 'RR' + IntToStr(LElementNo);
        SetShapeElementNo(AShape, LElementNo);
        SetShapeVersion(AShape, CThisVersion);
        if (FIrrBlockList.IndexOf(IntToStr(LElementNo)) >= 0) then
        begin
          LCount := CountElementInDuplicateList(FDuplicateIrrBlockList, IntToStr(LElementNo));
          SetShapeDuplicateNo(AShape, LCount + 1);
        end;
        RefreshIrrBlock(AShape, LElementNo);
        if (FDrawing.GISDrawing = 1) then
          PositionShapeOnGIS(AShape, LElementNo);
        if (NOT IsShapeDuplicate(AShape)) then
          SnapRoutesToAddedModule(AShape, AShape.Text);  
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.IrrBlockShapeDeleted (const AShape: IVShape);
const OPNAME = 'THydroNVEventHandler.IrrBlockShapeDeleted';
var
  lSelection      : IVSelection;
  LElementNo      : string;
  LElementIndex   : integer;
  LDupNo          : Integer;
begin
  try
    LElementNo := GetShapeElementNoAsString(AShape);
    if (IsShapeDuplicate(AShape)) then
    begin
      LDupNo := GetShapeDuplicateNo(AShape);
      RemoveElementFromDuplicateList(FDuplicateIrrBlockList, LElementNo);
      RenameDuplicateShapes(mtHydroIrrBlock, LElementNo, LDupNo);
    end
    else
    begin
      DeleteDuplicateShapes(mtHydroIrrBlock, LElementNo);
      ClearElementFromDuplicateList(FDuplicateIrrBlockList, LElementNo);
      LElementIndex := FIrrBlockList.IndexOf(LElementNo);
      if (LElementIndex >= 0) then
        FIrrBlockList.Delete(LElementIndex);
    end;
    lSelection := FVisioApp.ActiveWindow.Selection;
    DeleteAllTextShapes(AShape);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.MineShapeAdded(const AShape: IVShape; AElementNo : Integer = -1);
const OPNAME = 'THydroNVEventHandler.MineShapeAdded';
var
  LElementNo      : integer;
  LCount          : Integer;
  LHydrologyModel : IHydrologyModel;
  LMine           : IMineModule;
begin
  try
    LElementNo := AElementNo;
    if (FBufferMineList.Count > 0) AND (LElementNo = -1) then
      // Upgraded Mine
      FBufferMineList.Delete(0)
    else
    begin
      if (AShape.CellExists[cNumber, 0] <> 0) AND (GetShapeElementNoAsString(AShape) <> '') then
        // Copied Mine
        LElementNo  := GetShapeElementNo(AShape, 0)
      else if (LElementNo <= 0) then
        // New Mine - from stencil
        LElementNo := ShowSelectMinesDialog;
      if (LElementNo = -1) then
        AShape.Delete
      else
      begin
        if (LElementNo = 0) then
        begin
          LHydrologyModel := FAppModules.Model as IHydrologyModel;
          LMine           := LHydrologyModel.Network.MineModuleAgent.CreateNewMineModule(LHydrologyModel.Network.NetworkID);
          LElementNo      := LMine.ModuleNumber;
        end;

        AShape.Text := 'MM' + IntToStr(LElementNo);
        SetShapeElementNo(AShape, LElementNo);
        SetShapeVersion(AShape, CThisVersion);
        if (FMineList.IndexOf(IntToStr(LElementNo)) >= 0) then
        begin
          LCount := CountElementInDuplicateList(FDuplicateMineList, IntToStr(LElementNo));
          SetShapeDuplicateNo(AShape, LCount + 1);
        end;
        RefreshMine(AShape, LElementNo);
        if (FDrawing.GISDrawing = 1) then
          PositionShapeOnGIS(AShape, LElementNo);
        if (NOT IsShapeDuplicate(AShape)) then
          SnapRoutesToAddedModule(AShape, AShape.Text);  
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.MineShapeDeleted (const AShape: IVShape);
const OPNAME = 'THydroNVEventHandler.MineShapeDeleted';
var
  lSelection      : IVSelection;
  LElementNo      : string;
  LElementIndex   : integer;
  LDupNo          : Integer;
begin
  try
    LElementNo := GetShapeElementNoAsString(AShape);
    if (IsShapeDuplicate(AShape)) then
    begin
      LDupNo := GetShapeDuplicateNo(AShape);
      RemoveElementFromDuplicateList(FDuplicateMineList, LElementNo);
      RenameDuplicateShapes(mtHydroMine, LElementNo, LDupNo);
    end
    else
    begin
      DeleteDuplicateShapes(mtHydroMine, LElementNo);
      ClearElementFromDuplicateList(FDuplicateMineList, LElementNo);
      LElementIndex := FMineList.IndexOf(LElementNo);
      if (LElementIndex >= 0) then
        FMineList.Delete(LElementIndex);
    end;
    lSelection := FVisioApp.ActiveWindow.Selection;
    DeleteAllTextShapes(AShape);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.PositionObsPointOnRoute (const AShape : IVShape; const ARouteShape : IVShape);
const OPNAME = 'THydroNVEventHandler.PositionObsPointOnRoute';
var
  LRouteBeginX      : Double;
  LRouteEndX        : Double;
  LRouteBeginY      : Double;
  LRouteEndY        : Double;
  LCrossX           : Double;
  LCrossY           : Double;
  LRouteM           : Double;
  LObsPointM        : Double;
  LPercFromStart    : Double;
  LRads             : Double;
  LAngle            : Double;
begin
  try
    if (ARouteShape <> nil) then
    begin
      LRouteBeginX   := StrToFloatDef(UnMM(UnQuote(ARouteShape.Cells[cBeginX].ResultStr[visMillimeters])),0);
      LRouteEndX     := StrToFloatDef(UnMM(UnQuote(ARouteShape.Cells[cEndX].ResultStr[visMillimeters])),0);
      LRouteBeginY   := StrToFloatDef(UnMM(UnQuote(ARouteShape.Cells[cBeginY].ResultStr[visMillimeters])),0);
      LRouteEndY     := StrToFloatDef(UnMM(UnQuote(ARouteShape.Cells[cEndY].ResultStr[visMillimeters])),0);
      LPercFromStart := GetShapePercFromStart(AShape);

      LCrossX        :=  LRouteBeginX + (LRouteEndX - LRouteBeginX) * LPercFromStart;
      LCrossY        :=  LRouteBeginY + (LRouteEndY - LRouteBeginY) * LPercFromStart;
      if (LRouteEndX = LRouteBeginX) then
        LAngle := 90
      else if (LRouteEndY = LRouteBeginY) then 
        LAngle := 0
      else
      begin
        LRouteM    := (LRouteEndY - LRouteBeginY) / (LRouteEndX - LRouteBeginX);
        LObsPointM := -1 / LRouteM;
        LRads      := ArcTan(LObsPointM);
        LAngle     := LRads * 180 / Pi - 90;
      end;

      SetShapePinX(AShape, LCrossX);
      SetShapePinY(AShape, LCrossY);
      AShape.Cells[cAngle].Formula  := '"' + FloatToStrF(LAngle, ffFixed, 12, 2) + 'deg"';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.ObsPointShapeAdded(const AShape: IVShape; AElementNo : Integer = -1; ADeleteLock : Integer = 0);
const OPNAME = 'THydroNVEventHandler.ObsPointShapeAdded';
var
  LElementNo      : integer;
  LRouteShape     : IVShape;
  LCount          : Integer;
  LHydrologyModel : IHydrologyModel;
  LObsPoint       : IObservationPoint;
begin
  try
    LElementNo := AElementNo;
    if (FBufferObsPointList.Count > 0) AND (LElementNo = -1) then
      // Upgraded observation points
      FBufferObsPointList.Delete(0)
    else
    begin
      if (AShape.CellExists[cNumber, 0] <> 0) AND (GetShapeElementNoAsString(AShape) <> '') then
        // Copied observation point
        LElementNo  := GetShapeElementNo(AShape, 0)
      else if (LElementNo <= 0) then
        // New observation point - from stencil
        LElementNo := ShowSelectObsPointsDialog;
      if (LElementNo = -1) then
        AShape.Delete
      else
      begin
        LHydrologyModel := FAppModules.Model as IHydrologyModel;
        LObsPoint := LHydrologyModel.Network.ObservationPointAgent.ObservationPointByRouteNo[LElementNo];
        if (LObsPoint = nil) then
        begin
          LObsPoint := LHydrologyModel.Network.ObservationPointAgent.CreateNewObservationPoint(LHydrologyModel.Network.NetworkID, LElementNo);
          LElementNo := LObsPoint.RouteNo;
        end;

        AShape.Text := '';
        SetShapeVersion(AShape, CThisVersion);
        if (FObsPointList.IndexOf(IntToStr(LElementNo)) >= 0) then
        begin
          LCount := CountElementInDuplicateList(FDuplicateObsPointList, IntToStr(LElementNo));
          SetShapeDuplicateNo(AShape, LCount + 1);
        end;

//        LRouteShape := FindShapeWithPropertyValue(mtHydroRoute, cNumber, IntToStr(LObsPoint.RouteNo));
        LRouteShape := FindShapeWithPropertyValue(mtHydroRoute, cNumber, IntToStr(LElementNo));
        if (LRouteShape <> nil) then
        begin
          SetShapeElementNo(AShape, LElementNo);
          SetShapeParent(AShape, LRouteShape.Name);
          SetShapePercFromStart(AShape, 2/3);
          SetShapeParentMoved(AShape, 0);
          SetShapeVersion(AShape, CThisVersion);
          if (FObsPointList.IndexOf(IntToStr(LElementNo)) >= 0) then
          begin
            LCount := CountElementInDuplicateList(FDuplicateObsPointList, IntToStr(LElementNo));
            SetShapeDuplicateNo(AShape, LCount + 1);
          end;
          AShape.Text := '';
          PositionObsPointOnRoute(AShape, LRouteShape);
          RefreshObsPoint(AShape, LElementNo);
        end;

      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.ObsPointShapeDeleted (const AShape: IVShape);
const OPNAME = 'THydroNVEventHandler.ObsPointShapeDeleted';
var
  lSelection      : IVSelection;
  LElementNo      : string;
  LElementIndex   : integer;
  LDupNo          : Integer;
begin
  try
    LElementNo := GetShapeElementNoAsString(AShape);
    if (IsShapeDuplicate(AShape)) then
    begin
      LDupNo := GetShapeDuplicateNo(AShape);
      RemoveElementFromDuplicateList(FDuplicateObsPointList, LElementNo);
      RenameDuplicateShapes(mtHydroObsPoint, LElementNo, LDupNo);
    end
    else
    begin
      DeleteDuplicateShapes(mtHydroObsPoint, LElementNo);
      ClearElementFromDuplicateList(FDuplicateObsPointList, LElementNo);
      LElementIndex := FObsPointList.IndexOf(LElementNo);
      if (LElementIndex >= 0) then
        FObsPointList.Delete(LElementIndex);
    end;
    lSelection := FVisioApp.ActiveWindow.Selection;
    DeleteAllTextShapes(AShape);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.TextShapeAdded(const AShape: IVShape);
const OPNAME = 'THydroNVEventHandler.TextShapeAdded';
var
  lElementNr      : integer;
  lElementType    : string;
  LElementSubType : THydroNVElementSubType;
  LSectionNo      : Integer;
  lTextType       : THydroNVTextType;
  lParentShape    : IVShape;
  lXParent        : Double;
  lYParent        : Double;
  lXChild         : Double;
  lYChild         : Double;
  lIndex          : integer;
begin
  try
    if (FBufferTextList.Count > 0) AND (FBufferTextList.IndexOf(AShape.Name) >= 0) then
    begin
      lIndex := FBufferTextList.IndexOf(AShape.Name);
      FBufferTextList.Delete(lIndex);
    end
    else
    if (AShape.CellExists[cParent, 0] <> 0) AND (GetShapeParent(AShape) <> '') then
    begin
      AShape.Delete;
    end
    else
    begin
      if (ShowSelectTextDialog(lElementType, lElementNr, lTextType, LElementSubType, LSectionNo)) then
      begin
        if (lElementNr >= 0) AND (lElementType <> '') then
        begin
          if (lElementType = mtHydroNetwork) then
          begin
            SetShapeParent(AShape, mtHydroNetwork);
            AShape.CellsSRC[visSectionObject, visRowXFormOut, visXFormWidth].FormulaU := '50 mm';
          end
          else
          begin
            lParentShape := FindShapeWithPropertyValue(lElementType, cNumber, IntToStr(lElementNr));
            AShape.CellsSRC[visSectionObject, visRowXFormOut, visXFormWidth].FormulaU := 'GUARD(TEXTWIDTH(TheText))';
          end;
          if (lParentShape <> nil) OR (lElementType = mtHydroNetwork) then
          begin
            if (lParentShape <> nil) AND (AShape.CellExists[cParent, 0] = -1) then
            begin
              lXParent := StrToFloatDef(UnMM(UnQuote(lParentShape.Cells[cPinX].ResultStr[visMillimeters])),0);
              lYParent := StrToFloatDef(UnMM(UnQuote(lParentShape.Cells[cPinY].ResultStr[visMillimeters])),0);
              lXChild  := GetShapePinX(AShape);
              lYChild  := GetShapePinY(AShape);

              SetShapeParent(AShape, lParentShape.Name);
              SetShapeParentXDiff(AShape, lXParent - lXChild);
              SetShapeParentYDiff(AShape, lYParent - lYChild);
              SetShapeParentXMoved(AShape, 0);
              SetShapeParentYMoved(AShape,0);
            end;

            SetShapeVersion(AShape, CThisVersion);
            AShape.AddNamedRow(visSectionProp, 'TextType', visTagDefault);
            SetShapeHydroNVTextType(AShape, lTextType);
            AShape.AddNamedRow(visSectionProp, 'Number', visTagDefault);
            SetShapeElementNo(AShape, lElementNr);
            AShape.AddNamedRow(visSectionProp, 'ElementSubType', visTagDefault);
            SetShapeHydroNVElementSubType(AShape, LElementSubType);
            AShape.AddNamedRow(visSectionProp, 'SectionNo', visTagDefault);
            SetShapeSectionNo(AShape, LSectionNo);

            RefreshText(AShape);

          end;
        end;
      end
      else
        AShape.Delete;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.OutputSelectorShapeAdded(const AShape: IVShape);
const OPNAME = 'THydroNVEventHandler.OutputSelectorShapeAdded';
var
  lMessage    : string;
begin
  try
   if (FOutputSelector <> nil) then
    begin
      lMessage := 'The diagram already contains an output selector.';
      MessageDlg(lMessage, mtError, [mbOK], 0);
      AShape.Delete;
    end
    else
    begin
      FOutputSelector := AShape;
      SetShapeVersion(AShape, CThisVersion);
      RefreshOutputSelector(AShape);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.RouteShapeAdded(const AShape: IVShape; AElementNo : Integer = -1);
const OPNAME = 'THydroNVEventHandler.RouteShapeAdded';
var
  LElementNo      : integer;
  LCount          : Integer;
  LHydrologyModel : IHydrologyModel;
  LNetworkRoute   : INetworkRoute;
begin
  try
    LElementNo := AElementNo;
    if (FBufferRouteList.Count > 0) AND (LElementNo = -1) then
      // Upgraded route
      FBufferRouteList.Delete(0)
    else
    begin
      if (AShape.CellExists[cNumber, 0] <> 0) AND (GetShapeElementNoAsString(AShape) <> '') then
        // Copied route
        LElementNo := GetShapeElementNo(AShape, 0)
      else if (LElementNo <= 0) then
        // New route - from stencil
        LElementNo := ShowSelectRoutesDialog;
      if (LElementNo = -1) then
        AShape.Delete
      else
      begin
        LHydrologyModel := FAppModules.Model as IHydrologyModel;
        if (LElementNo = 0) then
        begin
          LNetworkRoute := LHydrologyModel.Network.NetworkRouteAgent.CreateNewNetworkRoute(LHydrologyModel.Network.NetworkID);
          LElementNo    := LNetworkRoute.RouteNo;
        end
        else
          LNetworkRoute := LHydrologyModel.Network.NetworkRouteAgent.NetworkRouteByRouteNo[LElementNo];

        AShape.Text := IntToStr(LElementNo);
        SetShapeElementNo(AShape, LElementNo);
        SetShapeVersion(AShape, CThisVersion);
        SetShapeRouteID(AShape, LNetworkRoute.RouteID);
        if (FRouteList.IndexOf(IntToStr(LElementNo)) >= 0) then
        begin
          LCount := CountElementInDuplicateList(FDuplicateRouteList, IntToStr(LElementNo));
          SetShapeDuplicateNo(AShape, LCount + 1);
        end;
        RefreshRoute(AShape, LElementNo);
        SnapAddedRouteToModules(AShape, LElementNo);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.RouteShapeDeleted(const AShape: IVShape);
const OPNAME = 'THydroNVEventHandler.RouteShapeDeleted';
var
  lSelection    : IVSelection;
  LElementNo    : string;
  LElementIndex : integer;
  LDupNo        : Integer;
  LObsShape     : IVShape;
begin
  try
    LElementNo := GetShapeElementNoAsString(AShape);
    if (IsShapeDuplicate(AShape)) then
    begin
      LDupNo := GetShapeDuplicateNo(AShape);
      RemoveElementFromDuplicateList(FDuplicateRouteList, LElementNo);
      RenameDuplicateShapes(mtHydroRoute, LElementNo, LDupNo);
    end
    else
    begin
      DeleteDuplicateShapes(mtHydroRoute, LElementNo);
      ClearElementFromDuplicateList(FDuplicateRouteList, LElementNo);
      LElementIndex := FRouteList.IndexOf(LElementNo);
      if (LElementIndex >= 0) then
        FRouteList.Delete(LElementIndex);
    end;
    lSelection := FVisioApp.ActiveWindow.Selection;
    DeleteAllTextShapes(AShape);
    LObsShape := FindShapeWithParent(mtHydroObsPoint, AShape.Name);
    if (LObsShape <> nil) then
      LObsShape.Delete;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.SnapAddedRouteToModules (const AShape : IVShape;
                                                        ARouteNr     : integer);
const OPNAME = 'THydroNVEventHandler.SnapAddedRouteToModules';
var
  LNetwork        : INetwork;
  LRoute          : INetworkRoute;
  LRouteCell      : IVCell;
  lOtherCell      : IVCell;
  lDownShape      : IVShape;
  LUpShape        : IVShape;
  lShape          : IVShape;
  lIndex          : integer;
  lIsConnected    : Boolean;
  lConnect        : IVConnect;
  LSourceModuleID : integer;
  LSinkModuleID   : integer;
  lNumber         : integer;
  LHydrologyModel : IHydrologyModel;
  LShapeType      : THydroNVShapeType;
begin
  try
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    LNetwork := LHydrologyModel.Network;
    LRoute   := LNetwork.NetworkRouteAgent.NetworkRouteByRouteNo[ARouteNr];

    if (LRoute <> nil) then
    begin
//      LSourceSeqNo := LRoute.SourceModuleNo;
//      LSinkSeqNo   := LRoute.SinkModuleNo;
      LSourceModuleID := LRoute.SourceModuleID;
      LSinkModuleID   := LRoute.SinkModuleID;
      if (LSinkModuleID <> 0) then
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
               (HydroNVShapeType(lShape) in [stHydroReservoir, stHydroRunOff, stHydroChannel, stHydroIrrBlock, stHydroMine]) then
            begin
//              lNumber := GetShapeNetworkSequence(lShape);
              lNumber := GetShapeModuleID(lShape);
              if (lNumber = LSinkModuleID) then
                lIsConnected := TRUE;
            end;
          end;
          lIndex := lIndex + 1;
        end;
        if (NOT lIsConnected) then
        begin
          lDownShape := FindRouteSinkShape(LRoute);
          if (lDownShape <> nil) then
          begin
            LShapeType := HydroNVShapeType(lDownShape);
            case LShapeType of
              stHydroReservoir : lOtherCell := lDownShape.CellsSRC[7, 2, 0];
              stHydroRunOff    : lOtherCell := lDownShape.CellsSRC[7, 2, 0];
              stHydroIrrBlock  : lOtherCell := lDownShape.CellsSRC[7, 2, 0];
              stHydroChannel   : lOtherCell := lDownShape.CellsSRC[7, 2, 0];
              stHydroMine      : lOtherCell := lDownShape.CellsSRC[7, 2, 0];
            else
              lOtherCell := lDownShape.CellsSRC[7, 2, 0];
            end;
            LRouteCell := AShape.CellsU[cEndX];
            LRouteCell.GlueTo(lOtherCell);
          end;
        end;
      end;

      if (LSourceModuleID <> 0) then
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
               (HydroNVShapeType(lShape) in [stHydroReservoir, stHydroRunOff, stHydroChannel, stHydroIrrBlock, stHydroMine]) then
            begin
//              lNumber := GetShapeNetworkSequence(lShape);
              lNumber := GetShapeModuleID(lShape);
              if (lNumber = LSourceModuleID) then
                lIsConnected := TRUE;
            end;
          end;
          lIndex := lIndex + 1;
        end;
        if (NOT lIsConnected) then
        begin
          LUpShape := FindRouteSourceShape(LRoute);
          if (LUpShape <> nil) then
          begin
            LShapeType := HydroNVShapeType(LUpShape);
            case LShapeType of
              stHydroReservoir : lOtherCell := LUpShape.CellsSRC[7, 2, 0];
              stHydroRunOff    : lOtherCell := LUpShape.CellsSRC[7, 2, 0];
              stHydroChannel   : lOtherCell := LUpShape.CellsSRC[7, 2, 0];
              stHydroIrrBlock  : lOtherCell := LUpShape.CellsSRC[7, 2, 0];
              stHydroMine      : lOtherCell := LUpShape.CellsSRC[7, 2, 0];
            else
              lOtherCell := LUpShape.CellsSRC[7, 2, 0];
            end;
            LRouteCell := AShape.CellsU[cBeginX];
            LRouteCell.GlueTo(lOtherCell);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.SnapRoutesToAddedModule (const AShape : IVShape; ATypedNumber : String);
const OPNAME = 'THydroNVEventHandler.SnapRoutesToAddedModule';
var
  LPageIndex   : Integer;
  LShapeIndex  : Integer;
  LPage        : IVPage;
  LRouteShape  : IVShape;
  LRouteSource : String;
  LRouteSink   : String;
  LRouteCell   : IVCell;
  LOtherCell   : IVCell;
  LFlag        : Boolean;
begin
  try
    LFlag := FALSE;
    for LPageIndex := 1 to FVisioDoc.Pages.Count  do
    begin
      LPage := FVisioDoc.Pages.Item[LPageIndex];
      LShapeIndex := 1;
      while (LShapeIndex <= LPage.Shapes.Count)  do
      begin
        LRouteShape := LPage.Shapes.Item[LShapeIndex];
        if (Pos(PChar(mtHydroRoute), LRouteShape.Name) > 0) then
        begin
          LRouteSource := GetRouteShapeSource(LRouteShape);
          LRouteSink   := GetRouteShapeSink(LRouteShape);
          if (LRouteSource = ATypedNumber) then
          begin
            LOtherCell := AShape.CellsSRC[7, 2, 0];
            LRouteCell := LRouteShape.CellsU[cBeginX];
            LRouteCell.GlueTo(LOtherCell);
            LFlag := TRUE;
          end;
          if (LRouteSink = ATypedNumber) then
          begin
            LOtherCell := AShape.CellsSRC[7, 2, 0];
            LRouteCell := LRouteShape.CellsU[cEndX];
            LRouteCell.GlueTo(LOtherCell);
            LFlag := TRUE;
          end;
        end;
        LShapeIndex := LShapeIndex + 1;
      end;
    end;
    if (LFlag) then
      AShape.SendToBack;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.FindRouteSinkShape (ARoute : INetworkRoute) : IVShape;
const OPNAME = 'THydroNVEventHandler.FindRouteSinkShape';
var
  LSinkModuleID   : Integer;
  LSinkModuleNo   : String;
  LNetworkModule  : INetworkModule;
  LHydrologyModel : IHydrologyModel;
//  LModuleText     : String;
  LMasterName     : String;
begin
  Result := nil;
  try
    if (ARoute <> nil) then
    begin
//      LSinkSeqNo      := ARoute.SinkModuleNo;
      LSinkModuleID   := ARoute.SinkModuleID;
      LHydrologyModel := FAppModules.Model as IHydrologyModel;
      LNetworkModule  := LHydrologyModel.Network.ModuleByID[LSinkModuleID];
//      LModuleText     := LHydrologyModel.Network.ModuleTextBySequenceNumber[LSinkSeqNo];
//      LNetworkModule := LHydrologyModel.Network.ModuleBySequenceNumber[LSinkSeqNo];
      if (LNetworkModule <> nil) then
      begin
        LSinkModuleNo := IntToStr(LNetworkModule.ModuleNumber);
        LMasterName   := GetMasterName(LNetworkModule.ModuleType);
        Result        := FindShapeWithPropertyValue(LMasterName, cNumber, LSinkModuleNo);
      end;
      LNetworkModule := nil;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.FindRouteSourceShape (ARoute : INetworkRoute) : IVShape;
const OPNAME = 'THydroNVEventHandler.FindRouteSourceShape';
var
  LSourceModuleID : Integer;
  LSourceModuleNo : string;
  LNetworkModule  : INetworkModule;
  LHydrologyModel : IHydrologyModel;
  LMasterName     : String;
begin
  Result := nil;
  try
    if (ARoute <> nil) then
    begin
//      LSourceSeqNo    := ARoute.SourceModuleNo;
      LSourceModuleID := ARoute.SourceModuleID;
      LHydrologyModel := FAppModules.Model as IHydrologyModel;
      LNetworkModule  := LHydrologyModel.Network.ModuleByID[LSourceModuleID];
//      LNetworkModule  := LHydrologyModel.Network.ModuleBySequenceNumber[LSourceSeqNo];
      if (LNetworkModule <> nil) then
      begin
        LSourceModuleNo := IntToStr(LNetworkModule.ModuleNumber);
        LMasterName     := GetMasterName(LNetworkModule.ModuleType);
        Result          := FindShapeWithPropertyValue(LMasterName, cNumber, LSourceModuleNo);
      end;
      LNetworkModule := nil;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.UnQuote (AString : string) : string;
const OPNAME = 'THydroNVEventHandler.UnQuote';
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

function THydroNVEventHandler.UnMM (AString : string) : string;
const OPNAME = 'THydroNVEventHandler.UnMM';
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

function THydroNVEventHandler.UnDeg (AString : string) : string;
const OPNAME = 'THydroNVEventHandler.UnDeg';
var
  lPos : integer;
begin
  Result := AString;
  try
    lPos := Pos('deg', AString);
    if (lPos > 0) then
      AString := Trim(Copy(AString, 1, lPos - 1));
    Result := AString;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.TextTypeToStr (AType : THydroNVTextType) : string;
const OPNAME = 'THydroNVEventHandler.TextTypeToStr';
var
  LHydrologyModel : IHydrologyModel;
  LResultType     : IHydroResultType;
begin
  Result := '';
  try
    case AType of
      nvtGeneral            : Result := 'General';
      nvtName               : Result := 'Name';
      nvtObsFlowFile        : Result := 'Observed flow file';
      nvtDupCounts          : Result := 'Count of Duplicate Elements';
      nvtOutCounts          : Result := 'Count of Outstanding Elements';
      nvtDupElements        : Result := 'Duplicate Network Elements';
      nvtOutElements        : Result := 'Outstanding Network Elements';
    else
      LHydrologyModel := FAppModules.Model as IHydrologyModel;
      LResultType := LHydrologyModel.Network.HydroOutputAgent.ResultTypeByID[Ord(AType)];
      if (LResultType <> nil) then
        Result := LResultType.Description;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.ElementSubTypeToStr (AType : THydroNVElementSubType) : string;
const OPNAME = 'THydroNVEventHandler.ElementSubTypeToStr';
begin
  Result := '';
  try
    case AType of
      subNone            : Result := '';
      subPlantArea       : Result := 'Plant Area';
      subOpencast        : Result := 'Opencast Pit';
      subUnderground     : Result := 'Underground Section';
      subSlurryDump      : Result := 'Slurry Dump';
    else
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.PopulateAllReservoirLists (AIDList : TStringList; ANameList : TStringList; AInList : TStringList);
const OPNAME = 'THydroNVEventHandler.PopulateAllReservoirLists';
var
  LReservoir            : IReservoirModule;
  LReservoirModuleAgent : IReservoirModuleAgent;
  LIndex                : Integer;
  LIDStr                : String;
  LHydrologyModel       : IHydrologyModel;
begin
  try
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    LReservoirModuleAgent := LHydrologyModel.Network.ReservoirModuleAgent;
    for LIndex := 0 to LReservoirModuleAgent.ReservoirModuleCount - 1 do
    begin
      LReservoir := LReservoirModuleAgent.ReservoirModuleByIndex[LIndex];
      LIDStr     := IntToStr(LReservoir.ModuleNumber);
      if ((AInList = nil) OR (AInList.IndexOf(LIDStr) >= 0)) then
      begin
        AIDList.Add(LIDStr);
        ANameList.Add(LReservoir.ReservoirName);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.ShowSelectReservoirsDialog : integer;
const OPNAME = 'THydroNVEventHandler.ShowSelectReservoirsDialog';
var
  LReservoirIDList      : TStringList;
  LReservoirNameList    : TStringList;
  LXMLAgent             : THydroNVXMLAgent;
  LXMLDocIn             : IXMLDocument;
  LXMLDocOut            : IXMLDocument;
  LInputTxt             : String;
  LOutputTxt            : String;
  LHydrologyModel       : IHydrologyModel;
begin
  Result := -1;
  try
    LReservoirIDList   := TStringList.Create;
    LReservoirNameList := TStringList.Create;
    LXMLAgent          := THydroNVXMLAgent.Create;
    try
      PopulateAllReservoirLists(LReservoirIDList, LReservoirNameList, nil);

      LXMLDocIn  := LXMLAgent.CreateNVSelectElementInXMLDocument(LReservoirIDList, LReservoirNameList, FReservoirList);
      LXMLDocOut := LXMLAgent.CreateNVSelectElementOutXMLDocument;
      LXMLDocOut.Active := TRUE;

      LInputTxt  := LXMLDocIn.XML.Text;
      LOutputTxt := LXMLDocOut.XML.Text;

      LHydrologyModel := FAppModules.Model as IHydrologyModel;
      if (ShowNVSelectElementDlg(LHydrologyModel.MayChangeNetwork, 'Reservoir', LInputTxt, LOutputTxt)) then
      begin
        LXMLDocOut.XML.Text := LOutputTxt;
        LXMLDocOut.Active := TRUE;
        Result := StrToInt(LXMLDocOut.DocumentElement.ChildNodes['Selected'].Text);
      end;
    finally
      LXMLAgent.Free;
      LReservoirIDList.Free;
      LReservoirNameList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVEventHandler.PopulateAllRunOffLists (AIDList : TStringList; ANameList : TStringList; AInList : TStringList);
const OPNAME = 'THydroNVEventHandler.PopulateAllRunOffLists';
var
  LRunOff            : IRunOffModule;
  LRunOffModuleAgent : IRunOffModuleAgent;
  LIndex             : Integer;
  LIDStr             : String;
  LHydrologyModel    : IHydrologyModel;
begin
  try
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    LRunOffModuleAgent := LHydrologyModel.Network.RunOffModuleAgent;
    for LIndex := 0 to LRunOffModuleAgent.RunOffModuleCount - 1 do
    begin
      LRunOff := LRunOffModuleAgent.RunOffModuleByIndex[LIndex];
      LIDStr  := IntToStr(LRunOff.ModuleNumber);
      if ((AInList = nil) OR (AInList.IndexOf(LIDStr) >= 0)) then
      begin
        AIDList.Add(LIDStr);
        ANameList.Add(LRunOff.RunOffName);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVEventHandler.ShowSelectRunOffsDialog : integer;
const OPNAME = 'THydroNVEventHandler.ShowSelectRunOffsDialog';
var
  LRunOffIDList      : TStringList;
  LRunOffNameList    : TStringList;
  LXMLAgent          : THydroNVXMLAgent;
  LXMLDocIn          : IXMLDocument;
  LXMLDocOut         : IXMLDocument;
  LInputTxt          : String;
  LOutputTxt         : String;
  LHydrologyModel    : IHydrologyModel;
begin
  Result := -1;
  try
    LRunOffIDList   := TStringList.Create;
    LRunOffNameList := TStringList.Create;
    LXMLAgent       := THydroNVXMLAgent.Create;
    try
      PopulateAllRunOffLists(LRunOffIDList, LRunOffNameList, nil);

      LXMLDocIn  := LXMLAgent.CreateNVSelectElementInXMLDocument(LRunOffIDList, LRunOffNameList, FRunOffList);
      LXMLDocOut := LXMLAgent.CreateNVSelectElementOutXMLDocument;
      LXMLDocOut.Active := TRUE;

      LInputTxt  := LXMLDocIn.XML.Text;
      LOutputTxt := LXMLDocOut.XML.Text;

      LHydrologyModel := FAppModules.Model as IHydrologyModel;
      if (ShowNVSelectElementDlg(LHydrologyModel.MayChangeNetwork, 'Runoff', LInputTxt, LOutputTxt)) then
      begin
        LXMLDocOut.XML.Text := LOutputTxt;
        LXMLDocOut.Active := TRUE;
        Result := StrToInt(LXMLDocOut.DocumentElement.ChildNodes['Selected'].Text);
      end;
    finally
      LXMLAgent.Free;
      LRunOffIDList.Free;
      LRunOffNameList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVEventHandler.PopulateAllChannelLists (AIDList : TStringList; ANameList : TStringList; AInList : TStringList);
const OPNAME = 'THydroNVEventHandler.PopulateAllChannelLists';
var
  LChannel            : IChannelModule;
  LChannelModuleAgent : IChannelModuleAgent;
  LIndex              : Integer;
  LIDStr              : String;
  LHydrologyModel     : IHydrologyModel;
begin
  try
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    LChannelModuleAgent := LHydrologyModel.Network.ChannelModuleAgent;
    for LIndex := 0 to LChannelModuleAgent.ChannelModuleCount - 1 do
    begin
      LChannel := LChannelModuleAgent.ChannelModuleByIndex[LIndex];
      LIDStr   := IntToStr(LChannel.ModuleNumber);
      if ((AInList = nil) OR (AInList.IndexOf(LIDStr) >= 0)) then
      begin
        AIDList.Add(LIDStr);
        ANameList.Add(LChannel.ChannelName);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVEventHandler.ShowSelectChannelsDialog : integer;
const OPNAME = 'THydroNVEventHandler.ShowSelectChannelsDialog';
var
  LChannelIDList      : TStringList;
  LChannelNameList    : TStringList;
  LXMLAgent           : THydroNVXMLAgent;
  LXMLDocIn           : IXMLDocument;
  LXMLDocOut          : IXMLDocument;
  LInputTxt           : String;
  LOutputTxt          : String;
  LHydrologyModel     : IHydrologyModel;
begin
  Result := -1;
  try
    LChannelIDList   := TStringList.Create;
    LChannelNameList := TStringList.Create;
    LXMLAgent        := THydroNVXMLAgent.Create;
    try
      PopulateAllChannelLists(LChannelIDList, LChannelNameList, nil);

      LXMLDocIn  := LXMLAgent.CreateNVSelectElementInXMLDocument(LChannelIDList, LChannelNameList, FChannelList);
      LXMLDocOut := LXMLAgent.CreateNVSelectElementOutXMLDocument;
      LXMLDocOut.Active := TRUE;

      LInputTxt  := LXMLDocIn.XML.Text;
      LOutputTxt := LXMLDocOut.XML.Text;

      LHydrologyModel := FAppModules.Model as IHydrologyModel;
      if (ShowNVSelectElementDlg(LHydrologyModel.MayChangeNetwork, 'Channel Reach', LInputTxt, LOutputTxt)) then
      begin
        LXMLDocOut.XML.Text := LOutputTxt;
        LXMLDocOut.Active := TRUE;
        Result := StrToInt(LXMLDocOut.DocumentElement.ChildNodes['Selected'].Text);
      end;
    finally
      LXMLAgent.Free;
      LChannelIDList.Free;
      LChannelNameList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVEventHandler.PopulateAllIrrBlockLists (AIDList : TStringList; ANameList : TStringList; AInList : TStringList);
const OPNAME = 'THydroNVEventHandler.PopulateAllIrrBlockLists';
var
  LIrrBlock       : IIrrigationModule;
  LIrrModuleAgent : IIrrigationModuleAgent;
  LIndex          : Integer;
  LIDStr          : String;
  LHydrologyModel : IHydrologyModel;
begin
  try
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    LIrrModuleAgent := LHydrologyModel.Network.IrrigationModuleAgent;
    for LIndex := 0 to LIrrModuleAgent.IrrigationModuleCount - 1 do
    begin
      LIrrBlock := LIrrModuleAgent.IrrigationModuleByIndex[LIndex];
      LIDStr    := IntToStr(LIrrBlock.ModuleNumber);
      if ((AInList = nil) OR (AInList.IndexOf(LIDStr) >= 0)) then
      begin
        AIDList.Add(LIDStr);
        ANameList.Add(LIrrBlock.IrrigationName);
      end;  
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVEventHandler.ShowSelectIrrBlocksDialog : integer;
const OPNAME = 'THydroNVEventHandler.ShowSelectIrrBlocksDialog';
var
  LIrrBlockIDList      : TStringList;
  LIrrBlockNameList    : TStringList;
  LXMLAgent            : THydroNVXMLAgent;
  LXMLDocIn            : IXMLDocument;
  LXMLDocOut           : IXMLDocument;
  LInputTxt            : String;
  LOutputTxt           : String;
  LHydrologyModel      : IHydrologyModel;
begin
  Result := -1;
  try
    LIrrBlockIDList   := TStringList.Create;
    LIrrBlockNameList := TStringList.Create;
    LXMLAgent         := THydroNVXMLAgent.Create;
    try
      PopulateAllIrrBlockLists(LIrrBlockIDList, LIrrBlockNameList, nil);

      LXMLDocIn  := LXMLAgent.CreateNVSelectElementInXMLDocument(LIrrBlockIDList, LIrrBlockNameList, FIrrBlockList);
      LXMLDocOut := LXMLAgent.CreateNVSelectElementOutXMLDocument;
      LXMLDocOut.Active := TRUE;

      LInputTxt  := LXMLDocIn.XML.Text;
      LOutputTxt := LXMLDocOut.XML.Text;

      LHydrologyModel := FAppModules.Model as IHydrologyModel;
      if (ShowNVSelectElementDlg(LHydrologyModel.MayChangeNetwork, 'Irrigation Block', LInputTxt, LOutputTxt)) then
      begin
        LXMLDocOut.XML.Text := LOutputTxt;
        LXMLDocOut.Active := TRUE;
        Result := StrToInt(LXMLDocOut.DocumentElement.ChildNodes['Selected'].Text);
      end;
    finally
      LXMLAgent.Free;
      LIrrBlockIDList.Free;
      LIrrBlockNameList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVEventHandler.PopulateAllMineLists (AIDList : TStringList; ANameList : TStringList; AInList : TStringList);
const OPNAME = 'THydroNVEventHandler.PopulateAllMineLists';
var
  LMine            : IMineModule;
  LMineModuleAgent : IMineModuleAgent;
  LIndex           : Integer;
  LIDStr           : String;
  LHydrologyModel  : IHydrologyModel;
begin
  try
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    LMineModuleAgent := LHydrologyModel.Network.MineModuleAgent;
    for LIndex := 0 to LMineModuleAgent.MineModuleCount - 1 do
    begin
      LMine := LMineModuleAgent.MineModuleByIndex[LIndex];
      LIDStr := IntToStr(LMine.ModuleNumber);
      if ((AInList = nil) OR (AInList.IndexOf(LIDStr) >= 0)) then
      begin
        AIDList.Add(LIDStr);
        ANameList.Add(LMine.MineName);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVEventHandler.ShowSelectMinesDialog : integer;
const OPNAME = 'THydroNVEventHandler.ShowSelectMinesDialog';
var
  LMineIDList      : TStringList;
  LMineNameList    : TStringList;
  LXMLAgent        : THydroNVXMLAgent;
  LXMLDocIn        : IXMLDocument;
  LXMLDocOut       : IXMLDocument;
  LInputTxt        : String;
  LOutputTxt       : String;
  LHydrologyModel  : IHydrologyModel;
begin
  Result := -1;
  try
    LMineIDList   := TStringList.Create;
    LMineNameList := TStringList.Create;
    LXMLAgent     := THydroNVXMLAgent.Create;
    try
      PopulateAllMineLists(LMineIDList, LMineNameList, nil);

      LXMLDocIn  := LXMLAgent.CreateNVSelectElementInXMLDocument(LMineIDList, LMineNameList, FMineList);
      LXMLDocOut := LXMLAgent.CreateNVSelectElementOutXMLDocument;
      LXMLDocOut.Active := TRUE;

      LInputTxt  := LXMLDocIn.XML.Text;
      LOutputTxt := LXMLDocOut.XML.Text;

      LHydrologyModel := FAppModules.Model as IHydrologyModel;
      if (ShowNVSelectElementDlg(LHydrologyModel.MayChangeNetwork, 'Mine', LInputTxt, LOutputTxt)) then
      begin
        LXMLDocOut.XML.Text := LOutputTxt;
        LXMLDocOut.Active := TRUE;
        Result := StrToInt(LXMLDocOut.DocumentElement.ChildNodes['Selected'].Text);
      end;
    finally
      LXMLAgent.Free;
      LMineIDList.Free;
      LMineNameList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVEventHandler.PopulateAllRouteLists (AIDList : TStringList; ANameList : TStringList; AInList : TStringList);
const OPNAME = 'THydroNVEventHandler.PopulateAllRouteLists';
var
  LRoute              : INetworkRoute;
  LNetworkRouteAgent  : INetworkRouteAgent;
  LIndex              : Integer;
  LIDStr              : String;
  LHydrologyModel     : IHydrologyModel;
begin
  try
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    LNetworkRouteAgent := LHydrologyModel.Network.NetworkRouteAgent;
    for LIndex := 0 to LNetworkRouteAgent.NetworkRouteCount - 1 do
    begin
      LRoute := LNetworkRouteAgent.NetworkRouteByIndex[LIndex];
      LIDStr := IntToStr(LRoute.RouteNo);
      if ((AInList = nil) OR (AInList.IndexOf(LIDStr) >= 0)) then
      begin
        AIDList.Add(LIDStr);
        ANameList.Add('');
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVEventHandler.ShowSelectRoutesDialog : integer;
const OPNAME = 'THydroNVEventHandler.ShowSelectRoutesDialog';
var
  LRouteIDList        : TStringList;
  LRouteNameList      : TStringList;
  LXMLAgent           : THydroNVXMLAgent;
  LXMLDocIn           : IXMLDocument;
  LXMLDocOut          : IXMLDocument;
  LInputTxt           : String;
  LOutputTxt          : String;
  LHydrologyModel     : IHydrologyModel;
begin
  Result := -1;
  try
    LRouteIDList   := TStringList.Create;
    LRouteNameList := TStringList.Create;
    LXMLAgent      := THydroNVXMLAgent.Create;
    try
      PopulateAllRouteLists(LRouteIDList, LRouteNameList, nil);

      LXMLDocIn  := LXMLAgent.CreateNVSelectElementInXMLDocument(LRouteIDList, LRouteNameList, FRouteList);
      LXMLDocOut := LXMLAgent.CreateNVSelectElementOutXMLDocument;
      LXMLDocOut.Active := TRUE;

      LInputTxt  := LXMLDocIn.XML.Text;
      LOutputTxt := LXMLDocOut.XML.Text;

      LHydrologyModel := FAppModules.Model as IHydrologyModel;
      if (ShowNVSelectElementDlg(LHydrologyModel.MayChangeNetwork, 'Network Route', LInputTxt, LOutputTxt)) then
      begin
        LXMLDocOut.XML.Text := LOutputTxt;
        LXMLDocOut.Active := TRUE;
        Result := StrToInt(LXMLDocOut.DocumentElement.ChildNodes['Selected'].Text);
      end;
    finally
      LXMLAgent.Free;
      LRouteIDList.Free;
      LRouteNameList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVEventHandler.PopulateAllObsPointLists (AIDList   : TStringList;
                                                         ANameList : TStringList;
                                                         ANewList  : TStringList;
                                                         AInList   : TStringList);
const OPNAME = 'THydroNVEventHandler.PopulateAllObsPointLists';
var
  LObsPoint       : IObservationPoint;
  LObsPointAgent  : IObservationPointAgent;
  LRoute          : INetworkRoute;
  LRouteAgent     : INetworkRouteAgent;
  LIndex          : Integer;
  LIDStr          : String;
  LHydrologyModel : IHydrologyModel;
begin
  try
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    LObsPointAgent  := LHydrologyModel.Network.ObservationPointAgent;
    LRouteAgent     := LHydrologyModel.Network.NetworkRouteAgent;

    for LIndex := 0 to LObsPointAgent.ObservationPointCount - 1 do
    begin
      LObsPoint := LObsPointAgent.ObservationPointByIndex[LIndex];
      LIDStr    := IntToStr(LObsPoint.RouteNo);
      if ((AInList = nil) OR (AInList.IndexOf(LIDStr) >= 0)) then
      begin
        AIDList.Add(IntToStr(LObsPoint.RouteNo));
        ANameList.Add(LObsPoint.Name);
      end;
    end;

    if ((ANewList <> nil) AND (AInList <> nil)) then
    begin
      for LIndex := 0 to LRouteAgent.NetworkRouteCount - 1 do
      begin
        LRoute    := LRouteAgent.NetworkRouteByIndex[LIndex];
        LIDStr    := IntToStr(LRoute.RouteNo);
        LObsPoint := LObsPointAgent.ObservationPointByRouteNo[LRoute.RouteNo];
        if ((LObsPoint = nil) AND (AInList.IndexOf(LIDStr) >= 0)) then
          ANewList.Add(IntToStr(LRoute.RouteNo));
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVEventHandler.ShowSelectObsPointsDialog : integer;
const OPNAME = 'THydroNVEventHandler.ShowSelectObsPointsDialog';
var
  LObsPointIDList     : TStringList;
  LObsPointNameList   : TStringList;
  LNewList            : TStringList;
  LXMLAgent           : THydroNVXMLAgent;
  LXMLDocIn           : IXMLDocument;
  LXMLDocOut          : IXMLDocument;
  LInputTxt           : String;
  LOutputTxt          : String;
  LHydrologyModel     : IHydrologyModel;
begin
  Result := -1;
  try
    LObsPointIDList   := TStringList.Create;
    LObsPointNameList := TStringList.Create;
    LNewList          := TStringList.Create;
    LXMLAgent         := THydroNVXMLAgent.Create;
    try
      PopulateAllObsPointLists(LObsPointIDList, LObsPointNameList, LNewList, FRouteList);

      LXMLDocIn  := LXMLAgent.CreateNVSelectObsPointInXMLDocument(LObsPointIDList, LObsPointNameList, LNewList, FObsPointList);
      LXMLDocOut := LXMLAgent.CreateNVSelectElementOutXMLDocument;
      LXMLDocOut.Active := TRUE;

      LInputTxt  := LXMLDocIn.XML.Text;
      LOutputTxt := LXMLDocOut.XML.Text;

      LHydrologyModel := FAppModules.Model as IHydrologyModel;
      if (ShowNVSelectElementDlg(LHydrologyModel.MayChangeNetwork, 'Observation Point', LInputTxt, LOutputTxt)) then
      begin
        LXMLDocOut.XML.Text := LOutputTxt;
        LXMLDocOut.Active := TRUE;
        Result := StrToInt(LXMLDocOut.DocumentElement.ChildNodes['Selected'].Text);
      end;
    finally
      LXMLAgent.Free;
      LObsPointIDList.Free;
      LObsPointNameList.Free;
      LNewList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVEventHandler.ShowNVOutputSelectorDlg (var AInterval : Integer) : Boolean;
const OPNAME = 'THydroNVEventHandler.ShowNVOutputSelectorDlg';
var
  LHydrologyModel : IHydrologyModel;
begin
  Result := FALSE;
  try
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    FNVOutputSelectorDlg := THydroNVOutputSelectorDlg.Create(nil);
    try
      FNVOutputSelectorDlg.Interval            := AInterval;
      FNVOutputSelectorDlg.LblInterval.Caption := 'Interval : ' + LHydrologyModel.Network.IntervalText[AInterval];
      FNVOutputSelectorDlg.OnFirstInterval     := FirstIntervalClick;
      FNVOutputSelectorDlg.OnMinus12Intervals  := Minus12IntervalsClick;
      FNVOutputSelectorDlg.OnPreviousInterval  := PreviousIntervalClick;
      FNVOutputSelectorDlg.OnNextInterval      := NextIntervalClick;
      FNVOutputSelectorDlg.OnPlus12Intervals   := Plus12IntervalsClick;
      FNVOutputSelectorDlg.OnLastInterval      := LastIntervalClick;
      Result := (FNVOutputSelectorDlg.ShowModal = mrOK);
    finally
      FreeAndNil(FNVOutputSelectorDlg);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.FirstIntervalClick (Sender: TObject);
const OPNAME = 'THydroNVEventHandler.FirstIntervalClick';
var
  LHydrologyModel : IHydrologyModel;
begin
  try
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    if (Assigned(FNVOutputSelectorDlg)) then
    begin
      FInterval := 0;
      FNVOutputSelectorDlg.Interval := FInterval;
      FNVOutputSelectorDlg.LblInterval.Caption := 'Interval : ' + LHydrologyModel.Network.IntervalText[FInterval];
      RefreshOutputSelector(FOutputSelector);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.Minus12IntervalsClick(Sender: TObject);
const OPNAME = 'THydroNVEventHandler.Minus12IntervalsClick';
var
  LHydrologyModel : IHydrologyModel;
begin
  try
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    if (Assigned(FNVOutputSelectorDlg)) then
    begin
      FInterval := FInterval - 12;
      if (FInterval < 0) then
        FInterval := 0;
      FNVOutputSelectorDlg.Interval := FInterval;
      FNVOutputSelectorDlg.LblInterval.Caption := 'Interval : ' + LHydrologyModel.Network.IntervalText[FInterval];
      RefreshOutputSelector(FOutputSelector);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.PreviousIntervalClick(Sender: TObject);
const OPNAME = 'THydroNVEventHandler.FirstIntervalClick';
var
  LHydrologyModel : IHydrologyModel;
begin
  try
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    if (Assigned(FNVOutputSelectorDlg)) then
    begin
      FInterval := FInterval - 1;
      if (FInterval < 0) then
        FInterval := 0;
      FNVOutputSelectorDlg.Interval := FInterval;
      FNVOutputSelectorDlg.LblInterval.Caption := 'Interval : ' + LHydrologyModel.Network.IntervalText[FInterval];
      RefreshOutputSelector(FOutputSelector);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.NextIntervalClick(Sender: TObject);
const OPNAME = 'THydroNVEventHandler.NextIntervalClick';
var
  LHydrologyModel : IHydrologyModel;
  LNoOfIntervals  : Integer;
begin
  try
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    LNoOfIntervals  := LHydrologyModel.Network.NoOfIntervals;
    if (Assigned(FNVOutputSelectorDlg)) then
    begin
      FInterval := FInterval + 1;
      if (FInterval >= LNoOfIntervals) then
        FInterval := LNoOfIntervals - 1;
      FNVOutputSelectorDlg.Interval := FInterval;
      FNVOutputSelectorDlg.LblInterval.Caption := 'Interval : ' + LHydrologyModel.Network.IntervalText[FInterval];
      RefreshOutputSelector(FOutputSelector);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.Plus12IntervalsClick(Sender: TObject);
const OPNAME = 'THydroNVEventHandler.FirstIntervalClick';
var
  LHydrologyModel : IHydrologyModel;
  LNoOfIntervals  : Integer;
begin
  try
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    LNoOfIntervals  := LHydrologyModel.Network.NoOfIntervals;
    if (Assigned(FNVOutputSelectorDlg)) then
    begin
      FInterval := FInterval + 12;
      if (FInterval >= LNoOfIntervals) then
        FInterval := LNoOfIntervals - 1;
      FNVOutputSelectorDlg.Interval := FInterval;
      FNVOutputSelectorDlg.LblInterval.Caption := 'Interval : ' + LHydrologyModel.Network.IntervalText[FInterval];
      RefreshOutputSelector(FOutputSelector);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.LastIntervalClick(Sender: TObject);
const OPNAME = 'THydroNVEventHandler.FirstIntervalClick';
var
  LHydrologyModel : IHydrologyModel;
  LNoOfIntervals  : Integer;
begin
  try
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    LNoOfIntervals  := LHydrologyModel.Network.NoOfIntervals;
    if (Assigned(FNVOutputSelectorDlg)) then
    begin
      FInterval := LNoOfIntervals - 1;
      FNVOutputSelectorDlg.Interval := FInterval;
      FNVOutputSelectorDlg.LblInterval.Caption := 'Interval : ' + LHydrologyModel.Network.IntervalText[FInterval];
      RefreshOutputSelector(FOutputSelector);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{
procedure THydroNVEventHandler.PopulateTextTypeLists (ATypeIDList   : TStringList;
                                                      ATypeNameList : TStringList;
                                                      ATypeSet      : THydroNVTextTypeSet);
const OPNAME = 'THydroNVEventHandler.PopulateTextTypeLists';
var
  LTextType : THydroNVTextType;
begin
  try
    ATypeIDList.Clear;
    ATypeNameList.Clear;
    for LTextType := Low(THydroNVTextType) to High(THydroNVTextType) do
    begin
      if (LTextType in ATypeSet) then
      begin
        ATypeIDList.Add(IntToStr(Ord(LTextType)));
        ATypeNameList.ADD(TextTypeToStr(LTextType));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;
}

procedure THydroNVEventHandler.GenerateNetworkTextXML (AXMLDocument : IXMLDocument;
                                                       AXMLAgent    : THydroNVXMLAgent);
const OPNAME = 'THydroNVEventHandler.GenerateNetworkTextXML';
var
  LElementsList   : TStringList;
  LTextTypesList  : TStringList;
  LHydrologyModel : IHydrologyModel;
  LNetwork        : INetwork;
  LTempText       : String;
  LTextType       : THydroNVTextType;
  LNetworkText    : THydroNVTextTypeSet;
begin
  try
    LNetworkText   := [nvtName, nvtDupCounts, nvtDupElements, nvtOutCounts, nvtOutElements];
    LElementsList  := TStringList.Create;
    LTextTypesList := TStringList.Create;
    try
      LHydrologyModel := FAppModules.Model as IHydrologyModel;
      LNetwork        := LHydrologyModel.Network;
      LTempText       := IntToStr(LNetwork.NetworkID) + ',"' + LNetwork.NetworkCode + '"';
      LElementsList.Add(LTempText);

      for LTextType := Low(THydroNVTextType) to High(THydroNVTextType) do
      begin
        if (LTextType in LNetworkText) then
        begin
          LTempText := IntToStr(Ord(LTextType)) + ',"' + TextTypeToStr(LTextType) + '"';
          LTextTypesList.Add(LTempText);
        end;
      end;
      AXMLAgent.AddNVSelectTextInElementType(AXMLDocument, mtHydroNetwork, nil, LElementsList, LTextTypesList);
    finally
      LElementsList.Free;
      LTextTypesList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVEventHandler.GenerateReservoirTextXML (AXMLDocument : IXMLDocument;
                                                         AXMLAgent    : THydroNVXMLAgent);
const OPNAME = 'THydroNVEventHandler.GenerateReservoirTextXML';
var
  LReservoir            : IReservoirModule;
  LReservoirModuleAgent : IReservoirModuleAgent;
  LElementsList         : TStringList;
  LTextTypesList        : TStringList;
  LHydrologyModel       : IHydrologyModel;
  LTempText             : String;
  LTextType             : THydroNVTextType;
  LTextTypeSet          : THydroNVTextTypeSet;
  LIndex                : Integer;
  LElementNo            : Integer;
begin
  try
    LTextTypeSet   := [nvtGeneral, nvtName, nvtRVReservoirStorage];
    LElementsList  := TStringList.Create;
    LTextTypesList := TStringList.Create;
    try
      LHydrologyModel := FAppModules.Model as IHydrologyModel;
      LReservoirModuleAgent := LHydrologyModel.Network.ReservoirModuleAgent;

      for LTextType := Low(THydroNVTextType) to High(THydroNVTextType) do
      begin
        if (LTextType in LTextTypeSet) then
        begin
          LTempText := IntToStr(Ord(LTextType)) + ',"' + TextTypeToStr(LTextType) + '"';
          LTextTypesList.Add(LTempText);
        end;
      end;

      for LIndex := 0 to FReservoirList.Count - 1 do
      begin
        LElementNo := StrToInt(FReservoirList.Strings[LIndex]);
        LReservoir := LReservoirModuleAgent.ReservoirModuleByNumber[LElementNo];
        LTempText  := IntToStr(LElementNo) + ',"' + LReservoir.ReservoirName + '"';
        LElementsList.Add(LTempText);
      end;
      AXMLAgent.AddNVSelectTextInElementType(AXMLDocument, mtHydroReservoir, nil, LElementsList, LTextTypesList);
    finally
      LElementsList.Free;
      LTextTypesList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVEventHandler.GenerateRunOffTextXML (AXMLDocument : IXMLDocument;
                                                      AXMLAgent    : THydroNVXMLAgent);
const OPNAME = 'THydroNVEventHandler.GenerateRunOffTextXML';
var
  LRunOff               : IRunOffModule;
  LRunOffModuleAgent    : IRunOffModuleAgent;
  LElementsList         : TStringList;
  LTextTypesList        : TStringList;
  LHydrologyModel       : IHydrologyModel;
  LTempText             : String;
  LTextType             : THydroNVTextType;
  LTextTypeSet          : THydroNVTextTypeSet;
  LIndex                : Integer;
  LElementNo            : Integer;
begin
  try
    LTextTypeSet   := [nvtGeneral, nvtName, nvtRUNetCatchmentRunOff, nvtRUTotalSurfaceRunOff, nvtRUGroundWaterOutFlow,
                       nvtRUPavedAreaFlow, nvtRUPitmanS, nvtRUAquiferStorage, nvtRUGroundWaterRecharge,
                       nvtRUWeightedPitmanS, nvtRUGroundWaterBaseFlow, nvtRUInterflow];
    LElementsList  := TStringList.Create;
    LTextTypesList := TStringList.Create;
    try
      LHydrologyModel := FAppModules.Model as IHydrologyModel;
      LRunOffModuleAgent := LHydrologyModel.Network.RunOffModuleAgent;

      for LTextType := Low(THydroNVTextType) to High(THydroNVTextType) do
      begin
        if (LTextType in LTextTypeSet) then
        begin
          LTempText := IntToStr(Ord(LTextType)) + ',"' + TextTypeToStr(LTextType) + '"';
          LTextTypesList.Add(LTempText);
        end;
      end;

      for LIndex := 0 to FRunOffList.Count - 1 do
      begin
        LElementNo := StrToInt(FRunOffList.Strings[LIndex]);
        LRunOff    := LRunOffModuleAgent.RunOffModuleByNumber[LElementNo];
        LTempText  := IntToStr(LElementNo) + ',"' + LRunOff.RunOffName + '"';
        LElementsList.Add(LTempText);
      end;
      AXMLAgent.AddNVSelectTextInElementType(AXMLDocument, mtHydroRunOff, nil, LElementsList, LTextTypesList);
    finally
      LElementsList.Free;
      LTextTypesList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVEventHandler.GenerateChannelTextXML (AXMLDocument : IXMLDocument;
                                                       AXMLAgent    : THydroNVXMLAgent);
const OPNAME = 'THydroNVEventHandler.GenerateChannelTextXML';
var
  LChannel              : IChannelModule;
  LChannelModuleAgent   : IChannelModuleAgent;
  LElementsList         : TStringList;
  LTextTypesList        : TStringList;
  LHydrologyModel       : IHydrologyModel;
  LTempText             : String;
  LTextType             : THydroNVTextType;
  LTextTypeSet          : THydroNVTextTypeSet;
  LIndex                : Integer;
  LElementNo            : Integer;
begin
  try
    LTextTypeSet   := [nvtGeneral, nvtName, nvtCRWetlandUpstreamFlow, nvtCRWetlandInFlow,
                       nvtCRWetlandStorage, nvtCRWetlandReturnFlow];
    LElementsList  := TStringList.Create;
    LTextTypesList := TStringList.Create;
    try
      LHydrologyModel := FAppModules.Model as IHydrologyModel;
      LChannelModuleAgent := LHydrologyModel.Network.ChannelModuleAgent;

      for LTextType := Low(THydroNVTextType) to High(THydroNVTextType) do
      begin
        if (LTextType in LTextTypeSet) then
        begin
          LTempText := IntToStr(Ord(LTextType)) + ',"' + TextTypeToStr(LTextType) + '"';
          LTextTypesList.Add(LTempText);
        end;
      end;

      for LIndex := 0 to FChannelList.Count - 1 do
      begin
        LElementNo := StrToInt(FChannelList.Strings[LIndex]);
        LChannel    := LChannelModuleAgent.ChannelModuleByNumber[LElementNo];
        LTempText  := IntToStr(LElementNo) + ',"' + LChannel.ChannelName + '"';
        LElementsList.Add(LTempText);
      end;
      AXMLAgent.AddNVSelectTextInElementType(AXMLDocument, mtHydroChannel, nil, LElementsList, LTextTypesList);
    finally
      LElementsList.Free;
      LTextTypesList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVEventHandler.GenerateIrrBlockTextXML (AXMLDocument : IXMLDocument;
                                                        AXMLAgent    : THydroNVXMLAgent);
const OPNAME = 'THydroNVEventHandler.GenerateIrrBlockTextXML';
var
  LIrrBlock             : IIrrigationModule;
  LIrrBlockModuleAgent  : IIrrigationModuleAgent;
  LElementsList         : TStringList;
  LTextTypesList        : TStringList;
  LHydrologyModel       : IHydrologyModel;
  LTempText             : String;
  LTextType             : THydroNVTextType;
  LTextTypeSet          : THydroNVTextTypeSet;
  LIndex                : Integer;
  LElementNo            : Integer;
begin
  try
    LTextTypeSet   := [nvtGeneral, nvtName];
    LElementsList  := TStringList.Create;
    LTextTypesList := TStringList.Create;
    try
      LHydrologyModel := FAppModules.Model as IHydrologyModel;
      LIrrBlockModuleAgent := LHydrologyModel.Network.IrrigationModuleAgent;

      for LTextType := Low(THydroNVTextType) to High(THydroNVTextType) do
      begin
        if (LTextType in LTextTypeSet) then
        begin
          LTempText := IntToStr(Ord(LTextType)) + ',"' + TextTypeToStr(LTextType) + '"';
          LTextTypesList.Add(LTempText);
        end;
      end;

      for LIndex := 0 to FIrrBlockList.Count - 1 do
      begin
        LElementNo := StrToInt(FIrrBlockList.Strings[LIndex]);
        LIrrBlock  := LIrrBlockModuleAgent.IrrigationModuleByNumber[LElementNo];
        LTempText  := IntToStr(LElementNo) + ',"' + LIrrBlock.IrrigationName + '"';
        LElementsList.Add(LTempText);
      end;
      AXMLAgent.AddNVSelectTextInElementType(AXMLDocument, mtHydroIrrBlock, nil, LElementsList, LTextTypesList);
    finally
      LElementsList.Free;
      LTextTypesList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVEventHandler.GenerateMineTextXML (AXMLDocument : IXMLDocument;
                                                    AXMLAgent    : THydroNVXMLAgent);
const OPNAME = 'THydroNVEventHandler.GenerateMineTextXML';
var
  LMine            : IMineModule;
  LMineModuleAgent : IMineModuleAgent;
  LIndex           : Integer;
  LCount           : Integer;
  LElementNo       : Integer;
  LHydrologyModel  : IHydrologyModel;
  LElementsList    : TStringList;
  LSubElementsList : TStringList;
  LSubTypesList    : TStringList;
  LTextTypesList   : TStringList;
  LTempText        : String;
  LOpencastPit     : IOpencastPit;
  LUnderground     : IUndergroundSection;
  LSlurryDump      : ISlurryDump;
  LTextType        : THydroNVTextType;
  LOpencastText    : THydroNVTextTypeSet;
  LUndergroundText : THydroNVTextTypeSet;
  LSlurryText      : THydroNVTextTypeSet;
  LElementSubType  : THydroNVElementSubType;
begin
  try
    LUndergroundText := [nvtMMUGUpStreamAreaRunOff,
                         nvtMMUGRecharge, nvtMMUGBoardPillarRunOff, nvtMMUGHighExtractionRunOff];
    LSlurryText      := [nvtMMSDSurfaceRunOff,
                         nvtMMSDSeepage, nvtMMSDPCDInFlow, nvtMMSDPCDStorage, nvtMMSDPCDSpillage];
    LOpencastText    := [nvtMMOCDisturbedAreaRunOff, nvtMMOCDisturbedAreaRecharge, nvtMMOCWorkingAreaRunOff, nvtMMOCDisturbedWorkingsRecharge,
                         nvtMMOCDisturbedWorkingsRunOff, nvtMMOCDisturbedWorkingsSeepage, nvtMMOCDisturbedWorkingsDecant, nvtMMOCPCDSpillage,
                         nvtMMOCPCDWaterBalance, nvtMMOCPCDMonthStartStorage, nvtMMOCPCDMonthEndStorage, nvtMMOCInspoilStorage];
    LHydrologyModel  := FAppModules.Model as IHydrologyModel;
    LMineModuleAgent := LHydrologyModel.Network.MineModuleAgent;
    LSubTypesList    := TStringList.Create;
    LElementsList    := TStringList.Create;
    LTextTypesList   := TStringList.Create;
    try
      // Information for sub-types.
{
      LTempText := '1,"Plant Area"';
      LSubTypesList.Add(LTempText);
      LTempText := '2,"Opencast Pit"';
      LSubTypesList.Add(LTempText);
      LTempText := '3,"Underground Section"';
      LSubTypesList.Add(LTempText);
      LTempText := '4,"Slurry Dump"';
      LSubTypesList.Add(LTempText);
}
      for LElementSubType := Low(THydroNVElementSubType) to High(THydroNVElementSubType) do
      begin
        if (LElementSubType in [subPlantArea, subOpencast, subUnderground, subSlurryDump]) then
        begin
          LTempText := IntToStr(Ord(LElementSubType)) + ',"' + ElementSubTypeToStr(LElementSubType) + '"';
          LSubTypesList.Add(LTempText);
        end;
      end;
      
      for LTextType := Low(THydroNVTextType) to High(THydroNVTextType) do
      begin
        LElementSubType := subNone;
        if (LTextType in [nvtGeneral, nvtName]) then
          LElementSubType := subNone
        else if (LTextType in [nvtMMPlantRunOff]) then
          LElementSubType := subPlantArea
        else if (LTextType in LOpencastText) then
          LElementSubType := subOpencast
        else if (LTextType in LUndergroundText) then
          LElementSubType := subUnderground
        else if (LTextType in LSlurryText) then
          LElementSubType := subSlurryDump;
        if (LElementSubType <> subNone) then
        begin
          LTempText := IntToStr(Ord(LElementSubType)) + ',' + IntToStr(Ord(LTextType)) + ',"' + TextTypeToStr(LTextType) + '"';
          LTextTypesList.Add(LTempText);
        end;
      end;

      for LIndex := 0 to FMineList.Count - 1 do
      begin
        LSubElementsList := TStringList.Create;
        LElementNo := StrToInt(FMineList.Strings[LIndex]);
        LMine      := LMineModuleAgent.MineModuleByNumber[LElementNo];
        LTempText  := IntToStr(LElementNo) + ',"' + LMine.MineName + '"';
        LElementsList.AddObject(LTempText, LSubElementsList);
        LElementSubType := subPlantArea;
        LTempText := IntToStr(Ord(LElementSubType)) + ',1,"Plant Area"';
        LSubElementsList.Add(LTempText);
        if (LMine.NoOfOpencastPits > 0) then
        begin
          LElementSubType := subOpencast;
          for LCount := 0 to LMine.NoOfOpencastPits - 1 do
          begin
            LOpencastPit := LMine.OpencastPitByIndex[LCount];
            LTempText    := IntToStr(Ord(LElementSubType)) + ',' + IntToStr(LOpencastPit.SectionNo) + ',"' + LOpencastPit.SectionName + '"';
            LSubElementsList.Add(LTempText);
          end;
        end;
        if (LMine.NoOfUndergroundSections > 0) then
        begin
          LElementSubType := subUnderground;
          for LCount := 0 to LMine.NoOfUndergroundSections - 1 do
          begin
            LUnderground := LMine.UndergroundSectionByIndex[LCount];
            LTempText    := IntToStr(Ord(LElementSubType)) + ',' + IntToStr(LUnderground.SectionNo) + ',"' + LUnderground.SectionName + '"';
            LSubElementsList.Add(LTempText);
          end;
        end;
        if (LMine.NoOfSlurryDumps > 0) then
        begin
          LElementSubType := subSlurryDump;
          for LCount := 0 to LMine.NoOfSlurryDumps - 1 do
          begin
            LSlurryDump  := LMine.SlurryDumpByIndex[LCount];
            LTempText    := IntToStr(Ord(LElementSubType)) + ',' + IntToStr(LSlurryDump.SectionNo) + ',"' + LSlurryDump.SectionName+ '"';
            LSubElementsList.Add(LTempText);
          end;
        end;
      end;
      AXMLAgent.AddNVSelectTextInElementType(AXMLDocument, mtHydroMine, LSubTypesList, LElementsList, LTextTypesList);
    finally
     LElementsList.Free;
     LTextTypesList.Free;
     LSubTypesList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVEventHandler.GenerateNetworkRouteTextXML (AXMLDocument : IXMLDocument;
                                                            AXMLAgent    : THydroNVXMLAgent);
const OPNAME = 'THydroNVEventHandler.GenerateNetworkRouteTextXML';
var
  LNetworkRoute      : INetworkRoute;
  LNetworkRouteAgent : INetworkRouteAgent;
  LElementsList      : TStringList;
  LTextTypesList     : TStringList;
  LHydrologyModel    : IHydrologyModel;
  LTempText          : String;
  LTextType          : THydroNVTextType;
  LTextTypeSet       : THydroNVTextTypeSet;
  LIndex             : Integer;
  LElementNo         : Integer;
begin
  try
    LTextTypeSet   := [nvtGeneral, nvtRQSimulatedRouteFlow, nvtRQDemands, nvtRQShortages];
    LElementsList  := TStringList.Create;
    LTextTypesList := TStringList.Create;
    try
      LHydrologyModel := FAppModules.Model as IHydrologyModel;
      LNetworkRouteAgent := LHydrologyModel.Network.NetworkRouteAgent;

      for LTextType := Low(THydroNVTextType) to High(THydroNVTextType) do
      begin
        if (LTextType in LTextTypeSet) then
        begin
          LTempText := IntToStr(Ord(LTextType)) + ',"' + TextTypeToStr(LTextType) + '"';
          LTextTypesList.Add(LTempText);
        end;
      end;

      for LIndex := 0 to FRouteList.Count - 1 do
      begin
        LElementNo    := StrToInt(FRouteList.Strings[LIndex]);
        LNetworkRoute := LNetworkRouteAgent.NetworkRouteByRouteNo[LElementNo];
        LTempText     := IntToStr(LElementNo) + ',""';
        LElementsList.Add(LTempText);
      end;
      AXMLAgent.AddNVSelectTextInElementType(AXMLDocument, mtHydroRoute, nil, LElementsList, LTextTypesList);
    finally
      LElementsList.Free;
      LTextTypesList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVEventHandler.GenerateObsPointTextXML (AXMLDocument : IXMLDocument;
                                                        AXMLAgent    : THydroNVXMLAgent);
const OPNAME = 'THydroNVEventHandler.GenerateObsPointTextXML';
var
  LObservationPoint      : IObservationPoint;
  LObservationPointAgent : IObservationPointAgent;
  LElementsList          : TStringList;
  LTextTypesList         : TStringList;
  LHydrologyModel        : IHydrologyModel;
  LTempText              : String;
  LTextType              : THydroNVTextType;
  LTextTypeSet           : THydroNVTextTypeSet;
  LIndex                 : Integer;
  LElementNo             : Integer;
begin
  try
    LTextTypeSet   := [nvtGeneral, nvtName, nvtObsFlowFile];
    LElementsList  := TStringList.Create;
    LTextTypesList := TStringList.Create;
    try
      LHydrologyModel := FAppModules.Model as IHydrologyModel;
      LObservationPointAgent := LHydrologyModel.Network.ObservationPointAgent;

      for LTextType := Low(THydroNVTextType) to High(THydroNVTextType) do
      begin
        if (LTextType in LTextTypeSet) then
        begin
          LTempText := IntToStr(Ord(LTextType)) + ',"' + TextTypeToStr(LTextType) + '"';
          LTextTypesList.Add(LTempText);
        end;
      end;

      for LIndex := 0 to FObsPointList.Count - 1 do
      begin
        LElementNo        := StrToInt(FObsPointList.Strings[LIndex]);
        LObservationPoint := LObservationPointAgent.ObservationPointByRouteNo[LElementNo];
        LTempText         := IntToStr(LElementNo) + ',"' + LObservationPoint.Name + '"';
        LElementsList.Add(LTempText);
      end;
      AXMLAgent.AddNVSelectTextInElementType(AXMLDocument, mtHydroObsPoint, nil, LElementsList, LTextTypesList);
    finally
      LElementsList.Free;
      LTextTypesList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVEventHandler.ShowSelectTextDialog  (var AElementType    : string;
                                                     var AElementNo      : Integer;
                                                     var ATextType       : THydroNVTextType;
                                                     var AElementSubType : THydroNVElementSubType;
                                                     var ASectionNo      : Integer): Boolean;
const OPNAME = 'THydroNVEventHandler.ShowSelectTextDialog';
var
  LXMLAgent        : THydroNVXMLAgent;
  LXMLDocIn        : IXMLDocument;
  LXMLDocOut       : IXMLDocument;
  LInputTxt        : String;
  LOutputTxt       : String;
  LHydrologyModel  : IHydrologyModel;
begin
  Result := FALSE;
  try
    LXMLAgent := THydroNVXMLAgent.Create;
    try
      LHydrologyModel := FAppModules.Model as IHydrologyModel;
      LXMLDocIn  := LXMLAgent.CreateNVSelectTextInXMLDocument;

      GenerateNetworkTextXML(LXMLDocIn, LXMLAgent);
      GenerateReservoirTextXML(LXMLDocIn, LXMLAgent);
      GenerateRunOffTextXML(LXMLDocIn, LXMLAgent);
      GenerateChannelTextXML(LXMLDocIn, LXMLAgent);
      GenerateIrrBlockTextXML(LXMLDocIn, LXMLAgent);
      GenerateMineTextXML(LXMLDocIn, LXMLAgent);
      GenerateNetworkRouteTextXML(LXMLDocIn, LXMLAgent);
      GenerateObsPointTextXML(LXMLDocIn, LXMLAgent);

      LXMLDocOut := LXMLAgent.CreateNVSelectTextOutXMLDocument;
      LXMLDocOut.Active := TRUE;

      LInputTxt  := LXMLDocIn.XML.Text;
      LOutputTxt := LXMLDocOut.XML.Text;

      if (ShowNVSelectTextDlg(LInputTxt, LOutputTxt)) then
      begin
        LXMLDocOut.XML.Text := LOutputTxt;
        LXMLDocOut.Active := TRUE;
        AElementType := LXMLDocOut.DocumentElement.ChildNodes['ElementType'].Text;
        if (AElementType <> '') then
        begin
          AElementNo      := StrToInt(LXMLDocOut.DocumentElement.ChildNodes['ElementNo'].Text);
          ATextType       := THydroNVTextType(StrToInt(LXMLDocOut.DocumentElement.ChildNodes['TextType'].Text));
          AElementSubType := THydroNVElementSubType(StrToInt(LXMLDocOut.DocumentElement.ChildNodes['ElementSubType'].Text));
          ASectionNo      := StrToInt(LXMLDocOut.DocumentElement.ChildNodes['SectionNo'].Text);
        end;
        Result       := TRUE;
      end;
    finally
      LXMLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVEventHandler.FindServerShape : IVShape;
const OPNAME = 'THydroNVEventHandler.FindServerShape';
var
  lPageIdx  : integer;
  lShapeIdx : integer;
  lShape    : IVShape;
  lPage     : IVPage;
begin
  Result := nil;
  try
    lPageIdx := 1;
    while ((Result = nil) AND (lPageIdx <= FVisioApp.ActiveDocument.Pages.Count)) do
    begin
      lPage := FVisioApp.ActiveDocument.Pages[lPageIdx];
      lShapeIdx := 1;
      while ((Result = nil) AND (lShapeIdx <= lPage.Shapes.Count)) do
      begin
        lShape := lPage.Shapes[lShapeIdx];
        if (Pos(PChar(mtHydroServer), lShape.Name) > 0) then
          Result := lShape
        else
          lShapeIdx := lShapeIdx + 1;
      end;
      lPageIdx := lPageIdx + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.FindShapeWithParent (AMasterName : string;
                                                   AParentName : string) : IVShape;
const OPNAME = 'THydroNVEventHandler.FindShapeWithParent';
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
            lTempStr := GetShapeParent(lShape);
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

function THydroNVEventHandler.FindShapeWithName (AShapeName : string) : IVShape;
const OPNAME = 'THydroNVEventHandler.FindShapeWithName';
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

function  THydroNVEventHandler.FindShapeWithPropertyValue (AMasterName : string;
                                                           AProperty   : string;
                                                           AValue      : string) : IVShape;
const OPNAME = 'THydroNVEventHandler.FindShapeWithPropertyValue';
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

procedure  THydroNVEventHandler.RenameDuplicateShapes (AMasterName : string;
                                                       ANumber     : string;
                                                       ADupNo      : Integer);
const OPNAME = 'THydroNVEventHandler.RenameDuplicateShapes';
var
  LPageIndex  : Integer;
  LShapeIndex : Integer;
  LPage       : IVPage;
  LShape      : IVShape;
  LDupVal     : Integer;
  LPos        : Integer;
  LShapeName  : String;
begin
  try
    LPageIndex := 1;
    while (LPageIndex <= FVisioApp.ActiveDocument.Pages.Count) do
    begin
      LPage := FVisioApp.ActiveDocument.Pages[LPageIndex];
      LShapeIndex := 1;
      while (LShapeIndex <= LPage.Shapes.Count) do
      begin
        LShape := LPage.Shapes[LShapeIndex];
        if ((LShape <> nil) AND (LShape.Master <> nil) AND
            (Pos(PChar(AMasterName), LShape.Master.Name) > 0) AND (IsShapeDuplicate(LShape)) AND
            (GetShapeElementNoAsString(LShape) = ANumber)) then
        begin
          LDupVal := GetShapeDuplicateNo(LShape);
          if (LDupVal > ADupNo) then
          begin
            SetShapeDuplicateNo(LShape, LDupVal - 1);
            LShapeName := GetShapeName(LShape);
            LPos       := Pos(' (Copy ', LShapeName);
            LShapeName := Copy(LShapeName, 1, LPos) + '(Copy ' + IntToStr(LDupVal-1) + ')';
            SetShapeName(LShape, LShapeName);
          end;
        end;
        LShapeIndex := LShapeIndex + 1;
      end;
      LPageIndex := LPageIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure  THydroNVEventHandler.DeleteDuplicateShapes (AMasterName : string;
                                                       ANumber     : string);
const OPNAME = 'THydroNVEventHandler.DeleteDuplicateShapes';
var
  lPageIndex  : Integer;
  LShapeIndex : Integer;
  lPage       : IVPage;
  LShape      : IVShape;
begin
  try
    FSystemFlag := TRUE;
    lPageIndex := 1;
    while (lPageIndex <= FVisioApp.ActiveDocument.Pages.Count) do
    begin
      lPage := FVisioApp.ActiveDocument.Pages[lPageIndex];
      lShapeIndex := 1;
      while (lShapeIndex <= lPage.Shapes.Count) do
      begin
        lShape := lPage.Shapes[lShapeIndex];
        if ((lShape <> nil) AND (lShape.Master <> nil) AND
            (Pos(PChar(AMasterName), lShape.Master.Name) > 0) AND
            (IsShapeDuplicate(lShape)) AND
            (GetShapeElementNoAsString(lShape) = ANumber)) then
          lShape.Delete
        else
          lShapeIndex := lShapeIndex + 1;
      end;
      lPageIndex := lPageIndex + 1;
    end;
    FSystemFlag := FALSE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.FindTextShapesWithParent (AParentName : string;
                                                         AShapesList : TStringList);
const OPNAME = 'THydroNVEventHandler.FindTextShapesWithParent';
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
//        if ((lShape <> nil) AND (Pos(mtHydroText, lShape.Name) > 0)) then
        if ((lShape <> nil) AND (HydroNVShapeType(lShape) = stHydroText)) then
        begin
          if (lShape.CellExists[cParent, 1] <> 0) then
          begin
            lTempStr := GetShapeParent(lShape);
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

procedure THydroNVEventHandler.DeleteAllTextShapes (const AParentShape : IVShape);
const OPNAME = 'THydroNVEventHandler.DeleteAllTextShapes';
var
  lTextShape  : IVShape;
  lShapesLst  : TStringList;
  lIndex      : integer;
  lShapeName  : string;
  lSelection  : IVSelection;
  LTextType   : THydroNVTextType;
  LItemIndex  : Integer;
begin
  try
    lSelection := FVisioApp.ActiveWindow.Selection;
    lShapesLst := TStringList.Create;
    try
      FindTextShapesWithParent(AParentShape.Name, lShapesLst);
      for lIndex := 0 to lShapesLst.Count - 1 do
      begin
        lShapeName := lShapesLst.Strings[lIndex];
        lTextShape := FindShapeWithName(lShapeName);
        if (lTextShape <> nil) AND (NOT ShapeIsSelected(lSelection, lTextShape.Name)) then
        begin
          LTextType := GetShapeHydroNVTextType(lTextShape);
          if (LTextType in OutputTextTypeSet) then
          begin
            LItemIndex := FOutputTextList.IndexOf(lTextShape.Name);
            if (LItemIndex >= 0) then
              FOutputTextList.Delete(LItemIndex);
          end;
          lTextShape.Delete;
        end;
      end;
    finally
      FreeAndNil(lShapesLst);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVEventHandler.ShapeIsSelected (ASelection : IVSelection;
                                               AShapeName : string) : Boolean;
const OPNAME = 'THydroNVEventHandler.ShapeIsSelected';
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

function THydroNVEventHandler.ProcessHydroNVToggleGIS: boolean;
const OPNAME = 'THydroNVEventHandler.ProcessHydroNVToggleGIS';
begin
  Result := False;
  try
    if (FDrawing.DrawingID <> 0) then
    begin
      if (FDrawing.GISDrawing = 0) then
        FDrawing.GISDrawing := 1
      else
        FDrawing.GISDrawing := 0;
      if (FDrawing.GISDrawing = 1) then
        ShowGIS
      else
        HideGIS;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.ProcessHydroNVDoubleClicked (AShapeName : String;
                                                           ANumber    : integer) : WordBool;
const OPNAME = 'THydroNVEventHandler.ProcessHydroNVDoubleClicked';
var
  LHydrologyModel : IHydrologyModel;
  LShape          : IVShape;
  LShapeType      : THydroNVShapeType;
begin
  Result := FALSE;
  try
    LShapeType := stHydroNone;
    LShape     := FindShapeWithName(AShapeName);
    if (LShape <> nil) then
      LShapeType := HydroNVShapeType(LShape);
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    case LShapeType of
      stHydroObsPoint       : Result := LHydrologyModel.ShowObservationPointDialog(ANumber);
      stHydroRoute          : Result := LHydrologyModel.ShowNetworkRouteDialog(ANumber);
      stHydroReservoir      : Result := LHydrologyModel.ShowReservoirModuleDialog(ANumber);
      stHydroRunOff         : Result := LHydrologyModel.ShowRunOffModuleDialog(ANumber);
      stHydroIrrBlock       : Result := LHydrologyModel.ShowIrrigationModuleDialog(ANumber);
      stHydroChannel        : Result := LHydrologyModel.ShowChannelModuleDialog(ANumber);
      stHydroMine           : Result := LHydrologyModel.ShowMineModuleDialog(ANumber);
      stHydroOutputSelector : Result := ShowNVOutputSelectorDlg(FInterval);
    else
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.ProcessHydroNVRightClicked (AShapeName : string;
                                                          ANumber    : integer) : WordBool;
const OPNAME = 'THydroNVEventHandler.ProcessHydroNVRightClicked';
var
  LHydrologyModel : IHydrologyModel;
  LShape          : IVShape;
  LShapeType      : THydroNVShapeType;
begin
  Result := FALSE;
  try
    LShapeType := stHydroNone;
    LShape     := FindShapeWithName(AShapeName);
    if (LShape <> nil) then
      LShapeType := HydroNVShapeType(LShape);
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    case LShapeType of
      stHydroObsPoint :
        begin
          if (LHydrologyModel.Network.ObservationPointAgent.ObservationPointByRouteNo[ANumber] = nil) then
            MessageDlg('Observation point on route ' + IntToStr(ANumber) + ' does not exits in network ' +
                       LHydrologyModel.Network.NetworkCode, mtError, [mbOK], 0)
          else
            Result := ProcessHydroNVObservationPointRightClicked(AShapeName, ANumber);
        end;
      stHydroRoute :
        begin
          if (LHydrologyModel.Network.NetworkRouteAgent.NetworkRouteByRouteNo[ANumber] = nil) then
            MessageDlg('Network route ' + IntToStr(ANumber) + ' does not exits in network ' +
                       LHydrologyModel.Network.NetworkCode, mtError, [mbOK], 0)
          else
            Result := ProcessHydroNVNetworkRouteRightClicked(AShapeName, ANumber);
        end;
      stHydroReservoir :
        begin
          if (LHydrologyModel.Network.ReservoirModuleAgent.ReservoirModuleByNumber[ANumber] = nil) then
            MessageDlg('ReservoirModule ' + IntToStr(ANumber) + ' does not exits in network ' +
                       LHydrologyModel.Network.NetworkCode, mtError, [mbOK], 0)
          else
            Result := ProcessHydroNVReservoirModuleRightClicked(AShapeName, ANumber);
        end;
      stHydroRunOff :
        begin
          if (LHydrologyModel.Network.RunOffModuleAgent.RunOffModuleByNumber[ANumber] = nil) then
            MessageDlg('RunOffModule ' + IntToStr(ANumber) + ' does not exits in network ' +
                       LHydrologyModel.Network.NetworkCode, mtError, [mbOK], 0)
          else
            Result := ProcessHydroNVRunOffModuleRightClicked(AShapeName, ANumber);
        end;
      stHydroIrrBlock :
        begin
          if (LHydrologyModel.Network.IrrigationModuleAgent.IrrigationModuleByNumber[ANumber] = nil) then
            MessageDlg('IrrigationModule ' + IntToStr(ANumber) + ' does not exits in network ' +
                       LHydrologyModel.Network.NetworkCode, mtError, [mbOK], 0)
          else
            Result := ProcessHydroNVIrrigationModuleRightClicked(AShapeName, ANumber);
        end;
      stHydroMine :
        begin
          if (LHydrologyModel.Network.MineModuleAgent.MineModuleByNumber[ANumber] = nil) then
            MessageDlg('MineModule ' + IntToStr(ANumber) + ' does not exits in network ' +
                       LHydrologyModel.Network.NetworkCode, mtError, [mbOK], 0)
          else
            Result := ProcessHydroNVMineModuleRightClicked(AShapeName, ANumber);
        end;
      stHydroChannel :
        begin
          if (LHydrologyModel.Network.ChannelModuleAgent.ChannelModuleByNumber[ANumber] = nil) then
            MessageDlg('ChannelModule ' + IntToStr(ANumber) + ' does not exits in network ' +
                       LHydrologyModel.Network.NetworkCode, mtError, [mbOK], 0)
          else
            Result := ProcessHydroNVChannelModuleRightClicked(AShapeName, ANumber);
        end;
      else
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.ProcessHydroNVObservationPointRightClicked (AShapeName : string; ANumber : integer) : WordBool;
const OPNAME = 'THydroNVEventHandler.ProcessHydroNVObservationPointRightClicked';
var
  LMenuDlg        : THydroNVShapeMenuDlg;
  LOption         : integer;
  LHydrologyModel : IHydrologyModel;
  LShape          : IVShape;
begin
  Result := FALSE;
  try
    LMenuDlg := THydroNVShapeMenuDlg.Create(nil);
    try
      LMenuDlg.Options.Add('Cancel...=0');
      LMenuDlg.Options.Add('Observation Point Properties=1');
      if (FMayChangeStructure) then
        LMenuDlg.Options.Add('Delete Observation Point from Network=2');
      LOption := LMenuDlg.ShowShapeMenuDlg;
      case lOption of
        1 : begin
              LHydrologyModel := FAppModules.Model as IHydrologyModel;
              Result := LHydrologyModel.ShowObservationPointDialog(ANumber);
            end;
        2 : begin
              Result := DeleteObservationPointFromNetwork(ANumber);
              if (Result) then
              begin
                LShape := FindShapeWithName(AShapeName);
                LShape.Delete;
              end;
            end;
      else
      end;
    finally
      LMenuDlg.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.ProcessHydroNVNetworkRouteRightClicked (AShapeName : string; ANumber : integer) : WordBool;
const OPNAME = 'THydroNVEventHandler.ProcessHydroNVNetworkRouteRightClicked';
var
  LMenuDlg        : THydroNVShapeMenuDlg;
  LOption         : integer;
  LHydrologyModel : IHydrologyModel;
  LShape          : IVShape;
begin
  Result := FALSE;
  try
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    LMenuDlg := THydroNVShapeMenuDlg.Create(nil);
    try
      LMenuDlg.Options.Add('Cancel...=0');
      LMenuDlg.Options.Add('Network Route Properties=1');
      LMenuDlg.Options.Add('Network Route Output=2');
      if (FMayChangeStructure) then
        LMenuDlg.Options.Add('Delete Route from Network=3');

      LOption := LMenuDlg.ShowShapeMenuDlg;
      case lOption of
        1 : begin
              Result := LHydrologyModel.ShowNetworkRouteDialog(ANumber);
            end;
        2 : begin
              LHydrologyModel := FAppModules.Model as IHydrologyModel;
              Result := LHydrologyModel.ShowHydroOutputDialog('RQ', '', ANumber, 0, 0);
            end;
        3 : begin
              Result := DeleteRouteFromNetwork(ANumber);
              if (Result) then
              begin
                LShape := FindShapeWithName(AShapeName);
                LShape.Delete;
              end;
            end;
      else
      end;
    finally
      LMenuDlg.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.DetermineNetworkSequence (AModuleID : Integer): Boolean;
const OPNAME = 'THydroNVEventHandler.DetermineNetworkSequence';
var
  LHydrologyModel : IHydrologyModel;
begin
  Result := FALSE;
  try
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    if (LHydrologyModel.Network.DetermineNetworkSequence) then
    begin
      RefreshDocument(FVisioDoc);
      Result := LHydrologyModel.ShowNetworkSequenceDialog(AModuleID);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.ProcessHydroNVReservoirModuleRightClicked (AShapeName : string; ANumber : integer) : WordBool;
const OPNAME = 'THydroNVEventHandler.ProcessHydroNVReservoirModuleRightClicked';
var
  LMenuDlg        : THydroNVShapeMenuDlg;
  LOption         : integer;
  LHydrologyModel : IHydrologyModel;
  LShape          : IVShape;
  LModule         : INetworkModule;

//  LErrorMsg       : WideString;
//  LNetworkID      : Integer;
//  LNetworkCode    : WideString;

begin
  Result := FALSE;
  try
    LMenuDlg := THydroNVShapeMenuDlg.Create(nil);
    try
      LMenuDlg.Options.Add('Cancel...=0');
      LMenuDlg.Options.Add('Reservoir Module Properties=1');
      LMenuDlg.Options.Add('Reservoir Module Output=2');
      if (FMayChangeStructure) then
      begin
        LMenuDlg.Options.Add('Delete Reservoir Module from Network=3');
        LMenuDlg.Options.Add('Change Module Sequence Number=4');
        LMenuDlg.Options.Add('Determine Network Sequence=5');
      end;
(*
      LMenuDlg.Options.Add('Copy Network=6');
      LMenuDlg.Options.Add('Delete Network=7');
      LMenuDlg.Options.Add('Export Network=8');
      LMenuDlg.Options.Add('Import Network=9');
      LMenuDlg.Options.Add('Create New Network=10');
//      LMenuDlg.Options.Add('All Network Codes=11');
*)
      LOption := LMenuDlg.ShowShapeMenuDlg;
      case lOption of
        1 : begin
              LHydrologyModel := FAppModules.Model as IHydrologyModel;
              Result := LHydrologyModel.ShowReservoirModuleDialog(ANumber);
            end;
        2 : begin
              LHydrologyModel := FAppModules.Model as IHydrologyModel;
              Result := LHydrologyModel.ShowHydroOutputDialog('RV', '', ANumber, 0, 0);
            end;
        3 : begin
              Result := DeleteReservoirFromNetwork(ANumber);
              if (Result) then
              begin
                LShape := FindShapeWithName(AShapeName);
                LShape.Delete;
              end;
            end;
        4 : begin
              LHydrologyModel := FAppModules.Model as IHydrologyModel;
              LModule := LHydrologyModel.Network.ReservoirModuleAgent.ReservoirModuleByNumber[ANumber];
              if (LModule <> nil) then
                Result := LHydrologyModel.ShowNetworkSequenceDialog(LModule.ModuleID);
            end;
        5 : begin
              LHydrologyModel := FAppModules.Model as IHydrologyModel;
              LModule := LHydrologyModel.Network.ReservoirModuleAgent.ReservoirModuleByNumber[ANumber];
              if (LModule <> nil) then
                Result := DetermineNetworkSequence(LModule.ModuleID);
            end;
(*
        6 : begin
              LHydrologyModel := FAppModules.Model as IHydrologyModel;
              Result := LHydrologyModel.CopyNetwork(LHydrologyModel.Network.NetworkID, 'C72', LErrorMsg);
            end;
        7 : begin
              LHydrologyModel := FAppModules.Model as IHydrologyModel;
              Result := LHydrologyModel.DeleteNetwork(3, LErrorMsg);
            end;
        8 : begin
              LHydrologyModel := FAppModules.Model as IHydrologyModel;
              Result := LHydrologyModel.ExportNetwork(3, 'R:\WRIMS\Deployment\Data Files\Hydro\', LErrorMsg);
            end;
        9 : begin
              LHydrologyModel := FAppModules.Model as IHydrologyModel;
              Result := LHydrologyModel.ImportNetwork('R:\WRIMS\Deployment\Data Files\Hydro\', LNetworkID, LErrorMsg);
            end;
        10 : begin
               LHydrologyModel := FAppModules.Model as IHydrologyModel;
               Result := LHydrologyModel.CreateNewNetwork('C72', 1, 'C:\WINT\projects\WRSM2000\TestData\C72\',
                         'C:\WINT\projects\WRSM2000\TestData\C72\', 'N', 0, 0, 'N', 1920, 2004, 0, LNetworkID, LErrorMsg);
            end;
{        11 : begin
               LHydrologyModel := FAppModules.Model as IHydrologyModel;
               LErrorMsg := LHydrologyModel.AllNetworkCodes;
               ShowMessage(LErrorMsg);
             end;}
*)             
      else
      end;
    finally
      LMenuDlg.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.DeleteReservoirFromNetwork (ANumber : Integer) : WordBool;
const OPNAME = 'THydroNVEventHandler.DeleteReservoirFromNetwork';
var
  LHydrologyModel : IHydrologyModel;
  LReservoir      : IReservoirModule;
  LIndex          : Integer;
  LInflowRoute    : IInflowRoute;
  LOutflowRoute   : IOutflowRoute;
  LRouteShape     : IVShape;
  LRouteNo        : Integer;
  LMessage        : String;
begin
  Result := FALSE;
  try
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    LReservoir      := LHydrologyModel.Network.ReservoirModuleAgent.ReservoirModuleByNumber[ANumber];
    if (LReservoir <> nil) then
    begin
      LMessage := 'This action will delete RV%d from the network. ' +
                  'This change will not only be applied to the diagram but ' +
                  ' also to the network data. Are you sure you want to continue?';
      LMessage := Format(LMessage, [LReservoir.ModuleNumber]);
      if (MessageDlg(LMessage, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      begin
        for LIndex := 0 to LReservoir.NoOfInFlowRoutes - 1 do
        begin
          LInflowRoute := LReservoir.InflowRouteByIndex[LIndex];
          LRouteNo     := LInflowRoute.RouteNo;
          LHydrologyModel.Network.SetRouteSink(LInflowRoute.RouteNo, 0);
          LRouteShape := FindShapeWithPropertyValue(mtHydroRoute, cNumber, IntToStr(LRouteNo));
          RefreshRoute(LRouteShape, LRouteNo);
        end;
        for LIndex := 0 to LReservoir.NoOfOutFlowRoutes - 1 do
        begin
          LOutflowRoute := LReservoir.OutFlowRouteByIndex[LIndex];
          LRouteNo      := LOutflowRoute.RouteNo;
          LHydrologyModel.Network.SetRouteSource(LOutflowRoute.RouteNo, 0);
          LRouteShape := FindShapeWithPropertyValue(mtHydroRoute, cNumber, IntToStr(LRouteNo));
          RefreshRoute(LRouteShape, LRouteNo);
        end;
        Result := LHydrologyModel.Network.ReservoirModuleAgent.RemoveReservoirModule(ANumber);
      end;  
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.DeleteChannelFromNetwork (ANumber : Integer) : WordBool;
const OPNAME = 'THydroNVEventHandler.DeleteChannelFromNetwork';
var
  LHydrologyModel : IHydrologyModel;
  LChannel        : IChannelModule;
  LIndex          : Integer;
  LInflowRoute    : IInflowRoute;
  LOutflowRoute   : IOutflowRoute;
  LRouteShape     : IVShape;
  LRouteNo        : Integer;
  LMessage        : String;
begin
  Result := FALSE;
  try
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    LChannel        := LHydrologyModel.Network.ChannelModuleAgent.ChannelModuleByNumber[ANumber];
    if (LChannel <> nil) then
    begin
      LMessage := 'This action will delete CR%d from the network. ' +
                  'This change will not only be applied to the diagram but ' +
                  ' also to the network data. Are you sure you want to continue?';
      LMessage := Format(LMessage, [LChannel.ModuleNumber]);
      if (MessageDlg(LMessage, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      begin
        for LIndex := 0 to LChannel.NoOfInFlowRoutes - 1 do
        begin
          LInflowRoute := LChannel.InflowRouteByIndex[LIndex];
          LRouteNo     := LInflowRoute.RouteNo;
          LHydrologyModel.Network.SetRouteSink(LInflowRoute.RouteNo, 0);
          LRouteShape := FindShapeWithPropertyValue(mtHydroRoute, cNumber, IntToStr(LRouteNo));
          RefreshRoute(LRouteShape, LRouteNo);
        end;
        for LIndex := 0 to LChannel.NoOfOutFlowRoutes - 1 do
        begin
          LOutflowRoute := LChannel.OutFlowRouteByIndex[LIndex];
          LRouteNo      := LOutflowRoute.RouteNo;
          LHydrologyModel.Network.SetRouteSource(LOutflowRoute.RouteNo, 0);
          LRouteShape := FindShapeWithPropertyValue(mtHydroRoute, cNumber, IntToStr(LRouteNo));
          RefreshRoute(LRouteShape, LRouteNo);
        end;
        Result := LHydrologyModel.Network.ChannelModuleAgent.RemoveChannelModule(ANumber);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function THydroNVEventHandler.DeleteRunOffFromNetwork (ANumber : Integer) : WordBool;
const OPNAME = 'THydroNVEventHandler.DeleteRunOffFromNetwork';
var
  LHydrologyModel : IHydrologyModel;
  LRunOff         : IRunOffModule;
  LOutflowRoute   : IRunOffOutflowRoute;
  LRouteShape     : IVShape;
  LRouteNo        : Integer;
  LMessage        : String;
begin
  Result := FALSE;
  try
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    LRunOff         := LHydrologyModel.Network.RunOffModuleAgent.RunOffModuleByNumber[ANumber];
    if (LRunOff <> nil) then
    begin
      LMessage := 'This action will delete RU%d from the network. ' +
                  'This change will not only be applied to the diagram but ' +
                  ' also to the network data. Are you sure you want to continue?';
      LMessage := Format(LMessage, [LRunOff.ModuleNumber]);
      if (MessageDlg(LMessage, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      begin
        while (LRunOff.NoOfOutFlowRoutes > 0) do
        begin
          LOutflowRoute := LRunOff.OutFlowRouteByIndex[0];
          LRouteNo      := LOutflowRoute.RouteNo;
          LHydrologyModel.Network.SetRouteSource(LOutflowRoute.RouteNo, 0);
          LRouteShape := FindShapeWithPropertyValue(mtHydroRoute, cNumber, IntToStr(LRouteNo));
          RefreshRoute(LRouteShape, LRouteNo);
        end;
        Result := LHydrologyModel.Network.RunOffModuleAgent.RemoveRunOffModule(ANumber);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.DeleteIrrigationFromNetwork (ANumber : Integer) : WordBool;
const OPNAME = 'THydroNVEventHandler.DeleteIrrigationFromNetwork';
var
  LHydrologyModel : IHydrologyModel;
  LIrrigation     : IIrrigationModule;
  LIndex          : Integer;
  LRoute          : INetworkRoute;
  LRouteShape     : IVShape;
  LRouteNo        : Integer;
  LMessage        : String;
begin
  Result := FALSE;
  try
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    LIrrigation     := LHydrologyModel.Network.IrrigationModuleAgent.IrrigationModuleByNumber[ANumber];
    if (LIrrigation <> nil) then
    begin
      LMessage := 'This action will delete RR%d from the network. ' +
                  'This change will not only be applied to the diagram but ' +
                  ' also to the network data. Are you sure you want to continue?';
      LMessage := Format(LMessage, [LIrrigation.ModuleNumber]);
      if (MessageDlg(LMessage, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      begin
        for LIndex := 0 to LHydrologyModel.Network.NetworkRouteAgent.NetworkRouteCount - 1 do
        begin
          LRoute := LHydrologyModel.Network.NetworkRouteAgent.NetworkRouteByIndex[LIndex];
          if (LRoute.SourceModuleID = LIrrigation.ModuleID) then
          begin
            LRouteNo := LRoute.RouteNo;
            LHydrologyModel.Network.SetRouteSource(LRoute.RouteNo, 0);
            LRouteShape := FindShapeWithPropertyValue(mtHydroRoute, cNumber, IntToStr(LRouteNo));
            RefreshRoute(LRouteShape, LRouteNo);
          end;
          if (LRoute.SinkModuleID = LIrrigation.ModuleID) then
          begin
            LRouteNo := LRoute.RouteNo;
            LHydrologyModel.Network.SetRouteSink(LRoute.RouteNo, 0);
            LRouteShape := FindShapeWithPropertyValue(mtHydroRoute, cNumber, IntToStr(LRouteNo));
            RefreshRoute(LRouteShape, LRouteNo);
          end;
        end;
        Result := LHydrologyModel.Network.IrrigationModuleAgent.RemoveIrrigationModule(ANumber);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.DeleteMineFromNetwork (ANumber : Integer) : WordBool;
const OPNAME = 'THydroNVEventHandler.DeleteMineFromNetwork';
var
  LHydrologyModel : IHydrologyModel;
  LMine           : IMineModule;
  LIndex          : Integer;
  LRoute          : INetworkRoute;
  LRouteShape     : IVShape;
  LRouteNo        : Integer;
  LMessage        : String;
begin
  Result := FALSE;
  try
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    LMine           := LHydrologyModel.Network.MineModuleAgent.MineModuleByNumber[ANumber];
    if (LMine <> nil) then
    begin
      LMessage := 'This action will delete MM%d from the network. ' +
                  'This change will not only be applied to the diagram but ' +
                  ' also to the network data. Are you sure you want to continue?';
      LMessage := Format(LMessage, [LMine.ModuleNumber]);
      if (MessageDlg(LMessage, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      begin
        for LIndex := 0 to LHydrologyModel.Network.NetworkRouteAgent.NetworkRouteCount - 1 do
        begin
          LRoute := LHydrologyModel.Network.NetworkRouteAgent.NetworkRouteByIndex[LIndex];
          if (LRoute.SourceModuleID = LMine.ModuleID) then
          begin
            LRouteNo := LRoute.RouteNo;
            LHydrologyModel.Network.SetRouteSource(LRoute.RouteNo, 0);
            LRouteShape := FindShapeWithPropertyValue(mtHydroRoute, cNumber, IntToStr(LRouteNo));
            RefreshRoute(LRouteShape, LRouteNo);
          end;
          if (LRoute.SinkModuleID = LMine.ModuleID) then
          begin
            LRouteNo := LRoute.RouteNo;
            LHydrologyModel.Network.SetRouteSink(LRoute.RouteNo, 0);
            LRouteShape := FindShapeWithPropertyValue(mtHydroRoute, cNumber, IntToStr(LRouteNo));
            RefreshRoute(LRouteShape, LRouteNo);
          end;
        end;
        Result := LHydrologyModel.Network.MineModuleAgent.RemoveMineModule(ANumber);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.DeleteRouteFromNetwork (ANumber : Integer) : WordBool;
const OPNAME = 'THydroNVEventHandler.DeleteRouteFromNetwork';
var
  LHydrologyModel : IHydrologyModel;
  LRoute          : INetworkRoute;
  LSourceShape    : IVShape;
  LSinkShape      : IVShape;
  LRouteNo        : Integer;
  LObsPoint       : IObservationPoint;
  LMessage        : String;
begin
  Result := FALSE;
  try
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    LRoute          := LHydrologyModel.Network.NetworkRouteAgent.NetworkRouteByRouteNo[ANumber];
    if (LRoute <> nil) then
    begin
      LMessage := 'This action will delete Route %d from the network. ' +
                  'This change will not only be applied to the diagram but ' +
                  ' also to the network data. Are you sure you want to continue?';
      LMessage := Format(LMessage, [LRoute.RouteNo]);
      if (MessageDlg(LMessage, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      begin
        LRouteNo      := LRoute.RouteNo;
        LObsPoint     := LHydrologyModel.Network.ObservationPointAgent.ObservationPointByRouteNo[LRouteNo];
        if (LObsPoint <> nil) then
          LHydrologyModel.Network.ObservationPointAgent.RemoveObservationPoint(LHydrologyModel.Network.NetworkID, LRouteNo);
        LSourceShape  := FindRouteSourceShape(LRoute);
        LSinkShape    := FindRouteSinkShape(LRoute);
        LHydrologyModel.Network.SetRouteSink(LRouteNo, 0);
        LHydrologyModel.Network.SetRouteSource(LRouteNo, 0);
        if (LSourceShape <> nil) then
          RefreshShape(LSourceShape);
        if (LSinkShape <> nil) then
          RefreshShape(LSinkShape);
        Result := LHydrologyModel.Network.NetworkRouteAgent.RemoveNetworkRoute(LHydrologyModel.Network.NetworkID, LRouteNo);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.DeleteObservationPointFromNetwork (ANumber : Integer) : WordBool;
const OPNAME = 'THydroNVEventHandler.DeleteObservationPointFromNetwork';
var
  LHydrologyModel : IHydrologyModel;
  LObsPoint       : IObservationPoint;
  LRouteNo        : Integer;
  LMessage        : String;
begin
  Result := FALSE;
  try
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    LObsPoint       := LHydrologyModel.Network.ObservationPointAgent.ObservationPointByRouteNo[ANumber];
    if (LObsPoint <> nil) then
    begin
      LMessage := 'This action will delete the observation point on route %d from the network. ' +
                  'This change will not only be applied to the diagram but ' +
                  ' also to the network data. Are you sure you want to continue?';
      LMessage := Format(LMessage, [LObsPoint.RouteNo]);
      if (MessageDlg(LMessage, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      begin
        LRouteNo := LObsPoint.RouteNo;
        Result   := LHydrologyModel.Network.ObservationPointAgent.RemoveObservationPoint(LHydrologyModel.Network.NetworkID, LRouteNo);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.ProcessHydroNVRunOffModuleRightClicked (AShapeName : string; ANumber : integer) : WordBool;
const OPNAME = 'THydroNVEventHandler.ProcessHydroNVRunOffModuleRightClicked';
var
  LMenuDlg        : THydroNVShapeMenuDlg;
  LOption         : integer;
  LHydrologyModel : IHydrologyModel;
  LShape          : IVShape;
  LModule         : INetworkModule;
begin
  Result := FALSE;
  try
    LMenuDlg := THydroNVShapeMenuDlg.Create(nil);
    try
      LMenuDlg.Options.Add('Cancel...=0');
      LMenuDlg.Options.Add('RunOff Module Properties=1');
      LMenuDlg.Options.Add('RunOff Module Output=2');
      if (FMayChangeStructure) then
      begin
        LMenuDlg.Options.Add('Delete RunOff Module from Network=3');
        LMenuDlg.Options.Add('Change Module Sequence Number=4');
        LMenuDlg.Options.Add('Determine Network Sequence=5');
      end;
      LOption := LMenuDlg.ShowShapeMenuDlg;
      case lOption of
        1 : begin
              LHydrologyModel := FAppModules.Model as IHydrologyModel;
              Result := LHydrologyModel.ShowRunOffModuleDialog(ANumber);
            end;
        2 : begin
              LHydrologyModel := FAppModules.Model as IHydrologyModel;
              Result := LHydrologyModel.ShowHydroOutputDialog('RU', '', ANumber, 0, 0);
            end;
        3 : begin
              Result := DeleteRunOffFromNetwork(ANumber);
              if (Result) then
              begin
                LShape := FindShapeWithName(AShapeName);
                LShape.Delete;
              end;
            end;
        4 : begin
              LHydrologyModel := FAppModules.Model as IHydrologyModel;
              LModule := LHydrologyModel.Network.RunOffModuleAgent.RunOffModuleByNumber[ANumber];
              if (LModule <> nil) then
                Result := LHydrologyModel.ShowNetworkSequenceDialog(LModule.ModuleID);
            end;
        5 : begin
              LHydrologyModel := FAppModules.Model as IHydrologyModel;
              LModule := LHydrologyModel.Network.RunOffModuleAgent.RunOffModuleByNumber[ANumber];
              if (LModule <> nil) then
                Result := DetermineNetworkSequence(LModule.ModuleID);
            end;
      else
      end;
    finally
      LMenuDlg.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.ProcessHydroNVIrrigationModuleRightClicked (AShapeName : string; ANumber : integer) : WordBool;
const OPNAME = 'THydroNVEventHandler.ProcessHydroNVIrrigationModuleRightClicked';
var
  LMenuDlg        : THydroNVShapeMenuDlg;
  LOption         : integer;
  LHydrologyModel : IHydrologyModel;
  LModule         : INetworkModule;
  LShape          : IVShape;
begin
  Result := FALSE;
  try
    LMenuDlg := THydroNVShapeMenuDlg.Create(nil);
    try
      LMenuDlg.Options.Add('Cancel...=0');
      LMenuDlg.Options.Add('Irrigation Module Properties=1');
      if (FMayChangeStructure) then
      begin
        LMenuDlg.Options.Add('Delete Irrigation Module from Network=2');
        LMenuDlg.Options.Add('Change Module Sequence Number=3');
        LMenuDlg.Options.Add('Determine Network Sequence=4');
      end;
      LOption := LMenuDlg.ShowShapeMenuDlg;
      case lOption of
        1 : begin
              LHydrologyModel := FAppModules.Model as IHydrologyModel;
              Result := LHydrologyModel.ShowIrrigationModuleDialog(ANumber);
            end;
        2 : begin
              Result := DeleteIrrigationFromNetwork(ANumber);
              if (Result) then
              begin
                LShape := FindShapeWithName(AShapeName);
                LShape.Delete;
              end;
            end;
        3 : begin
              LHydrologyModel := FAppModules.Model as IHydrologyModel;
              LModule := LHydrologyModel.Network.IrrigationModuleAgent.IrrigationModuleByNumber[ANumber];
              if (LModule <> nil) then
                Result := LHydrologyModel.ShowNetworkSequenceDialog(LModule.ModuleID);
            end;
        4 : begin
              LHydrologyModel := FAppModules.Model as IHydrologyModel;
              LModule := LHydrologyModel.Network.IrrigationModuleAgent.IrrigationModuleByNumber[ANumber];
              if (LModule <> nil) then
                Result := DetermineNetworkSequence(LModule.ModuleID);
            end;
      else
      end;
    finally
      LMenuDlg.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.ProcessHydroNVMineModuleRightClicked (AShapeName : string; ANumber : integer) : WordBool;
const OPNAME = 'THydroNVEventHandler.ProcessHydroNVMineModuleRightClicked';
var
  LMenuDlg        : THydroNVShapeMenuDlg;
  LOption         : integer;
  LMine           : IMineModule;
  LIndex          : Integer;
  LOpencastPit    : IOpencastPit;
  LUnderground    : IUndergroundSection;
  LSlurryDump     : ISlurryDump;
  LHydrologyModel : IHydrologyModel;
  LCount          : Integer;
  LValue          : Integer;
  LModule         : INetworkModule;
  LShape          : IVShape;
  LOCSubList      : TStringList;
  LUGSubList      : TStringList;
  LSDSubList      : TStringList;
begin
  Result := FALSE;
  try
    LMenuDlg   := THydroNVShapeMenuDlg.Create(nil);
    LOCSubList := TStringList.Create;
    LUGSubList := TStringList.Create;
    LSDSubList := TStringList.Create;
    try
      LMenuDlg.Options.Add('Cancel...=0');
      LMenuDlg.Options.Add('Mine Module Properties=1');
      LMenuDlg.Options.Add('Mine Module Output=2');
      if (FMayChangeStructure) then
      begin
        LMenuDlg.Options.Add('Add/Remove Sections=3');
        LMenuDlg.Options.Add('Delete Mine Module from Network=4');
        LMenuDlg.Options.Add('Change Module Sequence Number=5');
        LMenuDlg.Options.Add('Determine Network Sequence=6');
      end;
      LCount := LMenuDlg.Options.Count;
      LValue := LMenuDlg.Options.Count - 1;
      LHydrologyModel := FAppModules.Model as IHydrologyModel;
      LMine := LHydrologyModel.Network.MineModuleAgent.MineModuleByNumber[ANumber];

      if (LMine.NoOfOpencastPits > 0) then
      begin
        LOCSubList.Add('Cancel...=0');
        for LIndex := 0 to LMine.NoOfOpencastPits - 1 do
        begin
          LOpencastPit := LMine.OpencastPitByIndex[LIndex];
          Inc(LValue);
          LOCSubList.Add(LOpencastPit.SectionName + '=' + IntToStr(LValue));
        end;
        LMenuDlg.Options.AddObject('Opencast pits >=999', LOCSubList);
      end;

      if (LMine.NoOfUndergroundSections > 0) then
      begin
        LUGSubList.Add('Cancel...=0');
        for LIndex := 0 to LMine.NoOfUndergroundSections - 1 do
        begin
          LUnderground := LMine.UndergroundSectionByIndex[LIndex];
          Inc(LValue);
          LUGSubList.Add(LUnderground.SectionName + '=' + IntToStr(LValue));
        end;
        LMenuDlg.Options.AddObject('Underground sections >=999', LUGSubList);
      end;

      if (LMine.NoOfSlurryDumps > 0) then
      begin
        LSDSubList.Add('Cancel...=0');
        for LIndex := 0 to LMine.NoOfSlurryDumps - 1 do
        begin
          LSlurryDump := LMine.SlurryDumpByIndex[LIndex];
          Inc(LValue);
          LSDSubList.Add(LSlurryDump.SectionName + '=' + IntToStr(LValue));
        end;
        LMenuDlg.Options.AddObject('Slurry dumps >=999', LSDSubList);
      end;

      try
        LOption := LMenuDlg.ShowShapeMenuDlg;
        case lOption of
          0 : begin
              end;
          1 : begin
                Result := LHydrologyModel.ShowMineModuleDialog(ANumber);
              end;
          2 : begin
                LHydrologyModel := FAppModules.Model as IHydrologyModel;
                Result := LHydrologyModel.ShowHydroOutputDialog('MM', '', ANumber, 0, 0);
              end;
          3 : begin
                Result := LHydrologyModel.ShowMineSectionsDialog(ANumber);
              end;
          4 : begin
                Result := DeleteMineFromNetwork(ANumber);
                if (Result) then
                begin
                  LShape := FindShapeWithName(AShapeName);
                  LShape.Delete;
                end;
              end;
          5 : begin
                LModule := LHydrologyModel.Network.MineModuleAgent.MineModuleByNumber[ANumber];
                if (LModule <> nil) then
                  Result := LHydrologyModel.ShowNetworkSequenceDialog(LModule.ModuleID);
              end;
          6 : begin
                LHydrologyModel := FAppModules.Model as IHydrologyModel;
                LModule := LHydrologyModel.Network.MineModuleAgent.MineModuleByNumber[ANumber];
                if (LModule <> nil) then
                  Result := DetermineNetworkSequence(LModule.ModuleID);
              end;

          else
          begin
            if (LOption < LMine.NoOfOpencastPits + LCount) then
            begin
              LIndex := LOption - LCount;
              LOpencastPit := LMine.OpencastPitByIndex[LIndex];
              Result := LHydrologyModel.ShowMineOpencastPitDialog(ANumber, LOpencastPit.SectionNo);
            end
            else if (LOption < LMine.NoOfUndergroundSections + LMine.NoOfOpencastPits + LCount) then
            begin
              LIndex := LOption - LMine.NoOfOpencastPits - LCount;
              LUnderground := LMine.UndergroundSectionByIndex[LIndex];
              Result := LHydrologyModel.ShowMineUndergroundSectionDialog(ANumber, LUnderground.SectionNo);
            end
            else if (LOption < LMine.NoOfSlurryDumps + LMine.NoOfUndergroundSections + LMine.NoOfOpencastPits + LCount) then
            begin
              LIndex := LOption - LMine.NoOfUndergroundSections - LMine.NoOfOpencastPits - LCount;
              LSlurryDump := LMine.SlurryDumpByIndex[LIndex];
              Result := LHydrologyModel.ShowMineSlurryDumpDialog(ANumber, LSlurryDump.SectionNo);
            end;
          end;
        end;
      finally
        LMenuDlg.Free;
      end;
    finally
      FreeAndNil(LOCSubList);
      FreeAndNil(LUGSubList);
      FreeAndNil(LSDSubList);
    end
  except on E: Exception do HandleError(E, OPNAME) end;
end;

(*
function THydroNVEventHandler.ProcessHydroNVMineModuleRightClicked (AShapeName : string; ANumber : integer) : WordBool;
const OPNAME = 'THydroNVEventHandler.ProcessHydroNVMineModuleRightClicked';
var
  LOptionList     : TStringList;
  LMenuDlg        : THydroNVShapeMenuDlg;
  LOption         : integer;
  LMine           : IMineModule;
  LIndex          : Integer;
  LOpencastPit    : IOpencastPit;
  LUnderground    : IUndergroundSection;
  LSlurryDump     : ISlurryDump;
  LHydrologyModel : IHydrologyModel;
  LCount          : Integer;
  LModule         : INetworkModule;
  LShape          : IVShape;
begin
  Result := FALSE;
  try
    lOptionList := TStringList.Create;
    try
      LOptionList.Add('Cancel...');
      LOptionList.Add('Mine Module Properties');
      LOptionList.Add('Mine Module Output');
      if (FMayChangeStructure) then
      begin
        LOptionList.Add('Add/Remove Sections');
        lOptionList.Add('Delete Mine Module from Network');
        lOptionList.Add('Change Module Sequence Number');
        lOptionList.Add('Determine Network Sequence');
      end;
      LCount := LOptionList.Count;
      LHydrologyModel := FAppModules.Model as IHydrologyModel;
      LMine := LHydrologyModel.Network.MineModuleAgent.MineModuleByNumber[ANumber];

      for LIndex := 0 to LMine.NoOfOpencastPits - 1 do
      begin
        LOpencastPit := LMine.OpencastPitByIndex[LIndex];
        LOptionList.Add('Opencast pit - ' + LOpencastPit.SectionName);
      end;
      for LIndex := 0 to LMine.NoOfUndergroundSections - 1 do
      begin
        LUnderground := LMine.UndergroundSectionByIndex[LIndex];
        LOptionList.Add('Underground section - ' + LUnderground.SectionName);
      end;
      for LIndex := 0 to LMine.NoOfSlurryDumps - 1 do
      begin
        LSlurryDump := LMine.SlurryDumpByIndex[LIndex];
        LOptionList.Add('Slurry dump - ' + LSlurryDump.SectionName);
      end;

      LMenuDlg := THydroNVShapeMenuDlg.Create(nil);
      try
        LOption := LMenuDlg.ShowShapeMenuDialog(LOptionList);
        case lOption of
          0 : begin
              end;
          1 : begin
                Result := LHydrologyModel.ShowMineModuleDialog(ANumber);
              end;
          2 : begin
                LHydrologyModel := FAppModules.Model as IHydrologyModel;
                Result := LHydrologyModel.ShowHydroOutputDialog('MM', '', ANumber, 0, 0);
              end;
// RianaChangeStructure
          3 : begin
                Result := LHydrologyModel.ShowMineSectionsDialog(ANumber);
              end;
          4 : begin
                Result := DeleteMineFromNetwork(ANumber);
                if (Result) then
                begin
                  LShape := FindShapeWithName(AShapeName);
                  LShape.Delete;
                end;
              end;
          5 : begin
                LModule := LHydrologyModel.Network.MineModuleAgent.MineModuleByNumber[ANumber];
                if (LModule <> nil) then
                  Result := LHydrologyModel.ShowNetworkSequenceDialog(LModule.ModuleID);
              end;
          6 : begin
                LHydrologyModel := FAppModules.Model as IHydrologyModel;
                LModule := LHydrologyModel.Network.MineModuleAgent.MineModuleByNumber[ANumber];
                if (LModule <> nil) then
                  Result := DetermineNetworkSequence(LModule.ModuleID);
              end;

          else
          begin
            if (LOption < LMine.NoOfOpencastPits + LCount) then
            begin
              LIndex := LOption - LCount;
              LOpencastPit := LMine.OpencastPitByIndex[LIndex];
              Result := LHydrologyModel.ShowMineOpencastPitDialog(ANumber, LOpencastPit.SectionNo);
            end
            else if (LOption < LMine.NoOfUndergroundSections + LMine.NoOfOpencastPits + LCount) then
            begin
              LIndex := LOption - LMine.NoOfOpencastPits - LCount;
              LUnderground := LMine.UndergroundSectionByIndex[LIndex];
              Result := LHydrologyModel.ShowMineUndergroundSectionDialog(ANumber, LUnderground.SectionNo);
            end
            else if (LOption < LMine.NoOfSlurryDumps + LMine.NoOfUndergroundSections + LMine.NoOfOpencastPits + LCount) then
            begin
              LIndex := LOption - LMine.NoOfUndergroundSections - LMine.NoOfOpencastPits - LCount;
              LSlurryDump := LMine.SlurryDumpByIndex[LIndex];
              Result := LHydrologyModel.ShowMineSlurryDumpDialog(ANumber, LSlurryDump.SectionNo);
            end;

          end;
        end;
{
        if (LOption = 1) then
        begin
          Result := LHydrologyModel.ShowMineModuleDialog(ANumber);
        end
        else if (LOption = 2) then
        begin
          Result := LHydrologyModel.ShowMineSectionsDialog(ANumber);
        end
        else if ((LOption > 1) AND (LOption < LMine.NoOfOpencastPits + LCount)) then
        begin
          LIndex := LOption - LCount;
          LOpencastPit := LMine.OpencastPitByIndex[LIndex];
          Result := LHydrologyModel.ShowMineOpencastPitDialog(ANumber, LOpencastPit.SectionNo);
        end
        else if ((LOption > 1) AND (LOption < LMine.NoOfUndergroundSections + LMine.NoOfOpencastPits + LCount)) then
        begin
          LIndex := LOption - LMine.NoOfOpencastPits - LCount;
          LUnderground := LMine.UndergroundSectionByIndex[LIndex];
          Result := LHydrologyModel.ShowMineUndergroundSectionDialog(ANumber, LUnderground.SectionNo);
        end
        else if ((LOption > 1) AND (LOption < LMine.NoOfSlurryDumps + LMine.NoOfUndergroundSections + LMine.NoOfOpencastPits + LCount)) then
        begin
          LIndex := LOption - LMine.NoOfUndergroundSections - LMine.NoOfOpencastPits - LCount;
          LSlurryDump := LMine.SlurryDumpByIndex[LIndex];
          Result := LHydrologyModel.ShowMineSlurryDumpDialog(ANumber, LSlurryDump.SectionNo);
        end;
}
      finally
        LMenuDlg.Free;
      end;
    finally
      FreeAndNil(LOptionList);
    end
  except on E: Exception do HandleError(E, OPNAME) end;
end;
*)

function THydroNVEventHandler.ProcessHydroNVChannelModuleRightClicked (AShapeName : string; ANumber : integer) : WordBool;
const OPNAME = 'THydroNVEventHandler.ProcessHydroNVChannelModuleRightClicked';
var
  LMenuDlg        : THydroNVShapeMenuDlg;
  LOption         : integer;
  LHydrologyModel : IHydrologyModel;
  LShape          : IVShape;
  LModule         : INetworkModule;
begin
  Result := FALSE;
  try
    LMenuDlg := THydroNVShapeMenuDlg.Create(nil);
    try
      LMenuDlg.Options.Add('Cancel...=0');
      LMenuDlg.Options.Add('Channel Module Properties=1');
      LMenuDlg.Options.Add('Channel Module Output=2');
      if (FMayChangeStructure) then
      begin
        LMenuDlg.Options.Add('Delete Channel Module from Network=3');
        LMenuDlg.Options.Add('Change Module Sequence Number=4');
        LMenuDlg.Options.Add('Determine Network Sequence=5');
      end;
      LOption := LMenuDlg.ShowShapeMenuDlg;
      case lOption of
        1 : begin
              LHydrologyModel := FAppModules.Model as IHydrologyModel;
              Result := LHydrologyModel.ShowChannelModuleDialog(ANumber);
            end;
        2 : begin
              LHydrologyModel := FAppModules.Model as IHydrologyModel;
              Result := LHydrologyModel.ShowHydroOutputDialog('CR', '', ANumber, 0, 0);
            end;
        3 : begin
              Result := DeleteChannelFromNetwork(ANumber);
              if (Result) then
              begin
                LShape := FindShapeWithName(AShapeName);
                LShape.Delete;
              end;
            end;
        4 : begin
              LHydrologyModel := FAppModules.Model as IHydrologyModel;
              LModule := LHydrologyModel.Network.ChannelModuleAgent.ChannelModuleByNumber[ANumber];
              if (LModule <> nil) then
                Result := LHydrologyModel.ShowNetworkSequenceDialog(LModule.ModuleID);
            end;
        5 : begin
              LHydrologyModel := FAppModules.Model as IHydrologyModel;
              LModule := LHydrologyModel.Network.ChannelModuleAgent.ChannelModuleByNumber[ANumber];
              if (LModule <> nil) then
                Result := DetermineNetworkSequence(LModule.ModuleID);
            end;
      else
      end;
    finally
      LMenuDlg.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.AddElementToDuplicateList (ADuplicateList : TStringList; AElementNoStr : String);
const OPNAME = 'THydroNVEventHandler.AddElementToDuplicateList';
var
  LCount : Integer;
begin
  try
    if (ADuplicateList.IndexOfName(AElementNoStr) < 0) then
      ADuplicateList.Add(AElementNoStr + '=1')
    else
    begin
      LCount := StrToInt(ADuplicateList.Values[AElementNoStr]);
      ADuplicateList.Values[AElementNoStr] := IntToStr(LCount + 1);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.RemoveElementFromDuplicateList (ADuplicateList : TStringList; AElementNoStr : String);
const OPNAME = 'THydroNVEventHandler.RemoveElementFromDuplicateList';
var
  LCount : Integer;
  LIndex : Integer;
begin
  try
    LIndex := ADuplicateList.IndexOfName(AElementNoStr);
    if (LIndex >= 0) then
    begin
      LCount := StrToInt(ADuplicateList.Values[AElementNoStr]);
      if (LCount > 1) then
        ADuplicateList.Values[AElementNoStr] := IntToStr(LCount - 1)
      else
        ADuplicateList.Delete(LIndex);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.ClearElementFromDuplicateList (ADuplicateList : TStringList; AElementNoStr : String);
const OPNAME = 'THydroNVEventHandler.ClearElementFromDuplicateList';
var
  LIndex : Integer;
begin
  try
    LIndex := ADuplicateList.IndexOfName(AElementNoStr);
    if (LIndex >= 0) then
    begin
      ADuplicateList.Delete(LIndex);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.TotalElementsInDuplicateList (ADuplicateList : TStringList) : Integer;
const OPNAME = 'THydroNVEventHandler.TotalElementsInDuplicateList';
var
  LIndex : Integer;
  LTotal : Integer;
begin
  Result := 0;
  try
    LIndex := 0;
    LTotal := 0;
    while (LIndex < ADuplicateList.Count) do
    begin
      LTotal := LTotal + StrToInt(ADuplicateList.ValueFromIndex[LIndex]);
      LIndex := LIndex + 1;
    end;
    Result := LTotal;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.CountElementInDuplicateList (ADuplicateList : TStringList; AElementNoStr : String) : Integer;
const OPNAME = 'THydroNVEventHandler.CountElementInDuplicateList';
var
  LIndex : Integer;
begin
  Result := 0;
  try
    LIndex := ADuplicateList.IndexOfName(AElementNoStr);
    if (LIndex >= 0) then
    begin
      Result := StrToInt(ADuplicateList.Values[AElementNoStr]);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.GetShapeElementNoAsString (AShape : IVShape) : String;
const OPNAME = 'THydroNVEventHandler.GetShapeElementNoAsString';
begin
  Result := '';
  try
    Result := UnQuote(AShape.Cells[cNumber].Formula);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.GetShapeElementNo (AShape : IVShape; ADefault : Integer) : Integer;
const OPNAME = 'THydroNVEventHandler.GetShapeElementNo';
begin
  Result := 0;
  try
    Result := StrToIntDef(UnQuote(AShape.Cells[cNumber].Formula), ADefault);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.SetShapeElementNo (AShape : IVShape; AValue : Integer);
const OPNAME = 'THydroNVEventHandler.SetShapeElementNo';
begin
  try
    AShape.Cells[cNumber].Formula  := '"' + IntToStr(AValue) + '"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.GetShapeModuleID (AShape : IVShape) : Integer;
const OPNAME = 'THydroNVEventHandler.GetShapeModuleID';
begin
  Result := 0;
  try
    Result := StrToIntDef(UnQuote(AShape.Cells[cModuleID].Formula),0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.SetShapeModuleID (AShape : IVShape; AValue : Integer);
const OPNAME = 'THydroNVEventHandler.SetShapeModuleID';
begin
  try
    AShape.Cells[cModuleID].Formula  := '"' + IntToStr(AValue) + '"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{
function THydroNVEventHandler.GetShapeRouteNo (AShape : IVShape) : Integer;
const OPNAME = 'THydroNVEventHandler.GetShapeRouteNo';
begin
  Result := 0;
  try
    Result := StrToIntDef(UnQuote(AShape.Cells[cRouteNo].Formula),0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;
}

function THydroNVEventHandler.GetShapeRouteID (AShape : IVShape) : Integer;
const OPNAME = 'THydroNVEventHandler.GetShapeRouteID';
begin
  Result := 0;
  try
    Result := StrToIntDef(UnQuote(AShape.Cells[cRouteID].Formula),0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.SetShapeRouteID (AShape : IVShape; AValue : Integer);
const OPNAME = 'THydroNVEventHandler.SetShapeRouteID';
begin
  try
    AShape.Cells[cRouteID].Formula  := '"' + IntToStr(AValue) + '"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{
function THydroNVEventHandler.GetShapeNetworkSequence (AShape : IVShape) : Integer;
const OPNAME = 'THydroNVEventHandler.GetShapeNetworkSequence';
begin
  Result := 0;
  try
    Result := StrToIntDef(UnQuote(AShape.Cells[cNetworkSequence].Formula),0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;
}
procedure THydroNVEventHandler.SetShapeNetworkSequence (AShape : IVShape; AValue : Integer);
const OPNAME = 'THydroNVEventHandler.SetShapeNetworkSequence';
begin
  try
    AShape.Cells[cNetworkSequence].Formula  := '"' + IntToStr(AValue) + '"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.GetShapeDuplicateNoAsString (AShape : IVShape) : String;
const OPNAME = 'THydroNVEventHandler.GetShapeDuplicateNoAsString';
begin
  Result := '';
  try
    Result := UnQuote(AShape.Cells[cDuplicate].Formula);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.GetShapeDuplicateNo (AShape : IVShape) : Integer;
const OPNAME = 'THydroNVEventHandler.GetShapeDuplicateNo';
begin
  Result := 0;
  try
    Result := StrToIntDef(UnQuote(AShape.Cells[cDuplicate].Formula),0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.SetShapeDuplicateNo (AShape : IVShape; AValue : Integer);
const OPNAME = 'THydroNVEventHandler.SetShapeDuplicateNo';
begin
  try
    AShape.Cells[cDuplicate].Formula  := '"' + IntToStr(AValue) + '"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.GetShapeHydroNVTextType (AShape : IVShape) : THydroNVTextType;
const OPNAME = 'THydroNVEventHandler.GetShapeHydroNVTextType';
begin
  Result := nvtGeneral;
  try
    Result := THydroNVTextType(StrToIntDef(UnQuote(AShape.Cells[cTextType].Formula),0));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.SetShapeHydroNVTextType (AShape : IVShape; ATextType : THydroNVTextType);
const OPNAME = 'THydroNVEventHandler.SetShapeHydroNVTextType';
begin
  try
    AShape.Cells[cTextType].Formula := '"' + IntToStr(Ord(ATextType)) + '"';
    AShape.Cells[cTextTypeLbl].Formula  := '"' + TextTypeToStr(ATextType) + '"';
    AShape.Cells[cTextTypeInv].Formula  := '"TRUE"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.GetShapeHydroNVElementSubType (AShape : IVShape) : THydroNVElementSubType;
const OPNAME = 'THydroNVEventHandler.GetShapeHydroNVElementSubType';
begin
  Result := subNone;
  try
    Result := THydroNVElementSubType(StrToIntDef(UnQuote(AShape.Cells[cElementSubType].Formula),0));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.SetShapeHydroNVElementSubType (AShape : IVShape; AElementSubType : THydroNVElementSubType);
const OPNAME = 'THydroNVEventHandler.SetShapeHydroNVElementSubType';
begin
  try
    AShape.Cells[cElementSubType].Formula := '"' + IntToStr(Ord(AElementSubType)) + '"';
    AShape.Cells[cElementSubTypeLbl].Formula  := '"' + ElementSubTypeToStr(AElementSubType) + '"';
    AShape.Cells[cElementSubTypeInv].Formula  := '"TRUE"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.GetShapeSectionNo (AShape : IVShape) : Integer;
const OPNAME = 'THydroNVEventHandler.GetShapeSectionNo';
begin
  Result := 0;
  try
    Result := StrToIntDef(UnQuote(AShape.Cells[cSectionNo].Formula),0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.SetShapeSectionNo (AShape : IVShape; AValue : Integer);
const OPNAME = 'THydroNVEventHandler.SetShapeSectionNo';
begin
  try
    AShape.Cells[cSectionNo].Formula  := '"' + IntToStr(AValue) + '"';
    AShape.Cells[cSectionNoInv].Formula  := '"TRUE"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.GetShapeVersion (AShape : IVShape) : String;
const OPNAME = 'THydroNVEventHandler.GetShapeVersion';
begin
  Result := '';
  try
    Result := Trim(UnQuote(AShape.Cells[cVersion].Formula));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.SetShapeVersion (AShape : IVShape; AVersion : String);
const OPNAME = 'THydroNVEventHandler.SetShapeVersion';
begin
  try
    AShape.Cells[cVersion].Formula := '"' + AVersion + '"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.SetShapeColour (AShape : IVShape; AColour : Integer);
const OPNAME = 'THydroNVEventHandler.SetShapeColour';
begin
  try
    AShape.CellsSRC[visSectionObject, visRowFill, visFillForegnd].FormulaU := IntToStr(AColour);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.SetShapeLinePattern (AShape : IVShape; APattern : Integer);
const OPNAME = 'THydroNVEventHandler.SetShapeLinePattern';
begin
  try
    AShape.CellsSRC[visSectionObject, visRowLine, visLinePattern].FormulaU := IntToStr(APattern);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.SetShapeActionProcessVNVSpecial (AShape : IVShape);
const OPNAME = 'THydroNVEventHandler.SetShapeActionProcessVNVSpecial';
begin
  try
    AShape.Cells[cRow1Action].Formula := 'RUNMACRO("ThisDocument.ProcessVNVSpecial 1")';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.GetShapeName (AShape : IVShape) : String;
const OPNAME = 'THydroNVEventHandler.GetShapeName';
begin
  Result := '';
  try
    Result := UnQuote(AShape.Cells[cName].Formula);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.SetShapeName (AShape : IVShape; AName : String);
const OPNAME = 'THydroNVEventHandler.SetShapeName';
begin
  try
    AShape.Cells[cName].Formula := '"' + AName + '"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.GetShapePinX (AShape : IVShape) : Double;
const OPNAME = 'THydroNVEventHandler.GetShapePinX';
begin
  Result := 0.0;
  try
    Result := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinX].Formula)),0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.SetShapePinX (AShape : IVShape; AValue : Double);
const OPNAME = 'THydroNVEventHandler.SetShapePinX';
begin
  try
    AShape.Cells[cPinX].Formula := '"' + FloatToStrF(AValue, ffFixed, 12, 2) + 'mm"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.GetShapePinY (AShape : IVShape) : Double;
const OPNAME = 'THydroNVEventHandler.GetShapePinY';
begin
  Result := 0.0;
  try
    Result := StrToFloatDef(UnMM(UnQuote(AShape.Cells[cPinY].Formula)),0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.SetShapePinY (AShape : IVShape; AValue : Double);
const OPNAME = 'THydroNVEventHandler.SetShapePinY';
begin
  try
    AShape.Cells[cPinY].Formula := '"' + FloatToStrF(AValue, ffFixed, 12, 2) + 'mm"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.GetShapeParent (AShape : IVShape) : String;
const OPNAME = 'THydroNVEventHandler.GetShapeParent';
begin
  Result := '';
  try
    Result := Trim(UnQuote(AShape.Cells[cParent].Formula));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.SetShapeParent (AShape : IVShape; AParent : String);
const OPNAME = 'THydroNVEventHandler.SetShapeParent';
begin
  try
    AShape.Cells[cParent].Formula := '"' + AParent + '"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.GetShapeParentXDiff (AShape : IVShape) : Double;
const OPNAME = 'THydroNVEventHandler.GetShapeParentXDiff';
begin
  Result := 0.0;
  try
    Result := StrToFloatDef(UnQuote(AShape.Cells[cParentXDiff].Formula),0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.SetShapeParentXDiff (AShape : IVShape; AValue : Double);
const OPNAME = 'THydroNVEventHandler.SetShapeParentXDiff';
begin
  try
    AShape.Cells[cParentXDiff].Formula := '"' + FloatToStrF(AValue, ffFixed, 12, 2) + '"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.GetShapeParentYDiff (AShape : IVShape) : Double;
const OPNAME = 'THydroNVEventHandler.GetShapeParentYDiff';
begin
  Result := 0.0;
  try
    Result := StrToFloatDef(UnQuote(AShape.Cells[cParentYDiff].Formula),0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.SetShapeParentYDiff (AShape : IVShape; AValue : Double);
const OPNAME = 'THydroNVEventHandler.SetShapeParentYDiff';
begin
  try
    AShape.Cells[cParentYDiff].Formula := '"' + FloatToStrF(AValue, ffFixed, 12, 2) + '"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.GetShapeParentXMoved (AShape : IVShape) : Integer;
const OPNAME = 'THydroNVEventHandler.GetShapeParentXMoved';
begin
  Result := 0;
  try
    Result := StrToIntDef(UnQuote(AShape.Cells[cParentXMoved].Formula),0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.SetShapeParentXMoved (AShape : IVShape; AValue : Integer);
const OPNAME = 'THydroNVEventHandler.SetShapeParentXMoved';
begin
  try
    AShape.Cells[cParentXMoved].Formula := '"' + IntToStr(AValue) + '"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.GetShapeParentYMoved (AShape : IVShape) : Integer;
const OPNAME = 'THydroNVEventHandler.GetShapeParentYMoved';
begin
  Result := 0;
  try
    Result := StrToIntDef(UnQuote(AShape.Cells[cParentYMoved].Formula),0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.SetShapeParentYMoved (AShape : IVShape; AValue : Integer);
const OPNAME = 'THydroNVEventHandler.SetShapeParentYMoved';
begin
  try
    AShape.Cells[cParentYMoved].Formula := '"' + IntToStr(AValue) + '"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.GetShapeParentMoved (AShape : IVShape) : Integer;
const OPNAME = 'THydroNVEventHandler.GetShapeParentMoved';
begin
  Result := 0;
  try
    Result := StrToIntDef(UnQuote(AShape.Cells[cParentMoved].Formula),0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.SetShapeParentMoved (AShape : IVShape; AValue : Integer);
const OPNAME = 'THydroNVEventHandler.SetShapeParentMoved';
begin
  try
    AShape.Cells[cParentMoved].Formula := '"' + IntToStr(AValue) + '"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.GetShapePercFromStart (AShape : IVShape) : Double;
const OPNAME = 'THydroNVEventHandler.GetShapePercFromStart';
begin
  Result := 0.0;
  try
    Result := StrToFloatDef(UnQuote(AShape.Cells[cPercFromStart].Formula),0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.SetShapePercFromStart (AShape : IVShape; AValue : Double);
const OPNAME = 'THydroNVEventHandler.SetShapePercFromStart';
begin
  try
    AShape.Cells[cPercFromStart].Formula := '"' + FloatToStrF(AValue, ffFixed, 12, 2) + '"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.GetRouteShapeSource (AShape : IVShape) : String;
const OPNAME = 'THydroNVEventHandler.GetRouteShapeSource';
begin
  Result := '';
  try
    Result := UnQuote(AShape.Cells[cSource].Formula);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.SetRouteShapeSource (AShape : IVShape; AValue : String);
const OPNAME = 'THydroNVEventHandler.SetRouteShapeSource';
begin
  try
    AShape.Cells[cSource].Formula   := '"' + AValue + '"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.GetRouteShapeSink (AShape : IVShape) : String;
const OPNAME = 'THydroNVEventHandler.GetRouteShapeSink';
begin
  Result := '';
  try
    Result := UnQuote(AShape.Cells[cSink].Formula);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVEventHandler.SetRouteShapeSink (AShape : IVShape; AValue : String);
const OPNAME = 'THydroNVEventHandler.SetRouteShapeSink';
begin
  try
    AShape.Cells[cSink].Formula   := '"' + AValue + '"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.HydroNVShapeType (AShape : IVShape) : THydroNVShapeType;
const OPNAME = 'THydroNVEventHandler.HydroNVShapeType';
begin
  Result := stHydroNone;
  try
    if (Pos(PChar(mtHydroServer), AShape.Name) > 0) then
      Result := stHydroServer
    else if (Pos(PChar(mtHydroNetwork), AShape.Name) > 0) then
      Result := stHydroNetwork
    else if (Pos(PChar(mtHydroReservoir), AShape.Name) > 0) then
      Result := stHydroReservoir
    else if (Pos(PChar(mtHydroRunOff), AShape.Name) > 0) then
      Result := stHydroRunOff
    else if (Pos(PChar(mtHydroChannel), AShape.Name) > 0) then
      Result := stHydroChannel
    else if (Pos(PChar(mtHydroIrrBlock), AShape.Name) > 0) then
      Result := stHydroIrrBlock
    else if (Pos(PChar(mtHydroMine), AShape.Name) > 0) then
      Result := stHydroMine
    else if (Pos(PChar(mtHydroRoute), AShape.Name) > 0) then
      Result := stHydroRoute
    else if (Pos(PChar(mtHydroObsPoint), AShape.Name) > 0) then
      Result := stHydroObsPoint
    else if (Pos(PChar(mtHydroText), AShape.Name) > 0) then
      Result := stHydroText
    else if (Pos(PChar(mtHydroOutputSelector), AShape.Name) > 0) then
      Result := stHydroOutputSelector;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVEventHandler.GetMasterName (AModuleType : String) : String;
const OPNAME = 'THydroNVEventHandler.GetMasterName';
begin
  Result := '';
  try
    if (AModuleType = 'RV') then
      Result := mtHydroReservoir
    else if (AModuleType = 'RU') then
      Result := mtHydroRunOff
    else if (AModuleType = 'CR') then
      Result := mtHydroChannel
    else if (AModuleType = 'RR') then
      Result := mtHydroIrrBlock
    else if (AModuleType = 'MM') then
      Result := mtHydroMine
    else if (AModuleType = 'Route') then
      Result := mtHydroRoute
    else if (AModuleType = 'Obs') then
      Result := mtHydroObsPoint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

