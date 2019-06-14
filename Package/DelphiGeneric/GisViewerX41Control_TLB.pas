unit GisViewerX41Control_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : 1.2
// File generated on 2012/07/06 04:17:10 PM from Type Library described below.

// ************************************************************************  //
// Type Lib: R:\GIS\Esri\GisViewerX41Control.ocx (1)
// LIBID: {7A6AAAAC-4DF0-4976-80B1-EF23158FFE07}
// LCID: 0
// Helpfile: 
// HelpString: GisViewerX41Control Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\system32\stdole2.tlb)
//   (2) v4.0 StdVCL, (C:\WINDOWS\system32\stdvcl40.dll)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Vcl.Graphics, Vcl.OleCtrls, Vcl.OleServer, StdVCL, Variants;
  


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  GisViewerX41ControlMajorVersion = 1;
  GisViewerX41ControlMinorVersion = 0;

  LIBID_GisViewerX41Control: TGUID = '{7A6AAAAC-4DF0-4976-80B1-EF23158FFE07}';

  IID_IGisViewerX: TGUID = '{23556852-1BFD-457C-8A97-0DDFBF459827}';
  DIID_IGisViewerXEvents: TGUID = '{2CEAE78C-9252-4DF1-9CBD-46426278C84C}';
  CLASS_GisViewerX: TGUID = '{3D7F3F4B-BE3E-4E59-85E3-93F643A0435E}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum TxActiveFormBorderStyle
type
  TxActiveFormBorderStyle = TOleEnum;
const
  afbNone = $00000000;
  afbSingle = $00000001;
  afbSunken = $00000002;
  afbRaised = $00000003;

// Constants for enum TxPrintScale
type
  TxPrintScale = TOleEnum;
const
  poNone = $00000000;
  poProportional = $00000001;
  poPrintToFit = $00000002;

// Constants for enum TxMouseButton
type
  TxMouseButton = TOleEnum;
const
  mbLeft = $00000000;
  mbRight = $00000001;
  mbMiddle = $00000002;

// Constants for enum TxGisEventType
type
  TxGisEventType = TOleEnum;
const
  gisRemoveAllLayers = $00000000;
  gisLoadLocality = $00000001;
  gisSetLocality = $00000002;
  gisRemoveLocality = $00000003;
  gisAddLabelRenderer = $00000004;
  gisAddValueRenderer = $00000005;
  gisAddClassRenderer = $00000006;
  gisRemoveRenderer = $00000007;
  gisRemoveLayer = $00000008;
  gisSetLayerMapping = $00000009;
  gisSetLayerProperties = $0000000A;
  gisSetFlyOver = $0000000B;
  gisSearchLayer = $0000000C;
  gisUserSelection = $0000000D;
  gisSelectionMode = $0000000E;

// Constants for enum TxGisToolActionType
type
  TxGisToolActionType = TOleEnum;
const
  toolOpenLayout = $00000000;
  toolSaveLayout = $00000001;
  toolRemoveLayers = $00000002;
  toolLoadLayers = $00000003;
  toolZoomExtent = $00000004;
  toolZoomLayer = $00000005;
  toolZoomOut = $00000006;
  toolZoomIn = $00000007;
  toolZoomPrevious = $00000008;
  toolZoomMode = $00000009;
  toolPanMode = $0000000A;
  toolIdentifyMode = $0000000B;
  toolMeasureMode = $0000000C;
  toolLockedQueryMode = $0000000D;
  toolSelectionMode = $0000000E;
  toolBuildQuery = $0000000F;
  toolClearQuery = $00000010;
  toolProperties = $00000011;
  toolCoordinates = $00000012;
  toolHelp = $00000013;
  toolPrint = $00000014;
  toolDisableMode = $00000015;

// Constants for enum TxGisToolType
type
  TxGisToolType = TOleEnum;
const
  getZoomMode = $00000000;
  getPanMode = $00000001;
  getIdentifyMode = $00000002;
  getMeasureMode = $00000003;
  getLockedQueryMode = $00000004;
  getSelectionMode = $00000005;

// Constants for enum TxRenderType
type
  TxRenderType = TOleEnum;
const
  rtLabel = $00000000;
  trValue = $00000001;
  rtClass = $00000002;
  rtChart = $00000003;
  rtNone = $00000004;

// Constants for enum TxQueryShape
type
  TxQueryShape = TOleEnum;
const
  qsRectangle = $00000000;
  qsLine = $00000001;
  qsPolygon = $00000002;
  qsCircle = $00000003;
  qsFeature = $00000004;

// Constants for enum TxCursorType
type
  TxCursorType = TOleEnum;
const
  crDefault = $00000000;
  crNone = $FFFFFFFF;
  crArrow = $FFFFFFFE;
  crCross = $FFFFFFFD;
  crBeam = $FFFFFFFC;
  crSizeNESW = $FFFFFFFB;
  crSizeNS = $FFFFFFFA;
  crSizeNWSE = $FFFFFFF9;
  crSizeWE = $FFFFFFF8;
  crUpArrow = $FFFFFFF7;
  crHourGlass = $FFFFFFF6;
  crDrag = $FFFFFFF5;
  crNoDrop = $FFFFFFF4;
  crHSplit = $FFFFFFF3;
  crVSplit = $FFFFFFF2;
  crMultiDrag = $FFFFFFF1;
  crSQLWait = $FFFFFFF0;
  crNo = $FFFFFFEF;
  crAppStart = $FFFFFFEE;
  crHelp = $FFFFFFED;
  crHandPoint = $FFFFFFEC;
  crSize = $FFFFFFEB;
  crSizeAll = $FFFFFFEA;

// Constants for enum TxMoPointerType
type
  TxMoPointerType = TOleEnum;
const
  moDefault = $00000000;
  moArrow = $00000001;
  moCross = $00000002;
  moIbeam = $00000003;
  moIconPointer = $00000004;
  moSizePointer = $00000005;
  moSizeNESW = $00000006;
  moSizeNS = $00000007;
  moSizeNWSE = $00000008;
  moSizeWE = $00000009;
  moUpArrow = $0000000A;
  moHourglass = $0000000B;
  moNoDrop = $0000000C;
  moArrowHourglass = $0000000D;
  moArrowQuestion = $0000000E;
  moSizeAll = $0000000F;
  moZoom = $00000032;
  moZoomIn = $00000033;
  moZoomOut = $00000034;
  moPan = $00000035;
  moPanning = $00000036;
  moIdentify = $00000037;
  moLabel = $00000038;
  moHotLink = $00000039;
  moPencil = $0000003A;

// Constants for enum TxShiftState
type
  TxShiftState = TOleEnum;
const
  ssShift = $00000000;
  ssAlt = $00000001;
  ssCtrl = $00000002;
  ssLeft = $00000003;
  ssRight = $00000004;
  ssMiddle = $00000005;
  ssDouble = $00000006;

// Constants for enum TxEllipsoid
type
  TxEllipsoid = TOleEnum;
const
  Airy_1830 = $00000000;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IGisViewerX = interface;
  IGisViewerXDisp = dispinterface;
  IGisViewerXEvents = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  GisViewerX = IGisViewerX;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  PPUserType1 = ^IFontDisp; {*}


// *********************************************************************//
// Interface: IGisViewerX
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {23556852-1BFD-457C-8A97-0DDFBF459827}
// *********************************************************************//
  IGisViewerX = interface(IDispatch)
    ['{23556852-1BFD-457C-8A97-0DDFBF459827}']
    function Get_Visible: WordBool; safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    function Get_AutoScroll: WordBool; safecall;
    procedure Set_AutoScroll(Value: WordBool); safecall;
    function Get_AutoSize: WordBool; safecall;
    procedure Set_AutoSize(Value: WordBool); safecall;
    function Get_AxBorderStyle: TxActiveFormBorderStyle; safecall;
    procedure Set_AxBorderStyle(Value: TxActiveFormBorderStyle); safecall;
    function Get_Caption: WideString; safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    function Get_Color: OLE_COLOR; safecall;
    procedure Set_Color(Value: OLE_COLOR); safecall;
    function Get_Font: IFontDisp; safecall;
    procedure Set_Font(const Value: IFontDisp); safecall;
    procedure _Set_Font(var Value: IFontDisp); safecall;
    function Get_KeyPreview: WordBool; safecall;
    procedure Set_KeyPreview(Value: WordBool); safecall;
    function Get_PixelsPerInch: Integer; safecall;
    procedure Set_PixelsPerInch(Value: Integer); safecall;
    function Get_PrintScale: TxPrintScale; safecall;
    procedure Set_PrintScale(Value: TxPrintScale); safecall;
    function Get_Scaled: WordBool; safecall;
    procedure Set_Scaled(Value: WordBool); safecall;
    function Get_Active: WordBool; safecall;
    function Get_DropTarget: WordBool; safecall;
    procedure Set_DropTarget(Value: WordBool); safecall;
    function Get_HelpFile: WideString; safecall;
    procedure Set_HelpFile(const Value: WideString); safecall;
    function Get_ScreenSnap: WordBool; safecall;
    procedure Set_ScreenSnap(Value: WordBool); safecall;
    function Get_SnapBuffer: Integer; safecall;
    procedure Set_SnapBuffer(Value: Integer); safecall;
    function Get_DoubleBuffered: WordBool; safecall;
    procedure Set_DoubleBuffered(Value: WordBool); safecall;
    function Get_AlignDisabled: WordBool; safecall;
    function Get_VisibleDockClientCount: Integer; safecall;
    function Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    procedure Extent_Set(aLeft: Single; aTop: Single; aRight: Single; aBottom: Single); safecall;
    function Get_Extent_Left: Single; safecall;
    procedure Set_Extent_Left(aValue: Single); safecall;
    function Get_Extent_Top: Single; safecall;
    procedure Set_Extent_Top(aValue: Single); safecall;
    function Get_Extent_Right: Single; safecall;
    procedure Set_Extent_Right(aValue: Single); safecall;
    function Get_Extent_Bottom: Single; safecall;
    procedure Set_Extent_Bottom(aValue: Single); safecall;
    procedure Output_Map(hDC: LongWord); safecall;
    procedure Output_Map2(hDC: LongWord; aX: Integer; aY: Integer; aWidth: Integer; 
                          aHeight: Integer; aDrawFlags: OleVariant); safecall;
    procedure Output_PrintMap(const aDocName: WideString; const aOutputFile: WideString; 
                              aLandscapeOrientation: WordBool); safecall;
    procedure Output_CopyMap(aScaleFactor: Double); safecall;
    procedure Output_ExportMap(aExportType: Integer; const aOutputFile: WideString; 
                               aScaleFactor: Double); safecall;
    procedure Output_ExportMap2(aExportType: Integer; const aOutputFile: WideString; 
                                aScaleFactor: Double; aUseSourceDepth: OleVariant); safecall;
    procedure Toolbar_Action(aToolAction: SYSINT); safecall;
    procedure Toolbar_Property(aToolAction: SYSINT; aVisible: WordBool; aEnabled: WordBool); safecall;
    function Toolbar_Mode(aTool: SYSINT): WordBool; safecall;
    function hWnd_Map: LongWord; safecall;
    function hWnd_Toolbar: LongWord; safecall;
    function hWnd_Legend: LongWord; safecall;
    function hWnd_Statusbar: LongWord; safecall;
    procedure GLF_SetFilePath1(const aFilePath: WideString); safecall;
    procedure GLF_SetFilePath2(const aFilePath: WideString); safecall;
    procedure GLF_SetFilePath3(const aFilePath: WideString); safecall;
    procedure GLF_SetFilePath4(const aFilePath: WideString); safecall;
    function GLF_GetFilePath1: WideString; safecall;
    function GLF_GetFilePath2: WideString; safecall;
    function GLF_GetFilePath3: WideString; safecall;
    function GLF_GetFilePath4: WideString; safecall;
    procedure Layers_RemoveAll; safecall;
    function Layers_Count: Integer; safecall;
    function Layers_Index(const aLayerName: WideString): Integer; safecall;
    procedure Layers_Remove(aLayerIndex: Integer); safecall;
    procedure Layers_RemoveName(const aLayerName: WideString); safecall;
    function Layers_LayerName(aLayerIndex: Integer): WideString; safecall;
    procedure Layers_SetVisible(const aLayerName: WideString; aVisible: WordBool); safecall;
    procedure Layers_SetSymbolProps(const aLayerName: WideString; aStyle: Integer; aSize: Integer; 
                                    aColor: Integer; aOutlineColor: Integer; aOutline: WordBool); safecall;
    procedure Layers_LoadShape(const aShapeFile: WideString; const aLayerName: WideString; 
                               aVisible: WordBool); safecall;
    procedure Layers_LoadImage(const aImageFile: WideString; const aLayerName: WideString; 
                               aVisible: WordBool); safecall;
    procedure Layers_LoadLayout(const aFilePath: WideString; aUseLocalPath: WordBool); safecall;
    procedure Layers_SaveLayout(const aFilePath: WideString); safecall;
    function Locality_Count: Integer; safecall;
    function Locality_Index(const aLayerName: WideString): Integer; safecall;
    procedure Locality_Remove(aLayerIndex: Integer); safecall;
    procedure Locality_RemoveName(const aLayerName: WideString); safecall;
    function Locality_LayerName(aLayerIndex: Integer): WideString; safecall;
    procedure Locality_SetVisible(const aLayerName: WideString; aVisible: WordBool); safecall;
    procedure Locality_SetSymbolProps(const aLayerName: WideString; aStyle: Integer; 
                                      aSize: Integer; aColor: Integer; aOutlineColor: Integer; 
                                      aOutline: WordBool); safecall;
    procedure Locality_LoadShape(const aShapeFile: WideString; const aLayerName: WideString; 
                                 aVisible: WordBool); safecall;
    procedure Renderer_RemoveAll(const aLayerName: WideString); safecall;
    function Renderer_Count(const aLayerName: WideString): Integer; safecall;
    function Renderer_Index(const aLayerName: WideString; const aRenderName: WideString; 
                            aRenderType: SYSINT): Integer; safecall;
    procedure Renderer_Remove(const aLayerName: WideString; aRenderIndex: Integer); safecall;
    procedure Renderer_RemoveName(const aLayerName: WideString; const aRenderName: WideString; 
                                  aRenderType: SYSINT); safecall;
    function Renderer_RendererName(const aLayerName: WideString; aRenderIndex: Integer): WideString; safecall;
    procedure Renderer_SetVisible(const aLayerName: WideString; const aRenderName: WideString; 
                                  aRenderType: SYSINT; aVisible: WordBool); safecall;
    function Renderer_RendererType(const aLayerName: WideString; aRenderIndex: Integer): SYSINT; safecall;
    procedure Query_Setup(const aQueryString: IStrings); safecall;
    function Query_GetFeatureResults(const aLayerName: WideString; const aFieldName: WideString; 
                                     const aLayerValue: WideString; aRefresh: WordBool): IStrings; safecall;
    function Query_GetExtentResults(aLeft: Single; aTop: Single; aRight: Single; aBottom: Single; 
                                    aRefresh: WordBool): IStrings; safecall;
    function Query_GetRadiusResults(aRadius: Double; aX: Double; aY: Double; aRefresh: WordBool): IStrings; safecall;
    procedure Query_LockedLayers(aX: Double; aY: Double; aRefresh: WordBool); safecall;
    function Query_GetMapLayers: IStrings; safecall;
    function Query_GetLayerFields(const aLayerName: WideString): IStrings; safecall;
    function Query_GetFieldValues(const aLayerName: WideString; const aFieldName: WideString): IStrings; safecall;
    procedure Query_SelectMapFeatures(const aLayerName: WideString; const aFieldName: WideString; 
                                      aSelectColor: Integer; const aValueList: IStrings; 
                                      aRefresh: WordBool); safecall;
    procedure Chart_ClearAll; safecall;
    procedure Chart_Add(const aFormat: WideString; const aPosition: WideString; 
                        const aData: WideString); safecall;
    procedure Chart_AddShape(const aLayerName: WideString; const aFieldName: WideString; 
                             const aValue: WideString; const aFormat: WideString; 
                             const aData: WideString); safecall;
    procedure Arrow_ClearAll; safecall;
    procedure Arrow_Add(const aArrow: WideString); safecall;
    procedure Arrow_AddShape(const aLayerName: WideString; const aFieldName: WideString; 
                             const aStartValue: WideString; const aEndValue: WideString; 
                             const aColor: WideString; aSize: Integer); safecall;
    procedure Display_ShowMessage(aShow: WordBool; aAssumeYes: WordBool); safecall;
    function Get_Display_DataTab: WordBool; safecall;
    procedure Set_Display_DataTab(aValue: WordBool); safecall;
    function Get_Display_ToolBar: WordBool; safecall;
    procedure Set_Display_ToolBar(aValue: WordBool); safecall;
    function Get_Display_Legend: WordBool; safecall;
    procedure Set_Display_Legend(aValue: WordBool); safecall;
    function Get_Display_Interface: WordBool; safecall;
    procedure Set_Display_Interface(aValue: WordBool); safecall;
    function Get_Display_ScrollBars: WordBool; safecall;
    procedure Set_Display_ScrollBars(aValue: WordBool); safecall;
    function Map_LonLatToXY(aLon: Double; aLat: Double): IStrings; safecall;
    function Map_XYToLonLat(aX: Integer; aY: Integer): IStrings; safecall;
    function Map_LastX: Double; safecall;
    function Map_LastY: Double; safecall;
    procedure Map_SetCursor(aCursor: SYSINT); safecall;
    procedure Map_SetPointer(aPointer: SYSINT); safecall;
    procedure Control_RefreshAll; safecall;
    procedure Control_ChangeCursor(aChange: WordBool); safecall;
    procedure Control_SetCursor(aCursor: SYSINT); safecall;
    procedure Control_StatusBar(const aStatus: WideString); safecall;
    function Control_MemoryLoad: Integer; safecall;
    function Get_Display_StatusBar: WordBool; safecall;
    procedure Set_Display_StatusBar(aValue: WordBool); safecall;
    function Query_GetAllResults: IStrings; safecall;
    function Query_GetResults(const aLayerName: WideString; const aFieldName: WideString): IStrings; safecall;
    procedure Query_ClearResults; safecall;
    procedure Query_ClearSQL; safecall;
    procedure Layers_RemoveLocked(aLocked: WordBool); safecall;
    procedure Layers_LoadXML(const aFilePath: WideString); safecall;
    procedure Control_Initialize; safecall;
    function Map_LOXYExtent: IStrings; safecall;
    function Layers_GetStats(const aLayerName: WideString; const aFieldName: WideString): IStrings; safecall;
    procedure Renderer_AddLabel(const aLayerName: WideString; const aFieldName: WideString; 
                                aAllowDuplicates: WordBool); safecall;
    procedure Renderer_AddClass(const aLayerName: WideString; const aFieldName: WideString; 
                                aMinColor: Integer; aMaxColor: Integer; aMinSize: Integer; 
                                aMaxSize: Integer; aBreaks: Integer; aMinValue: Double; 
                                aMaxValue: Double); safecall;
    procedure Renderer_AddValue(const aLayerName: WideString; const aFieldName: WideString; 
                                const aValueList: IStrings; const aColorList: IStrings); safecall;
    procedure Renderer_AddChart(const aLayerName: WideString; const aFieldName: WideString; 
                                const aRenderName: WideString; const aFormat: WideString; 
                                aMinColor: Integer; aMaxColor: Integer; aBreaks: Integer; 
                                const aValueList: IStrings); safecall;
    procedure Chart_AddLegend(const aFormat: WideString; const aData: WideString); safecall;
    procedure Chart_RemoveLegend; safecall;
    procedure Query_MultiQuery(aMultiQuery: WordBool); safecall;
    procedure Query_SetSelection(const aLayerName: WideString; const aFieldName: WideString; 
                                 const aValueString: WideString; aRefresh: WordBool); safecall;
    procedure Display_FlyoverHints(aShow: WordBool); safecall;
    procedure Display_ViewerHints(aShow: WordBool); safecall;
    function Map_GetFilePathCovers: WideString; safecall;
    function Map_GetFilePathXML: WideString; safecall;
    procedure Map_SetFilePathCovers(const aFilePath: WideString); safecall;
    procedure Map_SetFilePathXML(const aFilePath: WideString); safecall;
    function Map_GetLO: Single; safecall;
    function Map_GetEllipsoid: SYSINT; safecall;
    function Map_ConvertLonLatToXY(aLon: Single; aLat: Single; aLO: Single; aEllipse: SYSINT): IStrings; safecall;
    function Control_GetIStrings: IStrings; safecall;
    property Visible: WordBool read Get_Visible write Set_Visible;
    property AutoScroll: WordBool read Get_AutoScroll write Set_AutoScroll;
    property AutoSize: WordBool read Get_AutoSize write Set_AutoSize;
    property AxBorderStyle: TxActiveFormBorderStyle read Get_AxBorderStyle write Set_AxBorderStyle;
    property Caption: WideString read Get_Caption write Set_Caption;
    property Color: OLE_COLOR read Get_Color write Set_Color;
    property Font: IFontDisp read Get_Font write Set_Font;
    property KeyPreview: WordBool read Get_KeyPreview write Set_KeyPreview;
    property PixelsPerInch: Integer read Get_PixelsPerInch write Set_PixelsPerInch;
    property PrintScale: TxPrintScale read Get_PrintScale write Set_PrintScale;
    property Scaled: WordBool read Get_Scaled write Set_Scaled;
    property Active: WordBool read Get_Active;
    property DropTarget: WordBool read Get_DropTarget write Set_DropTarget;
    property HelpFile: WideString read Get_HelpFile write Set_HelpFile;
    property ScreenSnap: WordBool read Get_ScreenSnap write Set_ScreenSnap;
    property SnapBuffer: Integer read Get_SnapBuffer write Set_SnapBuffer;
    property DoubleBuffered: WordBool read Get_DoubleBuffered write Set_DoubleBuffered;
    property AlignDisabled: WordBool read Get_AlignDisabled;
    property VisibleDockClientCount: Integer read Get_VisibleDockClientCount;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
    property Extent_Left: Single read Get_Extent_Left write Set_Extent_Left;
    property Extent_Top: Single read Get_Extent_Top write Set_Extent_Top;
    property Extent_Right: Single read Get_Extent_Right write Set_Extent_Right;
    property Extent_Bottom: Single read Get_Extent_Bottom write Set_Extent_Bottom;
    property Display_DataTab: WordBool read Get_Display_DataTab write Set_Display_DataTab;
    property Display_ToolBar: WordBool read Get_Display_ToolBar write Set_Display_ToolBar;
    property Display_Legend: WordBool read Get_Display_Legend write Set_Display_Legend;
    property Display_Interface: WordBool read Get_Display_Interface write Set_Display_Interface;
    property Display_ScrollBars: WordBool read Get_Display_ScrollBars write Set_Display_ScrollBars;
    property Display_StatusBar: WordBool read Get_Display_StatusBar write Set_Display_StatusBar;
  end;

// *********************************************************************//
// DispIntf:  IGisViewerXDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {23556852-1BFD-457C-8A97-0DDFBF459827}
// *********************************************************************//
  IGisViewerXDisp = dispinterface
    ['{23556852-1BFD-457C-8A97-0DDFBF459827}']
    property Visible: WordBool dispid 201;
    property AutoScroll: WordBool dispid 202;
    property AutoSize: WordBool dispid 203;
    property AxBorderStyle: TxActiveFormBorderStyle dispid 204;
    property Caption: WideString dispid -518;
    property Color: OLE_COLOR dispid -501;
    property Font: IFontDisp dispid -512;
    property KeyPreview: WordBool dispid 205;
    property PixelsPerInch: Integer dispid 206;
    property PrintScale: TxPrintScale dispid 207;
    property Scaled: WordBool dispid 208;
    property Active: WordBool readonly dispid 209;
    property DropTarget: WordBool dispid 210;
    property HelpFile: WideString dispid 211;
    property ScreenSnap: WordBool dispid 212;
    property SnapBuffer: Integer dispid 213;
    property DoubleBuffered: WordBool dispid 214;
    property AlignDisabled: WordBool readonly dispid 215;
    property VisibleDockClientCount: Integer readonly dispid 216;
    property Enabled: WordBool dispid -514;
    procedure Extent_Set(aLeft: Single; aTop: Single; aRight: Single; aBottom: Single); dispid 217;
    property Extent_Left: Single dispid 218;
    property Extent_Top: Single dispid 219;
    property Extent_Right: Single dispid 220;
    property Extent_Bottom: Single dispid 221;
    procedure Output_Map(hDC: LongWord); dispid 222;
    procedure Output_Map2(hDC: LongWord; aX: Integer; aY: Integer; aWidth: Integer; 
                          aHeight: Integer; aDrawFlags: OleVariant); dispid 223;
    procedure Output_PrintMap(const aDocName: WideString; const aOutputFile: WideString; 
                              aLandscapeOrientation: WordBool); dispid 224;
    procedure Output_CopyMap(aScaleFactor: Double); dispid 225;
    procedure Output_ExportMap(aExportType: Integer; const aOutputFile: WideString; 
                               aScaleFactor: Double); dispid 226;
    procedure Output_ExportMap2(aExportType: Integer; const aOutputFile: WideString; 
                                aScaleFactor: Double; aUseSourceDepth: OleVariant); dispid 227;
    procedure Toolbar_Action(aToolAction: SYSINT); dispid 228;
    procedure Toolbar_Property(aToolAction: SYSINT; aVisible: WordBool; aEnabled: WordBool); dispid 229;
    function Toolbar_Mode(aTool: SYSINT): WordBool; dispid 230;
    function hWnd_Map: LongWord; dispid 231;
    function hWnd_Toolbar: LongWord; dispid 232;
    function hWnd_Legend: LongWord; dispid 233;
    function hWnd_Statusbar: LongWord; dispid 234;
    procedure GLF_SetFilePath1(const aFilePath: WideString); dispid 235;
    procedure GLF_SetFilePath2(const aFilePath: WideString); dispid 236;
    procedure GLF_SetFilePath3(const aFilePath: WideString); dispid 237;
    procedure GLF_SetFilePath4(const aFilePath: WideString); dispid 238;
    function GLF_GetFilePath1: WideString; dispid 239;
    function GLF_GetFilePath2: WideString; dispid 240;
    function GLF_GetFilePath3: WideString; dispid 241;
    function GLF_GetFilePath4: WideString; dispid 242;
    procedure Layers_RemoveAll; dispid 243;
    function Layers_Count: Integer; dispid 244;
    function Layers_Index(const aLayerName: WideString): Integer; dispid 245;
    procedure Layers_Remove(aLayerIndex: Integer); dispid 246;
    procedure Layers_RemoveName(const aLayerName: WideString); dispid 247;
    function Layers_LayerName(aLayerIndex: Integer): WideString; dispid 248;
    procedure Layers_SetVisible(const aLayerName: WideString; aVisible: WordBool); dispid 249;
    procedure Layers_SetSymbolProps(const aLayerName: WideString; aStyle: Integer; aSize: Integer; 
                                    aColor: Integer; aOutlineColor: Integer; aOutline: WordBool); dispid 250;
    procedure Layers_LoadShape(const aShapeFile: WideString; const aLayerName: WideString; 
                               aVisible: WordBool); dispid 251;
    procedure Layers_LoadImage(const aImageFile: WideString; const aLayerName: WideString; 
                               aVisible: WordBool); dispid 252;
    procedure Layers_LoadLayout(const aFilePath: WideString; aUseLocalPath: WordBool); dispid 253;
    procedure Layers_SaveLayout(const aFilePath: WideString); dispid 254;
    function Locality_Count: Integer; dispid 255;
    function Locality_Index(const aLayerName: WideString): Integer; dispid 256;
    procedure Locality_Remove(aLayerIndex: Integer); dispid 257;
    procedure Locality_RemoveName(const aLayerName: WideString); dispid 258;
    function Locality_LayerName(aLayerIndex: Integer): WideString; dispid 259;
    procedure Locality_SetVisible(const aLayerName: WideString; aVisible: WordBool); dispid 260;
    procedure Locality_SetSymbolProps(const aLayerName: WideString; aStyle: Integer; 
                                      aSize: Integer; aColor: Integer; aOutlineColor: Integer; 
                                      aOutline: WordBool); dispid 261;
    procedure Locality_LoadShape(const aShapeFile: WideString; const aLayerName: WideString; 
                                 aVisible: WordBool); dispid 262;
    procedure Renderer_RemoveAll(const aLayerName: WideString); dispid 263;
    function Renderer_Count(const aLayerName: WideString): Integer; dispid 264;
    function Renderer_Index(const aLayerName: WideString; const aRenderName: WideString; 
                            aRenderType: SYSINT): Integer; dispid 265;
    procedure Renderer_Remove(const aLayerName: WideString; aRenderIndex: Integer); dispid 266;
    procedure Renderer_RemoveName(const aLayerName: WideString; const aRenderName: WideString; 
                                  aRenderType: SYSINT); dispid 267;
    function Renderer_RendererName(const aLayerName: WideString; aRenderIndex: Integer): WideString; dispid 268;
    procedure Renderer_SetVisible(const aLayerName: WideString; const aRenderName: WideString; 
                                  aRenderType: SYSINT; aVisible: WordBool); dispid 269;
    function Renderer_RendererType(const aLayerName: WideString; aRenderIndex: Integer): SYSINT; dispid 270;
    procedure Query_Setup(const aQueryString: IStrings); dispid 271;
    function Query_GetFeatureResults(const aLayerName: WideString; const aFieldName: WideString; 
                                     const aLayerValue: WideString; aRefresh: WordBool): IStrings; dispid 272;
    function Query_GetExtentResults(aLeft: Single; aTop: Single; aRight: Single; aBottom: Single; 
                                    aRefresh: WordBool): IStrings; dispid 273;
    function Query_GetRadiusResults(aRadius: Double; aX: Double; aY: Double; aRefresh: WordBool): IStrings; dispid 274;
    procedure Query_LockedLayers(aX: Double; aY: Double; aRefresh: WordBool); dispid 275;
    function Query_GetMapLayers: IStrings; dispid 276;
    function Query_GetLayerFields(const aLayerName: WideString): IStrings; dispid 277;
    function Query_GetFieldValues(const aLayerName: WideString; const aFieldName: WideString): IStrings; dispid 278;
    procedure Query_SelectMapFeatures(const aLayerName: WideString; const aFieldName: WideString; 
                                      aSelectColor: Integer; const aValueList: IStrings; 
                                      aRefresh: WordBool); dispid 279;
    procedure Chart_ClearAll; dispid 280;
    procedure Chart_Add(const aFormat: WideString; const aPosition: WideString; 
                        const aData: WideString); dispid 281;
    procedure Chart_AddShape(const aLayerName: WideString; const aFieldName: WideString; 
                             const aValue: WideString; const aFormat: WideString; 
                             const aData: WideString); dispid 282;
    procedure Arrow_ClearAll; dispid 283;
    procedure Arrow_Add(const aArrow: WideString); dispid 284;
    procedure Arrow_AddShape(const aLayerName: WideString; const aFieldName: WideString; 
                             const aStartValue: WideString; const aEndValue: WideString; 
                             const aColor: WideString; aSize: Integer); dispid 285;
    procedure Display_ShowMessage(aShow: WordBool; aAssumeYes: WordBool); dispid 286;
    property Display_DataTab: WordBool dispid 288;
    property Display_ToolBar: WordBool dispid 290;
    property Display_Legend: WordBool dispid 291;
    property Display_Interface: WordBool dispid 292;
    property Display_ScrollBars: WordBool dispid 293;
    function Map_LonLatToXY(aLon: Double; aLat: Double): IStrings; dispid 294;
    function Map_XYToLonLat(aX: Integer; aY: Integer): IStrings; dispid 295;
    function Map_LastX: Double; dispid 296;
    function Map_LastY: Double; dispid 297;
    procedure Map_SetCursor(aCursor: SYSINT); dispid 298;
    procedure Map_SetPointer(aPointer: SYSINT); dispid 299;
    procedure Control_RefreshAll; dispid 300;
    procedure Control_ChangeCursor(aChange: WordBool); dispid 301;
    procedure Control_SetCursor(aCursor: SYSINT); dispid 302;
    procedure Control_StatusBar(const aStatus: WideString); dispid 303;
    function Control_MemoryLoad: Integer; dispid 304;
    property Display_StatusBar: WordBool dispid 289;
    function Query_GetAllResults: IStrings; dispid 307;
    function Query_GetResults(const aLayerName: WideString; const aFieldName: WideString): IStrings; dispid 306;
    procedure Query_ClearResults; dispid 308;
    procedure Query_ClearSQL; dispid 305;
    procedure Layers_RemoveLocked(aLocked: WordBool); dispid 309;
    procedure Layers_LoadXML(const aFilePath: WideString); dispid 310;
    procedure Control_Initialize; dispid 311;
    function Map_LOXYExtent: IStrings; dispid 312;
    function Layers_GetStats(const aLayerName: WideString; const aFieldName: WideString): IStrings; dispid 313;
    procedure Renderer_AddLabel(const aLayerName: WideString; const aFieldName: WideString; 
                                aAllowDuplicates: WordBool); dispid 314;
    procedure Renderer_AddClass(const aLayerName: WideString; const aFieldName: WideString; 
                                aMinColor: Integer; aMaxColor: Integer; aMinSize: Integer; 
                                aMaxSize: Integer; aBreaks: Integer; aMinValue: Double; 
                                aMaxValue: Double); dispid 315;
    procedure Renderer_AddValue(const aLayerName: WideString; const aFieldName: WideString; 
                                const aValueList: IStrings; const aColorList: IStrings); dispid 316;
    procedure Renderer_AddChart(const aLayerName: WideString; const aFieldName: WideString; 
                                const aRenderName: WideString; const aFormat: WideString; 
                                aMinColor: Integer; aMaxColor: Integer; aBreaks: Integer; 
                                const aValueList: IStrings); dispid 317;
    procedure Chart_AddLegend(const aFormat: WideString; const aData: WideString); dispid 318;
    procedure Chart_RemoveLegend; dispid 319;
    procedure Query_MultiQuery(aMultiQuery: WordBool); dispid 320;
    procedure Query_SetSelection(const aLayerName: WideString; const aFieldName: WideString; 
                                 const aValueString: WideString; aRefresh: WordBool); dispid 321;
    procedure Display_FlyoverHints(aShow: WordBool); dispid 322;
    procedure Display_ViewerHints(aShow: WordBool); dispid 323;
    function Map_GetFilePathCovers: WideString; dispid 324;
    function Map_GetFilePathXML: WideString; dispid 325;
    procedure Map_SetFilePathCovers(const aFilePath: WideString); dispid 326;
    procedure Map_SetFilePathXML(const aFilePath: WideString); dispid 327;
    function Map_GetLO: Single; dispid 287;
    function Map_GetEllipsoid: SYSINT; dispid 328;
    function Map_ConvertLonLatToXY(aLon: Single; aLat: Single; aLO: Single; aEllipse: SYSINT): IStrings; dispid 329;
    function Control_GetIStrings: IStrings; dispid 330;
  end;

// *********************************************************************//
// DispIntf:  IGisViewerXEvents
// Flags:     (4096) Dispatchable
// GUID:      {2CEAE78C-9252-4DF1-9CBD-46426278C84C}
// *********************************************************************//
  IGisViewerXEvents = dispinterface
    ['{2CEAE78C-9252-4DF1-9CBD-46426278C84C}']
    procedure OnActivate; dispid 201;
    procedure OnClick; dispid 202;
    procedure OnCreate; dispid 203;
    procedure OnDblClick; dispid 204;
    procedure OnDestroy; dispid 205;
    procedure OnDeactivate; dispid 206;
    procedure OnKeyPress(var Key: Smallint); dispid 207;
    procedure OnPaint; dispid 208;
    procedure OnAfterLayerDraw(index: Smallint; cancelled: WordBool; hDC: LongWord); dispid 209;
    procedure OnBeforeLayerDraw(index: Smallint; hDC: LongWord); dispid 210;
    procedure OnMapMouseMove(Shift: Integer; X: Integer; Y: Integer); dispid 211;
    procedure OnMapMouseDown(Button: TxMouseButton; Shift: Integer; X: Integer; Y: Integer); dispid 212;
    procedure OnMapMouseUp(Button: TxMouseButton; Shift: Integer; X: Integer; Y: Integer); dispid 213;
    procedure OnGisEvent(aGisEvent: SYSINT); dispid 214;
    procedure OnToolbarRequest(aToolAction: SYSINT; var aCancel: WordBool); dispid 215;
    procedure OnMapDblClick; dispid 216;
    procedure OnShowHint(const aHint: WideString); dispid 217;
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TGisViewerX
// Help String      : GisViewerX Control
// Default Interface: IGisViewerX
// Def. Intf. DISP? : No
// Event   Interface: IGisViewerXEvents
// TypeFlags        : (34) CanCreate Control
// *********************************************************************//
  TGisViewerXOnKeyPress = procedure(ASender: TObject; var Key: Smallint) of object;
  TGisViewerXOnAfterLayerDraw = procedure(ASender: TObject; index: Smallint; cancelled: WordBool; 
                                                            hDC: LongWord) of object;
  TGisViewerXOnBeforeLayerDraw = procedure(ASender: TObject; index: Smallint; hDC: LongWord) of object;
  TGisViewerXOnMapMouseMove = procedure(ASender: TObject; Shift: Integer; X: Integer; Y: Integer) of object;
  TGisViewerXOnMapMouseDown = procedure(ASender: TObject; Button: TxMouseButton; Shift: Integer; 
                                                          X: Integer; Y: Integer) of object;
  TGisViewerXOnMapMouseUp = procedure(ASender: TObject; Button: TxMouseButton; Shift: Integer; 
                                                        X: Integer; Y: Integer) of object;
  TGisViewerXOnGisEvent = procedure(ASender: TObject; aGisEvent: SYSINT) of object;
  TGisViewerXOnToolbarRequest = procedure(ASender: TObject; aToolAction: SYSINT; 
                                                            var aCancel: WordBool) of object;
  TGisViewerXOnShowHint = procedure(ASender: TObject; const aHint: WideString) of object;

  TGisViewerX = class(TOleControl)
  private
    FOnActivate: TNotifyEvent;
    FOnClick: TNotifyEvent;
    FOnCreate: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FOnKeyPress: TGisViewerXOnKeyPress;
    FOnPaint: TNotifyEvent;
    FOnAfterLayerDraw: TGisViewerXOnAfterLayerDraw;
    FOnBeforeLayerDraw: TGisViewerXOnBeforeLayerDraw;
    FOnMapMouseMove: TGisViewerXOnMapMouseMove;
    FOnMapMouseDown: TGisViewerXOnMapMouseDown;
    FOnMapMouseUp: TGisViewerXOnMapMouseUp;
    FOnGisEvent: TGisViewerXOnGisEvent;
    FOnToolbarRequest: TGisViewerXOnToolbarRequest;
    FOnMapDblClick: TNotifyEvent;
    FOnShowHint: TGisViewerXOnShowHint;
    FIntf: IGisViewerX;
    function  GetControlInterface: IGisViewerX;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
  public
    procedure Extent_Set(aLeft: Single; aTop: Single; aRight: Single; aBottom: Single);
    procedure Output_Map(hDC: LongWord);
    procedure Output_Map2(hDC: LongWord; aX: Integer; aY: Integer; aWidth: Integer; 
                          aHeight: Integer; aDrawFlags: OleVariant);
    procedure Output_PrintMap(const aDocName: WideString; const aOutputFile: WideString; 
                              aLandscapeOrientation: WordBool);
    procedure Output_CopyMap(aScaleFactor: Double);
    procedure Output_ExportMap(aExportType: Integer; const aOutputFile: WideString; 
                               aScaleFactor: Double);
    procedure Output_ExportMap2(aExportType: Integer; const aOutputFile: WideString; 
                                aScaleFactor: Double; aUseSourceDepth: OleVariant);
    procedure Toolbar_Action(aToolAction: SYSINT);
    procedure Toolbar_Property(aToolAction: SYSINT; aVisible: WordBool; aEnabled: WordBool);
    function Toolbar_Mode(aTool: SYSINT): WordBool;
    function hWnd_Map: LongWord;
    function hWnd_Toolbar: LongWord;
    function hWnd_Legend: LongWord;
    function hWnd_Statusbar: LongWord;
    procedure GLF_SetFilePath1(const aFilePath: WideString);
    procedure GLF_SetFilePath2(const aFilePath: WideString);
    procedure GLF_SetFilePath3(const aFilePath: WideString);
    procedure GLF_SetFilePath4(const aFilePath: WideString);
    function GLF_GetFilePath1: WideString;
    function GLF_GetFilePath2: WideString;
    function GLF_GetFilePath3: WideString;
    function GLF_GetFilePath4: WideString;
    procedure Layers_RemoveAll;
    function Layers_Count: Integer;
    function Layers_Index(const aLayerName: WideString): Integer;
    procedure Layers_Remove(aLayerIndex: Integer);
    procedure Layers_RemoveName(const aLayerName: WideString);
    function Layers_LayerName(aLayerIndex: Integer): WideString;
    procedure Layers_SetVisible(const aLayerName: WideString; aVisible: WordBool);
    procedure Layers_SetSymbolProps(const aLayerName: WideString; aStyle: Integer; aSize: Integer; 
                                    aColor: Integer; aOutlineColor: Integer; aOutline: WordBool);
    procedure Layers_LoadShape(const aShapeFile: WideString; const aLayerName: WideString; 
                               aVisible: WordBool);
    procedure Layers_LoadImage(const aImageFile: WideString; const aLayerName: WideString; 
                               aVisible: WordBool);
    procedure Layers_LoadLayout(const aFilePath: WideString; aUseLocalPath: WordBool);
    procedure Layers_SaveLayout(const aFilePath: WideString);
    function Locality_Count: Integer;
    function Locality_Index(const aLayerName: WideString): Integer;
    procedure Locality_Remove(aLayerIndex: Integer);
    procedure Locality_RemoveName(const aLayerName: WideString);
    function Locality_LayerName(aLayerIndex: Integer): WideString;
    procedure Locality_SetVisible(const aLayerName: WideString; aVisible: WordBool);
    procedure Locality_SetSymbolProps(const aLayerName: WideString; aStyle: Integer; 
                                      aSize: Integer; aColor: Integer; aOutlineColor: Integer; 
                                      aOutline: WordBool);
    procedure Locality_LoadShape(const aShapeFile: WideString; const aLayerName: WideString; 
                                 aVisible: WordBool);
    procedure Renderer_RemoveAll(const aLayerName: WideString);
    function Renderer_Count(const aLayerName: WideString): Integer;
    function Renderer_Index(const aLayerName: WideString; const aRenderName: WideString; 
                            aRenderType: SYSINT): Integer;
    procedure Renderer_Remove(const aLayerName: WideString; aRenderIndex: Integer);
    procedure Renderer_RemoveName(const aLayerName: WideString; const aRenderName: WideString; 
                                  aRenderType: SYSINT);
    function Renderer_RendererName(const aLayerName: WideString; aRenderIndex: Integer): WideString;
    procedure Renderer_SetVisible(const aLayerName: WideString; const aRenderName: WideString; 
                                  aRenderType: SYSINT; aVisible: WordBool);
    function Renderer_RendererType(const aLayerName: WideString; aRenderIndex: Integer): SYSINT;
    procedure Query_Setup(const aQueryString: IStrings);
    function Query_GetFeatureResults(const aLayerName: WideString; const aFieldName: WideString; 
                                     const aLayerValue: WideString; aRefresh: WordBool): IStrings;
    function Query_GetExtentResults(aLeft: Single; aTop: Single; aRight: Single; aBottom: Single; 
                                    aRefresh: WordBool): IStrings;
    function Query_GetRadiusResults(aRadius: Double; aX: Double; aY: Double; aRefresh: WordBool): IStrings;
    procedure Query_LockedLayers(aX: Double; aY: Double; aRefresh: WordBool);
    function Query_GetMapLayers: IStrings;
    function Query_GetLayerFields(const aLayerName: WideString): IStrings;
    function Query_GetFieldValues(const aLayerName: WideString; const aFieldName: WideString): IStrings;
    procedure Query_SelectMapFeatures(const aLayerName: WideString; const aFieldName: WideString; 
                                      aSelectColor: Integer; const aValueList: IStrings; 
                                      aRefresh: WordBool);
    procedure Chart_ClearAll;
    procedure Chart_Add(const aFormat: WideString; const aPosition: WideString; 
                        const aData: WideString);
    procedure Chart_AddShape(const aLayerName: WideString; const aFieldName: WideString; 
                             const aValue: WideString; const aFormat: WideString; 
                             const aData: WideString);
    procedure Arrow_ClearAll;
    procedure Arrow_Add(const aArrow: WideString);
    procedure Arrow_AddShape(const aLayerName: WideString; const aFieldName: WideString; 
                             const aStartValue: WideString; const aEndValue: WideString; 
                             const aColor: WideString; aSize: Integer);
    procedure Display_ShowMessage(aShow: WordBool; aAssumeYes: WordBool);
    function Map_LonLatToXY(aLon: Double; aLat: Double): IStrings;
    function Map_XYToLonLat(aX: Integer; aY: Integer): IStrings;
    function Map_LastX: Double;
    function Map_LastY: Double;
    procedure Map_SetCursor(aCursor: SYSINT);
    procedure Map_SetPointer(aPointer: SYSINT);
    procedure Control_RefreshAll;
    procedure Control_ChangeCursor(aChange: WordBool);
    procedure Control_SetCursor(aCursor: SYSINT);
    procedure Control_StatusBar(const aStatus: WideString);
    function Control_MemoryLoad: Integer;
    function Query_GetAllResults: IStrings;
    function Query_GetResults(const aLayerName: WideString; const aFieldName: WideString): IStrings;
    procedure Query_ClearResults;
    procedure Query_ClearSQL;
    procedure Layers_RemoveLocked(aLocked: WordBool);
    procedure Layers_LoadXML(const aFilePath: WideString);
    procedure Control_Initialize;
    function Map_LOXYExtent: IStrings;
    function Layers_GetStats(const aLayerName: WideString; const aFieldName: WideString): IStrings;
    procedure Renderer_AddLabel(const aLayerName: WideString; const aFieldName: WideString; 
                                aAllowDuplicates: WordBool);
    procedure Renderer_AddClass(const aLayerName: WideString; const aFieldName: WideString; 
                                aMinColor: Integer; aMaxColor: Integer; aMinSize: Integer; 
                                aMaxSize: Integer; aBreaks: Integer; aMinValue: Double; 
                                aMaxValue: Double);
    procedure Renderer_AddValue(const aLayerName: WideString; const aFieldName: WideString; 
                                const aValueList: IStrings; const aColorList: IStrings);
    procedure Renderer_AddChart(const aLayerName: WideString; const aFieldName: WideString; 
                                const aRenderName: WideString; const aFormat: WideString; 
                                aMinColor: Integer; aMaxColor: Integer; aBreaks: Integer; 
                                const aValueList: IStrings);
    procedure Chart_AddLegend(const aFormat: WideString; const aData: WideString);
    procedure Chart_RemoveLegend;
    procedure Query_MultiQuery(aMultiQuery: WordBool);
    procedure Query_SetSelection(const aLayerName: WideString; const aFieldName: WideString; 
                                 const aValueString: WideString; aRefresh: WordBool);
    procedure Display_FlyoverHints(aShow: WordBool);
    procedure Display_ViewerHints(aShow: WordBool);
    function Map_GetFilePathCovers: WideString;
    function Map_GetFilePathXML: WideString;
    procedure Map_SetFilePathCovers(const aFilePath: WideString);
    procedure Map_SetFilePathXML(const aFilePath: WideString);
    function Map_GetLO: Single;
    function Map_GetEllipsoid: SYSINT;
    function Map_ConvertLonLatToXY(aLon: Single; aLat: Single; aLO: Single; aEllipse: SYSINT): IStrings;
    function Control_GetIStrings: IStrings;
    property  ControlInterface: IGisViewerX read GetControlInterface;
    property  DefaultInterface: IGisViewerX read GetControlInterface;
    property Visible: WordBool index 201 read GetWordBoolProp write SetWordBoolProp;
    property Active: WordBool index 209 read GetWordBoolProp;
    property DropTarget: WordBool index 210 read GetWordBoolProp write SetWordBoolProp;
    property HelpFile: WideString index 211 read GetWideStringProp write SetWideStringProp;
    property ScreenSnap: WordBool index 212 read GetWordBoolProp write SetWordBoolProp;
    property SnapBuffer: Integer index 213 read GetIntegerProp write SetIntegerProp;
    property DoubleBuffered: WordBool index 214 read GetWordBoolProp write SetWordBoolProp;
    property AlignDisabled: WordBool index 215 read GetWordBoolProp;
    property VisibleDockClientCount: Integer index 216 read GetIntegerProp;
    property Enabled: WordBool index -514 read GetWordBoolProp write SetWordBoolProp;
  published
    property Anchors;
    property  ParentColor;
    property  ParentFont;
    property  Align;
    property  DragCursor;
    property  DragMode;
    property  ParentShowHint;
    property  PopupMenu;
    property  ShowHint;
    property  TabOrder;
    property  OnDragDrop;
    property  OnDragOver;
    property  OnEndDrag;
    property  OnEnter;
    property  OnExit;
    property  OnStartDrag;
    property AutoScroll: WordBool index 202 read GetWordBoolProp write SetWordBoolProp stored False;
    property AutoSize: WordBool index 203 read GetWordBoolProp write SetWordBoolProp stored False;
    property AxBorderStyle: TOleEnum index 204 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property Caption: WideString index -518 read GetWideStringProp write SetWideStringProp stored False;
    property Color: TColor index -501 read GetTColorProp write SetTColorProp stored False;
    property Font: TFont index -512 read GetTFontProp write SetTFontProp stored False;
    property KeyPreview: WordBool index 205 read GetWordBoolProp write SetWordBoolProp stored False;
    property PixelsPerInch: Integer index 206 read GetIntegerProp write SetIntegerProp stored False;
    property PrintScale: TOleEnum index 207 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property Scaled: WordBool index 208 read GetWordBoolProp write SetWordBoolProp stored False;
    property Extent_Left: Single index 218 read GetSingleProp write SetSingleProp stored False;
    property Extent_Top: Single index 219 read GetSingleProp write SetSingleProp stored False;
    property Extent_Right: Single index 220 read GetSingleProp write SetSingleProp stored False;
    property Extent_Bottom: Single index 221 read GetSingleProp write SetSingleProp stored False;
    property Display_DataTab: WordBool index 288 read GetWordBoolProp write SetWordBoolProp stored False;
    property Display_ToolBar: WordBool index 290 read GetWordBoolProp write SetWordBoolProp stored False;
    property Display_Legend: WordBool index 291 read GetWordBoolProp write SetWordBoolProp stored False;
    property Display_Interface: WordBool index 292 read GetWordBoolProp write SetWordBoolProp stored False;
    property Display_ScrollBars: WordBool index 293 read GetWordBoolProp write SetWordBoolProp stored False;
    property Display_StatusBar: WordBool index 289 read GetWordBoolProp write SetWordBoolProp stored False;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnKeyPress: TGisViewerXOnKeyPress read FOnKeyPress write FOnKeyPress;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnAfterLayerDraw: TGisViewerXOnAfterLayerDraw read FOnAfterLayerDraw write FOnAfterLayerDraw;
    property OnBeforeLayerDraw: TGisViewerXOnBeforeLayerDraw read FOnBeforeLayerDraw write FOnBeforeLayerDraw;
    property OnMapMouseMove: TGisViewerXOnMapMouseMove read FOnMapMouseMove write FOnMapMouseMove;
    property OnMapMouseDown: TGisViewerXOnMapMouseDown read FOnMapMouseDown write FOnMapMouseDown;
    property OnMapMouseUp: TGisViewerXOnMapMouseUp read FOnMapMouseUp write FOnMapMouseUp;
    property OnGisEvent: TGisViewerXOnGisEvent read FOnGisEvent write FOnGisEvent;
    property OnToolbarRequest: TGisViewerXOnToolbarRequest read FOnToolbarRequest write FOnToolbarRequest;
    property OnMapDblClick: TNotifyEvent read FOnMapDblClick write FOnMapDblClick;
    property OnShowHint: TGisViewerXOnShowHint read FOnShowHint write FOnShowHint;
  end;

procedure Register;

resourcestring
  dtlServerPage = 'ActiveX';

  dtlOcxPage = 'ActiveX';

implementation

uses ComObj;

procedure TGisViewerX.InitControlData;
const
  CEventDispIDs: array [0..16] of DWORD = (
    $000000C9, $000000CA, $000000CB, $000000CC, $000000CD, $000000CE,
    $000000CF, $000000D0, $000000D1, $000000D2, $000000D3, $000000D4,
    $000000D5, $000000D6, $000000D7, $000000D8, $000000D9);
  CTFontIDs: array [0..0] of DWORD = (
    $FFFFFE00);
  CControlData: TControlData2 = (
    ClassID: '{3D7F3F4B-BE3E-4E59-85E3-93F643A0435E}';
    EventIID: '{2CEAE78C-9252-4DF1-9CBD-46426278C84C}';
    EventCount: 17;
    EventDispIDs: @CEventDispIDs;
    LicenseKey: nil (*HR:$00000000*);
    Flags: $0000001D;
    Version: 401;
    FontCount: 1;
    FontIDs: @CTFontIDs);
begin
  ControlData := @CControlData;
  TControlData2(CControlData).FirstEventOfs := Cardinal(@@FOnActivate) - Cardinal(Self);
end;

procedure TGisViewerX.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as IGisViewerX;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TGisViewerX.GetControlInterface: IGisViewerX;
begin
  CreateControl;
  Result := FIntf;
end;

procedure TGisViewerX.Extent_Set(aLeft: Single; aTop: Single; aRight: Single; aBottom: Single);
begin
  DefaultInterface.Extent_Set(aLeft, aTop, aRight, aBottom);
end;

procedure TGisViewerX.Output_Map(hDC: LongWord);
begin
  DefaultInterface.Output_Map(hDC);
end;

procedure TGisViewerX.Output_Map2(hDC: LongWord; aX: Integer; aY: Integer; aWidth: Integer; 
                                  aHeight: Integer; aDrawFlags: OleVariant);
begin
  DefaultInterface.Output_Map2(hDC, aX, aY, aWidth, aHeight, aDrawFlags);
end;

procedure TGisViewerX.Output_PrintMap(const aDocName: WideString; const aOutputFile: WideString; 
                                      aLandscapeOrientation: WordBool);
begin
  DefaultInterface.Output_PrintMap(aDocName, aOutputFile, aLandscapeOrientation);
end;

procedure TGisViewerX.Output_CopyMap(aScaleFactor: Double);
begin
  DefaultInterface.Output_CopyMap(aScaleFactor);
end;

procedure TGisViewerX.Output_ExportMap(aExportType: Integer; const aOutputFile: WideString; 
                                       aScaleFactor: Double);
begin
  DefaultInterface.Output_ExportMap(aExportType, aOutputFile, aScaleFactor);
end;

procedure TGisViewerX.Output_ExportMap2(aExportType: Integer; const aOutputFile: WideString; 
                                        aScaleFactor: Double; aUseSourceDepth: OleVariant);
begin
  DefaultInterface.Output_ExportMap2(aExportType, aOutputFile, aScaleFactor, aUseSourceDepth);
end;

procedure TGisViewerX.Toolbar_Action(aToolAction: SYSINT);
begin
  DefaultInterface.Toolbar_Action(aToolAction);
end;

procedure TGisViewerX.Toolbar_Property(aToolAction: SYSINT; aVisible: WordBool; aEnabled: WordBool);
begin
  DefaultInterface.Toolbar_Property(aToolAction, aVisible, aEnabled);
end;

function TGisViewerX.Toolbar_Mode(aTool: SYSINT): WordBool;
begin
  Result := DefaultInterface.Toolbar_Mode(aTool);
end;

function TGisViewerX.hWnd_Map: LongWord;
begin
  Result := DefaultInterface.hWnd_Map;
end;

function TGisViewerX.hWnd_Toolbar: LongWord;
begin
  Result := DefaultInterface.hWnd_Toolbar;
end;

function TGisViewerX.hWnd_Legend: LongWord;
begin
  Result := DefaultInterface.hWnd_Legend;
end;

function TGisViewerX.hWnd_Statusbar: LongWord;
begin
  Result := DefaultInterface.hWnd_Statusbar;
end;

procedure TGisViewerX.GLF_SetFilePath1(const aFilePath: WideString);
begin
  DefaultInterface.GLF_SetFilePath1(aFilePath);
end;

procedure TGisViewerX.GLF_SetFilePath2(const aFilePath: WideString);
begin
  DefaultInterface.GLF_SetFilePath2(aFilePath);
end;

procedure TGisViewerX.GLF_SetFilePath3(const aFilePath: WideString);
begin
  DefaultInterface.GLF_SetFilePath3(aFilePath);
end;

procedure TGisViewerX.GLF_SetFilePath4(const aFilePath: WideString);
begin
  DefaultInterface.GLF_SetFilePath4(aFilePath);
end;

function TGisViewerX.GLF_GetFilePath1: WideString;
begin
  Result := DefaultInterface.GLF_GetFilePath1;
end;

function TGisViewerX.GLF_GetFilePath2: WideString;
begin
  Result := DefaultInterface.GLF_GetFilePath2;
end;

function TGisViewerX.GLF_GetFilePath3: WideString;
begin
  Result := DefaultInterface.GLF_GetFilePath3;
end;

function TGisViewerX.GLF_GetFilePath4: WideString;
begin
  Result := DefaultInterface.GLF_GetFilePath4;
end;

procedure TGisViewerX.Layers_RemoveAll;
begin
  DefaultInterface.Layers_RemoveAll;
end;

function TGisViewerX.Layers_Count: Integer;
begin
  Result := DefaultInterface.Layers_Count;
end;

function TGisViewerX.Layers_Index(const aLayerName: WideString): Integer;
begin
  Result := DefaultInterface.Layers_Index(aLayerName);
end;

procedure TGisViewerX.Layers_Remove(aLayerIndex: Integer);
begin
  DefaultInterface.Layers_Remove(aLayerIndex);
end;

procedure TGisViewerX.Layers_RemoveName(const aLayerName: WideString);
begin
  DefaultInterface.Layers_RemoveName(aLayerName);
end;

function TGisViewerX.Layers_LayerName(aLayerIndex: Integer): WideString;
begin
  Result := DefaultInterface.Layers_LayerName(aLayerIndex);
end;

procedure TGisViewerX.Layers_SetVisible(const aLayerName: WideString; aVisible: WordBool);
begin
  DefaultInterface.Layers_SetVisible(aLayerName, aVisible);
end;

procedure TGisViewerX.Layers_SetSymbolProps(const aLayerName: WideString; aStyle: Integer; 
                                            aSize: Integer; aColor: Integer; 
                                            aOutlineColor: Integer; aOutline: WordBool);
begin
  DefaultInterface.Layers_SetSymbolProps(aLayerName, aStyle, aSize, aColor, aOutlineColor, aOutline);
end;

procedure TGisViewerX.Layers_LoadShape(const aShapeFile: WideString; const aLayerName: WideString; 
                                       aVisible: WordBool);
begin
  DefaultInterface.Layers_LoadShape(aShapeFile, aLayerName, aVisible);
end;

procedure TGisViewerX.Layers_LoadImage(const aImageFile: WideString; const aLayerName: WideString; 
                                       aVisible: WordBool);
begin
  DefaultInterface.Layers_LoadImage(aImageFile, aLayerName, aVisible);
end;

procedure TGisViewerX.Layers_LoadLayout(const aFilePath: WideString; aUseLocalPath: WordBool);
begin
  DefaultInterface.Layers_LoadLayout(aFilePath, aUseLocalPath);
end;

procedure TGisViewerX.Layers_SaveLayout(const aFilePath: WideString);
begin
  DefaultInterface.Layers_SaveLayout(aFilePath);
end;

function TGisViewerX.Locality_Count: Integer;
begin
  Result := DefaultInterface.Locality_Count;
end;

function TGisViewerX.Locality_Index(const aLayerName: WideString): Integer;
begin
  Result := DefaultInterface.Locality_Index(aLayerName);
end;

procedure TGisViewerX.Locality_Remove(aLayerIndex: Integer);
begin
  DefaultInterface.Locality_Remove(aLayerIndex);
end;

procedure TGisViewerX.Locality_RemoveName(const aLayerName: WideString);
begin
  DefaultInterface.Locality_RemoveName(aLayerName);
end;

function TGisViewerX.Locality_LayerName(aLayerIndex: Integer): WideString;
begin
  Result := DefaultInterface.Locality_LayerName(aLayerIndex);
end;

procedure TGisViewerX.Locality_SetVisible(const aLayerName: WideString; aVisible: WordBool);
begin
  DefaultInterface.Locality_SetVisible(aLayerName, aVisible);
end;

procedure TGisViewerX.Locality_SetSymbolProps(const aLayerName: WideString; aStyle: Integer; 
                                              aSize: Integer; aColor: Integer; 
                                              aOutlineColor: Integer; aOutline: WordBool);
begin
  DefaultInterface.Locality_SetSymbolProps(aLayerName, aStyle, aSize, aColor, aOutlineColor, 
                                           aOutline);
end;

procedure TGisViewerX.Locality_LoadShape(const aShapeFile: WideString; 
                                         const aLayerName: WideString; aVisible: WordBool);
begin
  DefaultInterface.Locality_LoadShape(aShapeFile, aLayerName, aVisible);
end;

procedure TGisViewerX.Renderer_RemoveAll(const aLayerName: WideString);
begin
  DefaultInterface.Renderer_RemoveAll(aLayerName);
end;

function TGisViewerX.Renderer_Count(const aLayerName: WideString): Integer;
begin
  Result := DefaultInterface.Renderer_Count(aLayerName);
end;

function TGisViewerX.Renderer_Index(const aLayerName: WideString; const aRenderName: WideString; 
                                    aRenderType: SYSINT): Integer;
begin
  Result := DefaultInterface.Renderer_Index(aLayerName, aRenderName, aRenderType);
end;

procedure TGisViewerX.Renderer_Remove(const aLayerName: WideString; aRenderIndex: Integer);
begin
  DefaultInterface.Renderer_Remove(aLayerName, aRenderIndex);
end;

procedure TGisViewerX.Renderer_RemoveName(const aLayerName: WideString; 
                                          const aRenderName: WideString; aRenderType: SYSINT);
begin
  DefaultInterface.Renderer_RemoveName(aLayerName, aRenderName, aRenderType);
end;

function TGisViewerX.Renderer_RendererName(const aLayerName: WideString; aRenderIndex: Integer): WideString;
begin
  Result := DefaultInterface.Renderer_RendererName(aLayerName, aRenderIndex);
end;

procedure TGisViewerX.Renderer_SetVisible(const aLayerName: WideString; 
                                          const aRenderName: WideString; aRenderType: SYSINT; 
                                          aVisible: WordBool);
begin
  DefaultInterface.Renderer_SetVisible(aLayerName, aRenderName, aRenderType, aVisible);
end;

function TGisViewerX.Renderer_RendererType(const aLayerName: WideString; aRenderIndex: Integer): SYSINT;
begin
  Result := DefaultInterface.Renderer_RendererType(aLayerName, aRenderIndex);
end;

procedure TGisViewerX.Query_Setup(const aQueryString: IStrings);
begin
  DefaultInterface.Query_Setup(aQueryString);
end;

function TGisViewerX.Query_GetFeatureResults(const aLayerName: WideString; 
                                             const aFieldName: WideString; 
                                             const aLayerValue: WideString; aRefresh: WordBool): IStrings;
begin
  Result := DefaultInterface.Query_GetFeatureResults(aLayerName, aFieldName, aLayerValue, aRefresh);
end;

function TGisViewerX.Query_GetExtentResults(aLeft: Single; aTop: Single; aRight: Single; 
                                            aBottom: Single; aRefresh: WordBool): IStrings;
begin
  Result := DefaultInterface.Query_GetExtentResults(aLeft, aTop, aRight, aBottom, aRefresh);
end;

function TGisViewerX.Query_GetRadiusResults(aRadius: Double; aX: Double; aY: Double; 
                                            aRefresh: WordBool): IStrings;
begin
  Result := DefaultInterface.Query_GetRadiusResults(aRadius, aX, aY, aRefresh);
end;

procedure TGisViewerX.Query_LockedLayers(aX: Double; aY: Double; aRefresh: WordBool);
begin
  DefaultInterface.Query_LockedLayers(aX, aY, aRefresh);
end;

function TGisViewerX.Query_GetMapLayers: IStrings;
begin
  Result := DefaultInterface.Query_GetMapLayers;
end;

function TGisViewerX.Query_GetLayerFields(const aLayerName: WideString): IStrings;
begin
  Result := DefaultInterface.Query_GetLayerFields(aLayerName);
end;

function TGisViewerX.Query_GetFieldValues(const aLayerName: WideString; const aFieldName: WideString): IStrings;
begin
  Result := DefaultInterface.Query_GetFieldValues(aLayerName, aFieldName);
end;

procedure TGisViewerX.Query_SelectMapFeatures(const aLayerName: WideString; 
                                              const aFieldName: WideString; aSelectColor: Integer; 
                                              const aValueList: IStrings; aRefresh: WordBool);
begin
  DefaultInterface.Query_SelectMapFeatures(aLayerName, aFieldName, aSelectColor, aValueList, 
                                           aRefresh);
end;

procedure TGisViewerX.Chart_ClearAll;
begin
  DefaultInterface.Chart_ClearAll;
end;

procedure TGisViewerX.Chart_Add(const aFormat: WideString; const aPosition: WideString; 
                                const aData: WideString);
begin
  DefaultInterface.Chart_Add(aFormat, aPosition, aData);
end;

procedure TGisViewerX.Chart_AddShape(const aLayerName: WideString; const aFieldName: WideString; 
                                     const aValue: WideString; const aFormat: WideString; 
                                     const aData: WideString);
begin
  DefaultInterface.Chart_AddShape(aLayerName, aFieldName, aValue, aFormat, aData);
end;

procedure TGisViewerX.Arrow_ClearAll;
begin
  DefaultInterface.Arrow_ClearAll;
end;

procedure TGisViewerX.Arrow_Add(const aArrow: WideString);
begin
  DefaultInterface.Arrow_Add(aArrow);
end;

procedure TGisViewerX.Arrow_AddShape(const aLayerName: WideString; const aFieldName: WideString; 
                                     const aStartValue: WideString; const aEndValue: WideString; 
                                     const aColor: WideString; aSize: Integer);
begin
  DefaultInterface.Arrow_AddShape(aLayerName, aFieldName, aStartValue, aEndValue, aColor, aSize);
end;

procedure TGisViewerX.Display_ShowMessage(aShow: WordBool; aAssumeYes: WordBool);
begin
  DefaultInterface.Display_ShowMessage(aShow, aAssumeYes);
end;

function TGisViewerX.Map_LonLatToXY(aLon: Double; aLat: Double): IStrings;
begin
  Result := DefaultInterface.Map_LonLatToXY(aLon, aLat);
end;

function TGisViewerX.Map_XYToLonLat(aX: Integer; aY: Integer): IStrings;
begin
  Result := DefaultInterface.Map_XYToLonLat(aX, aY);
end;

function TGisViewerX.Map_LastX: Double;
begin
  Result := DefaultInterface.Map_LastX;
end;

function TGisViewerX.Map_LastY: Double;
begin
  Result := DefaultInterface.Map_LastY;
end;

procedure TGisViewerX.Map_SetCursor(aCursor: SYSINT);
begin
  DefaultInterface.Map_SetCursor(aCursor);
end;

procedure TGisViewerX.Map_SetPointer(aPointer: SYSINT);
begin
  DefaultInterface.Map_SetPointer(aPointer);
end;

procedure TGisViewerX.Control_RefreshAll;
begin
  DefaultInterface.Control_RefreshAll;
end;

procedure TGisViewerX.Control_ChangeCursor(aChange: WordBool);
begin
  DefaultInterface.Control_ChangeCursor(aChange);
end;

procedure TGisViewerX.Control_SetCursor(aCursor: SYSINT);
begin
  DefaultInterface.Control_SetCursor(aCursor);
end;

procedure TGisViewerX.Control_StatusBar(const aStatus: WideString);
begin
  DefaultInterface.Control_StatusBar(aStatus);
end;

function TGisViewerX.Control_MemoryLoad: Integer;
begin
  Result := DefaultInterface.Control_MemoryLoad;
end;

function TGisViewerX.Query_GetAllResults: IStrings;
begin
  Result := DefaultInterface.Query_GetAllResults;
end;

function TGisViewerX.Query_GetResults(const aLayerName: WideString; const aFieldName: WideString): IStrings;
begin
  Result := DefaultInterface.Query_GetResults(aLayerName, aFieldName);
end;

procedure TGisViewerX.Query_ClearResults;
begin
  DefaultInterface.Query_ClearResults;
end;

procedure TGisViewerX.Query_ClearSQL;
begin
  DefaultInterface.Query_ClearSQL;
end;

procedure TGisViewerX.Layers_RemoveLocked(aLocked: WordBool);
begin
  DefaultInterface.Layers_RemoveLocked(aLocked);
end;

procedure TGisViewerX.Layers_LoadXML(const aFilePath: WideString);
begin
  DefaultInterface.Layers_LoadXML(aFilePath);
end;

procedure TGisViewerX.Control_Initialize;
begin
  DefaultInterface.Control_Initialize;
end;

function TGisViewerX.Map_LOXYExtent: IStrings;
begin
  Result := DefaultInterface.Map_LOXYExtent;
end;

function TGisViewerX.Layers_GetStats(const aLayerName: WideString; const aFieldName: WideString): IStrings;
begin
  Result := DefaultInterface.Layers_GetStats(aLayerName, aFieldName);
end;

procedure TGisViewerX.Renderer_AddLabel(const aLayerName: WideString; const aFieldName: WideString; 
                                        aAllowDuplicates: WordBool);
begin
  DefaultInterface.Renderer_AddLabel(aLayerName, aFieldName, aAllowDuplicates);
end;

procedure TGisViewerX.Renderer_AddClass(const aLayerName: WideString; const aFieldName: WideString; 
                                        aMinColor: Integer; aMaxColor: Integer; aMinSize: Integer; 
                                        aMaxSize: Integer; aBreaks: Integer; aMinValue: Double; 
                                        aMaxValue: Double);
begin
  DefaultInterface.Renderer_AddClass(aLayerName, aFieldName, aMinColor, aMaxColor, aMinSize, 
                                     aMaxSize, aBreaks, aMinValue, aMaxValue);
end;

procedure TGisViewerX.Renderer_AddValue(const aLayerName: WideString; const aFieldName: WideString; 
                                        const aValueList: IStrings; const aColorList: IStrings);
begin
  DefaultInterface.Renderer_AddValue(aLayerName, aFieldName, aValueList, aColorList);
end;

procedure TGisViewerX.Renderer_AddChart(const aLayerName: WideString; const aFieldName: WideString; 
                                        const aRenderName: WideString; const aFormat: WideString; 
                                        aMinColor: Integer; aMaxColor: Integer; aBreaks: Integer; 
                                        const aValueList: IStrings);
begin
  DefaultInterface.Renderer_AddChart(aLayerName, aFieldName, aRenderName, aFormat, aMinColor, 
                                     aMaxColor, aBreaks, aValueList);
end;

procedure TGisViewerX.Chart_AddLegend(const aFormat: WideString; const aData: WideString);
begin
  DefaultInterface.Chart_AddLegend(aFormat, aData);
end;

procedure TGisViewerX.Chart_RemoveLegend;
begin
  DefaultInterface.Chart_RemoveLegend;
end;

procedure TGisViewerX.Query_MultiQuery(aMultiQuery: WordBool);
begin
  DefaultInterface.Query_MultiQuery(aMultiQuery);
end;

procedure TGisViewerX.Query_SetSelection(const aLayerName: WideString; 
                                         const aFieldName: WideString; 
                                         const aValueString: WideString; aRefresh: WordBool);
begin
  DefaultInterface.Query_SetSelection(aLayerName, aFieldName, aValueString, aRefresh);
end;

procedure TGisViewerX.Display_FlyoverHints(aShow: WordBool);
begin
  DefaultInterface.Display_FlyoverHints(aShow);
end;

procedure TGisViewerX.Display_ViewerHints(aShow: WordBool);
begin
  DefaultInterface.Display_ViewerHints(aShow);
end;

function TGisViewerX.Map_GetFilePathCovers: WideString;
begin
  Result := DefaultInterface.Map_GetFilePathCovers;
end;

function TGisViewerX.Map_GetFilePathXML: WideString;
begin
  Result := DefaultInterface.Map_GetFilePathXML;
end;

procedure TGisViewerX.Map_SetFilePathCovers(const aFilePath: WideString);
begin
  DefaultInterface.Map_SetFilePathCovers(aFilePath);
end;

procedure TGisViewerX.Map_SetFilePathXML(const aFilePath: WideString);
begin
  DefaultInterface.Map_SetFilePathXML(aFilePath);
end;

function TGisViewerX.Map_GetLO: Single;
begin
  Result := DefaultInterface.Map_GetLO;
end;

function TGisViewerX.Map_GetEllipsoid: SYSINT;
begin
  Result := DefaultInterface.Map_GetEllipsoid;
end;

function TGisViewerX.Map_ConvertLonLatToXY(aLon: Single; aLat: Single; aLO: Single; aEllipse: SYSINT): IStrings;
begin
  Result := DefaultInterface.Map_ConvertLonLatToXY(aLon, aLat, aLO, aEllipse);
end;

function TGisViewerX.Control_GetIStrings: IStrings;
begin
  Result := DefaultInterface.Control_GetIStrings;
end;

procedure Register;
begin
  RegisterComponents(dtlOcxPage, [TGisViewerX]);
end;

end.
