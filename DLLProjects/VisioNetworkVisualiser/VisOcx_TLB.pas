unit VisOcx_TLB;

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

// $Rev: 52393 $
// File generated on 2017/04/13 09:27:18 AM from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Program Files (x86)\Microsoft Office\Office14\VISOCX.DLL (1)
// LIBID: {F1A8DFE4-BC61-48BA-AFDA-96DF10247AF0}
// LCID: 0
// Helpfile: 
// HelpString: Microsoft Visio 14.0 Drawing Control Type Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
//   (2) v4.14 Visio, (C:\Program Files (x86)\Microsoft Office\Office14\VISLIB.DLL)
// SYS_KIND: SYS_WIN32
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}

interface

uses Winapi.Windows, System.Classes, System.Variants, System.Win.StdVCL, Vcl.Graphics, Vcl.OleCtrls, Vcl.OleServer, Visio_TLB, 
Winapi.ActiveX;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  VisOcxMajorVersion = 1;
  VisOcxMinorVersion = 2;

  LIBID_VisOcx: TGUID = '{F1A8DFE4-BC61-48BA-AFDA-96DF10247AF0}';

  IID_IDrawingControl: TGUID = '{9BF6FD73-F05B-406E-B938-09E2B413528E}';
  DIID_EVisOcx: TGUID = '{BD086122-F160-436A-BE7B-6B1D58D14703}';
  CLASS_DrawingControl: TGUID = '{E4615FA3-23B0-4976-BD3E-D611DDBE330E}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IDrawingControl = interface;
  IDrawingControlDisp = dispinterface;
  EVisOcx = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  DrawingControl = IDrawingControl;


// *********************************************************************//
// Interface: IDrawingControl
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9BF6FD73-F05B-406E-B938-09E2B413528E}
// *********************************************************************//
  IDrawingControl = interface(IDispatch)
    ['{9BF6FD73-F05B-406E-B938-09E2B413528E}']
    function Get_Document: IVDocument; safecall;
    function Get_Window: IVWindow; safecall;
    function Get_Src: WideString; safecall;
    procedure Set_Src(const pVal: WideString); safecall;
    function Get_HostID: WideString; safecall;
    procedure Set_HostID(const pVal: WideString); safecall;
    function Get_NegotiateMenus: WordBool; safecall;
    procedure Set_NegotiateMenus(pbVal: WordBool); safecall;
    function Get_NegotiateToolbars: WordBool; safecall;
    procedure Set_NegotiateToolbars(pbVal: WordBool); safecall;
    function Get_PageSizingBehavior: VisPageSizingBehaviors; safecall;
    procedure Set_PageSizingBehavior(peVal: VisPageSizingBehaviors); safecall;
    property Document: IVDocument read Get_Document;
    property Window: IVWindow read Get_Window;
    property Src: WideString read Get_Src write Set_Src;
    property HostID: WideString read Get_HostID write Set_HostID;
    property NegotiateMenus: WordBool read Get_NegotiateMenus write Set_NegotiateMenus;
    property NegotiateToolbars: WordBool read Get_NegotiateToolbars write Set_NegotiateToolbars;
    property PageSizingBehavior: VisPageSizingBehaviors read Get_PageSizingBehavior write Set_PageSizingBehavior;
  end;

// *********************************************************************//
// DispIntf:  IDrawingControlDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9BF6FD73-F05B-406E-B938-09E2B413528E}
// *********************************************************************//
  IDrawingControlDisp = dispinterface
    ['{9BF6FD73-F05B-406E-B938-09E2B413528E}']
    property Document: IVDocument readonly dispid 1610743808;
    property Window: IVWindow readonly dispid 1610743809;
    property Src: WideString dispid 1;
    property HostID: WideString dispid 2;
    property NegotiateMenus: WordBool dispid 3;
    property NegotiateToolbars: WordBool dispid 4;
    property PageSizingBehavior: VisPageSizingBehaviors dispid 5;
  end;

// *********************************************************************//
// DispIntf:  EVisOcx
// Flags:     (4112) Hidden Dispatchable
// GUID:      {BD086122-F160-436A-BE7B-6B1D58D14703}
// *********************************************************************//
  EVisOcx = dispinterface
    ['{BD086122-F160-436A-BE7B-6B1D58D14703}']
    procedure DocumentOpened(const doc: IVDocument); dispid 2;
    procedure DocumentCreated(const doc: IVDocument); dispid 1;
    procedure DocumentSaved(const doc: IVDocument); dispid 3;
    procedure DocumentSavedAs(const doc: IVDocument); dispid 4;
    procedure DocumentChanged(const doc: IVDocument); dispid 8194;
    procedure BeforeDocumentClose(const doc: IVDocument); dispid 16386;
    procedure StyleAdded(const style: IVStyle); dispid 32772;
    procedure StyleChanged(const style: IVStyle); dispid 8196;
    procedure BeforeStyleDelete(const style: IVStyle); dispid 16388;
    procedure MasterAdded(const master: IVMaster); dispid 32776;
    procedure MasterChanged(const master: IVMaster); dispid 8200;
    procedure BeforeMasterDelete(const master: IVMaster); dispid 16392;
    procedure PageAdded(const page: IVPage); dispid 32784;
    procedure PageChanged(const page: IVPage); dispid 8208;
    procedure BeforePageDelete(const page: IVPage); dispid 16400;
    procedure ShapeAdded(const shape: IVShape); dispid 32832;
    procedure BeforeSelectionDelete(const selection: IVSelection); dispid 901;
    procedure RunModeEntered(const doc: IVDocument); dispid 5;
    procedure DesignModeEntered(const doc: IVDocument); dispid 6;
    procedure BeforeDocumentSave(const doc: IVDocument); dispid 7;
    procedure BeforeDocumentSaveAs(const doc: IVDocument); dispid 8;
    function QueryCancelDocumentClose(const doc: IVDocument): WordBool; dispid 9;
    procedure DocumentCloseCanceled(const doc: IVDocument); dispid 10;
    function QueryCancelStyleDelete(const style: IVStyle): WordBool; dispid 300;
    procedure StyleDeleteCanceled(const style: IVStyle); dispid 301;
    function QueryCancelMasterDelete(const master: IVMaster): WordBool; dispid 400;
    procedure MasterDeleteCanceled(const master: IVMaster); dispid 401;
    function QueryCancelPageDelete(const page: IVPage): WordBool; dispid 500;
    procedure PageDeleteCanceled(const page: IVPage); dispid 501;
    procedure ShapeParentChanged(const shape: IVShape); dispid 802;
    procedure BeforeShapeTextEdit(const shape: IVShape); dispid 803;
    procedure ShapeExitedTextEdit(const shape: IVShape); dispid 804;
    function QueryCancelSelectionDelete(const selection: IVSelection): WordBool; dispid 903;
    procedure SelectionDeleteCanceled(const selection: IVSelection); dispid 904;
    function QueryCancelUngroup(const selection: IVSelection): WordBool; dispid 905;
    procedure UngroupCanceled(const selection: IVSelection); dispid 906;
    function QueryCancelConvertToGroup(const selection: IVSelection): WordBool; dispid 907;
    procedure ConvertToGroupCanceled(const selection: IVSelection); dispid 908;
    procedure SelectionChanged(const Window: IVWindow); dispid 701;
    procedure BeforeWindowClosed(const Window: IVWindow); dispid 16385;
    procedure WindowActivated(const Window: IVWindow); dispid 4224;
    procedure BeforeWindowSelDelete(const Window: IVWindow); dispid 702;
    procedure BeforeWindowPageTurn(const Window: IVWindow); dispid 703;
    procedure WindowTurnedToPage(const Window: IVWindow); dispid 704;
    procedure WindowChanged(const Window: IVWindow); dispid 8193;
    procedure ViewChanged(const Window: IVWindow); dispid 705;
    function QueryCancelWindowClose(const Window: IVWindow): WordBool; dispid 706;
    procedure WindowCloseCanceled(const Window: IVWindow); dispid 707;
    function OnKeystrokeMessageForAddon(const MSG: IVMSGWrap): WordBool; dispid 708;
    procedure MouseDown(Button: Integer; KeyButtonState: Integer; X: Double; Y: Double; 
                        var CancelDefault: WordBool); dispid 709;
    procedure MouseMove(Button: Integer; KeyButtonState: Integer; X: Double; Y: Double; 
                        var CancelDefault: WordBool); dispid 710;
    procedure MouseUp(Button: Integer; KeyButtonState: Integer; X: Double; Y: Double; 
                      var CancelDefault: WordBool); dispid 711;
    procedure KeyDown(KeyCode: Integer; KeyButtonState: Integer; var CancelDefault: WordBool); dispid 712;
    procedure KeyPress(KeyAscii: Integer; var CancelDefault: WordBool); dispid 713;
    procedure KeyUp(KeyCode: Integer; KeyButtonState: Integer; var CancelDefault: WordBool); dispid 714;
    function QueryCancelGroup(const selection: IVSelection): WordBool; dispid 909;
    procedure GroupCanceled(const selection: IVSelection); dispid 910;
    procedure ShapeDataGraphicChanged(const shape: IVShape); dispid 807;
    procedure BeforeDataRecordsetDelete(const dataRecordset: IVDataRecordset); dispid 16416;
    procedure DataRecordsetAdded(const dataRecordset: IVDataRecordset); dispid 32800;
    procedure AfterRemoveHiddenInformation(const doc: IVDocument); dispid 11;
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TDrawingControl
// Help String      : Microsoft Visio 14.0 Drawing Control
// Default Interface: IDrawingControl
// Def. Intf. DISP? : No
// Event   Interface: EVisOcx
// TypeFlags        : (2) CanCreate
// *********************************************************************//
  TDrawingControlDocumentOpened = procedure(ASender: TObject; const doc: IVDocument) of object;
  TDrawingControlDocumentCreated = procedure(ASender: TObject; const doc: IVDocument) of object;
  TDrawingControlDocumentSaved = procedure(ASender: TObject; const doc: IVDocument) of object;
  TDrawingControlDocumentSavedAs = procedure(ASender: TObject; const doc: IVDocument) of object;
  TDrawingControlDocumentChanged = procedure(ASender: TObject; const doc: IVDocument) of object;
  TDrawingControlBeforeDocumentClose = procedure(ASender: TObject; const doc: IVDocument) of object;
  TDrawingControlStyleAdded = procedure(ASender: TObject; const style: IVStyle) of object;
  TDrawingControlStyleChanged = procedure(ASender: TObject; const style: IVStyle) of object;
  TDrawingControlBeforeStyleDelete = procedure(ASender: TObject; const style: IVStyle) of object;
  TDrawingControlMasterAdded = procedure(ASender: TObject; const master: IVMaster) of object;
  TDrawingControlMasterChanged = procedure(ASender: TObject; const master: IVMaster) of object;
  TDrawingControlBeforeMasterDelete = procedure(ASender: TObject; const master: IVMaster) of object;
  TDrawingControlPageAdded = procedure(ASender: TObject; const page: IVPage) of object;
  TDrawingControlPageChanged = procedure(ASender: TObject; const page: IVPage) of object;
  TDrawingControlBeforePageDelete = procedure(ASender: TObject; const page: IVPage) of object;
  TDrawingControlShapeAdded = procedure(ASender: TObject; const shape: IVShape) of object;
  TDrawingControlBeforeSelectionDelete = procedure(ASender: TObject; const selection: IVSelection) of object;
  TDrawingControlRunModeEntered = procedure(ASender: TObject; const doc: IVDocument) of object;
  TDrawingControlDesignModeEntered = procedure(ASender: TObject; const doc: IVDocument) of object;
  TDrawingControlBeforeDocumentSave = procedure(ASender: TObject; const doc: IVDocument) of object;
  TDrawingControlBeforeDocumentSaveAs = procedure(ASender: TObject; const doc: IVDocument) of object;
  TDrawingControlQueryCancelDocumentClose = procedure(ASender: TObject; const doc: IVDocument) of object;
  TDrawingControlDocumentCloseCanceled = procedure(ASender: TObject; const doc: IVDocument) of object;
  TDrawingControlQueryCancelStyleDelete = procedure(ASender: TObject; const style: IVStyle) of object;
  TDrawingControlStyleDeleteCanceled = procedure(ASender: TObject; const style: IVStyle) of object;
  TDrawingControlQueryCancelMasterDelete = procedure(ASender: TObject; const master: IVMaster) of object;
  TDrawingControlMasterDeleteCanceled = procedure(ASender: TObject; const master: IVMaster) of object;
  TDrawingControlQueryCancelPageDelete = procedure(ASender: TObject; const page: IVPage) of object;
  TDrawingControlPageDeleteCanceled = procedure(ASender: TObject; const page: IVPage) of object;
  TDrawingControlShapeParentChanged = procedure(ASender: TObject; const shape: IVShape) of object;
  TDrawingControlBeforeShapeTextEdit = procedure(ASender: TObject; const shape: IVShape) of object;
  TDrawingControlShapeExitedTextEdit = procedure(ASender: TObject; const shape: IVShape) of object;
  TDrawingControlQueryCancelSelectionDelete = procedure(ASender: TObject; const selection: IVSelection) of object;
  TDrawingControlSelectionDeleteCanceled = procedure(ASender: TObject; const selection: IVSelection) of object;
  TDrawingControlQueryCancelUngroup = procedure(ASender: TObject; const selection: IVSelection) of object;
  TDrawingControlUngroupCanceled = procedure(ASender: TObject; const selection: IVSelection) of object;
  TDrawingControlQueryCancelConvertToGroup = procedure(ASender: TObject; const selection: IVSelection) of object;
  TDrawingControlConvertToGroupCanceled = procedure(ASender: TObject; const selection: IVSelection) of object;
  TDrawingControlSelectionChanged = procedure(ASender: TObject; const Window: IVWindow) of object;
  TDrawingControlBeforeWindowClosed = procedure(ASender: TObject; const Window: IVWindow) of object;
  TDrawingControlWindowActivated = procedure(ASender: TObject; const Window: IVWindow) of object;
  TDrawingControlBeforeWindowSelDelete = procedure(ASender: TObject; const Window: IVWindow) of object;
  TDrawingControlBeforeWindowPageTurn = procedure(ASender: TObject; const Window: IVWindow) of object;
  TDrawingControlWindowTurnedToPage = procedure(ASender: TObject; const Window: IVWindow) of object;
  TDrawingControlWindowChanged = procedure(ASender: TObject; const Window: IVWindow) of object;
  TDrawingControlViewChanged = procedure(ASender: TObject; const Window: IVWindow) of object;
  TDrawingControlQueryCancelWindowClose = procedure(ASender: TObject; const Window: IVWindow) of object;
  TDrawingControlWindowCloseCanceled = procedure(ASender: TObject; const Window: IVWindow) of object;
  TDrawingControlOnKeystrokeMessageForAddon = procedure(ASender: TObject; const MSG: IVMSGWrap) of object;
  TDrawingControlMouseDown = procedure(ASender: TObject; Button: Integer; KeyButtonState: Integer; 
                                                         X: Double; Y: Double; 
                                                         var CancelDefault: WordBool) of object;
  TDrawingControlMouseMove = procedure(ASender: TObject; Button: Integer; KeyButtonState: Integer; 
                                                         X: Double; Y: Double; 
                                                         var CancelDefault: WordBool) of object;
  TDrawingControlMouseUp = procedure(ASender: TObject; Button: Integer; KeyButtonState: Integer; 
                                                       X: Double; Y: Double; 
                                                       var CancelDefault: WordBool) of object;
  TDrawingControlKeyDown = procedure(ASender: TObject; KeyCode: Integer; KeyButtonState: Integer; 
                                                       var CancelDefault: WordBool) of object;
  TDrawingControlKeyPress = procedure(ASender: TObject; KeyAscii: Integer; 
                                                        var CancelDefault: WordBool) of object;
  TDrawingControlKeyUp = procedure(ASender: TObject; KeyCode: Integer; KeyButtonState: Integer; 
                                                     var CancelDefault: WordBool) of object;
  TDrawingControlQueryCancelGroup = procedure(ASender: TObject; const selection: IVSelection) of object;
  TDrawingControlGroupCanceled = procedure(ASender: TObject; const selection: IVSelection) of object;
  TDrawingControlShapeDataGraphicChanged = procedure(ASender: TObject; const shape: IVShape) of object;
  TDrawingControlBeforeDataRecordsetDelete = procedure(ASender: TObject; const dataRecordset: IVDataRecordset) of object;
  TDrawingControlDataRecordsetAdded = procedure(ASender: TObject; const dataRecordset: IVDataRecordset) of object;
  TDrawingControlAfterRemoveHiddenInformation = procedure(ASender: TObject; const doc: IVDocument) of object;

  TDrawingControl = class(TOleControl)
  private
    FOnDocumentOpened: TDrawingControlDocumentOpened;
    FOnDocumentCreated: TDrawingControlDocumentCreated;
    FOnDocumentSaved: TDrawingControlDocumentSaved;
    FOnDocumentSavedAs: TDrawingControlDocumentSavedAs;
    FOnDocumentChanged: TDrawingControlDocumentChanged;
    FOnBeforeDocumentClose: TDrawingControlBeforeDocumentClose;
    FOnStyleAdded: TDrawingControlStyleAdded;
    FOnStyleChanged: TDrawingControlStyleChanged;
    FOnBeforeStyleDelete: TDrawingControlBeforeStyleDelete;
    FOnMasterAdded: TDrawingControlMasterAdded;
    FOnMasterChanged: TDrawingControlMasterChanged;
    FOnBeforeMasterDelete: TDrawingControlBeforeMasterDelete;
    FOnPageAdded: TDrawingControlPageAdded;
    FOnPageChanged: TDrawingControlPageChanged;
    FOnBeforePageDelete: TDrawingControlBeforePageDelete;
    FOnShapeAdded: TDrawingControlShapeAdded;
    FOnBeforeSelectionDelete: TDrawingControlBeforeSelectionDelete;
    FOnRunModeEntered: TDrawingControlRunModeEntered;
    FOnDesignModeEntered: TDrawingControlDesignModeEntered;
    FOnBeforeDocumentSave: TDrawingControlBeforeDocumentSave;
    FOnBeforeDocumentSaveAs: TDrawingControlBeforeDocumentSaveAs;
    FOnQueryCancelDocumentClose: TDrawingControlQueryCancelDocumentClose;
    FOnDocumentCloseCanceled: TDrawingControlDocumentCloseCanceled;
    FOnQueryCancelStyleDelete: TDrawingControlQueryCancelStyleDelete;
    FOnStyleDeleteCanceled: TDrawingControlStyleDeleteCanceled;
    FOnQueryCancelMasterDelete: TDrawingControlQueryCancelMasterDelete;
    FOnMasterDeleteCanceled: TDrawingControlMasterDeleteCanceled;
    FOnQueryCancelPageDelete: TDrawingControlQueryCancelPageDelete;
    FOnPageDeleteCanceled: TDrawingControlPageDeleteCanceled;
    FOnShapeParentChanged: TDrawingControlShapeParentChanged;
    FOnBeforeShapeTextEdit: TDrawingControlBeforeShapeTextEdit;
    FOnShapeExitedTextEdit: TDrawingControlShapeExitedTextEdit;
    FOnQueryCancelSelectionDelete: TDrawingControlQueryCancelSelectionDelete;
    FOnSelectionDeleteCanceled: TDrawingControlSelectionDeleteCanceled;
    FOnQueryCancelUngroup: TDrawingControlQueryCancelUngroup;
    FOnUngroupCanceled: TDrawingControlUngroupCanceled;
    FOnQueryCancelConvertToGroup: TDrawingControlQueryCancelConvertToGroup;
    FOnConvertToGroupCanceled: TDrawingControlConvertToGroupCanceled;
    FOnSelectionChanged: TDrawingControlSelectionChanged;
    FOnBeforeWindowClosed: TDrawingControlBeforeWindowClosed;
    FOnWindowActivated: TDrawingControlWindowActivated;
    FOnBeforeWindowSelDelete: TDrawingControlBeforeWindowSelDelete;
    FOnBeforeWindowPageTurn: TDrawingControlBeforeWindowPageTurn;
    FOnWindowTurnedToPage: TDrawingControlWindowTurnedToPage;
    FOnWindowChanged: TDrawingControlWindowChanged;
    FOnViewChanged: TDrawingControlViewChanged;
    FOnQueryCancelWindowClose: TDrawingControlQueryCancelWindowClose;
    FOnWindowCloseCanceled: TDrawingControlWindowCloseCanceled;
    FOnKeystrokeMessageForAddon: TDrawingControlOnKeystrokeMessageForAddon;
    FOnMouseDown: TDrawingControlMouseDown;
    FOnMouseMove: TDrawingControlMouseMove;
    FOnMouseUp: TDrawingControlMouseUp;
    FOnKeyDown: TDrawingControlKeyDown;
    FOnKeyPress: TDrawingControlKeyPress;
    FOnKeyUp: TDrawingControlKeyUp;
    FOnQueryCancelGroup: TDrawingControlQueryCancelGroup;
    FOnGroupCanceled: TDrawingControlGroupCanceled;
    FOnShapeDataGraphicChanged: TDrawingControlShapeDataGraphicChanged;
    FOnBeforeDataRecordsetDelete: TDrawingControlBeforeDataRecordsetDelete;
    FOnDataRecordsetAdded: TDrawingControlDataRecordsetAdded;
    FOnAfterRemoveHiddenInformation: TDrawingControlAfterRemoveHiddenInformation;
    FIntf: IDrawingControl;
    function  GetControlInterface: IDrawingControl;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
    function Get_Document: IVDocument;
    function Get_Window: IVWindow;
  public
    property  ControlInterface: IDrawingControl read GetControlInterface;
    property  DefaultInterface: IDrawingControl read GetControlInterface;
    property Document: IVDocument read Get_Document;
    property Window: IVWindow read Get_Window;
  published
    property Anchors;
    property  TabStop;
    property  Align;
    property  DragCursor;
    property  DragMode;
    property  ParentShowHint;
    property  PopupMenu;
    property  ShowHint;
    property  TabOrder;
    property  Visible;
    property  OnDragDrop;
    property  OnDragOver;
    property  OnEndDrag;
    property  OnEnter;
    property  OnExit;
    property  OnStartDrag;
    property Src: WideString index 1 read GetWideStringProp write SetWideStringProp stored False;
    property HostID: WideString index 2 read GetWideStringProp write SetWideStringProp stored False;
    property NegotiateMenus: WordBool index 3 read GetWordBoolProp write SetWordBoolProp stored False;
    property NegotiateToolbars: WordBool index 4 read GetWordBoolProp write SetWordBoolProp stored False;
    property PageSizingBehavior: TOleEnum index 5 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property OnDocumentOpened: TDrawingControlDocumentOpened read FOnDocumentOpened write FOnDocumentOpened;
    property OnDocumentCreated: TDrawingControlDocumentCreated read FOnDocumentCreated write FOnDocumentCreated;
    property OnDocumentSaved: TDrawingControlDocumentSaved read FOnDocumentSaved write FOnDocumentSaved;
    property OnDocumentSavedAs: TDrawingControlDocumentSavedAs read FOnDocumentSavedAs write FOnDocumentSavedAs;
    property OnDocumentChanged: TDrawingControlDocumentChanged read FOnDocumentChanged write FOnDocumentChanged;
    property OnBeforeDocumentClose: TDrawingControlBeforeDocumentClose read FOnBeforeDocumentClose write FOnBeforeDocumentClose;
    property OnStyleAdded: TDrawingControlStyleAdded read FOnStyleAdded write FOnStyleAdded;
    property OnStyleChanged: TDrawingControlStyleChanged read FOnStyleChanged write FOnStyleChanged;
    property OnBeforeStyleDelete: TDrawingControlBeforeStyleDelete read FOnBeforeStyleDelete write FOnBeforeStyleDelete;
    property OnMasterAdded: TDrawingControlMasterAdded read FOnMasterAdded write FOnMasterAdded;
    property OnMasterChanged: TDrawingControlMasterChanged read FOnMasterChanged write FOnMasterChanged;
    property OnBeforeMasterDelete: TDrawingControlBeforeMasterDelete read FOnBeforeMasterDelete write FOnBeforeMasterDelete;
    property OnPageAdded: TDrawingControlPageAdded read FOnPageAdded write FOnPageAdded;
    property OnPageChanged: TDrawingControlPageChanged read FOnPageChanged write FOnPageChanged;
    property OnBeforePageDelete: TDrawingControlBeforePageDelete read FOnBeforePageDelete write FOnBeforePageDelete;
    property OnShapeAdded: TDrawingControlShapeAdded read FOnShapeAdded write FOnShapeAdded;
    property OnBeforeSelectionDelete: TDrawingControlBeforeSelectionDelete read FOnBeforeSelectionDelete write FOnBeforeSelectionDelete;
    property OnRunModeEntered: TDrawingControlRunModeEntered read FOnRunModeEntered write FOnRunModeEntered;
    property OnDesignModeEntered: TDrawingControlDesignModeEntered read FOnDesignModeEntered write FOnDesignModeEntered;
    property OnBeforeDocumentSave: TDrawingControlBeforeDocumentSave read FOnBeforeDocumentSave write FOnBeforeDocumentSave;
    property OnBeforeDocumentSaveAs: TDrawingControlBeforeDocumentSaveAs read FOnBeforeDocumentSaveAs write FOnBeforeDocumentSaveAs;
    property OnQueryCancelDocumentClose: TDrawingControlQueryCancelDocumentClose read FOnQueryCancelDocumentClose write FOnQueryCancelDocumentClose;
    property OnDocumentCloseCanceled: TDrawingControlDocumentCloseCanceled read FOnDocumentCloseCanceled write FOnDocumentCloseCanceled;
    property OnQueryCancelStyleDelete: TDrawingControlQueryCancelStyleDelete read FOnQueryCancelStyleDelete write FOnQueryCancelStyleDelete;
    property OnStyleDeleteCanceled: TDrawingControlStyleDeleteCanceled read FOnStyleDeleteCanceled write FOnStyleDeleteCanceled;
    property OnQueryCancelMasterDelete: TDrawingControlQueryCancelMasterDelete read FOnQueryCancelMasterDelete write FOnQueryCancelMasterDelete;
    property OnMasterDeleteCanceled: TDrawingControlMasterDeleteCanceled read FOnMasterDeleteCanceled write FOnMasterDeleteCanceled;
    property OnQueryCancelPageDelete: TDrawingControlQueryCancelPageDelete read FOnQueryCancelPageDelete write FOnQueryCancelPageDelete;
    property OnPageDeleteCanceled: TDrawingControlPageDeleteCanceled read FOnPageDeleteCanceled write FOnPageDeleteCanceled;
    property OnShapeParentChanged: TDrawingControlShapeParentChanged read FOnShapeParentChanged write FOnShapeParentChanged;
    property OnBeforeShapeTextEdit: TDrawingControlBeforeShapeTextEdit read FOnBeforeShapeTextEdit write FOnBeforeShapeTextEdit;
    property OnShapeExitedTextEdit: TDrawingControlShapeExitedTextEdit read FOnShapeExitedTextEdit write FOnShapeExitedTextEdit;
    property OnQueryCancelSelectionDelete: TDrawingControlQueryCancelSelectionDelete read FOnQueryCancelSelectionDelete write FOnQueryCancelSelectionDelete;
    property OnSelectionDeleteCanceled: TDrawingControlSelectionDeleteCanceled read FOnSelectionDeleteCanceled write FOnSelectionDeleteCanceled;
    property OnQueryCancelUngroup: TDrawingControlQueryCancelUngroup read FOnQueryCancelUngroup write FOnQueryCancelUngroup;
    property OnUngroupCanceled: TDrawingControlUngroupCanceled read FOnUngroupCanceled write FOnUngroupCanceled;
    property OnQueryCancelConvertToGroup: TDrawingControlQueryCancelConvertToGroup read FOnQueryCancelConvertToGroup write FOnQueryCancelConvertToGroup;
    property OnConvertToGroupCanceled: TDrawingControlConvertToGroupCanceled read FOnConvertToGroupCanceled write FOnConvertToGroupCanceled;
    property OnSelectionChanged: TDrawingControlSelectionChanged read FOnSelectionChanged write FOnSelectionChanged;
    property OnBeforeWindowClosed: TDrawingControlBeforeWindowClosed read FOnBeforeWindowClosed write FOnBeforeWindowClosed;
    property OnWindowActivated: TDrawingControlWindowActivated read FOnWindowActivated write FOnWindowActivated;
    property OnBeforeWindowSelDelete: TDrawingControlBeforeWindowSelDelete read FOnBeforeWindowSelDelete write FOnBeforeWindowSelDelete;
    property OnBeforeWindowPageTurn: TDrawingControlBeforeWindowPageTurn read FOnBeforeWindowPageTurn write FOnBeforeWindowPageTurn;
    property OnWindowTurnedToPage: TDrawingControlWindowTurnedToPage read FOnWindowTurnedToPage write FOnWindowTurnedToPage;
    property OnWindowChanged: TDrawingControlWindowChanged read FOnWindowChanged write FOnWindowChanged;
    property OnViewChanged: TDrawingControlViewChanged read FOnViewChanged write FOnViewChanged;
    property OnQueryCancelWindowClose: TDrawingControlQueryCancelWindowClose read FOnQueryCancelWindowClose write FOnQueryCancelWindowClose;
    property OnWindowCloseCanceled: TDrawingControlWindowCloseCanceled read FOnWindowCloseCanceled write FOnWindowCloseCanceled;
    property OnKeystrokeMessageForAddon: TDrawingControlOnKeystrokeMessageForAddon read FOnKeystrokeMessageForAddon write FOnKeystrokeMessageForAddon;
    property OnMouseDown: TDrawingControlMouseDown read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TDrawingControlMouseMove read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TDrawingControlMouseUp read FOnMouseUp write FOnMouseUp;
    property OnKeyDown: TDrawingControlKeyDown read FOnKeyDown write FOnKeyDown;
    property OnKeyPress: TDrawingControlKeyPress read FOnKeyPress write FOnKeyPress;
    property OnKeyUp: TDrawingControlKeyUp read FOnKeyUp write FOnKeyUp;
    property OnQueryCancelGroup: TDrawingControlQueryCancelGroup read FOnQueryCancelGroup write FOnQueryCancelGroup;
    property OnGroupCanceled: TDrawingControlGroupCanceled read FOnGroupCanceled write FOnGroupCanceled;
    property OnShapeDataGraphicChanged: TDrawingControlShapeDataGraphicChanged read FOnShapeDataGraphicChanged write FOnShapeDataGraphicChanged;
    property OnBeforeDataRecordsetDelete: TDrawingControlBeforeDataRecordsetDelete read FOnBeforeDataRecordsetDelete write FOnBeforeDataRecordsetDelete;
    property OnDataRecordsetAdded: TDrawingControlDataRecordsetAdded read FOnDataRecordsetAdded write FOnDataRecordsetAdded;
    property OnAfterRemoveHiddenInformation: TDrawingControlAfterRemoveHiddenInformation read FOnAfterRemoveHiddenInformation write FOnAfterRemoveHiddenInformation;
  end;

procedure Register;

resourcestring
  dtlServerPage = 'ActiveX';

  dtlOcxPage = 'ActiveX';

implementation

uses System.Win.ComObj;

procedure TDrawingControl.InitControlData;
const
  CEventDispIDs: array [0..60] of DWORD = (
    $00000002, $00000001, $00000003, $00000004, $00002002, $00004002,
    $00008004, $00002004, $00004004, $00008008, $00002008, $00004008,
    $00008010, $00002010, $00004010, $00008040, $00000385, $00000005,
    $00000006, $00000007, $00000008, $00000009, $0000000A, $0000012C,
    $0000012D, $00000190, $00000191, $000001F4, $000001F5, $00000322,
    $00000323, $00000324, $00000387, $00000388, $00000389, $0000038A,
    $0000038B, $0000038C, $000002BD, $00004001, $00001080, $000002BE,
    $000002BF, $000002C0, $00002001, $000002C1, $000002C2, $000002C3,
    $000002C4, $000002C5, $000002C6, $000002C7, $000002C8, $000002C9,
    $000002CA, $0000038D, $0000038E, $00000327, $00004020, $00008020,
    $0000000B);
  CLicenseKey: array[0..21] of Word = ( $0056, $0069, $0073, $004F, $0063, $0078, $002E, $0044, $0072, $0061, $0077
    , $0069, $006E, $0067, $0043, $006F, $006E, $0074, $0072, $006F, $006C
    , $0000);
  CControlData: TControlData2 = (
    ClassID:      '{E4615FA3-23B0-4976-BD3E-D611DDBE330E}';
    EventIID:     '{BD086122-F160-436A-BE7B-6B1D58D14703}';
    EventCount:   61;
    EventDispIDs: @CEventDispIDs;
    LicenseKey:   @CLicenseKey;
    Flags:        $00000000;
    Version:      500);
begin
  ControlData := @CControlData;
  TControlData2(CControlData).FirstEventOfs := UIntPtr(@@FOnDocumentOpened) - UIntPtr(Self);
end;

procedure TDrawingControl.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as IDrawingControl;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TDrawingControl.GetControlInterface: IDrawingControl;
begin
  CreateControl;
  Result := FIntf;
end;

function TDrawingControl.Get_Document: IVDocument;
begin
  Result := DefaultInterface.Document;
end;

function TDrawingControl.Get_Window: IVWindow;
begin
  Result := DefaultInterface.Window;
end;

procedure Register;
begin
  RegisterComponents(dtlOcxPage, [TDrawingControl]);
end;

end.
