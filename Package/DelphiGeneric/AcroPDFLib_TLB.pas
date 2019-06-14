unit AcroPDFLib_TLB;

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
// File generated on 10-16-2006 18:06:23 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Program Files\Adobe\Acrobat 7.0\ActiveX\AcroPDF.dll (1)
// LIBID: {05BFD3F1-6319-4F30-B752-C7A22889BCC4}
// LCID: 0
// Helpfile: 
// HelpString: Adobe Acrobat 7.0 Browser Control Type Library 1.0
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINNT\system32\stdole2.tlb)
// Errors:
//   Hint: Parameter 'On' of IAcroAXDocShim.setShowToolbar changed to 'On_'
//   Hint: Parameter 'to' of IAcroAXDocShim.printPages changed to 'to_'
//   Hint: Parameter 'to' of IAcroAXDocShim.printPagesFit changed to 'to_'
//   Hint: Parameter 'On' of IAcroAXDocShim.setShowScrollbars changed to 'On_'
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, vcl.Graphics, vcl.OleCtrls, vcl.OleServer, StdVCL, Variants;
  


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  AcroPDFLibMajorVersion = 1;
  AcroPDFLibMinorVersion = 0;

  LIBID_AcroPDFLib: TGUID = '{05BFD3F1-6319-4F30-B752-C7A22889BCC4}';

  DIID__IAcroAXDocShimEvents: TGUID = '{E790E1D1-9DE8-4853-8AC6-933D4FD9C927}';
  IID_IAcroAXDocShim: TGUID = '{3B813CE7-7C10-4F84-AD06-9DF76D97A9AA}';
  CLASS_AcroPDF: TGUID = '{CA8A9780-280D-11CF-A24D-444553540000}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  _IAcroAXDocShimEvents = dispinterface;
  IAcroAXDocShim = interface;
  IAcroAXDocShimDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  AcroPDF = IAcroAXDocShim;


// *********************************************************************//
// DispIntf:  _IAcroAXDocShimEvents
// Flags:     (4096) Dispatchable
// GUID:      {E790E1D1-9DE8-4853-8AC6-933D4FD9C927}
// *********************************************************************//
  _IAcroAXDocShimEvents = dispinterface
    ['{E790E1D1-9DE8-4853-8AC6-933D4FD9C927}']
    procedure OnError; dispid 1;
    procedure OnMessage; dispid 2;
  end;

// *********************************************************************//
// Interface: IAcroAXDocShim
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {3B813CE7-7C10-4F84-AD06-9DF76D97A9AA}
// *********************************************************************//
  IAcroAXDocShim = interface(IDispatch)
    ['{3B813CE7-7C10-4F84-AD06-9DF76D97A9AA}']
    function Get_src: WideString; safecall;
    procedure Set_src(const pVal: WideString); safecall;
    function LoadFile(const fileName: WideString): WordBool; safecall;
    procedure setShowToolbar(On_: WordBool); safecall;
    procedure gotoFirstPage; safecall;
    procedure gotoLastPage; safecall;
    procedure gotoNextPage; safecall;
    procedure gotoPreviousPage; safecall;
    procedure setCurrentPage(n: Integer); safecall;
    procedure goForwardStack; safecall;
    procedure goBackwardStack; safecall;
    procedure setPageMode(const pageMode: WideString); safecall;
    procedure setLayoutMode(const layoutMode: WideString); safecall;
    procedure setNamedDest(const namedDest: WideString); safecall;
    procedure Print; safecall;
    procedure printWithDialog; safecall;
    procedure setZoom(percent: Single); safecall;
    procedure setZoomScroll(percent: Single; left: Single; top: Single); safecall;
    procedure setView(const viewMode: WideString); safecall;
    procedure setViewScroll(const viewMode: WideString; offset: Single); safecall;
    procedure setViewRect(left: Single; top: Single; width: Single; height: Single); safecall;
    procedure printPages(from: Integer; to_: Integer); safecall;
    procedure printPagesFit(from: Integer; to_: Integer; shrinkToFit: WordBool); safecall;
    procedure printAll; safecall;
    procedure printAllFit(shrinkToFit: WordBool); safecall;
    procedure setShowScrollbars(On_: WordBool); safecall;
    function GetVersions: OleVariant; safecall;
    procedure setCurrentHightlight(a: Integer; b: Integer; c: Integer; d: Integer); safecall;
    procedure setCurrentHighlight(a: Integer; b: Integer; c: Integer; d: Integer); safecall;
    procedure postMessage(strArray: OleVariant); safecall;
    function Get_messageHandler: OleVariant; safecall;
    procedure Set_messageHandler(pVarOut: OleVariant); safecall;
    property src: WideString read Get_src write Set_src;
    property messageHandler: OleVariant read Get_messageHandler write Set_messageHandler;
  end;

// *********************************************************************//
// DispIntf:  IAcroAXDocShimDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {3B813CE7-7C10-4F84-AD06-9DF76D97A9AA}
// *********************************************************************//
  IAcroAXDocShimDisp = dispinterface
    ['{3B813CE7-7C10-4F84-AD06-9DF76D97A9AA}']
    property src: WideString dispid 1;
    function LoadFile(const fileName: WideString): WordBool; dispid 2;
    procedure setShowToolbar(On_: WordBool); dispid 3;
    procedure gotoFirstPage; dispid 4;
    procedure gotoLastPage; dispid 5;
    procedure gotoNextPage; dispid 6;
    procedure gotoPreviousPage; dispid 7;
    procedure setCurrentPage(n: Integer); dispid 8;
    procedure goForwardStack; dispid 9;
    procedure goBackwardStack; dispid 10;
    procedure setPageMode(const pageMode: WideString); dispid 11;
    procedure setLayoutMode(const layoutMode: WideString); dispid 12;
    procedure setNamedDest(const namedDest: WideString); dispid 13;
    procedure Print; dispid 14;
    procedure printWithDialog; dispid 15;
    procedure setZoom(percent: Single); dispid 16;
    procedure setZoomScroll(percent: Single; left: Single; top: Single); dispid 17;
    procedure setView(const viewMode: WideString); dispid 18;
    procedure setViewScroll(const viewMode: WideString; offset: Single); dispid 19;
    procedure setViewRect(left: Single; top: Single; width: Single; height: Single); dispid 20;
    procedure printPages(from: Integer; to_: Integer); dispid 21;
    procedure printPagesFit(from: Integer; to_: Integer; shrinkToFit: WordBool); dispid 22;
    procedure printAll; dispid 23;
    procedure printAllFit(shrinkToFit: WordBool); dispid 24;
    procedure setShowScrollbars(On_: WordBool); dispid 25;
    function GetVersions: OleVariant; dispid 26;
    procedure setCurrentHightlight(a: Integer; b: Integer; c: Integer; d: Integer); dispid 27;
    procedure setCurrentHighlight(a: Integer; b: Integer; c: Integer; d: Integer); dispid 28;
    procedure postMessage(strArray: OleVariant); dispid 29;
    property messageHandler: OleVariant dispid 30;
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TAcroPDF
// Help String      : Adobe Acrobat 7.0 Browser Control
// Default Interface: IAcroAXDocShim
// Def. Intf. DISP? : No
// Event   Interface: _IAcroAXDocShimEvents
// TypeFlags        : (2) CanCreate
// *********************************************************************//
  TAcroPDF = class(TOleControl)
  private
    FOnError: TNotifyEvent;
    FOnMessage: TNotifyEvent;
    FIntf: IAcroAXDocShim;
    function  GetControlInterface: IAcroAXDocShim;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
    function Get_messageHandler: OleVariant;
    procedure Set_messageHandler(pVarOut: OleVariant);
  public
    function LoadFile(const fileName: WideString): WordBool;
    procedure setShowToolbar(On_: WordBool);
    procedure gotoFirstPage;
    procedure gotoLastPage;
    procedure gotoNextPage;
    procedure gotoPreviousPage;
    procedure setCurrentPage(n: Integer);
    procedure goForwardStack;
    procedure goBackwardStack;
    procedure setPageMode(const pageMode: WideString);
    procedure setLayoutMode(const layoutMode: WideString);
    procedure setNamedDest(const namedDest: WideString);
    procedure Print;
    procedure printWithDialog;
    procedure setZoom(percent: Single);
    procedure setZoomScroll(percent: Single; left: Single; top: Single);
    procedure setView(const viewMode: WideString);
    procedure setViewScroll(const viewMode: WideString; offset: Single);
    procedure setViewRect(left: Single; top: Single; width: Single; height: Single);
    procedure printPages(from: Integer; to_: Integer);
    procedure printPagesFit(from: Integer; to_: Integer; shrinkToFit: WordBool);
    procedure printAll;
    procedure printAllFit(shrinkToFit: WordBool);
    procedure setShowScrollbars(On_: WordBool);
    function GetVersions: OleVariant;
    procedure setCurrentHightlight(a: Integer; b: Integer; c: Integer; d: Integer);
    procedure setCurrentHighlight(a: Integer; b: Integer; c: Integer; d: Integer);
    procedure postMessage(strArray: OleVariant);
    property  ControlInterface: IAcroAXDocShim read GetControlInterface;
    property  DefaultInterface: IAcroAXDocShim read GetControlInterface;
    property messageHandler: OleVariant index 30 read GetOleVariantProp write SetOleVariantProp;
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
    property src: WideString index 1 read GetWideStringProp write SetWideStringProp stored False;
    property OnError: TNotifyEvent read FOnError write FOnError;
    property OnMessage: TNotifyEvent read FOnMessage write FOnMessage;
  end;

procedure Register;

resourcestring
  dtlServerPage = 'ActiveX';

  dtlOcxPage = 'ActiveX';

implementation

uses ComObj;

procedure TAcroPDF.InitControlData;
const
  CEventDispIDs: array [0..1] of DWORD = (
    $00000001, $00000002);
  CControlData: TControlData2 = (
    ClassID: '{CA8A9780-280D-11CF-A24D-444553540000}';
    EventIID: '{E790E1D1-9DE8-4853-8AC6-933D4FD9C927}';
    EventCount: 2;
    EventDispIDs: @CEventDispIDs;
    LicenseKey: nil (*HR:$80004002*);
    Flags: $00000000;
    Version: 401);
begin
  ControlData := @CControlData;
  TControlData2(CControlData).FirstEventOfs := Cardinal(@@FOnError) - Cardinal(Self);
end;

procedure TAcroPDF.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as IAcroAXDocShim;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TAcroPDF.GetControlInterface: IAcroAXDocShim;
begin
  CreateControl;
  Result := FIntf;
end;

function TAcroPDF.Get_messageHandler: OleVariant;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.messageHandler;
end;

procedure TAcroPDF.Set_messageHandler(pVarOut: OleVariant);
begin
  DefaultInterface.Set_messageHandler(pVarOut);
end;

function TAcroPDF.LoadFile(const fileName: WideString): WordBool;
begin
  Result := DefaultInterface.LoadFile(fileName);
end;

procedure TAcroPDF.setShowToolbar(On_: WordBool);
begin
  DefaultInterface.setShowToolbar(On_);
end;

procedure TAcroPDF.gotoFirstPage;
begin
  DefaultInterface.gotoFirstPage;
end;

procedure TAcroPDF.gotoLastPage;
begin
  DefaultInterface.gotoLastPage;
end;

procedure TAcroPDF.gotoNextPage;
begin
  DefaultInterface.gotoNextPage;
end;

procedure TAcroPDF.gotoPreviousPage;
begin
  DefaultInterface.gotoPreviousPage;
end;

procedure TAcroPDF.setCurrentPage(n: Integer);
begin
  DefaultInterface.setCurrentPage(n);
end;

procedure TAcroPDF.goForwardStack;
begin
  DefaultInterface.goForwardStack;
end;

procedure TAcroPDF.goBackwardStack;
begin
  DefaultInterface.goBackwardStack;
end;

procedure TAcroPDF.setPageMode(const pageMode: WideString);
begin
  DefaultInterface.setPageMode(pageMode);
end;

procedure TAcroPDF.setLayoutMode(const layoutMode: WideString);
begin
  DefaultInterface.setLayoutMode(layoutMode);
end;

procedure TAcroPDF.setNamedDest(const namedDest: WideString);
begin
  DefaultInterface.setNamedDest(namedDest);
end;

procedure TAcroPDF.Print;
begin
  DefaultInterface.Print;
end;

procedure TAcroPDF.printWithDialog;
begin
  DefaultInterface.printWithDialog;
end;

procedure TAcroPDF.setZoom(percent: Single);
begin
  DefaultInterface.setZoom(percent);
end;

procedure TAcroPDF.setZoomScroll(percent: Single; left: Single; top: Single);
begin
  DefaultInterface.setZoomScroll(percent, left, top);
end;

procedure TAcroPDF.setView(const viewMode: WideString);
begin
  DefaultInterface.setView(viewMode);
end;

procedure TAcroPDF.setViewScroll(const viewMode: WideString; offset: Single);
begin
  DefaultInterface.setViewScroll(viewMode, offset);
end;

procedure TAcroPDF.setViewRect(left: Single; top: Single; width: Single; height: Single);
begin
  DefaultInterface.setViewRect(left, top, width, height);
end;

procedure TAcroPDF.printPages(from: Integer; to_: Integer);
begin
  DefaultInterface.printPages(from, to_);
end;

procedure TAcroPDF.printPagesFit(from: Integer; to_: Integer; shrinkToFit: WordBool);
begin
  DefaultInterface.printPagesFit(from, to_, shrinkToFit);
end;

procedure TAcroPDF.printAll;
begin
  DefaultInterface.printAll;
end;

procedure TAcroPDF.printAllFit(shrinkToFit: WordBool);
begin
  DefaultInterface.printAllFit(shrinkToFit);
end;

procedure TAcroPDF.setShowScrollbars(On_: WordBool);
begin
  DefaultInterface.setShowScrollbars(On_);
end;

function TAcroPDF.GetVersions: OleVariant;
begin
  Result := DefaultInterface.GetVersions;
end;

procedure TAcroPDF.setCurrentHightlight(a: Integer; b: Integer; c: Integer; d: Integer);
begin
  DefaultInterface.setCurrentHightlight(a, b, c, d);
end;

procedure TAcroPDF.setCurrentHighlight(a: Integer; b: Integer; c: Integer; d: Integer);
begin
  DefaultInterface.setCurrentHighlight(a, b, c, d);
end;

procedure TAcroPDF.postMessage(strArray: OleVariant);
begin
  DefaultInterface.postMessage(strArray);
end;

procedure Register;
begin
  RegisterComponents(dtlOcxPage, [TAcroPDF]);
end;

end.
