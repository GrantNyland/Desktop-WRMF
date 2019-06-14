//
//
//  UNIT      : Contains TAbstractComponent Classes
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 24/01/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UAbstractComponent;

interface

uses
  Classes,

  VCLTee.Chart,
  Vcl.Grids,
  Vcl.Forms,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Menus,
  Vcl.Buttons,
  Vcl.CheckLst,
  Vcl.ImgList,
  Vcl.Controls,
  Vcl.DBGrids,
  Messages,
  Windows,
  Types,

  UITypes,
  UViewDataItem,
  UGridActionObject,
  UGenericModelLinkClasses,
  UAbstractObject;

const
  CMenuSortWeightLeftMost  = 0;
  CMenuSortWeightRightMost = 1000;
  CmeNull      = -1;
  CmeSeparator =  0;

type
  TModelTabSheetName      = (mtsnNone,mtsnInput,mtsnOutput);

  TAbstractMainForm = class(TForm, ILanguageEventsInterface)
  protected
    FClosing: boolean;
    FAppModules: TAppModules;
    procedure CreateMemberObjects; virtual;
    procedure DestroyMemberObjects; virtual;
    procedure AssignHelpContext; virtual;
  public
    function LanguageHasChanged: boolean; virtual;
    function StudyHasChanged: boolean; virtual;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; virtual;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function Initialise: boolean; virtual;
    function SaveState: boolean; virtual;
    function ResetState: boolean; virtual;
    property Closing : boolean read FClosing write FClosing;
  end;

  TAbstractForm = class(TAbstractMainForm)
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
    constructor CreateWithoutDFM(AOwner: TComponent; AAppModules: TAppModules;ADefault: integer=0); virtual;
  end;

  TAbstractControl = class(TCustomControl, ILanguageEventsInterface)
  protected
    FAppModules: TAppModules;
    procedure CreateMemberObjects; virtual;
    procedure DestroyMemberObjects; virtual;
    procedure AssignHelpContext; virtual;
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
    destructor Destroy; override;
    function StudyHasChanged: boolean; virtual;
    function LanguageHasChanged: boolean; virtual;
    function Initialise: boolean; virtual;
    function SaveState: boolean; virtual;
    function ResetState: boolean; virtual;
  end;

  TAbstractPanel = class(TPanel, ILanguageEventsInterface)
  protected
    FAppModules: TAppModules;
    procedure CreateMemberObjects; virtual;
    procedure DestroyMemberObjects; virtual;
    procedure AssignHelpContext; virtual;
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
    destructor Destroy; override;
    function StudyHasChanged: boolean; virtual;
    function LanguageHasChanged: boolean; virtual;
    function Initialise: boolean; virtual;
    function SaveState: boolean; virtual;
    function ResetState: boolean; virtual;
    function CanCopyToClipboard: boolean;virtual;
    function CanExport: boolean;virtual;
    function CanPrint: boolean;virtual;
    procedure DoCopyToClipboard;virtual;
    procedure DoExport(AFileName: string = '');virtual;
    procedure DoPrint;virtual;
  published
    property BevelInner;
    property BevelOuter;
    property BorderStyle;
  end;

  TAbstractScrollBox = class(TScrollBox, ILanguageEventsInterface)
  protected
    FAppModules: TAppModules;
    procedure CreateMemberObjects; virtual;
    procedure DestroyMemberObjects; virtual;
    procedure AssignHelpContext; virtual;
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
    destructor Destroy; override;
    function StudyHasChanged: boolean; virtual;
    function LanguageHasChanged: boolean; virtual;
    function Initialise: boolean; virtual;
    function SaveState: boolean; virtual;
    function ResetState: boolean; virtual;
  end;

  TAbstractSpeedButton = class(TSpeedButton, ILanguageEventsInterface)
  protected
    FAppModules: TAppModules;
    FButtonKey: string;
    FStatusReason: string;
    procedure AssignHelpContext; virtual;
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules; AButtonKey: string); reintroduce; virtual;
    function StudyHasChanged: boolean; virtual;
    function LanguageHasChanged: boolean; virtual;
    function Initialise: boolean; virtual;
    function SaveState: boolean; virtual;
    function ResetState: boolean; virtual;
    property ButtonKey: string read FButtonKey write FButtonKey;
    property StatusReason: string read FStatusReason write FStatusReason;
  end;

  TAbstractMainSpeedButton = class(TAbstractSpeedButton)
  protected
    FMenuEvent: integer;
    FData: TObject;
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules; AButtonKey: string); reintroduce; virtual;
    destructor Destroy; override;
    property ButtonKey: string read FButtonKey write FButtonKey;
    property MenuEvent: integer read FMenuEvent write FMenuEvent;
    property Data: TObject read FData write FData;
  end;

  TAbstractMenuItem = class(TMenuItem, ILanguageEventsInterface)
  protected
    FAppModules: TAppModules;
    FMenuKey: string;
    FSortWeight: integer;
    FLanguageSwitchable: boolean;
    FMenuEvent: integer;
    FData: TObject;
    FStatusReason: string;
    procedure AssignHelpContext; virtual;
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules; AMenuKey: string); reintroduce; virtual;
    destructor Destroy; override;
    function StudyHasChanged: boolean; virtual;
    function LanguageHasChanged: boolean; virtual;
    function Initialise: boolean; virtual;
    function SaveState: boolean; virtual;
    function ResetState: boolean; virtual;
    property MenuKey: string read FMenuKey write FMenuKey;
    property SortWeight: integer read FSortWeight write FSortWeight;
    property LanguageSwitchable: boolean read FLanguageSwitchable write FLanguageSwitchable;
    property MenuEvent: integer read FMenuEvent write FMenuEvent;
    property Data: TObject read FData write FData;
    property StatusReason: string read FStatusReason write FStatusReason;
  end;

  TMenuEventHandler = function (MenuEvent: integer; AData: TObject): boolean of object;
  TAbstractMainMenu = class(TMainMenu, ILanguageEventsInterface)
  protected
    FAppModules: TAppModules;
    procedure CreateMemberObjects; virtual;
    procedure DestroyMemberObjects; virtual;
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
    destructor Destroy; override;
    function StudyHasChanged: boolean; virtual;
    function LanguageHasChanged: boolean; virtual;
    function Initialise: boolean; virtual;
    function SaveState: boolean; virtual;
    function ResetState: boolean; virtual;
    function AddMenuItem(AMenuKeys: array of string; ASortWeight: integer; AEvent: integer; AData: TObject): TAbstractMenuItem; virtual; abstract;
    function SetMenuItem(AMenuKeys: array of string; AnAction: TMenuSetAction; AStatusReason: string): boolean; virtual; abstract;
    function SetMenuItemCaption(AMenuKeys: array of string; ACaption: string): boolean; virtual;
    function SetMenuItemHelpContext(AMenuKeys: array of string; AHelpContextID : THelpContext): boolean; virtual;
    function GetMenuItemProperties(AMenuKeys: array of string) : TSetOfMenuAction; virtual;
  end;

  TAbstractToolBar = class(TAbstractPanel)
  protected
    FChildToolBar: TAbstractToolBar;
    procedure SetChildToolBar(AChildToolBar: TAbstractToolBar); virtual;
    procedure SetHorizontalPositions; virtual;
    procedure SetButtonEnabled(AButton: TAbstractSpeedButton; AEnabled: boolean; ADisabledReason: string); virtual;
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
    function LanguageHasChanged: boolean; override;
    property ChildToolBar: TAbstractToolBar read FChildToolBar write SetChildToolBar;
  end;

  TAbstractTabSheet = class(TTabSheet, ILanguageEventsInterface)
  protected
    FAppModules: TAppModules;
    FTabCaptionKey: string;
    FModelTabSheetName: TModelTabSheetName;
    procedure CreateMemberObjects; virtual;
    procedure DestroyMemberObjects; virtual;
    function GetToolBar: TAbstractToolBar; virtual; abstract;
    procedure AssignHelpContext; virtual;
    function GetModelTabSheetName:TModelTabSheetName;virtual;
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function StudyHasChanged: boolean; virtual;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; virtual;
    function ProcessParameterChangeEvent : boolean; virtual;
    function ProcessMetaDataEvent : boolean; virtual;
    function LanguageHasChanged: boolean; virtual;
    function Initialise: boolean; virtual;
    function SaveState: boolean; virtual;
    function ResetState: boolean; virtual;
    procedure ProcessCustomEvent(AData: TObject); virtual;
    procedure SetMenuVisible(AVisible: boolean); virtual;
    procedure DoOnHint; virtual;
    function CanTabChange: boolean; virtual;
    procedure TabHasChanged; virtual;
    function CanCopyToCLipboard: boolean; virtual;
    function CanExport: boolean; virtual;
    function CanPrint: boolean; virtual;
    procedure DoCopyToCLipboard; virtual;
    procedure DoExport(AFileName: string = '');virtual;
    procedure DoPrint; virtual;
    function DoJumpRequest(AViewDataNode: TViewDataNode): boolean; virtual;
    function DoesTabSheetOverrideJumpToHandling : boolean; virtual;
    procedure DoTabSheetOverrideJumpToHandling (AStringList : TStringlist); virtual;
    function DoGridAction(AGridActionObject: TGridActionObject): boolean; virtual;
    function DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): boolean; virtual;
    function CanApplicationClose : boolean; virtual;
    procedure ApplicationIsClosing; virtual;
    function EntityDescription (AFieldPropName : string;
                                AKeyValues     : string;
                                AFieldIndex    : string) : string; virtual;

    property ToolBar: TAbstractToolBar read GetToolBar;
    property ModelTabSheetName: TModelTabSheetName read GetModelTabSheetName;
  end;

  TAbstractDBGrid = class(TCustomDBGrid, ILanguageEventsInterface)
  protected
    FAppModules: TAppModules;
    procedure CreateMemberObjects; virtual;
    procedure DestroyMemberObjects; virtual;
    procedure AssignHelpContext; virtual;
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
    destructor Destroy; override;
    function StudyHasChanged: boolean; virtual;
    function LanguageHasChanged: boolean; virtual;
    function Initialise: boolean; virtual;
    function SaveState: boolean; virtual;
    function ResetState: boolean; virtual;
  end;

  TAbstractStringGrid = class(TStringGrid, ILanguageEventsInterface)
  protected
    FAppModules: TAppModules;
    FAlignment: TAlignment;
    procedure CreateMemberObjects; virtual;
    procedure DestroyMemberObjects; virtual;
    procedure AssignHelpContext; virtual;
    procedure CopyGridDataInto(var AStringList: TStringList);
    procedure SetAlignment(Value: TAlignment);
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
    destructor Destroy; override;
    function StudyHasChanged: boolean; virtual;
    function LanguageHasChanged: boolean; virtual;
    function Initialise: boolean; virtual;
    function SaveState: boolean; virtual;
    function ResetState: boolean; virtual;
    procedure DoPrint(Title: string); virtual;
    procedure DoCopyToClipboard; virtual;
    procedure DoExport(AFileName: string = '');virtual;
    function InsertColumn(AColIndex: Integer): integer;
    function InsertRow(ARowIndex: Integer): integer;
    property Alignment: TAlignment read FAlignment write SetAlignment  default taLeftJustify;
  end;

  TAbstractChart = class(TChart, ILanguageEventsInterface)
  protected
    FAppModules: TAppModules;
    procedure CreateMemberObjects; virtual;
    procedure DestroyMemberObjects; virtual;
    procedure AssignHelpContext; virtual;
    procedure DblClick; override;
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
    destructor Destroy; override;
    function StudyHasChanged: boolean; virtual;
    function LanguageHasChanged: boolean; virtual;
    function Initialise: boolean; virtual;
    function SaveState: boolean; virtual;
    function ResetState: boolean; virtual;
    procedure EditChartProperties; virtual;
    procedure DoPrint; virtual;
    procedure DoCopyToClipboard; virtual;
    procedure DoExport(AFileName: string = '');virtual;
  end;

  TAbstractRichEdit = class(TRichEdit, ILanguageEventsInterface)
  protected
    FAppModules: TAppModules;
    procedure CreateMemberObjects; virtual;
    procedure DestroyMemberObjects; virtual;
    procedure AssignHelpContext; virtual;
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
    destructor Destroy; override;
    function StudyHasChanged: boolean; virtual;
    function LanguageHasChanged: boolean; virtual;
    function Initialise: boolean; virtual;
    function SaveState: boolean; virtual;
    function ResetState: boolean; virtual;
  end;

  IStringImageList = interface
    ['{BA0BDB9A-A033-4FFA-85CF-F653F96C1027}']
    function AddResourceBitmap(AResourceBitmapName: string): integer;
    function GetResourceBitmapImageIndex(AResourceBitmapName: string): integer;
  end;

  TAbstractTreeView = class(TTreeView, ILanguageEventsInterface, IStringImageList)
  protected
    FAppModules: TAppModules;
    FAppImageList: TObject;
    FStringImageList: IStringImageList;
    procedure CreateMemberObjects; virtual;
    procedure DestroyMemberObjects; virtual;
    procedure AssignHelpContext; virtual;
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
    destructor Destroy; override;
    function StudyHasChanged: boolean; virtual;
    function LanguageHasChanged: boolean; virtual;
    function Initialise: boolean; virtual;
    function SaveState: boolean; virtual;
    function ResetState: boolean; virtual;
    property StringImageList: IStringImageList read FStringImageList implements IStringImageList;
  end;

  TAbstractPageControl = class(TPageControl, ILanguageEventsInterface)
  protected
    FAppModules: TAppModules;
    procedure CreateMemberObjects; virtual;
    procedure DestroyMemberObjects; virtual;
    procedure AssignHelpContext; virtual;
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
    destructor Destroy; override;
    procedure Change; override;
    function StudyHasChanged: boolean; virtual;
    function LanguageHasChanged: boolean; virtual;
    function Initialise: boolean; virtual;
    function SaveState: boolean; virtual;
    function ResetState: boolean; virtual;
  end;

  TAbstractFieldEdit = class(TEdit, ILanguageEventsInterface)
  protected
    FAppModules: TAppModules;
    procedure CreateMemberObjects; virtual;
    procedure DestroyMemberObjects; virtual;
    procedure AssignHelpContext; virtual;
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
    destructor Destroy; override;
    function StudyHasChanged: boolean; virtual;
    function LanguageHasChanged: boolean; virtual;
    function Initialise: boolean; virtual;
    function SaveState: boolean; virtual;
    function ResetState: boolean; virtual;
  end;

  TAbstractCheckBox = class(TCheckBox, ILanguageEventsInterface)
  protected
    FAppModules: TAppModules;
    procedure CreateMemberObjects; virtual;
    procedure DestroyMemberObjects; virtual;
    procedure AssignHelpContext; virtual;
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
    destructor Destroy; override;
    function StudyHasChanged: boolean; virtual;
    function LanguageHasChanged: boolean; virtual;
    function Initialise: boolean; virtual;
    function SaveState: boolean; virtual;
    function ResetState: boolean; virtual;
  end;


  TAbstractRadioGroup = class(TRadioGroup, ILanguageEventsInterface)
  protected
    FAppModules: TAppModules;
    procedure CreateMemberObjects; virtual;
    procedure DestroyMemberObjects; virtual;
    procedure AssignHelpContext; virtual;
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
    destructor Destroy; override;
    function StudyHasChanged: boolean; virtual;
    function LanguageHasChanged: boolean; virtual;
    function Initialise: boolean; virtual;
    function SaveState: boolean; virtual;
    function ResetState: boolean; virtual;
  end;

  TAbstractDateTimePicker = class(TDateTimePicker, ILanguageEventsInterface)
  protected
    FAppModules: TAppModules;
    procedure CreateMemberObjects; virtual;
    procedure DestroyMemberObjects; virtual;
    procedure AssignHelpContext; virtual;
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
    destructor Destroy; override;
    function StudyHasChanged: boolean; virtual;
    function LanguageHasChanged: boolean; virtual;
    function Initialise: boolean; virtual;
    function SaveState: boolean; virtual;
    function ResetState: boolean; virtual;
  end;

  TAbstractFieldButton = class(TButton, ILanguageEventsInterface)
  protected
    FAppModules: TAppModules;
    FButtonKey: string;
    FStatusReason: string;
    procedure CreateMemberObjects; virtual;
    procedure DestroyMemberObjects; virtual;
    procedure AssignHelpContext; virtual;
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules; AButtonKey: string); reintroduce; virtual;
    function StudyHasChanged: boolean; virtual;
    function LanguageHasChanged: boolean; virtual;
    function Initialise: boolean; virtual;
    function SaveState: boolean; virtual;
    function ResetState: boolean; virtual;
    property ButtonKey: string read FButtonKey write FButtonKey;
    property StatusReason: string read FStatusReason write FStatusReason;
  end;

  TAbstractComboBox = class(TComboBox, ILanguageEventsInterface)
  protected
    FAppModules: TAppModules;
    procedure CreateMemberObjects; virtual;
    procedure DestroyMemberObjects; virtual;
    procedure AssignHelpContext; virtual;
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
    destructor Destroy; override;
    function StudyHasChanged: boolean; virtual;
    function LanguageHasChanged: boolean; virtual;
    function Initialise: boolean; virtual;
    function SaveState: boolean; virtual;
    function ResetState: boolean; virtual;
  end;

  TAbstractFieldBitBtn = class(TBitBtn, ILanguageEventsInterface)
  protected
    FAppModules: TAppModules;
    FStatusReason: string;
    procedure CreateMemberObjects; virtual;
    procedure DestroyMemberObjects; virtual;
    procedure AssignHelpContext; virtual;
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
    function StudyHasChanged: boolean; virtual;
    function LanguageHasChanged: boolean; virtual;
    function Initialise: boolean; virtual;
    function SaveState: boolean; virtual;
    function ResetState: boolean; virtual;
    property StatusReason: string read FStatusReason write FStatusReason;
  end;

  TAbstractFieldListBox = class(TListBox, ILanguageEventsInterface)
  protected
    FAppModules: TAppModules;
    procedure CreateMemberObjects; virtual;
    procedure DestroyMemberObjects; virtual;
    procedure AssignHelpContext; virtual;
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
    destructor Destroy; override;
    function StudyHasChanged: boolean; virtual;
    function LanguageHasChanged: boolean; virtual;
    function Initialise: boolean; virtual;
    function SaveState: boolean; virtual;
    function ResetState: boolean; virtual;
  end;

  TAbstractCheckListBox = class(TCheckListBox, ILanguageEventsInterface)
  protected
    FAppModules: TAppModules;
    procedure CreateMemberObjects; virtual;
    procedure DestroyMemberObjects; virtual;
    procedure AssignHelpContext; virtual;
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
    destructor Destroy; override;
    function StudyHasChanged: boolean; virtual;
    function LanguageHasChanged: boolean; virtual;
    function Initialise: boolean; virtual;
    function SaveState: boolean; virtual;
    function ResetState: boolean; virtual;
  end;

  TAbstractRadioButton = class(TRadioButton, ILanguageEventsInterface)
  protected
    FAppModules: TAppModules;
    procedure CreateMemberObjects; virtual;
    procedure DestroyMemberObjects; virtual;
    procedure AssignHelpContext; virtual;
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
    destructor Destroy; override;
    function StudyHasChanged: boolean; virtual;
    function LanguageHasChanged: boolean; virtual;
    function Initialise: boolean; virtual;
    function SaveState: boolean; virtual;
    function ResetState: boolean; virtual;
  end;

  TAbstractStringGridWithEditor = class(TAbstractStringGrid, ILanguageEventsInterface)
  protected
    FEditcomboBox : TAbstractComboBox;
    FOldRow       : Integer;
    FOldCol       : Integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); override;
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function ResetState: boolean; override;
    procedure DoPrint(Title: string); override;
    procedure DoCopyToClipboard; override;
    property OldRow  : Integer                        read FOldRow                          write FOldRow;
    property OldCol  : Integer                        read FOldCol                          write FOldCol;
    property EditcomboBox     : TAbstractComboBox     read FEditcomboBox                    write FEditcomboBox;
  end;

   IFrame = interface
  ['{81EE8DDD-475E-4FAB-B1F5-AAD36D0118E3}']
    procedure OnShow;
    procedure OnHide;
  end;

  TAbstractFrame = class(TFrame, IFrame, ILanguageEventsInterface)
  protected
    FAppModules: TAppModules;
    FParentWnd: HWND;
    procedure CreateMemberObjects; virtual;
    procedure DestroyMemberObjects; virtual;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure OnShow; virtual;
    procedure OnHide; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent; AParentWnd: HWND;AAppModules: TAppModules);  reintroduce; overload; virtual;
    destructor Destroy; override;
    function StudyHasChanged: boolean; virtual;
    function LanguageHasChanged: boolean; virtual;
    function Initialise: boolean; virtual;
    function SaveState: boolean; virtual;
    function ResetState: boolean; virtual;
  end;



  TAbstractFrameHandle = class(TAbstractFrame)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnShow; override;
    procedure OnHide; override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent; AParentWnd: HWND;AAppModules: TAppModules);  override;
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function ResetState: boolean; override;
  end;


implementation

uses

  SysUtils,
  Vcl.Graphics,
  Vcl.Printers,
  Vcl.Clipbrd,
//  VCLTee.TeeEdit,
//  VCLTee.TeeProcs,


  UConstants,
  UErrorHandlingOperations;


{ Implementation classes }

{ TAbstractFrame }
procedure TAbstractFrame.CMShowingChanged(var Message: TMessage);
const OPNAME = 'TAbstractFrame.CMShowingChanged';
begin
  try
    inherited;
    SetFocus;
    SelectFirst; // Put the focus to the first control in the frame
  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TAbstractFrame.Create(AOwner: TComponent; AParentWnd: HWND;AAppModules: TAppModules);
const OPNAME = 'TAbstractFrame.Create';
begin
  try
    inherited Create(AOwner);
    FAppModules := AAppModules;
    FParentWnd := AParentWnd;
    CreateMemberObjects;
    Create(AOwner); // Let this call out to the child consructor, if overridden
  except on E: Exception do HandleError(E, OPNAME) end;
end;



procedure TAbstractFrame.CreateMemberObjects;
const OPNAME = 'TAbstractFrame.CreateMemberObjects';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractFrame.CreateParams(var Params: TCreateParams);
const OPNAME = 'TAbstractFrame.CreateParams';
begin
  try
  inherited CreateParams(Params);
  if (Parent = nil) and IsLibrary and not (csDestroying in ComponentState) then
    Params.WndParent := FParentWnd;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TAbstractFrame.Destroy;
const OPNAME = 'TAbstractFrame.DestroyMemberObjects';
begin
  try
    DestroyMemberObjects;
    inherited Destroy;
    FAppModules := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractFrame.DestroyMemberObjects;
const OPNAME = 'TAbstractFrame.DestroyMemberObjects';
begin
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractFrame.Initialise: boolean;
const OPNAME = 'TAbstractFrame.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractFrame.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractFrame.LanguageHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractFrame.OnShow;
const OPNAME = 'TAbstractFrame.OnShow';
begin
  try
  // Empty
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractFrame.ResetState: boolean;
const OPNAME = 'TAbstractFrame.ResetState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
function TAbstractFrame.SaveState: boolean;
const OPNAME = 'TAbstractFrame.SaveState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractFrame.StudyHasChanged: boolean;
const OPNAME = 'TAbstractFrame.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractFrame.OnHide;
const OPNAME = 'TAbstractFrame.OnHide';
begin
  try
  // Empty
  except on E: Exception do HandleError(E, OPNAME) end;
end;


{ TAppImageList }

type
  TAppImageList = class(TAggregatedObject, IStringImageList)
  protected
    FResourceNames: TStringList;
    FImageList: TCustomImageList;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    function AddResourceBitmap(AResourceBitmapName: string): integer;
    function GetResourceBitmapImageIndex(AResourceBitmapName: string): integer;
    property ResourceNames: TStringList read FResourceNames;
    property ImageList: TCustomImageList read FImageList;
  end;

constructor TAppImageList.Create(AOwner: TComponent);
const OPNAME = 'TAppImageList.Create';
begin
  try
    inherited Create(AOwner);
    FResourceNames := TStringList.Create;
    FImageList := TCustomImageList.Create(AOwner);
    FResourceNames.Add('DUMMY');
    FImageList.ResInstLoad(HImagesInstance, rtBitmap, 'DUMMY', clNone);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TAppImageList.Destroy;
const OPNAME = 'TAppImageList.Destroy';
begin
  try
    FreeAndNil(FResourceNames);
    inherited Destroy;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppImageList.AddResourceBitmap(AResourceBitmapName: string): integer;
const OPNAME = 'TAppImageList.AddResourceBitmap';
begin
  Result := 0;
  try
    if (AResourceBitmapName <> '') then
      if (FResourceNames.IndexOf(AResourceBitmapName) < 0) then
        if FImageList.GetInstRes(HImagesInstance, rtBitmap, AResourceBitmapName, 16, [], clWhite) then
          FResourceNames.Add(AResourceBitmapName);
    Result := GetResourceBitmapImageIndex(AResourceBitmapName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAppImageList.GetResourceBitmapImageIndex(AResourceBitmapName: string): integer;
const OPNAME = 'TAppImageList.GetResourceBitmapImageIndex';
begin
  Result := 0;
  try
    if (AResourceBitmapName <> '') then
      Result := FResourceNames.IndexOf(AResourceBitmapName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAbstractMainForm }

procedure TAbstractMainForm.CreateMemberObjects;
const OPNAME = 'TAbstractMainForm.CreateMemberObjects';
begin
  try
    // Do not create anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractMainForm.DestroyMemberObjects;
const OPNAME = 'TAbstractMainForm.DestroyMemberObjects';
begin
  try
    // Do not free anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractMainForm.AssignHelpContext;
const OPNAME = 'TAbstractMainForm.AssignHelpContext';
begin
  try
    // Do not assign anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractMainForm.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractMainForm.LanguageHasChanged';
begin
  Result := True;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractMainForm.AfterConstruction;
const OPNAME = 'TAbstractMainForm.AfterConstruction';
begin
  try
    inherited AfterConstruction;
    CreateMemberObjects;
    AssignHelpContext;
    if Assigned(FAppModules) and Assigned(FAppModules.ViewIni()) then
      FAppModules.ViewIni.LoadFormView(self);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TAbstractMainForm.BeforeDestruction;
const OPNAME = 'TAbstractMainForm.BeforeDestruction';
begin
  try
    if Assigned(FAppModules) and Assigned(FAppModules.ViewIni()) then
      FAppModules.ViewIni.SaveFormView(self);
    DestroyMemberObjects;
    inherited BeforeDestruction;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAbstractMainForm.StudyHasChanged: boolean;
const OPNAME = 'TAbstractMainForm.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractMainForm.StudyDataHasChanged(AContext: TChangeContext;
                                               AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TAbstractMainForm.StudyDataHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractMainForm.Initialise: boolean;
const OPNAME = 'TAbstractMainForm.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractMainForm.SaveState: boolean;
const OPNAME = 'TAbstractMainForm.SaveState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractMainForm.ResetState: boolean;
const OPNAME = 'TAbstractMainForm.ResetState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAbstractForm }

constructor TAbstractForm.Create(AOwner: TComponent; AAppModules: TAppModules);
const OPNAME = 'TAbstractForm.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TAbstractForm.CreateWithoutDFM(AOwner: TComponent;AAppModules: TAppModules;ADefault: integer=0);
const OPNAME = 'TAbstractForm.CreateWithoutDFM';
begin
  inherited CreateNew(AOwner);
  try
    FAppModules := AAppModules;
    //AfterConstruction;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAbstractControl }

procedure TAbstractControl.AssignHelpContext;
const OPNAME = 'TAbstractControl.AssignHelpContext';
begin
  try
   // Do not assign anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TAbstractControl.Create(AOwner: TComponent; AAppModules: TAppModules);
const OPNAME = 'TAbstractControl.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
    CreateMemberObjects;
    AssignHelpContext;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractControl.CreateMemberObjects;
const OPNAME = 'TAbstractControl.CreateMemberObjects';
begin
  try
   // Do not create anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TAbstractControl.Destroy;
const OPNAME = 'TAbstractControl.Destroy';
begin
  try
    DestroyMemberObjects;
    inherited Destroy;
    FAppModules := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractControl.DestroyMemberObjects;
const OPNAME = 'TAbstractControl.DestroyMemberObjects';
begin
  try
   // Do not free anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractControl.Initialise: boolean;
const OPNAME = 'TAbstractControl.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractControl.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractControl.LanguageHasChanged';
begin
  Result := True;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractControl.ResetState: boolean;
const OPNAME = 'TAbstractControl.ResetState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractControl.SaveState: boolean;
const OPNAME = 'TAbstractControl.SaveState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractControl.StudyHasChanged: boolean;
const OPNAME = 'TAbstractControl.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAbstractPanel }

procedure TAbstractPanel.AssignHelpContext;
const OPNAME = 'TAbstractPanel.AssignHelpContext';
begin
  try
   // Do not assign anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractPanel.CanCopyToClipboard: boolean;
const OPNAME = 'TAbstractPanel.CanCopyToClipboard';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractPanel.CanExport: boolean;
const OPNAME = 'TAbstractPanel.CanExport';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractPanel.CanPrint: boolean;
const OPNAME = 'TAbstractPanel.CanPrint';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TAbstractPanel.Create(AOwner: TComponent; AAppModules: TAppModules);
const OPNAME = 'TAbstractPanel.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
    CreateMemberObjects;
    AssignHelpContext;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractPanel.CreateMemberObjects;
const OPNAME = 'TAbstractPanel.CreateMemberObjects';
begin
  try
   // Do not create anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TAbstractPanel.Destroy;
const OPNAME = 'TAbstractPanel.Destroy';
begin
  try
    DestroyMemberObjects;
    inherited Destroy;
    FAppModules := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractPanel.DestroyMemberObjects;
const OPNAME = 'TAbstractPanel.DestroyMemberObjects';
begin
  try
   // Do not free anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractPanel.DoCopyToClipboard;
const OPNAME = 'TAbstractPanel.DoCopyToClipboard';
begin
  try
   // Do nothing here as the descendent will do the implementation.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractPanel.DoExport(AFileName: string = '');
const OPNAME = 'TAbstractPanel.DoExport';
begin
  try
   // Do nothing here as the descendent will do the implementation.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractPanel.DoPrint;
const OPNAME = 'TAbstractPanel.DoPrint';
begin
  try
   // Do nothing here as the descendent will do the implementation.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractPanel.Initialise: boolean;
const OPNAME = 'TAbstractPanel.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractPanel.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractPanel.LanguageHasChanged';
begin
  Result := True;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractPanel.ResetState: boolean;
const OPNAME = 'TAbstractPanel.ResetState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractPanel.SaveState: boolean;
const OPNAME = 'TAbstractPanel.SaveState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractPanel.StudyHasChanged: boolean;
const OPNAME = 'TAbstractPanel.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAbstractMenuItem }

procedure TAbstractMenuItem.AssignHelpContext;
const OPNAME = 'TAbstractMenuItem.AssignHelpContext';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TAbstractMenuItem.Create(AOwner: TComponent; AAppModules: TAppModules; AMenuKey: string);
const OPNAME = 'TAbstractMainForm.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
    FMenuKey := AMenuKey;
    FLanguageSwitchable := True;
    FData := nil;
    FStatusReason := 'Grant';
    AssignHelpContext;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TAbstractMenuItem.Destroy;
const OPNAME = 'TAbstractMenuItem.Destroy';
begin
  try
    FreeAndNil(FData);
    inherited;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractMenuItem.Initialise: boolean;
const OPNAME = 'TAbstractMenuItem.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractMenuItem.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractMenuItem.LanguageHasChanged';
var LIndex: integer;
begin
  Result := True;
  try

    // Make sure that language switching is required.
    Hint := '';
    if (FMenuEvent = CmeSeparator) then
    begin
      Caption := '-';
      Hint    := '';
    end
    else
    begin
      if FLanguageSwitchable and Assigned(FAppModules.Language()) then
      begin
        Caption := FAppModules.Language.GetString('MenuCaption.' + FMenuKey);
        Hint    := FAppModules.Language.GetString('MenuHint.'    + FMenuKey);
      end;
    end;

    // Check if a status reason is required on the hint.
    if (FStatusReason <> '') and Assigned(FAppModules.Language()) then
    begin
      if (Hint <> '') then
      begin
        Hint := Hint + #13#10#13#10'  ' + FAppModules.Language.GetString('ButtonReason.' + FStatusReason);
      end else begin
        Hint := FAppModules.Language.GetString('ButtonReason.' + FStatusReason);
      end;
    end;

    // Process all the children.
    for LIndex := 0 to Count - 1 do
      TAbstractMenuItem(Items[LIndex]).LanguageHasChanged;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractMenuItem.ResetState: boolean;
const OPNAME = 'TAbstractMenuItem.ResetState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractMenuItem.SaveState: boolean;
const OPNAME = 'TAbstractMenuItem.SaveState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractMenuItem.StudyHasChanged: boolean;
const OPNAME = 'TAbstractMenuItem.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAbstractMainMenu }

constructor TAbstractMainMenu.Create(AOwner: TComponent; AAppModules: TAppModules);
const OPNAME = 'TAbstractMainMenu.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
    CreateMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractMainMenu.CreateMemberObjects;
const OPNAME = 'TAbstractMainMenu.CreateMemberObjects';
begin
  try
   // Do not create anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TAbstractMainMenu.Destroy;
const OPNAME = 'TAbstractMainMenu.Destroy';
begin
  try
    DestroyMemberObjects;
    inherited Destroy;
    FAppModules := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractMainMenu.DestroyMemberObjects;
const OPNAME = 'TAbstractMainMenu.DestroyMemberObjects';
begin
  try
   // Do not free anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractMainMenu.GetMenuItemProperties(
  AMenuKeys: array of string): TSetOfMenuAction;
const OPNAME = 'TAbstractMainMenu.GetMenuItemProperties';
begin
  Result := [];
  try
    Result := [];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractMainMenu.Initialise: boolean;
const OPNAME = 'TAbstractMainMenu.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractMainMenu.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractMainMenu.LanguageHasChanged';
var LIndex: integer;
begin
  Result := True;
  try
    for LIndex := 0 to Items.Count - 1 do
      TAbstractMenuItem(Items[LIndex]).LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractMainMenu.ResetState: boolean;
const OPNAME = 'TAbstractMainMenu.ResetState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractMainMenu.SaveState: boolean;
const OPNAME = 'TAbstractMainMenu.SaveState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractMainMenu.SetMenuItemCaption(AMenuKeys: array of string; ACaption: string): boolean;
const OPNAME = 'TAbstractMainMenu.SetMenuItemCaption';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractMainMenu.SetMenuItemHelpContext(
  AMenuKeys: array of string; AHelpContextID: THelpContext): boolean;
const OPNAME = 'SetMenuItemHelpContext';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractMainMenu.StudyHasChanged: boolean;
const OPNAME = 'TAbstractMainMenu.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAbstractSpeedButton }

procedure TAbstractSpeedButton.AssignHelpContext;
const OPNAME = 'TAbstractSpeedButton.AssignHelpContext';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TAbstractSpeedButton.Create(AOwner: TComponent; AAppModules: TAppModules; AButtonKey: string);
const OPNAME = 'TAbstractSpeedButton.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
    FButtonKey := AButtonKey;
    AssignHelpContext;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractSpeedButton.Initialise: boolean;
const OPNAME = 'TAbstractSpeedButton.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractSpeedButton.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractSpeedButton.LanguageHasChanged';
begin
  Result := True;
  try

    // Set the hint as required.
    Hint := '';
    if Assigned(FAppModules.Language()) then
      Hint := FAppModules.Language.GetString('ButtonHint.' + FButtonKey);

    // Check if a status reason is required on the hint.
    if (FStatusReason <> '') and Assigned(FAppModules.Language()) then
    begin
      if (Hint <> '') then
      begin
        Hint := Hint + #13#10#13#10'  ' + FAppModules.Language.GetString('ButtonReason.' + FStatusReason);
      end else begin
        Hint := FAppModules.Language.GetString('ButtonReason.' + FStatusReason);
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractSpeedButton.ResetState: boolean;
const OPNAME = 'TAbstractSpeedButton.ResetState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractSpeedButton.SaveState: boolean;
const OPNAME = 'TAbstractSpeedButton.SaveState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractSpeedButton.StudyHasChanged: boolean;
const OPNAME = 'TAbstractSpeedButton.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAbstractMainSpeedButton }

constructor TAbstractMainSpeedButton.Create(AOwner: TComponent; AAppModules: TAppModules; AButtonKey: string);
const OPNAME = 'TAbstractMainSpeedButton.Create';
begin
  inherited Create(AOwner, AAppModules, AButtonKey);
  try
    FMenuEvent := 0;
    FData := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TAbstractMainSpeedButton.Destroy;
const OPNAME = 'TAbstractMainSpeedButton.Destroy';
begin
  try
    FreeAndNil(FData);
    inherited;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAbstractToolBar }

constructor TAbstractToolBar.Create(AOwner: TComponent; AAppModules: TAppModules);
const OPNAME = 'TAbstractToolBar.Create';
begin
  inherited Create(AOwner, AAppModules);
  try
    ShowHint := True;
    FChildToolBar := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractToolBar.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractToolBar.LanguageHasChanged';
begin
  Result := True;
  try
    if Assigned(FChildToolBar) then
      FChildToolBar.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractToolBar.SetButtonEnabled(AButton: TAbstractSpeedButton; AEnabled: boolean; ADisabledReason: string);
const OPNAME = 'TAbstractToolBar.SetButtonEnabled';
begin
  try
    if Assigned(AButton) then
    begin
      AButton.Enabled := AEnabled;
      if AEnabled then
      begin
        AButton.StatusReason := '';
      end else begin
        AButton.StatusReason := ADisabledReason;
      end;
      AButton.LanguageHasChanged;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractToolBar.SetChildToolBar(AChildToolBar: TAbstractToolBar);
const OPNAME = 'TAbstractToolBar.SetChildToolBar';
begin
  try
    if Assigned(FChildToolBar) then
      FChildToolBar.Parent := nil;
    FChildToolBar := AChildToolBar;
    if Assigned(FChildToolBar) then
    begin
      FChildToolBar.Parent := self;
      FChildToolBar.Top    := 3;
    end;
    SetHorizontalPositions;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractToolBar.SetHorizontalPositions;
const
  OPNAME = 'TAbstractToolBar.SetHorizontalPositions';
begin
  try
    if Assigned(FChildToolBar) then
      FChildToolBar.SetHorizontalPositions;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAbstractTabSheet }

constructor TAbstractTabSheet.Create(AOwner: TComponent; AAppModules: TAppModules);
const OPNAME = 'TAbstractTabSheet.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
    FTabCaptionKey := '';
    FModelTabSheetName := mtsnNone;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractTabSheet.AfterConstruction;
const OPNAME = 'TAbstractTabSheet.AfterConstruction';
begin
  try
    inherited;
    CreateMemberObjects;
    AssignHelpContext;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractTabSheet.CreateMemberObjects;
const OPNAME = 'TAbstractTabSheet.CreateMemberObjects';
begin
  try
   // Do not create anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractTabSheet.BeforeDestruction;
const OPNAME = 'TAbstractTabSheet.BeforeDestruction';
begin
  inherited BeforeDestruction;
  try
    DestroyMemberObjects;
    FAppModules := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractTabSheet.DestroyMemberObjects;
const OPNAME = 'TAbstractTabSheet.DestroyMemberObjects';
begin
  try
   // Do not free anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractTabSheet.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractTabSheet.LanguageHasChanged';
begin
  Result := True;
  try
    Caption := FAppModules.Language.GetString('TabCaption.' + FTabCaptionKey);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractTabSheet.SetMenuVisible(AVisible: boolean);
const OPNAME = 'TAbstractTabSheet.SetMenuVisible';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractTabSheet.ProcessCustomEvent(AData: TObject);
const OPNAME = 'TAbstractTabSheet.ProcessCustomEvent';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractTabSheet.DoOnHint;
const OPNAME = 'TAbstractTabSheet.DoOnHint';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractTabSheet.CanTabChange: boolean;
const OPNAME = 'TAbstractTabSheet.CanTabChange';
begin
  Result := True;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractTabSheet.TabHasChanged;
const OPNAME = 'TAbstractTabSheet.TabHasChanged';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractTabSheet.CanCopyToCLipboard: boolean;
const OPNAME = 'TAbstractTabSheet.CanCopyToCLipboard';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractTabSheet.CanExport: boolean;
const OPNAME = 'TAbstractTabSheet.CanExport';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractTabSheet.CanPrint: boolean;
const OPNAME = 'TAbstractTabSheet.CanPrint';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractTabSheet.DoCopyToClipboard;
const OPNAME = 'TAbstractTabSheet.DoCopyToClipboard';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractTabSheet.DoExport(AFileName: string = '');
const OPNAME = 'TAbstractTabSheet.DoExport';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractTabSheet.DoPrint;
const OPNAME = 'TAbstractTabSheet.DoPrint';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractTabSheet.StudyHasChanged: boolean;
const OPNAME = 'TAbstractTabSheet.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractTabSheet.Initialise: boolean;
const OPNAME = 'TAbstractTabSheet.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractTabSheet.SaveState: boolean;
const OPNAME = 'TAbstractTabSheet.SaveState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractTabSheet.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TAbstractTabSheet.StudyDataHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractTabSheet.ProcessParameterChangeEvent: boolean;
const OPNAME = 'TAbstractTabSheet.ProcessParameterChangeEvent';
begin
  Result := FALSE;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractTabSheet.ProcessMetaDataEvent: boolean;
const OPNAME = 'TAbstractTabSheet.ProcessMetaDataEvent';
begin
  Result := FALSE;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractTabSheet.AssignHelpContext;
const OPNAME = 'TAbstractTabSheet.AssignHelpContext';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractTabSheet.DoGridAction(AGridActionObject: TGridActionObject): boolean;
const OPNAME = 'TAbstractTabSheet.DoGridAction';
begin
  Result := False;
  try
    raise Exception.Create('The requested action is not yet implemented for the current sheet.');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractTabSheet.DoJumpRequest(AViewDataNode: TViewDataNode): boolean;
const OPNAME = 'TAbstractTabSheet.DoJumpRequest';
begin
  Result := False;
  try
    raise Exception.Create('The requested jump action is not yet implemented for the current sheet.');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractTabSheet.DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): boolean;
const OPNAME = 'TAbstractTabSheet.DoCustomTabSheetEvent';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractTabSheet.CanApplicationClose: boolean;
const OPNAME = 'TAbstractTabSheet.CanApplicationClose';
begin
  Result := True;
end;

procedure TAbstractTabSheet.ApplicationIsClosing;
const OPNAME = 'TAbstractTabSheet.ApplicationIsClosing';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractTabSheet.DoesTabSheetOverrideJumpToHandling: boolean;
const OPNAME = 'TAbstractTabSheet.DoesTabSheetOverrideJumpToHandling';
begin
  Result := false;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractTabSheet.DoTabSheetOverrideJumpToHandling(AStringList: TStringlist);
const OPNAME = 'TAbstractTabSheet.DoTabSheetOverrideJumpToHandling';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractTabSheet.ResetState: boolean;
const OPNAME = 'TAbstractTabSheet.ResetState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractTabSheet.EntityDescription (AFieldPropName : string;
                                              AKeyValues     : string;
                                              AFieldIndex    : string): string;
const OPNAME = 'TAbstractTabSheet.EntityDescription';
begin
  Result := 'Unknown entity';
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractTabSheet.GetModelTabSheetName: TModelTabSheetName;
const OPNAME = 'TAbstractTabSheet.GetModelTabSheetName';
begin
  Result := mtsnNone;
  try
    Result := FModelTabSheetName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAbstractDBGrid }

procedure TAbstractDBGrid.AssignHelpContext;
const OPNAME = 'TAbstractDBGrid.AssignHelpContext';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TAbstractDBGrid.Create(AOwner: TComponent; AAppModules: TAppModules);
const OPNAME = 'TAbstractDBGrid.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
    CreateMemberObjects;
    AssignHelpContext;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractDBGrid.CreateMemberObjects;
const OPNAME = 'TAbstractDBGrid.CreateMemberObjects';
begin
  try
   // Do not create anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TAbstractDBGrid.Destroy;
const OPNAME = 'TAbstractDBGrid.Destroy';
begin
  try
    DestroyMemberObjects;
    inherited Destroy;
    FAppModules := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractDBGrid.DestroyMemberObjects;
const OPNAME = 'TAbstractDBGrid.DestroyMemberObjects';
begin
  try
   // Do not free anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDBGrid.Initialise: boolean;
const OPNAME = 'TAbstractDBGrid.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDBGrid.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractDBGrid.LanguageHasChanged';
begin
  Result := True;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDBGrid.ResetState: boolean;
const OPNAME = 'TAbstractDBGrid.ResetState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDBGrid.SaveState: boolean;
const OPNAME = 'TAbstractDBGrid.SaveState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDBGrid.StudyHasChanged: boolean;
const OPNAME = 'TAbstractDBGrid.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAbstractStringGrid }

procedure TAbstractStringGrid.AssignHelpContext;
const OPNAME = 'TAbstractStringGrid.AssignHelpContext';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TAbstractStringGrid.Create(AOwner: TComponent; AAppModules: TAppModules);
const OPNAME = 'TAbstractStringGrid.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
    FAlignment  := taLeftJustify;
    CreateMemberObjects;
    AssignHelpContext;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractStringGrid.CreateMemberObjects;
const OPNAME = 'TAbstractStringGrid.CreateMemberObjects';
begin
  try
   // Do not create anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TAbstractStringGrid.Destroy;
const OPNAME = 'TAbstractStringGrid.Destroy';
begin
  try
    DestroyMemberObjects;
    inherited Destroy;
    FAppModules := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractStringGrid.DestroyMemberObjects;
const OPNAME = 'TAbstractStringGrid.DestroyMemberObjects';
begin
  try
   // Do not free anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractStringGrid.Initialise: boolean;
const OPNAME = 'TAbstractStringGrid.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractStringGrid.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractStringGrid.LanguageHasChanged';
begin
  Result := True;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractStringGrid.ResetState: boolean;
const OPNAME = 'TAbstractStringGrid.ResetState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractStringGrid.SaveState: boolean;
const OPNAME = 'TAbstractStringGrid.SaveState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractStringGrid.StudyHasChanged: boolean;
const OPNAME = 'TAbstractStringGrid.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

//Author: Reinhard Schatzl
//Site:http://www.swissdelphicenter.ch/torry/showcode.php?id=1577
// Modified from German to english
procedure TAbstractStringGrid.DoPrint(Title: string);
const OPNAME = 'TAbstractStringGrid.DoPrint';
var
  LPageIndex, LLinesIndex, LColumnIndex, LYPos, LXPos, LHorzSize, LVertSize: Integer;
  LPageCount, LPageCurrent, LLines, LHeaderSize, LFooterSize, LLineSize, LFontHeight: Integer;
  LPageWidth, LPageHeight: Extended;
  LFooter: string;
begin
  LHeaderSize := 100;
  LFooterSize := 200;
  LLineSize := 36;
  LFontHeight := 36;

  Printer.Title  := Title;
  Printer.BeginDoc;

  LPageWidth := GetDeviceCaps(Printer.Canvas.Handle, PHYSICALWIDTH) /
    GetDeviceCaps(Printer.Canvas.Handle, LOGPIXELSX) * 25.4; 
  LPageHeight := GetDeviceCaps(Printer.Canvas.Handle, PHYSICALHEIGHT) / 
    GetDeviceCaps(Printer.Canvas.Handle, LOGPIXELSY) * 25.4; 

  LVertSize := Trunc(LPageHeight) * 10;
  LHorzSize := Trunc(LPageWidth) * 10;
  SetMapMode(Printer.Canvas.Handle, MM_LOMETRIC);

  LLines := (LVertSize - LHeaderSize - LFooterSize) div LLineSize;
  if Self.RowCount mod LLines <> 0 then
    LPageCount := Self.RowCount div LLines + 1
  else
    LPageCount := Self.RowCount div LLines;

  LPageCurrent := 1;
  for LPageIndex := 1 to LPageCount do
  begin
    Printer.Canvas.Font.Height := 48;
    Printer.Canvas.TextOut((LHorzSize div 2 - (Printer.Canvas.TextWidth(Title) div 2)),
      - 20,Title);
    Printer.Canvas.Pen.Width := 5;
    Printer.Canvas.MoveTo(0, - LHeaderSize);
    Printer.Canvas.LineTo(LHorzSize, - LHeaderSize);
    Printer.Canvas.MoveTo(0, - LVertSize + LFooterSize);
    Printer.Canvas.LineTo(LHorzSize, - LVertSize + LFooterSize);
    Printer.Canvas.Font.Height := 36;
    LFooter := 'Page: ' + IntToStr(LPageCurrent) + ' of ' + IntToStr(LPageCount);
    Printer.Canvas.TextOut((LHorzSize div 2 - (Printer.Canvas.TextWidth(LFooter) div 2)),
      - VertSize + 150,LFooter);
    Printer.Canvas.Font.Height := LFontHeight;
    LYPos := LHeaderSize + 10;
    for LLinesIndex := 1 to LLines do
    begin
      if Self.RowCount >= LLinesIndex + (LPageCurrent - 1) * LLines then
      begin
        LXPos := 0;
        for LColumnIndex := 0 to Self.ColCount - 1 do
        begin
          Printer.Canvas.TextOut(LXPos, - LYPos,
            Self.Cells[LColumnIndex, LLinesIndex + (LPageCurrent - 1) * LLines - 1]);
          LXPos := LXPos + Self.ColWidths[LColumnIndex] * 3;
        end;
        LYPos := LYPos + LLineSize;
      end;
    end;
    Inc(LPageCurrent);
    if LPageCurrent <= LPageCount then Printer.NewPage;
  end;
  Printer.EndDoc;
end;

procedure TAbstractStringGrid.DoCopyToClipboard;
const OPNAME = 'TAbstractStringGrid.DoCopyToClipboard';
var LGridData: TStringList;
begin
  try
    LGridData := TStringList.Create;
    try
      CopyGridDataInto(LGridData);
      ClipBoard.AsText := LGridData.Text;
    finally
      LGridData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractStringGrid.DoExport(AFileName: string = '');
const OPNAME = 'TAbstractStringGrid.DoExport';
var
  LGridData       : TStringList;
  LExportFilename : string;
begin
  try
    if(Trim(AFileName) <> '') then
    begin
      LExportFilename := AFileName;
      LExportFilename := ChangeFileExt(LExportFilename,'.csv');
    end;

    if FAppModules.GetExportFilename('.csv',
      'Comma Separated Files (*.csv)|*.csv|All Files (*.*)|*.*', LExportFilename) then
    begin
      LGridData := TStringList.Create;
      try
        CopyGridDataInto(LGridData);
        LGridData.SaveToFile(LExportFilename);
      finally
        FreeAndNil(LGridData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TAbstractStringGrid.CopyGridDataInto(var AStringList: TStringList);
const OPNAME = 'TAbstractStringGrid.CopyGridDataInto';
var LRowIndex: integer;
  function FirstRowEmpty: boolean;
  const OPNAME = 'UAbstractComponent.FirstRowEmpty';
  var LCount: integer;
  begin
    Result := True;
    for LCount := 0 to ColCount - 1 do
      if (Trim(Cells[LCount,0]) <> '') then
      begin
        Result := False;
        Break;
      end;
  end;

  function FirstColEmpty: boolean;
  const OPNAME = 'UAbstractComponent.FirstColEmpty';
  var LCount: integer;
  begin
    Result := True;
    for LCount := 0 to RowCount - 1 do
      if (Trim(Cells[0,LCount]) <> '') then
      begin
        Result := False;
        Break;
      end;
  end;
begin
  try
    AStringList.Clear;
    if FirstRowEmpty then
    begin
      for LRowIndex := 1 to RowCount - 1 do
        AStringList.Add(Rows[LRowIndex].CommaText);
    end
    else
    if FirstColEmpty then
    begin
      for LRowIndex := 0 to RowCount - 1 do
        AStringList.Add(Rows[LRowIndex].CommaText);
      for LRowIndex := 0 to AStringList.Count - 1 do
        if(Length(AStringList[LRowIndex]) > 1) and (AStringList[LRowIndex][1] = ',') then
          AStringList[LRowIndex] := Copy(AStringList[LRowIndex],2,Length(AStringList[LRowIndex]));
    end
    else
    begin
      for LRowIndex := 0 to RowCount - 1 do
        AStringList.Add(Rows[LRowIndex].CommaText);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractStringGrid.InsertColumn(AColIndex: Integer):integer;
const OPNAME = 'TAbstractStringGrid.InsertColumn';
var
  LRow,
  LCol   : Integer;
begin
  Result := NullInteger;
  try
    if(AColIndex < 0) then
      AColIndex := 0;
    if(AColIndex >= Self.ColCount) then
      AColIndex := Self.ColCount;

    Self.ColCount :=  Self.ColCount + 1 ;
    if AColIndex <= Self.ColCount  then
    begin
      for LRow := 0 to Self.RowCount -1 do
        for LCol := Self.ColCount downto AColIndex + 1  do
          Self.Cells[LCol,LRow]:= Self.Cells[LCol-1,LRow];

      for LRow := 0 to Self.RowCount -1 do
        Self.Cells[AColIndex,LRow] := '';
    end;
    Result := AColIndex;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractStringGrid.InsertRow(ARowIndex: Integer):integer;
const OPNAME = 'TAbstractStringGrid.InsertRow';
var
  LRow,
  LCol   : Integer;
begin
  Result := NullInteger;
  try
    if(ARowIndex < 0) then
      ARowIndex := 0;
    if(ARowIndex >= Self.RowCount) then
      ARowIndex := Self.RowCount;

    Self.RowCount := Self.RowCount + 1;
    if ARowIndex <= Self.RowCount  then
    begin
     for LRow := Self.RowCount  downto ARowIndex + 1  do
       for LCol := 0 to Self.ColCount -1 do
         Self.Cells[LCol,LRow]:= Self.Cells[LCol,LRow-1];

     for LCol := 0 to Self.ColCount -1 do
       Self.Cells[LCol,ARowIndex] := '';
    end;
    Result := ARowIndex;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractStringGrid.SetAlignment(Value: TAlignment);
const OPNAME = 'TAbstractStringGrid.SetAlignment';
begin
  try
    if FAlignment <> Value then
    begin
      FAlignment := Value;
      Invalidate;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractStringGrid.DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
const OPNAME = 'TAbstractStringGrid.DrawCell';
procedure WriteText(StringGrid: TStringGrid; ACanvas: TCanvas; const ARect: TRect;
    const Text: string; AAlignment: TAlignment);

const
    OPNAME = 'WriteText';
    DX = 2;

    DY = 2;

  var

    S: array[0..255] of Char;

  begin

    with Stringgrid, ACanvas, ARect do

    begin

      case AAlignment of

        taLeftJustify: ExtTextOut(Handle, Left + DX, Top + DY,

            //ETO_OPAQUE or ETO_CLIPPED, @ARect, StrPCopy(S, Text), Length(Text), nil);
            ETO_CLIPPED, @ARect, StrPCopy(S, Text), Length(Text), nil);



        taRightJustify: ExtTextOut(Handle, Right - TextWidth(Text) - 3, Top + DY,

            //ETO_OPAQUE or ETO_CLIPPED, @ARect, StrPCopy(S, Text),
            ETO_CLIPPED, @ARect, StrPCopy(S, Text),

            Length(Text),nil);



        taCenter: ExtTextOut(Handle, Left + (Right - Left - TextWidth(Text)) div 2,

            //Top + DY, ETO_OPAQUE or ETO_CLIPPED, @ARect,
            Top + DY, ETO_CLIPPED, @ARect,

            StrPCopy(S, Text), Length(Text), nil);

      end; 

    end; 

  end;

begin
  try
    if FAlignment <> taLeftJustify then
    begin
      WriteText(Self,Self.Canvas,ARect,Self.Cells[ACol, ARow],FAlignment);
    end
    else
     inherited;
  except on E: Exception do HandleError(E, OPNAME) end;

end;

{ TAbstractChart }

procedure TAbstractChart.AssignHelpContext;
const OPNAME = 'TAbstractChart.AssignHelpContext';
begin
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TAbstractChart.Create(AOwner: TComponent; AAppModules: TAppModules);
const OPNAME = 'TAbstractChart.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
    CreateMemberObjects;
    AssignHelpContext;
    Self.View3D               := False;
    Self.BackWall.Visible     := True;
    Self.BackWall.Color       := clWhite;
    Self.BackWall.Transparent := False;
    Self.Zoom.Pen.Color       := clInactiveCaption;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractChart.CreateMemberObjects;
const OPNAME = 'TAbstractChart.CreateMemberObjects';
begin
  try
   // Do not create anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TAbstractChart.Destroy;
const OPNAME = 'TAbstractChart.Destroy';
begin
  try
    DestroyMemberObjects;
    inherited Destroy;
    FAppModules := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractChart.DestroyMemberObjects;
const OPNAME = 'TAbstractChart.DestroyMemberObjects';
begin
  try
   // Do not free anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractChart.Initialise: boolean;
const OPNAME = 'TAbstractChart.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractChart.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractChart.LanguageHasChanged';
begin
  Result := True;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractChart.ResetState: boolean;
const OPNAME = 'TAbstractChart.ResetState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractChart.SaveState: boolean;
const OPNAME = 'TAbstractChart.SaveState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractChart.StudyHasChanged: boolean;
const OPNAME = 'TAbstractChart.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractChart.DoPrint;
const OPNAME = 'TAbstractChart.DoPrint';
var
  lPrinterOrientation : TPrinterOrientation;
  lBitmap             : vcl.Graphics.TBitmap;
  lX1                 : integer;
  lY1                 : integer;
  lX2                 : integer;
  lY2                 : integer;
  lAspectRatio        : Double;
  lColor              : TColor;
begin
  try
    if (Printer.Printers.Count > 0) then
    begin
      lBitmap :=  Vcl.Graphics.TBitmap.Create;
      try
        lColor         := Self.Color;
        Self.Color     := clWhite;
        lBitmap.Width  := Trunc(Self.Width);
        lBitmap.Height := Trunc(Self.Height);
        Self.PaintTo(lBitmap.Canvas,0,0);

        lPrinterOrientation := Printer.Orientation;
        try
          Printer.Orientation := poLandscape;

          lX1 := lBitmap.Width;
          lY1 := lBitMap.Height;
          lX2 := Printer.PageWidth;
          lY2 := Printer.PageHeight;

          if (lY1 <> 0) and (lY2 <> 0) then
          begin
            lAspectRatio := lX1 / lY1;
            if (lAspectRatio > (lX2 / lY2)) then
              lY2 := Trunc(lX2 / lAspectRatio)
            else
              lX2 := Trunc(lY2 * lAspectRatio);
          end
          else
          begin
            lX2 := 0;
            lY2 := 0;
          end;

          if (lX2 < Printer.PageWidth) then
          begin
            lX1 := ((Printer.PageWidth - lX2) div 2);
            lY1 := 0;
            lX2 := lX2 + ((Printer.PageWidth - lX2) div 2);
            lY2 := lY2;
          end;
          if (lY2 < Printer.PageHeight) then
          begin
            lX1 := 0;
            lY1:= ((Printer.PageHeight - lY2) div 2);
            lX2 := lX2;
            lY2 := lY2 + ((Printer.PageHeight - lY2) div 2);
          end;

          Printer.BeginDoc;
          Printer.Canvas.Stretchdraw(Rect(lX1, lY1 ,lX2 , lY2), lBitmap);
          Printer.EndDoc;
        finally
          Printer.Orientation := lPrinterOrientation;
          Self.Color          := lColor;
        end;
      finally
        FreeAndNil(lBitmap);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractChart.DoCopyToClipboard;
const OPNAME = 'TAbstractChart.DoCopyToClipboard';
var
  lBitmap : Vcl.Graphics.TBitmap;
  lColor  : TColor;
begin
  try
    lBitmap    := Vcl.Graphics.TBitmap.Create;
    lColor     := Self.Color;
    Self.Color := clWhite;
    try
      lBitmap.Width  := Trunc(Self.Width);
      lBitmap.Height := Trunc(Self.Height);
      Self.PaintTo(LBitmap.Canvas,0,0);
      Clipboard.Assign(lBitmap);
    finally
      FreeAndNil(lBitmap);
      Self.Color := lColor;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractChart.DoExport(AFileName: string = '');
const OPNAME = 'TAbstractChart.DoExport';
var
  lBitmap         : Vcl.Graphics.TBitmap;
  lExportFilename : string;
  lColor          : TColor;
begin
  try
    if(Trim(AFileName) <> '') then
    begin
      LExportFilename := AFileName;
      LExportFilename := ChangeFileExt(LExportFilename,'.bmp');
    end;

    lBitmap := Vcl.Graphics.TBitmap.Create;
    if (FAppModules.GetExportFilename
         ('.bmp', 'Bitmap files (*.bmp)|*.BMP', LExportFilename)) then
    begin
      lColor     := Self.Color;
      Self.Color := clWhite;
      try
        lBitmap.Width  := Trunc(Self.Width);
        lBitmap.Height := Trunc(Self.Height);
        Self.PaintTo(lBitmap.Canvas,0,0);
        lBitmap.SaveToFile(lExportFilename);
      finally
        FreeAndNil(lBitmap);
        Self.Color := lColor;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAbstractTreeView }

procedure TAbstractTreeView.AssignHelpContext;
const OPNAME = 'TAbstractTreeView.AssignHelpContext';
begin
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TAbstractTreeView.Create(AOwner: TComponent; AAppModules: TAppModules);
const OPNAME = 'TAbstractTreeView.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
    FAppImageList := TAppImageList.Create(self);
    FStringImageList := TAppImageList(FAppImageList);
    StateImages := TAppImageList(FAppImageList).ImageList;
    StateImages.Width  := 16;
    StateImages.Height := 16;
    HideSelection := False;
    CreateMemberObjects;
    AssignHelpContext;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractTreeView.CreateMemberObjects;
const OPNAME = 'TAbstractTreeView.CreateMemberObjects';
begin
  try
   // Do not create anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TAbstractTreeView.Destroy;
const OPNAME = 'TAbstractTreeView.Destroy';
begin
  try
    DestroyMemberObjects;
    inherited Destroy;
    FAppModules := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractTreeView.DestroyMemberObjects;
const OPNAME = 'TAbstractTreeView.DestroyMemberObjects';
begin
  try
   // Do not free anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractTreeView.Initialise: boolean;
const OPNAME = 'TAbstractTreeView.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractTreeView.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractTreeView.LanguageHasChanged';
begin
  Result := True;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractTreeView.ResetState: boolean;
const OPNAME = 'TAbstractTreeView.ResetState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractTreeView.SaveState: boolean;
const OPNAME = 'TAbstractTreeView.SaveState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractTreeView.StudyHasChanged: boolean;
const OPNAME = 'TAbstractTreeView.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAbstractScrollBox }

procedure TAbstractScrollBox.AssignHelpContext;
const OPNAME = 'TAbstractScrollBox.AssignHelpContext';
begin
  try
   // Do not assign anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TAbstractScrollBox.Create(AOwner: TComponent; AAppModules: TAppModules);
const OPNAME = 'TAbstractScrollBox.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
    CreateMemberObjects;
    AssignHelpContext;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractScrollBox.CreateMemberObjects;
const OPNAME = 'TAbstractScrollBox.CreateMemberObjects';
begin
  try
   // Do not create anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TAbstractScrollBox.Destroy;
const OPNAME = 'TAbstractScrollBox.Destroy';
begin
  try
    DestroyMemberObjects;
    inherited Destroy;
    FAppModules := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractScrollBox.DestroyMemberObjects;
const OPNAME = 'TAbstractScrollBox.DestroyMemberObjects';
begin
  try
   // Do not free anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractScrollBox.Initialise: boolean;
const OPNAME = 'TAbstractScrollBox.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractScrollBox.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractScrollBox.LanguageHasChanged';
begin
  Result := True;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractScrollBox.ResetState: boolean;
const OPNAME = 'TAbstractScrollBox.ResetState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractScrollBox.SaveState: boolean;
const OPNAME = 'TAbstractScrollBox.SaveState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractScrollBox.StudyHasChanged: boolean;
const OPNAME = 'TAbstractScrollBox.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


{ TAbstractPageControl }

procedure TAbstractPageControl.AssignHelpContext;
const OPNAME = 'TAbstractPageControl.AssignHelpContext';
begin
  try
   // Do not assign anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractPageControl.Change;
const OPNAME = 'TAbstractPageControl.Change';
begin
  try
    inherited Change;
    if (FAppModules.MainForm = nil) or (not FAppModules.MainForm.MainForm.Active) then
      Exit;
    if(FAppModules.MainForm.MainForm.ActiveControl <> nil) then
    begin
      FAppModules.MainForm.MainForm.ActiveControl.Perform(CM_EXIT,0,0)
      //FAppModules.MainForm.MainForm.ActiveControl.Perform(WM_CHAR,VK_TAB,0)
      //PostMessage(LParentForm.Handle,WM_SYSKEYDOWN,VK_TAB,0);
      //PostMessage(LParentForm.Handle,WM_SYSKEYUP,VK_TAB,0);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TAbstractPageControl.Create(AOwner: TComponent; AAppModules: TAppModules);
const OPNAME = 'TAbstractPageControl.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
    CreateMemberObjects;
    AssignHelpContext;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractPageControl.CreateMemberObjects;
const OPNAME = 'TAbstractPageControl.CreateMemberObjects';
begin
  try
   // Do not create anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TAbstractPageControl.Destroy;
const OPNAME = 'TAbstractPageControl.Destroy';
begin
  try
    DestroyMemberObjects;
    inherited Destroy;
    FAppModules := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractPageControl.DestroyMemberObjects;
const OPNAME = 'TAbstractPageControl.DestroyMemberObjects';
begin
  try
   // Do not free anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractPageControl.Initialise: boolean;
const OPNAME = 'TAbstractPageControl.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractPageControl.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractPageControl.LanguageHasChanged';
begin
  Result := True;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractPageControl.ResetState: boolean;
const OPNAME = 'TAbstractPageControl.ResetState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractPageControl.SaveState: boolean;
const OPNAME = 'TAbstractPageControl.SaveState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractPageControl.StudyHasChanged: boolean;
const OPNAME = 'TAbstractPageControl.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
{ TAbstractChart }

procedure TAbstractRichEdit.AssignHelpContext;
const OPNAME = 'TAbstractRichEdit.AssignHelpContext';
begin
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TAbstractRichEdit.Create(AOwner: TComponent; AAppModules: TAppModules);
const OPNAME = 'TAbstractRichEdit.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
    CreateMemberObjects;
    AssignHelpContext;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractRichEdit.CreateMemberObjects;
const OPNAME = 'TAbstractRichEdit.CreateMemberObjects';
begin
  try
   // Do not create anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TAbstractRichEdit.Destroy;
const OPNAME = 'TAbstractRichEdit.Destroy';
begin
  try
    DestroyMemberObjects;
    inherited Destroy;
    FAppModules := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractRichEdit.DestroyMemberObjects;
const OPNAME = 'TAbstractRichEdit.DestroyMemberObjects';
begin
  try
   // Do not free anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractRichEdit.Initialise: boolean;
const OPNAME = 'TAbstractRichEdit.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractRichEdit.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractRichEdit.LanguageHasChanged';
begin
  Result := True;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractRichEdit.ResetState: boolean;
const OPNAME = 'TAbstractRichEdit.ResetState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractRichEdit.SaveState: boolean;
const OPNAME = 'TAbstractRichEdit.SaveState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractRichEdit.StudyHasChanged: boolean;
const OPNAME = 'TAbstractRichEdit.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractChart.EditChartProperties;
const OPNAME = 'TAbstractChart.EditChartProperties';
//var
//  LChartEditor: TChartEditor;
begin
  try
//    LChartEditor := TChartEditor.Create(nil);
//    try
//      LChartEditor.Chart := Self;
//      LChartEditor.RememberPosition := True;
      //LChartEditor.TreeView := True;
//      LChartEditor.Execute;
//    finally
//      LChartEditor.Free;
//    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TAbstractChart.DblClick;
const OPNAME = 'TAbstractChart.DblClick';
begin
  inherited;
  EditChartProperties;
  Abort;
end;

{ TAbstractFieldEdit }

procedure TAbstractFieldEdit.AssignHelpContext;
const OPNAME = 'TAbstractFieldEdit.AssignHelpContext';
begin
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TAbstractFieldEdit.Create(AOwner: TComponent; AAppModules: TAppModules);
const OPNAME = 'TAbstractFieldEdit.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
    CreateMemberObjects;
    AssignHelpContext;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractFieldEdit.CreateMemberObjects;
const OPNAME = 'TAbstractFieldEdit.CreateMemberObjects';
begin
  try
   // Do not create anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TAbstractFieldEdit.Destroy;
const OPNAME = 'TAbstractFieldEdit.Destroy';
begin
  try
    DestroyMemberObjects;
    inherited Destroy;
    FAppModules := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractFieldEdit.DestroyMemberObjects;
const OPNAME = 'TAbstractFieldEdit.DestroyMemberObjects';
begin
  try
   // Do not free anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractFieldEdit.Initialise: boolean;
const OPNAME = 'TAbstractFieldEdit.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractFieldEdit.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractFieldEdit.LanguageHasChanged';
begin
  Result := True;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractFieldEdit.ResetState: boolean;
const OPNAME = 'TAbstractFieldEdit.ResetState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractFieldEdit.SaveState: boolean;
const OPNAME = 'TAbstractFieldEdit.SaveState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractFieldEdit.StudyHasChanged: boolean;
const OPNAME = 'TAbstractFieldEdit.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
{ TAbstractCheckBox }

procedure TAbstractCheckBox.AssignHelpContext;
const OPNAME = 'TAbstractCheckBox.AssignHelpContext';
begin
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TAbstractCheckBox.Create(AOwner: TComponent; AAppModules: TAppModules);
const OPNAME = 'TAbstractCheckBox.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
    CreateMemberObjects;
    AssignHelpContext;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractCheckBox.CreateMemberObjects;
const OPNAME = 'TAbstractCheckBox.CreateMemberObjects';
begin
  try
   // Do not create anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TAbstractCheckBox.Destroy;
const OPNAME = 'TAbstractCheckBox.Destroy';
begin
  try
    DestroyMemberObjects;
    inherited Destroy;
    FAppModules := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractCheckBox.DestroyMemberObjects;
const OPNAME = 'TAbstractCheckBox.DestroyMemberObjects';
begin
  try
   // Do not free anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractCheckBox.Initialise: boolean;
const OPNAME = 'TAbstractCheckBox.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractCheckBox.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractCheckBox.LanguageHasChanged';
begin
  Result := True;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractCheckBox.ResetState: boolean;
const OPNAME = 'TAbstractCheckBox.ResetState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractCheckBox.SaveState: boolean;
const OPNAME = 'TAbstractCheckBox.SaveState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractCheckBox.StudyHasChanged: boolean;
const OPNAME = 'TAbstractCheckBox.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAbstractRadioGroup }

procedure TAbstractRadioGroup.AssignHelpContext;
const OPNAME = 'TAbstractRadioGroup.AssignHelpContext';
begin
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TAbstractRadioGroup.Create(AOwner: TComponent; AAppModules: TAppModules);
const OPNAME = 'TAbstractRadioGroup.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
    CreateMemberObjects;
    AssignHelpContext;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractRadioGroup.CreateMemberObjects;
const OPNAME = 'TAbstractRadioGroup.CreateMemberObjects';
begin
  try
   // Do not create anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TAbstractRadioGroup.Destroy;
const OPNAME = 'TAbstractRadioGroup.Destroy';
begin
  try
    DestroyMemberObjects;
    inherited Destroy;
    FAppModules := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractRadioGroup.DestroyMemberObjects;
const OPNAME = 'TAbstractRadioGroup.DestroyMemberObjects';
begin
  try
   // Do not free anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractRadioGroup.Initialise: boolean;
const OPNAME = 'TAbstractRadioGroup.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractRadioGroup.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractRadioGroup.LanguageHasChanged';
begin
  Result := True;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractRadioGroup.ResetState: boolean;
const OPNAME = 'TAbstractRadioGroup.ResetState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractRadioGroup.SaveState: boolean;
const OPNAME = 'TAbstractRadioGroup.SaveState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractRadioGroup.StudyHasChanged: boolean;
const OPNAME = 'TAbstractRadioGroup.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAbstractDateTimePicker }

procedure TAbstractDateTimePicker.AssignHelpContext;
const OPNAME = 'TAbstractDateTimePicker.AssignHelpContext';
begin
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TAbstractDateTimePicker.Create(AOwner: TComponent; AAppModules: TAppModules);
const OPNAME = 'TAbstractDateTimePicker.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
    CreateMemberObjects;
    AssignHelpContext;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractDateTimePicker.CreateMemberObjects;
const OPNAME = 'TAbstractDateTimePicker.CreateMemberObjects';
begin
  try
   // Do not create anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TAbstractDateTimePicker.Destroy;
const OPNAME = 'TAbstractDateTimePicker.Destroy';
begin
  try
    DestroyMemberObjects;
    inherited Destroy;
    FAppModules := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractDateTimePicker.DestroyMemberObjects;
const OPNAME = 'TAbstractDateTimePicker.DestroyMemberObjects';
begin
  try
   // Do not free anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDateTimePicker.Initialise: boolean;
const OPNAME = 'TAbstractDateTimePicker.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDateTimePicker.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractDateTimePicker.LanguageHasChanged';
begin
  Result := True;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDateTimePicker.ResetState: boolean;
const OPNAME = 'TAbstractDateTimePicker.ResetState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDateTimePicker.SaveState: boolean;
const OPNAME = 'TAbstractDateTimePicker.SaveState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDateTimePicker.StudyHasChanged: boolean;
const OPNAME = 'TAbstractDateTimePicker.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAbstractFieldButton }

procedure TAbstractFieldButton.AssignHelpContext;
const OPNAME = 'TAbstractFieldButton.AssignHelpContext';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TAbstractFieldButton.Create(AOwner: TComponent; AAppModules: TAppModules; AButtonKey: string);
const OPNAME = 'TAbstractFieldButton.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
    FButtonKey := AButtonKey;
    AssignHelpContext;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractFieldButton.CreateMemberObjects;
const OPNAME = 'TAbstractFieldButton.CreateMemberObjects';
begin
  try
   // Do not create anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractFieldButton.DestroyMemberObjects;
const OPNAME = 'TAbstractFieldButton.DestroyMemberObjects';
begin
  try
   // Do not free anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractFieldButton.Initialise: boolean;
const OPNAME = 'TAbstractFieldButton.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractFieldButton.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractFieldButton.LanguageHasChanged';
begin
  Result := True;
  try

    // Set the hint as required.
    Hint := '';
    if Assigned(FAppModules.Language()) then
      Hint := FAppModules.Language.GetString('ButtonHint.' + FButtonKey);

    // Check if a status reason is required on the hint.
    if (FStatusReason <> '') and Assigned(FAppModules.Language()) then
    begin
      if (Hint <> '') then
      begin
        Hint := Hint + #13#10#13#10'  ' + FAppModules.Language.GetString('ButtonReason.' + FStatusReason);
      end else begin
        Hint := FAppModules.Language.GetString('ButtonReason.' + FStatusReason);
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractFieldButton.ResetState: boolean;
const OPNAME = 'TAbstractFieldButton.ResetState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractFieldButton.SaveState: boolean;
const OPNAME = 'TAbstractFieldButton.SaveState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractFieldButton.StudyHasChanged: boolean;
const OPNAME = 'TAbstractFieldButton.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAbstractComboBox }

procedure TAbstractComboBox.AssignHelpContext;
const OPNAME = 'TAbstractComboBox.AssignHelpContext';
begin
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TAbstractComboBox.Create(AOwner: TComponent; AAppModules: TAppModules);
const OPNAME = 'TAbstractComboBox.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
    CreateMemberObjects;
    AssignHelpContext;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractComboBox.CreateMemberObjects;
const OPNAME = 'TAbstractComboBox.CreateMemberObjects';
begin
  try
   // Do not create anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TAbstractComboBox.Destroy;
const OPNAME = 'TAbstractComboBox.Destroy';
begin
  try
    DestroyMemberObjects;
    inherited Destroy;
    FAppModules := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractComboBox.DestroyMemberObjects;
const OPNAME = 'TAbstractComboBox.DestroyMemberObjects';
begin
  try
   // Do not free anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractComboBox.Initialise: boolean;
const OPNAME = 'TAbstractComboBox.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractComboBox.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractComboBox.LanguageHasChanged';
begin
  Result := True;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractComboBox.ResetState: boolean;
const OPNAME = 'TAbstractComboBox.ResetState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractComboBox.SaveState: boolean;
const OPNAME = 'TAbstractComboBox.SaveState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractComboBox.StudyHasChanged: boolean;
const OPNAME = 'TAbstractComboBox.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAbstractFieldBitBtn }

procedure TAbstractFieldBitBtn.AssignHelpContext;
const OPNAME = 'TAbstractFieldBitBtn.AssignHelpContext';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TAbstractFieldBitBtn.Create(AOwner: TComponent; AAppModules: TAppModules);
const OPNAME = 'TAbstractFieldBitBtn.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
    AssignHelpContext;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractFieldBitBtn.CreateMemberObjects;
const OPNAME = 'TAbstractFieldBitBtn.CreateMemberObjects';
begin
  try
   // Do not create anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractFieldBitBtn.DestroyMemberObjects;
const OPNAME = 'TAbstractFieldBitBtn.DestroyMemberObjects';
begin
  try
   // Do not free anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractFieldBitBtn.Initialise: boolean;
const OPNAME = 'TAbstractFieldBitBtn.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractFieldBitBtn.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractFieldBitBtn.LanguageHasChanged';
begin
  Result := True;
  try
     Result := True;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractFieldBitBtn.ResetState: boolean;
const OPNAME = 'TAbstractFieldBitBtn.ResetState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractFieldBitBtn.SaveState: boolean;
const OPNAME = 'TAbstractFieldBitBtn.SaveState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractFieldBitBtn.StudyHasChanged: boolean;
const OPNAME = 'TAbstractFieldBitBtn.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAbstractFieldListBox }


procedure TAbstractFieldListBox.AssignHelpContext;
const OPNAME = 'TAbstractFieldListBox.AssignHelpContext';
begin
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TAbstractFieldListBox.Create(AOwner: TComponent; AAppModules: TAppModules);
const OPNAME = 'TAbstractFieldListBox.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
    CreateMemberObjects;
    AssignHelpContext;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractFieldListBox.CreateMemberObjects;
const OPNAME = 'TAbstractFieldListBox.CreateMemberObjects';
begin
  try
   // Do not create anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TAbstractFieldListBox.Destroy;
const OPNAME = 'TAbstractFieldListBox.Destroy';
begin
  try
    DestroyMemberObjects;
    inherited Destroy;
    FAppModules := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractFieldListBox.DestroyMemberObjects;
const OPNAME = 'TAbstractFieldListBox.DestroyMemberObjects';
begin
  try
   // Do not free anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractFieldListBox.Initialise: boolean;
const OPNAME = 'TAbstractFieldListBox.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractFieldListBox.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractFieldListBox.LanguageHasChanged';
begin
  Result := True;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractFieldListBox.ResetState: boolean;
const OPNAME = 'TAbstractFieldListBox.ResetState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractFieldListBox.SaveState: boolean;
const OPNAME = 'TAbstractFieldListBox.SaveState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractFieldListBox.StudyHasChanged: boolean;
const OPNAME = 'TAbstractFieldListBox.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


{ TAbstractCheckListBox }

procedure TAbstractCheckListBox.AssignHelpContext;
const OPNAME = 'TAbstractCheckListBox.AssignHelpContext';
begin
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TAbstractCheckListBox.Create(AOwner: TComponent; AAppModules: TAppModules);
const OPNAME = 'TAbstractCheckListBox.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
    CreateMemberObjects;
    AssignHelpContext;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractCheckListBox.CreateMemberObjects;
const OPNAME = 'TAbstractCheckListBox.CreateMemberObjects';
begin
  try
   // Do not create anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TAbstractCheckListBox.Destroy;
const OPNAME = 'TAbstractCheckListBox.Destroy';
begin
  try
    DestroyMemberObjects;
    inherited Destroy;
    FAppModules := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractCheckListBox.DestroyMemberObjects;
const OPNAME = 'TAbstractCheckListBox.DestroyMemberObjects';
begin
  try
   // Do not free anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractCheckListBox.Initialise: boolean;
const OPNAME = 'TAbstractCheckListBox.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractCheckListBox.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractCheckListBox.LanguageHasChanged';
begin
  Result := True;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractCheckListBox.ResetState: boolean;
const OPNAME = 'TAbstractCheckListBox.ResetState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractCheckListBox.SaveState: boolean;
const OPNAME = 'TAbstractCheckListBox.SaveState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractCheckListBox.StudyHasChanged: boolean;
const OPNAME = 'TAbstractCheckListBox.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAbstractRadioButton }

procedure TAbstractRadioButton.AssignHelpContext;
const OPNAME = 'TAbstractRadioButton.AssignHelpContext';
begin
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TAbstractRadioButton.Create(AOwner: TComponent; AAppModules: TAppModules);
const OPNAME = 'TAbstractRadioButton.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
    CreateMemberObjects;
    AssignHelpContext;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractRadioButton.CreateMemberObjects;
const OPNAME = 'TAbstractRadioButton.CreateMemberObjects';
begin
  try
   // Do not create anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TAbstractRadioButton.Destroy;
const OPNAME = 'TAbstractRadioButton.Destroy';
begin
  try
    DestroyMemberObjects;
    inherited Destroy;
    FAppModules := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractRadioButton.DestroyMemberObjects;
const OPNAME = 'TAbstractRadioButton.DestroyMemberObjects';
begin
  try
   // Do not free anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractRadioButton.Initialise: boolean;
const OPNAME = 'TAbstractRadioButton.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractRadioButton.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractRadioButton.LanguageHasChanged';
begin
  Result := True;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractRadioButton.ResetState: boolean;
const OPNAME = 'TAbstractRadioButton.ResetState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractRadioButton.SaveState: boolean;
const OPNAME = 'TAbstractRadioButton.SaveState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractRadioButton.StudyHasChanged: boolean;
const OPNAME = 'TAbstractRadioButton.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
{ TAbstractStringGridWithEditor }
// Added by JDT 2006-10-16

procedure TAbstractStringGridWithEditor.AssignHelpContext;
const OPNAME = 'TAbstractStringGridWithEditor.AssignHelpContext';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TAbstractStringGridWithEditor.Create(AOwner: TComponent; AAppModules: TAppModules);
const OPNAME = 'TAbstractStringGridWithEditor.Create';
begin
  inherited Create(AOwner,AAppModules);
  try
     FEditcomboBox := TAbstractCombobox.Create(AOwner,FAppModules);
     FOldRow       := 0;
     FOldCol       := 0;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractStringGridWithEditor.CreateMemberObjects;
const OPNAME = 'TAbstractStringGridWithEditor.CreateMemberObjects';
begin
  try
   // Do not create anything here as inherited is never called.
  except on E: Exception do HandleError(E, OPNAME) end;
end;
 {
destructor TAbstractStringGridWithEditor.Destroy;
const OPNAME = 'TAbstractStringGridWithEditor.Destroy';
begin
  try
//    DestroyMemberObjects;
    inherited Destroy;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
}
procedure TAbstractStringGridWithEditor.DestroyMemberObjects;
const OPNAME = 'TAbstractStringGridWithEditor.DestroyMemberObjects';
begin
  try
{    If Assigned(FEditcomboBox) then
      FreeAndNil(FEditcomboBox);}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractStringGridWithEditor.Initialise: boolean;
const OPNAME = 'TAbstractStringGridWithEditor.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractStringGridWithEditor.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractStringGridWithEditor.LanguageHasChanged';
begin
  Result := True;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractStringGridWithEditor.ResetState: boolean;
const OPNAME = 'TAbstractStringGridWithEditor.ResetState';
begin
  Result := False;
  try
    Result := inherited  ResetState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractStringGridWithEditor.SaveState: boolean;
const OPNAME = 'TAbstractStringGridWithEditor.SaveState';
begin
  Result := False;
  try
    Result := inherited SaveState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractStringGridWithEditor.StudyHasChanged: boolean;
const OPNAME = 'TAbstractStringGridWithEditor.StudyHasChanged';
begin
  Result := False;
  try
    inherited StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractStringGridWithEditor.DoPrint(Title: string);
const OPNAME = 'TAbstractStringGridWithEditor.DoPrint';
begin
 Try
    inherited;
 except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractStringGridWithEditor.DoCopyToClipboard;
const OPNAME = 'TAbstractStringGridWithEditor.DoCopyToClipboard';
begin
  try
    inherited;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAbstractFrameHandle }

constructor TAbstractFrameHandle.Create(AOwner: TComponent; AParentWnd: HWND; AAppModules: TAppModules);
const OPNAME = 'TAbstractFrame.CreateMemberObjects';
begin
  try
    inherited Create(AOwner,AParentWnd, AAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractFrameHandle.CreateMemberObjects;
const OPNAME = 'TAbstractFrame.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractFrameHandle.CreateParams(var Params: TCreateParams);
const OPNAME = 'TAbstractFrame.CreateParams';
begin
  try
    inherited CreateParams(Params);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractFrameHandle.DestroyMemberObjects;
const OPNAME = 'TAbstractFrame.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractFrameHandle.Initialise: boolean;
const OPNAME = 'TAbstractFrame.Initialise';
begin
  Result := False;
  try
    Result := inherited Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TAbstractFrameHandle.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractFrame.LanguageHasChanged';
begin
  Result := False;
  try
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractFrameHandle.OnHide;
const OPNAME = 'TAbstractFrame.OnHide';
begin
  try
    inherited;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractFrameHandle.OnShow;
const OPNAME = 'TAbstractFrame.OnShow';
begin
  try
    inherited;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractFrameHandle.ResetState: boolean;
const OPNAME = 'TAbstractFrame.ResetState';
begin
  Result := False;
  try
    Result := inherited ResetState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TAbstractFrameHandle.SaveState: boolean;
const OPNAME = 'TAbstractFrame.SaveState';
begin
  Result := False;
  try
    Result := inherited SaveState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractFrameHandle.StudyHasChanged: boolean;
const OPNAME = 'TAbstractFrame.StudyHasChanged';
begin
  Result := False;
  try
    Result := inherited StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
