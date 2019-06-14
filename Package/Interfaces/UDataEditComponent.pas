//
//
//  UNIT      : Contains TAbstractComponent Classes
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 24/01/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//

unit UDataEditComponent;

interface

uses
  Classes,
  VCLTee.Chart,
  Vcl.Grids,
  Vcl.Menus,
  Vcl.Graphics,
  Vcl.ComCtrls,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.CheckLst,

  PsAPI,
  Windows,
  Contnrs,
  Messages,

  UAbstractComponent,
  UStringGridWithCellChange,
  UAbstractObject;

type
  TGeneralEditIdentifier = (giUnknown,giReservoirName, giDrainageScale, giAfforestationScale, giIrrigationScale,
                            giCatchmentRef, giIncludeSummary, giFullSupplyLevel, giDeadStorageLevel, giBottomOfReservoir,
                            giUpNodeNumber);

  TFieldChkBox = class(TAbstractCheckBox)
  protected
    FXPos,FYPos        : integer;
    FPrevValue         : boolean;
    FChkBoxIdentifier  : TGeneralEditIdentifier;
    FFieldProperty     : TAbstractFieldProperty;
    FIsEnabled         : boolean;
    FValidationError   : String;
    FInValidationError : boolean;
    FHasChanges        : boolean;
    FHasMetaData       : boolean;

    procedure CreateMemberObjects; override;

    procedure DoExit; override;
    procedure DoEnter; override;
    procedure Toggle; override;
    procedure Click; override;

    procedure SetIsEnabled(AIsEnabled: boolean);
    procedure SetFieldProperty(AFieldProperty : TAbstractFieldProperty);
    procedure SetHasChanges (AHasChanges : boolean);
    procedure SetHasMetaData (AHasMetaData : boolean);

    procedure WMPAINT(var Message: TMessage); message WM_PAINT;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure PaintChangesAndMetaData;
    function HotSpotChangesClicked(X, Y: Integer): boolean;
    function HotSpotMetaDataClicked(X, Y: Integer): boolean;
  public
    function HasValueChanged: boolean;
    procedure ShowErrorState (AError : Boolean);
    property ValidationError : String read FValidationError write FValidationError;
    property InValidationError : boolean read FInValidationError write FInValidationError;
    property FieldProperty: TAbstractFieldProperty read FFieldProperty write SetFieldProperty;
    Property IsEnabled : boolean read FIsEnabled write SetIsEnabled;
    property CheckBoxIdentifier: TGeneralEditIdentifier read FChkBoxIdentifier write FChkBoxIdentifier;
    property HasChanges   : boolean read FHasChanges write SetHasChanges;
    property HasMetaData   : boolean read FHasMetaData write SetHasMetaData;
  end;

  TFieldCheckListBox = class(TAbstractCheckListBox)
  protected
    FFieldProperty     : TAbstractFieldProperty;
    FIsEnabled         : boolean;
    FOldIndex          : string;
    FValidationError   : String;
    FInValidationError : boolean;
    FHasChanges        : boolean;
    FHasMetaData       : boolean;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure DoExit; override;
    procedure DoEnter; override;
    procedure Click; override;

    procedure SetIsEnabled(AIsEnabled: boolean);
    procedure SetFieldProperty(AFieldProperty : TAbstractFieldProperty);
    procedure SetHasChanges (AHasChanges : boolean);
    procedure SetHasMetaData (AHasMetaData : boolean);

    procedure WMPAINT(var Message: TMessage); message WM_PAINT;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PaintChangesAndMetaData;
    function HotSpotChangesClicked(X, Y: Integer): boolean;
    function HotSpotMetaDataClicked(X, Y: Integer): boolean;
  public
    function HasValueChanged: boolean;
    procedure ShowErrorState (AError : Boolean);
    property ValidationError : String read FValidationError write FValidationError;
    property InValidationError : boolean read FInValidationError write FInValidationError;
    property FieldProperty: TAbstractFieldProperty read FFieldProperty write SetFieldProperty;
    Property IsEnabled : boolean read FIsEnabled write SetIsEnabled;
    property HasChanges   : boolean read FHasChanges write SetHasChanges;
    property HasMetaData   : boolean read FHasMetaData write SetHasMetaData;
  end;

  TFieldRadioGroup = class(TAbstractRadioGroup)
  protected
    FOldIndex          : integer;
    FFieldProperty     : TAbstractFieldProperty;
    FIsEnabled         : boolean;
    FValidationError   : String;
    FInValidationError : boolean;
    FHints             : TStringList;
    FHasChanges        : boolean;
    FHasMetaData       : boolean;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure DoExit; override;
    procedure DoEnter; override;
    procedure Click; override;

    function CanModify: Boolean; override;

    procedure SetIsEnabled(AIsEnabled: boolean);
    procedure SetFieldProperty(AFieldProperty : TAbstractFieldProperty);
    procedure SetHasChanges (AHasChanges : boolean);
    procedure SetHasMetaData (AHasMetaData : boolean);

    procedure WMPAINT(var Message: TMessage); message WM_PAINT;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PaintChangesAndMetaData;
    function HotSpotChangesClicked(X, Y: Integer): boolean;
    function HotSpotMetaDataClicked(X, Y: Integer): boolean;
  public
    function HasValueChanged: boolean;
    procedure ShowErrorState (AError : Boolean);
    property ValidationError : String read FValidationError write FValidationError;
    property InValidationError : boolean read FInValidationError write FInValidationError;
    property FieldProperty: TAbstractFieldProperty read FFieldProperty write SetFieldProperty;
    Property IsEnabled : boolean read FIsEnabled write SetIsEnabled;
    property Hints : TStringList read FHints;
    property HasChanges   : boolean read FHasChanges write SetHasChanges;
    property HasMetaData   : boolean read FHasMetaData write SetHasMetaData;
  end;

  TFieldRadioButton = class(TAbstractRadioButton)
  protected
    FPrevValue         : boolean;
    FFieldProperty     : TAbstractFieldProperty;
    FIsEnabled         : Boolean;
    FValidationError   : String;
    FInValidationError : boolean;
    FHasChanges        : boolean;
    FHasMetaData       : boolean;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure DoExit; override;
    procedure DoEnter; override;

    procedure SetIsEnabled(AIsEnabled: boolean);
    procedure SetFieldProperty(AFieldProperty : TAbstractFieldProperty);
    procedure SetHasChanges (AHasChanges : boolean);
    procedure SetHasMetaData (AHasMetaData : boolean);

    procedure WMPAINT(var Message: TMessage); message WM_PAINT;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PaintChangesAndMetaData;
    function HotSpotChangesClicked(X, Y: Integer): boolean;
    function HotSpotMetaDataClicked(X, Y: Integer): boolean;
  public
    function HasValueChanged: boolean;
    procedure ShowErrorState (AError : Boolean);
    property ValidationError : String read FValidationError write FValidationError;
    property InValidationError : boolean read FInValidationError write FInValidationError;
    property FieldProperty: TAbstractFieldProperty read FFieldProperty write SetFieldProperty;
    Property IsEnabled : boolean read FIsEnabled write SetIsEnabled;
    property HasChanges   : boolean read FHasChanges write SetHasChanges;
    property HasMetaData   : boolean read FHasMetaData write SetHasMetaData;
  end;

  TFieldDateTimePicker = class(TAbstractDateTimePicker)
  protected
    FPrevValue         : TDateTime;
    FFieldProperty     : TAbstractFieldProperty;
    FIsEnabled         : Boolean;
    FValidationError   : String;
    FInValidationError : boolean;
    FHasChanges        : boolean;
    FHasMetaData       : boolean;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure DoExit; override;
    procedure DoEnter; override;

    procedure SetIsEnabled(AIsEnabled: boolean);
    procedure SetFieldProperty(AFieldProperty : TAbstractFieldProperty);
    procedure SetHasChanges (AHasChanges : boolean);
    procedure SetHasMetaData (AHasMetaData : boolean);

    procedure WMPAINT(var Message: TMessage); message WM_PAINT;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PaintChangesAndMetaData;
    function HotSpotChangesClicked(X, Y: Integer): boolean;
    function HotSpotMetaDataClicked(X, Y: Integer): boolean;
  public
    function HasValueChanged: boolean;
    procedure ShowErrorState (AError : Boolean);
    property ValidationError : String read FValidationError write FValidationError;
    property InValidationError : boolean read FInValidationError write FInValidationError;
    property FieldProperty: TAbstractFieldProperty read FFieldProperty write SetFieldProperty;
    Property IsEnabled : boolean read FIsEnabled write SetIsEnabled;
    property HasChanges   : boolean read FHasChanges write SetHasChanges;
    property HasMetaData   : boolean read FHasMetaData write SetHasMetaData;
  end;

  TFieldTreeView = class(TAbstractTreeView)
  protected
    FPrevValue         : boolean;
    FFieldProperty     : TAbstractFieldProperty;
    FIsEnabled         : Boolean;
    FValidationError   : String;
    FInValidationError : boolean;
    FHasChanges        : boolean;
    FHasMetaData       : boolean;
    FDisabledColor     : TColor;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure DoExit; override;
    procedure DoEnter; override;

    procedure SetIsEnabled(AIsEnabled: boolean);
    procedure SetFieldProperty(AFieldProperty : TAbstractFieldProperty);
    procedure SetHasChanges (AHasChanges : boolean);
    procedure SetHasMetaData (AHasMetaData : boolean);

    procedure WMPAINT(var Message: TMessage); message WM_PAINT;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PaintChangesAndMetaData;
    function HotSpotChangesClicked(X, Y: Integer): boolean;
    function HotSpotMetaDataClicked(X, Y: Integer): boolean;
  public
    function HasValueChanged: boolean;
    procedure ShowErrorState (AError : Boolean);
    property DisabledColor: TColor read FDisabledColor write FDisabledColor;
    property ValidationError : String read FValidationError write FValidationError;
    property InValidationError : boolean read FInValidationError write FInValidationError;
    property FieldProperty: TAbstractFieldProperty read FFieldProperty write SetFieldProperty;
    Property IsEnabled : boolean read FIsEnabled write SetIsEnabled;
    property HasChanges   : boolean read FHasChanges write SetHasChanges;
    property HasMetaData   : boolean read FHasMetaData write SetHasMetaData;
  end;

  TFieldButton = class(TAbstractFieldButton)
  protected
    FPrevValue         : boolean;
    FFieldProperty     : TAbstractFieldProperty;
    FIsEnabled         : Boolean;
    FValidationError   : String;
    FInValidationError : boolean;
    FHasChanges        : boolean;
    FHasMetaData       : boolean;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure DoExit; override;
    procedure DoEnter; override;

    procedure SetIsEnabled(AIsEnabled: boolean);
    procedure SetFieldProperty(AFieldProperty : TAbstractFieldProperty);
    procedure SetHasChanges (AHasChanges : boolean);
    procedure SetHasMetaData (AHasMetaData : boolean);

    procedure WMPAINT(var Message: TMessage); message WM_PAINT;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PaintChangesAndMetaData;
    function HotSpotChangesClicked(X, Y: Integer): boolean;
    function HotSpotMetaDataClicked(X, Y: Integer): boolean;
  public
    function HasValueChanged: boolean;
    procedure ShowErrorState (AError : Boolean);
    property ValidationError : String read FValidationError write FValidationError;
    property InValidationError : boolean read FInValidationError write FInValidationError;
    property FieldProperty: TAbstractFieldProperty read FFieldProperty write SetFieldProperty;
    Property IsEnabled : boolean read FIsEnabled write SetIsEnabled;
    property HasChanges   : boolean read FHasChanges write SetHasChanges;
    property HasMetaData   : boolean read FHasMetaData write SetHasMetaData;
  end;

  TFieldComboBox = class(TAbstractComboBox)
  protected
    FFieldProperty     : TAbstractFieldProperty;
    FCmbBoxIdentifier  : TGeneralEditIdentifier;
    FIsEnabled         : Boolean;
    FOldIndex          : integer;
    FValidationError   : String;
    FInValidationError : boolean;
    FHasChanges        : boolean;
    FHasMetaData       : boolean;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure DoExit; override;
    procedure DoEnter; override;

    procedure SetHasChanges (AHasChanges : boolean);
    procedure SetHasMetaData (AHasMetaData : boolean);
    procedure SetIsEnabled(AIsEnabled: boolean);
    procedure SetFieldProperty(AFieldProperty : TAbstractFieldProperty);

    procedure WMPAINT(var Message: TMessage); message WM_PAINT;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PaintChangesAndMetaData;
    function HotSpotChangesClicked(X, Y: Integer): boolean;
    function HotSpotMetaDataClicked(X, Y: Integer): boolean;
    procedure Change; override;
  public

    function Initialise: boolean; override;
    function HasValueChanged: boolean;
    procedure ShowErrorState (AError : Boolean);
    procedure SetFieldIndex(AIndex: integer);
    property FieldProperty: TAbstractFieldProperty read FFieldProperty write SetFieldProperty;
    property CmbBoxIdentifier: TGeneralEditIdentifier read FCmbBoxIdentifier write FCmbBoxIdentifier;
    property PreviousIndex: integer read FOldIndex;
    property ValidationError : String read FValidationError write FValidationError;
    property InValidationError : boolean read FInValidationError write FInValidationError;
    Property IsEnabled : boolean read FIsEnabled write SetIsEnabled;
    property HasChanges   : boolean read FHasChanges write SetHasChanges;
    property HasMetaData   : boolean read FHasMetaData write SetHasMetaData;
  end;

  TFieldBitBtn = class(TAbstractFieldBitBtn)
  protected
    FPrevValue         : boolean;
    FFieldProperty     : TAbstractFieldProperty;
    FIsEnabled         : Boolean;
    FValidationError   : String;
    FInValidationError : boolean;
    FHasChanges        : boolean;
    FHasMetaData       : boolean;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure DoExit; override;
    procedure DoEnter; override;

    procedure SetIsEnabled(AIsEnabled: boolean);
    procedure SetFieldProperty(AFieldProperty : TAbstractFieldProperty);
    procedure SetHasChanges (AHasChanges : boolean);
    procedure SetHasMetaData (AHasMetaData : boolean);

    procedure WMPAINT(var Message: TMessage); message WM_PAINT;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PaintChangesAndMetaData;
    function HotSpotChangesClicked(X, Y: Integer): boolean;
    function HotSpotMetaDataClicked(X, Y: Integer): boolean;
  public
    function HasValueChanged: boolean;
    procedure ShowErrorState (AError : Boolean);
    property ValidationError : String read FValidationError write FValidationError;
    property InValidationError : boolean read FInValidationError write FInValidationError;
    property FieldProperty: TAbstractFieldProperty read FFieldProperty write SetFieldProperty;
    Property IsEnabled : boolean read FIsEnabled write SetIsEnabled;
    property HasChanges   : boolean read FHasChanges write SetHasChanges;
    property HasMetaData   : boolean read FHasMetaData write SetHasMetaData;
  end;

  TFieldListBox = class(TAbstractFieldListBox)
  protected
    FFieldProperty     : TAbstractFieldProperty;
    FIsEnabled         : Boolean;
    FOldIndex          : string;
    FValidationError   : String;
    FInValidationError : boolean;
    FHasChanges        : boolean;
    FHasMetaData       : boolean;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure DoExit; override;
    procedure DoEnter; override;

    procedure SetIsEnabled(AIsEnabled: boolean);
    procedure SetFieldProperty(AFieldProperty : TAbstractFieldProperty);
    procedure SetHasChanges (AHasChanges : boolean);
    procedure SetHasMetaData (AHasMetaData : boolean);

    procedure WMPAINT(var Message: TMessage); message WM_PAINT;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PaintChangesAndMetaData;
    function HotSpotChangesClicked(X, Y: Integer): boolean;
    function HotSpotMetaDataClicked(X, Y: Integer): boolean;
  public
    function HasValueChanged: boolean;
    procedure ShowErrorState (AError : Boolean);
    property ValidationError : String read FValidationError write FValidationError;
    property InValidationError : boolean read FInValidationError write FInValidationError;
    property FieldProperty: TAbstractFieldProperty read FFieldProperty write FFieldProperty;
    Property IsEnabled : boolean read FIsEnabled write SetIsEnabled;
    property HasChanges   : boolean read FHasChanges write SetHasChanges;
    property HasMetaData   : boolean read FHasMetaData write SetHasMetaData;
  end;

  TFieldEdit = class(TAbstractFieldEdit)
  protected
    FEditIdentifier         : TGeneralEditIdentifier;
    FFieldProperty          : TAbstractFieldProperty;
    FIsEnabled              : Boolean;
    FOldValue               : string;
    FFieldValidationError   : string;
    FContextValidationError : string;
    FHasChanges             : boolean;
    FHasMetaData            : boolean;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure DoEnter; override;
    procedure DoExit; override;
    procedure ShowErrorState;

    procedure SetHasChanges (AHasChanges : boolean);
    procedure SetHasMetaData (AHasMetaData : boolean);
    procedure SetFieldProperty(AFieldProperty : TAbstractFieldProperty);
    procedure SetFieldValidationError(AErrorMesg : String);
    procedure SetContextValidationError(AErrorMesg : String);
    procedure SetIsEnabled(AIsEnabled: boolean);
    function GetIsEnabled: boolean;

    procedure KeyPress(var AKey: Char); override;
    procedure WMPAINT(var Message: TMessage); message WM_PAINT;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PaintChangesAndMetaData;
    function HotSpotChangesClicked(X, Y: Integer): boolean;
    function HotSpotMetaDataClicked(X, Y: Integer): boolean;
  public
    procedure SetFieldValue(AValue: string);  overload;
    procedure SetFieldValue(AValue: integer);  overload;
    procedure SetFieldValue(AValue: double);  overload;
    procedure Reset;
    function HasValueChanged: boolean;
    property EditIdentifier: TGeneralEditIdentifier read FEditIdentifier write FEditIdentifier;
    property PreviousValue: string read FOldValue;
    property FieldValidationError : String read FFieldValidationError write SetFieldValidationError;
    property ContextValidationError : String read FContextValidationError write SetContextValidationError;
    property IsEnabled : boolean read GetIsEnabled write SetIsEnabled;
    property FieldProperty: TAbstractFieldProperty read FFieldProperty write SetFieldProperty;
    property HasChanges   : boolean read FHasChanges write SetHasChanges;
    property HasMetaData   : boolean read FHasMetaData write SetHasMetaData;
  end;

  TFieldChart = class(TAbstractChart)
  protected
    FFieldProperty: TAbstractFieldProperty;
    FHasMetaData            : boolean;
    procedure CreateMemberObjects; override;
    procedure SetHasMetaData (AHasMetaData : boolean);
    procedure SetFieldProperty(AFieldProperty : TAbstractFieldProperty);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure OnCompletedDrawing(Sender: TObject);
    function HotSpotMetaDataClicked(X, Y: Single): boolean;
    procedure PaintMetaData;
  public
    property FieldProperty: TAbstractFieldProperty read FFieldProperty write SetFieldProperty;
    property HasMetaData   : boolean read FHasMetaData write SetHasMetaData;
  end;


  TFieldRichEdit = class(TAbstractRichEdit)
  protected
    FFieldProperty          : TAbstractFieldProperty;
    FIsEnabled              : Boolean;
    FOldValue               : string;
    FFieldValidationError   : string;
    FContextValidationError : string;
    FHasChanges             : boolean;
    FHasMetaData            : boolean;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure DoEnter; override;
    procedure DoExit; override;
    procedure ShowErrorState;

    procedure SetHasChanges (AHasChanges : boolean);
    procedure SetHasMetaData (AHasMetaData : boolean);
    procedure SetFieldProperty(AFieldProperty : TAbstractFieldProperty);
    procedure SetFieldValidationError(AErrorMesg : String);
    procedure SetContextValidationError(AErrorMesg : String);
    procedure SetIsEnabled(AIsEnabled: boolean);
    function GetIsEnabled: boolean;

    procedure KeyPress(var AKey: Char); override;
    procedure WMPAINT(var Message: TMessage); message WM_PAINT;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PaintChangesAndMetaData;
    function HotSpotChangesClicked(X, Y: Integer): boolean;
    function HotSpotMetaDataClicked(X, Y: Integer): boolean;
 public
    procedure SetFieldValue(AValue: string);
    procedure Reset;
    function HasValueChanged: boolean;
    property PreviousValue: string read FOldValue;
    property FieldValidationError : String read FFieldValidationError write SetFieldValidationError;
    property ContextValidationError : String read FContextValidationError write SetContextValidationError;
    property IsEnabled : boolean read GetIsEnabled write SetIsEnabled;
    property FieldProperty: TAbstractFieldProperty read FFieldProperty write SetFieldProperty;
    property HasChanges   : boolean read FHasChanges write SetHasChanges;
    property HasMetaData   : boolean read FHasMetaData write SetHasMetaData;
  end;

  TOnColEnterEvent = procedure (Sender: TObject;ACol, ARow: integer) of object;
  TGridErrorType = (gveCellField, gveColContext, gveCellContext);

  TFieldStringGridPopupMenu = class(TPopupMenu)
  protected
    FMenuItemUndo: TMenuItem;
    FMenuItemCut: TMenuItem;
    FMenuItemCopy: TMenuItem;
    FMenuItemPaste: TMenuItem;
    FMenuItemDelete: TMenuItem;
    FmnuSelectAll: TMenuItem;
    FMenuItemPastefromExcel: TMenuItem;
    FMenuItemCopyColumn: TMenuItem;
    FMenuItemPasteColumn: TMenuItem;
    FMenuItemCopyColumnsAndRows: TMenuItem;
    FMenuItemPasteColumnsAndRows: TMenuItem;
  public
    constructor Create(AOwner: TComponent); override;
    property MenuItemUndo: TMenuItem read FMenuItemUndo;
    property MenuItemCut: TMenuItem read FMenuItemCut;
    property MenuItemCopy: TMenuItem read FMenuItemCopy;
    property MenuItemPaste: TMenuItem read FMenuItemPaste;
    property MenuItemDelete: TMenuItem read FMenuItemDelete;
    property mnuSelectAll: TMenuItem read FmnuSelectAll;
    property MenuItemPastefromExcel: TMenuItem read FMenuItemPastefromExcel;
    property MenuItemCopyColumn: TMenuItem read FMenuItemCopyColumn;
    property MenuItemPasteColumn: TMenuItem read FMenuItemPasteColumn;
    property MenuItemCopyColumnsAndRows: TMenuItem read FMenuItemCopyColumnsAndRows;
    property MenuItemPasteColumnsAndRows: TMenuItem read FMenuItemPasteColumnsAndRows;
  end;

  TFieldStringGrid = class(TStringGridWithCellChange)
  protected
    FOnColEnterEvent      : TOnColEnterEvent;
    FFieldPropertyList    : TObjectList;
    FOldValue             : string;
    FCurrenCell           : TGridCoord;
    FHasValueChanged      : boolean;
    FRowIsEnabled         : TStringList;
    FColIsEnabled         : TStringList;
    FCellContextError     : TStringList;
    FColContextError      : TStringList;
    FCellFieldError       : TStringList;
    FHasChanges           : TStringList;
    FHasMetaData          : TStringList;
    FDisabledColor        : TColor;
    FGridPopupMenu        : TFieldStringGridPopupMenu;
    FOnPasteFromExcel     : TNotifyEvent;
    FBeforePasteColumnData : TNotifyEvent;
    FAfterPasteColumnData  : TNotifyEvent;
    FBeforePasteColumnsAndRowsData : TNotifyEvent;
    FAfterPasteColumnsAndRowsData  : TNotifyEvent;
    FWrapHeaderText,
    FAllowPasteFromExcel    : boolean;
    FShowGridPopupMenu      : boolean;
    FResizeColCount: boolean;
    FResizeRowCount: boolean;

    procedure MenuCopyClick(Sender: TObject);
    procedure MenuCutClick(Sender: TObject);
    procedure MenuDeleteClick(Sender: TObject);
    procedure MenuPasteClick(Sender: TObject);
    procedure MenuPastefromExcelClick(Sender: TObject);
    procedure MenuCopyColumnDataClick(Sender: TObject);
    procedure MenuPasteColumnDataClick(Sender: TObject);
    procedure MenuSelectAllClick(Sender: TObject);
    procedure MenuUndoClick(Sender: TObject);
    procedure MenuCopyColumnsAndRowsDataClick(Sender: TObject);
    procedure MenuPasteColumnsAndRowsDataClick(Sender: TObject);
    procedure SetHasChanges (ACol        : integer;
                             ARow        : Integer;
                             AHasChanges : boolean);
    function GetHasChanges (ACol : integer;
                            ARow : integer) : boolean;
    procedure SetHasMetaData (ACol        : integer;
                             ARow        : Integer;
                             AHasChanges : boolean);
    function GetHasMetaData (ACol : integer;
                            ARow : integer) : boolean;
    procedure SetValidationError (ACol, ARow : Integer;
                                  AErrorType : TGridErrorType;
                                  AError     : string);
    function GetValidationError (ACol, ARow : Integer;
                                 AErrorType : TGridErrorType): string;
    function GetInValidationError (ACol, ARow : Integer;
                                   AErrorType : TGridErrorType): Boolean;
    procedure SizeChanged(OldColCount, OldRowCount: Longint); override;
    procedure KeyPress(var AKey: Char); override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    procedure EnterColumn(ACol, ARow: integer);
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure CreateFieldStringGridPopupMenu; virtual;

    function GetRowIsEnabled(ARow:integer): boolean;
    procedure SetRowIsEnabled(ARow: integer;AIsEnabled: boolean);
    function GetColumnIsEnabled(ACol:integer): boolean;
    procedure SetColumnIsEnabled(ACol: integer;AIsEnabled: boolean);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function HotSpotChangesClicked(X, Y: Integer): boolean;
    function HotSpotMetaDataClicked(X, Y: Integer): boolean;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure SetPopupMenuState;

    function GetProcessFileName(PID: DWORD): string;
    function ClipboardOwnerIsExcel : boolean;

    procedure SetAllowPasteFromExcel(AValue : boolean);
    procedure SetShowGridPopupMenu(AValue : boolean);

    function CanPasteColumnData:boolean;
    function CanPasteColumnsAndRowsData:boolean;
    function CanCopyColumnsAndRowsData:boolean;

    procedure DoBeforePasteColumnsAndRowsData; virtual;
    procedure DoAfterPasteColumnsAndRowsData; virtual;

    procedure DoBeforePasteColumnData; virtual;
    procedure DoAfterPasteColumnData; virtual;

    function PasteFromExcel: boolean; virtual;
    procedure SetCanResizeColCount(const Value: boolean);
    procedure SetCanResizeRowCount(const Value: boolean);
  public
    procedure SetFieldValue(ACol,ARow: integer;AValue: string);  overload;
    procedure SetFieldValue(ACol,ARow: integer;AValue: integer);  overload;
    procedure SetFieldValue(ACol,ARow: integer;AValue: double);  overload;
    procedure Reset;
    procedure AddFieldProperty(AFieldProperty : TAbstractFieldProperty);
    procedure ClearFieldProperties;
    procedure ClearErrors;
    function RemoveFieldProperty(AColIndex: integer):boolean;
    function CurrenCell: TGridCoord;
    function FieldProperty(AColIndex: integer):TAbstractFieldProperty;
    function FieldPropertiesCount : integer;

//    procedure DoCopyToClipboard;
//    procedure DoExport;
//    procedure DoPrint(Title: string); override;

    property HasMetaData[ACol, ARow : integer]: boolean read GetHasMetaData write SetHasMetaData;
    property HasChanges[ACol, ARow : integer]: boolean read GetHasChanges write SetHasChanges;
    property ValidationError[ACol, ARow : Integer; AErrorType : TGridErrorType]: string read GetValidationError write SetValidationError;
    property InValidationError[ACol, ARow : Integer; AErrorType : TGridErrorType]: boolean read GetInValidationError;

    property HasValueChanged: boolean read FHasValueChanged;
    property OnColEnter: TOnColEnterEvent read FOnColEnterEvent write FOnColEnterEvent;

    Property IsColumnEnabled[ACol:integer] : boolean read GetColumnIsEnabled write SetColumnIsEnabled;
    Property IsRowEnabled[ARow:integer] : boolean read GetRowIsEnabled write SetRowIsEnabled;
    property DisabledColor: TColor read FDisabledColor write FDisabledColor;

    property AllowPasteFromExcel  : boolean read FAllowPasteFromExcel  write SetAllowPasteFromExcel;
    property ShowGridPopupMenu    : boolean read FShowGridPopupMenu    write SetShowGridPopupMenu;

    property OnPasteFromExcel: TNotifyEvent read FOnPasteFromExcel write FOnPasteFromExcel;
    property OnBeforePasteColumnsAndRowsData : TNotifyEvent read FBeforePasteColumnsAndRowsData write FBeforePasteColumnsAndRowsData;
    property OnAfterPasteColumnsAndRowsData  : TNotifyEvent read FAfterPasteColumnsAndRowsData write FAfterPasteColumnsAndRowsData;
    property OnBeforePasteColumnData : TNotifyEvent read FBeforePasteColumnData write FBeforePasteColumnData;
    property OnAfterPasteColumnData  : TNotifyEvent read FAfterPasteColumnData write FAfterPasteColumnData;
    property CanResizeColCount : boolean read FResizeColCount write SetCanResizeColCount;
    property CanResizeRowCount : boolean read FResizeRowCount write SetCanResizeRowCount;
    property WrapHeaderText : boolean read FWrapHeaderText write FWrapHeaderText;
  end;

  TStringGridButtonClickEvent = procedure (Sender: TObject; ACol, ARow: Longint) of object;
  TStringGridButtonData = class(TObject)
  protected
    FOnButtonClickEventHandler : TStringGridButtonClickEvent;
    FCaption                   : string;
  public
    property OnButtonClickEventHandler: TStringGridButtonClickEvent read FOnButtonClickEventHandler write FOnButtonClickEventHandler;
    property Caption : string read FCaption write FCaption;
  end;

  TFieldButtonStringGrid = class(TFieldStringGrid)
  protected
    FButtonColums : TStringList;
    FButtonDown: Boolean;
    FDownCol   : Integer;
    FDownRow   : Integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure SizeChanged(OldColCount, OldRowCount: Longint); override;
    function Get_ButtonColumn(ACol: integer): boolean;
    procedure Set_ButtonColumn(ACol: integer; AButtonColumn: Boolean);
    function Get_ButtonColumnOnClick(ACol: integer): TStringGridButtonClickEvent;
    procedure Set_ButtonColumnOnClick(ACol: integer; AButtonClickEventHandler: TStringGridButtonClickEvent);
    function Get_ButtonColumnCaption(ACol: integer): string;
    procedure Set_ButtonColumnCaption(ACol: integer; ACaption: string);
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    procedure ButtonClick(ACol, ARow: Longint); virtual;
  public
    property ButtonColumnCaption[ACol:integer] : string read Get_ButtonColumnCaption write Set_ButtonColumnCaption;
    property ButtonColumnOnClick[ACol:integer] : TStringGridButtonClickEvent read Get_ButtonColumnOnClick write Set_ButtonColumnOnClick;
    property ButtonColumn[ACol:integer] : boolean read Get_ButtonColumn write Set_ButtonColumn;
  end;

  TFieldCheckListStringGrid = class(TFieldStringGrid)
  protected
    FCheckedRows : TStringList;
    FChecked : Boolean;
    FCheckedRow   : Integer;
    FCheckedCol   : Integer;
    FCheckBoxClick : TNotifyEvent;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Get_CheckeRow(ARow: integer): boolean;
    procedure Set_CheckeRow(ARow: integer; ACheckedRow: Boolean);
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    procedure CheckBoxClick(ARow: Longint); virtual;
  public
    property CheckedRow[ARow:integer] : boolean read Get_CheckeRow write Set_CheckeRow;
    property OnCheckBoxClick : TNotifyEvent read FCheckBoxClick write FCheckBoxClick;
  end;

  TCellType = (ctValue, ctAggregate);

  TPropertyCell = class(TAbstractPanel)
  protected
    FBorder   : TShape;
    FCellType : TCellType;
    procedure SetCellType(ACellType : TCellType);
    procedure CreateMemberObjects; override;
  public
    property CellType : TCellType read FCellType write SetCellType;
  end;

  TProperty = class(TAbstractAppObject)
  protected
    FPropertyName    : string;
    FPropertyCaption : string;
    FPropertyValue   : string;
    FPropertyCell,
    FValueCell       : TPropertyCell;
    function GetPropertyName  : string;
    function GetPropertyValue : string;
    procedure SetPropertyName(AName : string);
    procedure SetPropertyValue(AValue : string);
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    property PropertyCaption : string  read FPropertyCaption  write FPropertyCaption;
    property PropertyName    : string  read GetPropertyName   write SetPropertyName;
    property PropertyValue   : string  read GetPropertyValue  write SetPropertyValue;
    property PropertyCell    : TPropertyCell  read FPropertyCell;
    property ValueCell       : TPropertyCell  read FValueCell;
  end;

  TPropertyGroup = class(TAbstractAppObject)
  protected
    FPropertyGroupName    : string;
    FPropertyGroupCaption : string;
    FPropertyList         : TObjectList;
    FCount                : integer;
    FPropertyGroupPanel,
    FPropertyPanel,
    FValuePanel           : TAbstractPanel;
    function GetCount : integer;
    procedure SetPropertyGroupCaption(const Value: string);
    procedure SetPropertyGroupName(APropertyGroupName : string);
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure AddProperty(APropertyName, APropertyValue : string);
    procedure UpdateProperty(APropertyName, APropertyValue : string);
    function DeleteProperty(APropertyName : string) : boolean;
    function ValidProperty(APropertyName : string) : boolean;

    function PropertyByIndex(AIndex : integer) : TProperty;
    function FindProperty(APropertyName : string) : TProperty;

    property PropertyGroupName : string read FPropertyGroupName write SetPropertyGroupName;
    property PropertyGroupCaption : string read FPropertyGroupCaption write SetPropertyGroupCaption;
    property PropertyCount : integer read GetCount;
    property PropertyGroupPanel : TAbstractPanel read FPropertyGroupPanel;
    property PropertyPanel      : TAbstractPanel read FPropertyPanel;
    property ValuePanel         : TAbstractPanel read FValuePanel;
  end;

  TWaterBalanceDataViewer = class(TAbstractPanel)
  protected
    FObjectName : string;
    FHeadingPanel,
    FPropGroupPanel,
    FPropPanel,
    FValPanel : TAbstractPanel;
    //FSpl1,
    //FSpl2     : TSplitter;
    FPropertyGroupList : TObjectList;
    function FindGroup(AGroupName : string) : TPropertyGroup;
    function IndexOfGroup(APropertyGroup : TPropertyGroup) : integer;
    function GetObjectName : string;
    procedure SetObjectName(AName : string);
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    function SaveState: boolean; override;

    procedure AddProperty(AGroupName, APropertyName, APropertyValue : string; APropertyType : TCellType);
    procedure UpdateProperty(AGroupName, APropertyName, APropertyValue : string);
    procedure DeleteProperty(AGroupName, APropertyName : string);
    procedure DeleteGroup(AGroupName: string);
    //procedure AggregateGroup(AGroupName : string)
    procedure Resize; override;
    property ObjectName : string read GetObjectName write SetObjectName;
  end;

implementation

uses
  System.UITypes,
  Vcl.Dialogs,
  Math,
  VCL.Forms,
  SysUtils,
  VCLTee.TeExport,
  Types,

  UConstants,
  UDBConstants,
  Vcl.Printers,
  Vcl.Clipbrd,
  UUtilities,
  UMainMenuEventType,
  UErrorHandlingOperations, VCLTee.TeeProcs;

{ Implementation classes }

function GetCaptionFromName(AName : string) : string;
const OPNAME = 'GetCaptionFromName';
var
  LIndex : integer;
  LResult : string;
begin
  LResult := '';
  LResult := LResult + AName[1];
  for LIndex := 2 to Length(AName) do
    if (Ord(AName[LIndex]) > 64) and
       (Ord(AName[LIndex]) < 91) then
      LResult := LResult + ' ' + AName[LIndex]
    else
      LResult := LResult + AName[LIndex];
  Result := LResult;
end;


{ TFieldEdit }

function TFieldEdit.HasValueChanged: boolean;
const OPNAME = 'TFieldEdit.HasValueChanged';
begin
  Result := False;
  try
    Result := (FOldValue <> Text) or Self.Modified;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TFieldEdit.KeyPress(var AKey: Char);
const OPNAME = 'TFieldEdit.KeyPress';
var
  LMinValue: double;
  //LParentForm: TCustomForm;
begin
  try
    {If AKey = #13 then
    begin
      If HiWord(GetKeyState(VK_SHIFT)) <> 0 then
        SelectNext(Self as TWinControl,False,False)
      else
        SelectNext(Self as TWinControl,True,False) ;
      AKey := #0
    end;}

    inherited KeyPress(AKey);

    if IsPintableCharacter(AKey) and
     (HiWord(GetKeyState(VK_MENU)) = 0) and
     (HiWord(GetKeyState(VK_RMENU)) = 0) and
     (HiWord(GetKeyState(VK_CONTROL)) = 0) and
     (HiWord(GetKeyState(VK_RCONTROL)) = 0) Then
    begin
      if (NOT IsEnabled) OR
         (HasChanges) then
        AKey := #0
      else
      begin
        if Assigned(FFieldProperty) then
        begin
          if (AKey ='.') then
          begin
            if (FFieldProperty.FieldDataType = FieldFloatType) then
            begin
              if (Pos(AKey, Self.Text) > 0) then
                    AKey := #0;
            end
            else
              AKey := #0;
          end
          else
          if (AKey ='-') then
          begin
            if (FFieldProperty.FieldGroup in [fgMinMax, fgMinMaxArray]) and
               (FFieldProperty.FieldDataType in [FieldIntegerType,FieldFloatType]) then
            begin
              if (Pos(AKey, Self.Text) > 0) then
                  AKey := #0;
              if(FFieldProperty.FieldMinimumValue <> NegativeInf) and
                (FFieldProperty.FieldMinimumValue <> PositiveInf) then
              begin
                LMinValue := StrToFloat(FFieldProperty.FieldMinimumValue);
                if(LMinValue >= 0.0) then
                  AKey := #0;
              end;
            end;
          end
          else
          begin
            if FFieldProperty.FieldDataType in [FieldIntegerType,FieldFloatType] then
            begin
              if not CharInSet(AKey,['0'..'9', #8]) then
                  AKey := #0;
            end;
          end;
        end;
      end;
      if(AKey = #0) then
        SysUtils.Beep;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TFieldEdit.SetHasChanges (AHasChanges : boolean);
const OPNAME = 'TFieldEdit.SetHasChanges';
begin
  try
    FHasChanges := AHasChanges;
    ReadOnly    := FHasChanges;
    Self.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldEdit.SetIsEnabled(AIsEnabled: boolean);
const OPNAME = 'TFieldEdit.SetIsEnabled';
begin
  try
    if Assigned(FFieldProperty) then
      AIsEnabled := AIsEnabled and (FAppModules.FieldProperties.ModeIsEditable(FFieldProperty.FieldName))
    else
      AIsEnabled := False;
    ReadOnly := not AIsEnabled;
    FIsEnabled := AIsEnabled;
    if AIsEnabled then
      Self.Color := clWindow
    else
      Self.Color := clSilver;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldEdit.GetIsEnabled: boolean;
const OPNAME = 'TFieldEdit.GetIsEnabled';
begin
  Result := False;
  try
    Result := FIsEnabled or (not ReadOnly);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldEdit.Reset;
const OPNAME = 'TFieldEdit.Reset';
begin
  try
    FOldValue := '';
    Text := '';
  except on E: Exception do HandleError(E, OPNAME); end;
end;
procedure TFieldEdit.SetFieldValue(AValue: string);
const OPNAME = 'TFieldEdit.SetFieldValue';
begin
  try
    if Assigned(FFieldProperty) then
    begin
      AValue := Trim(AValue);
      if(AValue <> '') then
      begin
        case FFieldProperty.FieldDataType of
          FieldStringType : AValue := Trim(Format(FFieldProperty.FormatStringGrid, [Trim(AValue)]));
          FieldFloatType  : try AValue := Trim(Format(FFieldProperty.FormatStringGrid, [StrToFloat(Trim(AValue))])); except end;
          FieldIntegerType: try AValue := Trim(Format(FFieldProperty.FormatStringGrid, [StrToInt(Trim(AValue))])); except end;
          FieldDTimeType  : ;
          FieldCharType   : ;
        else
          raise Exception.CreateFmt('Unknown field data type [%d].', [FFieldProperty.FieldDataType]);
        end;
      end;
      Text := AValue;
      FOldValue := AValue;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldEdit.SetFieldValue(AValue: integer);
const OPNAME = 'TFieldEdit.SetFieldValue';
var
  LText: string;
begin
  try
    if Assigned(FFieldProperty) then
    begin
      if (FFieldProperty.FieldDataType = FieldIntegerType) then
      begin
        LText := Trim(Format(FFieldProperty.FormatStringGrid, [AValue]));
        Text := LText;
        FOldValue := Text;
      end
      else
        raise Exception.CreateFmt('Unknown field data type [%d].', [FFieldProperty.FieldDataType]);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldEdit.SetFieldValue(AValue: double);
const OPNAME = 'TFieldEdit.SetFieldValue';
var
  LText: string;
begin
  try
    if Assigned(FFieldProperty) then
    begin
      if (FFieldProperty.FieldDataType = FieldFloatType) then
      begin
        LText := Trim(Format(FFieldProperty.FormatStringGrid, [AValue]));
        Text := LText;
        FOldValue := Text;
      end
      else
        raise Exception.CreateFmt('Unknown field data type [%d].', [FFieldProperty.FieldDataType]);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldEdit.ShowErrorState;
const OPNAME = 'TFieldEdit.ShowErrorState';
begin
  try
     if(FFieldValidationError <> '') then
       Color := clRed
     else if(FContextValidationError <> '') then
       Color := clTeal
     else if (not FIsEnabled) then
       Color := clSilver
     else
       Color := clWindow
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldEdit.DoExit;
const OPNAME = 'TFieldEdit.DoExit';
begin
  if Application.Terminated then Exit;
  inherited;
  try
    ShowErrorState;
    PaintChangesAndMetaData;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldEdit.SetFieldProperty(AFieldProperty: TAbstractFieldProperty);
const OPNAME = 'TFieldEdit.SetFieldProperty';
begin
 try
   FFieldProperty := AFieldProperty;
   if assigned(FFieldProperty) then
     IsEnabled := FAppModules.FieldProperties.ModeIsEditable(AFieldProperty.FieldName)
   else
     IsEnabled := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldEdit.CreateMemberObjects;
const OPNAME = 'TFieldEdit.CreateMemberObjects';
begin
  inherited;
  try
    FEditIdentifier    := giUnknown;
    FFieldProperty     := nil;
    FOldValue          := '';
    FIsEnabled         := False;
    FHasChanges        := False;
    FHasMetaData       := FALSE;
    FFieldValidationError     := '';
    FContextValidationError   := '';
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldEdit.DestroyMemberObjects;
const OPNAME = 'TFieldEdit.DestroyMemberObjects';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
  inherited;
end;

procedure TFieldEdit.SetContextValidationError(AErrorMesg: String);
const OPNAME = 'TFieldEdit.SetContextValidationError';
begin
  try
    FContextValidationError := AErrorMesg;
    ShowErrorState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldEdit.SetFieldValidationError(AErrorMesg: String);
const OPNAME = 'TFieldEdit.SetFieldValidationError';
begin
  try
    FFieldValidationError := AErrorMesg;
    ShowErrorState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldEdit.DoEnter;
const OPNAME = 'TFieldEdit.DoEnter';
begin
  if Application.Terminated then Exit;
  inherited;
  try
    FOldValue := Text;
    Modified  := False;
    if (Assigned(FAppModules.Changes()) AND
        Assigned(FFieldProperty) AND
        FFieldProperty.InChangeList) then
      FAppModules.Changes.SetParameterChanges(TRUE)
    else
      FAppModules.Changes.SetParameterChanges(FALSE);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldEdit.SetHasMetaData(AHasMetaData: boolean);
const OPNAME = 'TFieldEdit.SetHasMetaData';
begin
  try
    FHasMetaData := AHasMetaData;
    Self.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldEdit.WMPAINT(var Message: TMessage);
const OPNAME = 'TFieldEdit.WMPAINT';
begin
  inherited;
  try
    PaintChangesAndMetaData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldEdit.PaintChangesAndMetaData;
const OPNAME = 'TFieldEdit.PaintChangesAndMetaData';
var
  LCanvas: TCanvas;
  lSize  : integer;
  LDC    : HDC;
begin
  try
    LDC := GetDC(Self.Handle);
    try
      LCanvas := TCanvas.Create;
      try
        LCanvas.Handle := LDC;
        lSize := 7;//Round(Self.Height / 3);
        if HasChanges then
        begin
            LCanvas.Brush.Color := clLime;
            LCanvas.Pen.Color   := clBlack;
            LCanvas.Polygon([Point(Self.ClientRect.Left, Self.ClientRect.Top),
                             Point(Self.ClientRect.Left + lSize, Self.ClientRect.Top),
                             Point(Self.ClientRect.Left, Self.ClientRect.Top + lSize)]);
        end;

        if HasMetaData then
        begin
            LCanvas.Brush.Color := clAqua;
            LCanvas.Pen.Color   := clBlack;
            LCanvas.Polygon([Point(Self.ClientRect.Left,         Self.ClientRect.Bottom),
                             Point(Self.ClientRect.Left + lSize, Self.ClientRect.Bottom),
                             Point(Self.ClientRect.Left,         Self.ClientRect.Bottom - lSize)]);
        end;
      finally
        LCanvas.Free;
      end;
    finally
      ReleaseDC(Self.Handle, LDC);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;

end;

procedure TFieldEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TFieldEdit.MouseDown';
begin
  try
    if (Button = mbLeft) then
    begin
      if HotSpotChangesClicked(X, Y) then
      begin
        if HasChanges then
          FAppModules.Model.ProcessEvent(CmeChangeParameter,nil)
        else
          inherited MouseDown(Button,Shift,X, Y);
      end
      else
      begin
        if HotSpotMetaDataClicked(X,Y) and  HasMetaData then
          FAppModules.Model.ProcessEvent(CmeMetaData,nil)
        else
          inherited MouseDown(Button,Shift,X, Y);
      end;
    end
    else
      inherited MouseDown(Button,Shift,X, Y);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldEdit.HotSpotChangesClicked(X, Y: Integer): boolean;
const OPNAME = 'TFieldEdit.HotSpotChangesClicked';
var
  lSize : integer;
begin
  Result := False;
  try
    lSize := 7;//Round(Self.Height / 3);
    Result := (X >= 0) and
              (Y >= 0) and
              (X <= lSize) and
              (Y <=  lSize);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldEdit.HotSpotMetaDataClicked(X, Y: Integer): boolean;
const OPNAME = 'TFieldEdit.HotSpotMetaDataClicked';
var
  lSize : integer;
begin
  Result := False;
  try
    lSize := 7;//Round(Self.Height / 3);
    Result := (X >= 0) and
              (Y >= 0) and
              (X <= lSize) and
              (Y >=  Self.Height - lSize);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TFieldRichEdit }

function TFieldRichEdit.HasValueChanged: boolean;
const OPNAME = 'TFieldRichEdit.HasValueChanged';
begin
  Result := False;
  try
    Result := (FOldValue <> Text) or Self.Modified;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRichEdit.KeyPress(var AKey: Char);
const OPNAME = 'TFieldRichEdit.KeyPress';
begin
  try
    inherited KeyPress(AKey);
    if IsPintableCharacter(AKey) and
     (HiWord(GetKeyState(VK_MENU)) = 0) and
     (HiWord(GetKeyState(VK_RMENU)) = 0) and
     (HiWord(GetKeyState(VK_CONTROL)) = 0) and
     (HiWord(GetKeyState(VK_RCONTROL)) = 0) Then
    begin
      if (NOT IsEnabled) then
        AKey := #0;
      if(AKey = #0) then
        SysUtils.Beep;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TFieldRichEdit.SetHasChanges (AHasChanges : boolean);
const OPNAME = 'TFieldRichEdit.SetHasChanges';
begin
  try
    FHasChanges := AHasChanges;
    ReadOnly    := FHasChanges;
    Self.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRichEdit.SetIsEnabled(AIsEnabled: boolean);
const OPNAME = 'TFieldRichEdit.SetIsEnabled';
begin
  try
    if Assigned(FFieldProperty) then
      AIsEnabled := AIsEnabled and (FAppModules.FieldProperties.ModeIsEditable(FFieldProperty.FieldName))
    else
      AIsEnabled := False;
    ReadOnly := not AIsEnabled;
    FIsEnabled := AIsEnabled;
    if AIsEnabled then
      Self.Color := clWindow
    else
      Self.Color := clSilver;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldRichEdit.GetIsEnabled: boolean;
const OPNAME = 'TFieldRichEdit.GetIsEnabled';
begin
  Result := False;
  try
    Result := ReadOnly or FIsEnabled;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRichEdit.Reset;
const OPNAME = 'TFieldRichEdit.Reset';
begin
  try
    FOldValue := '';
    Text := '';
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TFieldRichEdit.SetFieldValue(AValue: string);
const OPNAME = 'TFieldRichEdit.SetFieldValue';
begin
  try
    if Assigned(FFieldProperty) then
    begin
      AValue := Trim(AValue);
{      if (AValue <> '') then
      begin
        case FFieldProperty.FieldDataType of
          FieldStringType : AValue := Trim(Format(FFieldProperty.FormatStringGrid, [Trim(AValue)]));
          FieldFloatType  : try AValue := Trim(Format(FFieldProperty.FormatStringGrid, [StrToFloat(Trim(AValue))])); except end;
          FieldIntegerType: try AValue := Trim(Format(FFieldProperty.FormatStringGrid, [StrToInt(Trim(AValue))])); except end;
          FieldDTimeType  : ;
          FieldCharType   : ;
        else
          raise Exception.CreateFmt('Unknown field data type [%d].', [FFieldProperty.FieldDataType]);
        end;
      end;}
      Text := AValue;
      FOldValue := AValue;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRichEdit.ShowErrorState;
const OPNAME = 'TFieldRichEdit.ShowErrorState';
begin
  try
     if (FFieldValidationError <> '') then
       Color := clRed
     else if(FContextValidationError <> '') then
       Color := clTeal
     else if (not FIsEnabled) then
       Color := clSilver
     else
       Color := clWindow
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRichEdit.DoExit;
const OPNAME = 'TFieldRichEdit.DoExit';
begin
  if Application.Terminated then Exit;
  inherited;
  try
    ShowErrorState;
    PaintChangesAndMetaData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRichEdit.SetFieldProperty (AFieldProperty : TAbstractFieldProperty);
const OPNAME = 'TFieldRichEdit.SetFieldProperty';
begin
 try
   FFieldProperty := AFieldProperty;
   if assigned(FFieldProperty) then
     IsEnabled := FAppModules.FieldProperties.ModeIsEditable(AFieldProperty.FieldName)
   else
     IsEnabled := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRichEdit.CreateMemberObjects;
const OPNAME = 'TFieldRichEdit.CreateMemberObjects';
begin
  inherited;
  try
    FFieldProperty          := nil;
    FOldValue               := '';
    FIsEnabled              := False;
    FHasChanges             := False;
    FHasMetaData            := FALSE;
    FFieldValidationError   := '';
    FContextValidationError := '';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRichEdit.DestroyMemberObjects;
const OPNAME = 'TFieldRichEdit.DestroyMemberObjects';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
  inherited;
end;

procedure TFieldRichEdit.SetContextValidationError(AErrorMesg: String);
const OPNAME = 'TFieldRichEdit.SetContextValidationError';
begin
  try
    FContextValidationError := AErrorMesg;
    ShowErrorState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRichEdit.SetFieldValidationError(AErrorMesg: String);
const OPNAME = 'TFieldRichEdit.SetFieldValidationError';
begin
  try
    FFieldValidationError := AErrorMesg;
    ShowErrorState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRichEdit.DoEnter;
const OPNAME = 'TFieldRichEdit.DoEnter';
begin
  if Application.Terminated then Exit;
  inherited;
  try
    if (Assigned(FAppModules.Changes()) AND
        Assigned(FFieldProperty) AND
        FFieldProperty.InChangeList) then
      FAppModules.Changes.SetParameterChanges(TRUE)
    else
      FAppModules.Changes.SetParameterChanges(FALSE);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRichEdit.SetHasMetaData(AHasMetaData: boolean);
const OPNAME = 'TFieldRichEdit.SetHasMetaData';
begin
  try
    FHasMetaData := AHasMetaData;
    Self.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRichEdit.WMPAINT(var Message: TMessage);
const OPNAME = 'TFieldRichEdit.WMPAINT';
begin
  inherited;
  try
    PaintChangesAndMetaData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRichEdit.PaintChangesAndMetaData;
const OPNAME = 'TFieldRichEdit.PaintChangesAndMetaData';
var
  LCanvas : TCanvas;
  lSize   : integer;
  LDC     : HDC;
begin
  try
    LDC := GetDC(Self.Handle);
    try
      LCanvas := TCanvas.Create;
      try
        LCanvas.Handle := LDC;
        lSize := 7;
        if HasChanges then
        begin
          LCanvas.Brush.Color := clLime;
          LCanvas.Pen.Color   := clBlack;
          LCanvas.Polygon([Point(Self.ClientRect.Left, Self.ClientRect.Top),
                           Point(Self.ClientRect.Left + lSize, Self.ClientRect.Top),
                           Point(Self.ClientRect.Left, Self.ClientRect.Top + lSize)]);
        end;

        if HasMetaData then
        begin
          LCanvas.Brush.Color := clAqua;
          LCanvas.Pen.Color   := clBlack;
          LCanvas.Polygon([Point(Self.ClientRect.Left,         Self.ClientRect.Bottom),
                           Point(Self.ClientRect.Left + lSize, Self.ClientRect.Bottom),
                           Point(Self.ClientRect.Left,         Self.ClientRect.Bottom - lSize)]);
        end;
      finally
        LCanvas.Free;
      end;
    finally
      ReleaseDC(Self.Handle, LDC);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRichEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TFieldRichEdit.MouseDown';
begin
  try
    if (Button = mbLeft) then
    begin
      if HotSpotChangesClicked(X, Y) then
      begin
        if HasChanges then
          FAppModules.Model.ProcessEvent(CmeChangeParameter,nil)
        else
          inherited MouseDown(Button,Shift,X, Y);
      end
      else
      begin
        if HotSpotMetaDataClicked(X,Y) and  HasMetaData then
          FAppModules.Model.ProcessEvent(CmeMetaData,nil)
        else
          inherited MouseDown(Button,Shift,X, Y);
      end;
    end
    else
      inherited MouseDown(Button,Shift,X, Y);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldRichEdit.HotSpotChangesClicked(X, Y: Integer): boolean;
const OPNAME = 'TFieldRichEdit.HotSpotChangesClicked';
var
  lSize : integer;
begin
  Result := False;
  try
    lSize := 7;
    Result := (X >= 0) and
              (Y >= 0) and
              (X <= lSize) and
              (Y <=  lSize);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldRichEdit.HotSpotMetaDataClicked(X, Y: Integer): boolean;
const OPNAME = 'TFieldRichEdit.HotSpotMetaDataClicked';
var
  lSize : integer;
begin
  Result := False;
  try
    lSize := 7;
    Result := (X >= 0) and
              (Y >= 0) and
              (X <= lSize) and
              (Y >=  Self.Height - lSize);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TFieldStringGrid }
procedure TFieldStringGrid.CreateMemberObjects;
const OPNAME = 'TFieldStringGrid.CreateMemberObjects';
begin
  inherited;
  try
    FFieldPropertyList    := TObjectList.Create(False);
    FColIsEnabled         := TStringList.Create;
    FRowIsEnabled         := TStringList.Create;
    FCellContextError     := TStringList.Create;
    FColContextError      := TStringList.Create;
    FCellFieldError       := TStringList.Create;
    FHasChanges           := TStringList.Create;
    FHasMetaData          := TStringList.Create;
    FOldValue             := '';
    FCurrenCell.X         := -1;
    FCurrenCell.Y         := -1;
    FHasValueChanged      := False;
    FOnColEnterEvent      := nil;
    Self.Options          := Self.Options + [ goEditing];
    FDisabledColor        := clSilver;
    FGridPopupMenu        := nil;

    FAllowPasteFromExcel  := False;
    FShowGridPopupMenu    := False;
    FResizeColCount       := False;
    FResizeRowCount       := False;

    FOnPasteFromExcel              := nil;
    FBeforePasteColumnData         := nil;
    FAfterPasteColumnData          := nil;
    FBeforePasteColumnsAndRowsData := nil;
    FAfterPasteColumnsAndRowsData  := nil;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.DestroyMemberObjects;
const OPNAME = 'TFieldStringGrid.DestroyMemberObjects';
begin
  try
    FreeAndNil(FFieldPropertyList);
    FreeAndNil(FCellContextError);
    FreeAndNil(FColContextError);
    FreeAndNil(FCellFieldError);
    FreeAndNil(FColIsEnabled);
    FreeAndNil(FRowIsEnabled);
    FreeAndNil(FHasChanges);
    FreeAndNil(FHasMetaData);
  except on E: Exception do HandleError(E, OPNAME) end;
  inherited;
end;

procedure TFieldStringGrid.MenuUndoClick(Sender: TObject);
const OPNAME = 'TFieldStringGrid.MenuUndoClick';
begin
  try
    if (Self.InplaceEditor <> nil) and Self.InplaceEditor.Visible then
     Self.InplaceEditor.Undo;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.MenuCutClick(Sender: TObject);
const OPNAME = 'TFieldStringGrid.MenuCutClick';
begin
  try
    if (Self.InplaceEditor <> nil) and Self.InplaceEditor.Visible then
     Self.InplaceEditor.CutToClipboard;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.MenuCopyClick(Sender: TObject);
const OPNAME = 'TFieldStringGrid.MenuCopyClick';
begin
  try
    if (Self.InplaceEditor <> nil) and Self.InplaceEditor.Visible then
     Self.InplaceEditor.CopyToClipboard;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.MenuPasteClick(Sender: TObject);
const OPNAME = 'TFieldStringGrid.MenuPasteClick';
begin
  try
    if (Self.InplaceEditor <> nil) and Self.InplaceEditor.Visible then
     Self.InplaceEditor.PasteFromClipboard;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.MenuDeleteClick(Sender: TObject);
const OPNAME = 'TFieldStringGrid.MenuDeleteClick';
begin
  try
    if (Self.InplaceEditor <> nil) and Self.InplaceEditor.Visible then
     Self.InplaceEditor.ClearSelection;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.MenuSelectAllClick(Sender: TObject);
const OPNAME = 'TFieldStringGrid.MenuSelectAllClick';
begin
  try
    if (Self.InplaceEditor <> nil) and Self.InplaceEditor.Visible then
     Self.InplaceEditor.SelectAll;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.MenuPastefromExcelClick(Sender: TObject);
const OPNAME = 'TFieldStringGrid.MenuPastefromExcelClick';
begin
  try
    if PasteFromExcel then
      if Assigned(FOnPasteFromExcel) then FOnPasteFromExcel(Self);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.ClearErrors;
const OPNAME = 'TFieldStringGrid.ClearErrors';
var
  LCount : integer;
begin
  try
    FCellContextError.Clear;
    FColContextError.Clear;
    FCellFieldError.Clear;
    for LCount := 0 to (RowCount * ColCount) -1 do
    begin
      FCellContextError.Add('');
      FCellFieldError.Add('');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.Reset;
const OPNAME = 'TFieldStringGrid.Reset';
var
  LCount : integer;
begin
  try
    FOldValue         := '';
    FCurrenCell.X     := -1;
    FCurrenCell.Y     := -1;
    FHasValueChanged  := False;

    FColIsEnabled.Clear;
    FRowIsEnabled.Clear;
    FFieldPropertyList.Clear;
    FCellContextError.Clear;
    FColContextError.Clear;
    FCellFieldError.Clear;
    FHasChanges.Clear;
    FHasMetaData.Clear;
    for LCount := 0 to ColCount -1  do
      FColContextError.Add('');
    for LCount := 0 to (RowCount * ColCount) -1 do
    begin
      FCellContextError.Add('');
      FCellFieldError.Add('');
      FHasChanges.Add('');
      FHasMetaData.Add('');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldStringGrid.CurrenCell: TGridCoord;
const OPNAME = 'TFieldStringGrid.CurrenCell';
begin
  Result := FCurrenCell;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.EnterColumn(ACol, ARow: integer);
const OPNAME = 'TFieldStringGrid.EnterColumn';
begin
  try
    if Assigned(FOnColEnterEvent) then FOnColEnterEvent(Self,ACol, ARow);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldStringGrid.FieldProperty(AColIndex: integer): TAbstractFieldProperty;
const OPNAME = 'TFieldStringGrid.FieldProperty';
begin
  Result := nil;
  try
    if(AColIndex >= 0) and (AColIndex < FFieldPropertyList.Count) then
    begin
      Result := TAbstractFieldProperty(FFieldPropertyList.Items[AColIndex]);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.KeyPress(var AKey: Char);
const OPNAME = 'TFieldStringGrid.KeyPress';
var
  LFieldProperty: TAbstractFieldProperty;
  LMinValue: double;
  //LParentForm: TCustomForm;
begin
  try
    {if AKey = #13 then
    begin
      if HiWord(GetKeyState(VK_SHIFT)) <> 0 then
      begin
        if Self.Row > 0 then
           Self.Row := Self.Row - 1
        else
          Self.Row := Self.RowCount - 1
      end
      else
      begin
        if Self.Row < (Self.RowCount - 1) then
          Self.Row := Self.Row  + 1
        else
          Self.Row := 0
      end;
      AKey := #0
    end;

    if (NOT IsColumnEnabled[Self.Col]) OR
       (NOT IsRowEnabled[Self.Row]) then
    begin
      if(AKey = Char(VK_BACK)) or
        (AKey = Char(VK_CLEAR)) or
        (AKey = Char(VK_INSERT)) or
        (AKey = Char(VK_DELETE)) or
        (AKey = Char(VK_INSERT)) or
        (IsPintableCharacter(AKey)) then
      begin
        AKey := #0
      end
    end
    else
    begin}

    if IsPintableCharacter(AKey) and
       (HiWord(GetKeyState(VK_MENU)) = 0) and
       (HiWord(GetKeyState(VK_RMENU)) = 0) and
       (HiWord(GetKeyState(VK_CONTROL)) = 0) and
       (HiWord(GetKeyState(VK_RCONTROL)) = 0) Then
    begin
      if (NOT IsColumnEnabled[Self.Col]) OR
         (NOT IsRowEnabled[Self.Row]) OR
         (HasChanges[Self.Col,Self.Row]) then
        AKey := #0
      else
      begin
        LFieldProperty := FieldProperty(FCurrenCell.Y);
        if Assigned(LFieldProperty) then
        begin
          if (AKey ='.') then
          begin
            if (LFieldProperty.FieldDataType = FieldFloatType) then
            begin
              if (Pos(AKey, Cells[Col,Row]) > 0) then
                    AKey := #0;
            end
            else
              AKey := #0;
          end
          else
          if (AKey ='-') then
          begin
            if (LFieldProperty.FieldGroup in [fgMinMax, fgMinMaxArray]) and
               (LFieldProperty.FieldDataType in [FieldIntegerType,FieldFloatType]) then
            begin
              if (Pos(AKey, Cells[Col,Row]) > 0) then
                  AKey := #0;
              if(LFieldProperty.FieldMinimumValue <> NegativeInf) and
                (LFieldProperty.FieldMinimumValue <> PositiveInf) then
              begin
                LMinValue := StrToFloat(LFieldProperty.FieldMinimumValue);
                if(LMinValue >= 0.0) then
                  AKey := #0;
              end;
            end;
          end
          else
          begin
            if LFieldProperty.FieldDataType in [FieldIntegerType,FieldFloatType] then
            begin
              if not CharInSet(AKey,['0'..'9', #8]) then
                  AKey := #0;
            end;
          end;
        end;
      end;
      if(AKey = #0) then
        SysUtils.Beep;
    end;
    inherited KeyPress(AKey);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldStringGrid.SelectCell(ACol, ARow: Integer): Boolean;
const OPNAME = 'TFieldStringGrid.SelectCell';
var
  LOldColumn: integer;
begin
  Result := False;
  try
    LOldColumn := FCurrenCell.Y;
    FCurrenCell.X := ARow;
    FCurrenCell.Y := ACol;
    FOldValue     := Cells[ACol,ARow];
    Result := inherited SelectCell(ACol, ARow);
    //if (NOT IsColumnEnabled[ACol]) OR
    //   (NOT IsRowEnabled[ARow]) then
    //Result := False;
    if ((LOldColumn <> ACol) OR
        InValidationError[ACol, ARow, gveCellField] OR
        InValidationError[ACol, ARow, gveCellContext] OR
        InValidationError[ACol, ARow, gveColContext]) then
      EnterColumn(ACol,ARow);
    if (Assigned(FAppModules.Changes()) AND
        Assigned(FieldProperty(Col)) AND
        FieldProperty(Col).InChangeList) then
      FAppModules.Changes.SetParameterChanges(TRUE)
    else
      FAppModules.Changes.SetParameterChanges(FALSE);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.DoEnter;
const OPNAME = 'TFieldStringGrid.DoEnter';
begin
  if Application.Terminated then Exit;
  inherited DoEnter;
  try
    FOldValue     := '';
    SelectCell(Col,Row);
  except on E: Exception do HandleError(E, OPNAME) end;
end;
procedure TFieldStringGrid.DoExit;
const OPNAME = 'TFieldStringGrid.DoExit';
begin
  if Application.Terminated then Exit;
  inherited DoExit;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.SetFieldValue(ACol,ARow: integer;AValue: string);
const OPNAME = 'TFieldStringGrid.SetFieldValue';
var
  LFieldProperty: TAbstractFieldProperty;
begin
  try
    LFieldProperty := FieldProperty(ACol);
    if Assigned(LFieldProperty) then
    begin
      if (LFieldProperty.FieldDataType = FieldStringType) then
      begin
        AValue := Trim(Format(LFieldProperty.FormatStringGrid, [AValue]));
        Cells[ACol, ARow] := AValue;
        StoreStartValue;
      end
      else
        raise Exception.CreateFmt('Unknown field data type [%d].', [LFieldProperty.FieldDataType]);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.SetFieldValue(ACol, ARow, AValue: integer);
const OPNAME = 'TFieldStringGrid.SetFieldValue';
var
  LFieldProperty: TAbstractFieldProperty;
  LText: string;
begin
  try
    LFieldProperty := FieldProperty(ACol);
    if Assigned(LFieldProperty) then
    begin
      if (LFieldProperty.FieldDataType = FieldIntegerType) then
      begin
        LText := Trim(Format(LFieldProperty.FormatStringGrid, [AValue]));
        Cells[ACol, ARow] := LText;
        StoreStartValue;
      end
      else
        raise Exception.CreateFmt('Unknown field data type [%d].', [LFieldProperty.FieldDataType]);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.SetFieldValue(ACol, ARow: integer; AValue: double);
const OPNAME = 'TFieldStringGrid.SetFieldValue';
var
  LFieldProperty: TAbstractFieldProperty;
  LText: string;
begin
  try
    LFieldProperty := FieldProperty(ACol);
    if Assigned(LFieldProperty) then
    begin
      if (LFieldProperty.FieldDataType = FieldFloatType) then
      begin
        LText := Trim(Format(LFieldProperty.FormatStringGrid, [AValue]));
        Cells[ACol, ARow] := LText;
        StoreStartValue;
      end
      else
        raise Exception.CreateFmt('Unknown field data type [%d].', [LFieldProperty.FieldDataType]);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.AddFieldProperty(AFieldProperty: TAbstractFieldProperty);
const OPNAME = 'TFieldStringGrid.AddFieldProperty';
begin
  try
    if Assigned(AFieldProperty) then
    begin
      FFieldPropertyList.Add(AFieldProperty);
      if FAppModules.FieldProperties.ModeIsEditable(AFieldProperty.FieldName) then
        FColIsEnabled.Add('TRUE')
      else
        FColIsEnabled.Add('FALSE');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.ClearFieldProperties;
const OPNAME = 'TFieldStringGrid.ClearFieldProperties';
begin
  try
    FFieldPropertyList.Clear;
    FColIsEnabled.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldStringGrid.FieldPropertiesCount: integer;
const OPNAME = 'TFieldStringGrid.FieldPropertiesCount';
begin
  Result := 0;
  try
    if Assigned(FFieldPropertyList) then
      Result := FFieldPropertyList.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldStringGrid.RemoveFieldProperty(AColIndex: integer): boolean;
const OPNAME = 'TFieldStringGrid.RemoveFieldProperty';
begin
  Result := False;
  try
    if Assigned(FFieldPropertyList) then
    begin
      if AColIndex < FFieldPropertyList.Count then
      begin
        if AColIndex < FColIsEnabled.Count then
          FColIsEnabled.Delete(AColIndex);
        FFieldPropertyList.Delete(AColIndex);
        Result := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.SizeChanged(OldColCount, OldRowCount: Integer);
const OPNAME = 'TFieldStringGrid.SizeChanged';
var
  LCount :integer;
begin
  inherited SizeChanged(OldColCount, OldRowCount);
  try
    if not (csDestroying	 in ComponentState) then
    begin
      FCellContextError.Clear;
      FColContextError.Clear;
      FCellFieldError.Clear;
      FHasChanges.Clear;
      FHasMetaData.Clear;
      for LCount := 0 to ColCount -1  do
        FColContextError.Add('');
      for LCount := 0 to (RowCount * ColCount) -1 do
      begin
        FCellContextError.Add('');
        FCellFieldError.Add('');
        FHasChanges.Add('');
        FHasMetaData.Add('');
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldStringGrid.GetInValidationError (ACol, ARow : Integer;
                                                AErrorType : TGridErrorType): Boolean;
const OPNAME = 'TFieldStringGrid.GetInValidationError';
var
  LIndex : integer;
begin
  Result := False;
  try
    if (AErrorType in [gveColContext]) then
    begin
      Result := (ACol < FColContextError.Count) and (Trim(FColContextError.Strings[ACol]) <> '');
    end
    else
    begin
      LIndex := (ARow * ColCount) + ACol;
      if (AErrorType in [gveCellContext]) then
        Result := (LIndex < FCellContextError.Count) and
                  (Trim(FCellContextError.Strings[LIndex]) <> '')
      else
      if (AErrorType in [gveCellField]) then
        Result := (LIndex < FCellContextError.Count) and
                  (Trim(FCellFieldError.Strings[LIndex]) <> '')
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldStringGrid.GetValidationError (ACol, ARow : Integer;
                                              AErrorType : TGridErrorType): string;
const OPNAME = 'TFieldStringGrid.GetValidationError';
var
  LIndex : integer;
begin
  Result := '';
  try
    if (AErrorType in [gveColContext]) then
    begin
      if(ACol < FColContextError.Count) then
        Result := FColContextError.Strings[ACol];
    end
    else
    begin
      LIndex := (ARow * ColCount) + ACol;
      if (AErrorType in [gveCellContext]) then
        Result := FCellContextError.Strings[LIndex]
      else
      if (AErrorType in [gveCellField]) then
        Result := FCellFieldError.Strings[LIndex]
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.SetValidationError (ACol, ARow : Integer;
                                               AErrorType : TGridErrorType;
                                               AError     : string);
const OPNAME = 'TFieldStringGrid.SetValidationError';
var
  LIndex : integer;
begin
  try
    if (AErrorType in [gveColContext]) then
    begin
      while (ACol > FColContextError.Count-1) do
         FColContextError.Add('');
      if (Trim(AError) <> '')  then
        FColContextError.Strings[ACol] := Trim(AError)
      else
        FColContextError.Strings[ACol] := '';
    end
    else
    begin
      LIndex := (ARow * ColCount) + ACol;
      if (AErrorType in [gveCellContext]) then
      begin
        while (LIndex > FCellContextError.Count-1) do
           FCellContextError.Add('');
        if (Trim(AError) <> '') then
          FCellContextError.Strings[LIndex] := Trim(AError)
        else
          FCellContextError.Strings[LIndex] := '';
      end
      else
      if (AErrorType in [gveCellField]) then
      begin
        while (LIndex > FCellFieldError.Count-1) do
           FCellFieldError.Add('');
        if (Trim(AError) <> '') then
          FCellFieldError.Strings[LIndex] := Trim(AError)
        else
          FCellFieldError.Strings[LIndex] := '';
      end;
    end;
    Self.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.SetHasChanges (ACol        : integer;
                                          ARow        : Integer;
                                          AHasChanges : boolean);
const OPNAME = 'TFieldStringGrid.SetHasChanges';
var
  lIndex : integer;
begin
  try
    lIndex := (ARow * ColCount) + ACol;
    if (AHasChanges) then
      FHasChanges[lIndex] := 'Y'
    else
      FHasChanges[lIndex] := '';
    Self.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldStringGrid.GetHasChanges (ACol : integer;
                                         ARow : integer): boolean;
const OPNAME = 'TFieldStringGrid.GetHasChanges';
var
  lIndex : integer;
begin
  Result := FALSE;
  try
    lIndex := (ARow * ColCount) + ACol;
    Result := (LIndex < FHasChanges.Count) and (FHasChanges[lIndex] = 'Y');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldStringGrid.GetHasMetaData(ACol, ARow: integer): boolean;
const OPNAME = 'TFieldStringGrid.GetHasMetaData';
var
  lIndex : integer;
begin
  Result := FALSE;
  try
    lIndex := (ARow * ColCount) + ACol;
    Result :=  (LIndex < FHasMetaData.Count) and (FHasMetaData[lIndex] = 'Y');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.SetHasMetaData(ACol, ARow: Integer;AHasChanges: boolean);
const OPNAME = 'TFieldStringGrid.SetHasMetaData';
var
  lIndex : integer;
begin
  try
    lIndex := (ARow * ColCount) + ACol;
    if (AHasChanges) then
      FHasMetaData[lIndex] := 'Y'
    else
      FHasMetaData[lIndex] := '';
    Self.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
const OPNAME = 'TFieldStringGrid.DrawCell';
var
  LRect          : TRect;
  LOldBrushColor : TColor;
  LOldPenColor   : TColor;
  LLeft          : integer;
  LBottom        : integer;
  lSize          : integer;
  //LTextHeight    : Integer;
  LNewRect       : TRect;
begin
  try
    if(ACol < FixedCols) or  (ARow < FixedRows) then
    begin
      if(ARow < FixedRows) then
      begin
        if WrapHeaderText then
        begin
          LNewRect:= ARect;
          //LTextHeight :=
          Canvas.FillRect(LNewRect);
          DrawText(Canvas.Handle,pchar(Cells[ACol, ARow]),length(Cells[ACol, ARow]),
                                 LNewRect, DT_CENTER or DT_NOPREFIX OR DT_WORDBREAK ); //OR DT_CALCRECT

          {if (RowHeights[ARow] < LTextHeight) then
          begin
            RowHeights[ARow]:= LTextHeight;
            LNewRect.Bottom:= LNewRect.Top + LTextHeight;
          end;
          DrawText(Canvas.Handle, pchar(Cells[ACol, ARow]),length(Cells[ACol, ARow]),
                   LNewRect, DT_NOPREFIX OR DT_WORDBREAK);}
        end
        else
          inherited;
      end
      else
        inherited;
      Exit;
    end;

    if IsColumnEnabled[ACol] and IsRowEnabled[ARow] then
      inherited
    else
    begin
      LOldBrushColor := Self.Canvas.Brush.Color;
      try
        Self.Canvas.Brush.Color := DisabledColor;
        inherited;
      finally
        Self.Canvas.Brush.Color := LOldBrushColor;
      end;
    end;

    if (InValidationError[ACol, ARow, gveCellContext] OR
        InValidationError[ACol, ARow, gveColContext] OR
        InValidationError[ACol, ARow, gveCellField]) then
    begin
      LLeft       := Max((ARect.Right - 15), ARect.Left);
      LBottom     := Min((ARect.Top + 10), ARect.Bottom);

      LRect.Top     := ARect.Top;
      LRect.Right   := ARect.Right;
      LRect.Left    := LLeft;
      LRect.Bottom  := LBottom;

      LOldBrushColor := Self.Canvas.Brush.Color;
      LOldPenColor   := Self.Canvas.Pen.Color;
      try
        if (InValidationError[ACol, ARow, gveCellField]) then
        begin
          Self.Canvas.Brush.Color := clRed;
          Self.Canvas.Pen.Color   := clBlack;
        end
        else
        begin
          Self.Canvas.Brush.Color := clTeal;
          Self.Canvas.Pen.Color   := clBlack;
        end;
        Self.Canvas.Polygon([Point(LRect.Left, LRect.Top),
                             Point(LRect.Right, LRect.Bottom),
                             Point(LRect.Right, LRect.Top)]);
      finally
        Self.Canvas.Brush.Color := LOldBrushColor;
        Self.Canvas.Pen.Color   := LOldPenColor;
      end;
    end;

    if (HasChanges[ACol, ARow]) then
    begin
      lSize := 7;//Round(RowHeights[ARow] / 3);
      LOldBrushColor := Self.Canvas.Brush.Color;
      LOldPenColor   := Self.Canvas.Pen.Color;
      try
        Self.Canvas.Brush.Color := clLime;
        Self.Canvas.Pen.Color   := clBlack;
        Self.Canvas.Polygon([Point(ARect.Left,         ARect.Top),
                             Point(ARect.Left + lSize, ARect.Top),
                             Point(ARect.Left,         ARect.Top + lSize)]);
      finally
        Self.Canvas.Brush.Color := LOldBrushColor;
        Self.Canvas.Pen.Color   := LOldPenColor;
      end;
    end;

    if (HasMetaData[ACol, ARow]) then
    begin
      lSize := 7;//Round(RowHeights[ARow] / 3);
      LOldBrushColor := Self.Canvas.Brush.Color;
      LOldPenColor   := Self.Canvas.Pen.Color;
      try
        Self.Canvas.Brush.Color := clAqua;
        Self.Canvas.Pen.Color   := clBlack;
        Self.Canvas.Polygon([Point(ARect.Left,         ARect.Bottom),
                             Point(ARect.Left + lSize, ARect.Bottom),
                             Point(ARect.Left,         ARect.Bottom - lSize)]);
      finally
        Self.Canvas.Brush.Color := LOldBrushColor;
        Self.Canvas.Pen.Color   := LOldPenColor;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;


{procedure TFieldStringGrid.SetIsEnabled(AIsEnabled: boolean);
const OPNAME = 'TFieldStringGrid.SetIsEnabled';
begin
  try
    FIsEnabled := AIsEnabled;
    if AIsEnabled then
    begin
      Self.Color := clSilver;
      Self.Enabled := False;
    end
    else
    begin
      Self.Color := clWindow;
      Self.Enabled := True;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

function TFieldStringGrid.GetColumnIsEnabled(ACol: integer): boolean;
const OPNAME = 'TFieldStringGrid.GetColumnIsEnabled';
begin
  Result := False;
  try
    if ACol < FColIsEnabled.Count then
      Result := FColIsEnabled[ACol] = 'TRUE';
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.SetCanResizeColCount(const Value: boolean);
const OPNAME = 'TFieldStringGrid.SetCanResizeColCount';
begin
  try
    FResizeColCount := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TFieldStringGrid.SetCanResizeRowCount(const Value: boolean);
const OPNAME = 'TFieldStringGrid.SetCanResizeRowCount';
begin
  try
    FResizeRowCount := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TFieldStringGrid.SetColumnIsEnabled(ACol: integer;AIsEnabled: boolean);
const OPNAME = 'TFieldStringGrid.SetColumnIsEnabled';
begin
  try
    if AIsEnabled then
       AIsEnabled := (FAppModules.User.UserRights in CUR_EditData);
    if AIsEnabled and (FAppModules.StudyArea <> nil) then
      AIsEnabled := not FAppModules.StudyArea.ScenarioLocked;

    if ACol < FColIsEnabled.Count then
      if AIsEnabled then
        FColIsEnabled[ACol] := 'TRUE'
      else
        FColIsEnabled[ACol] := 'FALSE';
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;
(*
//Author: Reinhard Schatzl
//Site:http://www.swissdelphicenter.ch/torry/showcode.php?id=1577
// Modified from German to english
procedure TFieldStringGrid.DoPrint(Title: string);
const OPNAME = 'TFieldStringGrid.DoPrint';
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

procedure TFieldStringGrid.DoCopyToClipboard;
const OPNAME = 'TFieldStringGrid.DoCopyToClipboard';
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
*)
{
procedure TFieldStringGrid.DoExport;
const OPNAME = 'TFieldStringGrid.DoExport';
var
  LGridData: TStringList;
  LExportFilename: string;
begin
  try
    if FAppModules.GetExportFilename('.txt',
      'TXT Files (*.txt)|*.txt|All Files (*.*)|*.*', LExportFilename) then
    begin
      LGridData := TStringList.Create;
      try
        CopyGridDataInto(LGridData);
        LGridData.SaveToFile(LExportFilename);
      finally
        FreeAndNil(LGridData);
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TFieldStringGrid.CopyGridDataInto(var AStringList: TStringList);
const OPNAME = 'TFieldStringGrid.CopyGridDataInto';
var LRowIndex: integer;
function FirstRowEmpty: boolean;
const OPNAME = 'FirstRowEmpty';
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
const OPNAME = 'FirstColEmpty';
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
}

function TFieldStringGrid.GetRowIsEnabled(ARow: integer): boolean;
const OPNAME = 'TFieldStringGrid.GetRowIsEnabled';
begin
  Result := False;
  try
    Result := FRowIsEnabled.IndexOf(IntToStr(ARow)) < 0;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.SetRowIsEnabled(ARow: integer; AIsEnabled: boolean);
const OPNAME = 'TFieldStringGrid.SetRowIsEnabled';
begin
  try
    if AIsEnabled then
       AIsEnabled := (FAppModules.User.UserRights in CUR_EditData);
    if AIsEnabled then
      if(FAppModules.StudyArea <> nil) then
       AIsEnabled := (FAppModules.StudyArea.ScenarioLocked);

    if AIsEnabled   then
    begin
      if(FRowIsEnabled.IndexOf(IntToStr(ARow)) >= 0) then
        FRowIsEnabled.Delete(FRowIsEnabled.IndexOf(IntToStr(ARow)));
    end
    else
    begin
      if(FRowIsEnabled.IndexOf(IntToStr(ARow)) < 0) then
        FRowIsEnabled.Add(IntToStr(ARow));
    end;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TFieldStringGrid.MouseDown';
begin
  try
    if (Button = mbLeft) then
    begin
      if HotSpotChangesClicked(X, Y) then
      begin
        if HasChanges[Col, Row] then
          FAppModules.Model.ProcessEvent(CmeChangeParameter,nil)
        else
          inherited MouseDown(Button,Shift,X, Y);
      end
      else
      begin
        if HotSpotMetaDataClicked(X,Y) and  HasMetaData[Col, Row] then
          FAppModules.Model.ProcessEvent(CmeMetaData,nil)
        else
          inherited MouseDown(Button,Shift,X, Y);
      end;
    end
    else
      inherited MouseDown(Button,Shift,X, Y);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldStringGrid.HotSpotChangesClicked(X, Y: Integer): boolean;
const OPNAME = 'TFieldStringGrid.HotSpotChangesClicked';
var
  LCurrentCell: TRect;
  lSize : integer;
begin
  Result := False;
  try
    LCurrentCell := CellRect(Self.Col,Self.Row);
    lSize := 7;//Round(RowHeights[Row] / 3);
    Result := (X >= 0) and
              (Y >= 0) and
              (X >= LCurrentCell.Left) and
              (X <= (LCurrentCell.Left + lSize)) and
              (Y >= LCurrentCell.Top) and
              (Y <= LCurrentCell.Top + lSize);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldStringGrid.HotSpotMetaDataClicked(X, Y: Integer): boolean;
const OPNAME = 'TFieldStringGrid.HotSpotMetaDataClicked';
var
  LCurrentCell: TRect;
  lSize : integer;
begin
  Result := False;
  try
    LCurrentCell := CellRect(Self.Col,Self.Row);
    lSize := 7;//Round(RowHeights[Row] / 3);
    Result := (X >= 0) and
              (Y >= 0) and
              (X >= LCurrentCell.Left) and
              (X <= (LCurrentCell.Left + lSize)) and
              (Y <= LCurrentCell.Bottom) and
              (Y >= LCurrentCell.Bottom - lSize);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
const OPNAME = 'TFieldStringGrid.DoContextPopup';
var
  LCol,LRow: integer;
begin
  try
    Self.PopupMenu := nil;
    if(goEditing in Self.Options) then
    begin
      Self.MouseToCell(MousePos.X,MousePos.Y,LCol,LRow);
      if(LCol >= 0) and (LRow >= 0) then
        Self.PopupMenu := FGridPopupMenu;
    end;
    if(Self.PopupMenu <> nil) then
      SetPopupMenuState;

    inherited;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function  TFieldStringGrid.PasteFromExcel : boolean;
const OPNAME = 'TFieldStringGrid.PasteFromExcel';
procedure ExcelCommatext(AExcelData: TStringList);
const OPNAME = 'ExcelCommatext';
var
  LLineData : TStringList;
  LString,
  LLine: string;
  LPos: integer;
  LIndex: integer;
begin
  LLineData := TStringList.Create;
  try
    for LIndex := 0 to AExcelData.Count -1 do
    begin
      LLineData.Clear;
      LLine := AExcelData[LIndex];
      while(Length(LLine) > 0) do
      begin
        LPos := Pos(#9,LLine);
        if(LPos = 0) then
        begin
          LLineData.Add(LLine);
          LLine := '';
        end
        else
        if(LPos = 1) then
        begin
          LLine := Copy(LLine,2,Length(LLine));
          LLineData.Add('');
        end
        else
        begin
          LString := Copy(LLine,1,LPos-1);
          LLine   := Copy(LLine,LPos+1,Length(LLine));
          LLineData.Add(LString);
        end;
      end;
      if(LLineData.Count > 0) then
        AExcelData[LIndex] := LLineData.CommaText;
    end;
  finally
    LLineData.Free;
  end;
end;
var
  LLineData,
  LExcelData: TStringList;
  LRow,
  LCol,
  LCount,
  LIndex,
  LMaxCol : integer;
  LResizeCols,
  LResizeRows: boolean;
  LDialogResult: Word;
  //LRowData: string;
begin
  Result := False;
  try
    LExcelData := TStringList.Create;
    try
      Clipboard.Open;
      try
        if Clipboard.HasFormat(CF_TEXT) then
        begin
          if ClipboardOwnerIsExcel then
          begin
            LExcelData.Text := Clipboard.AsText;
            ExcelCommatext(LExcelData);
          end
          else
            LExcelData.CommaText := Clipboard.AsText;
        end;
      finally
        Clipboard.Close;
      end;
      if (LExcelData.Count = 0) then
      begin
        ShowMessage(FAppModules.Language.GetString('Message.NoExcelDataMsg'));
        Exit;
      end;

      LResizeRows := False;
      if(LExcelData.Count <> (Self.RowCount - Self.FixedRows)) then
      begin
        LDialogResult := MessageDlg(FAppModules.Language.GetString('Message.NumberOfExcelCopiedRows'),mtConfirmation,mbYesNoCancel,0);
        if(LDialogResult = mrCancel) then  Exit;
        if(LDialogResult = mrYes) and (FResizeRowCount) then
          LResizeRows := True;
      end;
      LMaxCol := 0;
      LLineData := TStringList.Create;
      try
        for LIndex := 0 to LExcelData.Count-1 do
        begin
          LLineData.CommaText := LExcelData[LIndex];
          if(LLineData.Count > LMaxCol) then
            LMaxCol := LLineData.Count;
        end;

        LResizeCols := False;
        if(LMaxCol <> (Self.ColCount - Self.FixedCols)) then
        begin
          LDialogResult := MessageDlg(FAppModules.Language.GetString('Message.NumberOfExcelCopiedColumns'),mtConfirmation,mbYesNoCancel,0);
          if(LDialogResult = mrCancel) then  Exit;
          if(LDialogResult = mrYes) and (FResizeColCount) then
            LResizeCols := True;;
        end;

        Self.HideEdit;
        if LResizeRows then
          Self.RowCount := LExcelData.Count + Self.FixedRows;
        if LResizeCols then
          Self.ColCount := Self.FixedCols + LMaxCol;

        if(LExcelData.Count >= Self.RowCount - Self.FixedRows) then
          LRow := Self.FixedRows
        else
          LRow := Self.Row;

        for LIndex := 0 to LExcelData.Count-1 do
        begin
          if(LRow >= Self.RowCount) then
            Break;
          LLineData.CommaText := LExcelData[LIndex];
          if LResizeCols then
            LCol := Self.FixedCols
          else
            LCol := Self.Col; // + Self.FixedCols;
          for LCount := 0 to LLineData.Count -1 do
          begin
            if(LCol >= Self.ColCount) and (LResizeCols) then
              Break;
            Self.Cells[LCol,LRow] := '';
            Self.Cells[LCol,LRow] := Trim(LLineData[LCount]);
            LCol := LCol + 1;
          end;
          LRow := LRow + 1;
        end;
      finally
        LLineData.Free;
      end;
      StoreStartValue;
      Result := True;
    finally
      LExcelData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.SetPopupMenuState;
const OPNAME = 'TFieldStringGrid.SetPopupMenuState';
var
  LCanEdit: boolean;
begin
  try
    if(FGridPopupMenu = nil) then Exit;
    LCanEdit :=  (FAppModules.User.UserRights in CUR_EditData) and (not FAppModules.StudyArea.ScenarioLocked);
    FGridPopupMenu.MenuItemUndo.Enabled   := LCanEdit and (Self.InplaceEditor <> nil) and Self.InplaceEditor.Visible and Self.InplaceEditor.CanUndo;
    FGridPopupMenu.MenuItemCut.Enabled    := LCanEdit and (Self.InplaceEditor <> nil) and Self.InplaceEditor.Visible and (Self.InplaceEditor.SelLength > 0);
    FGridPopupMenu.MenuItemCopy.Enabled   := (Self.InplaceEditor <> nil) and Self.InplaceEditor.Visible and (Self.InplaceEditor.SelLength > 0);;
    FGridPopupMenu.MenuItemPaste.Enabled  := LCanEdit and (Self.InplaceEditor <> nil) and Self.InplaceEditor.Visible and Clipboard.HasFormat(CF_TEXT);
    FGridPopupMenu.MenuItemDelete.Enabled := LCanEdit and FGridPopupMenu.MenuItemCut.Enabled;
    FGridPopupMenu.mnuSelectAll.Enabled   := (Self.InplaceEditor <> nil) and Self.InplaceEditor.Visible and (Self.InplaceEditor.EditText <> '');
    FGridPopupMenu.MenuItemPastefromExcel.Enabled := AllowPasteFromExcel and LCanEdit and Clipboard.HasFormat(CF_TEXT) and ClipboardOwnerIsExcel;
    FGridPopupMenu.MenuItemCopyColumn.Enabled     := (Self.Row >= 0) and ((Self.RowCount - Self.FixedRows) > 1);
    FGridPopupMenu.MenuItemCopyColumn.Visible     := FGridPopupMenu.MenuItemCopyColumn.Enabled;
    FGridPopupMenu.MenuItemPasteColumn.Enabled    := LCanEdit and CanPasteColumnData;
    FGridPopupMenu.MenuItemPasteColumn.Visible    := FGridPopupMenu.MenuItemPasteColumn.Enabled;
    FGridPopupMenu.MenuItemCopyColumnsAndRows.Enabled     := CanCopyColumnsAndRowsData;
    FGridPopupMenu.MenuItemCopyColumnsAndRows.Visible     := FGridPopupMenu.MenuItemCopyColumnsAndRows.Enabled;
    FGridPopupMenu.MenuItemPasteColumnsAndRows.Enabled    := LCanEdit and CanPasteColumnsAndRowsData;
    FGridPopupMenu.MenuItemPasteColumnsAndRows.Visible    := FGridPopupMenu.MenuItemPasteColumnsAndRows.Enabled;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldStringGrid.ClipboardOwnerIsExcel : boolean;
const OPNAME = 'TFieldStringGrid.ClipboardOwnerIsExcel';
var
  PID: THandle; //DWORD;
  LProcessFileName : string;
begin
  Result := False;
  try
    PID := INVALID_HANDLE_VALUE;
    GetWindowThreadProcessID(getClipboardOwner, @PID);
    LProcessFileName := GetProcessFileName(PID);
    Result := Pos('MICROSOFT OFFICE',UpperCase(LProcessFileName)) > 0;
    Result := Result and (UpperCase(ExtractFileName(LProcessFileName)) = 'EXCEL.EXE');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFieldStringGrid.GetProcessFileName(PID: DWORD): string;
const OPNAME = 'TFieldStringGrid.GetProcessFileName';
var
  LHandle    : THandle;
begin
  Result    := '';
  try
    LHandle    := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, PID);
    if LHandle <> 0 then
    begin
      try
        SetLength(Result, MAX_PATH);
        if GetModuleFileNameEx(LHandle, 0, PChar(Result), MAX_PATH) > 0 then
          SetLength(Result, StrLen(PChar(Result)))
        else
          Result := '';
      finally
        CloseHandle(LHandle);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TFieldStringGrid.CreateFieldStringGridPopupMenu;
const OPNAME = 'TFieldStringGrid.CreateFieldStringGridPopupMenu';
begin
  try
    if FShowGridPopupMenu then
    begin
      FGridPopupMenu                                := TFieldStringGridPopupMenu.Create(Self);
      FGridPopupMenu.MenuItemUndo.OnClick           := MenuUndoClick;
      FGridPopupMenu.MenuItemCut.OnClick            := MenuCutClick;
      FGridPopupMenu.MenuItemCopy.OnClick           := MenuCopyClick;
      FGridPopupMenu.MenuItemPaste.OnClick          := MenuPasteClick;
      FGridPopupMenu.MenuItemDelete.OnClick         := MenuDeleteClick;
      FGridPopupMenu.mnuSelectAll.OnClick           := MenuSelectAllClick;
      FGridPopupMenu.MenuItemPastefromExcel.OnClick := MenuPastefromExcelClick;
      FGridPopupMenu.MenuItemCopyColumn.OnClick     := MenuCopyColumnDataClick;
      FGridPopupMenu.MenuItemPasteColumn.OnClick    := MenuPasteColumnDataClick;
      FGridPopupMenu.MenuItemCopyColumnsAndRows.OnClick     := MenuCopyColumnsAndRowsDataClick;
      FGridPopupMenu.MenuItemPasteColumnsAndRows.OnClick    := MenuPasteColumnsAndRowsDataClick;
    end
    else
    begin
      Self.PopupMenu := nil;
      FreeAndNil(FGridPopupMenu);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.SetShowGridPopupMenu(AValue: boolean);
const OPNAME = 'TFieldStringGrid.SetShowGridPopupMenu';
begin
  try
    FShowGridPopupMenu := AValue;
    if FShowGridPopupMenu then
      CreateFieldStringGridPopupMenu;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.SetAllowPasteFromExcel(AValue: boolean);
const OPNAME = 'TFieldStringGrid.SetAllowPasteFromExcel';
begin
  try
    FAllowPasteFromExcel := AValue;
    CreateFieldStringGridPopupMenu;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.MenuCopyColumnDataClick(Sender: TObject);
const OPNAME = 'TFieldStringGrid.MenuCopyColumnDataClick';
var
  LData : TStringList;
  LIndex: integer;
begin
  try
    LData := TStringList.Create;
    try
      LData.CommaText := Self.Cols[Self.Col].CommaText;
      for LIndex := 0 to Self.FixedRows-1 do
        LData.Delete(0);
      Clipboard.AsText := LData.CommaText;
    finally
      LData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.MenuPasteColumnDataClick(Sender: TObject);
const OPNAME = 'TFieldStringGrid.MenuPasteColumnDataClick';
var
  LData : TStringList;
  LIndex: integer;
begin
  try
    if Clipboard.HasFormat(CF_TEXT) then
    begin
      LData := TStringList.Create;
      try
        DoBeforePasteColumnData;
        LData.CommaText := Clipboard.AsText;
        if(Self.RowCount < LData.Count) then
          Self.RowCount := Self.FixedRows + LData.Count;
        for LIndex := 0 to LData.Count-1 do
        begin
          if((LIndex+ Self.FixedRows) < Self.RowCount) then
          begin
            Self.Cells[Self.Col,LIndex+ Self.FixedRows] := LData[LIndex];
          end;
        end;
        DoAfterPasteColumnData;
      finally
        LData.Free;
      end;
    end
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldStringGrid.CanPasteColumnData: boolean;
const OPNAME = 'TFieldStringGrid.CanPasteColumnData';
var
  LData : TStringList;
  LRowData     : TStringList;
  LAllData     : TStringList;
begin
  Result := False;
  try
    if Clipboard.HasFormat(CF_TEXT) then
    begin
      LData := TStringList.Create;
      LRowData := TStringList.Create;
      LAllData := TStringList.Create;
      try
        LAllData.CommaText := Clipboard.AsText;
        LRowData.CommaText := LAllData[0];
        Result := (LRowData.Count = 1) and (Self.Col >= Self.FixedCols);
      finally
        LData.Free;
        LRowData.Free;
        LAllData.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldStringGrid.CanCopyColumnsAndRowsData: boolean;
const OPNAME = 'TFieldStringGrid.CanCopyColumnsAndRowsData';
begin
  Result := False;
  try
    Result := ((Self.ColCount - Self.FixedCols) > 1);
    Result := Result and ((Self.RowCount - Self.FixedRows) >= 1);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldStringGrid.CanPasteColumnsAndRowsData: boolean;
const OPNAME = 'TFieldStringGrid.CanPasteColumnsAndRowsData';
var
  LRowData     : TStringList;
  LAllData     : TStringList;
begin
  Result := False;
  try
    if Clipboard.HasFormat(CF_TEXT) then
    begin
      LRowData  := TStringList.Create;
      LAllData  := TStringList.Create;
      try
        LAllData.CommaText := Clipboard.AsText;
        LRowData.CommaText := LAllData[0];
        Result := ((Self.ColCount - Self.FixedCols) = LRowData.Count);
        Result := Result and ((Self.ColCount - Self.FixedCols) > 1);
      finally
        LAllData.Free;
        LRowData.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.MenuCopyColumnsAndRowsDataClick(Sender: TObject);
const OPNAME = 'TFieldStringGrid.MenuCopyColumnsAndRowsDataClick';
var
  LRowData     : TStringList;
  LAllData     : TStringList;
  LRowIndex,
  LIndex : integer;
begin
  try
    LRowData  := TStringList.Create;
    LAllData  := TStringList.Create;
    try
      Clipboard.Clear;
      for LRowIndex := Self.FixedRows to Self.RowCount - 1 do
      begin
        LRowData.CommaText := Self.Rows[LRowIndex].CommaText;
        for LIndex := 0 to Self.FixedCols-1 do
          LRowData.Delete(0);
        for LIndex := 0 to LRowData.Count-1 do
          LRowData[LIndex] := Trim( LRowData[LIndex]);
        LAllData.Add(LRowData.CommaText);
      end;
      Clipboard.AsText := LAllData.CommaText;
    finally
      LAllData.Free;
      LRowData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.MenuPasteColumnsAndRowsDataClick(Sender: TObject);
const OPNAME = 'TFieldStringGrid.MenuPasteColumnsAndRowsDataClick';
var
  LRowData     : TStringList;
  LAllData     : TStringList;
  LColIndex,
  LRowIndex : integer;
begin
  try
    if Clipboard.HasFormat(CF_TEXT) then
    begin
      LRowData  := TStringList.Create;
      LAllData  := TStringList.Create;
      try
        DoBeforePasteColumnsAndRowsData;
        LAllData.CommaText := Clipboard.AsText;
        if(Self.RowCount < LAllData.Count) then
          Self.RowCount := Self.FixedRows + LAllData.Count;
        for LRowIndex := 0 to LAllData.Count-1 do
        begin
          LRowData.CommaText := LAllData[LRowIndex];
          for LColIndex := 0 to LRowData.Count-1 do
          begin
            Self.Cells[Self.FixedCols+LColIndex,Self.FixedRows+LRowIndex] := LRowData[LColIndex];
          end;
        end;
        DoAfterPasteColumnsAndRowsData;
      finally
        LAllData.Free;
        LRowData.Free;
      end;
    end
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.DoAfterPasteColumnData;
const OPNAME = 'TFieldStringGrid.DoAfterPasteColumnData';
begin
  try
    if((PasteFromExcel) and (Assigned(FAfterPasteColumnData))) then
      FAfterPasteColumnData(Self);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.DoBeforePasteColumnData;
const OPNAME = 'TFieldStringGrid.DoBeforePasteColumnData';
begin
  try
    if Assigned(FBeforePasteColumnData) then
      FBeforePasteColumnData(Self);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.DoBeforePasteColumnsAndRowsData;
const OPNAME = 'TFieldStringGrid.DoBeforePasteColumnsAndRowsData';
begin
  try
    if Assigned(FBeforePasteColumnsAndRowsData) then
      FBeforePasteColumnsAndRowsData(Self);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldStringGrid.DoAfterPasteColumnsAndRowsData;
const OPNAME = 'TFieldStringGrid.DoAfterPasteColumnsAndRowsData';
begin
  try
    if Assigned(FAfterPasteColumnsAndRowsData) then
      FAfterPasteColumnsAndRowsData(Self);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TFieldComboBox }

procedure TFieldComboBox.CreateMemberObjects;
const OPNAME = 'TFieldComboBox.CreateMemberObjects';
begin
  inherited;
  try
    FOldIndex          := -1;
    FValidationError   := '';
    FInValidationError := False;
    FHasChanges        := False;
    FHasMetaData       := FALSE;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldComboBox.DestroyMemberObjects;
const OPNAME = 'TFieldComboBox.DestroyMemberObjects';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
  inherited;
end;

procedure TFieldComboBox.DoEnter;
const OPNAME = 'TFieldComboBox.DoEnter';
begin
  if Application.Terminated then Exit;
  inherited;
  try
    FOldIndex  := Self.ItemIndex;
    if (Assigned(FAppModules.Changes()) AND
        Assigned(FFieldProperty) AND
        FFieldProperty.InChangeList) then
      FAppModules.Changes.SetParameterChanges(TRUE)
    else
      FAppModules.Changes.SetParameterChanges(FALSE);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldComboBox.DoExit;
const OPNAME = 'TFieldComboBox.DoExit';
begin
  if Application.Terminated then Exit;
  inherited;
  try
    if (not HasValueChanged) and (not InValidationError) then
       FValidationError := '';

    if(FValidationError = '') then
      ShowErrorState(FALSE)
    else
      ShowErrorState(True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldComboBox.HasValueChanged: boolean;
const OPNAME = 'TFieldComboBox.HasValueChanged';
begin
  Result := False;
  try
    Result := (FOldIndex  <> Self.ItemIndex);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldComboBox.HotSpotChangesClicked(X, Y: Integer): boolean;
const OPNAME = 'TFieldComboBox.HotSpotChangesClicked';
var
  lSize : integer;
begin
  Result := False;
  try
    lSize := 7;//Round(Self.Height / 3);
    Result := (X >= 0) and
              (Y >= 0) and
              (X <= lSize) and
              (Y <=  lSize);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldComboBox.HotSpotMetaDataClicked(X, Y: Integer): boolean;
const OPNAME = 'TFieldComboBox.HotSpotMetaDataClicked';
var
  lSize : integer;
begin
  Result := False;
  try
    lSize := 7;//Round(Self.Height / 3);
    Result := (X >= 0) and
              (Y >= 0) and
              (X <= lSize) and
              (Y >=  Self.Height - lSize);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldComboBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TFieldComboBox.MouseDown';
begin
  try
    if (Button = mbLeft) then
    begin
      if (HotSpotChangesClicked(X, Y)) AND (HasChanges) then
        FAppModules.Model.ProcessEvent(CmeChangeParameter,nil)
      else
      if (HotSpotMetaDataClicked(X,Y)) AND (HasMetaData) then
        FAppModules.Model.ProcessEvent(CmeMetaData,nil)
      else
        inherited MouseDown(Button,Shift,X, Y);
    end
    else
      inherited MouseDown(Button,Shift,X, Y);
{    if (Button = mbLeft) then
    begin
      if HotSpotChangesClicked(X, Y) then
      begin
        if HasChanges then
          FAppModules.Model.ProcessEvent(CmeChangeParameter,nil)
        else
          inherited MouseDown(Button,Shift,X, Y);
      end
      else
      begin
        if HotSpotMetaDataClicked(X,Y) and  HasMetaData then
          FAppModules.Model.ProcessEvent(CmeMetaData,nil)
        else
          inherited MouseDown(Button,Shift,X, Y);
      end;
    end
    else
    if (IsEnabled) AND (NOT HasChanges) then
      inherited MouseDown(Button,Shift,X, Y);}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldComboBox.PaintChangesAndMetaData;
const OPNAME = 'TFieldComboBox.PaintChangesAndMetaData';
var
  LCanvas: TCanvas;
  lSize  : integer;
  LDC    : HDC;
begin
  try
    LDC := GetDC(Self.Handle);
    try
      LCanvas := TCanvas.Create;
      try
        LCanvas.Handle := LDC;
        lSize := 7;//Round(Self.Height / 3);
        if HasChanges then
        begin
            LCanvas.Brush.Color := clLime;
            LCanvas.Pen.Color   := clBlack;
            LCanvas.Polygon([Point(Self.ClientRect.Left, Self.ClientRect.Top),
                             Point(Self.ClientRect.Left + lSize, Self.ClientRect.Top),
                             Point(Self.ClientRect.Left, Self.ClientRect.Top + lSize)]);
        end;

        if HasMetaData then
        begin
            LCanvas.Brush.Color := clAqua;
            LCanvas.Pen.Color   := clBlack;
            LCanvas.Polygon([Point(Self.ClientRect.Left,         Self.ClientRect.Bottom),
                             Point(Self.ClientRect.Left + lSize, Self.ClientRect.Bottom),
                             Point(Self.ClientRect.Left,         Self.ClientRect.Bottom - lSize)]);
        end;
      finally
        LCanvas.Free;
      end;
    finally
      ReleaseDC(Self.Handle, LDC);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldComboBox.SetFieldIndex(AIndex: integer);
const OPNAME = 'TFieldComboBox.SetFieldIndex';
begin
  try
    if(AIndex >= 0) and (AIndex < Self.Items.Count) then
    begin
      Self.ItemIndex := AIndex;
      Self.Text      := Self.Items[AIndex];
      FOldIndex      := AIndex;
    end
    else
    begin
      Self.ItemIndex := FOldIndex;
      Self.Text      := Self.Items[AIndex];
    end;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldComboBox.SetFieldProperty(AFieldProperty: TAbstractFieldProperty);
const OPNAME = 'TFieldComboBox.SetFieldProperty';
begin
 try
   FFieldProperty := AFieldProperty;
   if assigned(FFieldProperty) then
     IsEnabled := FAppModules.FieldProperties.ModeIsEditable(AFieldProperty.FieldName)
   else
     IsEnabled := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldComboBox.SetHasChanges (AHasChanges : boolean);
const OPNAME = 'TFieldComboBox.SetHasChanges';
begin
  try
    FHasChanges := AHasChanges;
    Self.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldComboBox.SetHasMetaData(AHasMetaData: boolean);
const OPNAME = 'TFieldComboBox.SetHasMetaData';
begin
  try
    FHasMetaData := AHasMetaData;
    Self.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldComboBox.SetIsEnabled(AIsEnabled: boolean);
const OPNAME = 'TFieldComboBox.SetIsEnabled';
begin
  try
    if Assigned(FFieldProperty) then
      AIsEnabled := AIsEnabled and (FAppModules.FieldProperties.ModeIsEditable(FFieldProperty.FieldName))
    else
      AIsEnabled := False;
    FIsEnabled := AIsEnabled;
    Self.Enabled := FIsEnabled;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldComboBox.ShowErrorState(AError: Boolean);
const OPNAME = 'TFieldComboBox.ShowErrorState';
begin
  try
    if (AError) then
    begin
      if InValidationError then
       Color := clTeal
      else
       Color := clRed;
    end
    else
      Color := clWindow;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldComboBox.WMPAINT(var Message: TMessage);
const OPNAME = 'TFieldComboBox.WMPAINT';
begin
  inherited;
  try
    PaintChangesAndMetaData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldComboBox.Change;
const OPNAME = 'TFieldComboBox.Change';
begin
  try
    if(ItemIndex = -1) then
      inherited Change
    else
    if(ItemIndex = FOldIndex) then
      inherited Change
    else
    if (not IsEnabled) or HasChanges then
      ItemIndex := FOldIndex
    else
      inherited Change;
  except on E: Exception do HandleError(E, OPNAME) end;

end;

function TFieldComboBox.Initialise: boolean;
const OPNAME = 'TFieldComboBox.Initialise';
begin
  Result := inherited Initialise;
  try
    ItemIndex := -1;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TFieldChkBox }

procedure TFieldChkBox.CreateMemberObjects;
const OPNAME = 'TFieldChkBox.CreateMemberObjects';
begin
  inherited;
  try
    ClicksDisabled     := True;
    FXPos              := -1;
    FYPos              := -1;
    FPrevValue         := False;
    FChkBoxIdentifier  := giUnknown;
    FFieldProperty     := nil;
    FValidationError   := '';
    FInValidationError := False;
    FHasChanges        := False;
    FHasMetaData       := FALSE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldChkBox.DoEnter;
const OPNAME = 'TFieldChkBox.DoEnter';
begin
  ClicksDisabled     := False;
  inherited;
  try
    FPrevValue := Self.Checked;
    if (Assigned(FAppModules.Changes()) AND
        Assigned(FFieldProperty) AND
        FFieldProperty.InChangeList) then
      FAppModules.Changes.SetParameterChanges(TRUE)
    else
      FAppModules.Changes.SetParameterChanges(FALSE);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldChkBox.Click;
const OPNAME = 'TFieldChkBox.Click';
begin
  inherited;
  try
    FPrevValue     := Self.Checked;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldChkBox.DoExit;
const OPNAME = 'TFieldChkBox.DoExit';
begin
  if Application.Terminated then Exit;
  inherited;
  try
    FXPos              := -1;
    FYPos              := -1;
    if (not HasValueChanged) and (not InValidationError) then
       FValidationError := '';

    if(FValidationError = '') then
      ShowErrorState(FALSE)
    else
      ShowErrorState(True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldChkBox.HasValueChanged: boolean;
const OPNAME = 'TFieldChkBox.HasValueChanged';
begin
  Result := False;
  try
    Result := (FPrevValue <> Self.Checked);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldChkBox.HotSpotChangesClicked(X, Y: Integer): boolean;
const OPNAME = 'TFieldChkBox.HotSpotChangesClicked';
{var
  lSize : integer;}
begin
  Result := False;
  try
    {lSize := 7;//Round(Self.Height / 3);
    Result := (X >= 0) and
              (Y >= 0) and
              (X <= lSize) and
              (Y <=  lSize) ;}
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldChkBox.HotSpotMetaDataClicked(X, Y: Integer): boolean;
const OPNAME = 'TFieldChkBox.HotSpotMetaDataClicked';
{var
  lSize : integer;}
begin
  Result := False;
  try
    {lSize := 7;//Round(Self.Height / 3);
    Result := (X >= 0) and
              (Y >= 0) and
              (X <= lSize) and
              (Y >=  Self.Height - lSize);}
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldChkBox.MouseDown(Button: TMouseButton; Shift: TShiftState;  X, Y: Integer);
const OPNAME = 'TFieldChkBox.MouseDown';
begin
  try
    if (Button = mbLeft) then
    begin
      if HotSpotChangesClicked(X, Y) then
      begin
        if HasChanges then
          FAppModules.Model.ProcessEvent(CmeChangeParameter,nil)
        else
          inherited MouseDown(Button,Shift,X, Y);
      end
      else
      begin
        if HotSpotMetaDataClicked(X,Y) and  HasMetaData then
          FAppModules.Model.ProcessEvent(CmeMetaData,nil)
        else
          inherited MouseDown(Button,Shift,X, Y);
      end;
    end
    else
      inherited MouseDown(Button,Shift,X, Y);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldChkBox.MouseMove(Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TFieldChkBox.MouseMove';
begin
  inherited;
  try
    FXPos  := X;
    FYPos  := Y;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldChkBox.PaintChangesAndMetaData;
const OPNAME = 'TFieldChkBox.PaintChangesAndMetaData';
var
  LCanvas: TCanvas;
  lSize  : integer;
  LDC    : HDC;
begin
  try
    LDC := GetDC(Self.Handle);
    try
      LCanvas := TCanvas.Create;
      try
        LCanvas.Handle := LDC;
        lSize := 7;//Round(Self.Height / 3);
        if HasChanges then
        begin
            LCanvas.Brush.Color := clLime;
            LCanvas.Pen.Color   := clBlack;
            LCanvas.Polygon([Point(Self.ClientRect.Left, Self.ClientRect.Top),
                             Point(Self.ClientRect.Left + lSize, Self.ClientRect.Top),
                             Point(Self.ClientRect.Left, Self.ClientRect.Top + lSize)]);
        end;

        if HasMetaData then
        begin
            LCanvas.Brush.Color := clAqua;
            LCanvas.Pen.Color   := clBlack;
            LCanvas.Polygon([Point(Self.ClientRect.Left,         Self.ClientRect.Bottom),
                             Point(Self.ClientRect.Left + lSize, Self.ClientRect.Bottom),
                             Point(Self.ClientRect.Left,         Self.ClientRect.Bottom - lSize)]);
        end;
      finally
        LCanvas.Free;
      end;
    finally
      ReleaseDC(Self.Handle, LDC);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldChkBox.SetFieldProperty(AFieldProperty: TAbstractFieldProperty);
const OPNAME = 'TFieldChkBox.SetFieldProperty';
begin
 try
   FFieldProperty := AFieldProperty;
   if assigned(FFieldProperty) then
     IsEnabled := FAppModules.FieldProperties.ModeIsEditable(AFieldProperty.FieldName)
   else
     IsEnabled := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldChkBox.SetHasChanges(AHasChanges: boolean);
const OPNAME = 'TFieldChkBox.SetHasChanges';
begin
  try
    FHasChanges := AHasChanges;
    Self.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldChkBox.SetHasMetaData(AHasMetaData: boolean);
const OPNAME = 'TFieldChkBox.SetHasMetaData';
begin
  try
    FHasMetaData := AHasMetaData;
    Self.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldChkBox.SetIsEnabled(AIsEnabled: boolean);
const OPNAME = 'TFieldChkBox.SetIsEnabled';
begin
  try
    if Assigned(FFieldProperty) then
      AIsEnabled := AIsEnabled and (FAppModules.FieldProperties.ModeIsEditable(FFieldProperty.FieldName))
    else
      AIsEnabled := False;
    FIsEnabled := AIsEnabled;
    Self.Enabled := FIsEnabled;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldChkBox.ShowErrorState(AError: Boolean);
const OPNAME = 'TFieldChkBox.ShowErrorState';
begin
  try
    if not Enabled then
      Exit;
    if (AError) then
    begin
      if InValidationError then
       Color := clTeal
      else
       Color := clRed;
    end
    else
      Color := clBtnFace;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldChkBox.Toggle;
const OPNAME = 'TFieldChkBox.Toggle';
begin
  try
    if HotSpotChangesClicked(FXPos,FYPos) then Exit;
    if HotSpotMetaDataClicked(FXPos,FYPos) then Exit;
    if (IsEnabled) AND (NOT HasChanges) then
      inherited Toggle;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldChkBox.WMPAINT(var Message: TMessage);
const OPNAME = 'TFieldChkBox.WMPAINT';
begin
  inherited;
  try
    PaintChangesAndMetaData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TFieldCheckListBox }

procedure TFieldCheckListBox.CreateMemberObjects;
const OPNAME = 'TFieldCheckListBox.CreateMemberObjects';
begin
  inherited;
  try
    FFieldProperty     := nil;
    FValidationError   := '';
    FInValidationError := False;
    FHasChanges        := False;
    FHasMetaData       := FALSE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldCheckListBox.DestroyMemberObjects;
const OPNAME = 'TFieldCheckListBox.DestroyMemberObjects';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
  inherited;
end;

procedure TFieldCheckListBox.DoEnter;
const OPNAME = 'TFieldCheckListBox.DoEnter';
var
  LCount: integer;
begin
  if Application.Terminated then Exit;
  inherited;
  try
    FOldIndex  := '';
    for LCount := 0 to Self.Items.Count -1 do
      if Self.Checked[LCount] then
       FOldIndex := FOldIndex + IntToStr(LCount)+',';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldCheckListBox.Click;
const OPNAME = 'TFieldCheckListBox.Click';
var
  LCount: integer;
begin
  inherited;
  try
    FOldIndex  := '';
    for LCount := 0 to Self.Items.Count - 1 do
      if Self.Checked[LCount] then
       FOldIndex := FOldIndex + IntToStr(LCount) + ',';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldCheckListBox.DoExit;
const OPNAME = 'TFieldCheckListBox.DoExit';
begin
  if Application.Terminated then Exit;
  try
    if (not InValidationError) then
       FValidationError := '';

    if(FValidationError = '') then
      ShowErrorState(FALSE)
    else
      ShowErrorState(True);
    inherited;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldCheckListBox.HasValueChanged: boolean;
const OPNAME = 'TFieldCheckListBox.HasValueChanged';
var
  LCurrentIndex: string;
  LCount: integer;
begin
  inherited;
  Result := False;
  try
    LCurrentIndex  := '';
    for LCount := 0 to Self.Items.Count -1 do
      if Self.Checked[LCount] then
       LCurrentIndex := LCurrentIndex + IntToStr(LCount)+',';
    Result := (LCurrentIndex <> FOldIndex);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldCheckListBox.HotSpotChangesClicked(X, Y: Integer): boolean;
const OPNAME = 'TFieldCheckListBox.HotSpotChangesClicked';
var
  lSize : integer;
begin
  Result := False;
  try
    lSize := 7;//Round(Self.Height / 3);
    Result := (X >= 0) and
              (Y >= 0) and
              (X <= lSize) and
              (Y <=  lSize);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldCheckListBox.HotSpotMetaDataClicked(X, Y: Integer): boolean;
const OPNAME = 'TFieldCheckListBox.HotSpotMetaDataClicked';
var
  lSize : integer;
begin
  Result := False;
  try
    lSize := 7;//Round(Self.Height / 3);
    Result := (X >= 0) and
              (Y >= 0) and
              (X <= lSize) and
              (Y >=  Self.Height - lSize);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldCheckListBox.MouseDown(Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TFieldCheckListBox.MouseDown';
begin
  try
    if (Button = mbLeft) then
    begin
      if HotSpotChangesClicked(X, Y) then
      begin
        if HasChanges then
          FAppModules.Model.ProcessEvent(CmeChangeParameter,nil)
        else
          inherited MouseDown(Button,Shift,X, Y);
      end
      else
      begin
        if HotSpotMetaDataClicked(X,Y) and  HasMetaData then
          FAppModules.Model.ProcessEvent(CmeMetaData,nil)
        else
          inherited MouseDown(Button,Shift,X, Y);
      end;
    end
    else
      inherited MouseDown(Button,Shift,X, Y);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldCheckListBox.PaintChangesAndMetaData;
const OPNAME = 'TFieldCheckListBox.PaintChangesAndMetaData';
var
  LCanvas: TCanvas;
  lSize  : integer;
  LDC    : HDC;
begin
  try
    LDC := GetDC(Self.Handle);
    try
      LCanvas := TCanvas.Create;
      try
        LCanvas.Handle := LDC;
        lSize := 7;//Round(Self.Height / 3);
        if HasChanges then
        begin
            LCanvas.Brush.Color := clLime;
            LCanvas.Pen.Color   := clBlack;
            LCanvas.Polygon([Point(Self.ClientRect.Left, Self.ClientRect.Top),
                             Point(Self.ClientRect.Left + lSize, Self.ClientRect.Top),
                             Point(Self.ClientRect.Left, Self.ClientRect.Top + lSize)]);
        end;

        if HasMetaData then
        begin
            LCanvas.Brush.Color := clAqua;
            LCanvas.Pen.Color   := clBlack;
            LCanvas.Polygon([Point(Self.ClientRect.Left,         Self.ClientRect.Bottom),
                             Point(Self.ClientRect.Left + lSize, Self.ClientRect.Bottom),
                             Point(Self.ClientRect.Left,         Self.ClientRect.Bottom - lSize)]);
        end;
      finally
        LCanvas.Free;
      end;
    finally
      ReleaseDC(Self.Handle, LDC);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldCheckListBox.SetFieldProperty(AFieldProperty: TAbstractFieldProperty);
const OPNAME = 'TFieldCheckListBox.SetFieldProperty';
begin
 try
   FFieldProperty := AFieldProperty;
   if assigned(FFieldProperty) then
     IsEnabled := FAppModules.FieldProperties.ModeIsEditable(AFieldProperty.FieldName)
   else
     IsEnabled := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldCheckListBox.SetHasChanges(AHasChanges: boolean);
const OPNAME = 'TFieldCheckListBox.SetHasChanges';
begin
  try
    FHasChanges := AHasChanges;
    Self.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldCheckListBox.SetHasMetaData(AHasMetaData: boolean);
const OPNAME = 'TFieldCheckListBox.SetHasMetaData';
begin
  try
    FHasMetaData := AHasMetaData;
    Self.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldCheckListBox.SetIsEnabled(AIsEnabled: boolean);
const OPNAME = 'TFieldCheckListBox.SetIsEnabled';
begin
  try
    if Assigned(FFieldProperty) then
      AIsEnabled := AIsEnabled and (FAppModules.FieldProperties.ModeIsEditable(FFieldProperty.FieldName))
    else
      AIsEnabled := False;
    FIsEnabled := AIsEnabled;
    Self.Enabled := FIsEnabled;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldCheckListBox.ShowErrorState(AError: Boolean);
const OPNAME = 'TFieldCheckListBox.ShowErrorState';
begin
  try
    if not Enabled then
      Exit;
    if (AError) then
    begin
      if InValidationError then
       Color := clTeal
      else
       Color := clRed;
    end
    else
      Color := clWindow;
      
    Invalidate;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldCheckListBox.WMPAINT(var Message: TMessage);
const OPNAME = 'TFieldCheckListBox.WMPAINT';
begin
  inherited;
  try
    PaintChangesAndMetaData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TFieldDateTimePicker }

procedure TFieldDateTimePicker.CreateMemberObjects;
const OPNAME = 'TFieldDateTimePicker.CreateMemberObjects';
begin
  inherited;
  try
    FPrevValue         := 0.0;
    FFieldProperty     := nil;
    FValidationError   := '';
    FInValidationError := False;
    FHasChanges        := False;
    FHasMetaData       := FALSE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldDateTimePicker.DestroyMemberObjects;
const OPNAME = 'TFieldDateTimePicker.DestroyMemberObjects';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
  inherited;
end;

procedure TFieldDateTimePicker.DoEnter;
const OPNAME = 'TFieldDateTimePicker.DoEnter';
begin
  if Application.Terminated then Exit;
  inherited;
  try
    FPrevValue         :=Self.DateTime;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldDateTimePicker.DoExit;
const OPNAME = 'TFieldDateTimePicker.DoExit';
begin
  if Application.Terminated then Exit;
  inherited;
  try
    if (not HasValueChanged) and (not InValidationError) then
       FValidationError := '';

    if(FValidationError = '') then
      ShowErrorState(FALSE)
    else
      ShowErrorState(True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldDateTimePicker.HasValueChanged: boolean;
const OPNAME = 'TFieldDateTimePicker.HasValueChanged';
begin
  inherited;
  Result := False;
  try
    Result := (FPrevValue <> Self.DateTime);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldDateTimePicker.HotSpotChangesClicked(X, Y: Integer): boolean;
const OPNAME = 'TFieldDateTimePicker.HotSpotChangesClicked';
var
  lSize : integer;
begin
  Result := False;
  try
    lSize := 7;//Round(Self.Height / 3);
    Result := (X >= 0) and
              (Y >= 0) and
              (X <= lSize) and
              (Y <=  lSize);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldDateTimePicker.HotSpotMetaDataClicked(X, Y: Integer): boolean;
const OPNAME = 'TFieldDateTimePicker.HotSpotMetaDataClicked';
var
  lSize : integer;
begin
  Result := False;
  try
    lSize := 7;//Round(Self.Height / 3);
    Result := (X >= 0) and
              (Y >= 0) and
              (X <= lSize) and
              (Y >=  Self.Height - lSize);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldDateTimePicker.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TFieldDateTimePicker.MouseDown';
begin
  try
    if (Button = mbLeft) then
    begin
      if HotSpotChangesClicked(X, Y) then
      begin
        if HasChanges then
          FAppModules.Model.ProcessEvent(CmeChangeParameter,nil)
        else
          inherited MouseDown(Button,Shift,X, Y);
      end
      else
      begin
        if HotSpotMetaDataClicked(X,Y) and  HasMetaData then
          FAppModules.Model.ProcessEvent(CmeMetaData,nil)
        else
          inherited MouseDown(Button,Shift,X, Y);
      end;
    end
    else
      inherited MouseDown(Button,Shift,X, Y);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldDateTimePicker.PaintChangesAndMetaData;
const OPNAME = 'TFieldDateTimePicker.PaintChangesAndMetaData';
var
  LCanvas: TCanvas;
  lSize  : integer;
  LDC    : HDC;
begin
  try
    LDC := GetDC(Self.Handle);
    try
      LCanvas := TCanvas.Create;
      try
        LCanvas.Handle := LDC;
        lSize := 7;//Round(Self.Height / 3);
        if HasChanges then
        begin
            LCanvas.Brush.Color := clLime;
            LCanvas.Pen.Color   := clBlack;
            LCanvas.Polygon([Point(Self.ClientRect.Left, Self.ClientRect.Top),
                             Point(Self.ClientRect.Left + lSize, Self.ClientRect.Top),
                             Point(Self.ClientRect.Left, Self.ClientRect.Top + lSize)]);
        end;

        if HasMetaData then
        begin
            LCanvas.Brush.Color := clAqua;
            LCanvas.Pen.Color   := clBlack;
            LCanvas.Polygon([Point(Self.ClientRect.Left,         Self.ClientRect.Bottom),
                             Point(Self.ClientRect.Left + lSize, Self.ClientRect.Bottom),
                             Point(Self.ClientRect.Left,         Self.ClientRect.Bottom - lSize)]);
        end;
      finally
        LCanvas.Free;
      end;
    finally
      ReleaseDC(Self.Handle, LDC);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldDateTimePicker.SetFieldProperty(AFieldProperty: TAbstractFieldProperty);
const OPNAME = 'TFieldDateTimePicker.SetFieldProperty';
begin
 try
   FFieldProperty := AFieldProperty;
   if assigned(FFieldProperty) then
     IsEnabled := FAppModules.FieldProperties.ModeIsEditable(AFieldProperty.FieldName)
   else
     IsEnabled := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldDateTimePicker.SetHasChanges(AHasChanges: boolean);
const OPNAME = 'TFieldDateTimePicker.SetHasChanges';
begin
  try
    FHasChanges := AHasChanges;
    Self.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldDateTimePicker.SetHasMetaData(AHasMetaData: boolean);
const OPNAME = 'TFieldDateTimePicker.SetHasMetaData';
begin
  try
    FHasMetaData := AHasMetaData;
    Self.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldDateTimePicker.SetIsEnabled(AIsEnabled: boolean);
const OPNAME = 'TFieldDateTimePicker.SetIsEnabled';
begin
  try
    if Assigned(FFieldProperty) then
      AIsEnabled := AIsEnabled and (FAppModules.FieldProperties.ModeIsEditable(FFieldProperty.FieldName))
    else
      AIsEnabled := False;
    FIsEnabled := AIsEnabled;
    Self.Enabled := FIsEnabled;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldDateTimePicker.ShowErrorState(AError: Boolean);
const OPNAME = 'TFieldDateTimePicker.ShowErrorState';
begin
  try
    if not Enabled then
      Exit;
    if (AError) then
    begin
      if InValidationError then
       Color := clTeal
      else
       Color := clRed;
    end
    else
      Color := clWindow;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldDateTimePicker.WMPAINT(var Message: TMessage);
const OPNAME = 'TFieldDateTimePicker.WMPAINT';
begin
  inherited;
  try
    PaintChangesAndMetaData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TFieldRadioGroup }

procedure TFieldRadioGroup.DoEnter;
const OPNAME = 'TFieldRadioGroup.DoEnter';
begin
  if Application.Terminated then Exit;
  inherited;
  try
    FOldIndex := Self.ItemIndex;
    if (Assigned(FAppModules.Changes()) AND
        Assigned(FFieldProperty) AND
        FFieldProperty.InChangeList) then
      FAppModules.Changes.SetParameterChanges(TRUE)
    else
      FAppModules.Changes.SetParameterChanges(FALSE);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRadioGroup.Click;
const OPNAME = 'TFieldRadioGroup.Click';
begin
  inherited;
  try
    FOldIndex := Self.ItemIndex;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRadioGroup.DoExit;
const OPNAME = 'TFieldRadioGroup.DoExit';
begin
  if Application.Terminated then Exit;
  inherited;
  try
    if (not HasValueChanged) and (not InValidationError) then
       FValidationError := '';

    if(FValidationError = '') then
      ShowErrorState(FALSE)
    else
      ShowErrorState(True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldRadioGroup.HasValueChanged: boolean;
const OPNAME = 'TFieldRadioGroup.HasValueChanged';
begin
  Result := False;
  try
    Result := (FOldIndex <> Self.ItemIndex);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRadioGroup.ShowErrorState(AError: Boolean);
const OPNAME = 'TFieldRadioGroup.ShowErrorState';
begin
  try
    if not Enabled then
      Exit;
    if (AError) then
    begin
      if InValidationError then
       Color := clTeal
      else
       Color := clRed;
    end
    else
       Color :=  clBtnFace;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRadioGroup.SetFieldProperty(AFieldProperty: TAbstractFieldProperty);
const OPNAME = 'TFieldRadioGroup.SetFieldProperty';
begin
 try
   FFieldProperty := AFieldProperty;
   if assigned(FFieldProperty) then
     IsEnabled := FAppModules.FieldProperties.ModeIsEditable(AFieldProperty.FieldName)
   else
     IsEnabled := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRadioGroup.SetIsEnabled(AIsEnabled: boolean);
const OPNAME = 'TFieldRadioGroup.SetIsEnabled';
begin
  try
    if Assigned(FFieldProperty) then
      AIsEnabled := AIsEnabled and (FAppModules.FieldProperties.ModeIsEditable(FFieldProperty.FieldName))
    else
      AIsEnabled := False;
    FIsEnabled := AIsEnabled;
    Self.Enabled := FIsEnabled;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRadioGroup.DestroyMemberObjects;
const OPNAME = 'TFieldRadioGroup.DestroyMemberObjects';
begin
  try
    FreeAndNil(FHints);
  except on E: Exception do HandleError(E, OPNAME) end;
  inherited;
end;

function TFieldRadioGroup.HotSpotChangesClicked(X, Y: Integer): boolean;
const OPNAME = 'TFieldRadioGroup.HotSpotChangesClicked';
var
  lSize : integer;
begin
  Result := False;
  try
    lSize := 7;//Round(Self.Height / 3);
    Result := (X >= 0) and
              (Y >= 0) and
              (X <= lSize) and
              (Y <=  lSize);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldRadioGroup.HotSpotMetaDataClicked(X, Y: Integer): boolean;
const OPNAME = 'TFieldRadioGroup.HotSpotMetaDataClicked';
var
  lSize : integer;
begin
  Result := False;
  try
    lSize := 7;//Round(Self.Height / 3);
    Result := (X >= 0) and
              (Y >= 0) and
              (X <= lSize) and
              (Y >=  Self.Height - lSize);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRadioGroup.MouseDown(Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TFieldRadioGroup.MouseDown';
begin
  try
    inherited MouseDown(Button,Shift,X, Y);
    DoEnter;
    Self.SetFocus;
    if (Button = mbLeft) then
    begin
      if HotSpotChangesClicked(X, Y) AND HasChanges then
        FAppModules.Model.ProcessEvent(CmeChangeParameter,nil)
      else
      if HotSpotMetaDataClicked(X,Y) AND HasMetaData then
        FAppModules.Model.ProcessEvent(CmeMetaData,nil);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRadioGroup.PaintChangesAndMetaData;
const OPNAME = 'TFieldRadioGroup.PaintChangesAndMetaData';
var
  LCanvas: TCanvas;
  lSize  : integer;
  LDC    : HDC;
begin
  try
    LDC := GetDC(Self.Handle);
    try
      LCanvas := TCanvas.Create;
      try
        LCanvas.Handle := LDC;
        lSize := 7;//Round(Self.Height / 3);
        if HasChanges then
        begin
            LCanvas.Brush.Color := clLime;
            LCanvas.Pen.Color   := clBlack;
            LCanvas.Polygon([Point(Self.ClientRect.Left, Self.ClientRect.Top),
                             Point(Self.ClientRect.Left + lSize, Self.ClientRect.Top),
                             Point(Self.ClientRect.Left, Self.ClientRect.Top + lSize)]);
        end;

        if HasMetaData then
        begin
            LCanvas.Brush.Color := clAqua;
            LCanvas.Pen.Color   := clBlack;
            LCanvas.Polygon([Point(Self.ClientRect.Left,         Self.ClientRect.Bottom),
                             Point(Self.ClientRect.Left + lSize, Self.ClientRect.Bottom),
                             Point(Self.ClientRect.Left,         Self.ClientRect.Bottom - lSize)]);
        end;
      finally
        LCanvas.Free;
      end;
    finally
      ReleaseDC(Self.Handle, LDC);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRadioGroup.SetHasChanges(AHasChanges: boolean);
const OPNAME = 'TFieldRadioGroup.SetHasChanges';
begin
  try
    FHasChanges := AHasChanges;
    Self.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRadioGroup.SetHasMetaData(AHasMetaData: boolean);
const OPNAME = 'TFieldRadioGroup.SetHasMetaData';
begin
  try
    FHasMetaData := AHasMetaData;
    Self.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRadioGroup.WMPAINT(var Message: TMessage);
const OPNAME = 'TFieldRadioGroup.WMPAINT';
begin
  inherited;
  try
    PaintChangesAndMetaData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldRadioGroup.CanModify: Boolean;
const OPNAME = 'TFieldRadioGroup.CanModify';
begin
  Result := False;
  try
    Result := IsEnabled and (not HasChanges);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TFieldRadioButton }

procedure TFieldRadioGroup.CreateMemberObjects;
const OPNAME = 'TFieldRadioGroup.CreateMemberObjects';
begin
  inherited;
  try
    FFieldProperty     := nil;
    FValidationError   := '';
    FInValidationError := False;
    FHints             := TStringList.Create;
    FHasChanges        := False;
    FHasMetaData       := FALSE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRadioButton.CreateMemberObjects;
const OPNAME = 'TFieldRadioButton.CreateMemberObjects';
begin
  inherited;
  try
    FPrevValue         := False;
    FFieldProperty     := nil;
    FValidationError   := '';
    FInValidationError := False;
    FHasChanges        := False;
    FHasMetaData       := FALSE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRadioButton.DestroyMemberObjects;
const OPNAME = 'TFieldRadioButton.DestroyMemberObjects';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
  inherited;
end;

procedure TFieldRadioButton.DoEnter;
const OPNAME = 'TFieldRadioButton.DoEnter';
begin
  if Application.Terminated then Exit;
  inherited;
  try
    FPrevValue := Self.Checked;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRadioButton.DoExit;
const OPNAME = 'TFieldRadioButton.DoExit';
begin
  if Application.Terminated then Exit;
  inherited;
  try
    if (not HasValueChanged) and (not InValidationError) then
       FValidationError := '';

    if(FValidationError = '') then
      ShowErrorState(FALSE)
    else
      ShowErrorState(True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldRadioButton.HasValueChanged: boolean;
const OPNAME = 'TFieldRadioButton.HasValueChanged';
begin
  Result := False;
  try
    Result := (FPrevValue <> Self.Checked);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldRadioButton.HotSpotChangesClicked(X, Y: Integer): boolean;
const OPNAME = 'TFieldRadioButton.HotSpotChangesClicked';
var
  lSize : integer;
begin
  Result := False;
  try
    lSize := 7;//Round(Self.Height / 3);
    Result := (X >= 0) and
              (Y >= 0) and
              (X <= lSize) and
              (Y <=  lSize);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldRadioButton.HotSpotMetaDataClicked(X, Y: Integer): boolean;
const OPNAME = 'TFieldRadioButton.HotSpotMetaDataClicked';
var
  lSize : integer;
begin
  Result := False;
  try
    lSize := 7;//Round(Self.Height / 3);
    Result := (X >= 0) and
              (Y >= 0) and
              (X <= lSize) and
              (Y >=  Self.Height - lSize);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRadioButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TFieldRadioButton.MouseDown';
begin
  try
    if (Button = mbLeft) then
    begin
      if HotSpotChangesClicked(X, Y) then
      begin
        if HasChanges then
          FAppModules.Model.ProcessEvent(CmeChangeParameter,nil)
        else
          inherited MouseDown(Button,Shift,X, Y);
      end
      else
      begin
        if HotSpotMetaDataClicked(X,Y) and  HasMetaData then
          FAppModules.Model.ProcessEvent(CmeMetaData,nil)
        else
          inherited MouseDown(Button,Shift,X, Y);
      end;
    end
    else
      inherited MouseDown(Button,Shift,X, Y);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRadioButton.PaintChangesAndMetaData;
const OPNAME = 'TFieldRadioButton.PaintChangesAndMetaData';
var
  LCanvas: TCanvas;
  lSize  : integer;
  LDC    : HDC;
begin
  try
    LDC := GetDC(Self.Handle);
    try
      LCanvas := TCanvas.Create;
      try
        LCanvas.Handle := LDC;
        lSize := 7;//Round(Self.Height / 3);
        if HasChanges then
        begin
            LCanvas.Brush.Color := clLime;
            LCanvas.Pen.Color   := clBlack;
            LCanvas.Polygon([Point(Self.ClientRect.Left, Self.ClientRect.Top),
                             Point(Self.ClientRect.Left + lSize, Self.ClientRect.Top),
                             Point(Self.ClientRect.Left, Self.ClientRect.Top + lSize)]);
        end;

        if HasMetaData then
        begin
            LCanvas.Brush.Color := clAqua;
            LCanvas.Pen.Color   := clBlack;
            LCanvas.Polygon([Point(Self.ClientRect.Left,         Self.ClientRect.Bottom),
                             Point(Self.ClientRect.Left + lSize, Self.ClientRect.Bottom),
                             Point(Self.ClientRect.Left,         Self.ClientRect.Bottom - lSize)]);
        end;
      finally
        LCanvas.Free;
      end;
    finally
      ReleaseDC(Self.Handle, LDC);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRadioButton.SetFieldProperty(AFieldProperty: TAbstractFieldProperty);
const OPNAME = 'TFieldRadioButton.SetFieldProperty';
begin
 try
   FFieldProperty := AFieldProperty;
   if assigned(FFieldProperty) then
     IsEnabled := FAppModules.FieldProperties.ModeIsEditable(AFieldProperty.FieldName)
   else
     IsEnabled := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRadioButton.SetHasChanges(AHasChanges: boolean);
const OPNAME = 'TFieldRadioButton.SetHasChanges';
begin
  try
    FHasChanges := AHasChanges;
    Self.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRadioButton.SetHasMetaData(AHasMetaData: boolean);
const OPNAME = 'TFieldRadioButton.SetHasMetaData';
begin
  try
    FHasMetaData := AHasMetaData;
    Self.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRadioButton.SetIsEnabled(AIsEnabled: boolean);
const OPNAME = 'TFieldRadioButton.SetIsEnabled';
begin
  try
    if Assigned(FFieldProperty) then
      AIsEnabled := AIsEnabled and (FAppModules.FieldProperties.ModeIsEditable(FFieldProperty.FieldName))
    else
      AIsEnabled := False;
    FIsEnabled := AIsEnabled;
    Self.Enabled := FIsEnabled;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRadioButton.ShowErrorState(AError: Boolean);
const OPNAME = 'TFieldRadioButton.ShowErrorState';
begin
  try
    if not Enabled then
      Exit;
    if (AError) then
    begin
      if InValidationError then
       Color := clTeal
      else
       Color := clRed;
    end
    else
      Color := clWindow;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldRadioButton.WMPAINT(var Message: TMessage);
const OPNAME = 'TFieldRadioButton.WMPAINT';
begin
  inherited;
  try
    PaintChangesAndMetaData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TFieldTreeView }

procedure TFieldTreeView.DoEnter;
const OPNAME = 'TFieldTreeView.DoEnter';
begin
  if Application.Terminated then Exit;
  inherited;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldTreeView.DoExit;
const OPNAME = 'TFieldTreeView.DoExit';
begin
  if Application.Terminated then Exit;
  inherited;
  try
    if (not HasValueChanged) and (not InValidationError) then
       FValidationError := '';

    if(FValidationError = '') then
      ShowErrorState(FALSE)
    else
      ShowErrorState(True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldTreeView.HasValueChanged: boolean;
const OPNAME = 'TFieldTreeView.HasValueChanged';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldTreeView.ShowErrorState(AError: Boolean);
const OPNAME = 'TFieldTreeView.ShowErrorState';
begin
  try
    if not Enabled then
      Exit;
    if (AError) then
    begin
      if InValidationError then
       Color := clTeal
      else
       Color := clRed;
    end
    else
    Color := clWindow;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldTreeView.SetFieldProperty(AFieldProperty: TAbstractFieldProperty);
const OPNAME = 'TFieldTreeView.SetFieldProperty';
begin
 try
   FFieldProperty := AFieldProperty;
   if assigned(FFieldProperty) then
     IsEnabled := FAppModules.FieldProperties.ModeIsEditable(AFieldProperty.FieldName)
   else
     IsEnabled := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldTreeView.SetIsEnabled(AIsEnabled: boolean);
const OPNAME = 'TFieldTreeView.SetIsEnabled';
begin
  try
    if Assigned(FFieldProperty) then
      AIsEnabled := AIsEnabled and (FAppModules.FieldProperties.ModeIsEditable(FFieldProperty.FieldName))
    else
      AIsEnabled := False;
    FIsEnabled := AIsEnabled;
    Self.ReadOnly := not FIsEnabled;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TFieldTreeView.CreateMemberObjects;
const OPNAME = 'TFieldTreeView.CreateMemberObjects';
begin
  inherited;
  try
    Self.ReadOnly      := True;
    FFieldProperty     := nil;
    FValidationError   := '';
    FIsEnabled         := False;
    FInValidationError := False;
    FHasChanges        := False;
    FHasMetaData       := FALSE;
    FDisabledColor     := clSilver;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldTreeView.DestroyMemberObjects;
const OPNAME = 'TFieldTreeView.DestroyMemberObjects';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
  inherited;
end;

function TFieldTreeView.HotSpotChangesClicked(X, Y: Integer): boolean;
const OPNAME = 'TFieldTreeView.HotSpotChangesClicked';
var
  lSize : integer;
begin
  Result := False;
  try
    lSize := 7;//Round(Self.Height / 3);
    Result := (X >= 0) and
              (Y >= 0) and
              (X <= lSize) and
              (Y <=  lSize);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldTreeView.HotSpotMetaDataClicked(X, Y: Integer): boolean;
const OPNAME = 'TFieldTreeView.HotSpotMetaDataClicked';
var
  lSize : integer;
begin
  Result := False;
  try
    lSize := 7;//Round(Self.Height / 3);
    Result := (X >= 0) and
              (Y >= 0) and
              (X <= lSize) and
              (Y >=  Self.Height - lSize);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldTreeView.MouseDown(Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TFieldTreeView.MouseDown';
begin
  try
    if (Button = mbLeft) then
    begin
      if HotSpotChangesClicked(X, Y) then
      begin
        if HasChanges then
          FAppModules.Model.ProcessEvent(CmeChangeParameter,nil)
        else
          inherited MouseDown(Button,Shift,X, Y);
      end
      else
      begin
        if HotSpotMetaDataClicked(X,Y) and  HasMetaData then
          FAppModules.Model.ProcessEvent(CmeMetaData,nil)
        else
          inherited MouseDown(Button,Shift,X, Y);
      end;
    end
    else
      inherited MouseDown(Button,Shift,X, Y);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldTreeView.PaintChangesAndMetaData;
const OPNAME = 'TFieldTreeView.PaintChangesAndMetaData';
var
  LCanvas: TCanvas;
  lSize  : integer;
  LDC    : HDC;
begin
  try
    LDC := GetDC(Self.Handle);
    try
      LCanvas := TCanvas.Create;
      try
        LCanvas.Handle := LDC;
        lSize := 7;//Round(Self.Height / 3);
        if HasChanges then
        begin
            LCanvas.Brush.Color := clLime;
            LCanvas.Pen.Color   := clBlack;
            LCanvas.Polygon([Point(Self.ClientRect.Left, Self.ClientRect.Top),
                             Point(Self.ClientRect.Left + lSize, Self.ClientRect.Top),
                             Point(Self.ClientRect.Left, Self.ClientRect.Top + lSize)]);
        end;

        if HasMetaData then
        begin
            LCanvas.Brush.Color := clAqua;
            LCanvas.Pen.Color   := clBlack;
            LCanvas.Polygon([Point(Self.ClientRect.Left,         Self.ClientRect.Bottom),
                             Point(Self.ClientRect.Left + lSize, Self.ClientRect.Bottom),
                             Point(Self.ClientRect.Left,         Self.ClientRect.Bottom - lSize)]);
        end;
      finally
        LCanvas.Free;
      end;
    finally
      ReleaseDC(Self.Handle, LDC);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldTreeView.SetHasChanges(AHasChanges: boolean);
const OPNAME = 'TFieldTreeView.SetHasChanges';
begin
  try
    FHasChanges := AHasChanges;
    Self.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldTreeView.SetHasMetaData(AHasMetaData: boolean);
const OPNAME = 'TFieldTreeView.SetHasMetaData';
begin
  try
    FHasMetaData := AHasMetaData;
    Self.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldTreeView.WMPAINT(var Message: TMessage);
const OPNAME = 'TFieldTreeView.WMPAINT';
begin
  try
    try
      if FIsEnabled then
        Self.Color := clWindow
      else
        Self.Color := FDisabledColor;
    finally
      inherited;
    end;
    PaintChangesAndMetaData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TFieldButton }

procedure TFieldButton.CreateMemberObjects;
const OPNAME = 'TFieldButton.CreateMemberObjects';
begin
  inherited;
  try
    FFieldProperty     := nil;
    FValidationError   := '';
    FInValidationError := False;
    FHasChanges        := False;
    FHasMetaData       := FALSE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldButton.DestroyMemberObjects;
const OPNAME = 'TFieldButton.DestroyMemberObjects';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
  inherited;
end;

procedure TFieldButton.DoEnter;
const OPNAME = 'TFieldButton.DoEnter';
begin
  if Application.Terminated then Exit;
  inherited;
  try
    FPrevValue := Self.Checked;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldButton.DoExit;
const OPNAME = 'TFieldButton.DoExit';
begin
  if Application.Terminated then Exit;
  inherited;
  try
    if (not HasValueChanged) and (not InValidationError) then
       FValidationError := '';

    if(FValidationError = '') then
      ShowErrorState(FALSE)
    else
      ShowErrorState(True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldButton.HasValueChanged: boolean;
const OPNAME = 'TFieldButton.HasValueChanged';
begin
  Result := False;
  try
    Result := (FPrevValue <> Self.Checked);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldButton.HotSpotChangesClicked(X, Y: Integer): boolean;
const OPNAME = 'TFieldButton.HotSpotChangesClicked';
var
  lSize : integer;
begin
  Result := False;
  try
    lSize := 7;//Round(Self.Height / 3);
    Result := (X >= 0) and
              (Y >= 0) and
              (X <= lSize) and
              (Y <=  lSize);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldButton.HotSpotMetaDataClicked(X, Y: Integer): boolean;
const OPNAME = 'TFieldButton.HotSpotMetaDataClicked';
var
  lSize : integer;
begin
  Result := False;
  try
    lSize := 7;//Round(Self.Height / 3);
    Result := (X >= 0) and
              (Y >= 0) and
              (X <= lSize) and
              (Y >=  Self.Height - lSize);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TFieldButton.MouseDown';
begin
  try
    if (Button = mbLeft) then
    begin
      if HotSpotChangesClicked(X, Y) then
      begin
        if HasChanges then
          FAppModules.Model.ProcessEvent(CmeChangeParameter,nil)
        else
          inherited MouseDown(Button,Shift,X, Y);
      end
      else
      begin
        if HotSpotMetaDataClicked(X,Y) and  HasMetaData then
          FAppModules.Model.ProcessEvent(CmeMetaData,nil)
        else
          inherited MouseDown(Button,Shift,X, Y);
      end;
    end
    else
      inherited MouseDown(Button,Shift,X, Y);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldButton.PaintChangesAndMetaData;
const OPNAME = 'TFieldButton.PaintChangesAndMetaData';
var
  LCanvas: TCanvas;
  lSize  : integer;
  LDC    : HDC;
begin
  try
    LDC := GetDC(Self.Handle);
    try
      LCanvas := TCanvas.Create;
      try
        LCanvas.Handle := LDC;
        lSize := 7;//Round(Self.Height / 3);
        if HasChanges then
        begin
            LCanvas.Brush.Color := clLime;
            LCanvas.Pen.Color   := clBlack;
            LCanvas.Polygon([Point(Self.ClientRect.Left, Self.ClientRect.Top),
                             Point(Self.ClientRect.Left + lSize, Self.ClientRect.Top),
                             Point(Self.ClientRect.Left, Self.ClientRect.Top + lSize)]);
        end;

        if HasMetaData then
        begin
            LCanvas.Brush.Color := clAqua;
            LCanvas.Pen.Color   := clBlack;
            LCanvas.Polygon([Point(Self.ClientRect.Left,         Self.ClientRect.Bottom),
                             Point(Self.ClientRect.Left + lSize, Self.ClientRect.Bottom),
                             Point(Self.ClientRect.Left,         Self.ClientRect.Bottom - lSize)]);
        end;
      finally
        LCanvas.Free;
      end;
    finally
      ReleaseDC(Self.Handle, LDC);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldButton.SetFieldProperty(AFieldProperty: TAbstractFieldProperty);
const OPNAME = 'TFieldButton.SetFieldProperty';
begin
 try
   FFieldProperty := AFieldProperty;
   if assigned(FFieldProperty) then
     IsEnabled := FAppModules.FieldProperties.ModeIsEditable(AFieldProperty.FieldName)
   else
     IsEnabled := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldButton.SetHasChanges(AHasChanges: boolean);
const OPNAME = 'TFieldButton.SetHasChanges';
begin
  try
    FHasChanges := AHasChanges;
    Self.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldButton.SetHasMetaData(AHasMetaData: boolean);
const OPNAME = 'TFieldButton.SetHasMetaData';
begin
  try
    FHasMetaData := AHasMetaData;
    Self.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldButton.SetIsEnabled(AIsEnabled: boolean);
const OPNAME = 'TFieldButton.SetIsEnabled';
begin
  try
    if Assigned(FFieldProperty) then
      AIsEnabled := AIsEnabled and (FAppModules.FieldProperties.ModeIsEditable(FFieldProperty.FieldName))
    else
      AIsEnabled := False;
    FIsEnabled := AIsEnabled;
    Self.Enabled := FIsEnabled;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldButton.ShowErrorState(AError: Boolean);
const OPNAME = 'TFieldButton.ShowErrorState';
begin
  try
    if not Enabled then
      Exit;
    if (AError) then
    begin
      if InValidationError then
       Color := clTeal
      else
       Color := clRed;
    end
    else
      Color := clBtnFace;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TFieldButton.WMPAINT(var Message: TMessage);
const OPNAME = 'TFieldButton.WMPAINT';
begin
  inherited;
  try
    PaintChangesAndMetaData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TFieldBitBtn }

procedure TFieldBitBtn.CreateMemberObjects;
const OPNAME = 'TFieldBitBtn.CreateMemberObjects';
begin
  inherited;
  try
    FFieldProperty     := nil;
    FValidationError   := '';
    FInValidationError := False;
    FHasChanges        := False;
    FHasMetaData       := FALSE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldBitBtn.DestroyMemberObjects;
const OPNAME = 'TFieldBitBtn.DestroyMemberObjects';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
  inherited;
end;

procedure TFieldBitBtn.DoEnter;
const OPNAME = 'TFieldBitBtn.DoEnter';
begin
  if Application.Terminated then Exit;
  inherited;
  try
    FPrevValue := Self.Checked;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldBitBtn.DoExit;
const OPNAME = 'TFieldBitBtn.DoExit';
begin
  if Application.Terminated then Exit;
  inherited;
  try
    if (not HasValueChanged) and (not InValidationError) then
       FValidationError := '';

    if(FValidationError = '') then
      ShowErrorState(FALSE)
    else
      ShowErrorState(True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldBitBtn.HasValueChanged: boolean;
const OPNAME = 'TFieldBitBtn.HasValueChanged';
begin
  Result := False;
  try
    Result := (FPrevValue <> Self.Checked);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldBitBtn.HotSpotChangesClicked(X, Y: Integer): boolean;
const OPNAME = 'TFieldBitBtn.HotSpotChangesClicked';
var
  lSize : integer;
begin
  Result := False;
  try
    lSize := 7;//Round(Self.Height / 3);
    Result := (X >= 0) and
              (Y >= 0) and
              (X <= lSize) and
              (Y <=  lSize);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldBitBtn.HotSpotMetaDataClicked(X, Y: Integer): boolean;
const OPNAME = 'TFieldBitBtn.HotSpotMetaDataClicked';
var
  lSize : integer;
begin
  Result := False;
  try
    lSize := 7;//Round(Self.Height / 3);
    Result := (X >= 0) and
              (Y >= 0) and
              (X <= lSize) and
              (Y >=  Self.Height - lSize);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldBitBtn.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TFieldBitBtn.MouseDown';
begin
  try
    if (Button = mbLeft) then
    begin
      if HotSpotChangesClicked(X, Y) then
      begin
        if HasChanges then
          FAppModules.Model.ProcessEvent(CmeChangeParameter,nil)
        else
          inherited MouseDown(Button,Shift,X, Y);
      end
      else
      begin
        if HotSpotMetaDataClicked(X,Y) and  HasMetaData then
          FAppModules.Model.ProcessEvent(CmeMetaData,nil)
        else
          inherited MouseDown(Button,Shift,X, Y);
      end;
    end
    else
      inherited MouseDown(Button,Shift,X, Y);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldBitBtn.PaintChangesAndMetaData;
const OPNAME = 'TFieldBitBtn.PaintChangesAndMetaData';
var
  LCanvas: TCanvas;
  lSize  : integer;
  LDC    : HDC;
begin
  try
    LDC := GetDC(Self.Handle);
    try
      LCanvas := TCanvas.Create;
      try
        LCanvas.Handle := LDC;
        lSize := 7;//Round(Self.Height / 3);
        if HasChanges then
        begin
            LCanvas.Brush.Color := clLime;
            LCanvas.Pen.Color   := clBlack;
            LCanvas.Polygon([Point(Self.ClientRect.Left, Self.ClientRect.Top),
                             Point(Self.ClientRect.Left + lSize, Self.ClientRect.Top),
                             Point(Self.ClientRect.Left, Self.ClientRect.Top + lSize)]);
        end;

        if HasMetaData then
        begin
            LCanvas.Brush.Color := clAqua;
            LCanvas.Pen.Color   := clBlack;
            LCanvas.Polygon([Point(Self.ClientRect.Left,         Self.ClientRect.Bottom),
                             Point(Self.ClientRect.Left + lSize, Self.ClientRect.Bottom),
                             Point(Self.ClientRect.Left,         Self.ClientRect.Bottom - lSize)]);
        end;
      finally
        LCanvas.Free;
      end;
    finally
      ReleaseDC(Self.Handle, LDC);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldBitBtn.SetFieldProperty(AFieldProperty: TAbstractFieldProperty);
const OPNAME = 'TFieldBitBtn.SetFieldProperty';
begin
 try
   FFieldProperty := AFieldProperty;
   if assigned(FFieldProperty) then
     IsEnabled := FAppModules.FieldProperties.ModeIsEditable(AFieldProperty.FieldName)
   else
     IsEnabled := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldBitBtn.SetHasChanges(AHasChanges: boolean);
const OPNAME = 'TFieldBitBtn.SetHasChanges';
begin
  try
    FHasChanges := AHasChanges;
    Self.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldBitBtn.SetHasMetaData(AHasMetaData: boolean);
const OPNAME = 'TFieldBitBtn.SetHasMetaData';
begin
  try
    FHasMetaData := AHasMetaData;
    Self.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldBitBtn.SetIsEnabled(AIsEnabled: boolean);
const OPNAME = 'TFieldBitBtn.SetIsEnabled';
begin
  try
    if Assigned(FFieldProperty) then
      AIsEnabled := AIsEnabled and (FAppModules.FieldProperties.ModeIsEditable(FFieldProperty.FieldName))
    else
      AIsEnabled := False;
    FIsEnabled := AIsEnabled;
    Self.Enabled := FIsEnabled;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldBitBtn.ShowErrorState(AError: Boolean);
const OPNAME = 'TFieldBitBtn.ShowErrorState';
begin
  try
    if not Enabled then
      Exit;
    if (AError) then
    begin
      if InValidationError then
       Color := clTeal
      else
       Color := clRed;
    end
    else
      Color := clBtnFace;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldBitBtn.WMPAINT(var Message: TMessage);
const OPNAME = 'TFieldBitBtn.WMPAINT';
begin
  inherited;
  try
    PaintChangesAndMetaData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TFieldListBox }

procedure TFieldListBox.CreateMemberObjects;
const OPNAME = 'TFieldListBox.CreateMemberObjects';
begin
  inherited;
  try
    FOldIndex          := '';
    FFieldProperty     := nil;
    FValidationError   := '';
    FInValidationError := False;
    FHasChanges        := False;
    FHasMetaData       := FALSE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldListBox.DestroyMemberObjects;
const OPNAME = 'TFieldListBox.DestroyMemberObjects';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
  inherited;
end;

procedure TFieldListBox.DoEnter;
const OPNAME = 'TFieldListBox.DoEnter';
begin
  if Application.Terminated then Exit;
  inherited;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldListBox.DoExit;
const OPNAME = 'TFieldListBox.DoExit';
begin
  if Application.Terminated then Exit;
  inherited;
  try
    if (not InValidationError) then
       FValidationError := '';

    if(FValidationError = '') then
      ShowErrorState(FALSE)
    else
      ShowErrorState(True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldListBox.HasValueChanged: boolean;
const OPNAME = 'TFieldListBox.HasValueChanged';
begin
  inherited;
  Result := False;
  try
    Result := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldListBox.HotSpotChangesClicked(X, Y: Integer): boolean;
const OPNAME = 'TFieldListBox.HotSpotChangesClicked';
var
  lSize : integer;
begin
  Result := False;
  try
    lSize := 7;//Round(Self.Height / 3);
    Result := (X >= 0) and
              (Y >= 0) and
              (X <= lSize) and
              (Y <=  lSize);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldListBox.HotSpotMetaDataClicked(X, Y: Integer): boolean;
const OPNAME = 'TFieldListBox.HotSpotMetaDataClicked';
var
  lSize : integer;
begin
  Result := False;
  try
    lSize := 7;//Round(Self.Height / 3);
    Result := (X >= 0) and
              (Y >= 0) and
              (X <= lSize) and
              (Y >=  Self.Height - lSize);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldListBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TFieldListBox.MouseDown';
begin
  try
    if (Button = mbLeft) then
    begin
      if HotSpotChangesClicked(X, Y) then
      begin
        if HasChanges then
          FAppModules.Model.ProcessEvent(CmeChangeParameter,nil)
        else
          inherited MouseDown(Button,Shift,X, Y);
      end
      else
      begin
        if HotSpotMetaDataClicked(X,Y) and  HasMetaData then
          FAppModules.Model.ProcessEvent(CmeMetaData,nil)
        else
          inherited MouseDown(Button,Shift,X, Y);
      end;
    end
    else
      inherited MouseDown(Button,Shift,X, Y);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldListBox.PaintChangesAndMetaData;
const OPNAME = 'TFieldListBox.PaintChangesAndMetaData';
var
  LCanvas: TCanvas;
  lSize  : integer;
  LDC    : HDC;
begin
  try
    LDC := GetDC(Self.Handle);
    try
      LCanvas := TCanvas.Create;
      try
        LCanvas.Handle := LDC;
        lSize := 7;//Round(Self.Height / 3);
        if HasChanges then
        begin
            LCanvas.Brush.Color := clLime;
            LCanvas.Pen.Color   := clBlack;
            LCanvas.Polygon([Point(Self.ClientRect.Left, Self.ClientRect.Top),
                             Point(Self.ClientRect.Left + lSize, Self.ClientRect.Top),
                             Point(Self.ClientRect.Left, Self.ClientRect.Top + lSize)]);
        end;

        if HasMetaData then
        begin
            LCanvas.Brush.Color := clAqua;
            LCanvas.Pen.Color   := clBlack;
            LCanvas.Polygon([Point(Self.ClientRect.Left,         Self.ClientRect.Bottom),
                             Point(Self.ClientRect.Left + lSize, Self.ClientRect.Bottom),
                             Point(Self.ClientRect.Left,         Self.ClientRect.Bottom - lSize)]);
        end;
      finally
        LCanvas.Free;
      end;
    finally
      ReleaseDC(Self.Handle, LDC);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldListBox.SetFieldProperty(AFieldProperty: TAbstractFieldProperty);
const OPNAME = 'TFieldListBox.SetFieldProperty';
begin
 try
   FFieldProperty := AFieldProperty;
   if assigned(FFieldProperty) then
     IsEnabled := FAppModules.FieldProperties.ModeIsEditable(AFieldProperty.FieldName)
   else
     IsEnabled := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldListBox.SetHasChanges(AHasChanges: boolean);
const OPNAME = 'TFieldListBox.SetHasChanges';
begin
  try
    FHasChanges := AHasChanges;
    Self.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldListBox.SetHasMetaData(AHasMetaData: boolean);
const OPNAME = 'TFieldListBox.SetHasMetaData';
begin
  try
    FHasMetaData := AHasMetaData;
    Self.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldListBox.SetIsEnabled(AIsEnabled: boolean);
const OPNAME = 'TFieldListBox.SetIsEnabled';
begin
  try
    if Assigned(FFieldProperty) then
      AIsEnabled := AIsEnabled and (FAppModules.FieldProperties.ModeIsEditable(FFieldProperty.FieldName))
    else
      AIsEnabled := False;
    FIsEnabled := AIsEnabled;
    Self.Enabled := FIsEnabled;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldListBox.ShowErrorState(AError: Boolean);
const OPNAME = 'TFieldListBox.ShowErrorState';
begin
  try
    if not Enabled then
      Exit;
    if (AError) then
    begin
      if InValidationError then
       Color := clTeal
      else
       Color := clRed;
    end
    else
      Color := clWindow;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldListBox.WMPAINT(var Message: TMessage);
const OPNAME = 'TFieldListBox.WMPAINT';
begin
  inherited;
  try
    PaintChangesAndMetaData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TPropertyCell }

procedure TPropertyCell.CreateMemberObjects;
const OPNAME = 'TPropertyCell.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    Self.Width          := 140;
    Self.Height         := 20;
    Self.BevelWidth     := 1;
    Self.Caption        := '';
    Self.BevelInner     := bvLowered;
    Self.BevelOuter     := bvLowered;

    FCellType           := ctValue;
    FBorder             := TShape.Create(Self);
    FBorder.Parent      := Self;
    FBorder.Brush.Style := bsClear;
    FBorder.Pen.Width   := 2;
    FBorder.Align       := alClient;
    FBorder.Visible     := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPropertyCell.SetCellType(ACellType : TCellType);
const OPNAME = 'TPropertyCell.SetCellType';
begin
  try
    FCellType := ACellType;
    FBorder.Visible := (FCellType = ctAggregate);
    case FCellType of
      ctValue     : Self.Font.Style := Self.Font.Style - [fsBold];
      ctAggregate : Self.Font.Style := Self.Font.Style + [fsBold];
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TProperty }


procedure TProperty.CreateMemberObjects;
const OPNAME = 'TProperty.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FPropertyName    := '';
    FPropertyCaption := '';
    FPropertyValue   := '';
    FPropertyCell    := TPropertyCell.Create(nil,FAppModules);
    FValueCell       := TPropertyCell.Create(nil,FAppModules);
    FPropertyCell.Alignment := taLeftJustify;
    FValueCell.Alignment := taRightJustify;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TProperty.DestroyMemberObjects;
const OPNAME = 'TProperty.DestroyMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FPropertyCell.Parent := nil;
    FValueCell.Parent    := nil;
    FPropertyCell.Free;
    FValueCell.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TProperty.GetPropertyName: string;
const OPNAME = 'TProperty.GetPropertyName';
begin
  try
    Result := FPropertyName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TProperty.GetPropertyValue: string;
const OPNAME = 'TProperty.GetPropertyValue';
begin
  try
    Result := FPropertyValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TProperty.SetPropertyName(AName: string);
const OPNAME = 'TProperty.SetPropertyName';
begin
  try
    FPropertyName      := AName;
    FPropertyCaption   := GetCaptionFromName(AName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TProperty.SetPropertyValue(AValue: string);
const OPNAME = 'TProperty.SetPropertyValue';
begin
  try
    FPropertyValue      := AValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TPropertyGroup }

procedure TPropertyGroup.AddProperty(APropertyName,
                                     APropertyValue: string);
const OPNAME = 'TPropertyGroup.AddProperty';
var
  LProperty : TProperty;
begin
  try
    if ValidProperty(APropertyName) then
    begin
      LProperty := TProperty.Create(FAppModules);
      LProperty.PropertyName    := APropertyName;
      LProperty.PropertyValue   := APropertyValue;
      FPropertyList.Add(LProperty);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPropertyGroup.UpdateProperty(APropertyName, APropertyValue: string);
const OPNAME = 'TPropertyGroup.UpdateProperty';
var
  LIndex : integer;
  LProperty : TProperty;
begin
  try
    for LIndex := 0 to Pred(FPropertyList.Count) do
    begin
      LProperty := TProperty(FPropertyList[LIndex]);
      if Assigned(LProperty) and
        (LProperty.PropertyName = APropertyName) then
      begin
        LProperty.PropertyValue     := APropertyValue;
        LProperty.ValueCell.Caption := APropertyValue;
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPropertyGroup.DeleteProperty(APropertyName: string) : boolean;
const OPNAME = 'TPropertyGroup.DeleteProperty';
var
  LIndex : integer;
  LProperty : TProperty;
begin
  Result := False;
  try
    for LIndex := 0 to Pred(FPropertyList.Count) do
    begin
      LProperty := TProperty(FPropertyList[LIndex]);
      if Assigned(LProperty) and
        (LProperty.PropertyName = APropertyName) then
      begin
        FPropertyList.Delete(LIndex);
        FPropertyGroupPanel.Height := FPropertyGroupPanel.Height - 20;
        Result := True;
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPropertyGroup.CreateMemberObjects;
const OPNAME = 'TPropertyGroup.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FPropertyGroupName    := '';
    FPropertyGroupCaption := '';
    FCount                := 0;
    FPropertyList         := TObjectList.Create(True);
    FPropertyGroupPanel   := TAbstractPanel.Create(nil,FAppModules);
    FPropertyPanel        := TAbstractPanel.Create(nil,FAppModules);
    FValuePanel           := TAbstractPanel.Create(nil,FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPropertyGroup.DestroyMemberObjects;
const OPNAME = 'TPropertyGroup.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FPropertyList.Clear;
    FPropertyList.Free;
    FPropertyGroupPanel.Parent := nil;
    FPropertyPanel.Parent := nil;
    FValuePanel.Parent := nil;
    FPropertyGroupPanel.Free;
    FPropertyPanel.Free;
    FValuePanel.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPropertyGroup.GetCount: integer;
const OPNAME = 'TPropertyGroup.GetCount';
begin
  Result := 0;
  try
    FCount := FPropertyList.Count;
    Result := FCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPropertyGroup.SetPropertyGroupCaption(const Value: string);
const OPNAME = 'TPropertyGroup.SetPropertyGroupCaption';
begin
  try
    FPropertyGroupCaption := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPropertyGroup.SetPropertyGroupName(APropertyGroupName: string);
const OPNAME = 'TPropertyGroup.SetPropertyGroupName';
begin
  try
    FPropertyGroupName    := APropertyGroupName;
    FPropertyGroupCaption := GetCaptionFromName(APropertyGroupName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPropertyGroup.ValidProperty(APropertyName : string): boolean;
const OPNAME = 'TPropertyGroup.ValidProperty';
var
  LIndex : integer;
begin
  Result := True;
  try
    for LIndex := 0 to Pred(FPropertyList.Count) do
    begin
      Result := (TProperty(FPropertyList[LIndex]).PropertyName <> APropertyName);
      if not Result then
        Break;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPropertyGroup.PropertyByIndex(AIndex: integer): TProperty;
const OPNAME = 'TPropertyGroup.PropertyByIndex';
begin
  Result := nil;
  try
    Result := TProperty(FPropertyList[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPropertyGroup.FindProperty(APropertyName: string): TProperty;
const OPNAME = 'TPropertyGroup.FindProperty';
var
  LPropertyIndex : integer;
  LProperty : TProperty;
begin
  Result := nil;
  try
    for LPropertyIndex := 0 to Pred(FPropertyList.Count) do
    begin
      LProperty := PropertyByIndex(LPropertyIndex);
      if Assigned(LProperty) then
      begin
        if (LProperty.PropertyName = APropertyName) then
        begin
          Result := LProperty;
          Break;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TWaterBalanceDataViewer }

procedure TWaterBalanceDataViewer.CreateMemberObjects;
const OPNAME = 'TWaterBalanceDataViewer.CreateMemberObjects';
begin
  try
    Self.BevelInner          := bvLowered;
    Self.BevelOuter          := bvRaised;
    Self.BevelWidth          := 2;
    Self.Width               := 500;

    FObjectName              := '';

    FPropertyGroupList       := TObjectList.Create(True);

    FHeadingPanel            := TAbstractPanel.Create(Self, FAppModules);
    FHeadingPanel.Parent     := Self;
    FHeadingPanel.Align      := alTop;
    FHeadingPanel.Height     := 20;

    Self.Height              := FHeadingPanel.Height + 12;

    FPropGroupPanel          := TAbstractPanel.Create(Self, FAppModules);
    FPropGroupPanel.Parent   := Self;
    FPropGroupPanel.Caption  := '';
    FPropGroupPanel.Align    := alLeft;
    FPropGroupPanel.Width    := Self.Width div 3;

    {FSpl1                    := TSplitter.Create(Self);
    FSpl1.Parent             := Self;
    FSpl1.AutoSnap           := True;
    FSpl1.Align              := alLeft;}

    FPropPanel               := TAbstractPanel.Create(Self, FAppModules);
    FPropPanel.Parent        := Self;
    FPropPanel.Caption       := '';
    FPropPanel.Align         := alLeft;
    FPropPanel.Width         := Self.Width div 3;

    {FSpl2                    := TSplitter.Create(Self);
    FSpl2.Parent             := Self;
    FSpl2.AutoSnap           := True;
    FSpl2.Align              := alLeft;}

    FValPanel                := TAbstractPanel.Create(Self, FAppModules);
    FValPanel.Parent         := Self;
    FValPanel.Caption        := '';
    FValPanel.Align          := alClient;
    FValPanel.Width          := Self.Width div 3;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterBalanceDataViewer.DestroyMemberObjects;
const OPNAME = 'TWaterBalanceDataViewer.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FObjectName     := '';
    FPropertyGroupList.Clear;
    FPropertyGroupList.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterBalanceDataViewer.Initialise: boolean;
const OPNAME = 'TWaterBalanceDataViewer.Initialise';
begin
  Result := inherited Initialise;
  try
    FPropertyGroupList.Clear;
    Self.Height := FHeadingPanel.Height + 12;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterBalanceDataViewer.LanguageHasChanged: boolean;
const OPNAME = 'TWaterBalanceDataViewer.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterBalanceDataViewer.SaveState: boolean;
const OPNAME = 'TWaterBalanceDataViewer.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterBalanceDataViewer.StudyHasChanged: boolean;
const OPNAME = 'TWaterBalanceDataViewer.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Initialise;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterBalanceDataViewer.Resize;
const OPNAME = 'TWaterBalanceDataViewer.Resize';
begin
  try
    inherited Resize;
    FPropGroupPanel.Width    := Self.Width div 3;
    FPropPanel.Width         := Self.Width div 3;
    FValPanel.Align          := alClient;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterBalanceDataViewer.GetObjectName: string;
const OPNAME = 'TWaterBalanceDataViewer.GetObjectName';
begin
  Result := '';
  try
    Result := FObjectName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterBalanceDataViewer.AddProperty(AGroupName, APropertyName, APropertyValue: string; APropertyType : TCellType);
const OPNAME = 'TWaterBalanceDataViewer.AddProperty';
var
  LGroup : TPropertyGroup;
  LProperty : TProperty;
begin
  try
    LGroup := FindGroup(AGroupName);
    if not Assigned(LGroup) then
    begin
      LGroup                               := TPropertyGroup.Create(FAppModules);
      LGroup.PropertyGroupName             := AGroupName;
      FPropertyGroupList.Add(LGroup);
      FPropGroupPanel.Visible := (FPropertyGroupList.Count > 1);

      LGroup.PropertyGroupPanel.Parent     := FPropGroupPanel;
      LGroup.PropertyGroupPanel.Caption    := LGroup.PropertyGroupCaption;
      LGroup.PropertyGroupPanel.Align      := alBottom;
      LGroup.PropertyGroupPanel.Align      := alTop;
      LGroup.PropertyGroupPanel.Font.Style := LGroup.PropertyGroupPanel.Font.Style +
                                                      [fsBold];
      LGroup.PropertyGroupPanel.BevelInner := bvRaised;
      LGroup.PropertyGroupPanel.BevelOuter := bvLowered;
      LGroup.PropertyGroupPanel.Height     := 5;

      LGroup.PropertyPanel.Parent          := FPropPanel;
      LGroup.PropertyPanel.Caption         := '';
      LGroup.PropertyPanel.Align           := alBottom;
      LGroup.PropertyPanel.Align           := alTop;
      LGroup.PropertyPanel.BevelInner      := bvRaised;
      LGroup.PropertyPanel.BevelOuter      := bvLowered;
      LGroup.PropertyPanel.Height          := 5;

      LGroup.ValuePanel.Parent             := FValPanel;
      LGroup.ValuePanel.Caption            := '';
      LGroup.ValuePanel.Align              := alBottom;
      LGroup.ValuePanel.Align              := alTop;
      LGroup.ValuePanel.BevelInner         := bvRaised;
      LGroup.ValuePanel.BevelOuter         := bvLowered;
      LGroup.ValuePanel.Height             := 5;

      LGroup.AddProperty(APropertyName, APropertyValue);
      LProperty                            := LGroup.FindProperty(APropertyName);
      LProperty.PropertyCell.Parent        := LGroup.PropertyPanel;
      LProperty.PropertyCell.CellType      := APropertyType;
      LProperty.PropertyCell.Color         := clBtnFace;
      LProperty.PropertyCell.Caption       := LProperty.PropertyCaption;
      LProperty.PropertyCell.Align         := alBottom;
      LProperty.PropertyCell.Align         := alTop;

      LProperty.ValueCell.Parent           := LGroup.ValuePanel;
      LProperty.PropertyCell.CellType      := APropertyType;
      LProperty.ValueCell.Color            := clWindow;
      LProperty.ValueCell.Caption          := LProperty.PropertyValue;
      LProperty.ValueCell.Align            := alBottom;
      LProperty.ValueCell.Align            := alTop;
      LGroup.PropertyGroupPanel.Height     := LGroup.PropertyGroupPanel.Height +
                                              LProperty.PropertyCell.Height;
      LGroup.PropertyPanel.Height          := LGroup.PropertyPanel.Height +
                                              LProperty.PropertyCell.Height;
      LGroup.ValuePanel.Height             := LGroup.ValuePanel.Height +
                                              LProperty.PropertyCell.Height;
      Self.Height                          := Self.Height +
                                              LGroup.PropertyGroupPanel.Height;
    end
    else
    begin
      if LGroup.ValidProperty(APropertyName) then
      begin
        LGroup.AddProperty(APropertyName, APropertyValue);
        FPropGroupPanel.Visible            := (FPropertyGroupList.Count > 1);
        LProperty                          := LGroup.FindProperty(APropertyName);
        LProperty.PropertyCell.Parent      := LGroup.PropertyPanel;
        LProperty.PropertyCell.CellType    := APropertyType;
        LProperty.PropertyCell.Color       := clBtnFace;
        LProperty.PropertyCell.Caption     := LProperty.PropertyCaption;
        LProperty.PropertyCell.Align       := alBottom;
        LProperty.PropertyCell.Align       := alTop;

        LProperty.ValueCell.Parent         := LGroup.ValuePanel;
        LProperty.ValueCell.CellType       := APropertyType;
        LProperty.ValueCell.Color          := clWindow;
        LProperty.ValueCell.Caption        := LProperty.PropertyValue;
        LProperty.ValueCell.Align          := alBottom;
        LProperty.ValueCell.Align          := alTop;

        LGroup.PropertyGroupPanel.Height   := LGroup.PropertyGroupPanel.Height +
                                              LProperty.PropertyCell.Height;
        LGroup.PropertyPanel.Height        := LGroup.PropertyPanel.Height +
                                              LProperty.PropertyCell.Height;
        LGroup.ValuePanel.Height           := LGroup.ValuePanel.Height +
                                              LProperty.PropertyCell.Height;
        Self.Height := Self.Height + LProperty.PropertyCell.Height;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterBalanceDataViewer.UpdateProperty(AGroupName, APropertyName,APropertyValue: string);
const OPNAME = 'TWaterBalanceDataViewer.UpdateProperty';
var
  LGroup : TPropertyGroup;
begin
  try
    LGroup := FindGroup(AGroupName);
    if Assigned(LGroup) then
      LGroup.UpdateProperty(APropertyName, APropertyValue);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterBalanceDataViewer.DeleteGroup(AGroupName: string);
const OPNAME = 'TWaterBalanceDataViewer.DeleteGroup';
var
  LGroup        : TPropertyGroup;
  LProperty     : TProperty;
  LIndex        : integer;
begin
  try
    LGroup := FindGroup(AGroupName);
    if Assigned(LGroup) then
    begin
      for LIndex := 0 to LGroup.PropertyCount-1 do
      begin
        LProperty := LGroup.PropertyByIndex(LIndex);
        DeleteProperty(AGroupName,LProperty.PropertyName);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterBalanceDataViewer.DeleteProperty(AGroupName, APropertyName : string);
const OPNAME = 'TWaterBalanceDataViewer.DeleteProperty';
var
  LGroup : TPropertyGroup;
begin
  try
    LGroup := FindGroup(AGroupName);
    if Assigned(LGroup) then
    begin
      if LGroup.DeleteProperty(APropertyName) then
      begin
        LGroup.PropertyPanel.Height        := LGroup.PropertyPanel.Height - 20;
        LGroup.ValuePanel.Height           := LGroup.ValuePanel.Height - 20;
        if (LGroup.PropertyCount = 0) then
        begin
          FPropertyGroupList.Delete(FPropertyGroupList.IndexOf(LGroup));
          LGroup.Free;
          Self.Height                      := Self.Height - 5;
        end;
        Self.Height                        := Self.Height - 20;
      end;
    end;
    FPropGroupPanel.Visible := (FPropertyGroupList.Count > 1);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterBalanceDataViewer.FindGroup(AGroupName: string): TPropertyGroup;
const OPNAME = 'TWaterBalanceDataViewer.FindGroup';
var
  LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to Pred(FPropertyGroupList.Count) do
    begin
      Result := TPropertyGroup(FPropertyGroupList[LIndex]);
      if (Result.PropertyGroupName = AGroupName) then
        Break
      else
        Result := nil;;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterBalanceDataViewer.IndexOfGroup(APropertyGroup: TPropertyGroup): integer;
const OPNAME = 'TWaterBalanceDataViewer.IndexOfGroup';
var
  LIndex : integer;
  LGroup : TPropertyGroup;
begin
  Result := -1;
  try
    for LIndex := 0 to Pred(FPropertyGroupList.Count) do
    begin
      LGroup := TPropertyGroup(FPropertyGroupList[LIndex]);
      if (LGroup.PropertyGroupName = APropertyGroup.PropertyGroupName) then
      begin
        Result := LIndex;
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterBalanceDataViewer.SetObjectName(AName: string);
const OPNAME = 'TWaterBalanceDataViewer.SetObjectName';
begin
  try
    FObjectName            := AName;
    FHeadingPanel.Caption  := GetCaptionFromName(AName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TFieldChart }

procedure TFieldChart.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TFieldChart.MouseDown';
begin
  try
    if (Button = mbLeft) then
    begin
      if HotSpotMetaDataClicked(X,Y) and  HasMetaData then
        FAppModules.Model.ProcessEvent(CmeMetaData,nil)
      else
        inherited MouseDown(Button,Shift,X, Y);
    end
    else
      inherited MouseDown(Button,Shift,X, Y);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldChart.CreateMemberObjects;
const OPNAME = 'TFieldChart.CreateMemberObjects';
begin
  inherited;
  try
    FHasMetaData := False;
    FFieldProperty := nil;
    Self.OnAfterDraw := OnCompletedDrawing;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldChart.HotSpotMetaDataClicked(X, Y: Single): boolean;
const OPNAME = 'TFieldChart.HotSpotMetaDataClicked';
var
  lSize : integer;
begin
  Result := False;
  try
    lSize := 12;;
    Result := (X >= 0) and
              (Y >= 0) and
              (X <= lSize) and
              (Y <= lSize);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldChart.OnCompletedDrawing(Sender: TObject);
const OPNAME = 'TFieldChart.OnCompletedDrawing';
begin
  try
    PaintMetaData
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldChart.PaintMetaData;
const OPNAME = 'TFieldChart.PaintMetaData';
var
  lSize  : integer;
begin
  try
    if HasMetaData then
    begin
      lSize := 12;//Round(Self.Height / 3);
      Canvas.Brush.Color := clAqua;
      Canvas.Pen.Color   := clBlack;
      {
      Canvas.Polygon([PointF( Self.Left,Self.Top),
                       PointF(Self.Left + lSize, Self.Top),
                       PointF(Self.Left,         Self.Top + lSize)]);
       }
      Canvas.Polygon([Point( Self.ClientRect.Left,Self.ClientRect.Top),
                       Point(Self.ClientRect.Left + lSize, Self.ClientRect.Top),
                       Point(Self.ClientRect.Left,         Self.ClientRect.Top + lSize)]);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldChart.SetFieldProperty(AFieldProperty: TAbstractFieldProperty);
const OPNAME = 'TFieldChart.SetFieldProperty';
begin
  try
    FFieldProperty := AFieldProperty;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldChart.SetHasMetaData(AHasMetaData: boolean);
const OPNAME = 'TFieldChart.SetHasMetaData';
begin
  try
    FHasMetaData := AHasMetaData;
    Self.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TFieldStringGridPopupMenu }

constructor TFieldStringGridPopupMenu.Create(AOwner: TComponent);
const OPNAME = 'TFieldStringGridPopupMenu.Create';
var
  LSpacer1,
  LSpacer2,
  LSpacer3: TMenuItem;
begin
  inherited Create(AOwner);
  try
    FMenuItemUndo          := TMenuItem.Create(Self);
    Self.Items.Add(FMenuItemUndo);
    LSpacer1               := TMenuItem.Create(Self);
    Self.Items.Add(LSpacer1);
    FMenuItemCut           := TMenuItem.Create(Self);
    Self.Items.Add(FMenuItemCut);
    FMenuItemCopy          := TMenuItem.Create(Self);
    Self.Items.Add(FMenuItemCopy);
    FMenuItemPaste         := TMenuItem.Create(Self);
    Self.Items.Add(FMenuItemPaste);
    FMenuItemDelete        := TMenuItem.Create(Self);
    Self.Items.Add(FMenuItemDelete);
    LSpacer2               := TMenuItem.Create(Self);
    Self.Items.Add(LSpacer2);
    FmnuSelectAll          := TMenuItem.Create(Self);
    Self.Items.Add(FmnuSelectAll);
    LSpacer3               := TMenuItem.Create(Self);
    Self.Items.Add(LSpacer3);
    FMenuItemPastefromExcel:= TMenuItem.Create(Self);
    Self.Items.Add(FMenuItemPastefromExcel);
    FMenuItemCopyColumn:= TMenuItem.Create(Self);
    Self.Items.Add(FMenuItemCopyColumn);
    FMenuItemPasteColumn:= TMenuItem.Create(Self);
    Self.Items.Add(FMenuItemPasteColumn);
    FMenuItemCopyColumnsAndRows := TMenuItem.Create(Self);
    Self.Items.Add(FMenuItemCopyColumnsAndRows);
    FMenuItemPasteColumnsAndRows := TMenuItem.Create(Self);
    Self.Items.Add(FMenuItemPasteColumnsAndRows);

    Self.Alignment             := paCenter;

    FMenuItemUndo.Caption      := 'Undo';
    FMenuItemUndo.GroupIndex   := 10;
    FMenuItemUndo.ShortCut     := 16474;

    FMenuItemCut.Caption       := 'Cut';
    FMenuItemCut.GroupIndex    := 10;
    FMenuItemCut.ShortCut      := 16472;

    FMenuItemCopy.Caption      := 'Copy';
    FMenuItemCopy.GroupIndex   := 10;
    FMenuItemCopy.ShortCut     := 16451;

    FMenuItemPaste.Caption     := 'Paste';
    FMenuItemPaste.GroupIndex  := 10;
    FMenuItemPaste.ShortCut    := 16470;

    FMenuItemDelete.Caption    := 'Delete';
    FMenuItemDelete.GroupIndex := 10;
    FMenuItemDelete.ShortCut   := 46;

    FmnuSelectAll.Caption      := 'Edit Select All';
    FmnuSelectAll.GroupIndex   := 10;
    FmnuSelectAll.ShortCut     := 16449;

    FMenuItemPastefromExcel.Caption    := 'Paste From Excel';
    FMenuItemPastefromExcel.GroupIndex := 10;

    FMenuItemCopyColumn.Caption        := 'Copy Column Data';
    FMenuItemCopyColumn.GroupIndex     := 10;

    FMenuItemPasteColumn.Caption       := 'Paste Column Data';
    FMenuItemPasteColumn.GroupIndex    := 10;

    FMenuItemCopyColumnsAndRows.Caption := 'Copy Rows and Columns Data';
    FMenuItemCopyColumnsAndRows.GroupIndex := 10;

    FMenuItemPasteColumnsAndRows.Caption := 'Paste Rows and Columns Data';
    FMenuItemPasteColumnsAndRows.GroupIndex := 10;

    LSpacer1.Caption    := '-';
    LSpacer1.GroupIndex := 10;
    LSpacer2.Caption    := '-';
    LSpacer2.GroupIndex := 10;
    LSpacer3.Caption    := '-';
    LSpacer3.GroupIndex := 10;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TFieldButtonStringGrid }

procedure TFieldButtonStringGrid.CreateMemberObjects;
const OPNAME = 'TFieldButtonStringGrid.CreateMemberObjects';
begin
  inherited;
  try
    FButtonDown   := False;
    FDownRow      := -1;
    FDownCol      := -1;
    FButtonColums := TStringList.Create;
    FButtonColums.Sorted := True;
    FButtonColums.Duplicates := dupIgnore;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldButtonStringGrid.DestroyMemberObjects;
const OPNAME = 'TFieldButtonStringGrid.DestroyMemberObjects';
var
  LIndex: integer;
begin
  inherited;
  try
    for LIndex := FButtonColums.Count-1 downto 0 do
    begin
      Set_ButtonColumn(StrToInt(FButtonColums[LIndex]),False);
    end;
    FreeAndNil(FButtonColums);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldButtonStringGrid.SizeChanged(OldColCount,OldRowCount: Integer);
const OPNAME = 'TFieldButtonStringGrid.SizeChanged';
var
  LCol,
  LIndex,
  LIndex2 : integer;
begin
  inherited;
  try
    if not (csDestroying	 in ComponentState) then
    begin
      LIndex := FButtonColums.Count-1;
      while (LIndex >= 0) and (FButtonColums.Count > 0) do
      begin
        LCol := StrToInt(FButtonColums[LIndex]);
        if(LCol >= ColCount) then
        begin
          Set_ButtonColumn(StrToInt(FButtonColums[LIndex]),False);
          FButtonColums.Delete(LIndex);
        end
        else
         LIndex := LIndex -1;
      end;

      for LIndex := 0 to FButtonColums.Count-1 do
      begin
        LCol := StrToInt(FButtonColums[LIndex]);
        for LIndex2 := FixedRows to RowCount-1 do
        begin
          if(FButtonColums.Objects[LIndex] <> nil) then
            Cells[LCol,LIndex2] := TStringGridButtonData(FButtonColums.Objects[LIndex]).Caption;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFieldButtonStringGrid.Get_ButtonColumn(ACol: integer): boolean;
const OPNAME = 'TFieldButtonStringGrid.Get_ButtonColumn';
begin
  Result := False;
  try
    Result := (FButtonColums.IndexOf(IntToStr(ACol)) >= 0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldButtonStringGrid.Set_ButtonColumn(ACol: integer; AButtonColumn: Boolean);
const OPNAME = 'TFieldButtonStringGrid.Set_ButtonColumn';
var
  LIndex : integer;
  LGridButtonData : TStringGridButtonData;
begin
  try
    if AButtonColumn then
    begin
      if(ACol >= FixedCols) then
      begin
        if(FButtonColums.IndexOf(IntToStr(ACol)) < 0) then
        begin
          LGridButtonData := TStringGridButtonData.Create;
          FButtonColums.AddObject(IntToStr(ACol),LGridButtonData);
        end;
      end
    end
    else
    begin
      LIndex := FButtonColums.IndexOf(IntToStr(ACol));
      if(LIndex >= 0) then
      begin
        if(FButtonColums.Objects[LIndex] <> nil) then
          TStringGridButtonData(FButtonColums.Objects[LIndex]).Free;
        FButtonColums.Delete(LIndex);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldButtonStringGrid.DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
const OPNAME = 'TFieldButtonStringGrid.DrawCell';
var
  LColIndex: integer;
  LGridButtonData : TStringGridButtonData;
begin
  inherited;
  try
    if (FButtonColums.IndexOf(IntToStr(ACol)) >= 0)  and (ARow >= FixedRows )then
    begin
      LColIndex := FButtonColums.IndexOf(IntToStr(ACol));
      if(LColIndex >= 0) then
      begin
        if(FButtonColums.Objects[LColIndex] <> nil) then
        begin
          LGridButtonData := TStringGridButtonData(FButtonColums.Objects[LColIndex]);
          if(LGridButtonData.Caption <> Cells[ACol, ARow]) then
            Cells[ACol, ARow] := LGridButtonData.Caption;
        end
      end;

      // draw a button in Rect
      DrawFrameControl(Canvas.Handle, ARect,
                        DFC_BUTTON,
                        DFCS_BUTTONPUSH or DFCS_ADJUSTRECT
                        or DFCS_PUSHED * Ord(FButtonDown and (ARow = FDOwnrow) and (ACol = FDownCol)));
      Canvas.Brush.Style := bsClear;
      Canvas.Font.Color  := clBlack;
      Canvas.TextRect(ARect, ARect.Left+2, ARect.Top+2, Cells[ACol, ARow]);
      Canvas.Brush := Brush;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldButtonStringGrid.MouseDown(Button: TMouseButton;  Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TFieldButtonStringGrid.MouseDown';
var
  LCol,
  LRow : integer;
begin
  inherited;
  try
    if (Button = mbLeft) and ((Shift - [ssLeft]) = []) then
    begin
      MouseToCell(X, Y, LCol,LRow);
      if (FButtonColums.IndexOf(IntToStr(LCol)) >= 0) then
      begin
        FDownRow := LRow;
        FDownCol := LCol;
        FButtonDown:= True;
        InvalidateCell(LCol,LRow);
        MouseCapture := True;
        Options := Options - [goRangeSelect];
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldButtonStringGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TFieldButtonStringGrid.MouseUp';
begin
  inherited;
  try
    If FButtonDown then
    begin
      MouseCapture := False;
      FButtonDown := False;
      InvalidateCell(FDownCol, FDownRow );
      Options := Options + [goRangeSelect];
      ButtonClick(FDownCol, FDownRow);
      FDownCol := -1;
      FDownRow := -1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldButtonStringGrid.SelectCell(ACol, ARow: Integer): Boolean;
const OPNAME = 'TFieldButtonStringGrid.SelectCell';
begin
  Result := inherited SelectCell(ACol, ARow);
  try
    Result := Result and (FButtonColums.IndexOf(IntToStr(ACol))< 0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldButtonStringGrid.Set_ButtonColumnOnClick(ACol: integer; AButtonClickEventHandler: TStringGridButtonClickEvent);
const OPNAME = 'TFieldButtonStringGrid.Set_ButtonColumnOnClick';
var
  LColIndex: integer;
  LGridButtonData : TStringGridButtonData;
begin
  try
    LColIndex := FButtonColums.IndexOf(IntToStr(ACol));
    if(LColIndex >= 0) then
    begin
      if(FButtonColums.Objects[LColIndex] <> nil) then
      begin
        LGridButtonData := TStringGridButtonData(FButtonColums.Objects[LColIndex]);
        LGridButtonData.OnButtonClickEventHandler := AButtonClickEventHandler;
      end
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldButtonStringGrid.Get_ButtonColumnOnClick(ACol: integer): TStringGridButtonClickEvent;
const OPNAME = 'TFieldButtonStringGrid.Get_ButtonColumnOnClick';
var
  LColIndex: integer;
  LGridButtonData : TStringGridButtonData;
begin
  try
    LColIndex := FButtonColums.IndexOf(IntToStr(ACol));
    if(LColIndex >= 0) then
    begin
      if(FButtonColums.Objects[LColIndex] <> nil) then
      begin
        LGridButtonData := TStringGridButtonData(FButtonColums.Objects[LColIndex]);
        Result := LGridButtonData.FOnButtonClickEventHandler;
      end;
    end
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldButtonStringGrid.ButtonClick(ACol, ARow: Integer);
const OPNAME = 'TFieldButtonStringGrid.ButtonClick';
var
  LColIndex: integer;
  LGridButtonData : TStringGridButtonData;
begin
  try
    if (FButtonColums.IndexOf(IntToStr(ACol)) >= 0)  and (ARow >= FixedRows )then
    begin
      LColIndex := FButtonColums.IndexOf(IntToStr(ACol));
      if(FButtonColums.Objects[LColIndex] <> nil) then
      begin
        LGridButtonData := TStringGridButtonData(FButtonColums.Objects[LColIndex]);
        if Assigned(LGridButtonData.FOnButtonClickEventHandler) then
          LGridButtonData.FOnButtonClickEventHandler(Self,ACol, ARow);
      end;
    end
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldButtonStringGrid.Get_ButtonColumnCaption(ACol: integer): string;
const OPNAME = 'TFieldButtonStringGrid.Get_ButtonColumnCaption';
var
  LColIndex: integer;
  LGridButtonData : TStringGridButtonData;
begin
  try
    LColIndex := FButtonColums.IndexOf(IntToStr(ACol));
    if(LColIndex >= 0) then
    begin
      if(FButtonColums.Objects[LColIndex] <> nil) then
      begin
        LGridButtonData := TStringGridButtonData(FButtonColums.Objects[LColIndex]);
        Result := LGridButtonData.Caption;
      end;
    end
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldButtonStringGrid.Set_ButtonColumnCaption(ACol: integer;ACaption: string);
const OPNAME = 'TFieldButtonStringGrid.Set_ButtonColumnCaption';
var
  LColIndex: integer;
  LGridButtonData : TStringGridButtonData;
begin
  try
    LColIndex := FButtonColums.IndexOf(IntToStr(ACol));
    if(LColIndex >= 0) then
    begin
      if(FButtonColums.Objects[LColIndex] <> nil) then
      begin
        LGridButtonData := TStringGridButtonData(FButtonColums.Objects[LColIndex]);
        LGridButtonData.Caption := ACaption;
      end
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TFieldCheckListStringGrid }

procedure TFieldCheckListStringGrid.CheckBoxClick(ARow: Integer);
const OPNAME = 'TFieldCheckListStringGrid.CheckBoxClick';
var
  LRowIndex: integer;
begin
  try
    if (FCheckedRows.IndexOf(IntToStr(ARow)) >= 0)then
    begin
      LRowIndex := FCheckedRows.IndexOf(IntToStr(ARow));
      if(LRowIndex >=0) then
      begin
        CheckedRow[ARow] := False;
      end
      else
        CheckedRow[ARow] := True;;
    end
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldCheckListStringGrid.CreateMemberObjects;
const OPNAME = 'TFieldCheckListStringGrid.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FCheckedRow   := -1;
    FCheckedCol   := 0;
    FCheckedRows := TStringList.Create;
    FCheckedRows.Sorted := True;
    FCheckedRows.Duplicates := dupIgnore;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TFieldCheckListStringGrid.DestroyMemberObjects;
const OPNAME = 'TFieldCheckListStringGrid.DestroyMemberObjects';
var
  LIndex: integer;
begin
  inherited;
  try
    for LIndex := FCheckedRows.Count-1 downto 0 do
      Set_CheckeRow(StrToInt(FCheckedRows[LIndex]),False);
    FreeAndNil(FCheckedRows);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldCheckListStringGrid.DrawCell(ACol, ARow: Integer;ARect: TRect; AState: TGridDrawState);
const OPNAME = 'TFieldCheckListStringGrid.DrawCell';
var
  LRowIndex: integer;
begin
  inherited;
  try
    if (ARow > 0) and (ACol = 0) then
    begin
      LRowIndex := FCheckedRows.IndexOf(IntToStr(ARow));
      if (LRowIndex >= 0) then
        DrawFrameControl(Canvas.Handle, ARect,
                          DFC_BUTTON,
                          DFCS_CHECKED or DFCS_ADJUSTRECT
                          or DFCS_PUSHED * Ord(FChecked and (ARow = FCheckedRow) and (ACol = 0)))
      else
        DrawFrameControl(Canvas.Handle, ARect,
                          DFC_BUTTON,
                          DFCS_BUTTONCHECK or DFCS_ADJUSTRECT
                          or DFCS_PUSHED * Ord(FChecked and (ARow = FCheckedRow) and (ACol = 0)));
      Canvas.Brush.Style := bsClear;
      Canvas.Font.Color  := clBlack;
      Canvas.TextRect(ARect, ARect.Left+2, ARect.Top+2, Cells[ACol, ARow]);
      Canvas.Brush := Brush;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldCheckListStringGrid.Get_CheckeRow(ARow: integer): boolean;
const OPNAME = 'TFieldCheckListStringGrid.Get_CheckeRow';
begin
  Result := False;
  try
    Result := (FCheckedRows.IndexOf(IntToStr(ARow)) >= 0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldCheckListStringGrid.MouseDown(Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TFieldCheckListStringGrid.MouseDown';
var
  LCol,
  LRow : integer;
begin
  inherited;
  try
    if (Button = mbLeft) and ((Shift - [ssLeft]) = []) then
    begin
      MouseToCell(X, Y, LCol,LRow);
      if LCol = 0 then
      begin
        if (FCheckedRows.IndexOf(IntToStr(LRow)) >= 0) then
        begin
          CheckedRow[LRow] := False;
          InvalidateCell(LCol,LRow);
          MouseCapture := True;
          Options := Options - [goRangeSelect];
        end
        else
        begin
          CheckedRow[LRow] := True;
          InvalidateCell(LCol,LRow);
          MouseCapture := False;
          Options := Options - [goRangeSelect];
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TFieldCheckListStringGrid.MouseUp(Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TFieldCheckListStringGrid.MouseUp';
begin
  inherited;
  try
    if Assigned(FCheckBoxClick) then
      FCheckBoxClick(Self);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFieldCheckListStringGrid.SelectCell(ACol, ARow: Longint): Boolean;
const OPNAME = 'TFieldCheckListStringGrid.SelectCell';
begin
  Result := inherited SelectCell(ACol, ARow);
  try
    Result := Result and (FCheckedRows.IndexOf(IntToStr(ARow))< 0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldCheckListStringGrid.Set_CheckeRow(ARow: integer;ACheckedRow: Boolean);
const OPNAME = 'TFieldCheckListStringGrid.Set_CheckeRow';
var
  LIndex : integer;
begin
  try
    if ACheckedRow then
    begin
      if(FCheckedRows.IndexOf(IntToStr(ARow)) < 0) then
      begin
        FCheckedRows.AddObject(IntToStr(ARow),TObject(ARow));
      end;
    end
    else
    begin
      LIndex := FCheckedRows.IndexOf(IntToStr(ARow));
      if(LIndex >= 0) then
      begin
        if(FCheckedRows.Objects[LIndex] <> nil) then
        begin
          FCheckedRows.Delete(LIndex);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
