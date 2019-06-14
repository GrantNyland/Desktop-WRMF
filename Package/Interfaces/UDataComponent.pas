//
//
//  UNIT      : Contains TAbstractComponent Classes
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 24/01/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UDataComponent;

interface

uses
  Windows,
  Winapi.Messages,
  SysUtils,
  Classes,
  {$WARN UNIT_PLATFORM OFF}
  VCL.FileCtrl,
  {$WARN UNIT_PLATFORM ON}
  Vcl.ComCtrls,
  Contnrs,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Grids,
  VCL.Buttons,

//  VCLTee.Series,
//  VCLTee.Chart,
//  VCLTee.TeEngine,

//  VCLTee.TeeProcs,
//  VCLTee.TeeEdit,

  Types,
  Vcl.CheckLst,
  Vcl.Graphics,
  Vcl.Samples.Spin,
  VoaimsCom_TLB,
  UAbstractYRCData,
  UDataEditComponent,
  UAbstractComponent,
  UAbstractObject;

const
  C_ControlBorder  = 5;
  C_LabelOffset    = 3;
  C_GroupBoxOffset = 5;
  C_LabelHeight    = 18;
  C_EditBoxHeight  = 22;
  C_ControlOffset  = 10;

type
  TDoHintChangeFunction = procedure of object;

  TAbstractDataTabSheet = class(TAbstractTabSheet)
  protected
    function GetToolBar: TAbstractToolBar; override;
  end;

  {TAbstractDataStringGrid = class(TAbstractStringGrid)
  protected
    procedure CreateMemberObjects; override;
  end;}


  TAbstractScrollablePanel = class(TAbstractPanel)
  protected
    FViewMode           : TViewMode;
    FNetworkElementType : TNetworkElementType;
    FScrollBox          : TAbstractScrollBox;
    FHintDisplay        : TStatusBar;
    FPopulated          : boolean;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
    procedure SetPopulated(APopulated: boolean);virtual;
    procedure SetViewMode(AViewMode: TViewMode);virtual;
    procedure SetNetworkElementType(ANetworkElementType:TNetworkElementType); virtual;
  public
    procedure RestoreColourState; virtual;
    function ControlsOwner : TComponent;virtual;
    function ControlsParent : TWinControl;virtual;
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    procedure ShowCurrentHint; virtual;
    procedure ShowError(AMessage: string); virtual;
    procedure ShowWarning(AMessage: string); virtual;
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    procedure DoCopyToClipboard;override;
    procedure DoPrint;override;
    procedure DoExport(AFileName: string = '');override;
    property Populated : boolean read FPopulated write SetPopulated;
    property ViewMode       : TViewMode        read FViewMode       write SetViewMode;
    property NetworkElementType : TNetworkElementType  read FNetworkElementType write SetNetworkElementType;
  end;

  TAbstractDataDialogValidator = class(TAbstractAppObject)
  protected
    FViewMode           : TViewMode;
    FNetworkElementType : TNetworkElementType;
    FIdentifier         : integer;
    FTreeNodeIndex      : integer;
    FElementName        : String;
    FPanelOwner         : TComponent;
    FPanel              : TAbstractScrollablePanel;
    FTabShetCaption     : String;
    FActiveControl      : TWinControl;
    FErrorMessage       : WideString;
//    FErrorMessages    : TStringList;
    FAllErrorMessages   : TStringList;
    FOnValueChange      : TNotifyEvent;
    FOnDataChange       : TNotifyEvent;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); virtual;
    procedure OnEditControltExit(Sender: TObject); virtual;
    procedure OnStringGridColEnter(Sender: TObject;ACol, ARow: integer); virtual;
    procedure OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);  virtual;
    procedure SetViewMode(AViewMode: TViewMode);virtual;
    procedure SetNetworkElementType(ANetworkElementType:TNetworkElementType); virtual;
  public
    constructor Create(APanelOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function ProcessParameterChangeEvent : boolean; virtual;
    function ProcessMetaDataEvent : boolean; virtual;
    procedure ShowCurrentComponentHint; virtual;

    procedure ClearDataViewer;  virtual;
    procedure PopulateDataViewer;  virtual;
    procedure ShowWizardStep (ASequence : integer = 0); virtual;
    function DetermineWizardStatus (ASequence : integer = 0) : integer; virtual;

    function CanCopyToCLipboard: boolean; virtual;
    function CanExport: boolean; virtual;
    function CanPrint: boolean; virtual;
    procedure DoCopyToCLipboard; virtual;
    procedure DoExport(AFileName: string = ''); virtual;
    procedure DoPrint; virtual;
    procedure ExitCurrentEditControl;

    property ViewMode           : TViewMode                read FViewMode           write SetViewMode;
    property NetworkElementType : TNetworkElementType      read FNetworkElementType write SetNetworkElementType;
    property Panel              : TAbstractScrollablePanel read FPanel;
    property TabShetCaption     : String                   read FTabShetCaption     write FTabShetCaption;
    property OnValueChange      : TNotifyEvent             read FOnValueChange      write FOnValueChange;
    property OnDataChange       : TNotifyEvent             read FOnDataChange       write FOnDataChange;
    //property ErrorMessages    : TStringList              read FErrorMessages;
    property AllErrorMessages   : TStringList              read FAllErrorMessages;
    property Identifier         : integer                  read  FIdentifier        write FIdentifier;
    property TreeNodeIndex      : integer                  read  FTreeNodeIndex     write FTreeNodeIndex;
    property ElementName        : String                   read  FElementName       write FElementName;
  end;

  TAbstractDataPageControl = class(TAbstractPageControl)
  protected
    FLastTabSelected: string;
    FValidatorList: TObjectList;
    FPageContolWithFocus: boolean;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure ChangeActivePage(Page: TAbstractDataTabSheet);
  public
    procedure Change; override;
    function GetValidatorByClassName(AClassName: string): TAbstractDataDialogValidator; virtual;
    function GetValidatorByIndex(AIndex: integer): TAbstractDataDialogValidator; virtual;
    function AddValidator(AValidator:TAbstractDataDialogValidator): boolean; virtual;
    function HideAllTabSheets: boolean;
    function DeleteValidatorByClassName(AClassName: string): Boolean; virtual;
    function DeleteAllTabSheets: boolean;
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; virtual;
    function ProcessParameterChangeEvent : boolean;
    function ProcessmetaDataEvent : boolean;
    function ValidatorCount: integer;

    function CanCopyToCLipboard: boolean; virtual;
    function CanExport: boolean; virtual;
    function CanPrint: boolean; virtual;
    procedure DoCopyToCLipboard; virtual;
    procedure DoExport(AFileName: string = ''); virtual;
    procedure DoPrint; virtual;
    procedure ExitCurrentEditControl;
    procedure SelectLastActiveTabsheet;
  end;

  TAbstractOutputDialogValidator = class(TAbstractDataDialogValidator)
  public
    procedure ShowSelectedRecord(ASender: TObject;ACurrentRecord: integer); virtual;
    function GetNextSignificantRecord(ASender: TObject;ACurrentRecord: integer): integer; virtual;
    function GetPreviousSignificantRecord(ASender: TObject;ACurrentRecord: integer): integer; virtual;
    function CalculateMCMPerMonthGridValues(AGrid:TAbstractStringGrid): boolean;
//    property Identifier: integer read FIdentifier write FIdentifier;
  end;

  TAbstractNavigator = class(TAbstractPanel)
  protected
    FValidatorList             : TObjectList;
    FFirstRecord               : TFieldBitBtn;
    FPreviousRecord            : TFieldBitBtn;
    FNextRecord                : TFieldBitBtn;
    FLastRecord                : TFieldBitBtn;
    FCbxSelector               : TComboBox;
    FCurrentRecordDisplay      : TAbstractPanel;
    FRecordCount               : integer;
    FCurrentRecord             : integer;
    FOnSelectionChange         : TNotifyEvent;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnFirstRecordClck(Sender: TObject); virtual;
    procedure OnNextRecordClck(Sender: TObject); virtual;
    procedure OnPreviousRecordClck(Sender: TObject); virtual;
    procedure OnLastRecordClck(Sender: TObject); virtual;
    procedure SetRecordCount(AValue: integer); virtual;
    procedure SetCurrentRecord(AValue: integer); virtual;
    procedure DisplayCurrentRecord; virtual;
    procedure SetButtonsState; virtual;
    function CreateButton(AButtonKey: string; AAlign : TAlign; AAppModules : TAppModules): TFieldBitBtn; virtual;
    procedure SetEnabled(Value: Boolean); override;
    procedure OnComboBoxChange(ASender: TObject);
  public
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure Resize;override;
    property OnSelectionChange : TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
  end;

  TLoadCaseSelectorNavigator = class(TAbstractNavigator)
  protected
    procedure SetRecordCount(AValue: integer); override;
    function CreateComboBox(AAlign : TAlign; AAppModules : TAppModules): TComboBox;
    function PopulateLoadCaseSelector : boolean;
  public
    property  LoadCaseCount   : integer read FRecordCount write SetRecordCount;
    property  CurrentLoadCase : integer read FCurrentRecord write SetCurrentRecord;
  end;

  TMonthSelectorNavigator = class(TAbstractNavigator)
  protected
    FPreviousSignificantRecord : TFieldBitBtn;
    FNextSignificantRecord     : TFieldBitBtn;

    procedure CreateMemberObjects; override;
    procedure OnNextSignificantRecordClck(Sender: TObject);
    procedure OnPreviousSignificantRecordClck(Sender: TObject);
    procedure OnFirstRecordClck(Sender: TObject); override;
    procedure OnLastRecordClck(Sender: TObject); override;
    procedure SetButtonsState; override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetRecordCount(AValue: integer); override;
    function CreateComboBox(AAlign : TAlign; AAppModules : TAppModules): TComboBox;
    function PopulateMonthSelector : boolean;
  public
    procedure Resize;override;
    property  MonthCount   : integer read FRecordCount write SetRecordCount;
    property  CurrentMonth : integer read FCurrentRecord write SetCurrentRecord;
  end;

  TSequenceSelectorNavigator = class(TAbstractNavigator)
  protected
    procedure SetButtonsState; override;
    procedure SetRecordCount(AValue: integer); override;
    function CreateComboBox(AAlign : TAlign; AAppModules : TAppModules): TComboBox;
    function PopulateSequenceSelector : boolean;
  public
    property  SequenceCount   : integer read FRecordCount write SetRecordCount;
    property  CurrentSequence : integer read FCurrentRecord write SetCurrentRecord;
  end;

  TRISelector = class(TAbstractPanel)
  protected
    FRISelector:TCheckListBox;
    FBtnPanel,
    FEdtPanel : TPanel;
    FBtnAdd,
    FBtnDelete,
    FBtnOk,
    FBtnCancel: TButton;
    FEdtRecurranceInterval : TEdit;
    FLblRecurranceInterval : TLabel;
    FAssuranceInterval : integer;
    function GetRecurranceInterval: integer;
    function YRCGraphDataObject   :TAbstractYRCGraphDataObject;
    procedure SetRecurranceInterval(const AValue: integer);
    procedure CreateMemberObjects;override;
    procedure CenterControls;
    procedure OnAssuranceIntervalSelectorAddBtnClick(Sender: TObject);
    procedure OnAssuranceIntervalSelectorDelBtnClick(Sender: TObject);
    procedure OnAssuranceSelectorClick(Sender : TObject);

    function  InArray(AValue : integer; AArray : TIntegerArray) : boolean;
    procedure InsertIntoArray(var AArray : TIntegerArray; const AValue : integer);
    procedure DeleteFromArray(var AArray : TIntegerArray; const AValue : integer);

    property RISelector : TCheckListBox read FRISelector;
    property BtnAdd    : TButton read FBtnAdd;
    property BtnDelete : TButton read FBtnDelete;
    property EdtRecurrranceInterval : TEdit read FEdtRecurranceInterval;
  public
    function LanguageHasChanged: boolean; override;
    function GetCurrentRecurranceInterval : integer;
    procedure Resize; override;
    procedure PopulateRecurrance(AYearValues, ASavedValues:TIntegerArray;ANumberOfYUears : integer;AStochasticRan : boolean);
    procedure ReadRecurranceSaved(var ASavedValues: TIntegerArray; AYearValues : TIntegerArray);
    property RecurranceInterval : integer read GetRecurranceInterval write SetRecurranceInterval;
  end;

  TBoxPlotSeriesSelector = class(TAbstractPanel)
  protected
    FSeriesSelector:TCheckListBox;
    FLoadCaseSelector:TCheckListBox;
    FHearderPanel : TPanel;
    FBtnPanel,
    FEdtPanel : TPanel;
    FBtnAdd,
    FBtnDelete,
    FBtnOk,
    FBtnCancel: TButton;
    FEdtSeries : TEdit;
    FLblSeries ,
    FLblBottomChartLabel  : TLabel;
    FMonthTypeGroup       : TGroupBox;
    FMonthNumber          : TRadioButton;
    FMonthName            : TRadioButton;
    FLblSeriesSelector    : TLabel;
    FLblSeriesLoadCase    : TLabel;
    procedure CreateMemberObjects;override;
    procedure OnCheckedChange(Sender: TObject);
    procedure OnMonthTypeCheckedChange(Sender: TObject);
    procedure OnSelectionChange(Sender: TObject);
    procedure OnDeleteSelection(Sender: TObject);
    procedure OnAddSelection(Sender: TObject);
    procedure OnAnyKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnEditTextChange(Sender: TObject);
    procedure OnLoadCaseSelectorClick(Sender : TObject);
    procedure UnSelectLoadCases;
    procedure LoadFromViewIni;
    procedure SaveToViewIni;
  public
    function LanguageHasChanged: boolean; override;
    procedure Resize; override;
    function SeriesValuesCommatext: string;
    Function LoadCase : integer;
    property LoadCaseSelector : TCheckListBox read FLoadCaseSelector;
    property LblBottomChartLabel : TLabel read FLblBottomChartLabel;
    property MonthTypeGroup : TGroupBox read FMonthTypeGroup;
    property MonthNumber : TRadioButton read FMonthNumber;
    property MonthName : TRadioButton read FMonthName;
  end;

  TMonthlyDeficitGrid = class(TAbstractStringGrid)
  protected
    FHeading      : string;
    FCellColor : TColor;
    FCellColoring     : TStringList;
    procedure DrawCell(ACol, ARow: Integer; Rect: TRect; State: TGridDrawState); override;
    procedure ClearGrid;
    function IsColored(ACol, ARow : integer) : boolean;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure Resize; override;
    function LanguageHasChanged : boolean; override;
    function StudyHasChanged : boolean; override;
    function Initialise: boolean; override;
    property Heading : string read FHeading write FHeading;
    property CellColor : TColor read FCellColor write FCellColor;
    property Coloring     : TStringList read FCellColoring write FCellColoring;
  end;


  TIFRStatsComplianceGrid = class(TAbstractStringGrid)
  protected
    FHeading      : string;
    FCellColor : TColor;
    FCellColoring     : TStringList;
    procedure DrawCell(ACol, ARow: Integer; Rect: TRect; State: TGridDrawState); override;
    procedure ClearGrid;
    function IsColored(ACol, ARow : integer) : boolean;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure Resize; override;
    function LanguageHasChanged : boolean; override;
    function StudyHasChanged : boolean; override;
    function Initialise: boolean; override;
    property Heading : string read FHeading write FHeading;
    property CellColor : TColor read FCellColor write FCellColor;
    property Coloring     : TStringList read FCellColoring write FCellColoring;
  end;


  TGridType = (gtFiltered, gtNormal);
  TSupplyComplianceGrid = class(TAbstractStringGrid)
  protected
    FGridType : TGridType;
    FCellColor : TColor;
    FCellColoring     : TStringList;
    function IsColored(ACol, ARow : integer) : boolean;
    function GetGridType : TGridType;
    procedure SetGridType(AGridType : TGridType);
    procedure DrawCell(ACol, ARow: Integer; Rect: TRect; State: TGridDrawState); override;
    procedure ClearGrid;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure Resize; override;
    function  LanguageHasChanged : boolean; override;
    function  StudyHasChanged : boolean; override;
    function  Initialise: boolean; override;
    property  GridType : TGridType read GetGridType write SetGridType;
    property  CellColor : TColor read FCellColor write FCellColor;
    property  Coloring     : TStringList read FCellColoring write FCellColoring;
  end;

  TSupplyComplianceAggregateGrid = class(TAbstractStringGrid)
  protected
    FGridType : TGridType;
    FCellColor : TColor;
    FCellColoring     : TStringList;
    function IsColored(ACol, ARow : integer) : boolean;
    procedure DrawCell(ACol, ARow: Integer; Rect: TRect; State: TGridDrawState); override;
    procedure ClearGrid;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure Resize; override;
    function  LanguageHasChanged : boolean; override;
    function  StudyHasChanged : boolean; override;
    function  Initialise: boolean; override;
    property  CellColor : TColor read FCellColor write FCellColor;
    property  Coloring     : TStringList read FCellColoring write FCellColoring;
  end;

  TSelectionChange = procedure(ASelection: integer) of object;

  TSelector = class(TAbstractPanel)
   protected
    FName: TLabel;
    FSelector: TSpinEdit;
    FSelectionChange: TSelectionChange;
    FEditable : boolean;
    procedure CreateMemberObjects; override;
    procedure OnSelectorChanged(Sender: TObject);
    procedure CenterControls;
    procedure SelectorKeyPress(Sender: TObject; var Key: Char);
    procedure SelectorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnRequestPopup (Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    // Overriden from Delphi.
  public
    procedure Resize; override;
    procedure SetCaption(ACaption: string);
    procedure Select(AIndex: integer);
    procedure SetEnabled(AEnabled : boolean); override;
    function Populate(AMinValue,AMaxValue: integer): boolean;
    property OnSelectionChange: TSelectionChange read FSelectionChange write FSelectionChange;
    property Editable: boolean read FEditable write FEditable;
  end;



//=========================================Stomsa===================================//

  TExpandPanel = class(TAbstractPanel)
  protected
    FTopPanel : TPanel;
    FExpanded : Boolean;
    FTopColor : TColor;
    FTopHeight : Integer;
    FTopCaption : TCaption;
    FFullHeight : integer;
    FOnExpand : TNotifyEvent;

    procedure SetExpanded(Value : Boolean);
    procedure SetFullHeight(Value : Integer);
    procedure SetTopColor(Value : TColor);
    procedure SetTopHeight(Value : Integer);
    procedure SetTopCaption(Value : TCaption);
  public
    procedure CreateMemberObjects; override;
    //Constructor Create(AOwner : TComponent); override;
  published
    Property Align;
    Property Anchors;
    Property BevelInner;
    Property BevelOuter;
    Property BevelWidth;
    Property Caption;
    Property Color;
    Property TabOrder;
    property Expanded: Boolean Read FExpanded Write SetExpanded;
    property FullHeight: integer Read FFullHeight Write SetFullHeight;
    property TopCaption: TCaption Read FTopCaption Write SetTopCaption;
    Property TopColor: TColor Read FTopColor Write SetTopColor;
    property TopHeight: Integer Read FTopHeight Write SetTopHeight;
    property OnExpand: TNotifyEvent read FOnExpand write FOnExpand;
    procedure OnTopPanelClick(Sender : TObject);
  end;



  TextPos = (TopPos, BottomPos, LeftPos, RightPos);
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  THotBtn = class(TAbstractPanel)
  protected
    FTheImage   : TImage;
    FLabel      : TLabel;
    FText       : string;
    FUpState    : boolean;
    FEnabled    : boolean;
    FImageAlign : TAlign;
    FHotGlyph   : TBitmap;
    FPlainGlyph : TBitmap;
    FDisabledGlyph : TBitmap;

    procedure SetHotGlyph(Value: TBitmap);
    procedure SetPlainGlyph(Value: TBitmap);
    procedure SetDisabledGlyph(Value: TBitmap);
    procedure SetText(value : string);
    procedure SetImageAlign(Value : TAlign);
    procedure TCMMouseEnter(var Message : TMessage); message CM_MOUSEENTER;
    procedure TCMMouseLeave(var Message : TMessage); message CM_MOUSELEAVE;
    procedure TWMPaint(var Message : TWMPaint); message WM_PAINT;

    procedure Click; override;
    procedure MouseDown(Button: TMouseButton;Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton;Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    //constructor Create(AOwner : TComponent); override;
    procedure CreateMemberObjects; override;
    procedure SetEnabled(Value : boolean); override;
    procedure MseDwn(Sender : TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
    procedure MseUp(Sender : TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
    procedure MseMve(Sender : TObject; Shift: TShiftState; X, Y: Integer);
    procedure Clck(Sender : TObject);

    property TheLabel : TLabel read FLabel write FLabel;
    property TheImage : TImage read FTheImage write FTheImage;
    property UpState : boolean read FUpState write FUpState;
  published
    //sl 2000.08.04 - Add ACTION property to tie into the ActionList component
    property Action;
    property Align;
    property Anchors;
    property BevelOuter;
    property Enabled: boolean read FEnabled write SetEnabled;
    property ShowHint;
    property Visible;

    property GlyphHot: TBitmap read FHotGlyph write SetHotGlyph;
    property GlyphPlain: TBitmap read FPlainGlyph write SetPlainGlyph;
    property GlyphDisabled: TBitmap read FDisabledGlyph write SetDisabledGlyph;

    property Text : string read FText write SetText;
    property ImageAlign : TAlign read FImageAlign write SetImageAlign;

    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
  end;



  TFileEvent = procedure(Sender: TObject; FileName,FileDir : string) of Object;
  TFileSelectEvent = procedure(Sender: TObject; AIndex: integer) of Object;
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  TFileCollect = class(TCustomPanel)
  protected
    FTopPanel      : TPanel;
    FComboPanel    : TPanel;
    FButtonPanel   : TPanel;
    FDirectories   : TDirectoryListBox;
    FFiles         : TFileListBox;
    FSelectedDirs  : TListBox;
    FSelectedSplit : TSplitter;
    FSelectedFiles : TListBox;
    FDriveCombo    : TDriveComboBox;
    FFilterCombo   : TFilterComboBox;
    FAddBtn        : TSpeedButton;
    FSelectAllBtn  : TSpeedButton;
    FRemoveBtn     : TSpeedButton;
    FFileArea      : TStringGrid;
    FScenarioPath  : string;
    FDirectory     : string;

    FOnFileAdd            : TFileEvent;
    FOnFileAddComplete    : TNotifyEvent;
    FOnFileRemove         : TFileEvent;
    FOnFileRemoveComplete : TNotifyEvent;
    FOnSelectFile         : TFileSelectEvent;

    procedure ResizeFileAreaGrid;
    procedure DirectoryChange(Sender : TObject);
    procedure DriveChange(Sender : TObject);
    procedure FilterChange(Sender : TObject);
    procedure SelectAllBtnClick(Sender : TObject);
    procedure AddBtnClick(Sender : TObject);
    procedure FilesDblClick(Sender : TObject);
    procedure RemoveBtnClick(Sender : TObject);
    procedure RemoveDblClick(Sender : TObject);
    procedure SelectFileClick(Sender : TObject);
    procedure Set_ScenarioPath(AValue:string);
    procedure SelectedFile(var AFileDir,AFileName : string);
    function Get_SelectedFileNameByIndex(AIndex: integer): string;
  public
    constructor Create(AOwner : TComponent); override;

    procedure AddItem(AFileName, AFileDir : string);
    procedure ClearAll;
    property FileAreaGrid : TStringGrid read FFileArea;
    property SelectedFileNameByIndex[AIndex: integer] : string read Get_SelectedFileNameByIndex;
    property ScenarioPath : string read FScenarioPath write Set_ScenarioPath;
  published
    property Align;
    property Anchors;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;

    property Directory: String read FDirectory write FDirectory;

    property OnFileAdd: TFileEvent read FOnFileAdd write FOnFileAdd;
    property OnFileAddComplete: TNotifyEvent read FOnFileAddComplete write FOnFileAddComplete;
    property OnFileRemove: TFileEvent read FOnFileRemove write FOnFileRemove;
    property OnFileRemoveComplete: TNotifyEvent read FOnFileRemoveComplete write FOnFileRemoveComplete;
    property FOnFileSelected: TFileSelectEvent read FOnSelectFile write FOnSelectFile;
  end;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  JoinString = String[4];
  TextPosString = String[2];
  OptionRec = Record
    //Cell column and row numbers
    TheCol,TheRow : Integer;
    //Cell background colour
    TheColour : LongInt;
    //Font colour
    FontColour : LongInt;
    //Allow cell editing or not
    Locked : Boolean;

    TheJoin : JoinString;
    //blank string - nothing
    //'1   ' - Joined to cell to the left
    //' 1  ' - Joined to cell below
    //'  1 ' - Joined to cell to the right
    //'   1' - Joined to cell above
    TextPos : TextPosString;
    //first char
      //l - left
      //c - centre
      //r - right
    //second char
      //t - top
      //c - centre
      //b - bottom
//    TheFont : TFont;
  End;//record

  TMarkStringGrid = class(TStringGrid)
  protected
    OptionArray : Array of OptionRec;
    CurrentCell : Integer;
    FJoinResult : JoinString;
    HardCopy    : Boolean; // flag to check if we are printing or not
    PrintLeft,PrintTop : Integer;
    PrintScaling : Double;
    procedure DeleteCellValues(Index:Integer);
    function OriginalValues(TheCell:OptionRec):Boolean;
    function GetCellValues(ACol,ARow:Integer):OptionRec;
    procedure SetCellValues(TheCell:OptionRec);
    //private cell joining
    procedure Join2Cells(FirstCol,FirstRow,SecondCol,SecondRow:Integer);
    procedure Clear2Joins(FirstCol,FirstRow,SecondCol,SecondRow:Integer);
    function DoScale(TheVal:Double):Integer;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure SetEditText(ACol, ARow: Longint; const Value: String); override;

  public
    { Public declarations }
    Constructor Create (AOwner:TComponent); override;
    //Cell colouring
    function GetColour(ACol,ARow:Integer):LongInt;
    procedure SetColour(ACol,ARow:Integer;TheColour:LongInt);
    Procedure ResetCellColor(ACol,ARow:Integer);
    Procedure ResetGridColor;
    //Font colouring
    Function GetFontColour(ACol,ARow:Integer):LongInt;
    Procedure SetFontColour(ACol,ARow:Integer;TheColour:LongInt);
    Procedure ResetFontColor(ACol,ARow:Integer);
    Procedure ResetGridFontColor;
    //cell joining
    Function IsJoined(TheCol,TheRow:Integer):Boolean;
    Procedure SetJoin(FirstCol,FirstRow,SecondCol,SecondRow:Integer);
    Procedure ClearJoin(FirstCol,FirstRow,SecondCol,SecondRow:Integer);

    //Text positioning
    Procedure SetTextPos(ACol,ARow : Integer;HorPos,VertPos:AnsiChar);
    Procedure GetTextPos(ACol,ARow : Integer;Var HorPos,VertPos:AnsiChar);
    Procedure ClearTextPos(ACol,ARow : Integer);

    //Column processing
    Procedure SetColColours(ACol:Integer;BackColour,FontColour:LongInt);
    Procedure SetColLock(ACol:Integer);
    Procedure ClearColLock(ACol:Integer);

    //Row processing
    Procedure SetRowColours(ARow:Integer;BackColour,FontColour:LongInt);
    Procedure SetRowLock(ARow:Integer);
    Procedure ClearRowLock(ARow:Integer);

    //Cell locking
    Procedure SetCellLock(ACol, ARow: Longint);
    Procedure ClearCellLock(ACol, ARow: Longint);
    Function GetCellLock(ACol, ARow: LongInt):Boolean;
    Property JoinResult : JoinString Read FJoinResult;
    //Font
{    Procedure SetFont(ACol,ARow : Integer;TheFont:TFont);
    Function GetFont(ACol,ARow:Integer):TFont;
    Procedure ClearFont(ACol,ARow : Integer);}

    Procedure DoPrint(StartX,StartY:Integer;ScalePrint:Double);

  published
    { Published declarations }
  end;


//========================================stomsa=====================================//


implementation

uses

  System.UITypes,
  vcl.Clipbrd,
  Vcl.Printers,
  Vcl.Dialogs,
  Vcl.Themes,
  Math,
  UConstants,
  UUtilities,
  UDBConstants,
  UMainMenuEventType,
  UAbstractYRCModelDataObject,
  UErrorHandlingOperations;


Constructor TMarkStringGrid.Create (AOwner:TComponent);
Begin
  Inherited;

  Self.DefaultDrawing:=False; //have to have this or else the component
                              //overwrites a lot of my stuff from drawcell
  HardCopy := False; //don't send to the printer by default
End;//create

procedure TMarkStringGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
Var
  TmpCell,JoinedCell : OptionRec;
  TmpRect : TRect;
  StopJoin : Boolean;
  Tmp,tx,ty,StrIndex,tWidth,tHeight : Integer;
  tText : String;
  MyText : TStringList;
//  TmpFont : TFont;
Begin
  //Draw the cell according to the values specified in my array
  //Get the formatting
  TmpCell:=GetCellValues(ACol,ARow);

  //Get the cell dimensions
  TmpRect:=ARect;
  //Check if we should change the dimensions if this is a joined cell
  If IsJoined(ACol,ARow) Then Begin
    If TmpCell.TheJoin[1]='1' Then Begin
      //Have to call the other cell again or else this one won't be repainted
      //when it is scrolled open on its own
         { DONE -ome -cDrawing : I am not catering for multiple joined rows }
      ARect.Right:=ARect.Left-GridLineWidth;
      ARect.Left:=ARect.Right-ColWidths[ACol-1];
      DrawCell(ACol-1,ARow,ARect,AState);
      Exit; //Left:  skip - handle in other cell
    End;
    If TmpCell.TheJoin[4]='1' Then Begin
      //Have to call the other cell again or else this one won't be repainted
      //when it is scrolled open on its own
      ARect.Bottom:=ARect.Top-GridLineWidth;
      ARect.Top:=ARect.Bottom-RowHeights[ARow-1];
      DrawCell(ACol,ARow-1,ARect,AState);
      Exit; //Above: skip - handle in other cell
    End;
    If TmpCell.TheJoin[3]='1' Then Begin
      //Joined to the right
      StopJoin:=False;
      Tmp:=ACol+1;
      Repeat
        JoinedCell:=GetCellValues(Tmp,ARow);
        If JoinedCell.TheJoin[1]='1' Then
          TmpRect.Right:=TmpRect.Right+ColWidths[Tmp]+GridLineWidth
        Else
          StopJoin:=True;
        Inc(Tmp);
      until(StopJoin);
    End;
    If TmpCell.TheJoin[2]='1' Then Begin
      //Joined below
      StopJoin:=False;
      Tmp:=ARow+1;
      Repeat
        JoinedCell:=GetCellValues(ACol,Tmp);
        If JoinedCell.TheJoin[4]='1' Then
          TmpRect.Bottom:=TmpRect.Bottom+RowHeights[Tmp]+GridLineWidth
        Else
          StopJoin:=True;
        Inc(Tmp);
      until(StopJoin);
    End;
  End;//if joined

  //draw the inherited values only if cell is not joined from above or the left
  Inherited;

  //Set the colour of the cell
  If TmpCell.TheColour<>-1 Then
   Canvas.Brush.Color:=TmpCell.TheColour
  Else Begin
    //fixed cells
    If AState=[gdFixed] Then Canvas.Brush.Color:=FixedColor;
    //normal cells
    If AState=[] Then Canvas.Brush.Color:=Color;
  End;
  //focused cell

  //sl 2000.08.07 - Only highlight the cell if the Option is selected
  If (ACol=Col) and (ARow=Row) then
  begin
    if (goDrawFocusSelected in Options) Then
      Canvas.Brush.Color:=clActiveCaption
    else
      Canvas.Brush.Color:=Color;
  end;

  //Draw the background colour
  Canvas.FillRect(TmpRect);

  //If we are printing the produce the rectangle
  If HardCopy Then
    Printer.Canvas.Rectangle(
      DoScale(TmpRect.Left)+PrintLeft,
      DoScale(TmpRect.Top)+PrintTop,
      DoScale(TmpRect.Left+TmpRect.Right)+PrintLeft,
      DoScale(TmpRect.Top+TmpRect.Bottom)+PrintTop);

  //Cut up the text so that it will fit into the width of the column
  //first try and do this on word divisions, but if this does not work, then
  //simply chop off chars until it does
  tText:=Cells[ACol,ARow];
  MyText:=TStringList.Create;
  StrIndex:=0;
  MyText.Append(''); //add a blank string
  While tText<>'' Do Begin
    While (Canvas.TextWidth(MyText.Strings[StrIndex])<(TmpRect.Right-TmpRect.Left)-4) and (tText<>'') Do Begin
      MyText.Strings[strIndex]:=MyText.Strings[strIndex]+tText[1];
      tText:=Copy(tText,2,Length(tText));
    End;//while - adding text
    if (Canvas.TextWidth(MyText.Strings[StrIndex])>=(TmpRect.Right-TmpRect.Left)-4) Then Begin
      //Now check if we could have chopped this off sooner at the start of a word.
      If (Pos(' ',MyText.Strings[StrIndex])>0) Or (Pos(',',MyText.Strings[StrIndex])>0) Then Begin
        MyText.Append('');
        While (Copy(MyText.Strings[StrIndex],Length(MyText.Strings[StrIndex]),1)<>' ') And
          (Copy(MyText.Strings[StrIndex],Length(MyText.Strings[StrIndex]),1)<>',') Do Begin
          MyText.Strings[StrIndex+1]:=Copy(MyText.Strings[StrIndex],Length(MyText.Strings[StrIndex]),1)+MyText.Strings[StrIndex+1];
          MyText.Strings[StrIndex]:=Copy(MyText.Strings[StrIndex],1,Length(MyText.Strings[StrIndex])-1);
        End;//while
      End
      Else
        MyText.Append('');
      Inc(StrIndex);
    End;
  End;//while tText

  //Calculate the width and height of the text block
  tHeight:=Trunc(MyText.Count*Canvas.TextHeight(Cells[ACol,ARow])*1.1);
  tWidth:=0;
  StrIndex:=0;
  While StrIndex<MyText.Count Do Begin
    if Canvas.TextWidth(MyText.Strings[StrIndex])>tWidth Then
      tWidth:=Canvas.TextWidth(MyText.Strings[StrIndex]);
    Inc(StrIndex);
  End;

  //focused cell - change text colour
  //sl 2001.02.23 only highlight if we have godrawfocusselected in the option list
  If (ACol=Col) and (ARow=Row) and (goDrawFocusSelected in Options) Then
    Canvas.Font.Color:=clHighlightText
  Else Begin
    //Set the font colour
    If TmpCell.FontColour<>-1 Then
      Canvas.Font.Color:=TmpCell.FontColour
    Else
      Canvas.Font.Color:=clBlack;
  End;

  //Display the cell contents
  tx:=0;
  ty:=0;
  StrIndex:=0;
  While StrIndex<MyText.Count Do Begin
    //Position the text
    //horizontal
    If TmpCell.TextPos[1]='L' then tx:=TmpRect.Left + 3;
    If TmpCell.TextPos[1]='C' then tx:=TmpRect.Left+((TmpRect.Right-TmpRect.Left) Div 2) - Canvas.TextWidth(MyText.Strings[StrIndex]) Div 2;
    If TmpCell.TextPos[1]='R' then tx:=TmpRect.Right - 3 - Canvas.TextWidth(MyText.Strings[StrIndex]);
    //vertical
    If TmpCell.TextPos[2]='T' then ty:=TmpRect.Top + 3 + Trunc(Canvas.TextHeight(MyText.Strings[StrIndex])*1.1*(StrIndex));
    If TmpCell.TextPos[2]='C' then ty:=TmpRect.Top + (TmpRect.Bottom-TmpRect.Top) Div 2 - tHeight div 2 + Trunc(Canvas.TextHeight(MyText.Strings[StrIndex])*1.1*(StrIndex));
    If TmpCell.TextPos[2]='B' then ty:=TmpRect.Bottom - 3 - Trunc(Canvas.TextHeight(MyText.Strings[StrIndex])*1.1*(MyText.Count-StrIndex));

    //Display the text
    if (tx>=TmpRect.Left) And (tx<=TmpRect.Right) Then Begin
      if (ty>=TmpRect.Top) And (ty<=TmpRect.Bottom) Then Begin
        Canvas.TextOut(tx,ty,MyText.Strings[StrIndex]);
        If HardCopy Then
          Printer.Canvas.TextOut(DoScale(tx)+PrintLeft,DoScale(ty)+PrintTop,MyText.Strings[StrIndex]);
      End;
    End;
    Inc(StrIndex);
  end;

  //Reset the font again
//  Canvas.Font:=TmpFont;

  //further formatting
  //Draw the fixed cells in 3d
  If AState=[gdFixed] Then Begin
    //put in the 3d effect
    Canvas.Pen.Color:=cl3DLight;
    Canvas.MoveTo(TmpRect.Left+1,TmpRect.Top+1);
    Canvas.LineTo(TmpRect.Right-1,TmpRect.Top+1);
    Canvas.MoveTo(TmpRect.Left+1,TmpRect.Top+1);
    Canvas.LineTo(TmpRect.Left+1,TmpRect.Bottom-1);
    Canvas.Pen.Color:=cl3DDkShadow;
    Canvas.MoveTo(TmpRect.Right,TmpRect.Bottom);
    Canvas.LineTo(TmpRect.Right,TmpRect.Top+1);
    Canvas.MoveTo(TmpRect.Right,TmpRect.Bottom);
    Canvas.LineTo(TmpRect.Left+1,TmpRect.Bottom);
  End;

End;

Function TMarkStringGrid.OriginalValues(TheCell:OptionRec):Boolean;
Begin
  //checks to see if all the values have been set to their default values
  OriginalValues:=False; //default assumption that value have changed

  If TheCell.TextPos<>'LT' Then Exit;
  If TheCell.TheColour<>-1 Then Exit;
  If TheCell.FontColour<>-1 Then Exit;
  If TheCell.TheJoin<>'    ' Then Exit;
  If TheCell.Locked Then Exit;

  OriginalValues:=True; //if we get here then all the values are original
End;//Original values

Function TMarkStringGrid.GetCellValues(ACol,ARow:Integer):OptionRec;
Begin
  //Insert default not found values
  GetCellValues.TheColour:=-1;
  GetCellValues.FontColour:=-1;
  GetCellValues.TheJoin:='    ';
  GetCellValues.TheCol:=ACol;
  GetCellValues.TheRow:=ARow;
  GetCellValues.TextPos:='LT';
  GetCellValues.Locked:=False;
//  GetCellValues.TheFont:=Nil;

  If OptionArray=Nil Then Exit;
  //quick check to see if we are already on the correct cell
  If (OptionArray[CurrentCell].TheCol=ACol) And (OptionArray[CurrentCell].TheRow=ARow) Then Begin
    GetCellValues:=OptionArray[CurrentCell];
    Exit;
  End;

  //No, check through all the cells
  CurrentCell:=0;
  Repeat
    If (OptionArray[CurrentCell].TheCol=ACol) And (OptionArray[CurrentCell].TheRow=ARow) Then Begin
      GetCellValues:=OptionArray[CurrentCell];
      Exit;
    End;
    Inc(CurrentCell);
  Until(CurrentCell>Length(OptionArray)-1);
End;

Procedure TMarkStringGrid.SetCellValues(TheCell:OptionRec);
Var
  Count : Integer;
Begin
  //Set the colour
  //First check to make sure that this cell has not been specified before
  If OptionArray=Nil Then Begin
    //This is the first element - create it now
    SetLength(OptionArray,1);
    OptionArray[0]:=TheCell;
    If OriginalValues(OptionArray[0]) Then DeleteCellValues(0);
    Exit;
  End
  Else Begin
    Count:=0;
    Repeat
      If (OptionArray[Count].TheCol=TheCell.TheCol) And (OptionArray[Count].TheRow=TheCell.TheRow) Then Begin
        //Found the element - update the values
        OptionArray[Count]:=TheCell;
        //Exit immediately
        Exit;
      End;
      Inc(Count);
    Until(Count>Length(OptionArray)-1);
  End;//if

  //If we got to here then we could not find the element and we
  //must therefore create a new one
  Count:=Length(OptionArray);
  SetLength(OptionArray,Count+1);
  //Set the values
  OptionArray[Count]:=TheCell;
//  If OriginalValues(OptionArray[Count]) Then DeleteCellValues(Count);
End;//SetCellValues

Procedure TMarkStringGrid.DeleteCellValues(Index:Integer);
Begin
  While (Index<Length(OptionArray)-1) Do Begin
    OptionArray[Index]:=OptionArray[Index+1];
    Inc(Index);
  End;
  OptionArray:=Copy(OptionArray,0,Length(Optionarray)-1);
End;//deletecellvalues

//---- Colour operations -------------------------------------------------------
Function TMarkStringGrid.GetColour(ACol,ARow:Integer):LongInt;
Var
  TmpCell : OptionRec;
Begin
  //simple wrapper for the getcellvalues
  TmpCell:=GetCellValues(ACol,ARow);
  GetColour:=TmpCell.TheColour;
End;

Procedure TMarkStringGrid.SetColour(ACol,ARow:Integer;TheColour:LongInt);
Var
  TmpCell : OptionRec;
Begin
  //Simple wrapper for setcellvalues
  TmpCell:=GetCellValues(ACol,ARow);
  TmpCell.TheCol:=ACol;
  TmpCell.TheRow:=ARow;
  TmpCell.TheColour:=TheColour; //set the new colour
  //Set the colour
  SetCellValues(TmpCell);
End;

Procedure TMarkStringGrid.ResetCellColor(ACol,ARow:Integer);
Var
  TmpCell : OptionRec;
Begin
  //Simple wrapper for setcellvalues
  TmpCell:=GetCellValues(ACol,ARow);
  TmpCell.TheCol:=ACol;
  TmpCell.TheRow:=ARow;
  TmpCell.TheColour:=-1; //reset the colour
  //Set the colour
  SetCellValues(TmpCell);
End;

Procedure TMarkStringGrid.ResetGridColor;
Var
  Count:Integer;
Begin
  //Reset the colour of all the cells
  Count:=0;
  If OptionArray<>Nil Then Begin
    While (Count<Length(OptionArray)) Do Begin
      ResetCellColor(OptionArray[Count].TheCol,OptionArray[Count].TheRow);
      Inc(Count);
    End;
    Refresh;
  End;
End;

//Text colour operations -------------------------------------------------------
Function TMarkStringGrid.GetFontColour(ACol,ARow:Integer):LongInt;
Var
  TmpCell : OptionRec;
Begin
  //simple wrapper for the getcellvalues
  TmpCell:=GetCellValues(ACol,ARow);
  GetFontColour:=TmpCell.FontColour;
End;

Procedure TMarkStringGrid.SetFontColour(ACol,ARow:Integer;TheColour:LongInt);
Var
  TmpCell : OptionRec;
Begin
  //Simple wrapper for setcellvalues
  TmpCell:=GetCellValues(ACol,ARow);
  TmpCell.TheCol:=ACol;
  TmpCell.TheRow:=ARow;
  TmpCell.FontColour:=TheColour; //set the new colour
  //Set the colour
  SetCellValues(TmpCell);
End;

Procedure TMarkStringGrid.ResetFontColor(ACol,ARow:Integer);
Var
  TmpCell : OptionRec;
Begin
  //Simple wrapper for setcellvalues
  TmpCell:=GetCellValues(ACol,ARow);
  TmpCell.TheCol:=ACol;
  TmpCell.TheRow:=ARow;
  TmpCell.FontColour:=-1; //reset the colour
  //Set the colour
  SetCellValues(TmpCell);
End;

Procedure TMarkStringGrid.ResetGridFontColor;
Var
  Count:Integer;
Begin
  //Reset the colour of all the cells
  Count:=0;
  If OptionArray<>Nil Then Begin
    While (Count<Length(OptionArray)) Do Begin
      ResetFontColor(OptionArray[Count].TheCol,OptionArray[Count].TheRow);
      Inc(Count);
    End;
    Refresh;
  End;
End;

//---- Join procedures ---------------------------------------------------------

Function TMarkStringGrid.IsJoined(TheCol,TheRow:Integer):Boolean;
Var
  Tmpcell : OptionRec;
Begin
  //Returns a boolean indicating whether this cell is joined with another one
  TmpCell:=GetCellValues(TheCol,TheRow);
  If TmpCell.TheJoin='    ' Then
    IsJoined:=False
  Else
    IsJoined:=True;
  FJoinResult:=TmpCell.TheJoin;

End;//IsJoined

Procedure TMarkStringGrid.Join2Cells(FirstCol,FirstRow,SecondCol,SecondRow:Integer);
Var
  CanJoin : Boolean;
  FirstCell,SecondCell : OptionRec;
Begin
  //Join 2 adjoining cells
  //First make sure that the cells are adjoining
  CanJoin:=True;
  If Abs(FirstCol-SecondCol)>1 Then Begin
    If Abs(FirstRow-SecondRow)>1 Then CanJoin:=False;
  End;
  //Check that it is not a join to itself
  If (FirstCol=SecondCol) And (FirstRow=SecondRow) Then CanJoin:=False;

  If CanJoin=False Then Exit;

  //Get the values of the current cells
  FirstCell:=GetCellValues(FirstCol,FirstRow);
  SecondCell:=GetCellValues(SecondCol,SecondRow);
  //Now see how the join hangs together
  //Join values are as follows:
    //'1   ' - Joined to cell to the left
    //' 1  ' - Joined to cell below
    //'  1 ' - Joined to cell to the right
    //'   1' - Joined to cell above

  //if second cell is to right of first cell
  If (SecondCol=FirstCol+1) And (SecondRow=FirstRow) Then Begin
    FirstCell.TheJoin[3]:='1';
    SecondCell.TheJoin[1]:='1';
  End;
  //if second cell is to left of first cell
  If (SecondCol=FirstCol-1) And (SecondRow=FirstRow) Then Begin
    FirstCell.TheJoin[1]:='1';
    SecondCell.TheJoin[3]:='1';
  End;
  //if second cell is above first cell
  If (SecondCol=FirstCol) And (SecondRow=FirstRow-1) Then Begin
    FirstCell.TheJoin[4]:='1';
    SecondCell.TheJoin[2]:='1';
  End;
  //if second cell is below first cell
  If (SecondCol=FirstCol) And (SecondRow=FirstRow+1) Then Begin
    FirstCell.TheJoin[2]:='1';
    SecondCell.TheJoin[4]:='1';
  End;
  //Update the values
  SetCellValues(FirstCell);
  SetCellValues(SecondCell);
end;//SetJoin

Procedure TMarkStringGrid.Clear2Joins(FirstCol,FirstRow,SecondCol,SecondRow:Integer);
Var
  CanJoin : Boolean;
  FirstCell,SecondCell : OptionRec;
Begin
  //Clear join from 2 adjoining cells
  //First make sure that the cells are adjoining
  CanJoin:=True;
  If Abs(FirstCol-SecondCol)>1 Then Begin
    If Abs(FirstRow-SecondRow)>1 Then CanJoin:=False;
  End;
  If CanJoin=False Then Exit;

  //Get the values of the current cells
  FirstCell:=GetCellValues(FirstCol,FirstRow);
  SecondCell:=GetCellValues(SecondCol,SecondRow);
  //Now see how the join hangs together
  //Join values are as follows:
    //'1   ' - Joined to cell to the left
    //' 1  ' - Joined to cell below
    //'  1 ' - Joined to cell to the right
    //'   1' - Joined to cell above

  //if second cell is to right of first cell
  If (SecondCol=FirstCol+1) And (SecondRow=FirstRow) Then Begin
    FirstCell.TheJoin[3]:=' ';
    SecondCell.TheJoin[1]:=' ';
  End;
  //if second cell is to left of first cell
  If (SecondCol=FirstCol-1) And (SecondRow=FirstRow) Then Begin
    FirstCell.TheJoin[1]:=' ';
    SecondCell.TheJoin[3]:=' ';
  End;
  //if second cell is above first cell
  If (SecondCol=FirstCol) And (SecondRow=FirstRow-1) Then Begin
    FirstCell.TheJoin[4]:=' ';
    SecondCell.TheJoin[2]:=' ';
  End;
  //if second cell is below first cell
  If (SecondCol=FirstCol) And (SecondRow=FirstRow+1) Then Begin
    FirstCell.TheJoin[2]:=' ';
    SecondCell.TheJoin[4]:=' ';
  End;
  //Update the values
  SetCellValues(FirstCell);
  SetCellValues(SecondCell);
End;//Clear2Joins;

Procedure TMarkStringGrid.SetJoin(FirstCol,FirstRow,SecondCol,SecondRow:Integer);
Var
  TmpCol,StartRow,TmpRow:Integer;
  ColsDone,RowsDone : Boolean;
Begin
  //Wrapper for Join2Cells so that it is possible to join multiple cells in
  //one go

  TmpCol:=FirstCol;
  ColsDone:=False;
  Repeat
    TmpRow:=FirstRow;
    StartRow:=FirstRow;
    RowsDone:=False;
    Repeat
      Join2Cells(FirstCol,StartRow,TmpCol,TmpRow); //Do the Join
      If SecondRow>FirstRow Then Begin
        Inc(TmpRow);
        If TmpRow>SecondRow Then RowsDone:=True;
        If TmpRow-StartRow>1 Then Inc(StartRow);
      End;
      If SecondRow<FirstRow Then Begin
        Dec(TmpRow);
        If TmpRow<SecondRow Then RowsDone:=True;
        If StartRow-TmpRow>1 Then Dec(StartRow);
      End;
      If SecondRow=FirstRow Then RowsDone:=True;
    Until(RowsDone);

    If SecondCol>FirstCol Then Begin
      Inc(TmpCol);
      If TmpCol>SecondCol Then ColsDone:=True;
      If TmpCol-FirstCol>1 Then Inc(FirstCol);
    End;
    If SecondCol<FirstCol Then Begin
      Dec(TmpCol);
      If TmpCol<SecondCol Then ColsDone:=True;
      If FirstCol-TmpCol>1 Then Inc(FirstCol);
    End;
    If SecondCol=FirstCol Then ColsDone:=True;
  Until(ColsDone);
End;//SetJoin

Procedure TMarkStringGrid.ClearJoin(FirstCol,FirstRow,SecondCol,SecondRow:Integer);
Var
  TmpCol,StartRow,TmpRow:Integer;
  ColsDone,RowsDone : Boolean;
Begin
  //Wrapper for Clear2Joins so that multiple cells can be cleared in one shot
  TmpCol:=FirstCol;
  ColsDone:=False;
  Repeat
    TmpRow:=FirstRow;
    StartRow:=FirstRow;
    RowsDone:=False;
    Repeat
      Clear2Joins(FirstCol,StartRow,TmpCol,TmpRow); //clear the Join
      If SecondRow>FirstRow Then Begin
        Inc(TmpRow);
        If TmpRow>SecondRow Then RowsDone:=True;
        If TmpRow-StartRow>1 Then Inc(StartRow);
      End;
      If SecondRow<FirstRow Then Begin
        Dec(TmpRow);
        If TmpRow<SecondRow Then RowsDone:=True;
        If StartRow-TmpRow>1 Then Dec(StartRow);
      End;
      If SecondRow=FirstRow Then RowsDone:=True;
    Until(RowsDone);

    If SecondCol>FirstCol Then Begin
      Inc(TmpCol);
      If TmpCol>SecondCol Then ColsDone:=True;
      If TmpCol-FirstCol>1 Then Inc(FirstCol);
    End;
    If SecondCol<FirstCol Then Begin
      Dec(TmpCol);
      If TmpCol<SecondCol Then ColsDone:=True;
      If FirstCol-TmpCol>1 Then Inc(FirstCol);
    End;
    If SecondCol=FirstCol Then ColsDone:=True;
  Until(ColsDone);

End;//ClearJoin

//--- Text positioning ---------------------------------------------------------
Procedure TMarkStringGrid.SetTextPos(ACol,ARow : Integer;HorPos,VertPos:AnsiChar);
Var
  TmpCell : OptionRec;
Begin
  //Set the text position
  TmpCell:=GetCellValues(ACol,ARow); //get the current values if any
  //Check the positioning
  Horpos:=UpCase(Horpos);
  If Not CharInSet(Horpos,['L','C','R']) Then Exit;
  Vertpos:=UpCase(Vertpos);
  If Not CharInSet(Vertpos, ['T','C','B']) Then Exit;
  //Assign the new values
  TmpCell.TextPos:=Horpos+VertPos;
  SetCellValues(TmpCell);
End;//SetTextPos

Procedure TMarkStringGrid.GetTextPos(ACol,ARow : Integer;Var HorPos,VertPos:AnsiChar);
Var
  TmpCell : OptionRec;
Begin
  //return the current text positioning
  TmpCell:=GetCellValues(ACol,ARow); //get the current values if any
  HorPos:=TmpCell.TextPos[1];
  VertPos:=TmpCell.TextPos[2];
End;//GetTextPos

Procedure TMarkStringGrid.ClearTextPos(ACol,ARow : Integer);
Var
  TmpCell : OptionRec;
Begin
  //Set the text position
  TmpCell:=GetCellValues(ACol,ARow); //get the current values if any
  TmpCell.TextPos:='LT';//default values
  SetCellValues(TmpCell);
End;//ClearTextPos

//Column processing-------------------------------------------------------------
Procedure TMarkStringGrid.SetColColours(ACol:Integer;BackColour,FontColour:LongInt);
Var
  TmpRow : Integer;
Begin
  TmpRow:=Self.FixedRows; //start after the fixed rows
  While TmpRow<Self.RowCount Do Begin
    //Set the background if it is not a bypass value
    If BackColour<>-1 Then
      SetColour(ACol,TmpRow,BackColour);
    //Set the font colour if it is not a bypass value
    If FontColour<>-1 Then
      SetFontColour(ACol,TmpRow,FontColour);
    Inc(TmpRow);
  End;
End;//SetColColours;

Procedure TMarkStringGrid.SetColLock(ACol:Integer);
Var
  TmpRow : Integer;
Begin
  TmpRow:=Self.FixedRows; //start after the fixed rows
  While TmpRow<Self.RowCount Do Begin
    SetCellLock(ACol,TmpRow);
    Inc(TmpRow);
  End;
End;//SetColLock

Procedure TMarkStringGrid.ClearColLock(ACol:Integer);
Var
  TmpRow : Integer;
Begin
  TmpRow:=Self.FixedRows; //start after the fixed rows
  While TmpRow<Self.RowCount Do Begin
    ClearCellLock(ACol,TmpRow);
    Inc(TmpRow);
  End;
End;//ClearColLock

//Row processing ---------------------------------------------------------------
Procedure TMarkStringGrid.SetRowColours(ARow:Integer;BackColour,FontColour:LongInt);
Var
  TmpCol : Integer;
Begin
  TmpCol:=Self.FixedCols; //start after the fixed columns
  While TmpCol<Self.ColCount Do Begin
    //Set the background if it is not a bypass value
    If BackColour<>-1 Then
      SetColour(TmpCol,ARow,BackColour);
    //Set the font colour if it is not a bypass value
    If FontColour<>-1 Then
      SetFontColour(TmpCol,ARow,FontColour);
    Inc(TmpCol);
  End;
End;//SetRowColours;

Procedure TMarkStringGrid.SetRowLock(ARow:Integer);
Var
  TmpCol : Integer;
Begin
  TmpCol:=Self.FixedCols; //start after the fixed columns
  While TmpCol<Self.ColCount Do Begin
    SetCellLock(TmpCol,ARow);
    Inc(TmpCol);
  End;
End;//SetRowLock

Procedure TMarkStringGrid.ClearRowLock(ARow:Integer);
Var
  TmpCol : Integer;
Begin
  TmpCol:=Self.FixedCols; //start after the fixed columns
  While TmpCol<Self.ColCount Do Begin
    ClearCellLock(TmpCol,ARow);
    Inc(TmpCol);
  End;
End;//SetRowLock

//Cell locking -----------------------------------------------------------------
Procedure TMarkStringGrid.SetEditText(ACol, ARow: Longint; const Value: String);
Var
  TmpCell : OptionRec;
Begin
  //Check if the cell is locked and if it is then don't allow the user to edit the values
  TmpCell:=GetCellValues(ACol,ARow); //get the current values if any
  If TmpCell.Locked Then Begin
    Beep;
    Exit;
  End;
  //proceed with the normal call if it is unlocked
  Inherited;
End;

Procedure TMarkStringGrid.SetCellLock(ACol, ARow: Longint);
Var
  TmpCell : OptionRec;
Begin
  //Set the cell lock
  TmpCell:=GetCellValues(ACol,ARow); //get the current values if any
  TmpCell.Locked:=True;
  SetCellValues(TmpCell);
End;//setcelllock

Procedure TMarkStringGrid.ClearCellLock(ACol, ARow: Longint);
Var
  TmpCell : OptionRec;
Begin
  //Set the cell lock
  TmpCell:=GetCellValues(ACol,ARow); //get the current values if any
  TmpCell.Locked:=False;
  SetCellValues(TmpCell);
End;//setcelllock

Function TMarkStringGrid.GetCellLock(ACol, ARow: LongInt):Boolean;
Var
  TmpCell : OptionRec;
Begin
  //Set the cell lock
  TmpCell:=GetCellValues(ACol,ARow); //get the current values if any
  GetCellLock:=TmpCell.Locked;
End;//getcelllock


//--- Printing ---------------------------------------------------------------
Function TMarkStringGrid.DoScale(TheVal:Double):Integer;
Begin
  DoScale:=Round(TheVal*PrintScaling);
End;

Procedure TMarkStringGrid.DoPrint(StartX,StartY:Integer;ScalePrint:Double);
Begin
  //Output the grid to the printer
  HardCopy:=True;
  PrintLeft:=StartX;
  PrintTop:=StartY;
  PrintScaling:=ScalePrint;
  Self.Refresh;
  HardCopy:=False;
End;





constructor TFileCollect.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Self.BevelOuter := bvNone;
  //Bottom Panel
  FTopPanel := TPanel.Create(Self);
  FTopPanel.Parent := Self;
  FTopPanel.Brush.Color := clBlue;
  FTopPanel.Align := alTop;
  FTopPanel.Height := 47;

  //Directory List Box
  FDirectories := TDirectoryListBox.Create(Self);
  FDirectories.Parent := Self;
  FDirectories.Align := alLeft;
  FDirectories.Width := 161;
  FDirectories.Ctl3D := false;
  FDirectory := FDirectories.Directory;
  FDirectories.OnChange := DirectoryChange;

  //File List Box
  FFiles := TFileListBox.Create(Self);
  FFiles.Parent := Self;
  FFiles.MultiSelect := true;
  FFiles.Align := alLeft;
  FFiles.Width := 180;
  FFiles.Ctl3D := False;
  FFiles.Mask := '*.INC';
  FFiles.OnDblClick := FilesDblClick;

  //Selected Directories List Box
  FSelectedDirs := TListBox.Create(Self);
  FSelectedDirs.Parent := Self;
  FSelectedDirs.Align := alLeft;
  FSelectedDirs.Width := 84;
  FSelectedDirs.OnClick := SelectFileClick;

  //Spiltter
  FSelectedSplit := TSplitter.Create(Self);
  FSelectedSplit.Parent := Self;
  FSelectedSplit.Align := alLeft;
  FSelectedSplit.Left := 400;
  FSelectedSplit.Width := 2;

  //Selected Files List Box
  FSelectedFiles := TListBox.Create(Self);
  FSelectedFiles.Parent := Self;
  FSelectedFiles.Align := alClient;
  FSelectedFiles.OnDblClick := RemoveDblClick;
  FSelectedFiles.OnClick     := SelectFileClick;

  //Combo Panel
  FFileArea             := TStringGrid.Create(Self);
  FFileArea.Parent      := Self;
  FFileArea.Align       := alRight;
  FFileArea.ColCount    := 2;
  FFileArea.RowCount    := 2;
  FFileArea.FixedRows   := 1;
  FFileArea.FixedCols   := 1;
  FFileArea.Options     := FFileArea.Options  + [goDrawFocusSelected,goEditing,goAlwaysShowEditor];
  FFileArea.OnClick     := SelectFileClick;
  FFileArea.DefaultRowHeight  := 15;
  FFileArea.DefaultColWidth   := 100;
  FFileArea.Cells[0,0] := 'MAR';
  FFileArea.Cells[1,0] := 'Catchment Area';

  //Combo Panel
  FComboPanel := TPanel.Create(Self);
  FComboPanel.Parent := FTopPanel;
  FComboPanel.Brush.Color := clBlue;
  FComboPanel.BevelOuter := bvNone;
  FComboPanel.Align := alLeft;
  FComboPanel.Width := 161;

  //Drive Combo Box
  FDriveCombo := TDriveComboBox.Create(Self);
  FDriveCombo.Parent := FComboPanel;
  FDriveCombo.Left := 2;
  FDriveCombo.Top := 2;
  FDriveCombo.Width := 161;
  FDriveCombo.OnChange :=  DriveChange;

  //Filter Combo Box;
  FFilterCombo := TFilterComboBox.Create(Self);
  FFilterCombo.Parent := FComboPanel;
  FFilterCombo.Left := 2;
  FFilterCombo.Top := 24;
  FFilterCombo.Width := 161;
  FFilterCombo.Filter := 'INC Files (*.INC)|*.INC';
  FFilterCombo.FileList := FFiles;
  FFilterCombo.OnChange := FilterChange;

  //Button Panel
  FButtonPanel := TPanel.Create(Self);
  FButtonPanel.Parent := FTopPanel;
  FButtonPanel.Brush.Color := clBlue;
  FButtonPanel.BevelOuter := bvNone;
  FButtonPanel.Align := alClient;
  FButtonPanel.Width := 500;

  //Select All Button
  FSelectAllBtn := TSpeedButton.Create(Self);
  FSelectAllBtn.Parent := FButtonPanel;
  FSelectAllBtn.Flat := true;
  FSelectAllBtn.Align := alLeft;
  FSelectAllBtn.Width := 60;
  FSelectAllBtn.Caption := 'Select All';
  FSelectAllBtn.OnClick := SelectAllBtnClick;

  //Add Button
  FAddBtn := TSpeedButton.Create(Self);
  FAddBtn.Parent := FButtonPanel;
  FAddBtn.Flat := true;
  FAddBtn.Align := alLeft;
  FAddBtn.Width := 120;
  FAddBtn.Caption := 'Add >>>>>>>>';
  FAddBtn.OnClick := AddBtnClick;

  //Remove Button
  FRemoveBtn := TSpeedButton.Create(Self);
  FRemoveBtn.Parent := FButtonPanel;
  FRemoveBtn.Flat := true;
  FRemoveBtn.Align := alClient;
  FRemoveBtn.Caption := '<<<<<< Remove';
  FRemoveBtn.OnClick := RemoveBtnClick;
end;

//---- ---- INTERNAL EVENT HANDLERS ---- ----

procedure TFileCollect.DirectoryChange(Sender : TObject);
begin
  inherited;
  FFiles.Directory := FDirectories.Directory;
  FDirectory := FDirectories.Directory;
end;

procedure TFileCollect.DriveChange(Sender : TObject);
begin
  inherited;
  FDirectories.Drive := FDriveCombo.Drive;
end;

procedure TFileCollect.FilterChange(Sender : TObject);
begin
  inherited;
  FFiles.Mask := FFilterCombo.Mask;
end;

//---- ---- EXTERNAL EVENT HANDLERS ---- ----

procedure TFileCollect.SelectAllBtnClick(Sender : TObject);
var
  Loop : integer;
begin
  inherited Click;
  for loop := 0 to (FFiles.Items.Count - 1) do
    FFiles.Selected[Loop] := true;
end;

procedure TFileCollect.AddBtnClick(Sender : TObject);
var
  loop : integer;
begin
  inherited Click;
  for loop := 0 to (FFiles.Items.Count - 1) do
  begin
    if FFiles.Selected[loop] and ((FSelectedFiles.Items.IndexOf(FFiles.Items[loop]) = -1) or
       (FSelectedDirs.Items.IndexOf(FDirectories.Directory) = -1))  then
    begin
      AddItem(FFiles.Items[loop],FDirectories.Directory);
    end;
  end;
  ResizeFileAreaGrid;
  if Assigned(FOnFileAddComplete) then FOnFileAddComplete(Sender);
end;

procedure TFileCollect.FilesDblClick(Sender : TObject);
begin
  inherited DblClick;
  if (FSelectedFiles.Items.IndexOf(FFiles.Items[FFiles.ItemIndex]) = -1) or
     (FSelectedDirs.Items.IndexOf(FDirectories.Directory) = -1) then
  begin
      AddItem(FFiles.Items[FFiles.ItemIndex],FDirectories.Directory);
  end;
  ResizeFileAreaGrid;
  if Assigned(FOnFileAddComplete) then FOnFileAddComplete(Sender);
end;

procedure TFileCollect.RemoveBtnClick(Sender : TObject);
var
  loop : integer;
begin
  inherited Click;
  loop := FSelectedFiles.Items.Count - 1;
  while loop <> -1 do
  begin
    if FSelectedFiles.Selected[loop] then
    begin
      if Assigned(FOnFileRemove) then
        FOnFileRemove(Sender,FSelectedFiles.Items[Loop],FSelectedDirs.Items[Loop]+'\');
      FSelectedFiles.Items.Delete(Loop);
      FSelectedDirs.Items.Delete(Loop);
    end;
    Dec(loop);
  end;
  ResizeFileAreaGrid;
  if Assigned(FOnFileRemoveComplete) then FOnFileRemoveComplete(Sender);
end;

procedure TFileCollect.RemoveDblClick(Sender : TObject);
begin
  inherited DblClick;
  if Assigned(FOnFileRemove) then
    FOnFileRemove(Sender,FSelectedFiles.Items[FSelectedFiles.ItemIndex],FSelectedDirs.Items[FSelectedFiles.ItemIndex]+'\');
    FSelectedDirs.Items.Delete(FSelectedFiles.ItemIndex);
    FSelectedFiles.Items.Delete(FSelectedFiles.ItemIndex);
    ResizeFileAreaGrid;
  if Assigned(FOnFileRemoveComplete) then FOnFileRemoveComplete(Sender);
end;

//---- ---- METHODS ---- ----

procedure TFileCollect.AddItem(AFileName, AFileDir : string);
var
  LNewFileName,
  LNewFileNameStr: string;
begin
  if(FSelectedFiles.Items.IndexOf(AFileName) >= 0) then Exit;

  if(Trim(FScenarioPath) = '') then
  begin
     FScenarioPath := AFileDir;
  end;

  if(FScenarioPath <> AFileDir) then
  begin
    LNewFileNameStr   :=  IncludeTrailingPathDelimiter(AFileDir ) + AFileName;
    if(MessageDlg('The selected file (' + LNewFileNameStr + ') will be copied to the scenario folder before being added. Continue?' ,mtConfirmation,mbOKCancel,0) <> mrOk) then
      Exit;
    LNewFileName   :=  IncludeTrailingPathDelimiter(FScenarioPath ) + AFileName;
    if FileExists(LNewFileName) then
    begin
      if(MessageDlg('File already exist in the destination folder. Do you want to override it?' ,mtConfirmation,mbOKCancel,0) <> mrOk) then
        Exit;
    end;
    if not SysUtils.DirectoryExists(FScenarioPath) then
      SysUtils.ForceDirectories(FScenarioPath);
    CopyFile(PChar(LNewFileNameStr),PChar(LNewFileName),False);
  end;

  FSelectedFiles.Items.Add(AFileName);
  FSelectedDirs.Items.Add(FScenarioPath);
  ResizeFileAreaGrid;
  if Assigned(FOnFileAdd) then
    FOnFileAdd(Self,AFileName,FScenarioPath);
end;

procedure TFileCollect.ClearAll;
begin
  FSelectedDirs.Clear;
  FSelectedFiles.Clear;
  ResizeFileAreaGrid;
end;

procedure TFileCollect.ResizeFileAreaGrid;
var
  LIndex : integer;
begin
  if(FSelectedFiles.Items.Count = 0) then
    FFileArea.RowCount := 2
  else
    FFileArea.RowCount := FSelectedFiles.Items.Count+1;
  for LIndex := 1 to FFileArea.RowCount-1 do
     FFileArea.Rows[Lindex].Clear;
end;

procedure TFileCollect.SelectFileClick(Sender: TObject);
var
  LIndex : integer;
begin
  if Assigned(FOnSelectFile) then
  begin
    LIndex := -1;
    if(Sender = FSelectedFiles) then
      LIndex := FSelectedFiles.ItemIndex;
    if(Sender = FSelectedDirs) then
      LIndex := FSelectedDirs.ItemIndex;
    if(Sender = FFileArea) then
      LIndex := FFileArea.Row-1;
    if(LIndex >= 0) then
      FOnSelectFile(Self,LIndex);
  end;
end;

procedure TFileCollect.SelectedFile(var AFileDir,AFileName : string);
var
  LIndex : integer;
begin
  AFileName := '';
  AFileDir  := '';
  LIndex := -1;
  if(FSelectedFiles.ItemIndex >= 0) then
    LIndex := FSelectedFiles.ItemIndex;
  if(LIndex = -1) and (FSelectedDirs.ItemIndex >= 0) then
    LIndex := FSelectedDirs.ItemIndex;
  if(LIndex = -1) and (FFileArea.Row > 0)  then
    LIndex := FFileArea.Row-1;

  if(LIndex >= 0) then
  begin
    if(FSelectedFiles.Items.Count > 0) and (LIndex < FSelectedFiles.Items.Count) then
      AFileName := FSelectedFiles.Items[LIndex];
    if(FSelectedDirs.Items.Count > 0) and (LIndex < FSelectedDirs.Items.Count) then
      AFileDir  := FSelectedDirs.Items[LIndex];
  end;
end;

function TFileCollect.Get_SelectedFileNameByIndex(AIndex: integer): string;
var
  LPath,
  LFileName : string;
begin
  Result := '';
  if(FSelectedFiles.Items.Count > 0) and (AIndex < FSelectedFiles.Items.Count) then
  begin
    if(FSelectedDirs.Items.Count > 0) and (AIndex < FSelectedDirs.Items.Count) then
    begin
      LFileName := FSelectedFiles.Items[AIndex];
      LPath  := FSelectedDirs.Items[AIndex];
      Result := IncludeTrailingPathDelimiter(LPath) + ExtractFileName(LFileName);
    end;
  end;
end;

procedure TFileCollect.Set_ScenarioPath(AValue: string);
begin
  if SysUtils.DirectoryExists(AValue) then
  begin
    FDirectories.Directory  := AValue;
    FDriveCombo.Drive       := FDirectories.Drive;
    FDirectories.Directory  := AValue;
    FScenarioPath           := AValue;
  end;
end;



//constructor THotBtn.Create(AOwner : TComponent);
procedure THotBtn.CreateMemberObjects;
const OPNAME = 'THotBtn.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    Width := 70;
    Height := 52;
    UpState := false;
    BevelOuter := bvNone;
    caption := '';

    //should actually draw everything on a canvas (text + image)
    //can then control alignment myself, however can't disenable stuff

    TheLabel := TLabel.Create(self);
    Thelabel.Parent := Self;
    TheLabel.Caption := text;
    TheLabel.Alignment := taCenter;
    TheLabel.Align := alClient;
    TheLabel.OnMouseDown := MseDwn;
    TheLabel.OnMouseUp := MseUp;
    TheLabel.OnMouseMove := MseMve;
    TheLabel.OnClick := Clck;
    TheLabel.Enabled := true;

    TheImage := TImage.Create(self);
    TheImage.Parent := Self;
    TheImage.Center := true;
    TheImage.Transparent := true;
    TheImage.OnMouseDown := MseDwn;
    TheImage.OnMouseUp := MseUp;
    TheImage.OnMouseMove := MseMve;
    TheImage.OnClick := Clck;
    TheImage.Enabled := true;

    Enabled := true;

    ImageAlign := alTop;

    FHotGlyph := TBitmap.Create;
    FPlainGlyph := TBitmap.Create;
    FDisabledGlyph := TBitmap.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THotBtn.SetEnabled(Value : boolean);
const OPNAME = 'THotBtn.SetEnabled';
begin
  try
    if FEnabled <> Value then
    begin
      FEnabled := Value;
      TheImage.Enabled := Value;
      TheLabel.Enabled := Value;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THotBtn.SetImageAlign(Value : TAlign);
const OPNAME = 'THotBtn.SetImageAlign';
begin
  try
    FImageAlign := Value;
    TheImage.Align := Value;
    case Value of
      alTop,alBottom : begin
                         TheImage.Height := Self.Height-15;// FPlainGlyph.Height + 6
                         TheLabel.Layout := tlCenter;
                         TheLabel.Alignment := taCenter;
                       end;
      alLeft,alRight : begin
                         if FPlainGlyph <> nil then
                           TheImage.Width := FPlainGlyph.Width + 6
                         else
                           TheImage.Width := 10;
                         TheLabel.Layout := tlCenter;
                       end;
      alClient       : begin
                         TheLabel.Alignment := taCenter;
                         TheLabel.Layout := tlCenter;
                       end;
    end;//case
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THotBtn.MseDwn(Sender : TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
const OPNAME = 'THotBtn.MseDwn';
begin
  try
    MouseDown(Button,Shift,X,Y);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THotBtn.MseUp(Sender : TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
const OPNAME = 'THotBtn.MseUp';
begin
  try
    MouseUp(Button,Shift,X,Y);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THotBtn.MseMve(Sender : TObject;Shift: TShiftState; X, Y: Integer);
const OPNAME = 'THotBtn.MseMve';
begin
  try
    MouseMove(Shift,X,Y);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THotBtn.Clck(Sender : TObject);
const OPNAME = 'THotBtn.Clck';
begin
  try
    Click
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THotBtn.SetHotGlyph(Value: TBitmap);
const OPNAME = 'THotBtn.SetHotGlyph';
begin
  try
    Invalidate;
    FHotGlyph.Assign(Value);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THotBtn.SetPlainGlyph(Value: TBitmap);
const OPNAME = 'THotBtn.SetPlainGlyph';
begin
  try
    FPlainGlyph.Assign(Value);
    ImageAlign := ImageAlign;
    Invalidate;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THotBtn.SetDisabledGlyph(Value: TBitmap);
const OPNAME = 'THotBtn.SetDisabledGlyph';
begin
  try
    FDisabledGlyph.Assign(Value);
    Invalidate;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THotBtn.SetText(Value : string);
const OPNAME = 'THotBtn.SetText';
begin
  try
    if FText <> value then
    begin
      FLabel.Caption := Value;
      FText := Value;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THotBtn.Click;
const OPNAME = 'THotBtn.Click';
begin
  try
    IF FEnabled then
    begin
      inherited Click;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THotBtn.MouseDown(Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
const OPNAME = 'THotBtn.MouseDown';
begin
  try
    IF FEnabled then
    begin
      BevelOuter := bvLowered;
      inherited MouseDown(Button,Shift,x,y);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THotBtn.MouseUp(Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
const OPNAME = 'THotBtn.MouseUp';
begin
  try
    if FEnabled then
    begin
      BevelOuter := bvRaised;
      inherited MouseUp(Button,Shift,x,y);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THotBtn.MouseMove(Shift: TShiftState; X, Y: Integer);
const OPNAME = 'THotBtn.MouseMove';
begin
  try
    if FEnabled then
    begin
      BevelOuter := bvRaised;
      inherited MouseMove(Shift,x,y);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THotBtn.TWMPaint(var Message : TWMPaint);
const OPNAME = 'THotBtn.TWMPaint';
begin
  try
    Caption := '';
    if Enabled then
      if UpState then
      begin
        FHotGlyph.TransparentMode := tmAuto;
        TheImage.Picture.Bitmap := FHotGlyph;
      end
      Else
      begin
        FPlainGlyph.TransparentMode := tmAuto;
        TheImage.Picture.Bitmap := FPlainGlyph;
      end
    else
    begin
      FDisabledGlyph.TransparentMode := tmAuto;
      BevelOuter := bvNone;
      TheImage.Picture.Bitmap := FDisabledGlyph;
    end;
    inherited;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THotBtn.TCMMouseEnter(var Message : TMessage);
const OPNAME = 'THotBtn.TCMMouseEnter';
begin
  try
    if FEnabled then
    begin
      UpState := true;
      BevelOuter := bvRaised;
      inherited;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THotBtn.TCMMouseLeave(var Message : TMessage);
const OPNAME = 'THotBtn.TCMMouseLeave';
begin
  try
    if FEnabled then
    begin
      UpState := false;
      BevelOuter := bvNone;
      inherited;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

  {TExpandPanel}

procedure TExpandPanel.OnTopPanelClick(Sender : TObject);
const OPNAME = 'TExpandPanel.OnTopPanelClick';
begin
  try
    Self.Expanded := NOT(Self.Expanded);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TExpandPanel.SetExpanded(Value : Boolean);
const OPNAME = 'TExpandPanel.SetExpanded';
begin
  try
    if Value <> FExpanded then
    begin
      FExpanded := Value;
      if FExpanded = true then
      begin
        Self.Height := Self.FFullHeight;
      end
      else
      begin
        Self.Height := Self.FTopHeight;
      end;
      if Assigned(OnExpand) then
        OnExpand(Self);
      Self.Refresh;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TExpandPanel.SetFullHeight(Value : Integer);
const OPNAME = 'TExpandPanel.SetFullHeight';
begin
  try
    if Value <> FFullHeight then
    begin
      FFullHeight := Value;
      if Self.Expanded then
      begin
        Self.Height := FFullHeight;
        Self.Refresh;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TExpandPanel.SetTopCaption(Value : TCaption);
const OPNAME = 'TExpandPanel.SetTopCaption';
begin
  try
    if Value <> FTopCaption then
    begin
      FTopCaption := Value;
      FTopPanel.Caption := FTopCaption;
      FTopPanel.Refresh;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TExpandPanel.SetTopHeight(Value : Integer);
const OPNAME = 'TExpandPanel.SetTopHeight';
begin
  try
    if Value <> FTopHeight then
    begin
      FTopHeight := Value;
      FTopPanel.Height := FTopHeight;
      FTopPanel.Refresh;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TExpandPanel.SetTopColor(Value : TColor);
const OPNAME = 'TExpandPanel.SetTopColor';
begin
  try
    if Value <> FTopColor then
    begin
      FTopColor := Value;
      FTopPanel.Color := FTopColor;
      FTopPanel.Refresh;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TExpandPanel.CreateMemberObjects;
const OPNAME = 'TExpandPanel.CreateMemberObjects';
//Constructor TExpandPanel.Create(AOwner : TComponent);
begin
  try
    inherited CreateMemberObjects;

    Self.BorderStyle := bsSingle;
    FTopPanel := TPanel.Create(Self);
    FTopPanel.BevelInner := bvNone;
    FTopPanel.BevelOuter := bvNone;
    FTopPanel.Align := alTop;
    FTopPanel.BorderStyle := bsSingle;
    FTopPanel.Alignment := taLeftJustify;
    FTopPanel.OnClick := OnTopPanelClick;
    FTopPanel.Parent := Self;

    FullHeight := 100;
    TopColor := clTeal;
    TopHeight := 20;
    Expanded := True;
    Self.Height := TopHeight;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


{ TAbstractScrollablePanel }

procedure TAbstractScrollablePanel.CreateMemberObjects;
const OPNAME = 'TAbstractScrollablePanel.CreateMemberObjects';
var
  LStatusPanel: TStatusPanel;
begin
  inherited CreateMemberObjects;
  try
    FViewMode := vmNone;
    FPopulated := False;
    Self.BevelInner := bvNone;
    Self.BevelOuter := bvNone;

    FHintDisplay := TStatusBar.Create(Self);
    FHintDisplay.Parent := Self;
    FHintDisplay.Align := alBottom;
    LStatusPanel := FHintDisplay.Panels.Add;
    LStatusPanel.Width := 1;
    FHintDisplay.Panels.Add;

    FScrollBox := TAbstractScrollBox.Create(Self,FAppModules);
    FScrollBox.Parent := Self;
    FScrollBox.Align := alClient;
    FScrollBox.BevelInner := bvNone;
    FScrollBox.BevelOuter := bvNone;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractScrollablePanel.DestroyMemberObjects;
const OPNAME = 'TAbstractScrollablePanel.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractScrollablePanel.DoExport(AFileName: string);
const OPNAME = 'TAbstractScrollablePanel.DoExport';
Var
  LBMP: TBitmap;
begin
  try
    if(AFileName = '') then
      if not PromptForFileName(AFileName,'Bitmap files |*.bmp','bmp','Select output file','',True) then Exit;

    LBMP:= TBitmap.Create;
    Try
      LBMP.Width := Self.Width;
      LBMP.Height:= Self.Height;
      Self.PaintTo(LBMP.Canvas,0,0);
      LBMP.SaveToFile(AFileName);
    Finally
      LBMP.free;
    End;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractScrollablePanel.DoCopyToClipboard;
const OPNAME = 'TAbstractScrollablePanel.DoCopyToClipboard';
Var
  LBMP: TBitmap;
begin
  try
    LBMP:= TBitmap.Create;
    Try
      LBMP.Width := Self.Width;
      LBMP.Height:= Self.Height;
      Self.PaintTo(LBMP.Canvas,0,0);
      clipboard.Assign( LBMP );
    Finally
      LBMP.free;
    End;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractScrollablePanel.DoPrint;
procedure DrawImage(Canvas: TCanvas; DestRect: TRect; ABitmap: TBitmap);
var
  Header, Bits: Pointer;
  HeaderSize: DWORD;
  BitsSize: DWORD;
begin
  GetDIBSizes(ABitmap.Handle, HeaderSize, BitsSize);
  Header := AllocMem(HeaderSize);
  Bits := AllocMem(BitsSize);
  try
    GetDIB(ABitmap.Handle, ABitmap.Palette, Header^, Bits^);
    StretchDIBits(Canvas.Handle, DestRect.Left, DestRect.Top,
      DestRect.Right, DestRect.Bottom,
      0, 0, ABitmap.Width, ABitmap.Height, Bits, TBitmapInfo(Header^),
      DIB_RGB_COLORS, SRCCOPY);
  finally
    FreeMem(Header, HeaderSize);
    FreeMem(Bits, BitsSize);
  end;
end;

const OPNAME = 'TAbstractScrollablePanel.DoPrint';
{Var
  LBitmap       : TBitmap;
  relHeight, relWidth: integer;
  ZoomPercent   : integer;}
begin
  try
    {Screen.Cursor := crHourglass;
    LBitmap:= TBitmap.Create;
    Try
      LBitmap.Width := Self.Width;
      LBitmap.Height:= Self.Height;
      LBitmap.PixelFormat := pf24bit;  // avoid palettes
      Self.PaintTo(LBitmap.Canvas,0,0);

      Vcl.Printers.Printer.BeginDoc;
      try
        Vcl.Printers.Printer.Title       := Self.Caption;
        ZoomPercent         := 90;
        if ((LBitmap.Width / LBitmap.Height) > (Vcl.Printers.Printer.PageWidth / Vcl.Printers.Printer.PageHeight)) then
        begin
          // Stretch Bitmap to width of PrinterPage
          relWidth := Vcl.Printers.Printer.PageWidth;
          relHeight := MulDiv(LBitmap.Height, Vcl.Printers.Printer.PageWidth, LBitmap.Width);
        end
        else
        begin
          // Stretch Bitmap to height of PrinterPage
          relWidth  := MulDiv(LBitmap.Width, Vcl.Printers.Printer.PageHeight, LBitmap.Height);
          relHeight := Vcl.Printers.Printer.PageHeight;
        end;
        relWidth := Round(relWidth * ZoomPercent / 100);
        relHeight := Round(relHeight * ZoomPercent / 100);
        DrawImage(Vcl.Printers.Printer.Canvas, Rect(0, 0, relWidth, relHeight), LBitmap);
      Finally
        Vcl.Printers.Printer.EndDoc;
      End;
    Finally
      Screen.cursor := crDefault;
      LBitmap.free;
    End;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractScrollablePanel.AssignHelpContext;
const OPNAME = 'TAbstractScrollablePanel.AssignHelpContext';
begin
  inherited AssignHelpContext;
  try
    //FScrollBox.AssignHelpContext;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractScrollablePanel.Initialise: boolean;
const OPNAME = 'TAbstractScrollablePanel.Initialise';
begin
  Result := False;
  try
    Result := FScrollBox.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractScrollablePanel.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractScrollablePanel.LanguageHasChanged';
begin
  Result := False;
  try
    Result := FScrollBox.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractScrollablePanel.StudyHasChanged: boolean;
const OPNAME = 'TAbstractScrollablePanel.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Result := FScrollBox.StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractScrollablePanel.SaveState: boolean;
const OPNAME = 'TAbstractScrollablePanel.SaveState';
begin
  Result := False;
  try
    Result := FScrollBox.SaveState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractScrollablePanel.ControlsOwner: TComponent;
const OPNAME = 'TAbstractScrollablePanel.ControlsOwner';
begin
  Result := nil;
  try
    Result := TComponent(FScrollBox);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractScrollablePanel.ControlsParent: TWinControl;
const OPNAME = 'TAbstractScrollablePanel.ControlsParent';
begin
  Result := nil;
  try
    Result := TWinControl(FScrollBox);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractScrollablePanel.SetPopulated(APopulated: boolean);
const OPNAME = 'TAbstractScrollablePanel.SetPopulated';
begin
  try
     FPopulated := APopulated;
     Resize;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractScrollablePanel.ShowCurrentHint;
const OPNAME = 'TAbstractScrollablePanel.ShowCurrentHint';
begin
  try
    FHintDisplay.Panels.Items[1].Text := Hint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractScrollablePanel.ShowError(AMessage: string);
const OPNAME = 'TAbstractScrollablePanel.ShowError';
begin
  try
    FHintDisplay.Font.Color := clRed;
    FHintDisplay.Panels.Items[1].Text := AMessage;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractScrollablePanel.ShowWarning(AMessage: string);
const OPNAME = 'TAbstractScrollablePanel.ShowWarning';
begin
  try
    FHintDisplay.Font.Color := clBlue;
    FHintDisplay.Panels.Items[1].Text := AMessage;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractScrollablePanel.RestoreColourState;
const OPNAME = 'TAbstractScrollablePanel.RestoreColourState';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractScrollablePanel.SetViewMode(AViewMode: TViewMode);
const OPNAME = 'TAbstractScrollablePanel.SetViewMode';
begin
  try
    FViewMode := AViewMode;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractScrollablePanel.SetNetworkElementType(ANetworkElementType: TNetworkElementType);
const OPNAME = 'TAbstractScrollablePanel.SetNetworkElementType';
begin
  try
    FNetworkElementType := ANetworkElementType;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAbstractDataDialogValidator }

constructor TAbstractDataDialogValidator.Create(APanelOwner: TComponent; AAppModules: TAppModules);
const OPNAME = 'TAbstractDataDialogValidator.CreateMemberObjects';
begin
  try
    FPanelOwner := APanelOwner;
    inherited Create(AAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractDataDialogValidator.CreateMemberObjects;
const OPNAME = 'TAbstractDataDialogValidator.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FNetworkElementType := votNone;
    FViewMode           := vmNone;
    FPanel              := nil;
    FActiveControl      := nil;
    TabShetCaption      := '';
    FIdentifier         := -1;
    FTreeNodeIndex      := -1;
    FElementName        := '';
//    FErrorMessages    := TStringList.Create;
    FAllErrorMessages   := TStringList.Create;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractDataDialogValidator.DestroyMemberObjects;
const OPNAME = 'TAbstractDataDialogValidator.DestroyMemberObjects';
begin
  try
//    FErrorMessages.Free;
    FAllErrorMessages.Free;
    FActiveControl := nil;
    if(FPanel.Owner = nil) then
    begin
      FPanel.Parent := nil;
      FPanel.Free;
    end;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDataDialogValidator.Initialise: boolean;
const OPNAME = 'TAbstractDataDialogValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    ViewMode := vmNone;
    Result := FPanel.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDataDialogValidator.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractDataDialogValidator.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    Result := FPanel.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractDataDialogValidator.OnStringGridColEnter(Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TAbstractDataDialogValidator.OnStringGridColEnter';
var
  LFieldProperty: TAbstractFieldProperty;
  LGridHintText: string;
begin
  try
    LGridHintText := '';
    if Assigned(Sender) then
    begin
      if Sender.ClassNameIs('TFieldStringGrid') or Sender.ClassNameIs('TColumnSelectGrid') then
      begin
        if (TFieldStringGrid(Sender).InValidationError[ACol, ARow, gveCellField]) then
          LGridHintText := TFieldStringGrid(Sender).ValidationError[ACol, ARow, gveCellField]
        else if (TFieldStringGrid(Sender).InValidationError[ACol, ARow, gveCellContext]) then
          LGridHintText := TFieldStringGrid(Sender).ValidationError[ACol, ARow, gveCellContext]
        else if (TFieldStringGrid(Sender).InValidationError[ACol, ARow, gveColContext]) then
          LGridHintText := TFieldStringGrid(Sender).ValidationError[ACol, ARow, gveColContext]
        else
        begin
          LFieldProperty := TFieldStringGrid(Sender).FieldProperty(ACol);
          if Assigned(LFieldProperty) then
          begin
            LGridHintText := LFieldProperty.FieldDescription;
            LGridHintText :=  FAppModules.Language.GetString(LGridHintText);
          end;
        end;
      end;

      if Sender.ClassNameIs('TFieldButtonStringGrid') or Sender.ClassNameIs('TColumnSelectGrid') then
      begin
        if (TFieldButtonStringGrid(Sender).InValidationError[ACol, ARow, gveCellField]) then
          LGridHintText := TFieldButtonStringGrid(Sender).ValidationError[ACol, ARow, gveCellField]
        else if (TFieldButtonStringGrid(Sender).InValidationError[ACol, ARow, gveCellContext]) then
          LGridHintText := TFieldButtonStringGrid(Sender).ValidationError[ACol, ARow, gveCellContext]
        else if (TFieldButtonStringGrid(Sender).InValidationError[ACol, ARow, gveColContext]) then
          LGridHintText := TFieldButtonStringGrid(Sender).ValidationError[ACol, ARow, gveColContext]
        else
        begin
          LFieldProperty := TFieldButtonStringGrid(Sender).FieldProperty(ACol);
          if Assigned(LFieldProperty) then
          begin
            LGridHintText := LFieldProperty.FieldDescription;
            LGridHintText :=  FAppModules.Language.GetString(LGridHintText);
          end;
        end;
      end;

    end;
    FPanel.Hint := LGridHintText;
    FPanel.ShowCurrentHint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractDataDialogValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TAbstractDataDialogValidator.OnEditControlEnter';
var
  LFieldProperty : TAbstractFieldProperty;
  LGridHintText  : string;
begin
  try
    LGridHintText := '';
    LFieldProperty := nil;
    FActiveControl := nil;
//    FErrorMessages.Clear;

    if Assigned(Sender) then
    begin
      if Sender.ClassNameIs('TButton') then
      begin
        LGridHintText  := TButton(Sender).Hint;
        FActiveControl := TButton(Sender);
      end;

      if Sender.ClassNameIs('TBitBtn') then
      begin
        LGridHintText := TBitBtn(Sender).Hint;
        FActiveControl := TBitBtn(Sender);
      end;

      if Sender.ClassNameIs('TFieldBitBtn') then
      begin
        LGridHintText := TBitBtn(Sender).Hint;
        FActiveControl := TBitBtn(Sender);
      end;

      if Sender.ClassNameIs('TFieldRadioGroup') then
      begin
        if(TFieldRadioGroup(Sender).ValidationError <> '') then
          LGridHintText := TFieldRadioGroup(Sender).ValidationError
        else
        begin
          if ((TFieldRadioGroup(Sender).ItemIndex >= 0) AND
              (TFieldRadioGroup(Sender).Hints.Count > TFieldRadioGroup(Sender).ItemIndex)) then
            LGridHintText := TFieldRadioGroup(Sender).Hints[TFieldRadioGroup(Sender).ItemIndex]
          else
          begin
            LFieldProperty := TFieldRadioGroup(Sender).FieldProperty;
            if not Assigned(LFieldProperty) then
              LGridHintText := TFieldRadioGroup(Sender).Hint;
          end;    
        end;
        FActiveControl := TFieldRadioGroup(Sender);
      end;

      if Sender.ClassNameIs('TFieldButton') then
      begin
        if(TFieldButton(Sender).ValidationError <> '') then
          LGridHintText := TFieldButton(Sender).ValidationError
        else
        begin
          LFieldProperty := TFieldButton(Sender).FieldProperty;
          if not Assigned(LFieldProperty) then
            LGridHintText := TFieldChkBox(Sender).Hint;
        end;
        FActiveControl := TFieldButton(Sender);
      end;

      if Sender.ClassNameIs('TFieldChkBox') then
      begin
        if(TFieldChkBox(Sender).ValidationError <> '') then
          LGridHintText := TFieldChkBox(Sender).ValidationError
        else
        begin
          LFieldProperty := TFieldChkBox(Sender).FieldProperty;
          if not Assigned(LFieldProperty) then
            LGridHintText := TFieldChkBox(Sender).Hint;
        end;
        FActiveControl := TFieldChkBox(Sender);
      end;

      if Sender.ClassNameIs('TFieldComboBox') then
      begin
        if(TFieldComboBox(Sender).ValidationError <> '') then
          LGridHintText := TFieldComboBox(Sender).ValidationError
        else
        begin
          LFieldProperty := TFieldComboBox(Sender).FieldProperty;
          if not Assigned(LFieldProperty) then
            LGridHintText := TFieldComboBox(Sender).Hint;
        end;
        FActiveControl := TFieldComboBox(Sender);
      end;

      if Sender.ClassNameIs('TFieldEdit') then
      begin
        if(TFieldEdit(Sender).FieldValidationError <> '') then
          LGridHintText := TFieldEdit(Sender).FieldValidationError
        else
        if(TFieldEdit(Sender).ContextValidationError <> '') then
          LGridHintText := TFieldEdit(Sender).ContextValidationError
        else
        begin
          LFieldProperty := TFieldEdit(Sender).FieldProperty;
          if not Assigned(LFieldProperty) then
            LGridHintText := TFieldEdit(Sender).Hint;
        end;
        FActiveControl := TFieldEdit(Sender);
      end;

      if Sender.ClassNameIs('TFieldRichEdit') then
      begin
        if (TFieldRichEdit(Sender).FieldValidationError <> '') then
          LGridHintText := TFieldRichEdit(Sender).FieldValidationError
        else
        if (TFieldRichEdit(Sender).ContextValidationError <> '') then
          LGridHintText := TFieldRichEdit(Sender).ContextValidationError
        else
        begin
          LFieldProperty := TFieldRichEdit(Sender).FieldProperty;
          if not Assigned(LFieldProperty) then
            LGridHintText := TFieldRichEdit(Sender).Hint;
        end;
        FActiveControl := TFieldRichEdit(Sender);
      end;

      if Sender.ClassNameIs('TFieldCheckListBox') then
      begin
        if(TFieldCheckListBox(Sender).ValidationError <> '') then
          LGridHintText := TFieldCheckListBox(Sender).ValidationError
        else
        begin
          LFieldProperty := TFieldCheckListBox(Sender).FieldProperty;
          if not Assigned(LFieldProperty) then
            LGridHintText := TFieldCheckListBox(Sender).Hint;
        end;
        FActiveControl := TFieldCheckListBox(Sender);
      end;

      if Sender.ClassNameIs('TFieldStringGrid') then
      begin
        LFieldProperty := TFieldStringGrid(Sender).FieldProperty(TFieldStringGrid(Sender).Col);
        if not Assigned(LFieldProperty) then
          LGridHintText := TFieldStringGrid(Sender).Hint;
        FActiveControl := TFieldStringGrid(Sender);
      end;

      if Sender.ClassNameIs('TFieldButtonStringGrid') then
      begin
        LFieldProperty := TFieldButtonStringGrid(Sender).FieldProperty(TFieldButtonStringGrid(Sender).Col);
        if not Assigned(LFieldProperty) then
          LGridHintText := TFieldButtonStringGrid(Sender).Hint;
        FActiveControl := TFieldButtonStringGrid(Sender);
      end;


      if Sender.ClassNameIs('TFieldDateTimePicker') then
      begin
        if(TFieldDateTimePicker(Sender).ValidationError <> '') then
          LGridHintText := TFieldDateTimePicker(Sender).ValidationError
        else
        begin
          LFieldProperty := TFieldDateTimePicker(Sender).FieldProperty;
          if not Assigned(LFieldProperty) then
            LGridHintText := TFieldDateTimePicker(Sender).Hint;
        end;
        FActiveControl := TFieldDateTimePicker(Sender);
      end;

      if Sender.InheritsFrom(TFieldTreeView) then
      begin
        if(TFieldTreeView(Sender).ValidationError <> '') then
          LGridHintText := TFieldTreeView(Sender).ValidationError
        else
        begin
          LFieldProperty := TFieldTreeView(Sender).FieldProperty;
          if not Assigned(LFieldProperty) then
            LGridHintText := TFieldTreeView(Sender).Hint;
        end;
        FActiveControl := TFieldTreeView(Sender);
      end;

      if Assigned(LFieldProperty) then
      begin
        LGridHintText := LFieldProperty.FieldDescription;
        LGridHintText := FAppModules.Language.GetString(LGridHintText);
      end;

      FPanel.Hint := LGridHintText;
      FPanel.ShowCurrentHint;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractDataDialogValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TAbstractDataDialogValidator.OnEditControltExit';
var
  LFieldProperty: TAbstractFieldProperty;
begin
  try
    FActiveControl := nil;
    FPanel.Hint := '';
    FPanel.ShowCurrentHint;
    if Sender.ClassNameIs('TFieldEdit') then
    begin
      LFieldProperty := TFieldEdit(Sender).FieldProperty;
      if(Trim(TFieldEdit(Sender).Text) = '') and Assigned(LFieldProperty) then
      begin
        if (LFieldProperty.FieldDataType in [FieldFloatType,FieldIntegerType,FieldDTimeType,FieldCharType,FieldBoolType]) then
          TFieldEdit(Sender).Text := TFieldEdit(Sender).PreviousValue;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractDataDialogValidator.ShowCurrentComponentHint;
const OPNAME = 'TAbstractDataDialogValidator.ShowCurrentComponentHint';
begin
  try
    if Assigned(FActiveControl) and Assigned(Screen.ActiveForm) then
      Screen.ActiveForm.SetFocusedControl(FActiveControl);

       //OnEditControlEnter(FActiveControl);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractDataDialogValidator.ClearDataViewer;
const OPNAME = 'TAbstractDataDialogValidator.ClearDataViewer';
begin
  try
    FPanel.RestoreColourState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractDataDialogValidator.PopulateDataViewer;
const OPNAME = 'TAbstractDataDialogValidator.PopulateDataViewer';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractDataDialogValidator.ShowWizardStep (ASequence : integer = 0); 
const OPNAME = 'TAbstractDataDialogValidator.ShowWizardStep';
begin
  try
    FPanel.Show;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDataDialogValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TAbstractDataDialogValidator.DetermineWizardStatus';
begin
  Result := 0;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDataDialogValidator.SaveState: boolean;
const OPNAME = 'TAbstractDataDialogValidator.SaveState';
begin
  Result := inherited SaveState;
  try
    Result := FPanel.SaveState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDataDialogValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TAbstractDataDialogValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (Assigned(FOnValueChange)) then
      FOnValueChange(Self);
    if (Assigned(FOnDataChange)) then
      FOnDataChange(Self);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDataDialogValidator.StudyHasChanged: boolean;
const OPNAME = 'TAbstractDataDialogValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Result := FPanel.StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDataDialogValidator.ProcessParameterChangeEvent : boolean;
const OPNAME = 'TAbstractDataDialogValidator.ProcessParameterChangeEvent';
begin
  Result := FALSE;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDataDialogValidator.ProcessMetaDataEvent : boolean;
const OPNAME = 'TAbstractDataDialogValidator.ProcessMetaDataEvent';
begin
  Result := FALSE;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractDataDialogValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TAbstractDataDialogValidator.OnStringGridCellDataHasChanged';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractDataDialogValidator.SetViewMode(AViewMode: TViewMode);
const OPNAME = 'TAbstractDataDialogValidator.SetViewMode';
begin
  try
    FViewMode := AViewMode;

    if Assigned(FPanel) then
      FPanel.ViewMode := AViewMode;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDataDialogValidator.CanCopyToCLipboard: boolean;
const OPNAME = 'TAbstractDataDialogValidator.CanCopyToCLipboard';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDataDialogValidator.CanExport: boolean;
const OPNAME = 'TAbstractDataDialogValidator.CanExport';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDataDialogValidator.CanPrint: boolean;
const OPNAME = 'TAbstractDataDialogValidator.CanPrint';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractDataDialogValidator.DoCopyToCLipboard;
const OPNAME = 'TAbstractDataDialogValidator.DoCopyToCLipboard';
begin
  try
    FPanel.DoCopyToCLipboard;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractDataDialogValidator.DoExport(AFileName: string = '');
const OPNAME = 'TAbstractDataDialogValidator.DoExport';
begin
  try
    FPanel.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractDataDialogValidator.DoPrint;
const OPNAME = 'TAbstractDataDialogValidator.DoPrint';
begin
  try
    FPanel.DoPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractDataDialogValidator.ExitCurrentEditControl;
const OPNAME = 'TAbstractDataDialogValidator.ExitCurrentEditControl';
begin
  try
   if(FActiveControl <> nil) then
      FActiveControl.Perform(CM_EXIT,0,0)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractDataDialogValidator.SetNetworkElementType(ANetworkElementType: TNetworkElementType);
const OPNAME = 'TAbstractDataDialogValidator.SetNetworkElementType';
begin
  try
    FNetworkElementType := ANetworkElementType;

    if Assigned(FPanel) then
      FPanel.NetworkElementType := ANetworkElementType;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAbstractDataPageControl }

procedure TAbstractDataPageControl.CreateMemberObjects;
const OPNAME = 'TAbstractDataPageControl.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FLastTabSelected := '';
    FValidatorList   := TObjectList.Create(True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractDataPageControl.DestroyMemberObjects;
const OPNAME = 'TAbstractDataPageControl.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    //DeleteAllTabSheets;
    FreeAndNil(FValidatorList);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDataPageControl.Initialise: boolean;
const OPNAME = 'TAbstractDataPageControl.Initialise';
var
  LCount: integer;
begin
  Result := inherited Initialise;
  try
    for LCount := 0 to FValidatorList.Count - 1 do
    begin
      Result := Result and TAbstractDataDialogValidator(FValidatorList.Items[LCount]).Initialise;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDataPageControl.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractDataPageControl.LanguageHasChanged';
var
  LCount: integer;
begin
  Result := inherited LanguageHasChanged;
  try
    for LCount := 0 to FValidatorList.Count - 1 do
    begin
      Result := Result and TAbstractDataDialogValidator(FValidatorList.Items[LCount]).LanguageHasChanged;
      Self.Pages[LCount].Caption := TAbstractDataDialogValidator(FValidatorList.Items[LCount]).TabShetCaption;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDataPageControl.SaveState: boolean;
const OPNAME = 'TAbstractDataPageControl.SaveState';
var
  LCount: integer;
begin
  Result := inherited SaveState;
  try
    for LCount := 0 to FValidatorList.Count - 1 do
    begin
      Result := Result and TAbstractDataDialogValidator(FValidatorList.Items[LCount]).SaveState;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDataPageControl.StudyHasChanged: boolean;
const OPNAME = 'TAbstractDataPageControl.StudyHasChanged';
var
  LCount: integer;
begin
  Result := inherited StudyHasChanged;
  try
    for LCount := 0 to FValidatorList.Count - 1 do
    begin
      Result := Result and TAbstractDataDialogValidator(FValidatorList.Items[LCount]).StudyHasChanged;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDataPageControl.AddValidator(AValidator: TAbstractDataDialogValidator): boolean;
const OPNAME = 'TAbstractDataPageControl.AddValidator';
var
  LCount: integer;
  LTabSheet:TAbstractDataTabSheet;
  LFound: boolean;
begin
  Result := False;
  try
    if Assigned(AValidator) then
    begin
      LFound := False;
      for LCount := 0 to FValidatorList.Count - 1 do
      begin
        if (FValidatorList.Items[LCount] = AValidator) then
        begin
          Self.Pages[LCount].TabVisible := True;
          LFound := True;
          Break;
        end;
      end;
      if not LFound then
      begin
        LTabSheet := TAbstractDataTabSheet.Create(Self,FAppModules);
        LTabSheet.PageControl := Self;
        AValidator.Panel.Parent := LTabSheet;
        LTabSheet.Caption := AValidator.TabShetCaption;
        AValidator.Panel.Align := alClient;
        FValidatorList.Add(AValidator);
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDataPageControl.DeleteValidatorByClassName(AClassName: string): Boolean;
const OPNAME = 'TAbstractDataPageControl.DeleteValidatorByClassName';
var
  LDialogValidator : TAbstractDataDialogValidator;
  LTabSheet : TAbstractDataTabSheet;
  LCount : integer;
begin
  Result := False;
  try
    if (trim(AClassName) = '') then Exit;
    for LCount := 0 to FValidatorList.Count - 1 do
    begin
      if (UpperCase(FValidatorList.Items[LCount].ClassName) = UpperCase(Trim(AClassName))) then
      begin
        LDialogValidator := TAbstractDataDialogValidator(FValidatorList.Items[LCount]);
        if LDialogValidator <> nil then
        begin
          TAbstractDataDialogValidator(FValidatorList.Items[LCount]).Panel.Parent := nil;
          LTabSheet := TAbstractDataTabSheet(Self.Pages[LCount]);
          LTabSheet.TabVisible := False;
          LTabSheet.PageControl := nil;
          LTabSheet.Free;
          FValidatorList.Remove(LDialogValidator);
          Result := True;
          Break;
        end;
      end;  
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDataPageControl.GetValidatorByClassName(AClassName: string): TAbstractDataDialogValidator;
const OPNAME = 'TAbstractDataPageControl.GetValidatorByClassName';
var
  LCount: integer;
begin
  Result := nil;
  try
    if (trim(AClassName) = '') then Exit;
    for LCount := 0 to FValidatorList.Count - 1 do
    begin
      if(UpperCase(FValidatorList.Items[LCount].ClassName) = UpperCase(Trim(AClassName))) then
        Result := TAbstractDataDialogValidator(FValidatorList.Items[LCount]);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractDataPageControl.ChangeActivePage(Page: TAbstractDataTabSheet);
const OPNAME = 'TAbstractDataPageControl.ChangeActivePage';
var
  ParentForm: TCustomForm;

begin
  try
    if Self.ActivePage <> Page then
    begin
      ParentForm := GetParentForm(Self);
      if (ParentForm <> nil) and (Self.ActivePage <> nil) and
        Self.ActivePage.ContainsControl(ParentForm.ActiveControl) then
      begin
        ParentForm.ActiveControl := Self.ActivePage;
        if ParentForm.ActiveControl <> Self.ActivePage then
        begin
          TabIndex := Self.ActivePage.TabIndex;
          Exit;
        end;
      end;
      if Page <> nil then
      begin
        Page.BringToFront;
        Page.Visible := True;
        if (ParentForm <> nil) and (Self.ActivePage <> nil) and
          (ParentForm.ActiveControl = Self.ActivePage) then
          if Page.CanFocus then
            ParentForm.ActiveControl := Page else
            ParentForm.ActiveControl := Self;
        if not Page.TabVisible and TStyleManager.IsCustomStyleActive then
          RedrawWindow(Page.Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
      end;
      if Self.ActivePage <> nil then Self.ActivePage.Visible := False;
      Self.ActivePage := Page;
      if (ParentForm <> nil) and (Self.ActivePage <> nil) and
        (ParentForm.ActiveControl = Self.ActivePage) then
       // Self.ActivePage.SelectFirst;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractDataPageControl.Change;
const OPNAME = 'TAbstractDataPageControl.Change';
var
  LIndex: integer;
  LDialogValidator:TAbstractDataDialogValidator;
begin
  inherited Change;
  try

    FLastTabSelected := Self.ActivePage.Caption;
    LIndex := Self.ActivePageIndex;
    if(LIndex >= 0) and (LIndex < FValidatorList.Count) then
    begin
      LDialogValidator := TAbstractDataDialogValidator(FValidatorList.Items[LIndex]);
      if Assigned(LDialogValidator) then
      begin
        LDialogValidator.ShowCurrentComponentHint;
      end;
      if Assigned(FAppModules.MainForm()) then
        FAppModules.ProcessEvent(CmeRefreshMenuItems,nil);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;

end;

function TAbstractDataPageControl.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TAbstractDataPageControl.StudyDataHasChanged';
var
  LCount: integer;
begin
  Result := True;
  try
    for LCount := 0 to  FValidatorList.Count - 1 do
    begin
      if LCount >= FValidatorList.Count then exit;
      Result := Result and TAbstractDataDialogValidator(FValidatorList.Items[LCount]).StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDataPageControl.ProcessParameterChangeEvent : boolean;
const OPNAME = 'TAbstractDataPageControl.ProcessParameterChangeEvent';
var
  lCount: integer;
begin
  Result := FALSE;
  try
    lCount := 0;
    while ((NOT Result) AND (lCount < FValidatorList.Count)) do
    begin
      Result := TAbstractDataDialogValidator(FValidatorList.Items[LCount]).ProcessParameterChangeEvent;
      lCount := lCount + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDataPageControl.ProcessmetaDataEvent : boolean;
const OPNAME = 'TAbstractDataPageControl.ProcessmetaDataEvent';
var
  lCount     : integer;
begin
  Result := FALSE;
  try
    lCount := 0;
    while ((NOT Result) AND (lCount < FValidatorList.Count)) do
    begin
      if (lCount = Self.ActivePageIndex) then
        Result := TAbstractDataDialogValidator(FValidatorList.Items[LCount]).ProcessMetaDataEvent;
      lCount := lCount + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDataPageControl.HideAllTabSheets: boolean;
const OPNAME = 'TAbstractDataPageControl.HideAllTabSheets';
var
  LCount: integer;
begin
  Result := False;
  try
    for LCount := 0 to Self.PageCount - 1 do
    begin
      Self.Pages[LCount].TabVisible := False;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDataPageControl.DeleteAllTabSheets: boolean;
const OPNAME = 'TAbstractDataPageControl.DeleteAllTabSheets';
var
  LCount: integer;
  LTabSheet:TAbstractDataTabSheet;
begin
  Result := True;
  try
    for LCount := 0 to FValidatorList.Count - 1 do
    begin
      TAbstractDataDialogValidator(FValidatorList.Items[LCount]).Panel.Parent := nil;
    end;
    While Self.PageCount > 0 do
    begin
      LTabSheet := TAbstractDataTabSheet(Self.Pages[0]);
      LTabSheet.TabVisible := False;
      LTabSheet.PageControl := nil;
      LTabSheet.Free;
    end;
    FValidatorList.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDataPageControl.CanCopyToCLipboard: boolean;
const OPNAME = 'TAbstractDataPageControl.CanCopyToCLipboard';
begin
  Result := False;
  try
    if(Self.ActivePageIndex >= 0) then
      Result := TAbstractDataDialogValidator(FValidatorList.Items[Self.ActivePageIndex]).CanCopyToCLipboard;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDataPageControl.CanExport: boolean;
const OPNAME = 'TAbstractDataPageControl.CanExport';
begin
  Result := False;
  try
    if(Self.ActivePageIndex >= 0) then
      Result := TAbstractDataDialogValidator(FValidatorList.Items[Self.ActivePageIndex]).CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDataPageControl.CanPrint: boolean;
const OPNAME = 'TAbstractDataPageControl.CanPrint';
begin
  Result := False;
  try
    if(Self.ActivePageIndex >= 0) then
      Result := TAbstractDataDialogValidator(FValidatorList.Items[Self.ActivePageIndex]).CanPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractDataPageControl.DoCopyToCLipboard;
const OPNAME = 'TAbstractDataPageControl.DoCopyToCLipboard';
begin
  try
    if(Self.ActivePageIndex >= 0) then
      TAbstractDataDialogValidator(FValidatorList.Items[Self.ActivePageIndex]).DoCopyToCLipboard;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractDataPageControl.DoExport(AFileName: string = '');
const OPNAME = 'TAbstractDataPageControl.DoExport';
begin
  try
    if(Self.ActivePageIndex >= 0) then
      TAbstractDataDialogValidator(FValidatorList.Items[Self.ActivePageIndex]).DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractDataPageControl.DoPrint;
const OPNAME = 'TAbstractDataPageControl.DoPrint';
begin
  try
    if(Self.ActivePageIndex >= 0) then
      TAbstractDataDialogValidator(FValidatorList.Items[Self.ActivePageIndex]).DoPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractDataPageControl.ExitCurrentEditControl;
const OPNAME = 'TAbstractDataPageControl.ExitCurrentEditControl';
begin
  try
    if(Self.ActivePageIndex >= 0) then
      TAbstractDataDialogValidator(FValidatorList.Items[Self.ActivePageIndex]).ExitCurrentEditControl;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDataPageControl.GetValidatorByIndex(AIndex: integer): TAbstractDataDialogValidator;
const OPNAME = 'TAbstractDataPageControl.GetValidatorByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FValidatorList.Count) then
      Result := TAbstractDataDialogValidator(FValidatorList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDataPageControl.ValidatorCount: integer;
const OPNAME = 'TAbstractDataPageControl.ValidatorCount';
begin
  Result := 0;
  try
    Result := FValidatorList.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractDataPageControl.SelectLastActiveTabsheet;
const OPNAME = 'TAbstractDataPageControl.SelectLastActiveTabsheet';
var
  LIndex: integer;
begin
  try
    if(FLastTabSelected = '') then Exit;
    for LIndex := 0 to Self.PageCount-1 do
    begin
      if(FLastTabSelected = Self.Pages[LIndex].Caption) then
      begin
        if(LIndex <> Self.ActivePageIndex) then
          Self.ActivePageIndex := LIndex;
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAbstractDataTabSheet }

function TAbstractDataTabSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TAbstractDataTabSheet.GetToolBar';
begin
  Result := nil;
  try
    Result := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAbstractOutputDialogValidator }

function TAbstractOutputDialogValidator.CalculateMCMPerMonthGridValues(AGrid: TAbstractStringGrid): boolean;
const OPNAME = 'TAbstractOutputDialogValidator.CalculateMCMPerMonthGridValues';
var
  LMonthlyTotals  : array[1..12] of double;
  LMonthlyValue   : double;
  LGrandTotal     : double;
  LAnnualTotal    : double;
  LCol,
  LRow            : integer;
  LTotal          : string;
begin
  Result := False;
  try
    if(AGrid = nil) then Exit;
    if(AGrid.RowCount <= 2) then Exit;
    if(AGrid.ColCount < 14) then Exit;

    for LCol := 1 to 12 do
      LMonthlyTotals[LCol] := 0.0;
    LGrandTotal := 0.0;

    for LRow := 1 to AGrid.RowCount-2 do
    begin
      LAnnualTotal := 0.0;
      for LCol := 1 to 12 do
      begin
        LMonthlyValue := StrToFloatDef(AGrid.Cells[LCol,LRow],0.0);
        LAnnualTotal  := LAnnualTotal + LMonthlyValue;
        LMonthlyTotals[LCol] := LMonthlyTotals[LCol] + LMonthlyValue;
      end;
      LGrandTotal := LGrandTotal + LAnnualTotal;
      AGrid.Cells[13,LRow] := FormatFloat('##0.000',LAnnualTotal);
    end;
    
    LTotal := FAppModules.Language.GetString('GridHeading.Total');
    AGrid.Cells[13,0] := LTotal;
    AGrid.Cells[0,AGrid.RowCount-1] := 'Average'; //LTotal;

    for LCol := 1 to 12 do
      AGrid.Cells[LCol,AGrid.RowCount-1] := FormatFloat('##0.000',LMonthlyTotals[LCol]/(AGrid.RowCount-2));
    AGrid.Cells[13,AGrid.RowCount-1] := FormatFloat('##0.000',LGrandTotal/(AGrid.RowCount-2));
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractOutputDialogValidator.GetNextSignificantRecord(ASender: TObject;ACurrentRecord: integer): integer;
const OPNAME = 'TAbstractOutputDialogValidator.GetNextSignificantRecord';
begin
  Result := 0;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractOutputDialogValidator.GetPreviousSignificantRecord(ASender: TObject;ACurrentRecord: integer): integer;
const OPNAME = 'TAbstractOutputDialogValidator.GetPreviousSignificantRecord';
begin
  Result := 0;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractOutputDialogValidator.ShowSelectedRecord(ASender: TObject;ACurrentRecord: integer);
const OPNAME = 'TAbstractOutputDialogValidator.ShowSelectedRecord';
begin
  try
    //Do nothing here
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAbstractNavigator }

procedure TAbstractNavigator.CreateMemberObjects;
const OPNAME = 'TAbstractNavigator.CreateMemberObjects';
begin
  inherited;
  try
    FOnSelectionChange          := nil;
    FRecordCount                := 0;
    FCurrentRecord              := 0;
    Width                       := 280;
    Height                      := 23;
    BevelInner                  := bvLowered;
    BevelOuter                  := bvNone;

    FValidatorList              := TObjectList.Create(False);

    FFirstRecord          := CreateButton('OutputReviewFirstRecord', alLeft, FAppModules);
    FPreviousRecord       := CreateButton('OutputReviewPrevRecord', alLeft, FAppModules);
    FNextRecord           := CreateButton('OutputReviewNextRecord', alRight, FAppModules);
    FLastRecord           := CreateButton('OutputReviewLastRecord', alRight, FAppModules);

    FPreviousRecord.Enabled     := False;
    FNextRecord.Enabled         := False;
    FFirstRecord.Enabled        := False;
    FLastRecord.Enabled         := False;

    FPreviousRecord.OnClick     := OnPreviousRecordClck;
    FNextRecord.OnClick         := OnNextRecordClck;
    FFirstRecord.OnClick        := OnFirstRecordClck;
    FLastRecord.OnClick         := OnLastRecordClck;

    FCbxSelector                := TComboBox.Create(Self);
    FCbxSelector.Parent         := Self;
    FCbxSelector.OnChange       := OnComboBoxChange;

    FCurrentRecordDisplay            := TAbstractPanel.Create(Self.Owner, FAppModules);
    FCurrentRecordDisplay.Parent     := Self;
    FCurrentRecordDisplay.TabStop    := False;
    FCurrentRecordDisplay.Color      := clWhite;
    FCurrentRecordDisplay.BevelInner := bvLowered;
    FCurrentRecordDisplay.BevelOuter := bvNone;
    FCurrentRecordDisplay.Alignment  := taCenter;
    FCurrentRecordDisplay.Caption    := FAppModules.Language.GetString('PanelCaption.PnlOf');

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractNavigator.DestroyMemberObjects;
const OPNAME = 'TAbstractNavigator.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FValidatorList);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractNavigator.Initialise: boolean;
const OPNAME = 'TAbstractNavigator.Initialise';
begin
  Result := inherited Initialise;
  try
    FValidatorList.Clear;
    FRecordCount                  := 0;
    FCurrentRecord                := 0;
    FCurrentRecordDisplay.Caption := FAppModules.Language.GetString('PanelCaption.PnlOf');
    SetButtonsState;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractNavigator.StudyHasChanged: boolean;
const OPNAME = 'TAbstractNavigator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    FValidatorList.Clear;
    FRecordCount                   := 0;
    FCurrentRecord                 := 0;
    FCurrentRecordDisplay.Caption  := FAppModules.Language.GetString('PanelCaption.PnlOf');
    SetButtonsState;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractNavigator.LanguageHasChanged: boolean;
const OPNAME = 'TAbstractNavigator.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    DisplayCurrentRecord;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractNavigator.Resize;
const OPNAME = 'TAbstractNavigator.Resize';
var
//  LWidth: integer;
  lBtnWidth : integer;
begin
  inherited;
  try
    lBtnWidth                   := 45;
    FFirstRecord.Left           := 0;
    FFirstRecord.Width          := lBtnWidth;
    FFirstRecord.Align          := alLeft;

    FPreviousRecord.Left        := lBtnWidth;
    FPreviousRecord.Width       := lBtnWidth;
    FPreviousRecord.Align       := alLeft;

    FCbxSelector.Left           := (2*lBtnWidth);
    FCbxSelector.Width          := 50;
    FCbxSelector.Align          := alLeft;

    FLastRecord.Left            := Self.Width - (1*lBtnWidth);
    FLastRecord.Width           := lBtnWidth;
    FLastRecord.Align           := alRight;

    FNextRecord.Left            := Self.Width - (2*lBtnWidth);
    FNextRecord.Width           := lBtnWidth;
    FNextRecord.Align           := alRight;

    FCurrentRecordDisplay.Left  := 50 + (2*lBtnWidth);
    FCurrentRecordDisplay.Width := 50;
    FCurrentRecordDisplay.Align := alClient;
(*
    LWidth                           := Self.ClientWidth div 10;
    FFirstRecord.Align               := alNone;
    FPreviousRecord.Align            := alNone;
    FNextRecord.Align                := alNone;
    FLastRecord.Align                := alNone;
    FCurrentRecordDisplay.Align      := alNone;

    FFirstRecord.Left               := Self.ClientWidth div 2;
    FPreviousRecord.Left            := Self.ClientWidth div 2;
    FNextRecord.Left                := Self.ClientWidth div 2;
    FLastRecord.Left                := Self.ClientWidth div 2;
    FCurrentRecordDisplay.Left      := Self.ClientWidth div 2;

    FFirstRecord.Align               := alLeft;
    FFirstRecord.Width               := LWidth;

    FPreviousRecord.Align            := alLeft;
    FPreviousRecord.Width            := LWidth;

    FLastRecord.Align                := alRight;
    FLastRecord.Width                := LWidth;

    FNextRecord.Align                := alRight;
    FNextRecord.Width                := LWidth;

    FCurrentRecordDisplay.Align      := alClient;
*)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractNavigator.CreateButton(AButtonKey: string; AAlign: TAlign; AAppModules: TAppModules): TFieldBitBtn;
const OPNAME = 'TAbstractNavigator.CreateButton';
begin
  Result := nil;
  try
    Result := TFieldBitBtn.Create(Self, AAppModules);
    Result.Parent    := Self;
    Result.TabStop   := False;
    Result.Top       := 0;
    Result.Width     := 21;
    Result.Align     := AAlign;
    Result.Name      := AButtonKey;
    Result.Caption   := '';
    Result.Glyph.LoadFromResourceName(HImagesInstance, UpperCase(AButtonKey));
    Result.NumGlyphs := Result.Glyph.Width div Result.Glyph.Height;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractNavigator.DisplayCurrentRecord;
const OPNAME = 'TAbstractNavigator.DisplayCurrentRecord';
begin
  try
    FCbxSelector.ItemIndex := FCbxSelector.Items.IndexOf(IntToStr(FCurrentRecord));
    FCurrentRecordDisplay.Caption := 'of ' + IntToStr(FRecordCount);;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractNavigator.OnFirstRecordClck(Sender: TObject);
const OPNAME = 'TAbstractNavigator.OnFirstRecordClck';
begin
  try
    FCurrentRecord := 1;
    if(FRecordCount < 1) then
       FCurrentRecord := 0;
    DisplayCurrentRecord;
    SetButtonsState;
    if Assigned(FOnSelectionChange) then
      FOnSelectionChange(Self);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractNavigator.OnLastRecordClck(Sender: TObject);
const OPNAME = 'TAbstractNavigator.OnLastRecordClck';
begin
  try
    FCurrentRecord := FRecordCount;
    DisplayCurrentRecord;
    SetButtonsState;
    if Assigned(FOnSelectionChange) then
      FOnSelectionChange(Self);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractNavigator.OnNextRecordClck(Sender: TObject);
const OPNAME = 'TAbstractNavigator.OnNextRecordClck';
begin
  try
    FCurrentRecord := FCurrentRecord + 1;
    if(FCurrentRecord > FRecordCount) then
       FCurrentRecord := FRecordCount;
    DisplayCurrentRecord;
    SetButtonsState;
    if Assigned(FOnSelectionChange) then
      FOnSelectionChange(Self);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractNavigator.OnPreviousRecordClck(Sender: TObject);
const OPNAME = 'TAbstractNavigator.OnPreviousRecordClck';
begin
  try
    FCurrentRecord := FCurrentRecord -1;
    if(FCurrentRecord < 1) then
       FCurrentRecord := 1;
    DisplayCurrentRecord;
    SetButtonsState;
    if Assigned(FOnSelectionChange) then
      FOnSelectionChange(Self);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractNavigator.SetButtonsState;
const OPNAME = 'TAbstractNavigator.SetButtonsState';
begin
  try
    FFirstRecord.Enabled               := False;
    FPreviousRecord.Enabled            := False;
    FNextRecord.Enabled                := False;
    FLastRecord.Enabled                := False;
    if(FRecordCount > 1) then
    begin
      if(FCurrentRecord = FRecordCount) and (FRecordCount > 0) then
      begin
        FFirstRecord.Enabled               := True;
        FPreviousRecord.Enabled            := True;
      end
      else
      if(FCurrentRecord = 1) then
      begin
        FNextRecord.Enabled                := True;
        FLastRecord.Enabled                := True;
      end
      else
      begin
        FFirstRecord.Enabled               := True;
        FPreviousRecord.Enabled            := True;
        FNextRecord.Enabled                := True;
        FLastRecord.Enabled                := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractNavigator.SetCurrentRecord(AValue: integer);
const OPNAME = 'TAbstractNavigator.SetCurrentRecord';
begin
  try
    FCurrentRecord := AValue;
    DisplayCurrentRecord;
    SetButtonsState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractNavigator.SetRecordCount(AValue: integer);
const OPNAME = 'TAbstractNavigator.SetRecordCount';
begin
  try
    FRecordCount := AValue;
    DisplayCurrentRecord;
    SetButtonsState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractNavigator.SetEnabled(Value: Boolean);
const OPNAME = 'TAbstractNavigator.SetEnabled';
begin
  inherited;
  try
    if Value then
      SetButtonsState
    else
    begin
      FPreviousRecord.Enabled := False;
      FNextRecord.Enabled := False;
      FFirstRecord.Enabled := False;
      FLastRecord.Enabled := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractNavigator.OnComboBoxChange(ASender: TObject);
const OPNAME = 'TAbstractNavigator.OnComboBoxChange';
begin
  try
    FCurrentRecord := StrToInt(FCbxSelector.Items[FCbxSelector.ItemIndex]);
    DisplayCurrentRecord;
    SetButtonsState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{TMonthlyDeficitGrid}

procedure TMonthlyDeficitGrid.CreateMemberObjects;
const OPNAME = 'TMonthlyDeficitGrid.CreateMemberObjects';
begin
  try
    FHeading       := '';
    FAlignment     := taRightJustify;
    FCellColoring  := TStringList.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMonthlyDeficitGrid.DestroyMemberObjects;
const OPNAME = 'TMonthlyDeficitGrid.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FHeading    := '';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMonthlyDeficitGrid.LanguageHasChanged : boolean;
const OPNAME = 'TMonthlyDeficitGrid.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMonthlyDeficitGrid.StudyHasChanged : boolean;
const OPNAME = 'TMonthlyDeficitGrid.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMonthlyDeficitGrid.IsColored(ACol, ARow: integer): boolean;
const OPNAME = 'TMonthlyDeficitGrid.IsColored';
var
  LStrings : TStringList;
begin
  Result := False;
  try
    if (FCellColoring.Count > ARow) then
    begin
      LStrings             := TStringList.Create;
      try
        LStrings.CommaText := FCellColoring[ARow];
        if (LStrings.Count > ACol) then
          Result           := (UpperCase(LStrings[ACol]) = 'Y');
      finally
        LStrings.Clear;
        LStrings.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMonthlyDeficitGrid.DrawCell(ACol, ARow: Integer; Rect: TRect;State: TGridDrawState);
const OPNAME = 'TMonthlyDeficitGrid.DrawCell';
var
  LTxtRect: TRect;
  LStr: string;
  LTxtWidth,
  LTxtHeight,
  LTxtXCoord,
  LTxtYCoord : integer;
  LColor : TColor;
begin
  inherited DrawCell(ACol, ARow, Rect, State);
  try
    if (ARow = 0) then
    begin
      Rect.Right              := Self.width;
      Rect.Left               := 0;

      Self.Canvas.Brush.Color := clBtnFace;
      Self.Brush.Style        := bsSolid;
      Self.Canvas.Pen.Style   := psClear;
      Self.Canvas.FillRect(Rect);

      LtxtRect                := Rect;
      LtxtRect.Left           := 0;

      Lstr                    := FHeading;
      LTxtWidth               := Self.Canvas.TextWidth(LStr);
      LTxtHeight              := Self.Canvas.TextHeight(LStr);

      LTxtXCoord              := (LtxtRect.Right  div 2) - (LTxtWidth  div 2);
      LTxtYCoord              := (LtxtRect.Bottom div 2) - (LTxtHeight div 2);

      Self.Canvas.Font.Color  := clInfoText;
      Self.Canvas.Font.Name   := Self.Font.Name;
      Self.Canvas.Font.Size   := Self.Font.Size;
      Self.Canvas.Font.Style  := [fsBold];
      Self.Canvas.TextRect(LtxtRect, LTxtXCoord, LTxtYCoord, LStr);
    end
    else
    if (ARow = 1) then
    begin
      Self.Canvas.Brush.Color := clBtnFace;
      Self.Brush.Style        := bsSolid;
      Self.Canvas.Pen.Style   := psSolid;
      Self.Canvas.FillRect(Rect);

      LtxtRect                := Rect;
      LTxtRect.Left           := Rect.Left + 4;
      LTxtRect.Top            := Rect.Top + 2;

      LStr                    := Self.Cells[ACol, ARow];

      Self.Canvas.Font.Color  := clInfoText;
      Self.Canvas.Font.Name   := Self.Font.Name;
      Self.Canvas.Font.Size   := Self.Font.Size;
      Self.Canvas.Font.Style  := [fsBold];
      DrawText(Self.Canvas.Handle,
               PChar(LStr),
               length(LStr), LTxtRect,
               DT_SINGLELINE or DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);
    end
    else
    begin
      if IsColored(ACol, ARow) then
        LColor := FCellColor
      else
        LColor := clBlack;

      Self.Canvas.Brush.Color := clWindow;
      Self.Brush.Style        := bsSolid;
      Self.Canvas.Pen.Style   := psSolid;
      Self.Canvas.FillRect(Rect);

      LtxtRect                := Rect;
      LTxtRect.Left           := Rect.Left + 4;
      LTxtRect.Top            := Rect.Top + 2;

      LStr                    := Self.Cells[ACol, ARow];

      Self.Canvas.Font.Color  := clInfoText;
      Self.Canvas.Font.Name   := Self.Font.Name;
      Self.Canvas.Font.Size   := Self.Font.Size;
      Self.Canvas.Font.Color  := LColor;
      Self.Canvas.Font.Style  := [fsBold];
      inherited  DrawCell(ACol, ARow,Rect,State);

      {DrawText(Self.Canvas.Handle,
               PChar(LStr),
               Length(LStr), LTxtRect,
               DT_SINGLELINE or DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);
      }
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMonthlyDeficitGrid.Initialise : boolean;
const OPNAME = 'TMonthlyDeficitGrid.Initialise';
begin
  Result := inherited Initialise;
  try
    if Result then
    begin
      Self.ColCount   := 14;
      Self.RowCount   := 2;
      Self.FixedCols  := 0;
      Self.FixedRows  := 0;
      Self.Color      := clWindow;
      Self.EditorMode := False;
      Self.Options    := [goFixedVertLine,goFixedHorzLine,goVertLine,goHorzLine];
      Self.Height     := 240;
      Self.ScrollBars := ssVertical;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMonthlyDeficitGrid.Resize;
const OPNAME = 'TMonthlyDeficitGrid.Resize';
var
  LWidth : integer;
begin
  inherited Resize;
  try
    LWidth := (Self.ClientWidth-(2 * Self.ColCount)) div Self.ColCount+1;
    Self.DefaultColWidth := LWidth;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMonthlyDeficitGrid.ClearGrid;
const OPNAME = 'TMonthlyDeficitGrid.ClearGrid';
var
  LIndex: integer;
begin
  try
    for LIndex := 0 to Self.ColCount -1 do
      Self.Cols[LIndex].Clear;
    Self.ColCount := 14;
    Self.RowCount := 2;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TRISelector }

procedure TRISelector.CreateMemberObjects;
const OPNAME = 'TRISelector.CreateMemberObjects';
begin
  inherited;
  try
    FRISelector                  := TCheckListBox.Create(Self);
    FBtnPanel                    := TPanel.Create(Self);
    FEdtPanel                    := TPanel.Create(Self);

    FLblRecurranceInterval       := TLabel.Create(Self);
    FEdtRecurranceInterval       := TEdit.Create(Self);

    FBtnAdd                      := TButton.Create(Self);
    FBtnDelete                   := TButton.Create(Self);

    FBtnOk                       := TButton.Create(Self);
    FBtnCancel                   := TButton.Create(Self);

    FRISelector.Parent           := Self;
    FBtnPanel.Parent             := Self;
    FEdtPanel.Parent             := Self;

    FLblRecurranceInterval.Parent := FEdtPanel;
    FEdtRecurranceInterval.Parent := FEdtPanel;
    FBtnAdd.Parent                := FEdtPanel;
    FBtnDelete.Parent             := FEdtPanel;

    FBtnOk.Parent                := FBtnPanel;
    FBtnCancel.Parent            := FBtnPanel;

    FBtnAdd.Enabled              := True;
    FBtnDelete.Enabled           := False;

    FBtnAdd.ModalResult          := mrNone;
    FBtnDelete.ModalResult       := mrNone;
    FBtnOk.ModalResult           := mrOk;
    FBtnCancel.ModalResult       := mrCancel;

    FBtnAdd.OnClick              := OnAssuranceIntervalSelectorAddBtnClick;
    FBtnDelete.OnClick           := OnAssuranceIntervalSelectorDelBtnClick;
    FRISelector.OnClickCheck     := OnAssuranceSelectorClick;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRISelector.Resize;
const OPNAME = 'TRISelector.Resize';
begin
  inherited;
  try
    FBtnPanel.Align            := alNone;
    FEdtPanel.Align            := alNone;
    FRISelector.Align   := alNone;

    FBtnPanel.Align            := alBottom;
    FEdtPanel.Align            := alBottom;
    FRISelector.Align   := alClient;

    CenterControls;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRISelector.CenterControls;
const OPNAME = 'TRISelector.CenterControls';
var
  LWidth: integer;
begin
  try
    LWidth                     := FBtnPanel.ClientWidth div 2;
    LWidth                     := LWidth - FBtnOk.Width - FBtnCancel.Width;
    LWidth                     := Max(1,LWidth);
    LWidth                     := LWidth div 3;

    FLblRecurranceInterval.Left := 5;
    FEdtRecurranceInterval.Left := FLblRecurranceInterval.Left + FLblRecurranceInterval.Width + 5;

    FBtnAdd.Left               := FEdtRecurranceInterval.Left + FEdtRecurranceInterval.Width + 5;;
    FBtnDelete.Left            := FBtnAdd.Left + FBtnAdd.Width + 5;

    FBtnOk.Left                := (FBtnPanel.ClientWidth div 2) + LWidth;
    FBtnCancel.Left            :=  FBtnPanel.ClientWidth - LWidth - FBtnCancel.Width;

    FLblRecurranceInterval.Top  := (FEdtPanel.Height - FLblRecurranceInterval.Height) div 2;
    FEdtRecurranceInterval.Top  := (FEdtPanel.Height - FEdtRecurranceInterval.Height) div 2;

    FBtnAdd.Top                := (FBtnPanel.Height - FBtnAdd.Height) div 2;
    FBtnDelete.Top             := (FBtnPanel.Height - FBtnDelete.Height) div 2;

    FBtnOk.Top                 := (FBtnPanel.Height - FBtnOk.Height) div 2;
    FBtnCancel.Top             := (FBtnPanel.Height - FBtnCancel.Height) div 2;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRISelector.LanguageHasChanged: boolean;
const OPNAME = 'TRISelector.LanguageHasChanged';
var
  LRecurranceInterval : integer;
begin
  Result := Inherited LanguageHasChanged;
  try
    Self.Caption := FAppModules.Language.GetString('TRISelectorPanel.PanelCaption');
    FEdtPanel.Caption  := '';
    FLblRecurranceInterval.Caption := FAppModules.Language.GetString('LabelText.RecurranceInterval');
    LRecurranceInterval := GetCurrentRecurranceInterval;
    if (LRecurranceInterval > 0) then
      FEdtRecurranceInterval.Text := IntToStr(LRecurranceInterval)
    else
      FEdtRecurranceInterval.Text := '';
    FBtnPanel.Caption  := '';
    FBtnAdd.Caption    := FAppModules.Language.GetString('ButtonCaption.Add');
    FBtnDelete.Caption := FAppModules.Language.GetString('ButtonCaption.Delete');
    FBtnOk.Caption     := FAppModules.Language.GetString('ButtonCaption.OK');
    FBtnCancel.Caption := FAppModules.Language.GetString('ButtonCaption.Cancel');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRISelector.YRCGraphDataObject: TAbstractYRCGraphDataObject;
const OPNAME = 'TRISelector.YRCGraphDataObject';
begin
  Result := nil;
  try
    Result := TAbstractYRCModelDataObject(FAppModules.Model.ModelData).YRCGraphDataObject;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRISelector.PopulateRecurrance(AYearValues, ASavedValues: TIntegerArray;ANumberOfYUears : integer;AStochasticRan : boolean);
const OPNAME = 'TRISelector.PopulateRecurrance';
var
  LIndex: integer;
begin
  try
    FRISelector.Items.Clear;
    for LIndex := 0 to Length(AYearValues) - 1 do
    begin
      FRISelector.Items.AddObject(Format('1/%d years', [AYearValues[LIndex]]), TObject(AYearValues[LIndex]));
      if (AYearValues[LIndex] > ANumberOfYUears) and
         (not AStochasticRan) then
      begin
        FRISelector.Checked[LIndex] := False;
        FRISelector.ItemEnabled[LIndex] := False;
      end;
    end;

    for LIndex := Low(ASavedValues) to High(ASavedValues)do
       FRISelector.Checked[LIndex] := (ASavedValues[LIndex] > 0);
    FRISelector.ItemIndex := 0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRISelector.ReadRecurranceSaved(var ASavedValues: TIntegerArray; AYearValues : TIntegerArray);
const OPNAME = 'TRISelector.ReadRecurranceSaved';
var
  LIndex: integer;
begin
  try
    SetLength(ASavedValues, Length(AYearValues));
    for LIndex := Low(AYearValues) to High(AYearValues) do
      ASavedValues[LIndex] := AYearValues[LIndex];

    for LIndex := Low(ASavedValues) to High(ASavedValues) do
       if not FRISelector.Checked[LIndex] then
         ASavedValues[LIndex] := 0;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRISelector.GetRecurranceInterval: integer;
const OPNAME = 'TRISelector.GetRecurranceInterval';
begin
  Result := 0;
  try
    FAssuranceInterval := StrToInt(FEdtRecurranceInterval.Text);
    Result             := FAssuranceInterval;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRISelector.SetRecurranceInterval(const AValue: integer);
const OPNAME = 'TRISelector.SetRecurranceInterval';
begin
  try
    FAssuranceInterval         := AValue;
    FEdtRecurranceInterval.Text := IntToStr(FAssuranceInterval);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRISelector.GetCurrentRecurranceInterval: integer;
const OPNAME = 'TRISelector.GetCurrentRecurranceInterval';
begin
  Result := 0;
  try
    if (FRISelector.ItemIndex >= 0) then
      Result := Integer(FRISelector.Items.Objects[FRISelector.ItemIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRISelector.OnAssuranceSelectorClick(Sender: TObject);
const OPNAME = 'TRISelector.OnAssuranceSelectorClick';
var
  LValue : integer;
begin
  try
    if (YRCGraphDataObject <> nil) and (RISelector.ItemIndex >= 0) then
    begin
      LValue := Integer(RISelector.Items.Objects[RISelector.ItemIndex]);
      RecurranceInterval := LValue;
      if InArray(LValue, YRCGraphDataObject.SelectedAssuranceIntervalDefaultArray) then
        BtnDelete.Enabled := False
      else
        BtnDelete.Enabled := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRISelector.OnAssuranceIntervalSelectorAddBtnClick(Sender: TObject);
const OPNAME = 'TRISelector.OnAssuranceIntervalSelectorAddBtnClick';
var
  LYearValues : TIntegerArray;
  LIndex : integer;
begin
  try
    if (YRCGraphDataObject <> nil) and (EdtRecurrranceInterval.Text <> '') then
    begin
      SetLength(LYearValues, Length(YRCGraphDataObject.SelectedAssuranceIntervalYearsArray));
      for LIndex := Low(LYearValues) to High(LYearValues) do
        LYearValues[LIndex] := YRCGraphDataObject.SelectedAssuranceIntervalYearsArray[LIndex];
      InsertIntoArray(LYearValues, RecurranceInterval);
      YRCGraphDataObject.SelectedAssuranceIntervalYearsArray := LYearValues;
      PopulateRecurrance(YRCGraphDataObject.SelectedAssuranceIntervalYearsArray,
                                           YRCGraphDataObject.SelectedAssuranceIntervalsavedArray,
                                           YRCGraphDataObject.SelectedPlane.PlaneYears,True);
      RISelector.ItemIndex := RISelector.Items.IndexOfObject(TObject(RecurranceInterval));
      OnAssuranceSelectorClick(nil);
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRISelector.OnAssuranceIntervalSelectorDelBtnClick(Sender: TObject);
const OPNAME = 'TRISelector.OnAssuranceIntervalSelectorDelBtnClick';
var
  LYearValues : TIntegerArray;
  LIndex : integer;
begin
  try
    if (YRCGraphDataObject <> nil) and (EdtRecurrranceInterval.Text <> '') then
    begin
      SetLength(LYearValues, Length(YRCGraphDataObject.SelectedAssuranceIntervalYearsArray));
      for LIndex := Low(LYearValues) to High(LYearValues) do
        LYearValues[LIndex] := YRCGraphDataObject.SelectedAssuranceIntervalYearsArray[LIndex];
      DeleteFromArray(LYearValues, RecurranceInterval);
      YRCGraphDataObject.SelectedAssuranceIntervalYearsArray := LYearValues;
      PopulateRecurrance(YRCGraphDataObject.SelectedAssuranceIntervalYearsArray,
                                            YRCGraphDataObject.SelectedAssuranceIntervalsavedArray,
                                            YRCGraphDataObject.SelectedPlane.PlaneYears,True);
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TRISelector.InArray(AValue : integer; AArray : TIntegerArray) : boolean;
const OPNAME = 'TRISelector.InArray';
var
  LIndex : integer;
begin
  Result := False;
  try
    for LIndex := Low(AArray) to High(AArray) do
      if (AValue = AArray[LIndex]) then
      begin
        Result := True;
        Break;
      end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRISelector.InsertIntoArray(var AArray : TIntegerArray; const AValue : integer);
const OPNAME = 'TRISelector.InsertIntoArray';
var
  LIndex,
  LCount,
  LPos,
  LInt : integer;
  LArray : TIntegerArray;
begin
  try
    if (YRCGraphDataObject <> nil) and (not InArray(AValue, AArray)) then
    begin
      LPos := -1;
      if (AValue < AArray[Low(AArray)]) then
        LPos := Low(AArray)
      else
      if (AValue > AArray[High(AArray)]) then
        LPos := High(AArray) + 1
      else
      for LIndex := 0 to Length(AArray) - 2 do
      begin
        LInt := AArray[LIndex];
        if (AValue > LInt) and
           (AValue < AArray[LIndex + 1]) then
        begin
          LPos := LIndex + 1;
          Break;
        end;
      end;
      SetLength(AArray, Length(AArray) + 1);
      if (LPos >= 0) then
      begin
        for LIndex := High(AArray) downto LPos do
          AArray[LIndex] := AArray[LIndex - 1];
        AArray[LPos] := AValue;
        SetLength(LArray, Length(YRCGraphDataObject.SelectedAssuranceIntervalsavedArray) + 1);
        LCount := 0;
        for LIndex := Low(LArray) to High(LArray) do
        begin
          if (LIndex = LPos) then
            LArray[LIndex] := 0
          else
          begin
            LArray[LIndex] := YRCGraphDataObject.SelectedAssuranceIntervalsavedArray[LCount];
            LCount := LCount + 1;
          end;
        end;
        YRCGraphDataObject.SelectedAssuranceIntervalSavedArray := LArray;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRISelector.DeleteFromArray(var AArray : TIntegerArray; const AValue : integer);
const OPNAME = 'TRISelector.DeleteFromArray';
var
  LIndex,
  LPos : integer;
  LArray : TIntegerArray;
begin
  try
    if (YRCGraphDataObject <> nil) and (not InArray(AValue, YRCGraphDataObject.SelectedAssuranceIntervalDefaultArray)) then
    begin
      LPos := -1;
      for LIndex := 0 to High(AArray) do
      begin
        if (AValue = AArray[LIndex]) then
        begin
          LPos := LIndex;
          Break;
        end;
      end;
      if (LPos >= 0) then
      begin
        AArray[LPos] := 0;
        for LIndex := LPos to (High(AArray) - 1) do
          AArray[LIndex] := AArray[LIndex + 1];
        AArray[High(AArray)] := 0;
        SetLength(LArray, Length(YRCGraphDataObject.SelectedAssuranceIntervalSavedArray));
        for LIndex := 0 to (High(LArray) - 1) do
          if (LIndex >= LPos) then
            LArray[LIndex] := YRCGraphDataObject.SelectedAssuranceIntervalSavedArray[LIndex + 1]
          else
            LArray[LIndex] := YRCGraphDataObject.SelectedAssuranceIntervalSavedArray[LIndex];
        SetLength(LArray, Length(LArray) - 1);
        YRCGraphDataObject.SelectedAssuranceIntervalSavedArray := LArray;
        SetLength(AArray, Length(AArray) - 1);
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;


{ TBoxPlotSeriesSelector }

procedure TBoxPlotSeriesSelector.CreateMemberObjects;
const OPNAME = 'TBoxPlotSeriesSelector.CreateMemberObjects';
begin
  inherited;
  try
    FSeriesSelector              := TCheckListBox.Create(Self);
    FLoadCaseSelector            := TCheckListBox.Create(Self);
    FHearderPanel                := TPanel.Create(Self);
    FBtnPanel                    := TPanel.Create(Self);
    FEdtPanel                    := TPanel.Create(Self);

    FLblSeries                   := TLabel.Create(Self);
    FLblBottomChartLabel         := TLabel.Create(Self);
    FLblSeriesSelector           := TLabel.Create(Self);
    FLblSeriesLoadCase           := TLabel.Create(Self);

    FMonthTypeGroup              := TGroupBox.Create(Self);
    FMonthName                   := TRadioButton.Create(Self);
    FMonthNumber                 := TRadioButton.Create(FMonthTypeGroup);

    FEdtSeries                   := TEdit.Create(Self);

    FBtnAdd                      := TButton.Create(Self);
    FBtnDelete                   := TButton.Create(Self);

    FBtnOk                       := TButton.Create(Self);
    FBtnCancel                   := TButton.Create(Self);

    FMonthTypeGroup.Parent       := FEdtPanel;
    FMonthName.Parent            := FMonthTypeGroup;
    FMonthNumber.Parent          := FMonthTypeGroup;

    FSeriesSelector.Parent       := Self;
    FLoadCaseSelector.Parent     := Self;
    FHearderPanel.Parent         := Self;
    FBtnPanel.Parent             := Self;
    FEdtPanel.Parent             := Self;

    FLblSeries.Parent            := FEdtPanel;
    FLblBottomChartLabel.Parent  := FEdtPanel;
    FEdtSeries.Parent            := FEdtPanel;
    FBtnAdd.Parent               := FBtnPanel;
    FBtnDelete.Parent            := FBtnPanel;
    FBtnOk.Parent                := FBtnPanel;
    FBtnCancel.Parent            := FBtnPanel;
    FLblSeriesSelector.Parent    := FHearderPanel;
    FLblSeriesLoadCase.Parent    := FHearderPanel;


    FBtnAdd.Enabled              := False;
    FBtnDelete.Enabled           := False;

    FBtnOk.ModalResult           := mrOk;
    FBtnCancel.ModalResult       := mrCancel;

    FSeriesSelector.OnClickCheck := OnCheckedChange;
    FSeriesSelector.OnClick      := OnSelectionChange;
    FMonthNumber.OnClick         := OnMonthTypeCheckedChange;
    FMonthName.OnClick           := OnMonthTypeCheckedChange;
    FSeriesSelector.OnKeyDown    := OnAnyKeyDown;
    FBtnDelete.OnClick           := OnDeleteSelection;
    FEdtSeries.OnChange          := OnEditTextChange;
    FBtnAdd.OnClick                := OnAddSelection;
    FLoadCaseSelector.OnClickCheck := OnLoadCaseSelectorClick;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TBoxPlotSeriesSelector.LanguageHasChanged: boolean;
const OPNAME = 'TBoxPlotSeriesSelector.LanguageHasChanged';
begin
  Result := Inherited LanguageHasChanged;
  try
    Self.Caption       := FAppModules.Language.GetString('TBoxPlotSeriesSelector.PanelCaption');
    FEdtPanel.Caption  := '';
    FLblSeries.Caption := FAppModules.Language.GetString('LabelCaption.BoxPlotInterval');
    FLblBottomChartLabel.Caption := FAppModules.Language.GetString('LabelCaption.MonthTypeValues');
    FMonthNumber.Caption         := FAppModules.Language.GetString('RadioCaption.MonthNumbers');
    FMonthName.Caption           := FAppModules.Language.GetString('RadioCaption.YearMonthNames');
    FEdtSeries.Text    := '';
    FBtnPanel.Caption  := '';
    FBtnAdd.Caption    := FAppModules.Language.GetString('ButtonCaption.Add');
    FBtnDelete.Caption := FAppModules.Language.GetString('ButtonCaption.Delete');
    FBtnOk.Caption     := FAppModules.Language.GetString('ButtonCaption.OK');
    FBtnCancel.Caption := FAppModules.Language.GetString('ButtonCaption.Cancel');
    FLblSeriesSelector.Caption := FAppModules.Language.GetString('LabelCaption.SeriesSelecter');
    FLblSeriesLoadCase.Caption := FAppModules.Language.GetString('LabelCaption.SeriesLoadCase');
    
    FSeriesSelector.Items.Add('Min');
    FSeriesSelector.Items.Add('0.5%');
    FSeriesSelector.Items.Add('1%');
    FSeriesSelector.Items.Add('2%');
    FSeriesSelector.Items.Add('98%');
    FSeriesSelector.Items.Add('99%');
    FSeriesSelector.Items.Add('99.5%');
    FSeriesSelector.Items.Add('Max');

    {FSeriesSelector.Checked[0] := True;
    FSeriesSelector.Checked[1] := True;
    FSeriesSelector.Checked[2] := True;
    FSeriesSelector.Checked[3] := True;
    FSeriesSelector.Checked[4] := True;
    FSeriesSelector.Checked[5] := True;
    FSeriesSelector.Checked[6] := True;}
    LoadFromViewIni;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TBoxPlotSeriesSelector.OnCheckedChange(Sender: TObject);
const OPNAME = 'TBoxPlotSeriesSelector.OnCheckedChange';
begin
  try
    //if (FSeriesSelector.ItemIndex <= 6) then
    //begin
    //FSeriesSelector.Checked[FSeriesSelector.ItemIndex] := True;
    //end;
    SaveToViewIni;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TBoxPlotSeriesSelector.OnMonthTypeCheckedChange(Sender: TObject);
const OPNAME = 'TBoxPlotSeriesSelector.OnMonthTypeCheckedChange';
begin
  try
    SaveToViewIni;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TBoxPlotSeriesSelector.OnDeleteSelection(Sender: TObject);
const OPNAME = 'TBoxPlotSeriesSelector.OnDeleteSelection';
begin
  try
    if (FSeriesSelector.ItemIndex > 7) then
    begin
      FSeriesSelector.Items.Delete(FSeriesSelector.ItemIndex);
      OnSelectionChange(nil);
    end;
    SaveToViewIni;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TBoxPlotSeriesSelector.OnAnyKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
const OPNAME = 'TBoxPlotSeriesSelector.OnAnyKeyDown';
begin
  try
    OnSelectionChange(nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TBoxPlotSeriesSelector.OnSelectionChange(Sender: TObject);
const OPNAME = 'TBoxPlotSeriesSelector.OnSelectionChange';
begin
  try
    FBtnDelete.Enabled := False;
    if (FSeriesSelector.ItemIndex > 7) then
    begin
      FBtnDelete.Enabled := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TBoxPlotSeriesSelector.Resize;
const OPNAME = 'TBoxPlotSeriesSelector.Resize';
var
  LWidth: integer;
begin
  inherited;
  try
    FBtnPanel.Align         := alNone;
    FEdtPanel.Align         := alNone;
    FSeriesSelector.Align   := alNone;

    FBtnPanel.Align         := alBottom;
    FEdtPanel.Align         := alBottom;
    FHearderPanel.Align     := alTop;
    FSeriesSelector.Align   := alLeft;
    FSeriesSelector.Width   := Self.ClientWidth div 2;
    FLoadCaseSelector.Align := alClient;

    if not (FLoadCaseSelector.Visible) then
      FSeriesSelector.Align := alClient;

    FLblSeriesSelector.Top := 15;
    FLblSeriesLoadCase.Top := 15;

    FLblSeriesSelector.Left := 20;
    FLblSeriesLoadCase.Left := FSeriesSelector.Width + 20;

    LWidth                     := FBtnPanel.ClientWidth div 2;
    LWidth                     := LWidth - FBtnOk.Width - FBtnCancel.Width;
    LWidth                     := Max(1,LWidth);
    LWidth                     := LWidth div 3;

    FLblSeries.Left            := 5;
    FEdtSeries.Left            := FLblSeries.Left + FLblSeries.Width + 5;
    FLblBottomChartLabel.Left  := FEdtSeries.Left + FEdtSeries.Width + 10;

    FMonthTypeGroup.Left       := FLblBottomChartLabel.Left + FLblBottomChartLabel.Width + 5;
    FMonthTypeGroup.Height     := FEdtPanel.Height - 10;
    FMonthTypeGroup.Width      := (FEdtPanel.Width - FMonthTypeGroup.Left - 5);
    FMonthName.Left            := 5;
    FMonthName.Width           := 115;
    FMonthNumber.Left          := FMonthName.Left + FMonthName.Width;
    FMonthNumber.Width         := 95;

    FBtnAdd.Left               := 5;
    FBtnDelete.Left            := FBtnAdd.Left + FBtnAdd.Width + 5;

    FBtnOk.Left                := (FBtnPanel.ClientWidth div 2) + LWidth;
    FBtnCancel.Left            :=  FBtnPanel.ClientWidth - LWidth - FBtnCancel.Width;

    FLblSeries.Top             := (FEdtPanel.Height - FLblSeries.Height) div 2;
    FEdtSeries.Top             := (FEdtPanel.Height - FEdtSeries.Height) div 2;
    FLblBottomChartLabel.Top   := (FEdtPanel.Height - FLblBottomChartLabel.Height) div 2;

    FMonthTypeGroup.Top        := (FEdtPanel.Height - FMonthTypeGroup.Height) div 2;
    FMonthNumber.Top           := 10;
    FMonthName.Top             := 10;

    FBtnAdd.Top                := (FBtnPanel.Height - FBtnAdd.Height) div 2;
    FBtnDelete.Top             := (FBtnPanel.Height - FBtnDelete.Height) div 2;

    FBtnOk.Top                 := (FBtnPanel.Height - FBtnOk.Height) div 2;
    FBtnCancel.Top             := (FBtnPanel.Height - FBtnCancel.Height) div 2;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TBoxPlotSeriesSelector.SeriesValuesCommatext: string;
const OPNAME = 'TBoxPlotSeriesSelector.SeriesValuesCommatext';
var
  LValue: string;
  LIndex : integer;
begin
  Result := '';
  try
    for LIndex := 0 to FSeriesSelector.Items.Count -1 do
    begin
      if FSeriesSelector.Checked[LIndex] then
      begin
        if(LIndex = 0) then
          LValue := '0%'
        else if(LIndex = 7) then
          LValue := '100%'
        else
          LValue := Trim(FSeriesSelector.Items[LIndex]);
        LValue := Copy(LValue,1,Length(LValue)-1);
        Result := Result + LValue + ',';
      end;
    end;
    if(Result <> '') then
      Result := Copy(Result,1,Length(Result)-1);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TBoxPlotSeriesSelector.LoadCase : integer;
const OPNAME = 'TBoxPlotSeriesSelector.LoadCase';
var
  LIndex : integer;
begin
  Result := 0;
  try
    for LIndex := 0 to FLoadCaseSelector.Count-1 do
    begin
      if FLoadCaseSelector.Checked[LIndex] then
      begin
        Result := LIndex + 1;
        Break;
      end;  
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TBoxPlotSeriesSelector.OnAddSelection(Sender: TObject);
const OPNAME = 'TBoxPlotSeriesSelector.OnAddSelection';
begin
  try
    FSeriesSelector.Items.Add(Trim(FEdtSeries.Text) + '%');
    SaveToViewIni;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TBoxPlotSeriesSelector.OnEditTextChange(Sender: TObject);
const OPNAME = 'TBoxPlotSeriesSelector.OnEditTextChange';
var
  LValue: string;
  LData: double;
begin
  FBtnAdd.Enabled := False;
  FEdtSeries.Color := clWindow;
  try
    LValue := Trim(FEdtSeries.Text);
    if(LValue <> '') then
    begin
      LData  := StrToFloatDef(LValue,-1.0);
      if(LData < 0.0) or (LData > 100.0) then
      begin
        FEdtSeries.Color := clRed;
      end
      else
      begin
        FBtnAdd.Enabled := True;
      end
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TBoxPlotSeriesSelector.OnLoadCaseSelectorClick(Sender : TObject);
const OPNAME = 'TBoxPlotSeriesSelector.OnLoadCaseSelectorClick';
var
  LIndex : integer;
begin
  try
    LIndex := FLoadCaseSelector.ItemIndex;
    UnSelectLoadCases;
    FLoadCaseSelector.Checked[LIndex] := True;
    SaveToViewIni;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TBoxPlotSeriesSelector.UnSelectLoadCases;
const OPNAME = 'TBoxPlotSeriesSelector.UnSelectLoadCases';
var
  LIndex : integer;
begin
  try
    for LIndex := 0 to FLoadCaseSelector.Count-1 do
      FLoadCaseSelector.Checked[LIndex] := False;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TBoxPlotSeriesSelector.LoadFromViewIni;
const OPNAME = 'TBoxPlotSeriesSelector.LoadFromViewIni';
var
  LAllData: String;
  LSelectedData: String;
  LAllValues: TStringList;
  LSelectedValues: TStringList;
  LSelectedLoadCase : string;
  LLoadCase : integer;
  LPos,
  LIndex,
  LSelectedMonthType: integer;
begin
  try
    LAllValues      := TStringList.Create;
    LSelectedValues := TStringList.Create;
    try
      LAllData           := FAppModules.ViewIni.ReadString(ClassName,'AllValues','');
      LSelectedData      := FAppModules.ViewIni.ReadString(ClassName,'SelectedValues','');
      LSelectedLoadCase  := FAppModules.ViewIni.ReadString(ClassName,'SelectedLoadCase', '');
      LLoadCase          := FAppModules.ViewIni.ReadInteger(ClassName,'LoadCase',0);
      LSelectedMonthType := FAppModules.ViewIni.ReadInteger(ClassName,'SelectedMonthType', 0);
      LAllValues.CommaText := LAllData;
      LSelectedValues.CommaText := LSelectedData;
      for LIndex := 0 to LAllValues.Count -1 do
        FSeriesSelector.Items.Add(LAllValues[LIndex]+'%');
      for LIndex := 0 to LSelectedValues.Count -1 do
      begin
        LPos := StrToIntDef(LSelectedValues[LIndex],0);
        if(LPos >= 0) and (LPos < FSeriesSelector.Items.Count) then
          FSeriesSelector.Checked[LPos] := True;
      end;
      for LIndex := 1 to LLoadCase do
        FLoadCaseSelector.Items.Add('Load Case '+IntToStr(LIndex));
      for LIndex := 0 to FLoadCaseSelector.Count-1 do
      begin
        if UpperCase(LSelectedLoadCase) = UpperCase(FLoadCaseSelector.Items[LIndex]) then
        begin
          FLoadCaseSelector.Checked[LIndex] := True;
          Break;
        end;
      end;
      if(LSelectedMonthType = 0 ) then
        FMonthName.Checked := True
      else
      if(LSelectedMonthType = 1) then
        FMonthNumber.Checked := True;
    finally
      LAllValues.Free;
      LSelectedValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TBoxPlotSeriesSelector.SaveToViewIni;
const OPNAME = 'TBoxPlotSeriesSelector.SaveToViewIni';
var
  LValue : String;
  LAllValues: TStringList;
  LSelectedValues: TStringList;
  LIndex: integer;
  LSelectedLoadCase : string;
  LSelectedMonthType : integer;
begin
  try
    LAllValues      := TStringList.Create;
    LSelectedValues := TStringList.Create;
    try
      for LIndex := 0 to FSeriesSelector.Items.Count -1 do
      begin
        if(LIndex > 7) then
        begin
          LValue := Trim(FSeriesSelector.Items[LIndex]);
          LValue := Copy(LValue,1,Length(LValue)-1);
          LAllValues.Add(LValue);
        end;
        if FSeriesSelector.Checked[LIndex] then
           LSelectedValues.Add(IntToStr(LIndex));
      end;
      for LIndex := 0 to FLoadCaseSelector.Items.Count -1 do
      begin
        if FLoadCaseSelector.Checked[LIndex] then
          LSelectedLoadCase := FLoadCaseSelector.Items[LIndex];
      end;

      LSelectedMonthType := -1;
      if(FMonthName.Checked) then
        LSelectedMonthType := 0
      else
      if(FMonthNumber.Checked) then
        LSelectedMonthType := 1;

      FAppModules.ViewIni.WriteString(ClassName,'AllValues',LAllValues.CommaText);
      FAppModules.ViewIni.WriteString(ClassName,'SelectedValues',LSelectedValues.CommaText);
      FAppModules.ViewIni.WriteString(ClassName,'SelectedLoadCase', LSelectedLoadCase);
      FAppModules.ViewIni.WriteInteger(ClassName,'SelectedMonthType', LSelectedMonthType);

    finally
      LAllValues.Free;
      LSelectedValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TSupplyComplianceGrid }

procedure TSupplyComplianceGrid.ClearGrid;
const OPNAME = 'TSupplyComplianceGrid.ClearGrid';
var
  LIndex : integer;
begin
  try
    for LIndex := 0 to Self.ColCount -1 do
      Self.Cols[LIndex].Clear;
    Self.RowCount := 2;
    Self.ColCount := 12 + Integer(FGridType);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSupplyComplianceGrid.CreateMemberObjects;
const OPNAME = 'TSupplyComplianceGrid.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FAlignment    := taRightJustify;
    FCellColoring := TStringList.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSupplyComplianceGrid.DestroyMemberObjects;
const OPNAME = 'TSupplyComplianceGrid.DestroyMemberObjects';
begin
  try
    FCellColoring.Free;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSupplyComplianceGrid.DrawCell(ACol, ARow: Integer; Rect: TRect;State: TGridDrawState);
const OPNAME = 'TSupplyComplianceGrid.DrawCell';
var
  LTxtRect: TRect;
  LStr: string;
  LTxtHeight,
  LColor : TColor;
begin
  inherited DrawCell(ACol, ARow, Rect, State);
  try
    case FGridType of
      gtNormal:
      begin
        if (ARow in [0, 1]) then
        begin
          Self.Canvas.Brush.Color   := clBtnFace;
          Self.Brush.Style          := bsSolid;
          Self.Canvas.Pen.Style     := psSolid;
          Self.Canvas.FillRect(Rect);

          if (ACol in [0, 1, 2]) then
          begin
            Rect.Top                := 0;
            Rect.Bottom             := Rect.Bottom + Self.DefaultRowHeight;

            Self.Canvas.Brush.Color := clBtnFace;
            Self.Brush.Style        := bsSolid;
            Self.Canvas.Pen.Style   := psSolid;
            Self.Canvas.FillRect(Rect);

            LtxtRect                := Rect;
            LTxtRect.Left           := Rect.Left + 4;
            LTxtRect.Top            := 8;

            LStr                    := Self.Cells[ACol, 0];

            Self.Canvas.Font.Color  := clInfoText;
            Self.Canvas.Font.Name   := Self.Font.Name;
            Self.Canvas.Font.Size   := Self.Font.Size;
            Self.Canvas.Font.Style  := [fsBold];
            DrawText(Self.Canvas.Handle,
                     PChar(LStr),
                     length(LStr), LTxtRect,
                     DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);
          end
          else
          if (ACol > 2) and
             (ACol < 8) then
          begin
            if (ARow = 0) then
            begin
              Rect.Left               := (Rect.Left - (Self.DefaultColWidth * (ACol - 3))) - (ACol - 3);
              Rect.Right              :=  Rect.Left + (Self.DefaultColWidth * 5);

              Self.Canvas.Brush.Color := clBtnFace;
              Self.Brush.Style        := bsSolid;
              Self.Canvas.Pen.Style   := psSolid;
              Self.Canvas.FillRect(Rect);

              Lstr                    := Self.Cells[3, ARow];
              LTxtHeight              := Self.Canvas.TextHeight(LStr);

              LtxtRect                := Rect;
              LTxtRect.Left           := LTxtRect.Left + 12;
              LTxtRect.Top            := (LtxtRect.Bottom div 2) - (LTxtHeight div 2);

              Self.Canvas.Font.Color  := clInfoText;
              Self.Canvas.Font.Name   := Self.Font.Name;
              Self.Canvas.Font.Size   := Self.Font.Size;
              Self.Canvas.Font.Style  := [fsBold];
              DrawText(Self.Canvas.Handle,
                       PChar(LStr),
                       length(LStr), LTxtRect,
                       DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);
            end;
            if (ARow = 1) then
            begin
              LtxtRect                := Rect;
              LTxtRect.Left           := Rect.Left + 4;
              LTxtRect.Top            := Rect.Top + 2;

              LStr                    := Self.Cells[ACol, ARow];

              Self.Canvas.Font.Color  := clInfoText;
              Self.Canvas.Font.Name   := Self.Font.Name;
              Self.Canvas.Font.Size   := Self.Font.Size;
              Self.Canvas.Font.Style  := [fsBold];
              DrawText(Self.Canvas.Handle,
                       PChar(LStr),
                       length(LStr), LTxtRect,
                       DT_SINGLELINE or DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);
            end;
          end
          else
          if (ACol > 7) then
          begin
            if (ARow = 0) then
            begin
              Rect.Left               := (Rect.Left - (Self.DefaultColWidth * (ACol - 8))) - (ACol - 8);
              Rect.Right              :=  Rect.Left + (Self.DefaultColWidth * 5);

              Self.Canvas.Brush.Color := clBtnFace;
              Self.Brush.Style        := bsSolid;
              Self.Canvas.Pen.Style   := psSolid;
              Self.Canvas.FillRect(Rect);

              Lstr                    := Self.Cells[8, ARow];
              LTxtHeight              := Self.Canvas.TextHeight(LStr);

              LtxtRect                := Rect;
              LTxtRect.Left           := LTxtRect.Left + 12;
              LTxtRect.Top            := (LtxtRect.Bottom div 2) - (LTxtHeight div 2);

              Self.Canvas.Font.Color  := clInfoText;
              Self.Canvas.Font.Name   := Self.Font.Name;
              Self.Canvas.Font.Size   := Self.Font.Size;
              Self.Canvas.Font.Style  := [fsBold];
              DrawText(Self.Canvas.Handle,
                       PChar(LStr),
                       length(LStr), LTxtRect,
                       DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);
            end;
            if (ARow = 1) then
            begin
              LtxtRect                := Rect;
              LTxtRect.Left           := Rect.Left + 4;
              LTxtRect.Top            := Rect.Top + 2;

              LStr                    := Self.Cells[ACol, ARow];

              Self.Canvas.Font.Color  := clInfoText;
              Self.Canvas.Font.Name   := Self.Font.Name;
              Self.Canvas.Font.Size   := Self.Font.Size;
              Self.Canvas.Font.Style  := [fsBold];
              DrawText(Self.Canvas.Handle,
                       PChar(LStr),
                       length(LStr), LTxtRect,
                       DT_SINGLELINE or DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);
            end;
          end;
        end
        else
        begin
          if IsColored(ACol, ARow) then
            LColor := FCellColor
          else
            LColor := clBlack;

          Self.Canvas.Brush.Color := clWindow;
          Self.Brush.Style        := bsSolid;
          Self.Canvas.Pen.Style   := psSolid;
          Self.Canvas.FillRect(Rect);

          LtxtRect                := Rect;
          LTxtRect.Left           := Rect.Left + 4;
          LTxtRect.Top            := Rect.Top + 2;

          LStr                    := Self.Cells[ACol, ARow];

          Self.Canvas.Font.Color  := clInfoText;
          Self.Canvas.Font.Name   := Self.Font.Name;
          Self.Canvas.Font.Size   := Self.Font.Size;
          Self.Canvas.Font.Color  := LColor;
          Self.Canvas.Font.Style  := [fsBold];
          inherited  DrawCell(ACol, ARow,Rect,State);

          {DrawText(Self.Canvas.Handle,
                   PChar(LStr),
                   length(LStr), LTxtRect,
                   DT_SINGLELINE or DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);}
        end;
      end;
      gtFiltered:
      begin
        if (ARow in [0, 1]) then
        begin
          Self.Canvas.Brush.Color   := clBtnFace;
          Self.Brush.Style          := bsSolid;
          Self.Canvas.Pen.Style     := psSolid;
          Self.Canvas.FillRect(Rect);

          if (ACol in [0, 1]) then
          begin
            Rect.Top                := 0;
            Rect.Bottom             := Rect.Bottom + Self.DefaultRowHeight;

            Self.Canvas.Brush.Color := clBtnFace;
            Self.Brush.Style        := bsSolid;
            Self.Canvas.Pen.Style   := psSolid;
            Self.Canvas.FillRect(Rect);

            LtxtRect                := Rect;
            LTxtRect.Left           := Rect.Left + 4;
            LTxtRect.Top            := 8;

            LStr                    := Self.Cells[ACol, 0];

            Self.Canvas.Font.Color  := clInfoText;
            Self.Canvas.Font.Name   := Self.Font.Name;
            Self.Canvas.Font.Size   := Self.Font.Size;
            Self.Canvas.Font.Style  := [fsBold];
            DrawText(Self.Canvas.Handle,
                     PChar(LStr),
                     length(LStr), LTxtRect,
                     DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);
          end
          else
          if (ACol > 1) and
             (ACol < 7) then
          begin
            if (ARow = 0) then
            begin
              Rect.Left               := (Rect.Left - (Self.DefaultColWidth * (ACol - 2))) - (ACol - 2);
              Rect.Right              :=  Rect.Left + (Self.DefaultColWidth * 5);

              Self.Canvas.Brush.Color := clBtnFace;
              Self.Brush.Style        := bsSolid;
              Self.Canvas.Pen.Style   := psSolid;
              Self.Canvas.FillRect(Rect);

              Lstr                    := Self.Cells[2, ARow];
              LTxtHeight              := Self.Canvas.TextHeight(LStr);

              LtxtRect                := Rect;
              LTxtRect.Left           := LTxtRect.Left + 12;
              LTxtRect.Top            := (LtxtRect.Bottom div 2) - (LTxtHeight div 2);

              Self.Canvas.Font.Color  := clInfoText;
              Self.Canvas.Font.Name   := Self.Font.Name;
              Self.Canvas.Font.Size   := Self.Font.Size;
              Self.Canvas.Font.Style  := [fsBold];
              DrawText(Self.Canvas.Handle,
                       PChar(LStr),
                       length(LStr), LTxtRect,
                       DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);
            end;
            if (ARow = 1) then
            begin
              LtxtRect                := Rect;
              LTxtRect.Left           := Rect.Left + 4;
              LTxtRect.Top            := Rect.Top + 2;

              LStr                    := Self.Cells[ACol, ARow];

              Self.Canvas.Font.Color  := clInfoText;
              Self.Canvas.Font.Name   := Self.Font.Name;
              Self.Canvas.Font.Size   := Self.Font.Size;
              Self.Canvas.Font.Style  := [fsBold];
              DrawText(Self.Canvas.Handle,
                       PChar(LStr),
                       length(LStr), LTxtRect,
                       DT_SINGLELINE or DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);
            end;
          end
          else
          if (ACol > 6) then
          begin
            if (ARow = 0) then
            begin
              Rect.Left               := (Rect.Left - (Self.DefaultColWidth * (ACol - 7))) - (ACol - 7);
              Rect.Right              :=  Rect.Left + (Self.DefaultColWidth * 5);

              Self.Canvas.Brush.Color := clBtnFace;
              Self.Brush.Style        := bsSolid;
              Self.Canvas.Pen.Style   := psSolid;
              Self.Canvas.FillRect(Rect);

              Lstr                    := Self.Cells[7, ARow];
              LTxtHeight              := Self.Canvas.TextHeight(LStr);

              LtxtRect                := Rect;
              LTxtRect.Left           := LTxtRect.Left + 12;
              LTxtRect.Top            := (LtxtRect.Bottom div 2) - (LTxtHeight div 2);

              Self.Canvas.Font.Color  := clInfoText;
              Self.Canvas.Font.Name   := Self.Font.Name;
              Self.Canvas.Font.Size   := Self.Font.Size;
              Self.Canvas.Font.Style  := [fsBold];
              DrawText(Self.Canvas.Handle,
                       PChar(LStr),
                       length(LStr), LTxtRect,
                       DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);
            end;
            if (ARow = 1) then
            begin
              LtxtRect                := Rect;
              LTxtRect.Left           := Rect.Left + 4;
              LTxtRect.Top            := Rect.Top + 2;

              LStr                    := Self.Cells[ACol, ARow];

              Self.Canvas.Font.Color  := clInfoText;
              Self.Canvas.Font.Name   := Self.Font.Name;
              Self.Canvas.Font.Size   := Self.Font.Size;
              Self.Canvas.Font.Style  := [fsBold];
              DrawText(Self.Canvas.Handle,
                       PChar(LStr),
                       length(LStr), LTxtRect,
                       DT_SINGLELINE or DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);
            end;
          end;
        end
        else
        begin
          if IsColored(ACol, ARow) then
            LColor := FCellColor
          else
            LColor := clBlack;

          Self.Canvas.Brush.Color := clWindow;
          Self.Brush.Style        := bsSolid;
          Self.Canvas.Pen.Style   := psSolid;
          Self.Canvas.FillRect(Rect);

          LtxtRect                := Rect;
          LTxtRect.Left           := Rect.Left + 4;
          LTxtRect.Top            := Rect.Top + 2;

          LStr                    := Self.Cells[ACol, ARow];

          Self.Canvas.Font.Color  := clInfoText;
          Self.Canvas.Font.Name   := Self.Font.Name;
          Self.Canvas.Font.Size   := Self.Font.Size;
          Self.Canvas.Font.Color  := LColor;
          Self.Canvas.Font.Style  := [fsBold];
          inherited  DrawCell(ACol, ARow,Rect,State);

          {DrawText(Self.Canvas.Handle,
                   PChar(LStr),
                   length(LStr), LTxtRect,
                   DT_SINGLELINE or DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);
          }
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSupplyComplianceGrid.GetGridType: TGridType;
const OPNAME = 'TSupplyComplianceGrid.GetGridType';
begin
  Result := gtNormal;
  try
    Result := FGridType;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSupplyComplianceGrid.Initialise: boolean;
const OPNAME = 'TSupplyComplianceGrid.Initialise';
begin
  Result := False;
  try
    Self.RowCount := 2;
    Self.ColCount := 12;
    Self.FixedCols  := 0;
    Self.FixedRows  := 0;
    Self.Color      := clWindow;
    Self.EditorMode := False;
    Self.Options    := [goFixedVertLine,goFixedHorzLine,goVertLine,goHorzLine];
    Self.Height     := 240;
    Self.ScrollBars := ssVertical;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSupplyComplianceGrid.IsColored(ACol, ARow: integer): boolean;
const OPNAME = 'TSupplyComplianceGrid.IsColored';
var
  LStrings : TStringList;
begin
  Result := False;
  try
    if (FCellColoring.Count > ARow) then
    begin
      LStrings             := TStringList.Create;
      try
        LStrings.CommaText := FCellColoring[ARow];
        if (LStrings.Count > ACol) then
          Result           := (LStrings[ACol] = 'Y');
      finally
        LStrings.Clear;
        LStrings.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSupplyComplianceGrid.LanguageHasChanged: boolean;
const OPNAME = 'TSupplyComplianceGrid.LanguageHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSupplyComplianceGrid.Resize;
const OPNAME = 'TSupplyComplianceGrid.Resize';
var
  LWidth : integer;
begin
  inherited Resize;
  try
    LWidth := (Self.ClientWidth - (2 * Self.ColCount)) div Self.ColCount+1;
    Self.DefaultColWidth := LWidth;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSupplyComplianceGrid.SetGridType(AGridType: TGridType);
const OPNAME = 'TSupplyComplianceGrid.SetGridType';
begin
  try
    FGridType := AGridType;
    Self.ColCount := Self.ColCount + Integer(FGridType);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSupplyComplianceGrid.StudyHasChanged: boolean;
const OPNAME = 'TSupplyComplianceGrid.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{TSupplyComplianceAggregateGrid}

procedure TSupplyComplianceAggregateGrid.ClearGrid;
const OPNAME = 'TSupplyComplianceAggregateGrid.ClearGrid';
var
  LIndex : integer;
begin
  try
    for LIndex := 0 to Self.ColCount -1 do
      Self.Cols[LIndex].Clear;
    Self.RowCount := 3;
    Self.ColCount := 13;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSupplyComplianceAggregateGrid.CreateMemberObjects;
const OPNAME = 'TSupplyComplianceAggregateGrid.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FAlignment    := taRightJustify;
    FCellColoring := TStringList.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSupplyComplianceAggregateGrid.DestroyMemberObjects;
const OPNAME = 'TSupplyComplianceAggregateGrid.DestroyMemberObjects';
begin
  try
    FCellColoring.Free;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSupplyComplianceAggregateGrid.DrawCell(ACol, ARow: Integer; Rect: TRect;State: TGridDrawState);
const OPNAME = 'TSupplyComplianceAggregateGrid.DrawCell';
var
  LTxtRect: TRect;
  LStr: string;
  LTxtHeight,
  LColor : TColor;
begin
  inherited DrawCell(ACol, ARow, Rect, State);
  try
    if (ARow in [0, 1, 2]) then
    begin
      Self.Canvas.Brush.Color   := clBtnFace;
      Self.Brush.Style          := bsSolid;
      Self.Canvas.Pen.Style     := psSolid;
      Self.Canvas.FillRect(Rect);

      if (ACol = 0) then
      begin
        Rect.Top                := 0;
        Rect.Bottom             := Rect.Bottom + (Self.DefaultRowHeight * ((3 - ARow) - 1));

        Self.Canvas.Brush.Color := clBtnFace;
        Self.Brush.Style        := bsSolid;
        Self.Canvas.Pen.Style   := psSolid;
        Self.Canvas.FillRect(Rect);

        LtxtRect                := Rect;
        LTxtRect.Left           := Rect.Left + 8;
        LTxtRect.Top            := 18;

        LStr                    := Self.Cells[ACol, 0];

        Self.Canvas.Font.Color  := clInfoText;
        Self.Canvas.Font.Name   := Self.Font.Name;
        Self.Canvas.Font.Size   := Self.Font.Size;
        Self.Canvas.Font.Style  := [fsBold];
        DrawText(Self.Canvas.Handle,
                 PChar(LStr),
                 length(LStr), LTxtRect,
                 DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);
      end
      else
      if (ACol in [1, 2]) then
      begin
        if (ARow = 0) then
        begin
          Rect.Left               := (Rect.Left - (Self.DefaultColWidth * (ACol - 1))) - (ACol - 1);
          Rect.Right              :=  Rect.Left + (Self.DefaultColWidth * 2);

          Self.Canvas.Brush.Color := clBtnFace;
          Self.Brush.Style        := bsSolid;
          Self.Canvas.Pen.Style   := psSolid;
          Self.Canvas.FillRect(Rect);

          Lstr                    := Self.Cells[1, ARow];
          LTxtHeight              := Self.Canvas.TextHeight(LStr);

          LtxtRect                := Rect;
          LTxtRect.Left           := LTxtRect.Left + 2;
          LTxtRect.Top            := (LtxtRect.Bottom div 2) - (LTxtHeight div 2);

          Self.Canvas.Font.Color  := clInfoText;
          Self.Canvas.Font.Name   := Self.Font.Name;
          Self.Canvas.Font.Size   := Self.Font.Size;
          Self.Canvas.Font.Style  := [fsBold];
          DrawText(Self.Canvas.Handle,
                   PChar(LStr),
                   length(LStr), LTxtRect,
                   DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);
        end;
        if (ARow in [1, 2]) then
        begin
          Rect.Top                := Rect.Top - ((Self.DefaultRowHeight * (ARow - 1)) - (ARow - 1));
          Rect.Bottom             := Rect.Bottom + ((Self.DefaultRowHeight * (2 - ARow)) -(2 - ARow));

          Self.Canvas.Brush.Color := clBtnFace;
          Self.Brush.Style        := bsSolid;
          Self.Canvas.Pen.Style   := psSolid;
          Self.Canvas.FillRect(Rect);

          LtxtRect                := Rect;
          LTxtRect.Left           := Rect.Left + 4;
          LTxtRect.Top            := LTxtRect.Top + 8;

          LStr                    := Self.Cells[ACol, 1];

          Self.Canvas.Font.Color  := clInfoText;
          Self.Canvas.Font.Name   := Self.Font.Name;
          Self.Canvas.Font.Size   := Self.Font.Size;
          Self.Canvas.Font.Style  := [fsBold];
          DrawText(Self.Canvas.Handle,
                   PChar(LStr),
                   length(LStr), LTxtRect,
                   DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);
        end;
      end
      else
      if (ACol in [3..7]) then
      begin
        if (ARow = 0) then
        begin
          Rect.Left               := (Rect.Left - (Self.DefaultColWidth * (ACol - 3))) - (ACol - 3);
          Rect.Right              :=  Rect.Left + (Self.DefaultColWidth * 5);

          Self.Canvas.Brush.Color := clBtnFace;
          Self.Brush.Style        := bsSolid;
          Self.Canvas.Pen.Style   := psSolid;
          Self.Canvas.FillRect(Rect);

          Lstr                    := Self.Cells[3, ARow];
          LTxtHeight              := Self.Canvas.TextHeight(LStr);

          LtxtRect                := Rect;
          LTxtRect.Left           := LTxtRect.Left + 12;
          LTxtRect.Top            := (LtxtRect.Bottom div 2) - (LTxtHeight div 2);

          Self.Canvas.Font.Color  := clInfoText;
          Self.Canvas.Font.Name   := Self.Font.Name;
          Self.Canvas.Font.Size   := Self.Font.Size;
          Self.Canvas.Font.Style  := [fsBold];
          DrawText(Self.Canvas.Handle,
                   PChar(LStr),
                   length(LStr), LTxtRect,
                   DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);
        end;
        if (ARow in [1, 2]) then
        begin
          Rect.Top                := Rect.Top - ((Self.DefaultRowHeight * (ARow - 1)) - (ARow - 1));
          Rect.Bottom             := Rect.Bottom + ((Self.DefaultRowHeight * (2 - ARow)) -(2 - ARow));

          Self.Canvas.Brush.Color := clBtnFace;
          Self.Brush.Style        := bsSolid;
          Self.Canvas.Pen.Style   := psSolid;
          Self.Canvas.FillRect(Rect);

          LtxtRect                := Rect;
          LTxtRect.Left           := LTxtRect.Left + 4;
          LTxtRect.Top            := LTxtRect.Top + 2;

          LStr                    := Self.Cells[ACol, 1];

          Self.Canvas.Font.Color  := clInfoText;
          Self.Canvas.Font.Name   := Self.Font.Name;
          Self.Canvas.Font.Size   := Self.Font.Size;
          Self.Canvas.Font.Style  := [fsBold];
          DrawText(Self.Canvas.Handle,
                   PChar(LStr),
                   length(LStr), LTxtRect,
                   DT_SINGLELINE or DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);
        end;
      end
      else
      if (ACol in [8..12]) then
      begin
        if (ARow = 0) then
        begin
          Rect.Left               := (Rect.Left - (Self.DefaultColWidth * (ACol - 8))) - (ACol - 8);
          Rect.Right              :=  Rect.Left + (Self.DefaultColWidth * 5);

          Self.Canvas.Brush.Color := clBtnFace;
          Self.Brush.Style        := bsSolid;
          Self.Canvas.Pen.Style   := psSolid;
          Self.Canvas.FillRect(Rect);

          Lstr                    := Self.Cells[8, ARow];
          LTxtHeight              := Self.Canvas.TextHeight(LStr);

          LtxtRect                := Rect;
          LTxtRect.Left           := LTxtRect.Left + 12;
          LTxtRect.Top            := (LtxtRect.Bottom div 2) - (LTxtHeight div 2);

          Self.Canvas.Font.Color  := clInfoText;
          Self.Canvas.Font.Name   := Self.Font.Name;
          Self.Canvas.Font.Size   := Self.Font.Size;
          Self.Canvas.Font.Style  := [fsBold];
          DrawText(Self.Canvas.Handle,
                   PChar(LStr),
                   length(LStr), LTxtRect,
                   DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);
        end;
        if (ARow in [1, 2]) then
        begin
          Rect.Top                := Rect.Top - ((Self.DefaultRowHeight * (ARow - 1)) - (ARow - 1));
          Rect.Bottom             := Rect.Bottom + ((Self.DefaultRowHeight * (2 - ARow)) -(2 - ARow));

          Self.Canvas.Brush.Color := clBtnFace;
          Self.Brush.Style        := bsSolid;
          Self.Canvas.Pen.Style   := psSolid;
          Self.Canvas.FillRect(Rect);

          LtxtRect                := Rect;
          LTxtRect.Left           := LTxtRect.Left + 4;
          LTxtRect.Top            := LTxtRect.Top + 2;

          LStr                    := Self.Cells[ACol, 1];

          Self.Canvas.Font.Color  := clInfoText;
          Self.Canvas.Font.Name   := Self.Font.Name;
          Self.Canvas.Font.Size   := Self.Font.Size;
          Self.Canvas.Font.Style  := [fsBold];
          DrawText(Self.Canvas.Handle,
                   PChar(LStr),
                   length(LStr), LTxtRect,
                   DT_SINGLELINE or DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);
        end;
      end;
    end
    else
    begin
      if IsColored(ACol, ARow) then
        LColor := FCellColor
      else
        LColor := clBlack;

      Self.Canvas.Brush.Color := clWindow;
      Self.Brush.Style        := bsSolid;
      Self.Canvas.Pen.Style   := psSolid;
      Self.Canvas.FillRect(Rect);

      LtxtRect                := Rect;
      LTxtRect.Left           := Rect.Left + 4;
      LTxtRect.Top            := Rect.Top + 2;

      LStr                    := Self.Cells[ACol, ARow];

      Self.Canvas.Font.Color  := clInfoText;
      Self.Canvas.Font.Name   := Self.Font.Name;
      Self.Canvas.Font.Size   := Self.Font.Size;
      Self.Canvas.Font.Color  := LColor;
      Self.Canvas.Font.Style  := [fsBold];
      inherited  DrawCell(ACol, ARow,Rect,State);

      {DrawText(Self.Canvas.Handle,
               PChar(LStr),
               length(LStr), LTxtRect,
               DT_SINGLELINE or DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);
      }
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSupplyComplianceAggregateGrid.Initialise: boolean;
const OPNAME = 'TSupplyComplianceAggregateGrid.Initialise';
begin
  Result := False;
  try
    Self.RowCount   := 3;
    Self.ColCount   := 13;
    Self.FixedCols  := 0;
    Self.FixedRows  := 0;
    Self.Color      := clWindow;
    Self.EditorMode := False;
    Self.Options    := [goFixedVertLine,goFixedHorzLine,goVertLine,goHorzLine];
    Self.Height     := 240;
    Self.ScrollBars := ssVertical;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSupplyComplianceAggregateGrid.IsColored(ACol, ARow: integer): boolean;
const OPNAME = 'TSupplyComplianceAggregateGrid.IsColored';
var
  LStrings : TStringList;
begin
  Result := False;
  try
    if (FCellColoring.Count > ARow) then
    begin
      LStrings             := TStringList.Create;
      try
        LStrings.CommaText := FCellColoring[ARow];
        if (LStrings.Count > ACol) then
          Result           := (LStrings[ACol] = 'Y');
      finally
        LStrings.Clear;
        LStrings.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSupplyComplianceAggregateGrid.LanguageHasChanged: boolean;
const OPNAME = 'TSupplyComplianceAggregateGrid.LanguageHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSupplyComplianceAggregateGrid.Resize;
const OPNAME = 'TSupplyComplianceAggregateGrid.Resize';
var
  LWidth : integer;
begin
  inherited Resize;
  try
    LWidth := (Self.ClientWidth - (2 * Self.ColCount)) div Self.ColCount+1;
    Self.DefaultColWidth := LWidth;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSupplyComplianceAggregateGrid.StudyHasChanged: boolean;
const OPNAME = 'TSupplyComplianceAggregateGrid.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TSelector }

procedure TSelector.CreateMemberObjects;
const OPNAME = 'TSelector.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FEditable         := True;

    Self.BevelOuter   := bvNone;
    OnSelectionChange := nil;

    FName             := TLabel.Create(Self);
    FName.AutoSize    := True;
    FSelector         := TSpinEdit.Create(Self);
    FSelector.OnKeyPress  := SelectorKeyPress;
    FSelector.OnKeyDown   := SelectorKeyDown;

    FName.Parent      := Self;
    FSelector.Parent  := Self;

    FName.Visible     := True;
    FSelector.Visible := True;

    FName.Layout      := tlCenter;
    FName.Alignment   := taLeftJustify;

    FSelector.OnChange  := OnSelectorChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSelector.Resize;
const OPNAME = 'TSelector.Resize';
begin
  inherited;
  try
    FName.Align      := alNone;
    FSelector.Align  := alNone;

    FSelector.Align  := alLeft;
    FName.Align      := alLeft;
    CenterControls;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSelector.CenterControls;
const OPNAME = 'TSelector.CenterControls';
var
  LWidth,
  LLeft: integer;
begin
  try
    LLeft            := FSelector.Left;
    LWidth           := FSelector.Width;
    FName.Align      := alNone;
    FSelector.Align  := alNone;

    FName.Height     := 22;
    FSelector.Height := 22;

    FName.Top        := (Self.Height - FName.Height) div 2;
    FSelector.Top    := FName.Top;
    FSelector.Left   := LLeft;
    FSelector.Width  := LWidth;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSelector.OnSelectorChanged(Sender: TObject);
const OPNAME = 'TSelector.OnSelectorChanged';
var
  LValue : integer;
begin
  try
    LValue := StrToIntDef(FSelector.Text,NullInteger);
    if(LValue <> NullInteger) then
      if Assigned(OnSelectionChange) then
         OnSelectionChange(StrToInt(FSelector.Text));

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSelector.SelectorKeyPress(Sender: TObject; var Key: Char);
const OPNAME = 'TSelector.SelectorKeyPress';
begin
  try
    if not FEditable then
    begin
      Key := Char(0);
      Beep;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSelector.SelectorKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
const OPNAME = 'TSelector.SelectorKeyDown';
begin
  try
    if not FEditable then
    begin
      if (Key = VK_DELETE) then
      begin
        Key := 0;
        Beep;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSelector.Populate(AMinValue,AMaxValue: integer): boolean;
const OPNAME = 'TSelector.Populate';
begin
  Result := False;
  try
    FSelector.Increment := 1;
    FSelector.MaxValue       := AMaxValue;
    FSelector.MinValue       := AMinValue;
    FSelector.Enabled        := (AMinValue <> AMaxValue);
    FSelector.Value          := AMinValue;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSelector.Select(AIndex: integer);
const OPNAME = 'TSelector.Select';
begin
  try
    if(AIndex >= FSelector.MinValue) and (AIndex <= FSelector.MaxValue) then
      FSelector.Value := AIndex;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSelector.SetCaption(ACaption: string);
const OPNAME = 'TSelector.SetCaption';
begin
  try
    FName.Caption := ACaption;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSelector.SetEnabled(AEnabled: boolean);
const OPNAME = 'TSelector.SetEnabled';
begin
  inherited SetEnabled(AEnabled);
  try
    FSelector.Enabled := AEnabled and(FSelector.MaxValue <> FSelector.MinValue);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSelector.OnRequestPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
const OPNAME = 'TSelector.OnRequestPopup';
begin
  try
    Handled := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TMonthSelectorNavigator }

function TMonthSelectorNavigator.CreateComboBox(AAlign: TAlign;AAppModules: TAppModules): TComboBox;
const OPNAME = 'TMonthSelectorNavigator.CreateComboBox';
begin
  Result := nil;
  try
    Result := TComboBox.Create(Self{, AAppModules});
    Result.Parent    := Self;
    Result.TabStop   := True;
    Result.Top       := 0;
    Result.Width     := 21;
    Result.Style     := csDropDownList;
    Result.Align     := AAlign;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMonthSelectorNavigator.CreateMemberObjects;
const OPNAME = 'TMonthSelectorNavigator.CreateMemberObjects';
begin
  inherited;
  try
    FPreviousSignificantRecord  := CreateButton('OutputReviewPrevSigRecord', alLeft, FAppModules);
    FNextSignificantRecord      := CreateButton('OutputReviewNextSigRecord', alRight, FAppModules);

    FPreviousSignificantRecord.Enabled := False;
    FNextSignificantRecord.Enabled     := False;

    FPreviousSignificantRecord.OnClick := OnPreviousSignificantRecordClck;
    FNextSignificantRecord.OnClick     := OnNextSignificantRecordClck;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMonthSelectorNavigator.OnFirstRecordClck(Sender: TObject);
const OPNAME = 'TMonthSelectorNavigator.OnFirstRecordClck';
begin
  try
    FCurrentRecord := 1;
    if(FRecordCount < 1) then
       FCurrentRecord := 0;
    DisplayCurrentRecord;
    SetButtonsState;
    if Assigned(FOnSelectionChange) then
      FOnSelectionChange(Self);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMonthSelectorNavigator.OnLastRecordClck(Sender: TObject);
const OPNAME = 'TMonthSelectorNavigator.OnLastRecordClck';
begin
  try
    FCurrentRecord := FRecordCount;
    DisplayCurrentRecord;
    SetButtonsState;
    if Assigned(FOnSelectionChange) then
      FOnSelectionChange(Self);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMonthSelectorNavigator.OnNextSignificantRecordClck(Sender: TObject);
const OPNAME = 'TMonthSelectorNavigator.OnNextSignificantRecordClck';
begin
  try
    DisplayCurrentRecord;
    SetButtonsState;
    if Assigned(FOnSelectionChange) then
      FOnSelectionChange(Self);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMonthSelectorNavigator.OnPreviousSignificantRecordClck(Sender: TObject);
const OPNAME = 'TMonthSelectorNavigator.OnPreviousSignificantRecordClck';
begin
  try
    DisplayCurrentRecord;
    SetButtonsState;
    if Assigned(FOnSelectionChange) then
      FOnSelectionChange(Self);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMonthSelectorNavigator.PopulateMonthSelector: boolean;
const OPNAME = 'TMonthSelectorNavigator.PopulateMonthSelector';
var
  LIndex : integer;
begin
  Result := FALSE;
  try
    if MonthCount  > 0 then
    begin
      for LIndex := 1 to FRecordCount do
      begin
        FCbxSelector.Items.Add(IntToStr(LIndex));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMonthSelectorNavigator.Resize;
const OPNAME = 'TMonthSelectorNavigator.Resize';
var
  lBtnWidth : integer;
begin
  inherited;
  try
    lBtnWidth                   := 30;
    FFirstRecord.Left           := 0;
    FFirstRecord.Width          := lBtnWidth;
    FFirstRecord.Align          := alLeft;

    FPreviousSignificantRecord.Left  := lBtnWidth;
    FPreviousSignificantRecord.Width := lBtnWidth;
    FPreviousSignificantRecord.Align := alLeft;

    FPreviousRecord.Left        := (2*lBtnWidth);
    FPreviousRecord.Width       := lBtnWidth;
    FPreviousRecord.Align       := alLeft;

    FCbxSelector.Left           := (3*lBtnWidth);
    FCbxSelector.Width          := 50;
    FCbxSelector.Align          := alLeft;

    FLastRecord.Left            := Self.Width - (1*lBtnWidth);
    FLastRecord.Width           := lBtnWidth;
    FLastRecord.Align           := alRight;

    FNextSignificantRecord.Left   := Self.Width - (2*lBtnWidth);
    FNextSignificantRecord.Width  := lBtnWidth;
    FNextSignificantRecord.Align  := alRight;

    FNextRecord.Left            := Self.Width - (3*lBtnWidth);
    FNextRecord.Width           := lBtnWidth;
    FNextRecord.Align           := alRight;

    FCurrentRecordDisplay.Left  := 50 + (3*lBtnWidth);
    FCurrentRecordDisplay.Width := 50;
    FCurrentRecordDisplay.Align := alClient;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMonthSelectorNavigator.SetButtonsState;
const OPNAME = 'TMonthSelectorNavigator.SetButtonsState';
begin
  inherited;
  try
    FPreviousSignificantRecord.Enabled        := False;
    FNextSignificantRecord.Enabled            := False;
    if(FRecordCount > 1) then
    begin
      if(FCurrentRecord = FRecordCount) and (FRecordCount > 0) then
      begin
        FPreviousSignificantRecord.Enabled := True;
      end
      else
      if(FCurrentRecord = 1) then
      begin
        FNextSignificantRecord.Enabled     := True;
      end
      else
      begin
        FPreviousSignificantRecord.Enabled := False;
        FNextSignificantRecord.Enabled     := False;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMonthSelectorNavigator.SetEnabled(Value: Boolean);
const OPNAME = 'TMonthSelectorNavigator.SetEnabled';
begin
  inherited;
  try
    if Value then
      SetButtonsState
    else
    begin
      FPreviousSignificantRecord.Enabled := False;
      FNextSignificantRecord.Enabled := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMonthSelectorNavigator.SetRecordCount(AValue: integer);
const OPNAME = 'TMonthSelectorNavigator.SetRecordCount';
begin
  try
    FRecordCount := AValue;
    DisplayCurrentRecord;
    PopulateMonthSelector;
    SetButtonsState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TSequenceSelectorNavigator }

function TSequenceSelectorNavigator.CreateComboBox(AAlign: TAlign;AAppModules: TAppModules): TComboBox;
const OPNAME = 'TSequenceSelectorNavigator.CreateComboBox';
begin
  Result := nil;
  try
    Result := TComboBox.Create(Self{, AAppModules});
    Result.Parent    := Self;
    Result.TabStop   := True;
    Result.Top       := 0;
    Result.Width     := 21;
    Result.Style     := csDropDownList;
    Result.Align     := AAlign;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSequenceSelectorNavigator.PopulateSequenceSelector: boolean;
const OPNAME = 'TSequenceSelectorNavigator.PopulateSequenceSelector';
var
  LIndex : integer;
begin
  Result := FALSE;
  try
    if SequenceCount  > 0 then
    begin
      for LIndex := 1 to FRecordCount do
      begin
        FCbxSelector.Items.Add(IntToStr(LIndex));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSequenceSelectorNavigator.SetButtonsState;
const OPNAME = 'TSequenceSelectorNavigator.SetButtonsState';
begin
  inherited;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSequenceSelectorNavigator.SetRecordCount(AValue: integer);
const OPNAME = 'TSequenceSelectorNavigator.SetRecordCount';
begin
  try
    FRecordCount := AValue;
    DisplayCurrentRecord;
    PopulateSequenceSelector;
    SetButtonsState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TLoadCaseSelectorNavigator }

function TLoadCaseSelectorNavigator.CreateComboBox(AAlign: TAlign;AAppModules: TAppModules): TComboBox;
const OPNAME = 'TLoadCaseSelectorNavigator.CreateComboBox';
begin
  Result := nil;
  try
    Result := TComboBox.Create(Self{, AAppModules});
    Result.Parent    := Self;
    Result.TabStop   := True;
    Result.Top       := 0;
    Result.Width     := 21;
    Result.Style     := csDropDownList;
    Result.Align     := AAlign;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLoadCaseSelectorNavigator.PopulateLoadCaseSelector: boolean;
const OPNAME = 'TLoadCaseSelectorNavigator.PopulateLoadCaseSelector';
var
  LIndex : integer;
begin
  Result := FALSE;
  try
    if LoadCaseCount  > 0 then
    begin
      for LIndex := 1 to FRecordCount do
      begin
        FCbxSelector.Items.Add(IntToStr(LIndex));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLoadCaseSelectorNavigator.SetRecordCount(AValue: integer);
const OPNAME = 'TLoadCaseSelectorNavigator.SetRecordCount';
begin
  try
    FRecordCount := AValue;
    DisplayCurrentRecord;
    PopulateLoadCaseSelector;
    SetButtonsState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TIFRStatsComplianceGrid }

procedure TIFRStatsComplianceGrid.ClearGrid;
const OPNAME = 'TIFRStatsComplianceGrid.ClearGrid';
var
  LIndex: integer;
begin
  try
    for LIndex := 0 to Self.ColCount -1 do
      Self.Cols[LIndex].Clear;
    Self.ColCount := 49;
    Self.RowCount := 2;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRStatsComplianceGrid.CreateMemberObjects;
const OPNAME = 'TIFRStatsComplianceGrid.CreateMemberObjects';
begin
  try
    //FHeading     := '';
    FAlignment     := taRightJustify;
    FCellColoring  := TStringList.Create;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIFRStatsComplianceGrid.DestroyMemberObjects;
const OPNAME = 'TIFRStatsComplianceGrid.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRStatsComplianceGrid.DrawCell(ACol, ARow: Integer;Rect: TRect; State: TGridDrawState);
const OPNAME = 'TIFRStatsComplianceGrid.DrawCell';
var
  LTxtRect: TRect;
  LStr: string;
  LColor : TColor;
begin
  inherited DrawCell(ACol, ARow, Rect, State);
  try
    if (ARow = 0) and (ACol = 0)then
    begin
      Self.Canvas.Brush.Color := clBtnFace;
      Self.Brush.Style        := bsSolid;
      Self.Canvas.Pen.Style   := psClear;
      Self.Canvas.FillRect(Rect);

      LtxtRect                := Rect;
      LtxtRect.Left           := 0;
      LtxtRect.Right          := Self.DefaultColWidth+(Self.DefaultColWidth*4);
      Lstr                    := Self.Cells[ACol,0];
      Self.Canvas.Font.Color  := clInfoText;
      Self.Canvas.Font.Name   := Self.Font.Name;
      Self.Canvas.Font.Size   := Self.Font.Size;
      Self.Canvas.Font.Style  := [fsBold];
      DrawText(Self.Canvas.Handle,
               PChar(LStr),
               length(LStr), LTxtRect,
               DT_SINGLELINE or DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);
    end
    else
    if (ARow = 0) and (ACol > 0)then
    begin
      Self.Canvas.Brush.Color := clBtnFace;
      Self.Brush.Style        := bsSolid;
      Self.Canvas.Pen.Style   := psClear;
      Self.Canvas.FillRect(Rect);

      LtxtRect                := Rect;
      LtxtRect.Left           := Rect.Left;
      LtxtRect.Right          := Rect.Left + (Self.DefaultColWidth * 4);
      Lstr                    := Self.Cells[ACol,0];
      Self.Canvas.Font.Color  := clInfoText;
      Self.Canvas.Font.Name   := Self.Font.Name;
      Self.Canvas.Font.Size   := Self.Font.Size;
      Self.Canvas.Font.Style  := [fsBold];
      DrawText(Self.Canvas.Handle,
               PChar(LStr),
               length(LStr), LTxtRect,
               DT_SINGLELINE or DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);

    end
    else
    if (ARow = 1) then
    begin
      Self.Canvas.Brush.Color := clBtnFace;
      Self.Brush.Style        := bsSolid;
      Self.Canvas.Pen.Style   := psSolid;
      Self.Canvas.FillRect(Rect);

      LtxtRect                := Rect;
      LTxtRect.Left           := Rect.Left + 4;
      LTxtRect.Top            := Rect.Top + 2;

      LStr                    := Self.Cells[ACol, ARow];

      Self.Canvas.Font.Color  := clInfoText;
      Self.Canvas.Font.Name   := Self.Font.Name;
      Self.Canvas.Font.Size   := Self.Font.Size;
      Self.Canvas.Font.Style  := [fsBold];
      DrawText(Self.Canvas.Handle,
               PChar(LStr),
               length(LStr), LTxtRect,
               DT_SINGLELINE or DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);
    end
    else
    begin
      if IsColored(ACol, ARow) then
        LColor := FCellColor
      else
        LColor := clBlack;

      Self.Canvas.Brush.Color := clWindow;
      Self.Brush.Style        := bsSolid;
      Self.Canvas.Pen.Style   := psSolid;
      Self.Canvas.FillRect(Rect);

      LtxtRect                := Rect;
      LTxtRect.Left           := Rect.Left + 4;
      LTxtRect.Top            := Rect.Top + 2;

      LStr                    := Self.Cells[ACol, ARow];

      Self.Canvas.Font.Color  := clInfoText;
      Self.Canvas.Font.Name   := Self.Font.Name;
      Self.Canvas.Font.Size   := Self.Font.Size;
      Self.Canvas.Font.Color  := LColor;
      Self.Canvas.Font.Style  := [fsBold];
      inherited  DrawCell(ACol, ARow,Rect,State);

      {DrawText(Self.Canvas.Handle,
               PChar(LStr),
               Length(LStr), LTxtRect,
               DT_SINGLELINE or DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);}
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRStatsComplianceGrid.Initialise: boolean;
const OPNAME = 'TIFRStatsComplianceGrid.Initialise';
begin
  Result := inherited Initialise;
  try
    if Result then
    begin
      Self.ColCount   := 49;
      Self.RowCount   := 2;
      Self.FixedCols  := 0;
      Self.FixedRows  := 0;
      Self.Color      := clWindow;
      Self.EditorMode := False;
      Self.Options    := [goFixedVertLine,goFixedHorzLine,goVertLine,goHorzLine];
      Self.Height     := 240;
      Self.ScrollBars := ssBoth;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRStatsComplianceGrid.IsColored(ACol, ARow: integer): boolean;
const OPNAME = 'TIFRStatsComplianceGrid.IsColored';
var
  LStrings : TStringList;
begin
  Result := False;
  try
    if (FCellColoring.Count > ARow) then
    begin
      LStrings             := TStringList.Create;
      try
        LStrings.CommaText := FCellColoring[ARow];
        if (LStrings.Count > ACol) then
          Result           := (UpperCase(LStrings[ACol]) = 'Y');
      finally
        LStrings.Clear;
        LStrings.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRStatsComplianceGrid.LanguageHasChanged: boolean;
const OPNAME = 'TIFRStatsComplianceGrid.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRStatsComplianceGrid.Resize;
const OPNAME = 'TIFRStatsComplianceGrid.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIFRStatsComplianceGrid.StudyHasChanged: boolean;
const OPNAME = 'TIFRStatsComplianceGrid.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
