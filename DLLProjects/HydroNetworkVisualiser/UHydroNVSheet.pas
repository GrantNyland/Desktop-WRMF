{******************************************************************************}
{* UNIT : Contains the class UHydroNVSheet.                                   *}
{*        (Network Visualiser for Hydrology model)                            *}
{******************************************************************************}

unit UHydroNVSheet;

interface

uses
  // Delphi
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,

  // DWAF
  UAbstractObject,
  UViewDataItem,
  UTreeViewTabSheet,
  UAbstractGridData,
  UGridActionObject,
  UAbstractComponent,
//  UNetworkVisualiserData,
  UHelpContexts,
  UHydroNVMenuItemManager;

type
  THydroNVSheet = class(TTreeViewTabSheet)
  protected
    FHintDisplay        : TStatusBar;
//    FDrawingData        : TDrawingData;
    procedure CreateMemberObjects; override;
    function GetToolBar: TAbstractToolBar; override;
    function GetMenuItemManager: THydroNVMenuItemManager;
    procedure AssignHelpContext; override;
  public
    procedure DoOnHint; override;
    function CanCopyToCLipboard: boolean; override;
    function CanExport: boolean; override;
    function CanPrint: boolean; override;
    procedure DoCopyToCLipboard; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function Initialise: boolean; override;
    procedure SetMenuVisible(AVisible: boolean); override;
    property MenuItemManager: THydroNVMenuItemManager read GetMenuItemManager;
//    property DrawingData: TDrawingData read FDrawingData write FDrawingData;
    property HintDisplay: TStatusBar read FHintDisplay;
  end;

implementation

uses
  // Delphi
  SysUtils,
  Variants,
  VCL.Graphics,

  // DWAF
  VoaimsCom_TLB,
  UErrorHandlingOperations;

procedure THydroNVSheet.CreateMemberObjects;
const OPNAME = 'THydroNVSheet.CreateMemberObjects';
var
  LStatusPanel: TStatusPanel;
begin
  try
    inherited CreateMemberObjects;

    FHintDisplay := TStatusBar.Create(Self);
    FHintDisplay.Parent := Self;
    FHintDisplay.Align := alBottom;
    FHintDisplay.Font.Color := clRed;
    LStatusPanel := FHintDisplay.Panels.Add;
    LStatusPanel.Width := 1;
    FHintDisplay.Panels.Add;
    Self.Color  := clBtnFace;

    // Set ancestor properties.
    ShowHint := False;
    ParentShowHint := False;

    // Set defaults.
    FTabCaptionKey := 'NetworkVisualiser';

    // Create the menu item manager.
    FMenuItemManager := THydroNVMenuItemManager.Create(FAppModules);

    FTreeView.SortType := stText;
    FTreeView.ReadOnly := False;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure THydroNVSheet.AssignHelpContext;
const OPNAME = 'THydroNVSheet.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,      HC_BuildingASystemNetwork);
    SetControlHelpContext(FTreeView, HC_BuildingASystemNetwork);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVSheet.Initialise: boolean;
const OPNAME = 'THydroNVSheet.Initialise';
begin
  Result := inherited Initialise;
  try
    FMenuItemManager.Initialise;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVSheet.SetMenuVisible(AVisible: boolean);
const OPNAME = 'THydroNVSheet.SetMenuVisible';
begin
  inherited;
  try
    Self.TreeView.OnChange(Self, TreeView.Selected);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'THydroNVSheet.GetToolBar';
begin
  Result := nil;
  try
    Result := MenuItemManager.ToolBar;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVSheet.LanguageHasChanged: boolean;
const OPNAME = 'THydroNVSheet.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVSheet.GetMenuItemManager: THydroNVMenuItemManager;
const OPNAME = 'THydroNVSheet.GetMenuItemManager';
begin
  Result := nil;
  try
    Result := THydroNVMenuItemManager(FMenuItemManager);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVSheet.DoOnHint;
const OPNAME = 'THydroNVSheet.DoOnHint';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVSheet.StudyHasChanged: boolean;
const OPNAME = 'THydroNVSheet.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    MenuItemManager.StudyHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVSheet.CanCopyToClipboard: boolean;
const OPNAME = 'THydroNVSheet.CanCopyToClipboard';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVSheet.CanExport: boolean;
const OPNAME = 'THydroNVSheet.CanExport';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVSheet.CanPrint: boolean;
const OPNAME = 'THydroNVSheet.CanPrint';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVSheet.DoCopyToClipboard;
const OPNAME = 'THydroNVSheet.DoCopyToClipboard';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVSheet.DoExport(AFileName: string = '');
const OPNAME = 'THydroNVSheet.DoExport';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVSheet.DoPrint;
const OPNAME = 'THydroNVSheet.DoPrint';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVSheet.StudyDataHasChanged(AContext: TChangeContext;  AFieldName, AOldValue, ANewValue: string): boolean;
const OPNAME = 'THydroNVSheet.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
