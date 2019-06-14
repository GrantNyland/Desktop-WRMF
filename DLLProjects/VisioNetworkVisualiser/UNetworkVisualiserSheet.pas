//
//
//  UNIT      : Contains TNetworkVisualiserSheet Class
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 2005/02/24
//  COPYRIGHT : Copyright © 2005 DWAF
//
//
unit UNetworkVisualiserSheet;

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
  UNetworkVisualiserData,
  UHelpContexts,
  UNetworkVisualiserMenuItemManager;

type
  TNetworkVisualiserSheet = class(TTreeViewTabSheet)
  protected
    FHintDisplay        : TStatusBar;
    FDrawingData        : TDrawingData;
    procedure CreateMemberObjects; override;
    function GetToolBar: TAbstractToolBar; override;
    function GetMenuItemManager: TNetworkVisualiserMenuItemManager;
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
    property MenuItemManager: TNetworkVisualiserMenuItemManager read GetMenuItemManager;
    property DrawingData: TDrawingData read FDrawingData write FDrawingData;
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

procedure TNetworkVisualiserSheet.CreateMemberObjects;
const OPNAME = 'TNetworkVisualiserSheet.CreateMemberObjects';
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
    FMenuItemManager := TNetworkVisualiserMenuItemManager.Create(FAppModules);

    FTreeView.SortType := stText;
    FTreeView.ReadOnly := False;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TNetworkVisualiserSheet.AssignHelpContext;
const OPNAME = 'TNetworkVisualiserSheet.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,      HC_BuildingASystemNetwork);
    SetControlHelpContext(FTreeView, HC_BuildingASystemNetwork);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkVisualiserSheet.Initialise: boolean;
const OPNAME = 'TNetworkVisualiserSheet.Initialise';
begin
  Result := inherited Initialise;
  try
    FMenuItemManager.Initialise;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TNetworkVisualiserSheet.SetMenuVisible(AVisible: boolean);
const OPNAME = 'TNetworkVisualiserSheet.SetMenuVisible';
begin
  inherited;
  try
    Self.TreeView.OnChange(Self, TreeView.Selected);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TNetworkVisualiserSheet.GetToolBar';
begin
  Result := nil;
  try
    Result := MenuItemManager.ToolBar;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserSheet.LanguageHasChanged: boolean;
const OPNAME = 'TNetworkVisualiserSheet.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserSheet.GetMenuItemManager: TNetworkVisualiserMenuItemManager;
const OPNAME = 'TNetworkVisualiserSheet.GetMenuItemManager';
begin
  Result := nil;
  try
    Result := TNetworkVisualiserMenuItemManager(FMenuItemManager);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserSheet.DoOnHint;
const OPNAME = 'TNetworkVisualiserSheet.DoOnHint';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserSheet.StudyHasChanged: boolean;
const OPNAME = 'TNetworkVisualiserSheet.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    MenuItemManager.StudyHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserSheet.CanCopyToClipboard: boolean;
const OPNAME = 'TNetworkVisualiserSheet.CanCopyToClipboard';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserSheet.CanExport: boolean;
const OPNAME = 'TNetworkVisualiserSheet.CanExport';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserSheet.CanPrint: boolean;
const OPNAME = 'TNetworkVisualiserSheet.CanPrint';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserSheet.DoCopyToClipboard;
const OPNAME = 'TNetworkVisualiserSheet.DoCopyToClipboard';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserSheet.DoExport(AFileName: string = '');
const OPNAME = 'TNetworkVisualiserSheet.DoExport';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TNetworkVisualiserSheet.DoPrint;
const OPNAME = 'TNetworkVisualiserSheet.DoPrint';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkVisualiserSheet.StudyDataHasChanged(AContext: TChangeContext;  AFieldName, AOldValue, ANewValue: string): boolean;
const OPNAME = 'TNetworkVisualiserSheet.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;
end.
