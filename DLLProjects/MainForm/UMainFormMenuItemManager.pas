//
//
//  UNIT      : Contains TMainFormMenuItemManager Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/04
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UMainFormMenuItemManager;

interface

uses
  Vcl.Controls,
  UAbstractObject,
  UToolBar,
  UMenuItemManager;

type
  TMainFormMenuItemManager = class(TMenuItemManager, IMainFormMenuItemManager)
  protected
    FToolBar: TToolBar;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure AddMenuItems; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;

    // Implementations for IMainFormMenuItemManager.
    function IsToolBarVisible: boolean;
    procedure SetViewToolBarChecked(AChecked: boolean);
    procedure SetViewStudyPanelChecked(AChecked: boolean);
    procedure SetClipboardEnabled(AEnabled: boolean);
    procedure SetExportEnabled(AEnabled: boolean);
    procedure SetMenuToggleWhatIsThisDown(ADown: boolean);
    function IsButtonWhatIsThisDown: boolean;
    function SaveState: boolean; override;
    function ResetState: boolean; override;
    procedure RefreshState; virtual;

    // Introduced in this class.
    procedure SetParent(AParent: TWinControl);
    property ToolBar: TToolBar read FToolBar;
  end;

implementation

uses
  SysUtils,
  UAbstractComponent,
  UMainMenuEventType,
  UErrorHandlingOperations;

const

  // Top line menu items.
  CFile                : array[0..0] of WideString = ('File');
  CEdit                : array[0..0] of WideString = ('Edit');
  CView                : array[0..0] of WideString = ('View');
  CHelp                : array[0..0] of WideString = ('Help');

  CExit                : array[0..1] of WideString = ('File','Exit');

  // Edit menu items.
  CCopyToClipboard     : array[0..1] of WideString = ('Edit','CopyToClipboard');
  CExportToFile        : array[0..1] of WideString = ('Edit','ExportToFile');

  // View menu items.
  CViewToolBar         : array[0..1] of WideString = ('View','ViewToolBar');
  CViewStudyPanel      : array[0..1] of WideString = ('View','ViewStudyPanel');
  CViewStudyPanelSep   : array[0..1] of WideString = ('View','ViewStudyPanelSep');
  CViewReset           : array[0..1] of WideString = ('View','ViewReset');

  // Help menu items.
  CHelpContents             : array[0..1] of WideString = ('Help','HelpContents');
  CHelpReadMe               : array[0..1] of WideString = ('Help','ReadMe');
  CHelpReleaseNote          : array[0..1] of WideString = ('Help','ReleaseNote');
  CHelpContentsSep1         : array[0..1] of WideString = ('Help','HelpContentsSep');
//  CHelpUserManual           : array[0..1] of WideString = ('Help','HelpUserManual');
  CHelpExampleFile          : array[0..1] of WideString = ('Help','HelpExampleFiles');
  CHelpManualSep            : array[0..1] of WideString = ('Help','HelpManualSep');
  CHelpAbout                : array[0..1] of WideString = ('Help','HelpAbout');
  CHelpWhatIsThis           : array[0..1] of WideString = ('Help','HelpWhatIsThis');
  CHelpOpenFile             : array[0..1] of WideString = ('Help','HelpOpenFile');


procedure TMainFormMenuItemManager.CreateMemberObjects;
const OPNAME = 'TMainFormMenuItemManager.CreateMemberObjects';
begin
  try
    FToolBar := TToolBar.Create(nil, FAppModules);
    inherited CreateMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMainFormMenuItemManager.DestroyMemberObjects;
const OPNAME = 'TMainFormMenuItemManager.DestroyMemberObjects';
begin
  try
    FToolBar.ChildToolBar := nil;
    FToolBar.Parent := nil;
    FreeAndNil(FToolBar);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMainFormMenuItemManager.SetParent(AParent: TWinControl);
const OPNAME = 'TMainFormMenuItemManager.SetParent';
begin
  try
    FToolBar.Parent := AParent;
    FToolBar.Align := alTop;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMainFormMenuItemManager.AddMenuItems;
const OPNAME = 'TMainFormMenuItemManager.AddMenuItems';
begin
  try

    // Top line of menu items.
    AddMenuItemEntry(CFile,               100);
    AddMenuItemEntry(CEdit,               200);
    AddMenuItemEntry(CView,               300);
    AddMenuItemEntry(CHelp,               900);

    AddMenuItemEntry(CExit,               900, CmeExit);

    // Edit menu items.
    AddMenuItemEntry(CCopyToClipboard,    100, CmeCopyToClipboard);
    AddMenuItemEntry(CExportToFile,       200, CmeExportToFile);

    // View menu items.
    AddMenuItemEntry(CViewToolBar,        100, CmeViewToolBar);
    AddMenuItemEntry(CViewStudyPanel,     110, CmeViewStudyPanel);
    AddMenuItemEntry(CViewStudyPanelSep,  120, CmeSeparator);
    AddMenuItemEntry(CViewReset,          900, CmeViewReset);

    // Help menu items.
    AddMenuItemEntry(CHelpContents,             100, CmeWRMFHelpContents);
    AddMenuItemEntry(CHelpReadMe,               120, CmeWRMFHelpReadMe);
    AddMenuItemEntry(CHelpReleaseNote,          130, CmeWRMFHelpReleaseNote);
    AddMenuItemEntry(CHelpContentsSep1,         200, CmeSeparator);
    AddMenuItemEntry(CHelpExampleFile,          220, CmeExampleFile);
    AddMenuItemEntry(CHelpOpenFile,             230, CmeOpenFile);
    AddMenuItemEntry(CHelpManualSep,            260, CmeSeparator);
    AddMenuItemEntry(CHelpWhatIsThis,           370, CmeHelpWhatIsThis);
    AddMenuItemEntry(CHelpAbout,                300, CmeHelpAbout);

    // Disable model actioned menu items.
    SetViewToolBarChecked(False);
    SetViewStudyPanelChecked(False);
    SetClipboardEnabled(False);
    SetExportEnabled(False);

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMainFormMenuItemManager.SetClipboardEnabled(AEnabled: boolean);
const OPNAME = 'TMainFormMenuItemManager.SetClipboardEnabled';
begin
  try
    if AEnabled then
    begin
      FAppModules.SetMenuItem(CCopyToClipboard, msEnable);
    end
    else
    begin
      FAppModules.SetMenuItem(CCopyToClipboard, msDisable);
    end;
    FToolBar.SetClipboardEnabled(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMainFormMenuItemManager.SetExportEnabled(AEnabled: boolean);
const OPNAME = 'TMainFormMenuItemManager.SetExportEnabled';
begin
  try
    if AEnabled then
    begin
      FAppModules.SetMenuItem(CExportToFile, msEnable);
      FToolBar.SetExportEnabled(AEnabled);
    end
    else
    begin
      FAppModules.SetMenuItem(CExportToFile, msDisable, 'ExportDisabled');
      FToolBar.SetExportEnabled(AEnabled);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMainFormMenuItemManager.SetViewToolBarChecked(AChecked: boolean);
const OPNAME = 'TMainFormMenuItemManager.SetViewToolBarChecked';
begin
  try
    if AChecked then
    begin
      FAppModules.SetMenuItem(CViewToolBar, msChecked);
    end
    else
    begin
      FAppModules.SetMenuItem(CViewToolBar, msUnChecked);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMainFormMenuItemManager.SetViewStudyPanelChecked(AChecked: boolean);
const OPNAME = 'TMainFormMenuItemManager.SetViewStudyPanelChecked';
begin
  try
    if AChecked then
    begin
      FAppModules.SetMenuItem(CViewStudyPanel, msChecked);
    end
    else
    begin
      FAppModules.SetMenuItem(CViewStudyPanel, msUnChecked);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMainFormMenuItemManager.IsToolBarVisible: boolean;
const OPNAME = 'TMainFormMenuItemManager.IsToolBarVisible';
begin
  Result := False;
  try
    if Assigned(ToolBar) then
      Result := ToolBar.Visible;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMainFormMenuItemManager.LanguageHasChanged: boolean;
const OPNAME = 'TMainFormMenuItemManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    if Result then
      Result := ToolBar.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMainFormMenuItemManager.StudyHasChanged: boolean;
const OPNAME = 'TMainFormMenuItemManager.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    if Result then
      Result := ToolBar.StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMainFormMenuItemManager.ResetState: boolean;
const OPNAME = 'TMainFormMenuItemManager.ResetState';
begin
  Result := False;
  try
      Result := FToolBar.ResetState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMainFormMenuItemManager.SaveState: boolean;
const OPNAME = 'TMainFormMenuItemManager.SaveState';
begin
  Result := False;
  try
      Result := FToolBar.SaveState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMainFormMenuItemManager.RefreshState;
const OPNAME = 'TMainFormMenuItemManager.RefreshState';
begin
  try
      FToolBar.RefreshState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMainFormMenuItemManager.SetMenuToggleWhatIsThisDown(ADown: boolean);
const OPNAME = 'TMainFormMenuItemManager.SetMenuToggleWhatIsThisDown';
begin
  try
      FToolBar.IsButtonWhatIsThisDown := ADown;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMainFormMenuItemManager.IsButtonWhatIsThisDown: boolean;
const OPNAME = 'TMainFormMenuItemManager.IsButtonWhatIsThisDown';
begin
  Result := False;
  try
     Result := FToolBar.IsButtonWhatIsThisDown;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
