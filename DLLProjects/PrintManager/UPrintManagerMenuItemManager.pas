//
//
//  UNIT      : Contains TPrintManagerMenuItemManager Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2002/01/03
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UPrintManagerMenuItemManager;

interface

uses
  Vcl.Controls,
  UAbstractObject,
  UPrintManagerToolBar,
  UMenuItemManager;

type
  TPrintManagerMenuItemManager = class(TMenuItemManager)
  protected
    FToolBar: TPrintManagerToolBar;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function Initialise: boolean; override;
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;

    procedure AddMenuItems; override;

    procedure SetMenuPrintSettingsState(AAction: TMenuSetAction);
    procedure SetMenuPrintState(AAction: TMenuSetAction);

    property ToolBar: TPrintManagerToolBar read FToolBar;
  end;

implementation

uses
  SysUtils,
  UAbstractComponent,
  UMainMenuEventType,
  UGenericModelLinkClasses,
  UErrorHandlingOperations;

const
  // Printing menu items.
  CPrintSettings : array[0..1] of WideString = ('File','PrintSettings');
  CPrint         : array[0..1] of WideString = ('File','Print');
  CPrintSep      : array[0..1] of WideString = ('File','PrintSep');

{ TPrintManagerMenuItemManager }

procedure TPrintManagerMenuItemManager.AddMenuItems;
const OPNAME = 'TPrintManagerMenuItemManager.AddMenuItems';
begin
  try
    // Model menu items.
    AddMenuItemEntry(CPrintSettings, 200, CmePrintSettings);
    AddMenuItemEntry(CPrint,         220, CmePrint);
    AddMenuItemEntry(CPrintSep,      250, CmeSeparator);
    Disable;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPrintManagerMenuItemManager.CreateMemberObjects;
const OPNAME = 'TPrintManagerMenuItemManager.CreateMemberObjects';
begin
  inherited;
  try
    FToolBar := TPrintManagerToolBar.Create(nil, FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPrintManagerMenuItemManager.DestroyMemberObjects;
const OPNAME = 'TPrintManagerMenuItemManager.DestroyMemberObjects';
begin
  inherited;
    if Assigned(FAppModules.MainForm()) then
     FAppModules.MainForm.RemoveSystemChildToolBar(FToolBar);
  FreeAndNil(FToolBar);
end;

function TPrintManagerMenuItemManager.Initialise: boolean;
const OPNAME = 'TPrintManagerMenuItemManager.Initialise';
begin
  Result := False;
  try
    if Assigned(FAppModules.MainForm()) then
     FAppModules.MainForm.AddSystemChildToolBar(FToolBar);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPrintManagerMenuItemManager.StudyHasChanged: boolean;
const OPNAME = 'TPrintManagerMenuItemManager.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Result := Result and FToolBar.StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPrintManagerMenuItemManager.LanguageHasChanged: boolean;
const OPNAME = 'TPrintManagerMenuItemManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    Result := Result and FToolBar.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPrintManagerMenuItemManager.SetMenuPrintSettingsState(AAction: TMenuSetAction);
const OPNAME = 'TPrintManagerMenuItemManager.SetMenuPrintSettingsState';
begin
  try
    FToolBar.SetPrintSettingsState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CPrintSettings, AAction, 'PrintSettingsDisabled');
    else
      FAppModules.SetMenuItem(CPrintSettings, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPrintManagerMenuItemManager.SetMenuPrintState(AAction: TMenuSetAction);
const OPNAME = 'TPrintManagerMenuItemManager.SetMenuPrintState';
begin
  try
    FToolBar.SetPrintState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CPrint, AAction, 'PrintDisabled');
    else
      FAppModules.SetMenuItem(CPrint, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
