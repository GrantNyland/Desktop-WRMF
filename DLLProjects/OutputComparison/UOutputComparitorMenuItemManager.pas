//
//
//  UNIT      : Contains TOutputComparitorMenuItemManager Class
//  AUTHOR    : Sam Dhlamini(ARAVIA)
//  DATE      : 2008/06/05
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UOutputComparitorMenuItemManager;

interface

uses
  UAbstractObject,
  UGUIConstants,
  UOutputComparitorToolBar,
  UMenuItemManager;

type

  TOutputComparitorMenuItemManager = class(TMenuItemManager)
  protected
    FToolBar: TOutputComparitorToolBar;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure ToggleAllEnabledState(AEnabled: boolean);
    procedure SetMenuShowChartLegendDialog(AAction: TMenuSetAction);

    procedure AddMenuItems; override;
    function Initialise: Boolean; override;
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    property ToolBar: TOutputComparitorToolBar read FToolBar;
  end;

implementation

uses
  SysUtils,
  UConstants,
  UAbstractComponent,
  UMainMenuEventType,
  UGenericModelLinkClasses,
  UErrorHandlingOperations;

const
  COutputComparitorSep               : array[0..1] of WideString = ('View','OutputComparitorSep');
  COutputComparitorChartLegendDialog : array[0..1] of WideString = ('View','OutputComparitorShowChartLegendDialog');

{ TOutputComparitorMenuItemManager }

procedure TOutputComparitorMenuItemManager.AddMenuItems;
const OPNAME = 'TOutputComparitorMenuItemManager.AddMenuItems';
begin
  try
    AddMenuItemEntry(COutputComparitorSep,          1750, CmeSeparator);
    AddMenuItemEntry(COutputComparitorChartLegendDialog,    1751, CmeCustomModelEvent, TModelMenuData.Create(meOutputComparitorShowChartLegendDialog));
    Hide;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparitorMenuItemManager.CreateMemberObjects;
const OPNAME = 'TOutputComparitorMenuItemManager.CreateMemberObjects';
begin
  inherited;
  try
    FToolBar := TOutputComparitorToolBar.Create(nil, FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparitorMenuItemManager.DestroyMemberObjects;
const OPNAME = 'TOutputComparitorMenuItemManager.DestroyMemberObjects';
begin
  inherited;
  FreeAndNil(FToolBar);
end;

function TOutputComparitorMenuItemManager.StudyHasChanged: boolean;
const OPNAME = 'TOutputComparitorMenuItemManager.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    FToolBar.StudyHasChanged;
    ToggleAllEnabledState(False);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComparitorMenuItemManager.LanguageHasChanged: boolean;
const OPNAME = 'TOutputComparitorMenuItemManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    if Result then
      Result := FToolBar.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComparitorMenuItemManager.Initialise: Boolean;
const OPNAME = 'TOutputComparitorMenuItemManager: Initialise';
begin
  Result := inherited Initialise;
  try
    ToggleAllEnabledState(False);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparitorMenuItemManager.ToggleAllEnabledState(AEnabled: boolean);
const OPNAME = 'TOutputComparitorMenuItemManager: ToggleAllEnabledState';
var
  LEnabled: TMenuSetAction;
begin
  try
    AEnabled := AEnabled and (FAppModules.User.UserRights in CUR_EditData);
    AEnabled := AEnabled and  (FAppModules.StudyArea <> nil) and (not(FAppModules.StudyArea.ScenarioLocked));
    if AEnabled then
      LEnabled := msEnable
    else
      LEnabled := msDisable;

    SetMenuShowChartLegendDialog(LEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparitorMenuItemManager.SetMenuShowChartLegendDialog(AAction: TMenuSetAction);
const OPNAME = 'TOutputComparitorMenuItemManager.SetMenuShowChartLegendDialog';
begin
  try
    FToolBar.SetShowChartLegendDialogState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(COutputComparitorChartLegendDialog, AAction, 'ShowChartLegendDialogDisabled');
    else
      FAppModules.SetMenuItem(COutputComparitorChartLegendDialog, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
