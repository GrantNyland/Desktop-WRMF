//
//
//  UNIT      : Contains TTimeSeriesComparitorMenuItemManager Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2002/09/18
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UTimeSeriesComparitorMenuItemManager;

interface

uses
  UAbstractObject,
  UGUIConstants,
  UTimeSeriesComparitorToolBar,
  UMenuItemManager;

type

  TTimeSeriesComparitorMenuItemManager = class(TMenuItemManager)
  protected
    FToolBar: TTimeSeriesComparitorToolBar;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure ToggleAllEnabledState(AEnabled: boolean);
    procedure ToggleViewEnabledState(AEnabled: boolean);
    procedure ToggleChartEnabledState(AEnabled: boolean);
    procedure ToggleSeriesEnabledState(AEnabled: boolean);

    procedure SetMenuCreateChart(AAction: TMenuSetAction);
    procedure SetMenuRenameChart(AAction: TMenuSetAction);
    procedure SetMenuDeleteChart(AAction: TMenuSetAction);

    procedure SetMenuCreateView(AAction: TMenuSetAction);
    procedure SetMenuRenameView(AAction: TMenuSetAction);
    procedure SetMenuDeleteView(AAction: TMenuSetAction);

    procedure SetMenuAddSeries(AAction: TMenuSetAction);
    procedure SetMenuRemoveSeries(AAction: TMenuSetAction);
    procedure SetMenuSeriesColor(AAction: TMenuSetAction);

    procedure SetMenuChartName(AAction: TMenuSetAction);
    procedure SetMenuSaveView(AAction: TMenuSetAction);
    procedure SetMenuShowChartLegendDialog(AAction: TMenuSetAction);
    procedure SetTSCToggleIndividualSeries(AAction: TMenuSetAction);

    procedure AddMenuItems; override;
    function Initialise: Boolean; override;
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    property ToolBar: TTimeSeriesComparitorToolBar read FToolBar;
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
  CTimeComparitorSep               : array[0..1] of WideString = ('View','TimeComparitorSep');

  CTimeComparitorCreateChart       : array[0..1] of WideString = ('View','TimeComparitorCreateChart');
  CTimeComparitorRenameChart       : array[0..1] of WideString = ('View','TimeComparitorRenameChart');
  CTimeComparitorDeleteChart       : array[0..1] of WideString = ('View','TimeComparitorDeleteChart');
  CTimeComparitorAddChart          : array[0..1] of WideString = ('View','TimeComparitorAddChart');
  CTimeComparitorRemoveChart       : array[0..1] of WideString = ('View','TimeComparitorRemoveChart');

  CTimeComparitorCreateView        : array[0..1] of WideString = ('View','TimeComparitorCreateView');
  CTimeComparitorRenameView        : array[0..1] of WideString = ('View','TimeComparitorRenameView');
  CTimeComparitorDeleteView        : array[0..1] of WideString = ('View','TimeComparitorDeleteView');

  CTimeComparitorAddSeries         : array[0..1] of WideString = ('View','TimeComparitorAddSeries');
  CTimeComparitorRemoveSeries      : array[0..1] of WideString = ('View','TimeComparitorRemoveSeries');
  CTimeComparitorSeriesColor       : array[0..1] of WideString = ('View','TimeComparitorSeriesColor');

  CTimeComparitorChartName         : array[0..1] of WideString = ('View','TimeComparitorChartName');
  CTimeComparitorChartLegendDialog : array[0..1] of WideString = ('View','TimeComparitorShowChartLegendDialog');
  CTimeComparitorSaveView          : array[0..1] of WideString = ('View','TimeComparitorSaveView');
  CTSCToggleIndividualSeries       : array[0..1] of WideString = ('View','TimeComparitorToggleIndividualSeries');


{ TTimeSeriesComparitorMenuItemManager }

procedure TTimeSeriesComparitorMenuItemManager.AddMenuItems;
const OPNAME = 'TTimeSeriesComparitorMenuItemManager.AddMenuItems';
begin
  try
    AddMenuItemEntry(CTimeComparitorSep,          1750, CmeSeparator);

    AddMenuItemEntry(CTimeComparitorCreateChart,  1751, CmeCustomModelEvent, TModelMenuData.Create(meCreateChart));
    AddMenuItemEntry(CTimeComparitorRenameChart,  1752, CmeCustomModelEvent, TModelMenuData.Create(meRenameChart));
    AddMenuItemEntry(CTimeComparitorDeleteChart,  1753, CmeCustomModelEvent, TModelMenuData.Create(meDeleteChart));
    AddMenuItemEntry(CTimeComparitorAddChart,     1754, CmeCustomModelEvent, TModelMenuData.Create(meAddChart));
    AddMenuItemEntry(CTimeComparitorRemoveChart,  1755, CmeCustomModelEvent, TModelMenuData.Create(meRemoveChart));

    AddMenuItemEntry(CTimeComparitorCreateView,   1756, CmeCustomModelEvent, TModelMenuData.Create(meCreateView));
    AddMenuItemEntry(CTimeComparitorRenameView,   1757, CmeCustomModelEvent, TModelMenuData.Create(meRenameView));
    AddMenuItemEntry(CTimeComparitorDeleteView,   1758, CmeCustomModelEvent, TModelMenuData.Create(meDeleteView));

    AddMenuItemEntry(CTimeComparitorAddSeries,    1759, CmeCustomModelEvent, TModelMenuData.Create(meAddSeries));
    AddMenuItemEntry(CTimeComparitorRemoveSeries, 1760, CmeCustomModelEvent, TModelMenuData.Create(meRemoveSeries));
    AddMenuItemEntry(CTimeComparitorSeriesColor , 1761, CmeCustomModelEvent, TModelMenuData.Create(meSeriesColor));

    AddMenuItemEntry(CTimeComparitorChartName,    1762, CmeCustomModelEvent, TModelMenuData.Create(meChartName));
    AddMenuItemEntry(CTimeComparitorChartLegendDialog,    1763, CmeCustomModelEvent, TModelMenuData.Create(meShowChartLegendDialog));
    AddMenuItemEntry(CTimeComparitorSaveView,     1764, CmeCustomModelEvent, TModelMenuData.Create(meSaveView));
    AddMenuItemEntry(CTSCToggleIndividualSeries,  1765, CmeCustomModelEvent, TModelMenuData.Create(meTSCToggleIndividualSeries));
    Hide;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorMenuItemManager.CreateMemberObjects;
const OPNAME = 'TTimeSeriesComparitorMenuItemManager.CreateMemberObjects';
begin
  inherited;
  try
    FToolBar := TTimeSeriesComparitorToolBar.Create(nil, FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorMenuItemManager.DestroyMemberObjects;
const OPNAME = 'TTimeSeriesComparitorMenuItemManager.DestroyMemberObjects';
begin
  inherited;
  FreeAndNil(FToolBar);
end;

function TTimeSeriesComparitorMenuItemManager.StudyHasChanged: boolean;
const OPNAME = 'TTimeSeriesComparitorMenuItemManager.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    FToolBar.StudyHasChanged;
    ToggleAllEnabledState(False);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorMenuItemManager.LanguageHasChanged: boolean;
const OPNAME = 'TTimeSeriesComparitorMenuItemManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    if Result then
      Result := FToolBar.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorMenuItemManager.SetMenuAddSeries(AAction: TMenuSetAction);
const OPNAME = 'TTimeSeriesComparitorMenuItemManager.SetMenuAddSeries';
begin
  try
    FToolBar.SetAddSeriesState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CTimeComparitorAddSeries, AAction, 'AddSeriesDisabled');
    else
      FAppModules.SetMenuItem(CTimeComparitorAddSeries, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorMenuItemManager.SetMenuRemoveSeries(AAction: TMenuSetAction);
const OPNAME = 'TTimeSeriesComparitorMenuItemManager.SetMenuRemoveSeries';
begin
  try
    FToolBar.SetRemoveSeriesState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CTimeComparitorRemoveSeries, AAction, 'RemoveSeriesDisabled');
    else
      FAppModules.SetMenuItem(CTimeComparitorRemoveSeries, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorMenuItemManager.Initialise: Boolean;
const OPNAME = 'TTimeSeriesComparitorMenuItemManager: Initialise';
begin
  Result := inherited Initialise;
  try
    ToggleAllEnabledState(False);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorMenuItemManager.ToggleAllEnabledState(AEnabled: boolean);
const OPNAME = 'TTimeSeriesComparitorMenuItemManager: ToggleAllEnabledState';
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

    SetMenuCreateChart(LEnabled);
    SetMenuRenameChart(LEnabled);
    SetMenuShowChartLegendDialog(LEnabled);
    SetMenuDeleteChart(LEnabled);

    SetMenuCreateView(LEnabled);
    SetMenuRenameView(LEnabled);
    SetMenuDeleteView(LEnabled);

    SetMenuAddSeries(LEnabled);
    SetMenuRemoveSeries(LEnabled);
    SetMenuSeriesColor(LEnabled);

    SetMenuChartName(LEnabled);
    SetMenuSaveView(LEnabled);
    SetTSCToggleIndividualSeries(LEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorMenuItemManager.ToggleViewEnabledState(AEnabled: boolean);
const OPNAME = 'TTimeSeriesComparitorMenuItemManager: ToggleViewEnabledState';
var
  LEnabled: TMenuSetAction;
begin
  try
    if AEnabled then
      LEnabled := msEnable
    else
      LEnabled := msDisable;

    SetMenuRenameView(LEnabled);
    SetMenuDeleteView(LEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorMenuItemManager.ToggleChartEnabledState(AEnabled: boolean);
const OPNAME = 'TTimeSeriesComparitorMenuItemManager: ToggleChartEnabledState';
var
  LEnabled: TMenuSetAction;
begin
  try
    if AEnabled then
      LEnabled := msEnable
    else
      LEnabled := msDisable;

    SetMenuRenameChart(LEnabled);
    SetMenuDeleteChart(LEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorMenuItemManager.ToggleSeriesEnabledState(AEnabled: boolean);
const OPNAME = 'TTimeSeriesComparitorMenuItemManager: ToggleSeriesEnabledState';
var
  LEnabled: TMenuSetAction;
begin
  try
    if AEnabled then
      LEnabled := msEnable
    else
      LEnabled := msDisable;

    SetMenuAddSeries(LEnabled);
    SetMenuRemoveSeries(LEnabled);
    SetMenuSeriesColor(LEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorMenuItemManager.SetMenuSaveView(AAction: TMenuSetAction);
const OPNAME = 'TTimeSeriesComparitorMenuItemManager.SetMenuSaveView';
begin
  try
    if not(FAppModules.User.UserRights in CUR_EditData) and
       (FAppModules.StudyArea <> nil) and
       (FAppModules.StudyArea.ScenarioLocked) then
      AAction := msDisable
    else
      AAction := msEnable;
    FToolBar.SetSaveViewState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CTimeComparitorSaveView, AAction, 'SaveViewDisabled');
    else
      FAppModules.SetMenuItem(CTimeComparitorSaveView, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorMenuItemManager.SetMenuChartName(AAction: TMenuSetAction);
const OPNAME = 'TTimeSeriesComparitorMenuItemManager.SetMenuChartName';
begin
  try
    FToolBar.SetChartNameState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CTimeComparitorChartName, AAction, 'ChartNameDisabled');
    else
      FAppModules.SetMenuItem(CTimeComparitorChartName, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorMenuItemManager.SetMenuCreateChart(AAction: TMenuSetAction);
const OPNAME = 'TTimeSeriesComparitorMenuItemManager.SetMenuCreateChart';
begin
  try
    FToolBar.SetCreateChartState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CTimeComparitorCreateChart, AAction, 'CreateChartDisabled');
    else
      FAppModules.SetMenuItem(CTimeComparitorCreateChart, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorMenuItemManager.SetMenuCreateView(AAction: TMenuSetAction);
const OPNAME = 'TTimeSeriesComparitorMenuItemManager.SetMenuCreateView';
begin
  try
    FToolBar.SetCreateViewState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CTimeComparitorCreateView, AAction, 'CreateViewDisabled');
    else
      FAppModules.SetMenuItem(CTimeComparitorCreateView, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorMenuItemManager.SetMenuDeleteChart(AAction: TMenuSetAction);
const OPNAME = 'TTimeSeriesComparitorMenuItemManager.SetMenuDeleteChart';
begin
  try
    FToolBar.SetDeleteChartState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CTimeComparitorDeleteChart, AAction, 'DeleteChartDisabled');
    else
      FAppModules.SetMenuItem(CTimeComparitorDeleteChart, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorMenuItemManager.SetMenuDeleteView(AAction: TMenuSetAction);
const OPNAME = 'TTimeSeriesComparitorMenuItemManager.SetMenuDeleteView';
begin
  try
    FToolBar.SetDeleteViewState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CTimeComparitorDeleteView, AAction, 'DeleteViewDisabled');
    else
      FAppModules.SetMenuItem(CTimeComparitorDeleteView, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorMenuItemManager.SetMenuRenameChart(AAction: TMenuSetAction);
const OPNAME = 'TTimeSeriesComparitorMenuItemManager.SetMenuRenameChart';
begin
  try
    FToolBar.SetRenameChartState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CTimeComparitorRenameChart, AAction, 'RenameChartDisabled');
    else
      FAppModules.SetMenuItem(CTimeComparitorRenameChart, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorMenuItemManager.SetMenuRenameView(AAction: TMenuSetAction);
const OPNAME = 'TTimeSeriesComparitorMenuItemManager.SetMenuRenameView';
begin
  try
    FToolBar.SetRenameViewState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CTimeComparitorRenameView, AAction, 'RenameViewDisabled');
    else
      FAppModules.SetMenuItem(CTimeComparitorRenameView, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorMenuItemManager.SetMenuSeriesColor(AAction: TMenuSetAction);
const OPNAME = 'TTimeSeriesComparitorMenuItemManager.SetMenuSeriesColor';
begin
  try
    FToolBar.SetSeriesColorState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CTimeComparitorSeriesColor, AAction, 'SeriesColorDisabled');
    else
      FAppModules.SetMenuItem(CTimeComparitorSeriesColor, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorMenuItemManager.SetTSCToggleIndividualSeries(AAction: TMenuSetAction);
const OPNAME = 'TTimeSeriesComparitorMenuItemManager.SetTSCToggleIndividualSeries';
begin
  try
    FToolBar.SetTSCToggleIndividualSeriesState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CTSCToggleIndividualSeries, AAction, 'TSCToggleIndividualSeriesDisabled');
    else
      FAppModules.SetMenuItem(CTSCToggleIndividualSeries, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorMenuItemManager.SetMenuShowChartLegendDialog(AAction: TMenuSetAction);
const OPNAME = 'TTimeSeriesComparitorMenuItemManager.SetMenuShowChartLegendDialog';
begin
  try
    FToolBar.SetShowChartLegendDialogState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CTimeComparitorChartLegendDialog, AAction, 'ShowChartLegendDialogDisabled');
    else
      FAppModules.SetMenuItem(CTimeComparitorChartLegendDialog, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
