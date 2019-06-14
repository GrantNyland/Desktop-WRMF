//
//
//  UNIT      : Contains the class TTimeSeriesComparitorToolBar.
//  AUTHOR    : Grant Nyland
//  DATE      : 2002/03/02
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UTimeSeriesComparitorToolBar;

interface

uses
  UAbstractComponent,
  UHelpContexts,
  UChildToolBar;

type
  TTimeSeriesComparitorToolBar = class(TChildToolBar)
  protected
    FCreateChart,
    FRenameChart,
    FDeleteChart,
    FCreateView,
    FRenameView,
    FDeleteView,
    FAddSeries,
    FRemoveSeries,
    FSeriesColor,
    FChartName,
    FSaveView,
    FShowChartLegendDialog,
    FTCSToggleIndividualSeries : TAbstractSpeedButton;
    
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
    procedure SetHorizontalPositions; override;

    procedure OnCreateChart(Sender: TObject);
    procedure OnRenameChart(Sender: TObject);
    procedure OnDeleteChart(Sender: TObject);
    procedure OnAddChart(Sender: TObject);
    procedure OnRemoveChart(Sender: TObject);

    procedure OnCreateView(Sender: TObject);
    procedure OnRenameView(Sender: TObject);
    procedure OnDeleteView(Sender: TObject);

    procedure OnAddSeries(Sender: TObject);
    procedure OnRemoveSeries(Sender: TObject);
    procedure OnSeriesColor(Sender: TObject);

    procedure OnChartName(Sender: TObject);
    procedure OnSaveView(Sender: TObject);
    procedure OnShowChartLegendDialog(Sender: TObject);
    procedure OnTSCToggleIndividualSeries(Sender: TObject);
  public
    procedure SetCreateChartState(AEnabled: boolean);
    procedure SetRenameChartState(AEnabled: boolean);
    procedure SetDeleteChartState(AEnabled: boolean);

    procedure SetCreateViewState(AEnabled: boolean);
    procedure SetRenameViewState(AEnabled: boolean);
    procedure SetDeleteViewState(AEnabled: boolean);

    procedure SetAddSeriesState(AEnabled: boolean);
    procedure SetRemoveSeriesState(AEnabled: boolean);
    procedure SetSeriesColorState(AEnabled: boolean);

    procedure SetChartNameState(AEnabled: boolean);
    procedure SetSaveViewState(AEnabled: boolean);
    procedure SetShowChartLegendDialogState(AEnabled: boolean);
    procedure SetTSCToggleIndividualSeriesState(AEnabled: boolean);

    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    property ToggleIndividualSeries : TAbstractSpeedButton read FTCSToggleIndividualSeries;
  end;

implementation

uses
  SysUtils,
  UGenericModelLinkClasses,
  UMainMenuEventType,
  UErrorHandlingOperations;

procedure TTimeSeriesComparitorToolBar.CreateMemberObjects;
const OPNAME = 'TTimeSeriesComparitorToolBar.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try

    FCreateChart := CreateButton('TimeComparitorCreateChart');
    FCreateChart.OnClick := OnCreateChart;
    FCreateChart.Enabled := False;

    FRenameChart := CreateButton('TimeComparitorRenameChart');
    FRenameChart.OnClick := OnRenameChart;
    FRenameChart.Enabled := False;

    FDeleteChart := CreateButton('TimeComparitorDeleteChart');
    FDeleteChart.OnClick := OnDeleteChart;
    FDeleteChart.Enabled := False;

    FCreateView := CreateButton('TimeComparitorCreateView');
    FCreateView.OnClick := OnCreateView;
    FCreateView.Enabled := False;

    FRenameView := CreateButton('TimeComparitorRenameView');
    FRenameView.OnClick := OnRenameView;
    FRenameView.Enabled := False;

    FDeleteView := CreateButton('TimeComparitorDeleteView');
    FDeleteView.OnClick := OnDeleteView;
    FDeleteView.Enabled := False;

    FAddSeries := CreateButton('TimeComparitorAddSeries');
    FAddSeries.OnClick := OnAddSeries;
    FAddSeries.Enabled := False;

    FRemoveSeries := CreateButton('TimeComparitorRemoveSeries');
    FRemoveSeries.OnClick := OnRemoveSeries;
    FRemoveSeries.Enabled := False;

    FSeriesColor := CreateButton('Color');
    FSeriesColor.OnClick := OnSeriesColor;
    FSeriesColor.Enabled := False;

    FChartName := CreateButton('TimeComparitorChartName');
    FChartName.OnClick := OnChartName;
    FChartName.Enabled := False;

    FSaveView := CreateButton('TimeComparitorSaveView');
    FSaveView.OnClick := OnSaveView;
    FSaveView.Enabled := False;

    FShowChartLegendDialog         := CreateButton('TimeComparitorShowChartLegendDialog');
    FShowChartLegendDialog.OnClick := OnShowChartLegendDialog;
    FShowChartLegendDialog.Enabled := False;

    FTCSToggleIndividualSeries := CreateButton('TSCToggleView');
    FTCSToggleIndividualSeries.OnClick := OnTSCToggleIndividualSeries;
    FTCSToggleIndividualSeries.GroupIndex := 99;
    FTCSToggleIndividualSeries.AllowAllUp := True;
    FTCSToggleIndividualSeries.Enabled := False;

    SetHorizontalPositions;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorToolBar.AssignHelpContext;
const OPNAME = 'TTimeSeriesComparitorToolBar.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,HC_TimeSeriesComparator);
    SetControlHelpContext(FAddSeries,HC_TimeComparitorAddSeries);
    SetControlHelpContext(FRemoveSeries,HC_TimeComparitorRemoveSeries);

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesComparitorToolBar.SetHorizontalPositions;
const OPNAME = 'TTimeSeriesComparitorToolBar.SetHorizontalPositions';
var
  LButtonCount, LGaps: integer;
begin
  try
    inherited SetHorizontalPositions;
    LButtonCount := -1;
    LGaps :=  0;

    SetButtonHorizontalPosition(FCreateView,  True, False, LButtonCount, LGaps);
    Width := FCreateView.Left + FCreateView.Width;
    SetButtonHorizontalPosition(FRenameView,  True, False, LButtonCount, LGaps);
    Width := FRenameView.Left + FRenameView.Width;
    SetButtonHorizontalPosition(FDeleteView,  True, False, LButtonCount, LGaps);
    Width := FDeleteView.Left + FDeleteView.Width;
    SetButtonHorizontalPosition(FSaveView,  True, False, LButtonCount, LGaps);
    Width := FSaveView.Left + FSaveView.Width;

    SetButtonHorizontalPosition(FCreateChart,  True, TRUE, LButtonCount, LGaps);
    Width := FCreateChart.Left + FCreateChart.Width;
    SetButtonHorizontalPosition(FRenameChart,  True, False, LButtonCount, LGaps);
    Width := FRenameChart.Left + FRenameChart.Width;
    SetButtonHorizontalPosition(FChartName,  True, False, LButtonCount, LGaps);
    Width := FChartName.Left + FChartName.Width;
    SetButtonHorizontalPosition(FShowChartLegendDialog,  True, False, LButtonCount, LGaps);
    Width := FShowChartLegendDialog.Left + FShowChartLegendDialog.Width;
    SetButtonHorizontalPosition(FDeleteChart,  True, False, LButtonCount, LGaps);
    Width := FDeleteChart.Left + FDeleteChart.Width;

    SetButtonHorizontalPosition(FAddSeries,  True, TRUE, LButtonCount, LGaps);
    Width := FAddSeries.Left + FAddSeries.Width;
    SetButtonHorizontalPosition(FRemoveSeries,  True, False, LButtonCount, LGaps);
    Width := FRemoveSeries.Left + FRemoveSeries.Width;
    SetButtonHorizontalPosition(FSeriesColor,  True, False, LButtonCount, LGaps);
    Width := FSeriesColor.Left + FSeriesColor.Width;

    SetButtonHorizontalPosition(FTCSToggleIndividualSeries,  True, False, LButtonCount, LGaps);
    Width := FTCSToggleIndividualSeries.Left + FTCSToggleIndividualSeries.Width;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorToolBar.LanguageHasChanged: boolean;
const OPNAME = 'TTimeSeriesComparitorToolBar.LanguageHasChanged';
begin
  Result := True;
  try
    inherited LanguageHasChanged;

    if Assigned(FCreateChart) then
      FCreateChart.LanguageHasChanged;

    if Assigned(FRenameChart) then
      FRenameChart.LanguageHasChanged;

    if Assigned(FDeleteChart) then
      FDeleteChart.LanguageHasChanged;

    if Assigned(FCreateView) then
      FCreateView.LanguageHasChanged;

    if Assigned(FRenameView) then
      FRenameView.LanguageHasChanged;

    if Assigned(FDeleteView) then
      FDeleteView.LanguageHasChanged;

    if Assigned(FAddSeries) then
      FAddSeries.LanguageHasChanged;

    if Assigned(FRemoveSeries) then
      FRemoveSeries.LanguageHasChanged;

    if Assigned(FSeriesColor) then
      FSeriesColor.LanguageHasChanged;

    if Assigned(FChartName) then
      FChartName.LanguageHasChanged;

    if Assigned(FSaveView) then
      FSaveView.LanguageHasChanged;

    if Assigned(FShowChartLegendDialog) then
      FShowChartLegendDialog.LanguageHasChanged;

    if Assigned(FTCSToggleIndividualSeries) then
      FTCSToggleIndividualSeries.LanguageHasChanged;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorToolBar.StudyHasChanged: boolean;
const OPNAME = 'TTimeSeriesComparitorToolBar.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorToolBar.SetAddSeriesState(AEnabled: boolean);
const OPNAME = 'TTimeSeriesComparitorToolBar.SetAddSeriesState';
begin
  try
    SetButtonEnabled(FAddSeries, AEnabled, 'AddSeriesDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorToolBar.SetRemoveSeriesState(AEnabled: boolean);
const OPNAME = 'TTimeSeriesComparitorToolBar.SetRemoveSeriesState';
begin
  try
    SetButtonEnabled(FRemoveSeries, AEnabled, 'RemoveSeriesDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorToolBar.SetSaveViewState(AEnabled: boolean);
const OPNAME = 'TTimeSeriesComparitorToolBar.SetSaveViewState';
begin
  try
    SetButtonEnabled(FSaveView, AEnabled, 'SaveViewDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorToolBar.OnAddSeries(Sender: TObject);
const OPNAME = 'TTimeSeriesComparitorToolBar.OnAddSeries';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meAddSeries));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorToolBar.OnRemoveSeries(Sender: TObject);
const OPNAME = 'TTimeSeriesComparitorToolBar.OnRemoveSeries';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meRemoveSeries));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorToolBar.OnSeriesColor(Sender: TObject);
const OPNAME = 'TTimeSeriesComparitorToolBar.OnSeriesColor';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meSeriesColor));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorToolBar.OnAddChart(Sender: TObject);
const OPNAME = 'TTimeSeriesComparitorToolBar.OnAddChart';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meAddChart));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorToolBar.OnRemoveChart(Sender: TObject);
const OPNAME = 'TTimeSeriesComparitorToolBar.OnRemoveChart';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meRemoveChart));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorToolBar.OnSaveView(Sender: TObject);
const OPNAME = 'TTimeSeriesComparitorToolBar.OnSaveView';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meSaveView));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorToolBar.OnChartName(Sender: TObject);
const OPNAME = 'TTimeSeriesComparitorToolBar.OnChartName';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meChartName));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorToolBar.SetChartNameState(AEnabled: boolean);
const OPNAME = 'TTimeSeriesComparitorToolBar.SetChartNameState';
begin
  try
    SetButtonEnabled(FChartName, AEnabled, 'ChartNameDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorToolBar.OnCreateChart(Sender: TObject);
const OPNAME = 'TTimeSeriesComparitorToolBar.OnCreateChart';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meCreateChart));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorToolBar.OnCreateView(Sender: TObject);
const OPNAME = 'TTimeSeriesComparitorToolBar.OnCreateView';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meCreateView));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorToolBar.OnDeleteChart(Sender: TObject);
const OPNAME = 'TTimeSeriesComparitorToolBar.OnDeleteChart';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meDeleteChart));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorToolBar.OnDeleteView(Sender: TObject);
const OPNAME = 'TTimeSeriesComparitorToolBar.OnDeleteView';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meDeleteView));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorToolBar.OnRenameChart(Sender: TObject);
const OPNAME = 'TTimeSeriesComparitorToolBar.OnRenameChart';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meRenameChart));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorToolBar.OnRenameView(Sender: TObject);
const OPNAME = 'TTimeSeriesComparitorToolBar.OnRenameView';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meRenameView));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorToolBar.SetCreateChartState(AEnabled: boolean);
const OPNAME = 'TTimeSeriesComparitorToolBar.SetCreateChartState';
begin
  try
    SetButtonEnabled(FCreateChart, AEnabled, 'CreateChartDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorToolBar.SetCreateViewState(AEnabled: boolean);
const OPNAME = 'TTimeSeriesComparitorToolBar.SetCreateViewState';
begin
  try
    SetButtonEnabled(FCreateView, AEnabled, 'CreateViewDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorToolBar.SetDeleteChartState(AEnabled: boolean);
const OPNAME = 'TTimeSeriesComparitorToolBar.SetDeleteChartState';
begin
  try
    SetButtonEnabled(FDeleteChart, AEnabled, 'DeleteChartDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorToolBar.SetDeleteViewState(AEnabled: boolean);
const OPNAME = 'TTimeSeriesComparitorToolBar.SetDeleteViewState';
begin
  try
    SetButtonEnabled(FDeleteView, AEnabled, 'DeleteViewDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorToolBar.SetRenameChartState(AEnabled: boolean);
const OPNAME = 'TTimeSeriesComparitorToolBar.SetRenameChartState';
begin
  try
    SetButtonEnabled(FRenameChart, AEnabled, 'RenameChartDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorToolBar.SetRenameViewState(AEnabled: boolean);
const OPNAME = 'TTimeSeriesComparitorToolBar.SetRenameViewState';
begin
  try
    SetButtonEnabled(FRenameView, AEnabled, 'RenameViewDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorToolBar.SetSeriesColorState(AEnabled: boolean);
const OPNAME = 'TTimeSeriesComparitorToolBar.SetSeriesColorState';
begin
  try
    SetButtonEnabled(FSeriesColor, AEnabled, 'SeriesColorDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorToolBar.OnShowChartLegendDialog(Sender: TObject);
const OPNAME = 'TTimeSeriesComparitorToolBar.OnShowChartLegendDialog';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meShowChartLegendDialog));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorToolBar.OnTSCToggleIndividualSeries(Sender: TObject);
const OPNAME = 'TTimeSeriesComparitorToolBar.OnTSCToggleIndividualSeries';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meTSCToggleIndividualSeries));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorToolBar.SetShowChartLegendDialogState(AEnabled: boolean);
const OPNAME = 'TTimeSeriesComparitorToolBar.SetShowChartLegendDialogState';
begin
  try
    SetButtonEnabled(FShowChartLegendDialog, AEnabled, 'ShowChartLegendDialogDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorToolBar.SetTSCToggleIndividualSeriesState(AEnabled: boolean);
const OPNAME = 'TTimeSeriesComparitorToolBar.SetTSCToggleIndividualSeriesState';
begin
  try
    SetButtonEnabled(FTCSToggleIndividualSeries, AEnabled, 'TSCToggleViewDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
