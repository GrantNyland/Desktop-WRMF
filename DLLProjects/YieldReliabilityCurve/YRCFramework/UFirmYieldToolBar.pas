//
//
//  UNIT      : Contains the class TFirmYieldToolBar.
//  AUTHOR    : Grant Nyland
//  DATE      : 2002/03/02
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UFirmYieldToolBar;

interface

uses
  UAbstractComponent,
  UHelpContexts,
  UChildToolBar;

type
  TFirmYieldToolBar = class(TChildToolBar)
  protected
    FNewChart,
    FOpenChart,
    FSaveChart,
    FAddSeries,
    FDeleteSeries : TAbstractSpeedButton;
    procedure CreateMemberObjects; override;
    procedure SetHorizontalPositions; override;
    procedure OnNewChart(Sender: TObject);
    procedure OnOpenChart(Sender: TObject);
    procedure OnSaveChart(Sender: TObject);
    procedure OnAddSeries(Sender: TObject);
    procedure OnDeleteSeries(Sender: TObject);
  public
    function LanguageHasChanged: boolean; override;
    procedure SetNewChartState(AEnabled: boolean);
    procedure SetOpenChartState(AEnabled: boolean);
    procedure SetSaveChartState(AEnabledState: boolean);
    procedure SetAddSeriesState(AEnabledState: boolean);
    procedure SetDeleteSeriesState(AEnabledState: boolean);
  end;

implementation

uses
  SysUtils,
  UGenericModelLinkClasses,
  UMainMenuEventType,
  UErrorHandlingOperations;

procedure TFirmYieldToolBar.CreateMemberObjects;
const OPNAME = 'TFirmYieldToolBar.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FNewChart := CreateButton('YRCFYNewChart');
    FNewChart.OnClick := OnNewChart;
    FNewChart.Enabled := False;

    FOpenChart := CreateButton('YRCFYOpenChart');
    FOpenChart.OnClick := OnOpenChart;
    FOpenChart.Enabled := False;

    FSaveChart := CreateButton('YRCFYSaveChart');
    FSaveChart.OnClick := OnSaveChart;
    FSaveChart.Enabled := False;

    FAddSeries := CreateButton('YRCFYAddSeries');
    FAddSeries.OnClick := OnAddSeries;
    FAddSeries.Enabled := False;

    FDeleteSeries := CreateButton('YRCFYDeleteSeries');
    FDeleteSeries.OnClick := OnDeleteSeries;
    FDeleteSeries.Enabled := False;

    SetHorizontalPositions;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFirmYieldToolBar.LanguageHasChanged: boolean;
const OPNAME = 'TFirmYieldToolBar.LanguageHasChanged';
begin
  Result := True;
  try
    inherited LanguageHasChanged;

    if Assigned(FNewChart) then
      FNewChart.LanguageHasChanged;

    if Assigned(FOpenChart) then
      FOpenChart.LanguageHasChanged;

    if Assigned(FSaveChart) then
      FSaveChart.LanguageHasChanged;

    if Assigned(FAddSeries) then
      FAddSeries.LanguageHasChanged;

    if Assigned(FDeleteSeries) then
      FDeleteSeries.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFirmYieldToolBar.SetHorizontalPositions;
const OPNAME = 'TFirmYieldToolBar.SetHorizontalPositions';
var
  LButtonCount, LGaps: integer;
begin
  try
    inherited SetHorizontalPositions;
    LButtonCount := -1;
    LGaps :=  0;
    SetButtonHorizontalPosition(FNewChart,  True, False, LButtonCount, LGaps);
    Width := FNewChart.Left + FNewChart.Width;
    SetButtonHorizontalPosition(FOpenChart,  True, False, LButtonCount, LGaps);
    Width := FOpenChart.Left + FOpenChart.Width;
    SetButtonHorizontalPosition(FSaveChart,  True, False, LButtonCount, LGaps);
    Width := FSaveChart.Left + FSaveChart.Width;
    SetButtonHorizontalPosition(FAddSeries,  True, False, LButtonCount, LGaps);
    Width := FAddSeries.Left + FAddSeries.Width;
    SetButtonHorizontalPosition(FDeleteSeries,  True, False, LButtonCount, LGaps);
    Width := Width + FDeleteSeries.Left + FDeleteSeries.Width;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFirmYieldToolBar.OnNewChart(Sender: TObject);
const OPNAME = 'TFirmYieldToolBar.OnNewChart';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meFirmYieldNewChart));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFirmYieldToolBar.OnOpenChart(Sender: TObject);
const OPNAME = 'TFirmYieldToolBar.OnOpenChart';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meFirmYieldOpenChart));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFirmYieldToolBar.OnSaveChart(Sender: TObject);
const OPNAME = 'TFirmYieldToolBar.OnSaveChart';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meFirmYieldSaveChart));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFirmYieldToolBar.OnAddSeries(Sender: TObject);
const OPNAME = 'TFirmYieldToolBar.OnAddSeries';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meFirmYieldAddSeries));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFirmYieldToolBar.OnDeleteSeries(Sender: TObject);
const OPNAME = 'TFirmYieldToolBar.OnDeleteSeries';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meFirmYieldDeleteSeries));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFirmYieldToolBar.SetNewChartState(AEnabled: boolean);
const OPNAME = 'TFirmYieldToolBar.SetNewChartState';
begin
  try
    SetButtonEnabled(FNewChart, AEnabled, 'FirmYieldNewChartDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFirmYieldToolBar.SetOpenChartState(AEnabled: boolean);
const OPNAME = 'TFirmYieldToolBar.SetOpenChartState';
begin
  try
    SetButtonEnabled(FOpenChart, AEnabled, 'FirmYieldOpenChartDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFirmYieldToolBar.SetSaveChartState(AEnabledState: boolean);
const OPNAME = 'TFirmYieldToolBar.SetSaveChartState';
begin
  try
    SetButtonEnabled(FSaveChart, AEnabledState, 'FirmYieldSaveChartDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFirmYieldToolBar.SetAddSeriesState(AEnabledState: boolean);
const OPNAME = 'TFirmYieldToolBar.SetAddSeriesState';
begin
  try
    SetButtonEnabled(FAddSeries, AEnabledState, 'FirmYieldAddSeriesDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFirmYieldToolBar.SetDeleteSeriesState(AEnabledState: boolean);
const OPNAME = 'TFirmYieldToolBar.SetDeleteSeriesState';
begin
  try
    SetButtonEnabled(FDeleteSeries, AEnabledState, 'FirmYieldDeleteSeriesDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
