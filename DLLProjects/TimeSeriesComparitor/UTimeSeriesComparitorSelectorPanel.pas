//
//
//  UNIT      : Contains TTimeSeriesComparitorSelectorPanel Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2003/03/04
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UTimeSeriesComparitorSelectorPanel;

interface
uses
  VCLTee.TeEngine,
  VCLTee.Series,
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  UHintComboBox,
  UAbstractComponent,

  UTimeSeriesComparitorLinkClasses,
  UTimeSeriesComparitorViewList,
  UTimeSeriesComparitorChartList,
  UTimeSeriesComparitorSeriesList;

type
  TTimeSeriesComparitorSelectorPanel = class(TAbstractPanel)
  protected
    FActiveComboBox           : THintComboBox;
    FViewComboBox             : THintComboBox;
    FChartComboBox            : THintComboBox;
    FSeriesComboBox           : THintComboBox;
    FNoDataPanel              : TPanel;
    FNoDataMessage            : TLabel;
    FViewLabel                : TLabel;
    FChartLabel               : TLabel;
    FSeriesLabel              : TLabel;
    FRightYAxis               : TCheckBox;
    FZoomCurrchartOnly        : TCheckBox;
    FPlotReservoirFixedValues : TCheckBox;

    FOnSelectView             : TOnSelectView;
    FOnSelectChart            : TOnSelectChart;
    FOnSelectSeries           : TOnSelectSeries;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure CentreControls(Sender: TObject);
    procedure RightYAxisClick(Sender: TObject);
    procedure DoZoomCurrchartOnlyClick(Sender: TObject);
    procedure DoPlotReservoirValuesClick(Sender: TObject);

    function GetCurrentView: TTimeSeriesComparitorView;
    function GetCurrentChart: TTimeSeriesComparitorChart;
    function GetCurrentSeries: TTimeSeriesComparitorSeries;
    procedure IgnoreKeyPress(Sender: TObject; var Key: Char);
    procedure OnComboBoxEnter(Sender: TObject);
    procedure AssignHelpContext; override;
    function GetPlotReservoirCheckBoxEnabled(ASavedSeriesName : string) : boolean;
    function LineSeriesVisible : boolean;
  public
    destructor Destroy; override;
    procedure Resize; override;
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function ViewItems: TStrings;
    function ChartItems: TStrings;
    function SeriesItems: TStrings;

    function SelectCurrentView(AView: TTimeSeriesComparitorView): boolean;
    function AddView(AView: TTimeSeriesComparitorView): boolean;
    function DeleteView(AView: TTimeSeriesComparitorView): boolean;
    function RenameView(AView: TTimeSeriesComparitorView): boolean;

    function SelectCurrentChart(AChart: TTimeSeriesComparitorChart): boolean;
    function AddChart(AChart: TTimeSeriesComparitorChart): boolean;
    function DeleteChart(AChart: TTimeSeriesComparitorChart): boolean;
    function RenameChart(AChart: TTimeSeriesComparitorChart): boolean;

    function SelectCurrentSeries(ASeries: TTimeSeriesComparitorSeries): boolean;
    function AddSeries(ASeries: TTimeSeriesComparitorSeries): boolean;
    function DeleteSeries(ASeries: TTimeSeriesComparitorSeries): boolean;
    function RenameSeries(ASeries: TTimeSeriesComparitorSeries): boolean;

    procedure HideNoDataMessage;
    procedure EnablePlotReservoirCheckBox;
    procedure ShowNoDataMessage (AMessage : string);
    procedure RepopulateChartComboBox;
    procedure RepopulateSeriesComboBox;
    procedure ComboBoxSelect(Sender: TObject);

    property OnSelectView             : TOnSelectView               read FOnSelectView   write FOnSelectView;
    property OnSelectChart            : TOnSelectChart              read FOnSelectChart  write FOnSelectChart;
    property OnSelectSeries           : TOnSelectSeries             read FOnSelectSeries write FOnSelectSeries;

    property CurrentView              : TTimeSeriesComparitorView   read GetCurrentView;
    property CurrentChart             : TTimeSeriesComparitorChart  read GetCurrentChart;
    property CurrentSeries            : TTimeSeriesComparitorSeries read GetCurrentSeries;
    property RightYAxis               : TCheckBox                   read FRightYAxis write FRightYAxis;
    property ZoomCurrchartOnly        : TCheckBox                   read FZoomCurrchartOnly;
    property PlotReservoirFixedValues : TCheckBox                   read FPlotReservoirFixedValues;

    property ViewComboBox           : THintComboBox read FViewComboBox     write FViewComboBox;
    property ChartComboBox          : THintComboBox read FChartComboBox    write FChartComboBox;
    property SeriesComboBox         : THintComboBox read FSeriesComboBox   write FSeriesComboBox;
    property ActiveComboBox         : THintComboBox read FActiveComboBox;
  end;

const
  RESERVOIRELEVATION = 'MONTH-END RESERVOIR ELEVATION(M)';
  RESERVOIRVOLUME    = 'MONTH-END RESERVOIR VOLUME(MCM)';
  GROSSEVAPORATION   = 'GROSS EVAPORATION LOSS FROM RESERVOIR(M3/S)';
  RESERVOIRSURFACE   = 'RAINFALL ON RESERVOIR SURFACE(M3/S)';
  NETBASINRUNOFF     = 'NET BASIN RUNOFF INTO RES AREA(M3/S)';

implementation
uses
  VCL.Graphics,
  SysUtils,
  UHelpContexts,
//  TeEngine,
  UErrorHandlingOperations,
  Math;

{ TTimeSeriesComparitorSelectorPanel }

procedure TTimeSeriesComparitorSelectorPanel.CreateMemberObjects;
const OPNAME = 'TTimeSeriesComparitorSelectorPanel.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try

    FViewLabel              := TLabel.Create(Self);
    FViewLabel.Parent       := Self;
    FViewLabel.AutoSize     := False;
    FViewLabel.Width        := 38;
    FViewLabel.Alignment    := taRightJustify;

    FChartLabel             := TLabel.Create(Self);
    FChartLabel.Parent      := Self;
    FChartLabel.Width       := 38;
    FChartLabel.AutoSize    := False;
    FChartLabel.Alignment   := taRightJustify;

    FSeriesLabel             := TLabel.Create(Self);
    FSeriesLabel.Parent      := Self;
    FSeriesLabel.Width       := 38;
    FSeriesLabel.AutoSize    := False;
    FSeriesLabel.Alignment   := taRightJustify;

    FViewComboBox              := THintComboBox.Create(Self, FAppModules);
    FViewComboBox.Parent       := Self;
    FViewComboBox.OnSelect     := ComboBoxSelect;
    FViewComboBox.Style        := csDropDownList;
    FViewComboBox.AutoComplete := False;
    FViewComboBox.Sorted       := True;
    FViewComboBox.Style        := csDropDownList;
    FViewComboBox.OnEnter      := OnComboBoxEnter;
//    FViewComboBox.OnKeyPress   := IgnoreKeyPress;
//    FViewComboBox.Color        := clAqua;

    FChartComboBox              := THintComboBox.Create(Self, FAppModules);
    FChartComboBox.Parent       := Self;
    FChartComboBox.OnSelect     := ComboBoxSelect;
    FChartComboBox.Style        := csDropDownList;
    FChartComboBox.AutoComplete := False;
    FChartComboBox.Sorted       := True;
    FChartComboBox.Style        := csDropDownList;
    FChartComboBox.OnEnter      := OnComboBoxEnter;
//    FChartComboBox.OnKeyPress   := IgnoreKeyPress;
//    FChartComboBox.Color        := clFuchsia;

    FSeriesComboBox              := THintComboBox.Create(Self, FAppModules);
    FSeriesComboBox.Parent       := Self;
    FSeriesComboBox.Style        := csDropDownList;
    FSeriesComboBox.AutoCloseUp  := True;
    FSeriesComboBox.AutoDropDown := True;
    FSeriesComboBox.OnSelect     := ComboBoxSelect;
    FSeriesComboBox.AutoComplete := False;
    FSeriesComboBox.Sorted       := True;
    FSeriesComboBox.OnEnter      := OnComboBoxEnter;
//    FSeriesComboBox.OnKeyPress   := IgnoreKeyPress;
//    FSeriesComboBox.Color        := clYellow;

    FRightYAxis                  := TCheckBox.Create(Self);
    FRightYAxis.Parent           := Self;
    FRightYAxis.OnClick          := RightYAxisClick;
    FRightYAxis.OnKeyPress       := IgnoreKeyPress;
    FRightYAxis.Enabled          := False;
    FRightYAxis.Left             := FViewLabel.Left;
    FRightYAxis.Alignment        := taLeftJustify;

    FZoomCurrchartOnly           := TCheckBox.Create(Self);
    FZoomCurrchartOnly.Parent    := Self;
    FZoomCurrchartOnly.OnClick   := DoZoomCurrchartOnlyClick;

    FZoomCurrchartOnly.Left      := FViewLabel.Left + FViewLabel.Width + 10;
    FZoomCurrchartOnly.Alignment := taLeftJustify;

    FPlotReservoirFixedValues           := TCheckBox.Create(Self);
    FPlotReservoirFixedValues.Parent    := Self;
    FPlotReservoirFixedValues.OnClick   := DoPlotReservoirValuesClick;

    FPlotReservoirFixedValues.Left      := FZoomCurrchartOnly.Left + FZoomCurrchartOnly.Width + 50;
    FPlotReservoirFixedValues.Alignment := taLeftJustify;

    FNoDataPanel              := TPanel.Create(Self);
    FNoDataPanel.Parent       := Self;
    FNoDataPanel.Align        := alClient;
    FNoDataPanel.BevelInner   := bvNone;
    FNodataPanel.BevelOuter   := bvNone;

    FNoDataMessage            := TLabel.Create(Self);
    FNoDataMessage.Parent     := FNoDataPanel;
    FNoDataMessage.Align      := alClient;
    FNoDataMessage.Layout     := tlCenter;
    FNoDataMessage.Alignment  := taCenter;
    FNoDataMessage.Font.Color := clRed;
    FNoDataMessage.Font.Size  := 10;

    FViewLabel.Top       := 12;
    FViewComboBox.Top    := 10;
    FChartLabel.Top      := 12;
    FChartComboBox.Top   := 10;
    FSeriesLabel.Top     := 12;
    FSeriesComboBox.Top  := 10;
    FRightYAxis.Top      := FViewComboBox.Top + FViewComboBox.Height + 2;
    FZoomCurrchartOnly.Top := FRightYAxis.Top;
    FPlotReservoirFixedValues.Top := FRightYAxis.Top;

    FOnSelectView   := nil;
    FOnSelectChart  := nil;
    FOnSelectSeries := nil;

    Self.BevelInner := bvLowered;
    Self.BevelOuter := bvRaised;
    Self.Caption := '';
    Self.Height  := 60;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

destructor TTimeSeriesComparitorSelectorPanel.Destroy;
const OPNAME = 'TTimeSeriesComparitorSelectorPanel.BeforeDestruction';
begin
  try
    OnSelectView   := nil;
    OnSelectChart  := nil;
    OnSelectSeries := nil;
    inherited Destroy;
  except on E : Exception do HandleError(E,OPNAME); end;
end;


procedure TTimeSeriesComparitorSelectorPanel.DestroyMemberObjects;
const OPNAME = 'TTimeSeriesComparitorSelectorPanel.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSelectorPanel.Initialise: boolean;
const OPNAME = 'TTimeSeriesComparitorSelectorPanel.Initialise';
begin
  Result := inherited Initialise;
  try
    FViewComboBox.Items.Clear;
    FViewComboBox.Text := '';
    FViewComboBox.ItemIndex := -1;
    FChartComboBox.Items.Clear;
    FChartComboBox.Text := '';
    FChartComboBox.ItemIndex := -1;
    FSeriesComboBox.Items.Clear;
    FSeriesComboBox.Items.Add(' ');
    FSeriesComboBox.Text := '';
    FSeriesComboBox.ItemIndex := -1;
    FRightYAxis.Checked := False;
    FZoomCurrchartOnly.Checked := False;
    FPlotReservoirFixedValues.Checked := False;
    FPlotReservoirFixedValues.Enabled := False;
    Result := True;

  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSelectorPanel.LanguageHasChanged: boolean;
const OPNAME = 'TTimeSeriesComparitorSelectorPanel.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FViewLabel.Caption := FAppModules.Language.GetString('TSCCaptionSelectorPanel.Views')+' :';
    FChartLabel.Caption := FAppModules.Language.GetString('TSCCaptionSelectorPanel.Charts')+' :';
    FSeriesLabel.Caption := FAppModules.Language.GetString('TSCCaptionSelectorPanel.Series')+' :';
    FRightYAxis.Caption  := FAppModules.Language.GetString('TSCCaptionSelectorPanel.RightYAxis') + ' :';
    FZoomCurrchartOnly.Caption := FAppModules.Language.GetString('TSCCaptionSelectorPanel.ZoomCurrchartOnly') + ' :';
    FPlotReservoirFixedValues.Caption := FAppModules.Language.GetString('TSCCaptionSelectorPanel.PlotReservoirFixedValues') + ' :';
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorSelectorPanel.CentreControls(Sender: TObject);
const OPNAME = 'TTimeSeriesComparitorSelectorPanel.CentreControls';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorSelectorPanel.Resize;
const OPNAME = 'TTimeSeriesComparitorSelectorPanel.Resize';
{var
  LCount: integer;
  LControl: TControl;
}
begin
  inherited Resize;
  try
    FViewLabel.Left       := pnlSpace;
    FViewComboBox.Left    := FViewLabel.Left + FViewLabel.Width + pnlSpace;
    FChartLabel.Left      := FViewComboBox.Left + FViewComboBox.Width + pnlSpace;
    FChartComboBox.Left   := FChartLabel.Left + FChartLabel.Width + pnlSpace;
    FSeriesLabel.Left     := FChartComboBox.Left + FChartComboBox.Width + pnlSpace;
    FSeriesComboBox.Left  := FSeriesLabel.Left + FSeriesLabel.Width + pnlSpace;
    FSeriesComboBox.Width := Self.Width - FSeriesComboBox.Left - 5;
    FRightYAxis.Left      := pnlSpace + 3;
    FZoomCurrchartOnly.Left := (FRightYAxis.Left*10)+FRightYAxis.Width + 10;
    FZoomCurrchartOnly.Width := 150;
    FPlotReservoirFixedValues.Left := FZoomCurrchartOnly.Left + FZoomCurrchartOnly.Width + 50;
    FPlotReservoirFixedValues.Width := 250;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorSelectorPanel.ComboBoxSelect(Sender: TObject);
const OPNAME = 'TTimeSeriesComparitorSelectorPanel.ComboBoxSelect';
var
  LSeries  : TTimeSeriesComparitorSeries;
  LChart   : TTimeSeriesComparitorChart;
  LView    : TTimeSeriesComparitorView;
begin
  try
    if (Sender = FViewComboBox) then
    begin
      if Assigned(FOnSelectView) then
      begin
        if(FViewComboBox.ItemIndex >= 0) then
          LView := TTimeSeriesComparitorView(FViewComboBox.Items.Objects[FViewComboBox.ItemIndex])
        else
          LView := nil;
        OnSelectView(LView);
      end;
    end
    else
    if (Sender = FChartComboBox) then
    begin
      if Assigned(OnSelectChart) then
      begin
        if(FChartComboBox.ItemIndex >= 0) then
          LChart := TTimeSeriesComparitorChart(FChartComboBox.Items.Objects[FChartComboBox.ItemIndex])
        else
          LChart := nil;
        OnSelectChart(LChart);
      end;
    end
    else
    if (Sender = FSeriesComboBox) then
    begin
      if Assigned(OnSelectSeries) then
      begin
        if(FSeriesComboBox.ItemIndex >= 0) then
          LSeries := TTimeSeriesComparitorSeries(FSeriesComboBox.Items.Objects[FSeriesComboBox.ItemIndex])
        else
          LSeries := nil;
        OnSelectSeries(LSeries);
        if Assigned(LSeries) and
          (LSeries.LineSeries <> nil) then
        FRightYAxis.Checked := (LSeries.LineSeries.VertAxis = aRightAxis);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorSelectorPanel.SelectCurrentView(AView: TTimeSeriesComparitorView): boolean;
const OPNAME = 'TTimeSeriesComparitorSelectorPanel.SelectCurrentView';
begin
  Result := False;
  try
    if Assigned(AView) then
    begin
      if (FViewComboBox.Items.IndexOfObject(AView) >= 0) then
      begin
        FViewComboBox.Text := AView.ViewName;
        FViewComboBox.ItemIndex := FViewComboBox.Items.IndexOfObject(AView);
        ComboBoxSelect(FViewComboBox);
      end
      else
      begin
        FViewComboBox.Text := '';
        FViewComboBox.ItemIndex := -1;
      end;
    end
    else
    begin
      FViewComboBox.Text := '';
      FViewComboBox.ItemIndex := -1;
      ComboBoxSelect(FViewComboBox);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorSelectorPanel.SelectCurrentChart(AChart: TTimeSeriesComparitorChart): boolean;
const OPNAME = 'TTimeSeriesComparitorSelectorPanel.SelectCurrentChart';
begin
  Result := False;
  try
    if Assigned(AChart) then
    begin
      if (FChartComboBox.Items.IndexOfObject(AChart) >= 0) then
      begin
        FChartComboBox.Text := AChart.ChartName;
        FChartComboBox.ItemIndex := FChartComboBox.Items.IndexOfObject(AChart);
        ComboBoxSelect(FChartComboBox);
      end
      else
      begin
        FChartComboBox.Text := '';
        FChartComboBox.ItemIndex := -1;
      end;
    end
    else
    begin
        FChartComboBox.Text := '';
        FChartComboBox.ItemIndex := -1;
        ComboBoxSelect(FChartComboBox);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorSelectorPanel.SelectCurrentSeries(ASeries: TTimeSeriesComparitorSeries):boolean;
const OPNAME = 'TTimeSeriesComparitorSelectorPanel.SelectCurrentSeries';
begin
  Result := False;
  try
    if Assigned(ASeries) then
    begin
      if (FSeriesComboBox.Items.IndexOfObject(ASeries) >= 0) then
      begin
        FSeriesComboBox.Text := ASeries.CommaTextCaption;
        FSeriesComboBox.ItemIndex := FSeriesComboBox.Items.IndexOfObject(ASeries);
        ComboBoxSelect(FSeriesComboBox);
      end
      else
      begin
        FSeriesComboBox.Text := '';
        FSeriesComboBox.ItemIndex := -1;
      end;
    end
    else
    begin
        FSeriesComboBox.Text := '';
        FSeriesComboBox.ItemIndex := 0;
        ComboBoxSelect(FSeriesComboBox);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorSelectorPanel.ChartItems: TStrings;
const OPNAME = 'TTimeSeriesComparitorSelectorPanel.ChartItems';
begin
  Result := nil;
  try
    Result := FChartComboBox.Items;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorSelectorPanel.ViewItems: TStrings;
const OPNAME = 'TTimeSeriesComparitorSelectorPanel.ViewItems';
begin
  Result := nil;
  try
    Result := FViewComboBox.Items;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorSelectorPanel.GetCurrentView: TTimeSeriesComparitorView;
const OPNAME = 'TTimeSeriesComparitorSelectorPanel.GetCurrentView';
begin
  Result := nil;
  try
    if(FViewComboBox.ItemIndex >= 0) then
      Result := TTimeSeriesComparitorView(FViewComboBox.Items.Objects[FViewComboBox.ItemIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorSelectorPanel.GetCurrentChart: TTimeSeriesComparitorChart;
const OPNAME = 'TTimeSeriesComparitorSelectorPanel.GetCurrentChart';
begin
  Result := nil;
  try
    if(FChartComboBox.ItemIndex >= 0) then
      Result := TTimeSeriesComparitorChart(FChartComboBox.Items.Objects[FChartComboBox.ItemIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorSelectorPanel.GetCurrentSeries: TTimeSeriesComparitorSeries;
const OPNAME = 'TTimeSeriesComparitorSelectorPanel.GetCurrentSeries';
begin
  Result := nil;
  try
    if(FSeriesComboBox.ItemIndex >= 0) then
      Result := TTimeSeriesComparitorSeries(FSeriesComboBox.Items.Objects[FSeriesComboBox.ItemIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorSelectorPanel.AddView(AView: TTimeSeriesComparitorView): boolean;
const OPNAME = 'TTimeSeriesComparitorSelectorPanel.AddView';
begin
  Result := False;
  try
    if not Assigned(AView) then Exit;
    if(FViewComboBox.Items.IndexOf(AView.ViewName) < 0) then
      FViewComboBox.Items.AddObject(AView.ViewName,AView);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorSelectorPanel.AddChart(AChart: TTimeSeriesComparitorChart): boolean;
const OPNAME = 'TTimeSeriesComparitorSelectorPanel.AddChart';
begin
  Result := False;
  try
    if not Assigned(AChart) then Exit;
    if(FChartComboBox.Items.IndexOf(AChart.ChartName) < 0) then
      FChartComboBox.Items.AddObject(AChart.ChartName,AChart);
    Result := True;
    FRightYAxis.Enabled := (FChartComboBox.Items.Count > 0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorSelectorPanel.AddSeries(ASeries: TTimeSeriesComparitorSeries): boolean;
const OPNAME = 'TTimeSeriesComparitorSelectorPanel.AddSeries';
begin
  Result := False;
  try
    if not Assigned(ASeries) then Exit;
    if(FSeriesComboBox.Items.IndexOf(ASeries.CommaTextCaption) < 0) then
      FSeriesComboBox.Items.AddObject(ASeries.CommaTextCaption,ASeries);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorSelectorPanel.DeleteView(AView: TTimeSeriesComparitorView): boolean;
const OPNAME = 'TTimeSeriesComparitorSelectorPanel.DeleteView';
begin
  Result := False;
  try
    if FViewComboBox.Items.IndexOfObject(AView) >= 0 then
    begin
       FViewComboBox.Items.Delete(FViewComboBox.Items.IndexOfObject(AView));
       FViewComboBox.Text := '';
    end;
    if (GetCurrentView <> nil) then
      FRightYAxis.Enabled := (GetCurrentView.ChartList.ChartCount > 0)
    else
    begin
      FRightYAxis.Checked := False;
      FRightYAxis.Enabled := False;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorSelectorPanel.DeleteChart(AChart: TTimeSeriesComparitorChart): boolean;
const OPNAME = 'TTimeSeriesComparitorSelectorPanel.DeleteChart';
begin
  Result := False;
  try
    if FChartComboBox.Items.IndexOfObject(AChart) >= 0 then
    begin
       FChartComboBox.Items.Delete(FChartComboBox.Items.IndexOfObject(AChart));
       FChartComboBox.Text := '';
    end;
    if (GetCurrentView <> nil) then
      FRightYAxis.Enabled := (GetCurrentView.ChartList.ChartCount > 0);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorSelectorPanel.DeleteSeries(ASeries: TTimeSeriesComparitorSeries): boolean;
const OPNAME = 'TTimeSeriesComparitorSelectorPanel.DeleteSeries';
begin
  Result := False;
  try
    if FSeriesComboBox.Items.IndexOfObject(ASeries) >= 0 then
    begin
       FSeriesComboBox.Items.Delete(FSeriesComboBox.Items.IndexOfObject(ASeries));
       FSeriesComboBox.Text := '';
    end;
    if Assigned(CurrentSeries) then
      FRightYAxis.Checked := (CurrentSeries.LineSeries.VertAxis = aRightAxis)
    else
      FRightYAxis.Checked := False;
    if Assigned(CurrentChart) and
    (CurrentChart.SeriesList.SeriessCount = 0) then
      CurrentChart.Chart.BottomAxis.Title.Caption := '';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorSelectorPanel.RenameView(AView: TTimeSeriesComparitorView): boolean;
const OPNAME = 'TTimeSeriesComparitorSelectorPanel.RenameView';
begin
  Result := False;
  try
    if not Assigned(AView) then Exit;
    if FViewComboBox.Items.IndexOfObject(AView) >= 0 then
    begin
       FViewComboBox.Items[FViewComboBox.Items.IndexOfObject(AView)] := AView.ViewName;
       FViewComboBox.ItemIndex :=  FViewComboBox.Items.IndexOf(AView.ViewName);
       FViewComboBox.Text := AView.ViewName;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorSelectorPanel.RenameChart(AChart: TTimeSeriesComparitorChart): boolean;
const OPNAME = 'TTimeSeriesComparitorSelectorPanel.RenameChart';
begin
  Result := False;
  try
    if not Assigned(AChart) then Exit;
    if FChartComboBox.Items.IndexOfObject(AChart) >= 0 then
    begin
       FChartComboBox.Items[FChartComboBox.Items.IndexOfObject(AChart)] := AChart.ChartName;
       FChartComboBox.ItemIndex :=  FChartComboBox.Items.IndexOf(AChart.ChartName);
       FChartComboBox.Text      := AChart.ChartName;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorSelectorPanel.RenameSeries(ASeries: TTimeSeriesComparitorSeries): boolean;
const OPNAME = 'TTimeSeriesComparitorSelectorPanel.RenameSeries';
begin
  Result := False;
  try
    if not Assigned(ASeries) then Exit;
    if FSeriesComboBox.Items.IndexOfObject(ASeries) >= 0 then
    begin
       FSeriesComboBox.Items[FSeriesComboBox.Items.IndexOfObject(ASeries)] := ASeries.CommaTextCaption;
       FSeriesComboBox.ItemIndex :=  FSeriesComboBox.Items.IndexOf(ASeries.CommaTextCaption);
       FSeriesComboBox.Text := ASeries.CommaTextCaption;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorSelectorPanel.IgnoreKeyPress(Sender: TObject; var Key: Char);
const OPNAME = 'TTimeSeriesComparitorSelectorPanel.IgnoreKeyPress';
begin
  try
    Key := Char(0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorSelectorPanel.OnComboBoxEnter(Sender: TObject);
const OPNAME = 'TTimeSeriesComparitorSelectorPanel.OnComboBoxEnter';
begin
  try
    FActiveComboBox := THintComboBox(Sender);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorSelectorPanel.SeriesItems: TStrings;
const OPNAME = 'TTimeSeriesComparitorSelectorPanel.SeriesItems';
begin
  Result := nil;
  try
    Result := FSeriesComboBox.Items;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorSelectorPanel.HideNoDataMessage;
const OPNAME = 'TTimeSeriesComparitorSelectorPanel: HideNoDataMessage';
begin
  try
    FNoDataPanel.Visible := FALSE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorSelectorPanel.ShowNoDataMessage (AMessage : string);
const OPNAME = 'TTimeSeriesComparitorSelectorPanel: ShowNoDataMessage';
begin
  try
    FNoDataPanel.Visible := TRUE;
    FNoDataPanel.BringToFront;
    FNoDataMessage.Caption := AMessage;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorSelectorPanel.RepopulateChartComboBox;
const OPNAME = 'TTimeSeriesComparitorSelectorPanel: RepopulateChartComboBox';
var
  LChart     : TTimeSeriesComparitorChart;
  LChartList : TimeSeriesComparitorChartList;
  LView      : TTimeSeriesComparitorView;
  nIndexA    : integer;
begin
  try
    FChartComboBox.Items.Clear;
    LView := GetCurrentView;
    if Assigned(LView) then
    begin
      LChartList := lView.ChartList;
      for nIndexA := 0 to LChartList.ChartCount - 1 do
      begin
        LChart := TTimeSeriesComparitorChart(LChartList.ChartByIndex[nIndexA]);
        if Assigned(LChart) then
          FChartComboBox.Items.AddObject(LChart.ChartName, LChart);
      end;
      //FRightYAxis.Enabled := (LChartList.ChartCount > 0);
      if (GetCurrentView <> nil) then
        FRightYAxis.Enabled := (GetCurrentView.ChartList.ChartCount > 0);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorSelectorPanel.RepopulateSeriesComboBox;
const OPNAME = 'TTimeSeriesComparitorSelectorPanel: RepopulateSeriesComboBox';
var
  LSeries     : TTimeSeriesComparitorSeries;
  LChart      : TTimeSeriesComparitorChart;
  LSeriesList : TTimeSeriesComparitorSeriesList;
  LIndex      : integer;
begin
  try
    FSeriesComboBox.Items.Clear;
    LChart := GetCurrentChart;
    if Assigned(LChart) then
    begin
      //FSeriesComboBox.Items.Add(FAppModules.Language.GetString('TSCCaptionSelectorPanel.NoneSelected'));
      FSeriesComboBox.Items.Add('  None Selected');
      LSeriesList := LChart.SeriesList;
      for LIndex := 0 to Pred(LSeriesList.SeriessCount) do
      begin
        LSeries := TTimeSeriesComparitorSeries(LSeriesList.SeriesByIndex[LIndex]);
        FSeriesComboBox.Items.InsertObject(LIndex + 1, LSeries.CommaTextCaption, LSeries);
//        FSeriesComboBox.Items.AddObject(LSeries.CommaTextCaption, LSeries);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorSelectorPanel.RightYAxisClick(Sender: TObject);
const OPNAME = 'TTimeSeriesComparitorSelectorPanel: RightYAxisClick';
begin
  try
    if GetCurrentView <> nil then
    begin
      if FRightYAxis.Checked then
        GetCurrentView.YAxisPosition := aRightAxis
      else
        GetCurrentView.YAxisPosition := aLeftAxis;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorSelectorPanel.DoZoomCurrchartOnlyClick(Sender: TObject);
const OPNAME = 'TTimeSeriesComparitorSelectorPanel: DoZoomCurrchartOnlyClick';
begin
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorSelectorPanel.AssignHelpContext;
const OPNAME = 'TTimeSeriesComparitorSelectorPanel: AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,            HC_TimeSeriesLineGraphs);
    SetControlHelpContext(FActiveComboBox, HC_TimeSeriesLineGraphs);
    SetControlHelpContext(FViewComboBox,   HC_TimeSeriesLineGraphs);
    SetControlHelpContext(FChartComboBox,  HC_TimeSeriesLineGraphs);
    SetControlHelpContext(FSeriesComboBox, HC_TimeSeriesLineGraphs);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorSelectorPanel.GetPlotReservoirCheckBoxEnabled(ASavedSeriesName : string): boolean;
const OPNAME = 'TTimeSeriesComparitorSelectorPanel.GetPlotReservoirCheckBoxEnabled';
var
  LSeriesNames    : TStringList;
  LReservoirValue : string;
begin
  Result := False;
  try
    if(ASavedSeriesName <> '') then
    begin
      LSeriesNames := TStringList.Create;
      try
        LSeriesNames.CommaText := ASavedSeriesName;
        if(LSeriesNames.Count >= 3) then
        begin
          LReservoirValue := LSeriesNames[2];
          LReservoirValue := UpperCase(LReservoirValue);
          Result := (LReservoirValue = RESERVOIRELEVATION);
        end;
      finally
        FreeAndNil(LSeriesNames);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorSelectorPanel.EnablePlotReservoirCheckBox;
const OPNAME = 'TTimeSeriesComparitorSelectorPanel.EnablePlotReservoirCheckBox';
var
  LChart : TTimeSeriesComparitorChart;
begin
  try
    FPlotReservoirFixedValues.Enabled := False;
    LChart := GetCurrentChart;
    if LChart <> nil then
      FPlotReservoirFixedValues.Enabled := GetPlotReservoirCheckBoxEnabled(LChart.SavedSeriesName) and LineSeriesVisible;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorSelectorPanel.DoPlotReservoirValuesClick(Sender: TObject);
const OPNAME = 'TTimeSeriesComparitorSelectorPanel.DoPlotReservoirValuesClick';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorSelectorPanel.LineSeriesVisible: boolean;
const OPNAME = 'TTimeSeriesComparitorSelectorPanel.LineSeriesVisible';
var
  LChartList : TimeSeriesComparitorChartList;
  LIndex     : integer;
  LChart     : TTimeSeriesComparitorChart;
begin
  Result := False;
  try
    LChartList := CurrentView.ChartList;
    for LIndex := 0 to LChartList.ChartCount - 1 do
    begin
      LChart := LChartList.ChartByIndex[LIndex];
      Result := LChart.SeriesList.SeriessCount = 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
