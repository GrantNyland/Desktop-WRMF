//
//
//  UNIT      : Contains the class TOutputGraphDialog.
//  AUTHOR    : Dziedzi Ramulondi (ARIVIA)
//  DATE      : 2005/05/03
//  COPYRIGHT : Copyright © 2005 DWAF
//
//

unit UOutputGraphDialog;

interface

uses
  Classes,
  Windows,
  VCLTee.Chart,
  VCLTee.TeEngine,
  VCLTee.TeCanvas,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.CheckLst,
  VCLTee.Series,
  //VCLTee.TeEngine,
  VCL.Forms,
  VCL.Graphics,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type
  TComparisonSeries = array of TLineSeries;
  TOutputGraphDialog = class(TAbstractScrollablePanel)
  protected
    FSelectorPanel     : TPanel;
    FViewDataType      : TFieldComboBox;
    FViewDataLabel     : TLabel;
    FUnitsLabel        : TLabel;
    FBtnDataSelection  : TFieldButton;
    FDataChart         : TFieldChart;
    FLineSeries,
    FDemandLineSeries,
    FDeadLineSeries,
    FFullLevelLineSeries : TLineSeries;
    FComparisonSeries    : TComparisonSeries;
    FElements            : integer;
    FPlotActualVolume    : TCheckBox;
    FPlotActualVolumeSeries : TLineSeries;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
    procedure OnGetLineSeriesMarkText(Sender : TChartSeries ;  ValueIndex : Longint ;  Var MarkText : String);
  public
    procedure Resize; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    function CanExport : boolean; override;
    function CanPrint  : boolean; override;
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure PrepareChart;
    procedure ClearChart;

    property cmbViewDataType     : TFieldComboBox          read FViewDataType;
    property Chart               : TFieldChart             read FDataChart;
    property LineSeries          : TLineSeries             read FLineSeries;
    property DemandLineSeries    : TLineSeries             read FDemandLineSeries;
    property BtnDataSelection    : TFieldButton            read FBtnDataSelection;
    property UnitsLabel          : TLabel                  read FUnitsLabel;
    property PlotActualVolume    : TCheckBox               read FPlotActualVolume;
    property DeadLineSeries      : TLineSeries             read FDeadLineSeries;
    property FullLevelLineSeries : TLineSeries             read FFullLevelLineSeries;
    property PlotActualVolumeSeries : TLineSeries          read FPlotActualVolumeSeries;
    property ComparisonSeries    : TComparisonSeries       read FComparisonSeries;
    property Elements            : integer                 read FElements                write FElements;
    property ViewDataLabel       : TLabel                  read FViewDataLabel;
  end;

  implementation

uses
  SysUtils,
  VCLTee.TeExport,
  VCLTee.TeeProcs,
  UHelpContexts,
  UControlCreationUtilities,
  UErrorHandlingOperations;



{ TOutputGraphDialog }

procedure TOutputGraphDialog.CreateMemberObjects;
const OPNAME = 'TOutputGraphDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FSelectorPanel              := TPanel.Create(ControlsOwner);
    FSelectorPanel.Parent       := ControlsParent;

    FViewDataLabel              := TLabel.Create(ControlsOwner);
    FViewDataLabel.Parent       := FSelectorPanel;
    FViewDataLabel.Alignment    := taCenter;
    FViewDataLabel.AutoSize     := False;

    FUnitsLabel                 := TLabel.Create(ControlsOwner);
    FUnitsLabel.Parent          := FSelectorPanel;

    FPlotActualVolume           := TCheckBox.Create(ControlsOwner);
    FPlotActualVolume.Parent    := FSelectorPanel;

    FViewDataType               := TFieldComboBox.Create(ControlsOwner,FAppModules);
    FViewDataType.Parent        := FSelectorPanel;

    FBtnDataSelection           := TFieldButton.Create(ControlsOwner, FAppModules,'');
    FBtnDataSelection.Parent    := FSelectorPanel;

    FDataChart                   := TFieldChart.Create(ControlsOwner, FAppModules);
    FDataChart.Parent            := ControlsParent;
    FDataChart.View3D            := False;
    FDataChart.Legend.Visible    := False;
    FDataChart.Visible           := False;

    FLineSeries                  := TLineSeries.Create(FDataChart);

    FDeadLineSeries              := TLineSeries.Create(FDataChart);
    with FDeadLineSeries do
    begin
      Color            := clBlack;
      SeriesColor      := clBlack;
      LinePen.Style    := psSolid;
      LinePen.Width    := 1;
      XValues.DateTime := True;
      XValues.Order    := loNone;
      Pointer.Visible  := False;
      Marks.Visible    := True;
      Marks.Clip       := True;
      OnGetMarkText    := OnGetLineSeriesMarkText;
    end;

    FFullLevelLineSeries         := TLineSeries.Create(FDataChart);
    with FFullLevelLineSeries do
    begin
      SeriesColor      := clBlack;
      LinePen.Style    := psDash;
      LinePen.Width    := 1;
      XValues.DateTime := True;
      XValues.Order    := loNone;
      Pointer.Visible  := False;
      Marks.Visible    := True;
      Marks.Clip       := True;
      OnGetMarkText    := OnGetLineSeriesMarkText;
    end;

    FPlotActualVolumeSeries := TLineSeries.Create(FDataChart);
    with FPlotActualVolumeSeries do
    begin
      SeriesColor      := clMaroon;
      LinePen.Style    := psSolid;
      LinePen.Width    := 1;
      XValues.DateTime := True;
      XValues.Order    := loNone;
      Pointer.Visible  := False;
      Marks.Visible    := True;
      Marks.Clip       := True;
      Marks.Visible    := False;
      //OnGetMarkText    := OnGetLineSeriesMarkText;
    end;

    FDemandLineSeries := TLineSeries.Create(FDataChart);
    with FDemandLineSeries do
    begin
      SeriesColor      := clLime;
      LinePen.Style    := psSolid;
      LinePen.Width    := 1;
      XValues.DateTime := True;
      XValues.Order    := loNone;
      Pointer.Visible  := False;
      Marks.Visible    := True;
      Marks.Clip       := True;
      Marks.Visible    := False;
    end;

    FDataChart.AddSeries(FLineSeries);
    FDataChart.AddSeries(FDeadLineSeries);
    FDataChart.AddSeries(FFullLevelLineSeries);
    FDataChart.AddSeries(FPlotActualVolumeSeries);
    FDataChart.AddSeries(FDemandLineSeries);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputGraphDialog.Initialise: boolean;
const OPNAME = 'TOutputGraphDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FViewDataType.Clear;
    FDataChart.Initialise;
    FSelectorPanel.BorderStyle := bsSingle;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputGraphDialog.StudyHasChanged: boolean;
const OPNAME = 'TOutputGraphDialog.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    FDataChart.StudyHasChanged;
    FViewDataType.StudyHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputGraphDialog.LanguageHasChanged: boolean;
const OPNAME = 'TOutputGraphDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FDataChart.LanguageHasChanged;
    FViewDataType.LanguageHasChanged;
    FViewDataLabel.Caption := FAppModules.Language.GetString('LabelText.ViewData');
    FBtnDataSelection.Caption := FAppModules.Language.GetString('LabelText.SelectData');

    FPlotActualVolume.Caption := FAppModules.Language.GetString('LabelText.PlotActualVolume');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGraphDialog.AssignHelpContext;
const OPNAME = 'TOutputGraphDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,              HC_ResultOutput);
    SetControlHelpContext(FViewDataType,     HC_ResultOutput);
    SetControlHelpContext(FDataChart,        HC_ResultOutput);
    SetControlHelpContext(FPlotActualVolume, HC_ResultOutput);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputGraphDialog.Resize;
const OPNAME = 'TOutputGraphDialog.Resize';
begin
  try
    LockWindowUpdate(Self.Handle);
    try
      FSelectorPanel.Align        := alTop;
      FSelectorPanel.ClientHeight := 30;

      FViewDataType.Align         := alLeft;
      FViewDataType.Width         := 300;

      FViewDataLabel.Align        := alLeft;
      FViewDataLabel.Width        := 60;
      FViewDataLabel.Layout       := tlCenter;

      FUnitsLabel.Top             := 5;
      FUnitsLabel.Left            := 370;
      FUnitsLabel.Width           := 60;
      FUnitsLabel.Font.Style      := [fsBold];

      FPlotActualVolume.Top       := 5;
      FPlotActualVolume.Left      := FViewDataLabel.Width + FViewDataType.Width + FUnitsLabel.Width + FUnitsLabel.Width + (C_ControlOffset * 11);
      FPlotActualVolume.Width     := 400;
      FPlotActualVolume.Visible   := False;
      FBtnDataSelection.Align     := alRight;
      FBtnDataSelection.Width     := 80;

      FDataChart.Align            := alNone;
      FDataChart.Align            := alClient;
    finally
      LockWindowUpdate(0);
    end;
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutputGraphDialog.CanExport: boolean;
const OPNAME = 'TOutputGraphDialog.CanExport';
begin
  Result := False;
  try
    Result := (Assigned(FDataChart)            and (FDataChart.Visible));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutputGraphDialog.CanPrint: boolean;
const OPNAME = 'TOutputGraphDialog.CanPrint';
begin
  Result := False;
  try
    Result := (Assigned(FDataChart)            and (FDataChart.Visible));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputGraphDialog.DoExport(AFileName: string = '');
const OPNAME = 'TOutputGraphDialog.DoExport';
begin
  try
    if FDataChart.Visible then
      TeeExport(Self,  VCLTee.TeeProcs.TCustomTeePanel(FDataChart));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputGraphDialog.DoPrint;
const OPNAME = 'TOutputGraphDialog.DoPrint';
begin
  try
    if FDataChart.Visible then FDataChart.DoPrint;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputGraphDialog.OnGetLineSeriesMarkText(Sender: TChartSeries; ValueIndex: Integer; var MarkText: String);
const OPNAME = 'TOutputGraphDialog.OnGetLineSeriesMarkText';
var
  LDisplayStr: string;
  LXValue : Double;
  LXPos,
  LYPos: Longint;
begin
  try
    MarkText := '';
    If not Self.Visible then Exit;
    if (ValueIndex <> 0) then Exit;
    if not (Assigned(FDataChart))  then
      Exit;
    LDisplayStr := '';
    if Sender = FFullLevelLineSeries then
      LDisplayStr := FAppModules.Language.GetString('ComplienceString.SupplyLevel')
    else
    if Sender = FDeadLineSeries then
      LDisplayStr := FAppModules.Language.GetString('ComplienceString.StorageLevel')
    else
      LDisplayStr := Format('Operating Level %d', [Sender.Tag]);

    if (LDisplayStr <> '') then
    begin
      LXValue  := Sender.XValues.MaxValue - Sender.XValues.MinValue;
      LXValue  := LXValue/2.5;
      LYPos   := Sender.CalcYPosValue(Sender.YValue[ValueIndex] + 1.5);
      LXPos   := Sender.CalcXPosValue(LXValue);
      FDataChart.Canvas.Font.Style := [fsBold];
      FDataChart.BottomAxis.DrawAxisLabel(LXPos,LYPos, 0,LDisplayStr);
    end;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGraphDialog.PrepareChart;
const OPNAME = 'TOutputGraphDialog.PrepareChart';
var
  LIndex : integer;
begin
  try
    if FElements > 0 then
    begin
      SetLength(FComparisonSeries, FElements);
      for LIndex := 0 to FElements-1 do
      begin
        FComparisonSeries[LIndex]                  := TLineSeries.Create(FDataChart);
        FComparisonSeries[LIndex].ParentChart      := FDataChart;
        FComparisonSeries[LIndex].SeriesColor      := FComparisonSeries[LIndex].SeriesColor;
        FComparisonSeries[LIndex].XValues.DateTime := True;
        FComparisonSeries[LIndex].LinePen.Style    := psSolid;
        FComparisonSeries[LIndex].LinePen.Width    := 1;
        FComparisonSeries[LIndex].XValues.Order    := loNone;
        FComparisonSeries[LIndex].Pointer.Visible  := False;
        FComparisonSeries[LIndex].Visible          := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGraphDialog.ClearChart;
const OPNAME = 'TOutputGraphDialog.ClearChart';
var
  LIndex : integer;
begin
  try
    if FElements > 0 then
    begin
      for LIndex := 0 to FElements - 1 do
      begin
        if (Length(FComparisonSeries) > 0)  then
        begin
          FComparisonSeries[LIndex].Active := False;
          FComparisonSeries[LIndex].Clear;
          FComparisonSeries[LIndex].ParentChart := nil;
          FComparisonSeries[LIndex]             := nil;
        end;
      end;
    end;
 except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
