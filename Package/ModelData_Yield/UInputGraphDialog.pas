//
//
//  UNIT      : Contains the class TInputGraphDialog.
//  AUTHOR    : Dziedzi Ramulondi (ARIVIA)
//  DATE      : 2005/05/03
//  COPYRIGHT : Copyright © 2005 DWAF
//
//

unit UInputGraphDialog;

interface

uses
  Classes,
  Windows,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.CheckLst,
  VCLTee.Series,
  VCL.Forms,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type
  TInputGraphDialog = class(TAbstractScrollablePanel)
  protected
    FDataChart           : TFieldChart;
    FLineSeriesA,
    FLineSeries          : TLineSeries;
    FCheckBoxPanel       : TAbstractPanel;
    FMeanLabel           : TLabel;

    FMeanChkbox,
    FStdDeviationChkBox  : TCheckBox;

    FMeanLineSeries,
    FStdDevLineSeriesMAbove,
    FStdDevLineSeriesMBelow,
    FMeanLineSeriesA,
    FStdDevLineSeriesAAbove,
    FStdDevLineSeriesABelow   : TLineSeries;

    FMonthlyAnnual       : TFieldRadioGroup;

    procedure CalculateLeftAxisMinMax(ALineSeries : TLineSeries);

    procedure CreateMemberObjects;  override;
    procedure AssignHelpContext;    override;
  public
    procedure Resize;                       override;
    function StudyHasChanged:     boolean;  override;
    function LanguageHasChanged:  boolean;  override;
    function Initialise:          boolean;  override;

    property Chart                    : TFieldChart       read FDataChart;
    property LineSeries               : TLineSeries       read FLineSeries;
    property LineSeriesA              : TLineSeries       read FLineSeriesA;
    property MeanLineSeries           : TLineSeries       read FMeanLineSeries;
    property MeanLineSeriesA          : TLineSeries       read FMeanLineSeriesA;
    property StdDevLineSeriesMAbove   : TLineSeries       read FStdDevLineSeriesMAbove;
    property StdDevLineSeriesMBelow   : TLineSeries       read FStdDevLineSeriesMBelow;
    property StdDevLineSeriesAAbove   : TLineSeries       read FStdDevLineSeriesAAbove;
    property StdDevLineSeriesABelow   : TLineSeries       read FStdDevLineSeriesABelow;

    property MeanCheckBox             : TCheckBox         read FMeanChkbox;
    property StdDevCheckBox           : TCheckBox         read FStdDeviationChkBox;
    property MeanLabel                : TLabel            read FMeanLabel;
    property MonthlyAnnual            : TFieldRadioGroup  read FMonthlyAnnual;
  end;

  implementation

uses
  SysUtils,
  VCL.Graphics,
  UHelpContexts,
  UControlCreationUtilities,
  UErrorHandlingOperations,
  VCLTee.TeEngine,
  VCLTee.Chart,
  VCLTee.TeeProcs;

{ TInputGraphDialog }

procedure TInputGraphDialog.CreateMemberObjects;
const OPNAME = 'TInputGraphDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try

    FCheckBoxPanel               := TAbstractPanel.Create(ControlsOwner, FAppModules);
    FCheckBoxPanel.Parent        := ControlsParent;

    FMonthlyAnnual               := TFieldRadioGroup.Create(FCheckBoxPanel, FAppModules);
    FMonthlyAnnual.Parent        := FCheckBoxPanel;
    FMonthlyAnnual.Columns       := 2;

    FMeanChkbox                  := TCheckBox.Create(FCheckBoxPanel);
    FMeanChkbox.Parent           := FCheckBoxPanel;

    FStdDeviationChkBox          := TCheckBox.Create(FCheckBoxPanel);
    FStdDeviationChkBox.Parent   := FCheckBoxPanel;

    FMeanLabel                   := TLabel.Create(FCheckBoxPanel);
    FMeanLabel.Parent            := FCheckBoxPanel;

    FDataChart                   := TFieldChart.Create(ControlsOwner, FAppModules);
    FDataChart.Parent            := ControlsParent;

    FLineSeries                  := TLineSeries.Create(FDataChart);
    FLineSeries.Tag              := 0;
    FLineSeries.ShowInLegend     := False;
    FLineSeries.Color            := clRed;
    FDataChart.AddSeries(FLineSeries);

    FLineSeriesA                  := TLineSeries.Create(FDataChart);
    FLineSeriesA.Tag              := 1;
    FLineSeriesA.ShowInLegend     := False;
    FLineSeriesA.Color            := clMaroon;
    FDataChart.AddSeries(FLineSeriesA);

    FMeanLineSeries               := TLineSeries.Create(FDataChart);
    FMeanLineSeries.Tag           := 2;
    FMeanLineSeries.ShowInLegend  := True;
    FMeanLineSeries.Color         := clGreen;
    FDataChart.AddSeries(FMeanLineSeries);

    FMeanLineSeriesA               := TLineSeries.Create(FDataChart);
    FMeanLineSeriesA.Tag           := 3;
    FMeanLineSeriesA.ShowInLegend  := False;
    FMeanLineSeriesA.Color         := clNavy;
    FDataChart.AddSeries(FMeanLineSeriesA);

    FStdDevLineSeriesMAbove               := TLineSeries.Create(FDataChart);
    FStdDevLineSeriesMAbove.Tag           := 4;
    FStdDevLineSeriesMAbove.ShowInLegend  := False;
    FStdDevLineSeriesMAbove.Color         := clPurple;
    FDataChart.AddSeries(FStdDevLineSeriesMAbove);

    FStdDevLineSeriesMBelow               := TLineSeries.Create(FDataChart);
    FStdDevLineSeriesMBelow.Tag           := 5;
    FStdDevLineSeriesMBelow.ShowInLegend  := False;
    FStdDevLineSeriesMBelow.Color         := clTeal;
    FDataChart.AddSeries(FStdDevLineSeriesMBelow);

    FStdDevLineSeriesAAbove               := TLineSeries.Create(FDataChart);
    FStdDevLineSeriesAAbove.Tag           := 6;
    FStdDevLineSeriesAAbove.ShowInLegend  := False;
    FStdDevLineSeriesAAbove.Color         := clGray;
    FDataChart.AddSeries(FStdDevLineSeriesAAbove);

    FStdDevLineSeriesABelow               := TLineSeries.Create(FDataChart);
    FStdDevLineSeriesABelow.Tag           := 7;
    FStdDevLineSeriesABelow.ShowInLegend  := False;
    FStdDevLineSeriesABelow.Color         := clYellow;
    FDataChart.AddSeries(FStdDevLineSeriesABelow);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TInputGraphDialog.Initialise: boolean;
const OPNAME = 'TInputGraphDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FDataChart.Initialise;
    FDataChart.View3D            := False;

    FDataChart.Legend.Alignment  := laBottom;
    FDataChart.Legend.Visible    := True;

    FMonthlyAnnual.Items.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TInputGraphDialog.StudyHasChanged: boolean;
const OPNAME = 'TInputGraphDialog.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    FDataChart.StudyHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TInputGraphDialog.LanguageHasChanged: boolean;
const OPNAME = 'TInputGraphDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FDataChart.LanguageHasChanged;
    FMeanChkbox.Caption          := FAppModules.Language.GetString('LineSeriesCheckbox.Mean');
    FStdDeviationChkBox.Caption  := FAppModules.Language.GetString('LineSeriesCheckbox.StandardDeviation');

    FMonthlyAnnual.Items.Clear;
    FMonthlyAnnual.Items.Add('Monthly');
    FMonthlyAnnual.Items.Add('Annual');

    FMonthlyAnnual.ItemIndex := 0;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInputGraphDialog.AssignHelpContext;
const OPNAME = 'TInputGraphDialog.AssignHelpContext';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TInputGraphDialog.Resize;
const OPNAME = 'TInputGraphDialog.Resize';
begin
  inherited;
  FDataChart.Align               := alNone;
  FCheckBoxPanel.Align           := alNone;
  FCheckBoxPanel.Width           := Self.ClientWidth;
  try
    FCheckBoxPanel.Align         := alTop;
    FCheckBoxPanel.Height        := 35;
    FMeanLabel.Left              := 5;

    FMeanChkbox.Top              := (FCheckBoxPanel.Height - FMeanChkbox.Height) div 2;
    FStdDeviationChkBox.Top      := (FCheckBoxPanel.Height - FStdDeviationChkBox.Height) div 2;
    FMeanChkbox.Width            := 115;
    FStdDeviationChkBox.Width    := FMeanChkbox.Width;
    FStdDeviationChkBox.Left     := FCheckBoxPanel.Width - FStdDeviationChkBox.Width -5;
    FMeanChkbox.Left             := FStdDeviationChkBox.Left - FMeanChkbox.Width -5;
    FMeanLabel.Top               := FMeanChkbox.Top;

    FMonthlyAnnual.Height        := FCheckBoxPanel.Height;
    FMonthlyAnnual.Top           := 0;//(FCheckBoxPanel.Height - FMonthlyAnnual.Height) div 2;
    FMonthlyAnnual.Left          := 0;
    FMonthlyAnnual.Width         := 300;//FCheckBoxPanel.ClientWidth * 3 div 16;

    FDataChart.Align             := alClient;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TInputGraphDialog.CalculateLeftAxisMinMax(ALineSeries: TLineSeries);
const OPNAME = 'TInputGraphDialog.CalculateLeftAxisMinMax';
var
  LValue,
  LMinValue,
  LMaxValue  : double;
begin
  try
    if Assigned(ALineSeries) then
    begin
      LValue    := (ALineSeries.MaxYValue - ALineSeries.MinYValue) * 0.1;
      LMinValue := ALineSeries.MinYValue - LValue;
      LMaxValue := ALineSeries.MaxYValue + LValue;

      if (LMinValue > FDataChart.LeftAxis.Maximum) then
      begin
        FDataChart.LeftAxis.Maximum := LMaxValue;
        FDataChart.LeftAxis.Minimum := LMinValue;
      end;
      if (LMaxValue < FDataChart.LeftAxis.Minimum) then
      begin
        FDataChart.LeftAxis.Minimum := LMinValue;
        FDataChart.LeftAxis.Maximum := LMaxValue;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
