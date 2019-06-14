//
//
//  UNIT      : Contains the class TSystemYieldStochasticDialog.
//  AUTHOR    : Sam Dhlamini(ARIVIA)
//  DATE      : 2005/02/15
//  COPYRIGHT : Copyright © 2003 DWAF
//
//

unit USystemYieldStochasticDialog;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.Graphics,
  VCLTee.Series,
  VCLTee.TeEngine,
  //UYRCChart,
  UResultYRCSheet,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type
  TStochasticLineSeries = array[0..9] of TLineSeries;
  TSystemYieldStochasticDialog = class(TAbstractScrollablePanel)
  protected
    FGroupBox : TGroupBox;
    FlblStochastic : TLabel;
    FlblNumberOfSeqAnalysed : TLabel;
    FStochasticGrid : TFieldStringGrid;
    FStochasticPointSeries : TPointSeries;
    FStochasticLineSeries : TStochasticLineSeries;
    FElements : integer;
    FClientPanel : TPanel;

    FStochasticPageControl : TAbstractPageControl;
    FGrid : TTabSheet;
    FGraph : TTabSheet;

    FStochasticGraph :TResultYRCSheet;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
    procedure Resize; override;
  public
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    //procedure PrepareChart;
    //procedure ClearChart;

    property StochasticGrid: TFieldStringGrid read FStochasticGrid;
    property lblStochastic : TLabel read FlblStochastic;
    property lblNumberOfSeqAnalysed : TLabel read FlblNumberOfSeqAnalysed;
    property StochasticPointSeries : TPointSeries read FStochasticPointSeries;
    property StochasticLineSeries : TStochasticLineSeries read FStochasticLineSeries;
    property StochasticGraph :TResultYRCSheet read FStochasticGraph;

  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  UConstants,
  UControlCreationUtilities,
  UErrorHandlingOperations, VCL.Grids, Math;

{ TSystemYieldStochasticDialog }

procedure TSystemYieldStochasticDialog.CreateMemberObjects;
const OPNAME = 'TSystemYieldStochasticDialog.CreateMemberObjects';
begin
  inherited;
  try

    FClientPanel := TPanel.Create(ControlsOwner);
    FClientPanel.Parent := ControlsParent;
    FClientPanel. Align := alClient;

    FStochasticPageControl := TAbstractPageControl.Create(ControlsOwner,FAppModules);
    FStochasticPageControl.Parent := FClientPanel;
    FGrid := TTabSheet.Create(ControlsOwner);
    FGraph   := TTabSheet.Create(ControlsOwner);

    FGrid.Parent  := FStochasticPageControl;
    FGraph.Parent    := FStochasticPageControl;

    FStochasticGraph := TResultYRCSheet.Create(ControlsOwner, FAppModules,True);
    FStochasticGraph.Parent := FGraph;
    StochasticGraph.Visible := True;
    
    FGroupBox               := TGroupBox.Create(ControlsOwner);
    FGroupBox.Parent        := FClientPanel;
    FGroupBox.Top           := 0;
    FGroupBox.Left          := 0;
    FGroupBox.Height        := 50;

    FlblStochastic          := TLabel.Create(ControlsOwner);
    FlblStochastic.Parent   := FGroupBox;
    FlblStochastic.Top      := 10;
    FlblStochastic.Left     := 10;

    FlblNumberOfSeqAnalysed          := TLabel.Create(ControlsOwner);
    FlblNumberOfSeqAnalysed.Parent   := FGroupBox;
    FlblNumberOfSeqAnalysed.Top      := 30;
    FlblNumberOfSeqAnalysed.Left     := 10;

    FStochasticGrid               := TFieldStringGrid.Create(ControlsOwner,FAppModules);
    FStochasticGrid.Parent        := FGrid;
    FStochasticGrid.RowCount      := 2;
    FStochasticGrid.ColCount      := 4;
    FStochasticGrid.FixedRows     := 1;
    FStochasticGrid.FixedCols     := 0;
    FStochasticGrid.Alignment     := taCenter;
    FStochasticGrid.Options       := FStochasticGrid.Options - [goEditing	];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSystemYieldStochasticDialog.DestroyMemberObjects;
const OPNAME = 'TSystemYieldStochasticDialog.DestroyMemberObjects';
begin
  try
    inherited;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemYieldStochasticDialog.Initialise: boolean;
const OPNAME = 'TSystemYieldStochasticDialog.Initialise';
var
  lIndex: integer;
begin
  Result := inherited Initialise;
  try
    FlblStochastic.Font.Style := [fsUnderline,fsBold];
    for lIndex := 0 to FStochasticGrid.ColCount - 1 do
      FStochasticGrid.ColWidths[lIndex] := FStochasticGrid.ColWidths[lIndex] + 60;
    //PrepareChart;

    FGrid.PageControl :=FStochasticPageControl;
    FGraph.PageControl := FStochasticPageControl;

    FGrid.Align := alClient;
    FGrid.Caption := 'Grid';
    FGraph.Align   := alClient;
    FGraph.Caption := 'Graph';

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemYieldStochasticDialog.LanguageHasChanged: boolean;
const OPNAME = 'TSystemYieldStochasticDialog.LanguageHasChanged';
begin
  Result := True;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSystemYieldStochasticDialog.AssignHelpContext;
const OPNAME = 'TSystemYieldStochasticDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,             HC_DeterminingTheSystemYield);
    SetControlHelpContext(FStochasticGrid,    HC_DeterminingTheSystemYield);
  except on E: Exception do HandleError(E, OPNAME); end;
end;
{procedure TSystemYieldStochasticDialog.PrepareChart;
const OPNAME = 'TSystemYieldStochasticDialog.PrepareChart';
var
  LIndex : integer;
begin
  try
    FStochasticPointSeries                  := TPointSeries.Create(FStochasticGraph);
    FStochasticPointSeries.ParentChart      := FStochasticGraph;
    FStochasticPointSeries.LinePen.Style    := psSolid;
    FStochasticPointSeries.LinePen.Width    := 2;
    FStochasticPointSeries.XValues.Order    := loNone;
    FStochasticPointSeries.Pointer.Visible  := True;
    FStochasticPointSeries.Visible          := True;
    FStochasticPointSeries.Marks.Visible := False;
    FStochasticPointSeries.LinePen.Width := 2;
    FStochasticPointSeries.Pointer.Size := 2;
    FStochasticPointSeries.Clear;
    for LIndex := 0 to 9 do
    begin
      FStochasticLineSeries[LIndex]                  := TLineSeries.Create(FStochasticGraph);
      FStochasticLineSeries[LIndex].ParentChart      := FStochasticGraph;
      FStochasticLineSeries[LIndex].LinePen.Style    := psSolid;
      FStochasticLineSeries[LIndex].LinePen.Width    := 1;
      FStochasticLineSeries[LIndex].XValues.Order    := loNone;
      FStochasticLineSeries[LIndex].Visible          := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSystemYieldStochasticDialog.ClearChart;
const OPNAME = 'TSystemYieldStochasticDialog.ClearChart';
var
  LIndex : integer;
begin
  try
    for LIndex := 0 to 9 do
    begin
      FStochasticLineSeries[LIndex].Active := False;
      FStochasticLineSeries[LIndex].Clear;
      FStochasticLineSeries[LIndex].ParentChart := nil;
      FStochasticLineSeries[LIndex]             := nil;
    end;
    FStochasticPointSeries.Active := False;
    FStochasticPointSeries.Clear;
    FStochasticPointSeries.ParentChart := nil;
    FStochasticPointSeries             := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

procedure TSystemYieldStochasticDialog.Resize;
const OPNAME = 'TSystemYieldStochasticDialog.Resize';
begin
  inherited Resize;
  try
    FStochasticGrid.Align := alClient;
    FStochasticGraph.Align := alClient;
    FStochasticPageControl.Top := FGroupBox.Height;
    FStochasticPageControl.Width := ClientWidth;
    FStochasticPageControl.Height := ClientHeight- FGroupBox.Height;
    FStochasticGrid.Height := ClientHeight- FGroupBox.Height;
    FGroupBox.Width := ClientWidth;
    {
    FStochasticGrid.Left :=0;

    FStochasticGrid.Top := FGroupBox.Height;//+ C_ControlBorder;
    FStochasticGrid.Width := (ClientWidth div 2);
    FStochasticGrid.Height := ClientHeight- FGroupBox.Height;
    FGroupBox.Width := FStochasticGrid.Width;
    FStochasticGraph.Left := FStochasticGrid.Width + C_GroupBoxOffset;
    FStochasticGraph.Width := FStochasticGrid.Width;
    FStochasticGraph.Height := FStochasticGrid.Height+FGroupBox.Height;
    FStochasticGraph.Top := 0;
    }
  except on E: Exception do HandleError(E, OPNAME) end;
end;
{
procedure TReservoirChannelComparitor.RefreshChartAxisDisplay(AChart: TFieldChart);
const OPNAME = 'TReservoirChannelComparitor.RefreshChartAxisDisplay';
var
  LDiff,
  LMin,
  LMax : double;
begin
  try
    AChart.LeftAxis.Automatic := True;
    AChart.LeftAxis.CalcMinMax(LMin, LMax);
    LDiff := (LMax - LMin);
    AChart.LeftAxis.Automatic := False;
    if (LMin > AChart.LeftAxis.Maximum) then
    begin
      AChart.LeftAxis.Maximum := LMax + (LDiff * 0.2);
      AChart.LeftAxis.Minimum := LMin - (LDiff * 0.2);
    end
    else
    if (LMax < AChart.LeftAxis.Minimum) then
    begin
      AChart.LeftAxis.Minimum := LMin - (LDiff * 0.2);
      AChart.LeftAxis.Maximum := LMax + (LDiff * 0.2);
    end
    else
    begin
      AChart.LeftAxis.Maximum := LMax + (LDiff * 0.2);
      AChart.LeftAxis.Minimum := LMin - (LDiff * 0.2);
    end;;
    AChart.LeftAxis.Increment := (LDiff * 0.1);
  except on E: Exception do HandleError(E, OPNAME) end;
end;
 }

end.
