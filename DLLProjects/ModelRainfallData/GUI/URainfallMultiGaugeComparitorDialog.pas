unit URainfallMultiGaugeComparitorDialog;

interface
uses
  Classes,
  vcl.Graphics,
  vcl.Dialogs,
  vcl.Controls,
  vcl.ComCtrls,
  vcl.StdCtrls,
  Contnrs,
  VCLTee.Chart,
  VCLTee.Series,
  VCLTee.TeeProcs,
  VCLTee.TeEngine,
  VCL.extctrls,
  VCL.CheckLst,
  Math,
  VCL.Forms,
  Windows,

  UHelpContexts,
  UAbstractObject,
  UDataEditComponent,
  UAbstractComponent,
  URainfallCommonGaugeSheet,
  URainfallGaugeStatsMenuItemManager,
  UMenuItemManager,
  UUtilities,
  UGenericModelLinkClasses;

  const
   C_Colors    : array [0..11]of TColor              = (clMaroon,clFuchsia,clYellow,clLime,clPurple,clBlue
                                                        ,clSkyBlue,clOlive,clNavy,clGray,clRed,clGreen);
type

  TRainfallMultiGaugeComparitorDialog = class (TRainfallCommonGaugeSheet)
  protected
    FPanelButton              : TPanel;
    FPanelSpacer              : TPanel;
    FMultipleGaugeCompareGrid : TFieldStringGrid;
    FPanelMain                : TPanel;
    FPanelLocalGrid           : TPanel;
    FHorSplitter              : TSplitter;
    FPanelGraph               : TPanel;
    FRainfallGraph            : TAbstractChart;
    FElements                 : integer;
    FLineSeries               : array of TLineSeries;
    FBarSeries                : array of TBarSeries;
   procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
    function GetLineSeries(AIndex : integer) : TLineSeries;
    function GetBarSeries(AIndex : integer) : TBarSeries;
  public
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    function CanExport : boolean; override;
    function CanPrint  : boolean; override;
    function Initialise: boolean; override;
    procedure SetSeriesCount(aIndex : integer);
    procedure ClearSeries;

    property PanelButton              : TPanel         read FPanelButton;
    property MultipleGaugeCompareGrid : TFieldStringGrid read FMultipleGaugeCompareGrid;
    property HorSplitter              : TSplitter      read FHorSplitter;
    property PanelGraph               : TPanel         read FPanelGraph;
    property LineSeriesList[AIndex : integer]        : TLineSeries    read GetLineSeries;
    property BarSeriesList[AIndex : integer]         : TBarSeries     read GetBarSeries;
    property RainfallGraph                           : TAbstractChart read FRainfallGraph;
  end;

implementation

uses
  SysUtils,
  VCL.ImgList,
//  FileCtrl,
  VCL.Grids,
  UDatasetType,
  VCL.Printers,
  UConstants,
  UErrorHandlingOperations;

{ TRainfallMultiGaugeComparitorDialog }

procedure TRainfallMultiGaugeComparitorDialog.CreateMemberObjects;
const OPNAME = 'TRainfallMultiGaugeComparitorDialog.CreateMemberObjects';
begin
  inherited;
  try

    FPanelSpacer             := TPanel.Create(Self);
    FPanelSpacer.Parent      := FPanelLeft;
    FPanelSpacer.Align       := alTop;
    FPanelSpacer.Height      := 20;
    FPanelSpacer.BorderStyle := bsNone;
    FPanelSpacer.BevelInner  := bvNone;
    FPanelSpacer.BevelOuter  := bvNone;

    FPanelButton             := TPanel.Create(Self);
    FPanelButton.Parent      := FPanelClient;
    FPanelButton.Align       := alTop;
    FPanelButton.Height      := 20;
    FPanelButton.Top         := 0;
    FPanelButton.BorderStyle := bsNone;
    FPanelButton.BevelInner  := bvNone;
    FPanelButton.BevelOuter  := bvNone;

    FPanelMain               := TPanel.Create(Self);
    FPanelMain.Parent        := FPanelClient;
    FPanelMain.Align         := alClient;

    FPanelMain.Top           := 0;
    FPanelMain.BorderStyle   := bsNone;
    FPanelMain.BevelInner    := bvNone;
    FPanelMain.BevelOuter    := bvNone;


    FPanelLocalGrid               := TPanel.Create(Self);
    FPanelLocalGrid.Parent        := FPanelMain;
    FPanelLocalGrid.Align         := alTop;
    FPanelLocalGrid .Height       := 300;

    FPanelLocalGrid.Top           := 0;
    FPanelLocalGrid.BorderStyle   := bsNone;
    FPanelLocalGrid.BevelInner    := bvNone;
    FPanelLocalGrid.BevelOuter    := bvNone;

    FPanelGrid.Align         := alTop;
    FPanelGrid.Height        := 32;
    FPanelGrid.Top           := FPanelButton.Top + FPanelButton.Height;

    FPanelGrid.Align         := alClient;
    FPanelMain.Align         := alClient;
    FPanelGrid.Visible       := False;
    GaugeGrid.Visible        := False;
    FTvwGauges.MultiSelect   := True;
    FTvwGauges.ReadOnly      := True;

    FMultipleGaugeCompareGrid                  := TFieldStringGrid.Create(Self, FAppModules);
    FMultipleGaugeCompareGrid.Parent           := FPanelLocalGrid;
    FMultipleGaugeCompareGrid.FixedCols        := 0 ;
    FMultipleGaugeCompareGrid.Top              := 0;

    FMultipleGaugeCompareGrid.Align            := alClient;
    FMultipleGaugeCompareGrid.Options          := FMultipleGaugeCompareGrid.Options - [goEditing{, goRangeSelect}];
    FMultipleGaugeCompareGrid.WrapHeaderText   := True;


    FHorSplitter         := TSplitter.Create(Self);
    FHorSplitter.Parent  := FPanelMain;
    FHorSplitter.Align   := alTop;
    FHorSplitter.Top     := FPanelLocalGrid.Top + FPanelLocalGrid.Height; //FPanelGrid.Top + FPanelGrid.Height;
    FHorSplitter.Height  := 4;
    FHorSplitter.Beveled := TRUE;


    FPanelGraph             := TPanel.Create(Self);
    FPanelGraph.Parent      := FPanelMain;
    FPanelGraph.Align       := alClient;
    FPanelGraph.BorderStyle := bsNone;
    FPanelGraph.BevelInner  := bvNone;
    FPanelGraph.BevelOuter  := bvNone;

    FRainfallGraph                               := TAbstractChart.Create(Self, FAppModules);
    FRainfallGraph.Parent                        := FPanelGraph;
    FRainfallGraph.Align                         := alClient;
    FRainfallGraph.BevelOuter                    := bvNone;
    FRainfallGraph.Legend.Visible                := FALSE;
    FRainfallGraph.AxisVisible                   := TRUE;
    FRainfallGraph.AllowZoom                     := TRUE;
    FRainfallGraph.AllowPanning                  := pmHorizontal;
    FRainfallGraph.Gradient.Visible              := FALSE;
    FRainfallGraph.View3D                        := FALSE;
    FRainfallGraph.Title.Visible                 := TRUE;
    FRainfallGraph.Title.Font.Style              := [fsBold];
    FRainfallGraph.Title.Font.Color              := clBlack;
    FRainfallGraph.LeftAxis.Title.Angle          := 90;
    FRainfallGraph.BottomAxis.LabelsAngle        := 90;
   // FRainfallGraph.BottomAxis.TickLength         := 6;
    FRainfallGraph.BottomAxis.DateTimeFormat     := 'yyyy/MM';
    FRainfallGraph.BottomAxis.MinorTicks.Visible := FALSE;
  except on E : Exception do HandleError(E,OPNAME); end;
end;


function TRainfallMultiGaugeComparitorDialog.GetLineSeries(AIndex : integer) : TLineSeries;
const OPNAME = 'TRainfallMultiGaugeComparitorDialog.GetLineSeries';
begin
  Result := nil;
  try
    if (Low(FLineSeries) <= AIndex) and (High(FLineSeries) >= AIndex) then
      Result := FLineSeries[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallMultiGaugeComparitorDialog.GetBarSeries(AIndex : integer) : TBarSeries;
const OPNAME = 'TRainfallMultiGaugeComparitorDialog.GetBarSeries';
begin
  Result := nil;
  try
    if (Low(FBarSeries) <= AIndex) and (High(FBarSeries) >= AIndex) then
      Result := FBarSeries[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TRainfallMultiGaugeComparitorDialog.SetSeriesCount(AIndex : integer);
const OPNAME = 'TRainfallMultiGaugeComparitorDialog.SetSeriesCount';
var
  LIndex : integer;
begin
  try
  //  ClearSeries;
    FElements := AIndex;
    SetLength(FLineSeries, AIndex);
    SetLength(FBarSeries, AIndex);
    for LIndex := Low(FLineSeries) to High(FLineSeries) do
    begin
      FLineSeries[LIndex]               := TLineSeries.Create(FRainfallGraph);
      FLineSeries[LIndex].SeriesColor   := C_Colors[LIndex];  //random(color);
      FLineSeries[LIndex].ParentChart   := FRainfallGraph;
      FLineSeries[LIndex].Marks.Visible := FALSE;
      FLineSeries[LIndex].LinePen.Width := 2;
      FLineSeries[LIndex].XValues.DateTime := True;
      FLineSeries[LIndex].Clear;

      FBarSeries[LIndex]                 := TBarSeries.Create(FRainfallGraph);
      FBarSeries[LIndex].ParentChart     := FRainfallGraph;
      FBarSeries[LIndex].Marks.Visible   := FALSE;
      FBarSeries[LIndex].SeriesColor     := C_Colors[LIndex];  //random(color);
      FBarSeries[LIndex].XValues.DateTime := True;
      FBarSeries[LIndex].BarWidthPercent := 100;
      FBarSeries[LIndex].Clear;

    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallMultiGaugeComparitorDialog.ClearSeries;
const OPNAME = 'TRainfallMultiGaugeComparitorDialog.ClearLineSeries';
var
  LIndex : integer;
begin
  try

      for LIndex := Low(FLineSeries) to High(FLineSeries) do
      begin

          FLineSeries[LIndex].Active := False;
          FLineSeries[LIndex].Clear;
          FLineSeries[LIndex].ParentChart := nil;
          FLineSeries[LIndex]             := nil;

          FBarSeries[LIndex].Active := False;
          FBarSeries[LIndex].Clear;
          FBarSeries[LIndex].ParentChart := nil;
          FBarSeries[LIndex]             := nil;

      end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallMultiGaugeComparitorDialog.DestroyMemberObjects;
const OPNAME = 'TRainfallMultiGaugeComparitorDialog.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    Finalize(FLineSeries);
    Finalize(FBarSeries);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallMultiGaugeComparitorDialog.Initialise: boolean;
const OPNAME = 'TRainfallMultiGaugeComparitorDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FMultipleGaugeCompareGrid.ColCount := 3;
    FMultipleGaugeCompareGrid.RowCount := 2;
    FMultipleGaugeCompareGrid.FixedCols := 2;

    FMultipleGaugeCompareGrid.Cells[0,0] := 'Year';
    FMultipleGaugeCompareGrid.Cells[1,0] := 'Month';

    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallMultiGaugeComparitorDialog.AssignHelpContext;
const OPNAME = 'TRainfallMultiGaugeComparitorDialog.AssignHelpContext';
begin
  inherited;
  try
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TRainfallMultiGaugeComparitorDialog.CanExport: boolean;
const OPNAME = 'TRainfallMultiGaugeComparitorDialog.CanExport';
begin
  Result := False;
  try
    Result := (Assigned(FMultipleGaugeCompareGrid) and (FMultipleGaugeCompareGrid.Visible));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallMultiGaugeComparitorDialog.CanPrint: boolean;
const OPNAME = 'TRainfallMultiGaugeComparitorDialog.CanPrint';
begin
  Result := False;
  try
    Result := (Assigned(FMultipleGaugeCompareGrid) and (FMultipleGaugeCompareGrid.Visible));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallMultiGaugeComparitorDialog.DoExport(AFileName: string);
const OPNAME = 'TRainfallMultiGaugeComparitorDialog.DoExport';
begin
  try
    if FMultipleGaugeCompareGrid.Visible then
      FMultipleGaugeCompareGrid.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallMultiGaugeComparitorDialog.DoPrint;
const OPNAME = 'TRainfallMultiGaugeComparitorDialog.DoPrint';
begin
  try
    if FMultipleGaugeCompareGrid.Visible then
      FMultipleGaugeCompareGrid.DoPrint('');
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
 