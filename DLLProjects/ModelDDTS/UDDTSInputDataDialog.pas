
//
//
//  UNIT      : Contains     Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 24/07/2014
//  COPYRIGHT : Copyright © 2014 DWAF
//
//

unit UDDTSInputDataDialog;

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
  UDataComponent,
  UMenuItemManager,
  UUtilities,
  UGenericModelLinkClasses;

  const
   C_Colors    : array [0..11]of TColor              = (clMaroon,clFuchsia,clYellow,clLime,clPurple,clBlue
                                                        ,clSkyBlue,clOlive,clNavy,clGray,clRed,clGreen);
type

  TDDTSInputDataDialog = class (TAbstractScrollablePanel)
  protected
    FPanelButton              : TPanel;
    FPanelSpacer              : TPanel;
    FInputDataGrid            : TFieldStringGrid;
    FPanelMain                : TPanel;
    FPanelLocalGrid           : TPanel;
    FHorSplitter              : TSplitter;
    FPanelGraph               : TPanel;
    FInputDataGraph           : TAbstractChart;
    FElements                 : integer;
    FLineSeries               : array of TLineSeries;
    FBarSeries                : array of TBarSeries;
    FgboxDisplay               : TGroupBox;
    FRunoffChkBox             : TCheckBox;
    FOtherRunoffChkBox        : TCheckBox;
    FRainChkBox               : TCheckBox;
    FEvapChkBox               : TCheckBox;
    FDSChkBox                 : TCheckBox;
    FEWRChkBox                : TCheckBox;


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
    function LanguageHasChanged: Boolean; override;


    property PanelButton              : TPanel         read FPanelButton;
    property InputDataGrid            : TFieldStringGrid read FInputDataGrid;
    property HorSplitter              : TSplitter      read FHorSplitter;
    property PanelGraph               : TPanel         read FPanelGraph;
    property LineSeriesList[AIndex : integer]        : TLineSeries    read GetLineSeries;
    property BarSeriesList[AIndex : integer]         : TBarSeries     read GetBarSeries;
    property InputDataGraph                           : TAbstractChart read FInputDataGraph;
    property ElementsCount            : integer read FElements;
    property RunoffChkBox             : TCheckBox read FRunoffChkBox;
    property OtherRunoffChkBox        : TCheckBox read FOtherRunoffChkBox;
    property RainChkBox               : TCheckBox read FRainChkBox;
    property EvapChkBox               : TCheckBox read FEvapChkBox;
    property DSChkBox                 : TCheckBox read FDSChkBox;
    property EWRChkBox                : TCheckBox read FEWRChkBox;

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

{ TDDTSInputDataDialog }

procedure TDDTSInputDataDialog.CreateMemberObjects;
const OPNAME = 'TDDTSInputDataDialog.CreateMemberObjects';
var
  LOwner  : TComponent;
  LParent : TWinControl;
begin
  inherited;
  try
    LOwner  := ControlsOwner;
    LParent := ControlsParent;

   { FPanelSpacer             := TPanel.Create(LOwner);
    FPanelSpacer.Parent      := LParent;
    FPanelSpacer.Align       := alTop;
    FPanelSpacer.Height      := 20;
    FPanelSpacer.BorderStyle := bsNone;
    FPanelSpacer.BevelInner  := bvNone;
    FPanelSpacer.BevelOuter  := bvNone;
             }
    FPanelButton             := TPanel.Create(LOwner);
    FPanelButton.Parent      := LParent;
    FPanelButton.Align       := alTop;
    FPanelButton.Height      := 55;
    FPanelButton.Top         := 0;
    FPanelButton.BorderStyle := bsNone;
    FPanelButton.BevelInner  := bvNone;
    FPanelButton.BevelOuter  := bvNone;

    FPanelMain               := TPanel.Create(LOwner);
    FPanelMain.Parent        := LParent;
    FPanelMain.Align         := alClient;

    FPanelMain.Top           := 0;
    FPanelMain.BorderStyle   := bsNone;
    FPanelMain.BevelInner    := bvNone;
    FPanelMain.BevelOuter    := bvNone;


    FPanelLocalGrid               := TPanel.Create(LOwner);
    FPanelLocalGrid.Parent        := FPanelMain;
    FPanelLocalGrid.Align         := alTop;
    FPanelLocalGrid .Height       := 300;

    FPanelLocalGrid.Top           := 0;
    FPanelLocalGrid.BorderStyle   := bsNone;
    FPanelLocalGrid.BevelInner    := bvNone;
    FPanelLocalGrid.BevelOuter    := bvNone;

   { FPanelGrid.Align         := alTop;
    FPanelGrid.Height        := 32;
    FPanelGrid.Top           := FPanelButton.Top + FPanelButton.Height;

    FPanelGrid.Align         := alClient;
    FPanelMain.Align         := alClient;
    FPanelGrid.Visible       := False;
    GaugeGrid.Visible        := False;
    FTvwGauges.MultiSelect   := True;
    FTvwGauges.ReadOnly      := True;
    }
    FgboxDisplay              := TGroupBox.Create(ControlsOwner);
    FgboxDisplay.Parent       := FPanelButton;
    FgboxDisplay.Top          := 8;
    FgboxDisplay.Width        := 600;
    FgboxDisplay.Height       := 40;

    FRunoffChkBox             := TCheckBox.Create(LOwner);
    FRunoffChkBox.Parent      := FgboxDisplay;
    FRunoffChkBox.Left        := 12;
    FRunoffChkBox.Top         := 15;
    FRunoffChkBox.Width       := 80;
    FRunoffChkBox.Height      := 13;
    FRunoffChkBox.Alignment   := taRightJustify;

    FOtherRunoffChkBox             := TCheckBox.Create(LOwner);
    FOtherRunoffChkBox.Parent      := FgboxDisplay;
    FOtherRunoffChkBox.Left        := FRunoffChkBox.Left + FRunoffChkBox.Width + C_GroupBoxOffset;
    FOtherRunoffChkBox.Top         := 15;
    FOtherRunoffChkBox.Width       := 80;
    FOtherRunoffChkBox.Height      := 13;
    FOtherRunoffChkBox.Alignment   := taRightJustify;

    FRainChkBox               := TCheckBox.Create(LOwner);
    FRainChkBox.Parent      := FgboxDisplay;
    FRainChkBox.Left        := FOtherRunoffChkBox.Left + FOtherRunoffChkBox.Width + C_GroupBoxOffset;
    FRainChkBox.Top         := 15;
    FRainChkBox.Width       := 80;
    FRainChkBox.Height      := 13;
    FRainChkBox.Alignment   := taRightJustify;

    FEvapChkBox                 := TCheckBox.Create(LOwner);
    FEvapChkBox.Parent      := FgboxDisplay;
    FEvapChkBox.Left        := FRainChkBox.Left + FRainChkBox.Width + C_GroupBoxOffset;
    FEvapChkBox.Top         := 15;
    FEvapChkBox.Width       := 80;
    FEvapChkBox.Height      := 13;
    FEvapChkBox.Alignment   := taRightJustify;

    FDSChkBox                 := TCheckBox.Create(LOwner);
    FDSChkBox.Parent      := FgboxDisplay;
    FDSChkBox.Left        := FEvapChkBox.Left + FEvapChkBox.Width + C_GroupBoxOffset;
    FDSChkBox.Top         := 15;
    FDSChkBox.Width       := 130;
    FDSChkBox.Height      := 13;
    FDSChkBox.Alignment   := taRightJustify;

    FEWRChkBox                := TCheckBox.Create(LOwner);
    FEWRChkBox.Parent      := FgboxDisplay;
    FEWRChkBox.Left        :=  FDSChkBox.Left + FDSChkBox.Width + C_GroupBoxOffset;
    FEWRChkBox.Top         := 15;
    FEWRChkBox.Width       := 80;
    FEWRChkBox.Height      := 13;
    FEWRChkBox.Alignment   := taRightJustify;


    FInputDataGrid                  := TFieldStringGrid.Create(LOwner, FAppModules);
    FInputDataGrid.Parent           := FPanelLocalGrid;
    FInputDataGrid.FixedCols        := 0 ;
    FInputDataGrid.Top              := 0;

    FInputDataGrid.Align            := alClient;
    FInputDataGrid.Options          := FInputDataGrid.Options - [goEditing{, goRangeSelect}];
    FInputDataGrid.WrapHeaderText   := True;


    FHorSplitter         := TSplitter.Create(LOwner);
    FHorSplitter.Parent  := FPanelMain;
    FHorSplitter.Align   := alTop;
    FHorSplitter.Top     := FPanelLocalGrid.Top + FPanelLocalGrid.Height; //FPanelGrid.Top + FPanelGrid.Height;
    FHorSplitter.Height  := 4;
    FHorSplitter.Beveled := TRUE;


    FPanelGraph             := TPanel.Create(LOwner);
    FPanelGraph.Parent      := FPanelMain;
    FPanelGraph.Align       := alClient;
    FPanelGraph.BorderStyle := bsNone;
    FPanelGraph.BevelInner  := bvNone;
    FPanelGraph.BevelOuter  := bvNone;

    FInputDataGraph                               := TAbstractChart.Create(LOwner, FAppModules);
    FInputDataGraph.Parent                        := FPanelGraph;
    FInputDataGraph.Align                         := alClient;
    FInputDataGraph.BevelOuter                    := bvNone;
    FInputDataGraph.Legend.Visible                := FALSE;
    FInputDataGraph.AxisVisible                   := TRUE;
    FInputDataGraph.AllowZoom                     := TRUE;
    FInputDataGraph.AllowPanning                  := pmHorizontal;
    FInputDataGraph.Gradient.Visible              := FALSE;
    FInputDataGraph.View3D                        := FALSE;
    FInputDataGraph.Title.Visible                 := TRUE;
    FInputDataGraph.Title.Font.Style              := [fsBold];
    FInputDataGraph.Title.Font.Color              := clBlack;
    FInputDataGraph.LeftAxis.Title.Angle          := 90;
    FInputDataGraph.BottomAxis.LabelsAngle        := 90;
   // FInputDataGraph.BottomAxis.TickLength         := 6;
    FInputDataGraph.BottomAxis.DateTimeFormat     := 'yyyy/MM';
    FInputDataGraph.BottomAxis.MinorTicks.Visible := FALSE;
  except on E : Exception do HandleError(E,OPNAME); end;
end;


function TDDTSInputDataDialog.GetLineSeries(AIndex : integer) : TLineSeries;
const OPNAME = 'TDDTSInputDataDialog.GetLineSeries';
begin
  Result := nil;
  try
    if (Low(FLineSeries) <= AIndex) and (High(FLineSeries) >= AIndex) then
      Result := FLineSeries[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSInputDataDialog.GetBarSeries(AIndex : integer) : TBarSeries;
const OPNAME = 'TDDTSInputDataDialog.GetBarSeries';
begin
  Result := nil;
  try
    if (Low(FBarSeries) <= AIndex) and (High(FBarSeries) >= AIndex) then
      Result := FBarSeries[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TDDTSInputDataDialog.SetSeriesCount(AIndex : integer);
const OPNAME = 'TDDTSInputDataDialog.SetSeriesCount';
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
      FLineSeries[LIndex]               := TLineSeries.Create(FInputDataGraph);
      FLineSeries[LIndex].SeriesColor   := C_Colors[LIndex];
      FLineSeries[LIndex].ParentChart   := FInputDataGraph;
      FLineSeries[LIndex].Marks.Visible := FALSE;
      FLineSeries[LIndex].LinePen.Width := 2;
      FLineSeries[LIndex].XValues.DateTime := True;
      FLineSeries[LIndex].Clear;

      FBarSeries[LIndex]                 := TBarSeries.Create(FInputDataGraph);
      FBarSeries[LIndex].ParentChart     := FInputDataGraph;
      FBarSeries[LIndex].Marks.Visible   := FALSE;
      FBarSeries[LIndex].SeriesColor     := C_Colors[LIndex];
      FBarSeries[LIndex].XValues.DateTime := True;
      FBarSeries[LIndex].BarWidthPercent := 100;
      FBarSeries[LIndex].Clear;

    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSInputDataDialog.ClearSeries;
const OPNAME = 'TDDTSInputDataDialog.ClearLineSeries';
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

procedure TDDTSInputDataDialog.DestroyMemberObjects;
const OPNAME = 'TDDTSInputDataDialog.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    Finalize(FLineSeries);
    Finalize(FBarSeries);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSInputDataDialog.Initialise: boolean;
const OPNAME = 'TDDTSInputDataDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FInputDataGrid.ColCount := 7;
    FInputDataGrid.RowCount := 2;
    FInputDataGrid.FixedCols := 0;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSInputDataDialog.LanguageHasChanged: Boolean;
const OPNAME = 'TDDTSInputDataDialog.LanguageHasChanged';
begin
  Result := False;
  try
    FRunoffChkBox.Caption := 'Runoff';
    FOtherRunoffChkBox.Caption := 'Other Runoff';
    FRainChkBox.Caption := 'Rainfall';
    FEvapChkBox.Caption := 'Evaporation';
    FDSChkBox.Caption := 'Increamental Runoff';
    FEWRChkBox.Caption := 'EWR';
    FgboxDisplay.Caption := 'Display';
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TDDTSInputDataDialog.AssignHelpContext;
const OPNAME = 'TDDTSInputDataDialog.AssignHelpContext';
begin
  inherited;
  try
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TDDTSInputDataDialog.CanExport: boolean;
const OPNAME = 'TDDTSInputDataDialog.CanExport';
begin
  Result := False;
  try
    Result := (Assigned(FInputDataGrid) and (FInputDataGrid.Visible));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSInputDataDialog.CanPrint: boolean;
const OPNAME = 'TDDTSInputDataDialog.CanPrint';
begin
  Result := False;
  try
    Result := (Assigned(FInputDataGrid) and (FInputDataGrid.Visible));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSInputDataDialog.DoExport(AFileName: string);
const OPNAME = 'TDDTSInputDataDialog.DoExport';
begin
  try
    if FInputDataGrid.Visible then
      FInputDataGrid.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSInputDataDialog.DoPrint;
const OPNAME = 'TDDTSInputDataDialog.DoPrint';
begin
  try
    if FInputDataGrid.Visible then
      FInputDataGrid.DoPrint('');
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.

