{******************************************************************************}
{*  UNIT      : Contains the class TRainfallGraphDialog.                      *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/04/28                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit URainfallGraphDialog;

interface

uses
  VCL.Forms,
  VCL.Controls,
  VCL.Graphics,
  VCL.Buttons,
  VCL.ExtCtrls,
  VCL.Grids,
  VCL.ComCtrls,
  Types,
  VCL.Menus,
  Classes,
  Windows,
  VCL.StdCtrls,

  VCLTee.Chart,
  VCLTee.Series,
  VCLTee.TeeProcs,
  VCLTee.TeEngine,

  UConstants,
  UTabsheetManager,
  UAbstractObject,
  UAbstractComponent,
  URainfallGraphMenuItemManager,
  UMenuItemManager,
  UDataComponent,
  UDataEditComponent,
  UGenericModelLinkClasses;

const
  clRainYellow  = $0080FFFF;
  clRainBlue    = $00FFE2A8;
  clRainGreen   = $00C5E47E;
  clRainPink    = $00D8B0FF;
//  clRainPink    = $00BABDFE;

type
  TRainfallGraphDialog = class(TAbstractScrollablePanel)
  protected
    FPanelClient             : TPanel;
    FPanelTop                : TPanel;
    FPatchedShape            : TShape;
    FPatchedLabel            : TLabel;
    FMissingShape            : TShape;
    FMissingLabel            : TLabel;
    FFlaggedShape            : TShape;
    FFlaggedLabel            : TLabel;
    FHighLightShape          : TShape;
    FHighligtLabel           : TLabel;
    FIncludeUnreliableChkBox : TCheckBox;
    FHeadingLabel            : TLabel;
    FPanelGrid               : TPanel;
    FMonthlyGrid             : TAbstractStringGrid;
    FDailyGrid               : TAbstractStringGrid;
    FPanelGraph              : TPanel;
    FPanelCloseDailyGrid     : TPanel;
    FBtnCloseDailyGrid       : TSpeedButton;
    FGraphCntrlPanel         : TPanel;
    FBarRadioButton          : TRadioButton;
    FLineRadioButton         : TRadioButton;
    FGraphScrollBar          : TScrollBar;
    FMonthlyGraph            : TAbstractChart;
    FLineSeries              : TLineSeries;
    FBarSeries               : TBarSeries;
    FGraphTreeView           : TAbstractTreeView;
    FVerSplitter             : TSplitter;
    FHorSplitter1            : TSplitter;
    FHorSplitter2            : TSplitter;
    FDataPopupMenu           : TPopupMenu;
    FWeatherEventsMenuItem   : TMenuItem;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    function Initialise: boolean; override;
    property GraphTreeView       : TAbstractTreeView   read FGraphTreeView;
    property PanelClient         : TPanel              read FPanelClient;
    property PanelGrid           : TPanel              read FPanelGrid;
    property PanelTop            : TPanel              read FPanelTop;
    property PanelGraph          : TPanel              read FPanelGraph;
    property HorSplitter1        : TSplitter           read FHorSplitter1;
    property HorSplitter2        : TSplitter           read FHorSplitter2;
    property MonthlyGrid         : TAbstractStringGrid read FMonthlyGrid;
    property LineSeries          : TLineSeries         read FLineSeries;
    property BarSeries           : TBarSeries          read FBarSeries;
    property MonthlyGraph        : TAbstractChart      read FMonthlyGraph;
    property GraphScrollBar      : TScrollBar          read FGraphScrollBar;
    property DailyGrid           : TAbstractStringGrid read FDailyGrid;
    property WeatherEventsMenuItem : TMenuItem         read FWeatherEventsMenuItem;
    property DataPopupMenu       : TPopupMenu          read FDataPopupMenu;
    property PanelCloseDailyGrid : TPanel              read FPanelCloseDailyGrid;
    property BtnCloseDailyGrid   : TSpeedButton        read FBtnCloseDailyGrid;
    property BarRadioButton      : TRadioButton        read FBarRadioButton;
    property LineRadioButton     : TRadioButton        read FLineRadioButton;
    property VerticalSplitter    : TSplitter           read FVerSplitter;
    property HeadingLabel        : TLabel              read FHeadingLabel;
    property IncludeUnreliableChkBox : TCheckBox       read FIncludeUnreliableChkBox;

  end;

implementation

uses
  SysUtils,
  VCL.Dialogs,
  Math,
  VCL.Printers,
  UHelpContexts,
  Contnrs,
  UErrorHandlingOperations;

{ TRainfallGraphDialog }

procedure TRainfallGraphDialog.CreateMemberObjects;
const OPNAME = 'TRainfallGraphDialog.CreateMemberObjects';
begin
  inherited;
  try
    FGraphTreeView               := TAbstractTreeView.Create(Self, FAppModules);
    FGraphTreeView.Parent        := Self;
    FGraphTreeView.Align         := alLeft;
    FGraphTreeView.ReadOnly      := True;
    FGraphTreeView.HideSelection := False;

    FVerSplitter                 := TSplitter.Create(Self);
    FVerSplitter.Parent          := Self;
    FVerSplitter.Left            := FGraphTreeView.Width + 1;
    FVerSplitter.Height          := FGraphTreeView.Height;
    FVerSplitter.Beveled         := TRUE;
    FVerSplitter.Width           := 4;

    FPanelClient             := TPanel.Create(Self);
    FPanelClient.Parent      := Self;
    FPanelClient.Align       := alClient;
    FPanelClient.BorderStyle := bsNone;
    FPanelClient.BevelInner  := bvNone;
    FPanelClient.BevelOuter  := bvNone;
    FPanelClient.Visible     := FALSE;

    FPanelTop             := TPanel.Create(Self);
    FPanelTop.Parent      := FPanelClient;
    FPanelTop.Align       := alTop;
    FPanelTop.Top         := 0;
    FPanelTop.Height      := 50;
    FPanelTop.BorderStyle := bsNone;
    FPanelTop.BevelInner  := bvNone;
    FPanelTop.BevelOuter  := bvNone;

    FHeadingLabel          := TLabel.Create(Self);
    FHeadingLabel.Parent   := FPanelTop;
    FHeadingLabel.Left     := 10{350};
    FHeadingLabel.Top      := 5;
    FHeadingLabel.Caption  := FAppModules.Language.GetString('LabelText.MonthlyRainfall');
    FHeadingLabel.Font.Style := [fsBold];

    FPatchedShape        := TShape.Create(Self);
    FPatchedShape.Parent := FPanelTop;
    FPatchedShape.Left   := 440;
    FPatchedShape.Top    := 3;
    FPatchedShape.Width  := 20;
    FPatchedShape.Height := 15;
    FPatchedShape.Brush.Color := clRainYellow;

    FPatchedLabel          := TLabel.Create(Self);
    FPatchedLabel.Parent   := FPanelTop;
    FPatchedLabel.Left     := 463;
    FPatchedLabel.Top      := 3;
//    FPatchedLabel.WordWrap := TRUE;
    FPatchedLabel.AutoSize := FALSE;
    FPatchedLabel.Width    := 180;
    FPatchedLabel.Height   := 13;
    FPatchedLabel.Layout   := tlCenter;
    FPatchedLabel.Caption := FAppModules.Language.GetString('LabelText.PatchedData');

    FMissingShape        := TShape.Create(Self);
    FMissingShape.Parent := FPanelTop;
    FMissingShape.Left   := 660;
    FMissingShape.Top    := 3;
    FMissingShape.Width  := 20;
    FMissingShape.Height := 15;
    FMissingShape.Brush.Color := clRainBlue;

    FMissingLabel          := TLabel.Create(Self);
    FMissingLabel.Parent   := FPanelTop;
    FMissingLabel.Left     := 683;
    FMissingLabel.Top      := 3;
//    FMissingLabel.WordWrap := TRUE;
    FMissingLabel.AutoSize := FALSE;
    FMissingLabel.Width    := 180;
    FMissingLabel.Height   := 13;
    FMissingLabel.Layout   := tlCenter;
    FMissingLabel.Caption  := FAppModules.Language.GetString('LabelText.MissingDataExcluded');

    FFlaggedShape        := TShape.Create(Self);
    FFlaggedShape.Parent := FPanelTop;
    FFlaggedShape.Left   := 440;
    FFlaggedShape.Top    := 26;
    FFlaggedShape.Width  := 20;
    FFlaggedShape.Height := 15;
    FFlaggedShape.Brush.Color := clRainGreen;

    FFlaggedLabel          := TLabel.Create(Self);
    FFlaggedLabel.Parent   := FPanelTop;
    FFlaggedLabel.Left     := 463;
    FFlaggedLabel.Top      := 26;
//    FFlaggedLabel.WordWrap := TRUE;
    FFlaggedLabel.AutoSize := FALSE;
    FFlaggedLabel.Width    := 80;
    FFlaggedLabel.Height   := 13;
    FFlaggedLabel.Layout   := tlCenter;
    FFlaggedLabel.Caption  := FAppModules.Language.GetString('LabelText.UnreliableData');

    FIncludeUnreliableChkBox           := TCheckBox.Create(Self);
    FIncludeUnreliableChkBox.Parent    := FPanelTop;
    FIncludeUnreliableChkBox.Left      := 540;
    FIncludeUnreliableChkBox.Top       := 26;
    FIncludeUnreliableChkBox.Width     := 110;
    FIncludeUnreliableChkBox.Height    := 13;
    FIncludeUnreliableChkBox.Alignment := taRightJustify;
    FIncludeUnreliableChkBox.Caption   := FAppModules.Language.GetString('CheckBox.IncludeStats');

    FHighLightShape        := TShape.Create(Self);
    FHighLightShape.Parent := FPanelTop;
    FHighLightShape.Left   := 660;
    FHighLightShape.Top    := 26;
    FHighLightShape.Width  := 20;
    FHighLightShape.Height := 15;
    FHighLightShape.Brush.Color := clRainPink;

    FHighligtLabel          := TLabel.Create(Self);
    FHighligtLabel.Parent   := FPanelTop;
    FHighligtLabel.Left     := 683;
    FHighligtLabel.Top      := 26;
//    FHighligtLabel.WordWrap := TRUE;
    FHighligtLabel.AutoSize := FALSE;
    FHighligtLabel.Width    := 180;
    FHighligtLabel.Height   := 13;
    FHighligtLabel.Layout   := tlCenter;
    FHighligtLabel.Caption  := FAppModules.Language.GetString('LabelText.PossibleOutliers');

    FPanelGrid             := TPanel.Create(Self);
    FPanelGrid.Parent      := FPanelClient;
    FPanelGrid.Top         := 35;
    FPanelGrid.Align       := alTop;
    FPanelGrid.BorderStyle := bsNone;
    FPanelGrid.BevelInner  := bvNone;
    FPanelGrid.BevelOuter  := bvNone;

    FHorSplitter1         := TSplitter.Create(Self);
    FHorSplitter1.Parent  := FPanelClient;
    FHorSplitter1.Align   := alTop;
    FHorSplitter1.Top     := 100;
    FHorSplitter1.Height  := 4;
    FHorSplitter1.Beveled := TRUE;

    FMonthlyGrid                  := TAbstractStringGrid.Create(Self, FAppModules);
    FMonthlyGrid.Parent           := FPanelGrid;
    FMonthlyGrid.FixedCols        := 0 ;
    FMonthlyGrid.Top              := 0;
    FMonthlyGrid.Align            := alTop{alClient};
    FMonthlyGrid.Font.Name        := FAppModules.Language.GetString('GridFont.FontName');
    FMonthlyGrid.Font.Style       := [];
    FMonthlyGrid.ParentFont       := FALSE;
    FMonthlyGrid.ColCount         := 26;
    FMonthlyGrid.DefaultColWidth  := 60;
    FMonthlyGrid.DefaultRowHeight := 20;
    FMonthlyGrid.Options          := FMonthlyGrid.Options - [goEditing, goRangeSelect];

    FHorSplitter2         := TSplitter.Create(Self);
    FHorSplitter2.Parent  := FPanelGrid;
    FHorSplitter2.Align   := alTop;
    FHorSplitter2.Height  := 4;
    FHorSplitter2.Beveled := TRUE;

    FDailyGrid            := TAbstractStringGrid.Create(Self, FAppModules);
    FDailyGrid.Parent     := FPanelGrid;
    FDailyGrid.FixedCols  := 0;
    FDailyGrid.Top        := 35;
    FDailyGrid.Align      := alClient;
    FDailyGrid.DefaultRowHeight := 20;
    FDailyGrid.DefaultColWidth := 24;

    FPanelCloseDailyGrid := TPanel.Create ( FPanelGrid );
    FPanelCloseDailyGrid.Parent := FPanelGrid;
    FPanelCloseDailyGrid.Top := FDailyGrid.Top;
    FPanelCloseDailyGrid.Align := alRight;
    FPanelCloseDailyGrid.Height := FDailyGrid.Height;
    FPanelCloseDailyGrid.Width := 15;
    FPanelCloseDailyGrid.Visible := False;

    FbtnCloseDailyGrid := TSpeedButton.Create ( FPanelCloseDailyGrid );
    FbtnCloseDailyGrid.Parent := FPanelCloseDailyGrid;
    FbtnCloseDailyGrid.Top := 0;
    FbtnCloseDailyGrid.Width := FPanelCloseDailyGrid.Width - 1;
    FbtnCloseDailyGrid.Height := 15;
    FbtnCloseDailyGrid.Caption := 'X';
    FbtnCloseDailyGrid.Flat := True;

    FPanelGraph             := TPanel.Create(Self);
    FPanelGraph.Parent      := FPanelClient;
    FPanelGraph.Align       := alClient;
    FPanelGraph.BorderStyle := bsNone;
    FPanelGraph.BevelInner  := bvNone;
    FPanelGraph.BevelOuter  := bvNone;

    FGraphCntrlPanel             := TPanel.Create(Self);
    FGraphCntrlPanel.Parent      := FPanelGraph;
    FGraphCntrlPanel.Height      := 20;
    FGraphCntrlPanel.Align       := alTop;
    FGraphCntrlPanel.BorderStyle := bsNone;
    FGraphCntrlPanel.BevelInner  := bvNone;
    FGraphCntrlPanel.BevelOuter  := bvNone;

    FBarRadioButton         := TRadioButton.Create(Self);
    FBarRadioButton.Parent  := FGraphCntrlPanel;
    FBarRadioButton.Left    := 10;
    FBarRadioButton.Top     := 5;
    FBarRadioButton.Width   := 80;
    FBarRadioButton.Caption := FAppModules.Language.GetString('RadioCaption.BarChart');

    FLineRadioButton         := TRadioButton.Create(Self);
    FLineRadioButton.Parent  := FGraphCntrlPanel;
    FLineRadioButton.Left    := 100;
    FLineRadioButton.Top     := 5;
    FLineRadioButton.Width   := 80;
    FLineRadioButton.Caption := FAppModules.Language.GetString('RadioCaption.LineGraph');

    FGraphScrollBar          := TScrollBar.Create(Self);
    FGraphScrollBar.Parent   := FPanelGraph;
    FGraphScrollBar.Align    := alBottom;

    FMonthlyGraph := TAbstractChart.Create(Self, FAppModules);
    FMonthlyGraph.Parent                        := FPanelGraph;
    FMonthlyGraph.Align                         := alClient;
    FMonthlyGraph.BevelOuter                    := bvNone;
    FMonthlyGraph.Legend.Visible                := FALSE;
    FMonthlyGraph.AxisVisible                   := TRUE;
    FMonthlyGraph.AllowZoom                     := FALSE;
    FMonthlyGraph.AllowPanning                  := pmNone;
    FMonthlyGraph.Gradient.Visible              := FALSE;
    FMonthlyGraph.View3D                        := FALSE;
    FMonthlyGraph.Title.Visible                 := TRUE;
    FMonthlyGraph.Title.Font.Style              := [fsBold];
    FMonthlyGraph.Title.Font.Color              := clBlack;
    FMonthlyGraph.LeftAxis.Title.Caption        := FAppModules.Language.GetString('RainfallGraph.LeftTitle');
    FMonthlyGraph.LeftAxis.Title.Angle          := 90;
    FMonthlyGraph.BottomAxis.Title.Caption      := FAppModules.Language.GetString('RainfallGraph.BottomTitle');
    FMonthlyGraph.BottomAxis.LabelsAngle        := 90;
    FMonthlyGraph.BottomAxis.TickLength         := 6;
    FMonthlyGraph.ScaleLastPage                 := FALSE;
    FMonthlyGraph.BottomAxis.MinorTicks.Visible := FALSE;
    FMonthlyGraph.MaxPointsPerPage              := 120;

    FLineSeries := TLineSeries.Create(FMonthlyGraph);
    FLineSeries.ParentChart   := FMonthlyGraph;
    FLineSeries.Marks.Visible := FALSE;
    FLineSeries.LinePen.Width := 2;
    FLineSeries.Clear;

    FBarSeries := TBarSeries.Create(FMonthlyGraph);
    FBarSeries.ParentChart     := FMonthlyGraph;
    FBarSeries.Marks.Visible   := FALSE;
    FBarSeries.SeriesColor     := clRainBlue;
    FBarSeries.BarWidthPercent := 100;
    FBarSeries.Clear;

    FWeatherEventsMenuItem         := TMenuItem.Create(Self);
    FWeatherEventsMenuItem.Caption := FAppModules.Language.GetString('MenuCaption.WeatherEvents');

    FDataPopupMenu        := TPopupMenu.Create(Self);
    FDataPopupMenu.Items.Add(FWeatherEventsMenuItem);

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphDialog.DestroyMemberObjects;
const OPNAME = 'TRainfallGraphDialog.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallGraphDialog.Initialise: Boolean;
const OPNAME = 'TRainfallGraphDialog.Initialise';
var
  lIndex : integer;
begin
  Result := inherited initialise;
  try
    FMonthlyGrid.Cells[ 0, 0] := FAppModules.Language.GetString('GridHeading.Year');
    FMonthlyGrid.Cells[ 1, 0] := FAppModules.Language.GetString('GridHeading.Oct');
    FMonthlyGrid.Cells[ 3, 0] := FAppModules.Language.GetString('GridHeading.Nov');
    FMonthlyGrid.Cells[ 5, 0] := FAppModules.Language.GetString('GridHeading.Dec');
    FMonthlyGrid.Cells[ 7, 0] := FAppModules.Language.GetString('GridHeading.Jan');
    FMonthlyGrid.Cells[ 9, 0] := FAppModules.Language.GetString('GridHeading.Feb');
    FMonthlyGrid.Cells[11, 0] := FAppModules.Language.GetString('GridHeading.Mar');
    FMonthlyGrid.Cells[13, 0] := FAppModules.Language.GetString('GridHeading.Apr');
    FMonthlyGrid.Cells[15, 0] := FAppModules.Language.GetString('GridHeading.May');
    FMonthlyGrid.Cells[17, 0] := FAppModules.Language.GetString('GridHeading.Jun');
    FMonthlyGrid.Cells[19, 0] := FAppModules.Language.GetString('GridHeading.Jul');
    FMonthlyGrid.Cells[21, 0] := FAppModules.Language.GetString('GridHeading.Aug');
    FMonthlyGrid.Cells[23, 0] := FAppModules.Language.GetString('GridHeading.Sep');
    FMonthlyGrid.Cells[25, 0] := FAppModules.Language.GetString('GridHeading.Total');

    for lIndex := 1 to 12 do
    begin
      FMonthlyGrid.ColWidths[lIndex*2 - 1] := 51;
      FMonthlyGrid.ColWidths[lIndex*2 ]    := 10;
    end;
    FBarRadioButton.Checked := TRUE;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphDialog.AssignHelpContext;
const OPNAME = 'TRainfallGraphDialog.AssignHelpContext';
begin
  inherited;
  try
    SetControlHelpContext(FMonthlyGrid, HC_RainIMS);
    SetControlHelpContext(FBarRadioButton, HC_RainIMS);
    SetControlHelpContext(FLineRadioButton, HC_RainIMS );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


end.

