//
//
//  UNIT      : Contains the class TReservoirEvaporationDialog.
//  AUTHOR    : Valentino Naicker (arivia.kom)
//  DATE      : 2003/06/27
//  COPYRIGHT : Copyright © 2003 DWAF
//
//

unit UReservoirCatchmentProportionsDialog;

interface

uses

  // Delphi VCL
  Classes,
  VCLTee.Chart,
  VCLTee.TeeProcs,
  VCLTee.Series,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Grids,
  SysUtils,
  VCL.Buttons,
  VCL.Graphics,
  Windows,

  // arivia.kom
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent;

type
  TValueType  = (vtAnnual, vtMonthly);
  TActionHint = (allNone,allReservoir, allCatchment, curReservoir, curCatchment);
  TReservoirCatchmentProportionsDialog = class(TAbstractScrollablePanel)
  private
  protected
    FPanelCatchments     : TAbstractPanel;
    FPanelButton         : TPanel;
    FPanelRadioGroup     : TAbstractPanel;
    FPanelLabel          : TPanel;

    FBtnAdd              : TFieldBitBtn;
    FBtnRemove           : TFieldBitBtn;

    FGrdProportions      : TFieldStringGrid;
    FTvwAllCatchment     : TFieldTreeView;
    FTvwCurrentCatchment : TFieldTreeView;

    FRadFiles            : TFieldRadioGroup;
    FRadChartView        : TFieldRadioGroup;
    FChtTimeSeries       : TAbstractChart;

    FRadMonthlyAnnual    : TFieldRadioGroup;
    FChkMAR              : TFieldChkBox;

    FINCLineSeriesA      : TLineSeries;
    FRNKLineSeriesA      : TLineSeries;
    FAFFLineSeriesA      : TLineSeries;
    FIRRLineSeriesA      : TLineSeries;
    FRANLineSeriesA      : TLineSeries;
    FNETTLineSeriesA     : TLineSeries;
    FURBLineSeriesA      : TLineSeries;

    FMARLineSeriesINC    : TLineSeries;
    FMARLineSeriesRNK    : TLineSeries;
    FMARLineSeriesAFF    : TLineSeries;
    FMARLineSeriesIRR    : TLineSeries;
    FMARLineSeriesRAN    : TLineSeries;
    FMARLineSeriesURB    : TLineSeries;

    FINCLineSeriesM      : TLineSeries;
    FRNKLineSeriesM      : TLineSeries;
    FAFFLineSeriesM      : TLineSeries;
    FIRRLineSeriesM      : TLineSeries;
    FRANLineSeriesM      : TLineSeries;
    FNETTLineSeriesM     : TLineSeries;
    FURBLineSeriesM      : TLineSeries;

    FLblAllCatchment     : TLabel;
    FLblAction           : TLabel;
    FLblProportions      : TLabel;
    FLblCurrentCatchment : TLabel;
    FLblCatchmentArea    : TLabel;

    FCatchmentAreaEdt    : TFieldEdit;

    FFileNameContainer   : TStringList;

    FNegNumbers          : integer;
    FStdDeviation        : double;
    FAvarage             : double;

    FGridRowCount: integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;

    procedure SetGridRowCount(const Value: integer);
    procedure DrawTotalLines(ASender: TObject; ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
    procedure SelectCellEvent(Sender: TObject; ACol, ARow: Longint; var CanSelect: Boolean);
    procedure ConfigureLineSeries(ALineSeries : TLineSeries; AValueType : TValueType);
    procedure ConfigureChartBottomAxis(ALineSeries : TLineSeries; AValueType : TValueType);

    procedure OnRadNetFilesClick(Sender: TObject);
    procedure OnRadFilesClick(Sender: TObject);
    procedure CalculateLeftAxisMinMax(ALineSeries : TLineSeries);
  public
    procedure Resize; override;

    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    function RefreshTimeSeriesChart : boolean;

    function DispalyINCChart(AFileName : string) : boolean;
    function DispalyAFFChart(AFileName : string) : boolean;
    function DispalyIRRChart(AFileName : string) : boolean;
    function DispalyRNKChart(AFileName : string) : boolean;
    function DispalyRANChart(AFileName : string) : boolean;
    function DispalyURBChart(AFileName : string) : boolean;

    procedure RestoreColourState; override;
    procedure ShowTreeViewHint(AActionHint:TActionHint);
    procedure PopulateFileNames(AFileNameContainer : TStringList);
    procedure ToggleMARLines(ALineSeries : TLineSeries);
    procedure ToggleMonthlyAnnual;
    procedure CalculateStatistics;

    property GridRowCount        : integer          read FGridRowCount write SetGridRowCount;

    property BtnAdd              : TFieldBitBtn     read FBtnAdd;
    property BtnRemove           : TFieldBitBtn     read FBtnRemove;

    property TvwAllCatchment     : TFieldTreeView   read FTvwAllCatchment;
    property TvwCurrentCatchment : TFieldTreeView   read FTvwCurrentCatchment;
    property GrdProportions      : TFieldStringGrid read FGrdProportions;
    property RadFiles            : TFieldRadioGroup read FRadFiles;
    property RadChartView        : TFieldRadioGroup read FRadChartView;

    property ChtTimeSeries       : TAbstractChart   read FChtTimeSeries;

    property INCLineSeriesA      : TLineSeries      read FINCLineSeriesA;
    property RNKLineSeriesA      : TLineSeries      read FRNKLineSeriesA;
    property AFFLineSeriesA      : TLineSeries      read FAFFLineSeriesA;
    property IRRLineSeriesA      : TLineSeries      read FIRRLineSeriesA;
    property RANLineSeriesA      : TLineSeries      read FRANLineSeriesA;
    property NETTLineSeriesA     : TLineSeries      read FNETTLineSeriesA;
    property URBLineSeriesA      : TLineSeries      read FURBLineSeriesA;

    property MARLineSeriesINC    : TLineSeries      read FMARLineSeriesINC;
    property MARLineSeriesRNK    : TLineSeries      read FMARLineSeriesRNK;
    property MARLineSeriesAFF    : TLineSeries      read FMARLineSeriesAFF;
    property MARLineSeriesIRR    : TLineSeries      read FMARLineSeriesIRR;
    property MARLineSeriesRAN    : TLineSeries      read FMARLineSeriesRAN;
    property MARLineSeriesURB    : TLineSeries      read FMARLineSeriesURB;

    property INCLineSeriesM      : TLineSeries      read FINCLineSeriesM;
    property RNKLineSeriesM      : TLineSeries      read FRNKLineSeriesM;
    property AFFLineSeriesM      : TLineSeries      read FAFFLineSeriesM;
    property IRRLineSeriesM      : TLineSeries      read FIRRLineSeriesM;
    property RANLineSeriesM      : TLineSeries      read FRANLineSeriesM;
    property NETTLineSeriesM     : TLineSeries      read FNETTLineSeriesM;
    property URBLineSeriesM      : TLineSeries      read FURBLineSeriesM;
    {
    property INCNegNumbers       : integer          read FINCNegNumbers write FINCNegNumbers;
    property NETTNegNumbers      : integer          read FNETTNegNumbers write FNETTNegNumbers;
    property AFFNegNumbers       : integer          read FAFFNegNumber write FAFFNegNumber;
    property IRRNegNumbers       : integer          read FIRRNegNumbers write FIRRNegNumbers;
    property RANNegNumbers       : integer          read FRANNegNumbers write FRANNegNumbers;
      }
    property FileNameContainer   : TStringList      read FFileNameContainer;
    property RadMonthlyAnnual    : TFieldRadioGroup read FRadMonthlyAnnual;
    property ChkMAR              : TFieldChkBox     read FChkMAR;
    property CatchmentAreaEdt    : TFieldEdit       read FCatchmentAreaEdt;
  end;

implementation

uses

  // Delphi VCL

  VCL.Forms,
  Math,
  VCL.Dialogs,
  UHelpContexts,
  VCLTee.TeEngine,
  VCLTee.TeCanvas,
  UYieldModelDataObject,
  UErrorHandlingOperations;

{ TReservoirCatchmentProportionsDialog }

procedure TReservoirCatchmentProportionsDialog.CreateMemberObjects;
const OPNAME = 'TReservoirCatchmentProportionsDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FFileNameContainer := TStringList.Create;
    //FNegativeNumbers   := 0;
    FStdDeviation      := 0.0;
    FAvarage           := 0.0;

    // Create Panels
    FPanelCatchments := TAbstractPanel.Create(ControlsOwner, FAppModules);
    FPanelCatchments.Parent := ControlsParent;

    FPanelLabel            := TPanel.Create(ControlsOwner);
    FPanelLabel.Parent     := FPanelCatchments;
    FPanelLabel.BevelInner := bvNone;
    FPanelLabel.BevelOuter := bvNone;
    FPanelLabel.Align      := alTop;

    FLblAllCatchment     := TLabel.Create(FPanelLabel);
    FLblAction           := TLabel.Create(FPanelLabel);
    FLblCurrentCatchment := TLabel.Create(FPanelLabel);
    FLblProportions      := TLabel.Create(FPanelLabel);

    FLblAllCatchment.Parent := FPanelLabel;
    FLblAction.Parent := FPanelLabel;
    FLblCurrentCatchment.Parent := FPanelLabel;
    FLblProportions.Parent := FPanelLabel;

    FLblAllCatchment.AutoSize := False;
    FLblAction.AutoSize := False;
    FLblCurrentCatchment.AutoSize := False;
    FLblProportions.AutoSize := False;

    FLblAllCatchment.Alignment := taCenter;
    FLblAction.Alignment := taCenter;
    FLblCurrentCatchment.Alignment := taCenter;
    FLblProportions.Alignment := taCenter;

    // Files Treeview
    FTvwAllCatchment := TFieldTreeView.Create(FPanelCatchments, FAppModules);
    FTvwAllCatchment.Parent := FPanelCatchments;
    FTvwAllCatchment.SortType := stText;
    FTvwAllCatchment.DragMode := dmAutomatic;
    //FTvwAllCatchment.ReadOnly := True;
    FTvwAllCatchment.HideSelection := False;

    FTvwCurrentCatchment := TFieldTreeView.Create(FPanelCatchments, FAppModules);
    FTvwCurrentCatchment.Parent := FPanelCatchments;
    FTvwCurrentCatchment.SortType := stText;
    FTvwCurrentCatchment.DragMode := dmAutomatic;
    //FTvwCurrentCatchment.ReadOnly := True;
    FTvwCurrentCatchment.HideSelection := False;


    // Files Grid
    FGrdProportions := TFieldStringGrid.Create(FPanelCatchments,FAppModules);
    FGrdProportions.Parent := FPanelCatchments;
    FGrdProportions.Visible := True;
    FGrdProportions.OnSelectCell := SelectCellEvent;
    FGrdProportions.Options := FGrdProportions.Options + [goEditing , goAlwaysShowEditor];

    FPanelButton := TPanel.Create(FPanelCatchments);
    FPanelButton.Parent := FPanelCatchments;
    FPanelButton.BevelInner := bvNone;
    FPanelButton.BevelOuter := bvNone;


    FBtnAdd      := TFieldBitBtn.Create(FPanelButton, FAppModules);
    FBtnAdd.Parent := FPanelButton;
    FBtnRemove   := TFieldBitBtn.Create(FPanelButton, FAppModules);
    FBtnAdd.Parent := FPanelButton;
    FBtnRemove.Parent := FPanelButton;


    FPanelRadioGroup := TAbstractPanel.Create(ControlsParent, FAppModules);
    FPanelRadioGroup.Parent := ControlsParent;

    FRadChartView := TFieldRadioGroup.Create(FPanelRadioGroup, FAppModules);
    FRadChartView.Parent := FPanelRadioGroup;
    FRadChartView.Columns := 2;
    FRadChartView.OnClick := OnRadNetFilesClick;

    FRadFiles := TFieldRadioGroup.Create(FPanelRadioGroup, FAppModules);
    FRadFiles.Parent := FPanelRadioGroup;
    FRadFiles.Columns := 4;
    if (FAppModules.Model.ModelName = CPlanning) then
      FRadFiles.Columns := 5;
    FRadFiles.OnClick := OnRadFilesClick;

    FRadMonthlyAnnual  := TFieldRadioGroup.Create(FPanelRadioGroup, FAppModules);
    FChkMAR           := TFieldChkBox.Create(FPanelRadioGroup, FAppModules);
    FRadMonthlyAnnual.Parent  := FPanelRadioGroup;
    FChkMAR.Parent           := FPanelRadioGroup;
    FRadMonthlyAnnual.Columns := 2;

    FLblCatchmentArea := TLabel.Create(FPanelRadioGroup);
    FLblCatchmentArea.Parent := FPanelRadioGroup;

    FCatchmentAreaEdt := TFieldEdit.Create(FPanelRadioGroup,FAppModules);
    FCatchmentAreaEdt.Parent := FPanelRadioGroup;

    // Time Series Chart
    FChtTimeSeries := TAbstractChart.Create(ControlsOwner, FAppModules);
    TWinControl(FChtTimeSeries).Parent := ControlsParent;
    //FChtTimeSeries.Align := alClient;
    FChtTimeSeries.AllowPanning := pmBoth;
    FChtTimeSeries.AllowZoom := True;
    //FChtTimeSeries.BackWall.Brush.Color := clWhite;
    //FChtTimeSeries.BackWall.Brush.Style := bsClear;
    //FChtTimeSeries.BackWall.Pen.Visible := False;
    FChtTimeSeries.Title.Text.Text := '';
    FChtTimeSeries.AxisVisible := True;
    FChtTimeSeries.ClipPoints := True;
    FChtTimeSeries.Frame.Visible := True;
    FChtTimeSeries.Legend.Visible := False;
    FChtTimeSeries.View3D := False;
    //FChtTimeSeries.View3DWalls := False;
    FChtTimeSeries.Legend.Alignment := laBottom;
    FChtTimeSeries.Legend.LegendStyle := lsSeries;

    FINCLineSeriesA  := TLineSeries.Create(FChtTimeSeries);
    FRNKLineSeriesA  := TLineSeries.Create(FChtTimeSeries);
    FAFFLineSeriesA  := TLineSeries.Create(FChtTimeSeries);
    FIRRLineSeriesA  := TLineSeries.Create(FChtTimeSeries);
    FRANLineSeriesA  := TLineSeries.Create(FChtTimeSeries);
    FNETTLineSeriesA := TLineSeries.Create(FChtTimeSeries);
    FURBLineSeriesA  := TLineSeries.Create(FChtTimeSeries);

    FMARLineSeriesINC   := TLineSeries.Create(FChtTimeSeries);
    FMARLineSeriesRNK   := TLineSeries.Create(FChtTimeSeries);
    FMARLineSeriesAFF   := TLineSeries.Create(FChtTimeSeries);
    FMARLineSeriesIRR   := TLineSeries.Create(FChtTimeSeries);
    FMARLineSeriesRAN   := TLineSeries.Create(FChtTimeSeries);
    FMARLineSeriesURB   := TLineSeries.Create(FChtTimeSeries);

    FMARLineSeriesINC.ShowInLegend := False;
    FMARLineSeriesRNK.ShowInLegend := False;
    FMARLineSeriesAFF.ShowInLegend := False;
    FMARLineSeriesIRR.ShowInLegend := False;
    FMARLineSeriesRAN.ShowInLegend := False;
    FMARLineSeriesURB.ShowInLegend := False;

    FINCLineSeriesM  := TLineSeries.Create(FChtTimeSeries);
    FRNKLineSeriesM  := TLineSeries.Create(FChtTimeSeries);
    FAFFLineSeriesM  := TLineSeries.Create(FChtTimeSeries);
    FIRRLineSeriesM  := TLineSeries.Create(FChtTimeSeries);
    FRANLineSeriesM  := TLineSeries.Create(FChtTimeSeries);
    FNETTLineSeriesM := TLineSeries.Create(FChtTimeSeries);
    FURBLineSeriesM  := TLineSeries.Create(FChtTimeSeries);

    FChtTimeSeries.AddSeries(FINCLineSeriesA);
    FChtTimeSeries.AddSeries(FRNKLineSeriesA);
    FChtTimeSeries.AddSeries(FAFFLineSeriesA);
    FChtTimeSeries.AddSeries(FIRRLineSeriesA);
    FChtTimeSeries.AddSeries(FRANLineSeriesA);
    FChtTimeSeries.AddSeries(FNETTLineSeriesA);
    FChtTimeSeries.AddSeries(FURBLineSeriesA);

    FChtTimeSeries.AddSeries(FMARLineSeriesINC);
    FChtTimeSeries.AddSeries(FMARLineSeriesRNK);
    FChtTimeSeries.AddSeries(FMARLineSeriesAFF);
    FChtTimeSeries.AddSeries(FMARLineSeriesIRR);
    FChtTimeSeries.AddSeries(FMARLineSeriesRAN);
    FChtTimeSeries.AddSeries(FMARLineSeriesURB);

    FChtTimeSeries.AddSeries(FINCLineSeriesM);
    FChtTimeSeries.AddSeries(FRNKLineSeriesM);
    FChtTimeSeries.AddSeries(FAFFLineSeriesM);
    FChtTimeSeries.AddSeries(FIRRLineSeriesM);
    FChtTimeSeries.AddSeries(FRANLineSeriesM);
    FChtTimeSeries.AddSeries(FNETTLineSeriesM);
    FChtTimeSeries.AddSeries(FURBLineSeriesM);

    FGrdProportions.OnDrawCell := DrawTotalLines;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsDialog.DestroyMemberObjects;
const OPNAME = 'TReservoirCatchmentProportionsDialog.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
   FFileNameContainer.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsDialog.Resize;
const OPNAME = 'TReservoirCatchmentProportionsDialog.Resize';
var
  LButtonTop,
  LButtonPanelWidth,
  LOtherPanelWidth,
  LSelfClientHeight : integer;
begin
  inherited Resize;
  try
    FPanelRadioGroup.Align  := alTop;
    FPanelRadioGroup.Height := 70;

    FRadChartView.Height    := 33;
    FRadChartView.Top       := 0;
    FRadChartView.Left      := 0;
    FRadChartView.Width     := FPanelRadioGroup.ClientWidth div 4;

    FRadFiles.Height        := 33;
    FRadFiles.Top           := 0;
    FRadFiles.Left          := FRadChartView.Left + FRadChartView.Width;
    FRadFiles.Width         := FPanelRadioGroup.ClientWidth div 2;

    FRadMonthlyAnnual.Height := 33;
    FRadMonthlyAnnual.Top    := 0;
    FRadMonthlyAnnual.Left   := FRadFiles.Left + FRadFiles.Width;;
    FRadMonthlyAnnual.Width  := FPanelRadioGroup.ClientWidth * 3 div 16;

    FChkMAR.Height          := 33;
    FChkMAR.Top             := 0;
    FChkMAR.Left            := FRadMonthlyAnnual.Left + FRadMonthlyAnnual.Width;
    FChkMAR.Width           := FPanelRadioGroup.ClientWidth  div 16;

    FLblCatchmentArea.Top   := 42;
    FLblCatchmentArea.Left  := 10;
    FCatchmentAreaEdt.Top   := 40;
    FCatchmentAreaEdt.Left  := 270;

    if FAppModules.StudyArea.ModelVersion <> '7' then
    begin
      FPanelRadioGroup.Height   := 35;
      FLblCatchmentArea.Visible := False;
      FCatchmentAreaEdt.Visible := False;
    end else
    begin
      FPanelRadioGroup.Height   := 70;
      FLblCatchmentArea.Visible := True;
      FCatchmentAreaEdt.Visible := True;
    end;

    LSelfClientHeight       := (ControlsParent.ClientHeight - FPanelRadioGroup.Height)-2;

    FPanelCatchments.Left   := 0;
    if FAppModules.StudyArea.ModelVersion <> '7' then
      FPanelCatchments.Top    := FRadChartView.Top + FRadChartView.Height+1
    else
      FPanelCatchments.Top    := FPanelRadioGroup.Top + FPanelRadioGroup.Height + 1;

    FPanelCatchments.Width  := ControlsParent.ClientWidth;
    FPanelCatchments.Height := LSelfClientHeight div 2;

    TWinControl(FChtTimeSeries).Left   := 0;

//    FChtTimeSeries.Left := 0;
  //  FChtTimeSeries.MarginLeft := 0;

    //(FChtTimeSeries as TWinControl).Left := 0;
    //FChtTimeSeries.MarginTop := FPanelCatchments.Top + FPanelCatchments.Height+1;
    TWinControl(FChtTimeSeries).Top    := FPanelCatchments.Top + FPanelCatchments.Height+1;
    //FChtTimeSeries.Align.alClient;
    FChtTimeSeries.Width  := FPanelCatchments.Width;
    FChtTimeSeries.Height := (LSelfClientHeight div 2);

    LButtonPanelWidth := FPanelCatchments.ClientWidth * 10 div 100;
    LOtherPanelWidth  := FPanelCatchments.ClientWidth * 30 div 100;

    FPanelLabel.Height     := 19;

    FLblAllCatchment.Top := 1;
    FLblAllCatchment.Left := 0;
    FLblAllCatchment.Width := LOtherPanelWidth;

    FLblAction.Top := 1;
    FLblAction.Left := FLblAllCatchment.Left + FLblAllCatchment.Width;
    FLblAction.Width := LButtonPanelWidth;

    FLblCurrentCatchment.Top := 1;
    FLblCurrentCatchment.Left := FLblAction.Left + FLblAction.Width;;
    FLblCurrentCatchment.Width := LOtherPanelWidth;

    FLblProportions.Top := 1;
    FLblProportions.Left := FLblCurrentCatchment.Left + FLblCurrentCatchment.Width;;
    FLblProportions.Width := LOtherPanelWidth;

    //FTvwAllCatchment.Top    := FLblAllCatchment.Top + FLblAllCatchment.Height;
    //FTvwAllCatchment.Left   := 0;
    //FTvwAllCatchment.Height := FPanelCatchments.ClientHeight;
    FTvwAllCatchment.Align := alLeft;
    FTvwAllCatchment.Width  := FLblAllCatchment.Width;

    FPanelButton.Top    := FTvwAllCatchment.Top;
    FPanelButton.Left   := FTvwAllCatchment.Left + FTvwAllCatchment.Width;
    FPanelButton.Height := FPanelCatchments.ClientHeight;
    FPanelButton.Width  := FLblAction.Width;

    LButtonTop          := (FPanelButton.ClientHeight - (FBtnAdd.Height * 2)) div 3;

    //FBtnAdd.Height    := FPanelButton.Height div 5;
    FBtnAdd.Width     := FPanelButton.Width div 2;
    FBtnAdd.Top       := LButtonTop;
    FBtnAdd.Left      := FBtnAdd.Width div 2;

    //FBtnRemove.Height := FPanelButton.Height div 5;
    FBtnRemove.Width  := FPanelButton.Width div 2;
    FBtnRemove.Top    := FBtnAdd.Top + FBtnAdd.Height + LButtonTop;
    FBtnRemove.Left   := FBtnRemove.Width div 2;

    FTvwCurrentCatchment.Top    := FPanelButton.Top;
    FTvwCurrentCatchment.Left   := FPanelButton.Left + FPanelButton.Width;
    FTvwCurrentCatchment.Height := FTvwAllCatchment.Height; //FPanelCatchments.ClientHeight;
    FTvwCurrentCatchment.Width  := FLblCurrentCatchment.Width;

    FGrdProportions.Top    := FTvwCurrentCatchment.Top;
    FGrdProportions.Left   := FTvwCurrentCatchment.Left + FTvwCurrentCatchment.Width;
    FGrdProportions.Height := FTvwAllCatchment.Height;//FPanelCatchments.ClientHeight;
    FGrdProportions.Width  := FLblProportions.Width;
    FGrdProportions.DefaultColWidth := (FGrdProportions.Width div FGrdProportions.ColCount) -2;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirCatchmentProportionsDialog.Initialise: boolean;
const OPNAME = 'TReservoirCatchmentProportionsDialog.Initialise';
var
  LCount: integer;
  LItemsCount : integer;
begin
  Result := inherited Initialise;
  try
    //FNegativeNumbers  := 0;
    FStdDeviation     := 0.0;
    FAvarage          := 0.0;

    FGrdProportions.ColCount := 3;
    if (FAppModules.Model.ModelName = CPlanning) then
      FGrdProportions.ColCount := 4;

    FGrdProportions.RowCount := 2;
    FGrdProportions.FixedRows := 1;
    FGrdProportions.FixedCols := 0;        
    FGrdProportions.DefaultRowHeight  := 15;
    //FGrdProportions.IsEnabled   := False;

    FRadChartView.Items.Clear;
    for LCount := 0 to 1 do
      FRadChartView.Items.Add('');
    FRadChartView.ItemIndex := -1;
    //FRadChartView.IsEnabled     := False;

    LItemsCount := 3;
    if (FAppModules.Model.ModelName = CPlanning) then
      LItemsCount := 4;

    FRadFiles.Items.Clear;
    for LCount := 0 to LItemsCount do
      FRadFiles.Items.Add('');
    FRadFiles.ItemIndex := -1;
    FRadFiles.IsEnabled := False;

    FPanelLabel.Caption    := '';
    FBtnAdd.Caption := '';
    FBtnRemove.Caption := '';
    FBtnAdd.Glyph.LoadFromResourceName(HImagesInstance, 'CATCHADD');
    FBtnRemove.Glyph.LoadFromResourceName(HImagesInstance, 'CATCHREMOVE');
    FBtnAdd.NumGlyphs := 2;
    FBtnRemove.NumGlyphs := 2;

    FBtnAdd.IsEnabled := False;
    FBtnRemove.IsEnabled := False;

    //FGrdProportions.Enabled := False;
    //FTvwAllCatchment.IsEnabled := False;
    //FTvwCurrentCatchment.IsEnabled := False;

    for LCount := 0 to 2 do
      FGrdProportions.Rows[1].Strings[LCount]:= '';
    FGrdProportions.Row := 1;
    FGrdProportions.Col := 0;
    //FGrdProportions.IsEnabled := False;

    FRadMonthlyAnnual.Items.Clear;
    FChkMAR.Checked  := False;
    FChkMAR.Enabled  := False;

    FTvwAllCatchment.Items.Clear;
    FTvwCurrentCatchment.Items.Clear;

    FChtTimeSeries.SeriesList.Clear;
    FChtTimeSeries.RemoveAllSeries;
    FChtTimeSeries.UndoZoom;

    FChtTimeSeries.BottomAxis.LabelsFont.Size := 7;
    //FChtTimeSeries.BottomAxis.LabelsAngle := 45;
    FChtTimeSeries.BottomAxis.TickLength  := 6;
    FChtTimeSeries.BottomAxis.MinorTickCount  := 4;
    //FChtTimeSeries.LeftAxis.Automatic := False;
    FChtTimeSeries.LeftAxis.AxisValuesFormat := '######0.###';

    //FChtTimeSeries.BottomAxis.AxisValuesFormat := '####';
    //FChtTimeSeries.LeftAxis.Automatic := False;
    //FChtTimeSeries.BottomAxis.LabelsFont.Size := 7;
    //FChtTimeSeries.BottomAxis.LabelsAngle     := 90;
    //FChtTimeSeries.BottomAxis.MinorTicks.Visible := False;
    //FChtTimeSeries.BottomAxis.ExactDateTime   := True;
    {FChtTimeSeries.BottomAxis.TickLength      := 6;
    FChtTimeSeries.BottomAxis.MinorTickCount  := 4;
    FChtTimeSeries.BottomAxis.MinorTickLength := 4;
    FChtTimeSeries.BottomAxis.Increment       := 1825;
    }
    FINCLineSeriesA.SeriesColor     := clBlue;
    FAFFLineSeriesA.SeriesColor     := clPurple;
    FIRRLineSeriesA.SeriesColor     := clGreen;
    FRANLineSeriesA.SeriesColor     := clNavy;
    FNETTLineSeriesA.SeriesColor    := clRed;
    FURBLineSeriesA.SeriesColor     := clLime;

    FMARLineSeriesINC.SeriesColor   := clFuchsia;
    FMARLineSeriesRNK.SeriesColor   := clFuchsia;
    FMARLineSeriesAFF.SeriesColor   := clFuchsia;
    FMARLineSeriesIRR.SeriesColor   := clFuchsia;
    FMARLineSeriesRAN.SeriesColor   := clFuchsia;
    FMARLineSeriesURB.SeriesColor   := clFuchsia;

    FMARLineSeriesINC.LinePen.Style := psDash;
    FMARLineSeriesRNK.LinePen.Style := psDash;
    FMARLineSeriesAFF.LinePen.Style := psDash;
    FMARLineSeriesIRR.LinePen.Style := psDash;
    FMARLineSeriesRAN.LinePen.Style := psDash;
    FMARLineSeriesURB.LinePen.Style := psDash;

    FINCLineSeriesM.SeriesColor  := clBlue;
    FAFFLineSeriesM.SeriesColor  := clPurple;
    FIRRLineSeriesM.SeriesColor  := clGreen;
    FRANLineSeriesM.SeriesColor  := clNavy;
    FNETTLineSeriesM.SeriesColor := clRed;
    FURBLineSeriesM.SeriesColor  := clLime;

    ConfigureLineSeries(FINCLineSeriesA, vtAnnual);
    ConfigureLineSeries(FRNKLineSeriesA, vtAnnual);
    ConfigureLineSeries(FAFFLineSeriesA, vtAnnual);
    ConfigureLineSeries(FIRRLineSeriesA, vtAnnual);
    ConfigureLineSeries(FRANLineSeriesA, vtAnnual);
    ConfigureLineSeries(FNETTLineSeriesA, vtAnnual);
    ConfigureLineSeries(FURBLineSeriesA,  vtAnnual);

    ConfigureLineSeries(FINCLineSeriesM, vtMonthly);
    ConfigureLineSeries(FRNKLineSeriesM, vtMonthly);
    ConfigureLineSeries(FAFFLineSeriesM, vtMonthly);
    ConfigureLineSeries(FIRRLineSeriesM, vtMonthly);
    ConfigureLineSeries(FRANLineSeriesM, vtMonthly);
    ConfigureLineSeries(FNETTLineSeriesM, vtMonthly);
    ConfigureLineSeries(FURBLineSeriesM,  vtMonthly);

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirCatchmentProportionsDialog.LanguageHasChanged: boolean;
const OPNAME = 'TReservoirCatchmentProportionsDialog.LanguageHasChanged';
var
  lColCount : integer;
begin
  Result := inherited LanguageHasChanged;
  try
    lColCount := 2;
    if (FAppModules.Model.ModelName = CPlanning) then
      lColCount := 3;

    if Assigned(FGrdProportions) and (FGrdProportions.RowCount > 0) and (FGrdProportions.ColCount > lColCount) then
    begin
      FGrdProportions.Cells[0,0] := FAppModules.Language.GetString('GridHeading.FileInc');
      FGrdProportions.Cells[1,0] := FAppModules.Language.GetString('GridHeading.FileAff');
      FGrdProportions.Cells[2,0] := FAppModules.Language.GetString('GridHeading.FileIrr');
      if (lColCount = 3) then
        FGrdProportions.Cells[3,0] := FAppModules.Language.GetString('GridHeading.FileUrb');
    end;

    FRadChartView.Items.Strings[0] := FAppModules.Language.GetString('GridHeading.FileIncItem2');
    FRadChartView.Items.Strings[1] := FAppModules.Language.GetString('GridHeading.FileIncItem1');

    FRadFiles.Items.Strings[0] := 'INC';//FAppModules.Language.GetString('GridHeading.FileIncItem2');
    FRadFiles.Items.Strings[1] := 'AFF';//FAppModules.Language.GetString('GridHeading.FileIncItem2');
    FRadFiles.Items.Strings[2] := 'IRR';//FAppModules.Language.GetString('GridHeading.FileIncItem2');
    //FRadFiles.Items.Strings[3] := 'RNK';//FAppModules.Language.GetString('GridHeading.FileIncItem2');
    FRadFiles.Items.Strings[3] := 'RAN';//FAppModules.Language.GetString('GridHeading.FileIncItem2');
    if (FRadFiles.Columns = 5) then
      FRadFiles.Items.Strings[4] := 'URB';

    FLblCatchmentArea.Caption := FAppModules.Language.GetString('TField.CatchmentAreaParam');
    FLblAllCatchment.Caption  := FAppModules.Language.GetString('TCatchmentProportionsDlg.AllCatchments');
    FLblAction.Caption  := '';
    //FLblAction.Caption  := 'Action(Add/Remove)';
    FLblCurrentCatchment.Caption  := FAppModules.Language.GetString('TCatchmentProportionsDlg.CurrentCatchment');
    FLblProportions.Caption  := FAppModules.Language.GetString('TCatchmentProportionsDlg.Proportions');

    FBtnAdd.Hint    := FAppModules.Language.GetString('TCatchmentProportionsDlg.AddButtonHint');
    FBtnRemove.Hint := FAppModules.Language.GetString('TCatchmentProportionsDlg.RemoveButtonHint');

    FRadFiles.Hint     := FAppModules.Language.GetString('TCatchmentProportionsDlg.NettRunOff');

    FRadFiles.Hints.Add(FAppModules.Language.GetString('TCatchmentProportionsDlg.DrainageFactor'));
    FRadFiles.Hints.Add(FAppModules.Language.GetString('TCatchmentProportionsDlg.AfforestationFactor'));
    FRadFiles.Hints.Add(FAppModules.Language.GetString('TCatchmentProportionsDlg.IrrigationFactor'));
    FRadFiles.Hints.Add(FAppModules.Language.GetString('TCatchmentProportionsDlg.PointRainfallFactor'));
    FRadFiles.Hints.Add(FAppModules.Language.GetString('TCatchmentProportionsDlg.UrbanRunoffPortion'));

    //    FRadChartView.Hint := FAppModules.Language.GetString('TCatchmentProportionsDlg.IncrementalRunoffA');
    FRadChartView.Hints.Add(FAppModules.Language.GetString('TCatchmentProportionsDlg.NettValuesFile'));
    FRadChartView.Hints.Add(FAppModules.Language.GetString('TCatchmentProportionsDlg.IndividualValuesFiles'));
    
    FRadMonthlyAnnual.Items.Clear;
    FRadMonthlyAnnual.Items.Add('Annual');
    FRadMonthlyAnnual.Items.Add('Monthly');
    FRadMonthlyAnnual.Hints.Add(FAppModules.Language.GetString('TCatchmentProportionsDlg.AnnualValues'));
    FRadMonthlyAnnual.Hints.Add(FAppModules.Language.GetString('TCatchmentProportionsDlg.MonthlyValues'));
    FRadMonthlyAnnual.ItemIndex := 0;
    FChkMAR.Caption := 'MAR';

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsDialog.DrawTotalLines;
const OPNAME = 'TReservoirCatchmentProportionsDialog.DrawTotalLines';
begin
  try
    if (ARow = (FGrdProportions.RowCount - 1)) then
    begin
      FGrdProportions.Canvas.Pen.Color := clBlack;
      FGrdProportions.Canvas.Pen.Style := VCL.Graphics.TPenStyle(psSolid);
      FGrdProportions.Canvas.MoveTo(ARect.Left,  ARect.Top - 1);
      FGrdProportions.Canvas.LineTo(ARect.Right, ARect.Top - 1);
      FGrdProportions.Canvas.MoveTo(ARect.Left,  ARect.Bottom);
      FGrdProportions.Canvas.LineTo(ARect.Right, ARect.Bottom);
      FGrdProportions.Canvas.MoveTo(ARect.Left,  ARect.Bottom - 2);
      FGrdProportions.Canvas.LineTo(ARect.Right, ARect.Bottom - 2);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsDialog.SetGridRowCount(const Value: integer);
const OPNAME = 'TReservoirCatchmentProportionsDialog.SetGridRowCount';
var LIndex : integer;
begin
  try
    FGridRowCount := Value;
    FGrdProportions.RowCount := Value;
    for LIndex := 1 to FGrdProportions.RowCount - 1 do
      FGrdProportions.RowHeights[LIndex] := FGrdProportions.DefaultRowHeight + 2 * Ord(LIndex = FGrdProportions.RowCount - 1);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsDialog.OnRadNetFilesClick(Sender: TObject);
const OPNAME = 'TReservoirCatchmentProportionsDialog.OnRadNetFilesClick';
var
  LTitle: string;
begin
  try
    FChtTimeSeries.UndoZoom;
    FINCLineSeriesA.ParentChart  := nil;
    FRNKLineSeriesA.ParentChart  := nil;
    FAFFLineSeriesA.ParentChart  := nil;
    FIRRLineSeriesA.ParentChart  := nil;
    FRANLineSeriesA.ParentChart  := nil;
    FNETTLineSeriesA.ParentChart := nil;
    FURBLineSeriesA.ParentChart  := nil;

    FMARLineSeriesINC.ParentChart  := nil;
    FMARLineSeriesRNK.ParentChart  := nil;
    FMARLineSeriesAFF.ParentChart  := nil;
    FMARLineSeriesIRR.ParentChart  := nil;
    FMARLineSeriesRAN.ParentChart  := nil;
    FMARLineSeriesURB.ParentChart  := nil;

    FINCLineSeriesM.ParentChart  := nil;
    FRNKLineSeriesM.ParentChart  := nil;
    FAFFLineSeriesM.ParentChart  := nil;
    FIRRLineSeriesM.ParentChart  := nil;
    FRANLineSeriesM.ParentChart  := nil;
    FNETTLineSeriesM.ParentChart := nil;
    FURBLineSeriesM.ParentChart  := nil;

    FChtTimeSeries.Title.Text.Text := '';
    FChtTimeSeries.Legend.Visible := False;
    case FRadChartView.ItemIndex of
      0:begin
          CalculateStatistics;
          FRadFiles.IsEnabled         := False;
          FRadMonthlyAnnual.IsEnabled := False;
          FChkMAR.IsEnabled           := False;
          FChtTimeSeries.Title.Text.Text := FAppModules.Language.GetString('TCatchmentProportionsDlg.ChartTitleNett');
          LTitle := FAppModules.Language.GetString('TCatchmentProportionsDlg.NETTSeriesTitle');
          FNETTLineSeriesA.Title := Format(LTitle,[FormatFloat('###0.###',FAvarage),
                                                   FormatFloat('###0.###',FStdDeviation),
                                                   IntToStr(FNegNumbers)]);
          FChtTimeSeries.Legend.Visible := True;
          FNETTLineSeriesA.ParentChart := FChtTimeSeries;
          CalculateLeftAxisMinMax(FNETTLineSeriesA);
          ConfigureChartBottomAxis(FNETTLineSeriesA, vtAnnual);
          FChtTimeSeries.LeftAxis.Title.Caption := FAppModules.Language.GetString('TCatchmentProportionsDlg.NettRunoff');
        end;
      1:begin
          FRadFiles.IsEnabled         := True;
          FRadMonthlyAnnual.IsEnabled := True;
          FChkMAR.IsEnabled           := True;
          OnRadFilesClick(FRadFiles);
        end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsDialog.OnRadFilesClick(Sender: TObject);
const OPNAME = 'TReservoirCatchmentProportionsDialog.OnRadFilesClick';
var
  LFileName: string;
begin
  try
    
    FChtTimeSeries.UndoZoom;
    FINCLineSeriesA.Active := False;
    FRNKLineSeriesA.Active := False;
    FAFFLineSeriesA.Active := False;
    FIRRLineSeriesA.Active := False;
    FRANLineSeriesA.Active := False;
    FURBLineSeriesA.Active := False;

    FMARLineSeriesINC.Active := False;
    FMARLineSeriesRNK.Active := False;
    FMARLineSeriesAFF.Active := False;
    FMARLineSeriesIRR.Active := False;
    FMARLineSeriesRAN.Active := False;
    FMARLineSeriesURB.Active := False;

    FINCLineSeriesM.Active := False;
    FRNKLineSeriesM.Active := False;
    FAFFLineSeriesM.Active := False;
    FIRRLineSeriesM.Active := False;
    FRANLineSeriesM.Active := False;
    FURBLineSeriesM.Active := False;

    FINCLineSeriesA.ParentChart := nil;
    FRNKLineSeriesA.ParentChart := nil;
    FAFFLineSeriesA.ParentChart := nil;
    FIRRLineSeriesA.ParentChart := nil;
    FRANLineSeriesA.ParentChart := nil;
    FURBLineSeriesA.ParentChart := nil;

    FMARLineSeriesINC.ParentChart := nil;
    FMARLineSeriesRNK.ParentChart := nil;
    FMARLineSeriesAFF.ParentChart := nil;
    FMARLineSeriesIRR.ParentChart := nil;
    FMARLineSeriesRAN.ParentChart := nil;
    FMARLineSeriesURB.ParentChart := nil;

    FINCLineSeriesM.ParentChart := nil;
    FRNKLineSeriesM.ParentChart := nil;
    FAFFLineSeriesM.ParentChart := nil;
    FIRRLineSeriesM.ParentChart := nil;
    FRANLineSeriesM.ParentChart := nil;
    FURBLineSeriesM.ParentChart := nil;

    if(FRadChartView.ItemIndex = 1) then
    begin
      FNETTLineSeriesA.ParentChart := nil;
      FChtTimeSeries.Title.Text.Text := '';
      FChtTimeSeries.LeftAxis.Title.Caption := '';
      FChtTimeSeries.Legend.Visible := True;

      LFileName := '';
      if((FRadFiles.ItemIndex >= 0) and (FRadFiles.ItemIndex < FFileNameContainer.Count)) then
         LFileName := FFileNameContainer.Strings[FRadFiles.ItemIndex];
      LFileName := ExtractFileName(LFileName);

      case FRadFiles.ItemIndex of
        0: DispalyINCChart(LFileName);
        1: DispalyAFFChart(LFileName);
        2: DispalyIRRChart(LFileName);
        //3: DispalyRNKChart(LFileName);
        3: DispalyRANChart(LFileName);
        4: DispalyURBChart(LFileName);
      end;

      if (FRadFiles.ItemIndex >= 0) then
      begin
        if (Trim(LFileName) = '') then
          FChtTimeSeries.Title.Text.Text := FAppModules.Language.GetString('TCatchmentProportionsDlg.FileNotFound');
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsDialog.SelectCellEvent(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
const OPNAME = 'TReservoirCatchmentProportionsDialog.SelectCellEvent';
begin
  try
    case ACol of
      0: FRadFiles.ItemIndex := 0;
      1: FRadFiles.ItemIndex := 1;
      2: FRadFiles.ItemIndex := 2;
      3:
      begin
        if (FRadFiles.Columns = 5) then
          FRadFiles.ItemIndex := 4;
      end;
    end;
    OnRadFilesClick(FRadFiles);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirCatchmentProportionsDialog.DispalyAFFChart(AFileName : string): boolean;
const OPNAME = 'TReservoirCatchmentProportionsDialog.DispalyAFFChart';
var
  LTitle: string;
begin
  Result := False;
  try
    {if(TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.StreamFlowReductionList.StreamFlowReductionCount > 0) then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.AFFfilesNotUsed'));
      Exit;
    end;
     }
    if (FRadChartView.ItemIndex = 1) then
    begin
      if(Trim(AFileName) <> '') then
      begin
        case FRadMonthlyAnnual.ItemIndex of
          0:
          begin
            CalculateStatistics;
            FAFFLineSeriesA.ParentChart := FChtTimeSeries;
            FChtTimeSeries.Title.Text.Text := FAppModules.Language.GetString('TCatchmentProportionsDlg.ChartTitleFile')
                                              + AFileName;
            LTitle := FAppModules.Language.GetString('TCatchmentProportionsDlg.NETTSeriesTitle');
            FAFFLineSeriesA.Title := Format(LTitle,[FormatFloat('###0.###',FAvarage),
                                                   FormatFloat('###0.###',FStdDeviation),
                                                   IntToStr(FNegNumbers)]);
            FChtTimeSeries.LeftAxis.Title.Caption := FAppModules.Language.GetString('TCatchmentProportionsDlg.AfforestationUseA');
            CalculateLeftAxisMinMax(FAFFLineSeriesA);
            CalculateLeftAxisMinMax(FMARLineSeriesAFF);
            ConfigureChartBottomAxis(FAFFLineSeriesA, vtAnnual);
            FAFFLineSeriesA.Active  := True;
          end;
          1:
          begin
            CalculateStatistics;
            FAFFLineSeriesM.ParentChart := FChtTimeSeries;
            //FMARLineSeriesINC.ParentChart := FChtTimeSeries;
            FChtTimeSeries.Title.Text.Text := FAppModules.Language.GetString('TCatchmentProportionsDlg.ChartTitleFile')
                                             + AFileName;
            LTitle := FAppModules.Language.GetString('TCatchmentProportionsDlg.NETTSeriesTitle');
            FAFFLineSeriesM.Title := Format(LTitle,[FormatFloat('###0.###',FAvarage),
                                                   FormatFloat('###0.###',FStdDeviation),
                                                   IntToStr(FNegNumbers)]);
            FChtTimeSeries.LeftAxis.Title.Caption := FAppModules.Language.GetString('TCatchmentProportionsDlg.AfforestationUseM');
            CalculateLeftAxisMinMax(FAFFLineSeriesM);
            ConfigureChartBottomAxis(FAFFLineSeriesM, vtMonthly);
            FAFFLineSeriesM.Active := True;
          end;
        end;
      end;
      if ChkMAR.Checked = true then
        ToggleMARLines(FMARLineSeriesAFF);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirCatchmentProportionsDialog.DispalyINCChart(AFileName : string): boolean;
const OPNAME = 'TReservoirCatchmentProportionsDialog.DispalyINCChart';
var
  LTitle: string;
begin
  Result := False;
  try
    if (FRadChartView.ItemIndex = 1) then
    begin
      if(Trim(AFileName) <> '') then
      begin
        case FRadMonthlyAnnual.ItemIndex of
          0:
          begin
            CalculateStatistics;
            FINCLineSeriesA.ParentChart := FChtTimeSeries;
            //FMARLineSeriesINC.ParentChart := FChtTimeSeries;
            FChtTimeSeries.Title.Text.Text := FAppModules.Language.GetString('TCatchmentProportionsDlg.ChartTitleFile')
                                             + AFileName;
            LTitle := FAppModules.Language.GetString('TCatchmentProportionsDlg.NETTSeriesTitle');
            FINCLineSeriesA.Title := Format(LTitle,[FormatFloat('###0.###',FAvarage),
                                                   FormatFloat('###0.###',FStdDeviation),
                                                   IntToStr(FNegNumbers)]);
            FChtTimeSeries.LeftAxis.Title.Caption := FAppModules.Language.GetString('TCatchmentProportionsDlg.IncrementalRunoffA');
            CalculateLeftAxisMinMax(FINCLineSeriesA);
            CalculateLeftAxisMinMax(FMARLineSeriesINC);
            ConfigureChartBottomAxis(FINCLineSeriesA, vtAnnual);
            FINCLineSeriesA.Active := True;
          end;
          1:
          begin
            CalculateStatistics;
            FINCLineSeriesM.ParentChart := FChtTimeSeries;
            //FMARLineSeriesINC.ParentChart := FChtTimeSeries;
            FChtTimeSeries.Title.Text.Text := FAppModules.Language.GetString('TCatchmentProportionsDlg.ChartTitleFile')
                                             + AFileName;
            LTitle := FAppModules.Language.GetString('TCatchmentProportionsDlg.NETTSeriesTitle');
            FINCLineSeriesM.Title := Format(LTitle,[FormatFloat('###0.###',FAvarage),
                                                   FormatFloat('###0.###',FStdDeviation),
                                                   IntToStr(FNegNumbers)]);
            FChtTimeSeries.LeftAxis.Title.Caption := FAppModules.Language.GetString('TCatchmentProportionsDlg.IncrementalRunoffM');
            CalculateLeftAxisMinMax(FINCLineSeriesM);
            ConfigureChartBottomAxis(FINCLineSeriesM, vtMonthly);
            FINCLineSeriesM.Active := True;
          end;
        end;
      end;
      if ChkMAR.Checked = true then
        ToggleMARLines(FMARLineSeriesINC);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirCatchmentProportionsDialog.DispalyIRRChart(AFileName : string): boolean;
const OPNAME = 'TReservoirCatchmentProportionsDialog.DispalyIRRChart';
var
  LTitle: string;
begin
  Result := False;
  try
    if (FRadChartView.ItemIndex = 1) then
    begin
      if(Trim(AFileName) <> '') then
      begin
        case FRadMonthlyAnnual.ItemIndex of
          0:
          begin
            CalculateStatistics;
            FIRRLineSeriesA.ParentChart := FChtTimeSeries;
            FChtTimeSeries.Title.Text.Text := FAppModules.Language.GetString('TCatchmentProportionsDlg.ChartTitleFile')
                                              + AFileName;
            LTitle := FAppModules.Language.GetString('TCatchmentProportionsDlg.NETTSeriesTitle');
            FIRRLineSeriesA.Title := Format(LTitle,[FormatFloat('###0.###',FAvarage),
                                                   FormatFloat('###0.###',FStdDeviation),
                                                   IntToStr(FNegNumbers)]);
            FChtTimeSeries.LeftAxis.Title.Caption := FAppModules.Language.GetString('TCatchmentProportionsDlg.DiffuseDemandA');
            CalculateLeftAxisMinMax(FIRRLineSeriesA);
            CalculateLeftAxisMinMax(FMARLineSeriesIRR);
            ConfigureChartBottomAxis(FIRRLineSeriesA, vtAnnual);
            FIRRLineSeriesA.Active := True;
          end;
          1:
          begin
            CalculateStatistics;
            FIRRLineSeriesM.ParentChart := FChtTimeSeries;
            //FMARLineSeriesINC.ParentChart := FChtTimeSeries;
            FChtTimeSeries.Title.Text.Text := FAppModules.Language.GetString('TCatchmentProportionsDlg.ChartTitleFile')
                                             + AFileName;
            LTitle := FAppModules.Language.GetString('TCatchmentProportionsDlg.NETTSeriesTitle');
            FIRRLineSeriesM.Title := Format(LTitle,[FormatFloat('###0.###',FAvarage),
                                                   FormatFloat('###0.###',FStdDeviation),
                                                   IntToStr(FNegNumbers)]);
            FChtTimeSeries.LeftAxis.Title.Caption := FAppModules.Language.GetString('TCatchmentProportionsDlg.DiffuseDemandM');
            CalculateLeftAxisMinMax(FIRRLineSeriesM);
            ConfigureChartBottomAxis(FIRRLineSeriesM, vtMonthly);
            FIRRLineSeriesM.Active := True;
          end;
        end;
      end;
      if ChkMAR.Checked = true then
        ToggleMARLines(FMARLineSeriesIRR);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirCatchmentProportionsDialog.DispalyRANChart(AFileName: string): boolean;
const OPNAME = 'TReservoirCatchmentProportionsDialog.DispalyRANChart';
var
  LTitle: string;
begin
  Result := False;
  try
    if (FRadChartView.ItemIndex = 1) then
    begin
      if(Trim(AFileName) <> '') then
      begin
        case FRadMonthlyAnnual.ItemIndex of
          0:
          begin
            CalculateStatistics;
            FRANLineSeriesA.ParentChart := FChtTimeSeries;
            FChtTimeSeries.Title.Text.Text := FAppModules.Language.GetString('TCatchmentProportionsDlg.ChartTitleFile')
                                              + AFileName;
            LTitle := FAppModules.Language.GetString('TCatchmentProportionsDlg.NETTSeriesTitle');
            FRANLineSeriesA.Title := Format(LTitle,[FormatFloat('###0.###',FAvarage),
                                                   FormatFloat('###0.###',FStdDeviation),
                                                   IntToStr(FNegNumbers)]);
            FChtTimeSeries.LeftAxis.Title.Caption := FAppModules.Language.GetString('TCatchmentProportionsDlg.PointRainfallA');
            CalculateLeftAxisMinMax(FRANLineSeriesA);
            CalculateLeftAxisMinMax(FMARLineSeriesRAN);
            ConfigureChartBottomAxis(FRANLineSeriesA, vtAnnual);
            FRANLineSeriesA.Active := True;
          end;
          1:
          begin
            CalculateStatistics;
            FRANLineSeriesM.ParentChart := FChtTimeSeries;
            //FMARLineSeriesINC.ParentChart := FChtTimeSeries;
            FChtTimeSeries.Title.Text.Text := FAppModules.Language.GetString('TCatchmentProportionsDlg.ChartTitleFile')
                                             + AFileName;
            LTitle := FAppModules.Language.GetString('TCatchmentProportionsDlg.NETTSeriesTitle');
            FRANLineSeriesM.Title := Format(LTitle,[FormatFloat('###0.###',FAvarage),
                                                   FormatFloat('###0.###',FStdDeviation),
                                                   IntToStr(FNegNumbers)]);
            FChtTimeSeries.LeftAxis.Title.Caption := FAppModules.Language.GetString('TCatchmentProportionsDlg.PointRainfallA');
            CalculateLeftAxisMinMax(FRANLineSeriesM);
            ConfigureChartBottomAxis(FRANLineSeriesM, vtMonthly);
            FRANLineSeriesM.Active := True;
          end;
        end;
      end;
      if ChkMAR.Checked = true then
        ToggleMARLines(FMARLineSeriesRAN);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirCatchmentProportionsDialog.DispalyURBChart(AFileName: string): boolean;
const OPNAME = 'TReservoirCatchmentProportionsDialog.DispalyURBChart';
var
  LTitle: string;
begin
  Result := False;
  try
    if (FRadChartView.ItemIndex = 1) then
    begin
      if(Trim(AFileName) <> '') then
      begin
        case FRadMonthlyAnnual.ItemIndex of
          0:
          begin
            CalculateStatistics;
            FURBLineSeriesA.ParentChart    := FChtTimeSeries;
            FChtTimeSeries.Title.Text.Text := FAppModules.Language.GetString('TCatchmentProportionsDlg.ChartTitleFile')
                                              + AFileName;
            LTitle := FAppModules.Language.GetString('TCatchmentProportionsDlg.NETTSeriesTitle');
            FURBLineSeriesA.Title := Format(LTitle,[FormatFloat('###0.###',FAvarage),
                                                   FormatFloat('###0.###',FStdDeviation),
                                                   IntToStr(FNegNumbers)]);
            FChtTimeSeries.LeftAxis.Title.Caption := FAppModules.Language.GetString('TCatchmentProportionsDlg.UrbanRunoffA');
            CalculateLeftAxisMinMax(FURBLineSeriesA);
            CalculateLeftAxisMinMax(FMARLineSeriesURB);
            ConfigureChartBottomAxis(FURBLineSeriesA, vtAnnual);
            FURBLineSeriesA.Active := True;
          end;
          1:
          begin
            CalculateStatistics;
            FURBLineSeriesM.ParentChart := FChtTimeSeries;
            FChtTimeSeries.Title.Text.Text := FAppModules.Language.GetString('TCatchmentProportionsDlg.ChartTitleFile')
                                             + AFileName;
            LTitle := FAppModules.Language.GetString('TCatchmentProportionsDlg.NETTSeriesTitle');
            FURBLineSeriesM.Title := Format(LTitle,[FormatFloat('###0.###',FAvarage),
                                                   FormatFloat('###0.###',FStdDeviation),
                                                   IntToStr(FNegNumbers)]);
            FChtTimeSeries.LeftAxis.Title.Caption := FAppModules.Language.GetString('TCatchmentProportionsDlg.UrbanRunoffM');
            CalculateLeftAxisMinMax(FURBLineSeriesM);
            ConfigureChartBottomAxis(FURBLineSeriesM, vtMonthly);
            FURBLineSeriesM.Active := True;
          end;
        end;
      end;
      if ChkMAR.Checked = true then
        ToggleMARLines(FMARLineSeriesURB);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirCatchmentProportionsDialog.DispalyRNKChart(AFileName: string): boolean;
const OPNAME = 'TReservoirCatchmentProportionsDialog.DispalyRNKChart';
var
  LTitle: string;
begin
  Result := False;
  try
    if (FRadChartView.ItemIndex = 1) then
    begin
      if(Trim(AFileName) <> '') then
      begin
        case FRadMonthlyAnnual.ItemIndex of
          0:
          begin
            CalculateStatistics;
            FRNKLineSeriesA.ParentChart := FChtTimeSeries;
            FChtTimeSeries.Title.Text.Text := FAppModules.Language.GetString('TCatchmentProportionsDlg.ChartTitleFile')
                                              + AFileName;
            LTitle := FAppModules.Language.GetString('TCatchmentProportionsDlg.NETTSeriesTitle');
            FRNKLineSeriesA.Title := Format(LTitle,[FormatFloat('###0.###',FAvarage),
                                                   FormatFloat('###0.###',FStdDeviation),
                                                   IntToStr(FNegNumbers)]);
            FChtTimeSeries.LeftAxis.Title.Caption := FAppModules.Language.GetString('TCatchmentProportionsDlg.RankedA');
            CalculateLeftAxisMinMax(FRNKLineSeriesA);
            CalculateLeftAxisMinMax(FMARLineSeriesRNK);
            ConfigureChartBottomAxis(FRNKLineSeriesA, vtAnnual);
            FRNKLineSeriesA.Active := True;
          end;
          1:
          begin
            CalculateStatistics;
            FRNKLineSeriesM.ParentChart := FChtTimeSeries;
            //FMARLineSeriesINC.ParentChart := FChtTimeSeries;
            FChtTimeSeries.Title.Text.Text := FAppModules.Language.GetString('TCatchmentProportionsDlg.ChartTitleFile')
                                             + AFileName;
            LTitle := FAppModules.Language.GetString('TCatchmentProportionsDlg.NETTSeriesTitle');
            FRNKLineSeriesM.Title := Format(LTitle,[FormatFloat('###0.###',FAvarage),
                                                   FormatFloat('###0.###',FStdDeviation),
                                                   IntToStr(FNegNumbers)]);
            FChtTimeSeries.LeftAxis.Title.Caption := FAppModules.Language.GetString('TCatchmentProportionsDlg.RankedM');
            CalculateLeftAxisMinMax(FRNKLineSeriesM);
            ConfigureChartBottomAxis(FRNKLineSeriesM, vtMonthly);
            FRNKLineSeriesM.Active := True;
          end;
        end;
      end;
      if ChkMAR.Checked = true then
        ToggleMARLines(FMARLineSeriesRNK);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirCatchmentProportionsDialog.RefreshTimeSeriesChart: boolean;
const OPNAME = 'TReservoirCatchmentProportionsDialog.RefreshTimeSeriesChart';
begin
  Result := False;
  try
    OnRadNetFilesClick(Nil);
    OnRadFilesClick(Nil);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsDialog.RestoreColourState;
const OPNAME = 'TReservoirCatchmentProportionsDialog.RestoreColourState';
var
  LIndex : integer;
begin
  inherited RestoreColourState;
  try
    for LIndex := 0 to ControlsOwner.ComponentCount - 1 do
      if ControlsOwner.Components[LIndex].ClassName = TFieldEdit.ClassName then
        if TFieldEdit(ControlsOwner.Components[LIndex]).Color = clRed then
          TFieldEdit(ControlsOwner.Components[LIndex]).Color := clWindow;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsDialog.ShowTreeViewHint(AActionHint: TActionHint);
const OPNAME = 'TReservoirCatchmentProportionsDialog.ShowTreeViewHint';
begin
  try
    case AActionHint of
      allNone      : self.Hint := '';
      allReservoir : self.Hint := FAppModules.Language.GetString('TCatchmentProportionsDlg.ActionHintAllReservoir');
      allCatchment : self.Hint := FAppModules.Language.GetString('TCatchmentProportionsDlg.ActionHintAllCatchment');
      curReservoir : self.Hint := FAppModules.Language.GetString('TCatchmentProportionsDlg.ActionHintCurReservoir');
      curCatchment : self.Hint := FAppModules.Language.GetString('TCatchmentProportionsDlg.ActionHintCurCatchment');
    else
      self.Hint    := '';
    end;
    ShowCurrentHint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsDialog.PopulateFileNames(AFileNameContainer: TStringList);
const OPNAME = 'TReservoirCatchmentProportionsDialog.PopulateFileNames';
var
  LFileExt,
  LFileName: string;
  LCount: integer;
begin
  try
    FFileNameContainer.Clear;
    for LCount := 0 to 4 do
      FFileNameContainer.Add('');
    if Assigned(AFileNameContainer) then
    begin
      for LCount := 0 to AFileNameContainer.Count - 1  do
      begin
        LFileName := AFileNameContainer[LCount];
        LFileName := ExtractFileName(LFileName);
        LFileExt := UpperCase(ExtractFileExt(LFileName));
        if LFileExt = '.INC' then
          FFileNameContainer.Strings[0] := LFileName;
        if LFileExt = '.AFF' then
          FFileNameContainer.Strings[1] := LFileName;
        if LFileExt = '.IRR' then
          FFileNameContainer.Strings[2] := LFileName;
        //if LFileExt = '.RNK' then
        ///  FFileNameContainer.Strings[3] := LFileName;
        if LFileExt = '.RAN' then
          FFileNameContainer.Strings[3] := LFileName;
        if LFileExt = '.URB' then
          FFileNameContainer.Strings[4] := LFileName;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsDialog.CalculateLeftAxisMinMax(ALineSeries: TLineSeries);
const OPNAME = 'TReservoirCatchmentProportionsDialog.CalculateLeftAxisMinMax';
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

      if (LMinValue > FChtTimeSeries.LeftAxis.Maximum) then
      begin
        FChtTimeSeries.LeftAxis.Maximum := LMaxValue;
        FChtTimeSeries.LeftAxis.Minimum := LMinValue;
      end;
      if (LMaxValue < FChtTimeSeries.LeftAxis.Minimum) then
      begin
        FChtTimeSeries.LeftAxis.Minimum := LMinValue;
        FChtTimeSeries.LeftAxis.Maximum := LMaxValue;
      end;

      {FChtTimeSeries.LeftAxis.TickLength      := 6;
      FChtTimeSeries.LeftAxis.MinorTickCount  := 4;
      FChtTimeSeries.LeftAxis.MinorTickLength := 4;
      }
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsDialog.ConfigureLineSeries( ALineSeries: TLineSeries; AValueType : TValueType);
const OPNAME = 'TReservoirCatchmentProportionsDialog.ConfigureLineSeries';
begin
  try
    if Assigned(ALineSeries) then
    begin
      case AValueType of
        vtAnnual: ALineSeries.XValues.DateTime := False;
        vtMonthly: ALineSeries.XValues.DateTime := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsDialog.ConfigureChartBottomAxis(ALineSeries : TLineSeries; AValueType : TValueType);
const OPNAME = 'TReservoirCatchmentProportionsDialog.ConfigureChartBottomAxis';
begin
  try
    //if Assigned(ALineSeries)  and (ALineSeries.VisibleCount > 0)then
    //begin
    case AValueType of
      vtAnnual:
        FChtTimeSeries.BottomAxis.AxisValuesFormat  := '###0';
      vtMonthly:
        FChtTimeSeries.BottomAxis.DateTimeFormat  := 'yyyy/mm';
    end;

      {if (ALineSeries.VisibleCount <= (5*365))  then
      begin
        FChtTimeSeries.BottomAxis.Increment := DateTimeStep[dtOneMonth ];
      end
      else
      begin
        FChtTimeSeries.BottomAxis.Increment := DateTimeStep[dtOneYear ];
      end;
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsDialog.AssignHelpContext;
const OPNAME = 'TReservoirCatchmentProportionsDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                 HC_DefiningCatchmentHydrologyInflows);
    
    SetControlHelpContext(FTvwAllCatchment,     HC_DefiningCatchmentHydrologyInflows);
    SetControlHelpContext(FTvwCurrentCatchment, HC_DefiningCatchmentHydrologyInflows);

    SetControlHelpContext(FBtnAdd,              HC_DefiningCatchmentHydrologyInflows);
    SetControlHelpContext(FBtnRemove,           HC_DefiningCatchmentHydrologyInflows);

    SetControlHelpContext(FGrdProportions,      HC_DefiningCatchmentHydrologyInflows);

    SetControlHelpContext(FRadFiles,            HC_DefiningCatchmentHydrologyInflows);
    SetControlHelpContext(FRadChartView,        HC_DefiningCatchmentHydrologyInflows);

    SetControlHelpContext(VCL.Controls.TControl(FChtTimeSeries),       HC_DefiningCatchmentHydrologyInflows);

    SetControlHelpContext(FPanelButton,         HC_DefiningCatchmentHydrologyInflows);

    SetControlHelpContext(FPanelCatchments,     HC_DefiningCatchmentHydrologyInflows);
    SetControlHelpContext(FPanelRadioGroup,     HC_DefiningCatchmentHydrologyInflows);
    SetControlHelpContext(FPanelLabel,          HC_DefiningCatchmentHydrologyInflows);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsDialog.ToggleMARLines(ALineSeries : TLineSeries);
const OPNAME = 'TReservoirCatchmentProportionsDialog.ToggleMARLines';
begin
  try
    if Assigned(ALineSeries) then
    begin
      FMARLineSeriesINC.Active      := False;
      FMARLineSeriesRNK.Active      := False;
      FMARLineSeriesAFF.Active      := False;
      FMARLineSeriesIRR.Active      := False;
      FMARLineSeriesRAN.Active      := False;
      FMARLineSeriesURB.Active      := False;

      FMARLineSeriesINC.ParentChart := nil;
      FMARLineSeriesRNK.ParentChart := nil;
      FMARLineSeriesAFF.ParentChart := nil;
      FMARLineSeriesIRR.ParentChart := nil;
      FMARLineSeriesRAN.ParentChart := nil;
      FMARLineSeriesURB.ParentChart := nil;

      if FChkMAR.Checked then
        ALineSeries.ParentChart := FChtTimeSeries
      else
        ALineSeries.ParentChart := nil;
      if Assigned(ALineSeries.ParentChart) then
        ALineSeries.Active := FChkMAR.Checked;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsDialog.ToggleMonthlyAnnual;
const OPNAME = 'TReservoirCatchmentProportionsDialog.ToggleMonthlyAnnual';
begin
  try
    OnRadFilesClick(nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsDialog.CalculateStatistics;
const OPNAME = 'TReservoirCatchmentProportionsDialog.CalculateStatistics';
var
 LYValues       : array of double;
 LCount         : integer;
 LCurrentSeries : TLineSeries;
begin
  try
    FAvarage         := 0;
    FStdDeviation    := 0;
    FNegNumbers      := 0;
    LCurrentSeries   := nil;
    try
      if FRadChartView.ItemIndex = 0 then
      begin
         LCurrentSeries := FNETTLineSeriesA;
      end
      else if FRadChartView.ItemIndex = 1 then
      begin
        case FRadFiles.ItemIndex of
            0:
            begin
              if FRadMonthlyAnnual.ItemIndex = 0 then
                LCurrentSeries    := FINCLineSeriesA
              else
                LCurrentSeries := FINCLineSeriesM;
            end;
            1:
            begin
              if FRadMonthlyAnnual.ItemIndex = 0 then
                LCurrentSeries    := FAFFLineSeriesA
              else
                LCurrentSeries := FAFFLineSeriesM;
            end;
            2:
            begin
              if FRadMonthlyAnnual.ItemIndex = 0 then
                LCurrentSeries := FIRRLineSeriesA
              else
                LCurrentSeries := FIRRLineSeriesM;
            end;
            3:
            begin
              if FRadMonthlyAnnual.ItemIndex = 0 then
                LCurrentSeries    := FRANLineSeriesA
              else
                LCurrentSeries := FRANLineSeriesM;
            end;
            4:
            begin
              if FRadMonthlyAnnual.ItemIndex = 0 then
                LCurrentSeries := FURBLineSeriesA
              else
                LCurrentSeries := FURBLineSeriesM; 
            end;
          end;
      end;
      finally
      if Assigned(LCurrentSeries) then
      begin
        if (LCurrentSeries.Count <> 0) then
        begin
          SetLength(LYValues,LCurrentSeries.Count);
          try
            for LCount := 0 to LCurrentSeries.Count - 1 do
            begin
              LYValues[LCount] := LCurrentSeries.YValues.Value[LCount];
              if LYValues[LCount]< 0.0 then
                inc(FNegNumbers);
            end;
            FAvarage      := Mean(LYValues);
            FStdDeviation := StdDev(LYValues);
          finally
            SetLength(LYValues,0);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


