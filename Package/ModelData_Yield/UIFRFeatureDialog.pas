{******************************************************************************}
{*  UNIT      : Contains the class TIFRFeatureDialog.                         *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/12/04                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UIFRFeatureDialog;

interface

uses
  Classes,

  VCLTee.Chart,

  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.Graphics,

  VCLTee.Series,
  VCLTee.TeeProcs,
  VCLTee.TeEngine,

  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;
  const
     C_Colors    : array [1..12]of TColor              = (clMaroon,clFuchsia,clYellow,clLime,clPurple,clBlue
                                                          ,clSkyBlue,clOlive,clNavy,clGray,clRed,clGreen);

type
  TIFRLineSeriesArray = array [1..12] of TLineSeries;
  TIFRFeatureDialog = class(TAbstractScrollablePanel)
  protected
    FSiteLabel              : TLabel;
    FSitesCbx               : TFieldComboBox;
    FrdgAnnualMonthlyOption : TFieldRadioGroup;
    FlblAnnualMonthlyOption : TLabel;
    FFeatureNameLabel       : TLabel;
    FFeatureNameEdit        : TFieldEdit;
    FLagInMonthsLabel       : TLabel;
    FLagInMonthsCbx         : TFieldComboBox;
    FNrOfPointsLabel        : TLabel;
    FNrOfPointsEdit         : TFieldEdit;
//    FCalcOptionLabel        : TLabel;
//    FCalcOptionEdit         : TFieldEdit;
    FUnitsOptionLabel       : TLabel;
    FUnitsOptionRadioGroup  : TFieldRadioGroup;
    FReferenceNodesLabel    : TLabel;
    FReferenceNodesCheckLbx : TFieldCheckListBox;
    FIFRPageControl         : TPageControl;
    FTableTabSheet          : TTabSheet;
    FGraphTabSheet          : TTabSheet;
    FInflowVsIFRTabsheet    : TTabSheet;
    FReferenceTabsheet      : TTabSheet;
    FMonthlyIFRLossTabsheet : TTabSheet;
    FExceedenceGrid         : TFieldStringGrid;
    FMonthsGrid             : TFieldStringGrid;
    FValuesGrid             : TFieldStringGrid;

    FInflowDescrLabel       : TLabel;
    FIFRDescrLabel          : TLabel;
    FExceedencePanel        : TPanel;
    FExceedenceLabel        : TLabel;
    FGraphMonthLabel        : TLabel;
    FGraphMonthCbx          : TFieldComboBox;
    FInflowIFRLabel         : TLabel;
    FInflowIFRMonthCbx      : TFieldComboBox;
    FMonthlyIFRGraph        : TAbstractChart;
    FInflowVsIFRChart       : TAbstractChart;
    FReferenceFlowVsDemand  : TAbstractChart;
    FRefMonthsCbx           : TFieldComboBox;
    //FRefDemandCbx           : TFieldComboBox;
    FRefMonthsLabel         : TLabel;
    //FRefDemandLabel         : TLabel;
    FDefinedLineSeriesArray,
    FIFRLineSeriesArray     : TIFRLineSeriesArray;
    FInflowLineSeriesArray  : TIFRLineSeriesArray;
    FInflowLineSeries       : TLineSeries;
    FIFRLineSeries          : TLineSeries;
    FRefLineSeries          : TLineSeries;
    FDemLineSeries          : TLineSeries;

    FUpdateIFRFromReferenceInflows : TCheckBox;
    FIFRFeatureExists       : TFieldChkBox;
    FIFRLoss                : TFieldChkBox;
    FMonthlyIFRLossGrid     : TFieldStringGrid;
    FTotalMARLabel          : TLabel;
    FTotalMAREdit           : TFieldEdit;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure PrepareChart;
    procedure ClearChart;

    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    property FeatureNameEdit        : TFieldEdit         read FFeatureNameEdit;
    property SitesCbx               : TFieldComboBox     read FSitesCbx;
    property LagInMonthsCbx         : TFieldComboBox     read FLagInMonthsCbx;
    property NrOfPointsEdit         : TFieldEdit         read FNrOfPointsEdit;
//    property CalcOptionEdit         : TFieldEdit         read FCalcOptionEdit;
    property UnitsOptionRadioGroup  : TFieldRadioGroup   read FUnitsOptionRadioGroup;
    property ReferenceNodesCheckLbx : TFieldCheckListBox read FReferenceNodesCheckLbx;
    property IFRPageControl         : TPageControl       read FIFRPageControl;
    property TableTabSheet          : TTabSheet          read FTableTabSheet;
    property GraphTabSheet          : TTabSheet          read FGraphTabSheet;
    property InflowVsIFRTabsheet    : TTabSheet          read FInflowVsIFRTabsheet;
    property ReferenceTabsheet      : TTabSheet          read FReferenceTabsheet;
    property MonthlyIFRLossTabsheet : TTabSheet          read FMonthlyIFRLossTabsheet;
    property RefMonthsCbx           : TFieldComboBox     read FRefMonthsCbx;
    //property RefDemandCbx           : TFieldComboBox     read FRefDemandCbx;
    property ReferenceFlowVsDemand  : TAbstractChart     read FReferenceFlowVsDemand;
    property ExceedenceGrid         : TFieldStringGrid   read FExceedenceGrid;
    property MonthsGrid             : TFieldStringGrid   read FMonthsGrid;
    property ValuesGrid             : TFieldStringGrid   read FValuesGrid;
    property InflowDescrLabel       : TLabel             read FInflowDescrLabel;
    property IFRDescrLabel          : TLabel             read FIFRDescrLabel;
    property GraphMonthCbx          : TFieldComboBox     read FGraphMonthCbx;
    property InflowIFRMonthCbx      : TFieldComboBox     read FInflowIFRMonthCbx;
    property MonthlyIFRGraph        : TAbstractChart     read FMonthlyIFRGraph;
    property InflowVsIFRChart       : TAbstractChart     read FInflowVsIFRChart;
    property DefinedLineSeriesArray : TIFRLineSeriesArray read FDefinedLineSeriesArray;
    property InflowLineSeriesArray  : TIFRLineSeriesArray read FInflowLineSeriesArray;
    property IFRLineSeriesArray     : TIFRLineSeriesArray read FIFRLineSeriesArray;
    property InflowLineSeries       : TLineSeries         read FInflowLineSeries;
    property IFRLineSeries          : TLineSeries         read FIFRLineSeries;
    property RefLineSeries          : TLineSeries         read FRefLineSeries;
    property DemLineSeries          : TLineSeries         read FDemLineSeries;
    property chkboxUpdateIFRFromReferenceInflows : TCheckBox   read FUpdateIFRFromReferenceInflows;
    property rdgAnnualMonthlyOption : TFieldRadioGroup    read FrdgAnnualMonthlyOption;
    property ChkboxFIFRFeatureExists : TFieldChkBox       read FIFRFeatureExists;
    property TotalMAREdit            : TFieldEdit         read FTotalMAREdit;
    property IFRLoss                 : TFieldChkBox       read FIFRLoss;
    property MonthlyIFRLossGrid      : TFieldStringGrid   read FMonthlyIFRLossGrid;

  end;

  implementation

uses

  SysUtils,
  UHelpContexts,
  VCL.Forms,
  VCL.Grids,
  UErrorHandlingOperations,
  UControlCreationUtilities;

{******************************************************************************}
{* TIFRFeatureDialog                                                          *}
{******************************************************************************}

procedure TIFRFeatureDialog.CreateMemberObjects;
const OPNAME = 'TIFRFeatureDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
  LLineSeries : TLineSeries;
  LIndex  : integer;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    //                                                                          Left  Top  Width Height
    FSiteLabel              := CreateFieldLabel       (lOwner, lParent,      10 , 10,    130,  20);
    FSitesCbx               := CreateFieldComboBox    (FAppModules, lOwner, lParent, 120 , 10, 190, 20, 0 , True, csDropDownList);
    FFeatureNameLabel       := CreateFieldLabel       (lOwner,          lParent,  10,  35, 130,  20);
    FFeatureNameEdit        := CreateFieldEdit        (FAppModules, lOwner,          lParent, 120,  35, 190,  20, 1, TRUE);
    FLagInMonthsLabel       := CreateFieldLabel       (lOwner,          lParent,  10,  60, 130,  20);
    FLagInMonthsCbx         := CreateFieldComboBox    (FAppModules, lOwner,          lParent, 120,  60,  60,  20, 2, TRUE, csDropDownList);
    FNrOfPointsLabel        := CreateFieldLabel       (lOwner,          lParent,  10,  85, 130,  20);
    FNrOfPointsEdit         := CreateFieldEdit        (FAppModules, lOwner,          lParent, 120,  85,  60,  20, 3, TRUE);
//    FCalcOptionLabel        := CreateFieldLabel       (lOwner, lParent, 10, 110, 130, 21);
//    FCalcOptionEdit         := CreateFieldEdit        (FAppModules, lOwner,          lParent, 120,110,60,20, 4, TRUE);

    FUnitsOptionLabel       := CreateFieldLabel       (lOwner,          lParent,  10,  110, 130,  20);
    FUnitsOptionRadioGroup  := CreateFieldRadioGroup  (FAppModules, lOwner, lParent,  120,  100, 190,  30, 5, True);
    FlblAnnualMonthlyOption := CreateFieldLabel       (lOwner,          lParent,  10, 135 , 130,  20);

    FReferenceNodesLabel    := CreateFieldLabel       (lOwner,          lParent, 325,  10, 105,  20);
    FReferenceNodesCheckLbx := CreateFieldCheckListBox(FAppModules, lOwner, lParent, 420,  10, 250, 120, 7, TRUE);
    FrdgAnnualMonthlyOption := CreateFieldRadioGroup  (FAppModules, lOwner, lParent,  120,  130, 190,  30, 5, True);


    FUpdateIFRFromReferenceInflows        := TCheckBox.Create(lOwner);
    FUpdateIFRFromReferenceInflows.Parent := lParent;
    FUpdateIFRFromReferenceInflows.Left   := 420;
    FUpdateIFRFromReferenceInflows.Top    := FReferenceNodesCheckLbx.Top + FReferenceNodesCheckLbx.Height + 5;
    FUpdateIFRFromReferenceInflows.Width  := FReferenceNodesCheckLbx.Width;

    FIFRFeatureExists        := TFieldChkBox.Create(ControlsOwner, FAppModules);
    FIFRFeatureExists.Parent := lParent;
    FIFRFeatureExists.Left   := 420;
    FIFRFeatureExists.Top    := FUpdateIFRFromReferenceInflows.Top + FUpdateIFRFromReferenceInflows.Height + 5;
    FIFRFeatureExists.Width  := FUpdateIFRFromReferenceInflows.Width;


    FIFRLoss        := TFieldChkBox.Create(ControlsOwner, FAppModules);
    FIFRLoss.Parent := lParent;
    FIFRLoss.Left   := 420;
    FIFRLoss.Top    := FIFRFeatureExists.Top + FIFRFeatureExists.Height + 5;
    FIFRLoss.Width  := FIFRFeatureExists.Width;


    FTotalMARLabel          := CreateFieldLabel       (lOwner,          lParent,  180,  200, 240,  20);
    FTotalMAREdit           := CreateFieldEdit        (FAppModules, lOwner,          lParent, 420,  200, 245,  20, 1, TRUE);
    FTotalMAREdit.Enabled   := False;
    
    FIFRPageControl         := TPageControl.Create(lOwner);
    FFeatureNameEdit.IsEnabled := False;
    FNrOfPointsLabel.WordWrap := TRUE;
    with FIFRPageControl do
    begin
      Parent   := lParent;
      Left     := 10;
//      Top      := 195;
      Top      := 220;
      Width    := 810;
      Height   := 310;
      TabOrder := 5;
    end;
    FTableTabSheet                := TTabSheet.Create(lOwner);
    FTableTabSheet.Parent         := lParent;
    FGraphTabSheet                := TTabSheet.Create(lOwner);
    FGraphTabSheet.Parent         := lParent;
    FInflowVsIFRTabsheet          := TTabSheet.Create(lOwner);
    FInflowVsIFRTabsheet.Parent   := lParent;
    FReferenceTabsheet            := TTabSheet.Create(lOwner);
    FMonthlyIFRLossTabsheet       := TTabSheet.Create(lOwner);
    FMonthlyIFRLossTabsheet.Parent:= lParent;

    FExceedenceGrid               := CreateFieldStringGrid (FAppModules, lOwner, FTableTabSheet,   5,  81,  76, 170, 0, TRUE);
    FMonthlyIFRLossGrid     := CreateFieldStringGrid (FAppModules, lOwner, FMonthlyIFRLossTabsheet,   5, 10,  450, 45, 0, TRUE);

     with FMonthlyIFRLossGrid do
    begin
      ScrollBars       := ssNone;
      ColCount         := 12;
      RowCount         := 2;
      FixedCols        := 0;
      FixedRows        := 1;
      DefaultRowHeight := 17;
      DefaultColWidth  := 50;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];
      Width            := (DefaultColWidth*ColCount)+5;
    end;

    with FExceedenceGrid do
    begin
      ScrollBars       := ssNone;
      ColCount         := 2;
      RowCount         := 10;
      FixedCols        := 1;
      FixedRows        := 0;
      DefaultRowHeight := 17;
      DefaultColWidth  := 50;
      ColWidths[0]     := 20;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];
    end;
    FMonthsGrid        := CreateFieldStringGrid (FAppModules, lOwner, FTableTabSheet,  82,  38, 696,  25, 1, FALSE);
    with FMonthsGrid do
    begin
      ScrollBars       := ssNone;
      ColCount         := 12;
      RowCount         := 2;
      FixedCols        := 0;
      FixedRows        := 1;
      DefaultRowHeight := 23;
      DefaultColWidth  := 81;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];
    end;
    FValuesGrid        := CreateFieldStringGrid (FAppModules, lOwner, FTableTabSheet,  82,  63, 712, 190, 2, TRUE);
    with FValuesGrid do
    begin
      ScrollBars       := ssBoth;
      ColCount         := 24;
      RowCount         := 11;
      FixedCols        := 0;
      FixedRows        := 1;
      DefaultRowHeight := 17;
      DefaultColWidth  := 40;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];
    end;
    FInflowDescrLabel          := CreateFieldLabel      (lOwner, FTableTabSheet,  82,   5, 400,  13);
    FIFRDescrLabel             := CreateFieldLabel      (lOwner, FTableTabSheet,  82,  20, 400,  13);
    FExceedencePanel           := CreatePanel           (lOwner, FTableTabSheet,   5,  38,  76,  44, 0);
    with FExceedencePanel do
    begin
      BevelInner := bvRaised;
      BevelOuter := bvNone;
      BorderStyle := bsSingle;
      BringToFront;
    end;
    FExceedenceLabel           := CreateFieldLabel    (lOwner, FExceedencePanel,   2,   2,  72,  40);
    with FExceedenceLabel do
    begin
      Align     := alClient;
      Alignment := taCenter;
      WordWrap  := TRUE;
    end;
    FGraphMonthLabel           := CreateFieldLabel      (lOwner, FGraphTabSheet,   5,   5,  40,  21);
    FGraphMonthCbx             := CreateFieldComboBox   (FAppModules, lOwner, FGraphTabSheet,  50,   5,  85,  21, 1, TRUE, csDropDownList);

    FInflowIFRLabel            := CreateFieldLabel      (lOwner, FInflowVsIFRTabsheet,   5,   5,  40,  21);
    FInflowIFRMonthCbx         := CreateFieldComboBox   (FAppModules, lOwner, FInflowVsIFRTabsheet,  50,   5,  85,  21, 1, TRUE, csDropDownList);

    FMonthlyIFRGraph := TAbstractChart.Create(ControlsOwner,FAppModules);
    FMonthlyIFRGraph.Parent := FGraphTabSheet;

    FInflowVsIFRChart := TAbstractChart.Create(ControlsOwner,FAppModules);
    FInflowVsIFRChart.Parent := FInflowVsIFRTabsheet;
    FReferenceFlowVsDemand := TAbstractChart.Create(ControlsOwner,FAppModules);
    FReferenceFlowVsDemand.Parent := FReferenceTabsheet;

    FRefMonthsLabel         := CreateFieldLabel(lOwner, FReferenceTabsheet,   5,   5,  40,  21);
    //FRefDemandLabel         := CreateFieldLabel(lOwner, FReferenceTabsheet,   290,   5,  50,  21);
    FRefMonthsCbx           := CreateFieldComboBox(FAppModules, lOwner, FReferenceTabsheet,  50,   5,  85,  21, 1, TRUE, csDropDownList);
    //FRefDemandCbx           := CreateFieldComboBox(FAppModules, lOwner, FReferenceTabsheet,  370,   5,  185,  21, 1, TRUE, csDropDownList);


    for LIndex := Low(FInflowLineSeriesArray) to High(FInflowLineSeriesArray) do
    begin
      LLineSeries := TLineSeries.Create(FMonthlyIFRGraph);
      LLineSeries.ParentChart        := FMonthlyIFRGraph;
      FInflowLineSeriesArray[LIndex] := LLineSeries;
    end;

    {for LIndex := Low(FDefinedLineSeriesArray) to High(FDefinedLineSeriesArray) do
    begin
      LLineSeries := TLineSeries.Create(FMonthlyIFRGraph);
      LLineSeries.ParentChart        := FMonthlyIFRGraph;
      FDefinedLineSeriesArray[LIndex] := LLineSeries;
    end;
     }
    PrepareChart;
    for LIndex := Low(FIFRLineSeriesArray) to High(FIFRLineSeriesArray) do
    begin
      LLineSeries := TLineSeries.Create(FMonthlyIFRGraph);
      LLineSeries.ParentChart := FMonthlyIFRGraph;
      FIFRLineSeriesArray[LIndex]     := LLineSeries;
    end;

    FInflowLineSeries             := TLineSeries.Create(FInflowVsIFRChart);
    FInflowLineSeries.ParentChart := FInflowVsIFRChart;
    FIFRLineSeries                := TLineSeries.Create(FInflowVsIFRChart);
    FIFRLineSeries.ParentChart    := FInflowVsIFRChart;
    FRefLineSeries                := TLineSeries.Create(FInflowVsIFRChart);
    FRefLineSeries.ParentChart    := FReferenceFlowVsDemand;

    FDemLineSeries                := TLineSeries.Create(FInflowVsIFRChart);
    FDemLineSeries.ParentChart    := FReferenceFlowVsDemand;


  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureDialog.Resize;
const OPNAME = 'TIFRFeatureDialog.Resize';
begin
  inherited Resize;
  try
    FTableTabSheet.PageControl        := FIFRPageControl;
    FGraphTabSheet.PageControl        := FIFRPageControl;
    FInflowVsIFRTabsheet.PageControl  := FIFRPageControl;
    FReferenceTabsheet.PageControl    := FIFRPageControl;
    FMonthlyIFRLossTabsheet.PageControl    := FIFRPageControl;

    FIFRPageControl.Height            := Self.Height - FIFRPageControl.Top - 30;
    FExceedenceGrid.Height  := FIFRPageControl.Height - FExceedencePanel.Height - (FInflowDescrLabel.Height*2) -20;
    FValuesGrid.Height                 := FExceedenceGrid.Height;
    FGraphTabSheet.Height              := FIFRPageControl.Height - 30;
    FInflowVsIFRTabsheet.Height        := FIFRPageControl.Height - 30;
    FReferenceTabsheet.Height          := FIFRPageControl.Height - 30;
    FMonthlyIFRGraph.Height            := FIFRPageControl.Height - 60;
    FInflowVsIFRChart.Height           := FIFRPageControl.Height - 60;
    FMonthlyIFRLossTabsheet.Height     := FIFRPageControl.Height - 60;
    FIFRPageControl.ActivePage         := FTableTabSheet;
//    FrdgAnnualMonthlyOption.Top       := FUpdateIFRFromReferenceInflows.Top + FUpdateIFRFromReferenceInflows.Height;
//    FrdgAnnualMonthlyOption.Left      := FUpdateIFRFromReferenceInflows.Left;
//    FrdgAnnualMonthlyOption.Height    := 43;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRFeatureDialog.Initialise: boolean;
const OPNAME = 'TIFRFeatureDialog.Initialise';
var
  LIndex : integer;
begin
  Result := inherited Initialise;
  try
    FUnitsOptionRadioGroup.Columns := 2;
    FUnitsOptionRadioGroup.Items.Clear;
    FUnitsOptionRadioGroup.Items.Add('');
    FUnitsOptionRadioGroup.Items.Add('');
    FrdgAnnualMonthlyOption.Columns := 2;
    FrdgAnnualMonthlyOption.Items.Clear;
    FrdgAnnualMonthlyOption.Items.Add('');
    FrdgAnnualMonthlyOption.Items.Add('');
    FrdgAnnualMonthlyOption.ItemIndex := 0;

    VCL.Controls.TControl(FMonthlyIFRGraph).Top       := FGraphMonthCbx.Top + FGraphMonthCbx.Height + C_ControlOffset;
    VCL.Controls.TControl(FMonthlyIFRGraph).Left      := FGraphMonthLabel.Left;
    FMonthlyIFRGraph.Height    := FGraphTabSheet.Height - VCL.Controls.TControl(FMonthlyIFRGraph).Top ;
    FMonthlyIFRGraph.Width     := FGraphTabSheet.Width ;

    VCL.Controls.TControl(FInflowVsIFRChart).Top      := FInflowIFRMonthCbx.Top + FInflowIFRMonthCbx.Height + C_ControlOffset;
    VCL.Controls.TControl(FInflowVsIFRChart).Left     := FInflowIFRLabel.Left;
    FInflowVsIFRChart.Height   := FInflowVsIFRTabsheet.Height - VCL.Controls.TControl(FInflowVsIFRChart).Top;
    FInflowVsIFRChart.Width    := FInflowVsIFRTabsheet.Width;

    VCL.Controls.TControl(FReferenceFlowVsDemand).Top       := FRefMonthsCbx.Top + FRefMonthsCbx.Height + C_ControlOffset;
    VCL.Controls.TControl(FReferenceFlowVsDemand).Left      := FRefMonthsLabel.Left;

    FReferenceFlowVsDemand.Height := FReferenceTabsheet.Height - VCL.Controls.TControl(FReferenceFlowVsDemand).Top;
    FReferenceFlowVsDemand.Width  := FReferenceTabsheet.Width;

    FMonthlyIFRGraph.BevelOuter                    := bvNone;
    FMonthlyIFRGraph.Legend.Visible                := False;
    FMonthlyIFRGraph.AxisVisible                   := True;
    FMonthlyIFRGraph.AllowZoom                     := True;
    FMonthlyIFRGraph.AllowPanning                  := pmBoth;
    FMonthlyIFRGraph.Gradient.Visible              := False;
    FMonthlyIFRGraph.View3D                        := False;
    FMonthlyIFRGraph.Title.Visible                 := True;
    FMonthlyIFRGraph.Title.Font.Style              := [fsBold];
    FMonthlyIFRGraph.Title.Font.Color              := clBlack;
    FMonthlyIFRGraph.LeftAxis.Title.Angle          := 90;

    FInflowVsIFRChart.BevelOuter                    := bvNone;
    FInflowVsIFRChart.Legend.Visible                := False;
    FInflowVsIFRChart.AxisVisible                   := True;
    FInflowVsIFRChart.AllowZoom                     := True;
    FInflowVsIFRChart.AllowPanning                  := pmBoth;
    FInflowVsIFRChart.Gradient.Visible              := False;
    FInflowVsIFRChart.View3D                        := False;
    FInflowVsIFRChart.Title.Visible                 := True;
    FInflowVsIFRChart.Title.Font.Style              := [fsBold];
    FInflowVsIFRChart.Title.Font.Color              := clBlack;
    FInflowVsIFRChart.LeftAxis.Title.Angle          := 90;

    //FMonthlyIFRGraph.BottomAxis.Automatic          := True;
    //FMonthlyIFRGraph.BottomAxis.TickLength         := 2;
    //FMonthlyIFRGraph.ScaleLastPage                 := False;
    //FMonthlyIFRGraph.BottomAxis.MinorTickCount     := 4;
    //FMonthlyIFRGraph.LeftAxis.MinimumOffset        := 10;
    //FMonthlyIFRGraph.BottomAxis.MinimumOffset      := 10;
    //FMonthlyIFRGraph.BottomAxis.Maximum            := 100;

    for LIndex := Low(FInflowLineSeriesArray) to High(FInflowLineSeriesArray) do
    begin
      FInflowLineSeriesArray[LIndex].Marks.Visible := False;
      FInflowLineSeriesArray[LIndex].LinePen.Width := 2;
      FInflowLineSeriesArray[LIndex].Clear;
      FInflowLineSeriesArray[LIndex].Color := clBlue;
    end;

    {for LIndex := Low(FDefinedLineSeriesArray) to High(FDefinedLineSeriesArray) do
    begin
      FDefinedLineSeriesArray[LIndex].Marks.Visible := False;
      FDefinedLineSeriesArray[LIndex].LinePen.Width := 2;
      FDefinedLineSeriesArray[LIndex].Clear;
      FDefinedLineSeriesArray[LIndex].Color := clBlue;
    end;
     }

    for LIndex := Low(FIFRLineSeriesArray) to High(FIFRLineSeriesArray) do
    begin
      FIFRLineSeriesArray[LIndex].Marks.Visible := False;
      FIFRLineSeriesArray[LIndex].LinePen.Width := 2;
      FIFRLineSeriesArray[LIndex].Clear;
      FIFRLineSeriesArray[LIndex].Color := clRed;
    end;
    FInflowLineSeries.Marks.Visible     := False;
    FInflowLineSeries.LinePen.Width     := 2;
    FInflowLineSeries.Clear;
    FInflowLineSeries.Color             := clRed;

    FIFRLineSeries.Marks.Visible        := False;
    FIFRLineSeries.LinePen.Width        := 2;
    FIFRLineSeries.Clear;
    FIFRLineSeries.Color                := clRed;

    FRefLineSeries.Marks.Visible        := False;
    FRefLineSeries.LinePen.Width        := 2;
    FRefLineSeries.Clear;
    FRefLineSeries.Color                := clRed;

    FDemLineSeries.Marks.Visible        := False;
    FDemLineSeries.LinePen.Width        := 2;
    FDemLineSeries.Clear;
    FDemLineSeries.Color                := clBlue;


    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRFeatureDialog.LanguageHasChanged: boolean;
const OPNAME = 'TIFRFeatureDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FSiteLabel.Caption           := FAppModules.Language.GetString('TField.IFRSiteID')+ ' :';
    FFeatureNameLabel.Caption    := FAppModules.Language.GetString('NetworkFeatures.IFRFeatureName') + ' :';
    FLagInMonthsLabel.Caption    := FAppModules.Language.GetString('TField.LagInMonthsCount') + ' :';
//    FCalcOptionLabel.Caption     := FAppModules.Language.GetString('TField.IFRCalculationOption') + ' :';
    FNrOfPointsLabel.Caption     := FAppModules.Language.GetString('TField.IFRPointsCount') + ' :';
    FReferenceNodesLabel.Caption := FAppModules.Language.GetString('NetworkFeatures.ReferenceNodes') + ' :';
    FTableTabSheet.Caption       := FAppModules.Language.GetString('TabCaption.Table');

    FGraphTabSheet.Caption       := FAppModules.Language.GetString('TabCaption.IFRRuleCurve');
    FInflowVsIFRTabsheet.Caption := FAppModules.Language.GetString('TabCaption.IFRVSReferenceFlow');

    FReferenceTabsheet.Caption   := FAppModules.Language.GetString('TabCaption.ExceedenceOfIFRReferenceFlows');
    FMonthlyIFRLossTabsheet.Caption := 'Monthly IFR Loss';

    FUnitsOptionLabel.Caption    := FAppModules.Language.GetString('TField.IFRUnits') + ' :';
    FlblAnnualMonthlyOption.Caption := FAppModules.Language.GetString('TField.IFRType') + ' :';
    FExceedenceLabel.Caption     := FAppModules.Language.GetString('NetworkFeatures.Exceedence%');
    FGraphMonthLabel.Caption     := FAppModules.Language.GetString('NetworkFeatures.Month') + ' :';
    FInflowIFRLabel.Caption      := FAppModules.Language.GetString('NetworkFeatures.Month') + ' :';
    FReferenceNodesCheckLbx.Hint := FAppModules.Language.GetString('TIFRFeatureDialog.ReferenceNodes');
    FExceedenceGrid.Hint         := FAppModules.Language.GetString('TIFRFeatureDialog.ExceedencePercentage');
    FUnitsOptionRadioGroup.Items[0]  := FAppModules.Language.GetString('TField.IFRUnitOptionM3PerSec');
    FUnitsOptionRadioGroup.Items[1]  := FAppModules.Language.GetString('TField.IFRUnitOptionM3PerMonth');
    FrdgAnnualMonthlyOption.Items[0] := FAppModules.Language.GetString('TIFRFeatureDialog.MonthlyRefenceFlow');
    FrdgAnnualMonthlyOption.Items[1] := FAppModules.Language.GetString('TIFRFeatureDialog.AnnualRefenceFlow');

    FUpdateIFRFromReferenceInflows.Caption  := FAppModules.Language.GetString('TIFRFeatureDialog.UpdateIFRFromReferenceInflows');
    FRefMonthsLabel.Caption := FAppModules.Language.GetString('TOutputMonthlyDeficitValidator.Months');
    //FRefDemandLabel.Caption := 'View Data: ';
    FIFRFeatureExists.Caption  := FAppModules.Language.GetString('TIFRFeatureDialog.IFRFeatureExists');
    FIFRLoss.Caption           := 'IFR Loss';
    FTotalMARLabel.Caption     := FAppModules.Language.GetString('TIFRFeatureDialog.TotalMAR');

    FInflowDescrLabel.Caption    := FAppModules.Language.GetString('TIFRFeatureDialog.InflowVariablesDescr');
    FIFRDescrLabel.Caption       := FAppModules.Language.GetString('TIFRFeatureDialog.IFRVariables');
    if(FUnitsOptionRadioGroup.ItemIndex = 0) then
    begin
      FInflowDescrLabel.Caption    := FInflowDescrLabel.Caption +'(' + FAppModules.Language.GetString('TField.IFRUnitOptionM3PerSec')+')';
      FIFRDescrLabel.Caption       := FIFRDescrLabel.Caption + '(' + FAppModules.Language.GetString('TField.IFRUnitOptionM3PerSec')+')';
    end;
    if(FUnitsOptionRadioGroup.ItemIndex = 1) then
    begin
      FInflowDescrLabel.Caption    := FInflowDescrLabel.Caption + '('+ FAppModules.Language.GetString('TField.IFRUnitOptionMM3PerMonth') +')';
      FIFRDescrLabel.Caption       := FIFRDescrLabel.Caption + '('+ FAppModules.Language.GetString('TField.IFRUnitOptionMM3PerMonth') +')';
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureDialog.RestoreColourState;
const OPNAME = 'TIFRFeatureDialog.RestoreColourState';
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

procedure TIFRFeatureDialog.AssignHelpContext;
const OPNAME = 'TIFRFeatureDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                    HC_CreatingIFRSites);
    SetControlHelpContext(FSitesCbx,               HC_CreatingIFRSites);
    SetControlHelpContext(FFeatureNameEdit,        HC_CreatingIFRSites);
    SetControlHelpContext(FLagInMonthsCbx,         HC_CreatingIFRSites);
    SetControlHelpContext(FNrOfPointsEdit,         HC_CreatingIFRSites);
//    SetControlHelpContext(FCalcOptionEdit,         HC_CreatingIFRSites);
    SetControlHelpContext(FReferenceNodesCheckLbx, HC_CreatingIFRSites);
    SetControlHelpContext(FExceedenceGrid,         HC_CreatingIFRSites);
    SetControlHelpContext(FMonthsGrid,             HC_CreatingIFRSites);
    SetControlHelpContext(FValuesGrid,             HC_CreatingIFRSites);
    SetControlHelpContext(FGraphMonthCbx,          HC_CreatingIFRSites);
    SetControlHelpContext(FInflowIFRMonthCbx,      HC_CreatingIFRSites);
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TIFRFeatureDialog.ClearChart;
const OPNAME = 'TIFRFeatureDialog.ClearChart';
var
  LIndex : integer;
begin
  try
    for LIndex := 1 to 12 do
    begin
      if (Length(FDefinedLineSeriesArray) > 0) then
      begin
        FDefinedLineSeriesArray[LIndex].Active         := False;
        FDefinedLineSeriesArray[LIndex].Clear;
        FDefinedLineSeriesArray[LIndex].ParentChart    := nil;
        FDefinedLineSeriesArray[LIndex]                := nil;

      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRFeatureDialog.PrepareChart;
const OPNAME = 'TIFRFeatureDialog.PrepareChart';
var
  LIndex : integer;
begin
  try
    FMonthlyIFRGraph.SeriesList.Clear;
    for LIndex := 1 to 12 do
    begin
      FDefinedLineSeriesArray[LIndex]               := TLineSeries.Create(FMonthlyIFRGraph);
      FDefinedLineSeriesArray[LIndex].ParentChart   := FMonthlyIFRGraph;
      FDefinedLineSeriesArray[LIndex].Tag           := LIndex + 1;
      FDefinedLineSeriesArray[LIndex].Pen.Width     := 1;
    end;
    
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.


