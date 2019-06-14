{******************************************************************************}
{*  UNIT      : Contains the class TDiversionFeatureDialog.                   *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/11/27                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UDiversionFeatureDialog;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCLTee.Chart,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.Graphics,
  VCLTee.TeeProcs,
  VCLTee.TeEngine,
  VCLTee.Series,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type

  TDiversionFeatureDialog = class(TAbstractScrollablePanel)
  private
    FFeatureNameLabel     : TLabel;
    FDivFeatureNameEdit   : TFieldEdit;
    FDivTypeRadioGroup    : TFieldRadioGroup;
    FType1GroupBox        : TGroupBox;
    FType1HeadPanel1      : TPanel;
    FType1HeadLabel1      : TLabel;
    FType1HeadPanel2      : TPanel;
    FType1HeadLabel2      : TLabel;
    FType1Grid            : TFieldStringGrid;

    FType2GroupBox        : TGroupBox;
    {FType2HeadPanel1      : TPanel;
    FType2HeadPanel2      : TPanel;
    FType2HeadLabel1      : TLabel;
    FType2HeadLabel2      : TLabel;
    }
    FType2Grid            : TFieldStringGrid;

    //FType4HeadLabel1      : TLabel;
    //FType4HeadLabel2      : TLabel;

    FType3GroupBox        : TGroupBox;
    FType3HeadPanel1      : TPanel;
    FType3HeadLabel1      : TLabel;
    FType3HeadPanel2      : TPanel;
    FType3HeadLabel2      : TLabel;
    FType3HeadLabel3      : TLabel;
    FType3FlowsGrid       : TFieldStringGrid;
    FType3LevelsGrid      : TFieldStringGrid;
    FType3ProportionsGrid : TFieldStringGrid;
    FType3Panel           : TGroupBox; // TPanel;
    FReservoirLabel       : TLabel;
    FReservoirCbx         : TFieldComboBox;
    FNrOfFlowsLabel       : TLabel;
    FNrOfFlowsEdit        : TFieldEdit;
    FNrOfLevelsLabel      : TLabel;
    FNrOfLevelsEdit       : TFieldEdit;
    FMinimumLabel         : TLabel;
    FMinimumEdit          : TFieldEdit;
    FMaximumLabel         : TLabel;
    FMaximumEdit          : TFieldEdit;
    FRangeLabel           : TLabel;
    FBtnInsertRow         : TFieldBitBtn;
    FBtnDeleteRow         : TFieldBitBtn;

    FcbxStations          : TFieldComboBox;
    FlblStations          : TLabel;
    FbtnGetStations       : TBitBtn;

    FFlowDiversionRelationshipChart : TAbstractChart;
    FFlowDiversionRelationshipLineSeries : TLineSeries;

  protected
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property DivFeatureNameEdit   : TFieldEdit        read FDivFeatureNameEdit;
    property DivTypeRadioGroup    : TFieldRadioGroup  read FDivTypeRadioGroup;
    property Type1GroupBox        : TGroupBox         read FType1GroupBox;
    property Type1Grid            : TFieldStringGrid  read FType1Grid;
    property Type2GroupBox        : TGroupBox         read FType2GroupBox;
    property Type2Grid            : TFieldStringGrid  read FType2Grid;
    {
    property Type2HeadLabel1      : TLabel            read FType2HeadLabel1;
    property Type2HeadLabel2      : TLabel            read FType2HeadLabel2;
    property Type4HeadLabel1      : TLabel            read FType4HeadLabel1;
    property Type4HeadLabel2      : TLabel            read FType4HeadLabel2;
    }
    property Type3GroupBox        : TGroupBox         read FType3GroupBox;
    property Type3FlowsGrid       : TFieldStringGrid  read FType3FlowsGrid;
    property Type3LevelsGrid      : TFieldStringGrid  read FType3LevelsGrid;
    property Type3ProportionsGrid : TFieldStringGrid  read FType3ProportionsGrid;
    property Type3Panel           : {TPanel}TGroupBox            read FType3Panel;
    property ReservoirCbx         : TFieldComboBox    read FReservoirCbx;
    property NrOfFlowsEdit        : TFieldEdit        read FNrOfFlowsEdit;
    property NrOfLevelsEdit       : TFieldEdit        read FNrOfLevelsEdit;
    property MinimumEdit          : TFieldEdit        read FMinimumEdit;
    property MaximumEdit          : TFieldEdit        read FMaximumEdit;
    //property DiversionRelationshipBtn : TFieldButton  read FDivRelationshipBtn;
    property BtnInsertRow             : TFieldBitBtn  read FBtnInsertRow;
    property BtnDeleteRow             : TFieldBitBtn  read FBtnDeleteRow;
    property cbxStations              : TFieldComboBox read FcbxStations;
    property btnGetStations           : TBitBtn        read FbtnGetStations;
    property FlowDiversionRelationshipChart : TAbstractChart read FFlowDiversionRelationshipChart;
    property FlowDiversionRelationshipLineSeries : TLineSeries read FFlowDiversionRelationshipLineSeries;

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
{* TDiversionFeatureDialog                                                    *}
{******************************************************************************}

procedure TDiversionFeatureDialog.CreateMemberObjects;
const OPNAME = 'TDiversionFeatureDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    //                                                                        Left  Top  Width Height
    FFeatureNameLabel    := CreateFieldLabel          (lOwner,          lParent,  10,  10, 130,  21);
    FDivFeatureNameEdit  := CreateFieldEdit       (FAppModules, lOwner, lParent, 140,  10, 160,  21, 0, TRUE);
    FDivTypeRadioGroup   := CreateFieldradioGroup (FAppModules, lOwner, lParent,  10,  33, 360,  110, 3, FALSE);
    FType1GroupBox       := CreateFieldGroupBox   (lOwner,          lParent,  10, 145, 250, 370, 4, FALSE);
    FType2GroupBox       := CreateFieldGroupBox   (lOwner,          lParent,  10, 145, 800, 370, 6, FALSE);
    FType3GroupBox       := CreateFieldGroupBox   (lOwner,          lParent,  10, 145, 550, 370, 7, FALSE);
    FType3Panel          := CreateFieldGroupBox   (lOwner,          lParent, 395,  33, 295,  100, 8,False);
    {with FType3Panel do
    begin
      BevelInner := bvNone;
      BevelOuter := bvNone;
      BorderStyle := bsNone;
    end;
    }
    FReservoirLabel    := CreateFieldLabel      (lOwner,      FType3Panel,   15,   15,  98,  21);
    FNrOfFlowsLabel    := CreateFieldLabel      (lOwner,      FType3Panel,   15,  40, 160,  21);
    FNrOfLevelsLabel   := CreateFieldLabel      (lOwner,      FType3Panel,   15,  65, 160,  21);
    FReservoirCbx      := CreateFieldComboBox   (FAppModules, lOwner,      FType3Panel, 130,   15, 160,  21, 0, TRUE, csDropDownList);
    FNrOfFlowsEdit     := CreateFieldEdit       (FAppModules, lOwner,      FType3Panel, 180,  40,  40,  21, 1, TRUE);
    FNrOfLevelsEdit    := CreateFieldEdit       (FAppModules, lOwner,      FType3Panel, 180,  65,  40,  21, 2, TRUE);
    // Diversion type 1
    FType1HeadPanel1   := CreatePanel           (lOwner,   FType1GroupBox,  76,  18,  76,  46, 0);
    FType1HeadPanel2   := CreatePanel           (lOwner,   FType1GroupBox, 157,  18,  76,  46, 1);
    FType1Grid         := CreateFieldStringGrid (FAppModules, lOwner,   FType1GroupBox,  10,  15, 226, 318, 2, TRUE);
    with FType1HeadPanel1 do
    begin
      BevelInner := bvNone;
      BevelOuter := bvNone;
      BorderStyle := bsNone;
      BringToFront;
    end;
    with FType1HeadPanel2 do
    begin
      BevelInner := bvNone;
      BevelOuter := bvNone;
      BorderStyle := bsNone;
      BringToFront;
    end;
    with FType1Grid do
    begin
      ScrollBars       := ssNone;
      ColCount         := 3;
      RowCount         := 13;
      FixedCols        := 1;
      FixedRows        := 1;
      DefaultRowHeight := 21;
      DefaultColWidth  := 80;
      ColWidths[0]     := 60;
      RowHeights[0]    := 50;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];
    end;
    FType1HeadLabel1   := CreateFieldLabel      (lOwner, FType1HeadPanel1,   0,   0,  76,  46);
    with FType1HeadLabel1 do
    begin
      Align     := alClient;
      Alignment := taCenter;
      WordWrap  := TRUE;
    end;
    FType1HeadLabel2   := CreateFieldLabel      (lOwner, FType1HeadPanel2,   0,   0,  76,  46);
    with FType1HeadLabel2 do
    begin
      Align     := alClient;
      Alignment := taCenter;
      WordWrap  := TRUE;
    end;
    // Diversion type 2 & Diversion type 4 = Loss type 1
    FBtnInsertRow := TFieldBitBtn.Create(lOwner, FAppModules);
    with FBtnInsertRow do
    begin
      Parent := Type2GroupBox;
      Left   := 10;
      Top    := 45;
      Width  := 70;
      Height := 45;
      Layout := blGlyphTop;
    end;
    FBtnDeleteRow := TFieldBitBtn.Create(lOwner, FAppModules);
    with FBtnDeleteRow do
    begin
      Parent := Type2GroupBox;
      Left   := 10;
      Top    := 100;
      Width  := 70;
      Height := 45;
      Layout := blGlyphTop;
    end;
    FcbxStations := TFieldComboBox.Create(lOwner, FAppModules);
    FcbxStations.Parent := Type2GroupBox;
    FcbxStations.Left   := 130;
    FcbxStations.Top    := 10;
    FcbxStations.Width  := 200;
    FcbxStations.Height := 45;

    FlblStations := TLabel.Create(LOwner);
    FlblStations.Parent := Type2GroupBox;
    FlblStations.Left   := 10;
    FlblStations.Top    := 13;
    FlblStations.Width  := 100;

    FbtnGetStations       := TBitBtn.Create(lOwner);
    FbtnGetStations.Parent := Type2GroupBox;
    FbtnGetStations.Left   := FcbxStations.Left + FcbxStations.Width + FlblStations.Left + 10;
    FbtnGetStations.Top    := 10;
    FbtnGetStations.Width  := 30;
    FbtnGetStations.Height := 20;

    {FType2HeadPanel1   := CreatePanel           (lOwner,   FType2GroupBox, 116,  18,  76,  46, 0);
    FType2HeadPanel2   := CreatePanel           (lOwner,   FType2GroupBox, 197,  18,  76,  46, 1);
    }
    FType2Grid         := CreateFieldStringGrid (FAppModules, lOwner,   FType2GroupBox,  90,  45, 186, 318, 3, TRUE);
    {with FType2HeadPanel1 do
    begin
      BevelInner := bvNone;
      BevelOuter := bvNone;
      BorderStyle := bsNone;
      BringToFront;
    end;
    with FType2HeadPanel2 do
    begin
      BevelInner := bvNone;
      BevelOuter := bvNone;
      BorderStyle := bsNone;
      BringToFront;
    end;
    }
    with FType2Grid do
    begin
      ScrollBars       := ssNone;
      ColCount         := 3;
      RowCount         := 13;
      FixedCols        := 1;
      FixedRows        := 1;
      DefaultRowHeight := 21;
      DefaultColWidth  := 80;
      ColWidths[0]     := 20;
      RowHeights[0]    := 50;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];
      WrapHeaderText   := True;
    end;
    {FType2HeadLabel1   := CreateFieldLabel      (lOwner, FType2HeadPanel1,   0,   0,  76,  46);
    with FType2HeadLabel1 do
    begin
      Align     := alClient;
      Alignment := taCenter;
      WordWrap  := TRUE;
    end;
    {FType2HeadLabel2   := CreateFieldLabel      (lOwner, FType2HeadPanel2,   0,   0,  76,  46);
    with FType2HeadLabel2 do
    begin
      Align     := alClient;
      Alignment := taCenter;
      WordWrap  := TRUE;
    end;

    FType4HeadLabel1   := CreateFieldLabel      (lOwner, FType2HeadPanel1,   0,   0,  76,  46);
    with FType4HeadLabel1 do
    begin
      Align     := alClient;
      Alignment := taCenter;
      WordWrap  := TRUE;
    end;
    FType4HeadLabel2   := CreateFieldLabel      (lOwner, FType2HeadPanel2,   0,   0,  76,  46);
    with FType4HeadLabel2 do
    begin
      Align     := alClient;
      Alignment := taCenter;
      WordWrap  := TRUE;
    end;
    }
    // Diversion type 3
    FType3HeadPanel1   := CreatePanel           (lOwner,   FType3GroupBox,  10,  15,  82, 268, 0);
    FType3HeadPanel1.Font.Color := clGreen;
    FType3HeadPanel2   := CreatePanel           (lOwner,   FType3GroupBox,  92,  15, 450,  80, 1);
    FType3HeadPanel2.Font.Color := clBlue;
    FType3ProportionsGrid := CreateFieldStringGrid(FAppModules, lOwner,  FType3GroupBox,  96,  95, 446, 188, 2, TRUE);
    with FType3ProportionsGrid do
    begin
      ScrollBars       := ssBoth;
      ColCount         := 6;
      RowCount         := 6;
      FixedCols        := 0;
      FixedRows        := 0;
      DefaultRowHeight := 20;
      DefaultColWidth  := 70;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];
    end;
    FType3HeadLabel3   := CreateFieldLabel      (lOwner,   FType3GroupBox,   2, 285, 546,  30);
    with FType3HeadLabel3 do
    begin
      Align      := alBottom;
      Alignment  := taCenter;
      WordWrap   := TRUE;
    end;
    FType3HeadLabel1   := CreateFieldLabel      (lOwner, FType3HeadPanel1,   1,   1,  76,  55);
    with FType3HeadLabel1 do
    begin
      Align      := alTop;
      Alignment  := taCenter;
      WordWrap   := TRUE;
    end;
    FType3FlowsGrid    := CreateFieldStringGrid(FAppModules, lOwner,  FType3HeadPanel1,   2,  78,  74, 129, 0, TRUE);
    with FType3FlowsGrid do
    begin
      ScrollBars       := ssNone;
      ColCount         := 1;
      RowCount         := 6;
      FixedCols        := 0;
      FixedRows        := 0;
      DefaultRowHeight := 20;
      DefaultColWidth  := 70;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];
    end;
    FType3HeadLabel2   := CreateFieldLabel      (lOwner, FType3HeadPanel2,   1,   1, 444,  30);
    with FType3HeadLabel2 do
    begin
      Align      := alTop;
      Alignment  := taCenter;
      WordWrap   := TRUE;
    end;
    FType3LevelsGrid   := CreateFieldStringGrid(FAppModules, lOwner,  FType3HeadPanel2,   2,  50, 358,  24, 0, TRUE);
    with FType3LevelsGrid do
    begin
      ScrollBars       := ssNone;
      ColCount         := 6;
      RowCount         := 1;
      FixedCols        := 0;
      FixedRows        := 0;
      DefaultRowHeight := 20;
      DefaultColWidth  := 70;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];
    end;

    FMinimumEdit       := CreateFieldEdit(FAppModules, lOwner, FType3HeadPanel2,   2,  25,  70,  21, 0, TRUE);
    FMinimumLabel      := CreateFieldLabel            (lOwner, FType3HeadPanel2,  75,  25,  46,  21);
    FRangeLabel        := CreateFieldLabel            (lOwner, FType3HeadPanel2, 150,  25, 130,  21);
    FMaximumLabel      := CreateFieldLabel            (lOwner, FType3HeadPanel2, 308,  25,  49,  21);
    FMaximumEdit       := CreateFieldEdit(FAppModules, lOwner, FType3HeadPanel2, 361,  25,  70,  21, 1, TRUE);
    FMinimumEdit.ReadOnly := TRUE;
    FMinimumEdit.Color    := clBtnFace;
    FMaximumEdit.ReadOnly := TRUE;
    FMaximumEdit.Color    := clBtnFace;

    //FDivRelationshipBtn := CreateFieldButton(FAppModules, lOwner, lParent, 320,  10, 150, 25, 3, FALSE, 'DiversionRelationship');
    FFlowDiversionRelationshipChart := TAbstractChart.Create(LOwner, FAppModules);
    FFlowDiversionRelationshipChart.Parent := FType2GroupBox ;

    FFlowDiversionRelationshipLineSeries := TLineSeries.Create(FFlowDiversionRelationshipChart);
    FFlowDiversionRelationshipLineSeries.ParentChart := FFlowDiversionRelationshipChart;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureDialog.Resize;
const OPNAME = 'TDiversionFeatureDialog.Resize';
begin
  inherited Resize;
  try
    VCL.Controls.TControl(FFlowDiversionRelationshipChart).Top := FType2Grid.Top;
    VCL.Controls.TControl(FFlowDiversionRelationshipChart).Left := FType2Grid.Left + FType2Grid.Width + C_ControlOffset;
    FFlowDiversionRelationshipChart.Width := (FType2Grid.Width*3)-(C_ControlOffset*5);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionFeatureDialog.Initialise: boolean;
const OPNAME = 'TDiversionFeatureDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FBtnInsertRow.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORADDSERIES');
    FBtnDeleteRow.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORREMOVESERIES');
    FBtnInsertRow.Enabled := FALSE;
    FBtnDeleteRow.Enabled := FALSE;

    FFlowDiversionRelationshipChart.BevelOuter                    := bvNone;
    FFlowDiversionRelationshipChart.Legend.Visible                := False;
    FFlowDiversionRelationshipChart.AxisVisible                   := True;
    FFlowDiversionRelationshipChart.AllowZoom                     := True;
    FFlowDiversionRelationshipChart.AllowPanning                  := pmBoth;
    FFlowDiversionRelationshipChart.Gradient.Visible              := False;
    FFlowDiversionRelationshipChart.View3D                        := False;
    FFlowDiversionRelationshipChart.Title.Visible                 := True;
    FFlowDiversionRelationshipChart.Title.Font.Style              := [fsBold];
    FFlowDiversionRelationshipChart.Title.Font.Color              := clBlack;
    FFlowDiversionRelationshipChart.LeftAxis.Title.Angle          := 90;
    FFlowDiversionRelationshipChart.BottomAxis.LabelsAngle        := 90;
    FFlowDiversionRelationshipChart.BottomAxis.Automatic          := True;
    FFlowDiversionRelationshipChart.BottomAxis.TickLength         := 6;
    FFlowDiversionRelationshipChart.ScaleLastPage                 := False;
    FFlowDiversionRelationshipChart.BottomAxis.MinorTickCount     := 4;

    FFlowDiversionRelationshipLineSeries.Marks.Visible := False;
    FFlowDiversionRelationshipLineSeries.LinePen.Width := 2;
    FFlowDiversionRelationshipLineSeries.Clear;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionFeatureDialog.LanguageHasChanged: boolean;
const OPNAME = 'TDiversionFeatureDialog.LanguageHasChanged';
var
  lLanguage : TAbstractLanguage;
begin
  Result := inherited LanguageHasChanged;
  try
    lLanguage := FAppModules.Language;
    FFeatureNameLabel.Caption  := lLanguage.GetString('TField.DiversionChannelName') + ' :';
    FType1HeadLabel1.Caption   := lLanguage.GetString('NetworkFeatures.DiversionDemand1');
    FType1HeadLabel2.Caption   := lLanguage.GetString('NetworkFeatures.NetNaturalInflow1');
    //FType2HeadLabel1.Caption   := lLanguage.GetString('NetworkFeatures.DiversionDemand2');
    //FType2HeadLabel2.Caption   := lLanguage.GetString('NetworkFeatures.NetNaturalInflow2');
    //FType4HeadLabel1.Caption   := lLanguage.GetString('NetworkFeatures.DiversionDemand4');
    //FType4HeadLabel2.Caption   := lLanguage.GetString('NetworkFeatures.NetNaturalInflow4');
    FType3HeadLabel1.Caption   := lLanguage.GetString('TField.FlowValue');
    FType3HeadLabel2.Caption   := lLanguage.GetString('TField.ControllingResLevels');
    FType3HeadLabel3.Caption   := lLanguage.GetString('TField.DivertedFlow');
    FReservoirLabel.Caption    := lLanguage.GetString('NetworkFeatures.ControllingReservoir') + ' :';
    FNrOfFlowsLabel.Caption    := lLanguage.GetString('TField.ReferenceFlowsCount') + ' :';
    FNrOfLevelsLabel.Caption   := lLanguage.GetString('TField.ReservoirStorageNumber') + ' :';
    FMinimumLabel.Caption      := lLanguage.GetString('Channel.Minimum');
    FMinimumEdit.Hint          := lLanguage.GetString('NetworkFeatures.MinimumResevoirLevel');
    FRangeLabel.Caption        := lLanguage.GetString('Channel.ReservoirLevelRange');
    FMaximumLabel.Caption      := lLanguage.GetString('Channel.Maximum');
    FMaximumEdit.Hint          := lLanguage.GetString('NetworkFeatures.MaximumResevoirLevel');
    FDivTypeRadioGroup.Caption := lLanguage.GetString('TField.DiversionChannelTypeDescr');    
    FDivTypeRadioGroup.Hints.Clear;
    FDivTypeRadioGroup.Hints.Add(lLanguage.GetString('TDiversionFeaturesDlg.DiversionChannelType1'));
    FDivTypeRadioGroup.Hints.Add(lLanguage.GetString('TDiversionFeaturesDlg.DiversionChannelType2'));
    FDivTypeRadioGroup.Hints.Add(lLanguage.GetString('TDiversionFeaturesDlg.DiversionChannelType3'));
    //FDivRelationshipBtn.Caption := lLanguage.GetString('ButtonCaption.DiversionRelationship');
    FBtnInsertRow.Caption := FAppModules.Language.GetString('ButtonCaption.InsertRow');
    FBtnDeleteRow.Caption := FAppModules.Language.GetString('ButtonCaption.DeleteRow');
    FlblStations.Caption  := 'Daily Diversion Station';
    FbtnGetStations.Caption := '...';

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionFeatureDialog.RestoreColourState;
const OPNAME = 'TDiversionFeatureDialog.RestoreColourState';
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

procedure TDiversionFeatureDialog.AssignHelpContext;
const OPNAME = 'TDiversionFeatureDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                  HC_StreamflowDiversionStructures);
    SetControlHelpContext(FDivFeatureNameEdit,   HC_StreamflowDiversionStructures);
    SetControlHelpContext(FDivTypeRadioGroup,    HC_StreamflowDiversionStructures);

    SetControlHelpContext(FType1HeadPanel1,      HC_StreamflowDiversionStructures);
    SetControlHelpContext(FType1HeadPanel2,      HC_StreamflowDiversionStructures);
    SetControlHelpContext(FType1Grid,            HC_StreamflowDiversionStructures);

    SetControlHelpContext(FType2Grid,            HC_StreamflowDiversionStructures);
    //SetControlHelpContext(FType2HeadPanel1,      HC_DiversionStructureEfficiencies);
    //SetControlHelpContext(FType1HeadPanel2,      HC_DiversionStructureEfficiencies);

    SetControlHelpContext(FType2GroupBox,        HC_StreamflowDiversionStructures);
    SetControlHelpContext(FType3HeadPanel1,      HC_StreamflowDiversionStructures);
    SetControlHelpContext(FType1HeadPanel2,      HC_StreamflowDiversionStructures);

    SetControlHelpContext(FType3GroupBox,        HC_StreamflowDiversionStructures);
    SetControlHelpContext(FType3FlowsGrid,       HC_StreamflowDiversionStructures);
    SetControlHelpContext(FType3LevelsGrid,      HC_StreamflowDiversionStructures);
    SetControlHelpContext(FType3ProportionsGrid, HC_StreamflowDiversionStructures);
    SetControlHelpContext(FType3Panel,           HC_StreamflowDiversionStructures);

    SetControlHelpContext(FReservoirCbx,         HC_StreamflowDiversionStructures);
    SetControlHelpContext(FNrOfFlowsEdit,        HC_StreamflowDiversionStructures);
    SetControlHelpContext(FNrOfLevelsEdit,       HC_StreamflowDiversionStructures);
    SetControlHelpContext(FMinimumEdit,          HC_StreamflowDiversionStructures);
    SetControlHelpContext(FMaximumEdit,          HC_StreamflowDiversionStructures);

    SetControlHelpContext(FcbxStations,          HC_StreamflowDiversionsPreprocessor);

   except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
