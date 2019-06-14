{******************************************************************************}
{*  UNIT      : Contains the class TPhysicalFlowConstraintDialog.             *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/11/27                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UPhysicalFlowConstraintDialog;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.Graphics,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type

  TPhysicalFlowConstraintDialog = class(TAbstractScrollablePanel)
  private
    FTopPanel                       : TPanel;
    FStructureTypeGroupBox          : TGroupBox;
    FStructureTypeLabel             : TLabel;
    FStructureTypeCbx               : TFieldComboBox;

    FFeatureNameLabel               : TLabel;
    FFeatureNameEdit                : TFieldEdit;
    FUpstreamReservoirLabel         : TLabel;
    FUpstreamReservoirCbx           : TFieldComboBox;
    FDownstreamReservoirLabel       : TLabel;
    FDownstreamReservoirCbx         : TFieldComboBox;

    FElevationOfSillLabel           : TLabel;
    FElevationOfSillEdit            : TFieldEdit;
    FGateHeightLabel                : TLabel;
    FGateHeightEdit                 : TFieldEdit;
    FDischargeCoefficientLabel      : TLabel;
    FDischargeCoefficientEdit       : TFieldEdit;
    FStructureLengthLabel           : TLabel;
    FStructureLengthEdit            : TFieldEdit;
    FWaterLevelAtDownstreamNodeLabel: TLabel;
    FWaterLevelAtDownstreamNodeEdit : TFieldEdit;
    FReferenceElevationLabel        : TLabel;
    FReferenceElevationEdit         : TFieldEdit;

    FBottomPanel                    : TPanel;
    FNrOfPointsGroupBox             : TGroupBox;
    FNrOfPointsLabel                : TLabel;
    FNrPointsEdit                   : TFieldEdit;
    FTwoColsGrid                    : TFieldStringGrid;
    FFourColsGrid                   : TFieldStringGrid;
    FTenColsGrid                    : TFieldStringGrid;
    FElevationDifferenceGrid        : TFieldStringGrid;

  protected
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    procedure SetFeatureType(AType: integer);
    procedure SetNumberOfPoints(APointNum: integer);
    procedure SetLanguagePerType(AType: integer);
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property FeatureNameEdit                : TFieldEdit        read FFeatureNameEdit;
    property UpstreamReservoirCbx           : TFieldComboBox    read FUpstreamReservoirCbx;
    property DownstreamReservoirCbx         : TFieldComboBox    read FDownstreamReservoirCbx;
    property StructureTypeCbx               : TFieldComboBox    read FStructureTypeCbx;
    property ElevationOfSillEdit            : TFieldEdit        read FElevationOfSillEdit;
    property GateHeightEdit                 : TFieldEdit        read FGateHeightEdit;
    property DischargeCoefficientEdit       : TFieldEdit        read FDischargeCoefficientEdit;
    property StructureLengthEdit            : TFieldEdit        read FStructureLengthEdit;
    property WaterLevelAtDownstreamNodeEdit : TFieldEdit        read FWaterLevelAtDownstreamNodeEdit;
    property ReferenceElevationEdit         : TFieldEdit        read FReferenceElevationEdit;
    property TwoColsGrid                    : TFieldStringGrid  read FTwoColsGrid;
    property TenColsGrid                    : TFieldStringGrid  read FTenColsGrid;
    property ElevationDifferenceGrid        : TFieldStringGrid  read FElevationDifferenceGrid;
    property FourColsGrid                   : TFieldStringGrid  read FFourColsGrid;
    property NrPointsEdit                   : TFieldEdit        read FNrPointsEdit;
  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  VCL.Forms,
  Windows,
  UErrorHandlingOperations,
  UControlCreationUtilities, VCL.Grids, UStringGridWithCellChange;

{******************************************************************************}
{* TPhysicalFlowConstraintDialog                                              *}
{******************************************************************************}

procedure TPhysicalFlowConstraintDialog.CreateMemberObjects;
const OPNAME = 'TPhysicalFlowConstraintDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;

    //                                                                             Left  Top  Width Height
    FTopPanel                        := CreatePanel                     (lOwner, lParent,    1,   1,  570,  34, 0);
    FStructureTypeGroupBox           := CreateFieldGroupBox             (lOwner, FTopPanel,    1,   1,  570,  38, 0,False);
    FStructureTypeLabel              := CreateFieldLabel                (lOwner, FStructureTypeGroupBox,  10,  10,  160,  21);
    FStructureTypeCbx                := CreateFieldComboBox(FAppModules, lOwner, FStructureTypeGroupBox, 200,  10,  380,  21, 3, TRUE, csDropDownList);
    FFeatureNameLabel                := CreateFieldLabel                (lOwner, FTopPanel,  10,  40, 160,  21);
    FFeatureNameEdit                 := CreateFieldEdit    (FAppModules, lOwner, FTopPanel, 200,  40, 200,  21, 0, TRUE);
    FUpstreamReservoirLabel          := CreateFieldLabel                (lOwner, FTopPanel,  10,  65, 160,  21);
    FUpstreamReservoirCbx            := CreateFieldComboBox(FAppModules, lOwner, FTopPanel, 200,  65, 180,  21, 1, TRUE, csDropDownList);
    FDownstreamReservoirLabel        := CreateFieldLabel                (lOwner, FTopPanel,  10,  90, 160,  21);
    FDownstreamReservoirCbx          := CreateFieldComboBox(FAppModules, lOwner, FTopPanel, 200,  90, 180,  21, 2, TRUE, csDropDownList);
    FElevationOfSillLabel            := CreateFieldLabel                (lOwner, FTopPanel,  10, 115, 160,  21);
    FElevationOfSillEdit             := CreateFieldEdit    (FAppModules, lOwner, FTopPanel, 200, 115, 100,  21, 4, TRUE);
    FGateHeightLabel                 := CreateFieldLabel                (lOwner, FTopPanel,  10, 140, 160,  21);
    FGateHeightEdit                  := CreateFieldEdit    (FAppModules, lOwner, FTopPanel, 200, 140, 100,  21, 5, TRUE);
    FDischargeCoefficientLabel       := CreateFieldLabel                (lOwner, FTopPanel,  10, 165, 160,  21);
    FDischargeCoefficientEdit        := CreateFieldEdit    (FAppModules, lOwner, FTopPanel, 200, 165, 100,  21, 6, TRUE);
    FStructureLengthLabel            := CreateFieldLabel                (lOwner, FTopPanel,  10, 190, 160,  21);
    FStructureLengthEdit             := CreateFieldEdit    (FAppModules, lOwner, FTopPanel, 200, 190, 100,  21, 7, TRUE);
    FWaterLevelAtDownstreamNodeLabel := CreateFieldLabel                (lOwner, FTopPanel,  10, 115, 180,  21);
    FWaterLevelAtDownstreamNodeEdit  := CreateFieldEdit    (FAppModules, lOwner, FTopPanel, 200, 115, 100,  21, 6, TRUE);
    FReferenceElevationLabel         := CreateFieldLabel                (lOwner, FTopPanel,  10, 115, 180,  21);
    FreferenceElevationEdit          := CreateFieldEdit    (FAppModules, lOwner, FTopPanel, 200, 115, 100,  21, 4, TRUE);

    FBottomPanel               := CreatePanel                     (lOwner, lParent, 178, 115, 190, 278, 0);
    FNrOfPointsGroupBox        := CreateFieldGroupBox             (lOwner, FBottomPanel,    1,   1,  570,  38, 0,False);
    FNrOfPointsLabel           := CreateFieldLabel                (lOwner, FNrOfPointsGroupBox, 10,  10, 160,  21);
    FNrPointsEdit              := CreateFieldEdit    (FAppModules, lOwner, FNrOfPointsGroupBox, 200, 10, 100,  21, 3, TRUE);;

    FTwoColsGrid               := CreateFieldStringGrid(FAppModules, lOwner, FBottomPanel,  2, 2, 186, 274, 8, TRUE);
    with FTwoColsGrid do
    begin
      //ScrollBars       := ssNone;
      ColCount         := 3;
      RowCount         := 11;
      FixedCols        := 1;
      FixedRows        := 1;
      DefaultRowHeight := 20;
      DefaultColWidth  := 80;
      ColWidths[0]     := 20;
      RowHeights[0]    := 60;
      WrapHeaderText := True;
    end;
    FFourColsGrid               := CreateFieldStringGrid(FAppModules, lOwner, FBottomPanel,  2, 2, 350, 274, 8, TRUE);
    with FFourColsGrid do
    begin
      //ScrollBars       := ssNone;
      ColCount         := 5;
      RowCount         := 11;
      FixedCols        := 1;
      FixedRows        := 1;
      DefaultRowHeight := 20;
      DefaultColWidth  := 80;
      ColWidths[0]     := 20;
      RowHeights[0]    := 60;
      WrapHeaderText := True;
    end;


//    FElevationDifferenceGrid            := CreateFieldStringGrid(FAppModules, lOwner, FBottomPanel, 160, 2, 585, 60, 9, TRUE);
    FElevationDifferenceGrid            := CreateFieldStringGrid(FAppModules, lOwner, FBottomPanel, 160, 2, 635, 60, 9, TRUE);
    with FElevationDifferenceGrid do
    begin
      //ScrollBars       := ssNone;
      ColCount         := 11;
      RowCount         := 1;
      FixedCols        := 1;
      FixedRows        := 0;
      DefaultRowHeight := 20;
      DefaultColWidth  := 50;

//      ColWidths[0]     := 70;
      ColWidths[0]     := 120;
      RowHeights[0]    := 60;
      WrapHeaderText   := True;

      //ScrollBars       := ssBoth;
      //DefaultDrawing   := FALSE;
      //DblClickColAutoSize := FALSE;
    end;

//    FTenColsGrid            := CreateFieldStringGrid(FAppModules, lOwner, FBottomPanel, 160, 50, 585, 274, 9, TRUE);
    FTenColsGrid            := CreateFieldStringGrid(FAppModules, lOwner, FBottomPanel, 160, 50, 635, 274, 9, TRUE);
    with FTenColsGrid do
    begin
      //ScrollBars       := ssNone;
      ColCount         := 12;
      RowCount         := 11;
      FixedCols        := 1;
      FixedRows        := 1;
      DefaultRowHeight := 20;
      DefaultColWidth  := 50;
      ColWidths[0]     := 20;
      ColWidths[1]     := 100;      
      RowHeights[0]    := 60;
      WrapHeaderText := True;
      //ScrollBars       := ssBoth;
      //DefaultDrawing   := FALSE;
      //DblClickColAutoSize := FALSE;
    end;

    FTopPanel.BorderStyle := bsNone;
    FBottomPanel.BorderStyle := bsNone;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintDialog.Resize;
const OPNAME = 'TPhysicalFlowConstraintDialog.Resize';
begin
   LockWindowUpdate(Self.Handle);
   try
    inherited Resize;
    FTopPanel.Align     := alNone;
    FBottomPanel.Align  := alNone;
    FStructureTypeGroupBox.Align  := alNone;
    FNrOfPointsGroupBox.Align     := alNone;
    try
      FTopPanel.Align                 := alTop;
      FStructureTypeGroupBox.Align    := alTop;
      FBottomPanel.Align              := alClient;
      FNrOfPointsGroupBox.Align       := alTop;

      FFourColsGrid.Left              := FNrPointsEdit.Left;
      FFourColsGrid.Top               := FNrOfPointsGroupBox.Top + FNrOfPointsGroupBox.Height;
      FFourColsGrid.Height            := FBottomPanel.ClientHeight-FTwoColsGrid.Top - 10;
      if FTenColsGrid.Visible then
      begin
        FTwoColsGrid.Left             := FNrOfPointsLabel.Left;
        FTwoColsGrid.Top              := FNrOfPointsGroupBox.Top;
        FTwoColsGrid.Height           := FBottomPanel.ClientHeight-FTwoColsGrid.Top - 10;

        FTenColsGrid.Left             := 10;
        FElevationDifferenceGrid.Top  := FTwoColsGrid.Top+2;
        FTenColsGrid.Top              := FElevationDifferenceGrid.Height + 2;
        FTenColsGrid.Height           := 350;

        FElevationDifferenceGrid.Left := 10;
      end
      else
      begin
        FTwoColsGrid.Left             := FNrPointsEdit.Left;
        FTwoColsGrid.Top              := FNrOfPointsGroupBox.Top + FNrOfPointsGroupBox.Height;
        FTwoColsGrid.Height           := FBottomPanel.ClientHeight-FTwoColsGrid.Top - 10;
      end;
   finally
     LockWindowUpdate(0);
   end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPhysicalFlowConstraintDialog.Initialise: boolean;
const OPNAME = 'TPhysicalFlowConstraintDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FStructureTypeCbx.DropDownCount := 14;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPhysicalFlowConstraintDialog.LanguageHasChanged: boolean;
const OPNAME = 'TPhysicalFlowConstraintDialog.LanguageHasChanged';
var
  lLanguage : TAbstractLanguage;
  lIndex    : integer;
begin
  Result := inherited LanguageHasChanged;
  try
    lLanguage := FAppModules.Language;
    FFeatureNameLabel.Caption          := lLanguage.GetString('NetworkFeatures.PhysicalFlowConstraintName') + ' :';
    FUpstreamReservoirLabel.Caption    := lLanguage.GetString('NetworkFeatures.UpStreamReservoir') + ' :';
    FDownstreamReservoirLabel.Caption  := lLanguage.GetString('NetworkFeatures.DownStreamReservoir') + ' :';
    FStructureTypeLabel.Caption        := lLanguage.GetString('TField.StructureType') + ' :';
    FElevationOfSillLabel.Caption      := lLanguage.GetString('TField.SillElevation') + ' :';
    FGateHeightLabel.Caption          := lLanguage.GetString('NetworkFeatures.MaximumGateHeight') + ' :';
    FGateHeightLabel.Caption          := FAppModules.Language.GetString('NetworkFeatures.MaximumGateHeight') + ' :';
    //FGateHeightLabelB.Caption          := lLanguage.GetString('NetworkFeatures.ReferenceElevation') + ' :';
    FDischargeCoefficientLabel.Caption := lLanguage.GetString('TField.DischargeCoefficient') + ' :';
    FStructureLengthLabel.Caption     := lLanguage.GetString('NetworkFeatures.StructureLength') + ' :';
    //FStructureLengthLabelB.Caption     := lLanguage.GetString('NetworkFeatures.MaximumSpecifiedChannelFlow') + ' :';
    //FTypeAllHeadLabel1.Caption         := lLanguage.GetString('NetworkFeatures.Elevations');
    //FTypeAllHeadLabel2.Caption         := lLanguage.GetString('NetworkFeatures.Discharges');
    //FType10HeadLabel1.Caption          := lLanguage.GetString('NetworkFeatures.PipeChannelNumbers');
    //FType10HeadLabel2.Caption          := lLanguage.GetString('NetworkFeatures.KFactors');
    //FType11HeadLabel1.Caption          := lLanguage.GetString('NetworkFeatures.AquiferHeadDifference');
    //FType11HeadLabel2.Caption          := lLanguage.GetString('NetworkFeatures.AquiferFlow');
    FNrOfPointsLabel.Caption                 := lLanguage.GetString('NetworkFeatures.NumberOfPoints');
    FWaterLevelAtDownstreamNodeLabel.Caption := lLanguage.GetString('NetworkFeatures.WaterLevelAtDownstreamNode');
    FReferenceElevationLabel.Caption         := lLanguage.GetString('NetworkFeatures.ReferenceElevation');
    for lIndex := 1 to 10 do
    begin
      //Type11Grid2.Cells[lIndex*2 - 1, 0] := lLanguage.GetString('NetworkFeatures.DownstreamNodeInflow');
      //Type11Grid2.Cells[lIndex*2, 0]     := lLanguage.GetString('NetworkFeatures.RiverDepth');
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintDialog.AssignHelpContext;
const OPNAME = 'TPhysicalFlowConstraintDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                           HC_FlowControlStructures);
    SetControlHelpContext(FStructureTypeCbx,              HC_FlowControlStructures);
    SetControlHelpContext(FFeatureNameEdit,               HC_FlowControlStructures);
    SetControlHelpContext(FUpstreamReservoirCbx,          HC_FlowControlStructures);
    SetControlHelpContext(FDownstreamReservoirCbx,        HC_FlowControlStructures);

    SetControlHelpContext(FElevationOfSillEdit,            HC_FlowControlStructures);
    SetControlHelpContext(FGateHeightEdit,                 HC_FlowControlStructures);
    SetControlHelpContext(FDischargeCoefficientEdit,       HC_FlowControlStructures);
    SetControlHelpContext(FStructureLengthEdit,            HC_FlowControlStructures);
    SetControlHelpContext(FWaterLevelAtDownstreamNodeEdit, HC_FlowControlStructures);
    SetControlHelpContext(FReferenceElevationEdit,         HC_FlowControlStructures);

    SetControlHelpContext(FTwoColsGrid,                    HC_FlowControlStructures);
    SetControlHelpContext(FTenColsGrid,                    HC_FlowControlStructures);
    SetControlHelpContext(FElevationDifferenceGrid,        HC_FlowControlStructures);
    SetControlHelpContext(FFourColsGrid,                   HC_SandAquifers);

  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TPhysicalFlowConstraintDialog.RestoreColourState;
const OPNAME = 'TPhysicalFlowConstraintDialog.RestoreColourState';
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


procedure TPhysicalFlowConstraintDialog.SetLanguagePerType(AType: integer);
const OPNAME = 'TPhysicalFlowConstraintDialog.SetLanguagePerType';
var
  LRow,LCol: integer;
begin
  try
    FStructureLengthLabel.Caption     := FAppModules.Language.GetString('NetworkFeatures.StructureLength') + ' :';
    case AType of
      1:
      begin
      end;
      2,3:
      begin
      end;
      6:
      begin
        FStructureLengthLabel.Caption     := FAppModules.Language.GetString('NetworkFeatures.MaxFlowRate') + ' :';
      end;
      4,5,7,8,9,14:
      begin
        FTwoColsGrid.Cells[0,0] := ' ';
        FTwoColsGrid.Cells[1,0] := FAppModules.Language.GetString('NetworkFeatures.Elevations');
        FTwoColsGrid.Cells[2,0] := FAppModules.Language.GetString('NetworkFeatures.Discharges');
        for LRow := 1 to FTwoColsGrid.RowCount-1 do
          FTwoColsGrid.Cells[0,LRow] := IntToStr(LRow);
      end;
      10:
      begin
        FStructureLengthLabel.Caption     := FAppModules.Language.GetString('NetworkFeatures.ReferenceElevation') + ' :';
        FTwoColsGrid.Cells[0,0] := ' ';
        FTwoColsGrid.Cells[1,0] := FAppModules.Language.GetString('NetworkFeatures.PipeChannelNumbers');
        FTwoColsGrid.Cells[2,0] := FAppModules.Language.GetString('NetworkFeatures.KFactors');
        for LRow := 1 to FTwoColsGrid.RowCount-1 do
          FTwoColsGrid.Cells[0,LRow] := IntToStr(LRow);
      end;
      11:
      begin
        FFourColsGrid.Cells[0,0] := ' ';
        FFourColsGrid.Cells[1,0] := FAppModules.Language.GetString('TField.ConstraintHeadDifferences');
        FFourColsGrid.Cells[2,0] := FAppModules.Language.GetString('TField.ConstraintAquiferFlows');
        FFourColsGrid.Cells[3,0] := FAppModules.Language.GetString('TField.ConstraintDownStreamNodeInflows');
        FFourColsGrid.Cells[4,0] := FAppModules.Language.GetString('TField.ConstraintRiverDepths');
        for LRow := 1 to FFourColsGrid.RowCount-1 do
          FFourColsGrid.Cells[0,LRow] := IntToStr(LRow);
      end;
      12:
      begin
        FTwoColsGrid.Cells[0,0] := ' ';
        FTwoColsGrid.Cells[1,0] := FAppModules.Language.GetString('TField.ConstraintElevationDifferences');
        FTwoColsGrid.Cells[2,0] := FAppModules.Language.GetString('TField.ConstraintMonthlyAverageInflows');
        for LRow := 1 to FTwoColsGrid.RowCount-1 do
          FTwoColsGrid.Cells[0,LRow] := IntToStr(LRow);
        for LRow := 1 to TenColsGrid.RowCount-1 do
          TenColsGrid.Cells[0,LRow] := IntToStr(LRow);

        TenColsGrid.Cells[1,0] := FAppModules.Language.GetString('TField.ConstraintMonthlyAverageInflows');
        for LCol := 2 to TenColsGrid.ColCount-1 do
          TenColsGrid.Cells[LCol,0] := Format(FAppModules.Language.GetString('TField.ConstraintMonthlyAverageDivertedFlow'),[LCol-1]);

        FElevationDifferenceGrid.Cells[0,0] := FAppModules.Language.GetString('TField.ConstraintElevationDifferences');
      end;
      13:
      begin
        FTwoColsGrid.Cells[0,0] := ' ';
        FTwoColsGrid.Cells[1,0] := FAppModules.Language.GetString('TField.ConstraintPumpingHeads');
        FTwoColsGrid.Cells[2,0] := FAppModules.Language.GetString('TField.ConstraintPumpingDischarges');
        for LRow := 1 to FTwoColsGrid.RowCount-1 do
          FTwoColsGrid.Cells[0,LRow] := IntToStr(LRow);
      end;
    end;//Case
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintDialog.SetFeatureType(AType: integer);
const OPNAME = 'TPhysicalFlowConstraintDialog.SetFeatureType';
var
  LCol: integer;
begin
  try
    if(AType > 0) and  (AType <= 14) then
    begin
      FBottomPanel.Visible                       := False;
      FElevationOfSillLabel.Visible              := False;
      FElevationOfSillEdit.Visible               := False;
      FGateHeightLabel.Visible                   := False;
      FGateHeightEdit.Visible                    := False;
      FDischargeCoefficientLabel.Visible         := False;
      FDischargeCoefficientEdit.Visible          := False;
      FStructureLengthLabel.Visible              := False;
      FStructureLengthEdit.Visible               := False;
      FWaterLevelAtDownstreamNodeLabel.Visible   := False;
      FWaterLevelAtDownstreamNodeEdit.Visible    := False;
      FReferenceElevationLabel.Visible           := False;
      FReferenceElevationEdit.Visible            := False;
      FBottomPanel.Visible                       := False;
      FNrOfPointsGroupBox.Visible                := False;
      FTwoColsGrid.Visible                       := False;
      FFourColsGrid.Visible                      := False;
      FTenColsGrid.Visible                       := False;
      FElevationDifferenceGrid.Visible           := False;
      GateHeightEdit.FieldProperty               := FAppModules.FieldProperties.FieldProperty('GateHeight');

      case AType of
        1:
        begin
          FTopPanel.Height                   := FDownstreamReservoirCbx.Top + FDownstreamReservoirCbx.Height + 20;
        end;
        2,3:
        begin
          FElevationOfSillLabel.Visible      := True;
          FElevationOfSillEdit.Visible       := True;
          FGateHeightLabel.Visible           := True;
          FGateHeightEdit.Visible            := True;
          FDischargeCoefficientLabel.Visible := True;
          FDischargeCoefficientEdit.Visible  := True;
          FStructureLengthLabel.Visible      := True;
          FStructureLengthEdit.Visible       := True;
          FStructureLengthLabel.Top          := 190;
          FStructureLengthEdit.Top           := 190;
          FTopPanel.Height                   := FStructureLengthEdit.Top + FStructureLengthEdit.Height + 20;
        end;
        6:
        begin
          FStructureLengthLabel.Visible      := True;
          FStructureLengthEdit.Visible       := True;
          FStructureLengthLabel.Top          := FElevationOfSillLabel.Top;
          FStructureLengthEdit.Top           := FElevationOfSillEdit.Top;
          FTopPanel.Height                   := FStructureLengthEdit.Top + FStructureLengthEdit.Height + 20;
        end;
        4,5,7,8,9,14:
        begin
          FBottomPanel.Visible               := True;
          FNrOfPointsGroupBox.Visible        := True;
          FTwoColsGrid.Visible               := True;
          FTopPanel.Height                   := FDownstreamReservoirCbx.Top + FDownstreamReservoirCbx.Height + 20;
          FTwoColsGrid.ClearFieldProperties;
          FTwoColsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
          FTwoColsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ConstraintElevation'));
          FTwoColsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ConstraintDischarge'));
        end;
        10:
        begin
          FStructureLengthLabel.Visible      := True;
          FStructureLengthEdit.Visible       := True;
          FStructureLengthLabel.Top          := FElevationOfSillLabel.Top;
          FStructureLengthEdit.Top           := FElevationOfSillEdit.Top;
          FTopPanel.Height                   := FStructureLengthEdit.Top + FStructureLengthEdit.Height + 20;
          FBottomPanel.Visible               := True;
          FNrOfPointsGroupBox.Visible        := True;
          FTwoColsGrid.Visible               := True;
          FTwoColsGrid.ClearFieldProperties;
          FTwoColsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
          FTwoColsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ConstraintChannelNumber'));
          FTwoColsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ConstraintKFactor'));
        end;
        11:
        begin
          FBottomPanel.Visible               := True;
          FNrOfPointsGroupBox.Visible        := True;
          FFourColsGrid.Visible              := True;
          FTopPanel.Height                   := FDownstreamReservoirCbx.Top + FDownstreamReservoirCbx.Height + 20;
          FFourColsGrid.ClearFieldProperties;
          FFourColsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
          FFourColsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ConstraintHeadDifferences'));
          FFourColsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ConstraintAquiferFlows'));
          FFourColsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ConstraintDownStreamNodeInflows'));
          FFourColsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ConstraintRiverDepths'));
        end;
        12:
        begin
          FReferenceElevationLabel.Visible   := True;
          FReferenceElevationEdit.Visible    := True; 
          FBottomPanel.Visible               := True;
          FNrOfPointsGroupBox.Visible        := False;
          FTwoColsGrid.Visible               := False;
//          FTwoColsGrid.Visible               := True;
//          FTwoColsGrid.RowCount              := 11;
          FElevationDifferenceGrid.Visible   := True;
          FTenColsGrid.Visible               := True;
          FTopPanel.Height                   := FReferenceElevationEdit.Top + FReferenceElevationEdit.Height + 20;
{          FTwoColsGrid.ClearFieldProperties;
          FTwoColsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
          FTwoColsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ConstraintElevationDifferences'));
          FTwoColsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ConstraintMonthlyAverageInflows'));}
          FElevationDifferenceGrid.ClearFieldProperties;
          FElevationDifferenceGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
          for LCol := 0 to FElevationDifferenceGrid.ColCount do
            FElevationDifferenceGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ConstraintElevationDifferences'));
          FTenColsGrid.ClearFieldProperties;
          FTenColsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
          FTenColsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ConstraintMonthlyAverageInflows'));
          for LCol := 1 to FTenColsGrid.ColCount do
            FTenColsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ConstraintMonthlyAverageDivertedFlow'));
        end;
        13:
        begin
          FWaterLevelAtDownstreamNodeLabel.Visible := True;
          FWaterLevelAtDownstreamNodeEdit.Visible  := True;
          FTopPanel.Height                   := FWaterLevelAtDownstreamNodeEdit.Top + FWaterLevelAtDownstreamNodeEdit.Height + 20;
          FBottomPanel.Visible               := True;
          FNrOfPointsGroupBox.Visible        := True;
          FTwoColsGrid.Visible               := True;
//          FTopPanel.Height                   := FDownstreamReservoirCbx.Top + FDownstreamReservoirCbx.Height + 20;
          FTwoColsGrid.ClearFieldProperties;
          FTwoColsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
          FTwoColsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ConstraintPumpingHeads'));
          FTwoColsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ConstraintPumpingDischarges'));
        end;
      end;
      SetLanguagePerType(AType);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintDialog.SetNumberOfPoints(APointNum: integer);
const OPNAME = 'TPhysicalFlowConstraintDialog.SetNumberOfPoints';
var
  LRow,LCol: integer;
begin
  try
    if(APointNum > 0) and (APointNum <= 10) then
    begin
      APointNum := APointNum + FTwoColsGrid.FixedRows;
      FTwoColsGrid.RowCount     := APointNum;
      FFourColsGrid.RowCount    := APointNum;
      FElevationDifferenceGrid.ColCount := APointNum;
    end
    else
    begin
      FTwoColsGrid.RowCount  := FTwoColsGrid.FixedRows+1;
      FFourColsGrid.RowCount := FTwoColsGrid.FixedRows+1;
    end;
    for LRow := 0 to FTwoColsGrid.RowCount-1 do
      FTwoColsGrid.Rows[LRow].Clear;
    for LRow := 0 to FFourColsGrid.RowCount-1 do
      FFourColsGrid.Rows[LRow].Clear;
    for LRow := 0 to FTenColsGrid.RowCount-1 do
      FTenColsGrid.Rows[LRow].Clear;
    for LCol := 0 to FElevationDifferenceGrid.ColCount-1 do
     FElevationDifferenceGrid.Cols[LCol].Clear;
    Resize;
    if (APointNum = 0)  then
    begin
      FTwoColsGrid.Options             := FTwoColsGrid.Options  - [goEditing	];
      FFourColsGrid.Options            := FFourColsGrid.Options - [goEditing	];
      FTenColsGrid.Options             := FTenColsGrid.Options  - [goEditing	];
      FElevationDifferenceGrid.Options := FElevationDifferenceGrid.Options - [goEditing	];
    end
    else
    begin
      FTwoColsGrid.Options             := FTwoColsGrid.Options  + [goEditing	];
      FFourColsGrid.Options            := FFourColsGrid.Options + [goEditing	];
      FTenColsGrid.Options             := FTenColsGrid.Options  + [goEditing	];
      FElevationDifferenceGrid.Options := FElevationDifferenceGrid.Options + [goEditing	];
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

