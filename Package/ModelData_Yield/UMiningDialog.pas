{******************************************************************************}
{*  UNIT      : Contains the class TMiningDialog.                             *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2007/03/09                                                    *}
{*  COPYRIGHT : Copyright © 2007 DWAF                                         *}
{******************************************************************************}

unit UMiningDialog;

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

  TMiningDialog = class(TAbstractScrollablePanel)
  private
  protected
    FMineNumberLabel                : TLabel;
    FMineNumberEdt                  : TFieldEdit;
    FMineNameLabel                  : TLabel;
    FMineNameEdt                    : TFieldEdit;
    FNrOfMineCastPitsLabel          : TLabel;
    FNrOfMineCastPitsEdt            : TFieldEdit;
    FNrOfUnderGroundMiningLabel     : TLabel;
    FNrOfUnderGroundMiningEdt       : TFieldEdit;
    FNrOfSlurryDumpLabel            : TLabel;
    FNrOfSlurryDumpEdt              : TFieldEdit;
//    FRiverChannelNumberLabel        : TLabel;
//    FRiverChannelNumberCbx          : TFieldComboBox;
    FHydrologyNodeNumberLabel       : TLabel;
    FHydrologyNodeNumberCbx         : TFieldComboBox;
    FBeneficiationPlantAreaLabel    : TLabel;
    FBeneficiationPlantAreaEdt      : TFieldEdit;
    FBeneficiationRunOffFactorLabel : TLabel;
    FBeneficiationRunOffFactorEdt   : TFieldEdit;
    FMonthlyMeanPanEvapLabel        : TLabel;
    FMonthlyMeanPanEvapBtn          : TFieldButton;
    FMonthlyLakeEvapFactorsLabel    : TLabel;
    FMonthlyLakeEvapFactorsBtn      : TFieldButton;
    FMineXCoordLabel                : TLabel;
    FMineXCoordEdit                 : TFieldEdit;
    FMineYCoordLabel                : TLabel;
    FMineYCoordEdit                 : TFieldEdit;
    FPCDExistsLabel                 : TLabel;
    FPCDExistsChkBox                : TFieldChkBox;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property MineNumberEdt                : TFieldEdit     read FMineNumberEdt;
    property MineNameEdt                  : TFieldEdit     read FMineNameEdt;
    property NrOfMineCastPitsEdt          : TFieldEdit     read FNrOfMineCastPitsEdt;
    property NrOfSlurryDumpEdt            : TFieldEdit     read FNrOfSlurryDumpEdt;
    property NrOfUnderGroundMiningEdt     : TFieldEdit     read FNrOfUnderGroundMiningEdt;
    property BeneficiationPlantAreaEdt    : TFieldEdit     read FBeneficiationPlantAreaEdt;
    property BeneficiationRunOffFactorEdt : TFieldEdit     read FBeneficiationRunOffFactorEdt;
//    property RiverChannelNumberCbx        : TFieldComboBox read FRiverChannelNumberCbx;
    property HydrologyNodeNumberLabel     : TLabel         read FHydrologyNodeNumberLabel;
    property HydrologyNodeNumberCbx       : TFieldComboBox read FHydrologyNodeNumberCbx;
    property MonthlyMeanPanEvapBtn        : TFieldButton   read FMonthlyMeanPanEvapBtn;
    property MonthlyLakeEvapFactorsBtn    : TFieldButton   read FMonthlyLakeEvapFactorsBtn;
    property MineXCoordEdit               : TFieldEdit     read FMineXCoordEdit;
    property MineYCoordEdit               : TFieldEdit     read FMineYCoordEdit;
    property PCDExistsChkBox              : TFieldChkBox    read FPCDExistsChkBox;
   end;

  implementation

uses
  VCL.Grids,
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities;

const
  C_ControlBorder  = 10;
  C_LabelOffset    = 3;

{******************************************************************************}
{* TMiningDialog                                              *}
{******************************************************************************}

procedure TMiningDialog.CreateMemberObjects;
const OPNAME = 'TMiningDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
//                                                                              Left  Top Width Height
    FMineNumberLabel                := CreateFieldLabel(lOwner, lParent,                10,  10,   180, 21);
    FMineNameLabel                  := CreateFieldLabel(lOwner, lParent,                10,  35,   180, 21);
    FNrOfMineCastPitsLabel          := CreateFieldLabel(lOwner, lParent,                10,  60,   180, 21);
    FNrOfSlurryDumpLabel            := CreateFieldLabel(lOwner, lParent,                10,  85,   180, 21);
    FNrOfUnderGroundMiningLabel     := CreateFieldLabel(lOwner, lParent,                10,  110,  180, 21);
//    FRiverChannelNumberLabel        := CreateFieldLabel(lOwner, lParent,              10,  185,  180, 21);
    FHydrologyNodeNumberLabel       := CreateFieldLabel(lOwner, lParent,                10,  135,  180, 21);
    FMonthlyMeanPanEvapLabel        := CreateFieldLabel(lOwner, lParent,                10,  160,  180, 21);
    FMonthlyLakeEvapFactorsLabel    := CreateFieldLabel(lOwner, lParent,                10,  185,  180, 21);
    FBeneficiationPlantAreaLabel    := CreateFieldLabel(lOwner, lParent,                10,  210,  180, 21);
    FBeneficiationRunOffFactorLabel := CreateFieldLabel(lOwner, lParent,                10,  235,  180, 21);
    FMineXCoordLabel                := CreateFieldLabel(lOwner, lParent,                10,  260,  180, 21);
    FMineYCoordLabel                := CreateFieldLabel(lOwner, lParent,                10,  285,  180, 21);

    FMineNumberEdt                  := CreateFieldEdit(FAppModules, lOwner, lParent,     210, 10,  80 ,  20 , 8, TRUE);
    FMineNameEdt                    := CreateFieldEdit(FAppModules, lOwner, lParent,     210, 35,  280,  20 , 8, TRUE);
    FNrOfMineCastPitsEdt            := CreateFieldEdit(FAppModules, lOwner, lParent,     210, 60,  80,  20 , 8, TRUE);
    FNrOfSlurryDumpEdt              := CreateFieldEdit(FAppModules, lOwner, lParent,     210, 85,  80,  20 , 8, TRUE);
    FNrOfUnderGroundMiningEdt       := CreateFieldEdit(FAppModules, lOwner, lParent,     210, 110, 80,  20 , 8, TRUE);
//    FRiverChannelNumberCbx          := CreateFieldComboBox(FAppModules, lOwner, lParent, 210, 185, 280, 21, 3,   TRUE, csDropDownList);
    FHydrologyNodeNumberCbx         := CreateFieldComboBox(FAppModules, lOwner, lParent, 210, 135, 280, 21, 3,   TRUE, csDropDownList);
    FMonthlyMeanPanEvapBtn          := CreateFieldButton(FAppModules, lOwner, lParent,   210, 160,  25, 21, 8,  TRUE, '...');
    FMonthlyLakeEvapFactorsBtn      := CreateFieldButton(FAppModules, lOwner, lParent,   210, 185,  25, 21, 8,  TRUE, '...');
    FBeneficiationPlantAreaEdt      := CreateFieldEdit(FAppModules, lOwner, lParent,     210, 210, 80,  20 , 8, TRUE);
    FBeneficiationRunOffFactorEdt   := CreateFieldEdit(FAppModules, lOwner, lParent,     210, 235, 80,  20 , 8, TRUE);
    FMineXCoordEdit                 := CreateFieldEdit(FAppModules, lOwner, lParent,     210, 260, 80,  20 , 8, TRUE);
    FMineYCoordEdit                 := CreateFieldEdit(FAppModules, lOwner, lParent,     210, 285, 80,  20 , 8, TRUE);

    FPCDExistsLabel                 := CreateFieldLabel            (lOwner, lParent, 10,  310,  180, 21);
    FPCDExistsChkBox                := CreateFieldChkBox(FAppModules,lOwner,lParent,210,310,80,21,0,True,taRightJustify)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningDialog.Resize;
const OPNAME = 'TMiningDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningDialog.Initialise: boolean;
const OPNAME = 'TMiningDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningDialog.LanguageHasChanged: boolean;
const OPNAME = 'TMiningDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FMineNumberLabel.Caption                := FAppModules.Language.GetString('TField.MineNumber');
    FMineNameLabel.Caption                  := FAppModules.Language.GetString('TField.MineName');
    FNrOfMineCastPitsLabel.Caption          := FAppModules.Language.GetString('TField.NrOfMineCastPits');
    FNrOfSlurryDumpLabel.Caption            := FAppModules.Language.GetString('TField.NrOfSlurryDump');
    FNrOfUnderGroundMiningLabel.Caption     := FAppModules.Language.GetString('TField.NrOfUnderGroundMining');
//    FRiverChannelNumberLabel.Caption        := FAppModules.Language.GetString('TField.RiverChannelNumber');
    FHydrologyNodeNumberLabel.Caption       := FAppModules.Language.GetString('TField.HydrologyNodeNumber');
    FMonthlyMeanPanEvapLabel.Caption        := FAppModules.Language.GetString('TField.MinePanEvaporationFactors');
    FMonthlyLakeEvapFactorsLabel.Caption    := FAppModules.Language.GetString('TField.MineLakeEvaporationFactors');
    FBeneficiationPlantAreaLabel.Caption    := FAppModules.Language.GetString('TField.BeneficiationPlantArea');
    FBeneficiationRunOffFactorLabel.Caption := FAppModules.Language.GetString('TField.BeneficiationRunOffFactor');
    FMineXCoordLabel.Caption                 := FAppModules.Language.GetString('TField.XCoord');
    FMineYCoordLabel.Caption                 := FAppModules.Language.GetString('TField.YCoord');
    FPCDExistsLabel.Caption                 := FAppModules.Language.GetString('LabelCaption.PCDExists');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningDialog.RestoreColourState;
const OPNAME = 'TMiningDialog.RestoreColourState';
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

procedure TMiningDialog.AssignHelpContext;
const OPNAME = 'TMiningDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                          HC_CoalMines);
    SetControlHelpContext(FMineNumberEdt,                HC_CoalMines);
    SetControlHelpContext(FMineNameEdt,                  HC_CoalMines);
    SetControlHelpContext(FNrOfMineCastPitsEdt,          HC_CoalMines);
    SetControlHelpContext(FNrOfUnderGroundMiningEdt,     HC_CoalMines);
    SetControlHelpContext(FNrOfSlurryDumpEdt,            HC_CoalMines);
    SetControlHelpContext(FHydrologyNodeNumberCbx,       HC_CoalMines);
    SetControlHelpContext(FBeneficiationPlantAreaEdt,    HC_CoalMines);
    SetControlHelpContext(FBeneficiationRunOffFactorEdt, HC_CoalMines);
    SetControlHelpContext(FMineXCoordEdit,               HC_CoalMines);
    SetControlHelpContext(FMineYCoordEdit,               HC_CoalMines);
    SetControlHelpContext(FPCDExistsChkBox,              HC_CoalMines);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
