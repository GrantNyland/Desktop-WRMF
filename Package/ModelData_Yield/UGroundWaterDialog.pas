unit UGroundWaterDialog;

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

  TGroundWaterDialog = class(TAbstractScrollablePanel)
  private
  protected
    FGroundWaterNodeNumberLabel       : TLabel;
    FGroundWaterNodeNumberEdt         : TFieldEdit;
    FGroundWaterNameLabel             : TLabel;
    FGroundWaterNameEdt               : TFieldEdit;
    FGroundWaterDescriptionLabel      : TLabel;
    FGroundWaterDescriptionEdt        : TFieldEdit;
    FAquiferStorativityLabel          : TLabel;
    FAquiferStorativityEdt            : TFieldEdit;
    FAquiferStaticWaterLevelLabel     : TLabel;
    FAquiferStaticWaterLevelEdt       : TFieldEdit;
    FUnsaturatedStorageCapacityLabel  : TLabel;
    FUnsaturatedStorageCapacityEdt    : TFieldEdit;
    FInitialUnsaturatedStorageLabel   : TLabel;
    FInitialUnsaturatedStorageEdt     : TFieldEdit;
    FMaximumAquiferRechargeLabel      : TLabel;
    FMaximumAquiferRechargeEdt        : TFieldEdit;
    FMovingAverageRechargeLabel       : TLabel;
    FMovingAverageRechargeEdt         : TFieldEdit;
    FMaximumBaseFlowRateLabel         : TLabel;
    FMaximumBaseFlowRateEdt           : TFieldEdit;
    FHeadBaseFlowPowerLabel           : TLabel;
    FHeadBaseFlowPowerEdt             : TFieldEdit;
    FMaximumHydrologicalGradientLabel : TLabel;
    FMaximumHydrologicalGradientEdt   : TFieldEdit;
    FAquiferTransmissivityLabel       : TLabel;
    FAquiferTransmissivityEdt         : TFieldEdit;
    FBoreholeDistanceToRiverLabel     : TLabel;
    FBoreholeDistanceToRiverEdt       : TFieldEdit;
    FMaximumWaterAbstractionLabel     : TLabel;
    FMaximumWaterAbstractionEdt       : TFieldEdit;
    FParameterk2Label                 : TLabel;
    FParameterk2Edt                   : TFieldEdit;
    FParameterk3Label                 : TLabel;
    FParameterk3Edt                   : TFieldEdit;
    FMonthlyWaterEvaporationLabel     : TLabel;
    FMonthlyWaterEvaporationBtn       : TFieldButton;
    FMonthlyUsageFactorLabel          : TLabel;
    FMonthlyUsageFactorBtn            : TFieldButton;
    FWaterEvaporationAreaLabel        : TLabel;
    FWaterEvaporationAreaEdt          : TFieldEdit;
    FRefNodeNumberCbxLabel            : TLabel;
    FRefNodeNumberCbx                 : TFieldComboBox;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property GroundWaterNodeNumberEdt       : TFieldEdit     read     FGroundWaterNodeNumberEdt;
    property GroundWaterNameEdt             : TFieldEdit     read     FGroundWaterNameEdt;
    property GroundWaterDescriptionEdt      : TFieldEdit     read     FGroundWaterDescriptionEdt;
    property RefNodeNumberCbx               : TFieldComboBox read     FRefNodeNumberCbx;
    property AquiferStorativityEdt          : TFieldEdit     read     FAquiferStorativityEdt;
    property AquiferStaticWaterLevelEdt     : TFieldEdit     read     FAquiferStaticWaterLevelEdt;
    property UnsaturatedStorageCapacityEdt  : TFieldEdit     read     FUnsaturatedStorageCapacityEdt;
    property InitialUnsaturatedStorageEdt   : TFieldEdit     read     FInitialUnsaturatedStorageEdt;
    property MaximumAquiferRechargeEdt      : TFieldEdit     read     FMaximumAquiferRechargeEdt;
    property MovingAverageRechargeEdt       : TFieldEdit     read     FMovingAverageRechargeEdt;
    property MaximumBaseFlowRateEdt         : TFieldEdit     read     FMaximumBaseFlowRateEdt;
    property HeadBaseFlowPowerEdt           : TFieldEdit     read     FHeadBaseFlowPowerEdt;
    property MaximumHydrologicalGradientEdt : TFieldEdit     read     FMaximumHydrologicalGradientEdt;
    property AquiferTransmissivityEdt       : TFieldEdit     read     FAquiferTransmissivityEdt;
    property BoreholeDistanceToRiverEdt     : TFieldEdit     read     FBoreholeDistanceToRiverEdt;
    property MaximumWaterAbstractionEdt     : TFieldEdit     read     FMaximumWaterAbstractionEdt;
    property Parameterk2Edt                 : TFieldEdit     read     FParameterk2Edt;
    property Parameterk3Edt                 : TFieldEdit     read     FParameterk3Edt;
    property MonthlyWaterEvaporationBtn     : TFieldButton   read     FMonthlyWaterEvaporationBtn;
    property MonthlyUsageFactorBtn          : TFieldButton   read     FMonthlyUsageFactorBtn;
    property WaterEvaporationAreaEdt        : TFieldEdit     read     FWaterEvaporationAreaEdt;
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
{* TGroundWaterDialog                                              *}
{******************************************************************************}

procedure TGroundWaterDialog.AssignHelpContext;
const OPNAME = 'TGroundWaterDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                  HC_WaterResourcesYieldModel);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWaterDialog.CreateMemberObjects;
const OPNAME = 'TGroundWaterDialog.CreateMemberObjects';
var
  LOwner  : TComponent;
  LParent : TWinControl;
begin
  inherited;
  try
    LOwner  := ControlsOwner;
    LParent := ControlsParent;
//                                                                                      Left  Top Width Height
    FGroundWaterNodeNumberLabel       := CreateFieldLabel(LOwner, LParent,                10,  10,    220, 21);
    FGroundWaterNameLabel             := CreateFieldLabel(LOwner, LParent,                10,  35,    220, 21);
    FGroundWaterDescriptionLabel      := CreateFieldLabel(LOwner, LParent,                10,  60,    220, 21);
    FRefNodeNumberCbxLabel            := CreateFieldLabel(lOwner, lParent,                10,  85,    220, 21);
    FAquiferStorativityLabel          := CreateFieldLabel(LOwner, LParent,                10,  110,   220, 21);
    FAquiferStaticWaterLevelLabel     := CreateFieldLabel(LOwner, LParent,                10,  135,   220, 21);
    FUnsaturatedStorageCapacityLabel  := CreateFieldLabel(LOwner, LParent,                10,  160,   220, 21);
    FInitialUnsaturatedStorageLabel   := CreateFieldLabel(LOwner, LParent,                10,  185,   220, 21);
    FMaximumAquiferRechargeLabel      := CreateFieldLabel(LOwner, LParent,                10,  210,   220, 21);
    FMovingAverageRechargeLabel       := CreateFieldLabel(LOwner, LParent,                10,  235,   220, 21);
    FMaximumBaseFlowRateLabel         := CreateFieldLabel(LOwner, LParent,                10,  260,   220, 21);
    FHeadBaseFlowPowerLabel           := CreateFieldLabel(LOwner, LParent,                10,  285,   220, 21);
    FMaximumHydrologicalGradientLabel := CreateFieldLabel(LOwner, LParent,                10,  310,   220, 21);
    FAquiferTransmissivityLabel       := CreateFieldLabel(LOwner, LParent,                10,  335,   220, 21);
    FBoreholeDistanceToRiverLabel     := CreateFieldLabel(LOwner, LParent,                10,  360,   220, 21);
    FMaximumWaterAbstractionLabel     := CreateFieldLabel(LOwner, LParent,                10,  385,   220, 21);
    FParameterk2Label                 := CreateFieldLabel(LOwner, LParent,                10,  410,   220, 21);
    FParameterk3Label                 := CreateFieldLabel(LOwner, LParent,                10,  435,   220, 21);
    FWaterEvaporationAreaLabel        := CreateFieldLabel(LOwner, LParent,                10,  460,   220, 21);
    FMonthlyWaterEvaporationLabel     := CreateFieldLabel(LOwner, LParent,                10,  485,   220, 21);
    FMonthlyUsageFactorLabel          := CreateFieldLabel(LOwner, LParent,                10,  510,   220, 21);

    FGroundWaterNodeNumberEdt         := CreateFieldEdit(FAppModules,   LOwner, LParent,    250, 10,  40 ,  20 , 8, TRUE);
    FGroundWaterNameEdt               := CreateFieldEdit(FAppModules,   LOwner, LParent,    250, 35,  180,  20 , 8, TRUE);
    FGroundWaterDescriptionEdt        := CreateFieldEdit(FAppModules,   LOwner, LParent,    250, 60,  380,  20 , 8, TRUE);
    FRefNodeNumberCbx                 := CreateFieldComboBox(FAppModules, lOwner, lParent,  250, 85,  300,  20, 8, TRUE, csDropDownList);
    FAquiferStorativityEdt            := CreateFieldEdit(FAppModules,   LOwner, LParent,    250, 110,  40 ,  20 , 8, TRUE);
    FAquiferStaticWaterLevelEdt       := CreateFieldEdit(FAppModules,   LOwner, LParent,    250, 135, 40 ,  20 , 8, TRUE);
    FUnsaturatedStorageCapacityEdt    := CreateFieldEdit(FAppModules,   LOwner, LParent,    250, 160, 40 ,  20 , 8, TRUE);
    FInitialUnsaturatedStorageEdt     := CreateFieldEdit(FAppModules,   LOwner, LParent,    250, 185, 40 ,  20 , 8, TRUE);
    FMaximumAquiferRechargeEdt        := CreateFieldEdit(FAppModules,   LOwner, LParent,    250, 210, 40 ,  20 , 8, TRUE);
    FMovingAverageRechargeEdt         := CreateFieldEdit(FAppModules,   LOwner, LParent,    250, 235, 40 ,  20 , 8, TRUE);
    FMaximumBaseFlowRateEdt           := CreateFieldEdit(FAppModules,   LOwner, LParent,    250, 260, 40 ,  20 , 8, TRUE);
    FHeadBaseFlowPowerEdt             := CreateFieldEdit(FAppModules,   LOwner, LParent,    250, 285, 40 ,  20 , 8, TRUE);
    FMaximumHydrologicalGradientEdt   := CreateFieldEdit(FAppModules,   LOwner, LParent,    250, 310, 40 ,  20 , 8, TRUE);
    FAquiferTransmissivityEdt         := CreateFieldEdit(FAppModules,   LOwner, LParent,    250, 335, 40 ,  20 , 8, TRUE);
    FBoreholeDistanceToRiverEdt       := CreateFieldEdit(FAppModules,   LOwner, LParent,    250, 360, 40 ,  20 , 8, TRUE);
    FMaximumWaterAbstractionEdt       := CreateFieldEdit(FAppModules,   LOwner, LParent,    250, 385, 40 ,  20 , 8, TRUE);
    FParameterk2Edt                   := CreateFieldEdit(FAppModules,   LOwner, LParent,    250, 410, 40 ,  20 , 8, TRUE);
    FParameterk3Edt                   := CreateFieldEdit(FAppModules,   LOwner, LParent,    250, 435, 40 ,  20 , 8, TRUE);
    FWaterEvaporationAreaEdt          := CreateFieldEdit(FAppModules,   LOwner, LParent,    250, 460, 40 ,  20 , 8, TRUE);
    FMonthlyWaterEvaporationBtn       := CreateFieldButton(FAppModules, LOwner, LParent,    250, 485, 25 ,  21 , 8,  TRUE, '...');
    FMonthlyUsageFactorBtn            := CreateFieldButton(FAppModules, LOwner, LParent,    250, 510, 25 ,  21 , 8,  TRUE, '...');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterDialog.Initialise: boolean;
const OPNAME = 'TGroundWaterDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterDialog.LanguageHasChanged: boolean;
const OPNAME = 'TGroundWaterDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FGroundWaterNodeNumberLabel.Caption       := FAppModules.Language.GetString('TField.GroundWaterNodeNumber');
    FGroundWaterNameLabel.Caption             := FAppModules.Language.GetString('TField.GroundWaterName');
    FGroundWaterDescriptionLabel.Caption      := FAppModules.Language.GetString('TField.GroundWaterDescription');
    FAquiferStorativityLabel.Caption          := FAppModules.Language.GetString('TField.AquiferStorativity');
    FAquiferStaticWaterLevelLabel.Caption     := FAppModules.Language.GetString('TField.AquiferStaticWaterLevel');
    FUnsaturatedStorageCapacityLabel.Caption  := FAppModules.Language.GetString('TField.UnsaturatedStorageCapacity');
    FInitialUnsaturatedStorageLabel.Caption   := FAppModules.Language.GetString('TField.InitialUnsaturatedStorage');
    FMaximumAquiferRechargeLabel.Caption      := FAppModules.Language.GetString('TField.MaximumDischargeRate');
    FMovingAverageRechargeLabel.Caption       := FAppModules.Language.GetString('TField.MovingAverageRecharge');
    FMaximumBaseFlowRateLabel.Caption         := FAppModules.Language.GetString('TField.MaximumRateOfGroundwaterBaseFlow');
    FHeadBaseFlowPowerLabel.Caption           := FAppModules.Language.GetString('TField.PowerHeadDifferenceBaseFlowEquation');
    FMaximumHydrologicalGradientLabel.Caption := FAppModules.Language.GetString('TField.MaximumHydrologicalGradient');
    FAquiferTransmissivityLabel.Caption       := FAppModules.Language.GetString('TField.AquiferTransmissivity');
    FBoreholeDistanceToRiverLabel.Caption     := FAppModules.Language.GetString('TField.BoreholeDistanceToRiver');
    FMaximumWaterAbstractionLabel.Caption     := FAppModules.Language.GetString('TField.MaximumGroundWaterAbstraction');
    FParameterk2Label.Caption                 := FAppModules.Language.GetString('TField.ParameterK2');
    FParameterk3Label.Caption                 := FAppModules.Language.GetString('TField.ParameterK3');
    FMonthlyWaterEvaporationLabel.Caption     := FAppModules.Language.GetString('TField.MonthlyWaterEvaporation');
    FMonthlyUsageFactorLabel.Caption          := FAppModules.Language.GetString('TField.MonthlyWaterUsageFactors');
    FWaterEvaporationAreaLabel.Caption        := FAppModules.Language.GetString('TField.GroundWaterEvaporationArea');
    FRefNodeNumberCbxLabel.Caption            := FAppModules.Language.GetString('TField.HydrologyNodeNumber');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterDialog.Resize;
const OPNAME = 'TGroundWaterDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterDialog.RestoreColourState;
const OPNAME = 'TGroundWaterDialog.RestoreColourState';
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

end.
