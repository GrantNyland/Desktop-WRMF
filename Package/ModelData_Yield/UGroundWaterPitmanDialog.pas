unit UGroundWaterPitmanDialog;

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
  TGroundWaterPitmanDialog = class(TAbstractScrollablePanel)
  private
  protected
    FSoilMoistureCapacityLabel         : TLabel;
    FSoilMoistureCapacityEdt           : TFieldEdit;
    FSoilMoistureStorageCapacityLabel  : TLabel;
    FSoilMoistureStorageCapacityEdt    : TFieldEdit;
    FSoilMoistureFlowStateLabel        : TLabel;
    FSoilMoistureFlowStateEdt          : TFieldEdit;
    FSoilMoistureFlowEquationLabel     : TLabel;
    FSoilMoistureFlowEquationEdt       : TFieldEdit;
    FMaximumGroundWaterFlowLabel       : TLabel;
    FMaximumGroundWaterFlowEdt         : TFieldEdit;
    FSoilMoistureRechargeEquationLabel : TLabel;
    FSoilMoistureRechargeEquationEdt   : TFieldEdit;
    FGroundWaterFlowLabel              : TLabel;
    FGroundWaterFlowEdt                : TFieldEdit;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;        

    property SoilMoistureCapacityEdt         : TFieldEdit     read     FSoilMoistureCapacityEdt;
    property SoilMoistureStorageCapacityEdt  : TFieldEdit     read     FSoilMoistureStorageCapacityEdt;
    property SoilMoistureFlowStateEdt        : TFieldEdit     read     FSoilMoistureFlowStateEdt;
    property SoilMoistureFlowEquationEdt     : TFieldEdit     read     FSoilMoistureFlowEquationEdt;
    property MaximumGroundWaterFlowEdt       : TFieldEdit     read     FMaximumGroundWaterFlowEdt;
    property SoilMoistureRechargeEquationEdt : TFieldEdit     read     FSoilMoistureRechargeEquationEdt;
    property GroundWaterFlowEdt              : TFieldEdit     read     FGroundWaterFlowEdt;
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

{ TGroundWaterPitmanDialog }

procedure TGroundWaterPitmanDialog.AssignHelpContext;
const OPNAME = 'TGroundWaterPitmanDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                  HC_WaterResourcesYieldModel);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWaterPitmanDialog.CreateMemberObjects;
const OPNAME = 'TGroundWaterPitmanDialog.CreateMemberObjects';
var
  LOwner  : TComponent;
  LParent : TWinControl;
begin
  inherited;
  try
    LOwner  := ControlsOwner;
    LParent := ControlsParent;
//                                                                                      Left  Top Width Height
    FSoilMoistureCapacityLabel         := CreateFieldLabel(LOwner, LParent,                10,  10,  300, 21);
    FSoilMoistureStorageCapacityLabel  := CreateFieldLabel(LOwner, LParent,                10,  35,  300, 21);
    FSoilMoistureFlowStateLabel        := CreateFieldLabel(LOwner, LParent,                10,  60,  300, 21);
    FSoilMoistureFlowEquationLabel     := CreateFieldLabel(LOwner, LParent,                10,  85,  300, 21);
    FMaximumGroundWaterFlowLabel       := CreateFieldLabel(LOwner, LParent,                10,  110, 300, 21);
    FSoilMoistureRechargeEquationLabel := CreateFieldLabel(LOwner, LParent,                10,  135, 300, 21);
    FGroundWaterFlowLabel              := CreateFieldLabel(LOwner, LParent,                10,  160, 300, 21);

    FSoilMoistureCapacityEdt         := CreateFieldEdit(FAppModules,   LOwner, LParent,    330, 10,  40 ,  20 , 8, TRUE);
    FSoilMoistureStorageCapacityEdt  := CreateFieldEdit(FAppModules,   LOwner, LParent,    330, 35,  40,  20 , 8, TRUE);
    FSoilMoistureFlowStateEdt        := CreateFieldEdit(FAppModules,   LOwner, LParent,    330, 60,  40,  20 , 8, TRUE);
    FSoilMoistureFlowEquationEdt     := CreateFieldEdit(FAppModules,   LOwner, LParent,    330, 85,  40 ,  20 , 8, TRUE);
    FMaximumGroundWaterFlowEdt       := CreateFieldEdit(FAppModules,   LOwner, LParent,    330, 110, 40 ,  20 , 8, TRUE);
    FSoilMoistureRechargeEquationEdt := CreateFieldEdit(FAppModules,   LOwner, LParent,    330, 135, 40 ,  20 , 8, TRUE);
    FGroundWaterFlowEdt              := CreateFieldEdit(FAppModules,   LOwner, LParent,    330, 160, 40 ,  20 , 8, TRUE);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterPitmanDialog.Initialise: boolean;
const OPNAME = 'TGroundWaterPitmanDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterPitmanDialog.LanguageHasChanged: boolean;
const OPNAME = 'TGroundWaterPitmanDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FSoilMoistureCapacityLabel.Caption         := FAppModules.Language.GetString('TField.PitmanSoilMoistureCapacity');
    FSoilMoistureStorageCapacityLabel.Caption  := FAppModules.Language.GetString('TField.PitmanSoilMoistureStorageCapacity');
    FSoilMoistureFlowStateLabel.Caption        := FAppModules.Language.GetString('TField.PitmansoilMoistureFlowState');
    FSoilMoistureFlowEquationLabel.Caption     := FAppModules.Language.GetString('TField.PitmanSoilMoistureFlowEquation');
    FMaximumGroundWaterFlowLabel.Caption       := FAppModules.Language.GetString('TField.PitmanMaximumGroundwaterFlow');
    FSoilMoistureRechargeEquationLabel.Caption := FAppModules.Language.GetString('TField.PitmanSoilMoistureRechargeEquation');
    FGroundWaterFlowLabel.Caption              := FAppModules.Language.GetString('TField.PitmanGroundwaterFlow');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterPitmanDialog.Resize;
const OPNAME = 'TGroundWaterPitmanDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterPitmanDialog.RestoreColourState;
const OPNAME = 'TGroundWaterPitmanDialog.RestoreColourState';
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
