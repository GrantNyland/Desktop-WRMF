{******************************************************************************}
{*  UNIT      : Contains the class TSoilPropertiesDialog                      *}
{*  AUTHOR    : Maurice Marinus                                               *}
{*  DATE      : 2006/06/26                                                    *)
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit USoilPropertiesDialog;

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

  TSoilPropertiesDialog = class(TAbstractScrollablePanel)
  protected
    FIrrigationBlockUpperZoneReturnFlowLabel            : TLabel;
    FIrrigationBlockUpperZoneReturnFlowEdit             : TFieldEdit;

    FIrrigationBlockLowerZoneReturnFlowLabel            : TLabel;
    FIrrigationBlockLowerZoneReturnFlowEdit             : TFieldEdit;

    FIrrigationBlockUpperZoneSoilMoistureCapacityLabel  : TLabel;
    FIrrigationBlockUpperZoneSoilMoistureCapacityEdit   : TFieldEdit;

    FIrrigationBlockLowerZoneSoilMoistureCapacityLabel  : TLabel;
    FIrrigationBlockLowerZoneSoilMoistureCapacityEdit   : TFieldEdit;

    FIrrigationBlockUpperZoneSoilMoistureTargetLabel    : TLabel;
    FIrrigationBlockUpperZoneSoilMoistureTargetEdit     : TFieldEdit;

    FIrrigationBlockInitialSoilMoistureStorageLabel     : TLabel;
    FIrrigationBlockInitialSoilMoistureStorageEdit      : TFieldEdit;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property IrrigationBlockUpperZoneReturnFlowEdit           : TFieldEdit read FIrrigationBlockUpperZoneReturnFlowEdit;
    property IrrigationBlockLowerZoneReturnFlowEdit           : TFieldEdit read FIrrigationBlockLowerZoneReturnFlowEdit;
    property IrrigationBlockUpperZoneSoilMoistureCapacityEdit : TFieldEdit read FIrrigationBlockUpperZoneSoilMoistureCapacityEdit;
    property IrrigationBlockLowerZoneSoilMoistureCapacityEdit : TFieldEdit read FIrrigationBlockLowerZoneSoilMoistureCapacityEdit;
    property IrrigationBlockUpperZoneSoilMoistureTargetEdit   : TFieldEdit read FIrrigationBlockUpperZoneSoilMoistureTargetEdit;
    property IrrigationBlockInitialSoilMoistureStorageEdit    : TFieldEdit read FIrrigationBlockInitialSoilMoistureStorageEdit;
    property IrrigationBlockCanalTransportLossesEdit          : TFieldEdit read FIrrigationBlockInitialSoilMoistureStorageEdit;
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
{* TSoilPropertiesDialog                                                      *}
{******************************************************************************}

procedure TSoilPropertiesDialog.CreateMemberObjects;
const OPNAME = 'TSoilPropertiesDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;

//                                                                         Left  Top Width Height
    FIrrigationBlockUpperZoneReturnFlowLabel           := CreateFieldLabel(lOwner, lParent, 10,  5,   420, 21);
    FIrrigationBlockUpperZoneReturnFlowEdit            := CreateFieldEdit(FAppModules, lOwner, lParent, 330,  5,   100,  20, 8, TRUE);

    FIrrigationBlockLowerZoneReturnFlowLabel           := CreateFieldLabel(lOwner, lParent, 10,  30,  420, 21);
    FIrrigationBlockLowerZoneReturnFlowEdit            := CreateFieldEdit(FAppModules, lOwner, lParent, 330,  30,  100,  20, 8, TRUE);

    FIrrigationBlockUpperZoneSoilMoistureCapacityLabel := CreateFieldLabel(lOwner, lParent, 10,  55,  420, 21);
    FIrrigationBlockUpperZoneSoilMoistureCapacityEdit  := CreateFieldEdit(FAppModules, lOwner, lParent, 330,  55,  100,  20, 8, TRUE);

    FIrrigationBlockLowerZoneSoilMoistureCapacityLabel := CreateFieldLabel(lOwner, lParent, 10,  80, 420, 21);
    FIrrigationBlockLowerZoneSoilMoistureCapacityEdit  := CreateFieldEdit(FAppModules, lOwner, lParent, 330,  80, 100,  20, 8, TRUE);

    FIrrigationBlockUpperZoneSoilMoistureTargetLabel   := CreateFieldLabel(lOwner, lParent, 10,  105, 420, 21);
    FIrrigationBlockUpperZoneSoilMoistureTargetEdit    := CreateFieldEdit(FAppModules, lOwner, lParent, 330,  105, 100,  20, 8, TRUE);

    FIrrigationBlockInitialSoilMoistureStorageLabel    := CreateFieldLabel(lOwner, lParent, 10,  130, 420, 21);
    FIrrigationBlockInitialSoilMoistureStorageEdit     := CreateFieldEdit(FAppModules, lOwner, lParent, 330,  130, 100,  20, 8, TRUE);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSoilPropertiesDialog.Resize;
const OPNAME = 'TSoilPropertiesDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSoilPropertiesDialog.Initialise: boolean;
const OPNAME = 'TSoilPropertiesDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSoilPropertiesDialog.LanguageHasChanged: boolean;
const OPNAME = 'TSoilPropertiesDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FIrrigationBlockUpperZoneReturnFlowLabel.Caption            := FAppModules.Language.GetString('TField.IrrigationBlockUpperZoneReturnFlow');
    FIrrigationBlockLowerZoneReturnFlowLabel.Caption            := FAppModules.Language.GetString('TField.IrrigationBlockLowerZoneReturnFlow');
    FIrrigationBlockUpperZoneSoilMoistureCapacityLabel.Caption  := FAppModules.Language.GetString('TField.IrrigationBlockUpperZoneSoilMoistureCapacity');
    FIrrigationBlockLowerZoneSoilMoistureCapacityLabel.Caption  := FAppModules.Language.GetString('TField.IrrigationBlockLowerZoneSoilMoistureCapacity');
    FIrrigationBlockUpperZoneSoilMoistureTargetLabel.Caption    := FAppModules.Language.GetString('TField.IrrigationBlockUpperZoneSoilMoistureTarget');
    FIrrigationBlockInitialSoilMoistureStorageLabel.Caption     := FAppModules.Language.GetString('TField.IrrigationBlockInitialSoilMoistureStorage');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSoilPropertiesDialog.RestoreColourState;
const OPNAME = 'TSoilPropertiesDialog.RestoreColourState';
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

procedure TSoilPropertiesDialog.AssignHelpContext;
const OPNAME = 'TSoilPropertiesDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                                              HC_Irrigation);
    SetControlHelpContext(FIrrigationBlockUpperZoneReturnFlowEdit,           HC_Irrigation);
    SetControlHelpContext(FIrrigationBlockLowerZoneReturnFlowEdit,           HC_Irrigation);
    SetControlHelpContext(FIrrigationBlockUpperZoneSoilMoistureCapacityEdit, HC_Irrigation);
    SetControlHelpContext(FIrrigationBlockLowerZoneSoilMoistureCapacityEdit, HC_Irrigation);
    SetControlHelpContext(FIrrigationBlockUpperZoneSoilMoistureTargetEdit,   HC_Irrigation);
    SetControlHelpContext(FIrrigationBlockInitialSoilMoistureStorageEdit,    HC_Irrigation);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
