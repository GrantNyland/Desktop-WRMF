{******************************************************************************}
{*  UNIT      : Contains the class TIrrBlockIrrigatedAreaDialog.                    *}
{*  AUTHOR    : Maurice Marinus                                               *}
{*  DATE      : 2006/06/23                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UIrrBlockIrrigatedAreaDialog;

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

  TIrrBlockIrrigatedAreaDialog = class(TAbstractScrollablePanel)
  protected
    //________________________________________________________Properties________________________________________
    FgboxIrrigatedArea                           : TGroupBox;

   // FMaxWaterAllocationLabel                     : TLabel;
   // FMaxWaterAllocationEdit                      : TFieldEdit;

    FIrrigationSupplyCapacityLabel               : TLabel;
    FIrrigationSupplyCapacityEdit                : TFieldEdit;
   {
    FEfficiencyFactorLabel                       : TLabel;
    FEfficiencyFactorEdit                        : TFieldEdit;

    FAllocatedIrrigationAreaLabel                : TLabel;
    FAllocatedIrrigationAreaEdit                 : TFieldEdit;  }

    //________________________________________________________Curtailment________________________________________
    FgboxCurtailment                    : TGroupBox;
    //FDroughtApplicableChkBox            : TFieldChkBox;
    FCurtailIrrAbstractionChkBox        : TFieldChkBox;

    //________________________________________________________Soil________________________________________
    FgboxSoil                                           : TGroupBox;
 {   FIrrigationBlockUpperZoneReturnFlowLabel            : TLabel;
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
      }
    FlblUpperSoilOutflow                                : TLabel;
    FedtUpperSoilOutflow                                : TFieldEdit;

    FlblMultiplicationFactor                            : TLabel;
    FedtMultiplicationFactor                            : TFieldEdit;

    FlblMaxUpperZoneMoisture                            : TLabel;
    FedtMaxUpperZoneMoisture                            : TFieldEdit;

    FlblMinUpperZoneMoisture                            : TLabel;
    FedtMinUpperZoneMoisture                            : TFieldEdit;

    //________________________________________________________ReturnFlow________________________________________
    FgboxReturnFlow                                     : TGroupBox;
   // FCanalTransportLossLabel                            : TLabel;
   // FCanalTransportLossEdit                             : TFieldEdit;
    FlblCanalSeepageLoss                                : TLabel;
    FedtCanalSeepageLoss                                : TFieldEdit;
    FlblCanalTransmissionLoss                           : TLabel;
    FedtCanalTransmissionLoss                           : TFieldEdit;
  //  FIrrigationBlockReturnFlowLossLabel                 : TLabel;
 //   FIrrigationBlockReturnFlowLossEdit                  : TFieldEdit;
 //   FIrrigationBlockReturnFlowFactorLabel               : TLabel;
 //   FIrrigationBlockReturnFlowFactorEdit                : TFieldEdit;

    procedure CreateMemberObjects;        override;
    procedure AssignHelpContext;          override;
  public
    procedure Resize;                     override;
    procedure RestoreColourState;         override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean;         override;

    //________________________________________________________Properties________________________________________
 //   property AllocatedIrrigationAreaEdit  : TFieldEdit   read FAllocatedIrrigationAreaEdit;
    property IrrigationSupplyCapacityLabel               : TLabel read  FIrrigationSupplyCapacityLabel;
    property IrrigationSupplyCapacityEdit : TFieldEdit   read FIrrigationSupplyCapacityEdit;
  //  property MaxWaterAllocationEdit       : TFieldEdit   read FMaxWaterAllocationEdit;
  //  property EfficiencyFactorEdit         : TFieldEdit   read FEfficiencyFactorEdit;

    //________________________________________________________Curtailment________________________________________
    //property DroughtApplicableChkBox      : TFieldChkBox read FDroughtApplicableChkBox;
    property CurtailIrrAbstractionChkBox  : TFieldChkBox read FCurtailIrrAbstractionChkBox;

    //________________________________________________________Soil________________________________________
   { property IrrigationBlockUpperZoneReturnFlowEdit           : TFieldEdit read FIrrigationBlockUpperZoneReturnFlowEdit;
    property IrrigationBlockLowerZoneReturnFlowEdit           : TFieldEdit read FIrrigationBlockLowerZoneReturnFlowEdit;
    property IrrigationBlockUpperZoneSoilMoistureCapacityEdit : TFieldEdit read FIrrigationBlockUpperZoneSoilMoistureCapacityEdit;
    property IrrigationBlockLowerZoneSoilMoistureCapacityEdit : TFieldEdit read FIrrigationBlockLowerZoneSoilMoistureCapacityEdit;
    property IrrigationBlockUpperZoneSoilMoistureTargetEdit   : TFieldEdit read FIrrigationBlockUpperZoneSoilMoistureTargetEdit;
    property IrrigationBlockInitialSoilMoistureStorageEdit    : TFieldEdit read FIrrigationBlockInitialSoilMoistureStorageEdit;  }
    property lblUpperSoilOutflow                              : TLabel     read FlblUpperSoilOutflow;
    property edtUpperSoilOutflow                              : TFieldEdit read FedtUpperSoilOutflow;
    property lblMultiplicationFactor                          : TLabel     read FlblMultiplicationFactor;
    property edtMultiplicationFactor                          : TFieldEdit read FedtMultiplicationFactor;

    property lblMaxUpperZoneMoisture                          : TLabel     read FlblMaxUpperZoneMoisture;
    property edtMaxUpperZoneMoisture                          : TFieldEdit read FedtMaxUpperZoneMoisture;
    property lblMinUpperZoneMoisture                          : TLabel     read FlblMinUpperZoneMoisture;
    property edtMinUpperZoneMoisture                          : TFieldEdit read FedtMinUpperZoneMoisture;

    //________________________________________________________ReturnFlow________________________________________
   // property CanalTransportLossEdit                           : TFieldEdit   read FCanalTransportLossEdit;
    property lblCanalSeepageLoss                              : TLabel       read FlblCanalSeepageLoss;

    property edtCanalSeepageLoss                              : TFieldEdit   read FedtCanalSeepageLoss;
    property lblCanalTransmissionLoss                         : TLabel       read FlblCanalTransmissionLoss;
    property edtCanalTransmissionLoss                         : TFieldEdit   read FedtCanalTransmissionLoss;
  //  property ReturnFlowLossEdit                               : TFieldEdit   read FIrrigationBlockReturnFlowLossEdit;
   // property ReturnFlowFactorEdit                             : TFieldEdit   read FIrrigationBlockReturnFlowFactorEdit;
  end;

implementation

uses
  VCL.Grids,
  SysUtils,
  VCL.Forms,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities;

const
  C_ControlBorder  = 10;
  C_LabelOffset    = 3;

{******************************************************************************}
{* TIrrBlockIrrigatedAreaDialog                                                     *}
{******************************************************************************}

procedure TIrrBlockIrrigatedAreaDialog.CreateMemberObjects;
const OPNAME = 'TIrrBlockIrrigatedAreaDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    //________________________________________________________Properties________________________________________
    FgboxIrrigatedArea                   := TGroupBox.Create(lOwner);
    FgboxIrrigatedArea.Parent            := lParent;
    FgboxIrrigatedArea.Align             := alTop;
    FgboxIrrigatedArea.Height            := 45;
   // lParent                              := FgboxIrrigatedArea;

  //  FAllocatedIrrigationAreaLabel       := CreateFieldLabel(lOwner, FgboxIrrigatedArea, 10,  15,  340, 21);
  //  FAllocatedIrrigationAreaEdit        := CreateFieldEdit(FAppModules, lOwner, FgboxIrrigatedArea, 330,  15,  100,  20, 8, TRUE);

    FIrrigationSupplyCapacityLabel      := CreateFieldLabel(lOwner, FgboxIrrigatedArea, 10,  15,  340, 21);
    FIrrigationSupplyCapacityEdit       := CreateFieldEdit(FAppModules, lOwner, FgboxIrrigatedArea, 330,  15,  100,  20, 8, TRUE);

 //   FEfficiencyFactorLabel              := CreateFieldLabel(lOwner, lParent, 10,  65,  340, 21);
 //   FEfficiencyFactorEdit               := CreateFieldEdit(FAppModules, lOwner, FgboxIrrigatedArea, 330,  65,  100,  20, 8, TRUE);  }

   // FMaxWaterAllocationLabel            := CreateFieldLabel(lOwner, FgboxIrrigatedArea, 10,  90,   340, 21);
  //  FMaxWaterAllocationEdit             := CreateFieldEdit(FAppModules, lOwner, FgboxIrrigatedArea, 330,  90,   100,  20, 8, TRUE);

    //________________________________________________________Curtailment________________________________________
    FgboxCurtailment                   := TGroupBox.Create(lOwner);
    FgboxCurtailment.Parent            := lParent;
    FgboxCurtailment.Align             := alTop;
    FgboxCurtailment.Height            := 45;
  //  lParent                            := FgboxCurtailment;

    //FDroughtApplicableChkBox           := CreateFieldChkBox(FAppModules, lOwner, FgboxCurtailment,   8,  15, 338, 17, 9,  TRUE, taLeftJustify);
    FCurtailIrrAbstractionChkBox       := CreateFieldChkBox  (FAppModules, lOwner, FgboxCurtailment, 8,  15, 338, 17, 9, TRUE, taLeftJustify);

    //________________________________________________________Soil________________________________________
    FgboxSoil                                          := TGroupBox.Create(lOwner);
    FgboxSoil.Parent                                   := lParent;
    FgboxSoil.Align                                    := alTop;
    FgboxSoil.Height                                   := 115;
    //lParent                                            := FgboxSoil;

    FlblUpperSoilOutflow                               := CreateFieldLabel(lOwner, FgboxSoil, 10,  15,  420, 21);
    FedtUpperSoilOutflow                               := CreateFieldEdit(FAppModules, lOwner, FgboxSoil, 330,  15,  100,  20, 8, TRUE);

    FlblMultiplicationFactor                           := CreateFieldLabel(lOwner, FgboxSoil, 10,  40,  420, 21);
    FedtMultiplicationFactor                           := CreateFieldEdit(FAppModules, lOwner, FgboxSoil, 330,  40,  100,  20, 8, TRUE);


  {
    FIrrigationBlockUpperZoneReturnFlowLabel           := CreateFieldLabel(lOwner, FgboxSoil, 10,  65,   420, 21);
    FIrrigationBlockUpperZoneReturnFlowEdit            := CreateFieldEdit(FAppModules, lOwner, FgboxSoil, 330,  65,   100,  20, 8, TRUE);

    FIrrigationBlockLowerZoneReturnFlowLabel           := CreateFieldLabel(lOwner, FgboxSoil, 10,  90,  420, 21);
    FIrrigationBlockLowerZoneReturnFlowEdit            := CreateFieldEdit(FAppModules, lOwner, FgboxSoil, 330,  90,  100,  20, 8, TRUE);



    FIrrigationBlockUpperZoneSoilMoistureCapacityLabel := CreateFieldLabel(lOwner, FgboxSoil, 10,  115,  420, 21);
    FIrrigationBlockUpperZoneSoilMoistureCapacityEdit  := CreateFieldEdit(FAppModules, lOwner, FgboxSoil, 330,  115,  100,  20, 8, TRUE);

    FIrrigationBlockLowerZoneSoilMoistureCapacityLabel := CreateFieldLabel(lOwner, FgboxSoil, 10,  140, 420, 21);
    FIrrigationBlockLowerZoneSoilMoistureCapacityEdit  := CreateFieldEdit(FAppModules, lOwner, FgboxSoil, 330,  140, 100,  20, 8, TRUE);

    FIrrigationBlockUpperZoneSoilMoistureTargetLabel   := CreateFieldLabel(lOwner, FgboxSoil, 10,  165, 420, 21);
    FIrrigationBlockUpperZoneSoilMoistureTargetEdit    := CreateFieldEdit(FAppModules, lOwner, FgboxSoil, 330,  165, 100,  20, 8, TRUE);

    FIrrigationBlockInitialSoilMoistureStorageLabel    := CreateFieldLabel(lOwner, FgboxSoil, 10,  190, 420, 21);
    FIrrigationBlockInitialSoilMoistureStorageEdit     := CreateFieldEdit(FAppModules, lOwner, FgboxSoil, 330,  190, 100,  20, 8, TRUE);
     }
    FlblMaxUpperZoneMoisture                           := CreateFieldLabel(lOwner, FgboxSoil, 10,  65, 420, 21);
    FedtMaxUpperZoneMoisture                           := CreateFieldEdit(FAppModules, lOwner, FgboxSoil, 330,  65, 100,  20, 8, TRUE);

    FlblMinUpperZoneMoisture                           := CreateFieldLabel(lOwner, FgboxSoil, 10,  90, 420, 21);
    FedtMinUpperZoneMoisture                           := CreateFieldEdit(FAppModules, lOwner, FgboxSoil, 330,  90, 100,  20, 8, TRUE);


    //________________________________________________________ReturnFlow________________________________________
    FgboxReturnFlow                                          := TGroupBox.Create(lOwner);
    FgboxReturnFlow.Parent                                   := lParent;
    FgboxReturnFlow.Align                                    := alTop;
    FgboxReturnFlow.Height                                   := 70;
    //lParent                                                  := FgboxReturnFlow;


   // FCanalTransportLossLabel                                 := CreateFieldLabel(lOwner, FgboxReturnFlow, 10,  15,  340, 21);
    //FCanalTransportLossEdit                                  := CreateFieldEdit(FAppModules, lOwner, FgboxReturnFlow, 330,  15,  100,  20, 8, TRUE);
    FlblCanalSeepageLoss                                     := CreateFieldLabel(lOwner, FgboxReturnFlow, 10,  15,   420, 21);
    FedtCanalSeepageLoss                                     := CreateFieldEdit(FAppModules, lOwner, FgboxReturnFlow, 330,  15,   100,  20, 8, TRUE);
    FlblCanalTransmissionLoss                                := CreateFieldLabel(lOwner, FgboxReturnFlow, 10,  40,   420, 21);
    FedtCanalTransmissionLoss                                := CreateFieldEdit(FAppModules, lOwner, FgboxReturnFlow, 330,  40,   100,  20, 8, TRUE);
 //   FIrrigationBlockReturnFlowLossLabel                      := CreateFieldLabel(lOwner, FgboxReturnFlow,  10,  90, 420, 21);
  //  FIrrigationBlockReturnFlowLossEdit                       := CreateFieldEdit(FAppModules, lOwner, FgboxReturnFlow, 330,  90, 100, 21, 6,   TRUE);
 //   FIrrigationBlockReturnFlowFactorLabel                    := CreateFieldLabel(lOwner, FgboxReturnFlow,  10,  115, 420, 21);
 //   FIrrigationBlockReturnFlowFactorEdit                     := CreateFieldEdit(FAppModules, lOwner, FgboxReturnFlow, 330,  115, 100, 21, 6,   TRUE);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockIrrigatedAreaDialog.Resize;
const OPNAME = 'TIrrBlockIrrigatedAreaDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockIrrigatedAreaDialog.Initialise: boolean;
const OPNAME = 'TIrrBlockIrrigatedAreaDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockIrrigatedAreaDialog.LanguageHasChanged: boolean;
const OPNAME = 'TIrrBlockIrrigatedAreaDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    //________________________________________________________Properties________________________________________
    FgboxIrrigatedArea.Caption                   := 'Irrigated Area Properties';
    FIrrigationSupplyCapacityLabel.Caption       := FAppModules.Language.GetString('TField.IrrigationSupplyCapacity')+':';
   // FMaxWaterAllocationLabel.Caption             := FAppModules.Language.GetString('TField.IrrigationBlockMaxWaterAllocation')+':';
    //FAllocatedIrrigationAreaLabel.Caption        := FAppModules.Language.GetString('TField.IrrigationBlockAllocatedIrrigationArea')+':';
   // FEfficiencyFactorLabel.Caption               := FAppModules.Language.GetString('TField.IrrigationBlockEfficiencyFactor')+':';

    //________________________________________________________Curtailment________________________________________
    FgboxCurtailment.Caption                     := 'Curtailment';
    //FDroughtApplicableChkBox.Caption             := FAppModules.Language.GetString('TField.IrrigationBlockDroughtApplicable')+':';
    FCurtailIrrAbstractionChkBox.Caption            := 'Curtail Abstraction ?';

    //________________________________________________________Soil________________________________________
    FgboxSoil.Caption := 'Soil Properties';
 {   FIrrigationBlockUpperZoneReturnFlowLabel.Caption            := FAppModules.Language.GetString('TField.IrrigationBlockUpperZoneReturnFlow')+':';
    FIrrigationBlockLowerZoneReturnFlowLabel.Caption            := FAppModules.Language.GetString('TField.IrrigationBlockLowerZoneReturnFlow')+':';
    FIrrigationBlockUpperZoneSoilMoistureCapacityLabel.Caption  := FAppModules.Language.GetString('TField.IrrigationBlockUpperZoneSoilMoistureCapacity')+':';
    FIrrigationBlockLowerZoneSoilMoistureCapacityLabel.Caption  := FAppModules.Language.GetString('TField.IrrigationBlockLowerZoneSoilMoistureCapacity')+':';
    FIrrigationBlockUpperZoneSoilMoistureTargetLabel.Caption    := FAppModules.Language.GetString('TField.IrrigationBlockUpperZoneSoilMoistureTarget')+':';
    FIrrigationBlockInitialSoilMoistureStorageLabel.Caption     := FAppModules.Language.GetString('TField.IrrigationBlockInitialSoilMoistureStorage')+':';
    }
    FlblUpperSoilOutflow.Caption := 'Upper Soil Outflow' +':';
    FlblMaxUpperZoneMoisture.Caption  := 'Max Upper Zone Moisture' +':';
    FlblMinUpperZoneMoisture.Caption  := 'Min Upper Zone Moisture' +':';
    FlblMultiplicationFactor.Caption := 'Multiplication Factor:';
    //________________________________________________________ReturnFlow________________________________________
    FgboxReturnFlow.Caption                       := 'Channels Details';
   // FCanalTransportLossLabel.Caption              := FAppModules.Language.GetString('TField.IrrigationBlockCanalTransportLoss')+':';
    FlblCanalSeepageLoss.Caption                  := 'Canal Seepage Loss'+':';
    FlblCanalTransmissionLoss.Caption             := 'Canal Transmission Loss'+':';
  //  FIrrigationBlockReturnFlowLossLabel.Caption   := FAppModules.Language.GetString('TField.IrrigationBlockReturnFlowLoss')+':';
   // FIrrigationBlockReturnFlowFactorLabel.Caption := FAppModules.Language.GetString('TField.IrrigationBlockReturnFlowFactor')+':';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockIrrigatedAreaDialog.RestoreColourState;
const OPNAME = 'TIrrBlockIrrigatedAreaDialog.RestoreColourState';
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

procedure TIrrBlockIrrigatedAreaDialog.AssignHelpContext;
const OPNAME = 'TIrrBlockIrrigatedAreaDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                         HC_Irrigation);
    //SetControlHelpContext(FCanalTransportLossEdit,      HC_Irrigation);
   // SetControlHelpContext(FEfficiencyFactorEdit,        HC_Irrigation);
  //  SetControlHelpContext(FAllocatedIrrigationAreaEdit, HC_Irrigation);
   // SetControlHelpContext(FDroughtApplicableChkBox,     HC_Irrigation);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
