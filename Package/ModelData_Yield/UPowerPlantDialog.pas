{******************************************************************************}
{*  UNIT      : Contains the class TPowerPlantDialog.                         *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/11/27                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UPowerPlantDialog;

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

  TPowerPlantDialog = class(TAbstractScrollablePanel)
  private
    FFeatureNameLabel         : TLabel;
    FFeatureNameEdit          : TFieldEdit;
    FUpstreamNodeLabel        : TLabel;
    FUpStreamNodeCbx          : TFieldComboBox;
    FPowerDownstreamNodeLabel : TLabel;
    FPowerDownStreamNodeCbx   : TFieldComboBox;
    FSpillDownstreamNodeLabel : TLabel;
    FSpillDownStreamNodeCbx   : TFieldComboBox;
    FMaxGeneratorLabel        : TLabel;
    FMaxTurbineLabel          : TLabel;
    FHeadLossLabel            : TLabel;
    FDownstreamPlantsLabel    : TLabel;
    FMaxGeneratorEdit         : TFieldEdit;
    FMaxTurbineEdit           : TFieldEdit;
    FPlantExistsChkBox        : TFieldChkBox;
    FHeadLossEdit             : TFieldEdit;
    FDowstreamPlantsCheckLbx  : TFieldCheckListBox;

  protected
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property FeatureNameEdit         : TFieldEdit         read FFeatureNameEdit;
    property UpStreamNodeCbx         : TFieldComboBox     read FUpStreamNodeCbx;
    property PowerDownStreamNodeCbx  : TFieldComboBox     read FPowerDownStreamNodeCbx;
    property SpillDownStreamNodeCbx  : TFieldComboBox     read FSpillDownStreamNodeCbx;
    property MaxGeneratorEdit        : TFieldEdit         read FMaxGeneratorEdit;
    property MaxTurbineEdit          : TFieldEdit         read FMaxTurbineEdit;
    property PlantExistsChkBox       : TFieldChkBox       read FPlantExistsChkBox;
    property HeadLossEdit            : TFieldEdit         read FHeadLossEdit;
    property DowstreamPlantsCheckLbx : TFieldCheckListBox read FDowstreamPlantsCheckLbx;
  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  VCL.Forms,
  UErrorHandlingOperations,
  UControlCreationUtilities;

{******************************************************************************}
{* TPowerPlantDialog                                                          *}
{******************************************************************************}

procedure TPowerPlantDialog.CreateMemberObjects;
const OPNAME = 'TPowerPlantDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    //                                                                  Left  Top  Width Height
    FFeatureNameLabel        := CreateFieldLabel                    (lOwner, lParent,  10,  10, 170,  21);
    FFeatureNameEdit         := CreateFieldEdit        (FAppModules, lOwner, lParent, 185,  10, 200,  21, 0, TRUE);
    FUpstreamNodeLabel       := CreateFieldLabel                    (lOwner, lParent,  10,  38, 170,  26);
    FUpStreamNodeLabel.WordWrap := TRUE;
    FUpStreamNodeCbx         := CreateFieldComboBox    (FAppModules, lOwner, lParent, 185,  40, 280,  21, 1, TRUE, csDropDownList);
    FPowerDownstreamNodeLabel:= CreateFieldLabel                    (lOwner, lParent,  10,  68, 170,  26);
    FPowerDownstreamNodeLabel.WordWrap := TRUE;
    FPowerDownStreamNodeCbx  := CreateFieldComboBox    (FAppModules, lOwner, lParent, 185,  70, 280,  21, 2, TRUE, csDropDownList);
    FSpillDownstreamNodeLabel:= CreateFieldLabel                    (lOwner, lParent,  10,  98, 170,  26);
    FSpillDownstreamNodeLabel.WordWrap := TRUE;
    FSpillDownStreamNodeCbx  := CreateFieldComboBox    (FAppModules, lOwner, lParent, 185, 100, 280,  21, 2, TRUE, csDropDownList);

    FMaxGeneratorLabel       := CreateFieldLabel                    (lOwner, lParent,  10, 130, 170,  21);
    FMaxGeneratorEdit        := CreateFieldEdit        (FAppModules, lOwner, lParent, 185, 130, 200,  21, 1, TRUE);
    FMaxTurbineLabel         := CreateFieldLabel                    (lOwner, lParent,  10, 155, 170,  21);
    FMaxTurbineEdit          := CreateFieldEdit        (FAppModules, lOwner, lParent, 185, 155, 200,  21, 2, TRUE);
    FPlantExistsChkBox       := CreateFieldChkBox      (FAppModules, lOwner, lParent,  10, 180, 188,  21, 3, TRUE, taLeftJustify);
    FHeadLossLabel           := CreateFieldLabel                    (lOwner, lParent,  10, 205, 170,  21);
    FHeadLossEdit            := CreateFieldEdit        (FAppModules, lOwner, lParent, 185, 205, 200,  21, 4, TRUE);
    FDownstreamPlantsLabel   := CreateFieldLabel                    (lOwner, lParent,  10, 230, 170,  21);
    FDowstreamPlantsCheckLbx := CreateFieldCheckListBox(FAppModules, lOwner, lParent, 185, 230, 200, 175, 5, TRUE);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantDialog.Resize;
const OPNAME = 'TPowerPlantDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantDialog.Initialise: boolean;
const OPNAME = 'TPowerPlantDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantDialog.LanguageHasChanged: boolean;
const OPNAME = 'TPowerPlantDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FFeatureNameLabel.Caption      := FAppModules.Language.GetString('TField.PowerPlantNameDescr') + ' :';
    FUpstreamNodeLabel.Caption     := FAppModules.Language.GetString('NetworkFeatures.PowerSpillUpstreamNode') + ' :';
    FPowerDownstreamNodeLabel.Caption := FAppModules.Language.GetString('NetworkFeatures.PowerDownstreamNode') + ' :';
    FSpillDownstreamNodeLabel.Caption := FAppModules.Language.GetString('NetworkFeatures.SpillDownstreamNode') + ' :';
    FMaxGeneratorLabel.Caption     := FAppModules.Language.GetString('TField.MaxCapGenerator') + ' :';
    FMaxTurbineLabel.Caption       := FAppModules.Language.GetString('TField.MaxCapTurbine') + ' :';
    FPlantExistsChkBox.Caption     := FAppModules.Language.GetString('TField.PowerPlantStatus') + ' :';
    FHeadLossLabel.Caption         := FAppModules.Language.GetString('TField.HeadLoss') + ' :';
    FDownstreamPlantsLabel.Caption := FAppModules.Language.GetString('TField.DownStreamPowerChannelNumber') + ' :';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantDialog.RestoreColourState;
const OPNAME = 'TPowerPlantDialog.RestoreColourState';
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

procedure TPowerPlantDialog.AssignHelpContext;
const OPNAME = 'TPowerPlantDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(self,                     HC_HydropowerPlants);
    SetControlHelpContext(FFeatureNameEdit,         HC_HydropowerPlants);
    SetControlHelpContext(FUpStreamNodeCbx,         HC_HydropowerPlants);
    SetControlHelpContext(FPowerDownStreamNodeCbx,  HC_HydropowerPlants);
    SetControlHelpContext(FSpillDownStreamNodeCbx,  HC_HydropowerPlants);
    SetControlHelpContext(FMaxGeneratorEdit,        HC_HydropowerPlants);
    SetControlHelpContext(FHeadLossEdit,            HC_HydropowerPlants);
    SetControlHelpContext(FMaxTurbineEdit,          HC_HydropowerPlants);
    SetControlHelpContext(FPlantExistsChkBox,       HC_HydropowerPlants);
    SetControlHelpContext(FDowstreamPlantsCheckLbx, HC_HydropowerPlants);
  except on E: Exception do HandleError(E, OPNAME); end;
end;
end.

