{******************************************************************************}
{*  UNIT      : Contains the class TPumpingFeatureDialog.                     *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/09/03                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UPumpingFeatureDialog;


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

  TPumpingFeatureDialog = class(TAbstractScrollablePanel)
  private
    FFeatureNameLabel        : TLabel;
    FFeatureNameEdit         : TFieldEdit;
    FPumpHeadLabel           : TLabel;
    FPumpHeadEdit            : TFieldEdit;
    FPumpEfficiencyLabel     : TLabel;
    FPumpEfficiencyEdit      : TFieldEdit;
  protected
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    property PumpHeadLabel           : TLabel           read FPumpHeadLabel;
    property PumpHeadEdit            : TFieldEdit       read FPumpHeadEdit;
    property PumpEfficiencyLabel     : TLabel           read FPumpEfficiencyLabel;
    property PumpEfficiencyEdit      : TFieldEdit       read FPumpEfficiencyEdit;
    property FeatureNameEdit         : TFieldEdit       read FFeatureNameEdit; 
  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities;

{******************************************************************************}
{* TPumpingFeatureDialog                                                       *}
{******************************************************************************}

procedure TPumpingFeatureDialog.CreateMemberObjects;
const OPNAME = 'TPumpingFeatureDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    //                                                                        Left  Top Width Height
    FFeatureNameLabel        := CreateFieldLabel             (lOwner, lParent,  10,  10, 140,  21);
    FFeatureNameEdit         := CreateFieldEdit (FAppModules, lOwner, lParent, 120,  10, 180,  21, 0, TRUE);
    FPumpHeadLabel           := CreateFieldLabel             (lOwner, lParent,  10,  35, 140,  21);
    FPumpHeadEdit            := CreateFieldEdit (FAppModules, lOwner, lParent, 120,  35,  60,  21,  0, TRUE);
    FPumpEfficiencyLabel     := CreateFieldLabel             (lOwner, lParent,  10,  60, 140,  21);
    FPumpEfficiencyEdit      := CreateFieldEdit (FAppModules, lOwner, lParent, 120,  60,  60,  21,  1, TRUE);
    FFeatureNameEdit.Enabled := TRUE;
    FFeatureNameEdit.Color   := clBtnFace;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPumpingFeatureDialog.Resize;
const OPNAME = 'TPumpingFeatureDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPumpingFeatureDialog.Initialise: boolean;
const OPNAME = 'TPumpingFeatureDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPumpingFeatureDialog.LanguageHasChanged: boolean;
const OPNAME = 'TPumpingFeatureDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FFeatureNameLabel.Caption        := FAppModules.Language.GetString('NetworkFeatures.FeatureName') + ' :';
    FPumpHeadLabel.Caption           := FAppModules.Language.GetString('TField.PumpingHead') + ' :';
    FPumpEfficiencyLabel.Caption     := FAppModules.Language.GetString('TField.PumpEfficiency') + ' :';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPumpingFeatureDialog.RestoreColourState;
const OPNAME = 'TPumpingFeatureDialog.RestoreColourState';
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

procedure TPumpingFeatureDialog.AssignHelpContext;
const OPNAME = 'TPumpingFeatureDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                HC_PumpingStations);
    SetControlHelpContext(FFeatureNameEdit,    HC_PumpingStations);
    SetControlHelpContext(FPumpHeadEdit,       HC_PumpingStations);
    SetControlHelpContext(FPumpEfficiencyEdit, HC_PumpingStations);
  except on E: Exception do HandleError(E, OPNAME); end;
end;
end.
