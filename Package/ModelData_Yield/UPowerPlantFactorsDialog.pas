{******************************************************************************}
{*  UNIT      : Contains the class TPowerPlantFactorsDialog.                  *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/11/27                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UPowerPlantFactorsDialog;

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

  TPowerPlantFactorsDialog = class(TAbstractScrollablePanel)
  private
    FFeatureNameLabel        : TLabel;
    FEfficiencyFactorPanel   : TPanel;
    FEfficiencyFactorLabel   : TLabel;
    FEfficiencyPanel         : TPanel;
    FEfficiencyLabel         : TLabel;
    FNetHeadFactorPanel      : TPanel;
    FNetHeadFactorLabel      : TLabel;
    FNetHeadPanel            : TPanel;
    FNetHeadLabel            : TLabel;
    FCombinedEfficiencyLabel : TLabel;
    FCombinedEfficiencyEdit  : TFieldEdit;
    FDesignHeadLabel         : TLabel;
    FDesignHeadEdit          : TFieldEdit;
    FMaxNetHeadLabel         : TLabel;
    FMaxNetHeadEdit          : TFieldEdit;
    FMinNetHeadLabel         : TLabel;
    FMinNetHeadEdit          : TFieldEdit;
    FFactorsGrid             : TFieldStringGrid;

  protected
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property FeatureNameLabel        : TLabel           read FFeatureNameLabel;
    property CombinedEfficiencyEdit  : TFieldEdit       read FCombinedEfficiencyEdit;
    property DesignHeadEdit          : TFieldEdit       read FDesignHeadEdit;
    property MaxNetHeadEdit          : TFieldEdit       read FMaxNetHeadEdit;
    property MinNetHeadEdit          : TFieldEdit       read FMinNetHeadEdit;
    property FactorsGrid             : TFieldStringGrid read FFactorsGrid;
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
{* TPowerPlantFactorsDialog                                                   *}
{******************************************************************************}

procedure TPowerPlantFactorsDialog.CreateMemberObjects;
const OPNAME = 'TPowerPlantFactorsDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    //                                                                                Left  Top  Width Height
    FFeatureNameLabel        := CreateFieldLabel      (lOwner, lParent,  10,  10, 240,  21);
    FFactorsGrid             := CreateFieldStringGrid (FAppModules, lOwner, lParent,  10,  35, 348, 259, 0, TRUE);
    with FFactorsGrid do
    begin
      ScrollBars       := ssNone;
      ColCount         := 5;
      RowCount         := 11;
      FixedCols        := 1;
      FixedRows        := 1;
      DefaultRowHeight := 20;
      DefaultColWidth  := 80;
      ColWidths[0]     := 20;
      RowHeights[0]    := 45;
      DefaultDrawing   := False;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];
    end;
    FEfficiencyFactorPanel   := CreatePanel           (lOwner, lParent,  36,  38,  74,  41, 1);
    FEfficiencyPanel         := CreatePanel           (lOwner, lParent, 117,  38,  74,  41, 2);
    FNetHeadFactorPanel      := CreatePanel           (lOwner, lParent, 198,  38,  74,  41, 3);
    FNetHeadPanel            := CreatePanel           (lOwner, lParent, 279,  38,  74,  41, 4);
    with FEfficiencyFactorPanel do
    begin
      BevelInner := bvNone;
      BevelOuter := bvNone;
      BorderStyle := bsNone;
      BringToFront;
    end;
    with FEfficiencyPanel do
    begin
      BevelInner := bvNone;
      BevelOuter := bvNone;
      BorderStyle := bsNone;
      BringToFront;
    end;
    with FNetHeadFactorPanel do
    begin
      BevelInner := bvNone;
      BevelOuter := bvNone;
      BorderStyle := bsNone;
      BringToFront;
    end;
    with FNetHeadPanel do
    begin
      BevelInner := bvNone;
      BevelOuter := bvNone;
      BorderStyle := bsNone;
      BringToFront;
    end;
    FEfficiencyFactorLabel   := CreateFieldLabel      (lOwner, FEfficiencyFactorPanel,   0,   0,  74,  41);
    FEfficiencyLabel         := CreateFieldLabel      (lOwner,       FEfficiencyPanel,   0,   0,  74,  41);
    FNetHeadFactorLabel      := CreateFieldLabel      (lOwner,    FNetHeadFactorPanel,   0,   0,  74,  41);
    FNetHeadLabel            := CreateFieldLabel      (lOwner,          FNetHeadPanel,   0,   0,  74,  41);
    with FEfficiencyFactorLabel do
    begin
      Align     := alClient;
      Alignment := taCenter;
      WordWrap  := TRUE;
    end;
    with FEfficiencyLabel do
    begin
      Align     := alClient;
      Alignment := taCenter;
      WordWrap  := TRUE;
    end;
    with FNetHeadFactorLabel do
    begin
      Align     := alClient;
      Alignment := taCenter;
      WordWrap  := TRUE;
    end;
    with FNetHeadLabel do
    begin
      Align     := alClient;
      Alignment := taCenter;
      WordWrap  := TRUE;
    end;

    FCombinedEfficiencyLabel := CreateFieldLabel      (lOwner, lParent,  50, 300,  60,  30);
    FDesignHeadLabel         := CreateFieldLabel      (lOwner, lParent, 210, 300,  60,  30);
    FMaxNetHeadLabel         := CreateFieldLabel      (lOwner, lParent, 170, 335, 100,  21);
    FMinNetHeadLabel         := CreateFieldLabel      (lOwner, lParent, 170, 360, 100,  21);
    FCombinedEfficiencyEdit  := CreateFieldEdit       (FAppModules, lOwner, lParent, 115, 305,  80,  21, 5, TRUE);
    FDesignHeadEdit          := CreateFieldEdit       (FAppModules, lOwner, lParent, 275, 305,  80,  21, 6, TRUE);
    FMaxNetHeadEdit          := CreateFieldEdit       (FAppModules, lOwner, lParent, 275, 335,  80,  21, 7, TRUE);
    FMinNetHeadEdit          := CreateFieldEdit       (FAppModules, lOwner, lParent, 275, 360,  80,  21, 8, TRUE);
    FCombinedEfficiencyLabel.WordWrap := TRUE;
    FDesignHeadLabel.WordWrap         := TRUE;
{
}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantFactorsDialog.Resize;
const OPNAME = 'TPowerPlantFactorsDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantFactorsDialog.Initialise: boolean;
const OPNAME = 'TPowerPlantFactorsDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantFactorsDialog.LanguageHasChanged: boolean;
const OPNAME = 'TPowerPlantFactorsDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FEfficiencyFactorLabel.Caption   := FAppModules.Language.GetString('TField.EfficiencyFactor');
    FEfficiencyLabel.Caption         := FAppModules.Language.GetString('NetworkFeatures.Efficiency');
    FNetHeadFactorLabel.Caption      := FAppModules.Language.GetString('TField.NetHeadFactors');
    FNetHeadLabel.Caption            := FAppModules.Language.GetString('NetworkFeatures.NetHead');
    FCombinedEfficiencyLabel.Caption := FAppModules.Language.GetString('TField.PowerEfficiency') + ' :';
    FDesignHeadLabel.Caption         := FAppModules.Language.GetString('TField.DesignHeadDescr') + ' :';
    FMaxNetHeadLabel.Caption         := FAppModules.Language.GetString('TField.MaxNetHead') + ' :';
    FMinNetHeadLabel.Caption         := FAppModules.Language.GetString('TField.MinNetHead') + ' :';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantFactorsDialog.RestoreColourState;
const OPNAME = 'TPowerPlantFactorsDialog.RestoreColourState';
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

procedure TPowerPlantFactorsDialog.AssignHelpContext;
const OPNAME = 'TPowerPlantFactorsDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                    HC_HydropowerPlants);
    SetControlHelpContext(FCombinedEfficiencyEdit, HC_HydropowerPlants);
    SetControlHelpContext(FDesignHeadEdit,         HC_HydropowerPlants);
    SetControlHelpContext(FMaxNetHeadEdit,         HC_HydropowerPlants);
    SetControlHelpContext(FMinNetHeadEdit,         HC_HydropowerPlants);
    SetControlHelpContext(FFactorsGrid,            HC_HydropowerPlants);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
