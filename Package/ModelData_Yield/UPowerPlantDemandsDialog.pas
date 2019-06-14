{******************************************************************************}
{*  UNIT      : Contains the class TPowerPlantDemandsDialog.                  *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/11/27                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UPowerPlantDemandsDialog;

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

  TPowerPlantDemandsDialog = class(TAbstractScrollablePanel)
  private
    FFeatureNameLabel : TLabel;
    FGenerationPanel  : TPanel;
    FGenerationLabel  : TLabel;
    FReleasePanel     : TPanel;
    FReleaseLabel     : TLabel;
    FDemandsGrid      : TFieldStringGrid;

  protected
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property FeatureNameLabel : TLabel            read FFeatureNameLabel;
    property GenerationPanel  : TPanel            read FGenerationPanel;
    property GenerationLabel  : TLabel            read FGenerationLabel;
    property ReleasePanel     : TPanel            read FReleasePanel;
    property ReleaseLabel     : TLabel            read FReleaseLabel;
    property DemandsGrid      : TFieldStringGrid  read FDemandsGrid;
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
{* TPowerPlantDemandsDialog                                                   *}
{******************************************************************************}

procedure TPowerPlantDemandsDialog.CreateMemberObjects;
const OPNAME = 'TPowerPlantDemandsDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    //                                                                                Left  Top  Width Height
    FFeatureNameLabel        := CreateFieldLabel      (lOwner,                lParent,  10,  10, 240,  21);
    FDemandsGrid             := CreateFieldStringGrid (FAppModules, lOwner,                lParent,  10,  40, 246, 322, 0, TRUE);
    with FDemandsGrid do
    begin
      ScrollBars       := ssNone;
      ColCount         := 3;
      RowCount         := 13;
      FixedCols        := 1;
      FixedRows        := 1;
      DefaultRowHeight := 20;
      DefaultColWidth  := 90;
      ColWidths[0]     := 60;
      RowHeights[0]    := 65;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];
    end;

    FGenerationPanel         := CreatePanel           (lOwner, lParent,  74,  44,  86,  61, 1);
    FReleasePanel            := CreatePanel           (lOwner, lParent, 164,  44,  86,  61, 3);
    with FGenerationPanel do
    begin
      BevelInner := bvNone;
      BevelOuter := bvNone;
      BorderStyle := bsNone;
      BringToFront;
    end;
    with FReleasePanel do
    begin
      BevelInner := bvNone;
      BevelOuter := bvNone;
      BorderStyle := bsNone;
      BringToFront;
    end;
    FGenerationLabel         := CreateFieldLabel      (lOwner, FGenerationPanel,   0,   0,  86,  61);
    FReleaseLabel            := CreateFieldLabel      (lOwner,    FReleasePanel,   0,   0,  86,  61);
    with FGenerationLabel do
    begin
      Align     := alClient;
      Alignment := taCenter;
      WordWrap  := TRUE;
    end;
    with FReleaseLabel do
    begin
      Align     := alClient;
      Alignment := taCenter;
      WordWrap  := TRUE;
    end;
{
      Caption = 'Power generation demands'
}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantDemandsDialog.Resize;
const OPNAME = 'TPowerPlantDemandsDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantDemandsDialog.Initialise: boolean;
const OPNAME = 'TPowerPlantDemandsDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantDemandsDialog.LanguageHasChanged: boolean;
const OPNAME = 'TPowerPlantDemandsDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FGenerationLabel.Caption := FAppModules.Language.GetString('TField.MinEnergyGeneratedDescr');
    FReleaseLabel.Caption    := FAppModules.Language.GetString('TField.MinPowerChannelReleaseDescr');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantDemandsDialog.RestoreColourState;
const OPNAME = 'TPowerPlantDemandsDialog.RestoreColourState';
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

procedure TPowerPlantDemandsDialog.AssignHelpContext;
const OPNAME = 'TPowerPlantDemandsDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,         HC_HydropowerPlants);
    SetControlHelpContext(FDemandsGrid, HC_HydropowerPlants);
  except on E: Exception do HandleError(E, OPNAME); end;
end;
end.
