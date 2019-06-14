{******************************************************************************}
{*  UNIT      : Contains the class TLossChannelDialog.                        *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/09/03                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit ULossChannelDialog;

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

  TLossChannelDialog = class(TAbstractScrollablePanel)
  private
    FFeatureNameLabel    : TLabel;
    FFeatureNameEdit     : TFieldEdit;
    FReferenceNodeLabel  : TLabel;
    FReferenceNodeCbx    : TFieldComboBox;
    FLossProportionLabel : TLabel;
    FLossProportionGrid  : TFieldStringGrid;
  protected
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    property FeatureNameEdit     : TFieldEdit       read FFeatureNameEdit;
    property ReferenceNodeLabel  : TLabel           read FReferenceNodeLabel;
    property ReferenceNodeCbx    : TFieldComboBox   read FReferenceNodeCbx;
    property LossProportionLabel : TLabel           read FLossProportionLabel;
    property LossProportionGrid  : TFieldStringGrid read FLossProportionGrid;
  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities;

{******************************************************************************}
{* TLossChannelDialog                                                         *}
{******************************************************************************}

procedure TLossChannelDialog.CreateMemberObjects;
const OPNAME = 'TLossChannelDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    //                                                                         Left  Top  Width Height
    FFeatureNameLabel    := CreateFieldLabel                  (lOwner, lParent,  10, 10, 140, 21);
    FFeatureNameEdit     := CreateFieldEdit      (FAppModules, lOwner, lParent, 160, 10, 180, 21, 0, TRUE);
    FReferenceNodeLabel  := CreateFieldLabel                  (lOwner, lParent,  10, 35, 140,  21);
    FReferenceNodeCbx    := CreateFieldComboBox  (FAppModules, lOwner, lParent, 160, 35, 160,  21, 1, TRUE, csDropDownList);
    FLossProportionLabel := CreateFieldLabel                  (lOwner, lParent,  10, 60, 140,  26);
    FLossProportionGrid  := CreateFieldStringGrid(FAppModules, lOwner, lParent, 160, 60, 160, 267, 2, TRUE);
//    FFeatureNameEdit.IsEnabled := FALSE;
//    FFeatureNameEdit.Color   := clBtnFace;
    with FLossProportionLabel do
    begin
      WordWrap  := TRUE;
    end;
    with FLossProportionGrid do
    begin
      ScrollBars       := ssNone;
      ColCount         := 2;
      RowCount         := 12;
      FixedCols        := 1;
      FixedRows        := 0;
      DefaultRowHeight := 21;
      DefaultColWidth  := 95;
      ColWidths[0]     := 60;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLossChannelDialog.Resize;
const OPNAME = 'TLossChannelDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLossChannelDialog.Initialise: boolean;
const OPNAME = 'TLossChannelDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLossChannelDialog.LanguageHasChanged: boolean;
const OPNAME = 'TLossChannelDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FFeatureNameLabel.Caption    := FAppModules.Language.GetString('NetworkFeatures.FeatureName') + ' :';
    FReferenceNodeLabel.Caption  := FAppModules.Language.GetString('TField.Reference') + ' :';
    FLossProportionLabel.Caption := FAppModules.Language.GetString('TField.WaterLoss') + ' :';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLossChannelDialog.RestoreColourState;
const OPNAME = 'TLossChannelDialog.RestoreColourState';
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

procedure TLossChannelDialog.AssignHelpContext;
const OPNAME = 'TLossChannelDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                HC_SystemLosses);
    SetControlHelpContext(FFeatureNameEdit,    HC_SystemLosses);
    SetControlHelpContext(FReferenceNodeCbx,   HC_SystemLosses);
    SetControlHelpContext(FLossProportionGrid, HC_SystemLosses);
  except on E: Exception do HandleError(E, OPNAME); end;
end;


end.
