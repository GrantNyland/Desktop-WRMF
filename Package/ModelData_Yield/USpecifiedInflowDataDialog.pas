{******************************************************************************}
{*  UNIT      : Contains the class TSpecifiedInflowDataDialog.                *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/12/01                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit USpecifiedInflowDataDialog;

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
  UDataComponent,
  UDataEditComponent;

type

  TSpecifiedInflowDataDialog = class(TAbstractScrollablePanel)
  private
    FFeatureNameLabel        : TLabel;
    FFeatureNameEdit         : TFieldEdit;
    FFileNameLabel           : TLabel;
    FFileNameComboBox        : TFieldComboBox;
    FInflowFileNameBtn       : TFieldButton;
    FInflowFileImportedLabel : TLabel;
    FInflowFileNameGridBtn   : TFieldBitBtn;
    FInflowFileNameGraphBtn  : TFieldBitBtn;

  protected
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property FeatureNameEdit         : TFieldEdit     read FFeatureNameEdit;
    property FileNameComboBox        : TFieldComboBox read FFileNameComboBox;
    property InflowFileNameBtn       : TFieldButton   read FInflowFileNameBtn;
    property InflowFileImportedLabel : TLabel         read FInflowFileImportedLabel;
    property InflowFileNameGridBtn   : TFieldBitBtn   read FInflowFileNameGridBtn;
    property InflowFileNameGraphBtn  : TFieldBitBtn   read FInflowFileNameGraphBtn;
  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities;

{******************************************************************************}
{* TSpecifiedInflowDataDialog                                                 *}
{******************************************************************************}

procedure TSpecifiedInflowDataDialog.CreateMemberObjects;
const OPNAME = 'TSpecifiedInflowDataDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    //                                                                Left  Top  Width Height
    FFeatureNameLabel        := CreateFieldLabel(lOwner, lParent,  10,  10, 130,  21);
    FFeatureNameEdit         := CreateFieldEdit(FAppModules,     lOwner, lParent, 140,  10, 160,  21, 0, TRUE);
    FFileNameLabel           := CreateFieldLabel(lOwner, lParent,  10,  35, 130,  21);
    FFileNameComboBox        := CreateFieldComboBox(FAppModules, lOwner, lParent, 140,  35, 280,  21, 0, TRUE,csDropDown);
    FInflowFileNameBtn       := CreateFieldButton(FAppModules, lOwner, lParent, 425, 35, 25, 21, 0, TRUE, '...');
    FInflowFileNameGridBtn   := CreateFieldBitButton(FAppModules, lOwner, lParent, 455, 35,  25, 21, 8, TRUE, 'VIEWDATAGRID');
    FInflowFileNameGraphBtn  := CreateFieldBitButton(FAppModules, lOwner, lParent, 485, 35,  25, 21, 8, TRUE, 'VIEWDATAGRAPH');

    FInflowFileImportedLabel := CreateFieldLabel(lOwner, lParent, 515, 35, 500, 21);
    FInflowFileImportedLabel.Font.Color := clRed;
    with FInflowFileNameBtn do
    begin
      Font.Name  := 'Arial';
      Font.Size  := 10;
      Font.Style := [fsBold];
      ParentFont := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedInflowDataDialog.Resize;
const OPNAME = 'TSpecifiedInflowDataDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSpecifiedInflowDataDialog.Initialise: boolean;
const OPNAME = 'TSpecifiedInflowDataDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
    FInflowFileNameGridBtn.Enabled := False;
    FInflowFileNameGraphBtn.Enabled := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSpecifiedInflowDataDialog.LanguageHasChanged: boolean;
const OPNAME = 'TSpecifiedInflowDataDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FFeatureNameLabel.Caption    := FAppModules.Language.GetString('NetworkFeatures.FeatureName') + ' :';
    FFileNameLabel.Caption       := FAppModules.Language.GetString('NetworkFeatures.InflowFileLabel') + ' :';
    FFileNameComboBox.Hint       := FAppModules.language.GetString('NetworkFeatures.InflowFileName');
    FInflowFileNameGridBtn.Hint  := FAppModules.Language.GetString('ButtonHint.ViewDataGrid');
    FInflowFileNameGraphBtn.Hint := FAppModules.Language.GetString('ButtonHint.ViewDataGraph');

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedInflowDataDialog.RestoreColourState;
const OPNAME = 'TSpecifiedInflowDataDialog.RestoreColourState';
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

procedure TSpecifiedInflowDataDialog.AssignHelpContext;
const OPNAME = 'TSpecifiedInflowDataDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,              HC_InflowsFromOtherSystems);
    SetControlHelpContext(FFeatureNameEdit,  HC_InflowsFromOtherSystems);
    SetControlHelpContext(FFileNameComboBox, HC_InflowsFromOtherSystems);
  except on E: Exception do HandleError(E, OPNAME); end;
end;
end.

