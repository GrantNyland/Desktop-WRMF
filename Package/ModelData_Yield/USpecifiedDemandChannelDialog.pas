{******************************************************************************}
{*  UNIT      : Contains the class TSpecifiedDemandChannelDialog.             *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/09/03                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit USpecifiedDemandChannelDialog;

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

  TSpecifiedDemandChannelDialog = class(TAbstractScrollablePanel)
  private
    FFeatureNameLabel        : TLabel;
    FFeatureNameEdit         : TFieldEdit;
    FCatchmentReferenceLabel : TLabel;
    FCatchmentReferenceCbx   : TFieldComboBox;
    FIndicatorLabel          : TLabel;
    FIndicatorRadioGroup     : TFieldRadioGroup;
    FDemandFileNameLabel     : TLabel;
    FDemandFileNameCbx       : TFieldComboBox;
    FDemandFileNameSelectBtn : TFieldButton;
    FDemandFileNameGridBtn   : TFieldBitBtn;
    FDemandFileNameGraphBtn  : TFieldBitBtn;
    FDemandFileImportedLabel : TLabel;
  protected
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    property FeatureNameEdit         : TFieldEdit       read FFeatureNameEdit;
    property CatchmentReferenceLabel : TLabel           read FCatchmentReferenceLabel;
    property CatchmentReferenceCbx   : TFieldComboBox   read FCatchmentReferenceCbx;
    property IndicatorLabel          : TLabel           read FIndicatorLabel;
    property IndicatorRadioGroup     : TFieldRadioGroup read FIndicatorRadioGroup;
    property DemandFileNameLabel     : TLabel           read FDemandFileNameLabel;
    property DemandFileNameCbx       : TFieldComboBox   read FDemandFileNameCbx;
    property DemandFileNameSelectBtn : TFieldButton     read FDemandFileNameSelectBtn;
    property DemandFileNameGridBtn   : TFieldBitBtn     read FDemandFileNameGridBtn;
    property DemandFileNameGraphBtn  : TFieldBitBtn     read FDemandFileNameGraphBtn;
    property DemandFileImportedLabel : TLabel           read FDemandFileImportedLabel;
  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities;

{******************************************************************************}
{* TSpecifiedDemandChannelDialog                                              *}
{******************************************************************************}

procedure TSpecifiedDemandChannelDialog.CreateMemberObjects;
const OPNAME = 'TSpecifiedDemandChannelDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    //                                                                             Left  Top Width Height
    FFeatureNameLabel        := CreateFieldLabel                  (lOwner, lParent,  10, 10, 140,  21);
    FFeatureNameEdit         := CreateFieldEdit      (FAppModules, lOwner, lParent, 160, 10, 180,  21, 0, TRUE);
    FCatchmentReferenceLabel := CreateFieldLabel                  (lOwner, lParent,  10,  35, 140, 21);
    FCatchmentReferenceCbx   := CreateFieldComboBox  (FAppModules, lOwner, lParent, 160,  35, 180, 21, 9, TRUE, csDropDownList);
    FIndicatorLabel          := CreateFieldLabel                  (lOwner, lParent,  10,  60, 140, 21);
    FIndicatorRadioGroup     := CreateFieldRadioGroup(FAppModules, lOwner, lParent, 160,  60, 180, 65,10, FALSE);
    FDemandFileNameLabel     := CreateFieldLabel                  (lOwner, lParent,  10, 130, 145, 21);
    FDemandFileNameCbx       := CreateFieldComboBox  (FAppModules, lOwner, lParent, 160, 130, 280, 21,11, TRUE, csDropDownList);

    FDemandFileNameSelectBtn := CreateFieldButton    (FAppModules, lOwner, lParent, 445, 130,  25, 21, 8, TRUE, '...');
    with FDemandFileNameSelectBtn do
    begin
      Font.Name  := 'Arial';
      Font.Size  := 10;
      Font.Style := [fsBold];
      ParentFont := False;
    end;
    FDemandFileNameGridBtn   := CreateFieldBitButton(FAppModules, lOwner, lParent, 485, 130,  25, 21, 8, TRUE, 'VIEWDATAGRID');
    FDemandFileNameGraphBtn  := CreateFieldBitButton(FAppModules, lOwner, lParent, 515, 130,  25, 21, 8, TRUE, 'VIEWDATAGRAPH');
    FDemandFileImportedLabel := CreateFieldLabel    (             lOwner, lParent, 545, 130, 500, 21);
    FDemandFileImportedLabel.Font.Color := clRed;

    FIndicatorRadioGroup.Columns := 1;
    FFeatureNameEdit.Enabled := True;
    FFeatureNameEdit.Color   := clBtnFace;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedDemandChannelDialog.Resize;
const OPNAME = 'TSpecifiedDemandChannelDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSpecifiedDemandChannelDialog.Initialise: boolean;
const OPNAME = 'TSpecifiedDemandChannelDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
    FDemandFileNameGridBtn.Enabled := False;
    FDemandFileNameGraphBtn.Enabled := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSpecifiedDemandChannelDialog.LanguageHasChanged: boolean;
const OPNAME = 'TSpecifiedDemandChannelDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FFeatureNameLabel.Caption        := FAppModules.Language.GetString('NetworkFeatures.FeatureName') + ' :';
    FCatchmentReferenceLabel.Caption := FAppModules.Language.GetString('TField.CatchmentRef') + ' :';
    FIndicatorLabel.Caption          := FAppModules.Language.GetString('TField.Stochastic') + ' :';
    FDemandFileNameLabel.Caption     := FAppModules.Language.GetString('TField.FullName') + ' :';
    FIndicatorRadioGroup.Hints.Clear;
    FIndicatorRadioGroup.Hints.Add(FAppModules.Language.GetString('TSpecifiedDemandChannelDialog.StochasticIndicator'));
    FIndicatorRadioGroup.Hints.Add(FAppModules.Language.GetString('TSpecifiedDemandChannelDialog.HistoricIndicator'));
    FDemandFileNameSelectBtn.Caption := FAppModules.Language.GetString('ButtonCaption.SelectDemandFile');
    FDemandFileNameSelectBtn.Hint    := FAppModules.Language.GetString('ButtonHint.SelectDemandFile');

    DemandFileNameGridBtn.Hint     := FAppModules.Language.GetString('ButtonHint.ViewDataGrid');
    DemandFileNameGraphBtn.Hint    := FAppModules.Language.GetString('ButtonHint.ViewDataGraph');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedDemandChannelDialog.RestoreColourState;
const OPNAME = 'TSpecifiedDemandChannelDialog.RestoreColourState';
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

procedure TSpecifiedDemandChannelDialog.AssignHelpContext;
const OPNAME = 'TSpecifiedDemandChannelDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                     HC_CreatingChannels);
    SetControlHelpContext(FFeatureNameEdit,         HC_CreatingChannels);
    SetControlHelpContext(FCatchmentReferenceCbx,   HC_CreatingChannels);
    SetControlHelpContext(FIndicatorRadioGroup,     HC_CreatingChannels);
    SetControlHelpContext(FDemandFileNameCbx,       HC_CreatingChannels);
    SetControlHelpContext(FDemandFileNameSelectBtn, HC_CreatingChannels);
    SetControlHelpContext(FDemandFileNameGridBtn,   HC_CreatingChannels);
    SetControlHelpContext(FDemandFileNameGraphBtn,  HC_CreatingChannels);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
