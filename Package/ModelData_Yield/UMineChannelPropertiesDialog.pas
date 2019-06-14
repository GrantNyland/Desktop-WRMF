{******************************************************************************}
{*  UNIT      : Contains the class TMineChannelPropertiesDialog.              *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2007/03/14                                                    *}
{*  COPYRIGHT : Copyright © 2007 DWAF                                         *}
{******************************************************************************}

unit UMineChannelPropertiesDialog;

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

  TMineChannelPropertiesDialog = class(TAbstractScrollablePanel)
  private
  protected
    FChannelNameLabel             : TLabel;
    FUpstreamNodeLabel            : TLabel;
    FDownstreamNodeLabel          : TLabel;
    FPenaltyStructureLabel        : TLabel;
    FChannelAreaLabel             : TLabel;
    FChannelNameEdit              : TFieldEdit;
    FChannelNumberEdit            : TFieldEdit;
    FUpStreamNodeEdit             : TFieldEdit;
    FDownStreamNodeCbx            : TFieldComboBox;
    FChannelAreaCbx               : TFieldComboBox;
    FPenaltyStructureEdit         : TFieldEdit;
    FSelectPenaltyStructureBtn    : TFieldButton;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    property ChannelNumberEdit         : TFieldEdit     read FChannelNumberEdit;
    property ChannelNameEdit           : TFieldEdit     read FChannelNameEdit;
    property UpStreamNodeEdit          : TFieldEdit     read FUpStreamNodeEdit;
    property DownStreamNodeCbx         : TFieldComboBox read FDownStreamNodeCbx;
    property ChannelAreaCbx            : TFieldComboBox read FChannelAreaCbx;
    property PenaltyStructureEdit      : TFieldEdit     read FPenaltyStructureEdit;
    property SelectPenaltyStructureBtn : TFieldButton   read FSelectPenaltyStructureBtn;
  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities;

const
  C_ControlBorder  = 10;
  C_LabelOffset    = 3;

{******************************************************************************}
{* TMineChannelPropertiesDialog                                                  *}
{******************************************************************************}

procedure TMineChannelPropertiesDialog.CreateMemberObjects;
const OPNAME = 'TMineChannelPropertiesDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    //                                                                Left  Top Width Height
    FChannelNameLabel          := CreateFieldLabel   (lOwner, lParent,  10,  10, 140, 21);
    FUpStreamNodeLabel         := CreateFieldLabel   (lOwner, lParent,  10,  35, 140, 21);
    FDownstreamNodeLabel       := CreateFieldLabel   (lOwner, lParent,  10,  60, 140, 21);
    FChannelAreaLabel          := CreateFieldLabel   (lOwner, lParent,  10,  85, 140, 21);
    FPenaltyStructureLabel     := CreateFieldLabel   (lOwner, lParent,  10,  110, 140, 21);

    FChannelNumberEdit         := CreateFieldEdit    (FAppModules, lOwner, lParent, 160,  10,  25, 21, 0,   FALSE);
    FChannelNameEdit           := CreateFieldEdit    (FAppModules, lOwner, lParent, 190,  10, 253, 21, 1,   TRUE);
    FUpStreamNodeEdit          := CreateFieldEdit    (FAppModules, lOwner, lParent, 160,  35, 280, 21, 3,   TRUE);
    FUpStreamNodeEdit          := CreateFieldEdit    (FAppModules, lOwner, lParent, 160,  35, 280, 21, 4,   TRUE);
    FDownStreamNodeCbx         := CreateFieldComboBox(FAppModules, lOwner, lParent, 160,  60, 280, 21, 6,   TRUE, csDropDownList);
    FChannelAreaCbx            := CreateFieldComboBox(FAppModules, lOwner, lParent, 160,  85, 280, 21, 7,   TRUE, csDropDownList);
    FPenaltyStructureEdit      := CreateFieldEdit    (FAppModules, lOwner, lParent, 160,  110,  50, 21, 7,  TRUE);
    FSelectPenaltyStructureBtn := CreateFieldButton  (FAppModules, lOwner, lParent, 215,  110,  25, 21, 8,  TRUE, '...');

    with FSelectPenaltyStructureBtn do
    begin
      Font.Name  := 'Arial';
      Font.Size  := 10;
      Font.Style := [fsBold];
      ParentFont := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineChannelPropertiesDialog.Resize;
const OPNAME = 'TMineChannelPropertiesDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineChannelPropertiesDialog.Initialise: boolean;
const OPNAME = 'TMineChannelPropertiesDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineChannelPropertiesDialog.LanguageHasChanged: boolean;
const OPNAME = 'TMineChannelPropertiesDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FChannelNameLabel.Caption        := FAppModules.Language.GetString('TField.MineName')    + ' :';
    FUpStreamNodeLabel.Caption       := FAppModules.Language.GetString('TField.UpStreamNode')   + ' :';
    FDownstreamNodeLabel.Caption     := FAppModules.Language.GetString('TField.DownNodeNumber') + ' :';
    FPenaltyStructureLabel.Caption   := FAppModules.Language.GetString('TField.PenaltyStruct')  + ' :';
    FChannelAreaLabel.Caption        := FAppModules.Language.GetString('TField.ChannelArea')    + ' :';
    FSelectPenaltyStructureBtn.Hint  := FAppModules.Language.GetString('ButtonHint.Select');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineChannelPropertiesDialog.RestoreColourState;
const OPNAME = 'TMineChannelPropertiesDialog.RestoreColourState';
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

procedure TMineChannelPropertiesDialog.AssignHelpContext;
const OPNAME = 'TMineChannelPropertiesDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                       HC_CreatingChannels);
    SetControlHelpContext(FChannelNameEdit,           HC_CreatingChannels);
    SetControlHelpContext(FChannelNumberEdit,         HC_CreatingChannels);
    SetControlHelpContext(FPenaltyStructureEdit,      HC_ChannelPenaltyStructures);
    SetControlHelpContext(FUpStreamNodeEdit,          HC_CreatingChannels);
    SetControlHelpContext(FDownStreamNodeCbx,         HC_CreatingChannels);
    SetControlHelpContext(FSelectPenaltyStructureBtn, HC_ChannelPenaltyStructures);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
