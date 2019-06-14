{******************************************************************************}
{*  UNIT      : Contains the class TSwitchDefinitionDialog.                   *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2006/02/27                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit USwitchDefinitionDialog;

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

  TSwitchDefinitionDialog = class(TAbstractScrollablePanel)
  private
  protected
    FLblSwitchDefFileName : TLabel;
    FLblStartDate         : TLabel;
    FEdtSwitchDefFileName : TFieldEdit;
    FCbxStartYear         : TFieldComboBox;
    FCbxStartMonth        : TFieldComboBox;
    FlbxChannels          : TFieldListBox;
    FlblChannels          : TLabel;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    property EdtSwitchDefFileName  : TFieldEdit     read FEdtSwitchDefFileName;
    property CbxStartYear          : TFieldComboBox read FCbxStartYear;
    property CbxStartMonth         : TFieldComboBox read FCbxStartMonth;
    property lbxChannels           : TFieldListBox read FlbxChannels;
  end;

implementation

uses
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities;

{******************************************************************************}
{* TSwitchDefinitionDialog                                                    *}
{******************************************************************************}

procedure TSwitchDefinitionDialog.CreateMemberObjects;
const OPNAME = 'TSwitchDefinitionDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    //                                                           Left  Top Width Height
    FLblStartDate         := CreateFieldLabel   (lOwner, lParent,  10,  10, 130, 21);
    FLblSwitchDefFileName := CreateFieldLabel   (lOwner, lParent,  10,  40, 130, 21);
    FlblChannels          := CreateFieldLabel   (lOwner, lParent,  10,  70, 130, 21);

    FCbxStartYear         := CreateFieldComboBox(FAppModules, lOwner, lParent, 140,  10,  80, 21, 3, TRUE, csDropDownList);
    FCbxStartMonth        := CreateFieldComboBox(FAppModules, lOwner, lParent, 230,  10,  80, 21, 3, TRUE, csDropDownList);
    FEdtSwitchDefFileName := CreateFieldEdit    (FAppModules, lOwner, lParent, 140,  40, 220, 21, 0, FALSE);
    FlbxChannels          := TFieldListBox.Create(ControlsOwner,FAppModules);
    FlbxChannels.Parent   := ControlsParent;
    FlbxChannels.Left     := 140;
    FlbxChannels.Top      := 70;
    FlbxChannels.Width    := 220;
    FlbxChannels.Height   := 100;
    FlbxChannels.Color    := clMenuBar;

    FCbxStartYear.DropDownCount := 25;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSwitchDefinitionDialog.Resize;
const OPNAME = 'TSwitchDefinitionDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSwitchDefinitionDialog.Initialise: boolean;
const OPNAME = 'TSwitchDefinitionDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSwitchDefinitionDialog.LanguageHasChanged: boolean;
const OPNAME = 'TSwitchDefinitionDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FLblSwitchDefFileName.Caption  := FAppModules.Language.GetString('PlanningGUI.SwitchDefFileName');
    FLblStartDate.Caption          := FAppModules.Language.GetString('PlanningGUI.AllocDefStartDate');
    FlblChannels.Caption           := FAppModules.Language.GetString('ViewData.ReviewchannelHeading');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSwitchDefinitionDialog.RestoreColourState;
const OPNAME = 'TSwitchDefinitionDialog.RestoreColourState';
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

procedure TSwitchDefinitionDialog.AssignHelpContext;
const OPNAME = 'TSwitchDefinitionDialog.AssignHelpContext';
begin
  try
{
    SetControlHelpContext(Self,                HC_CreatingChannels);
    SetControlHelpContext(FEdtSwitchDefFileName,    HC_CreatingChannels);
    SetControlHelpContext(FCbxStartYear,       HC_CreatingChannels);
    SetControlHelpContext(FCbxStartMonth,      HC_ChannelPenaltyStructures);
    SetControlHelpContext(FCbxEndYear,         HC_CreatingChannels);
    SetControlHelpContext(FCbxEndMonth,        HC_CreatingChannels);
}
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
