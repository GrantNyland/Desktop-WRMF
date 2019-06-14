//
//
//  UNIT      : Contains the class TNodePropertiesDialog.
//  AUTHOR    : Titi Ngubane (ARIVIA)
//  DATE      : 2003/07/01
//  COPYRIGHT : Copyright © 2003 DWAF
//
//

unit UNodePropertiesDialog;

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

  TNodePropertiesDialog = class(TAbstractScrollablePanel)
  protected
    FNodeNameLabel        : TLabel;
    FNodeNameEdit         : TFieldEdit;
    FNodeNumberLabel      : TLabel;
    FNodeNumberEdit       : TFieldEdit;
    FSummaryOutputChkBox  : TFieldChkBox;
    FNodeXCoordLabel      : TLabel;
    FNodeXCoordEdit       : TFieldEdit;
    FNodeYCoordLabel      : TLabel;
    FNodeYCoordEdit       : TFieldEdit;
    FOnHintChange         : TDoHintChangeFunction;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure SetNodeType(AReservoirID:integer;AWithInflow:boolean);
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property NodeNameEdit        : TFieldEdit read FNodeNameEdit;
    property NodeNumberEdit      : TFieldEdit read FNodeNumberEdit;
    property NodeXCoordEdit      : TFieldEdit read FNodeXCoordEdit;
    property NodeYCoordEdit      : TFieldEdit read FNodeYCoordEdit;
    property SummaryOutputChkBox : TFieldChkBox read FSummaryOutputChkBox;
  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  UControlCreationUtilities,
  UErrorHandlingOperations;

const
  C_ControlBorder  = 5;
  C_LabelOffset    = 3;
  C_GroupBoxOffset = 5;


{ TNodePropertiesDialog }

procedure TNodePropertiesDialog.CreateMemberObjects;
const OPNAME = 'TNodePropertiesDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    FNodeNumberLabel     := CreateFieldLabel (lOwner, lParent,  10, 10, 140, 21);
    FNodeNameLabel       := CreateFieldLabel (lOwner, lParent,  10, 35, 140, 21);
    FNodeXCoordLabel     := CreateFieldLabel (lOwner, lParent,  10, 60, 140, 21);
    FNodeYCoordLabel     := CreateFieldLabel (lOwner, lParent,  10, 85, 140, 21);
    FNodeNumberEdit      := CreateFieldEdit  (FAppModules, lOwner, lParent, 160, 10, 180, 21, 0, FALSE);
    FNodeNameEdit        := CreateFieldEdit  (FAppModules, lOwner, lParent, 160, 35, 180, 21, 0, TRUE);
    FNodeXCoordEdit      := CreateFieldEdit  (FAppModules, lOwner, lParent, 160, 60, 180, 21, 0, TRUE);
    FNodeYCoordEdit      := CreateFieldEdit  (FAppModules, lOwner, lParent, 160, 85, 180, 21, 0, TRUE);
    FSummaryOutputChkBox := CreateFieldChkBox(FAppModules, lOwner, lParent,  10,110, 163, 21, 1, TRUE, taLeftJustify);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNodePropertiesDialog.DestroyMemberObjects;
const OPNAME = 'TNodePropertiesDialog.DestroyMemberObjects';
begin
  try
    inherited;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNodePropertiesDialog.Initialise: boolean;
const OPNAME = 'TNodePropertiesDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNodePropertiesDialog.Resize;
const OPNAME = 'TNodePropertiesDialog.Resize';
begin
  inherited Resize;
  try

{    // Set out the controls.
    FNodeNumberEdit.Width              := Self.ClientWidth div 2 - C_ControlBorder;
    FNodeNumberEdit.Left               := Self.ClientWidth div 2;

    FNodeNameEdit.Left            := Self.ClientWidth div 2;
    FNodeNameEdit.Width           := Self.ClientWidth div 2 - C_ControlBorder;

    FSummaryIncludeChkBox.Width        := Self.ClientWidth div 2 - C_ControlBorder;
    FSummaryIncludeChkBox.Left         := Self.ClientWidth div 2;
}{RHS}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNodePropertiesDialog.LanguageHasChanged: boolean;
const OPNAME = 'TNodePropertiesDialog.LanguageHasChanged';
begin
  Result := True;
  try
    FNodeNumberLabel.Caption     := FAppModules.Language.GetString('TField.NodeNumber') + ' :';
    FNodeNameLabel.Caption       := FAppModules.Language.GetString('TField.NodeName') + ' :';
    FNodeXCoordLabel.Caption     := FAppModules.Language.GetString('TField.XCoord') + ' :';
    FNodeYCoordLabel.Caption     := FAppModules.Language.GetString('TField.YCoord') + ' :';
    FSummaryOutputChkBox.Caption := FAppModules.Language.GetString('TField.IncludeSummary') + ' ?';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNodePropertiesDialog.AssignHelpContext;
const OPNAME = 'TNodePropertiesDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                   HC_CreatingNodes);
    SetControlHelpContext(FNodeNameEdit,          HC_CreatingNodes);
    SetControlHelpContext(FNodeNumberEdit,        HC_CreatingNodes);
    SetControlHelpContext(FSummaryOutputChkBox,   HC_ResultOutputControl);
  except on E: Exception do HandleError(E, OPNAME); end;
end;
  
procedure TNodePropertiesDialog.SetNodeType(AReservoirID:integer;AWithInflow: boolean);
const OPNAME = 'TNodePropertiesDialog.SetNodeType';
begin
  try
    if AWithInflow then
    begin
      FNodeNameEdit.IsEnabled      := TRUE;
      FSummaryOutputChkBox.Visible := TRUE;
      FSummaryOutputChkBox.Enabled := TRUE;
      FNodeXCoordLabel.Visible     := TRUE;
      FNodeXCoordEdit.Visible      := TRUE;
      FNodeYCoordLabel.Visible     := TRUE;
      FNodeYCoordEdit.Visible      := TRUE;
    end
    else
    begin
      FNodeNameEdit.IsEnabled      := (AReservoirID <> 0);
      FSummaryOutputChkBox.Visible := FALSE;
      FSummaryOutputChkBox.Enabled := FALSE;
      FNodeXCoordLabel.Visible     := FNodeNameEdit.IsEnabled;
      FNodeXCoordEdit.Visible      := FNodeNameEdit.IsEnabled;
      FNodeYCoordLabel.Visible     := FNodeNameEdit.IsEnabled;
      FNodeYCoordEdit.Visible      := FNodeNameEdit.IsEnabled;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
