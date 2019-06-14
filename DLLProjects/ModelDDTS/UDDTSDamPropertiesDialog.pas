//
//
//  UNIT      : Contains the class TDDTSDamConfigValidator.
//  AUTHOR    : Sam Dhlamini
//  DATE      : 2014/07/17
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UDDTSDamPropertiesDialog;

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

  TDDTSDamPropertiesDialog = class(TAbstractScrollablePanel)
  private
  protected

    FgboxProperties       : TGroupBox;
    FReservoirNameLabel      : TLabel;
    FReservoirNameEdit       : TFieldEdit;

    FReservoirXCoordLabel      : TLabel;
    FReservoirXCoordEdit       : TFieldEdit;
    FReservoirYCoordLabel      : TLabel;
    FReservoirYCoordEdit       : TFieldEdit;

     FgboxConfig               : TGroupBox;
     FRunoffScaleFactorLabel      : TLabel;
     FRunoffScaleFactorEdit       : TFieldEdit;
     FOtherInflowScaleFactorLabel      : TLabel;
     FOtherInflowScaleFactorEdit       : TFieldEdit;
     FEWRScaleFactorLabel      : TLabel;
     FEWRScaleFactorEdit       : TFieldEdit;
     FTargetDraftLabel      : TLabel;
     FTargetDraftEdit       : TFieldEdit;
     FDSRequirmentLabel      : TLabel;
     FDSRequirmentEdit       : TFieldEdit;


    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure RestoreColourState; override;

    property ReservoirNameEdit          : TFieldEdit       read FReservoirNameEdit;
    property ReservoirXCoordEdit        : TFieldEdit       read FReservoirXCoordEdit;
    property ReservoirYCoordEdit        : TFieldEdit       read FReservoirYCoordEdit;


  end;

  implementation
uses
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations;

{ TDDTSDamPropertiesDialog }

procedure TDDTSDamPropertiesDialog.CreateMemberObjects;
const OPNAME = 'TDDTSDamPropertiesDialog.CreateMemberObjects';
begin
  inherited;
  try

    FgboxProperties                                           := TGroupBox.Create(ControlsOwner);
    FgboxProperties.Parent                                    := ControlsParent;
    FgboxProperties.Align                                     := alTop;
    FgboxProperties.Height                                    := 100;

    FReservoirNameEdit         := TFieldEdit.Create(ControlsOwner, FAppModules);
    FReservoirNameEdit.Parent  := FgboxProperties;
    FReservoirNameEdit.Top     := C_ControlBorder*3;

    FReservoirNameLabel        := TLabel.Create(ControlsOwner);
    FReservoirNameLabel.Parent := FgboxProperties;
    FReservoirNameLabel.Top    := FReservoirNameEdit.Top + C_LabelOffset;
    FReservoirNameLabel.Left   := C_LabelOffset;

    FReservoirXCoordEdit         := TFieldEdit.Create(ControlsOwner, FAppModules);
    FReservoirXCoordEdit.Parent  := FgboxProperties;
    FReservoirXCoordEdit.Top     := FReservoirNameEdit.Top + FReservoirNameEdit.Height + C_ControlBorder;

    FReservoirXCoordLabel        := TLabel.Create(ControlsOwner);
    FReservoirXCoordLabel.Parent := FgboxProperties;
    FReservoirXCoordLabel.Top    := FReservoirXCoordEdit.Top + C_LabelOffset;
    FReservoirXCoordLabel.Left   := C_LabelOffset;

    FReservoirYCoordEdit         := TFieldEdit.Create(ControlsOwner, FAppModules);
    FReservoirYCoordEdit.Parent  := FgboxProperties;
    FReservoirYCoordEdit.Top     := FReservoirXCoordEdit.Top + FReservoirXCoordEdit.Height + C_ControlBorder;

    FReservoirYCoordLabel        := TLabel.Create(ControlsOwner);
    FReservoirYCoordLabel.Parent := FgboxProperties;
    FReservoirYCoordLabel.Top    := FReservoirYCoordEdit.Top + C_LabelOffset;
    FReservoirYCoordLabel.Left   := C_LabelOffset;



  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TDDTSDamPropertiesDialog.Initialise: boolean;
const OPNAME = 'TDDTSDamPropertiesDialog.Initialise';
begin
  Result := inherited Initialise;
  try

    Result := True;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSDamPropertiesDialog.Resize;
const OPNAME = 'TDDTSDamPropertiesDialog.Resize';
begin
  // Call the ancestor.
  inherited Resize;
  try
    // Set out the controls.
    FReservoirNameEdit.Width         := Self.ClientWidth div 2 - C_ControlBorder;
    FReservoirNameEdit.Left          := Self.ClientWidth div 2;


    ReservoirXCoordEdit.Left           := Self.ClientWidth div 2;
    ReservoirXCoordEdit.Width          := FReservoirNameEdit.Width  div 3;

    ReservoirYCoordEdit.Left           := Self.ClientWidth div 2;
    ReservoirYCoordEdit.Width          := FReservoirNameEdit.Width  div 3;



  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSDamPropertiesDialog.LanguageHasChanged: boolean;
const OPNAME = 'TDDTSDamPropertiesDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FReservoirNameLabel.Caption       := FAppModules.Language.GetString('TField.ReservoirName');
    FReservoirXCoordLabel.Caption     := FAppModules.Language.GetString('TField.XCoord');
    FReservoirYCoordLabel.Caption     := FAppModules.Language.GetString('TField.YCoord');

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSDamPropertiesDialog.RestoreColourState;
const OPNAME = 'TDDTSDamPropertiesDialog.RestoreColourState';
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

procedure TDDTSDamPropertiesDialog.AssignHelpContext;
const OPNAME = 'TDDTSDamPropertiesDialog.AssignHelpContext';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.

