//
//
//  UNIT      : Contains the class TIFRFeaturesPropertyDialog.
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 2007/02/06
//  COPYRIGHT : Copyright © 2003 DWAF
//
//

unit UIFRFeaturesPropertyDialog;

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

  TIFRFeaturesPropertyDialog = class(TAbstractScrollablePanel)
  protected
    FReferenceInflowTypeLabel      : TLabel;
    FReferenceInflowTypeRadioGroup : TFieldRadioGroup;
    FOptionDescriptionsLabel       : TLabel;
    procedure CreateMemberObjects; override;
  public
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    property ReferenceInflowTypeRadioGroup : TFieldRadioGroup   read FReferenceInflowTypeRadioGroup;
    property OptionDescriptionsLabel       : TLabel             read FOptionDescriptionsLabel;
  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  UControlCreationUtilities,
  UErrorHandlingOperations;

{ TIFRFeaturesPropertyDialog }

procedure TIFRFeaturesPropertyDialog.CreateMemberObjects;
const OPNAME = 'TIFRFeaturesPropertyDialog.CreateMemberObjects';
begin
  inherited;
  try
    //                                                                                    Left  Top  Width Height
    FReferenceInflowTypeLabel      := CreateFieldLabel       (ControlsOwner,ControlsParent,  10,  10, 130,  20);
    FReferenceInflowTypeRadioGroup := CreateFieldRadioGroup  (FAppModules, ControlsOwner, ControlsParent,  120,  10, 190,  60, 0, True);
    FOptionDescriptionsLabel       := CreateFieldLabel       (ControlsOwner,ControlsParent,  10,  80,700,80);
    FOptionDescriptionsLabel.Font.Style := [fsBold];
    FOptionDescriptionsLabel.Font.Size  := 10;
    FOptionDescriptionsLabel.WordWrap   := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRFeaturesPropertyDialog.Initialise: boolean;
const OPNAME = 'TIFRFeaturesPropertyDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FReferenceInflowTypeRadioGroup.Items.Clear;
    FReferenceInflowTypeRadioGroup.Items.Add('');
    FReferenceInflowTypeRadioGroup.Items.Add('');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRFeaturesPropertyDialog.LanguageHasChanged: boolean;
const OPNAME = 'TIFRFeaturesPropertyDialog.LanguageHasChanged';
begin
  Result := True;
  try
    FReferenceInflowTypeLabel.Caption       := FAppModules.Language.GetString('TField.IFRInflowOption') + ' :';
    FReferenceInflowTypeRadioGroup.Items[0] := FAppModules.Language.GetString('TField.IFRInflowOptionNatural');
    FReferenceInflowTypeRadioGroup.Items[1] := FAppModules.Language.GetString('TField.IFRInflowOptionDeveloped');
    FOptionDescriptionsLabel.Caption        := '';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
