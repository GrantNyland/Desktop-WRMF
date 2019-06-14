//
//
//  UNIT      : Contains the class THydrologyAlocationDialog.
//  AUTHOR    : Dziedzi Ramulondi (Cornastone)
//  DATE      : 2004/07/23
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UHydrologyAlocationDialog;

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

  THydrologyAlocationDialog = class(TAbstractScrollablePanel)
  protected
    FParamFileNameLabel        : TLabel;
    FParamFileNameEdit         : TFieldEdit;
    FSelectParamFileButton     : TFieldButton;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property ParamFileNameEdit      : TFieldEdit read FParamFileNameEdit;
    property SelectParamFileButton : TFieldButton read FSelectParamFileButton;
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


{ THydrologyAlocationDialog }

procedure THydrologyAlocationDialog.CreateMemberObjects;
const OPNAME = 'THydrologyAlocationDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    FParamFileNameLabel       := CreateFieldLabel (lOwner, lParent,  10, 35, 140, 21);
    FParamFileNameEdit        := CreateFieldEdit  (FAppModules, lOwner, lParent, 160, 35, 180, 21, 0, TRUE);
    FSelectParamFileButton    := CreateFieldButton (FAppModules,lOwner, lParent,  10, 10, 140, 21,1,True,'');
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function THydrologyAlocationDialog.Initialise: boolean;
const OPNAME = 'THydrologyAlocationDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyAlocationDialog.Resize;
const OPNAME = 'THydrologyAlocationDialog.Resize';
begin
  inherited Resize;
  try
    FParamFileNameLabel.Top      := C_ControlBorder;
    FParamFileNameLabel.Left     := C_ControlBorder;
    FParamFileNameLabel.Width    := Self.Width div 4;

    FSelectParamFileButton.Top   := C_ControlBorder;
    FSelectParamFileButton.Width := Self.Width div 12;
    FSelectParamFileButton.Left  := Self.Width - FSelectParamFileButton.Width - C_ControlBorder;

    FParamFileNameEdit.Top       := C_ControlBorder;
    FParamFileNameEdit.Width     := FSelectParamFileButton.Left -
                                    FParamFileNameLabel.Width -
                                    FParamFileNameLabel.Left -2;
    FParamFileNameEdit.Left      := FParamFileNameLabel.Left +
                                    FParamFileNameLabel.Width;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyAlocationDialog.LanguageHasChanged: boolean;
const OPNAME = 'THydrologyAlocationDialog.LanguageHasChanged';
begin
  Result := True;
  try
    FParamFileNameLabel.Caption       := FAppModules.Language.GetString('LabelText.ParameterFile');
    FSelectParamFileButton.Caption     := FAppModules.Language.GetString('LabelText.ThreeDots')
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyAlocationDialog.AssignHelpContext;
const OPNAME = 'THydrologyAlocationDialog.AssignHelpContext';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;
end.
