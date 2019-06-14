{******************************************************************************}
{*  UNIT      : Contains the class TYieldRunTitleDialog                        }
{*  AUTHOR    : Presley Mudau                                                  }
{*  DATE      : 2004/11/18                                                     }
{*  COPYRIGHT : Copyright © 2004 DWAF                                          }
{******************************************************************************}

unit UYieldRunTitleDialog;

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

  TYieldRunTitleDialog = class(TAbstractScrollablePanel)
  private
  protected
    FRunTitle1Label        : TLabel;
    FRunTitle2Label        : TLabel;
    FRunTitle3Label        : TLabel;
    FRunTitle1Edit : TFieldEdit;
    FRunTitle2Edit : TFieldEdit;
    FRunTitle3Edit : TFieldEdit;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    property RunTitle1Edit : TFieldEdit read FRunTitle1Edit;
    property RunTitle2Edit : TFieldEdit read FRunTitle2Edit;
    property RunTitle3Edit : TFieldEdit read FRunTitle3Edit;
   end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities;

{******************************************************************************}
{* TYieldRunTitleDialog                                                       *}
{******************************************************************************}

procedure TYieldRunTitleDialog.CreateMemberObjects;
const OPNAME = 'TYieldRunTitleDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    //                                                                Left  Top Width Height
    FRunTitle1Label := CreateFieldLabel  (lOwner, lParent,  10,  10, 190, 21);
    FRunTitle2Label := CreateFieldLabel  (lOwner, lParent,  10,  40, 190, 21);
    FRunTitle3Label := CreateFieldLabel  (lOwner, lParent,  10,  70, 190, 21);

    FRunTitle1Edit := CreateFieldEdit  (FAppModules, lOwner, lParent, 60,  10, 500, 21, 0, TRUE);
    FRunTitle2Edit := CreateFieldEdit  (FAppModules, lOwner, lParent, 60,  40, 500, 21, 1, TRUE);
    FRunTitle3Edit := CreateFieldEdit  (FAppModules, lOwner, lParent, 60,  68, 500, 24, 2, TRUE);

    FRunTitle1Edit.MaxLength := 80;
    FRunTitle2Edit.MaxLength := 80;
    FRunTitle3Edit.MaxLength := 80;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunTitleDialog.Resize;
const OPNAME = 'TYieldRunTitleDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunTitleDialog.Initialise: boolean;
const OPNAME = 'TYieldRunTitleDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunTitleDialog.LanguageHasChanged: boolean;
const OPNAME = 'TYieldRunTitleDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FRunTitle1Label.Caption := FAppModules.Language.GetString('TField.Title1');
    FRunTitle2Label.Caption := FAppModules.Language.GetString('TField.Title2');
    FRunTitle3Label.Caption := FAppModules.Language.GetString('TField.Title3');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunTitleDialog.RestoreColourState;
const OPNAME = 'TYieldRunTitleDialog.RestoreColourState';
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

procedure TYieldRunTitleDialog.AssignHelpContext;
const OPNAME = 'TYieldRunTitleDialog.AssignHelpContext';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
