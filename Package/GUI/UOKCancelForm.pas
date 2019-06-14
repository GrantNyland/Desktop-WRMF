//
//
//  UNIT      : Contains TOKCancelForm Class
//  AUTHOR    : Dziedzi Ramulondi (ARIVIA)
//  DATE      : 13/10/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//

unit UOKCancelForm;

interface

uses

  // Delphi
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Classes,
  Vcl.Controls,
  UAbstractComponent;

type
  TOKCancelForm = class(TAbstractForm)
  protected
    FPanButton : TPanel;
    FBtnCancel : TButton;
    FBtnOk : TButton;

    // Member management issues
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
    function  Get_ButtonOKCaption: string;
    function  Get_ButtonCancelCaption: string;
    procedure Set_ButtonOKCaption(ACaption : string);
    procedure Set_ButtonCancelCaption(ACaption : string);
  public
    procedure Resize; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function Initialise: boolean; override;
    function AddControl(AControl:TControl): boolean;
    property ButtonOKCaption : string read Get_ButtonOKCaption write Set_ButtonOKCaption;
    property ButtonOKCancel : string read Get_ButtonCancelCaption write Set_ButtonCancelCaption;
  end;

implementation

uses

  // Delphi
  Sysutils,

  // DWAF
  UErrorHandlingOperations;

{ TOKCancelForm }

procedure TOKCancelForm.CreateMemberObjects;
const OPNAME = 'TOKCancelForm.CreateMemberObjects';
begin
  try
    FPanButton := TPanel.Create(Self);
    FBtnOk := TButton.Create(Self);
    FBtnCancel := TButton.Create(Self);
    FBtnOk.ModalResult := mrOk;
    FBtnCancel.ModalResult := mrCancel;

    //Self.Position := poScreenCenter;
    Self.FormStyle := fsNormal;

    Self.ClientHeight := 448;
    Self.ClientWidth := 448;

    BorderStyle := bsSizeToolWin;

    Caption := Application.Title;

    inherited CreateMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOKCancelForm.AssignHelpContext;
const OPNAME = 'TOKCancelForm.AssignHelpContext';
begin
  inherited AssignHelpContext;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOKCancelForm.LanguageHasChanged: boolean;
const OPNAME = 'TOKCancelForm.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FBtnOk.Caption := FAppModules.Language.GetString('ButtonCaption.OK');
    FBtnCancel.Caption := FAppModules.Language.GetString('ButtonCaption.Cancel');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TOKCancelForm.StudyHasChanged: boolean;
const OPNAME = 'TOKCancelForm.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOKCancelForm.Initialise: boolean;
const OPNAME = 'TOKCancelForm.Initialise';
begin
  Result := inherited Initialise;
  try
    FPanButton.Parent := Self;
    FBtnOk.Parent := FPanButton;
    FBtnCancel.Parent := FPanButton;
    FBtnOk.ModalResult := mrOk;
    FBtnCancel.ModalResult := mrCancel;

    FPanButton.Height := FBtnOk.Height + 10;
    FPanButton.Align := alTop;

    FBtnOk.Top := 5;
    FBtnCancel.Top := 5;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOKCancelForm.Resize;
const OPNAME = 'TOKCancelForm.Resize';
begin
  inherited;
  try
      FBtnOk.Left := 10;
      FBtnCancel.Left := FBtnOk.Left  + FBtnOk.Width + 10;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TOKCancelForm.AddControl(AControl:TControl): boolean;
const OPNAME = 'TOKCancelForm.AddControl';
begin
  Result := False;
  try
    if Assigned(AControl) then
    begin
      AControl.Parent := Self;
      AControl.Align := alClient;
      AControl.Visible := True;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOKCancelForm.Get_ButtonCancelCaption: string;
const OPNAME = 'TOKCancelForm.Get_ButtonCancelCaption';
begin
  Result := '';
  try
    Result := FBtnCancel.Caption;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOKCancelForm.Get_ButtonOKCaption: string;
const OPNAME = 'TOKCancelForm.Get_ButtonOKCaption';
begin
  Result := '';
  try
    Result := FBtnOk.Caption;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOKCancelForm.Set_ButtonCancelCaption(ACaption: string);
const OPNAME = 'TOKCancelForm.Set_ButtonCancelCaption';
begin
  try
    FBtnCancel.Caption := ACaption;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOKCancelForm.Set_ButtonOKCaption(ACaption: string);
const OPNAME = 'TOKCancelForm.Set_ButtonOKCaption';
begin
  try
    FBtnOk.Caption := ACaption;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
