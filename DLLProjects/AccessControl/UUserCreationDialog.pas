unit UUserCreationDialog;

interface

uses
  UAbstractObject,
  UAbstractComponent,
  System.UITypes,
  Windows,
  Messages,
  SysUtils,
  Variants,
  Contnrs,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  Buttons,
  UUser,
  UDataSetType,
  UErrorHandlingOperations;

type
  TUserCreationDialog = class(TAbstractForm)
    FPanel              : TPanel;
    GroupBox1           : TGroupBox;
    EdtConfirmPassword  : TEdit;
    EdtPassword         : TEdit;
    EdtUserId           : TEdit;
    LblUserId           : TLabel;
    LblPassword         : TLabel;
    LblConfirm          : TLabel;
    btnCancel           : TBitBtn;
    BtnOK               : TBitBtn;
    procedure btnCancelClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure OnEditChange(Sender: TObject);
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure FormShow (Sender: TObject);
    procedure SetButtonState;
  public
    function  OnValidateFields(AUsers : TObjectList) : boolean;
  end;

var
  UserCreationDialog: TUserCreationDialog;

implementation

{$R *.dfm}


procedure TUserCreationDialog.CreateMemberObjects;
const OPNAME = 'TUserCreationDialog.CreateMemberObjects';
begin
  try
    OnShow        := FormShow;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUserCreationDialog.DestroyMemberObjects;
const OPNAME = 'TUserCreationDialog.DestroyMemberObjects';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUserCreationDialog.FormShow(Sender: TObject);
const OPNAME = 'TUserCreationDialog.FormShow';
begin
  try
    SetButtonState;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TUserCreationDialog.OnValidateFields(AUsers : TObjectList) : boolean;
const OPNAME = 'TUserCreationDialog.OnValidateFields';
var
  LUser  : TUser;
  LIndex : integer;
begin
  Result := False;
  try
    if (AUsers <> nil) then
    begin
      for LIndex := 0 to AUsers.Count - 1 do
      begin
        LUser := TUser(AUsers.Items[LIndex]);
        if (LUser.UserId = Trim(EdtUserId.Text)) then
        begin
          MessageDlg(FAppModules.Language.GetString('AccessControl.UserIDExist'), mtInformation, [mbOK], 0);
          Result := False;
          Exit;
        end;
        Result := True;
      end;
    end;

    if Trim(EdtPassword.Text) <> Trim(EdtConfirmPassword.Text) then
    begin
      MessageDlg(FAppModules.Language.GetString('AccessControl.ChangingPasswordFailed'), mtInformation, [mbOK], 0);
      Result := False;
    end
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUserCreationDialog.BtnOKClick(Sender: TObject);
const OPNAME = 'TUserCreationDialog.BtnOKClick';
begin
  try
    ModalResult := mrOk;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUserCreationDialog.btnCancelClick(Sender: TObject);
const OPNAME = 'TUserCreationDialog.btnCancelClick';
begin
  try
    close;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TUserCreationDialog.SetButtonState;
const OPNAME = 'TUserCreationDialog.SetButtonState';
begin
  try
    BtnOK.Enabled := (Trim(EdtUserId.Text)          <> '') and
                     (Trim(EdtPassword.Text)        <> '') and
                     (Trim(EdtConfirmPassword.Text) <> '');
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUserCreationDialog.OnEditChange(Sender: TObject);
const OPNAME = 'TUserCreationDialog.OnEditChange';
begin
  try
    SetButtonState;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
