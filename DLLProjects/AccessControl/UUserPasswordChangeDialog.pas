unit UUserPasswordChangeDialog;

interface

uses
  UAbstractObject,
  UAbstractComponent,

  System.UITypes,
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  Buttons;


type
  TUserPasswordChangeDialog = class(TAbstractForm)
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    EditOldPassword: TEdit;
    EditNewPassword: TEdit;
    EditConfirmPassword: TEdit;
    btnClose: TBitBtn;
    btnOk: TBitBtn;
    procedure OnEditChange(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure FormShow (Sender: TObject);
    procedure SetButtonState;
  public
    function validatePassword : boolean;
  end;

var
  UserPasswordChangeDialog: TUserPasswordChangeDialog;

implementation

uses

UErrorHandlingOperations;

{$R *.dfm}

{ TFUserEditPasswordDilalog }

procedure TUserPasswordChangeDialog.CreateMemberObjects;
const OPNAME = 'TUserPasswordChangeDialog.CreateMemberObjects';
begin
  try
    OnShow  := FormShow;
    EditOldPassword.Enabled := False;
    EditOldPassword.Color   := clBtnFace;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TUserPasswordChangeDialog.DestroyMemberObjects;
const OPNAME = 'TUserPasswordChangeDialog.DestroyMemberObjects';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUserPasswordChangeDialog.FormShow(Sender: TObject);
const OPNAME = 'TUserPasswordChangeDialog.FormShow';
begin
  try
    SetButtonState;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUserPasswordChangeDialog.SetButtonState;
const OPNAME = 'TUserPasswordChangeDialog.SetButtonState';
begin
  try
    btnOk.Enabled := (Trim(EditNewPassword.Text)     <> '') and
                     (Trim(EditConfirmPassword.Text) <> '');

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUserPasswordChangeDialog.OnEditChange(Sender: TObject);
const OPNAME = 'TUserPasswordChangeDialog.OnEditChange';
begin
  try
    SetButtonState;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TUserPasswordChangeDialog.btnOkClick(Sender: TObject);
const OPNAME = 'TUserPasswordChangeDialog.btnOkClick';
begin
  try
    ModalResult := mrOk;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUserPasswordChangeDialog.validatePassword: boolean;
const OPNAME = 'TUserPasswordChangeDialog.validatePassword';
var
  LNewPassword,
  LConfirmPassword : string;
begin
  Result := FALSE;
  try
     LNewPassword := Trim(EditNewPassword.Text);
     LConfirmPassword := Trim(EditConfirmPassword.Text);
     if (LNewPassword <> LConfirmPassword) then
     begin
       MessageDlg(FAppModules.Language.GetString('AccessControl.ChangingPasswordFailed'), mtInformation, [mbOK], 0);
       Result := False;
     end
     else
        Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUserPasswordChangeDialog.btnCloseClick(Sender: TObject);
const OPNAME = 'TUserPasswordChangeDialog.btnCloseClick';
begin
  try
    Close;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
