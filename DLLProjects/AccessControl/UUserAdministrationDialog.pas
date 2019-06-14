unit UUserAdministrationDialog;

interface

uses
  // DWAF
  UUser,
  UAbstractObject,
  UAbstractComponent,

  Windows,
  Messages,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Contnrs,
  ComCtrls,
  ExtCtrls,
  Buttons,
  CheckLst,
  UUserCreationDialog;

type
  TDialogMode = (dlgmNone,dlgmEdit,dlgmNew,dlgmDelete);
  TUserAdministrationDialog = class(TAbstractForm)
    FPanel1           : TPanel;
    pnlMessage        : TPanel;
    FUserTreeView     : TTreeView;
    FGroupBox1        : TGroupBox;
    FUserIDLabel      : TLabel;
    FPasswordLabel    : TLabel;
    FUSerlabel        : TLabel;
    FPassLabel        : TLabel;
    FInitialsLabel    : TLabel;
    FFirstNameLabel   : TLabel;
    FSecondNameLabel  : TLabel;
    FLastNameLabel    : TLabel;
    FLanguageLabel    : TLabel;
    FUserRightsLabel  : TLabel;
    FUserTypeLabel    : TLabel;
    EdtUserId         : TEdit;
    EdtPassword       : TEdit;
    EdtUser           : TEdit;
    EdtPass           : TEdit;
    EdtInitials       : TEdit;
    EdtFirstName      : TEdit;
    EdtSecondName     : TEdit;
    EdtLastName       : TEdit;
    CbxLanguage       : TComboBox;
    CbxUserType       : TComboBox;
    CbxUserRights     : TComboBox;
    ChkModel          : TCheckListBox;
    btnSave           : TSpeedButton;
    btnClose          : TSpeedButton;
    btnDelete         : TSpeedButton;
    btnAdd            : TSpeedButton;
    btnValidate       : TButton;
    GroupBox1         : TGroupBox;
    chkStudy          : TCheckBox;
    chkLogOn          : TCheckBox;
    btnPasswordChange : TSpeedButton;

    procedure btnCloseClick(Sender: TObject);
    procedure OnEditChange(Sender: TObject);
    procedure DoOnKeypress ( Sender: TObject; var Key : Char );
    procedure FUserTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure ChkModelClick(Sender: TObject);
    procedure chkLogOnClick(Sender: TObject);
    procedure chkStudyClick(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,NewHeight: Integer; var Resize: Boolean);
  protected
    FEditmode   : boolean;
    FDialogMode : TDialogMode;
    FAdminUser  : TUser;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure SetButtonState;
    procedure SetEditColours;
    procedure PopulateCurrentUser(AUser: TUser);
    procedure PopulateUserModels(AUser: TUser);
    procedure PopulateUserRights(AUser: TUser);
    procedure PopulateUserTypes(AUser: TUser);
    procedure SetAdminUser(AUser: TUser);
    procedure ClearUserControls;
  public
    procedure PopulateTreeView(AUsersList: TObjectList);
    function  GetSelectedModels : string;
    procedure SelectUserNode(AUser: TUser);
    property AdminUser  : TUser  read FAdminUser write SetAdminUser;
    property DialogMode : TDialogMode read FDialogMode;
    property EditMode   : Boolean read FEditmode write FEditmode;
  end;

var
  UserAdministrationDialog: TUserAdministrationDialog;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

{$R *.dfm}

{ TUserAdministrationDialog }

procedure TUserAdministrationDialog.CreateMemberObjects;
const OPNAME = 'TUserAdministrationDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FAdminUser:= nil;
    EditMode := False;
    FDialogMode := dlgmNone;
  except on E : Exception do HandleError(e,OPNAME) end;;
end;

procedure TUserAdministrationDialog.DestroyMemberObjects;
const OPNAME = 'TUserAdministrationDialog.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E : Exception do HandleError(e,OPNAME) end;;
end;

procedure TUserAdministrationDialog.SetAdminUser(AUser: TUser);
const OPNAME = 'TUserAdministrationDialog.SetAdminUser';
begin
  try
    FAdminUser := AUser;
    FUserTreeView.Items.Clear;
    if Assigned(AUser) then
    begin
      EdtUserId.Text       := AUser.UserId;
      EdtPassword.Text     := AUser.Password;
      ChkModel.Items.CommaText := AUser.UserModel;
    end
    else
    begin
      EdtUserId.Text       := '';
      EdtPassword.Text     := '';
    end;
    SetButtonState;
  except on E : Exception do HandleError(e,OPNAME) end;;
end;

procedure TUserAdministrationDialog.SetButtonState;
const OPNAME = 'TUserAdministrationDialog.SetButtonState';
var
  LUser : TUser;
begin
  try
    EdtUserId.Enabled     := True;
    EdtPassword.Enabled   := True;

    btnValidate.Enabled   := False;

    btnDelete.Enabled     := False;
    btnAdd.Enabled        := False;
    chkLogOn.Enabled      := False;
    chkStudy.Enabled      := False;

    EdtUser.ReadOnly        := True;
    EdtPass.ReadOnly        := True;
    EdtLastName.ReadOnly    := True;
    EdtSecondName.ReadOnly  := True;
    EdtFirstName.ReadOnly   := True;
    EdtInitials.ReadOnly    := True;
    CbxUserType.Enabled     := False;
    CbxUserRights.Enabled   := False;
    CbxLanguage.Enabled     := False;
    ChkModel.Enabled        := False;

    btnClose.Enabled        := True;
    btnSave.Enabled         := False;
    btnPasswordChange.Visible := False;

    btnValidate.Enabled     := (Trim(EdtUserId.Text)     <> '') and
                               (Trim(EdtPassword.Text)   <> '');

    btnAdd.Enabled          :=  FUserTreeView.SelectionCount > 0;

    Case FDialogMode of
      dlgmEdit,dlgmNew :
      begin
         if (FUserTreeView.Selected <> nil) then
         begin
           LUser := TUser(FUserTreeView.Selected.Data);
           if (LUser <> nil) then
           begin
            EdtUserId.Enabled         := True;
            EdtPassword.Enabled       := True;
            CbxUserType.Enabled       := True;
            CbxUserRights.Enabled     := True;
            CbxLanguage.Enabled       := True;
            EdtUser.ReadOnly          := True;
            EdtPass.ReadOnly          := True;
            EdtLastName.ReadOnly      := False;
            EdtSecondName.ReadOnly    := False;
            EdtFirstName.ReadOnly     := False;
            EdtInitials.ReadOnly      := False;
            ChkModel.Enabled          := True;
            chkLogOn.Enabled          := True;
            chkStudy.Enabled          := True;
            btnAdd.Enabled            := True;

            btnPasswordChange.Visible :=  AdminUser.UserRights = 100;
            btnSave.Enabled           :=  (Trim(EdtUserId.Text)     <> LUser.UserId)   or
                                          (Trim(EdtPassword.Text)   <> LUser.Password)   or
                                          (Trim(EdtInitials.Text)   <> LUser.LastName)   or
                                          (Trim(EdtLastName.Text)   <> LUser.LastName)   or
                                          (Trim(EdtSecondName.Text) <> LUser.SecondName) or
                                          (Trim(EdtFirstName.Text)  <> LUser.FirstName) or
                                          (CbxLanguage.ItemIndex    <> CbxLanguage.Items.IndexOf(LUser.PreferedLanguage)) or
                                          (CbxUserRights.ItemIndex  <> CbxUserRights.Items.IndexOfObject(TObject(Ord(LUser.UserRights)))) or
                                          (CbxUserType.ItemIndex    <> CbxUserType.Items.IndexOfObject(TObject(Ord(LUser.UserType)))) or
                                          (chkStudy.Checked         <> LUser.AutoSelectStudy) or
                                          (chkLogOn.Checked         <> LUser.AutoLogOn) or
                                          (LUser.UserModel          <> GetSelectedModels);

            btnDelete.Enabled := (FUserTreeView.Items.Count > 0) and
                                 (FUserTreeView.Selected <> nil) and
                                 (FUserTreeView.Selected.Level = 1);
          end;
        end;
      end;
      dlgmDelete:
      begin
         btnSave.Enabled  := (Trim(EdtUser.Text)       <> '') and
                             (Trim(EdtPass.Text)       <> '');
      end;
    end;
    SetEditColours;
  except on E : Exception do HandleError(e,OPNAME) end;;
end;

procedure TUserAdministrationDialog.SetEditColours;
const OPNAME = 'TUserAdministrationDialog.SetEditColours';
var
  LEnabledColour,
  LDisabledColour: TColor;
begin
  try
    LEnabledColour := clWindow;
    LDisabledColour:= clBtnFace;

    if FUserTreeView.Enabled then FUserTreeView.Color := LEnabledColour
      else FUserTreeView.Color := LDisabledColour;

    if EdtUserId.Enabled then EdtUserId.Color := LEnabledColour
      else EdtUserId.Color := LDisabledColour;

    if EdtPassword.Enabled then EdtPassword.Color := LEnabledColour
      else EdtPassword.Color := LDisabledColour;

    if CbxUserType.Enabled then CbxUserType.Color := LEnabledColour
      else CbxUserType.Color := LDisabledColour;

    if CbxUserRights.Enabled then CbxUserRights.Color := LEnabledColour
      else CbxUserRights.Color := LDisabledColour;

    if CbxLanguage.Enabled then CbxLanguage.Color := LEnabledColour
      else CbxLanguage.Color := LDisabledColour;

    if EdtLastName.ReadOnly then EdtLastName.Color := LDisabledColour
      else EdtLastName.Color := LEnabledColour;

    if EdtSecondName.ReadOnly then EdtSecondName.Color := LDisabledColour
      else EdtSecondName.Color := LEnabledColour;

    if EdtFirstName.ReadOnly then EdtFirstName.Color := LDisabledColour
      else EdtFirstName.Color := LEnabledColour;

    if EdtInitials.ReadOnly then EdtInitials.Color := LDisabledColour
      else EdtInitials.Color := LEnabledColour;

    if EdtPass.ReadOnly then EdtPass.Color := LDisabledColour
      else EdtPass.Color := LEnabledColour;

    if EdtUser.ReadOnly then EdtUser.Color := LDisabledColour
      else EdtUser.Color := LEnabledColour;

  except on E : Exception do HandleError(e,OPNAME) end;;
end;

procedure TUserAdministrationDialog.ClearUserControls;
const OPNAME = 'TUserAdministrationDialog.ClearUserControls';
var
  LIndex: integer;
begin
  try
    EdtUser.Text       := '';
    EdtPass.Text       := '';
    EdtInitials.Text   := '';
    EdtFirstName.Text  := '';
    EdtSecondName.Text := '';
    EdtLastName.Text   := '';
    CbxLanguage.ItemIndex   := -1;
    CbxUserRights.ItemIndex := -1;
    CbxUserType.ItemIndex   := -1;;
    for LIndex := 0 to ChkModel.Count - 1 do
      ChkModel.Checked[LIndex] := False;
    SetButtonState;
    EdtUser.SetFocus;
  except on E : Exception do HandleError(e,OPNAME) end;;
end;

procedure TUserAdministrationDialog.PopulateCurrentUser(AUser: TUser);
const OPNAME = 'TUserAdministrationDialog.PopulateCurrentUser';
begin
  try
    if(AUser <> nil) then
    begin
      EdtUser.Text          := AUser.UserId;
      EdtPass.Text          := AUser.Password;
      EdtInitials.Text      := AUser.Initials;
      EdtLastName.Text      := AUser.LastName;
      EdtSecondName.Text    := AUser.SecondName;
      EdtFirstName.Text     := AUser.FirstName;
      CbxLanguage.ItemIndex := CbxLanguage.Items.IndexOf(AUser.PreferedLanguage);
      PopulateUserTypes(AUser);
      PopulateUserRights(AUser);
      chkLogOn.Checked := AUser.AutoLogOn;
      chkStudy.Checked := AUser.AutoSelectStudy;

    end;
 except on E : Exception do HandleError(e,OPNAME) end;;
end;

procedure TUserAdministrationDialog.btnCloseClick(Sender: TObject);
const OPNAME = 'TUserAdministrationDialog.btnCloseClick';
begin
  try
    Close;
  except on E : Exception do HandleError(e,OPNAME) end;
end;

procedure TUserAdministrationDialog.OnEditChange(Sender: TObject);
const OPNAME = 'TUserAdministrationDialog.OnEditChange';
begin
  try
    //if (sender <> EdtUserId) or (Sender <> EdtPassword) then
      SetButtonState;
  except on E : Exception do HandleError(e,OPNAME) end;;
end;

procedure TUserAdministrationDialog.PopulateTreeView(AUsersList: TObjectList);
const OPNAME = 'TUserAdministrationDialog.PopulateTreeView';
var
  LIndex    : integer;
  LUser     : TUser;
  LMainNode : TTreeNode;
begin
  try
    FUserTreeView.Items.Clear;
    ClearUserControls;
    if (AUsersList <> nil) then
    begin
      begin
        LMainNode := FUserTreeView.Items.AddObject(nil,AdminUser.UserId,AdminUser);
        for LIndex := 0 to AUsersList.Count -1 do
        begin
          LUser := TUser(AUsersList.Items[LIndex]);
          FUserTreeView.Items.AddChildObject(LMainNode,LUser.UserId,LUser);
        end;
      end;

      if(FUserTreeView.Items.Count > 0) then
      begin
        FUserTreeView.Selected := FUserTreeView.Items[0];
      end;
    end;

    for LIndex := 0 to ChkModel.Items.Count -1  do
    begin
      ChkModel.Checked[LIndex] := False;
    end;

  except on E : Exception do HandleError(e,OPNAME) end;;
end;

procedure TUserAdministrationDialog.FUserTreeViewChange(Sender: TObject; Node: TTreeNode);
const OPNAME = 'TUserAdministrationDialog.FUserTreeViewChange';
var
  LUser: TUser;
begin
  try
    ClearUserControls;
    if (Node <> nil) and (Node.Data <> nil) then
    begin
      FDialogMode := dlgmEdit;
      LUser := TUser(Node.Data);
      PopulateCurrentUser(LUser);
      PopulateUserModels(LUser);
      SetButtonState;
    end;
  except on E : Exception do HandleError(e,OPNAME) end;;
end;

procedure TUserAdministrationDialog.PopulateUserModels(AUser: TUser);
const OPNAME = 'TUserAdministrationDialog.PopulateUserModels';
var
  LItemIndex,
  LIndex     : integer;
  LUserModels     : TStringList;
begin
  try
    for LIndex := 0 to ChkModel.Items.Count -1  do
    begin
      ChkModel.Checked[LIndex] := False;
    end;

    if Assigned(AUser) then
    begin
      LUserModels := TStringList.Create;
      try
        LUserModels.CommaText := AUser.UserModel;
        for LIndex := 0 to LUserModels.Count -1 do
        begin
          LItemIndex := ChkModel.Items.IndexOf(LUserModels[LIndex]);
          ChkModel.Checked[LItemIndex] := True;
        end;
      finally
        LUserModels.Free;
      end;
    end;
  except on E : Exception do HandleError(e,OPNAME) end;;
end;

procedure TUserAdministrationDialog.PopulateUserRights(AUser: TUser);
const OPNAME = 'TUserAdministrationDialog.PopulateUserRights';
var
  LRightsDescr : string;
  LAdminRights,
  LRights,
  LCount,
  LIndex: integer;
begin
  try
    LRights := AUser.UserRights;
    LAdminRights := FAdminUser.UserRights;
    CbxUserRights.Items.Clear;
    LIndex := -1;
    for LCount := High(TheUserRights) downto Low(TheUserRights) do
    begin
      if(LAdminRights = TheUserRights[LCount]) then
      begin
        LIndex := LCount;
        Break;
      end;
    end;
    if(LIndex >= 0) then
    begin
    for LCount := Low(TheUserRights) to LIndex do
      begin
        LRightsDescr := FAdminUser.GetUserRightsDescr(TheUserRights[LCount]);
        CbxUserRights.Items.AddObject(LRightsDescr,TObject(TheUserRights[LCount]));
        CbxUserRights.ItemIndex := CbxUserRights.Items.IndexOfObject(TObject(LRights));
      end;
    end;
  except on E : Exception do HandleError(e,OPNAME) end;;
end;

procedure TUserAdministrationDialog.PopulateUserTypes(AUser: TUser);
const OPNAME = 'TUserAdministrationDialog.PopulateUserTypes';
begin
  try
    CbxUserType.Items.Clear;
    CbxUserType.Items.AddObject('Standard User',TObject(1));
    CbxUserType.Items.AddObject('Expert User',TObject(2));
    CbxUserType.ItemIndex := CbxUserType.Items.IndexOfObject(TObject(Ord(AUser.UserType)));
  except on E : Exception do HandleError(e,OPNAME) end;;
end;


procedure TUserAdministrationDialog.DoOnKeypress (Sender: TObject; var Key : Char);
const OPNAME = 'TUserAdministrationDialog.DoOnKeypress';
begin
  try
    EditMode := True;
    SetButtonState;
  except on E: Exception do HandleError (E, OPNAME) end;
end;

function TUserAdministrationDialog.GetSelectedModels: string;
const OPNAME = 'TUserAdministrationDialog.GetSelectedModels';
var
  LIndex  : integer;
  LModels : TStringList;
begin
  Result := '';
  try
    LModels := TStringList.Create;
    try
      for LIndex := 0 to ChkModel.Items.Count - 1 do
      begin
        if (ChkModel.Checked[LIndex]) then
        begin
          LModels.Add(ChkModel.Items.Strings[LIndex]);
          Result := LModels.CommaText;
        end;
      end;
     finally;
       LModels.Free;
     end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TUserAdministrationDialog.ChkModelClick(Sender: TObject);
const OPNAME = 'TUserAdministrationDialog.ChkModelClick';
begin
  try
    FDialogMode := dlgmEdit;
    SetButtonState;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TUserAdministrationDialog.SelectUserNode(AUser: TUser);
const OPNAME = 'TUserAdministrationDialog.SelectUserNode';
var
  LIndex: integer;
begin
  try
    FUserTreeView.Selected := nil;
    if Assigned(AUser) then
    begin
      for LIndex := 0 to FUserTreeView.Items.Count -1 do
      begin
        if(FUserTreeView.Items[LIndex].Data = AUser) then
        begin
          FUserTreeView.Selected := FUserTreeView.Items[LIndex];
          Break;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TUserAdministrationDialog.chkLogOnClick(Sender: TObject);
const OPNAME = 'TUserAdministrationDialog.chkLogOnClick';
begin
  try
    FDialogMode := dlgmEdit;
    SetButtonState;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TUserAdministrationDialog.chkStudyClick(Sender: TObject);
const OPNAME = 'TUserAdministrationDialog.chkStudyClick';
begin
  try
    FDialogMode := dlgmEdit;
    SetButtonState;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TUserAdministrationDialog.FormCanResize(Sender: TObject;
  var NewWidth, NewHeight: Integer; var Resize: Boolean);
const OPNAME = 'TUserAdministrationDialog.FormCanResize';
var
  LDefaultWidth : Integer;
begin
  try
    LDefaultWidth := 511;

    if (NewWidth <= LDefaultWidth ) then   //565
      Resize := false;

    if (NewHeight <=  419) then
      Resize := false;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
