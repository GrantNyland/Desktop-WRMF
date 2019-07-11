//
//
//  UNIT      : Contains TAccessControlManager Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/22
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UAccessControlManager;

interface

uses
  Contnrs,
  System.UITypes,
  UUser,
  UMenuItemManager,
  UUserAdministrationDialog,
  UUserCreationDialog,
  UUserPasswordChangeDialog,
  UAbstractObject;

type
  TAccessControlMenuItemManager = class(TMenuItemManager)
  public
    procedure AddMenuItems; override;
  end;

  TAccessControlManager = class(TAbstractAccessControlManager)
  protected
    FAccessControlMenuItemManager  : TAccessControlMenuItemManager;
    FUserAdminDialog               : TUserAdministrationDialog;
    FCurrentUserList               : TObjectList;
    FAllUserList                   : TObjectList;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function LoadFromIniFile(var AUserID,APassword: string):boolean;
    procedure OnValidateUserAdministrationClick(Sender: TObject);
    procedure OnCreateNewUser(Sender: TObject);
    procedure OnDeleteSelectedUser(sender : TObject);
    procedure OnUpdateUser(Sender : TObject);
    procedure OnPasswordChange(Sender : TObject);
    function GetAllUserList: boolean;
    function GetUserFromUserList(AUserID,APassword : string) : TUser;
    function GetCurrentUserList(ACreatedBy : string):boolean;
    function LogonUser(AUser: TUser; AUserID,APassword : string) : boolean;
  public
    procedure DoLogOn(var AUser: TAbstractUser); override;
    procedure DoUserAdministration(AUser: TAbstractUser); override;
    procedure DoLogOff(var AUser: TAbstractUser); override;
    procedure DoAutoLogOn(var AUser: TAbstractUser); override;

    procedure HideMenus; override;
  end;

implementation

uses
  SysUtils,
  Windows,
  Classes,
  Controls,
  Dialogs,
  UDataSetType,
  UAbstractComponent,
  UAccessControlDialog,
  UMainMenuEventType,
  USystemModelLinkClasses,
  UErrorHandlingOperations, DB, StdCtrls, ComCtrls, CheckLst, Math;

{ TAccessControlMenuItemManager }

const
  CUserAdministration : array[0..1] of WideString = ('File','UserAdministration');
  CLogOn              : array[0..1] of WideString = ('File','LogOn');
  CLogOff             : array[0..1] of WideString = ('File','LogOff');
  CLogOffSep          : array[0..1] of WideString = ('File','LogOffSep');

procedure TAccessControlMenuItemManager.AddMenuItems;
const OPNAME = 'TAccessControlMenuItemManager.AddMenuItems';
begin
  try
    AddMenuItemEntry(CUserAdministration, 300, CmeUserAdministration);
    AddMenuItemEntry(CLogOn,301, CmeLogOn);
    AddMenuItemEntry(CLogOff,    302, CmeLogOff);
    AddMenuItemEntry(CLogOffSep, 303, CmeSeparator);
    FAppModules.SetMenuItem(CLogOff, msDisable);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TAccessControlManager }

procedure TAccessControlManager.CreateMemberObjects;
const OPNAME = 'TAccessControlManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FAccessControlMenuItemManager := TAccessControlMenuItemManager.Create(FAppModules);
    FOwnedAppObjects.Add(FAccessControlMenuItemManager);

    FAllUserList     := TObjectList.Create(True);
    FCurrentUserList := TObjectList.Create(False);
    FUserAdminDialog := nil;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TAccessControlManager.DestroyMemberObjects;
const OPNAME = 'TAccessControlManager.DestroyMemberObjects';
begin
  try
    FCurrentUserList.Free;
    FAllUserList.Free;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAccessControlManager.LoadFromIniFile(var AUserID,APassword: string):boolean;
const OPNAME = 'TAccessControlManager.LoadFromIniFile';
begin
  Result := false;
  try
    AUserID   := FAppModules.IniFile.ReadString('USER','UserId','');
    APassword := FAppModules.IniFile.ReadString('USER','Password','');
    Result := True;

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TAccessControlManager.DoLogOn(var AUser: TAbstractUser);
const
  OPNAME = 'TAccessControlManager.DoLogOn';
var
  LLogonDialog   : TAccessControlDialog;
  LUserID,
  LPassword      : string;
  LUser          : TUser;
begin
  try
    if (not Assigned(AUser)) then
      AUser := TUser.Create(FAppModules);
    LLogonDialog := TAccessControlDialog.Create(nil,FAppModules);
    LUser        := TUser.Create(FAppModules);
    try
      LLogonDialog.ShowModal;
      if LLogonDialog.ModalResult = mrOK then
      begin
        LUserID := Trim(LLogonDialog.FEditUserID.Text);
        if LUserID <> '' then
        begin
          LPassword := Trim(LLogonDialog.FEditPassword.Text);
          if LogonUser(LUser,LUserID,LPassword) then
          begin
            TUser(AUser).AssignFrom(LUser);
            AUser.SetLoggedOn(True);
            FAppModules.SetMenuItem(CLogOn, msDisable);
            FAppModules.SetMenuItem(CLogOff, msEnable);
            FAppModules.MainForm.RefreshState;
          end;
        end;
      end;
    finally
      LUser.Free;
      LLogonDialog.Free;
    end;

  // Handle Exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAccessControlManager.LogonUser(AUser: TUser; AUserID,APassword : string): boolean;
const OPNAME = 'TAccessControlManager.LogonUser';
var
  LUsersDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    if (AUser <> nil) then
    begin
      AUser.Reset;
      FAppModules.Database.CreateDataset(integer(dtUserAdministration), LUsersDataSet);
      try
        LUsersDataSet.DataSet.Active := True;
        while not LUsersDataSet.DataSet.Eof do
        begin
          if (Trim(LUsersDataSet.DataSet.FieldByName('UserID').AsString) = AUserID) then
          begin
            AUser.InitialiseFromDataset(LUsersDataSet);
            if(AUser.Password = APassword) then
            begin
              Result := True;
              Exit;
            end
            else
            begin
              AUser.Reset;
              MessageDlg(FAppModules.Language.GetString('AccessControl.SetPassword') , mtInformation, [mbOK], 0);
              Exit;
            end
          end;
          LUsersDataSet.DataSet.Next;
        end;
        MessageDlg(FAppModules.Language.GetString('AccessControl.RecordsNotFound') , mtInformation, [mbOK], 0);
      finally
        LUsersDataSet.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAccessControlManager.DoLogOff(var AUser: TAbstractUser);
const OPNAME = 'TAccessControlManager.DoLogOff';
begin
  try
    if (not Assigned(AUser)) then
      AUser := TUser.Create(FAppModules);
    TUser(AUser).Reset;
    if(FAppModules.MainForm <> nil) then
    begin
      FAppModules.SetMenuItem(CLogOn, msEnable);
      FAppModules.SetMenuItem(CLogOff, msDisable);
      FAppModules.MainForm.RefreshState;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TAccessControlManager.DoAutoLogOn(var AUser: TAbstractUser);
const
  OPNAME = 'TAccessControlManager.DoAutoLogOn';
var
  LUser: TUser;
  LUserId,
  LPassword:string;
begin
  try
    if (not Assigned(AUser)) then
      AUser := TUser.Create(FAppModules);
    if (not LoadFromIniFile(LUserId, LPassword)) then
      Exit;
    LUser := TUser.Create(FAppModules);
    try
      if LogonUser(LUser,LUserID,LPassword) then
      begin
        TUser(AUser).AssignFrom(LUser);
        AUser.SetLoggedOn(True);
        FAppModules.GlobalData.SetAutoSelectUser(True);
        FAppModules.SetMenuItem(CLogOn, msDisable);
        FAppModules.SetMenuItem(CLogOff, msEnable);
      end;
    finally
      LUser.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAccessControlManager.HideMenus;
const OPNAME = 'TAccessControlManager.HideMenus';
begin
  try
    if FAppModules.GlobalData.AutoSelectUser then
    begin
      //FAppModules.SetMenuItem(CLogOn, msHide);
      //FAppModules.SetMenuItem(CLogOff, msHide);
    end;

  // Handle Exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAccessControlManager.DoUserAdministration(AUser: TAbstractUser);
const OPNAME = 'TAccessControlManager.DoUserAdministration';
var
  LAppUser:TUser;
begin
  try
    FUserAdminDialog := TUserAdministrationDialog.Create(nil,FAppModules);
    try
      FAllUserList.Clear;
      FCurrentUserList.Clear;
      GetAllUserList;
      FUserAdminDialog.AdminUser := TUser(AUser);
      FUserAdminDialog.btnValidate.OnClick := OnValidateUserAdministrationClick;
      FUserAdminDialog.btnDelete.OnClick   := OnDeleteSelectedUser;
      FUserAdminDialog.btnSave.OnClick     := OnUpdateUser;
      FUserAdminDialog.btnAdd.OnClick      := OnCreateNewUser;
      FUserAdminDialog.btnPasswordChange.OnClick := OnPasswordChange;
      FUserAdminDialog.ShowModal;
      if(FAppModules.User() <> nil) and (FAppModules.User.LoggedOn) then
      begin
        LAppUser := GetUserFromUserList(FAppModules.User.UserId,FAppModules.User.Password);
        if Assigned(LAppUser) then
        begin
          TUser(FAppModules.User).AssignFrom(LAppUser);
          TUser(FAppModules.User).SetLoggedOn(True);
        end;
      end;
    finally
      FAllUserList.Clear;
      FCurrentUserList.Clear;
      FUserAdminDialog.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAccessControlManager.OnValidateUserAdministrationClick(Sender: TObject);
const OPNAME = 'TAccessControlManager.OnValidateUserAdministrationClick';
var
  LUserId    : string;
  LPassword  : string;
  LLocalUser : TUser;
begin
  try
    FUserAdminDialog.FUserTreeView.Items.Clear;
    FUserAdminDialog.pnlMessage.Caption := '';
    LUserId := Trim(FUserAdminDialog.EdtUserId.Text);
    LPassword := Trim(FUserAdminDialog.EdtPassword.Text);
    LLocalUser := GetUserFromUserList(LUserId,LPassword);

    if not Assigned(LLocalUser) then Exit;

    FUserAdminDialog.AdminUser := LLocalUser;
    if GetCurrentUserList(LLocalUser.UserId) then
    begin
      FUserAdminDialog.PopulateTreeView(FCurrentUserList);
      FUserAdminDialog.EditMode := False;
      FUserAdminDialog.SelectUserNode(LLocalUser);
    end
   except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAccessControlManager.GetUserFromUserList(AUserID,APassword : string) : TUser;
const OPNAME = 'TAccessControlManager.GetUserFromUserList';
var
  LIndex : integer;
  LUser  : TUser;
begin
  Result := nil;
  try
    if(FAllUserList = nil) then
      raise Exception.Create('All User List parameter is not yet assigned.');

    for LIndex := 0 to FAllUserList.Count - 1 do
    begin
     LUser := TUser(FAllUserList.Items[LIndex]);
     if (LUser.UserId = AUserID) then
     begin
       if(LUser.Password <> APassword) then
       begin
         MessageDlg(FAppModules.Language.GetString('AccessControl.SetPassword') , mtInformation, [mbOK], 0);
         Exit;
       end
       else
       begin
         Result := LUser;
         Exit;
       end;
     end;
    end;
    MessageDlg(FAppModules.Language.GetString('AccessControl.RecordsNotFound') , mtInformation, [mbOK], 0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TAccessControlManager.GetCurrentUserList(ACreatedBy: string): boolean;
const OPNAME = 'TAccessControlManager.GetCurrentUserList';
var
  LUser    : TUser;
  LIndex : integer;
begin
  Result := False;
  try
    FCurrentUserList.Clear;
    for LIndex := 0 to FAllUserList.Count-1 do
    begin
      LUser := TUser(FAllUserList.Items[LIndex]);
      if(LUser.CreatedBy = ACreatedBy) then
        FCurrentUserList.Add(LUser);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAccessControlManager.OnDeleteSelectedUser(sender: TObject);
const OPNAME = 'TAccessControlManager.OnDeleteSelectedUser';
var
  LUser: TUser;
begin
  try
    if assigned(FUserAdminDialog.FUserTreeView.Selected) then
    begin
      LUser := TUser(FUserAdminDialog.FUserTreeView.Selected.Data);
      if(MessageDlg(FAppModules.Language.GetString('AccessControl.User') +  LUser.UserId +  FAppModules.Language.GetString('AccessControl.ConfirmUserDeletion') , mtConfirmation, mbOKCancel,0 )= mrOK) then
      begin
        if LUser.DeleteUserFromDB  then
        begin
          FCurrentUserList.Delete(FCurrentUserList.IndexOf(LUser));
          FAllUserList.Delete(FAllUserList.IndexOf(LUser));
          FUserAdminDialog.PopulateTreeView(FCurrentUserList);
          FUserAdminDialog.SelectUserNode(FUserAdminDialog.AdminUser);
        end;
      end;
    end;
  except on E : Exception do HandleError(e,OPNAME) end;
end;

procedure TAccessControlManager.OnUpdateUser(Sender: TObject);
const OPNAME = 'TAccessControlManager.OnUpdateUser';
var
  LUser       : TUser;
  LPassword,
  LInitials,
  LFirstName,
  LSecondName,
  LLastName,
  LLanguage   : string;
  LUserRights : integer;
  LUserType   : TUserType;
  LModel      : string;
  LAutoLogOn  : Boolean;
  LAutoSelectStudy: Boolean;

begin
  try
    if assigned(FUserAdminDialog.FUserTreeView.Selected) then
    begin
      LUser       := TUser(FUserAdminDialog.FUserTreeView.Selected.Data);
      LPassword   := Trim(FUserAdminDialog.EdtPass.Text);
      LInitials   := Trim(FUserAdminDialog.EdtInitials.Text);
      LFirstName  := Trim(FUserAdminDialog.EdtFirstName.Text);
      LSecondName := Trim(FUserAdminDialog.EdtSecondName.Text);
      LLastName   := Trim(FUserAdminDialog.EdtLastName.Text);
      LLanguage   := FUserAdminDialog.CbxLanguage.Text;
      LUserRights := integer(FUserAdminDialog.CbxUserRights.Items.Objects[FUserAdminDialog.CbxUserRights.ItemIndex]);
      LUserType   := TUserType(integer(FUserAdminDialog.CbxUserType.Items.Objects[FUserAdminDialog.CbxUserType.ItemIndex]));
      LModel      := FUserAdminDialog.GetSelectedModels;
      LAutoLogOn  := FUserAdminDialog.chkLogOn.Checked;
      LAutoSelectStudy := FUserAdminDialog.chkStudy.Checked;

      if (LModel = '') then
      begin
        MessageDlg(FAppModules.Language.GetString('AccessControl.NoModelAssignedToUser') , mtInformation, [mbOK], 0);
        Exit;
      end;

      if ((LAutoSelectStudy = True) and (LAutoLogOn = False)) then
      begin
        MessageDlg(FAppModules.Language.GetString('AccessControl.StudyCannotBeSelected') , mtInformation, [mbOK], 0);
        Exit;
      end;


      if LUser.PopulateUser(LUser.UserId, LPassword, LInitials, LFirstName, LSecondName, LLastName,
                                 LLanguage, LUserRights, LUserType, LModel, LUser.CreatedBy, LAutoLogOn, LAutoSelectStudy) then
      begin
        if LUser.UpdateUserDetails then
          FUserAdminDialog.SelectUserNode(LUser);
      end;
    end;
  except on E : Exception do HandleError(e,OPNAME) end;
end;

procedure TAccessControlManager.OnPasswordChange(Sender: TObject);
const OPNAME = 'TAccessControlManager.OnPasswordChange';
var
  LUser                 : TUser;
  LPasswordChangeDialog : TUserPasswordChangeDialog;
begin
  try
    LPasswordChangeDialog := TUserPasswordChangeDialog.Create(nil,FAppModules);
    try
      LUser := TUser(FUserAdminDialog.FUserTreeView.Selected.Data);
      if (LUser <> nil) then
      begin
        LPasswordChangeDialog.EditOldPassword.Text := LUser.Password;
        LPasswordChangeDialog.ShowModal;
        if LPasswordChangeDialog.ModalResult = mrOK then
        begin
          if LPasswordChangeDialog.validatePassword then
          begin
            FUserAdminDialog.EdtPass.ReadOnly := False;
            FUserAdminDialog.EdtPass.Text := LPasswordChangeDialog.EditNewPassword.Text;
            FUserAdminDialog.EdtPass.ReadOnly := True;
          end;
        end;
      end;
    finally
      LPasswordChangeDialog.Free;
    end;
  except on E : Exception do HandleError(e,OPNAME) end;
end;


function TAccessControlManager.GetAllUserList: boolean;
const OPNAME = 'TAccessControlManager.GetAllUserList';
var
  LLocalUser    : TUser;
  LUsersDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtUser), LUsersDataSet);
    try
      FAllUserList.Clear;
      FCurrentUserList.Clear;
      if Assigned(LUsersDataSet) and
         Assigned(LUsersDataSet.DataSet()) then
      begin
        LUsersDataSet.DataSet.Active := True;
        while not LUsersDataSet.DataSet.Eof do
        begin
          LLocalUser := TUser.Create(FAppModules);
          LLocalUser.InitialiseFromDataset(LUsersDataSet);
          FAllUserList.Add(LLocalUser);
          LUsersDataSet.DataSet.Next;
        end;
        Result := True;
      end;
    finally
      LUsersDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAccessControlManager.OnCreateNewUser(Sender: TObject);
const OPNAME = 'TAccessControlManager.OnCreateNewUser';
var
  FUserCreationDialog : TUserCreationDialog;
  LLocalUser          : TUser;
  LUser               : TUser;
  LUserId,
  LPassword,
  LInitials,
  LFirstName,
  LSecondName,
  LLastName,
  LLanguage           : String;
  LUserRights         : integer;
  LUserType           : TUserType;
  LCreatedBy          : string;
  LModel              : string;
  LAutoLogOn          : Boolean;
  LAutoSelectStudy    : Boolean;
begin
  try
    FUserCreationDialog := TUserCreationDialog.Create(nil,FAppModules);
    try
      FUserCreationDialog.ShowModal;
      if FUserCreationDialog.ModalResult = mrOK then
      begin
        if (FUserCreationDialog.OnValidateFields(FAllUserList)) then
        begin
          LLocalUser := TUser.Create(FAppModules);
          LUserId     := Trim(FUserCreationDialog.EdtUserId.Text);
          LPassword   := Trim(FUserCreationDialog.EdtPassword.Text);
          if assigned(FUserAdminDialog.FUserTreeView.Selected) then
          begin
            LUser       := TUser(FUserAdminDialog.FUserTreeView.Selected.Data);
            LInitials   := '';
            LFirstName  := '';
            LSecondName := '';
            LLastName   := '';
            LLanguage   := LUser.PreferedLanguage;
            LUserType   := LUser.UserType;
            LModel      := LUser.UserModel;
            LAutoLogOn  := LUser.AutoLogOn;
            LAutoSelectStudy := LUser.AutoSelectStudy;
            LCreatedBy       := FUserAdminDialog.AdminUser.UserId;
//            if (LUser.UserRights = 100) then
//               LUserRights := 1;
//            else
//               LUserRights := LUser.UserRights;

            LUserRights := 1;
            if (LModel = '') then
            begin
              MessageDlg(FAppModules.Language.GetString('AccessControl.NoModelAssignedToUser') , mtInformation, [mbOK], 0);
              LLocalUser.Free;
              Exit;
            end;

            if ((LAutoSelectStudy = True) and (LAutoLogOn = False)) then
            begin
              MessageDlg(FAppModules.Language.GetString('AccessControl.StudyCannotBeSelected') , mtInformation, [mbOK], 0);
              LLocalUser.Free;
              Exit;
            end;

            if (LLocalUser.PopulateUser(LUserId, LPassword, LInitials, LFirstName, LSecondName, LLastName,
                                        LLanguage, LUserRights, LUserType, LModel,LCreatedBy,LAutoLogOn,LAutoSelectStudy)) then
            begin
              if LLocalUser.SaveUserToDB then
              begin
                FCurrentUserList.Add(LLocalUser);
                FAllUserList.Add(LLocalUser);
              end;
              FUserAdminDialog.PopulateTreeView(FCurrentUserList);
              FUserAdminDialog.SelectUserNode(LLocalUser);
            end;
          end;
        end;
      end;
    finally
      FUserCreationDialog.Free;
    end;
  except on E : Exception do HandleError(E,OPNAME) end;
end;


end.


