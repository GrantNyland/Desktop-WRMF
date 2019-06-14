//
//  UNIT      : Contains TUser Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/16
//  COPYRIGHT : Copyright © 2002 DWAF
//
unit UUser;

interface

uses
  Classes,
  UAbstractObject;
const
  TheUserRights : array[0..3] of integer = (1,2,3,100);

type
  //TUserRights = (None, ReadOnly, WriteOnly, ReadAndWrite, SysAdmin);
  TUser = class(TAbstractUser)
  protected
    FUserId,
    FPassword,
    FInitials,
    FFirstName,
    FSecondName,
    FLastName,
    FCreatedBy,
    FPreferedLanguage: String;
    FUserModel : TStringList;
    FUserRights : integer;
    FLoggedOn: Boolean;
    FUserType : TUserType;
    FAutoLogOn: Boolean;
    FAutoSelectStudy: Boolean;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function DeCrypt(const ATheString: String): String;
    function EnCrypt(const ATheString: String): String;
    function GetUserModel: string;
    procedure RemoveUnlicencedModels;
    procedure AddPreProcessorModels;
  public
    procedure SetUserRights(ARights: integer); override;
    procedure SetUserType(AType: TUserType); override;
    procedure SetLoggedOn(ALoggedOn : boolean); override;
    procedure ReloadData; override;

    function UserId : String; override;
    function Password : String; override;
    function Initials : String; override;
    function FirstName : String; override;
    function SecondName : String; override;
    function LastName : String; override;
    function PreferedLanguage : String; override;
    function UserRights : integer; override;
    function LoggedOn : boolean; override;
    function UserType : TUserType; override;
    function UserModelCount: integer;override;
    function UserModelByIndex(AIndex: integer): string;override;
    function CreatedBy: string;
    function AutoLogOn: Boolean;override;
    function AutoSelectStudy: Boolean;override;

    procedure Reset;
    procedure AssignFrom(AUser: TUser);
    function PopulateUser(AUserId, APassword, AInitials,AFirstName,ASecondName,ALastName,ALanguage: string;
                          AUserRights : integer;AUserType: TUserType; AModelName,ACreatedBy  : string;
                          AAutoLogOn,AAutoSelectStudy: Boolean) : boolean;
    function InitialiseFromDataset(AUserDataset: TAbstractModelDataset): boolean;
    function UpdateUserDetails : boolean;
    function SaveUserToDB : boolean;
    function DeleteUserFromDB : boolean;
    function GetUserRightsDescr(AUserRights: integer) : string;
    property UserModel: string read GetUserModel;

  end;

implementation

uses
  SysUtils,
  Windows,
  UDataSetType,
  UErrorHandlingOperations, DB;

procedure TUser.AssignFrom(AUser: TUser);
const OPNAME = 'TUser.AssignFrom';
begin
  try
    FUserId           := AUser.FUserId;
    FPassword         := AUser.FPassword;
    FInitials         := AUser.FInitials;
    FFirstName        := AUser.FFirstName;
    FSecondName       := AUser.FSecondName;
    FLastName         := AUser.FLastName;
    FPreferedLanguage := AUser.FPreferedLanguage;
    FUserRights       := AUser.FUserRights;
    FLoggedOn         := AUser.FLoggedOn;
    FUserType         := AUser.FUserType;
    FCreatedBy        := AUser.CreatedBy;
    FAutoLogOn        := AUser.AutoLogOn;
    FAutoSelectStudy  := AUser.AutoSelectStudy;
    FUserModel.Assign(AUser.FUserModel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TUser.CreateMemberObjects;
const OPNAME = 'TUser.CreateMemberObjects';
begin
  try
    FUserModel        := TStringList.Create;
    //FUserModel.Sorted := true;
    //FUserModel.Duplicates := dupIgnore;
    Reset;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUser.FirstName: String;
const OPNAME = 'TUser.FirstName';
begin
  try
    Result := FFirstName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUser.Initials: String;
const OPNAME = 'TUser.Initials';
begin
  try
    Result := FInitials;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUser.LastName: String;
const OPNAME = 'TUser.LastName';
begin
  try
    Result := FLastName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUser.SecondName: String;
const OPNAME = 'TUser.SecondName';
begin
  try
    Result := FSecondName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUser.UserId: String;
const OPNAME = 'TUser.UserId';
begin
  try
    Result := FUserId;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUser.UserRights: integer;
const OPNAME = 'TUser.UserRights';
begin
  Result := 0;
  try

    Result := FUserRights;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUser.LoggedOn: boolean;
const OPNAME = 'TUser.LoggedOn';
begin
  Result := False;
  try
    Result := FLoggedOn;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUser.UserType: TUserType;
const OPNAME = 'TUser.UserType';
begin
  Result := utStandard;
  try
    Result := FUserType;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUser.Password: String;
const OPNAME = 'TUser.Password';
begin
  try
    Result := FPassword;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUser.PreferedLanguage: String;
const OPNAME = 'TUser.PreferedLanguage';
begin
  try
    Result := FPreferedLanguage;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TUser.Reset;
const OPNAME = 'TUser.Reset';
begin
  try
    FUserId           := '';
    FPassword         := '';
    FInitials         := '';
    FFirstName        := '';
    FSecondName       := '';
    FLastName         := '';
    FPreferedLanguage := '';
    FCreatedBy        := '';
    FUserRights       := 0;
    FLoggedOn         := False;
    FUserType         := utStandard;
    FAutoLogOn        := False;
    FAutoSelectStudy  := False;
    FUserModel.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUser.EnCrypt(const ATheString : String) : String;
const OPNAME = 'TUser.EnCrypt';
var
  LIndex : integer;
begin
  try
    Result := '';
    for LIndex := 1 to Length(ATheString) do
    begin
      Result := Result + IntToHex(Ord(ATheString[LIndex]),2);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUser.DeCrypt(const ATheString : String) : String;
const OPNAME = 'TUser.DeCrypt';
var
  LIndex : integer;
  LHexStr: string;
begin
  try
    Result := '';
    LIndex := 1;
    while LIndex < Length(ATheString) do
    begin
      LHexStr := Copy(ATheString,LIndex,2);
      Result := Result + Char(StrToInt('$'+LHexStr));
      LIndex := LIndex + 2;;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUser.InitialiseFromDataset(AUserDataset: TAbstractModelDataset): boolean;
const OPNAME = 'TUser.InitialiseFromDataset';
var
  LPassword,
  LAutoLogon,
  LAutoStudy : string;
begin
  Result := False;
  try
    if not (AUserDataset.DataSet.Eof and AUserDataset.DataSet.Bof) then
    begin
      FUserId     := Trim(AUserDataset.DataSet.FieldByName('UserId').AsString);
      LPassword   := Trim(AUserDataset.DataSet.FieldByName('Password').AsString);
      FPassword   := DeCrypt(LPassword);
      FInitials   := Trim(AUserDataset.DataSet.FieldByName('Initials').AsString);
      FFirstName  := Trim(AUserDataset.DataSet.FieldByName('FirstName').AsString);
      FSecondName := Trim(AUserDataset.DataSet.FieldByName('SecondName').AsString);
      FLastName   := Trim(AUserDataset.DataSet.FieldByName('LastName').AsString);
      FUserRights := AUserDataset.DataSet.FieldByName('UserRights').AsInteger;
      FUserType   := TUserType(AUserDataset.DataSet.FieldByName('UserType').AsInteger);
      FUserModel.CommaText := DWAF_ALL_MODELS;
      //FUserModel.CommaText := Trim(AUserDataset.DataSet.FieldByName('Model').AsString);
      //RemoveUnlicencedModels;
      //AddPreProcessorModels;
      //FUserModel.Add('Hydrology');//RianaHydro
      FPreferedLanguage  := Trim(AUserDataset.DataSet.FieldByName('PreferedLanguage').AsString);

      LAutoLogon :=  Trim(AUserDataset.DataSet.FieldByName('AutoLogon').AsString);
      if (LAutoLogon = '1')  then
        FAutoLogOn := True
      else
        FAutoLogOn := False;

      LAutoStudy := Trim(AUserDataset.DataSet.FieldByName('AutoSelectStudy').AsString);
      if (LAutoStudy = '1') then
        FAutoSelectStudy := True
      else
         FAutoSelectStudy := False;
      FCreatedBy := Trim(AUserDataset.DataSet.FieldByName('CreatedBy').AsString);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TUser.SetUserRights(ARights: integer);
const OPNAME = 'TUser.SetUserRights';
var
  LDataSet     : TAbstractModelDataset;
begin
  try
    FUserRights := ARights;
    FAppModules.Database. CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL('UPDATE Users SET UserRights = '+IntToStr(FUserRights) +
        ' WHERE UserId = '+ QuotedStr(FUserId));
        LDataset.ExecSQL;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TUser.SetUserType(AType: TUserType);
const OPNAME = 'TUser.SetUserType';
var
  LDataSet     : TAbstractModelDataset;
begin
  try
    FUserType := AType;
    FAppModules.Database. CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL('UPDATE Users SET UserType = ' + IntToStr(Integer(FUserType)) +
        ' WHERE UserId = '+ QuotedStr(FUserId));
        LDataset.ExecSQL;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUser.UserModelCount: integer;
const OPNAME = 'TUser.UserModelCount';
begin
  Result := 0;
  try
    Result := FUserModel.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUser.DestroyMemberObjects;
const OPNAME = 'TUser.DestroyMemberObjects';
begin
  try
    //Clear;
    FreeAndNil(FUserModel);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUser.UserModelByIndex(AIndex: integer): string;
const OPNAME = 'TUser.UserModelByIndex';
begin
  Result := '';
  try
    if(AIndex >= 0) and (AIndex < FUserModel.Count) then
      Result := FUserModel[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUser.SetLoggedOn(ALoggedOn: boolean);
const OPNAME = 'TUser.SetLoggedOn';
begin
  try
    FLoggedOn := ALoggedOn;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUser.SaveUserToDB : boolean;
const OPNAME = 'TUser.SaveUserToDB';
var
  LSQL       : string;
  LDataSet   : TAbstractModelDataset;
  LPassword,
  LAutoLogon : string;
  LAutoStudy : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if (assigned(lDataSet)) then
      begin
        lDataSet.DataSet.Close;
        if FAutoLogOn then
          LAutoLogon := '1'
        else
          LAutoLogon := '0';

          if FAutoSelectStudy then
            LAutoStudy := '1'
          else
            LAutoStudy := '0';
        LPassword := EnCrypt(FPassword);
        lSQL := 'INSERT INTO Users' +
                ' (UserId,[Password],Initials,FirstName,SecondName,LastName,PreferedLanguage,UserRights,UserType,Model,AutoLogon,AutoSelectStudy,CreatedBy)'+
                '  Values(' + QuotedStr(FUserId)+ ','+ QuotedStr(LPassword) + ',' + QuotedStr(FInitials)+ ',' + QuotedStr(FFirstName) + ',' + QuotedStr(FSecondName) + ',' +
                              QuotedStr(FLastName) + ',' + QuotedStr(FPreferedLanguage) + ',' + IntToStr(FUserRights) + ',' + IntToStr(integer(FUserType)) + ',' +
                              QuotedStr(FUserModel.CommaText) + ',' +  QuotedStr(LAutoLogon) + ',' + QuotedStr(LAutoStudy) + ',' + QuotedStr(FCreatedBy) + ')';
        lDataSet.SetSQL(lSQL);
        LDataset.ExecSQL;
        lDataSet.DataSet.Close;
        Result := True;
      end;
    finally
      FreeAndNil(lDataset);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUser.DeleteUserFromDB: boolean;
const OPNAME = 'TUser.DeleteUserFromDB';
var
  lSQL       : string;
  lDataSet   : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if (FUserId <> '') then
      begin
        if (assigned(lDataSet)) then
        begin
          lDataSet.DataSet.Close;
          lSQL := 'DELETE * FROM Users WHERE UserId = ' + QuotedStr(FUserId);
          lDataSet.SetSQL(lSQL);
          LDataset.ExecSQL;
          lDataSet.DataSet.Close;
          Result := True;
        end;
      end;
    finally
      FreeAndNil(lDataset);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUser.UpdateUserDetails: boolean;
const OPNAME = 'TUser.UpdateUserDetails';
var
  LSQL      : string;
  LPassword,
  LAutoLogon,
  LAutoStudy : string;
  LDataset  : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataset);
    try
      if (Assigned(LDataset)) then
      begin
        LDataset.DataSet.Close;

        if FAutoLogOn then
          LAutoLogon := '1'
        else
          LAutoLogon := '0';

          if FAutoSelectStudy then
            LAutoStudy := '1'
          else
            LAutoStudy := '0';


        LPassword := EnCrypt(FPassword);
        LSQL := 'UPDATE Users SET ' +
                '[Password]         = ' + QuotedStr(LPassword)   +
                ',Initials         = ' + QuotedStr(FInitials)   +
                ',FirstName        = ' + QuotedStr(FFirstName)  +
                ',SecondName       = ' + QuotedStr(FSecondName) +
                ',LastName         = ' + QuotedStr(FLastName)   +
                ',PreferedLanguage = ' + QuotedStr(FPreferedLanguage) +
                ',UserRights       = ' + IntToStr(FUserRights)  +
                ',UserType         = ' + IntToStr(integer(FUserType)) +
                ',Model            = ' + QuotedStr(FUserModel.CommaText)   +
                ',AutoLogon        = ' + QuotedStr(LAutoLogon)   +
                ',AutoSelectStudy  = ' + QuotedStr(LAutoStudy)   +
                ',CreatedBy        = ' + QuotedStr(FCreatedBy)  +
                'WHERE UserID      = ' + QuotedStr(FUserId);
       LDataset.SetSQL(LSQL);
       LDataset.ExecSQL;
       LDataset.DataSet.Close;
       Result := True;
      end
    finally
      FreeAndNil(LDataset);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TUser.GetUserModel: string;
const OPNAME = 'TUser.GetUserModel';
begin
  Result := '';
  try
    Result := FUserModel.CommaText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TUser.PopulateUser(AUserId, APassword, AInitials,AFirstName,ASecondName,ALastName,ALanguage: string;
                          AUserRights : integer;AUserType: TUserType; AModelName,ACreatedBy  : string;
                          AAutoLogOn,AAutoSelectStudy: Boolean) : boolean;
const OPNAME = 'TUser.PopulateUser';
begin
  Result := False;
  try
    FUserId              := AUserId;
    FPassword            := APassword;
    FInitials            := AInitials;
    FFirstName           := AFirstName;
    FSecondName          := ASecondName;
    FLastName            := ALastName;
    FPreferedLanguage    := ALanguage;
    FUserRights          := AUserRights;
    FUserType            := AUserType;
    FAutoLogOn           := AAutoLogOn;
    FAutoSelectStudy     := AAutoSelectStudy;
    FUserModel.CommaText := AModelName;
    FCreatedBy           := ACreatedBy;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUser.CreatedBy: string;
const OPNAME = 'TUser.CreatedBy';
begin
  Result := '';
  try
    Result := FCreatedBy;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUser.AutoLogOn: Boolean;
const OPNAME = 'TUser.AutoLogOn';
begin
  Result := FALSE;
  try
    Result := FAutoLogOn;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUser.AutoSelectStudy: Boolean;
const OPNAME = 'TUser.AutoSelectStudy';
begin
  Result := FALSE;
  try
    Result := FAutoSelectStudy;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUser.GetUserRightsDescr(AUserRights: integer) : string;
const OPNAME = 'TUser.GetUserRightsDescr';
begin
  Result := '';
  try
    case AUserRights of
      1  :  Result := FAppModules.Language.GetString('AccessControl.ReadOnly');
      2  :  Result := FAppModules.Language.GetString('AccessControl.WriteOnly');
      3  :  Result := FAppModules.Language.GetString('AccessControl.ReadAndWrite');
      100:  Result := FAppModules.Language.GetString('AccessControl.SystemAdministrator');
    end;
 except on E: Exception do HandleError(E, OPNAME) end;
end;



procedure TUser.RemoveUnlicencedModels;
const OPNAME = 'TUser.RemoveUnlicencedModels';
var
  LLicencedModels : TStringList;
  Lindex: integer;
begin
  try
    LLicencedModels := TStringList.Create;
    try
      LLicencedModels.CommaText := DWAF_ALL_MODELS;//FAppModules.LicenceManager.LicencedModelsCommatext;
      Lindex := 0;
      while LIndex < FUserModel.Count do
      begin
        if(LLicencedModels.IndexOf(FUserModel[Lindex]) < 0) then
          FUserModel.Delete(Lindex)
        else
         Lindex := LIndex + 1;
      end;
    finally
      LLicencedModels.Free;
    end;
 except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TUser.AddPreProcessorModels;
const OPNAME = 'TUser.AddPreProcessorModels';
begin
  try
    if(FUserModel.IndexOf(CYield) >= 0) then
    begin
      FUserModel.Add(CDailyDiversion);
      FUserModel.Add(CIFRPreProcessor);
      FUserModel.Add(CStomsa);
      FUserModel.Add(CDamSedimentation);
    end;
 except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TUser.ReloadData;
const OPNAME = 'TUser.ReloadData';
var
  LUsersDataSet : TAbstractModelDataset;
begin
  try
    if FLoggedOn then
    begin
      FAppModules.Database.CreateDataset(integer(dtUserAdministration), LUsersDataSet);
      try
        LUsersDataSet.DataSet.Active := True;
        while not LUsersDataSet.DataSet.Eof do
        begin
          if(Trim(LUsersDataSet.DataSet.FieldByName('UserID').AsString) = FUserId) then
          begin
            Self.Reset;
            Self.InitialiseFromDataset(LUsersDataSet);
            FLoggedOn := True;
            Break;
          end;
          LUsersDataSet.DataSet.Next;
        end;
      finally
        LUsersDataSet.DataSet.Close;
        LUsersDataSet.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
