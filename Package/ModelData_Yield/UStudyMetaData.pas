unit UStudyMetaData;

interface
uses
  Classes,
  Contnrs,
  UAbstractObject;

type

{******************************************************************************}
{* Study Error MetaData Features                                              *}
{******************************************************************************}

  TStudyMetaData = class(TAbstractAppObject)
  protected
    FStudyAreaName    : WideString;
    FIdentifier       : integer;
    FImportedBy       : Widestring;
    FErrorType        : Widestring;
    FErrorDescription : Widestring;
    FStudyErrors      : integer;
    FCorrectiveAction : Widestring;
    FReadOnly         : boolean;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function Get_StudyAreaName    : Widestring; safecall;
    function Get_Identifier       : integer;    safecall;
    function Get_ImportedBy       : Widestring; safecall;
    function Get_ErrorType        : Widestring; safecall;
    function Get_ErrorDescription : Widestring; safecall;
    function Get_StudyErrors      : integer;    safecall;
    function Get_CorrectiveAction : Widestring; safecall;
    function Get_ReadOnly         : boolean;    safecall;

    procedure Set_ImportedBy(const AImportedBy : Widestring); safecall;
    procedure Set_ErrorType(const AErrorType : Widestring); safecall;
    procedure Set_ErrorDescription(const AErrorDescription : Widestring); safecall;
    procedure Set_StudyErrors(const AStudyErrors : integer); safecall;
    procedure Set_CorrectiveAction(const ACorrectiveAction : Widestring); safecall;
  public
    function Initialise : boolean; override;
    function Populate(AIdentifier  : integer; AImportedBy,AErrorType,AErrorDescription : Widestring;
                      AStudyErrors : integer; ACorrectiveAction, AStudyAreaName : Widestring;
                      AReadOnly : boolean): WordBool;

    property StudyAreaName    : WideString read Get_StudyAreaName    write FStudyAreaName;              
    property Identifier       : integer    read Get_Identifier       write FIdentifier;
    property ImportedBy       : Widestring read Get_ImportedBy       write Set_ImportedBy;
    property ErrorType        : Widestring read Get_ErrorType        write Set_ErrorType;
    property ErrorDescription : Widestring read Get_ErrorDescription write Set_ErrorDescription;
    property StudyErrors      : integer    read Get_StudyErrors      write Set_StudyErrors;
    property CorrectiveAction : Widestring read Get_CorrectiveAction write Set_CorrectiveAction;
    property RecordReadOnly   : boolean    read Get_ReadOnly;
  end;

  TStudyMetaDataList = class(TAbstractAppObject)
  protected
    FStudyMetaDataList : TObjectList;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function Get_StudyMetaDataCount: integer; safecall;
    function GetCastStudyMetaDataByIndex(AIndex: integer): TStudyMetaData;
    function GetCastStudyMetaDataByID(AStudyMetaDataID: integer): TStudyMetaData;
    function DeleteStudyMetaDataWithID(AStudyMetaDataID : integer) : WordBool;
    function DeleteStudyMetaDataWithIndex(AIndex : integer) : WordBool;
  public
    function Initialise: Boolean; override;
    function NewStudyMetaData : TStudyMetaData;
    function CreateNewStudyMetaData : TStudyMetaData;

    property StudyMetaDataCount: integer                                  read Get_StudyMetaDataCount;
    property CastStudyMetaDataByID[AIdentifier: integer] : TStudyMetaData read GetCastStudyMetaDataByID;
    property CastStudyMetaDataByIndex[AIndex: integer]   : TStudyMetaData read GetCastStudyMetaDataByIndex;
    property DeleteStudyErrorByID[AStudyMetaDataID : integer] : WordBool  read DeleteStudyMetaDataWithID;
    property DeleteStudyErrorByIndex[AIndex : integer] : WordBool         read DeleteStudyMetaDataWithIndex;
  end;

implementation
uses
  System.Types,
  UStudyMetaDataSQLAgent,
  SysUtils,
  UConstants,
  UControlCreationUtilities,
  UErrorHandlingOperations;

{ TStudyMetaData }

function TStudyMetaData._AddRef: Integer;
const OPNAME = 'TStudyMetaData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudyMetaData._Release: Integer;
const OPNAME = 'TStudyMetaData._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudyMetaData.CreateMemberObjects;
const OPNAME = 'TStudyMetaData.CreateMemberObjects';
begin
  inherited;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudyMetaData.DestroyMemberObjects;
const OPNAME = 'TStudyMetaData.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudyMetaData.Get_CorrectiveAction: Widestring;
const OPNAME = 'TStudyMetaData.Get_CorrectiveAction';
begin
  Result := '';
  try
    Result := FCorrectiveAction;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudyMetaData.Get_ErrorDescription: Widestring;
const OPNAME = 'TStudyMetaData.Get_ErrorDescription';
begin
  Result := '';
  try
    Result := FErrorDescription;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudyMetaData.Get_ErrorType: Widestring;
const OPNAME = 'TStudyMetaData.Get_ErrorType';
begin
  Result := '';
  try
    Result := FErrorType;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudyMetaData.Get_StudyAreaName: Widestring;
const OPNAME = 'TStudyMetaData.Get_StudyAreaName';
begin
  Result := '';
  try
    Result := FStudyAreaName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudyMetaData.Get_Identifier: integer;
const OPNAME = 'TStudyMetaData.Get_Identifier';
begin
  Result := NullInteger;
  try
    Result := FIdentifier;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudyMetaData.Get_ImportedBy: Widestring;
const OPNAME = 'TStudyMetaData.Get_ImportedBy';
begin
  Result := '';
  try
    Result := FImportedBy;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudyMetaData.Get_StudyErrors: integer;
const OPNAME = 'TStudyMetaData.Get_StudyErrors';
begin
  Result := NullInteger;
  try
    Result := FStudyErrors;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudyMetaData.Get_ReadOnly: boolean;
const OPNAME = 'TStudyMetaData.Get_ReadOnly';
begin
  Result := False;
  try
    Result := FReadOnly;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudyMetaData.Initialise: boolean;
const OPNAME = 'TStudyMetaData.Initialise';
begin
  Result := False;
  try
    FIdentifier       := NullInteger;
    FImportedBy       := '';
    FErrorType        := '';
    FErrorDescription := '';
    FStudyErrors      := 0;
    FCorrectiveAction := '';
    FReadOnly         := False;
    Result            := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudyMetaData.Populate(AIdentifier: integer; AImportedBy, AErrorType, AErrorDescription : Widestring;
                                 AStudyErrors: integer; ACorrectiveAction, AStudyAreaName : Widestring;
                                 AReadOnly : boolean): WordBool;
const OPNAME = 'TStudyMetaData.Populate';
begin
  Result := False;
  try
    FStudyAreaName    := AStudyAreaName;                       
    FIdentifier       := AIdentifier;
    FImportedBy       := AImportedBy;
    FErrorType        := AErrorType;
    FErrorDescription := AErrorDescription;
    FStudyErrors      := AStudyErrors;
    FCorrectiveAction := ACorrectiveAction;
    FReadOnly         := AReadOnly;
    Result            := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudyMetaData.Set_CorrectiveAction(const ACorrectiveAction: Widestring);
const OPNAME = 'TStudyMetaData.Set_CorrectiveAction';
var
  LLoadAgent   : TStudyMetaDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TStudyMetaDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_StudyMetaData(LContextData,FIdentifier);
        if FAppModules.FieldProperties.UpdateFieldValue('CorrectiveAction', ACorrectiveAction, FCorrectiveAction, LContextData) then
        begin
          LOldValue         := FCorrectiveAction;
          FCorrectiveAction := ACorrectiveAction;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'CorrectiveAction',LOldValue,ACorrectiveAction);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyMetaData.Set_ErrorDescription(const AErrorDescription: Widestring);
const OPNAME = 'TStudyMetaData.Set_ErrorDescription';
var
  LLoadAgent   : TStudyMetaDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TStudyMetaDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_StudyMetaData(LContextData,FIdentifier);
        if FAppModules.FieldProperties.UpdateFieldValue('ErrorDescription', AErrorDescription, FErrorDescription, LContextData) then
        begin
          LOldValue         := FErrorDescription;
          FErrorDescription := AErrorDescription;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ErrorDescription',LOldValue,AErrorDescription);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyMetaData.Set_ErrorType(const AErrorType: Widestring);
const OPNAME = 'TStudyMetaData.Set_ErrorType';
var
  LLoadAgent   : TStudyMetaDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TStudyMetaDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_StudyMetaData(LContextData,FIdentifier);
        if FAppModules.FieldProperties.UpdateFieldValue('ErrorType', AErrorType, FErrorType, LContextData) then
        begin
          LOldValue  := FErrorType;
          FErrorType := AErrorType;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ErrorType',LOldValue,AErrorType);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyMetaData.Set_ImportedBy(const AImportedBy: Widestring);
const OPNAME = 'TStudyMetaData.Set_ImportedBy';
var
  LLoadAgent   : TStudyMetaDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TStudyMetaDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_StudyMetaData(LContextData,FIdentifier);
        if FAppModules.FieldProperties.UpdateFieldValue('ImportedBy', AImportedBy, FImportedBy, LContextData) then
        begin
          LOldValue   := FImportedBy;
          FImportedBy := AImportedBy;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ImportedBy',LOldValue,AImportedBy);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyMetaData.Set_StudyErrors(const AStudyErrors: integer);
const OPNAME = 'TStudyMetaData.Set_StudyErrors';
var
  LLoadAgent   : TStudyMetaDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TStudyMetaDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_StudyMetaData(LContextData,FIdentifier);
        if FAppModules.FieldProperties.UpdateFieldValue('StudyErrors', IntToStr(AStudyErrors), IntToStr(FStudyErrors), LContextData) then
        begin
          LOldValue    := IntToStr(FStudyErrors);
          FStudyErrors := AStudyErrors;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'StudyErrors',LOldValue,IntToStr(AStudyErrors));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TStudyMetaDataList }

function TStudyMetaDataList._AddRef: Integer;
const OPNAME = 'TStudyMetaDataList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudyMetaDataList._Release: Integer;
const OPNAME = 'TStudyMetaDataList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudyMetaDataList.CreateMemberObjects;
const OPNAME = 'TStudyMetaDataList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FStudyMetaDataList := TObjectList.Create(False);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudyMetaDataList.CreateNewStudyMetaData : TStudyMetaData;
const OPNAME = 'TStudyMetaDataList.CreateNewStudyMetaData';
var
  LLoadAgent     : TStudyMetaDataSQLAgent;
  LStudyMetaData : TStudyMetaData;
begin
  Result := nil;
  try
    LLoadAgent := TStudyMetaDataSQLAgent.Create(FAppModules);
    try
      LStudyMetaData            := NewStudyMetaData;
      LStudyMetaData.Identifier := LLoadAgent.GetMaxStudyMetaDataID + 1;
      LStudyMetaData.StudyAreaName := FAppModules.StudyArea.StudyAreaCode;
      if(LLoadAgent.AddStudyMetaData(LStudyMetaData)) then
        Result := LStudyMetaData
      else
        FStudyMetaDataList.Remove(LStudyMetaData);
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudyMetaDataList.DeleteStudyMetaDataWithID(AStudyMetaDataID: integer): WordBool;
const OPNAME = 'TStudyMetaDataList.DeleteStudyMetaDataWithID';
var
  LStudyMetaData : TStudyMetaData;
  LSQLAgent      : TStudyMetaDataSQLAgent;
begin
  Result := False;
  try
    LSQLAgent := TStudyMetaDataSQLAgent.Create(FAppModules);
    try
      LStudyMetaData := GetCastStudyMetaDataByID(AStudyMetaDataID);
      if(LStudyMetaData <> nil) then
      begin
        if(LSQLAgent.DeleteStudyMetaData(LStudyMetaData)) then
        begin
          FStudyMetaDataList.Remove(LStudyMetaData);
          Result := True;
        end;
      end;
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudyMetaDataList.DeleteStudyMetaDataWithIndex(AIndex: integer): WordBool;
const OPNAME = 'TStudyMetaDataList.DeleteStudyMetaDataWithIndex';
begin
  Result := False;
  try
    if((AIndex >= 0) and (AIndex < FStudyMetaDataList.Count)) then
    begin
      FStudyMetaDataList.Remove(FStudyMetaDataList.Items[AIndex]);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudyMetaDataList.DestroyMemberObjects;
const OPNAME = 'TStudyMetaDataList.DestroyMemberObjects';
var
  LCount : Integer;
begin
  try
    for LCount := FStudyMetaDataList.Count - 1 downto 0 do
      FStudyMetaDataList.Items[LCount].Free;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudyMetaDataList.Get_StudyMetaDataCount: integer;
const OPNAME = 'TStudyMetaDataList.Get_StudyMetaDataCount';
begin
  Result := 0;
  try
    Result := FStudyMetaDataList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudyMetaDataList.GetCastStudyMetaDataByID(AStudyMetaDataID: integer): TStudyMetaData;
const OPNAME = 'TStudyMetaDataList.GetCastStudyMetaDataByID';
var
 LIndex: integer;
begin
  Result := nil;
  try
    if(AStudyMetaDataID <> NullInteger) then
    begin
      for LIndex := 0 to FStudyMetaDataList.Count - 1 do
      begin
        if(TStudyMetaData(FStudyMetaDataList[LIndex]).Identifier = AStudyMetaDataID) then
        begin
          Result := TStudyMetaData(FStudyMetaDataList[LIndex]);
          Break;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudyMetaDataList.GetCastStudyMetaDataByIndex(AIndex: integer): TStudyMetaData;
const OPNAME = 'TStudyMetaDataList.GetCastStudyMetaDataByIndex';
begin
  Result := nil;
  try
    if((AIndex >= 0) and (AIndex < FStudyMetaDataList.Count)) then
      Result := TStudyMetaData(FStudyMetaDataList[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudyMetaDataList.Initialise: Boolean;
const OPNAME = 'TStudyMetaDataList.Initialise';
begin
  Result := inherited Initialise;
  try
    FStudyMetaDataList.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudyMetaDataList.NewStudyMetaData: TStudyMetaData;
const OPNAME = 'TStudyMetaDataList.NewStudyMetaData';
begin
  Result := nil;
  try
    Result := TStudyMetaData.Create(FAppModules);
    Result.Initialise;
    FStudyMetaDataList.Add(Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
 