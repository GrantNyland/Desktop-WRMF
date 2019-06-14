{******************************************************************************}
{*  UNIT      : Contains TSwitchDefinition Class                              *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 27/02/2006                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit USwitchDefinition;

interface
uses
  SysUtils,
  Classes,
  Contnrs,
  VoaimsCom_TLB,
  UAbstractObject,
  UYieldModelDataObject;

type

  TSwitchDefinition = class(TAbstractAppObject, ISwitchDefinition)
  protected
    FSwitchDefID    : integer;
    FStartMonth     : integer;
    FStartYear      : integer;
    FSwitchFileName : WideString;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Get_SwitchDefID : integer; safecall;
    function Get_SwitchDefStartMonth : integer; safecall;
    procedure Set_SwitchDefStartMonth(AValue : integer); safecall;
    function Get_SwitchDefStartYear : integer; safecall;
    procedure Set_SwitchDefStartYear(AValue : integer); safecall;
    function Get_SwitchDefFileName : WideString; safecall;
    procedure Set_SwitchDefFileName(const AValue : WideString); safecall;
    function ValidateSwitchDefFileName (AErrorMessages : TStrings) : WordBool;
    function ValidateSwitchDefStartYear (AErrorMessages : TStrings) : WordBool;
    function ValidateSwitchDefStartMonth (AErrorMessages : TStrings) : WordBool;
  public
    function Initialise: Boolean; override;
    function Populate (ASwitchDefID    : integer;
                       AStartYear      : integer;
                       AStartMonth     : integer;
                       ASwitchFileName : string) : Boolean;
    function Validate (var AErrors    : WideString;
                       const AContext : WideString) : WordBool; safecall;

    property SwitchDefID      : integer     read Get_SwitchDefID;
    property SwitchDefStartMonth       : integer     read Get_SwitchDefStartMonth   write Set_SwitchDefStartMonth;
    property SwitchDefStartYear        : integer     read Get_SwitchDefStartYear    write Set_SwitchDefStartMonth;
    property SwitchDefFileName   : WideString  read Get_SwitchDefFileName     write Set_SwitchDefFileName;
  end;

  TSwitchDefinitionsList = class(TAbstractAppObject, ISwitchDefinitionsList)
  protected
    FSwitchDefinitionsList : TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function AddSwitchDefinition (ASwitchDef : TSwitchDefinition): boolean;
  public
    function Initialise: Boolean; override;

    function NewSwitchDefinition : ISwitchDefinition; safecall;
    function CreateSwitchDefinition : TSwitchDefinition;
    function DeleteSwitchDefinitionWithID (AID : integer) : WordBool;
    function DeleteSwitchDefinitionWithIndex (AIndex : integer) : WordBool;
    function CastSwitchDefinitionByIndex (AIndex : integer): TSwitchDefinition;
    function CastSwitchDefinitionByID (AID : integer): TSwitchDefinition;

    function RemoveSwitchDefinitionWithID (AID : integer) : WordBool; safecall;
    function Get_SwitchDefinitionCount: integer; safecall;
    function Get_SwitchDefinitionByID (AID: integer): ISwitchDefinition; safecall;
    function Get_SwitchDefinitionByIndex (AIndex: integer): ISwitchDefinition; safecall;

    property SwitchDefinitionCount: integer read Get_SwitchDefinitionCount;
    property SwitchDefinitionByIndex[AIndex : integer]: ISwitchDefinition read Get_SwitchDefinitionByIndex;
    property SwitchDefinitionByID[AID : integer]: ISwitchDefinition read Get_SwitchDefinitionByID;
  end;

    procedure AddErrors (var AErrors : WideString;
                         AErrorList  : TStringList;
                         AErrorCols  : TStringList);

implementation

uses
  System.Types,
  Math,
  UConstants,
  VCL.ComCtrls,
  UFileNames,
  UFileChannelSwitchControlDatabaseAgent,
  UFilesActionPlanningManager,
  UAbstractDatabaseAgent,
  USwitchDefinitionLoadAgent,
  UPlanningModelDataObject,
  UErrorHandlingOperations;

procedure AddErrors (var AErrors : WideString;
                                           AErrorList  : TStringList;
                                           AErrorCols  : TStringList);
const OPNAME = 'USwitchDefinition.AddErrors';
begin
  try
    if (AErrorCols.Count = 0) then
      AErrors := AErrors + AErrorList.Text
    else
      AErrors := AErrors + CTStringsSeparator + AErrorList.Text +
                           CTStringsSeparator + AErrorCols.Text + CTStringsSeparator;
    AErrorList.Clear;
    AErrorCols.Clear;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{******************************************************************************}
{* TSwitchDefinition                                                      *}
{******************************************************************************}

function TSwitchDefinition._AddRef: Integer;
const OPNAME = 'TSwitchDefinition._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSwitchDefinition._Release: Integer;
const OPNAME = 'TSwitchDefinition._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSwitchDefinition.CreateMemberObjects;
const OPNAME = 'TSwitchDefinition.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSwitchDefinition.DestroyMemberObjects;
const OPNAME = 'TSwitchDefinition.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSwitchDefinition.Initialise : boolean;
const OPNAME = 'TSwitchDefinition.Initialise';
begin
  Result := inherited Initialise;
  try
    FStartYear              := 0;
    FStartMonth             := 0;
    FSwitchFileName         := '';
    Result := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSwitchDefinition.Populate (ASwitchDefID    : integer;
                                     AStartYear      : integer;
                                     AStartMonth     : integer;
                                     ASwitchFileName : string) : Boolean;
const OPNAME = 'TSwitchDefinition.Populate';
begin
  Result := FALSE;
  try
    FSwitchDefID     := ASwitchDefID;
    FStartYear       := AStartYear;
    FStartMonth      := AStartMonth;
    FSwitchFileName  := ASwitchFileName;
    Result := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSwitchDefinition.Get_SwitchDefStartYear : integer;
const OPNAME = 'TSwitchDefinition.Get_SwitchDefStartYear';
begin
  Result := 0;
  try
    Result := FStartYear;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSwitchDefinition.Set_SwitchDefStartYear (AValue : integer);
const OPNAME = 'TSwitchDefinition.Set_SwitchDefStartYear';
var
  LLoadAgent   : TSwitchDefinitionLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TSwitchDefinitionLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_SwitchDefID(LContextData, IntToStr(FSwitchDefID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'SwitchDefStartYear', IntToStr(AValue), IntToStr(FStartYear), LContextData) then
        begin
          LOldValue := IntToStr(FStartYear);
          FStartYear := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SwitchDefStartYear',LOldValue,IntToStr(AValue));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSwitchDefinition.Get_SwitchDefID : integer;
const OPNAME = 'TSwitchDefinition.Get_SwitchDefID';
begin
  Result := 0;
  try
    Result := FSwitchDefID;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSwitchDefinition.Get_SwitchDefStartMonth : integer;
const OPNAME = 'TSwitchDefinition.Get_SwitchDefStartMonth';
begin
  Result := 0;
  try
    Result := FStartMonth;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSwitchDefinition.Set_SwitchDefStartMonth (AValue : integer);
const OPNAME = 'TSwitchDefinition.Set_SwitchDefStartMonth';
var
  LLoadAgent : TSwitchDefinitionLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TSwitchDefinitionLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_SwitchDefID(LContextData, IntToStr(FSwitchDefID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'SwitchDefStartMonth', IntToStr(AValue), IntToStr(FStartMonth), LContextData) then
        begin
          LOldValue := IntToStr(FStartMonth);
          FStartMonth := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SwitchDefStartMonth',LOldValue,IntToStr(AValue));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSwitchDefinition.Get_SwitchDefFileName : WideString;
const OPNAME = 'TSwitchDefinition.Get_SwitchDefFileName';
begin
  Result := '';
  try
    Result := FSwitchFileName;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSwitchDefinition.Set_SwitchDefFileName (const AValue : WideString);
const OPNAME = 'TSwitchDefinition.Set_SwitchDefFileName';
var
  LLoadAgent   : TSwitchDefinitionLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TSwitchDefinitionLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_SwitchDefID(LContextData, IntToStr(FSwitchDefID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'SwitchDefFileName', AValue, FSwitchFileName, LContextData) then
        begin
          LOldValue := FSwitchFileName;
          FSwitchFileName := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit, 'SwitchDefFileName', LOldValue, FSwitchFileName);
        end;
      finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil (LLoadAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSwitchDefinition.Validate (var AErrors    : WideString;
                                         const AContext : WideString) : WordBool;
const OPNAME = 'TSwitchDefinition.Validate';
var
  lStopOnFirstError : Boolean;
  lErrorList        : TStringList;
  lErrorCols        : TStringList;
  lColFlag          : Boolean;
begin
  Result := FALSE;
  try
    LErrorList := TStringList.Create;
    lErrorCols := TStringList.Create;
    try
      lColFlag := FALSE;
      if (AContext = 'SwitchDefFileName') then
        Result := ValidateSwitchDefFileName(lErrorList)
      else
      if (AContext = 'SwitchDefStartYear') then
        Result := ValidateSwitchDefStartYear(lErrorList)
      else
      if (AContext = 'SwitchDefStartMonth') then
        Result := ValidateSwitchDefStartMonth(lErrorList)
      else
      begin
        Result := TRUE;
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        if (NOT ValidateSwitchDefFileName(lErrorList)) then
          Result := False;
        if (Result or (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateSwitchDefStartYear(lErrorList)) then
            Result := False;
        end
        else
        if (Result or (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateSwitchDefStartMonth(lErrorList)) then
            Result := False;
        end;
      end;
      if (lColFlag AND (NOT Result)) then
      begin
        if (lErrorCols.Count = 0) then
          AErrors := AErrors + lErrorList.Text
        else
          AErrors := AErrors + CTStringsSeparator + lErrorList.Text +
                               CTStringsSeparator + lErrorCols.Text+ CTStringsSeparator;
        lErrorList.Clear;
        lErrorCols.Clear;
      end;
      AErrors := AErrors + lErrorList.Text;
    finally
      FreeAndNil(lErrorList);
      FreeAndNil (lErrorCols);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSwitchDefinition.ValidateSwitchDefFileName (AErrorMessages : TStrings) : WordBool;
const OPNAME = 'TSwitchDefinition.ValidateSwitchDefFileName';
var
  lSwitchDefList : TSwitchDefinitionsList;
  lMessage       : string;
  lUnique        : Boolean;
  lIndex         : integer;
  lSwitchDef     : TSwitchDefinition;
begin
  Result := True;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('SwitchDefFileName', FSwitchFileName, lMessage )) then
      AErrorMessages.Add ('WARNING:'+FSwitchFileName + ':' + lMessage )
    else
    begin
      lSwitchDefList := TPlanningModelDataObject(FAppModules.Model.ModelData).
                          CastSwitchDefinitionsList;
      lUnique := TRUE;
      lIndex  := 0;
      while (lUnique AND (lIndex < LSwitchDefList.SwitchDefinitionCount)) do
      begin
        LSwitchDef := LSwitchDefList.CastSwitchDefinitionByIndex(LIndex);
        if ((FSwitchDefID <> LSwitchDef.FSwitchDefID) AND
            (UpperCase(Trim(FSwitchFileName)) = UpperCase(Trim(lSwitchDef.SwitchDefFileName)))) then
        begin
          lMessage := FAppModules.Language.GetString('ContextValidation.DuplicateSwitchDefName');
          AErrorMessages.Add('WARNING:'+Format (lMessage, [FSwitchFileName]));
          lUnique := FALSE;
        end
        else
          lIndex := lIndex + 1;
      end;
      //Result := lUnique;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSwitchDefinition.ValidateSwitchDefStartYear (AErrorMessages : TStrings) : WordBool;
const OPNAME = 'TSwitchDefinition.ValidateSwitchDefStartYear';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ('SwitchDefStartYear', IntToStr(FStartYear), LMessage);
    if (NOT Result) then
      AErrorMessages.Add ('ERROR:'+IntToStr(FStartYear) + ':' +  LMessage)
    else
    if (FStartYear = 0) then
    begin
      lMessage := FAppModules.Language.GetString ('ContextValidation.InvalidDate');
      AErrorMessages.Add('ERROR:'+lMessage);
      Result := FALSE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSwitchDefinition.ValidateSwitchDefStartMonth (AErrorMessages : TStrings) : WordBool;
const OPNAME = 'TSwitchDefinition.ValidateSwitchDefStartMonth';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ('SwitchDefStartMonth', IntToStr(FStartYear), lMessage);
    if (NOT Result) then
      AErrorMessages.Add('ERROR:'+IntToStr(FStartMonth) + ':' +  LMessage)
    else
    if (FStartMonth = 0) then
    begin
      lMessage := FAppModules.Language.GetString('ContextValidation.InvalidDate');
      AErrorMessages.Add('ERROR:'+lMessage);
      Result := FALSE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{******************************************************************************}
{* TSwitchDefinitionsList                                                 *}
{******************************************************************************}

procedure TSwitchDefinitionsList.CreateMemberObjects;
const OPNAME = 'TSwitchDefinitionsList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FSwitchDefinitionsList := TObjectList.Create(TRUE);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSwitchDefinitionsList.DestroyMemberObjects;
const OPNAME = 'TSwitchDefinitionsList.DestroyMemberObjects';
begin
  try
    FreeAndNil(FSwitchDefinitionsList);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSwitchDefinitionsList._AddRef: Integer;
const OPNAME = 'TSwitchDefinitionsList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSwitchDefinitionsList._Release: Integer;
const OPNAME = 'TSwitchDefinitionsList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSwitchDefinitionsList.AddSwitchDefinition (ASwitchDef : TSwitchDefinition): boolean;
const OPNAME = 'TSwitchDefinitionsList.AddSwitchDefinition';
begin
  Result := False;
  try
    if (ASwitchDef <> nil) then
    begin
      FSwitchDefinitionsList.Add(ASwitchDef);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSwitchDefinitionsList.Get_SwitchDefinitionByID (AID : integer): ISwitchDefinition;
const OPNAME = 'TSwitchDefinitionsList.Get_SwitchDefinitionByID';
begin
  Result := nil;
  try
    Result := CastSwitchDefinitionByID(AID);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSwitchDefinitionsList.CastSwitchDefinitionByID (AID : integer): TSwitchDefinition;
const OPNAME = 'TSwitchDefinitionsList.CastSwitchDefinitionByID';
var
 LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to  FSwitchDefinitionsList.Count -1 do
      if (TSwitchDefinition(FSwitchDefinitionsList[LIndex]).SwitchDefID = AID) then
      begin
        Result := TSwitchDefinition(FSwitchDefinitionsList[LIndex]);
        Break;
      end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSwitchDefinitionsList.Get_SwitchDefinitionByIndex (AIndex: integer): ISwitchDefinition;
const OPNAME = 'TSwitchDefinitionsList.Get_SwitchDefinitionByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FSwitchDefinitionsList.Count) then
      Result := TSwitchDefinition(FSwitchDefinitionsList[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSwitchDefinitionsList.CastSwitchDefinitionByIndex(AIndex: integer): TSwitchDefinition;
const OPNAME = 'TSwitchDefinitionsList.CastSwitchDefinitionByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) AND (AIndex < FSwitchDefinitionsList.Count) then
      Result := TSwitchDefinition(FSwitchDefinitionsList[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSwitchDefinitionsList.Get_SwitchDefinitionCount: integer;
const OPNAME = 'TSwitchDefinitionsList.Get_SwitchDefinitionCount';
begin
  Result := 0;
  try
    Result := FSwitchDefinitionsList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSwitchDefinitionsList.Initialise: boolean;
const OPNAME = 'TSwitchDefinitionsList.Initialise';
begin
  Result := inherited Initialise;
  try
    FSwitchDefinitionsList.Clear;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSwitchDefinitionsList.CreateSwitchDefinition : TSwitchDefinition;
const OPNAME = 'TSwitchDefinitionsList.CreateSwitchDefinition';
var
  lSwitchDef : TSwitchDefinition;
begin
  Result := nil;
  try
    lSwitchDef := TSwitchDefinition.Create(FAppModules);
    AddSwitchDefinition(lSwitchDef);
    Result := lSwitchDef;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSwitchDefinitionsList.NewSwitchDefinition : ISwitchDefinition;
const OPNAME = 'TSwitchDefinitionsList.NewSwitchDefinition';
var
  lSwitchDef   : TSwitchDefinition;
  lSwitchDefID : integer;
  lLoadAgent  : TSwitchDefinitionLoadAgent;
  lTempName   : string;
 
begin
  Result := nil;
  try
    lLoadAgent := TSwitchDefinitionLoadAgent.Create(FAppModules);
    try
      if (lLoadAgent.InsertSwitchDefinition(lSwitchDefID)) then
      begin
        lSwitchDef := CreateSwitchDefinition;
        lSwitchDef.Initialise;
        lSwitchDef.FSwitchDefID := lSwitchDefID;
//        lTempName := FAppModules.Language.GetString ('ContextValidation.SwitchDefinition');
        lTempName := 'SW' + IntToStr(lSwitchDefID) + '.DAT';
        lSwitchDef.SwitchDefFileName := lTempName;
        Result := lSwitchDef;

      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSwitchDefinitionsList.RemoveSwitchDefinitionWithID (AID : integer) : WordBool;
const OPNAME = 'TSwitchDefinitionsList.RemoveSwitchDefinitionWithID';
var
  lLoadAgent : TSwitchDefinitionLoadAgent;
  lSwitchDef  : TSwitchDefinition;
  LDataFilePrefix : string;
begin
  Result := False;
  try
    lSwitchDef := CastSwitchDefinitionByID(AID);
    if (lSwitchDef <> nil) then
    begin
      lLoadAgent := TSwitchDefinitionLoadAgent.Create(FAppModules);
      try
        if (lLoadAgent.DeleteSwitchDefinition(AID)) then
        begin
          LDataFilePrefix := Uppercase((FAppModules.Model.ModelData as IYieldModelData).DataFilePaths.DataFilePrefix);
          lLoadAgent.DeleteFileNamesTable(AID,LDataFilePrefix+lSwitchDef.FSwitchFileName);
          DeleteSwitchDefinitionWithID(AID);
          Result := TRUE;
        end;
      finally
        lLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSwitchDefinitionsList.DeleteSwitchDefinitionWithID (AID : integer) : WordBool;
const OPNAME = 'TSwitchDefinitionsList.DeleteSwitchDefinitionWithID';
var
  LIndex: integer;
begin
  Result := FALSE;
  try
    for lIndex := 0 to FSwitchDefinitionsList.Count -1 do
    begin
      if (TSwitchDefinition(FSwitchDefinitionsList.Items[lIndex]).SwitchDefID = AID) then
      begin
        FSwitchDefinitionsList.Remove(FSwitchDefinitionsList.Items[lIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSwitchDefinitionsList.DeleteSwitchDefinitionWithIndex(AIndex : integer) : WordBool;
const OPNAME = 'TSwitchDefinitionsList.DeleteSwitchDefinitionWithIndex';
begin
  Result := FALSE;
  try
    if (AIndex >= 0) and (AIndex < FSwitchDefinitionsList.Count) then
    begin
      FSwitchDefinitionsList.Remove(FSwitchDefinitionsList.Items[AIndex]);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
