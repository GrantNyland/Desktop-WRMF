{******************************************************************************}
{*  UNIT      : Contains the class TReservoirAreaGroup                        *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2007/11/05                                                    *}
{*  COPYRIGHT : Copyright © 2007 DWAF                                         *}
{******************************************************************************}

unit UReservoirAreaGroup;

interface

uses
  Classes,
  Contnrs,
  VoaimsCom_TLB,
  UAbstractObject;
type

{******************************************************************************}
{* Channel Area Features                                                      *}
{******************************************************************************}

  TReservoirAreaGroup = class(TAbstractAppObject, IReservoirAreaGroup)
  protected
    FGroupID    : integer;
    FGroupName  : string;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function ValidateReservoirGroupName (AErrorMessages : TStrings) : Boolean;

  public
    function Initialise : boolean; override;
    function Populate (AGroupID: integer; AGroupName : WideString ): WordBool;
    function PopulateReservoirArea (AGroupID  : integer; AGroupName: WideString): WordBool;
    function Get_GroupID : integer; safecall;
    function Get_GroupName : WideString; safecall;
    procedure Set_GroupName (const AName : WideString); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;

    property GroupID   : integer    read Get_GroupID;
    property GroupName : WideString read Get_GroupName write Set_GroupName;

  end;

  TReservoirAreaGroupList = class(TAbstractAppObject,IReservoirAreaGroupList)
  protected
    FReservoirAreaGroupList : TObjectList;
    FAreaGroupCount         : integer;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function ValidateReservoirAreaGroupCount (AErrorMessages : TStrings) : Boolean;
    function GetCastReservoirAreaGroupByIndex(AIndex : Integer): TReservoirAreaGroup;
  public
    function Initialise : boolean; override;
    function NewReservoirAreaGroup : TReservoirAreaGroup;
    function CreateNewReservoirAreaGroup : TReservoirAreaGroup;
    function DeleteReservoirAreaGroupWithID(AGroupID : integer) : WordBool;
    function DeleteReservoirAreaGroupWithIndex(AIndex : integer) : WordBool;
    function CastReservoirAreaGroupByID(AGroupID: integer): TReservoirAreaGroup;

    function CreateReservoirAreaGroup : IReservoirAreaGroup; safecall;
    function RemoveReservoirAreaGroup (AGroupID : integer) : WordBool; safecall;
    function Get_GroupAreaCount: Integer; safecall;
    procedure Set_GroupAreaCount(Value: Integer); safecall;
    function ReservoirAreaGroupByIndex(AIndex : integer) : IReservoirAreaGroup; safecall;
    function ReservoirAreaGroupByID(AGroupID : integer) : IReservoirAreaGroup; safecall;
    function ReservoirAreaGroupByName(const AName: WideString): IReservoirAreaGroup; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;

    property CastReservoirAreaGroupByIndex[AIndex: Integer]: TReservoirAreaGroup read GetCastReservoirAreaGroupByIndex;

    property GroupAreaCount : integer read Get_GroupAreaCount;
  end;


implementation

uses
  SysUtils,
  Math,
  UConstants,
  UReservoirDataSQLAgent,
  UYieldModelDataObject,
  UErrorHandlingOperations;

{ TReservoirAreaGroup }

procedure TReservoirAreaGroup.CreateMemberObjects;
const OPNAME = 'TReservoirAreaGroup.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirAreaGroup.DestroyMemberObjects;
const OPNAME = 'TReservoirAreaGroup.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreaGroup.Get_GroupID: integer;
const OPNAME = 'TReservoirAreaGroup.Get_GroupID';
begin
  Result := 0;
  try
    Result := FGroupID;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreaGroup.Get_GroupName: WideString;
const OPNAME = 'TReservoirAreaGroup.Get_GroupName';
begin
  Result := '';
  try
    Result := FGroupName;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreaGroup.Initialise: boolean;
const OPNAME = 'TReservoirAreaGroup.Initialise';
begin
  Result := inherited Initialise;
  try
    FGroupID    := 0;
    FGroupName  := '';
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreaGroup.Populate(AGroupID: integer;AGroupName: WideString): WordBool;
const OPNAME = 'TReservoirAreaGroup.Populate';
begin
  Result := FALSE;
  try
    FGroupID   := AGroupID;
    FGroupName := AGroupName;
    Result     := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreaGroup.PopulateReservoirArea(AGroupID: integer;
                                                   AGroupName: WideString): WordBool;
const OPNAME = 'TReservoirAreaGroup.PopulateReservoirArea';
begin
  Result := FALSE;
  try
    FGroupID   := AGroupID;
    FGroupName := AGroupName;
    Result     := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirAreaGroup.Set_GroupName(const AName: WideString);
const OPNAME = 'TReservoirAreaGroup.Set_GroupName';
var
  LLoadAgent   : TReservoirDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TReservoirDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_ReservoirGroupAreaID(LContextData, IntToStr(FGroupID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'ReservoirAreaGroupName', AName, FGroupName, LContextData) then
        begin
          LOldValue  := FGroupName;
          FGroupName := AName;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ReservoirAreaGroupName',LOldValue,AName);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreaGroup.Validate(var AErrors: WideString;const AContext: WideString): WordBool;
const OPNAME = 'TReservoirAreaGroup.Validate';
var
  lErrorMessage : TStringList;
begin
   Result := FALSE;
   try
     lErrorMessage := TStringList.Create;
     try
       if (AContext = 'ReservoirGroupName') then
         Result := ValidateReservoirGroupName(lErrorMessage)
       else
       begin
         Result := TRUE;
         if (NOT ValidateReservoirGroupName(lErrorMessage)) then
           Result := FALSE;
       end;
     finally
       lErrorMessage.Free;
     end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreaGroup.ValidateReservoirGroupName(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TReservoirAreaGroup.ValidateReservoirGroupName';
var
  lMessage : string;
begin
  Result := True;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('ReservoirGroupName', Trim(FGroupName), lMessage)) then
      AErrorMessages.Add('WARNING:' +lMessage);
    //else
    //  Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreaGroup._AddRef: Integer;
const OPNAME = 'TReservoirAreaGroup._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreaGroup._Release: Integer;
const OPNAME = 'TReservoirAreaGroup._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


{ TReservoirAreaGroupList }

function TReservoirAreaGroupList._AddRef: Integer;
const OPNAME = 'TReservoirAreaGroupList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreaGroupList._Release: Integer;
const OPNAME = 'TReservoirAreaGroupList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreaGroupList.CreateReservoirAreaGroup: IReservoirAreaGroup;
const OPNAME = 'TReservoirAreaGroupList.CreateReservoirAreaGroup';
var
  LReservoirAreaGroup : TReservoirAreaGroup;
begin
  Result := nil;
  try
    LReservoirAreaGroup := CreateNewReservoirAreaGroup;
    Result              := LReservoirAreaGroup;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirAreaGroupList.CreateMemberObjects;
const OPNAME = 'TReservoirAreaGroupList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FReservoirAreaGroupList := TObjectList.Create;
 except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirAreaGroupList.DestroyMemberObjects;
const OPNAME = 'TReservoirAreaGroupList.DestroyMemberObjects';
begin
  try
    FreeAndNil(FReservoirAreaGroupList);
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreaGroupList.Get_GroupAreaCount: Integer;
const OPNAME = 'TReservoirAreaGroupList.Get_GroupAreaCount';
begin
  Result := 0;
  try
    Result := FReservoirAreaGroupList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreaGroupList.ReservoirAreaGroupByID(AGroupID: integer): IReservoirAreaGroup;
const OPNAME = 'TReservoirAreaGroupList.ReservoirAreaGroupByID';
var
  LIndex              : integer;
  LReservoirAreaGroup : TReservoirAreaGroup;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FReservoirAreaGroupList.Count)) do
    begin
      lReservoirAreaGroup := TReservoirAreaGroup(FReservoirAreaGroupList.Items[lIndex]);
      if (lReservoirAreaGroup.FGroupID = AGroupID) then
        Result := lReservoirAreaGroup
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreaGroupList.ReservoirAreaGroupByIndex(AIndex: integer): IReservoirAreaGroup;
const OPNAME = 'TReservoirAreaGroupList.ReservoirAreaGroupByIndex';
var
  lReservoirAreaGroup : TReservoirAreaGroup;
begin
  Result := nil;
  try
    if ((AIndex >= 0) AND (AIndex < FReservoirAreaGroupList.Count)) then
    begin
      lReservoirAreaGroup := TReservoirAreaGroup(FReservoirAreaGroupList.Items[AIndex]);
      Result := lReservoirAreaGroup;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreaGroupList.Initialise: boolean;
const OPNAME = 'TReservoirAreaGroupList.Initialise';
begin
  Result := inherited Initialise;
  try
    FAreaGroupCount := 0;
    FReservoirAreaGroupList.Clear;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


{function TReservoirAreaGroupList.RemoveReservoirAreaGroup(AGroupID: integer): WordBool;
const OPNAME = 'TReservoirAreaGroupList.RemoveReservoirAreaGroup';
var
  lLoadAgent : TNetworkFeaturesSQLAgent;
begin
  Result := FALSE;
  try
    if (AGroupID > 0) then
    begin
      LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
      try
        if LLoadAgent.DeleteReservoirArea(AGroupID) then
        begin
          DeleteReservoirAreaGroupWithID(AGroupID);
          Result := TRUE;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end; }

procedure TReservoirAreaGroupList.Set_GroupAreaCount(Value: Integer);
const OPNAME = 'TReservoirAreaGroupList.Set_GroupAreaCount';
begin

end;

function TReservoirAreaGroupList.NewReservoirAreaGroup: TReservoirAreaGroup;
const OPNAME = 'TReservoirAreaGroupList.NewReservoirAreaGroup';
var
  lReservoirAreaGroup : TReservoirAreaGroup;
begin
  Result := nil;
  try
    lReservoirAreaGroup := TReservoirAreaGroup.Create(FAppModules);
    FReservoirAreaGroupList.Add(lReservoirAreaGroup);
    Result := lReservoirAreaGroup;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreaGroupList.DeleteReservoirAreaGroupWithID(AGroupID: integer): WordBool;
const OPNAME = 'TReservoirAreaGroupList.DeleteReservoirAreaGroupWithID';
var
  lReservoirAreaGroup : TReservoirAreaGroup;
  lIndex       : integer;
begin
  Result := FALSE;
  try
    lReservoirAreaGroup := CastReservoirAreaGroupByID(AGroupID);
    if (lReservoirAreaGroup <> nil) then
    begin
      lIndex := FReservoirAreaGroupList.IndexOf(lReservoirAreaGroup);
      Result := DeleteReservoirAreaGroupWithIndex(lIndex);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreaGroupList.CastReservoirAreaGroupByID(AGroupID: integer): TReservoirAreaGroup;
const OPNAME = 'TReservoirAreaGroupList.CastReservoirAreaGroupByID';
var
 LIndex              : integer;
 LReservoirAreaGroup : TReservoirAreaGroup;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FReservoirAreaGroupList.Count)) do
    begin
      LReservoirAreaGroup := TReservoirAreaGroup(FReservoirAreaGroupList.Items[LIndex]);
      if (LReservoirAreaGroup.FGroupID = AGroupID) then
        Result := LReservoirAreaGroup
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreaGroupList.DeleteReservoirAreaGroupWithIndex(AIndex: integer): WordBool;
const OPNAME = 'TReservoirAreaGroupList.DeleteReservoirAreaGroupWithIndex';
begin
  Result := FALSE;
  try
    if (AIndex >= 0) then
    begin
      FReservoirAreaGroupList.Delete(AIndex);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreaGroupList.ReservoirAreaGroupByName(const AName: WideString): IReservoirAreaGroup;
const OPNAME = 'TReservoirAreaGroupList.ReservoirAreaGroupByName';
var
  lIndex              : integer;
  lReservoirAreaGroup : TReservoirAreaGroup;
begin
  Result := nil;
  try
    for lIndex  := 0 to FReservoirAreaGroupList.Count -1  do
    begin
      lReservoirAreaGroup := TReservoirAreaGroup(FReservoirAreaGroupList.Items[lIndex]);
      if (lReservoirAreaGroup.FGroupName = AName) then
      begin
        Result := lReservoirAreaGroup;
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreaGroupList.Validate(var AErrors: WideString;const AContext: WideString): WordBool;
const OPNAME = 'TReservoirAreaGroupList.Validate';
var
  lErrorMessage : TStringList;
begin
  Result := FALSE;
  try
    lErrorMessage := TStringList.Create;
    try
      if (AContext = 'ReservoirAreaGroupCount') then
        Result := ValidateReservoirAreaGroupCount(lErrorMessage)
      else
      begin
        Result := TRUE;
        if (NOT ValidateReservoirAreaGroupCount(lErrorMessage)) then
          Result := FALSE;
      end;
      AErrors := AErrors + lErrorMessage.Text
    finally;
      lErrorMessage.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreaGroupList.ValidateReservoirAreaGroupCount(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TReservoirAreaGroupList.ValidateReservoirAreaGroupCount';
var

  LMessage : string;
  LValue   : integer;
begin
  Result := FALSE;
  try
    LMessage := '';
    LValue   := GroupAreaCount;
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty
              ('ReservoirAreaGroupCount', IntToStr(LValue), lMessage)) then
      AErrorMessages.Add('ERROR:' +lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreaGroupList.GetCastReservoirAreaGroupByIndex(AIndex: Integer): TReservoirAreaGroup;
const OPNAME = 'TReservoirAreaGroupList.GetCastReservoirAreaGroupByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FReservoirAreaGroupList.Count) then
      Result := TReservoirAreaGroup(FReservoirAreaGroupList[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreaGroupList.CreateNewReservoirAreaGroup: TReservoirAreaGroup;
const OPNAME = 'TReservoirAreaGroupList.CreateNewReservoirAreaGroup';
var
  LNewIdentifier      : integer;
  LLoadAgent          : TReservoirDataSQLAgent;
  LReservoirAreaGroup : TReservoirAreaGroup;
  LAreaGroupName           : string;
begin
  Result := nil;
  try
    LLoadAgent := TReservoirDataSQLAgent.Create(FAppModules);
    try
      LNewIdentifier := 0;
      if LLoadAgent.InsertReservoirAreaGroup(LNewIdentifier) then
      begin
        LReservoirAreaGroup := NewReservoirAreaGroup;
        LReservoirAreaGroup.Initialise;
        LAreaGroupName := UpperCase(FAppModules.Language.GetString('TField.ReservoirAreaGroupName')) + ' ' + IntToStr(LNewIdentifier);
        LReservoirAreaGroup.PopulateReservoirArea(LNewIdentifier, LAreaGroupName);
        Result := LReservoirAreaGroup;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreaGroupList.RemoveReservoirAreaGroup(AGroupID: integer): WordBool;
const OPNAME = 'TReservoirAreaGroupList.RemoveReservoirAreaGroup';
var
  lLoadAgent : TReservoirDataSQLAgent;
begin
  Result := FALSE;
  try
    if (AGroupID > 0) then
    begin
      LLoadAgent := TReservoirDataSQLAgent.Create(FAppModules);
      try
        if LLoadAgent.DeleteReservoirAreaGroup(AGroupID) then
        begin
          DeleteReservoirAreaGroupWithID(AGroupID);
          Result := TRUE;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.

