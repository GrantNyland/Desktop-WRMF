{******************************************************************************}
{*  UNIT      : Contains the class TChannelAreaFeature.                       *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2005/07/18                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UChannelAreas;

interface

uses
  Classes,
  Contnrs,
  UAbstractObject,
  VoaimsCom_TLB;

type

{******************************************************************************}
{* Channel Area Features                                                      *}
{******************************************************************************}

  TChannelArea = class(TAbstractAppObject, IChannelArea)
  protected
    FAreaID    : integer;
    FAreaName  : string;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function ValidateChannelAreaName (AErrorMessages : TStrings) : Boolean;

  public
    function Initialise : boolean; override;
    function Populate (AChannelAreaID: integer; AChannelAreaName : WideString ): WordBool;
    function PopulateChannelArea (AChannelAreaID  : integer; AChannelAreaName: WideString): WordBool;
    function Get_AreaID : integer; safecall;
    function Get_AreaName : WideString; safecall;
    procedure Set_AreaName (const AName : WideString); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;

    property AreaID   : integer    read Get_AreaID;
    property AreaName : WideString read Get_AreaName write Set_AreaName;

  end;

  TChannelAreaList = class(TAbstractAppObject, IChannelAreaList)
  protected
    FChanneAreaList : TObjectList;
    FAreaCount      : integer;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function ValidateChannelAreaCount (AErrorMessages : TStrings) : Boolean;

    function GetCastChannelAreaByIndex(AIndex : Integer): TChannelArea;
  public
    function Initialise : boolean; override;
    function NewChannelArea : TChannelArea;
    function CreateNewChannelArea : TChannelArea;
    function DeleteChannelAreaWithID(AChannelAreaID : integer) : WordBool;
    function DeleteChannelAreaWithIndex(AIndex : integer) : WordBool;
    function CastChannelAreaByID(AChannelAreaID: integer): TChannelArea;

    function CreateChannelArea : IChannelArea; safecall;
    function RemoveChannelArea (AChannelAreaID : integer) : WordBool; safecall;
    function Get_AreaCount: Integer; safecall;
    procedure Set_AreaCount(Value: Integer); safecall;
    function ChannelAreaByIndex(AIndex : integer) : IChannelArea; safecall;
    function ChannelAreaByID(AChannelAreaID : integer) : IChannelArea; safecall;
    function ChannelAreaByName(const AName: WideString): IChannelArea; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;

    property CastChannelAreaByIndex[AIndex: Integer]: TChannelArea read GetCastChannelAreaByIndex;
    property AreaCount : integer read Get_AreaCount;
  end;


implementation

uses
  SysUtils,
  Math,
  UConstants,
  UNetworkFeaturesSQLAgent,
  UYieldModelDataObject,
  UErrorHandlingOperations;

{ TChannelArea }

procedure TChannelArea.CreateMemberObjects;
const OPNAME = 'TChannelArea.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelArea.DestroyMemberObjects;
const OPNAME = 'TChannelArea.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelArea.Get_AreaID: integer;
const OPNAME = 'TChannelArea.Get_AreaID';
begin
  Result := 0;
  try
    Result := FAreaID;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TChannelArea.Get_AreaName: WideString;
const OPNAME = 'TChannelArea.Get_AreaName';
begin
  Result := '';
  try
    Result := FAreaName;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TChannelArea.Initialise: boolean;
const OPNAME = 'TChannelArea.Initialise';
begin
  Result := inherited Initialise;
  try
    FAreaID    := 0;
    FAreaName  := '';
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelArea.Populate(AChannelAreaID: integer;AChannelAreaName: WideString): WordBool;
const OPNAME = 'TChannelArea.Populate';
begin
  Result := FALSE;
  try
    FAreaID   := AChannelAreaID;
    FAreaName := AChannelAreaName;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelArea.PopulateChannelArea(AChannelAreaID: integer;
                                          AChannelAreaName: WideString): WordBool;
const OPNAME = 'TChannelArea.PopulateChannelArea';
begin
  Result := FALSE;
  try
    FAreaID   := AChannelAreaID;
    FAreaName := AChannelAreaName;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelArea.Set_AreaName(const AName: WideString);
const OPNAME = 'TChannelArea.Set_AreaName';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_ChannelAreaID(LContextData, IntToStr(FAreaID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'ChannelAreaName', AName, FAreaName, LContextData) then
        begin
          LOldValue        := FAreaName;
          FAreaName := AName;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ChannelAreaName',LOldValue,AName);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelArea.Validate(var AErrors: WideString;const AContext: WideString): WordBool;
const OPNAME = 'TChannelArea.Validate';
var
  lErrorMessage : TStringList;
begin
   Result := FALSE;
   try
     lErrorMessage := TStringList.Create;
     try
       if (AContext = 'ChannelAreaName') then
         Result := ValidateChannelAreaName(lErrorMessage)
       else
       begin
         Result := TRUE;
         if (NOT ValidateChannelAreaName(lErrorMessage)) then
           Result := FALSE;
       end;
     finally
       lErrorMessage.Free;
     end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelArea.ValidateChannelAreaName(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TChannelArea.ValidateChannelAreaName';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('ChannelAreaName', Trim(FAreaName), lMessage)) then
      AErrorMessages.Add('WARNING:' +lMessage);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelArea._AddRef: Integer;
const OPNAME = 'TChannelArea._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelArea._Release: Integer;
const OPNAME = 'TChannelArea._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


{ TChannelAreaList }

function TChannelAreaList._AddRef: Integer;
const OPNAME = 'TChannelAreaList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelAreaList._Release: Integer;
const OPNAME = 'TChannelAreaList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelAreaList.CreateChannelArea: IChannelArea;
const OPNAME = 'TChannelAreaList.CreateChannelArea';
var
  lChannelArea : TChannelArea;
begin
  Result := nil;
  try
    lChannelArea := CreateNewChannelArea;
    Result := lChannelArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelAreaList.CreateMemberObjects;
const OPNAME = 'TChannelAreaList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FChanneAreaList := TObjectList.Create;
 except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelAreaList.DestroyMemberObjects;
const OPNAME = 'TChannelAreaList.DestroyMemberObjects';
begin
  try
    FreeAndNil(FChanneAreaList);
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelAreaList.Get_AreaCount: Integer;
const OPNAME = 'TChannelAreaList.Get_AreaCount';
begin
  Result := 0;
  try
    Result := FChanneAreaList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelAreaList.ChannelAreaByID(AChannelAreaID: integer): IChannelArea;
const OPNAME = 'TChannelAreaList.ChannelAreaByID';
var
  lIndex       : integer;
  lChannelArea : TChannelArea;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FChanneAreaList.Count)) do
    begin
      lChannelArea := TChannelArea(FChanneAreaList.Items[lIndex]);
      if (lChannelArea.FAreaID = AChannelAreaID) then
        Result := lChannelArea
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelAreaList.ChannelAreaByIndex(AIndex: integer): IChannelArea;
const OPNAME = 'TChannelAreaList.ChannelAreaByIndex';
var
  lChannelArea : TChannelArea;
begin
  Result := nil;
  try
    if ((AIndex >= 0) AND (AIndex < FChanneAreaList.Count)) then
    begin
      lChannelArea := TChannelArea(FChanneAreaList.Items[AIndex]);
      Result := lChannelArea;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelAreaList.Initialise: boolean;
const OPNAME = 'TChannelAreaList.Initialise';
begin
  Result := inherited Initialise;
  try
    FAreaCount := 0;
    FChanneAreaList.Clear;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TChannelAreaList.RemoveChannelArea(AChannelAreaID: integer): WordBool;
const OPNAME = 'TChannelAreaList.RemoveChannelArea';
var
  lLoadAgent : TNetworkFeaturesSQLAgent;
begin
  Result := FALSE;
  try
    if (AChannelAreaID > 0) then
    begin
      LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
      try
        if LLoadAgent.DeleteChannelArea(AChannelAreaID) then
        begin
          DeleteChannelAreaWithID(AChannelAreaID);
          Result := TRUE;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelAreaList.Set_AreaCount(Value: Integer);
const OPNAME = 'TChannelAreaList.Set_AreaCount';
begin

end;

function TChannelAreaList.CreateNewChannelArea: TChannelArea;
const OPNAME = 'TChannelAreaList.CreateNewChannelArea';
var
  lNewIdentifier      : integer;
  lLoadAgent          : TNetworkFeaturesSQLAgent;
  lChannelArea        : TChannelArea;
  lAreaName           : string;
begin
  Result := nil;
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LNewIdentifier := 0;
      if LLoadAgent.InsertChannelArea(LNewIdentifier) then
      begin
        lChannelArea := NewChannelArea;
        lChannelArea.Initialise;
        lAreaName := UpperCase(FAppModules.Language.GetString('TField.ChannelArea')) + ' ' + IntToStr(lNewIdentifier);
        lChannelArea.PopulateChannelArea(LNewIdentifier, lAreaName);
        Result := lChannelArea;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelAreaList.NewChannelArea: TChannelArea;
const OPNAME = 'TChannelAreaList.NewChannelArea';
var
  lChannelArea : TChannelArea;
begin
  Result := nil;
  try
    lChannelArea := TChannelArea.Create(FAppModules);
    FChanneAreaList.Add(lChannelArea);
    Result := lChannelArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelAreaList.DeleteChannelAreaWithID(AChannelAreaID: integer): WordBool;
const OPNAME = 'TChannelAreaList.DeleteChannelAreaWithID';
var
  lChannelArea : TChannelArea;
  lIndex       : integer;
begin
  Result := FALSE;
  try
    lChannelArea := CastChannelAreaByID(AChannelAreaID);
    if (lChannelArea <> nil) then
    begin
      lIndex := FChanneAreaList.IndexOf(lChannelArea);
      Result := DeleteChannelAreaWithIndex(lIndex);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TChannelAreaList.CastChannelAreaByID(AChannelAreaID: integer): TChannelArea;
const OPNAME = 'TChannelAreaList.CastChannelAreaByID';
var
 lIndex      : integer;
 lChannelArea: TChannelArea;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FChanneAreaList.Count)) do
    begin
      lChannelArea := TChannelArea(FChanneAreaList.Items[lIndex]);
      if (lChannelArea.FAreaID = AChannelAreaID) then
        Result := lChannelArea
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelAreaList.DeleteChannelAreaWithIndex(AIndex: integer): WordBool;
const OPNAME = 'TChannelAreaList.DeleteChannelAreaWithIndex';
begin
  Result := FALSE;
  try
    if (AIndex >= 0) then
    begin
      FChanneAreaList.Delete(AIndex);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelAreaList.ChannelAreaByName(const AName: WideString): IChannelArea;
const OPNAME = 'TChannelAreaList.ChannelAreaByName';
var
  lIndex       : integer;
  lChannelArea : TChannelArea;
begin
  Result := nil;
  try
    for lIndex  := 0 to FChanneAreaList.Count -1  do
    begin
      lChannelArea := TChannelArea(FChanneAreaList.Items[lIndex]);
      if (lChannelArea.FAreaName = AName) then
      begin
        Result := lChannelArea;
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelAreaList.Validate(var AErrors: WideString;const AContext: WideString): WordBool;
const OPNAME = 'TChannelAreaList.Validate';
var
  lErrorMessage : TStringList;
begin
  Result := FALSE;
  try
    lErrorMessage := TStringList.Create;
    try
      if (AContext = 'ChannelAreaCount') then
        Result := ValidateChannelAreaCount(lErrorMessage)
      else
      begin
        Result := TRUE;
        if (NOT ValidateChannelAreaCount(lErrorMessage)) then
          Result := FALSE;
      end;
      AErrors := AErrors + lErrorMessage.Text
    finally;
      lErrorMessage.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelAreaList.ValidateChannelAreaCount(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TChannelAreaList.ValidateChannelAreaCount';
var
  lMessage : string;
  lValue   : integer;
begin
  Result := FALSE;
  try
    lMessage := '';
    lValue   := AreaCount;
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty
              ('ChannelAreaCount', IntToStr(lValue), lMessage)) then
      AErrorMessages.Add('ERROR:' +lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelAreaList.GetCastChannelAreaByIndex(AIndex: Integer): TChannelArea;
const OPNAME = 'TChannelAreaList.GetCastChannelAreaByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FChanneAreaList.Count) then
      Result := TChannelArea(FChanneAreaList[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
