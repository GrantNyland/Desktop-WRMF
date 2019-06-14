{******************************************************************************}
{*  UNIT      : Contains the class TChangeData, TChangeList.                  *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/02/10                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UMetaData;

interface

uses
  Classes,
  UAbstractObject,
  VoaimsCom_TLB;

type
  TMetaData = class(TAbstractAppObject, IMetaData)
  protected
    FMetaDataListID : integer;
    FParamField     : string;
    FKeyValues      : string;
    FFieldIndex     : string;
    FDateCreated    : TDateTime;
    FCreatedBy      : string;
    FComment        : string;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure LoadContextData (AContextData : TStringList);
  public
    function Initialise: Boolean; override;
    procedure Populate (AListID       : integer;
                        AParameter    : string;
                        AKeyValues    : string;
                        AFieldIndex   : string;
                        ADateCreated  : TDateTime;
                        ACreatedBy    : string;
                        AComment      : string);
    function Get_ParamField: WideString; safecall;
    function Get_FieldIndex: WideString; safecall;
    function Get_KeyValues: WideString; safecall;
    function Get_DateCreated: TDateTime; safecall;
    procedure Set_DateCreated(Value: TDateTime); safecall;
    function Get_CreatedBy: WideString; safecall;
    procedure Set_CreatedBy(const Value: WideString); safecall;
    function Get_Comment: WideString; safecall;
    procedure Set_Comment(const Value: WideString); safecall;
    property ParamField: WideString read Get_ParamField;
    property FieldIndex: WideString read Get_FieldIndex;
    property KeyValues: WideString read Get_KeyValues;
    property DateCreated: TDateTime read Get_DateCreated write Set_DateCreated;
    property CreatedBy: WideString read Get_CreatedBy write Set_CreatedBy;
    property Comment: WideString read Get_Comment write Set_Comment;
  end;

implementation

uses
  SysUtils,
  UDataSetType,
  UErrorHandlingOperations,
  Math;

{* TMetaData ******************************************************************}

function TMetaData._AddRef: Integer;
const OPNAME = 'TMetaData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMetaData._Release: Integer;
const OPNAME = 'TMetaData._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMetaData.CreateMemberObjects;
const OPNAME = 'TMetaData.CreateMemberObjects';
begin
  inherited;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMetaData.DestroyMemberObjects;
const OPNAME = 'TMetaData.DestroyMemberObjects';
begin
  try
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMetaData.Initialise: Boolean;
const OPNAME = 'TMetaData.Initialise';
begin
  Result := inherited Initialise;
  try
    FParamField  := '';
    FFieldIndex  := '';
    FKeyValues   := '';
    FDateCreated := 0;
    FCreatedBy   := '';
    FComment     := '';
    Result       := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMetaData.Populate (AListID       : integer;
                              AParameter    : string;
                              AKeyValues    : string;
                              AFieldIndex   : string;
                              ADateCreated  : TDateTime;
                              ACreatedBy    : string;
                              AComment      : string);
const OPNAME = 'TMetaData.Populate';
begin
  try
    FMetaDataListID := AListID;
    FParamField     := AParameter;
    FKeyValues      := AKeyValues;
    FFieldIndex     := AFieldIndex;
    FDateCreated    := ADateCreated;
    FCreatedBy      := ACreatedBy;
    FComment        := AComment;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMetaData.Get_ParamField : WideString;
const OPNAME = 'TMetaData.Get_ParamField';
begin
  Result := '';
  try
    Result := FParamField;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMetaData.Get_KeyValues : WideString;
const OPNAME = 'TMetaData.Get_KeyValues';
begin
  Result := '';
  try
    Result := FKeyValues;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMetaData.Get_FieldIndex : WideString;
const OPNAME = 'TMetaData.Get_FieldIndex';
begin
  Result := '';
  try
    Result := FFieldIndex;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMetaData.Get_DateCreated : TDateTime;
const OPNAME = 'TMetaData.Get_DateCreated';
begin
  Result := 0;
  try
    Result := FDateCreated;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMetaData.Get_CreatedBy : WideString;
const OPNAME = 'TMetaData.Get_CreatedBy';
begin
  Result := '';
  try
    Result := FCreatedBy;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMetaData.Get_Comment : WideString;
const OPNAME = 'TMetaData.Get_Comment';
begin
  Result := '';
  try
    Result := FComment;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMetaData.Set_DateCreated (Value : TDateTime);
const OPNAME = 'TMetaData.Set_DateCreated';
var
  lContextData : TStringList;
  lOldValue    : string;
  lNewValue    : string;
begin
  try
    lContextData := TStringList.Create;
    try
      LoadContextData(lContextData);
      lOldValue := FormatDateTime('dd/mm/yyyy', FDateCreated);
      lNewValue := FormatDateTime('dd/mm/yyyy', Value);
      if FAppModules.FieldProperties.UpdateFieldValue(
           'MetaDataDateCreated', lNewValue, lOldValue, lContextData) then
      begin
        FDateCreated := Value;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'MetaDataDateCreated',lOldValue,lNewValue);
      end;
    finally
      LContextData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMetaData.Set_CreatedBy (const Value : WideString);
const OPNAME = 'TMetaData.Set_CreatedBy';
var
  lContextData : TStringList;
  lOldValue    : string;
begin
  try
    lContextData := TStringList.Create;
    try
      lOldValue := FCreatedBy;
      LoadContextData(lContextData);
      if FAppModules.FieldProperties.UpdateFieldValue(
           'MetaDataCreatedBy', Value, lOldValue, lContextData) then
      begin
        FCreatedBy := Value;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'MetaDataCreatedBy',lOldValue,FCreatedBy);
      end;
    finally
      LContextData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMetaData.Set_Comment (const Value : WideString);
const OPNAME = 'TMetaData.Set_Comment';
var
  lContextData : TStringList;
  lOldValue    : string;
begin
  try
    lContextData := TStringList.Create;
    try
      lOldValue := FComment;
      LoadContextData(lContextData);
      if FAppModules.FieldProperties.UpdateFieldValue(
           'MetaDataComment', Value, lOldValue, lContextData) then
      begin
        FComment := Value;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'MetaDataComment',lOldValue,FComment);
      end;
    finally
      LContextData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMetaData.LoadContextData (AContextData : TStringList);
const OPNAME = 'TMetaData.LoadContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('MetaDataListID=' + IntToStr(FMetaDataListID));
    AContextData.Add('ParamField='     + FParamField);
    AContextData.Add('KeyValues='      + FKeyValues);
    AContextData.Add('FieldIndex='     + FFieldIndex);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

end.
