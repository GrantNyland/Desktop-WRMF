{******************************************************************************}
{*  UNIT      : Contains the class TChangeData, TChangeList.                  *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/02/10                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UChangeData;

interface

uses
  Classes,
  Contnrs,
  UAbstractObject,
  VoaimsCom_TLB;

type
  TParameterChange = class;
  TParameterChangeEvent = procedure(AParamField, AKeyValues, AFieldIndex : WideString; AParameterChange : TParameterChange) of object;
  TParameterChange = class(TAbstractAppObject, IParameterChange)
  protected
    FChangeListID   : integer;
    FParamField     : string;
    FKeyValues      : string;
    FFieldIndex     : string;
    FAbsolut        : boolean;
    FChange         : string;
    FParamDescr     : string;
    FFiltered       : wordbool;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure LoadContextDataChangeParam (AContextData : TStringList);
  public
    function Initialise: Boolean; override;
    procedure Populate (AChangeListID : integer;
                        AParameter    : string;
                        AKeyValues    : string;
                        AFieldIndex   : string;
                        AAbsolut      : boolean;
                        AChange       : string;
                        AParamDescr   : string;
                        AFiltered     : boolean);
    function Get_ChangeListID: Integer; safecall;
    function Get_ParamField: WideString; safecall;
    function Get_KeyValues: WideString; safecall;
    function Get_FieldIndex: WideString; safecall;
    function Get_Absolut: WordBool; safecall;
    procedure Set_Absolut(Value: WordBool); safecall;
    function Get_Change: WideString; safecall;
    procedure Set_Change(const Value: WideString); safecall;
    function Get_ParamDescr: WideString; safecall;
    procedure Set_ParamDescr(const Value: WideString); safecall;
    property ChangeListID  : Integer    read Get_ChangeListID;
    property ParamField    : WideString read Get_ParamField;
    property KeyValues     : WideString read Get_KeyValues;
    property FieldIndex    : Widestring    read Get_FieldIndex;
    property Absolut       : WordBool   read Get_Absolut      write Set_Absolut;
    property Change        : WideString read Get_Change       write Set_Change;
  end;

  TChangeList = class(TAbstractAppObject, IChangeList)
  protected
    FChangeListID    : integer;
    FChangeListKey   : string;
    FChangeListName  : string;
    FDateCreated     : TDateTime;
    FCreatedBy       : string;
    FDescription     : string;
    FParamChanges    : TStringList;
    FMarkForDelete   : Boolean;
    FIsResident      : Boolean;
    FOnParameterChangeAdded   : TParameterChangeEvent;
    FOnParameterChangeDeleted : TParameterChangeEvent;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure ClearParamChanges;
    procedure LoadContextDataChangeList (AContextData : TStringList);
    function DBCreateNewParamChange (AParamField  : string;
                                     AKeyValues   : string;
                                     AFieldIndex  : string;
                                     AAbsolut     : string;
                                     AChange      : string;
                                     AParamDescr  : string;
                                     AFiltered    : boolean): Boolean;
    function DBDeleteParamChange (AParamField : string;
                                  AKeyValues  : string;
                                  AFieldIndex : string) : Boolean;
  public
    { Public declarations }
    function Initialise: Boolean; override;
    procedure Populate (AChangeListID    : integer;
                        AChangeListKey   : string;
                        AIsResident      : boolean;
                        AChangeListName  : string;
                        ADateCreated     : TDateTime;
                        ACreatedBy       : string;
                        ADescription     : string);
    function Get_ChangeListID : integer; safecall;
    function Get_IsResident: WordBool; safecall;
    function Get_ChangeListName : WideString; safecall;
    procedure Set_ChangeListName (const AName : WideString); safecall;
    function Get_DateCreated : TDateTime; safecall;
    function Get_CreatedBy : WideString; safecall;
    procedure Set_CreatedBy (const ACreatedBy : WideString); safecall;
    function Get_Description : WideString; safecall;
    procedure Set_Description (const ADescr : WideString); safecall;
    function FindParamChange (const AParamField : WideString;
                              const AKeyValues  : WideString;
                              const AFieldIndex : WideString): IParameterChange; safecall;
    function CreateNewParamChange (const AParamField : WideString;
                                   const AKeyValues  : WideString;
                                   const AFieldIndex : WideString;
                                   const AAbsolut    : WideString;
                                   const AChange     : WideString;
                                   const AParamDescr : WideString;
                                         AFiltered   : wordbool): IParameterChange; safecall;
    procedure DeleteParamChange (const AParamField : WideString;
                                 const AKeyValues  : WideString;
                                 const AFieldIndex  : WideString); safecall;
    function ParamChangeByIndex(AIndex: Integer): IParameterChange; safecall;
    function ParamChangeCount: Integer; safecall;
    procedure ApplyAllParamChanges;
    function CreateParamChange(const AParamField : WideString;
                               const AKeyValues  : WideString;
                               const AFieldIndex : WideString ) : TParameterChange;
    function CastFindParamChange (AParamField : WideString;
                                  AKeyValues  : WideString;
                                  AFieldIndex : WideString): TParameterChange;
    function GetKeyValues (const AParamField : WideString; const AFieldIndex: WideString) : WideString; safecall;
    property ChangeListID    : integer    read Get_ChangeListID;
    property IsResident      : WordBool   read Get_IsResident;
    property ChangeListName  : WideString read Get_ChangeListName write Set_ChangeListName;
    property DateCreated     : TDateTime  read Get_DateCreated;
    property CreatedBy       : WideString read Get_CreatedBy      write Set_CreatedBy;
    property Description     : WideString read Get_Description    write Set_Description;
    property MarkForDelete   : Boolean    read FMarkForDelete     write FMarkForDelete;
    property OnParameterChangeAdded     : TParameterChangeEvent    read FOnParameterChangeAdded     write FOnParameterChangeAdded;
    property OnParameterChangeDeleted   : TParameterChangeEvent    read FOnParameterChangeDeleted   write FOnParameterChangeDeleted;
  end;

  TChangeGroupElement = class(TAbstractAppObject, IChangeGroupElement)
  protected
    FGroupID        : integer;
    FElementID      : integer;
    FIsElementGroup : Boolean;
    FOrder          : integer;
    FActive         : Boolean;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure LoadContextDataChangeGroupElement (AContextData : TStringList);

    function Get_GroupID: Integer; safecall;
    function Get_ElementID: Integer; safecall;
    function Get_IsElementGroup: WordBool; safecall;
    function Get_ElementOrder: Integer; safecall;
    procedure Set_ElementOrder(Value: Integer); safecall;
    function Get_ElementActive: WordBool; safecall;
    procedure Set_ElementActive(Value: WordBool); safecall;
  public
    function Initialise: Boolean; override;
    procedure Populate (AGroupID        : integer;
                        AElementID      : integer;
                        AIsElementGroup : Boolean;
                        AOrder          : integer;
                        AActive         : Boolean);
    property GroupID: Integer read Get_GroupID;
    property ElementID: Integer read Get_ElementID;
    property IsElementGroup: WordBool read Get_IsElementGroup;
    property ElementOrder: Integer read Get_ElementOrder write Set_ElementOrder;
    property ElementActive: WordBool read Get_ElementActive write Set_ElementActive;
  end;

  TChangeGroup = class(TAbstractAppObject, IChangeGroup)
  protected
    FGroupID       : integer;
    FGroupName     : string;
    FElements      : TList;
    FParentGroupID : integer;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function Get_GroupID: Integer; safecall;
    function Get_GroupName: WideString; safecall;
    procedure Set_GroupName(const Value: WideString); safecall;
    function Get_ParentGroupID: Integer; safecall;
    procedure Set_ParentGroupID (Value: Integer); safecall;
    function DBNewChangeGroupElement (AElementID      : integer;
                                      AIsElementGroup : string;
                                      AOrder          : integer;
                                      AActive         : string): Boolean;
    function DBRemoveChangeGroupElement (AElementID      : integer;
                                         AIsElementGroup : string) : Boolean;
    procedure LoadContextDataChangeGroup (AContextData : TStringList);
  public
    function Initialise: Boolean; override;
    procedure Populate (AGroupID    : integer;
                        AGroupName  : string);
    procedure ClearGroupElements;
    function CastChangeGroupElementByIndex (AIndex : integer): TChangeGroupElement;
    function CastChangeGroupElementByID (AElementID      : integer;
                                         AIsElementGroup : WordBool): TChangeGroupElement;
    function CreateChangeGroupElement : TChangeGroupElement;
    function DeleteChangeGroupElementByID (AElementID      : integer;
                                           AIsElementGroup : WordBool) : WordBool;
    function DeleteChangeGroupElementByIndex (AIndex : integer) : WordBool;
    function ElementCount: Integer; safecall;
    function ContainsChangeLists: WordBool; safecall;
    function ChangeGroupElementByIndex (AIndex : Integer): IChangeGroupElement; safecall;
    function ChangeGroupElementByID (AElementID      : Integer;
                                     AIsElementGroup : WordBool): IChangeGroupElement; safecall;
    function NewChangeGroupElement (AElementID      : Integer;
                                    AIsElementGroup : WordBool): IChangeGroupElement; safecall;
    function RemoveChangeGroupElementByID (AElementID      : Integer;
                                           AIsElementGroup : WordBool): WordBool; safecall;
    function RemoveChangeGroupElementByIndex (AIndex : Integer): WordBool; safecall;
    function MoveUpChangeGroupElement (AElementID      : Integer;
                                       AIsElementGroup : WordBool): WordBool; safecall;
    function MoveDownChangeGroupElement (AElementID      : Integer;
                                         AIsElementGroup : WordBool): WordBool; safecall;

    property GroupID: Integer read Get_GroupID;
    property GroupName: WideString read Get_GroupName write Set_GroupName;
    property ParentGroupID : Integer read Get_ParentGroupID write Set_ParentGroupID;
  end;

implementation

uses
  SysUtils,
  UDataSetType,
  UDBConstants,
  UErrorHandlingOperations,
  Math;

{******************************************************************************}
{* TParameterChange                                                           *}
{******************************************************************************}

function TParameterChange._AddRef: Integer;
const OPNAME = 'TParameterChange._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParameterChange._Release: Integer;
const OPNAME = 'TParameterChange._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TParameterChange.CreateMemberObjects;
const OPNAME = 'TParameterChange.CreateMemberObjects';
begin
  inherited;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TParameterChange.DestroyMemberObjects;
const OPNAME = 'TParameterChange.DestroyMemberObjects';
begin
  try
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParameterChange.Initialise: Boolean;
const OPNAME = 'TParameterChange.Initialise';
begin
  Result := inherited Initialise;
  try
    FParamField     := '';
    FFieldIndex     := '';
    FChangeListID   := 0;
    FAbsolut        := FALSE;
    FChange         := '';
    FParamDescr     := '';
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TParameterChange.Populate (AChangeListID : integer;
                                     AParameter    : string;
                                     AKeyValues    : string;
                                     AFieldIndex   : string;
                                     AAbsolut      : boolean;
                                     AChange       : string;
                                     AParamDescr   : string;
                                     AFiltered     : boolean);
const OPNAME = 'TParameterChange.Populate';
begin
  try
    FChangeListID := AChangeListID;
    FParamField   := AParameter;
    FKeyValues    := AKeyValues;
    FFieldIndex   := AFieldIndex;
    FAbsolut      := AAbsolut;
    FChange       := AChange;
    FParamDescr   := AParamDescr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParameterChange.Get_ChangeListID: Integer;
const OPNAME = 'TParameterChange.Get_ChangeListID';
begin
  Result := 0;
  try
    Result := FChangeListID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParameterChange.Get_ParamField: WideString;
const OPNAME = 'TParameterChange.Get_ParamField';
begin
  Result := '';
  try
    Result := FParamField;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParameterChange.Get_KeyValues: WideString;
const OPNAME = 'TParameterChange.Get_KeyValues';
begin
  Result := '';
  try
    Result := FKeyValues;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParameterChange.Get_FieldIndex: WideString;
const OPNAME = 'TParameterChange.Get_FieldIndex';
begin
  Result := '';
  try
    Result := FFieldIndex;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParameterChange.Get_Absolut: WordBool;
const OPNAME = 'TParameterChange.Get_Absolut';
begin
  Result := FALSE;
  try
    Result := FAbsolut;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParameterChange.Get_Change: WideString;
const OPNAME = 'TParameterChange.Get_Change';
begin
  Result := '';
  try
    Result := FChange;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParameterChange.Get_ParamDescr: WideString;
const OPNAME = 'TParameterChange.Get_ParamDescr';
begin
  Result := '';
  try
    Result := FParamDescr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TParameterChange.Set_Absolut (Value: WordBool);
const OPNAME = 'TParameterChange.Set_Absolut';
var
  lContextData : TStringList;
  lOldValue    : string;
  lNewValue    : string;
begin
  try
    lContextData := TStringList.Create;
    try
      if (FAbsolut) then lOldValue := 'Y' else lOldValue := 'N';
      if (Value)    then lNewValue := 'Y' else lNewValue := 'N';
      LoadContextDataChangeParam(lContextData);
      if FAppModules.FieldProperties.UpdateFieldValue(
           'ChangeParamAbsolut', lNewValue, lOldValue, lContextData) then
      begin
        FAbsolut := Value;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'ChangeParamAbsolut',lOldValue,lNewValue);
      end;
    finally
      LContextData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TParameterChange.Set_Change(const Value: WideString);
const OPNAME = 'TParameterChange.Set_Change';
var
  lContextData : TStringList;
  lOldValue    : string;
begin
  try
    lContextData := TStringList.Create;
    try
      lOldValue := FChange;
      LoadContextDataChangeParam(lContextData);
      if FAppModules.FieldProperties.UpdateFieldValue(
           'ChangeParamChange', Value, lOldValue, lContextData) then
      begin
        FChange := Value;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'ChangeParamChange',lOldValue,FChange);
      end;
    finally
      LContextData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TParameterChange.Set_ParamDescr(const Value: WideString);
const OPNAME = 'TParameterChange.Set_ParamDescr';
var
  lContextData : TStringList;
  lOldValue    : string;
begin
  try
    lContextData := TStringList.Create;
    try
      lOldValue := FParamDescr;
      LoadContextDataChangeParam(lContextData);
      if FAppModules.FieldProperties.UpdateFieldValue(
           'ChangeParamDescr', Value, lOldValue, lContextData) then
      begin
        FParamDescr := Value;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'ChangeParamDescr',lOldValue,FChange);
      end;
    finally
      LContextData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TParameterChange.LoadContextDataChangeParam(AContextData : TStringList);
const OPNAME = 'TParameterChange.LoadContextDataChangeParam';
begin
  try
    AContextData.Clear;
    AContextData.Add('ChangeListID='  + IntToStr(FChangeListID));
    AContextData.Add('ParamField='    + FParamField);
    AContextData.Add('KeyValues='     + FKeyValues);
    AContextData.Add('FieldIndex='    + FFieldIndex);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

{******************************************************************************}
{* TChangeList                                                                *}
{******************************************************************************}

function TChangeList._AddRef: Integer;
const OPNAME = 'TChangeList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeList._Release: Integer;
const OPNAME = 'TChangeList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeList.CreateMemberObjects;
const OPNAME = 'TChangeList.CreateMemberObjects';
begin
  inherited;
  try
    FParamChanges             := TStringList.Create;
    FParamChanges.Sorted      := True;
    FParamChanges.Duplicates  := dupAccept;
    FOnParameterChangeAdded   := nil;
    FOnParameterChangeDeleted := nil;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeList.DestroyMemberObjects;
const OPNAME = 'TChangeList.DestroyMemberObjects';
begin
  try
    ClearParamChanges;
    FreeAndNil(FParamChanges);
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeList.Initialise: Boolean;
const OPNAME = 'TChangeList.Initialise';
begin
  Result := inherited Initialise;
  try
    FChangeListID    := 0;
    FChangeListKey   := '';
    FDateCreated     := 0;
    FChangeListName  := '';
    FCreatedBy       := '';
    FDescription     := '';
    FMarkForDelete   := FALSE;
    FIsResident      := FALSE;
    ClearParamChanges;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeList.ClearParamChanges;
const OPNAME = 'TChangeList.ClearParamChanges';
var
  lParamChange : TParameterChange;
begin
  try
    while (FParamChanges.Count > 0) do
    begin
      lParamChange := TParameterChange(FParamChanges.Objects[0]);
      if(lParamChange <> nil) then
      begin
        if Assigned(FOnParameterChangeDeleted) then
           FOnParameterChangeDeleted(lParamChange.FParamField,lParamChange.FKeyValues,lParamChange.FFieldIndex,lParamChange);
      end;
      FParamChanges.Delete(0);
      FreeAndNil(lParamChange);
    end;
    FParamChanges.Clear;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeList.Populate (AChangeListID   : integer;
                                AChangeListKey  : string;
                                AIsResident     : boolean;
                                AChangeListName : string;
                                ADateCreated    : TDateTime;
                                ACreatedBy      : string;
                                ADescription    : string);
const OPNAME = 'TChangeList.Populate';
begin
  try
    FChangeListID    := AChangeListID;
    FChangeListKey   := AChangeListKey;
    FIsResident      := AIsResident;
    FChangeListName  := AChangeListName;
    FDateCreated     := ADateCreated;
    FCreatedBy       := ACreatedBy;
    FDescription     := ADescription;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeList.Get_ChangeListID : integer;
const OPNAME = 'TChangeList.Get_ChangeListID';
begin
  Result := 0;
  try
    Result := FChangeListID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeList.Get_IsResident : WordBool;
const OPNAME = 'TChangeList.Get_IsResident';
begin
  Result := FALSE;
  try
    Result := FIsResident;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeList.Get_ChangeListName: WideString;
const OPNAME = 'TChangeList.Get_ChangeListName';
begin
  Result := '';
  try
    Result := FChangeListName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeList.Get_CreatedBy: WideString;
const OPNAME = 'TChangeList.Get_CreatedBy';
begin
  Result := '';
  try
    Result := FCreatedBy;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeList.Get_DateCreated: TDateTime;
const OPNAME = 'TChangeList.Get_DateCreated';
begin
  Result := 0;
  try
    Result := FDateCreated;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeList.Get_Description: WideString;
const OPNAME = 'TChangeList.Get_Description';
begin
  Result := '';
  try
    Result := FDescription;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeList.LoadContextDataChangeList (AContextData : TStringList);
const OPNAME = 'TChangeList.LoadContextDataChangeList';
begin
  try
    AContextData.Clear;
    AContextData.Add('ChangeListID='  + IntToStr(FChangeListID));
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TChangeList.Set_ChangeListName(const AName: WideString);
const OPNAME = 'TChangeList.Set_ChangeListName';
var
  lContextData : TStringList;
  lOldValue    : string;
begin
  try
    lContextData := TStringList.Create;
    try
      lOldValue := FChangeListName;
      LoadContextDataChangeList(lContextData);
      if FAppModules.FieldProperties.UpdateFieldValue(
           'ChangeListName', AName, lOldValue, lContextData) then
      begin
        FChangeListName := AName;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'ChangeListName',lOldValue,AName);
      end;
    finally
      LContextData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeList.Set_CreatedBy(const ACreatedBy: WideString);
const OPNAME = 'TChangeList.Set_CreatedBy';
var
  lContextData : TStringList;
  lOldValue    : string;
begin
  try
    lContextData := TStringList.Create;
    try
      lOldValue := FCreatedBy;
      LoadContextDataChangeList(lContextData);
      if FAppModules.FieldProperties.UpdateFieldValue(
           'ChangeListCreatedBy', ACreatedBy, lOldValue, lContextData) then
      begin
        FCreatedBy := ACreatedBy;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'ChangeListCreatedBy',lOldValue,ACreatedBy);
      end;
    finally
      LContextData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeList.Set_Description(const ADescr: WideString);
const OPNAME = 'TChangeList.Set_Description';
var
  lContextData : TStringList;
  lOldValue    : string;
begin
  try
    lContextData := TStringList.Create;
    try
      lOldValue := FDescription;
      LoadContextDataChangeList(lContextData);
      if FAppModules.FieldProperties.UpdateFieldValue(
           'ChangeListDescription', ADescr, lOldValue, lContextData) then
      begin
        FDescription := ADescr;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'ChangeListDescription',lOldValue,ADescr);
      end;
    finally
      LContextData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeList.CastFindParamChange(AParamField : WideString;
                                         AKeyValues  : WideString;
                                         AFieldIndex : WideString): TParameterChange;
const OPNAME = 'TChangeList.CastFindParamChange';
var
  lIndex       : integer;
  LData        : TStringList;
  LKey         : string;
begin
  Result := nil;
  try
    LData := TStringList.Create;
    try
      LData.Add(AParamField);
      LData.Add(AKeyValues);
      LData.Add(AFieldIndex);
      LKey := UpperCase(LData.CommaText);
      lIndex :=  FParamChanges.IndexOf(LKey);
      if(lIndex >= 0) then
        Result :=  TParameterChange(FParamChanges.Objects[lIndex]);
    finally
      LData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeList.FindParamChange (const AParamField : WideString;
                                      const AKeyValues  : WideString;
                                      const AFieldIndex : WideString): IParameterChange;
const OPNAME = 'TChangeList.FindParamChange';
begin
  Result := nil;
  try
    Result := CastFindParamChange(AParamField, AKeyValues, AFieldIndex);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeList.ParamChangeByIndex(AIndex: Integer): IParameterChange;
const OPNAME = 'TChangeList.ParamChangeByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FParamChanges.Count) then
      Result := TParameterChange(FParamChanges.Objects[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeList.ParamChangeCount: Integer; safecall;
const OPNAME = 'TChangeList.ParamChangeCount';
begin
  Result := 0;
  try
    Result := FParamChanges.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeList.DeleteParamChange (const AParamField : WideString;
                                         const AKeyValues  : WideString;
                                         const AFieldIndex : WideString); safecall;
const OPNAME = 'TChangeList.DeleteParamChange';
var
  lParamChange : TParameterChange;
  LIndex : integer;
  LData        : TStringList;
  LKey         : string;

begin
  try
    if (DBDeleteParamChange(AParamField, AKeyValues, AFieldIndex)) then
    begin
      lParamChange := CastFindParamChange(AParamField, AKeyValues, AFieldIndex);
      if (lParamChange <> nil) then
      begin

        LData := TStringList.Create;
        try
          LData.Add(AParamField);
          LData.Add(AKeyValues);
          LData.Add(AFieldIndex);
          LKey := UpperCase(LData.CommaText);
        finally
          LData.Free;
        end;
        if Assigned(FOnParameterChangeDeleted) then
            FOnParameterChangeDeleted(lParamChange.FParamField,lParamChange.FKeyValues,lParamChange.FFieldIndex,lParamChange);
        LIndex :=  FParamChanges.IndexOf(LKey);
        FParamChanges.Delete(LIndex);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeList.CreateNewParamChange (const AParamField : WideString;
                                           const AKeyValues  : WideString;
                                           const AFieldIndex : WideString;
                                           const AAbsolut    : WideString;
                                           const AChange     : WideString;
                                           const AParamDescr : WideString;
                                                 AFiltered   : wordbool): IParameterChange;
const OPNAME = 'TChangeList.CreateNewParamChange';
var
  lParamChange : TParameterChange;
begin
  Result := nil;
  try
    if (DBCreateNewParamChange(AParamField, AKeyValues, AFieldIndex, AAbsolut, AChange,AParamDescr,AFiltered)) then
    begin
      lParamChange := CreateParamChange(AParamField, AKeyValues, AFieldIndex);
      lParamChange.Populate
                    (FChangeListID, AParamField, AKeyValues, AFieldIndex, (AAbsolut = 'Y'), AChange,AParamDescr,AFiltered);
      if Assigned(FOnParameterChangeAdded) then
         FOnParameterChangeAdded(AParamField, AKeyValues, AFieldIndex,lParamChange);
      Result := lParamChange;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeList.CreateParamChange(const AParamField : WideString;
                                       const AKeyValues  : WideString;
                                       const AFieldIndex : WideString): TParameterChange;
const OPNAME = 'TChangeList.CreateParamChange';
var
  LParamChange : TParameterChange;
  LData        : TStringList;
begin
  Result := nil;
  try
    LParamChange := TParameterChange.Create(FAppModules);
    LData := TStringList.Create;
    try
      LData.Add(AParamField);
      LData.Add(AKeyValues);
      LData.Add(AFieldIndex);
      FParamChanges.AddObject(UpperCase(LData.CommaText),LParamChange);
      if Assigned(FOnParameterChangeAdded) then
         FOnParameterChangeAdded(AParamField,AKeyValues,AFieldIndex,LParamChange);
      Result := LParamChange;
    finally
      LData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeList.DBCreateNewParamChange (AParamField  : string;
                                             AKeyValues   : string;
                                             AFieldIndex  : string;
                                             AAbsolut     : string;
                                             AChange      : string;
                                             AParamDescr  : string;
                                             AFiltered    : boolean): Boolean;
const OPNAME = 'TChangeList.DBCreateNewParamChange';
var
  lDataSet  : TAbstractModelDataset;
  lSQL      : string;
  LFiltered : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lDataset.DataSet.Close;
        lSQL := 'INSERT INTO ChangeParameter '+
                '(Model,StudyAreaName,SubArea,Scenario,ChangeListID, ParamField,'+
                ' KeyValues, FieldIndex, Absolut, Change,ParamDescr,Filtered)'+
                ' VALUES '+
                '(:Model,:StudyAreaName,:SubArea,:Scenario,:ChangeListID, :ParamField,'+
                ' :KeyValues, :FieldIndex, :Absolut, :Change,:ParamDescr,:Filtered)';

         if AFiltered then
           LFiltered := 'Y'
        else LFiltered := 'N';
        LDataSet.SetSQL(lSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['ChangeListID'], [IntToStr(FChangeListID)]);
        LDataSet.SetParams(['ParamField'], [AParamField]);
        LDataSet.SetParams(['KeyValues'], [AKeyValues]);
        LDataSet.SetParams(['FieldIndex'], [AFieldIndex]);
        LDataSet.SetParams(['Absolut'], [AAbsolut]);
        LDataSet.SetParams(['Change'], [AChange]);
        LDataSet.SetParams(['ParamDescr'], [AParamDescr]);
        LDataSet.SetParams(['Filtered'], [LFiltered]);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;
        Result := TRUE;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeList.DBDeleteParamChange (AParamField : string;
                                          AKeyValues  : string;
                                          AFieldIndex : string) : Boolean;
const OPNAME = 'TChangeList.DBDeleteParamChange';
var
  lDataSet : TAbstractModelDataset;
  lSQL     : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lDataset.DataSet.Close;
        lSQL := 'DELETE FROM ChangeParameter ' +
                ' WHERE  '+ AppModules.Model.GetChangeListWhereClause+
                ' AND ChangeListID      = ' + IntToStr(FChangeListID) +
                ' AND ParamField    = ' + QuotedStr(AParamField) +
                ' AND KeyValues     = ' + QuotedStr(AKeyValues) +
                ' AND FieldIndex    = ' + QuotedStr(AFieldIndex);
        LDataSet.SetSQL(lSQL);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;
        Result := TRUE;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeList.ApplyAllParamChanges;
const OPNAME = 'TChangeList.ApplyAllParamChanges';
var
  lParamChange    : TParameterChange;
  lBaseValue      : string;
  lFloatValue     : double;
  lIntValue       : integer;
  lFieldProperty  : TAbstractFieldProperty;
begin
  try
    while (FParamChanges.Count > 0) do
    begin
      lParamChange := TParameterChange(FParamChanges[0]);
      lBaseValue   := FAppModules.Model.GetBaseValue(lParamChange.ParamField,
                                                     lParamChange.KeyValues,
                                                     lParamChange.FieldIndex);
      lFieldProperty := FAppModules.FieldProperties.FieldProperty(lParamChange.ParamField);
      case lFieldProperty.FieldDataType of
        FieldFloatType   :
        begin
          try
            lFloatValue := StrToFloat(lBaseValue);
            if (lParamChange.Absolut) then
              lFloatValue := StrToFloat(Trim(lParamChange.Change))
            else
              lFloatValue := (1 + StrToFloat(Trim(lParamChange.Change)) / 100) * lFloatValue;
            if (lFieldProperty.FormatStringGrid = '') then
              lBaseValue := FloatToStr(lFloatValue)
            else
              lBaseValue := Format(lFieldProperty.FormatStringGrid, [lFloatValue]);
            FAppModules.Model.SetBaseValue(lParamChange.ParamField,
                                           lParamChange.KeyValues,
                                           lParamChange.FieldIndex,
                                           lBaseValue);
          except
          end;
        end;
        FieldIntegerType :
        begin
          try
            lIntValue := StrToInt(lBaseValue);
            if (lParamChange.Absolut) then
              lIntValue := StrToInt(Trim(lParamChange.Change))
            else
              lIntValue := ROUND((1 + StrToFloat(Trim(lParamChange.Change)) / 100) * lIntValue);
            if (lFieldProperty.FormatStringGrid = '') then
              lBaseValue := IntToStr(lIntValue)
            else
              lBaseValue := Format(lFieldProperty.FormatStringGrid, [lIntValue]);
            FAppModules.Model.SetBaseValue(lParamChange.ParamField,
                                           lParamChange.KeyValues,
                                           lParamChange.FieldIndex,
                                           lBaseValue);
          except
          end;
        end;
      else
      end;
      DeleteParamChange(lParamChange.ParamField,
                        lParamChange.KeyValues,
                        lParamChange.FieldIndex);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeList.GetKeyValues (const AParamField : WideString;
                                   const AFieldIndex : WideString) : WideString;
const OPNAME = 'TChangeList.GetKeyValues';
begin
  Result := '';
  try
    Result := FChangeListKey;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{******************************************************************************}
{* TChangeGroupElement                                                        *}
{******************************************************************************}

function TChangeGroupElement._AddRef: Integer;
const OPNAME = 'TChangeGroupElement._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeGroupElement._Release: Integer;
const OPNAME = 'TChangeGroupElement._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeGroupElement.CreateMemberObjects;
const OPNAME = 'TChangeGroupElement.CreateMemberObjects';
begin
  inherited;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeGroupElement.DestroyMemberObjects;
const OPNAME = 'TChangeGroupElement.DestroyMemberObjects';
begin
  try
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeGroupElement.Initialise: Boolean;
const OPNAME = 'TChangeGroupElement.Initialise';
begin
  Result := inherited Initialise;
  try
    FGroupID        := 0;
    FElementID      := 0;
    FIsElementGroup := FALSE;
    FOrder          := 0;
    FActive         := TRUE;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeGroupElement.Populate (AGroupID        : integer;
                                        AElementID      : integer;
                                        AIsElementGroup : Boolean;
                                        AOrder          : integer;
                                        AActive         : Boolean);
const OPNAME = 'TChangeGroupElement.Populate';
begin
  try
    FGroupID        := AGroupID;
    FElementID      := AElementID;
    FIsElementGroup := AIsElementGroup;
    FOrder          := AOrder;
    FActive         := AActive;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeGroupElement.LoadContextDataChangeGroupElement (AContextData : TStringList);
const OPNAME = 'TChangeGroupElement.LoadContextDataChangeGroupElement';
begin
  try
    AContextData.Clear;
    AContextData.Add('GroupID='  + IntToStr(FGroupID));
    AContextData.Add('ElementID='  + IntToStr(FElementID));
    if (FIsElementGroup) then
      AContextData.Add('IsElementGroup=Y')
    else
      AContextData.Add('IsElementGroup=N');
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TChangeGroupElement.Get_ElementActive: WordBool;
const OPNAME = 'TChangeGroupElement.Get_ElementActive';
begin
  Result := FALSE;
  try
    Result := FActive;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeGroupElement.Get_ElementID: Integer;
const OPNAME = 'TChangeGroupElement.Get_ElementID';
begin
  Result := 0;
  try
    Result := FElementID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeGroupElement.Get_ElementOrder: Integer;
const OPNAME = 'TChangeGroupElement.Get_ElementOrder';
begin
  Result := 0;
  try
    Result := FOrder;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeGroupElement.Get_GroupID: Integer;
const OPNAME = 'TChangeGroupElement.Get_GroupID';
begin
  Result := 0;
  try
    Result := FGroupID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeGroupElement.Get_IsElementGroup: WordBool;
const OPNAME = 'TChangeGroupElement.Get_IsElementGroup';
begin
  Result := FALSE;
  try
    Result := FIsElementGroup;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeGroupElement.Set_ElementActive(Value: WordBool);
const OPNAME = 'TChangeGroupElement.Set_ElementActive';
var
  lContextData : TStringList;
  lOldValue    : string;
  lNewValue    : string;
  lGroup       : IChangeGroup;
  lElement     : IChangeGroupElement;
  lCount       : integer;
begin
  try
    lContextData := TStringList.Create;
    try
      if (FActive) then
        lOldValue := 'Y'
      else
        lOldValue := 'N';
      if (Value) then
        lNewValue := 'Y'
      else
        lNewValue := 'N';
      LoadContextDataChangeGroupElement(lContextData);
      if FAppModules.FieldProperties.UpdateFieldValue(
           'ElementActive', lNewValue, lOldValue, lContextData) then
      begin
        FActive := Value;
        if (FIsElementGroup) then
        begin
          lGroup := FAppModules.Changes.ChangeGroupWithID(FElementID);
          if (lGroup <> nil) then
          begin
            for lCount := 0 to lGroup.ElementCount - 1 do
            begin
              lElement := lGroup.ChangeGroupElementByIndex(lCount);
              lElement.ElementActive := Value;
            end;
          end;
        end;
      end;
    finally
      LContextData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeGroupElement.Set_ElementOrder(Value: Integer);
const OPNAME = 'TChangeGroupElement.Set_ElementOrder';
var
  lContextData : TStringList;
  lOldValue    : string;
  lNewValue    : string;
begin
  try
    lContextData := TStringList.Create;
    try
      lOldValue := IntToStr(FOrder);
      lNewValue := IntToStr(Value);
      LoadContextDataChangeGroupElement(lContextData);
      if FAppModules.FieldProperties.UpdateFieldValue(
           'ElementOrder', lNewValue, lOldValue, lContextData) then
      begin
        FOrder := Value;
      end;
    finally
      LContextData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{******************************************************************************}
{* TChangeGroup                                                               *}
{******************************************************************************}

function TChangeGroup._AddRef: Integer;
const OPNAME = 'TChangeGroup._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeGroup._Release: Integer;
const OPNAME = 'TChangeGroup._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeGroup.CreateMemberObjects;
const OPNAME = 'TChangeGroup.CreateMemberObjects';
begin
  inherited;
  try
    FElements := TList.Create;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeGroup.DestroyMemberObjects;
const OPNAME = 'TChangeGroup.DestroyMemberObjects';
begin
  try
    ClearGroupElements;
    FreeAndNil(FElements);
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeGroup.Initialise: Boolean;
const OPNAME = 'TChangeGroup.Initialise';
begin
  Result := inherited Initialise;
  try
    FGroupID        := 0;
    FGroupName      := '';
    FParentGroupID  := 0;
    ClearGroupElements;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeGroup.Populate (AGroupID   : integer;
                                 AGroupName : string);
const OPNAME = 'TChangeGroup.Populate';
begin
  try
    FGroupID   := AGroupID;
    FGroupName := AGroupName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeGroup.ClearGroupElements;
const OPNAME = 'TChangeGroup.ClearGroupElements';
var
  lElement : TChangeGroupElement;
begin
  try
    while (FElements.Count > 0) do
    begin
      lElement := TChangeGroupElement(FElements.Items[0]);
      FElements.Delete(0);
      FreeAndNil(lElement);
    end;
    FElements.Clear;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeGroup.Get_GroupID: Integer;
const OPNAME = 'TChangeGroup.Get_GroupID';
begin
  Result := 0;
  try
    Result := FGroupID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeGroup.Get_GroupName: WideString;
const OPNAME = 'TChangeGroup.Get_GroupName';
begin
  Result := '';
  try
    Result := FGroupName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeGroup.Set_GroupName (const Value: WideString);
const OPNAME = 'TChangeGroup.Set_GroupName';
var
  lContextData : TStringList;
  lOldValue    : string;
begin
  try
    lContextData := TStringList.Create;
    try
      lOldValue := FGroupName;
      LoadContextDataChangeGroup(lContextData);
      if FAppModules.FieldProperties.UpdateFieldValue(
           'ChangeGroupName', Value, lOldValue, lContextData) then
      begin
        FGroupName := Value;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'ChangeGroupName',lOldValue,Value);
      end;
    finally
      LContextData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeGroup.Get_ParentGroupID: Integer;
const OPNAME = 'TChangeGroup.Get_ParentGroupID';
begin
  Result := 0;
  try
    Result := FParentGroupID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeGroup.Set_ParentGroupID  (Value: Integer);
const OPNAME = 'TChangeGroup.Set_ParentGroupID';
begin
  try
    FParentGroupID := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeGroup.NewChangeGroupElement (AElementID      : integer;
                                             AIsElementGroup : WordBool): IChangeGroupElement;
const OPNAME = 'TChangeGroup.NewChangeGroupElement';
var
  lElement     : TChangeGroupElement;
  lOrder       : integer;
  lIsGroup     : string;
  lSelfActive  : Boolean;
  lParent      : IChangeGroup;
  lSelfElement : IChangeGroupElement;
begin
  Result := nil;
  try
    lOrder := FElements.Count + 1;
    if (AIsElementGroup) then
      lIsGroup := 'Y'
    else
      lIsGroup := 'N';

    lSelfActive := FALSE;
    if (FParentGroupID >= 0) then
    begin
      lParent := FAppModules.Changes.ChangeGroupWithID(FParentGroupID);
      if (lParent <> nil) then
      begin
        lSelfElement := lParent.ChangeGroupElementByID(FGroupID, TRUE);
        if (lSelfElement <> nil) then
          lSelfActive := lSelfElement.ElementActive;
      end;
    end;

    if (DBNewChangeGroupElement(AElementID, lIsGroup, lOrder, 'N'{lIsActive})) then
    begin
      lElement := CreateChangeGroupElement;
      lElement.Populate(FGroupID, AElementID, AIsElementGroup, lOrder, FALSE{lActive});
      lElement.ElementActive := lSelfActive;
      Result := lElement;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeGroup.CreateChangeGroupElement : TChangeGroupElement;
const OPNAME = 'TChangeGroup.CreateChangeGroupElement';
var
  lElement : TChangeGroupElement;
begin
  Result := nil;
  try
    lElement := TChangeGroupElement.Create(FAppModules);
    FElements.Add(lElement);
    Result := lElement;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeGroup.DeleteChangeGroupElementByID (AElementID      : integer;
                                                    AIsElementGroup : WordBool) : WordBool;
const OPNAME = 'TChangeGroup.DeleteChangeGroupElementByID';
var
  lIndex   : integer;
  lElement : TChangeGroupElement;
begin
  Result := FALSE;
  try
    lIndex := 0;
    while (NOT Result) AND (lIndex < FElements.Count) do
    begin
      lElement := CastChangeGroupElementByIndex(lIndex);
      if (lElement.FElementID = AElementID) AND (lElement.FIsElementGroup = AIsElementGroup) then
        Result := DeleteChangeGroupElementByIndex(lIndex)
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeGroup.DeleteChangeGroupElementByIndex (AIndex : integer) : WordBool;
const OPNAME = 'TChangeGroup.DeleteChangeGroupElementByIndex';
var
  lElement : TChangeGroupElement;
  lCount   : integer;
begin
  Result := FALSE;
  try
    lElement := CastChangeGroupElementByIndex(AIndex);
    if (lElement <> nil) then
    begin
      FElements.Delete(AIndex);
      FreeAndNil(lElement);
      for lCount := AIndex to FElements.Count - 1 do
      begin
        lElement := CastChangeGroupElementByIndex(lCount);
        lElement.ElementOrder := lCount + 1;
      end;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeGroup.DBNewChangeGroupElement (AElementID      : integer;
                                               AIsElementGroup : string;
                                               AOrder          : integer;
                                               AActive         : string): Boolean;
const OPNAME = 'TChangeGroup.DBNewChangeGroupElement';
var
  lDataSet : TAbstractModelDataset;
  lSQL     : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lDataset.DataSet.Close;
        lSQL := 'INSERT INTO ChangeGroupElement '+
                '(Model,StudyAreaName,SubArea,Scenario,GroupID, ElementID, IsElementGroup, ElementActive, ElementOrder)'+
                ' VALUES '+
                '(:Model,:StudyAreaName,:SubArea,:Scenario,:GroupID,:ElementID,:IsElementGroup,:ElementActive,:ElementOrder)';

        LDataSet.SetSQL(lSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['GroupID'], [IntToStr(FGroupID)]);
        LDataSet.SetParams(['ElementID'], [IntToStr(AElementID)]);
        LDataSet.SetParams(['IsElementGroup'], [AIsElementGroup]);
        LDataSet.SetParams(['ElementActive'], [AActive]);
        LDataSet.SetParams(['ElementOrder'], [IntToStr(AOrder)]);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;
        Result := TRUE;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeGroup.RemoveChangeGroupElementByID (AElementID      : integer;
                                                    AIsElementGroup : WordBool) : WordBool;
const OPNAME = 'TChangeGroup.RemoveChangeGroupElementByID';
var
  lElement : TChangeGroupElement;
  lIsGroup : string;
  lIndex   : integer;
begin
  Result := FALSE;
  try
    lIndex := 0;
    while (NOT Result) AND (lIndex < FElements.Count) do
    begin
      lElement := CastChangeGroupElementByIndex(lIndex);
      if (lElement <> nil) AND (lElement.ElementID = AElementID) AND
         (lElement.IsElementGroup = AIsElementGroup) then
      begin
        if (AIsElementGroup) then
          lIsGroup := 'Y'
        else
          lIsGroup := 'N';
        if (DBRemoveChangeGroupElement(AElementID, lIsGroup)) then
        begin
          DeleteChangeGroupElementByIndex(lIndex);
          Result := TRUE;
        end;
      end
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeGroup.RemoveChangeGroupElementByIndex (AIndex : integer) : WordBool;
const OPNAME = 'TChangeGroup.RemoveChangeGroupElementByIndex';
var
  lElement : TChangeGroupElement;
  lIsGroup : string;
begin
  Result := FALSE;
  try
    lElement := CastChangeGroupElementByIndex(AIndex);
    if (lElement <> nil) then
    begin
      if (lElement.FIsElementGroup) then
        lIsGroup := 'Y'
      else
        lIsGroup := 'N';
      if (DBRemoveChangeGroupElement(lElement.FElementID, lIsGroup)) then
      begin
        DeleteChangeGroupElementByIndex(AIndex);
        Result := TRUE;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeGroup.DBRemoveChangeGroupElement (AElementID      : integer;
                                                  AIsElementGroup : string) : Boolean;
const OPNAME = 'TChangeGroup.DBRemoveChangeGroupElement';
var
  lDataSet : TAbstractModelDataset;
  lSQL     : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lDataset.DataSet.Close;
        lSQL := 'DELETE FROM ChangeGroupElement WHERE ' +
                ' WHERE  '+ AppModules.Model.GetChangeListWhereClause+
                ' AND GroupID       = ' + IntToStr(FGroupID) +
                ' AND ElementID = ' + IntToStr(AElementID) +
                ' AND IsElementGroup = ' + QuotedStr(AIsElementGroup);
        LDataSet.SetSQL(lSQL);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;
        Result := TRUE;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeGroup.ElementCount: Integer;
const OPNAME = 'TChangeGroup.ElementCount';
begin
  Result := 0;
  try
    Result := FElements.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeGroup.ContainsChangeLists: WordBool;
const OPNAME = 'TChangeGroup.ContainsChangeLists';
var
  lIndex : integer;
begin
  Result := FALSE;
  try
    lIndex := 0;
    while (NOT Result) AND (lIndex < FElements.Count) do
    begin
      if (NOT TChangeGroupElement(FElements.Items[lIndex]).FIsElementGroup) then
        Result := TRUE
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeGroup.ChangeGroupElementByIndex (AIndex : Integer): IChangeGroupElement;
const OPNAME = 'TChangeGroup.ChangeGroupElementByIndex';
begin
  Result := nil;
  try
    Result := CastChangeGroupElementByIndex(AIndex);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeGroup.ChangeGroupElementByID (AElementID      : Integer;
                                              AIsElementGroup : WordBool): IChangeGroupElement; safecall;
const OPNAME = 'TChangeGroup.ChangeGroupElementByID';
begin
  Result := nil;
  try
    Result := CastChangeGroupElementByID(AElementID, AIsElementGroup);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeGroup.CastChangeGroupElementByID (AElementID      : integer;
                                                  AIsElementGroup : WordBool): TChangeGroupElement;
const OPNAME = 'TChangeGroup.CastChangeGroupElementByID';
var
  lIndex   : integer;
  lElement : TChangeGroupElement;
begin
  Result := nil;
  try
    lIndex := 0;
    while (Result = nil) AND (lIndex < FElements.Count) do
    begin
      lElement := CastChangeGroupElementByIndex(lIndex);
      if (lElement.FElementID = AElementID) AND (lElement.FIsElementGroup = AIsElementGroup) then
        Result := lElement
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeGroup.CastChangeGroupElementByIndex (AIndex : integer): TChangeGroupElement;
const OPNAME = 'TChangeGroup.CastChangeGroupElementByIndex';
begin
  Result := nil;
  try
    if (AIndex < FElements.Count) then
      Result := TChangeGroupElement(FElements.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeGroup.LoadContextDataChangeGroup (AContextData : TStringList);
const OPNAME = 'TChangeGroup.LoadContextDataChangeGroup';
begin
  try
    AContextData.Clear;
    AContextData.Add('GroupID='  + IntToStr(FGroupID));
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TChangeGroup.MoveUpChangeGroupElement (AElementID      : Integer;
                                                AIsElementGroup : WordBool): WordBool; safecall;
const OPNAME = 'TChangeGroup.MoveUpChangeGroupElement';
var
  lElement : TChangeGroupElement;
  lIndex   : integer;
  lCount   : integer;
begin
  Result := FALSE;
  try
    if (AElementID <> 0) then
    begin
      lElement := CastChangeGroupElementByID(AElementID, AIsElementGroup);
      if (lElement <> nil) then
      begin
        lIndex := lElement.ElementOrder - 1;
        FElements.Delete(lIndex);
        FElements.Insert(lIndex - 1, lElement);
        for lCount := lIndex - 1 to FElements.Count - 1 do
        begin
          lElement := TChangeGroupElement(FElements.Items[lCount]);
          lElement.ElementOrder := lCount + 1;
        end;
        Result := TRUE;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TChangeGroup.MoveDownChangeGroupElement (AElementID      : Integer;
                                                  AIsElementGroup : WordBool): WordBool; safecall;
const OPNAME = 'TChangeGroup.MoveDownChangeGroupElement';
var
  lElement : TChangeGroupElement;
  lIndex   : integer;
  lCount   : integer;
begin
  Result := FALSE;
  try
    if (AElementID <> 0) then
    begin
      lElement := CastChangeGroupElementByID(AElementID, AIsElementGroup);
      if (lElement <> nil) then
      begin
        lIndex := lElement.ElementOrder - 1;
        FElements.Delete(lIndex);
        FElements.Insert(lIndex + 1, lElement);
        for lCount := lIndex to FElements.Count - 1 do
        begin
          lElement := TChangeGroupElement(FElements.Items[lCount]);
          lElement.ElementOrder := lCount + 1;
        end;
        Result := TRUE;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

end.
