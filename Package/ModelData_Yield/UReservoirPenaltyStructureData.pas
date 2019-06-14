//
//
//  UNIT      : Contains TReservoirLevelsData Class
//  AUTHOR    : Titi Ngubane (PDNA)
//  DATE      : 2003/03/05
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UReservoirPenaltyStructureData;

interface

uses
  Contnrs,
  Classes,
  UAbstractObject,
//  UAbstractReservoirPenaltyData,
  VoaimsCom_TLB;

type
  TReservoirPenaltyCounts = class(TAbstractAppObject,IReservoirPenaltyCounts)
  protected
    FStorageZoneCount: integer;
    FZoneRuleCurve: integer;
    FPenaltyStructureCount: integer;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function Initialise: boolean; override;
    function PopulatePenaltyCounts(ADataSet: TAbstractModelDataset): boolean;

    function Get_StorageZoneCount: integer; safecall;
    function Get_PenaltyStructureCount: integer; safecall;
    function Get_ZoneRuleCurve: integer; safecall;
    procedure Set_ZoneRuleCurve(Value: integer); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function GetKeyValues (const AParamField : WideString; const AFieldIndex: WideString) : WideString; safecall;

    property StorageZoneCount: Integer read Get_StorageZoneCount;
    property PenaltyStructureCount: Integer read Get_PenaltyStructureCount;
    property ZoneRuleCurve: Integer read Get_ZoneRuleCurve write Set_ZoneRuleCurve;
  end;

  TReservoirPenaltyZoneData = class(TAbstractAppObject,IReservoirPenaltyZoneData)
  protected
    FZoneName: string;
    FRecordIdentifier: integer;
    FStrategyIndicator: integer;
    FBalancingVariable: integer;
    FBalancingPolicy: integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function Initialise: boolean; override;
    function PopulatePenaltyZone(ADataSet: TAbstractModelDataset; var APenaltyValueArray: array of double): boolean;

    function Get_RecordIdentifier: integer; safecall;
    function Get_ZoneName: WideString; safecall;
    procedure Set_ZoneName(const Value: WideString); safecall;
    procedure Set_StrategyIndicator(Value: integer); safecall;
    function Get_StrategyIndicator: integer; safecall;
    procedure Set_BalancingVariable(Value: integer); safecall;
    function Get_BalancingVariable: integer; safecall;
    procedure Set_BalancingPolicy(Value: integer); safecall;
    function Get_BalancingPolicy: integer; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function GetKeyValues (const AParamField : WideString; const AFieldIndex: WideString) : WideString; safecall;

    property RecordIdentifier: Integer read Get_RecordIdentifier;
    property ZoneName: WideString read Get_ZoneName write Set_ZoneName;
    property StrategyIndicator: Integer read Get_StrategyIndicator write Set_StrategyIndicator;
    property BalancingVariable: Integer read Get_BalancingVariable write Set_BalancingVariable;
    property BalancingPolicy: Integer read Get_BalancingPolicy write Set_BalancingPolicy;
  end;

  TReservoirPenalty = class(TAbstractAppObject, IReservoirPenalty)
  private
    FReservoirPenaltyID     : integer;
    FReservoirPenaltyValues : TReservoirPenaltyValuesArray;
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function ValidateRuleCurve(var AErrorMesg : String; ARuleCurve: integer) : Boolean;
  public
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Initialise: boolean; override;
    procedure AddPenaltyValue (AIndex :integer; APenaltyValue: double);
    procedure InsertPenaltyValue (AIndex : integer; APenaltyValue : double);
    function RemovePenaltyValue (AIndex : integer): boolean;
    function PopulatePenaltyStructureIdentifier(APenaltyStructureIdentifier: integer): boolean;

    function Get_ReservoirPenaltyValueByIndex(AIndex: integer): double; safecall;
    procedure Set_ReservoirPenaltyValueByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_ReservoirPenaltyID: integer; safecall;
    procedure Set_ReservoirPenaltyID (Value: Integer); safecall;
    function Get_PenaltyValueCount: integer; safecall;
    function GetBaseValue(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;

    property PenaltyValueCount: Integer read Get_PenaltyValueCount;
    property ReservoirPenaltyID : Integer read Get_ReservoirPenaltyID write Set_ReservoirPenaltyID;
    property ReservoirPenaltyValueByIndex[AIndex: Integer]: Double read Get_ReservoirPenaltyValueByIndex write Set_ReservoirPenaltyValueByIndex;
  end;

  TReservoirPenaltyStructureList = class(TAbstractAppObject,IReservoirPenaltyList)
  protected
    FPenaltyStructureData: TObjectList;
    FPenaltyZoneData: TObjectList;
    FPenaltyCountsData: TReservoirPenaltyCounts;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function GetReservoirPenaltyStructureByIdentifierCast(AReservoirPenaltyStructureIdentifier: integer): TReservoirPenalty;
    function GetReservoirPenaltyCountsCast: TReservoirPenaltyCounts;
    function GetReservoirPenaltyStructureByIndexCast(AReservoirIndex: integer): TReservoirPenalty;
    function GetReservoirPenaltyZoneByIndexCast(AIndex: integer): TReservoirPenaltyZoneData;
  public
    function Initialise: boolean; override;
    function GetAllPenaltyIdentifiers(APenaltyStructureList:TStrings): boolean; virtual;

    function AddPenaltyStructure(ACopyFromPenaltyID, AInsertBeforePenaltyID: integer): boolean; virtual;
    function DeletePenaltyStructure(APenaltyID: integer): boolean; virtual;
    function CreatePenaltyZone(AZoneLevel: integer):IReservoirPenaltyZoneData ; virtual;
    function DeletePenaltyZone(AZoneLevel: integer; var ADeleteAction: TDeleteAction): boolean ; virtual;

    // Combination adder/setter
    procedure AddReservoirPenaltyStructure(AReservoirPenaltyStructure: TReservoirPenalty);
    procedure AddReservoirPenaltyZone(AReservoirPenaltyZone: TReservoirPenaltyZoneData);
    procedure DeleteReservoirPenaltyStructure(APenaltyStructureIndex: integer);

    // Update function Current Model data intercepts when update is in progress
    function UpdateReservoirZoneName(AFieldName,ANewValue, AOldValue: string; AContextData: TStrings): boolean;
    function UpdateZoneRuleCurve(AFieldName,ANewValue, AOldValue: string; AContextData: TStrings): boolean;
    function UpdateStrategyIndicator(AFieldName,ANewValue, AOldValue: string; AContextData: TStrings): boolean;
    function UpdateBalancingVariable(AFieldName,ANewValue, AOldValue: string; AContextData: TStrings): boolean;
    function UpdateBalancingPolicy(AFieldName,ANewValue, AOldValue: string; AContextData: TStrings): boolean;
    function UpdateReservoirPenalty(AFieldName,ANewValue, AOldValue: string; AContextData: TStrings): boolean;

    function Get_ReservoirPenaltyByIdentifier(AReservoirPenaltyStructureIdentifier: integer): IReservoirPenalty; safecall;
    function Get_ReservoirPenaltyByIndex(AReservoirIndex: integer): IReservoirPenalty; safecall;
    function Get_ReservoirPenaltyZoneByIndex(AZoneIndex: integer): IReservoirPenaltyZoneData; safecall;
    function Get_ReservoirPenaltyCounts: IReservoirPenaltyCounts; safecall;
    function Get_PenaltyCount: integer; safecall;
    function Get_PenaltyZoneCount: integer; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;

    property PenaltyCount: Integer read Get_PenaltyCount;
    property PenaltyZoneCount: Integer read Get_PenaltyZoneCount;
    property ReservoirPenaltyByIndex[APenaltyIndex: Integer]: IReservoirPenalty read Get_ReservoirPenaltyByIndex;
    property ReservoirPenaltyByIdentifier[AReservoirPenaltyStructureIdentifier: Integer]: IReservoirPenalty read Get_ReservoirPenaltyByIdentifier;
    property ReservoirPenaltyZoneByIndex[AZoneIndex: Integer]: IReservoirPenaltyZoneData read Get_ReservoirPenaltyZoneByIndex;
    property ReservoirPenaltyCounts: IReservoirPenaltyCounts read Get_ReservoirPenaltyCounts;

    // Readers
    property CastReservoirPenaltyStructureByIndex[AReservoirIndex: integer]: TReservoirPenalty read GetReservoirPenaltyStructureByIndexCast;
    property CastReservoirPenaltyStructureByIdentifier[AReservoirPenaltyStructureIdentifier: integer]: TReservoirPenalty read GetReservoirPenaltyStructureByIdentifierCast;
    property CastReservoirPenaltyZoneByIndex[AIndex: integer]: TReservoirPenaltyZoneData read GetReservoirPenaltyZoneByIndexCast;
    property CastReservoirPenaltyCounts: TReservoirPenaltyCounts read GetReservoirPenaltyCountsCast;
  end;

implementation

uses
  System.Types,
  System.UITypes,
  Math,
  VCL.Controls,
  VCL.Dialogs,
  SysUtils,
  UDataSetType,
  UConstants,
  UYieldModelDataObject,
  UReservoirPenaltyStructureDataSQLAgent,
  UErrorHandlingOperations;

{ TReservoirPenalty }

function TReservoirPenalty._AddRef: Integer;
const OPNAME = 'TReservoirPenalty._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirPenalty._Release: Integer;
const OPNAME = 'TReservoirPenalty._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirPenalty.ValidateRuleCurve(var AErrorMesg: String; ARuleCurve: integer): Boolean;
const OPNAME = 'TReservoirPenalty.ValidateRuleCurve';
var
  LIndex        : integer;
  LCurrentValue : double;
  LPrevValue    : double;
begin
  Result := False;
  try
    AErrorMesg := '';
    Result := True;
    lPrevValue := NullFloat;
    for LIndex := 1 to Self.PenaltyValueCount do
    begin
      LCurrentValue  := Self.ReservoirPenaltyValueByIndex[LIndex];
      if (LPrevValue <> NullFloat) then
      begin
        if (LIndex < ARuleCurve) then
        begin
          if (LCurrentValue > LPrevValue) then
          begin
            AErrorMesg := 'ERROR:'+ 'Penalty values are not decreasing above the rule curve.';
            Result := False;
            Break;
          end;
        end
        else
        if (LIndex > ARuleCurve) then
        begin
          if (LCurrentValue < LPrevValue) then
          begin
            AErrorMesg := 'WARNING:'+ FAppModules.language.GetString('PenaltyStructure.RuleCurve');
            //Result := False;
            Break;
          end;
        end;
      end;
      if (LIndex = ARuleCurve) then
        LPrevValue := NullFloat
      else
        LPrevValue := LCurrentValue;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirPenalty.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TReservoirPenalty.Validate';
var
  LMessg: string;
  LRuleCurve: integer;
  LErrorList : TStringList;
begin
  Result := False;
  try
    lErrorList := TStringList.Create;
    try
      LMessg  := '';
      LRuleCurve := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                    CastReservoirPenaltyStructureList.CastReservoirPenaltyCounts.ZoneRuleCurve;
      if (AContext= 'RuleCurve') then
      begin
        Result:= ValidateRuleCurve(LMessg,LRuleCurve);
        if (LMessg <> '') then
         lErrorList.Add(LMessg);
      end
      else
      begin
        Result:= ValidateRuleCurve(LMessg,LRuleCurve);
        if (LMessg <> '') then
         lErrorList.Add(LMessg);
      end;
      AErrors := AErrors + lErrorList.Text;
    finally
      lErrorList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenalty.CreateMemberObjects;
const OPNAME = 'TReservoirPenalty.CreateMemberObjects';
var
  lPenaltyField : TAbstractFieldProperty;
begin
  try
    inherited CreateMemberObjects;
    lPenaltyField := FAppModules.FieldProperties.FieldProperty('ReservoirPenalty');
    if (lPenaltyField = nil) then
      raise Exception.Create('Field (ReservoirPenalty) not found in field properties');
    SetLength(FReservoirPenaltyValues, lPenaltyField.ArrayLength);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirPenalty.DestroyMemberObjects;
const OPNAME = 'TReservoirPenalty.DestroyMemberObjects';
begin
  try
    Finalize(FReservoirPenaltyValues);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirPenalty.Initialise: boolean;
const OPNAME = 'TReservoirPenalty.Initialise';
var
  lPenaltyField : TAbstractFieldProperty;
  lIndex        : integer;
begin
  Result := False;
  try
    lPenaltyField := FAppModules.FieldProperties.FieldProperty('ReservoirPenalty');
    if (lPenaltyField = nil) then
      raise Exception.Create('Field (ReservoirPenalty) not found in field properties');
    for lIndex := lPenaltyField.ArrayLow to lPenaltyField.ArrayHigh do
      FReservoirPenaltyValues[lIndex] := NullFloat;
    FReservoirPenaltyID := -1;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenalty.Get_ReservoirPenaltyValueByIndex(AIndex: integer): double;
const OPNAME = 'TReservoirPenalty.Get_ReservoirPenaltyValueByIndex';
var
  lPenaltyField : TAbstractFieldProperty;
begin
  Result := NullFloat;
  try
    lPenaltyField := FAppModules.FieldProperties.FieldProperty('ReservoirPenalty');
    if (lPenaltyField = nil) then
      raise Exception.Create('Field (ReservoirPenalty) not found in field properties');
    if (AIndex >= lPenaltyField.ArrayLow) AND (AIndex <= lPenaltyField.ArrayHigh) then
    begin
      if (FReservoirPenaltyValues[AIndex] = NullFloat) then
        Result := NullFloat
      else
        Result := StrToFloat(FAppModules.Changes.GetParameterValue
                                              ('ReservoirPenalty',
                                              GetKeyValues('ReservoirPenalty', IntToStr(AIndex)),
                                              FloatToStr(FReservoirPenaltyValues[AIndex]),
                                              IntToStr(FReservoirPenaltyID)));
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirPenalty.Set_ReservoirPenaltyValueByIndex (AIndex: Integer; Value: Double);
const OPNAME = 'TReservoirPenalty.Set_ReservoirPenaltyValueByIndex';
var
  lPenaltyField   : TAbstractFieldProperty;
  LLoadAgent      : TReservoirPenaltyStructureDataSQLAgent;
  LContextData    : TStringList;
  LFieldName      : string;
  LOldValue       : string;
  LNewValue       : string;
  LReservoirCount : integer;
  LContainer      : TObjectList;
begin
  try
    lPenaltyField := FAppModules.FieldProperties.FieldProperty('ReservoirPenalty');
    if (lPenaltyField = nil) then
      raise Exception.Create('Field (ReservoirPenalty) not found in field properties');
    if (AIndex >= lPenaltyField.ArrayLow) AND (AIndex <= lPenaltyField.ArrayHigh) then
    begin
      LContainer := TObjectList.Create(False);
      try
        TYieldModelDataObject(FAppModules.Model.ModelData).
          CastNetworkElementData.CastReservoirList.
            GetReservoirsPerPenaltyStructure(FReservoirPenaltyID,LContainer);
        LReservoirCount := LContainer.Count;
      finally
        LContainer.Free;
      end;
      if(LReservoirCount > 0) then
        ShowMessage(FAppModules.Language.GetString('Message.AffectAllReservoirs'));
      LLoadAgent := TReservoirPenaltyStructureDataSQLAgent.Create(FAppModules);
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadStorageZoneNamesContextData
            (LContextData,IntToStr(AIndex),IntToStr(FReservoirPenaltyID));
          LOldValue  := FloatToStr(FReservoirPenaltyValues[AIndex]);
          LNewValue  := FloatToStr(Value);
          if FAppModules.FieldProperties.UpdateFieldValue(
            'ReservoirPenalty', LNewValue, LOldValue, LContextData) then
          begin
            LFieldName := 'ReservoirPenalty';
            FReservoirPenaltyValues[AIndex] := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,LFieldName,LOldValue,LNewValue);
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirPenalty.Get_PenaltyValueCount: integer;
const OPNAME = 'TReservoirPenalty.Get_PenaltyValueCount';
var
  lIndex        : integer;
  lPenaltyField : TAbstractFieldProperty;
begin
  Result := 0;
  try
    lPenaltyField := FAppModules.FieldProperties.FieldProperty('ReservoirPenalty');
    if (lPenaltyField = nil) then
      raise Exception.Create('Field (ReservoirPenalty) not found in field properties');
    for lIndex := lPenaltyField.ArrayLow to lPenaltyField.ArrayHigh do
      if (FReservoirPenaltyValues[LIndex] >= 0) then
        Result := Result + 1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenalty.AddPenaltyValue (AIndex :integer; APenaltyValue: double);
const OPNAME = 'TReservoirPenalty.AddPenaltyValue';
var
  lPenaltyField : TAbstractFieldProperty;
begin
  try
    lPenaltyField := FAppModules.FieldProperties.FieldProperty('ReservoirPenalty');
    if (lPenaltyField = nil) then
      raise Exception.Create('Field (ReservoirPenalty) not found in field properties');
    if (AIndex >= lPenaltyField.ArrayLow) AND (AIndex <= lPenaltyField.ArrayHigh) then
      FReservoirPenaltyValues[AIndex] := APenaltyValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenalty.InsertPenaltyValue (AIndex : integer; APenaltyValue : double);
const OPNAME = 'TReservoirPenalty.InsertPenaltyValue';
var
  lIndex        : integer;
  lPenaltyField : TAbstractFieldProperty;
begin
  try
    lPenaltyField := FAppModules.FieldProperties.FieldProperty('ReservoirPenalty');
    if (lPenaltyField = nil) then
      raise Exception.Create('Field (ReservoirPenalty) not found in field properties');
    if (AIndex >= lPenaltyField.ArrayLow) AND (AIndex <= lPenaltyField.ArrayHigh) then
    begin
      for lIndex := lPenaltyField.ArrayHigh downto AIndex + 1 do
        FReservoirPenaltyValues[lIndex] := FReservoirPenaltyValues[lIndex-1];
      FReservoirPenaltyValues[AIndex] := APenaltyValue;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenalty.RemovePenaltyValue (AIndex : integer): boolean;
const OPNAME = 'TReservoirPenalty.RemovePenaltyValue';
var
  lIndex        : integer;
  lPenaltyField : TAbstractFieldProperty;
begin
  Result := False;
  try
    lPenaltyField := FAppModules.FieldProperties.FieldProperty('ReservoirPenalty');
    if (lPenaltyField = nil) then
      raise Exception.Create('Field (ReservoirPenalty) not found in field properties');
    if (AIndex >= lPenaltyField.ArrayLow) AND (AIndex <= lPenaltyField.ArrayHigh) then
    begin
      for lIndex := AIndex to lPenaltyField.ArrayHigh - 1 do
        FReservoirPenaltyValues[lIndex] := FReservoirPenaltyValues[lIndex+1];
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenalty.PopulatePenaltyStructureIdentifier(APenaltyStructureIdentifier: integer): boolean;
const OPNAME = 'TReservoirPenalty.PopulatePenaltyStructureIdentifier';
begin
  Result := False;
  try
    FReservoirPenaltyID := APenaltyStructureIdentifier;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenalty.Get_ReservoirPenaltyID: integer;
const OPNAME = 'TReservoirPenalty.Get_ReservoirPenaltyID';
begin
  Result := 0;
  try
    Result := FReservoirPenaltyID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirPenalty.Set_ReservoirPenaltyID (Value: Integer);
const OPNAME = 'TReservoirPenalty.Set_ReservoirPenaltyID';
begin
  try
    FReservoirPenaltyID := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirPenalty.GetBaseValue (const AParamField : WideString;
                                         const AFieldIndex : WideString): WideString;
const OPNAME = 'TReservoirPenalty.GetBaseValue';
var
  lFieldProperty : TAbstractFieldProperty;
  lFormatStr     : string;
begin
  Result := '';
  try
    lFormatStr := '';
    lFieldProperty := FAppModules.FieldProperties.FieldProperty(AParamField);
    if (lFieldProperty <> nil) then
      lFormatStr := lFieldProperty.FormatStringGrid;
    try
      if (AParamField = 'ReservoirPenalty') then
      begin
        if (lFormatStr = '') then
          Result := FloatToStr(FReservoirPenaltyValues[StrToInt(AFieldIndex)])
        else
          Result := Format(lFormatStr, [FReservoirPenaltyValues[StrToInt(AFieldIndex)]]);
      end;
    except
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirPenalty.GetKeyValues (const AParamField: WideString;
                                         const AFieldIndex: WideString): WideString;
const OPNAME = 'TReservoirPenalty.GetKeyValues';
begin
  Result := '';
  try
    Result := GetDefaultKey;
    if (Result = '') then
      Result := 'Identifier=' + AFieldIndex
    else
      Result := Result + ',Identifier=' + AFieldIndex;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TReservoirPenaltyStructureList }

procedure TReservoirPenaltyStructureList.CreateMemberObjects;
const OPNAME = 'TReservoirPenaltyStructureList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPenaltyStructureData := TObjectList.Create;
    FPenaltyZoneData := TObjectList.Create;
    FPenaltyCountsData := TReservoirPenaltyCounts.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirPenaltyStructureList.DestroyMemberObjects;
const OPNAME = 'TReservoirPenaltyStructureList.DestroyMemberObjects';
begin
  try
    FreeAndNil(FPenaltyStructureData);
    FreeAndNil(FPenaltyZoneData);
    FreeAndNil(FPenaltyCountsData);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirPenaltyStructureList.Initialise: boolean;
const OPNAME = 'TReservoirPenaltyStructureList.Initialise';
begin
  Result := False;
  try
    FPenaltyStructureData.Clear;
    FPenaltyZoneData.Clear;
    FPenaltyCountsData.Initialise;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyStructureList.GetAllPenaltyIdentifiers(APenaltyStructureList:TStrings): boolean;
const OPNAME = 'TReservoirPenaltyStructureList.GetAllPenaltyIdentifiers';
var
  LIndex: integer;
begin
  Result := False;
  try
    if not Assigned(APenaltyStructureList) then
        raise Exception.Create('The Penalty Structure list is not assigned.');

    APenaltyStructureList.Clear;
    for LIndex := 0 to FPenaltyStructureData.Count - 1 do
      APenaltyStructureList.AddObject(IntToStr(
                            ReservoirPenaltyByIndex[LIndex].ReservoirPenaltyID),
                            CastReservoirPenaltyStructureByIndex[LIndex]);
   Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirPenaltyStructureList.Get_PenaltyCount: integer;
const OPNAME = 'TReservoirPenaltyStructureList.Get_PenaltyCount';
begin
  Result := 0;
  try
    Result := FPenaltyStructureData.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyStructureList.Get_PenaltyZoneCount: integer;
const OPNAME = 'TReservoirPenaltyStructureList.Get_PenaltyZoneCount';
begin
  Result := 0;
  try
    Result := FPenaltyZoneData.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyStructureList.AddReservoirPenaltyStructure(AReservoirPenaltyStructure: TReservoirPenalty);
const OPNAME = 'TReservoirPenaltyStructureList.AddReservoirPenaltyStructure';
begin
  try
    if Assigned(AReservoirPenaltyStructure) then
      FPenaltyStructureData.Add(AReservoirPenaltyStructure);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyStructureList.AddReservoirPenaltyZone(AReservoirPenaltyZone: TReservoirPenaltyZoneData);
const OPNAME = 'TReservoirPenaltyStructureList.AddReservoirPenaltyZone';
begin
  try
    if Assigned(AReservoirPenaltyZone) then
      FPenaltyZoneData.Add(AReservoirPenaltyZone);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyStructureList.DeleteReservoirPenaltyStructure(APenaltyStructureIndex: integer);
const OPNAME = 'TReservoirPenaltyStructureList.DeleteReservoirPenaltyStructure';
begin
  try
    FPenaltyStructureData.Delete(APenaltyStructureIndex);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyStructureList.Get_ReservoirPenaltyByIdentifier(AReservoirPenaltyStructureIdentifier: integer): IReservoirPenalty;
const OPNAME = 'TReservoirPenaltyStructureList.Get_ReservoirPenaltyByIdentifier';
var LPenaltyStructureIndex: integer;
begin
  Result := nil;
  try
    for LPenaltyStructureIndex := 0 to FPenaltyStructureData.Count - 1 do
     begin
       if (ReservoirPenaltyByIndex[LPenaltyStructureIndex].ReservoirPenaltyID =
         AReservoirPenaltyStructureIdentifier) then
       begin
         Result := ReservoirPenaltyByIndex[LPenaltyStructureIndex];
         break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirPenaltyStructureList.GetReservoirPenaltyStructureByIdentifierCast(AReservoirPenaltyStructureIdentifier: integer): TReservoirPenalty;
const OPNAME = 'TReservoirPenaltyStructureList.GetReservoirPenaltyStructureByIdentifierCast';
var LPenaltyStructureIndex: integer;
begin
  Result := nil;
  try
    for LPenaltyStructureIndex := 0 to FPenaltyStructureData.Count - 1 do
     begin
       if (ReservoirPenaltyByIndex[LPenaltyStructureIndex].ReservoirPenaltyID =
         AReservoirPenaltyStructureIdentifier) then
       begin
         Result := CastReservoirPenaltyStructureByIndex[LPenaltyStructureIndex];
         break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirPenaltyStructureList.Get_ReservoirPenaltyByIndex(AReservoirIndex: integer): IReservoirPenalty;
const OPNAME = 'TReservoirPenaltyStructureList.Get_ReservoirPenaltyByIndex';
begin
  Result := nil;
  try
    if (AReservoirIndex >= 0) and
       (AReservoirIndex < FPenaltyStructureData.Count) then
      Result := TReservoirPenalty(FPenaltyStructureData[AReservoirIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirPenaltyStructureList.GetReservoirPenaltyStructureByIndexCast(AReservoirIndex: integer): TReservoirPenalty;
const OPNAME = 'TReservoirPenaltyStructureList.GetReservoirPenaltyStructureByIndexCast';
begin
  Result := nil;
  try
    if (AReservoirIndex >= 0) and
       (AReservoirIndex < FPenaltyStructureData.Count) then
     Result := TReservoirPenalty(FPenaltyStructureData[AReservoirIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirPenaltyStructureList.Get_ReservoirPenaltyZoneByIndex(AZoneIndex: integer): IReservoirPenaltyZoneData;
const OPNAME = 'TReservoirPenaltyStructureList.Get_ReservoirPenaltyZoneByIndex';
begin
  Result := nil;
  try
    if (AZoneIndex >= 0) and
       (AZoneIndex < FPenaltyZoneData.Count) then
    Result := TReservoirPenaltyZoneData(FPenaltyZoneData[AZoneIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirPenaltyStructureList.GetReservoirPenaltyCountsCast: TReservoirPenaltyCounts;
const OPNAME = 'TReservoirPenaltyStructureList.GetReservoirPenaltyCountsCast';
begin
  Result := nil;
  try
    Result := FPenaltyCountsData;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirPenaltyStructureList.GetReservoirPenaltyZoneByIndexCast(AIndex: integer): TReservoirPenaltyZoneData;
const OPNAME = 'TReservoirPenaltyStructureList.GetReservoirPenaltyZoneByIndexCast';
begin
  Result := nil;
  try
    if (AIndex >= 0) and
       (AIndex < FPenaltyZoneData.Count) then
      Result := TReservoirPenaltyZoneData(FPenaltyZoneData[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirPenaltyStructureList.Get_ReservoirPenaltyCounts: IReservoirPenaltyCounts;
const OPNAME = 'TReservoirPenaltyStructureList.Get_ReservoirPenaltyCounts';
begin
  Result := nil;
  try
    Result := FPenaltyCountsData;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirPenaltyStructureList.UpdateReservoirZoneName(AFieldName,
  ANewValue, AOldValue: string; AContextData: TStrings): boolean;
const OPNAME = 'TReservoirPenaltyStructureList.UpdateReservoirZoneName';
begin
  Result := False;
  try
    if FAppModules.FieldProperties.UpdateFieldValue(AFieldName, ANewValue, AOldValue, AContextData) then
    begin
      TReservoirPenaltyZoneData(Get_ReservoirPenaltyZoneByIndex(-1 + StrToInt(AContextData.Values['Identifier']))).FZoneName :=
        ANewValue;
      Result := True;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirPenaltyStructureList.UpdateZoneRuleCurve(AFieldName,
  ANewValue, AOldValue: string; AContextData: TStrings): boolean;
const OPNAME = 'TReservoirPenaltyStructureList.UpdateZoneRuleCurve';
begin
  Result := False;
  try
    if FAppModules.FieldProperties.UpdateFieldValue(AFieldName, ANewValue, AOldValue, AContextData) then
    begin
      CastReservoirPenaltyCounts.ZoneRuleCurve := StrToInt(ANewValue);
      Result := True;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirPenaltyStructureList.UpdateReservoirPenalty(AFieldName,
  ANewValue, AOldValue: string; AContextData: TStrings): boolean;
const OPNAME = 'TReservoirPenaltyStructureList.UpdateReservoirPenalty';
begin
  Result := False;
  try
    if FAppModules.FieldProperties.UpdateFieldValue(AFieldName, ANewValue, AOldValue, AContextData) then
    begin
      CastReservoirPenaltyStructureByIdentifier[StrToInt(AContextData.Values['PenaltyStruct'])].
        ReservoirPenaltyValueByIndex[StrToInt(AContextData.Values['Identifier'])] :=
          StrToFloat(ANewValue);
      Result := True;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirPenaltyStructureList.UpdateBalancingPolicy(AFieldName,
  ANewValue, AOldValue: string; AContextData: TStrings): boolean;
const OPNAME = 'TReservoirPenaltyStructureList.UpdateBalancingPolicy';
begin
  Result := False;
  try
    if FAppModules.FieldProperties.UpdateFieldValue(AFieldName, ANewValue, AOldValue, AContextData) then
    begin
      TReservoirPenaltyZoneData(Get_ReservoirPenaltyZoneByIndex(-1 + StrToInt(AContextData.Values['Identifier']))).BalancingPolicy := StrToInt(ANewValue);
      Result := True;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirPenaltyStructureList.UpdateBalancingVariable(AFieldName,
  ANewValue, AOldValue: string; AContextData: TStrings): boolean;
const OPNAME = 'TReservoirPenaltyStructureList.UpdateBalancingVariable';
begin
  Result := False;
  try
    if FAppModules.FieldProperties.UpdateFieldValue(AFieldName, ANewValue, AOldValue, AContextData) then
    begin
      TReservoirPenaltyZoneData(Get_ReservoirPenaltyZoneByIndex(-1 + StrToInt(AContextData.Values['Identifier']))).BalancingVariable := StrToInt(ANewValue);
      Result := True;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirPenaltyStructureList.UpdateStrategyIndicator(AFieldName,
  ANewValue, AOldValue: string; AContextData: TStrings): boolean;
const OPNAME = 'TReservoirPenaltyStructureList.UpdateStrategyIndicator';
begin
  Result := False;
  try
    if FAppModules.FieldProperties.UpdateFieldValue(AFieldName, ANewValue, AOldValue, AContextData) then
    begin
      TReservoirPenaltyZoneData(Get_ReservoirPenaltyZoneByIndex(-1 + StrToInt(AContextData.Values['Identifier']))).StrategyIndicator := StrToInt(ANewValue);
      Result := True;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirPenaltyStructureList.AddPenaltyStructure(
         ACopyFromPenaltyID,AInsertBeforePenaltyID: integer): boolean;
const OPNAME = 'TReservoirPenaltyStructureList.AddPenaltyStructure';
var
  LNewPenaltyID,
  LIndex: integer;
  LCurrentPenaltyStructure : TReservoirPenalty;
  LNewPenaltyStructure : TReservoirPenalty;
  LCopyFromPenaltyStructure : TReservoirPenalty;
  LPenaltyStructureDataSQLAgent: TReservoirPenaltyStructureDataSQLAgent;
  lFieldProperty : TAbstractFieldProperty;
  LPenaltyValueArray: array [0..20] of double;
begin
  Result := False;
  try
    lFieldProperty := FAppModules.FieldProperties.FieldProperty('PenaltyStruct');
    if (FPenaltyCountsData.FPenaltyStructureCount >= StrToInt(lFieldProperty.FieldMaximumValue)) then
      raise Exception.Create('No more than ' + lFieldProperty.FieldMaximumValue + ' penalty structures can be added.');

    if (FPenaltyZoneData.Count = 0) then
      raise Exception.Create('Adding a penalty structure cannot be done before the penalty zone has been described.');

    if(ACopyFromPenaltyID >= 0) and (ACopyFromPenaltyID <=  StrToInt(lFieldProperty.FieldMaximumValue)) and
      (AInsertBeforePenaltyID >= 0) and (AInsertBeforePenaltyID <= StrToInt(lFieldProperty.FieldMaximumValue)) then
    begin
      lFieldProperty := FAppModules.FieldProperties.FieldProperty('ReservoirPenalty');
      for LIndex := 1 to lFieldProperty.ArrayHigh do
        LPenaltyValueArray[LIndex] := NullFloat;

      if(ACopyFromPenaltyID > 0) then
      begin
        LCopyFromPenaltyStructure := Self.CastReservoirPenaltyStructureByIdentifier[ACopyFromPenaltyID];
        if not Assigned(LCopyFromPenaltyStructure) then
          raise Exception.Create('The penalty structure('+IntToStr(ACopyFromPenaltyID)+' to copy from does not exist.')
        else
        begin
          for LIndex := 1 to LCopyFromPenaltyStructure.PenaltyValueCount do
            LPenaltyValueArray[LIndex] := LCopyFromPenaltyStructure.ReservoirPenaltyValueByIndex[LIndex];
        end;
      end
      else
      begin
        for LIndex := 1 to FPenaltyZoneData.Count do
          LPenaltyValueArray[LIndex] := 0;
      end;
      LNewPenaltyID := Max(1,AInsertBeforePenaltyID);

      LNewPenaltyStructure := TReservoirPenalty.Create(FAppModules);
      LNewPenaltyStructure.Initialise;
      LNewPenaltyStructure.PopulatePenaltyStructureIdentifier(LNewPenaltyID);

      for LIndex := 1 to FPenaltyZoneData.Count do
      begin
        LNewPenaltyStructure.AddPenaltyValue(LIndex, LPenaltyValueArray[LIndex]);
      end;

      LPenaltyStructureDataSQLAgent := TReservoirPenaltyStructureDataSQLAgent.Create(FAppModules);
      try
        Result := LPenaltyStructureDataSQLAgent.AddPenaltyStructure(Get_PenaltyCount,LNewPenaltyStructure);
        if Result then
        begin
          if (LNewPenaltyID > FPenaltyStructureData.Count) then
            FPenaltyStructureData.Add(LNewPenaltyStructure)
          else
          begin
            FPenaltyStructureData.Insert(LNewPenaltyID-1,LNewPenaltyStructure);
            for LIndex := LNewPenaltyID to FPenaltyStructureData.Count-1 do
            begin
              LCurrentPenaltyStructure := GetReservoirPenaltyStructureByIndexCast(LIndex);
              if Assigned(LCurrentPenaltyStructure) then
               LCurrentPenaltyStructure.ReservoirPenaltyID :=
               LCurrentPenaltyStructure.ReservoirPenaltyID + 1;
            end;
          end;
          FPenaltyCountsData.FPenaltyStructureCount := FPenaltyCountsData.FPenaltyStructureCount +1;
          Result := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.CastReservoirList.PenaltyStructureAdded(LNewPenaltyID)
        end
        else
          LNewPenaltyStructure.Free;
      finally
        LPenaltyStructureDataSQLAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirPenaltyStructureList.DeletePenaltyStructure(APenaltyID: integer): boolean;
const OPNAME = 'TReservoirPenaltyStructureList.DeletePenaltyStructure';
var
  LCount,
  LIndex: integer;
  LMsg: string;
  LCurrentPenaltyStructure : TReservoirPenalty;
  LPenaltyStructureDataSQLAgent: TReservoirPenaltyStructureDataSQLAgent;
begin
  Result := False;
  try
    if(APenaltyID > 0) and (APenaltyID <= 20) then
    begin
      LCount := TYieldModelDataObject(FAppModules.Model.ModelData).
        CastNetworkElementData.CastReservoirList.ReservoirCountPerPenaltyStructure(APenaltyID);
      if LCount > 0 then
      begin
        LMsg := 'Penalty structure '+IntToStr(APenaltyID)+' is currently used by '+
               IntToStr(LCount) +' reservoir/s or node/s. Do you want to continue deleting id? ';
        if (MessageDlg(LMsg, mtConfirmation,[mbYes, mbNo], 0) <> mrYes) then
          Exit;
      end;
      LCurrentPenaltyStructure := CastReservoirPenaltyStructureByIdentifier[APenaltyID];
      if not Assigned(LCurrentPenaltyStructure) then
        raise Exception.Create('The penalty structure('+IntToStr(APenaltyID)+' to be deleted does not exist.')
      else
      begin
        LPenaltyStructureDataSQLAgent := TReservoirPenaltyStructureDataSQLAgent.Create(FAppModules);
        try
          Result := LPenaltyStructureDataSQLAgent.DeletePenaltyStructure(Get_PenaltyCount,LCurrentPenaltyStructure);
          if Result then
          begin
            for LIndex := APenaltyID to FPenaltyStructureData.Count-1 do
            begin
              LCurrentPenaltyStructure := GetReservoirPenaltyStructureByIndexCast(LIndex);
              if Assigned(LCurrentPenaltyStructure) then
              begin
                LCurrentPenaltyStructure.ReservoirPenaltyID :=
                LCurrentPenaltyStructure.ReservoirPenaltyID - 1;
              end;
            end;
            FPenaltyCountsData.FPenaltyStructureCount := FPenaltyCountsData.FPenaltyStructureCount -1;
            FPenaltyStructureData.Delete(APenaltyID-1);
            Result := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.CastReservoirList.PenaltyStructureDeleted(APenaltyID)
          end;
        finally
          LPenaltyStructureDataSQLAgent.Free;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirPenaltyStructureList.CreatePenaltyZone(AZoneLevel: integer): IReservoirPenaltyZoneData;
const OPNAME = 'TReservoirPenaltyStructureList.CreatePenaltyZone';
var
  LPenaltyZoneData: TReservoirPenaltyZoneData;
  LPenaltyStructureData:TReservoirPenalty;
  LIndex: integer;
begin
  Result := nil;
  try
    if(FPenaltyZoneData.Count = 0) then
    begin
      LPenaltyZoneData := TReservoirPenaltyZoneData.Create(FAppModules);
      LPenaltyZoneData.FZoneName          := 'SPILL';
      LPenaltyZoneData.FRecordIdentifier  := 1;
      LPenaltyZoneData.FStrategyIndicator := 3;
      LPenaltyZoneData.FBalancingVariable := 1;
      LPenaltyZoneData.FBalancingPolicy   := 1;
      FPenaltyZoneData.Add(LPenaltyZoneData);

      LPenaltyZoneData := TReservoirPenaltyZoneData.Create(FAppModules);
      LPenaltyZoneData.FZoneName          := 'ZONE1';
      LPenaltyZoneData.FRecordIdentifier  := 2;
      LPenaltyZoneData.FStrategyIndicator := 3;
      LPenaltyZoneData.FBalancingVariable := 1;
      LPenaltyZoneData.FBalancingPolicy   := 1;
      FPenaltyZoneData.Add(LPenaltyZoneData);
      Result := LPenaltyZoneData;

      LPenaltyZoneData := TReservoirPenaltyZoneData.Create(FAppModules);
      LPenaltyZoneData.FZoneName          := 'ZONE2';
      LPenaltyZoneData.FRecordIdentifier  := 3;
      LPenaltyZoneData.FStrategyIndicator := 3;
      LPenaltyZoneData.FBalancingVariable := 1;
      LPenaltyZoneData.FBalancingPolicy   := 1;
      FPenaltyZoneData.Add(LPenaltyZoneData);

      LPenaltyZoneData := TReservoirPenaltyZoneData.Create(FAppModules);
      LPenaltyZoneData.FZoneName          := 'DSL';
      LPenaltyZoneData.FRecordIdentifier  := 4;
      LPenaltyZoneData.FStrategyIndicator := 3;
      LPenaltyZoneData.FBalancingVariable := 1;
      LPenaltyZoneData.FBalancingPolicy   := 1;
      FPenaltyZoneData.Add(LPenaltyZoneData);

      FPenaltyCountsData.FStorageZoneCount      := 4;
      FPenaltyCountsData.FZoneRuleCurve         := 1;
      FPenaltyCountsData.FPenaltyStructureCount := 0;
    end
    else
    begin
      LPenaltyZoneData := TReservoirPenaltyZoneData.Create(FAppModules);
      LPenaltyZoneData.FZoneName          := 'ZONE'+ IntToStr(AZoneLevel);
      LPenaltyZoneData.FRecordIdentifier  := AZoneLevel+1;
      LPenaltyZoneData.FStrategyIndicator := 3;
      LPenaltyZoneData.FBalancingVariable := 1;
      LPenaltyZoneData.FBalancingPolicy   := 1;
      for LIndex := 0 to FPenaltyZoneData.Count-1 do
      begin
        if(TReservoirPenaltyZoneData(FPenaltyZoneData[LIndex]).FRecordIdentifier > AZoneLevel) then
        TReservoirPenaltyZoneData(FPenaltyZoneData[LIndex]).FRecordIdentifier :=
          TReservoirPenaltyZoneData(FPenaltyZoneData[LIndex]).FRecordIdentifier + 1;
      end;
      FPenaltyZoneData.Insert(AZoneLevel,LPenaltyZoneData);
      FPenaltyCountsData.FStorageZoneCount := FPenaltyCountsData.FStorageZoneCount + 1;
      Result := LPenaltyZoneData;
    end;
    for LIndex := 0 to FPenaltyStructureData.Count -1 do
    begin
      LPenaltyStructureData := TReservoirPenalty(FPenaltyStructureData[LIndex]);
      LPenaltyStructureData.InsertPenaltyValue(AZoneLevel+1,0.0);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirPenaltyStructureList.DeletePenaltyZone(AZoneLevel: integer; var ADeleteAction: TDeleteAction): boolean;
const OPNAME = 'TReservoirPenaltyStructureList.DeletePenaltyZone';
      //ConFirmation = 'This is the last non-fixed zone left. Do you want to delete the whole zones structure.';
var
  LPenaltyZoneData      : TReservoirPenaltyZoneData;
  LPenaltyStructureData : TReservoirPenalty;
  LIndex                : integer;
begin
  Result := False;
  try
    ADeleteAction := daContinue;
    LPenaltyZoneData := CastReservoirPenaltyZoneByIndex[AZoneLevel];
    if Assigned(LPenaltyZoneData) then
    begin
      if(FPenaltyZoneData.Count = 3) then
      begin
        if(AZoneLevel = 1) then
        begin
          if(FPenaltyStructureData.Count = 0) then
          begin
            case MessageDlg(FAppModules.Language.GetString('Message.ReservoirPenaltyStructureListDeletePenaltyZone'),mtWarning,mbYesNoCancel,0) of
              mrYes   :
                begin
                  FPenaltyZoneData.Clear;
                  FPenaltyCountsData.FStorageZoneCount := 0;
                  ADeleteAction := daDeleteAll;
                  Result := True;
                  Exit;
                end;
              mrNo:
                begin
                  ADeleteAction := daClearData;
                  Result := True;
                  Exit;
                end;
              mrCancel:
                begin
                  ADeleteAction := daCancel;
                  Result := True;
                  Exit;
                end;
            end;
          end
          else
          begin
            for LIndex := 0 to FPenaltyStructureData.Count -1 do
            begin
              LPenaltyStructureData := TReservoirPenalty(FPenaltyStructureData[LIndex]);
              LPenaltyStructureData.ReservoirPenaltyValueByIndex[AZoneLevel+1] := 0.0;
            end;
            ADeleteAction := daClearData;
            Result := True;
            Exit;
          end;
        end;
      end;

      for LIndex := 0 to FPenaltyZoneData.Count-1 do
      begin
        if(TReservoirPenaltyZoneData(FPenaltyZoneData[LIndex]).FRecordIdentifier > AZoneLevel) then
          TReservoirPenaltyZoneData(FPenaltyZoneData[LIndex]).FRecordIdentifier :=
          TReservoirPenaltyZoneData(FPenaltyZoneData[LIndex]).FRecordIdentifier - 1;
      end;
      FPenaltyZoneData.Remove(LPenaltyZoneData);
      FPenaltyCountsData.FStorageZoneCount := FPenaltyCountsData.FStorageZoneCount - 1;
      for LIndex := 0 to FPenaltyStructureData.Count -1 do
      begin
        LPenaltyStructureData := TReservoirPenalty(FPenaltyStructureData[LIndex]);
        LPenaltyStructureData.RemovePenaltyValue(AZoneLevel+1);
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirPenaltyStructureList._AddRef: Integer;
const OPNAME = 'TReservoirPenaltyStructureList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirPenaltyStructureList._Release: Integer;
const OPNAME = 'TReservoirPenaltyStructureList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirPenaltyStructureList.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TReservoirPenaltyStructureList.Validate';
var
  LIndex: integer;
begin
  Result := True;
  try
    if not FPenaltyCountsData.Validate(AErrors,AContext) then
      Result := False;

    for LIndex := 0 to FPenaltyZoneData.Count -1 do
    begin
      if not ReservoirPenaltyZoneByIndex[LIndex].Validate(AErrors,AContext) then
        Result := False;
    end;

    for LIndex := 0 to FPenaltyStructureData.Count -1 do
    begin
      if not ReservoirPenaltyByIndex[LIndex].Validate(AErrors,AContext) then
        Result := False;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TReservoirPenaltyZoneData }

procedure TReservoirPenaltyZoneData.CreateMemberObjects;
const OPNAME = 'TReservoirPenaltyZoneData.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirPenaltyZoneData.DestroyMemberObjects;
const OPNAME = 'TReservoirPenaltyZoneData.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirPenaltyZoneData.GetKeyValues(const AParamField,AFieldIndex: WideString): WideString;
const OPNAME = 'TReservoirPenaltyZoneData.GetKeyValues';
begin
  Result := '';
  try
    Result := GetDefaultKey;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyZoneData.Get_BalancingPolicy: integer;
const OPNAME = 'TReservoirPenaltyZoneData.Get_BalancingPolicy';
begin
  Result := 0;
  try
    Result := FBalancingPolicy;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyZoneData.Get_BalancingVariable: integer;
const OPNAME = 'TReservoirPenaltyZoneData.Get_BalancingVariable';
begin
  Result := 0;
  try
    Result := FBalancingVariable;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyZoneData.Get_RecordIdentifier: integer;
const OPNAME = 'TReservoirPenaltyZoneData.Get_RecordIdentifier';
begin
  Result := 0;
  try
    Result := FRecordIdentifier;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyZoneData.Get_StrategyIndicator: integer;
const OPNAME = 'TReservoirPenaltyZoneData.Get_StrategyIndicator';
begin
  Result := 0;
  try
    Result := FStrategyIndicator;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyZoneData.Get_ZoneName: WideString;
const OPNAME = 'TReservoirPenaltyZoneData.Get_ZoneName';
begin
  Result := '';
  try
    Result := FZoneName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyZoneData.Initialise: boolean;
const OPNAME = 'TReservoirPenaltyZoneData.Initialise';
begin
  Result := False;
  try
    FZoneName := '';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TReservoirPenaltyZoneData.PopulatePenaltyZone(ADataSet: TAbstractModelDataset;var APenaltyValueArray: array of double): boolean;
const OPNAME = 'TReservoirPenaltyZoneData.PopulatePenaltyZone';
var
  lIndex : integer;
  lFieldName : string;
begin
  Result := False;
  try
    FRecordIdentifier := ADataset.DataSet.FieldByName('RecordIdentifier').AsInteger;
    FZoneName := Trim(ADataset.DataSet.FieldByName('ReservoirZoneName').AsString);
    FStrategyIndicator := ADataset.DataSet.FieldByName('StrategyIndicator').AsInteger;
    FBalancingVariable := ADataset.DataSet.FieldByName('BalancingVariable').AsInteger;
    FBalancingPolicy := ADataset.DataSet.FieldByName('BalancingPolicy').AsInteger;

    for lIndex := 1 to 20 do
    begin
      lFieldName := Format('%s%2.2d',['BalRef',lIndex]);
      if not ADataset.DataSet.FieldByName(lFieldName).IsNull then
        APenaltyValueArray[lIndex - 1] := ADataset.DataSet.FieldByName(lFieldName).AsFloat;
    end;
    
{    if not ADataset.DataSet.FieldByName('BalRef01').IsNull then
      APenaltyValueArray[0] := ADataset.DataSet.FieldByName('BalRef01').AsFloat;
    if not ADataset.DataSet.FieldByName('BalRef02').IsNull then
      APenaltyValueArray[1] := ADataset.DataSet.FieldByName('BalRef02').AsFloat;
    if not ADataset.DataSet.FieldByName('BalRef03').IsNull then
      APenaltyValueArray[2] := ADataset.DataSet.FieldByName('BalRef03').AsFloat;
    if not ADataset.DataSet.FieldByName('BalRef04').IsNull then
      APenaltyValueArray[3] := ADataset.DataSet.FieldByName('BalRef04').AsFloat;
    if not ADataset.DataSet.FieldByName('BalRef05').IsNull then
      APenaltyValueArray[4] := ADataset.DataSet.FieldByName('BalRef05').AsFloat;}
    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

procedure TReservoirPenaltyZoneData.Set_BalancingPolicy(Value: integer);
const OPNAME = 'TReservoirPenaltyZoneData.Set_BalancingPolicy';
var
  LLoadAgent: TReservoirPenaltyStructureDataSQLAgent;
  LContextData: TStringList;
  LFieldName,
  LOldValue,
  LNewValue: string;
begin
  try
    LLoadAgent := TReservoirPenaltyStructureDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadStorageZoneDetailsContextData(LContextData,IntToStr(FRecordIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'BalancingPolicy', IntToStr(Value), IntToStr(FBalancingPolicy), LContextData) then
        begin
          LFieldName := 'BalancingPolicy';
          LOldValue  := IntToStr(FBalancingPolicy);
          LNewValue  := IntToStr(Value);
          FBalancingPolicy := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,LFieldName,LOldValue,LNewValue);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyZoneData.Set_BalancingVariable(Value: integer);
const OPNAME = 'TReservoirPenaltyZoneData.Set_BalancingVariable';
var
  LLoadAgent: TReservoirPenaltyStructureDataSQLAgent;
  LContextData: TStringList;
  LFieldName,
  LOldValue,
  LNewValue: string;
begin
  try
    LLoadAgent := TReservoirPenaltyStructureDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadStorageZoneDetailsContextData(LContextData,IntToStr(FRecordIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'BalancingVariable', IntToStr(Value), IntToStr(FBalancingVariable), LContextData) then
        begin
          LFieldName := 'BalancingVariable';
          LOldValue  := IntToStr(FBalancingVariable);
          LNewValue  := IntToStr(Value);
          FBalancingVariable := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,LFieldName,LOldValue,LNewValue);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyZoneData.Set_StrategyIndicator(Value: integer);
const OPNAME = 'TReservoirPenaltyZoneData.Set_StrategyIndicator';
var
  LLoadAgent: TReservoirPenaltyStructureDataSQLAgent;
  LContextData: TStringList;
  LFieldName,
  LOldValue,
  LNewValue: string;
begin
  try
    LLoadAgent := TReservoirPenaltyStructureDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadStorageZoneDetailsContextData(LContextData,IntToStr(FRecordIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'StrategyIndicator', IntToStr(Value), IntToStr(FStrategyIndicator), LContextData) then
        begin
          LFieldName := 'StrategyIndicator';
          LOldValue  := IntToStr(FStrategyIndicator);
          LNewValue  := IntToStr(Value);
          FStrategyIndicator := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,LFieldName,LOldValue,LNewValue);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyZoneData.Set_ZoneName(const Value: WideString);
const OPNAME = 'TReservoirPenaltyZoneData.Set_ZoneName';
var
  LLoadAgent: TReservoirPenaltyStructureDataSQLAgent;
  LContextData: TStringList;
  LFieldName,
  LOldValue,
  LNewValue: string;
begin
  try
    LLoadAgent := TReservoirPenaltyStructureDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadStorageZoneDetailsContextData(LContextData,IntToStr(FRecordIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'ReservoirZoneName', Value, FZoneName, LContextData) then
        begin
          LFieldName := 'ReservoirZoneName';
          LOldValue  := FZoneName;
          LNewValue  := Value;
          FZoneName  := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,LFieldName,LOldValue,LNewValue);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyZoneData.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TReservoirPenaltyZoneData.Validate';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirPenaltyZoneData._AddRef: Integer;
const OPNAME = 'TReservoirPenaltyZoneData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirPenaltyZoneData._Release: Integer;
const OPNAME = 'TReservoirPenaltyZoneData._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TReservoirPenaltyCounts }

procedure TReservoirPenaltyCounts.CreateMemberObjects;
const OPNAME = 'TReservoirPenaltyCounts.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FStorageZoneCount      := 0;
    FZoneRuleCurve         := 0;
    FPenaltyStructureCount := 0;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirPenaltyCounts.DestroyMemberObjects;
const OPNAME = 'TReservoirPenaltyCounts.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirPenaltyCounts.GetKeyValues(const AParamField,AFieldIndex: WideString): WideString;
const OPNAME = 'TReservoirPenaltyCounts.GetKeyValues';
begin
  Result := '';
  try
    Result := GetDefaultKey;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyCounts.Get_PenaltyStructureCount: integer;
const OPNAME = 'TReservoirPenaltyCounts.Get_PenaltyStructureCount';
begin
  Result := 0;
  try
    Result := FPenaltyStructureCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyCounts.Get_StorageZoneCount: integer;
const OPNAME = 'TReservoirPenaltyCounts.Get_StorageZoneCount';
begin
  Result := 0;
  try
    Result := FStorageZoneCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyCounts.Get_ZoneRuleCurve: integer;
const OPNAME = 'TReservoirPenaltyCounts.Get_ZoneRuleCurve';
begin
  Result := 0;
  try
    Result := FZoneRuleCurve;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyCounts.Initialise: boolean;
const OPNAME = 'TReservoirPenaltyCounts.Initialise';
begin
  Result := False;
  try
    FStorageZoneCount      := 0;
    FZoneRuleCurve         := 0;
    FPenaltyStructureCount := 0;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyCounts.PopulatePenaltyCounts(ADataSet: TAbstractModelDataset): boolean;
const OPNAME = 'TReservoirPenaltyCounts.PopulatePenaltyCounts';
begin
  Result := False;
  try
    FStorageZoneCount := ADataset.DataSet.FieldByName('StorageZoneCount').AsInteger;
    FZoneRuleCurve := ADataset.DataSet.FieldByName('ZoneLowerBoundary').AsInteger;
    FPenaltyStructureCount := ADataset.DataSet.FieldByName('PenaltyStructureCount').AsInteger;
    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

procedure TReservoirPenaltyCounts.Set_ZoneRuleCurve(Value: integer);
const OPNAME = 'TReservoirPenaltyCounts.Set_ZoneRuleCurve';
var
  LLoadAgent: TReservoirPenaltyStructureDataSQLAgent;
  LContextData: TStringList;
  LFieldName,
  LOldValue,
  LNewValue: string;
begin
  try
    LLoadAgent := TReservoirPenaltyStructureDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadStorageZonesContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'ZoneRuleCurve', IntToStr(Value), IntToStr(FZoneRuleCurve), LContextData) then
        begin
          LFieldName := 'ZoneRuleCurve';
          LOldValue  := IntToStr(FZoneRuleCurve);
          LNewValue  := IntToStr(Value);
          FZoneRuleCurve := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,LFieldName,LOldValue,LNewValue);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyCounts.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TReservoirPenaltyCounts.Validate';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirPenaltyCounts._AddRef: Integer;
const OPNAME = 'TReservoirPenaltyCounts._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirPenaltyCounts._Release: Integer;
const OPNAME = 'TReservoirPenaltyCounts._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.

