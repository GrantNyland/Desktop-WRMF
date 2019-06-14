{******************************************************************************}
{*  UNIT      : Contains the class TMinimumFlowConstraint.                    *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2004/03/29                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UMinimumFlowConstraints;

interface

uses
  Classes,
  Contnrs,
  UAbstractObject,
  VoaimsCom_TLB;

type

{******************************************************************************}
{* Minimum Flow Constraints                                                   *}
{******************************************************************************}

  TMinimumFlowConstraint = class(TAbstractAppObject, IMinimumFlowConstraint)
  protected
    FFeatureID          : integer;
    FFeatureName        : string;
    FChannelNr          : integer;
    FFeatureType        : integer;
    FFeatureSubType     : integer;
    FMinimumFlowDemands : TMinMonthlyDoublesArray;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function ValidateMinFlowFeatureName (AErrorMessages : TStrings) : WordBool;
    function ValidateMinFlowDemand (AErrorMessages : TStrings) : WordBool;
  public
    function Initialise : boolean; override;
    function Populate (AFeatureID           : integer;
                       AFeatureName         : WideString;
                       AChannelNr           : integer;
                       AFeatureType         : integer;
                       AFeatureSubType      : integer;
                       AMonthlyMinimumFlows : TMinMonthlyDoublesArray): WordBool;
    procedure Assign(AChannelNumber : integer;AMinimumFlowConstraint:TMinimumFlowConstraint);
    function Get_FeatureID : integer; safecall;
    function Get_FeatureName : WideString; safecall;
    procedure Set_FeatureName (const AName : WideString); safecall;
    function Get_Channel : IGeneralFlowChannel; safecall;
    procedure Set_Channel (const AChannel : IGeneralFlowChannel); safecall;
    function Get_FeatureType : integer; safecall;
    procedure Set_FeatureType (AType : integer); safecall;
    function Get_FeatureSubType : integer; safecall;
    procedure Set_FeatureSubType (ASubType : integer); safecall;
    function Get_MinimumFlowDemandByMonth (AMonth : integer) : double; safecall;
    procedure Set_MinimumFlowDemandByMonth (AMonth : integer;AValue : double); safecall;
    function Validate (var AErrors : WideString; const AContext    : WideString='') : WordBool; safecall;
    function GetBaseValue(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function GetKeyValues (const AParamField : WideString; const AFieldIndex: WideString) : WideString; safecall;

    property FeatureID      : integer read Get_FeatureID;
    property FeatureName    : WideString read Get_FeatureName write Set_FeatureName;
    property Channel        : IGeneralFlowChannel read Get_Channel write Set_Channel;
    property FeatureType    : integer read Get_FeatureType write Set_FeatureType;
    property FeatureSubType : integer read Get_FeatureSubType write Set_FeatureSubType;
    property MinimumFlowDemandByMonth[AMonth : integer] : double
      read Get_MinimumFlowDemandByMonth write Set_MinimumFlowDemandByMonth;
  end;

  TMinimumFlowConstraintList = class(TAbstractAppObject, IMinimumFlowConstraintList)
  protected
    FMinimumFlowConstraintList : TList;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function AddMinimumFlowConstraint (AFeature : TMinimumFlowConstraint): boolean;
  public
    function Initialise : boolean; override;

    function NewMinimumFlowConstraint : TMinimumFlowConstraint;
    function CreateNewMinimumFlowConstraint : TMinimumFlowConstraint;
    function DeleteMinimumFlowConstraintWithID(AFeatureID : integer) : WordBool;
    function DeleteMinimumFlowConstraintWithIndex(AIndex : integer) : WordBool;
    function CastMinimumFlowConstraintByIndex (AIndex : integer): TMinimumFlowConstraint;
    function CastMinimumFlowConstraintByID(AFeatureID : integer): TMinimumFlowConstraint;
    function CastMinimumFlowConstraintByChannelNr(AChannelNr : integer): TMinimumFlowConstraint;
    function CopyMinimumFlowConstraint(ANewChannelNumber, AOldChannelNumber: integer): IMinimumFlowConstraint; safecall;
    function CreateMinimumFlowConstraint : IMinimumFlowConstraint; safecall;
    function RemoveMinimumFlowConstraintWithID (AFeatureID : integer) : WordBool; safecall;
    function Get_MinimumFlowConstraintByID(AFeatureID : integer) : IMinimumFlowConstraint; safecall;
    function Get_MinimumFlowConstraintByIndex(AIndex: integer): IMinimumFlowConstraint; safecall;
    function Get_MinimumFlowConstraintCount : integer; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;

    property MinimumFlowConstraintByIndex[AIndex : integer]: IMinimumFlowConstraint
      read Get_MinimumFlowConstraintByIndex;
    property MinimumFlowConstraintByID[AFeatureID: integer]: IMinimumFlowConstraint
      read Get_MinimumFlowConstraintByID;
    property MinimumFlowConstraintCount: integer read Get_MinimumFlowConstraintCount;
  end;

implementation

uses
  SysUtils,
  Math,
  UConstants,
  UChannelDataSQLAgent,
  UYieldModelDataObject,
  UErrorHandlingOperations;

{******************************************************************************}
{ TMinimumFlowConstraint                                                       }
{******************************************************************************}

function TMinimumFlowConstraint._AddRef: Integer;
const OPNAME = 'TMinimumFlowConstraint._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinimumFlowConstraint._Release: Integer;
const OPNAME = 'TMinimumFlowConstraint._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMinimumFlowConstraint.CreateMemberObjects;
const OPNAME = 'TMinimumFlowConstraint.CreateMemberObjects';
var
  LMinimumFlowField: TAbstractFieldProperty;
begin
  inherited;
  try
    LMinimumFlowField := FAppModules.FieldProperties.FieldProperty('MinFlowDemand');
    if not Assigned(LMinimumFlowField) then
      raise Exception.Create('Field(MinFlowDemand) not found in field properties');
    SetLength(FMinimumFlowDemands,LMinimumFlowField.ArrayLength);

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMinimumFlowConstraint.DestroyMemberObjects;
const OPNAME = 'TMinimumFlowConstraint.DestroyMemberObjects';
begin
  try
    Finalize(FMinimumFlowDemands);
  except on E: Exception do HandleError(E, OPNAME); end;
  inherited;
end;

function TMinimumFlowConstraint.Initialise: boolean;
const OPNAME = 'TMinimumFlowConstraint.Initialise';
var
  nIndex : integer;
  LMinimumFlowDemands: TAbstractFieldProperty;
begin
  Result := inherited Initialise;
  try
    FChannelNr            := 0;
    FFeatureID            := -1;
    FFeatureName          := '';
    FFeatureType          := -1;
    FFeatureSubType       := -1;
    LMinimumFlowDemands := FAppModules.FieldProperties.FieldProperty('MinFlowDemand');
    for nIndex := LMinimumFlowDemands.ArrayLow to LMinimumFlowDemands.ArrayHigh do
      FMinimumFlowDemands[nIndex] := NullFloat;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinimumFlowConstraint.Get_FeatureID : integer;
const OPNAME = 'TMinimumFlowConstraint.Get_FeatureID';
begin
  Result := 0;
  try
    Result := FFeatureID;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TMinimumFlowConstraint.Get_FeatureName : WideString;
const OPNAME = 'TMinimumFlowConstraint.Get_FeatureName';
begin
  Result := '';
  try
    Result := FFeatureName;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TMinimumFlowConstraint.Get_Channel : IGeneralFlowChannel;
const OPNAME = 'TMinimumFlowConstraint.Get_Channel';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.
              ChannelByChannelNumber[FChannelNr];
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TMinimumFlowConstraint.Get_FeatureType : integer;
const OPNAME = 'TMinimumFlowConstraint.Get_FeatureType';
begin
  Result := 0;
  try
    Result := FFeatureType;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TMinimumFlowConstraint.Get_FeatureSubType : integer;
const OPNAME = 'TMinimumFlowConstraint.Get_FeatureSubType';
begin
  Result := 0;
  try
    Result := FFeatureSubType;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TMinimumFlowConstraint.Get_MinimumFlowDemandByMonth (AMonth : integer) : double;
const OPNAME = 'TMinimumFlowConstraint.Get_MinimumFlowDemandByMonth';
begin
  Result := 0.0;
  try
    Result := StrToFloat(FAppModules.Changes.GetParameterValue
                                              ('MinFlowDemand',
                                              GetKeyValues('MinFlowDemand', IntToStr(AMonth)),
                                              FloatToStr(FMinimumFlowDemands[AMonth]),
                                              IntToStr(AMonth)));
//    Result := FMinimumFlowDemands[AMonth];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMinimumFlowConstraint.Set_FeatureName (const AName : WideString);
const OPNAME = 'TMinimumFlowConstraint.Set_FeatureName';
var
  LLoadAgent   : TChannelDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'MinFlowChannelName', AName, FFeatureName, LContextData) then
        begin
          LOldValue    := FFeatureName;
          FFeatureName := AName;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MinFlowChannelName',LOldValue,AName);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMinimumFlowConstraint.Set_Channel (const AChannel : IGeneralFlowChannel);
const OPNAME = 'TMinimumFlowConstraint.Set_Channel';
var
  LLoadAgent   : TChannelDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
  LNewValue    : string;
begin
  try
    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LOldValue := IntToStr(FChannelNr);
        if (AChannel <> nil) then
          LNewValue := IntToStr(AChannel.ChannelNumber)
        else
          LNewValue := '0';
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'MinFlowFeatureChannelNumber', LNewValue, LOldValue, LContextData) then
        begin
          FChannelNr := AChannel.ChannelNumber;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MinFlowChannelNumber',LOldValue,LNewValue);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMinimumFlowConstraint.Set_FeatureType (AType : integer);
const OPNAME = 'TMinimumFlowConstraint.Set_FeatureType';
var
  LLoadAgent   : TChannelDataSQLAgent;
  LContextData : TStringList;
  LOldValue: integer;
begin
  try
    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
      {RHS comeback = no DB field to store type of feature}
{
        LLoadAgent.LoadChannelPenaltyContextData(LContextData,
          IntToStr(FPenaltyStructType), IntToStr(FChannelPenaltyArcNumber));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'Penalty', FloatToStr(APenaltyValue), FloatToStr(FPenaltyValue), LContextData) then
        begin
}         LOldValue := FFeatureType;
          FFeatureType := AType;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'FeatureType',IntToStr(LOldValue),IntToStr(AType));
//        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMinimumFlowConstraint.Set_FeatureSubType (ASubType : integer);
const OPNAME = 'TMinimumFlowConstraint.Set_FeatureSubType';
var
  LLoadAgent   : TChannelDataSQLAgent;
  LContextData : TStringList;
  LOldValue: integer;
begin
  try
    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
      {RHS comeback = no DB field to store subtype of feature}
{
        LLoadAgent.LoadChannelPenaltyContextData(LContextData,
          IntToStr(FPenaltyStructType), IntToStr(FChannelPenaltyArcNumber));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'Penalty', FloatToStr(APenaltyValue), FloatToStr(FPenaltyValue), LContextData) then
        begin
}         LOldValue := FFeatureSubType;
          FFeatureSubType := ASubType;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'FeatureSubType',IntToStr(LOldValue),IntToStr(ASubType));
//        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMinimumFlowConstraint.Set_MinimumFlowDemandByMonth (AMonth : integer;
                                                              AValue : double);
const OPNAME = 'TMinimumFlowConstraint.Set_MinimumFlowDemandByMonth';
var
  LLoadAgent   : TChannelDataSQLAgent;
  LContextData : TStringList;
  LPrevValue   : double;
begin
  try
    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureIDFieldNameID(LContextData, IntToStr(FFeatureID), IntToStr(AMonth));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'MinFlowDemand', FloatToStr(AValue), FloatToStr(FMinimumFlowDemands[AMonth]), LContextData) then
        begin
          LPrevValue := FMinimumFlowDemands[AMonth];
          FMinimumFlowDemands[AMonth] := AValue;
          FAppModules.Model.StudyDataHasChanged
            (sdccEdit,'MinFlowDemand',FloatToStr(LPrevValue),FloatToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinimumFlowConstraint.Populate (AFeatureID           : integer;
                                          AFeatureName         : WideString;
                                          AChannelNr           : integer;
                                          AFeatureType         : integer;
                                          AFeatureSubType      : integer;
                                          AMonthlyMinimumFlows : TMinMonthlyDoublesArray): WordBool;
const OPNAME = 'TMinimumFlowConstraint.Populate';
var
  lMonth : integer;
  LMinimumFlowDemands: TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    FChannelNr      := AChannelNr;
    FFeatureID      := AFeatureID;
    FFeatureName    := AFeatureName;
    FFeatureType    := AFeatureType;
    FFeatureSubType := AFeatureSubType;
    LMinimumFlowDemands := FAppModules.FieldProperties.FieldProperty('MinFlowDemand');
    for lMonth := LMinimumFlowDemands.ArrayLow to LMinimumFlowDemands.ArrayHigh do
      FMinimumFlowDemands[lMonth] := AMonthlyMinimumFlows[lMonth];
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{******************************************************************************}
{ TMinimumFlowConstraintList                                                        }
{******************************************************************************}

function TMinimumFlowConstraintList._AddRef: Integer;
const OPNAME = 'TMinimumFlowConstraintList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinimumFlowConstraintList._Release: Integer;
const OPNAME = 'TMinimumFlowConstraintList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMinimumFlowConstraintList.CreateMemberObjects;
const OPNAME = 'TMinimumFlowConstraintList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FMinimumFlowConstraintList := TList.Create;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMinimumFlowConstraintList.DestroyMemberObjects;
const OPNAME = 'TMinimumFlowConstraintList.DestroyMemberObjects';
begin
  try
    while (FMinimumFlowConstraintList.Count > 0) do
      DeleteMinimumFlowConstraintWithIndex(0);
    FreeAndNil(FMinimumFlowConstraintList);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinimumFlowConstraintList.Initialise: boolean;
const OPNAME = 'TMinimumFlowConstraintList.Initialise';
begin
  Result := inherited Initialise;
  try
    FMinimumFlowConstraintList.Clear;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinimumFlowConstraintList.AddMinimumFlowConstraint (AFeature : TMinimumFlowConstraint): boolean;
const OPNAME = 'TMinimumFlowConstraintList.AddMinimumFlowConstraint';
begin
  Result := False;
  try
    if (AFeature <> nil) then
    begin
      FMinimumFlowConstraintList.Add(AFeature);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinimumFlowConstraintList.NewMinimumFlowConstraint : TMinimumFlowConstraint;
const OPNAME = 'TMinimumFlowConstraintList.NewMinimumFlowConstraint';
var
  lFeature : TMinimumFlowConstraint;
begin
  Result := nil;
  try
    lFeature := TMinimumFlowConstraint.Create(FAppModules);
    AddMinimumFlowConstraint(lFeature);
    Result := lFeature;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinimumFlowConstraintList.CreateNewMinimumFlowConstraint : TMinimumFlowConstraint;
const OPNAME = 'TMinimumFlowConstraintList.CreateNewMinimumFlowConstraint';
var
  LFeatureID       : integer;
  LLoadAgent       : TChannelDataSQLAgent;
  LFeature         : TMinimumFlowConstraint;
  LMonthlyFlows    : TMinMonthlyDoublesArray;
  LMonthFlowsField : TAbstractFieldProperty;
  lIndex           : integer;
begin
  Result := nil;
  try
    LMonthFlowsField := FAppModules.FieldProperties.FieldProperty('MinFlowDemand');
    if not Assigned(LMonthFlowsField) then
      raise Exception.Create('Field (Month) not found in field properties');
    SetLength(LMonthlyFlows,LMonthFlowsField.ArrayLength);

    for LIndex := LMonthFlowsField.ArrayLow to LMonthFlowsField.ArrayHigh do
      LMonthlyFlows[LIndex] := 0;

    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
    try
      LFeatureID := 0;
      if (LLoadAgent.InsertMinimumFlowConstraint(LFeatureID)) then
      begin
        LFeature := NewMinimumFlowConstraint;
        LFeature.Initialise;
        LFeature.Populate
          (LFeatureID,
           UpperCase(FAppModules.Language.GetString('NetworkFeatures.MinimumFlowFeature')) + ' ' + IntToStr(LFeatureID),
           0, 1, 0, LMonthlyFlows);
        Result := LFeature;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinimumFlowConstraintList.CreateMinimumFlowConstraint : IMinimumFlowConstraint;
const OPNAME = 'TMinimumFlowConstraintList.CreateMinimumFlowConstraint';
var
  LFeature : IMinimumFlowConstraint;
begin
  Result := nil;
  try
    lFeature := CreateNewMinimumFlowConstraint;
    Result := lFeature;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinimumFlowConstraintList.RemoveMinimumFlowConstraintWithID (AFeatureID : integer) : WordBool;
const OPNAME = 'TMinimumFlowConstraintList.RemoveMinimumFlowConstraintWithID';
var
  lLoadAgent : TChannelDataSQLAgent;
begin
  Result := FALSE;
  try
    if (AFeatureID > 0) then
    begin
      LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
      try
        if LLoadAgent.DeleteMinimumFlowConstraint(AFeatureID) then
        begin
          DeleteMinimumFlowConstraintWithID(AFeatureID);
          Result := TRUE;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinimumFlowConstraintList.DeleteMinimumFlowConstraintWithID(AFeatureID : integer) : WordBool;
const OPNAME = 'TMinimumFlowConstraintList.DeleteMinimumFlowConstraintWithID';
var
  lFeature : TMinimumFlowConstraint;
  lIndex   : integer;
begin
  Result := FALSE;
  try
    lFeature := CastMinimumFlowConstraintByID(AFeatureID);
    if (lFeature <> nil) then
    begin
      lIndex := FMinimumFlowConstraintList.IndexOf(lFeature);
      Result := DeleteMinimumFlowConstraintWithIndex(lIndex);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinimumFlowConstraintList.DeleteMinimumFlowConstraintWithIndex(AIndex : integer) : WordBool;
const OPNAME = 'TMinimumFlowConstraintList.DeleteMinimumFlowConstraintWithIndex';
var
  lFeature : TMinimumFlowConstraint;
begin
  Result := FALSE;
  try
    if (AIndex >= 0) then
    begin
      lFeature := TMinimumFlowConstraint(FMinimumFlowConstraintList.Items[AIndex]);
      FMinimumFlowConstraintList.Delete(AIndex);
      FreeAndNil(lFeature);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinimumFlowConstraintList.Get_MinimumFlowConstraintByIndex(AIndex: integer): IMinimumFlowConstraint;
const OPNAME = 'TMinimumFlowConstraintList.Get_MinimumFlowConstraintByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FMinimumFlowConstraintList.Count) then
      Result := TMinimumFlowConstraint(FMinimumFlowConstraintList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinimumFlowConstraintList.Get_MinimumFlowConstraintByID(AFeatureID : integer): IMinimumFlowConstraint;
const OPNAME = 'TMinimumFlowConstraintList.Get_MinimumFlowConstraintByID';
var
 lIndex   : integer;
 lFeature : TMinimumFlowConstraint;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FMinimumFlowConstraintList.Count)) do
    begin
      lFeature := TMinimumFlowConstraint(FMinimumFlowConstraintList.Items[lIndex]);
      if (lFeature.FeatureID = AFeatureID) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinimumFlowConstraintList.CastMinimumFlowConstraintByIndex(AIndex: integer): TMinimumFlowConstraint;
const OPNAME = 'TMinimumFlowConstraintList.CastMinimumFlowConstraintByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FMinimumFlowConstraintList.Count) then
      Result := TMinimumFlowConstraint(FMinimumFlowConstraintList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinimumFlowConstraintList.CastMinimumFlowConstraintByID(AFeatureID : integer): TMinimumFlowConstraint;
const OPNAME = 'TMinimumFlowConstraintList.CastMinimumFlowConstraintByID';
var
 lIndex   : integer;
 lFeature : TMinimumFlowConstraint;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FMinimumFlowConstraintList.Count)) do
    begin
      lFeature := TMinimumFlowConstraint(FMinimumFlowConstraintList.Items[lIndex]);
      if (lFeature.FeatureID = AFeatureID) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinimumFlowConstraintList.CastMinimumFlowConstraintByChannelNr(AChannelNr : integer): TMinimumFlowConstraint;
const OPNAME = 'TMinimumFlowConstraintList.CastMinimumFlowConstraintByChannelNr';
var
 lIndex   : integer;
 lFeature : TMinimumFlowConstraint;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FMinimumFlowConstraintList.Count)) do
    begin
      lFeature := TMinimumFlowConstraint(FMinimumFlowConstraintList.Items[lIndex]);
      if (lFeature.FChannelNr = AChannelNr) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TMinimumFlowConstraintList.Get_MinimumFlowConstraintCount: integer;
const OPNAME = 'TMinimumFlowConstraintList.Get_MinimumFlowConstraintCount';
begin
  Result := 0;
  try
    Result := FMinimumFlowConstraintList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinimumFlowConstraint.ValidateMinFlowFeatureName(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TMinimumFlowConstraint.ValidateMinFlowFeatureName';
var
  lIndex       : integer;
  lFeatureList : TMinimumFlowConstraintList;
  lFeature     : TMinimumFlowConstraint;
  lUnique      : Boolean;
  lMessage     : string;
begin
  Result := True;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('MinFlowChannelName', FFeatureName, lMessage)) then
      AErrorMessages.Add('WARNING:' +Channel.ChannelName+ ':'+lMessage)
    else
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        CastNetworkFeaturesData.CastMinimumFlowConstraintList;
      lUnique := TRUE;
      lIndex  := 0;
      while (lUnique AND (lIndex < lFeatureList.MinimumFlowConstraintCount)) do
      begin
        lFeature := lFeatureList.CastMinimumFlowConstraintByIndex(lIndex);
        if ((FFeatureID <> lFeature.FeatureID) AND
            (UpperCase(Trim(lFeature.FeatureName)) = UpperCase(Trim(FFeatureName)))) then
        begin
          lMessage := FAppModules.Language.GetString('ContextValidation.DuplicateMinFlowFeatureName');
          AErrorMessages.Add('WARNING:' +Format(lMessage, [Channel.ChannelName]));
          lUnique := FALSE;
        end
        else
          lIndex := lIndex + 1;
      end;
      //Result := lUnique;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinimumFlowConstraint.ValidateMinFlowDemand( AErrorMessages: TStrings): WordBool;
const OPNAME = 'TMinimumFlowConstraint.ValidateMinFlowDemand';
var
  lIndex            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lResult           : Boolean;
  LMinFlowDemand    : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := True;
    LMinFlowDemand := FAppModules.FieldProperties.FieldProperty('MinFlowDemand');
    if (LMinFlowDemand <> nil) then
    begin
      lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
      for lIndex := LMinFlowDemand.ArrayLow to LMinFlowDemand.ArrayHigh do
      begin
        lMessage := '';
        if (Not FAppModules.FieldProperties.ValidateFieldProperty
           ('MinFlowDemand', FloatToStr(FMinimumFlowDemands[lIndex]),
           lMessage,lIndex)) then
        begin
          lResult := FALSE;
          AErrorMessages.Add('ERROR:' +lMessage);
          if (lStopOnFirstError) then
            Break;
        end;
      end;
      Result := lResult;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinimumFlowConstraint.Validate (var AErrors : WideString; const AContext    : WideString='') : WordBool;
const OPNAME = 'TMinimumFlowConstraint.Validate';
var
  lStopOnFirstError : Boolean;
  lErrorList        : TStringList;
begin
  Result := FALSE;
  try
    lErrorList := TStringList.Create;
    try
      if (AContext = 'MinFlowChannelName') then
        Result := ValidateMinFlowFeatureName(lErrorList)
      else
      if (AContext = 'MinFlowDemand') then
        Result := ValidateMinFlowDemand(lErrorList)
      else
      begin
        Result := TRUE;
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        if (NOT ValidateMinFlowFeatureName(lErrorList)) then
          Result := FALSE;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateMinFlowDemand(lErrorList)) then
            Result := FALSE;
        end;
      end;
      AErrors := AErrors + lErrorList.Text;
    finally
      FreeAndNil(lErrorList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinimumFlowConstraintList.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TMinimumFlowConstraintList.Validate';
var
  LIndex            : integer;
  lStopOnFirstError : boolean;
begin
  Result := FALSE;
  try
    Result := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    for LIndex := 0 to FMinimumFlowConstraintList.Count - 1 do
    begin
      if (NOT MinimumFlowConstraintByIndex[LIndex].Validate(AErrors,AContext)) then
        Result := FALSE;
      if ((NOT Result) AND lStopOnFirstError) then
        Break;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinimumFlowConstraint.GetBaseValue (const AParamField: WideString;
                                              const AFieldIndex: WideString): WideString;
const OPNAME = 'TMinimumFlowConstraint.GetBaseValue';
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

    if (AParamField = 'MinFlowDemand') then
    begin
      if (lFormatStr = '') then
        Result := FloatToStr(FMinimumFlowDemands[StrToInt(AFieldIndex)])
      else
        Result := Format(lFormatStr, [FMinimumFlowDemands[StrToInt(AFieldIndex)]]);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinimumFlowConstraint.GetKeyValues (const AParamField : WideString;
                                              const AFieldIndex : WideString) : WideString;
const OPNAME = 'TMinimumFlowConstraint.GetKeyValues';
begin
  Result := '';
  try
    Result := GetDefaultKey;
    if (Result = '') then
      Result := 'Identifier=' + IntToStr(FFeatureID)
    else
      Result := Result + ',Identifier=' + IntToStr(FFeatureID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinimumFlowConstraintList.CopyMinimumFlowConstraint(ANewChannelNumber, AOldChannelNumber: integer): IMinimumFlowConstraint;
const OPNAME = 'TMinimumFlowConstraintList.CopyMinimumFlowConstraint';
var
  LMinimumFlowConstraint : TMinimumFlowConstraint;
  LMinimumFlowConstraintCopy : TMinimumFlowConstraint;
begin
  Result := nil;
  try
    LMinimumFlowConstraint := CastMinimumFlowConstraintByChannelNr(AOldChannelNumber);
    if (LMinimumFlowConstraint <> nil) then
    begin
      LMinimumFlowConstraintCopy := CreateNewMinimumFlowConstraint;
      if LMinimumFlowConstraintCopy <> nil then
      begin
        LMinimumFlowConstraintCopy.Assign(ANewChannelNumber,LMinimumFlowConstraint);
        Result := LMinimumFlowConstraintCopy;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinimumFlowConstraint.Assign(AChannelNumber: integer;AMinimumFlowConstraint: TMinimumFlowConstraint);
const OPNAME = 'TMinimumFlowConstraint.Assign';
var
  LChannel : IGeneralFlowChannel;
  LMonth          : integer;
  LMonthlyFactors : TAbstractFieldProperty;
begin
  try
    if (AMinimumFlowConstraint <> nil) and (AChannelNumber > 0) then
    begin
      LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[AChannelNumber];
      if LChannel <> nil then
        Channel := LChannel;
      FeatureName := 'Copy of '+AMinimumFlowConstraint.FeatureName;
      FeatureType := AMinimumFlowConstraint.FeatureType;
      FeatureSubType := AMinimumFlowConstraint.FeatureSubType;
      LMonthlyFactors := FAppModules.FieldProperties.FieldProperty('MinFlowDemand');
      for LMonth := LMonthlyFactors.ArrayLow to LMonthlyFactors.ArrayHigh do
        MinimumFlowDemandByMonth[LMonth] := AMinimumFlowConstraint.MinimumFlowDemandByMonth[LMonth];
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
