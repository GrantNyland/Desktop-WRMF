{******************************************************************************}
{*  UNIT      : Contains the class TPumpingFeature.                           *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2004/03/29                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UPumpingFeatures;

interface

uses
  Classes,
  Contnrs,
  UAbstractObject,
  VoaimsCom_TLB;

type

{******************************************************************************}
{* Pumping Features                                                           *}
{******************************************************************************}

  TPumpingFeature = class(TAbstractAppObject, IPumpingFeature)
  protected
    FFeatureID                : integer;
    FFeatureName              : string;
    FChannelNr                : integer;
    FFeatureType              : integer;
    FFeatureSubType           : integer;
    FPumpingHead              : double;
    FPumpingEfficiency        : double;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function ValidateFeatureName(AErrorMessages: TStrings): WordBool;
    function ValidatePumpingHead(AErrorMessages: TStrings): WordBool;
    function ValidatePumpingEfficiency(AErrorMessages: TStrings): WordBool;

  public
    procedure Assign(AChannelNumber : integer; APumpingFeature : TPumpingFeature);
    function Initialise : boolean; override;
    function Populate (AFeatureID           : integer;
                       AFeatureName         : WideString;
                       AChannelNr           : integer;
                       AFeatureType         : integer;
                       AFeatureSubType      : integer;
                       APumpingHead         : double;
                       APumpingEfficiency   : double): WordBool;
    function Get_FeatureID : integer; safecall;
    function Get_FeatureName : WideString; safecall;
    procedure Set_FeatureName (const AName : WideString); safecall;
    function Get_Channel : IGeneralFlowChannel; safecall;
    procedure Set_Channel (const AChannel : IGeneralFlowChannel); safecall;
    function Get_FeatureType : integer; safecall;
    procedure Set_FeatureType (AType : integer); safecall;
    function Get_FeatureSubType : integer; safecall;
    procedure Set_FeatureSubType (ASubType : integer); safecall;
    function Get_PumpingHead: double; safecall;
    procedure Set_PumpingHead(APumpingHead: double); safecall;
    function Get_PumpingEfficiency: double; safecall;
    procedure Set_PumpingEfficiency(APumpingEfficiency: double); safecall;
    function Validate (var AErrors : WideString; const AContext    : WideString='') : WordBool; safecall;
    function GetBaseValue(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function GetKeyValues (const AParamField : WideString; const AFieldIndex : WideString) : WideString; safecall;
    property FeatureID      : integer read Get_FeatureID;
    property FeatureName    : WideString read Get_FeatureName write Set_FeatureName;
    property Channel        : IGeneralFlowChannel read Get_Channel write Set_Channel;
    property FeatureType    : integer read Get_FeatureType write Set_FeatureType;
    property FeatureSubType : integer read Get_FeatureSubType write Set_FeatureSubType;
    property PumpingHead    : double read Get_PumpingHead write Set_PumpingHead;
    property PumpingEfficiency : double read Get_PumpingEfficiency write Set_PumpingEfficiency;
  end;

  TPumpingFeatureList = class(TAbstractAppObject, IPumpingFeatureList)
  protected
    FPumpingFeatureList : TList;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function AddPumpingFeature (AFeature : TPumpingFeature): boolean;
  public
    function Initialise : boolean; override;
    function NewPumpingFeature : TPumpingFeature;
    function CreateNewPumpingFeature : TPumpingFeature;

    function DeletePumpingFeatureWithID(AFeatureID : integer) : WordBool;
    function DeletePumpingFeatureWithIndex(AIndex : integer) : WordBool;
    function CastPumpingFeatureByID(AFeatureID : integer): TPumpingFeature;
    function CastPumpingFeatureByChannelNr(AChannelNr : integer): TPumpingFeature;

    function CastPumpingFeatureByIndex(AIndex : integer): TPumpingFeature;
    function CopyPumpingFeature(ANewChannelNumber, AOldChannelNumber: Integer):IPumpingFeature; safecall;
    function CreatePumpingFeature : IPumpingFeature; safecall;
    function RemovePumpingFeatureWithID (AFeatureID : integer) : WordBool; safecall;
    function Get_PumpingFeatureByIndex(AIndex: integer): IPumpingFeature; safecall;
    function Get_PumpingFeatureByID(AFeatureID : integer): IPumpingFeature; safecall;
    function Get_PumpingFeatureCount : integer; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;

    property PumpingFeatureByIndex[AIndex : integer]: IPumpingFeature
      read Get_PumpingFeatureByIndex;
    property PumpingFeatureByID[AFeatureID: integer]: IPumpingFeature
      read Get_PumpingFeatureByID;
    property PumpingFeatureCount: integer read Get_PumpingFeatureCount;
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
{ TPumpingFeature                                                       }
{******************************************************************************}

function TPumpingFeature._AddRef: Integer;
const OPNAME = 'TPumpingFeature._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPumpingFeature._Release: Integer;
const OPNAME = 'TPumpingFeature._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPumpingFeature.CreateMemberObjects;
const OPNAME = 'TPumpingFeature.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPumpingFeature.DestroyMemberObjects;
const OPNAME = 'TPumpingFeature.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPumpingFeature.Initialise: boolean;
const OPNAME = 'TPumpingFeature.Initialise';
begin
  Result := inherited Initialise;
  try
    FChannelNr            := 0;
    FFeatureID            := -1;
    FFeatureName          := '';
    FFeatureType          := -1;
    FFeatureSubType       := -1;
    FPumpingHead          := -1;
    FPumpingEfficiency    := -1;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPumpingFeature.Get_FeatureID : integer;
const OPNAME = 'TPumpingFeature.Get_FeatureID';
begin
  Result := 0;
  try
    Result := FFeatureID;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TPumpingFeature.Get_FeatureName : WideString;
const OPNAME = 'TPumpingFeature.Get_FeatureName';
begin
  Result := '';
  try
    Result := FFeatureName;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TPumpingFeature.Get_Channel : IGeneralFlowChannel;
const OPNAME = 'TPumpingFeature.Get_Channel';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.
              ChannelByChannelNumber[FChannelNr];
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TPumpingFeature.Get_FeatureType : integer;
const OPNAME = 'TPumpingFeature.Get_FeatureType';
begin
  Result := 0;
  try
    Result := FFeatureType;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TPumpingFeature.Get_FeatureSubType : integer;
const OPNAME = 'TPumpingFeature.Get_FeatureSubType';
begin
  Result := 0;
  try
    Result := FFeatureSubType;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TPumpingFeature.Get_PumpingEfficiency: double;
const OPNAME = 'TPumpingFeature.Get_PumpingEfficiency';
begin
  Result := 0.0;
  try
    Result := StrToFloat(FAppModules.Changes.GetParameterValue
                                              ('PumpEfficiency',
                                              GetKeyValues('PumpEfficiency', ''),
                                              FloatToStr(FPumpingEfficiency),
                                              ''));
//    Result := FPumpingEfficiency;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPumpingFeature.Get_PumpingHead: double;
const OPNAME = 'TPumpingFeature.Get_PumpingHead';
begin
  Result := 0.0;
  try
    Result := StrToFloat(FAppModules.Changes.GetParameterValue
                                              ('PumpingHead',
                                              GetKeyValues('PumpingHead', ''),
                                              FloatToStr(FPumpingHead),
                                              ''));
//    Result := FPumpingHead;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPumpingFeature.Set_FeatureName (const AName : WideString);
const OPNAME = 'TPumpingFeature.Set_FeatureName';
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
           'PumpingFeatureName', AName, FFeatureName, LContextData) then
        begin
          LOldValue    := FFeatureName;
          FFeatureName := AName;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'PumpingFeatureName',LOldValue,AName);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPumpingFeature.Set_Channel (const AChannel : IGeneralFlowChannel);
const OPNAME = 'TPumpingFeature.Set_Channel';
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
           'PumpingFeatureChannelNumber', LNewValue, LOldValue, LContextData) then
        begin
          FChannelNr := AChannel.ChannelNumber;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'PumpingFeatureName',LOldValue,LNewValue);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPumpingFeature.Set_FeatureType (AType : integer);
const OPNAME = 'TPumpingFeature.Set_FeatureType';
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

procedure TPumpingFeature.Set_FeatureSubType (ASubType : integer);
const OPNAME = 'TPumpingFeature.Set_FeatureSubType';
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

procedure TPumpingFeature.Set_PumpingEfficiency(APumpingEfficiency: double);
const OPNAME = 'TPumpingFeature.Set_PumpingEfficiency';
var
  LLoadAgent: TChannelDataSQLAgent;
  LContextData: TStringList;
  LOldValue : double;
begin
  try
    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'PumpEfficiency', FloatToStr(APumpingEfficiency), FloatToStr(FPumpingEfficiency), LContextData) then
        begin
          LOldValue := FPumpingEfficiency;
          FPumpingEfficiency := APumpingEfficiency;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'PumpEfficiency', FloatToStr(LOldValue),FloatToStr(APumpingEfficiency));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPumpingFeature.Set_PumpingHead(APumpingHead: double);
const OPNAME = 'TPumpingFeature.Set_PumpingHead';
var
  LLoadAgent: TChannelDataSQLAgent;
  LContextData: TStringList;
  LOldValue : double;
begin
  try
    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'PumpingHead', FloatToStr(APumpingHead), FloatToStr(FPumpingHead), LContextData) then
        begin
          LOldValue := FPumpingHead;
          FPumpingHead := APumpingHead;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'PumpingHead', FloatToStr(LOldValue),FloatToStr(APumpingHead))
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
    FPumpingHead := APumpingHead;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPumpingFeature.Populate (AFeatureID           : integer;
                                   AFeatureName         : WideString;
                                   AChannelnr           : integer;
                                   AFeatureType         : integer;
                                   AFeatureSubType      : integer;
                                   APumpingHead         : double;
                                   APumpingEfficiency   : double): WordBool;
const OPNAME = 'TPumpingFeature.Populate';
begin
  Result := FALSE;
  try
    FChannelNr         := AChannelNr;
    FFeatureID         := AFeatureID;
    FFeatureName       := AFeatureName;
    FFeatureType       := AFeatureType;
    FFeatureSubType    := AFeatureSubType;
    FPumpingHead       := APumpingHead;
    FPumpingEfficiency := APumpingEfficiency;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{******************************************************************************}
{ TPumpingFeatureList                                                        }
{******************************************************************************}

function TPumpingFeatureList._AddRef: Integer;
const OPNAME = 'TPumpingFeatureList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPumpingFeatureList._Release: Integer;
const OPNAME = 'TPumpingFeatureList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPumpingFeatureList.CreateMemberObjects;
const OPNAME = 'TPumpingFeatureList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPumpingFeatureList := TList.Create;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPumpingFeatureList.DestroyMemberObjects;
const OPNAME = 'TPumpingFeatureList.DestroyMemberObjects';
begin
  try
    while (FPumpingFeatureList.Count > 0) do
      DeletePumpingFeatureWithIndex(0);
    FreeAndNil(FPumpingFeatureList);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPumpingFeatureList.Initialise: boolean;
const OPNAME = 'TPumpingFeatureList.Initialise';
begin
  Result := inherited Initialise;
  try
    FPumpingFeatureList.Clear;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPumpingFeatureList.AddPumpingFeature (AFeature : TPumpingFeature): boolean;
const OPNAME = 'TPumpingFeatureList.AddPumpingFeature';
begin
  Result := False;
  try
    if (AFeature <> nil) then
    begin
      FPumpingFeatureList.Add(AFeature);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPumpingFeatureList.NewPumpingFeature : TPumpingFeature;
const OPNAME = 'TPumpingFeatureList.NewPumpingFeature';
var
  lFeature : TPumpingFeature;
begin
  Result := nil;
  try
    lFeature := TPumpingFeature.Create(FAppModules);
    AddPumpingFeature(lFeature);
    Result := lFeature;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPumpingFeatureList.CreateNewPumpingFeature : TPumpingFeature;
const OPNAME = 'TPumpingFeatureList.CreateNewPumpingFeature';
var
  LFeatureID : integer;
  LLoadAgent : TChannelDataSQLAgent;
  LFeature   : TPumpingFeature;
begin
  Result := nil;
  try
    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
    try
      LFeatureID := 0;
      if (LLoadAgent.InsertPumpingFeature(LFeatureID)) then
      begin
        LFeature := NewPumpingFeature;
        LFeature.Initialise;
        LFeature.Populate
          (LFeatureID,
           UpperCase(FAppModules.Language.GetString('NetworkFeatures.PumpingFeature')) + ' ' + IntToStr(LFeatureID),
           0, 11, 0, 0, 0);
        Result := LFeature;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPumpingFeatureList.CreatePumpingFeature : IPumpingFeature;
const OPNAME = 'TPumpingFeatureList.CreatePumpingFeature';
var
  LFeature : IPumpingFeature;
begin
  Result := nil;
  try
    LFeature := CreateNewPumpingFeature;
    Result := lFeature;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPumpingFeatureList.RemovePumpingFeatureWithID (AFeatureID : integer) : WordBool;
const OPNAME = 'TPumpingFeatureList.RemovePumpingFeatureWithID';
var
  lLoadAgent : TChannelDataSQLAgent;
begin
  Result := FALSE;
  try
    if (AFeatureID > 0) then
    begin
      LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
      try
        if LLoadAgent.DeletePumpingFeature(AFeatureID) then
        begin
          DeletePumpingFeatureWithID(AFeatureID);
          Result := TRUE;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPumpingFeatureList.DeletePumpingFeatureWithID(AFeatureID : integer) : WordBool;
const OPNAME = 'TPumpingFeatureList.DeletePumpingFeatureWithID';
var
  lFeature : TPumpingFeature;
  lIndex   : integer;
begin
  Result := FALSE;
  try
    lFeature := CastPumpingFeatureByID(AFeatureID);
    if (lFeature <> nil) then
    begin
      lIndex := FPumpingFeatureList.IndexOf(lFeature);
      Result := DeletePumpingFeatureWithIndex(lIndex);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPumpingFeatureList.DeletePumpingFeatureWithIndex(AIndex : integer) : WordBool;
const OPNAME = 'TPumpingFeatureList.DeletePumpingFeatureWithIndex';
var
  lFeature : TPumpingFeature;
begin
  Result := FALSE;
  try
    if (AIndex >= 0) then
    begin
      lFeature := TPumpingFeature(FPumpingFeatureList.Items[AIndex]);
      FPumpingFeatureList.Delete(AIndex);
      FreeAndNil(lFeature);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPumpingFeatureList.Get_PumpingFeatureByIndex(AIndex: integer): IPumpingFeature;
const OPNAME = 'TPumpingFeatureList.Get_PumpingFeatureByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FPumpingFeatureList.Count) then
      Result := TPumpingFeature(FPumpingFeatureList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPumpingFeatureList.Get_PumpingFeatureByID(AFeatureID : integer): IPumpingFeature;
const OPNAME = 'TPumpingFeatureList.Get_PumpingFeatureByID';
var
 lIndex   : integer;
 lFeature : TPumpingFeature;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FPumpingFeatureList.Count)) do
    begin
      lFeature := TPumpingFeature(FPumpingFeatureList.Items[lIndex]);
      if (lFeature.FeatureID = AFeatureID) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPumpingFeatureList.CastPumpingFeatureByID(AFeatureID : integer): TPumpingFeature;
const OPNAME = 'TPumpingFeatureList.CastPumpingFeatureByID';
var
 lIndex   : integer;
 lFeature : TPumpingFeature;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FPumpingFeatureList.Count)) do
    begin
      lFeature := TPumpingFeature(FPumpingFeatureList.Items[lIndex]);
      if (lFeature.FeatureID = AFeatureID) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TPumpingFeatureList.CastPumpingFeatureByChannelNr(AChannelNr : integer): TPumpingFeature;
const OPNAME = 'TPumpingFeatureList.CastPumpingFeatureByChannelNr';
var
 lIndex   : integer;
 lFeature : TPumpingFeature;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FPumpingFeatureList.Count)) do
    begin
      lFeature := TPumpingFeature(FPumpingFeatureList.Items[lIndex]);
      if (lFeature.FChannelNr = AChannelNr) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPumpingFeatureList.CastPumpingFeatureByIndex(AIndex : integer): TPumpingFeature;
const OPNAME = 'TPumpingFeatureList.CastPumpingFeatureByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FPumpingFeatureList.Count) then
      Result := TPumpingFeature(FPumpingFeatureList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPumpingFeatureList.Get_PumpingFeatureCount: integer;
const OPNAME = 'TPumpingFeatureList.Get_PumpingFeatureCount';
begin
  Result := 0;
  try
    Result := FPumpingFeatureList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPumpingFeature.Validate (var AErrors : WideString; const AContext    : WideString='') : WordBool;
const OPNAME = 'TPumpingFeature.Validate';
var
  lStopOnFirstError : Boolean;
  lErrorList        : TStringList;
begin
  Result := FALSE;
  try
    lErrorList := TStringList.Create;
    try
     if (AContext = 'PumpingFeatureName') then
       Result := ValidateFeatureName(lErrorList)
      else
      if (AContext = 'PumpingHead') then
        Result := ValidatePumpingHead(lErrorList)
      else
      if (AContext = 'PumpEfficiency') then
        Result := ValidatePumpingEfficiency(lErrorList)
      else
      begin
        Result := TRUE;
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        if (NOT ValidateFeatureName(lErrorList)) then
          Result := FALSE;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidatePumpingHead(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidatePumpingEfficiency(lErrorList)) then
            Result := FALSE;
        end;
      end;
      AErrors := AErrors + lErrorList.Text;
    finally
      FreeAndNil(lErrorList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPumpingFeature.ValidateFeatureName(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TPumpingFeature.ValidateFeatureName';
var
  lMessage     : string;
  lFeature     : TPumpingFeature;
  lFeatureList : TPumpingFeatureList;
  lUnique      : Boolean;
  lIndex       : integer;
begin
  Result := True;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('PumpingFeatureName', FFeatureName, lMessage)) then
      AErrorMessages.Add('WARNING:' +Channel.ChannelName +':'+lMessage)
    else
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                       CastNetworkFeaturesData.CastPumpingFeatureList;
      lUnique := True;
      lIndex  := 0;
      while (lUnique AND (lIndex < lFeatureList.PumpingFeatureCount)) do
      begin
        lFeature := lFeatureList.CastPumpingFeatureByIndex(lIndex);
        if ((FFeatureID <> lFeature.FeatureID) AND
            (UpperCase(trim(FFeatureName)) = UpperCase(trim(lFeature.FeatureName)))) then
        begin
          lMessage := FAppModules.language.GetString('ContextValidation.DuplicatePumpingFeatureName');
          AErrorMessages.Add('WARNING:' +Format(lMessage, [Channel.ChannelName]));
          lUnique := False;
        end
        else
          lIndex := lIndex + 1;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPumpingFeature.ValidatePumpingHead(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TPumpingFeature.ValidatePumpingHead';
var
  lIndex            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lValid            : Boolean;
  lResult           : Boolean;
  lFieldProperty    : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    lFieldProperty := FAppModules.FieldProperties.FieldProperty('PumpingHead');
    if Assigned(lFieldProperty) then
    begin
      for lIndex := 0 to StrToInt(lFieldProperty.FieldMinimumValue) do
      begin
        lMessage := '';
        lValid := FAppModules.FieldProperties.ValidateFieldProperty
                  ('PumpingHead', FloatToStr(FPumpingHead),
                  lMessage);
        if (NOT lValid) then
        begin
          lResult := FALSE;
          AErrorMessages.Add('ERROR:' +Channel.ChannelName+ ':'+lMessage);
          if (lStopOnFirstError) then
            Break;
        end;
      end;
    end;
      Result := lResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPumpingFeature.ValidatePumpingEfficiency(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TPumpingFeature.ValidatePumpingEfficiency';
var
  lIndex            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lValid            : Boolean;
  lResult           : Boolean;
  lFieldProperty    : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    lFieldProperty := FAppModules.FieldProperties.FieldProperty('PumpEfficiency');
    if assigned (lFieldProperty) then
    begin
      for lIndex := 0 to StrToInt(lFieldProperty.FieldMinimumValue) do
      begin
        lMessage := '';
        lValid := FAppModules.FieldProperties.ValidateFieldProperty
                  ('PumpEfficiency', FloatToStr(FPumpingEfficiency),
                  lMessage);
        if (NOT lValid) then
        begin
          lResult := FALSE;
          AErrorMessages.Add('ERROR:' +Channel.ChannelName+ ':'+lMessage);
          if (lStopOnFirstError) then
            Break;
        end;
      end;
    end;
      Result := lResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPumpingFeatureList.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TPumpingFeatureList.Validate';
var
  lStopOnFirstError : Boolean;
  LIndex            : integer;
begin
  Result := FALSE;
  try
    Result := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    for LIndex := 0 to FPumpingFeatureList.Count - 1 do
    begin
      if (NOT PumpingFeatureByIndex[LIndex].Validate(AErrors,AContext)) then
        Result := FALSE;
      if ((NOT Result) AND lStopOnFirstError) then
        Break;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPumpingFeature.GetBaseValue (const AParamField: WideString;
                                       const AFieldIndex: WideString): WideString;
const OPNAME = 'TPumpingFeature.GetBaseValue';
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

    if (AParamField = 'PumpingHead') then
    begin
      if (lFormatStr = '') then
        Result := FloatToStr(FPumpingHead)
      else
        Result := Format(lFormatStr, [FPumpingHead]);
    end
    else if (AParamField = 'PumpEfficiency') then
    begin
      if (lFormatStr = '') then
        Result := FloatToStr(FPumpingEfficiency)
      else
        Result := Format(lFormatStr, [FPumpingEfficiency]);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPumpingFeature.GetKeyValues (const AParamField : WideString;
                                       const AFieldIndex : WideString) : WideString;
const OPNAME = 'TPumpingFeature.GetKeyValues';
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

procedure TPumpingFeature.Assign(AChannelNumber: integer;APumpingFeature: TPumpingFeature);
const OPNAME = 'TPumpingFeature.Assign';
var
  LChannel : IGeneralFlowChannel;
begin
  try
    if (APumpingFeature <> nil) and (AChannelNumber > 0) then
    begin
      LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[AChannelNumber];
      if LChannel <> nil then
        Channel := LChannel;
      FeatureName := 'Copy of '+APumpingFeature.FeatureName;
      FeatureType := APumpingFeature.FeatureType;
      FeatureSubType := APumpingFeature.FeatureSubType;
      PumpingHead := APumpingFeature.PumpingHead;
      PumpingEfficiency := APumpingFeature.PumpingEfficiency;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPumpingFeatureList.CopyPumpingFeature(ANewChannelNumber,AOldChannelNumber: Integer): IPumpingFeature;
const OPNAME = 'TPumpingFeatureList.CopyPumpingFeature';
var
  LPumpingFeature : TPumpingFeature;
  LPumpingFeatureCopy : TPumpingFeature;
begin
  Result := nil;
  try
    LPumpingFeature := CastPumpingFeatureByChannelNr(AOldChannelNumber);
    if (LPumpingFeature <> nil) then
    begin
      LPumpingFeatureCopy := CreateNewPumpingFeature;
      if LPumpingFeatureCopy <> nil then
      begin
        LPumpingFeatureCopy.Assign(ANewChannelNumber,LPumpingFeature);
        Result := LPumpingFeatureCopy;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
