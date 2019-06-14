{******************************************************************************}
{*  UNIT      : Contains the class TLossFeature.                              *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2004/03/29                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit ULossFeatures;

interface

uses
  Classes,
  Contnrs,
  UAbstractObject,
  VoaimsCom_TLB;

type

{******************************************************************************}
{* Loss Features                                                              *}
{******************************************************************************}

  TLossFeature = class(TAbstractAppObject, ILossFeature)
  protected
    FFeatureID           : integer;
    FFeatureName         : string;
    FChannelNr           : integer;
    FFeatureType         : integer;
    FFeatureSubType      : integer;
    FReferenceNode       : integer;
    FMonthlyWaterLoss    : TLossMonthlyDoublesArray;
    FMonthlyDivertedFlow : TLossMonthlyDoublesArray;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function ValidateLossFeatureName(AErrorMessages: TStrings): WordBool;
    function ValidateWaterLoss (AErrorMessages : TStrings) : WordBool;
    function ValidateDivertedFlow (AErrorMessages : TStrings) : WordBool;
    function ValidateReferenceNodes(AErrorMessages : TStrings) : WordBool;
  public
    function Initialise : boolean; override;
    function Populate (AFeatureID           : integer;
                       AFeatureName         : WideString;
                       AChannelNr           : integer;
                       AFeatureType         : integer;
                       AFeatureSubType      : integer;
                       ARefNode             : integer;
                       AWaterLoss           : TLossMonthlyDoublesArray;
                       ADivertedFlow        : TLossMonthlyDoublesArray): WordBool;
    procedure Assign(AChannelNumber : integer;ALossFeature : TLossFeature);
    function Get_FeatureID : integer; safecall;
    function Get_FeatureName : WideString; safecall;
    procedure Set_FeatureName ( const AName : WideString); safecall;
    function Get_Channel : IGeneralFlowChannel; safecall;
    procedure Set_Channel (const AChannel : IGeneralFlowChannel); safecall;
    function Get_FeatureType : integer; safecall;
    procedure Set_FeatureType (AType : integer); safecall;
    function Get_FeatureSubType : integer; safecall;
    procedure Set_FeatureSubType (ASubType : integer); safecall;
    function Get_ReferenceNode: integer; safecall;
    procedure Set_ReferenceNode(ARefNode: integer); safecall;
    function Get_WaterLossByMonth (AMonth : integer) : double; safecall;
    procedure Set_WaterLossByMonth (AMonth : integer;AValue : double); safecall;
    function Get_DivertedFlowByMonth (AMonth : integer) : double; safecall;
    procedure Set_DivertedFlowByMonth (AMonth : integer;AValue : double); safecall;
    function Validate (var AErrors : WideString; const AContext    : WideString='') : WordBool; safecall;
    function GetBaseValue(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function GetKeyValues (const AParamField : WideString; const AFieldIndex: WideString) : WideString; safecall;
    property FeatureID      : integer read Get_FeatureID;
    property FeatureName    : WideString read Get_FeatureName write Set_FeatureName;
    property Channel        : IGeneralFlowChannel read Get_Channel write Set_Channel;
    property FeatureType    : integer read Get_FeatureType write Set_FeatureType;
    property FeatureSubType : integer read Get_FeatureSubType write Set_FeatureSubType;
    property WaterLossByMonth[AMonth : integer] : double
      read Get_WaterLossByMonth write Set_WaterLossByMonth;
    property DivertedFlowByMonth[AMonth : integer] : double
      read Get_DivertedFlowByMonth write Set_DivertedFlowByMonth;
  end;

  TLossFeatureList = class(TAbstractAppObject, ILossFeatureList)
  protected
    FLossFeatureList : TList;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function AddLossFeature (AFeature : TLossFeature): boolean;
  public
    function Initialise : boolean; override;
    function NewLossFeature : TLossFeature;
    function CreateNewLossFeature : ILossFeature; safecall;
    function CopyLossFeature(ANewChannelNumber, AOldChannelNumber: Integer): ILossFeature; safecall;
    function DeleteLossFeatureWithID(AFeatureID : integer) : WordBool;
    function DeleteLossFeatureWithIndex(AIndex : integer) : WordBool;
    function CastLossFeatureByIndex(AIndex : integer): TLossFeature;
    function CastLossFeatureByID(AFeatureID : integer): TLossFeature;
    function CastLossFeatureByChannelNr(AChannelNr : integer): TLossFeature;

    function CreateLossFeature : ILossFeature; safecall;
    function RemoveLossFeatureWithID (AFeatureID : integer) : WordBool; safecall;
    function Get_LossFeatureByIndex(AIndex: integer): ILossFeature; safecall;
    function Get_LossFeatureByID(AFeatureID : integer): ILossFeature; safecall;
    function Get_LossFeatureCount : integer; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString = ''): WordBool; safecall;

    property LossFeatureByIndex[AIndex : integer]: ILossFeature
      read Get_LossFeatureByIndex;
    property LossFeatureByID[AFeatureID: integer]: ILossFeature
      read Get_LossFeatureByID;
    property LossFeatureCount: integer read Get_LossFeatureCount;
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
{ TLossFeature                                                                 }
{******************************************************************************}

function TLossFeature._AddRef: Integer;
const OPNAME = 'TLossFeature._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TLossFeature._Release: Integer;
const OPNAME = 'TLossFeature._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TLossFeature.CreateMemberObjects;
const OPNAME = 'TLossFeature.CreateMemberObjects';
var
  LMonthlyWaterLoss,
  LMonthlyDivertedFlow: TAbstractFieldProperty;
begin
  inherited;
  try
    LMonthlyWaterLoss := FAppModules.FieldProperties.FieldProperty('WaterLoss');
    if not Assigned(LMonthlyWaterLoss) then
      raise Exception.Create('Field (WaterLoss) not found in field properties');
    SetLength(FMonthlyWaterLoss,LMonthlyWaterLoss.ArrayLength);

    LMonthlyDivertedFlow := FAppModules.FieldProperties.FieldProperty('DivertedFlowP');
    if not Assigned(LMonthlyDivertedFlow) then
      raise Exception.Create('Field (DivertedFlowP) not found in field properties');
    SetLength(FMonthlyDivertedFlow,LMonthlyDivertedFlow.ArrayLength);

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TLossFeature.DestroyMemberObjects;
const OPNAME = 'TLossFeature.DestroyMemberObjects';
begin
  try
    Finalize(FMonthlyWaterLoss);
    Finalize(FMonthlyDivertedFlow);
  except on E: Exception do HandleError(E, OPNAME); end;
  inherited;
end;

function TLossFeature.Initialise: boolean;
const OPNAME = 'TLossFeature.Initialise';
var
  nIndex : integer;
  LMonthlyWaterLoss,
  LMonthlyDivertedFlow: TAbstractFieldProperty;
begin
  Result := inherited Initialise;
  try
    FChannelNr            := 0;
    FFeatureID            := -1;
    FFeatureName          := '';
    FFeatureType          := -1;
    FFeatureSubType       := -1;
    FReferenceNode        := -1;
    LMonthlyWaterLoss := FAppModules.FieldProperties.FieldProperty('WaterLoss');
    for nIndex := LMonthlyWaterLoss.ArrayLow to LMonthlyWaterLoss.ArrayHigh do
      FMonthlyWaterLoss[nIndex] := NullFloat;

    LMonthlyDivertedFlow := FAppModules.FieldProperties.FieldProperty('DivertedFlowP');
    for nIndex := LMonthlyDivertedFlow.ArrayLow to LMonthlyDivertedFlow.ArrayHigh do
      FMonthlyDivertedFlow[nIndex] := NullFloat;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TLossFeature.Get_FeatureID : integer;
const OPNAME = 'TLossFeature.Get_FeatureID';
begin
  Result := 0;
  try
    Result := FFeatureID;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TLossFeature.Get_FeatureName : WideString;
const OPNAME = 'TLossFeature.Get_FeatureName';
begin
  Result := '';
  try
    Result := FFeatureName;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TLossFeature.Get_Channel : IGeneralFlowChannel;
const OPNAME = 'TLossFeature.Get_Channel';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                ChannelList.ChannelByChannelNumber[FChannelNr];
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TLossFeature.Get_FeatureType : integer;
const OPNAME = 'TLossFeature.Get_FeatureType';
begin
  Result := 0;
  try
    Result := FFeatureType;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TLossFeature.Get_FeatureSubType : integer;
const OPNAME = 'TLossFeature.Get_FeatureSubType';
begin
  Result := 0;
  try
    Result := FFeatureSubType;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TLossFeature.Get_ReferenceNode: integer;
const OPNAME = 'TLossFeature.Get_ReferenceNode';
begin
  Result := NullInteger;
  try
    Result := FReferenceNode;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TLossFeature.Get_WaterLossByMonth (AMonth : integer) : double;
const OPNAME = 'TLossFeature.Get_WaterLossByMonth';
begin
  Result := 0.0;
  try
    Result := StrToFloat(FAppModules.Changes.GetParameterValue
                                              ('WaterLoss',
                                              GetKeyValues('WaterLoss', IntToStr(AMonth)),
                                              FloatToStr(FMonthlyWaterLoss[AMonth]),
                                              IntToStr(AMonth)));
//    Result := FMonthlyWaterLoss[AMonth];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TLossFeature.Get_DivertedFlowByMonth (AMonth : integer) : double;
const OPNAME = 'TLossFeature.Get_DivertedFlowByMonth';
begin
  Result := 0.0;
  try
    Result := FMonthlyDivertedFlow[AMonth];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TLossFeature.Set_FeatureName(const AName : WideString);
const OPNAME = 'TLossFeature.Set_FeatureName';
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
           'LossFeatureName', AName, FFeatureName, LContextData) then
        begin
          LOldValue    := FFeatureName;
          FFeatureName := AName;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'LossFeatureName',LOldValue,AName);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TLossFeature.Set_Channel (const AChannel : IGeneralFlowChannel);
const OPNAME = 'TLossFeature.Set_Channel';
var
  LLoadAgent   : TChannelDataSQLAgent;
  LContextData : TStringList;
  LNewNr       : integer;
  LOldNr       : integer;
begin
  try
    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        if (AChannel <> nil) then
          LNewNr := AChannel.ChannelNumber
        else
          LNewNr := 0;
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'LossFeatureChannelNumber', IntToStr(LNewNr), IntToStr(FChannelNr), LContextData) then
        begin
          LOldNr     := FChannelNr;
          FChannelNr := LNewNr;
          FAppModules.Model.StudyDataHasChanged
            (sdccEdit,'LossFeatureChannelNumber', IntToStr(LOldNr),IntToStr(LNewNr));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TLossFeature.Set_FeatureType (AType : integer);
const OPNAME = 'TLossFeature.Set_FeatureType';
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

procedure TLossFeature.Set_FeatureSubType (ASubType : integer);
const OPNAME = 'TLossFeature.Set_FeatureSubType';
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

procedure TLossFeature.Set_WaterLossByMonth (AMonth : integer;
                                             AValue : double);
const OPNAME = 'TLossFeature.Set_WaterLossByMonth';
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
        LLoadAgent.LoadContextData_FeatureIDSubIDFieldNameID
          (LContextData, IntToStr(FFeatureID), IntToStr(0), IntToStr(AMonth));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'WaterLoss', FloatToStr(AValue), FloatToStr(FMonthlyWaterLoss[AMonth]), LContextData) then
        begin
          LPrevValue := FMonthlyWaterLoss[AMonth];
          FMonthlyWaterLoss[AMonth] := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'WaterLoss',FloatToStr(LPrevValue),FloatToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TLossFeature.Set_DivertedFlowByMonth (AMonth : integer;
                                                AValue : double);
const OPNAME = 'TLossFeature.Set_DivertedFlowByMonth';
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
        LLoadAgent.LoadContextData_FeatureIDSubIDFieldNameID
          (LContextData, IntToStr(FFeatureID), IntToStr(1), IntToStr(AMonth));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'DivertedFlowP', FloatToStr(AValue), FloatToStr(FMonthlyDivertedFlow[AMonth]), LContextData) then
        begin
          LPrevValue := FMonthlyDivertedFlow[AMonth];
          FMonthlyDivertedFlow[AMonth] := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MinFlowDemand',FloatToStr(LPrevValue),FloatToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TLossFeature.Set_ReferenceNode(ARefNode: integer);
const OPNAME = 'TLossFeature.Set_ReferenceNode';
var
  LLoadAgent   : TChannelDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : integer;
  lNewValue    : string;
begin
  try
    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if (ARefNode = 0) then
          lNewValue := ''
        else
          lNewValue := IntToStr(ARefNode);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'Reference', lNewValue, IntToStr(FReferenceNode), LContextData) then
        begin
          LOldValue := FReferenceNode;
          FReferenceNode := ARefNode;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'Reference',IntToStr(LOldValue),IntToStr(ARefNode));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TLossFeature.Populate (AFeatureID           : integer;
                                AFeatureName         : WideString;
                                AChannelNr           : integer;
                                AFeatureType         : integer;
                                AFeatureSubType      : integer;
                                ARefNode             : integer;
                                AWaterLoss           : TLossMonthlyDoublesArray;
                                ADivertedFlow        : TLossMonthlyDoublesArray): WordBool;
const OPNAME = 'TLossFeature.Populate';
var
  lMonth : integer;
  LMonthlyWaterLoss,
  LMonthlyDivertedFlow: TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    FChannelNr           := AChannelNr;
    FFeatureID           := AFeatureID;
    FFeatureName         := AFeatureName;
    FFeatureType         := AFeatureType;
    FFeatureSubType      := AFeatureSubType;
    FReferenceNode       := ARefNode;
    LMonthlyWaterLoss    := FAppModules.FieldProperties.FieldProperty('WaterLoss');
    LMonthlyDivertedFlow := FAppModules.FieldProperties.FieldProperty('DivertedFlowP');
    for lMonth := LMonthlyWaterLoss.ArrayLow to LMonthlyWaterLoss.ArrayHigh do
      FMonthlyWaterLoss[lMonth] := AWaterLoss[lMonth];
    for lMonth := LMonthlyDivertedFlow.ArrayLow to LMonthlyDivertedFlow.ArrayHigh do
      FMonthlyDivertedFlow[lMonth] := ADivertedFlow[lMonth];
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TLossFeature.ValidateWaterLoss(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TLossFeature.ValidateWaterLoss';
var
  lIndex            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lResult           : Boolean;
  lWaterLoss        : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    lWaterLoss := FAppModules.FieldProperties.FieldProperty('WaterLoss');
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    for lIndex := lWaterLoss.ArrayLow to lWaterLoss.ArrayHigh do
    begin
      lMessage := '';
      if (NOT FAppModules.FieldProperties.ValidateFieldProperty
                  ('WaterLoss', FloatToStr(FMonthlyWaterLoss[lIndex]),
                   lMessage, lIndex)) then
      begin
        lResult := FALSE;
        AErrorMessages.Add('ERROR:' + Channel.ChannelName +':'+lMessage);
        if (lStopOnFirstError) then
          Break;
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TLossFeature.ValidateDivertedFlow(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TLossFeature.ValidateDivertedFlow';
var
  lIndex            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lResult           : Boolean;
  lDivertedFlow     : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    lDivertedFlow := FAppModules.FieldProperties.FieldProperty('DivertedFlowP');
    if (FFeatureSubType = 1) then
    begin
      lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
      for lIndex := lDivertedFlow.ArrayLow to lDivertedFlow.ArrayHigh do
      begin
        lMessage := '';
        if (NOT FAppModules.FieldProperties.ValidateFieldProperty
                    ('DivertedFlowP', FloatToStr(FMonthlyDivertedFlow[lIndex]),
                     lMessage, lIndex)) then
        begin
          lResult := FALSE;
          AErrorMessages.Add('ERROR:' +Channel.ChannelName+ ':'+lMessage);
          if (lStopOnFirstError) then
            Break;
        end;
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{******************************************************************************}
{ TLossFeatureList                                                            *}
{******************************************************************************}

function TLossFeatureList._AddRef: Integer;
const OPNAME = 'TLossFeatureList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TLossFeatureList._Release: Integer;
const OPNAME = 'TLossFeatureList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TLossFeatureList.CreateMemberObjects;
const OPNAME = 'TLossFeatureList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FLossFeatureList := TList.Create;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TLossFeatureList.DestroyMemberObjects;
const OPNAME = 'TLossFeatureList.DestroyMemberObjects';
begin
  try
    while (FLossFeatureList.Count > 0) do
      DeleteLossFeatureWithIndex(0);
    FreeAndNil(FLossFeatureList);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TLossFeatureList.Initialise: boolean;
const OPNAME = 'TLossFeatureList.Initialise';
begin
  Result := inherited Initialise;
  try
    FLossFeatureList.Clear;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TLossFeatureList.AddLossFeature (AFeature : TLossFeature): boolean;
const OPNAME = 'TLossFeatureList.AddLossFeature';
begin
  Result := False;
  try
    if (AFeature <> nil) then
    begin
      FLossFeatureList.Add(AFeature);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TLossFeatureList.NewLossFeature : TLossFeature;
const OPNAME = 'TLossFeatureList.NewLossFeature';
var
  lFeature : TLossFeature;
begin
  Result := nil;
  try
    lFeature := TLossFeature.Create(FAppModules);
    AddLossFeature(lFeature);
    Result := lFeature;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TLossFeatureList.CreateLossFeature : ILossFeature;
const OPNAME = 'TLossFeatureList.CreateLossFeature';
var
  LFeature : ILossFeature;
begin
  Result := nil;
  try
    LFeature := CreateNewLossFeature;
    Result := lFeature;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TLossFeatureList.RemoveLossFeatureWithID (AFeatureID : integer) : WordBool;
const OPNAME = 'TLossFeatureList.RemoveLossFeatureWithID';
var
  lLoadAgent : TChannelDataSQLAgent;
begin
  Result := FALSE;
  try
    if (AFeatureID > 0) then
    begin
      LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
      try
        if LLoadAgent.DeleteLossFeature(AFeatureID) then
        begin
          DeleteLossFeatureWithID(AFeatureID);
          Result := TRUE;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TLossFeatureList.DeleteLossFeatureWithID(AFeatureID : integer) : WordBool;
const OPNAME = 'TLossFeatureList.DeleteLossFeatureWithID';
var
  lFeature : TLossFeature;
  lIndex   : integer;
begin
  Result := FALSE;
  try
    lFeature := CastLossFeatureByID(AFeatureID);
    if (lFeature <> nil) then
    begin
      lIndex := FLossFeatureList.IndexOf(lFeature);
      Result := DeleteLossFeatureWithIndex(lIndex);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TLossFeatureList.DeleteLossFeatureWithIndex(AIndex : integer) : WordBool;
const OPNAME = 'TLossFeatureList.DeleteLossFeatureWithIndex';
var
  lFeature : TLossFeature;
begin
  Result := FALSE;
  try
    if (AIndex >= 0) then
    begin
      lFeature := TLossFeature(FLossFeatureList.Items[AIndex]);
      FLossFeatureList.Delete(AIndex);
      FreeAndNil(lFeature);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TLossFeatureList.Get_LossFeatureByIndex(AIndex: integer): ILossFeature;
const OPNAME = 'TLossFeatureList.Get_LossFeatureByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FLossFeatureList.Count) then
      Result := TLossFeature(FLossFeatureList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TLossFeatureList.Get_LossFeatureByID(AFeatureID : integer): ILossFeature;
const OPNAME = 'TLossFeatureList.Get_LossFeatureByID';
var
 lIndex   : integer;
 lFeature : TLossFeature;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FLossFeatureList.Count)) do
    begin
      lFeature := TLossFeature(FLossFeatureList.Items[lIndex]);
      if (lFeature.FeatureID = AFeatureID) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TLossFeatureList.CastLossFeatureByIndex(AIndex: integer): TLossFeature;
const OPNAME = 'TLossFeatureList.CastLossFeatureByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FLossFeatureList.Count) then
      Result := TLossFeature(FLossFeatureList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TLossFeatureList.CastLossFeatureByID(AFeatureID : integer): TLossFeature;
const OPNAME = 'TLossFeatureList.CastLossFeatureByID';
var
 lIndex   : integer;
 lFeature : TLossFeature;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FLossFeatureList.Count)) do
    begin
      lFeature := TLossFeature(FLossFeatureList.Items[lIndex]);
      if (lFeature.FeatureID = AFeatureID) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TLossFeatureList.CastLossFeatureByChannelNr(AChannelNr : integer): TLossFeature;
const OPNAME = 'TLossFeatureList.CastLossFeatureByChannelNr';
var
 lIndex   : integer;
 lFeature : TLossFeature;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FLossFeatureList.Count)) do
    begin
      lFeature := TLossFeature(FLossFeatureList.Items[lIndex]);
      if (lFeature.FChannelNr = AChannelNr) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TLossFeatureList.Get_LossFeatureCount: integer;
const OPNAME = 'TLossFeatureList.Get_LossFeatureCount';
begin
  Result := 0;
  try
    Result := FLossFeatureList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TLossFeature.ValidateLossFeatureName(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TLossFeature.ValidateLossFeatureName';
var
  lIndex       : integer;
  lUnique      : Boolean;
  lMessage     : string;
  lFeature     : TLossFeature;
  lFeatureList : TLossFeatureList;
begin
  Result := True;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('LossFeatureName', FFeatureName, lMessage)) then
      AErrorMessages.Add('WARNING:' +Trim(Channel.ChannelName)+ ':'+lMessage)
    else
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        CastNetworkFeaturesData.CastLossFeatureList;
      lUnique := TRUE;
      lIndex := 0;
      while (lUnique AND (lIndex < lFeatureList.LossFeatureCount)) do
      begin
        lFeature := lFeatureList.CastLossFeatureByIndex(lIndex);
        if ((lFeature.FeatureID <>  FFeatureID) AND
           (UpperCase(trim(lFeature.FFeatureName)) = UpperCase(trim(FFeatureName)))) then
        begin
          lMessage := FAppModules.language.GetString('ContextValidation.DuplicateLossChannelName');
          AErrorMessages.Add('WARNING:' +Format(lMessage,[Channel.ChannelName]));
          lUnique := FALSE;
        end
        else
          lIndex := lIndex + 1;
      end;
      //Result := lUnique;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TLossFeature.ValidateReferenceNodes(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TLossFeature.ValidateReferenceNodes';
var
  lMessage       : string;
  lReferenceNode : IReservoirData;
begin
  Result := FALSE;
  try
    lMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty('Reference', IntToStr(FReferenceNode), lMessage);
    if (NOT Result) then
      AErrorMessages.Add('ERROR:' +lMessage)
    else
    begin
      lReferenceNode := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                 ReservoirList.ReservoirOrNodeByIdentifier[FReferenceNode];
      if (lReferenceNode = nil) then
      begin
        Result := FALSE;
        lMessage := FAppModules.language.GetString('ContextValidation.InvalidReferenceNodeNumber');
        AErrorMessages.Add('ERROR:' +Format(lMessage, [IntToStr(FReferenceNode)]));
      end
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TLossFeature.Validate (var AErrors : WideString; const AContext: WideString='') : WordBool;
const OPNAME = 'TLossFeature.Validate';
var
  lStopOnFirstError : Boolean;
  lErrorList        : TStringList;
begin
  Result := FALSE;
  try
    lErrorList := TStringList.Create;
    try
      if (AContext = 'MinFlowChannelName') then
        Result := ValidateLossFeatureName(lErrorList)
      else
      if (AContext = 'WaterLoss') then
        Result := ValidateWaterLoss(lErrorList)
      else
      if (AContext = 'DivertedFlowP') then
        Result := ValidateDivertedFlow(lErrorList)
      else
      if (AContext = 'Reference') then
        Result := ValidateReferenceNodes(lErrorList)
      else
      begin
        Result := TRUE;
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        if (NOT ValidateLossFeatureName(lErrorList)) then
          Result := FALSE;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateWaterLoss(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateDivertedFlow(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateReferenceNodes(lErrorList)) then
            Result := FALSE;
        end;
      end;
      AErrors := AErrors + lErrorList.Text;
    finally;
      FreeAndNil(lErrorList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLossFeatureList.CreateNewLossFeature: ILossFeature;
const OPNAME = 'TLossFeatureList.CreateNewLossFeature';
var
  LFeatureID           : integer;
  LLoadAgent           : TChannelDataSQLAgent;
  LFeature             : TLossFeature;
  LWaterLoss           : TLossMonthlyDoublesArray;
  LDivertedFlow        : TLossMonthlyDoublesArray;
  LMonthlyWaterLoss    : TAbstractFieldProperty;
  LMonthlyDivertedFlow : TAbstractFieldProperty;
  lIndex               : integer;
begin
  Result := nil;
  try
    LMonthlyWaterLoss := FAppModules.FieldProperties.FieldProperty('Month');
    if not Assigned(LMonthlyWaterLoss) then
      raise Exception.Create('Field (Month) not found in field properties');
    SetLength(LWaterLoss,LMonthlyWaterLoss.ArrayLength);

    LMonthlyDivertedFlow := FAppModules.FieldProperties.FieldProperty('Month');
    if not Assigned(LMonthlyDivertedFlow) then
      raise Exception.Create('Field (Month) not found in field properties');
    SetLength(LDivertedFlow,LMonthlyDivertedFlow.ArrayLength);

    for lIndex := LMonthlyWaterLoss.ArrayLow to LMonthlyDivertedFlow.ArrayHigh do
      lWaterLoss[lIndex] := 0;
    for lIndex := LMonthlyDivertedFlow.ArrayLow to LMonthlyDivertedFlow.ArrayHigh do
      lDivertedFlow[lIndex] := NullFloat;

    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
    try
      LFeatureID := 0;
      if (LLoadAgent.InsertLossFeature(LFeatureID)) then
      begin
        LFeature := NewLossFeature;
        LFeature.Initialise;
        LFeature.Populate
          (LFeatureID,
           UpperCase(FAppModules.Language.GetString('NetworkFeatures.LossFeature')) + ' ' + IntToStr(LFeatureID),
           0, 2, 0, 0, lWaterLoss, lDivertedFlow);

        Result := LFeature;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TLossFeatureList.Validate(var AErrors: WideString;const AContext: WideString): WordBool;
const OPNAME = 'TLossFeatureList.Validate';
var
  lStopOnFirstError : Boolean;
  LIndex            : integer;
begin
  Result := FALSE;
  try
    Result := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    for LIndex := 0 to FLossFeatureList.Count - 1 do
    begin
      if (NOT LossFeatureByIndex[LIndex].Validate(AErrors,AContext)) then
        Result := FALSE;
      if ((NOT Result) AND lStopOnFirstError) then
        Break;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TLossFeature.GetBaseValue (const AParamField: WideString;
                                    const AFieldIndex: WideString): WideString;
const OPNAME = 'TLossFeature.GetBaseValue';
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

    if (AParamField = 'WaterLoss') then
    begin
      if (lFormatStr = '') then
        Result := FloatToStr(FMonthlyWaterLoss[StrToInt(AFieldIndex)])
      else
        Result := Format(lFormatStr, [FMonthlyWaterLoss[StrToInt(AFieldIndex)]]);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TLossFeature.GetKeyValues (const AParamField : WideString;
                                    const AFieldIndex : WideString) : WideString;
const OPNAME = 'TLossFeature.GetKeyValues';
begin
  Result := '';
  try
    Result := GetDefaultKey;
    if (Result = '') then
      Result := 'Identifier=' + IntToStr(FFeatureID)
    else
      Result := Result + ',Identifier=' + IntToStr(FFeatureID);
    if (AParamField = 'WaterLoss') then
      Result := Result + ',SubIdentifier=0';
    if (AParamField = 'DivertedFlowP') then
      Result := Result + ',SubIdentifier=1';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLossFeatureList.CopyLossFeature(ANewChannelNumber,AOldChannelNumber: Integer): ILossFeature;
const OPNAME = 'TLossFeatureList.CopyLossFeature';
var
  LLossFeature : TLossFeature;
  LLossFeatureCopy : TLossFeature;
  LNewLossFeature : ILossFeature;
begin
  Result := nil;
  try
    LLossFeature := CastLossFeatureByChannelNr(AOldChannelNumber);
    if (LLossFeature <> nil) then
    begin
      LNewLossFeature := CreateNewLossFeature;
      if LNewLossFeature <> nil then
      begin
        LLossFeatureCopy := CastLossFeatureByID(LNewLossFeature.FeatureID);
        if LLossFeatureCopy <> nil then
        begin
          LLossFeatureCopy.Assign(ANewChannelNumber,LLossFeature);
          Result := LLossFeatureCopy;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TLossFeature.Assign(AChannelNumber: integer;ALossFeature: TLossFeature);
const OPNAME = 'TLossFeature.Assign';
var
  LChannel : IGeneralFlowChannel;
  LMonthlyWaterLoss,
  LMonthlyDivertedFlow: TAbstractFieldProperty;
  LMonth : integer;
begin
  try
    if (ALossFeature <> nil) and (AChannelNumber > 0) then
    begin
      LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[AChannelNumber];
      if LChannel <> nil then
        Channel := LChannel;
      FeatureName := 'Copy of '+ALossFeature.FeatureName;
      FeatureType := ALossFeature.FeatureType;
      FeatureSubType := ALossFeature.FeatureSubType;
      LMonthlyWaterLoss    := FAppModules.FieldProperties.FieldProperty('WaterLoss');
      LMonthlyDivertedFlow := FAppModules.FieldProperties.FieldProperty('DivertedFlowP');
      if FeatureSubType = 0 then
      begin
        for LMonth := LMonthlyWaterLoss.ArrayLow to LMonthlyWaterLoss.ArrayHigh do
          WaterLossByMonth[LMonth] := ALossFeature.WaterLossByMonth[LMonth];
      end
      else
      begin
        for lMonth := LMonthlyDivertedFlow.ArrayLow to LMonthlyDivertedFlow.ArrayHigh do
          DivertedFlowByMonth[LMonth] := ALossFeature.DivertedFlowByMonth[LMonth];
      end;

    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
