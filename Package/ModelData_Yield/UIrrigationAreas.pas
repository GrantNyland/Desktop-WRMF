{******************************************************************************}
{*  UNIT      : Contains the class TIrrigationArea.                           *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2004/03/10                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UIrrigationAreas;

interface

uses
  Classes,
  Contnrs,
  UAbstractObject,
  UReservoirData,
  VoaimsCom_TLB;

type

{******************************************************************************}
{* Irrigation Area                                                            *}
{******************************************************************************}

  TIrrigationArea = class(TAbstractAppObject, IIrrigationArea)
  protected
    FFeatureID             : integer;
    FFeatureName           : string;
    FIrrigationNodeNumber  : integer;
    FConsumptiveChannelNr  : integer;
    FFeatureType           : integer;
    FFeatureSubType        : integer;
    FMonthlyDiversionFlows : TIrrMonthlyDoublesArray;
    FMonthlyReturnFlows    : TIrrMonthlyDoublesArray;
    FDiversionChannelNr    : integer;
    FReturnFlowChannelNr   : integer;
    FIrrigationPolicy      : integer;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function ValidateIrrigationAreaName(AErrorMessages : TStrings): WordBool;
    function ValidateDiversionUpstreamNode(AErrorMessages : TStrings): WordBool;
    function ValidateReturnFlowDownstreamNode(AErrorMessages : TStrings): WordBool;
    function ValidateRelaxationPolicy(AErrorMessages : TStrings): WordBool;
    function ValidateDiversionFlows(AErrorMessages : TStrings; AErrorColumns: TStrings): WordBool;
    function ValidateReturnFlows(AErrorMessages : TStrings; AErrorColumns: TStrings): WordBool;
  public
    procedure Assign(AIrrigationArea: TIrrigationArea);
    function Initialise: boolean; override;
    function Populate (AFeatureID             : integer;
                       AFeatureName           : WideString;
                       AIrrigationNodeNumber  : integer;
                       AConsumptiveChannelNr  : integer;
                       AFeatureType           : integer;
                       AFeatureSubType        : integer;
                       AMonthlyDiversionFlows : TIrrMonthlyDoublesArray;
                       AMonthlyReturnFlows    : TIrrMonthlyDoublesArray;
                       ADiversionChannelNr    : integer;
                       AReturnFlowChannelNr   : integer;
                       AIrrigationPolicy      : integer) : WordBool;
    function Get_FeatureID : integer; safecall;
    function Get_FeatureName : WideString; safecall;
    procedure Set_FeatureName (const AName : WideString); safecall;
    function Get_IrrigationNodeNumber: integer; safecall;
    function Get_ConsumptiveChannelNumber: integer; safecall;
    function Get_DiversionChannelNumber: integer; safecall;
    function Get_ReturnFlowChannelNumber: integer; safecall;
    function Get_FeatureType : integer; safecall;
    procedure Set_FeatureType (AType : integer); safecall;
    function Get_FeatureSubType : integer; safecall;
    procedure Set_FeatureSubType (ASubType : integer); safecall;
    function Get_MonthlyDiversionFlows : TIrrMonthlyDoublesArray; safecall;
    function Get_DiversionFlowByMonth (AIndex : integer) : double; safecall;
    procedure Set_DiversionFlowByMonth(AIndex : integer;
                                       AValue : double); safecall;
    function Get_MonthlyReturnFlows : TIrrMonthlyDoublesArray; safecall;
    function Get_ReturnFlowByMonth (AIndex : integer) : double; safecall;
    procedure Set_ReturnFlowByMonth (AIndex : integer;
                                     AValue : double); safecall;
    function Get_DiversionChannel : IGeneralFlowChannel; safecall;
    function Get_ReturnFlowChannel : IGeneralFlowChannel; safecall;
    function Get_ConsumptiveChannel : IGeneralFlowChannel; safecall;
    function Get_IrrigationNode: IReservoirData; safecall;
    function Get_IrrigationPolicy : integer; safecall;
    procedure Set_IrrigationPolicy (APolicy : integer); safecall;
    function Validate (var AErrors : WideString; const AContext    : WideString='') : WordBool; safecall;
    function GetKeyValues (const AParamField : WideString; const AFieldIndex: WideString) : WideString; safecall;
    property FeatureID      : integer read Get_FeatureID;
    property FeatureName    : WideString read Get_FeatureName write Set_FeatureName;
    property IrrigationNodeNumber: integer read Get_IrrigationNodeNumber;
    property ConsumptiveChannelNumber: integer read Get_ConsumptiveChannelNumber;
    property DiversionChannelNumber: integer read Get_DiversionChannelNumber;
    property ReturnFlowChannelNumber: integer read Get_ReturnFlowChannelNumber;
    property FeatureType    : integer read Get_FeatureType write Set_FeatureType;
    property FeatureSubType : integer read Get_FeatureSubType write Set_FeatureSubType;
    property MonthlyDiversionFlows : TIrrMonthlyDoublesArray read Get_MonthlyDiversionFlows;
    property DiversionFlowByMonth[AIndex : integer] : double read Get_DiversionFlowByMonth write Set_DiversionFlowByMonth;
    property MonthlyReturnFlows : TIrrMonthlyDoublesArray read Get_MonthlyReturnFlows;
    property ReturnFlowByMonth[AIndex : integer] : double read Get_ReturnFlowByMonth write Set_ReturnFlowByMonth;
    property DiversionChannel : IGeneralFlowChannel   read Get_DiversionChannel;
    property ReturnFlowChannel : IGeneralFlowChannel  read Get_ReturnFlowChannel;
    property ConsumptiveChannel : IGeneralFlowChannel read Get_ConsumptiveChannel;
    property IrrigationNode : IReservoirData read Get_IrrigationNode;
    property IrrigationPolicy : integer  read Get_IrrigationPolicy write Set_IrrigationPolicy;
  end;

  TIrrigationAreaList = class(TAbstractAppObject, IIrrigationAreaList)
  protected
    FIrrigationAreaList : TList;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function AddIrrigationArea (AFeature : TIrrigationArea): boolean;
  public
    function Initialise : boolean; override;
    function NewIrrigationArea : TIrrigationArea;
    function DeleteIrrigationAreaWithID(AFeatureID : integer) : WordBool;
    function DeleteIrrigationAreaWithIndex(AIndex : integer) : WordBool;
    function CastIrrigationAreaByIndex (AIndex : integer): TIrrigationArea;
    function CastIrrigationAreaByID(AFeatureID : integer): TIrrigationArea;
    function CastIrrigationAreaByNodeNumber(ANodeNumber : integer): TIrrigationArea;
    function CopyCreate(AFeatureID: Integer): IIrrigationArea; safecall;
    function CreateIrrigationArea : IIrrigationArea; safecall;
    function RemoveIrrigationAreaWithID (AFeatureID : integer) : WordBool; safecall;
    function Get_IrrigationAreaByID(AFeatureID : integer): IIrrigationArea; safecall;
    function Get_IrrigationAreaByIndex(AIndex: integer): IIrrigationArea; safecall;
    function Get_IrrigationAreaByNodeNumber(ANodeNumber: integer): IIrrigationArea; safecall;
    function Get_IrrigationAreaCount : integer; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    procedure GetChannelNumbers(AFeatureID: integer;var ADiversionChannelNumber,
              AConsumptiveChannelNumber,AReturnFlowChannelNumber,AConsumptiveNodeNr: integer);

    property IrrigationAreaByIndex[AIndex : integer]: IIrrigationArea  read Get_IrrigationAreaByIndex;
    property IrrigationAreaByID[AFeatureID: integer]: IIrrigationArea  read Get_IrrigationAreaByID;
    property IrrigationAreaByNodeNumber[ANodeNumber: integer]: IIrrigationArea  read Get_IrrigationAreaByNodeNumber;
    property IrrigationAreaCount: integer read Get_IrrigationAreaCount;
  end;

implementation

uses
  SysUtils,
  Math,
  UConstants,
  UNetworkElementData,
  UNetworkFeaturesData,
  UYieldModelDataObject,
  UNetworkFeaturesSQLAgent,
  UErrorHandlingOperations;

{******************************************************************************}
{ TIrrigationArea                                                              }
{******************************************************************************}

function TIrrigationArea._AddRef: Integer;
const OPNAME = 'TIrrigationArea._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationArea._Release: Integer;
const OPNAME = 'TIrrigationArea._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationArea.CreateMemberObjects;
const OPNAME = 'TIrrigationArea.CreateMemberObjects';
var
  LMonthlyDiversionFlows,
  LMonthlyReturnFlows: TAbstractFieldProperty;
begin
  inherited;
  try
    LMonthlyDiversionFlows := FAppModules.FieldProperties.FieldProperty('DiversionFlow');
    if (LMonthlyDiversionFlows = nil) then
      raise Exception.Create('Field (DiversionFlow) not found in field properties');
    SetLength(FMonthlyDiversionFlows,LMonthlyDiversionFlows.ArrayLength);

    LMonthlyReturnFlows := FAppModules.FieldProperties.FieldProperty('ReturnFlow');
    if (LMonthlyReturnFlows = nil) then
      raise Exception.Create('Field (ReturnFlow) not found in field properties');
    SetLength(FMonthlyReturnFlows,LMonthlyReturnFlows.ArrayLength);

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationArea.DestroyMemberObjects;
const OPNAME = 'TIrrigationArea.DestroyMemberObjects';
begin
  try
    Finalize(FMonthlyDiversionFlows);
    Finalize(FMonthlyReturnFlows);
  except on E: Exception do HandleError(E, OPNAME); end;
  inherited;
end;

function TIrrigationArea.Initialise: boolean;
const OPNAME = 'TIrrigationArea.Initialise';
var
  nIndex : integer;
  LMonthlyReturnFlows,
  LMonthlyDiversionFlows: TAbstractFieldProperty;
begin
  Result := inherited Initialise;
  try
    FIrrigationNodeNumber  := -1;
    FConsumptiveChannelNr  := 0;
    FFeatureID             := -1;
    FFeatureName           := '';
    FFeatureType           := -1;
    FFeatureSubType        := -1;
    LMonthlyReturnFlows    := FAppModules.FieldProperties.FieldProperty('ReturnFlow');
    LMonthlyDiversionFlows := FAppModules.FieldProperties.FieldProperty('DiversionFlow');
    for nIndex := LMonthlyDiversionFlows.ArrayLow to LMonthlyDiversionFlows.ArrayHigh do
      FMonthlyDiversionFlows[nIndex] := NullFloat;
    for nIndex := LMonthlyReturnFlows.ArrayLow to LMonthlyReturnFlows.ArrayHigh do
      FMonthlyReturnFlows[nIndex] := NullFloat;
    FDiversionChannelNr   := 0;
    FReturnFlowChannelNr  := 0;
    FIrrigationPolicy     := -1;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationArea.Get_FeatureID : integer;
const OPNAME = 'TIrrigationArea.Get_FeatureID';
begin
  Result := 0;
  try
    Result := FFeatureID;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationArea.Get_FeatureName : WideString;
const OPNAME = 'TIrrigationArea.Get_FeatureName';
begin
  Result := '';
  try
    Result := FFeatureName;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationArea.Get_FeatureType : integer;
const OPNAME = 'TIrrigationArea.Get_FeatureType';
begin
  Result := 0;
  try
    Result := FFeatureType;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationArea.Get_FeatureSubType : integer;
const OPNAME = 'TIrrigationArea.Get_FeatureSubType';
begin
  Result := 0;
  try
    Result := FFeatureSubType;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationArea.Set_FeatureType (AType : integer);
const OPNAME = 'TIrrigationArea.Set_FeatureType';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LOldValue: integer;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
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

procedure TIrrigationArea.Set_FeatureSubType (ASubType : integer);
const OPNAME = 'TIrrigationArea.Set_FeatureSubType';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LOldValue: integer;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
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

function TIrrigationArea.Populate (AFeatureID             : integer;
                                   AFeatureName           : WideString;
                                   AIrrigationNodeNumber  : integer;
                                   AConsumptiveChannelNr  : integer;
                                   AFeatureType           : integer;
                                   AFeatureSubType        : integer;
                                   AMonthlyDiversionFlows : TIrrMonthlyDoublesArray;
                                   AMonthlyReturnFlows    : TIrrMonthlyDoublesArray;
                                   ADiversionChannelNr    : integer;
                                   AReturnFlowChannelNr   : integer;
                                   AIrrigationPolicy      : integer) : WordBool;
const OPNAME = 'TIrrigationArea.Populate';
var
  lIndex : integer;
  LMonthlyDiversionFlows: TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    FFeatureID             := AFeatureID;
    FFeatureName           := AFeatureName;
    FFeatureType           := AFeatureType;
    FFeatureSubType        := AFeatureSubType;
    FIrrigationNodeNumber  := AIrrigationNodeNumber;
    FConsumptiveChannelNr  := AConsumptiveChannelNr;
    FDiversionChannelNr    := ADiversionChannelNr;
    FReturnFlowChannelNr   := AReturnFlowChannelNr;
    FIrrigationPolicy      := AIrrigationPolicy;
    LMonthlyDiversionFlows := FAppModules.FieldProperties.FieldProperty('DiversionFlow');
    for lIndex := LMonthlyDiversionFlows.ArrayLow to LMonthlyDiversionFlows.ArrayHigh do
    begin
      FMonthlyDiversionFlows[lIndex] := AMonthlyDiversionFlows[lIndex];
      FMonthlyReturnFlows[lIndex]    := AMonthlyReturnFlows[lIndex];
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationArea.Get_MonthlyDiversionFlows : TIrrMonthlyDoublesArray;
const OPNAME = 'TIrrigationArea.Get_MonthlyDiversionFlows';
begin
  Result := FMonthlyDiversionFlows;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationArea.Get_DiversionFlowByMonth (AIndex : integer) : double;
const OPNAME = 'TIrrigationArea.Get_DiversionFlowByMonth';
begin
  Result := 0.0;
  try
    Result := FMonthlyDiversionFlows[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationArea.Get_MonthlyReturnFlows : TIrrMonthlyDoublesArray;
const OPNAME = 'TIrrigationArea.Get_MonthlyReturnFlows';
begin
  Result := FMonthlyReturnFlows;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationArea.Get_ReturnFlowByMonth (AIndex : integer) : double;
const OPNAME = 'TIrrigationArea.Get_ReturnFlowByMonth';
begin
  Result := 0.0;
  try
    Result := FMonthlyReturnFlows[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationArea.Get_DiversionChannel : IGeneralFlowChannel;
const OPNAME = 'TIrrigationArea.Get_DiversionChannel';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                ChannelList.ChannelByChannelNumber[FDiversionChannelNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationArea.Get_ReturnFlowChannel : IGeneralFlowChannel;
const OPNAME = 'TIrrigationArea.Get_ReturnFlowChannel';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                ChannelList.ChannelByChannelNumber[FReturnFlowChannelNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationArea.Get_ConsumptiveChannel : IGeneralFlowChannel;
const OPNAME = 'TIrrigationArea.Get_ConsumptiveChannel';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                ChannelList.ChannelByChannelNumber[FConsumptiveChannelNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationArea.Get_IrrigationPolicy : integer;
const OPNAME = 'TIrrigationArea.Get_IrrigationPolicy';
begin
  Result := 0;
  try
    Result := FIrrigationPolicy;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationArea.Set_FeatureName (const AName : WideString);
const OPNAME = 'TIrrigationArea.Set_FeatureName';
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
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'AreaName', AName, FFeatureName, LContextData) then
        begin
          LOldValue    := FFeatureName;
          FFeatureName := AName;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'AreaName',LOldValue,AName);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationArea.Set_DiversionFlowByMonth (AIndex : integer;
                                                    AValue : double);
const OPNAME = 'TIrrigationArea.Set_DiversionFlowByMonth';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LPrevValue   : double;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureIDFieldNameID
          (LContextData, IntToStr(FFeatureID), IntToStr(AIndex));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'DiversionFlow', FloatToStr(AValue), FloatToStr(FMonthlyDiversionFlows[AIndex]), LContextData) then
        begin
          LPrevValue := FMonthlyDiversionFlows[AIndex];
          FMonthlyDiversionFlows[AIndex] := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'DiversionFlow',FloatToStr(LPrevValue),FloatToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationArea.Set_ReturnFlowByMonth (AIndex : integer;
                                                 AValue : double);
const OPNAME = 'TIrrigationArea.Set_ReturnFlowByMonth';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LPrevValue   : double;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureIDFieldNameID
          (LContextData, IntToStr(FFeatureID), IntToStr(AIndex));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'ReturnFlow', FloatToStr(AValue), FloatToStr(FMonthlyReturnFlows[AIndex]), LContextData) then
        begin
          LPrevValue := FMonthlyReturnFlows[AIndex];
          FMonthlyReturnFlows[AIndex] := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ReturnFlow',FloatToStr(LPrevValue),FloatToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationArea.Set_IrrigationPolicy (APolicy : integer);
const OPNAME = 'TIrrigationArea.Set_IrrigationPolicy';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LPrevValue   : integer;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'RelaxationDemand', FloatToStr(APolicy), FloatToStr(FIrrigationPolicy), LContextData) then
        begin
          LPrevValue := FIrrigationPolicy;
          FIrrigationPolicy := APolicy;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'RelaxationDemand',IntToStr(LPrevValue),IntToStr(APolicy));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationArea.ValidateIrrigationAreaName(AErrorMessages : TStrings): WordBool;
const OPNAME = 'TIrrigationArea.ValidateIrrigationAreaName';
var
  lIndex       : integer;
  lIrrAreaList : TIrrigationAreaList;
  lIrrArea     : TIrrigationArea;
  lMessage     : string;
  lUnique      : Boolean;
begin
  Result := True;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('AreaName', FFeatureName, lMessage)) then
      AErrorMessages.Add('WARNING:' +FFeatureName+ ':'+lMessage)
    else
    begin
      lIrrAreaList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        CastNetworkFeaturesData.CastIrrigationAreaList;
      lUnique := TRUE;
      lIndex  := 0;
      while (lUnique AND (lIndex < lIrrAreaList.IrrigationAreaCount)) do
      begin
        lIrrArea := lIrrAreaList.CastIrrigationAreaByIndex(lIndex);
        if ((FFeatureID <> lIrrArea.FeatureID) AND
            (UpperCase(Trim(FFeatureName)) = UpperCase(Trim(lIrrArea.FeatureName)))) then
        begin
          lMessage := FAppModules.Language.GetString('ContextValidation.DuplicateIrrigationAreaName');
          AErrorMessages.Add('WARNING:' +Format(lMessage, [FFeatureName]));
          lUnique := FALSE;
        end
        else
          lIndex := lIndex + 1;
      end;
      //Result := lUnique;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationArea.ValidateRelaxationPolicy(AErrorMessages : TStrings): WordBool;
const OPNAME = 'TIrrigationArea.ValidateRelaxationPolicy';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    Result   := FAppModules.FieldProperties.ValidateFieldProperty
                  ('RelaxationDemand', IntToStr(FIrrigationPolicy), lMessage);
    if (NOT Result) then
      AErrorMessages.Add('ERROR:' +FFeatureName+ ':'+lMessage);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationArea.ValidateDiversionUpstreamNode(AErrorMessages : TStrings): WordBool;
const OPNAME = 'TIrrigationArea.ValidateDiversionUpstreamNode';
var
  lMessage          : WideString;
  lDiversionChannel : IGeneralFlowChannel;
begin
  Result := True;
  try
    if (FDiversionChannelNr <> 0) then
    begin
      lDiversionChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                             ChannelList.ChannelByChannelNumber[FDiversionChannelNr];
      if (lDiversionChannel.UpStreamNodeNumber = 0) then
      begin
        lMessage := FAppModules.Language.GetString('ContextValidation.InvalidUpstreamNodeNumber');
        AErrorMessages.Add('ERROR:' +Format(lMessage, [FFeatureName]));
        Result := False;
      end
      else
      begin
        if (NOT lDiversionChannel.Validate(lMessage,'UpNodeNumber')) then
        begin
          Result := False;
          AErrorMessages.Add('ERROR:' +FFeatureName+ ':'+lMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationArea.ValidateReturnFlowDownstreamNode(AErrorMessages : TStrings): WordBool;
const OPNAME = 'TIrrigationArea.ValidateReturnFlowDownstreamNode';
var
  lMessage           : WideString;
  lReturnFlowChannel : IGeneralFlowChannel;
begin
  Result := True;
  try
    if (FReturnFlowChannelNr <> 0) then
    begin
      lReturnFlowChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                              ChannelList.ChannelByChannelNumber[FReturnFlowChannelNr];
      if (NOT lReturnFlowChannel.Validate(lMessage,'DownNodeNumber')) then
      begin
        Result := False;
        AErrorMessages.Add('ERROR:' +FFeatureName+ ':'+lMessage);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationArea.ValidateDiversionFlows(AErrorMessages : TStrings; AErrorColumns: TStrings): WordBool;
const OPNAME = 'TIrrigationArea.ValidateDiversionFlows';
var
  lIndex            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lValid            : Boolean;
  lResult           : Boolean;
  LMonthlyDiversionFlows: TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    LMonthlyDiversionFlows := FAppModules.FieldProperties.FieldProperty('DiversionFlow');
    for lIndex := LMonthlyDiversionFlows.ArrayLow to LMonthlyDiversionFlows.ArrayHigh do
    begin
      lMessage := '';
      lValid := FAppModules.FieldProperties.ValidateFieldProperty
                  ('DiversionFlow', FloatToStr(FMonthlyDiversionFlows[lIndex]),
                  lMessage, lIndex);
      if (NOT lValid) then
      begin
        lResult := FALSE;
        AErrorMessages.Add('ERROR:' +FFeatureName+ ':'+lMessage);
        AErrorColumns.Add(IntToStr(lIndex));
        if (lStopOnFirstError) then
          Break;
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationArea.ValidateReturnFlows(AErrorMessages : TStrings; AErrorColumns: TStrings): WordBool;
const OPNAME = 'TIrrigationArea.ValidateReturnFlows';
var
  lIndex            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lValid            : Boolean;
  lResult           : Boolean;
  LMonthlyDiversionFlows: TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    LMonthlyDiversionFlows := FAppModules.FieldProperties.FieldProperty('DiversionFlow');
    for lIndex := LMonthlyDiversionFlows.ArrayLow to LMonthlyDiversionFlows.ArrayHigh do
    begin
      lMessage := '';
      lValid := FAppModules.FieldProperties.ValidateFieldProperty
                  ('ReturnFlow', FloatToStr(FMonthlyReturnFlows[lIndex]),
                  lMessage, lIndex);
      if (NOT lValid) then
      begin
        lResult := FALSE;
        AErrorMessages.Add('ERROR:' +FFeatureName+ ':'+lMessage);
        AErrorColumns.Add(IntToStr(lIndex));
        if (lStopOnFirstError) then
          Break;
      end;
      if (lResult OR (NOT lStopOnFirstError)) then
      begin
        if (FMonthlyDiversionFlows[lIndex] < FMonthlyReturnFlows[lIndex]) then
        begin
          lResult := FALSE;
          lMessage := FAppModules.Language.GetString('ContextValidation.DivertedGreaterThanReturnFlows');
          AErrorMessages.Add('ERROR:' +Format(lMessage, [FFeatureName]));
          if (lStopOnFirstError) then
            Break;
        end;
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationArea.Validate (var AErrors : WideString; const AContext    : WideString='') : WordBool;
const OPNAME = 'TIrrigationArea.Validate';
var
  lStopOnFirstError : Boolean;
  lErrorList        : TStringList;
  lErrorCols        : TStringList;
begin
  Result := FALSE;
  try
    lErrorList := TStringList.Create;
    lErrorCols := TStringList.Create;
    try
      if (AContext = 'AreaName') then
        Result := ValidateIrrigationAreaName(lErrorList)
      else
      if (AContext = 'RelaxationDemand') then
        Result := ValidateRelaxationPolicy(lErrorList)
      else
      if (AContext = 'DiversionFlow') then
      begin
        Result := ValidateDiversionFlows(lErrorList,lErrorCols);
        if (NOT Result) then
        begin
          if (lErrorCols.Count = 0) then
            AErrors := AErrors + lErrorList.Text
          else
            AErrors := AErrors + CTStringsSeparator + lErrorList.Text +
                                 CTStringsSeparator + lErrorCols.Text+ CTStringsSeparator;
          lErrorList.Clear;
          lErrorCols.Clear;
        end;
      end
      else
      if (AContext = 'ReturnFlow') then
      begin
        Result := ValidateReturnFlows(lErrorList,lErrorCols);
        if (NOT Result) then
        begin
          if (lErrorCols.Count = 0) then
            AErrors := AErrors + lErrorList.Text
          else
            AErrors := AErrors + CTStringsSeparator + lErrorList.Text +
                                 CTStringsSeparator + lErrorCols.Text+ CTStringsSeparator;
          lErrorList.Clear;
          lErrorCols.Clear;
        end;
      end
      else
      if (AContext = 'DiversionUpstreamNode') then
        Result := ValidateDiversionUpstreamNode(lErrorList)
      else
      if (AContext = 'ReturnFlowDownstreamNode') then
        Result := ValidateReturnFlowDownstreamNode(lErrorList)
      else
      begin
        Result := TRUE;
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        if (NOT ValidateIrrigationAreaName(lErrorList)) then
          Result := FALSE;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateRelaxationPolicy(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateDiversionFlows(lErrorList,lErrorCols)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateReturnFlows(lErrorList,lErrorCols)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateDiversionUpstreamNode(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateReturnFlowDownstreamNode(lErrorList)) then
            Result := FALSE;
        end;
      end;
      AErrors := AErrors + lErrorList.Text;
    finally
      FreeAndNil(lErrorList);
      FreeAndNil(lErrorCols);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationArea.GetKeyValues (const AParamField : WideString;
                                       const AFieldIndex : WideString) : WideString;
const OPNAME = 'TIrrigationArea.GetKeyValues';
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

{******************************************************************************}
{ TIrrigationAreaList                                                        }
{******************************************************************************}

function TIrrigationAreaList._AddRef: Integer;
const OPNAME = 'TIrrigationAreaList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationAreaList._Release: Integer;
const OPNAME = 'TIrrigationAreaList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationAreaList.CreateMemberObjects;
const OPNAME = 'TIrrigationAreaList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FIrrigationAreaList := TList.Create;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationAreaList.DestroyMemberObjects;
const OPNAME = 'TIrrigationAreaList.DestroyMemberObjects';
begin
  try
    while (FIrrigationAreaList.Count > 0) do
      DeleteIrrigationAreaWithIndex(0);
    FreeAndNil(FIrrigationAreaList);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationAreaList.Initialise: boolean;
const OPNAME = 'TIrrigationAreaList.Initialise';
begin
  Result := inherited Initialise;
  try
    FIrrigationAreaList.Clear;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationAreaList.AddIrrigationArea (AFeature : TIrrigationArea): boolean;
const OPNAME = 'TIrrigationAreaList.AddIrrigationArea';
begin
  Result := False;
  try
    if (AFeature <> nil) then
    begin
      FIrrigationAreaList.Add(AFeature);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationAreaList.NewIrrigationArea : TIrrigationArea;
const OPNAME = 'TIrrigationAreaList.NewIrrigationArea';
var
  lFeature : TIrrigationArea;
begin
  Result := nil;
  try
    lFeature := TIrrigationArea.Create(FAppModules);
    AddIrrigationArea(lFeature);
    Result := lFeature;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationAreaList.CreateIrrigationArea : IIrrigationArea;
const OPNAME = 'TIrrigationAreaList.CreateIrrigationArea';
var
  LFeatureID                  : integer;
  LLoadAgent                  : TNetworkFeaturesSQLAgent;
  LFeature                    : TIrrigationArea;
  lMonthlyDiversionFlows      : TIrrMonthlyDoublesArray;
  lMonthlyReturnFlows         : TIrrMonthlyDoublesArray;
  LMonthlyDiversionFlowsField : TAbstractFieldProperty;
  LMonthlyReturnFlowsField    : TAbstractFieldProperty;
  lMonth                      : integer;
  lIrrigationNode             : IReservoirData;
  lDiversionChannel           : IGeneralFlowChannel;
  lConsumptiveChannel         : IGeneralFlowChannel;
  lReturnFlowChannel          : IGeneralFlowChannel;
  lNetworkElementData         : TNetworkElementData;
  lIrrigationNodeNumber       : integer;
  lDestroy                    : Boolean;
begin
  Result := nil;
  try
    LMonthlyDiversionFlowsField := FAppModules.FieldProperties.FieldProperty('DiversionFlow');
    if (LMonthlyDiversionFlowsField = nil) then
      raise Exception.Create('Field (DiversionFlow) not found in field properties');
    SetLength(lMonthlyDiversionFlows,LMonthlyDiversionFlowsField.ArrayLength);

    LMonthlyReturnFlowsField := FAppModules.FieldProperties.FieldProperty('ReturnFlow');
    if (LMonthlyReturnFlowsField = nil) then
      raise Exception.Create('Field (ReturnFlow) not found in field properties');
    SetLength(lMonthlyReturnFlows,LMonthlyReturnFlowsField.ArrayLength);

    for lMonth := LMonthlyDiversionFlowsField.ArrayLow to LMonthlyDiversionFlowsField.ArrayHigh do
      lMonthlyDiversionFlows[lMonth] := 0;
    for lMonth := LMonthlyReturnFlowsField.ArrayLow to LMonthlyReturnFlowsField.ArrayHigh do
      lMonthlyReturnFlows[lMonth]    := 0;

    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      lNetworkElementData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData;
      lIrrigationNode     := lNetworkElementData.CastReservoirList.CreateNewIrrigationNode;
      lDiversionChannel   := lNetworkElementData.CastChannelList.CreateNewChannel;
      lConsumptiveChannel := lNetworkElementData.CastChannelList.CreateNewChannel;
      lReturnFlowChannel  := lNetworkElementData.CastChannelList.CreateNewChannel;

      LFeatureID := 0;
      lDestroy   := TRUE;
      if ((lIrrigationNode <> nil) AND
          (lDiversionChannel <> nil) AND
          (lConsumptiveChannel <> nil) AND
          (lReturnFlowChannel <> nil)) then
      begin
        lIrrigationNodeNumber := lIrrigationNode.ReservoirConfigurationData.ReservoirIdentifier;
        lDiversionChannel.DownStreamNodeNumber := lIrrigationNodeNumber;
        lDiversionChannel.ChannelType          := 4;
        lDiversionChannel.ChannelSubType       := 1;
        lConsumptiveChannel.UpStreamNodeNumber := lIrrigationNodeNumber;
        lConsumptiveChannel.ChannelType        := 4;
        lConsumptiveChannel.ChannelSubType     := 2;
        lReturnFlowChannel.UpStreamNodeNumber  := lIrrigationNodeNumber;
        lReturnFlowChannel.ChannelType         := 4;
        lReturnFlowChannel.ChannelSubType      := 3;
        if (LLoadAgent.InsertIrrigationArea(LFeatureID, lIrrigationNodeNumber,
                                            lDiversionChannel.ChannelNumber,
                                            lConsumptiveChannel.ChannelNumber,
                                            lReturnFlowChannel.ChannelNumber)) then
        begin
          lDestroy := FALSE;
          LFeature := NewIrrigationArea;
          LFeature.Initialise;
          LFeature.Populate(
            LFeatureID,
            UpperCase(FAppModules.Language.GetString('NetworkFeatures.IrrigationArea')) + ' ' + IntToStr(LFeatureID),
            lIrrigationNodeNumber,
            lConsumptiveChannel.ChannelNumber, 8, 0,
            lMonthlyDiversionFlows, lMonthlyReturnFlows,
            lDiversionChannel.ChannelNumber, lReturnFlowChannel.ChannelNumber, 0);
          Result := LFeature;
        end;
      end;
      if (lDestroy) then
      begin
        if (lIrrigationNode <> nil) then
          lNetworkElementData.CastReservoirList.DeleteIrrigationNode(lIrrigationNode.ReservoirConfigurationData.ReservoirIdentifier);
        if (lDiversionChannel <> nil) then
          lNetworkElementData.ChannelList.RemoveChannelWithID(lDiversionChannel.ChannelID);
        if (lConsumptiveChannel <> nil) then
          lNetworkElementData.ChannelList.RemoveChannelWithID(lConsumptiveChannel.ChannelID);
        if (lReturnFlowChannel <> nil) then
          lNetworkElementData.ChannelList.RemoveChannelWithID(lReturnFlowChannel.ChannelID);
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationAreaList.RemoveIrrigationAreaWithID (AFeatureID : integer) : WordBool;
const OPNAME = 'TIrrigationAreaList.RemoveIrrigationAreaWithID';
var
  lLoadAgent : TNetworkFeaturesSQLAgent;
begin
  Result := FALSE;
  try
    if (AFeatureID > 0) then
    begin
      LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
      try
        if LLoadAgent.DeleteIrrigationArea(AFeatureID) then
        begin
          DeleteIrrigationAreaWithID(AFeatureID);
          Result := TRUE;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;  
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationAreaList.DeleteIrrigationAreaWithID(AFeatureID : integer) : WordBool;
const OPNAME = 'TIrrigationAreaList.DeleteIrrigationAreaWithID';
var
  lFeature : TIrrigationArea;
  lIndex   : integer;
begin
  Result := FALSE;
  try
    lFeature := CastIrrigationAreaByID(AFeatureID);
    if (lFeature <> nil) then
    begin
      lIndex := FIrrigationAreaList.IndexOf(lFeature);
      Result := DeleteIrrigationAreaWithIndex(lIndex);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationAreaList.DeleteIrrigationAreaWithIndex(AIndex : integer) : WordBool;
const OPNAME = 'TIrrigationAreaList.DeleteIrrigationAreaWithIndex';
var
  lFeature : TIrrigationArea;
begin
  Result := FALSE;
  try
    if (AIndex >= 0) then
    begin
      lFeature := TIrrigationArea(FIrrigationAreaList.Items[AIndex]);
      FIrrigationAreaList.Delete(AIndex);
      FreeAndNil(lFeature);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationAreaList.Get_IrrigationAreaByIndex(AIndex: integer): IIrrigationArea;
const OPNAME = 'TIrrigationAreaList.Get_IrrigationAreaByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FIrrigationAreaList.Count) then
      Result := TIrrigationArea(FIrrigationAreaList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationAreaList.Get_IrrigationAreaByID(AFeatureID : integer): IIrrigationArea;
const OPNAME = 'TIrrigationAreaList.Get_IrrigationAreaByID';
var
 lIndex   : integer;
 lFeature : TIrrigationArea;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FIrrigationAreaList.Count)) do
    begin
      lFeature := TIrrigationArea(FIrrigationAreaList.Items[lIndex]);
      if (lFeature.FeatureID = AFeatureID) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationAreaList.CastIrrigationAreaByIndex (AIndex: integer): TIrrigationArea;
const OPNAME = 'TIrrigationAreaList.CastIrrigationAreaByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FIrrigationAreaList.Count) then
      Result := TIrrigationArea(FIrrigationAreaList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationAreaList.CastIrrigationAreaByID(AFeatureID : integer): TIrrigationArea;
const OPNAME = 'TIrrigationAreaList.CastIrrigationAreaByID';
var
 lIndex   : integer;
 lFeature : TIrrigationArea;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FIrrigationAreaList.Count)) do
    begin
      lFeature := TIrrigationArea(FIrrigationAreaList.Items[lIndex]);
      if (lFeature.FeatureID = AFeatureID) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationAreaList.CastIrrigationAreaByNodeNumber(ANodeNumber : integer): TIrrigationArea;
const OPNAME = 'TIrrigationAreaList.CastIrrigationAreaByNodeNumber';
var
 lIndex   : integer;
 lFeature : TIrrigationArea;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FIrrigationAreaList.Count)) do
    begin
      lFeature := TIrrigationArea(FIrrigationAreaList.Items[lIndex]);
      if (lFeature.FIrrigationNodeNumber = ANodeNumber) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationAreaList.Get_IrrigationAreaCount: integer;
const OPNAME = 'TIrrigationAreaList.Get_IrrigationAreaCount';
begin
  Result := 0;
  try
    Result := FIrrigationAreaList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationAreaList.Validate(var AErrors: WideString;const AContext: WideString): WordBool;
const OPNAME = 'TIrrigationAreaList.Validate';
var
  lStopOnFirstError : Boolean;
  LIndex            : integer;
begin
  Result := FALSE;
  try
    Result := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    for LIndex := 0 to FIrrigationAreaList.Count - 1 do
    begin
      if (NOT IrrigationAreaByIndex[LIndex].Validate(AErrors,AContext)) then
        Result := FALSE;
      if ((NOT Result) AND lStopOnFirstError) then
        Break;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationAreaList.GetChannelNumbers(AFeatureID: integer;
  var ADiversionChannelNumber, AConsumptiveChannelNumber, AReturnFlowChannelNumber,AConsumptiveNodeNr: integer);
const OPNAME = 'TIrrigationAreaList.GetChannelNumbers';
var
 lIrrigationArea : TIrrigationArea;
begin
  try
    ADiversionChannelNumber   := NullInteger;
    AConsumptiveChannelNumber := NullInteger;
    AReturnFlowChannelNumber  := NullInteger;
    AConsumptiveNodeNr        := NullInteger;
    lIrrigationArea := CastIrrigationAreaByID(AFeatureID);
    if (lIrrigationArea <> nil) then
    begin
      ADiversionChannelNumber   := lIrrigationArea.DiversionChannel.ChannelNumber;
      AConsumptiveChannelNumber := lIrrigationArea.ConsumptiveChannel.ChannelNumber;
      AReturnFlowChannelNumber  := lIrrigationArea.ReturnFlowChannel.ChannelNumber;
      AConsumptiveNodeNr        := lIrrigationArea.ConsumptiveChannel.UpStreamNodeNumber;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationArea.Get_IrrigationNode: IReservoirData;
const OPNAME = 'TIrrigationArea.Get_IrrigationNode';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[FIrrigationNodeNumber];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationAreaList.Get_IrrigationAreaByNodeNumber(ANodeNumber: integer): IIrrigationArea;
const OPNAME = 'TIrrigationAreaList.Get_IrrigationAreaByNodeNumber';
var
 lIndex   : integer;
 lFeature : TIrrigationArea;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FIrrigationAreaList.Count)) do
    begin
      lFeature := TIrrigationArea(FIrrigationAreaList.Items[lIndex]);
      if (lFeature.FIrrigationNodeNumber = ANodeNumber) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationArea.Get_ConsumptiveChannelNumber: integer;
const OPNAME = 'TIrrigationArea.Get_ConsumptiveChannelNumber';
begin
  Result := 0;
  try
    Result := FConsumptiveChannelNr;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationArea.Get_DiversionChannelNumber: integer;
const OPNAME = 'TIrrigationArea.Get_DiversionChannelNumber';
begin
  Result := 0;
  try
    Result := FDiversionChannelNr;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationArea.Get_IrrigationNodeNumber: integer;
const OPNAME = 'TIrrigationArea.Get_IrrigationNodeNumber';
begin
  Result := 0;
  try
    Result := FIrrigationNodeNumber;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationArea.Get_ReturnFlowChannelNumber: integer;
const OPNAME = 'TIrrigationArea.Get_ReturnFlowChannelNumber';
begin
  Result := 0;
  try
    Result := FReturnFlowChannelNr;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationAreaList.CopyCreate(AFeatureID: Integer): IIrrigationArea;
const OPNAME = 'TIrrigationAreaList.CopyCreate';
var
  LSourceIrrigationArea : TIrrigationArea;
  LDestIrrigationArea : IIrrigationArea;
  LIrrigationAreaCopy : TIrrigationArea;

begin
  try
    LSourceIrrigationArea := CastIrrigationAreaByNodeNumber(AFeatureID);
    if LSourceIrrigationArea <> nil then
    begin
      LDestIrrigationArea := CreateIrrigationArea;
      if LDestIrrigationArea <> nil then
      begin
        LIrrigationAreaCopy := CastIrrigationAreaByID(LDestIrrigationArea.FeatureID);
        if LIrrigationAreaCopy <> nil then
        begin
          LIrrigationAreaCopy.Assign(LSourceIrrigationArea);
          Result := LIrrigationAreaCopy;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIrrigationArea.Assign(AIrrigationArea: TIrrigationArea);
const OPNAME = 'TIrrigationArea.Assign';
var
  LIndex : integer;
  LDestDiversionChannel : IGeneralFlowChannel;
  LSourceDiversionChannel : IGeneralFlowChannel;
  LDestReturnFlowChannel : IGeneralFlowChannel;
  LSourceReturnFlowChannel : IGeneralFlowChannel;
  LDestConsumptiveChannel : IGeneralFlowChannel;
  LSourceConsumptiveChannel : IGeneralFlowChannel;
  LChannelList : IChannelList;
  LChnnelPenalty : IChannelPenalty;
  LDestIrrigationNode : TReservoirData;
  LSourceIrrigationNode : TReservoirData;
begin
  try
    FeatureName := 'Copy of '+AIrrigationArea.FeatureName;
    FeatureType    := AIrrigationArea.FeatureType;
    FeatureSubType := AIrrigationArea.FeatureSubType;
    IrrigationPolicy := AIrrigationArea.IrrigationPolicy;
    for LIndex := 1 to 12 do
    begin
      if AIrrigationArea.DiversionFlowByMonth[LIndex] <> NullFloat then
        DiversionFlowByMonth[LIndex] :=  AIrrigationArea.DiversionFlowByMonth[LIndex];
    end;
    for LIndex := 1 to 12 do
    begin
      if AIrrigationArea.ReturnFlowByMonth[LIndex] <> NullFloat then
        ReturnFlowByMonth[LIndex] :=  AIrrigationArea.ReturnFlowByMonth[LIndex];
    end;
    LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList;
    if LChannelList <> nil then
    begin
      LDestDiversionChannel := LChannelList.ChannelByChannelNumber[FDiversionChannelNr];
      LSourceDiversionChannel := LChannelList.ChannelByChannelNumber[AIrrigationArea.DiversionChannelNumber];
      if (LDestDiversionChannel <> nil) and (LSourceDiversionChannel <> nil) then
      begin
        LDestDiversionChannel.ChannelName := 'Copy of '+ LSourceDiversionChannel.ChannelName;
        LDestDiversionChannel.ChannelType := LSourceDiversionChannel.ChannelType;
        LDestDiversionChannel.ChannelSubType := LSourceDiversionChannel.ChannelSubType;
        LDestDiversionChannel.UpStreamNodeNumber := LSourceDiversionChannel.UpStreamNodeNumber;
        LDestDiversionChannel.DownStreamNodeNumber := FIrrigationNodeNumber;
        LDestDiversionChannel.SummaryOutputRequired := LSourceDiversionChannel.SummaryOutputRequired;
        LDestDiversionChannel.ChannelArea := LSourceDiversionChannel.ChannelArea;
        LChnnelPenalty := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelPenaltyList.
                            ChannelPenaltyByIdentifier[LSourceDiversionChannel.ChannelPenaltyNumber];
        if LChnnelPenalty <> nil then
          LDestDiversionChannel.ChannelPenalty := LChnnelPenalty;
      end;
      LDestReturnFlowChannel := LChannelList.ChannelByChannelNumber[FReturnFlowChannelNr];
      LSourceReturnFlowChannel := LChannelList.ChannelByChannelNumber[AIrrigationArea.ReturnFlowChannelNumber];
      if (LDestReturnFlowChannel <> nil) and (LSourceReturnFlowChannel <> nil) then
      begin
        LDestReturnFlowChannel.ChannelName := 'Copy of '+ LSourceReturnFlowChannel.ChannelName;
        LDestReturnFlowChannel.ChannelType := LSourceReturnFlowChannel.ChannelType;
        LDestReturnFlowChannel.ChannelSubType := LSourceReturnFlowChannel.ChannelSubType;
        LDestReturnFlowChannel.UpStreamNodeNumber := FIrrigationNodeNumber;
        LDestReturnFlowChannel.DownStreamNodeNumber := LSourceReturnFlowChannel.DownStreamNodeNumber;
        LDestReturnFlowChannel.SummaryOutputRequired := LSourceReturnFlowChannel.SummaryOutputRequired;
        LDestReturnFlowChannel.ChannelArea := LSourceReturnFlowChannel.ChannelArea;
        LChnnelPenalty := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelPenaltyList.
                            ChannelPenaltyByIdentifier[LSourceReturnFlowChannel.ChannelPenaltyNumber];
        if LChnnelPenalty <> nil then
          LDestDiversionChannel.ChannelPenalty := LChnnelPenalty;
      end;
      LDestConsumptiveChannel := LChannelList.ChannelByChannelNumber[FConsumptiveChannelNr];
      LSourceConsumptiveChannel := LChannelList.ChannelByChannelNumber[AIrrigationArea.ConsumptiveChannelNumber];
      if (LDestConsumptiveChannel <> nil) and (LSourceReturnFlowChannel <> nil) then
      begin
        LDestConsumptiveChannel.ChannelName := 'Copy of '+ LSourceConsumptiveChannel.ChannelName;
        LDestConsumptiveChannel.ChannelType := LSourceConsumptiveChannel.ChannelType;
        LDestConsumptiveChannel.ChannelSubType := LSourceConsumptiveChannel.ChannelSubType;
        LDestConsumptiveChannel.UpStreamNodeNumber := FIrrigationNodeNumber;
        LDestConsumptiveChannel.DownStreamNodeNumber := LSourceConsumptiveChannel.DownStreamNodeNumber;
        LDestConsumptiveChannel.SummaryOutputRequired := LSourceConsumptiveChannel.SummaryOutputRequired;
        LDestConsumptiveChannel.ChannelArea := LSourceConsumptiveChannel.ChannelArea;
        LChnnelPenalty := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelPenaltyList.
                            ChannelPenaltyByIdentifier[LSourceConsumptiveChannel.ChannelPenaltyNumber];
        if LChnnelPenalty <> nil then
          LDestConsumptiveChannel.ChannelPenalty := LChnnelPenalty;
        LDestConsumptiveChannel.IrrigationArea := Self;
      end;
    end;
    LSourceIrrigationNode := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                             CastReservoirList.CastReservoirOrNodeByIdentifier[AIrrigationArea.IrrigationNodeNumber];
    LDestIrrigationNode := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                             CastReservoirList.CastReservoirOrNodeByIdentifier[FIrrigationNodeNumber];
    if (LDestIrrigationNode <> nil) and (LSourceIrrigationNode <> nil) then
      LDestIrrigationNode.Assign(LSourceIrrigationNode);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
