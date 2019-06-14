{******************************************************************************}
{*  UNIT      : Contains the class TMinMaxFlowConstraint.                     *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2004/03/29                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UMinMaxFlowConstraints;

interface

uses
  Classes,
  Contnrs,
  UAbstractObject,
  VoaimsCom_TLB;

type

{******************************************************************************}
{* MinMax Flow Constraints                                                    *}
{******************************************************************************}

  TMinMaxFlowConstraint = class(TAbstractAppObject, IMinMaxFlowConstraint)
  protected
    FFeatureID                 : integer;
    FFeatureName               : string;
    FChannelNr                 : integer;
    FFeatureType               : integer;
    FFeatureSubType            : integer;
    FMonthlyFlowConstraints    : TMinMaxMonthlyDoublesArray;
    FMonthlyDistribution       : TMinMaxMonthlyDoublesArray;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function ValidateMinMaxFeatureName (AErrorMessages : TStrings) : WordBool;
    function ValidateMinMaxFlowConstraint (AErrorMessages : TStrings;
                                           AErrorColumns  : TStringList) : WordBool;
    function ValidateMinMaxDistributionFactors (AErrorMessages : TStrings;
                                           AErrorColumns  : TStringList) : WordBool;

  public
    procedure Assign(ANewChannelNumber : integer;AMinMaxFlowConstraint: TMinMaxFlowConstraint);
    function Initialise : boolean; override;
    function Populate (AFeatureID       : integer;
                       AFeatureName     : WideString;
                       AChannelNr       : integer;
                       AFeatureType     : integer;
                       AFeatureSubType  : integer;
                       AFlowConstraints : TMinMaxMonthlyDoublesArray): WordBool;
    function PopulateDistributionFactors(ADistributionByArcMonth : TMinMaxMonthlyDoublesArray): WordBool;
    function CreateFlowConstraints : WordBool; safecall;
    function RemoveFlowConstraints : WordBool; safecall;
    function Get_FeatureID : integer; safecall;
    function Get_FeatureName : WideString; safecall;
    procedure Set_FeatureName (const AName : WideString); safecall;
    function Get_Channel : IGeneralFlowChannel; safecall;
    procedure Set_Channel (const AChannel : IGeneralFlowChannel); safecall;
    function Get_FeatureType : integer; safecall;
    procedure Set_FeatureType (AType : integer); safecall;
    function Get_FeatureSubType : integer; safecall;
    procedure Set_FeatureSubType (ASubType : integer); safecall;
    function Get_FlowConstraintByArcMonth (AArc : integer; AMonth : integer): double; safecall;
    procedure Set_FlowConstraintByArcMonth(AArc: Integer; AMonth: Integer; Value: Double); safecall;
    function Get_FlowConstraintCount: integer; safecall;
    function Validate (var AErrors : WideString;const AContext: WideString='') : WordBool; safecall;
    function GetBaseValue(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function GetKeyValues (const AParamField : WideString; const AFieldIndex : WideString) : WideString; safecall;
    function Get_DistributionByArcMonth(AArc: Integer; AMonth: Integer): Double; safecall;
    procedure Set_DistributionByArcMonth(AArc: Integer; AMonth: Integer; Value: Double); safecall;
    procedure CalculateDistributionFactors; safecall;
    property FeatureID      : integer read Get_FeatureID;
    property FeatureName    : WideString read Get_FeatureName write Set_FeatureName;
    property Channel        : IGeneralFlowChannel read Get_Channel write Set_Channel;
    property FeatureType    : integer read Get_FeatureType write Set_FeatureType;
    property FeatureSubType : integer read Get_FeatureSubType write Set_FeatureSubType;
    property FlowConstraintCount : integer read Get_FlowConstraintCount;
    property FlowConstraintByArcMonth[AArc : integer; AMonth : integer] : double
      read Get_FlowConstraintByArcMonth write Set_FlowConstraintByArcMonth;
    property DistributionByArcMonth[AArc: Integer; AMonth: Integer]: Double read Get_DistributionByArcMonth write
      Set_DistributionByArcMonth;
      
  end;

  TMinMaxFlowConstraintList = class(TAbstractAppObject, IMinMaxFlowConstraintList)
  protected
    FMinMaxFlowConstraintList : TList;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function AddMinMaxFlowConstraint (AFeature : TMinMaxFlowConstraint): boolean;
  public
    function Initialise : boolean; override;
    function NewMinMaxFlowConstraint : TMinMaxFlowConstraint;
    function CreateNewMinMaxFlowConstraint : TMinMaxFlowConstraint;
    function DeleteMinMaxFlowConstraintWithID(AFeatureID : integer) : WordBool;
    function DeleteMinMaxFlowConstraintWithIndex(AIndex : integer) : WordBool;
    function CastMinMaxFlowConstraintByIndex(AIndex : integer): TMinMaxFlowConstraint;
    function CastMinMaxFlowConstraintByID(AFeatureID : integer): TMinMaxFlowConstraint;
    function CastMinMaxFlowConstraintByChannelNr(AChannelNr : integer): TMinMaxFlowConstraint;

    function CreateMinMaxFlowConstraint : IMinMaxFlowConstraint; safecall;
    function CopyMinMaxFlowConstraint(ANewChannelNumber : integer;AOldChannelNumber: Integer) : IMinMaxFlowConstraint; safecall;
    function RemoveMinMaxFlowConstraintWithID (AFeatureID : integer) : WordBool; safecall;
    function Get_MinMaxFlowConstraintByIndex(AIndex: integer): IMinMaxFlowConstraint; safecall;
    function Get_MinMaxFlowConstraintByID(AFeatureID : integer): IMinMaxFlowConstraint; safecall;
    function Get_MinMaxFlowConstraintCount : integer; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;

    property MinMaxFlowConstraintByIndex[AIndex : integer]: IMinMaxFlowConstraint
      read Get_MinMaxFlowConstraintByIndex;
    property MinMaxFlowConstraintByID[AFeatureID: integer]: IMinMaxFlowConstraint
      read Get_MinMaxFlowConstraintByID;
    property MinMaxFlowConstraintCount: integer read Get_MinMaxFlowConstraintCount;
  end;

implementation

uses
  SysUtils,
  Math,
  UConstants,
  UYieldModelDataObject,
  UChannelDataSQLAgent,
  UErrorHandlingOperations;

{******************************************************************************}
{ TMinMaxFlowConstraint                                                       }
{******************************************************************************}

function TMinMaxFlowConstraint._AddRef: Integer;
const OPNAME = 'TMinMaxFlowConstraint._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxFlowConstraint._Release: Integer;
const OPNAME = 'TMinMaxFlowConstraint._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMinMaxFlowConstraint.CreateMemberObjects;
const OPNAME = 'TMinMaxFlowConstraint.CreateMemberObjects';
var
  LMinMaxFlowConstraintsField: TAbstractFieldProperty;
begin
  try
    inherited CreateMemberObjects;
    LMinMaxFlowConstraintsField := FAppModules.FieldProperties.FieldProperty('FlowConstraints');
    if not Assigned(LMinMaxFlowConstraintsField) then
      raise Exception.Create('Field (FlowConstraints) not found in field properties');
    SetLength(FMonthlyFlowConstraints, LMinMaxFlowConstraintsField.ArrayLength, LMinMaxFlowConstraintsField.ArrayLength(1));
    SetLength(FMonthlyDistribution, LMinMaxFlowConstraintsField.ArrayLength, LMinMaxFlowConstraintsField.ArrayLength(1));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMinMaxFlowConstraint.DestroyMemberObjects;
const OPNAME = 'TMinMaxFlowConstraint.DestroyMemberObjects';
begin
  try
    Finalize(FMonthlyFlowConstraints);
    Finalize(FMonthlyDistribution);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxFlowConstraint.Initialise: boolean;
const OPNAME = 'TMinMaxFlowConstraint.Initialise';
var
  lArcCount        : integer;
  lMonth           : integer;
  LMinMaxFlowField : TAbstractFieldProperty;
begin
  Result := inherited Initialise;
  try
    FChannelNr            := 0;
    FFeatureID            := -1;
    FFeatureName          := '';
    FFeatureType          := -1;
    FFeatureSubType       := -1;
    LMinMaxFlowField := FAppModules.FieldProperties.FieldProperty('FlowConstraints');
    if not Assigned(LMinMaxFlowField) then
      raise Exception.Create('Field (FlowConstraints) not found in field properties');
    for lArcCount := LMinMaxFlowField.ArrayLow  to LMinMaxFlowField.ArrayHigh do
      for lMonth := LMinMaxFlowField.ArrayLowDimTwo  to LMinMaxFlowField.ArrayHighDimTwo do
      begin
        FMonthlyFlowConstraints[lArcCount, lMonth] := NullFloat;
        FMonthlyDistribution[lArcCount, lMonth]    := NullFloat;
      end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxFlowConstraint.Get_FeatureID : integer;
const OPNAME = 'TMinMaxFlowConstraint.Get_FeatureID';
begin
  Result := 0;
  try
    Result := FFeatureID;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxFlowConstraint.Get_FeatureName : WideString;
const OPNAME = 'TMinMaxFlowConstraint.Get_FeatureName';
begin
  Result := '';
  try
    Result := FFeatureName;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxFlowConstraint.Get_Channel : IGeneralFlowChannel;
const OPNAME = 'TMinMaxFlowConstraint.Get_Channel';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.
              ChannelByChannelNumber[FChannelNr];
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxFlowConstraint.Get_FeatureType : integer;
const OPNAME = 'TMinMaxFlowConstraint.Get_FeatureType';
begin
  Result := 0;
  try
    Result := FFeatureType;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxFlowConstraint.Get_FeatureSubType : integer;
const OPNAME = 'TMinMaxFlowConstraint.Get_FeatureSubType';
begin
  Result := 0;
  try
    Result := FFeatureSubType;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxFlowConstraint.Get_FlowConstraintByArcMonth (AArc   : integer;
                                                             AMonth : integer) : double;
const OPNAME = 'TMinMaxFlowConstraint.Get_FlowConstraintByArcMonth';
begin
  Result := 0.0;
  try
    Result := StrToFloat(FAppModules.Changes.GetParameterValue
                          ('FlowConstraints',
                          GetKeyValues('FlowConstraints', IntToStr(AArc) + ','+IntToStr(AMonth)),
                          FloatToStr(FMonthlyFlowConstraints[AArc,AMonth]),
                          IntToStr(AArc) + ',' + IntToStr(AMonth)));
//    Result := FMonthlyFlowConstraints[AArc, AMonth];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMinMaxFlowConstraint.Set_FlowConstraintByArcMonth (AArc   : Integer;
                                                              AMonth : Integer;
                                                              Value  : Double);
const OPNAME = 'TMinMaxFlowConstraint.Set_FlowConstraintByArcMonth';
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
        LLoadAgent.LoadContextData_FeatureIDSubIDFieldNameID(LContextData,
          IntToStr(FFeatureID), IntToStr(AArc), IntToStr(AMonth));
        if FAppModules.FieldProperties.UpdateFieldValue
          ('FlowConstraints', FloatToStr(Value),
          FloatToStr(FMonthlyFlowConstraints[AArc,AMonth]), LContextData) then
        begin
          LPrevValue := FMonthlyFlowConstraints[AARc,AMonth];
          FMonthlyFlowConstraints[AArc,AMonth] := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'FlowConstraints',
            FloatToStr(LPrevValue),FloatToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxFlowConstraint.Get_FlowConstraintCount: integer;
const OPNAME = 'TMinMaxFlowConstraint.Get_FlowConstraintCount';
var
  lArcCount        : integer;
  lMinMaxFlowField : TAbstractFieldProperty;
  lCount           : integer;
begin
  Result := 0;
  try
    LMinMaxFlowField := FAppModules.FieldProperties.FieldProperty('FlowConstraints');
    if not Assigned(LMinMaxFlowField) then
      raise Exception.Create('Field (FlowConstraints) not found in field properties');
    lCount := 0;
    for lArcCount := lMinMaxFlowField.ArrayLow  to lMinMaxFlowField.ArrayHigh do
    begin
      if (FMonthlyFlowConstraints[lArcCount, lMinMaxFlowField.ArrayLowDimTwo] <> NullFloat) then
        lCount := lCount + 1;
    end;
    Result := lCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxFlowConstraint.CreateFlowConstraints : WordBool;
const OPNAME = 'TMinMaxFlowConstraint.CreateFlowConstraints';
var
  lArc             : integer;
  lMonth           : integer;
  lMinMaxFlowField : TAbstractFieldProperty;
  LLoadAgent       : TChannelDataSQLAgent;
begin
  Result := FALSE;
  try
    if ((Channel <> nil) AND (Channel.ChannelPenalty <> nil)) then
    begin
      lMinMaxFlowField := FAppModules.FieldProperties.FieldProperty('FlowConstraints');
      if not Assigned(lMinMaxFlowField) then
        raise Exception.Create('Field (FlowConstraints) not found in field properties');

      LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
      try
        for lArc := FlowConstraintCount + 1 to Channel.ChannelPenalty.ChannelPenaltyArcCount do
        begin
          lLoadAgent.AddMonthlyConstraint(FFeatureID, lArc);
          if (FAppModules.Model.ModelName = CPlanning) then
            lLoadAgent.AddMinMaxChannelDistributionValues(FFeatureID, lArc);
          for lMonth := lMinMaxFlowField.ArrayLowDimTwo to lMinMaxFlowField.ArrayHighDimTwo do
          begin
            FMonthlyFlowConstraints[lArc, lMonth] := 0.0;
            if (FAppModules.Model.ModelName = CPlanning) then
              FMonthlyDistribution[lArc, lMonth] := 0.0;
          end;
        end;
      finally
        LLoadAgent.Free;
      end;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxFlowConstraint.RemoveFlowConstraints : WordBool;
const OPNAME = 'TMinMaxFlowConstraint.RemoveFlowConstraints';
var
  lArc             : integer;
  lMonth           : integer;
  LLoadAgent       : TChannelDataSQLAgent;
  lMinMaxFlowField : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    if ((Channel <> nil) AND (Channel.ChannelPenalty <> nil)) then
    begin
      lMinMaxFlowField := FAppModules.FieldProperties.FieldProperty('FlowConstraints');
      if not Assigned(lMinMaxFlowField) then
        raise Exception.Create('Field (FlowConstraints) not found in field properties');

      LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
      try
        for lArc := FlowConstraintCount downto Channel.ChannelPenalty.ChannelPenaltyArcCount + 1 do
        begin
          for lMonth := lMinMaxFlowField.ArrayLowDimTwo to lMinMaxFlowField.ArrayHighDimTwo do
          begin
            FMonthlyFlowConstraints[lArc, lMonth] := NullFloat;
            if (FAppModules.Model.ModelName = CPlanning) then
              FMonthlyDistribution[lArc, lMonth] := NullFloat;
          end;
          lLoadAgent.DeleteMonthlyConstraint(FFeatureID, lArc);
          if (FAppModules.Model.ModelName = CPlanning) then
            lLoadAgent.DeleteMonthlyDistribution(FFeatureID, lArc);
        end;
      finally
        LLoadAgent.Free;
      end;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMinMaxFlowConstraint.Set_FeatureName (const AName : WideString);
const OPNAME = 'TMinMaxFlowConstraint.Set_FeatureName';
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
           'MinMaxChannelName', AName, FFeatureName, LContextData) then
        begin
          LOldValue    := FFeatureName;
          FFeatureName := AName;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MinMaxChannelName',LOldValue,AName);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMinMaxFlowConstraint.Set_Channel (const AChannel : IGeneralFlowChannel);
const OPNAME = 'TMinMaxFlowConstraint.Set_Channel';
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
           'MinMaxChannelNumber', LNewValue, LOldValue, LContextData) then
        begin
          FChannelNr := AChannel.ChannelNumber;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MinMaxChannelNumber',LOldValue,LNewValue);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMinMaxFlowConstraint.Set_FeatureType (AType : integer);
const OPNAME = 'TMinMaxFlowConstraint.Set_FeatureType';
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

procedure TMinMaxFlowConstraint.Set_FeatureSubType (ASubType : integer);
const OPNAME = 'TMinMaxFlowConstraint.Set_FeatureSubType';
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

function TMinMaxFlowConstraint.Populate (AFeatureID        : integer;
                                          AFeatureName     : WideString;
                                          AChannelNr       : integer;
                                          AFeatureType     : integer;
                                          AFeatureSubType  : integer;
                                          AFlowConstraints : TMinMaxMonthlyDoublesArray): WordBool;
const OPNAME = 'TMinMaxFlowConstraint.Populate';
var
  lArcCount        : integer;
  lMonth           : integer;
  lMinMaxFlowField : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    FChannelNr      := AChannelNr;
    FFeatureID      := AFeatureID;
    FFeatureName    := AFeatureName;
    FFeatureType    := AFeatureType;
    FFeatureSubType := AFeatureSubType;
    lMinMaxFlowField := FAppModules.FieldProperties.FieldProperty('FlowConstraints');
    for lArcCount := LMinMaxFlowField.ArrayLow to LMinMaxFlowField.ArrayHigh do
      for lMonth := LMinMaxFlowField.ArrayLowDimTwo to LMinMaxFlowField.ArrayHighDimTwo do
      begin
        FMonthlyFlowConstraints[lArcCount, lMonth] := AFlowConstraints[lArcCount, lMonth];
        if(AFlowConstraints[lArcCount, lMonth] <> NullFloat) then
          FMonthlyDistribution[lArcCount, lMonth] := 0.0
        else
          FMonthlyDistribution[lArcCount, lMonth] := NullFloat;
      end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxFlowConstraint.PopulateDistributionFactors(ADistributionByArcMonth : TMinMaxMonthlyDoublesArray): WordBool;
const OPNAME = 'TMinMaxFlowConstraint.PopulateDistributionFactors';
var
  lArcCount        : integer;
  lMonth           : integer;
  lMinMaxFlowField : TAbstractFieldProperty;
begin
  Result := False;
  try
    lMinMaxFlowField := FAppModules.FieldProperties.FieldProperty('FlowConstraints');
    for lArcCount := LMinMaxFlowField.ArrayLow to LMinMaxFlowField.ArrayHigh do
      for lMonth := LMinMaxFlowField.ArrayLowDimTwo to LMinMaxFlowField.ArrayHighDimTwo do
        FMonthlyDistribution[lArcCount, lMonth] := ADistributionByArcMonth[lArcCount, lMonth];
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{******************************************************************************}
{ TMinMaxFlowConstraintList                                                    }
{******************************************************************************}

function TMinMaxFlowConstraintList._AddRef: Integer;
const OPNAME = 'TMinMaxFlowConstraintList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxFlowConstraintList._Release: Integer;
const OPNAME = 'TMinMaxFlowConstraintList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMinMaxFlowConstraintList.CreateMemberObjects;
const OPNAME = 'TMinMaxFlowConstraintList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FMinMaxFlowConstraintList := TList.Create;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMinMaxFlowConstraintList.DestroyMemberObjects;
const OPNAME = 'TMinMaxFlowConstraintList.DestroyMemberObjects';
begin
  try
    while (FMinMaxFlowConstraintList.Count > 0) do
      DeleteMinMaxFlowConstraintWithIndex(0);
    FreeAndNil(FMinMaxFlowConstraintList);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxFlowConstraintList.Initialise: boolean;
const OPNAME = 'TMinMaxFlowConstraintList.Initialise';
begin
  Result := inherited Initialise;
  try
    FMinMaxFlowConstraintList.Clear;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxFlowConstraintList.AddMinMaxFlowConstraint (AFeature : TMinMaxFlowConstraint): boolean;
const OPNAME = 'TMinMaxFlowConstraintList.AddMinMaxFlowConstraint';
begin
  Result := False;
  try
    if (AFeature <> nil) then
    begin
      FMinMaxFlowConstraintList.Add(AFeature);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxFlowConstraintList.NewMinMaxFlowConstraint : TMinMaxFlowConstraint;
const OPNAME = 'TMinMaxFlowConstraintList.NewMinMaxFlowConstraint';
var
  lFeature : TMinMaxFlowConstraint;
begin
  Result := nil;
  try
    lFeature := TMinMaxFlowConstraint.Create(FAppModules);
    AddMinMaxFlowConstraint(lFeature);
    Result := lFeature;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxFlowConstraintList.CreateNewMinMaxFlowConstraint : TMinMaxFlowConstraint;
const OPNAME = 'TMinMaxFlowConstraintList.CreateNewMinMaxFlowConstraint';
var
  LFeatureID       : integer;
  LLoadAgent       : TChannelDataSQLAgent;
  LFeature         : TMinMaxFlowConstraint;
  lValues          : TMinMaxMonthlyDoublesArray;
  lArcCount        : integer;
  lMonth           : integer;
  LMinMaxFlowField : TAbstractFieldProperty;
begin
  Result := nil;
  try
    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
    try
      LMinMaxFlowField := FAppModules.FieldProperties.FieldProperty('FlowConstraints');
      if not Assigned(LMinMaxFlowField) then
        raise Exception.Create('Field (FlowConstraints) not found in field properties');
      SetLength(lValues, LMinMaxFlowField.ArrayLength, LMinMaxFlowField.ArrayLength(1));

      for lArcCount := LMinMaxFlowField.ArrayLow to LMinMaxFlowField.ArrayHigh do
        for lMonth := LMinMaxFlowField.ArrayLowDimTwo to LMinMaxFlowField.ArrayHighDimTwo do
//          lValues[lArcCount, lMonth] := 0.0;
          lValues[lArcCount, lMonth] := NullFloat;

      LFeatureID := 0;
      if (LLoadAgent.InsertMinMaxFlowConstraint(LFeatureID)) then
      begin
        LFeature := NewMinMaxFlowConstraint;
        LFeature.Initialise;
        LFeature.Populate
          (LFeatureID,
           UpperCase(FAppModules.Language.GetString('NetworkFeatures.MinMaxFlowFeature')) + ' ' + IntToStr(LFeatureID),
           0, 5, 0, lValues);
        Result := LFeature;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxFlowConstraintList.CreateMinMaxFlowConstraint : IMinMaxFlowConstraint;
const OPNAME = 'TMinMaxFlowConstraintList.CreateMinMaxFlowConstraint';
var
  LFeature : IMinMaxFlowConstraint;
begin
  Result := nil;
  try
    lFeature := CreateNewMinMaxFlowConstraint;
    Result := lFeature;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxFlowConstraintList.RemoveMinMaxFlowConstraintWithID (AFeatureID : integer) : WordBool;
const OPNAME = 'TMinMaxFlowConstraintList.RemoveMinMaxFlowConstraintWithID';
var
  lLoadAgent : TChannelDataSQLAgent;
begin
  Result := FALSE;
  try
    if (AFeatureID > 0) then
    begin
      LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
      try
        if LLoadAgent.DeleteMinMaxFlowConstraint(AFeatureID) then
        begin
          DeleteMinMaxFlowConstraintWithID(AFeatureID);
          Result := TRUE;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxFlowConstraintList.DeleteMinMaxFlowConstraintWithID(AFeatureID : integer) : WordBool;
const OPNAME = 'TMinMaxFlowConstraintList.DeleteMinMaxFlowConstraintWithID';
var
  lFeature : TMinMaxFlowConstraint;
  lIndex   : integer;
begin
  Result := FALSE;
  try
    lFeature := CastMinMaxFlowConstraintByID(AFeatureID);
    if (lFeature <> nil) then
    begin
      lIndex := FMinMaxFlowConstraintList.IndexOf(lFeature);
      Result := DeleteMinMaxFlowConstraintWithIndex(lIndex);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxFlowConstraintList.DeleteMinMaxFlowConstraintWithIndex(AIndex : integer) : WordBool;
const OPNAME = 'TMinMaxFlowConstraintList.DeleteMinMaxFlowConstraintWithIndex';
var
  lFeature : TMinMaxFlowConstraint;
begin
  Result := FALSE;
  try
    if (AIndex >= 0) then
    begin
      lFeature := TMinMaxFlowConstraint(FMinMaxFlowConstraintList.Items[AIndex]);
      FMinMaxFlowConstraintList.Delete(AIndex);
      FreeAndNil(lFeature);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxFlowConstraintList.Get_MinMaxFlowConstraintByIndex(AIndex: integer): IMinMaxFlowConstraint;
const OPNAME = 'TMinMaxFlowConstraintList.Get_MinMaxFlowConstraintByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FMinMaxFlowConstraintList.Count) then
      Result := TMinMaxFlowConstraint(FMinMaxFlowConstraintList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxFlowConstraintList.Get_MinMaxFlowConstraintByID(AFeatureID : integer): IMinMaxFlowConstraint;
const OPNAME = 'TMinMaxFlowConstraintList.Get_MinMaxFlowConstraintByID';
var
 lIndex   : integer;
 lFeature : TMinMaxFlowConstraint;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FMinMaxFlowConstraintList.Count)) do
    begin
      lFeature := TMinMaxFlowConstraint(FMinMaxFlowConstraintList.Items[lIndex]);
      if (lFeature.FeatureID = AFeatureID) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxFlowConstraintList.CastMinMaxFlowConstraintByIndex(AIndex: integer): TMinMaxFlowConstraint;
const OPNAME = 'TMinMaxFlowConstraintList.CastMinMaxFlowConstraintByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FMinMaxFlowConstraintList.Count) then
      Result := TMinMaxFlowConstraint(FMinMaxFlowConstraintList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxFlowConstraintList.CastMinMaxFlowConstraintByID(AFeatureID : integer): TMinMaxFlowConstraint;
const OPNAME = 'TMinMaxFlowConstraintList.CastMinMaxFlowConstraintByID';
var
 lIndex   : integer;
 lFeature : TMinMaxFlowConstraint;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FMinMaxFlowConstraintList.Count)) do
    begin
      lFeature := TMinMaxFlowConstraint(FMinMaxFlowConstraintList.Items[lIndex]);
      if (lFeature.FeatureID = AFeatureID) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxFlowConstraintList.CastMinMaxFlowConstraintByChannelNr(AChannelNr : integer): TMinMaxFlowConstraint;
const OPNAME = 'TMinMaxFlowConstraintList.CastMinMaxFlowConstraintByChannelNr';
var
 lIndex   : integer;
 lFeature : TMinMaxFlowConstraint;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FMinMaxFlowConstraintList.Count)) do
    begin
      lFeature := TMinMaxFlowConstraint(FMinMaxFlowConstraintList.Items[lIndex]);
      if (lFeature.FChannelNr = AChannelNr) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxFlowConstraintList.Get_MinMaxFlowConstraintCount: integer;
const OPNAME = 'TMinMaxFlowConstraintList.Get_MinMaxFlowConstraintCount';
begin
  Result := 0;
  try
    Result := FMinMaxFlowConstraintList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxFlowConstraint.Validate (var AErrors : WideString; const AContext: WideString='') : WordBool;

const OPNAME = 'TMinMaxFlowConstraint.Validate';
var
  lStopOnFirstError : Boolean;
  lErrorCols        : TStringList;
  lErrorList        : TStringList;
begin
  Result := FALSE;
  try
    lErrorList := TStringList.Create;
    lErrorCols := TStringList.Create;
    try
      if (AContext = 'MinMaxFeatureName') then
        Result := ValidateMinMaxFeatureName(lErrorList)
      else
      if (AContext = 'MinMaxDistribution') then
      begin
        Result := ValidateMinMaxDistributionFactors(lErrorList,lErrorCols);
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
      if (AContext = 'MinMaxFlowConstraint') then
      begin
        Result := ValidateMinMaxFlowConstraint(lErrorList,lErrorCols);
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
      begin
        Result := TRUE;
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        if (NOT ValidateMinMaxFeatureName(lErrorList)) then
          Result := FALSE;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateMinMaxFlowConstraint(lErrorList,lErrorCols))then
          begin
            Result := FALSE;
            if (lErrorCols.Count = 0) then
              AErrors := AErrors + lErrorList.Text
            else
              AErrors := AErrors + CTStringsSeparator + lErrorList.Text +
                                   CTStringsSeparator + lErrorCols.Text + CTStringsSeparator;
            lErrorList.Clear;
            lErrorCols.Clear;
          end;
          if (NOT ValidateMinMaxDistributionFactors(lErrorList,lErrorCols)) then
           begin
            Result := FALSE;
            if (lErrorCols.Count = 0) then
              AErrors := AErrors + lErrorList.Text
            else
              AErrors := AErrors + CTStringsSeparator + lErrorList.Text +
                                   CTStringsSeparator + lErrorCols.Text + CTStringsSeparator;
            lErrorList.Clear;
            lErrorCols.Clear;
          end;
        end;
      end;
      AErrors := AErrors + lErrorList.Text
    finally
      FreeAndNil(lErrorList);
      FreeAndNil(lErrorCols);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinMaxFlowConstraint.ValidateMinMaxFeatureName( AErrorMessages: TStrings): WordBool;
const OPNAME = 'TMinMaxFlowConstraint.ValidateMinMaxFeatureName';
var
  lIndex       : integer;
  lFeatureList : TMinMaxFlowConstraintList;
  lFeature     : TMinMaxFlowConstraint;
  lUnique      : Boolean;
  lMessage     : string;
begin
  Result := True;
  try
    lMessage  := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('MinMaxChannelName', FFeatureName, lMessage)) then
       AErrorMessages.Add('WARNING:' +Channel.ChannelName+ ':'+lMessage)
    else
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        CastNetworkFeaturesData.CastMinMaxFlowConstraintList;
      lUnique := TRUE;
      lIndex  := 0;
      while (lUnique AND (lIndex < lFeatureList.MinMaxFlowConstraintCount)) do
      begin
        lFeature := lFeatureList.CastMinMaxFlowConstraintByIndex(lIndex);
        if ((lFeature.FeatureID <> FFeatureID) AND
           (UpperCase(Trim(lFeature.FeatureName)) = UpperCase(Trim(FFeatureName)))) then
        begin
          lMessage := FAppModules.Language.GetString('ContextValidation.DuplicateMinMaxFeatureName');
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

function TMinMaxFlowConstraint.ValidateMinMaxDistributionFactors(AErrorMessages : TStrings;
                                                            AErrorColumns  : TStringList): WordBool;
const OPNAME = 'TMinMaxFlowConstraint.ValidateMinMaxDistributionFactors';
var
  lMonth            : integer;
  lStopOnFirstError : Boolean;
  lResult           : Boolean;
  lMessage          : string;
  LMinMaxFlowField  : TAbstractFieldProperty;
  lArcCount         : integer;
  lCount            : integer;
begin
  Result := FALSE;
  try
    lResult := True;
    if (FAppModules.Model.ModelName = CPlanning) then
    begin
      lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
      LMinMaxFlowField := FAppModules.FieldProperties.FieldProperty('MinMaxDistribution');
      if (LMinMaxFlowField <> nil) then
      begin
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        for lArcCount := LMinMaxFlowField.ArrayLow to LMinMaxFlowField.ArrayHigh do
        begin
          for lMonth := LMinMaxFlowField.ArrayLowDimTwo to LMinMaxFlowField.ArrayHighDimTwo do
          begin
            lMessage := '';
            if (Not FAppModules.FieldProperties.ValidateFieldProperty
               ('MinMaxDistribution', FloatToStr(FMonthlyDistribution[lArcCount, lMonth]),
               lMessage, lArcCount, lMonth)) then
            begin
              lResult := FALSE;
              AErrorMessages.Add('ERROR:' +lMessage);
              if (lStopOnFirstError) then
                Break;
            end;
          end;
        end;
      end;
      if (FAppModules.Model.ModelName = CPlanning) then
      begin
        if (lResult OR (NOT lStopOnFirstError)) then
        begin
          lCount := 0;
          for lArcCount :=  LMinMaxFlowField.ArrayLow to LMinMaxFlowField.ArrayHigh do
          begin
            if (FMonthlyDistribution[lArcCount, LMinMaxFlowField.ArrayLowDimTwo] <> NullFloat) then
              lCount := lCount + 1;
          end;
          if ((Channel.ChannelPenalty <> nil) AND
              (Channel.ChannelPenalty.ChannelPenaltyArcCount <> lCount)) then
          begin
            lResult := FALSE;
            AErrorColumns.Add(IntToStr(Channel.ChannelPenalty.ChannelPenaltyArcCount));
            lMessage := FAppModules.Language.GetString('ContextValidation.ChannelPenaltyArcCount');
            AErrorMessages.Add('ERROR:' +Channel.ChannelName+ ':'+lMessage);
          end;
        end;
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxFlowConstraint.ValidateMinMaxFlowConstraint(AErrorMessages : TStrings;
                                                            AErrorColumns  : TStringList): WordBool;
const OPNAME = 'TMinMaxFlowConstraint.ValidateMinMaxFlowConstraint';
var
  lMonth            : integer;
  lStopOnFirstError : Boolean;
  lResult           : Boolean;
  lMessage          : string;
  LMinMaxFlowField  : TAbstractFieldProperty;
  lArcCount         : integer;
  lCount            : integer;
begin
  Result := FALSE;
  try
    lResult := True;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;

    LMinMaxFlowField := FAppModules.FieldProperties.FieldProperty('FlowConstraints');
    if (LMinMaxFlowField <> nil) then
    begin
      lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
      for lArcCount := LMinMaxFlowField.ArrayLow to LMinMaxFlowField.ArrayHigh do
      begin
        for lMonth := LMinMaxFlowField.ArrayLowDimTwo to LMinMaxFlowField.ArrayHighDimTwo do
        begin
          lMessage := '';
          if (Not FAppModules.FieldProperties.ValidateFieldProperty
             ('FlowConstraints', FloatToStr(FMonthlyFlowConstraints[lArcCount, lMonth]),
             lMessage, lArcCount, lMonth)) then
          begin
            lResult := FALSE;
            AErrorMessages.Add('ERROR:' +lMessage);
            if (lStopOnFirstError) then
              Break;
          end;
        end;
      end;

      if (lResult OR (NOT lStopOnFirstError)) then
      begin
        for lArcCount :=  LMinMaxFlowField.ArrayLow to LMinMaxFlowField.ArrayHigh - 1 do
        begin
          for lMonth := LMinMaxFlowField.ArrayLowDimTwo to LMinMaxFlowField.ArrayHighDimTwo do
          begin
            if (FMonthlyFlowConstraints[lArcCount, lMonth] <
                FMonthlyFlowConstraints[lArcCount+1, lMonth]) then
            begin
              lResult := FALSE;
              AErrorColumns.Add(IntToStr(lArcCount));
              lMessage := FAppModules.Language.GetString('ContextValidation.FlowConstraintValuesNotDescending');
              AErrorMessages.Add('ERROR:' +Channel.ChannelName+ ':'+lMessage);
              if (lStopOnFirstError) then
                Break;
            end;
          end;
          if ((NOT lResult) AND lStopOnFirstError) then
            Break;
        end;
      end;
    end;

    if (lResult OR (NOT lStopOnFirstError)) then
    begin
      lCount := 0;
      for lArcCount :=  LMinMaxFlowField.ArrayLow to LMinMaxFlowField.ArrayHigh do
      begin
        if (FMonthlyFlowConstraints[lArcCount, LMinMaxFlowField.ArrayLowDimTwo] <> NullFloat) then
          lCount := lCount + 1;
      end;
      if ((Channel.ChannelPenalty <> nil) AND
          (Channel.ChannelPenalty.ChannelPenaltyArcCount <> lCount)) then
      begin
        lResult := FALSE;
        AErrorColumns.Add(IntToStr(Channel.ChannelPenalty.ChannelPenaltyArcCount));
        lMessage := FAppModules.Language.GetString('ContextValidation.ChannelPenaltyArcCount');
        AErrorMessages.Add('ERROR:' +Channel.ChannelName+ ':'+lMessage);
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TMinMaxFlowConstraintList.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TMinMaxFlowConstraintList.Validate';
var
  lStopOnFirstError : Boolean;
  LIndex            : integer;
begin
  Result := FALSE;
  try
    Result := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    for LIndex := 0 to FMinMaxFlowConstraintList.Count - 1 do
    begin
      if (NOT MinMaxFlowConstraintByIndex[LIndex].Validate(AErrors,AContext)) then
        Result := FALSE;
      if ((NOT Result) AND lStopOnFirstError) then
        Break;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxFlowConstraint.GetBaseValue (const AParamField : WideString;
                                             const AFieldIndex : WideString): WideString;
const OPNAME = 'TMinMaxFlowConstraint.GetBaseValue';
var
  lFieldProperty : TAbstractFieldProperty;
  lFormatStr     : string;
  lDim1Idx       : integer;
  lDim2Idx       : integer;
begin
  Result := '';
  try
    lFormatStr := '';
    lFieldProperty := FAppModules.FieldProperties.FieldProperty(AParamField);
    if (lFieldProperty <> nil) then
      lFormatStr := lFieldProperty.FormatStringGrid;

    FAppModules.Changes.GetIndexes(AFieldIndex, lDim1Idx, lDim2Idx);
    if (AParamField = 'FlowConstraints') then
    begin
      if (lFormatStr = '') then
        Result := FloatToStr(FMonthlyFlowConstraints[lDim1Idx,lDim2Idx])
      else
        Result := Format(lFormatStr, [FMonthlyFlowConstraints[lDim1Idx,lDim2Idx]]);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMinMaxFlowConstraint.GetKeyValues (const AParamField : WideString;
                                             const AFieldIndex : WideString) : WideString;
const OPNAME = 'TMinMaxFlowConstraint.GetKeyValues';
var
  lDim1Idx     : integer;
  lDim2Idx     : integer;
begin
  Result := '';
  try
    Result := GetDefaultKey;
    if (Result = '') then
      Result := 'Identifier=' + IntToStr(FFeatureID)
    else
      Result := Result + ',Identifier=' + IntToStr(FFeatureID);
    if (AParamField = 'FlowConstraints') then
    begin
      FAppModules.Changes.GetIndexes(AFieldIndex, lDim1Idx, lDim2Idx);
      Result := Result + ',SubIdentifier=' + IntToStr(lDim1Idx);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinMaxFlowConstraint.CalculateDistributionFactors;
const OPNAME = 'TMinMaxFlowConstraint.CalculateDistributionFactors';
var
  LArcCount,
  LIndex : integer;
  LMonthDays : TMonthDaysArray;
  LFactorsTotal : double;
  LFieldProperty : TAbstractFieldProperty;
  LPenaltyStructure : IChannelPenalty;
  LMonthlyDemand : double;
begin
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('Days');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (Days) not found in field properties');
    SetLength(LMonthDays,LFieldProperty.ArrayLength);
    LMonthDays := TYieldModelDataObject(FAppModules.Model.ModelData).
                  CastRunConfigurationData.MonthsDaysArray;
    LPenaltyStructure := Channel.ChannelPenalty;
    if LPenaltyStructure <> nil then
    begin
      for LArcCount := 1 to LPenaltyStructure.ChannelPenaltyArcCount do
        for LIndex := MinMonths to MaxMonths do
          FMonthlyDistribution[LArcCount,LIndex] := 0.0;
      for LArcCount := 1 to LPenaltyStructure.ChannelPenaltyArcCount do
      begin
        LFactorsTotal := 0.0;
        for LIndex := MinMonths to MaxMonths do
        begin
          FMonthlyDistribution[LArcCount,LIndex] := (LMonthDays[LIndex]* 24*60*60*FlowConstraintByArcMonth[LArcCount,LIndex])/1000000;
          LFactorsTotal := LFactorsTotal + FMonthlyDistribution[LArcCount,LIndex];
        end;

         for LIndex := MinMonths to MaxMonths do
        begin
          LMonthlyDemand := (LFactorsTotal*LMonthDays[LIndex])/365.25;
          if LMonthlyDemand <= 0 then
            LMonthlyDemand := 1;
          if FMonthlyDistribution[LArcCount,LIndex] <= 0 then
            FMonthlyDistribution[LArcCount,LIndex] := 1;

          DistributionByArcMonth[LArcCount,LIndex] := FMonthlyDistribution[LArcCount,LIndex]/LMonthlyDemand;
        end;

      end;
    end;
    Finalize(LMonthDays);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMinMaxFlowConstraint.Get_DistributionByArcMonth(AArc,AMonth: Integer): Double;
const OPNAME = 'TMinMaxFlowConstraint.Get_DistributionByArcMonth';
begin
  Result := 0;
  try
    Result := FMonthlyDistribution[AArc,AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMinMaxFlowConstraint.Set_DistributionByArcMonth(AArc,AMonth: Integer; Value: Double);
const OPNAME = 'TMinMaxFlowConstraint.Set_DistributionByArcMonth';
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
        LLoadAgent.LoadContextData_FeatureIDSubIDFieldNameID(LContextData,
          IntToStr(FFeatureID), IntToStr(AArc), IntToStr(AMonth));
        if not LLoadAgent.MinMaxArcRecordFound(FFeatureID, AArc) then
        begin
          LLoadAgent.AddMinMaxChannelDistributionValues(FFeatureID,AArc);
        end;
        if FAppModules.FieldProperties.UpdateFieldValue
          ('MinMaxDistribution', FloatToStr(Value),
          FloatToStr(FMonthlyDistribution[AArc,AMonth]), LContextData) then
        begin
          LPrevValue := FMonthlyDistribution[AARc,AMonth];
          FMonthlyDistribution[AArc,AMonth] := Value;

          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MinMaxDistribution',
            FloatToStr(LPrevValue),FloatToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMinMaxFlowConstraint.Assign(ANewChannelNumber : integer;AMinMaxFlowConstraint: TMinMaxFlowConstraint);
const OPNAME = 'TMinMaxFlowConstraint.Assign';
var
  LChannel : IGeneralFlowChannel;
  LArcCount,
  LMonth : integer;
  LMinMaxFlowField : TAbstractFieldProperty;
begin
  try
    if (AMinMaxFlowConstraint <> nil) and (ANewChannelNumber > 0) then
    begin
      LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[ANewChannelNumber];
      if LChannel <> nil then
        Channel := LChannel;
      FeatureName := 'Copy of '+AMinMaxFlowConstraint.FeatureName;
      FeatureType := AMinMaxFlowConstraint.FeatureType;
      FeatureSubType := AMinMaxFlowConstraint.FeatureSubType;
      if CreateFlowConstraints then
      begin
        LMinMaxFlowField := FAppModules.FieldProperties.FieldProperty('FlowConstraints');
        for LArcCount := LMinMaxFlowField.ArrayLow to LMinMaxFlowField.ArrayHigh do
          for LMonth := LMinMaxFlowField.ArrayLowDimTwo to LMinMaxFlowField.ArrayHighDimTwo do
          begin
            if (AMinMaxFlowConstraint.FlowConstraintByArcMonth[LArcCount, LMonth] <> NullFloat) then
              FlowConstraintByArcMonth[LArcCount, LMonth] := AMinMaxFlowConstraint.FlowConstraintByArcMonth[LArcCount, LMonth];
            if (FAppModules.Model.ModelName = CPlanning) then
              DistributionByArcMonth[LArcCount, LMonth] := AMinMaxFlowConstraint.DistributionByArcMonth[LArcCount, LMonth];
          end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinMaxFlowConstraintList.CopyMinMaxFlowConstraint(ANewChannelNumber, AOldChannelNumber: Integer): IMinMaxFlowConstraint;
const OPNAME = 'TMinMaxFlowConstraintList.CopyMinMaxFlowConstraint';
var
  LMinMaxFlowConstraint : TMinMaxFlowConstraint;
  LMinMaxFlowConstraintCopy : TMinMaxFlowConstraint;
begin
  Result := nil;
  try
    LMinMaxFlowConstraint := CastMinMaxFlowConstraintByChannelNr(AOldChannelNumber);
    if (LMinMaxFlowConstraint <> nil) then
    begin
      LMinMaxFlowConstraintCopy := CreateNewMinMaxFlowConstraint;
      if LMinMaxFlowConstraintCopy <> nil then
      begin
        LMinMaxFlowConstraintCopy.Assign(ANewChannelNumber,LMinMaxFlowConstraint);
        Result := LMinMaxFlowConstraintCopy;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
