{******************************************************************************}
{*  UNIT      : Contains the class TDiversionFeature.                         *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2004/03/10                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UDiversionFeatures;

interface

uses
  Classes,
  Contnrs,
  UAbstractObject,
  VoaimsCom_TLB;

type

{******************************************************************************}
{* Diversion Feature                                                          *}
{******************************************************************************}

  TDiversionFeature = class(TAbstractAppObject, IDiversionFeature)
  protected
    FFeatureID                : integer;
    FFeatureName              : string;
    FChannelNr                : integer;
    FFeatureType              : integer;
    FFeatureSubType           : integer;
    FDiversionType            : integer;
    FDiversionDemands         : TDivMonthlyDoublesArray;
    FDivertedFlows            : TDivMonthlyDoublesArray;
    FControllingReservoirNr   : integer;
    FReservoirElevations      : TReservoirElevationsArray;
    FReferenceFlows           : TReferenceFlowsArray;
    FDivertedFlowProportions  : TDivertedFlowProportionsArray;
    FStation                  : string;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure WriteDiversionDemandToDB (AIndex : integer;
                                        AValue : double);
    procedure WriteDivertedFlowToDB (AIndex : integer;
                                     AValue : double);
    function Populate (AFeatureID      : integer;
                       AFeatureName    : WideString;
                       AChannelNr      : integer;
                       AFeatureType    : integer;
                       AFeatureSubType : integer;
                       ADiversionType  : integer): WordBool;
    function ValidateFeatureName (AErrorMessages : TStrings) : Boolean;
    function ValidateStation(AErrorMessages : TStrings) : Boolean;
    function ValidateDiversionType (AErrorMessages : TStrings) : Boolean;
    function ValidateType1DiversionDemands (AErrorMessages : TStrings;AErrorColumns: TStringList) : Boolean;
    function ValidateType1NetNaturalInflows (AErrorMessages : TStrings; AErrorColumns: TStringList) : Boolean;
    function ValidateType2FlowRanges (AErrorMessages : TStrings; AErrorColumns: TStringList) : Boolean;
    function ValidateType2ActualDivertedFlows (AErrorMessages : TStrings;AErrorColumns: TStringList) : Boolean;
    function ValidateControllingReservoir (AErrorMessages : TStrings) : boolean;
    function ValidateType3Levels(AErrorMessages : TStrings;
                                 AErrorColumns  : TStringList): boolean;
    function ValidateType3Flows(AErrorMessages : TStrings): boolean;
    function ValidateType3Proportions(AErrorMessages : TStrings;
                                      AErrorColumns  : TStringList): boolean;
  public
    procedure Assign(AChannelNumber : integer;ADiversionFeature:TDiversionFeature);
    function Initialise : boolean; override;
    function PopulateType1or2 (AFeatureID        : integer;
                               AFeatureName      : WideString;
                               AStation          : WideString;
                               AChannelNr        : integer;
                               AFeatureType      : integer;
                               AFeatureSubType   : integer;
                               ADiversionType    : integer;
                               ADiversionDemands : TDivMonthlyDoublesArray;
                               ADivertedFlows    : TDivMonthlyDoublesArray): WordBool;
    function PopulateType3 (AFeatureID                : integer;
                            AFeatureName              : WideString;
                            AChannelNr                : integer;
                            AFeatureType              : integer;
                            AFeatureSubType           : integer;
                            ADiversionType            : integer;
                            AControllingReservoir     : IReservoirData;
                            AReservoirElevationsCount : integer;
                            AReferenceFlowsCount      : integer;
                            AReservoirElevations      : TReservoirElevationsArray;
                            AReferenceFlows           : TReferenceFlowsArray;
                            ADivertedFlowProportions  : TDivertedFlowProportionsArray): WordBool;
    function Get_FeatureID : integer; safecall;
    function Get_FeatureName : WideString; safecall;
    procedure Set_FeatureName (const AName : WideString); safecall;
    function Get_Station : WideString; safecall;
    procedure Set_Station (const Value : WideString);safecall;
    function Get_Channel : IGeneralFlowChannel; safecall;
    procedure Set_Channel (const AChannel : IGeneralFlowChannel); safecall;
    function Get_FeatureType : integer; safecall;
    procedure Set_FeatureType (AType : integer); safecall;
    function Get_FeatureSubType : integer; safecall;
    procedure Set_FeatureSubType (ASubType : integer); safecall;
    function Get_DiversionType : integer; safecall;
    procedure Set_DiversionType (AType : integer); safecall;
    function Get_DiversionDemandsArray : TDivMonthlyDoublesArray; safecall;
    function Get_DiversionDemandByIndex (AIndex : integer) : double; safecall;
    procedure Set_DiversionDemandByIndex (AIndex : integer;
                                          AValue : double); safecall;
    function Get_DivertedFlowsArray : TDivMonthlyDoublesArray; safecall;
    function Get_DivertedFlowByIndex (AIndex : integer) : double; safecall;
    procedure Set_DivertedFlowByIndex (AIndex : integer;
                                       AValue : double); safecall;
    function Get_ControllingReservoir : IReservoirData; safecall;
    procedure Set_ControllingReservoir (const AReservoir : IReservoirData); safecall;
    function Get_ReservoirElevationsCount : integer; safecall;
    procedure Set_ReservoirElevationsCount(ACount : integer); safecall;
    function Get_ReferenceFlowsCount : integer; safecall;
    procedure Set_ReferenceFlowsCount(ACount : integer); safecall;
    function Get_ReferenceFlowsArray : TReferenceFlowsArray; safecall;
    function Get_ReferenceFlowByIndex (AIndex : integer) : double; safecall;
    procedure Set_ReferenceFlowByIndex (AIndex : integer;
                                        AValue : double); safecall;
    function Get_ReservoirElevationsArray : TReservoirElevationsArray; safecall;
    function Get_ReservoirElevationByIndex (AIndex : integer) : double; safecall;
    procedure Set_ReservoirElevationByIndex (AIndex : integer;
                                             AValue : double); safecall;
    function Get_DivertedFlowProportionsArray : TDivertedFlowProportionsArray; safecall;
    function Get_DivertedFlowProportion (AFlowIndex      : integer;
                                        AElevationIndex : integer) : double; safecall;
    procedure Set_DivertedFlowProportion (AFlowIndex      : integer;
                                          AElevationIndex : integer;
                                          AValue          : double); safecall;
    function Type2and4RowCount : integer; safecall;
    function InsertRow (AIndex : integer) : WordBool; safecall;
    function DeleteRow (AIndex : integer) : WordBool; safecall;
    function ImportedType2or4RelationshipFromPreProcessor(AStation: Integer) : WordBool; safecall;
    function Validate (var AErrors    : WideString;
                       const AContext : WideString) : WordBool; safecall;
    function GetKeyValues (const AParamField : WideString; const AFieldIndex: WideString) : WideString; safecall;
    property FeatureID      : integer read Get_FeatureID;
    property FeatureName    : WideString read Get_FeatureName write Set_FeatureName;
    property Channel        : IGeneralFlowChannel read Get_Channel write Set_Channel;
    property FeatureType    : integer read Get_FeatureType write Set_FeatureType;
    property FeatureSubType : integer read Get_FeatureSubType write Set_FeatureSubType;
    property DiversionType  : integer
      read Get_DiversionType write Set_DiversionType;
    property DiversionDemandsArray : TDivMonthlyDoublesArray
      read Get_DiversionDemandsArray;
    property DiversionDemandByIndex[AIndex : integer] : double
      read Get_DiversionDemandByIndex write Set_DiversionDemandByIndex;
    property DivertedFlowsArray : TDivMonthlyDoublesArray
      read Get_DivertedFlowsArray;
    property DivertedFlowByIndex[AIndex : integer] : double
      read Get_DivertedFlowByIndex write Set_DivertedFlowByIndex;
    property ControllingReservoir : IReservoirData
      read Get_ControllingReservoir write Set_ControllingReservoir;
    property ReservoirElevationsCount : integer
      read Get_ReservoirElevationsCount write Set_ReservoirElevationsCount;
    property ReservoirElevationsArray : TReservoirElevationsArray
      read Get_ReservoirElevationsArray;
    property ReservoirElevationByIndex[AIndex : integer] : double
      read Get_ReservoirElevationByIndex write Set_ReservoirElevationByIndex;
    property ReferenceFlowsCount : integer
      read Get_ReferenceFlowsCount write Set_ReferenceFlowsCount;
    property ReferenceFlowsArray : TReferenceFlowsArray
      read Get_ReferenceFlowsArray;
    property ReferenceFlowByIndex[AIndex : integer] : double
      read Get_ReferenceFlowByIndex write Set_ReferenceFlowByIndex;
    property DivertedFlowProportionsArray : TDivertedFlowProportionsArray
      read Get_DivertedFlowProportionsArray;
    property DivertedFlowProportion[AFlowIndex, AElevationIndex : integer] : double
      read Get_DivertedFlowProportion write Set_DivertedFlowProportion;
    property Station: WideString read Get_Station write Set_Station;

  end;

  TDiversionFeatureList = class(TAbstractAppObject, IDiversionFeatureList)
  protected
    FDiversionFeatureList : TList;
    FDiversionGaugeList : TStringList;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function AddDiversionFeature (AFeature : TDiversionFeature): boolean;
  public
    function Initialise : boolean; override;
    function NewDiversionFeature : TDiversionFeature;
    function CreateNewDiversionFeature : TDiversionFeature;
    function DeleteDiversionFeatureWithID (AFeatureID : integer) : WordBool;
    function DeleteDiversionFeatureWithIndex (AIndex : integer) : WordBool;
    function CastDiversionFeatureByIndex(AIndex: integer): TDiversionFeature;
    function CastDiversionFeatureByID(AFeatureID : integer): TDiversionFeature;
    function CastDiversionFeatureByChannelNr(AChannelNr : integer): TDiversionFeature;
    function CopyDiversionFeature(ANewChannelNumber: Integer; AOldChannelNumber: Integer): IDiversionFeature; safecall;
    function CreateDiversionFeature : IDiversionFeature; safecall;
    function RemoveDiversionFeatureWithID (AFeatureID : integer) : WordBool; safecall;
    function Get_DiversionFeatureByIndex(AIndex: integer): IDiversionFeature; safecall;
    function Get_DiversionFeatureByID (AFeatureID : integer): IDiversionFeature; safecall;
    function Get_DiversionFeatureCount : integer; safecall;
    procedure Set_DiversionGaugeList(const AValue : WideString); safecall;
    function Get_DiversionGaugeList : WideString; safecall;
    function GetStationIDByName(const AValue : WideString) : integer;safecall;
    function Validate (var AErrors : WideString;
                       const AContext    : WideString) : WordBool; safecall;
    property DiversionFeatureByIndex[AIndex : integer]: IDiversionFeature
      read Get_DiversionFeatureByIndex;
    property DiversionFeatureByID[AFeatureID: integer]: IDiversionFeature
      read Get_DiversionFeatureByID;
    property DiversionFeatureCount: integer read Get_DiversionFeatureCount;
    property DiversionGaugeList : WideString read Get_DiversionGaugeList;
  end;

implementation

uses
  SysUtils,
  Math,
  UConstants,
  UYieldModelDataObject,
  UNetworkFeaturesSQLAgent,
  UErrorHandlingOperations;

{******************************************************************************}
{ TDiversionFeature                                                            }
{******************************************************************************}

function TDiversionFeature._AddRef: Integer;
const OPNAME = 'TDiversionFeature._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeature._Release: Integer;
const OPNAME = 'TDiversionFeature._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDiversionFeature.CreateMemberObjects;
const OPNAME = 'TDiversionFeature.CreateMemberObjects';
var
  LDiversionDemands,
  LDivertedFlows,
  LReservoirElevations,
  LReferenceFlows,
  LDivertedFlowProportions: TAbstractFieldProperty;
begin
  inherited;
  try
    LDiversionDemands := FAppModules.FieldProperties.FieldProperty('DiversionDemand');
    if (LDiversionDemands = nil) then
      raise Exception.Create('Field (DiversionDemand) not found in field properties');
    SetLength(FDiversionDemands,LDiversionDemands.ArrayLength);

    LDivertedFlows := FAppModules.FieldProperties.FieldProperty('DiversionFlow');
    if (LDivertedFlows = nil) then
      raise Exception.Create('Field (DiversionFlow) not found in field properties');
    SetLength(FDivertedFlows,LDivertedFlows.ArrayLength);

    LReservoirElevations := FAppModules.FieldProperties.FieldProperty('ControllingResLevels');
    if (LReservoirElevations = nil) then
      raise Exception.Create('Field (ControllingResLevels) not found in field properties');
    SetLength(FReservoirElevations,LReservoirElevations.ArrayLength);

    LReferenceFlows := FAppModules.FieldProperties.FieldProperty('FlowValue');
    if (LReferenceFlows = nil) then
      raise Exception.Create('Field (FlowValue) not found in field properties');
    SetLength(FReferenceFlows,LReferenceFlows.ArrayLength);

    LDivertedFlowProportions := FAppModules.FieldProperties.FieldProperty('DivertedFlow');
    if (LDivertedFlowProportions = nil) then
      raise Exception.Create('Field (DivertedFlow) not found in field properties');
    SetLength(FDivertedFlowProportions,LDivertedFlowProportions.ArrayLength, LDivertedFlowProportions.ArrayLength(1));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDiversionFeature.DestroyMemberObjects;
const OPNAME = 'TDiversionFeature.DestroyMemberObjects';
begin
  try
    Finalize(FDiversionDemands);
    Finalize(FDivertedFlows);
    Finalize(FReservoirElevations);
    Finalize(FReferenceFlows);
    Finalize(FDivertedFlowProportions);
    inherited
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeature.Initialise: boolean;
const OPNAME = 'TDiversionFeature.Initialise';
var
  nIndexA : integer;
  nIndexB : integer;
  LDiversionDemands,
  LDivertedFlows,
  LReservoirElevations,
  LReferenceFlows:TAbstractFieldProperty;
begin
  Result := inherited Initialise;
  try
    FChannelNr              := 0;
    FFeatureID              := -1;
    FFeatureName            := '';
    FStation                := '';
    FFeatureType            := -1;
    FFeatureSubType         := -1;
    FControllingReservoirNr := -1;
    FDiversionType          := -1;
    LDiversionDemands := FAppModules.FieldProperties.FieldProperty('DiversionDemand');
    for nIndexA := LDiversionDemands.ArrayLow to LDiversionDemands.ArrayHigh do
      FDiversionDemands[nIndexA] := NullFloat;
    LDivertedFlows := FAppModules.FieldProperties.FieldProperty('DiversionFlow');
    for nIndexA := LDivertedFlows.ArrayLow to LDivertedFlows.ArrayHigh do
      FDivertedFlows[nIndexA]   := NullFloat;
    LReservoirElevations := FAppModules.FieldProperties.FieldProperty('ControllingResLevels');
    for nIndexA := LReservoirElevations.ArrayLow to LReservoirElevations.ArrayHigh do
      FReservoirElevations[nIndexA] := NullFloat;
    LReferenceFlows := FAppModules.FieldProperties.FieldProperty('FlowValue');
    for nIndexA := LReferenceFlows.ArrayLow to LReferenceFlows.ArrayHigh do
    begin
      FReferenceFlows[nIndexA] := NullFloat;
      for nIndexB := LReservoirElevations.ArrayLow to LReservoirElevations.ArrayHigh do
        FDivertedFlowProportions[nIndexA, nIndexB] := NullFloat;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeature.Get_FeatureID : integer;
const OPNAME = 'TDiversionFeature.Get_FeatureID';
begin
  Result := 0;
  try
    Result := FFeatureID;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeature.Get_FeatureName : WideString;
const OPNAME = 'TDiversionFeature.Get_FeatureName';
begin
  Result := '';
  try
    Result := FFeatureName;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeature.Get_Station : WideString;
const OPNAME = 'TDiversionFeature.Get_Station';
begin
  Result := '';
  try
    Result := FStation;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeature.Get_Channel : IGeneralFlowChannel;
const OPNAME = 'TDiversionFeature.Get_Channel';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                ChannelList.ChannelByChannelNumber[FChannelNr];
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeature.Get_FeatureType : integer;
const OPNAME = 'TDiversionFeature.Get_FeatureType';
begin
  Result := 0;
  try
    Result := FFeatureType;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeature.Get_FeatureSubType : integer;
const OPNAME = 'TDiversionFeature.Get_FeatureSubType';
begin
  Result := 0;
  try
    Result := FFeatureSubType;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TDiversionFeature.Set_FeatureType (AType : integer);
const OPNAME = 'TDiversionFeature.Set_FeatureType';
var
  //LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LOldValue: integer;
begin
  try
    //LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    //try
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
    //finally
    //  LLoadAgent.Free;
    //end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDiversionFeature.Set_FeatureSubType (ASubType : integer);
const OPNAME = 'TDiversionFeature.Set_FeatureSubType';
var
  //LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LOldValue: integer;
begin
  try
    //LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    //try
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
    //finally
    //  LLoadAgent.Free;
    //end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeature.Populate (AFeatureID      : integer;
                                     AFeatureName    : WideString;
                                     AChannelNr      : integer;
                                     AFeatureType    : integer;
                                     AFeatureSubType : integer;
                                     ADiversionType  : integer): WordBool;
const OPNAME = 'TDiversionFeature.Populate';
begin
  Result := FALSE;
  try
    FChannelNr      := AChannelNr;
    FFeatureID      := AFeatureID;
    FFeatureName    := AFeatureName;
    FFeatureType    := AFeatureType;
    FFeatureSubType := AFeatureSubType;
    FDiversionType  := ADiversionType;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeature.PopulateType1or2
                               (AFeatureID        : integer;
                                AFeatureName      : WideString;
                                AStation          : WideString;
                                AChannelNr        : integer;
                                AFeatureType      : integer;
                                AFeatureSubType   : integer;
                                ADiversionType    : integer;
                                ADiversionDemands : TDivMonthlyDoublesArray;
                                ADivertedFlows    : TDivMonthlyDoublesArray): WordBool;
const OPNAME = 'TDiversionFeature.PopulateType1or2';
var
  lIndex : integer;
  LDiversionDemands: TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    LDiversionDemands := FAppModules.FieldProperties.FieldProperty('DiversionDemand');
    if (Populate(AFeatureID, AFeatureName, AChannelNr, AFeatureType,
                 AFeatureSubType, ADiversionType)) then
    begin
      FStation := AStation;
      for lIndex := LDiversionDemands.ArrayLow to LDiversionDemands.ArrayHigh do
      begin
        FDiversionDemands[lIndex] := ADiversionDemands[lIndex];
        FDivertedFlows[lIndex]    := ADivertedFlows[lIndex];
      end;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeature.PopulateType3
                        (AFeatureID                : integer;
                         AFeatureName              : WideString;
                         AChannelNr                : integer;
                         AFeatureType              : integer;
                         AFeatureSubType           : integer;
                         ADiversionType            : integer;
                         AControllingReservoir     : IReservoirData;
                         AReservoirElevationsCount : integer;
                         AReferenceFlowsCount      : integer;
                         AReservoirElevations      : TReservoirElevationsArray;
                         AReferenceFlows           : TReferenceFlowsArray;
                         ADivertedFlowProportions  : TDivertedFlowProportionsArray): WordBool;
const OPNAME = 'TDiversionFeature.PopulateType3';
var
  lIndexA : integer;
  lIndexB : integer;
  LReservoirElevations,
  LReferenceFlows: TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    if (Populate(AFeatureID, AFeatureName, AChannelNr, AFeatureType,
                 AFeatureSubType, ADiversionType)) then
    begin
      if (AControllingReservoir <> nil) then
        FControllingReservoirNr := AControllingReservoir.ReservoirConfigurationData.ReservoirIdentifier
      else
        FControllingReservoirNr := -1;
      LReservoirElevations := FAppModules.FieldProperties.FieldProperty('ControllingResLevels');
      for lIndexA := LReservoirElevations.ArrayLow to LReservoirElevations.ArrayHigh do
        FReservoirElevations[lIndexA] := AReservoirElevations[lIndexA];
      LReferenceFlows := FAppModules.FieldProperties.FieldProperty('FlowValue');
      for lIndexA := LReferenceFlows.ArrayLow to LReferenceFlows.ArrayHigh do
        FReferenceFlows[lIndexA] := AReferenceFlows[lIndexA];
      for lIndexA := LReferenceFlows.ArrayLow to LReferenceFlows.ArrayHigh do
        for lIndexB := LReservoirElevations.ArrayLow to LReservoirElevations.ArrayHigh do
          FDivertedFlowProportions[lIndexA, lIndexB] := ADivertedFlowProportions[lIndexA, lIndexB];
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeature.Get_DiversionType : integer;
const OPNAME = 'TDiversionFeature.Get_DiversionType';
begin
  Result := 0;
  try
    Result := FDiversionType;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeature.Get_DiversionDemandsArray : TDivMonthlyDoublesArray;
const OPNAME = 'TDiversionFeature.Get_DiversionDemandsArray';
begin
  Result := FDiversionDemands;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeature.Get_DiversionDemandByIndex (AIndex : integer) : double;
const OPNAME = 'TDiversionFeature.Get_DiversionDemandByIndex';
var
  LDiversionDemands: TAbstractFieldProperty;
begin
  Result := 0.0;
  try
    LDiversionDemands := FAppModules.FieldProperties.FieldProperty('DiversionDemand');
    if (AIndex >= LDiversionDemands.ArrayLow) AND (AIndex <= LDiversionDemands.ArrayHigh) then
      Result := FDiversionDemands[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeature.Get_DivertedFlowsArray : TDivMonthlyDoublesArray;
const OPNAME = 'TDiversionFeature.Get_DivertedFlowsArray';
begin
  Result := FDivertedFlows;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeature.Get_DivertedFlowByIndex (AIndex : integer) : double;
const OPNAME = 'TDiversionFeature.Get_DivertedFlowByIndex';
var
  LDivertedFlows: TAbstractFieldProperty;
begin
  Result := 0.0;
  try
    LDivertedFlows := FAppModules.FieldProperties.FieldProperty('DiversionFlow');
    if (AIndex >= LDivertedFlows.ArrayLow) AND (AIndex <= LDivertedFlows.ArrayHigh) then
      Result := FDivertedFlows[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeature.Get_ControllingReservoir : IReservoirData;
const OPNAME = 'TDiversionFeature.Get_ControllingReservoir';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                ReservoirList.ReservoirOrNodeByIdentifier[FControllingReservoirNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeature.Get_ReservoirElevationsCount : integer;
const OPNAME = 'TDiversionFeature.Get_ReservoirElevationsCount';
var
  lIndex    : integer;
  lCount    : integer;
  lContinue : Boolean;
  LReservoirElevations: TAbstractFieldProperty;
begin
  Result := 0;
  try
    LReservoirElevations := FAppModules.FieldProperties.FieldProperty('ControllingResLevels');
    lIndex    := LReservoirElevations.ArrayLow;
    lContinue := TRUE;
    lCount    := 0;
    while (lContinue AND (lIndex <= LReservoirElevations.ArrayHigh)) do
    begin
      if (FReservoirElevations[lIndex] = NullFloat) then
        lContinue := FALSE
      else
      begin
        lCount := lCount + 1;
        lIndex := lIndex + 1;
      end;
    end;
    Result := lCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeature.Get_ReferenceFlowsCount : integer;
const OPNAME = 'TDiversionFeature.Get_ReferenceFlowsCount';
var
  lIndex    : integer;
  lCount    : integer;
  lContinue : Boolean;
  LReferenceFlows: TAbstractFieldProperty;
begin
  Result := 0;
  try
    LReferenceFlows := FAppModules.FieldProperties.FieldProperty('FlowValue');
    lIndex    := LReferenceFlows.ArrayLow;
    lContinue := TRUE;
    lCount    := 0;
    while (lContinue AND (lIndex <= LReferenceFlows.ArrayHigh)) do
    begin
      if (FReferenceFlows[lIndex] = NullFloat) then
        lContinue := FALSE
      else
      begin
        lCount := lCount + 1;
        lIndex := lIndex + 1;
      end;
    end;
    Result := lCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeature.Get_ReferenceFlowsArray : TReferenceFlowsArray;
const OPNAME = 'TDiversionFeature.Get_ReferenceFlowsArray';
begin
  Result := FReferenceFlows;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeature.Get_ReferenceFlowByIndex (AIndex : integer) : double;
const OPNAME = 'TDiversionFeature.Get_ReferenceFlowByIndex';
var
  LReferenceFlows: TAbstractFieldProperty;
begin
  Result := 0.0;
  try
    LReferenceFlows := FAppModules.FieldProperties.FieldProperty('FlowValue');
    if (AIndex >= LReferenceFlows.ArrayLow) AND (AIndex <= LReferenceFlows.ArrayHigh) then
      Result := FReferenceFlows[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeature.Get_ReservoirElevationsArray : TReservoirElevationsArray;
const OPNAME = 'TDiversionFeature.Get_ReservoirElevationsArray';
begin
  Result := FReservoirElevations;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeature.Get_ReservoirElevationByIndex (AIndex : integer) : double;
const OPNAME = 'TDiversionFeature.Get_ReservoirElevationByIndex';
var
  LReservoirElevations: TAbstractFieldProperty;
begin
  Result := 0.0;
  try
    LReservoirElevations := FAppModules.FieldProperties.FieldProperty('ControllingResLevels');
    if (AIndex >= LReservoirElevations.ArrayLow) AND (AIndex <= LReservoirElevations.ArrayHigh) then
      Result := FReservoirElevations[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeature.Get_DivertedFlowProportionsArray : TDivertedFlowProportionsArray;
const OPNAME = 'TDiversionFeature.Get_DivertedFlowProportionsArray';
begin
  Result := FDivertedFlowProportions;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeature.Get_DivertedFlowProportion (AFlowIndex      : integer;
                                                       AElevationIndex : integer) : double;
const OPNAME = 'TDiversionFeature.Get_DivertedFlowProportion';
begin
  Result := 0.0;
  try
    Result := FDivertedFlowProportions[AFlowIndex, AElevationIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDiversionFeature.Set_FeatureName (const AName : WideString);
const OPNAME = 'TDiversionFeature.Set_FeatureName';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LOldValue: string;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'DiversionChannelName', AName, FFeatureName, LContextData) then
        begin
          LOldValue := FFeatureName;
          FFeatureName := AName;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'FeatureName',LOldValue,AName);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDiversionFeature.Set_Station (const Value : WideString);
const OPNAME = 'TDiversionFeature.Set_Station';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LOldValue: string;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'DivStation', Value, FStation, LContextData) then
        begin
          LOldValue := FStation;
          FStation := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'DivStation',LOldValue,Value);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TDiversionFeature.Set_Channel (const AChannel : IGeneralFlowChannel);
const OPNAME = 'TDiversionFeature.Set_Channel';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LOldNr       : integer;
  LNewNr       : integer;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        if (AChannel <> nil) then
          LNewNr := AChannel.ChannelNumber
        else
          LNewNr := 0;
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'DiversionChannelNumber', IntToStr(LNewNr), IntToStr(FChannelNr), LContextData) then
        begin
          LOldNr     := FChannelNr;
          FChannelNr := lNewNr;
          FAppModules.Model.StudyDataHasChanged
            (sdccEdit,'DiversionChannelNumber',IntToStr(LOldNr),IntToStr(LNewNr));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDiversionFeature.Set_DiversionType (AType : integer);
const OPNAME = 'TDiversionFeature.Set_DiversionType';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  lIndex       : integer;
  LOldType     : integer;
  lResNum      : integer;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
//        LContextData.Add('Identifier='  + IntToStr(FChannelNr));
//        LContextData.Add('ChannelType=' + IntToStr(5));
        lOldType := FDiversionType;
        if FAppModules.FieldProperties.UpdateFieldValue(
          'DiversionChannelType', IntToStr(AType), IntToStr(FDiversionType), LContextData) then
        begin
          FDiversionType := AType;
          if ((AType = 3) AND ((lOldType = 1) OR (lOldType = 2))) then
          begin
            if (FControllingReservoirNr <> 0) then
              lResNum := FControllingReservoirNr
            else
              lResNum := -1;
            lLoadAgent.AddType3DiversionLevels(FFeatureID, lResNum, ReservoirElevationsCount, ReferenceFlowsCount);
            lLoadAgent.AddType3DiversionProportions(FFeatureID, ReferenceFlowsCount);
          end
          else
          if (((AType = 1) OR (AType = 2)) AND (lOldType = 3)) then
          begin
            lLoadAgent.AddType1_2DiversionData(FFeatureID);
            for lIndex := 1 to 12 do
            begin
              if (FDiversionDemands[lIndex] = NullFloat) then
                Set_DiversionDemandByIndex(lIndex, 0);
              if (FDivertedFlows[lIndex] = NullFloat) then
                Set_DivertedFlowByIndex(lIndex, 0);
            end;
          end
          else
          if ((AType = 1) AND (lOldType = 2)) then
          begin
            for lIndex := 1 to 12 do
            begin
              if (FDiversionDemands[lIndex] = NullFloat) then
                Set_DiversionDemandByIndex(lIndex, 0);
              if (FDivertedFlows[lIndex] = NullFloat) then
                Set_DivertedFlowByIndex(lIndex, 0);
            end;
          end;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'DiversionChannelType',IntToStr(LOldType),IntToStr(AType));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDiversionFeature.Set_DiversionDemandByIndex (AIndex : integer;
                                                        AValue : double);
const OPNAME = 'TDiversionFeature.Set_DiversionDemandByIndex';
var
  lIndex : integer;
  LDiversionDemands: TAbstractFieldProperty;
begin
  try
    LDiversionDemands := FAppModules.FieldProperties.FieldProperty('DiversionDemand');
    if (AIndex >= LDiversionDemands.ArrayLow) and (AIndex <= LDiversionDemands.ArrayHigh) then
    begin
      if (FDiversionDemands[AIndex] <> AValue) then
      begin
        if (AValue = NullFloat) then
        begin
          for lIndex := AIndex+1 to LDiversionDemands.ArrayHigh do
          begin
            WriteDiversionDemandToDB (lIndex-1, FDiversionDemands[lIndex]);
            WriteDivertedFlowToDB (lIndex-1, FDivertedFlows[lIndex]);
          end;
          WriteDiversionDemandToDB (LDiversionDemands.ArrayHigh, NullFloat);
          WriteDivertedFlowToDB (LDiversionDemands.ArrayHigh, NullFloat);
        end
        else
        begin
          for lIndex := 1 to AIndex-1 do
          begin
            if (DiversionDemandByIndex[lIndex] = NullFloat) then
              WriteDiversionDemandToDB(lIndex, 0.0);
            if (DivertedFlowByIndex[lIndex] = NullFloat) then
              WriteDivertedFlowToDB (lIndex, 0.0);
          end;
          WriteDiversionDemandToDB(AIndex, AValue);
          if (DivertedFlowByIndex[AIndex] = NullFloat) then
            WriteDivertedFlowToDB (AIndex, 0.0);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDiversionFeature.WriteDiversionDemandToDB (AIndex : integer;
                                                      AValue : double);
const OPNAME = 'TDiversionFeature.WriteDiversionDemandToDB';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  lOldValue    : string;
  lNewValue    : string;
  LPrevValue    : double;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureIDFieldNameID
          (LContextData,IntToStr(FFeatureID), IntToStr(AIndex));
        LContextData.Add('DiversionCode=1');
        if (AValue = NullFloat) then
          lNewValue := ''
        else
          lNewValue := FloatToStr(AValue);
        if (FDiversionDemands[AIndex] = NullFloat) then
          lOldValue := ''
        else
          lOldValue := FloatToStr(FDiversionDemands[AIndex]);
        if FAppModules.FieldProperties.UpdateFieldValue(
          'DiversionDemand', lNewValue, lOldValue, LContextData) then
        begin
          LPrevValue := FDiversionDemands[AIndex];
          FDiversionDemands[AIndex] := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'DiversionDemand', FloatToStr(LPrevValue), FloatToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDiversionFeature.Set_DivertedFlowByIndex (AIndex : integer;
                                                     AValue : double);
const OPNAME = 'TDiversionFeature.Set_DivertedFlowByIndex';
var
  lIndex : integer;
  LDivertedFlows: TAbstractFieldProperty;
begin
  try
    LDivertedFlows := FAppModules.FieldProperties.FieldProperty('DiversionFlow');
    if (AIndex >= LDivertedFlows.ArrayLow) and (AIndex <= LDivertedFlows.ArrayHigh) then
    begin
      if (FDivertedFlows[AIndex] <> AValue) then
      begin
        if (AValue = NullFloat) then
        begin
          if (FDiversionType = 1) then
          begin
            WriteDivertedFlowToDB (AIndex, AValue);
          end
          else
          begin
            for lIndex := AIndex+1 to LDivertedFlows.ArrayHigh do
            begin
              WriteDiversionDemandToDB (lIndex-1, FDiversionDemands[lIndex]);
              WriteDivertedFlowToDB (lIndex-1, FDivertedFlows[lIndex]);
            end;
            WriteDiversionDemandToDB (LDivertedFlows.ArrayHigh, NullFloat);
            WriteDivertedFlowToDB (LDivertedFlows.ArrayHigh, NullFloat);
          end;
        end
        else
        begin
          for lIndex := 1 to AIndex-1 do
          begin
            if (DiversionDemandByIndex[lIndex] = NullFloat) then
              WriteDiversionDemandToDB(lIndex, 0.0);
            if (DivertedFlowByIndex[lIndex] = NullFloat) then
              WriteDivertedFlowToDB (lIndex, 0.0);
          end;
          WriteDivertedFlowToDB(AIndex, AValue);
          if (DiversionDemandByIndex[AIndex] = NullFloat) then
            WriteDiversionDemandToDB (AIndex, 0.0);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDiversionFeature.WriteDivertedFlowToDB (AIndex : integer;
                                                   AValue : double);
const OPNAME = 'TDiversionFeature.WriteDivertedFlowToDB';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  lNewValue    : string;
  lOldValue    : string;
  LPrevValue   : double;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureIDFieldNameID
          (LContextData,IntToStr(FFeatureID), IntToStr(AIndex));
        LContextData.Add('DiversionCode=2');
        if ((AValue = NullFloat) AND (FDiversionType <> 1)) then
          lNewValue := ''
        else
          lNewValue := FloatToStr(AValue);
        if (FDivertedFlows[AIndex] = NullFloat) then
          lOldValue := ''
        else
          lOldValue := FloatToStr(FDivertedFlows[AIndex]);
        if FAppModules.FieldProperties.UpdateFieldValue(
          'NetNaturalInflow', lNewValue, lOldValue, LContextData) then
        begin
          LPrevValue := FDivertedFlows[AIndex];
          FDivertedFlows[AIndex] := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'NetNaturalInflow', FloatToStr(LPrevValue), FloatToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDiversionFeature.Set_ControllingReservoir (const AReservoir : IReservoirData);
const OPNAME = 'TDiversionFeature.Set_ControllingReservoir';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  lOldNumber   : string;
  lNewNumber   : string;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        lOldNumber := '';
        lNewNumber := '';
        if (AReservoir <> nil) then
          lNewNumber := IntToStr(AReservoir.ReservoirConfigurationData.ReservoirIdentifier);
        if (FControllingReservoirNr <> 0) then
          lOldNumber := IntToStr(FControllingReservoirNr);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
          'ControllingResNodeNumber', lNewNumber, lOldNumber, LContextData) then
        begin
          FControllingReservoirNr := AReservoir.ReservoirConfigurationData.ReservoirIdentifier;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ControllingResNodeNumber', lOldNumber, lNewNumber);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDiversionFeature.Set_ReservoirElevationByIndex (AIndex : integer;
                                                           AValue : double);
const OPNAME = 'TDiversionFeature.Set_ReservoirElevationByIndex';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  lOldValue    : string;
  lNewValue    : string;
  LPrevValue   : double;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureIDFieldNameID
          (LContextData, IntToStr(FFeatureID), IntToStr(AIndex));
        if (AValue = NullFloat) then
          lNewValue := ''
        else
          lNewValue := FloatToStr(AValue);
        if (FReservoirElevations[AIndex] = NullFloat) then
          lOldValue := ''
        else
          lOldValue := FloatToStr(FReservoirElevations[AIndex]);
        if FAppModules.FieldProperties.UpdateFieldValue(
          'ControllingResLevels', lNewValue, lOldValue, LContextData) then
        begin
          LPrevValue := FReservoirElevations[AIndex];
          FReservoirElevations[AIndex] := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ControllingResLevels',FloatToStr(LPrevValue),FloatToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDiversionFeature.Set_ReferenceFlowsCount(ACount : integer);
const OPNAME = 'TDiversionFeature.Set_ReferenceFlowsCount';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  lOldCount    : integer;
  lFlow        : integer;
  lLevel       : integer;
  LPrevValue   : integer;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        lOldCount := ReferenceFlowsCount;
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
          'ReferenceFlowsCount', IntToStr(ACount), IntToStr(lOldCount), LContextData) then
        begin
          LPrevValue := ReferenceFlowsCount;
          if (ACount > lOldCount) then
          begin
            for lFlow := lOldCount + 1 to ACount do
            begin
              lLoadAgent.AddDiversionProportions(FFeatureID, lFlow);
              Set_ReferenceFlowByIndex(lFlow, 0.0);
              for lLevel := 1 to ReservoirElevationsCount do
                Set_DivertedFlowProportion(lFlow, lLevel, 0.0);
            end;
          end
          else
          if (ACount < lOldCount) then
          begin
            for lFlow := lOldCount downto ACount + 1  do
            begin
              FReferenceFlows[lFlow] := NullFloat;
              for lLevel := 1 to ReservoirElevationsCount do
                FDivertedFlowProportions[lFlow, lLevel] := NullFloat;
              lLoadAgent.DeleteDiversionProportions(FFeatureID, lFlow);
            end;
          end;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ReferenceFlowsCount',IntToStr(LPrevValue),IntToStr(ACount));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDiversionFeature.Set_ReservoirElevationsCount(ACount : integer);
const OPNAME = 'TDiversionFeature.Set_ReservoirElevationsCount';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  lOldCount    : integer;
  lLevel       : integer;
  lFlow        : integer;
  LPrevValue   : integer;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        lOldCount := ReservoirElevationsCount;
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
          'ReservoirStorageNumber', IntToStr(ACount), IntToStr(ReservoirElevationsCount), LContextData) then
        begin
          LPrevValue := ReservoirElevationsCount;
          if (ACount > lOldCount) then
          begin
            for lLevel := lOldCount + 1 to ACount do
            begin
              Set_ReservoirElevationByIndex(lLevel, 0.0);
              for lFlow := 1 to ReferenceFlowsCount do
                Set_DivertedFlowProportion(lFlow, lLevel, 0.0);
            end;
          end
          else
          if (ACount < lOldCount) then
          begin
            for lLevel := lOldCount downto ACount + 1  do
            begin
              Set_ReservoirElevationByIndex(lLevel, NullFloat);
              for lFlow := 1 to ReferenceFlowsCount do
                Set_DivertedFlowProportion(lFlow, lLevel, NullFloat);
            end;    
          end;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ReservoirStorageNumber',IntToStr(LPrevValue),IntToStr(ACount));;
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDiversionFeature.Set_ReferenceFlowByIndex (AIndex : integer;
                                                      AValue : double);
const OPNAME = 'TDiversionFeature.Set_ReferenceFlowByIndex';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  lOldValue    : string;
  lNewValue    : string;
  LPrevValue   : double;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        LContextData.Add('DiversionIndex=' + IntToStr(AIndex));
        if (AValue = NullFloat) then
          lNewValue := ''
        else
          lNewValue := FloatToStr(AValue);
        if (FReferenceFlows[AIndex] = NullFloat) then
          lOldValue := ''
        else
          lOldValue := FloatToStr(FReferenceFlows[AIndex]);
        if FAppModules.FieldProperties.UpdateFieldValue(
          'FlowValue', lNewValue, lOldValue, LContextData) then
        begin
          LPrevValue := FReferenceFlows[AIndex];
          FReferenceFlows[AIndex] := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'FlowValue',FloatToStr(LPrevValue),FloatToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDiversionFeature.Set_DivertedFlowProportion (AFlowIndex      : integer;
                                                        AElevationIndex : integer;
                                                        AValue          : double);
const OPNAME = 'TDiversionFeature.Set_DivertedFlowProportion';
var
  LLoadAgent    : TNetworkFeaturesSQLAgent;
  LContextData  : TStringList;
  lOldValue     : string;
  lNewValue     : string;
  LPrevValue    : double;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureIDFieldNameID
          (LContextData, IntToStr(FFeatureID), IntToStr(AElevationIndex));
        LContextData.Add('DiversionIndex=' + IntToStr(AFlowIndex));
        if (AValue = NullFloat) then
          lNewValue := ''
        else
          lNewValue := FloatToStr(AValue);
        if (FDivertedFlowProportions[AFlowIndex, AElevationIndex] = NullFloat) then
          lOldValue := ''
        else
          lOldValue := FloatToStr(FDivertedFlowProportions[AFlowIndex, AElevationIndex]);
        if FAppModules.FieldProperties.UpdateFieldValue(
          'DivertedFlow', lNewValue, lOldValue, LContextData) then
        begin
          LPrevValue := FDivertedFlowProportions[AFlowIndex, AElevationIndex];
          FDivertedFlowProportions[AFlowIndex, AElevationIndex] := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'DivertedFlow',FloatToStr(LPrevValue),FloatToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeature.ValidateFeatureName(AErrorMessages : TStrings) : Boolean;
const OPNAME = 'TDiversionFeature.ValidateFeatureName';
var
  lIndex       : integer;
  lFeatureList : TDiversionFeatureList;
  lFeature     : TDiversionFeature;
  lUnique      : Boolean;
  lMessage     : string;
begin
  Result := True;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('DiversionChannelName', FFeatureName, lMessage)) then
      AErrorMessages.Add('WARNING:' +lMessage)
    else
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        CastNetworkFeaturesData.CastDiversionFeatureList;
      lUnique := TRUE;
      lIndex  := 0;
      while (lUnique AND (lIndex < lFeatureList.DiversionFeatureCount)) do
      begin
        lFeature := lFeatureList.CastDiversionFeatureByIndex(lIndex);
        if ((lFeature.FeatureID <> FFeatureID) AND
            (UpperCase(Trim(lFeature.FeatureName)) = UpperCase(Trim(FFeatureName)))) then
        begin
          lMessage := FAppModules.Language.GetString('ContextValidation.DuplicateDiversionFeatureName');
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

function TDiversionFeature.ValidateStation(AErrorMessages : TStrings) : Boolean;
const OPNAME = 'TDiversionFeature.ValidateStation';
var
  lMessage     : string;
begin
  Result := True;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty ('DivStation', FStation, lMessage)) then
      AErrorMessages.Add('WARNING:' +lMessage);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeature.ValidateDiversionType(AErrorMessages : TStrings) : Boolean;
const OPNAME = 'TDiversionFeature.ValidateDiversionType';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty
                ('DiversionChannelType', IntToStr(FDiversionType), lMessage)) then
      AErrorMessages.Add('ERROR:' +lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeature.ValidateType1DiversionDemands(AErrorMessages: TStrings; AErrorColumns: TStringList): Boolean;
const OPNAME = 'TDiversionFeature.ValidateType1DiversionDemands';
var
  lIndex            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lResult           : Boolean;
  LDiversionDemands: TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    LDiversionDemands := FAppModules.FieldProperties.FieldProperty('DiversionDemand');
    if (FDiversionType = 1) then
    begin
      lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
      for lIndex := LDiversionDemands.ArrayLow to LDiversionDemands.ArrayHigh do
      begin
        lMessage := '';
        if (NOT FAppModules.FieldProperties.ValidateFieldProperty
                    ('DiversionDemand', FloatToStr(FDiversionDemands[lIndex]),
                     lMessage, lIndex)) then
        begin
          lResult := FALSE;
          AErrorMessages.Add('ERROR:' +lMessage);
          AErrorColumns.Add(IntToStr(lIndex));
          if (lStopOnFirstError) then
            Break;
        end;
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeature.ValidateType1NetNaturalInflows(AErrorMessages: TStrings;AErrorColumns: TStringList): Boolean;
const OPNAME = 'TDiversionFeature.ValidateType1NetNaturalInflows';
var
  lIndex            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lResult           : Boolean;
  LDivertedFlows    : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    LDivertedFlows := FAppModules.FieldProperties.FieldProperty('DiversionFlow');
    if (FDiversionType = 1) then
    begin
      lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
      for lIndex := LDivertedFlows.ArrayLow to LDivertedFlows.ArrayHigh do
      begin
        lMessage := '';
        if (NOT FAppModules.FieldProperties.ValidateFieldProperty
                    ('NetNaturalInflow', FloatToStr(FDivertedFlows[lIndex]),
                    lMessage, lIndex)) then
        begin
          lResult := FALSE;
          AErrorMessages.Add('ERROR:' +lMessage);
          AErrorColumns.Add(IntToStr(lIndex));
          if (lStopOnFirstError) then
            Break;
        end;
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionFeature.ValidateType2ActualDivertedFlows(AErrorMessages: TStrings;AErrorColumns: TStringList): Boolean;
const OPNAME = 'TDiversionFeature.ValidateType2ActualDivertedFlows';
var
  lIndex            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lContinue         : Boolean;
  lCount            : integer;
  lResult           : boolean;
  lValid            : boolean;
  LDiversionDemands : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    LDiversionDemands := FAppModules.FieldProperties.FieldProperty('DiversionDemand');
    if (FDiversionType in [2,4]) then
    begin
      lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
      lIndex    := LDiversionDemands.ArrayLow;
      lContinue := TRUE;
      lCount    := 0;
      while (lContinue AND (lIndex  <= LDiversionDemands.ArrayHigh)) do
      begin
        if ((FDiversionDemands[lIndex] = NullFloat) AND
            (FDivertedFlows[lIndex] = NullFloat)) then
          lContinue := FALSE
        else
        begin
          lCount := lCount + 1;
          lMessage := '';
          if (NOT FAppModules.FieldProperties.ValidateFieldProperty
                      ('ActualDivertedFlow', FloatToStr(FDivertedFlows[lIndex]),
                      lMessage, lIndex)) then
          begin
            lResult := FALSE;
            AErrorMessages.Add('ERROR:' +lMessage);
            AErrorColumns.Add(IntToStr(lIndex));
            if (lStopOnFirstError) then
              Break;
          end;
          lIndex := lIndex + 1;
        end;
      end;
      if (lCount < 2) then
      begin
        lResult := FALSE;
        lMessage := FAppModules.Language.GetString('ContextValidation.TooFewFlowRangesAndDivertedFlows');
        AErrorMessages.Add('ERROR:' +Format(lMessage, [Channel.ChannelName]));
      end;
      if (lResult OR (NOT lStopOnFirstError)) then
      begin
        lValid := TRUE;
        lIndex := LDiversionDemands.ArrayLow;
        while (lValid AND (lIndex < LDiversionDemands.ArrayHigh)) do
        begin
          if (FDivertedFlows[lIndex] > FDiversionDemands[lIndex]) then
          begin
            lMessage := FAppModules.Language.GetString('ContextValidation.DivertedFlowGreaterThanFlowRanges');
            AErrorMessages.Add('ERROR:' +Format(lMessage, [Channel.ChannelName]));
            AErrorColumns.Add(IntToStr(lIndex));
            lValid := FALSE;
          end
          else
            lIndex := lIndex + 1;
        end;
        if (NOT lValid) then
          lResult := FALSE;
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionFeature.ValidateType2FlowRanges(AErrorMessages: TStrings; AErrorColumns: TStringList): Boolean;
const OPNAME = 'TDiversionFeature.ValidateType2FlowRanges';
var
  lIndex            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lContinue         : Boolean;
  lCount            : integer;
  lResult           : boolean;
  lValid            : boolean;
  LDiversionDemands : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    LDiversionDemands := FAppModules.FieldProperties.FieldProperty('DiversionDemand');
    if (FDiversionType in [2,4]) then
    begin
      lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
      lIndex    := LDiversionDemands.ArrayLow;
      lContinue := TRUE;
      lCount    := 0;
      while (lContinue AND (lIndex  <= LDiversionDemands.ArrayHigh)) do
      begin
        if ((FDiversionDemands[lIndex] = NullFloat) AND
            (FDivertedFlows[lIndex] = NullFloat)) then
          lContinue := FALSE
        else
        begin
          lCount := lCount + 1;
          lMessage := '';
          if (NOT FAppModules.FieldProperties.ValidateFieldProperty
                      ('FlowRange', FloatToStr(FDiversionDemands[lIndex]),
                      lMessage, lIndex)) then
          begin
            lResult := FALSE;
            AErrorMessages.Add('ERROR:' +lMessage);
            AErrorColumns.Add(IntToStr(lIndex));
            if (lStopOnFirstError) then
              Break;
          end;
          lIndex := lIndex + 1;
        end;
      end;
      if (lResult OR (NOT lStopOnFirstError)) then
      begin
        if (lCount < 2) then
        begin
          lResult := FALSE;
          lMessage := FAppModules.Language.GetString('ContextValidation.TooFewFlowRangesAndDivertedFlows');
          AErrorMessages.Add('ERROR:' +Format(lMessage, [Channel.ChannelName]));
        end;
      end;
      if (lResult OR (NOT lStopOnFirstError)) then
      begin
        lValid := TRUE;
        lIndex := LDiversionDemands.ArrayLow;
        while (lValid AND (lIndex < LDiversionDemands.ArrayHigh)) do
        begin
          if ((FDiversionDemands[lIndex+1] <> NullFloat) AND
              (FDiversionDemands[lIndex] > FDiversionDemands[lIndex+1])) then
          begin
            lMessage := FAppModules.Language.GetString('ContextValidation.FlowRangesNotInAscendingOrder');
            AErrorMessages.Add('ERROR:' +Format(lMessage, [Channel.ChannelName]));
            AErrorColumns.Add(IntToStr(lIndex));
            lValid := FALSE;
          end
          else
            lIndex := lIndex + 1;
        end;
        if (NOT lValid) then
          lResult := FALSE;
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionFeature.ValidateControllingReservoir (AErrorMessages : TStrings) : boolean;
const OPNAME = 'TDiversionFeature.ValidateControllingReservoir';
var
  lMessage          : string;
  lValid            : Boolean;
  lResult           : Boolean;
  lResNumber        : integer;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    if (FDiversionType = 3) then
    begin
      lMessage := '';
      if (FControllingReservoirNr <> 0) then
        lResNumber := FControllingReservoirNr
      else
        lResNumber := 0;
      lValid := FAppModules.FieldProperties.ValidateFieldProperty
                  ('ControllingResNodeNumber', IntToStr(lResNumber), lMessage);
      if (NOT lValid) then
      begin
        lResult := FALSE;
        AErrorMessages.Add('ERROR:' +lMessage);
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionFeature.ValidateType3Flows(AErrorMessages: TStrings): boolean;
const OPNAME = 'TDiversionFeature.ValidateType3Flows';
var
  lFlow             : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lValid            : Boolean;
  lResult           : Boolean;
  lRefFlowsProp     : TAbstractFieldProperty;
  lIndex            : integer;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    lRefFlowsProp := FAppModules.FieldProperties.FieldProperty('FlowValue');
    if (FDiversionType = 3) then
    begin
      lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
      lMessage := '';
      lValid := FAppModules.FieldProperties.ValidateFieldProperty
                  ('ReferenceFlowsCount', IntToStr(ReservoirElevationsCount), lMessage);
      if (NOT lValid) then
      begin
        lResult := FALSE;
        AErrorMessages.Add('ERROR:' +lMessage);
      end;
      if (lResult OR (NOT lStopOnFirstError)) then
      begin
        for lFlow := 1 to ReferenceFlowsCount do
        begin
          lMessage := '';
          if (NOT FAppModules.FieldProperties.ValidateFieldProperty
                      ('FlowValue', FloatToStr(FReferenceFlows[lFlow]),
                       lMessage, lFlow)) then
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
        lValid := TRUE;
        lIndex := lRefFlowsProp.ArrayLow;
        while (lValid AND (lIndex < lRefFlowsProp.ArrayHigh)) do
        begin
          if ((FReferenceFlows[lIndex+1] <> NullFloat) AND
              (FReferenceFlows[lIndex] > FReferenceFlows[lIndex+1])) then
          begin
            lMessage := FAppModules.Language.GetString('ContextValidation.ReferenceFlowsNotInAscendingOrder');
            AErrorMessages.Add('ERROR:' +Format(lMessage, [Channel.ChannelName]));
            lValid := FALSE;
          end
          else
            lIndex := lIndex + 1;
        end;
        if (NOT lValid) then
          lResult := FALSE;
      end;

    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionFeature.ValidateType3Levels (AErrorMessages : TStrings;
                                                AErrorColumns  : TStringList): boolean;
const OPNAME = 'TDiversionFeature.ValidateType3Levels';
var
  lLevel            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lValid            : Boolean;
  lResult           : Boolean;
  lCount            : integer;
  lMinimum          : double;
  lMaximum          : double;
  lElevation        : double;
  lControl          : Boolean;
  lControlReservoir : IReservoirData;
  lResLevelsProp    : TAbstractFieldProperty;
  lIndex            : integer;
begin
  Result := FALSE;
  try
    lResLevelsProp := FAppModules.FieldProperties.FieldProperty('ControllingResLevels');
    AErrorColumns.Clear;
    lResult := TRUE;
    if (FDiversionType = 3) then
    begin
      lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
      lMessage := '';
      lValid := FAppModules.FieldProperties.ValidateFieldProperty
                  ('ReservoirStorageNumber', IntToStr(ReservoirElevationsCount), lMessage);
      if (NOT lValid) then
      begin
        lResult := FALSE;
        AErrorMessages.Add('ERROR:' +lMessage);
      end;
      if (lResult OR (NOT lStopOnFirstError)) then
      begin
        lControl := FALSE;
        if (FControllingReservoirNr <> 0) then
          lControlReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                                 ReservoirList.ReservoirOrNodeByIdentifier[FControllingReservoirNr];
        if ((FControllingReservoirNr <> 0) AND (lControlReservoir <> nil) AND
            (lControlReservoir.ReservoirConfigurationData <> nil) AND
            (lControlReservoir.ReservoirElevationsData <> nil)) then
        begin
          lCount     := lControlReservoir.ReservoirConfigurationData.PointsCount;
          lMaximum   := lControlReservoir.ReservoirElevationsData.ReservoirElevationsByIndex[1];
          lMinimum   := lControlReservoir.ReservoirElevationsData.ReservoirElevationsByIndex[lCount];
          lControl   := TRUE;
        end
        else
        begin
          lMinimum := -1;
          lMaximum := -1;
        end;
        for lLevel := 1 to ReservoirElevationsCount do
        begin
          lMessage   := '';
          lElevation := FReservoirElevations[lLevel];
          lValid := FAppModules.FieldProperties.ValidateFieldProperty
                      ('ControllingResLevels', FloatToStr(lElevation),
                       lMessage, lLevel);
          if (NOT lValid) then
          begin
            lResult := FALSE;
            AErrorMessages.Add('ERROR:' +lMessage);
            if (lStopOnFirstError) then
              Break;
          end;
          if (lResult OR (NOT lStopOnFirstError)) then
          begin
            if (lControl AND ((lElevation < lMinimum) OR (lElevation > lMaximum))) then
            begin
              lResult := FALSE;
              lMessage := FAppModules.Language.GetString('ContextValidation.ReservoirLevelsNotInPhysicalRange');
              AErrorMessages.Add('ERROR:' +Format(lMessage, [Channel.ChannelName]));
              AErrorColumns.Add(IntToStr(lLevel));
              if (lStopOnFirstError) then
                Break;
            end;
          end;

          if (lResult OR (NOT lStopOnFirstError)) then
          begin
            lValid := TRUE;
            lIndex := lResLevelsProp.ArrayLow;
            while (lValid AND (lIndex < lResLevelsProp.ArrayHigh)) do
            begin
              if ((FReservoirElevations[lIndex+1] <> NullFloat) AND
                  (FReservoirElevations[lIndex] > FReservoirElevations[lIndex+1])) then
              begin
                lMessage := FAppModules.Language.GetString('ContextValidation.ResLevelsNotInAscendingOrder');
                AErrorMessages.Add('ERROR:' +Format(lMessage, [Channel.ChannelName]));
                AErrorColumns.Add(IntToStr(lIndex));
                lValid := FALSE;
              end
              else
                lIndex := lIndex + 1;
            end;
            if (NOT lValid) then
              lResult := FALSE;
          end;


        end;
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME) end;

end;

function TDiversionFeature.ValidateType3Proportions(AErrorMessages : TStrings;
                                                    AErrorColumns  : TStringList): boolean;
const OPNAME = 'TDiversionFeature.ValidateType3Proportions';
var
  lFlow             : integer;
  lLevel            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lValid            : Boolean;
  lResult           : Boolean;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    if (FDiversionType = 3) then
    begin
      lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
      lMessage := '';
      for lFlow := 1 to ReferenceFlowsCount do
      begin
        for lLevel := 1 to ReservoirElevationsCount do
        begin
          lMessage := '';
          lValid := FAppModules.FieldProperties.ValidateFieldProperty
                      ('DivertedFlow', FloatToStr(FDivertedFlowProportions[lFlow, lLevel]),
                       lMessage, lFlow, lLevel);
          if (NOT lValid) then
          begin
            lResult := FALSE;
            AErrorMessages.Add('ERROR:' +lMessage);
            if (AErrorColumns.IndexOf(IntToStr(lLevel)) < 0) then
              AErrorColumns.Add(IntToStr(lLevel));
            if (lStopOnFirstError) then
              Break;
          end;
        end;
        if ((NOT lResult) AND lStopOnFirstError) then
          Break;
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionFeature.Validate (var AErrors    : WideString;
                                     const AContext : WideString) : WordBool;
const OPNAME = 'TDiversionFeature.Validate';
var
  lMessage          : string;
  lStopOnFirstError : Boolean;
  lErrorCols        : TStringList;
  lErrorList        : TStringList;
begin
  Result := FALSE;
  try
    lErrorCols := TStringList.Create;
    lErrorList := TStringList.Create;
    try
      if (AContext = 'FeatureName') then
        Result := ValidateFeatureName(lErrorList)
      else
      if (AContext = 'DiversionType') then
        Result := ValidateDiversionType(lErrorList)
      else
      if (AContext = 'Type1DiversionDemands') then
      begin
        Result := ValidateType1DiversionDemands(lErrorList,lErrorCols);
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
      if (AContext = 'Type1NetNaturalInflows') then
      begin
        Result := ValidateType1NetNaturalInflows(lErrorList,lErrorCols);
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
      if (AContext = 'Type2FlowRanges') then
      begin
        Result := ValidateType2FlowRanges(lErrorList,lErrorCols);
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
      if (AContext = 'Type2ActualDivertedFlows') then
      begin
        Result := ValidateType2ActualDivertedFlows(lErrorList,lErrorCols);
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
      if (AContext = 'ControllingReservoir') then
        Result := ValidateControllingReservoir(lErrorList)
      else
      if (AContext = 'Type3Flows') then
        Result := ValidateType3Flows(lErrorList)
      else
      if (AContext = 'Type3Levels') then
      begin
        Result := ValidateType3Levels(lErrorList,lErrorCols);
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
      if (AContext = 'Type3Proportions') then
      begin
        Result := ValidateType3Proportions(lErrorList,lErrorCols);
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
        if (NOT ValidateFeatureName(lErrorList)) then
          Result := FALSE;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateDiversionType(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateType1DiversionDemands(lErrorList,lErrorCols)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateType1NetNaturalInflows(lErrorList,lErrorCols)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateType2FlowRanges(lErrorList,lErrorCols)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateType2ActualDivertedFlows(lErrorList,lErrorCols)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateControllingReservoir(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateType3Flows(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateType3Levels(lErrorList,lErrorCols)) then
          begin
            Result := FALSE;
            if (lErrorCols.Count = 0) then
              AErrors := AErrors + lErrorList.Text
            else
            AErrors := AErrors + CTStringsSeparator + lErrorList.Text +
                                 CTStringsSeparator + lErrorCols.Text+ CTStringsSeparator;
            lErrorList.Clear;
            lErrorCols.Clear;
          end;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateType3Proportions(lErrorList,lErrorCols)) then
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
        if (FChannelNr = 0) then
        begin
          lMessage := FAppModules.Language.GetString('ContextValidation.DiversionFeatureNotAssignedToChannel');
          lErrorList.Add(Format(lMessage, [FFeatureName]));
          Result := TRUE;
        end;
      end;
      AErrors := AErrors + lErrorList.Text
    finally
      FreeAndNil(lErrorCols);
      FreeAndNil(lErrorList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{******************************************************************************}
{ TDiversionFeatureList                                                        }
{******************************************************************************}

function TDiversionFeatureList._AddRef: Integer;
const OPNAME = 'TDiversionFeatureList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeatureList._Release: Integer;
const OPNAME = 'TDiversionFeatureList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDiversionFeatureList.CreateMemberObjects;
const OPNAME = 'TDiversionFeatureList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FDiversionFeatureList := TList.Create;
    FDiversionGaugeList   := TStringList.Create;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDiversionFeatureList.DestroyMemberObjects;
const OPNAME = 'TDiversionFeatureList.DestroyMemberObjects';
begin
  try
    while (FDiversionFeatureList.Count > 0) do
      DeleteDiversionFeatureWithIndex(0);
    FreeAndNil(FDiversionFeatureList);
    FreeAndNil(FDiversionGaugeList);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeatureList.Initialise: boolean;
const OPNAME = 'TDiversionFeatureList.Initialise';
begin
  Result := inherited Initialise;
  try
    FDiversionFeatureList.Clear;
    FDiversionGaugeList.Clear;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeatureList.AddDiversionFeature (AFeature : TDiversionFeature): boolean;
const OPNAME = 'TDiversionFeatureList.AddDiversionFeature';
begin
  Result := False;
  try
    if (AFeature <> nil) then
    begin
      FDiversionFeatureList.Add(AFeature);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeatureList.NewDiversionFeature : TDiversionFeature;
const OPNAME = 'TDiversionFeatureList.NewDiversionFeature';
var
  lFeature : TDiversionFeature;
begin
  Result := nil;
  try
    lFeature := TDiversionFeature.Create(FAppModules);
    AddDiversionFeature(lFeature);
    Result := lFeature;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeatureList.CreateNewDiversionFeature : TDiversionFeature;
const OPNAME = 'TDiversionFeatureList.CreateNewDiversionFeature';
var
  LFeatureID             : integer;
  LLoadAgent             : TNetworkFeaturesSQLAgent;
  LFeature               : TDiversionFeature;
  lDiversionDemands      : TDivMonthlyDoublesArray;
  lDivertedFlows         : TDivMonthlyDoublesArray;
  LDiversionDemandsField : TAbstractFieldProperty;
  LDivertedFlowsField    : TAbstractFieldProperty;
  lIndex                 : integer;
begin
  Result := nil;
  try
    LDiversionDemandsField := FAppModules.FieldProperties.FieldProperty('DiversionDemand');
    if (LDiversionDemandsField = nil) then
      raise Exception.Create('Field (DiversionDemand) not found in field properties');
    SetLength(lDiversionDemands,LDiversionDemandsField.ArrayLength);
    for lIndex := LDiversionDemandsField.ArrayLow to LDiversionDemandsField.ArrayHigh do
      lDiversionDemands[lIndex] := 0;

    LDivertedFlowsField := FAppModules.FieldProperties.FieldProperty('DiversionFlow');
    if (LDivertedFlowsField = nil) then
      raise Exception.Create('Field (DiversionFlow) not found in field properties');
    SetLength(lDivertedFlows,LDivertedFlowsField.ArrayLength);
    for lIndex := LDivertedFlowsField.ArrayLow to LDivertedFlowsField.ArrayHigh do
      lDivertedFlows[lIndex] := 0;

    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LFeatureID := 0;
      if (LLoadAgent.InsertDiversionFeature(LFeatureID)) then
      begin
        LFeature := NewDiversionFeature;
        LFeature.Initialise;
        LFeature.PopulateType1or2
          (LFeatureID,
           UpperCase(FAppModules.Language.GetString('NetworkFeatures.DiversionFeature')) + ' ' + IntToStr(LFeatureID),
           '',0, 3, 0, 1, lDiversionDemands, lDivertedFlows);
        Result := LFeature;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeatureList.CreateDiversionFeature : IDiversionFeature;
const OPNAME = 'TDiversionFeatureList.CreateDiversionFeature';
var
  LFeature : IDiversionFeature;
begin
  Result := nil;
  try
    LFeature := CreateNewDiversionFeature;
    Result := LFeature;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeatureList.RemoveDiversionFeatureWithID (AFeatureID : integer) : WordBool;
const OPNAME = 'TDiversionFeatureList.RemoveDiversionFeatureWithID';
var
  lLoadAgent : TNetworkFeaturesSQLAgent;
begin
  Result := FALSE;
  try
    if (AFeatureID > 0) then
    begin
      LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
      try
        if LLoadAgent.DeleteDiversionFeature(AFeatureID) then
        begin
          DeleteDiversionFeatureWithID(AFeatureID);
          Result := TRUE;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeatureList.DeleteDiversionFeatureWithID(AFeatureID : integer) : WordBool;
const OPNAME = 'TDiversionFeatureList.DeleteDiversionFeatureWithID';
var
  lFeature : TDiversionFeature;
  lIndex   : integer;
begin
  Result := FALSE;
  try
    lFeature := CastDiversionFeatureByID(AFeatureID);
    if (lFeature <> nil) then
    begin
      lIndex := FDiversionFeatureList.IndexOf(lFeature);
      Result := DeleteDiversionFeatureWithIndex(lIndex);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeatureList.DeleteDiversionFeatureWithIndex(AIndex : integer) : WordBool;
const OPNAME = 'TDiversionFeatureList.DeleteDiversionFeatureWithIndex';
var
  lFeature : TDiversionFeature;
begin
  Result := FALSE;
  try
    if (AIndex >= 0) then
    begin
      lFeature := TDiversionFeature(FDiversionFeatureList.Items[AIndex]);
      FDiversionFeatureList.Delete(AIndex);
      FreeAndNil(lFeature);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeatureList.Get_DiversionFeatureByIndex(AIndex: integer): IDiversionFeature;
const OPNAME = 'TDiversionFeatureList.Get_DiversionFeatureByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FDiversionFeatureList.Count) then
      Result := TDiversionFeature(FDiversionFeatureList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeatureList.Get_DiversionFeatureByID(AFeatureID : integer): IDiversionFeature;
const OPNAME = 'TDiversionFeatureList.Get_DiversionFeatureByID';
var
 lIndex   : integer;
 lFeature : TDiversionFeature;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FDiversionFeatureList.Count)) do
    begin
      lFeature := TDiversionFeature(FDiversionFeatureList.Items[lIndex]);
      if (lFeature.FeatureID = AFeatureID) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeatureList.CastDiversionFeatureByIndex(AIndex: integer): TDiversionFeature;
const OPNAME = 'TDiversionFeatureList.CastDiversionFeatureByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FDiversionFeatureList.Count) then
      Result := TDiversionFeature(FDiversionFeatureList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeatureList.CastDiversionFeatureByID(AFeatureID : integer): TDiversionFeature;
const OPNAME = 'TDiversionFeatureList.CastDiversionFeatureByID';
var
 lIndex   : integer;
 lFeature : TDiversionFeature;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FDiversionFeatureList.Count)) do
    begin
      lFeature := TDiversionFeature(FDiversionFeatureList.Items[lIndex]);
      if (lFeature.FeatureID = AFeatureID) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeatureList.CastDiversionFeatureByChannelNr(AChannelNr: integer): TDiversionFeature;
const OPNAME = 'TDiversionFeatureList.CastDiversionFeatureByChannelNr';
var
 lIndex   : integer;
 lFeature : TDiversionFeature;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FDiversionFeatureList.Count)) do
    begin
      lFeature := TDiversionFeature(FDiversionFeatureList.Items[lIndex]);
      if (lFeature.FChannelNr = AChannelNr) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TDiversionFeatureList.Get_DiversionFeatureCount: integer;
const OPNAME = 'TDiversionFeatureList.Get_DiversionFeatureCount';
begin
  Result := 0;
  try
    Result := FDiversionFeatureList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeatureList.Get_DiversionGaugeList : WideString;
const OPNAME = 'TDiversionFeatureList.Get_DiversionGaugeList';
begin
  Result := '';
  try
    Result := FDiversionGaugeList.CommaText;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionFeatureList.GetStationIDByName(const AValue : WideString) : integer;
const OPNAME = 'TDiversionFeatureList.GetStationIDByName';
var
  lLoadAgent : TNetworkFeaturesSQLAgent;
begin
  Result := -1;
  try
    lLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      Result := lLoadAgent.GetStationIDByName(AValue);
    finally
      FreeAndNil(lLoadAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDiversionFeatureList.Set_DiversionGaugeList(const AValue : WideString);
const OPNAME = 'TDiversionFeatureList.Set_DiversionGaugeList';
begin
  try
    FDiversionGaugeList.CommaText := AValue;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionFeatureList.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TDiversionFeatureList.Validate';
var
  LIndex            : integer;
  lStopOnFirstError : Boolean;
begin
  Result := FALSE;
  try
    Result := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    for LIndex := 0 to FDiversionFeatureList.Count -1 do
    begin
      if (NOT DiversionFeatureByIndex[LIndex].Validate(AErrors,AContext)) then
        Result := FALSE;
      if ((NOT Result) AND lStopOnFirstError) then
        Break;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeature.GetKeyValues (const AParamField : WideString;
                                         const AFieldIndex : WideString) : WideString;
const OPNAME = 'TDiversionFeature.GetKeyValues';
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
    if (AParamField = 'DiversionDemand') OR (AParamField = 'FlowRange') then
      Result := Result + ',DiversionCode=1';
    if (AParamField = 'NetNaturalInflow') OR (AParamField = 'ActualDivertedFlow') then
      Result := Result + ',DiversionCode=2';
    if (AParamField = 'FlowValue') OR (AParamField = 'DivertedFlow') then
    begin
      FAppModules.Changes.GetIndexes(AFieldIndex, lDim1Idx, lDim2Idx);
      Result := Result + ',DiversionIndex=' + IntToStr(lDim1Idx);;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionFeature.Type2and4RowCount : integer;
const OPNAME = 'TDiversionFeature.Type2and4RowCount';
var
  lIndex     : integer;
  lCount     : integer;
  lContinue  : boolean;
  lFieldProp : TAbstractFieldProperty;
begin
  Result := 0;
  try
    lFieldProp := FAppModules.FieldProperties.FieldProperty('DiversionDemand');
    lContinue  := TRUE;
    lIndex     := lFieldProp.ArrayLow;
    lCount     := 0;
    while (lContinue AND (lIndex <= lFieldProp.ArrayHigh)) do
    begin
      if (FDiversionDemands[lIndex] <> NullFloat) then
      begin
        lCount := lCount + 1;
        lIndex := lIndex + 1;
      end
      else
        lContinue := FALSE;
    end;
    Result := lCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionFeature.InsertRow (AIndex : integer) : WordBool;
const OPNAME = 'TDiversionFeature.InsertRow';
var
  lIndex     : integer;
  lFieldProp : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lFieldProp := FAppModules.FieldProperties.FieldProperty('DiversionDemand');
    if (Type2and4RowCount < lFieldProp.ArrayHigh) AND
       (AIndex >= lFieldProp.ArrayLow) AND
       (AIndex <= lFieldProp.ArrayHigh) then
    begin
      for lIndex := lFieldProp.ArrayHigh - 1 downto AIndex do
      begin
        WriteDiversionDemandToDB(lIndex+1, FDiversionDemands[lIndex]);
        WriteDivertedFlowToDB(lIndex+1, FDivertedFlows[lIndex]);
      end;
      WriteDiversionDemandToDB(AIndex, 0.0);
      WriteDivertedFlowToDB(AIndex, 0.0);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeature.ImportedType2or4RelationshipFromPreProcessor(AStation : integer) : wordbool;
const OPNAME = 'TDiversionFeature.ImportedType2or4RelationshipFromPreProcessor';
var
  LIndex     : integer;
  LReferenceFlows : TAbstractFieldProperty;
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
begin
  Result := False;
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    LContextData := TStringList.Create;
    try
      LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
      if LLoadAgent.ImportRelationship(AStation,IntToStr(FFeatureID),FDiversionDemands,FDivertedFlows) then
      begin
        LReferenceFlows := FAppModules.FieldProperties.FieldProperty('DiversionDemand');
        for LIndex := LReferenceFlows.ArrayLow to LReferenceFlows.ArrayHigh do
        begin
          WriteDiversionDemandToDB(lIndex, FDiversionDemands[LIndex]);
          WriteDivertedFlowToDB(lIndex, FDivertedFlows[LIndex]);
        end;
      end;
      Result := True;
    finally
      FreeAndNil(LContextData);
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TDiversionFeature.DeleteRow (AIndex : integer) : WordBool;
const OPNAME = 'TDiversionFeature.DeleteRow';
var
  lIndex     : integer;
  lFieldProp : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lFieldProp := FAppModules.FieldProperties.FieldProperty('DiversionDemand');
    if (Type2and4RowCount > 0) AND
       (AIndex >= lFieldProp.ArrayLow) AND
       (AIndex <= lFieldProp.ArrayHigh) then
    begin
      for lIndex := AIndex to lFieldProp.ArrayHigh - 1 do
      begin
        WriteDiversionDemandToDB(lIndex, FDiversionDemands[lIndex+1]);
        WriteDivertedFlowToDB(lIndex, FDivertedFlows[lIndex+1]);
      end;
      WriteDiversionDemandToDB(lFieldProp.ArrayHigh, NullFloat);
      WriteDivertedFlowToDB(lFieldProp.ArrayHigh, NullFloat);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionFeatureList.CopyDiversionFeature(ANewChannelNumber,AOldChannelNumber: Integer): IDiversionFeature;
const OPNAME = 'TDiversionFeatureList.CopyDiversionFeature';
var
  LDiversionFeature : TDiversionFeature;
  LDiversionFeatureCopy : TDiversionFeature;
begin
  Result := nil;
  try
    LDiversionFeature := CastDiversionFeatureByChannelNr(AOldChannelNumber);
    if (LDiversionFeature <> nil) then
    begin
      LDiversionFeatureCopy := CreateNewDiversionFeature;
      if LDiversionFeatureCopy <> nil then
      begin
        LDiversionFeatureCopy.Assign(ANewChannelNumber,LDiversionFeature);
        Result := LDiversionFeatureCopy;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TDiversionFeature.Assign(AChannelNumber: integer;ADiversionFeature: TDiversionFeature);
const OPNAME = 'TDiversionFeature.Assign';
var
  LChannel : IGeneralFlowChannel;
  LDiversionDemands: TAbstractFieldProperty;
  LIndex : integer;
  LIndexA : integer;
  LIndexB : integer;
  LReservoirElevations,
  LReferenceFlows: TAbstractFieldProperty;
begin
  try
    if (ADiversionFeature <> nil) and (AChannelNumber > 0) then
    begin
      LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[AChannelNumber];
      if LChannel <> nil then
        Channel := LChannel;
      FeatureName := 'Copy of '+ADiversionFeature.FeatureName;
      FeatureType := ADiversionFeature.FeatureType;
      FeatureSubType := ADiversionFeature.FeatureSubType;
      DiversionType  := ADiversionFeature.DiversionType;
      if (DiversionType in [1,2,4]) then
      begin
        LDiversionDemands := FAppModules.FieldProperties.FieldProperty('DiversionDemand');
        for LIndex := LDiversionDemands.ArrayLow to LDiversionDemands.ArrayHigh do
        begin
          DiversionDemandByIndex[LIndex] := ADiversionFeature.DiversionDemandByIndex[lIndex];
          DivertedFlowByIndex[LIndex]    := ADiversionFeature.DivertedFlowByIndex[lIndex];
        end;
      end
      else
      begin
        if (ADiversionFeature.ControllingReservoir <> nil) then
          ControllingReservoir := ADiversionFeature.ControllingReservoir
        else
          FControllingReservoirNr := -1;
        ReservoirElevationsCount := ADiversionFeature.ReservoirElevationsCount;
        LReservoirElevations := FAppModules.FieldProperties.FieldProperty('ControllingResLevels');
        for LIndexA := LReservoirElevations.ArrayLow to LReservoirElevations.ArrayHigh do
          ReservoirElevationByIndex[LIndexA] := ADiversionFeature.ReservoirElevationByIndex[lIndexA];
        ReferenceFlowsCount := ADiversionFeature.ReferenceFlowsCount;
        LReferenceFlows := FAppModules.FieldProperties.FieldProperty('FlowValue');
        for LIndexA := LReferenceFlows.ArrayLow to LReferenceFlows.ArrayHigh do
          ReferenceFlowByIndex[LIndexA] := ADiversionFeature.ReferenceFlowByIndex[LIndexA];
        for LIndexA := LReferenceFlows.ArrayLow to LReferenceFlows.ArrayHigh do
          for LIndexB := LReservoirElevations.ArrayLow to LReservoirElevations.ArrayHigh do
            DivertedFlowProportion[LIndexA, LIndexB] := ADiversionFeature.DivertedFlowProportion[LIndexA, LIndexB];
      end;
      Station :=  ADiversionFeature.Station;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
