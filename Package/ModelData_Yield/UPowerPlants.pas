{******************************************************************************}
{*  UNIT      : Contains the class TPowerPlant.                               *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2004/03/10                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UPowerPlants;

interface

uses
  Classes,
  Contnrs,
  UAbstractObject,
  VoaimsCom_TLB;

type

{******************************************************************************}
{* Power Plant                                                                *}
{******************************************************************************}

  TPowerPlant = class(TAbstractAppObject, IPowerPlant)
  protected
    FFeatureID                     : integer;
    FFeatureName                   : string;
    FChannelNr                     : integer;
    FFeatureType                   : integer;
    FFeatureSubType                : integer;
    FSpillChannelNr                : integer;
    FPowerPlantStatus              : Boolean;
    FMaximumGeneratorCapacity      : double;
    FMaximumTurbineCapacity        : double;
    FHeadLoss                      : double;
    FDesignHead                    : double;
    FCombinedEfficiency            : double;
    FMaximumNetHead                : double;
    FMinimumNetHead                : double;
    FDownstreamPowerChannelNrs     : TStringList;
    FEfficiencyFactors             : TEfficiencyFactorsArray;
    FNetHeadFactors                : TNetHeadFactorsArray;
    FDischarges                    : TDischargesArray;
    FTailwaterType                 : integer;
    FTailwaterElevations           : TElevationsArray;
    FMinimumMonthlyPowerGeneration : TPowMonthlyDoublesArray;
    FMinimumMonthlyPowerRelease    : TPowMonthlyDoublesArray;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure WriteEfficiencyFactorToDB(AIndex : integer; AValue : double);
    procedure WriteNetHeadFactorToDB(AIndex : integer; AValue : double);
    procedure WriteDischargeToDB(AIndex : integer;AValue : double);
    procedure WriteTailwaterElevationToDB(AIndex : integer; AValue : double);

    function ValidatePowerPlantName(AErrorMessages : TStrings): WordBool;
    function ValidatePowerSpillUpstreamNode(AErrorMessages : TStrings): WordBool;
    function ValidatePowerDownstreamNode(AErrorMessages : TStrings): WordBool;
    function ValidateSpillDownstreamNode(AErrorMessages : TStrings): WordBool;
    function ValidateMaximumGeneratorCapacity(AErrorMessages : TStrings): WordBool;
    function ValidateMaximumTurbineCapacity(AErrorMessages : TStrings): WordBool;
    function ValidateHeadLoss(AErrorMessages : TStrings): WordBool;
    function ValidateDownstreamPowerPlants(AErrorMessages : TStrings): WordBool;
    function ValidateCombinedEfficiency(AErrorMessages : TStrings): WordBool;
    function ValidateDesignHead(AErrorMessages : TStrings): WordBool;
    function ValidateMaximumNetHead(AErrorMessages : TStrings): WordBool;
    function ValidateMinimumNetHead(AErrorMessages : TStrings): WordBool;
    function ValidateEfficiencyFactors(AErrorMessages : TStrings; AErrorColumns: TStrings): WordBool;
    function ValidateNetHeadFactors(AErrorMessages : TStrings; AErrorColumns: TStrings): WordBool;
    function ValidateTailwaterType(AErrorMessages : TStrings): WordBool;
    function ValidateTailwaterDischarges(AErrorMessages : TStrings; AErrorColumns: TStrings): WordBool;
    function ValidateTailwaterElevations(AErrorMessages : TStrings; AErrorColumns: TStrings): WordBool;
    function ValidatePowerGenerations(AErrorMessages : TStrings; AErrorColumns: TStrings): WordBool;
    function ValidatePowerReleases(AErrorMessages : TStrings; AErrorColumns: TStrings): WordBool;
  public
    procedure Assign(APowerPlant : TPowerPlant);
    function Initialise: boolean; override;
    function Populate (AFeatureID                     : integer;
                       AFeatureName                   : WideString;
                       AChannelNr                     : integer;
                       AFeatureType                   : integer;
                       AFeatureSubType                : integer;
                       ASpillChannelNr                : integer;
                       APowerPlantStatus              : WordBool;
                       AMaximumGeneratorCapacity      : double;
                       AMaximumTurbineCapacity        : double;
                       AHeadLoss                      : double;
                       ADesignHead                    : double;
                       ACombinedEfficiency            : double;
                       AMaximumNetHead                : double;
                       AMinimumNetHead                : double;
                       ANetHeadEfficiencyCount        : integer;
                       ATailwaterElevationsCount      : integer;
                       ATailwaterType                 : integer;
                       AEfficiencyFactors             : TEfficiencyFactorsArray;
                       ANetHeadFactors                : TNetHeadFactorsArray;
                       ADischarges                    : TDischargesArray;
                       ATailwaterElevations           : TElevationsArray;
                       AMinimumMonthlyPowerGeneration : TPowMonthlyDoublesArray;
                       AMinimumMonthlyPowerRelease    : TPowMonthlyDoublesArray;
                       ADownstreamChannelNrs          : TStringList) : WordBool;
    function PopulateSome (AFeatureID                     : integer;
                           AFeatureName                   : WideString;
                           AChannelNr                     : integer;
                           AFeatureType                   : integer;
                           AFeatureSubType                : integer;
                           ASpillChannelNr                : integer;
                           APowerPlantStatus              : WordBool;
                           AMaximumGeneratorCapacity      : double;
                           AMaximumTurbineCapacity        : double;
                           AHeadLoss                      : double;
                           ADesignHead                    : double;
                           ACombinedEfficiency            : double;
                           AMaximumNetHead                : double;
                           AMinimumNetHead                : double;
                           ANetHeadEfficiencyCount        : integer;
                           ATailwaterElevationsCount      : integer;
                           ATailwaterType                 : integer;
                           AMinimumMonthlyPowerGeneration : TPowMonthlyDoublesArray;
                           AMinimumMonthlyPowerRelease    : TPowMonthlyDoublesArray) : WordBool;
    function Get_FeatureID : integer; safecall;
    function Get_FeatureName : WideString; safecall;
    procedure Set_FeatureName (const AName : WideString); safecall;
    function Get_Channel : IGeneralFlowChannel; safecall;
    function Get_FeatureType : integer; safecall;
    procedure Set_FeatureType (AType : integer); safecall;
    function Get_FeatureSubType : integer; safecall;
    procedure Set_FeatureSubType (ASubType : integer); safecall;
    function Get_PowerChannel : IGeneralFlowChannel; safecall;
    function Get_SpillChannel : IGeneralFlowChannel; safecall;
    function Get_MaximumGeneratorCapacity : double; safecall;
    procedure Set_MaximumGeneratorCapacity(ACapacity : double); safecall;
    function Get_MaximumTurbineCapacity : double; safecall;
    procedure Set_MaximumTurbineCapacity(ACapacity : double); safecall;
    function Get_CombinedEfficiency : double; safecall;
    procedure Set_CombinedEfficiency(AEfficiency : double); safecall;
    function Get_PowerPlantStatus : WordBool; safecall;
    procedure Set_PowerPlantStatus(AStatus : WordBool); safecall;
    function Get_HeadLoss : double; safecall;
    procedure Set_HeadLoss(ALoss : double); safecall;
    function Get_DesignHead : double; safecall;
    procedure Set_DesignHead(AValue : double); safecall;
    function Get_MaximumNetHead : double; safecall;
    procedure Set_MaximumNetHead(ANetHead : double); safecall;
    function Get_MinimumNetHead : double; safecall;
    procedure Set_MinimumNetHead(ANetHead : double); safecall;
    function Get_TailWaterType : integer; safecall;
    procedure Set_TailWaterType(AType : integer); safecall;
    function Get_DownstreamPowerChannelNrs : WideString; safecall;
    procedure Set_DownstreamPowerChannelNrs (const AChannelNrs : WideString); safecall;
    function Get_DownstreamPowerChannelNrsCount : integer; safecall;
    procedure Set_DownstreamPowerChannelNrsCount (ACount : integer); safecall;
    function Get_DownstreamPowerChannelNrByIndex(AIndex : integer) : integer; safecall;
    function Get_NetHeadEfficiencyCount : integer; safecall;
    procedure Set_NetHeadEfficiencyCount (Value : integer); safecall;
    function Get_EfficiencyFactorsArray : TEfficiencyFactorsArray; safecall;
    function Get_EfficiencyFactorByIndex(AIndex : integer) : double; safecall;
    procedure Set_EfficiencyFactorByIndex(AIndex : integer; AValue : double); safecall;
    function Get_NetHeadFactorsArray : TNetHeadFactorsArray; safecall;
    function Get_NetHeadFactorByIndex(AIndex : integer) : double; safecall;
    procedure Set_NetHeadFactorByIndex(AIndex : integer; AValue : double); safecall;
    function Get_TailwaterElevationCount: integer; safecall;
    procedure Set_TailwaterElevationCount (Value : integer); safecall;
    function Get_DischargeArray : TDischargesArray; safecall;
    function Get_DischargeByIndex(AIndex : integer) : double; safecall;
    procedure Set_DischargeByIndex(AIndex : integer; AValue : double); safecall;
    function Get_TailwaterElevationsArray : TElevationsArray; safecall;
    function Get_TailwaterElevationByIndex(AIndex : integer) : double; safecall;
    procedure Set_TailwaterElevationByIndex(AIndex : integer; AValue : double); safecall;
    function Get_MinimumMonthlyPowerGenerationArray : TPowMonthlyDoublesArray; safecall;
    function Get_MinimumPowerGenerationByMonth(AMonth : integer) : double; safecall;
    procedure Set_MinimumPowerGenerationByMonth(AMonth : integer; AValue : double); safecall;
    function Get_MinimumMonthlyPowerReleaseArray : TPowMonthlyDoublesArray; safecall;
    function Get_MinimumPowerReleaseByMonth(AMonth : integer) : double; safecall;
    procedure Set_MinimumPowerReleaseByMonth(AMonth : integer; AValue : double); safecall;
    function Validate (var AErrors : WideString; const AContext : WideString='') : WordBool; safecall;
    function GetKeyValues (const AParamField : WideString; const AFieldIndex: WideString) : WideString; safecall;
    property FeatureID      : integer read Get_FeatureID;
    property FeatureName    : WideString read Get_FeatureName write Set_FeatureName;
    property Channel        : IGeneralFlowChannel read Get_Channel;
    property FeatureType    : integer read Get_FeatureType write Set_FeatureType;
    property FeatureSubType : integer read Get_FeatureSubType write Set_FeatureSubType;
    property PowerChannel : IGeneralFlowChannel read Get_PowerChannel;
    property SpillChannel : IGeneralFlowChannel read Get_SpillChannel;
    property MaximumGeneratorCapacity : double read Get_MaximumGeneratorCapacity write Set_MaximumGeneratorCapacity;
    property MaximumTurbineCapacity : double read Get_MaximumTurbineCapacity write Set_MaximumTurbineCapacity;
    property CombinedEfficiency : double read Get_CombinedEfficiency write Set_CombinedEfficiency;
    property PowerPlantStatus : WordBool read Get_PowerPlantStatus write Set_PowerPlantStatus;
    property HeadLoss : double read Get_HeadLoss write Set_HeadLoss;
    property DesignHead : double read Get_DesignHead write Set_DesignHead;
    property MaximumNetHead : double read Get_MaximumNetHead write Set_MaximumNetHead;
    property MinimumNetHead : double read Get_MinimumNetHead write Set_MinimumNetHead;
    property TailWaterType : integer read Get_TailWaterType write Set_TailWaterType;
    property DownstreamPowerChannelNrs : WideString read Get_DownstreamPowerChannelNrs write Set_DownstreamPowerChannelNrs;
    property DownstreamPowerChannelNrByIndex[AIndex : integer] : integer read Get_DownstreamPowerChannelNrByIndex;
    property DownstreamPowerChannelNrsCount : integer read Get_DownstreamPowerChannelNrsCount write Set_DownstreamPowerChannelNrsCount;
    property NetHeadEfficiencyCount : integer read Get_NetHeadEfficiencyCount write Set_NetHeadEfficiencyCount;
    property EfficiencyFactorsArray : TEfficiencyFactorsArray read Get_EfficiencyFactorsArray;
    property EfficiencyFactorByIndex[AIndex : integer] : double read Get_EfficiencyFactorByIndex write Set_EfficiencyFactorByIndex;
    property NetHeadFactorsArray : TNetHeadFactorsArray read Get_NetHeadFactorsArray;
    property NetHeadFactorByIndex[AIndex : integer] : double read Get_NetHeadFactorByIndex write Set_NetHeadFactorByIndex;
    property TailwaterElevationCount : integer read Get_TailwaterElevationCount write Set_TailwaterElevationCount;
    property DischargeArray : TDischargesArray read Get_DischargeArray;
    property DischargeByIndex[AIndex : integer] : double read Get_DischargeByIndex write Set_DischargeByIndex;
    property TailwaterElevationsArray : TElevationsArray read Get_TailwaterElevationsArray;
    property TailwaterElevationByIndex[AIndex : integer] : double read Get_TailwaterElevationByIndex write Set_TailwaterElevationByIndex;
    property MinimumMonthlyPowerGenerationArray : TPowMonthlyDoublesArray read Get_MinimumMonthlyPowerGenerationArray;
    property MinimumPowerGenerationByMonth[AMonth : integer] : double read Get_MinimumPowerGenerationByMonth write Set_MinimumPowerGenerationByMonth;
    property MinimumMonthlyPowerReleaseArray : TPowMonthlyDoublesArray read Get_MinimumMonthlyPowerReleaseArray;
    property MinimumPowerReleaseByMonth[AMonth : integer] : double read Get_MinimumPowerReleaseByMonth write Set_MinimumPowerReleaseByMonth;
  end;


  TPowerPlantList = class(TAbstractAppObject, IPowerPlantList)
  protected
    FPowerPlantList : TObjectList;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function AddPowerPlant (AFeature : TPowerPlant): boolean;
  public
    function Initialise : boolean; override;
    function NewPowerPlant : TPowerPlant;
    function CastPowerPlantByIndex (AIndex : integer): TPowerPlant;
    function CastPowerPlantByID(AFeatureID : integer): TPowerPlant;
    function DeletePowerPlantWithID(AFeatureID : integer) : WordBool;
    function DeletePowerPlantWithIndex(AIndex : integer) : WordBool;
    function CopyPowerPlant(ANewChannelNumber: Integer; AOldChannelNumber: Integer): IPowerPlant; safecall;

    function CreatePowerPlant : IPowerPlant; safecall;
    function RemovePowerPlantWithID (AFeatureID : integer) : WordBool; safecall;
    function Get_PowerPlantByID(AFeatureID : integer): IPowerPlant; safecall;
    function Get_PowerPlantByIndex(AIndex: integer): IPowerPlant; safecall;
    function Get_PowerPlantCount : integer; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    procedure GetChannelNumbers(AFeatureID: integer;var APowerChannellNumber,ASpillChannelNumber: integer);

    property PowerPlantByIndex[AIndex : integer]: IPowerPlant
      read Get_PowerPlantByIndex;
    property PowerPlantByID[AFeatureID: integer]: IPowerPlant
      read Get_PowerPlantByID;
    property PowerPlantCount: integer read Get_PowerPlantCount;
  end;

implementation

uses
  System.Types,
  SysUtils,
  Math,
  UConstants,
  UNetworkElementData,
  UNetworkFeaturesSQLAgent,
  UYieldModelDataObject,
  UErrorHandlingOperations;

{******************************************************************************}
{ TPowerPlant                                                            }
{******************************************************************************}

function TPowerPlant._AddRef: Integer;
const OPNAME = 'TPowerPlant._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant._Release: Integer;
const OPNAME = 'TPowerPlant._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPowerPlant.CreateMemberObjects;
const OPNAME = 'TPowerPlant.CreateMemberObjects';
var
  LEfficiencyFactorsField,
  LNetHeadFactorsField,
  LDischargesField,
  LTailwaterElevationsField,
  LMinimumMonthlyPowerGenerationField,
  LMinimumMonthlyPowerReleasefield: TAbstractFieldProperty;
begin
  inherited;
  try
    FDownstreamPowerChannelNrs  := TStringList.Create;

    LEfficiencyFactorsField := FAppModules.FieldProperties.FieldProperty('EfficiencyFactor');
    if not Assigned(LEfficiencyFactorsField) then
      raise Exception.Create('Field (EfficiencyFactor) not found in field properties');
    SetLength(FEfficiencyFactors,LEfficiencyFactorsField.ArrayLength);

    LNetHeadFactorsField := FAppModules.FieldProperties.FieldProperty('NetHeadFactors');
    if not Assigned(LNetHeadFactorsField) then
      raise Exception.Create('Field (NetHeadFactors) not found in field properties');
    SetLength(FNetHeadFactors,LNetHeadFactorsField.ArrayLength);

    LDischargesField := FAppModules.FieldProperties.FieldProperty('DownStreamLevel');
    if not Assigned(LDischargesField) then
      raise Exception.Create('Field (DownStreamLevel) not found in field properties');
    SetLength(FDischarges,LDischargesField.ArrayLength);

    LTailwaterElevationsField := FAppModules.FieldProperties.FieldProperty('TailWaterElevation');
    if not Assigned(LTailwaterElevationsField) then
      raise Exception.Create('Field (TailWaterElevation) not found in field properties');
    SetLength(FTailwaterElevations,LTailwaterElevationsField.ArrayLength);

    LMinimumMonthlyPowerGenerationField := FAppModules.FieldProperties.FieldProperty('MinEnergyGenerated');
    if not Assigned(LMinimumMonthlyPowerGenerationField) then
      raise Exception.Create('Field (MinEnergyGenerated) not found in field properties');
    SetLength(FMinimumMonthlyPowerGeneration,LMinimumMonthlyPowerGenerationField.ArrayLength);

    LMinimumMonthlyPowerReleasefield := FAppModules.FieldProperties.FieldProperty('MinPowerChannelRelease');
    if not Assigned(LMinimumMonthlyPowerReleasefield) then
      raise Exception.Create('Field (MinPowerChannelRelease) not found in field properties');
    SetLength(FMinimumMonthlyPowerRelease,LMinimumMonthlyPowerReleasefield.ArrayLength);

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPowerPlant.DestroyMemberObjects;
const OPNAME = 'TPowerPlant.DestroyMemberObjects';
begin
  try
    FDownstreamPowerChannelNrs.Clear;
    FreeAndNil(FDownstreamPowerChannelNrs);
    Finalize(FEfficiencyFactors);
    Finalize(FDischarges);
    Finalize(FTailwaterElevations);
    Finalize(FMinimumMonthlyPowerGeneration);
    Finalize(FMinimumMonthlyPowerRelease);
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.Initialise: boolean;
const OPNAME = 'TPowerPlant.Initialise';
var
  nIndex : integer;
  LMinimumMonthlyPowerGeneration,
  LMinimumMonthlyPowerRelease,
  LEfficiencyFactors,
  LNetHeadFactors,
  LDischarges,
  LTailwaterElevations: TAbstractFieldProperty;
begin
  Result := inherited Initialise;
  try
    FChannelNr                := 0;
    FFeatureID                := -1;
    FFeatureName              := '';
    FFeatureType              := -1;
    FFeatureSubType           := -1;
    FSpillChannelNr           := 0;
    FPowerPlantStatus         := FALSE;
    FMaximumGeneratorCapacity := -1.0;
    FMaximumTurbineCapacity   := -1.0;
    FHeadLoss                 := -1.0;
    FDesignHead               := -1.0;
    FCombinedEfficiency       := -1.0;
    FMaximumNetHead           := -1.0;
    FMinimumNetHead           := -1.0;
    FTailwaterType            := -1;

    LMinimumMonthlyPowerGeneration := FAppModules.FieldProperties.FieldProperty('MinEnergyGenerated');
    LMinimumMonthlyPowerRelease    := FAppModules.FieldProperties.FieldProperty('MinPowerChannelRelease');
    LEfficiencyFactors             := FAppModules.FieldProperties.FieldProperty('EfficiencyFactor');
    LNetHeadFactors                := FAppModules.FieldProperties.FieldProperty('NetHeadFactors');
    LDischarges                    := FAppModules.FieldProperties.FieldProperty('DownStreamLevel');
    LTailwaterElevations           := FAppModules.FieldProperties.FieldProperty('TailWaterElevation');

    for nIndex := LMinimumMonthlyPowerGeneration.ArrayLow to LMinimumMonthlyPowerGeneration.ArrayHigh do
      FMinimumMonthlyPowerGeneration[nIndex] := NullFloat;
    for nIndex := LMinimumMonthlyPowerRelease.ArrayLow to LMinimumMonthlyPowerRelease.ArrayHigh do
      FMinimumMonthlyPowerRelease[nIndex]    := NullFloat;
    for nIndex := LEfficiencyFactors.ArrayLow to LEfficiencyFactors.ArrayHigh do
      FEfficiencyFactors[nIndex]   := NullFloat;
    for nIndex := LNetHeadFactors.ArrayLow to LNetHeadFactors.ArrayHigh do
      FNetHeadFactors[nIndex]      := NullFloat;
    for nIndex := LDischarges.ArrayLow to LDischarges.ArrayHigh do
      FDischarges[nIndex]          := NullFloat;
    for nIndex := LTailwaterElevations.ArrayLow to LTailwaterElevations.ArrayHigh do
      FTailwaterElevations[nIndex] := NullFloat;
    FDownstreamPowerChannelNrs.Clear;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.Populate (AFeatureID                     : integer;
                               AFeatureName                   : WideString;
                               AChannelNr                     : integer;
                               AFeatureType                   : integer;
                               AFeatureSubType                : integer;
                               ASpillChannelNr                : integer;
                               APowerPlantStatus              : WordBool;
                               AMaximumGeneratorCapacity      : double;
                               AMaximumTurbineCapacity        : double;
                               AHeadLoss                      : double;
                               ADesignHead                    : double;
                               ACombinedEfficiency            : double;
                               AMaximumNetHead                : double;
                               AMinimumNetHead                : double;
                               ANetHeadEfficiencyCount        : integer;
                               ATailwaterElevationsCount      : integer;
                               ATailwaterType                 : integer;
                               AEfficiencyFactors             : TEfficiencyFactorsArray;
                               ANetHeadFactors                : TNetHeadFactorsArray;
                               ADischarges                    : TDischargesArray;
                               ATailwaterElevations           : TElevationsArray;
                               AMinimumMonthlyPowerGeneration : TPowMonthlyDoublesArray;
                               AMinimumMonthlyPowerRelease    : TPowMonthlyDoublesArray;
                               ADownstreamChannelNrs          : TStringList) : WordBool;
const OPNAME = 'TPowerPlant.Populate';
var
  lIndex   : integer;
  LMinimumMonthlyPowerGeneration,
  LMinimumMonthlyPowerRelease,
  LEfficiencyFactors,
  LNetHeadFactors,
  LDischarges,
  LTailwaterElevations: TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    FChannelNr                     := AChannelNr;
    FFeatureID                     := AFeatureID;
    FFeatureName                   := AFeatureName;
    FFeatureType                   := AFeatureType;
    FFeatureSubType                := AFeatureSubType;
    FSpillChannelNr                := ASpillChannelNr;
    FPowerPlantStatus              := APowerPlantStatus;
    FMaximumGeneratorCapacity      := AMaximumGeneratorCapacity;
    FMaximumTurbineCapacity        := AMaximumTurbineCapacity;
    FHeadLoss                      := AHeadLoss;
    FDesignHead                    := ADesignHead;
    FCombinedEfficiency            := ACombinedEfficiency;
    FMaximumNetHead                := AMaximumNetHead;
    FMinimumNetHead                := AMinimumNetHead;
    FTailwaterType                 := ATailwaterType;

    LMinimumMonthlyPowerGeneration := FAppModules.FieldProperties.FieldProperty('MinEnergyGenerated');
    LMinimumMonthlyPowerRelease    := FAppModules.FieldProperties.FieldProperty('MinPowerChannelRelease');
    LEfficiencyFactors             := FAppModules.FieldProperties.FieldProperty('EfficiencyFactor');
    LNetHeadFactors                := FAppModules.FieldProperties.FieldProperty('NetHeadFactors');
    LDischarges                    := FAppModules.FieldProperties.FieldProperty('DownStreamLevel');
    LTailwaterElevations           := FAppModules.FieldProperties.FieldProperty('TailWaterElevation');

    for lIndex := LEfficiencyFactors.ArrayLow to LEfficiencyFactors.ArrayHigh do
      FEfficiencyFactors[lIndex] := AEfficiencyFactors[lIndex];
    for lIndex := LNetHeadFactors.ArrayLow to LNetHeadFactors.ArrayHigh do
      FNetHeadFactors[lIndex] := ANetHeadFactors[lIndex];
    for lIndex := LDischarges.ArrayLow to LDischarges.ArrayHigh do
      FDischarges[lIndex] := ADischarges[lIndex];
    for lIndex := LTailwaterElevations.ArrayLow to LTailwaterElevations.ArrayHigh do
      FTailwaterElevations[lIndex] := ATailwaterElevations[lIndex];
    for lIndex := LMinimumMonthlyPowerGeneration.ArrayLow to LMinimumMonthlyPowerGeneration.ArrayHigh do
      FMinimumMonthlyPowerGeneration[lIndex] := AMinimumMonthlyPowerGeneration[lIndex];
    for lIndex := LMinimumMonthlyPowerRelease.ArrayLow to LMinimumMonthlyPowerRelease.ArrayHigh do
      FMinimumMonthlyPowerRelease[lIndex] := AMinimumMonthlyPowerRelease[lIndex];

    FDownstreamPowerChannelNrs.Clear;

    for lIndex := 0 to ADownstreamChannelNrs.Count - 1 do
      FDownstreamPowerChannelNrs.Add(ADownstreamChannelNrs.Strings[lIndex]);

    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.PopulateSome (AFeatureID                     : integer;
                                   AFeatureName                   : WideString;
                                   AChannelNr                     : integer;
                                   AFeatureType                   : integer;
                                   AFeatureSubType                : integer;
                                   ASpillChannelNr                : integer;
                                   APowerPlantStatus              : WordBool;
                                   AMaximumGeneratorCapacity      : double;
                                   AMaximumTurbineCapacity        : double;
                                   AHeadLoss                      : double;
                                   ADesignHead                    : double;
                                   ACombinedEfficiency            : double;
                                   AMaximumNetHead                : double;
                                   AMinimumNetHead                : double;
                                   ANetHeadEfficiencyCount        : integer;
                                   ATailwaterElevationsCount      : integer;
                                   ATailwaterType                 : integer;
                                   AMinimumMonthlyPowerGeneration : TPowMonthlyDoublesArray;
                                   AMinimumMonthlyPowerRelease    : TPowMonthlyDoublesArray) : WordBool;
const OPNAME = 'TPowerPlant.PopulateSome';
var
  lIndex   : integer;
  LMinimumMonthlyPowerGeneration,
  LMinimumMonthlyPowerRelease : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    FChannelNr                     := AChannelNr;
    FFeatureID                     := AFeatureID;
    FFeatureName                   := AFeatureName;
    FFeatureType                   := AFeatureType;
    FFeatureSubType                := AFeatureSubType;
    FSpillChannelNr                := ASpillChannelNr;
    FPowerPlantStatus              := APowerPlantStatus;
    FMaximumGeneratorCapacity      := AMaximumGeneratorCapacity;
    FMaximumTurbineCapacity        := AMaximumTurbineCapacity;
    FHeadLoss                      := AHeadLoss;
    FDesignHead                    := ADesignHead;
    FCombinedEfficiency            := ACombinedEfficiency;
    FMaximumNetHead                := AMaximumNetHead;
    FMinimumNetHead                := AMinimumNetHead;
    FTailwaterType                 := ATailwaterType;

    LMinimumMonthlyPowerGeneration := FAppModules.FieldProperties.FieldProperty('MinEnergyGenerated');
    LMinimumMonthlyPowerRelease    := FAppModules.FieldProperties.FieldProperty('MinPowerChannelRelease');

    for lIndex := LMinimumMonthlyPowerGeneration.ArrayLow to LMinimumMonthlyPowerGeneration.ArrayHigh do
      FMinimumMonthlyPowerGeneration[lIndex] := AMinimumMonthlyPowerGeneration[lIndex];
    for lIndex := LMinimumMonthlyPowerRelease.ArrayLow to LMinimumMonthlyPowerRelease.ArrayHigh do
      FMinimumMonthlyPowerRelease[lIndex] := AMinimumMonthlyPowerRelease[lIndex];

    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.Get_FeatureID : integer;
const OPNAME = 'TPowerPlant.Get_FeatureID';
begin
  Result := 0;
  try
    Result := FFeatureID;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.Get_FeatureName : WideString;
const OPNAME = 'TPowerPlant.Get_FeatureName';
begin
  Result := '';
  try
    Result := FFeatureName;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.Get_Channel : IGeneralFlowChannel;
const OPNAME = 'TPowerPlant.Get_Channel';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                ChannelList.ChannelByChannelNumber[FChannelNr];
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.Get_FeatureType : integer;
const OPNAME = 'TPowerPlant.Get_FeatureType';
begin
  Result := 0;
  try
    Result := FFeatureType;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.Get_FeatureSubType : integer;
const OPNAME = 'TPowerPlant.Get_FeatureSubType';
begin
  Result := 0;
  try
    Result := FFeatureSubType;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.Get_PowerChannel : IGeneralFlowChannel;
const OPNAME = 'TPowerPlant.Get_PowerChannel';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                ChannelList.ChannelByChannelNumber[FChannelNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.Get_SpillChannel : IGeneralFlowChannel;
const OPNAME = 'TPowerPlant.Get_SpillChannel';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                ChannelList.ChannelByChannelNumber[FSpillChannelNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.Get_MaximumGeneratorCapacity : double;
const OPNAME = 'TPowerPlant.Get_MaximumGeneratorCapacity';
begin
  Result := 0.0;
  try
    Result := FMaximumGeneratorCapacity;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.Get_MaximumTurbineCapacity : double;
const OPNAME = 'TPowerPlant.Get_MaximumTurbineCapacity';
begin
  Result := 0.0;
  try
    Result := FMaximumTurbineCapacity;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.Get_CombinedEfficiency : double;
const OPNAME = 'TPowerPlant.Get_CombinedEfficiency';
begin
  Result := 0.0;
  try
    Result := FCombinedEfficiency;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.Get_PowerPlantStatus : WordBool;
const OPNAME = 'TPowerPlant.Get_PowerPlantStatus';
begin
  Result := FALSE;
  try
    Result := FPowerPlantStatus;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.Get_HeadLoss : double;
const OPNAME = 'TPowerPlant.Get_HeadLoss';
begin
  Result := 0.0;
  try
    Result := FHeadLoss;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.Get_DesignHead : double;
const OPNAME = 'TPowerPlant.Get_DesignHead';
begin
  Result := 0.0;
  try
    Result := FDesignHead;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.Get_MaximumNetHead : double;
const OPNAME = 'TPowerPlant.Get_MaximumNetHead';
begin
  Result := 0.0;
  try
    Result := FMaximumNetHead;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.Get_MinimumNetHead : double;
const OPNAME = 'TPowerPlant.Get_MinimumNetHead';
begin
  Result := 0.0;
  try
    Result := FMinimumNetHead;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.Get_TailWaterType : integer;
const OPNAME = 'TPowerPlant.Get_TailWaterType';
begin
  Result := 0;
  try
    Result := FTailWaterType;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.Get_DownstreamPowerChannelNrsCount : integer;
const OPNAME = 'TPowerPlant.Get_DownstreamPowerChannelNrsCount';
begin
  Result := 0;
  try
    Result := FDownstreamPowerChannelNrs.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.Get_DownstreamPowerChannelNrs : WideString;
const OPNAME = 'TPowerPlant.Get_DownstreamPowerChannelNrs';
begin
  Result := '';
  try
    Result := FDownstreamPowerChannelNrs.CommaText;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.Get_DownstreamPowerChannelNrByIndex(AIndex : integer) : integer;
const OPNAME = 'TPowerPlant.Get_DownstreamPowerChannelNrByIndex';
begin
  Result := -1;
  try
    Result := StrToInt(FDownstreamPowerChannelNrs.Strings[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPowerPlant.Set_DownstreamPowerChannelNrs (const AChannelNrs : WideString);
const OPNAME = 'TPowerPlant.Set_DownstreamPowerChannelNrs';
var
  lIndex       : integer;
  lOldCount    : integer;
  lNewNumbers  : TStringList;
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  lOldVal      : string;
  lNewVal      : string;
  lChange      : Boolean;
begin
  try
    lChange := FALSE;
    lNewNumbers := TStringList.Create;
    try
      LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
      try
        LContextData := TStringList.Create;
        try
          lNewNumbers.CommaText := AChannelNrs;
          lOldCount := FDownstreamPowerChannelNrs.Count;
          if (lOldCount <> lNewNumbers.Count) then
            DownstreamPowerChannelNrsCount := lNewNumbers.Count;
          for lIndex := 0 to lNewNumbers.Count - 1 do
          begin
            lNewVal  := lNewNumbers.Strings[lIndex];
            if (lIndex < lOldCount) then
              lOldVal  := FDownstreamPowerChannelNrs.Strings[lIndex]
            else
              lOldVal := '';
            if (lNewVal <> lOldVal) then
            begin
              lChange := TRUE;
              LContextData.Clear;
              LLoadAgent.LoadContextData_FeatureIDFieldNameID
                (LContextData, IntToStr(FFeatureID), IntToStr(lIndex+1));
              if (FAppModules.FieldProperties.UpdateFieldValue
                   ('DownStreamPowerChannelNumber', lNewVal, lOldVal, LContextData)) then
              if (lIndex < FDownstreamPowerChannelNrs.Count) then
                FDownstreamPowerChannelNrs.Strings[lIndex] := lNewVal
              else
                FDownstreamPowerChannelNrs.Add(lNewVal);
            end;
          end;
          for lIndex := lOldCount-1 downto lNewNumbers.Count do
          begin
            lChange := TRUE;
            lOldVal := FDownstreamPowerChannelNrs.Strings[lIndex];
            LContextData.Clear;
            LLoadAgent.LoadContextData_FeatureIDFieldNameID
              (LContextData, IntToStr(FFeatureID), IntToStr(lIndex+1));
            if (FAppModules.FieldProperties.UpdateFieldValue
                 ('DownStreamPowerChannelNumber', '', lOldVal, LContextData)) then
              FDownstreamPowerChannelNrs.Delete(lIndex);
          end;
          if (lChange) then
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'DownStreamPowerChannelNumber', '', '');
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    finally
      FreeAndNil(lNewNumbers);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.Get_NetHeadEfficiencyCount : integer;
const OPNAME = 'TPowerPlant.Get_NetHeadEfficiencyCount';
var
  lIndex    : integer;
  lCount    : integer;
  lContinue : Boolean;
  LNetHeadFactors: TAbstractFieldProperty;
begin
  Result := 0;
  try
    LNetHeadFactors := FAppModules.FieldProperties.FieldProperty('NetHeadFactors');
    lIndex    := LNetHeadFactors.ArrayLow;
    lContinue := TRUE;
    lCount    := 0;
    while (lContinue AND (lIndex <= LNetHeadFactors.ArrayHigh)) do
    begin
      if (FNetHeadFactors[lIndex] = NullFloat) then
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

function TPowerPlant.Get_EfficiencyFactorsArray : TEfficiencyFactorsArray;
const OPNAME = 'TPowerPlant.Get_EfficiencyFactorsArray';
begin
  Result := FEfficiencyFactors;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.Get_EfficiencyFactorByIndex(AIndex : integer) : double;
const OPNAME = 'TPowerPlant.Get_EfficiencyFactorByIndex';
begin
  Result := 0.0;
  try
    Result := FEfficiencyFactors[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.Get_NetHeadFactorsArray : TNetHeadFactorsArray;
const OPNAME = 'TPowerPlant.Get_NetHeadFactorsArray';
begin
  Result := FNetHeadFactors;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.Get_NetHeadFactorByIndex(AIndex : integer) : double;
const OPNAME = 'TPowerPlant.Get_NetHeadFactorByIndex';
begin
  Result := 0.0;
  try
    Result := FNetHeadFactors[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.Get_TailwaterElevationCount: integer;
const OPNAME = 'TPowerPlant.Get_TailwaterElevationCount';
var
  lIndex    : integer;
  lCount    : integer;
  lContinue : Boolean;
  LTailwaterElevations: TAbstractFieldProperty;
begin
  Result := 0;
  try
    LTailwaterElevations := FAppModules.FieldProperties.FieldProperty('TailWaterElevation');
    lIndex    := LTailwaterElevations.ArrayLow;
    lContinue := TRUE;
    lCount    := 0;
    while (lContinue AND (lIndex <= LTailwaterElevations.ArrayHigh)) do
    begin
      if (FTailwaterElevations[lIndex] = NullFloat) then
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

function TPowerPlant.Get_DischargeArray : TDischargesArray;
const OPNAME = 'TPowerPlant.Get_DischargeArray';
begin
  Result := FDischarges;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.Get_DischargeByIndex(AIndex : integer) : double;
const OPNAME = 'TPowerPlant.Get_DischargeByIndex';
begin
  Result := 0.0;
  try
    Result := FDischarges[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.Get_TailwaterElevationsArray : TElevationsArray;
const OPNAME = 'TPowerPlant.Get_TailwaterElevationsArray';
begin
  Result := FTailwaterElevations;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.Get_TailwaterElevationByIndex(AIndex : integer) : double;
const OPNAME = 'TPowerPlant.Get_TailwaterElevationByIndex';
begin
  Result := 0.0;
  try
    Result := FTailwaterElevations[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.Get_MinimumMonthlyPowerGenerationArray : TPowMonthlyDoublesArray;
const OPNAME = 'TPowerPlant.Get_MinimumMonthlyPowerGenerationArray';
begin
  Result := FMinimumMonthlyPowerGeneration;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.Get_MinimumPowerGenerationByMonth(AMonth : integer) : double;
const OPNAME = 'TPowerPlant.Get_MinimumPowerGenerationByMonth';
begin
  Result := 0.0;
  try
    Result := FMinimumMonthlyPowerGeneration[AMonth];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.Get_MinimumMonthlyPowerReleaseArray : TPowMonthlyDoublesArray;
const OPNAME = 'TPowerPlant.Get_MinimumMonthlyPowerReleaseArray';
begin
  Result := FMinimumMonthlyPowerRelease;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.Get_MinimumPowerReleaseByMonth(AMonth : integer) : double;
const OPNAME = 'TPowerPlant.Get_MinimumPowerReleaseByMonth';
begin
  Result := 0.0;
  try
    Result := FMinimumMonthlyPowerRelease[AMonth];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPowerPlant.Set_FeatureType (AType : integer);
const OPNAME = 'TPowerPlant.Set_FeatureType';
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

procedure TPowerPlant.Set_FeatureSubType (ASubType : integer);
const OPNAME = 'TPowerPlant.Set_FeatureSubType';
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

procedure TPowerPlant.Set_FeatureName (const AName : WideString);
const OPNAME = 'TPowerPlant.Set_FeatureName';
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
           'PowerPlantName', AName, FFeatureName, LContextData) then
        begin
          LOldValue    := FFeatureName;
          FFeatureName := AName;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'PowerPlantName',LOldValue,AName);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPowerPlant.Set_MaximumGeneratorCapacity(ACapacity : double);
const OPNAME = 'TPowerPlant.Set_MaximumGeneratorCapacity';
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
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'MaxCapGenerator', FloatToStr(ACapacity), FloatToStr(FMaximumGeneratorCapacity), LContextData) then
        begin
          LPrevValue := FMaximumGeneratorCapacity;
          FMaximumGeneratorCapacity := ACapacity;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MaxCapGenerator',FloatToStr(LPrevValue),FloatToStr(ACapacity));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPowerPlant.Set_MaximumTurbineCapacity(ACapacity : double);
const OPNAME = 'TPowerPlant.Set_MaximumTurbineCapacity';
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
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'MaxCapTurbine', FloatToStr(ACapacity), FloatToStr(FMaximumTurbineCapacity), LContextData) then
        begin
          LPrevValue := FMaximumTurbineCapacity;
          FMaximumTurbineCapacity := ACapacity;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MaxCapTurbine',FloatToStr(LPrevValue),FloatToStr(ACapacity));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPowerPlant.Set_CombinedEfficiency(AEfficiency : double);
const OPNAME = 'TPowerPlant.Set_CombinedEfficiency';
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
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'PowerEfficiency', FloatToStr(AEfficiency), FloatToStr(FCombinedEfficiency), LContextData) then
        begin
          LPrevValue := FCombinedEfficiency;
          FCombinedEfficiency := AEfficiency;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'PowerEfficiency',FloatToStr(LPrevValue),FloatToStr(AEfficiency));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPowerPlant.Set_PowerPlantStatus(AStatus : WordBool);
const OPNAME = 'TPowerPlant.Set_PowerPlantStatus';
var
  LLoadAgent     : TNetworkFeaturesSQLAgent;
  LContextData   : TStringList;
  lCurrentStatus : integer;
  lNewStatus     : integer;
begin
  try
    if AStatus then lNewStatus := 1 else lNewStatus := 0;
    if FPowerPlantStatus then lCurrentStatus := 1 else lCurrentStatus := 0;
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'PowerPlantStatus', IntToStr(lNewStatus), IntToStr(lCurrentStatus), LContextData) then
        begin
          FPowerPlantStatus := AStatus;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'PowerPlantStatus',IntToStr(lCurrentStatus),IntToStr(lNewStatus));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPowerPlant.Set_HeadLoss(ALoss : double);
const OPNAME = 'TPowerPlant.Set_HeadLoss';
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
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'HeadLoss', FloatToStr(ALoss), FloatToStr(FHeadLoss), LContextData) then
        begin
          LPrevValue := FHeadLoss;
          FHeadLoss := ALoss;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'HeadLoss',FloatToStr(LPrevValue),FloatToStr(ALoss));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPowerPlant.Set_DesignHead(AValue : double);
const OPNAME = 'TPowerPlant.Set_DesignHead';
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
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'DesignHead', FloatToStr(AValue), FloatToStr(FDesignHead), LContextData) then
        begin
          LPrevValue := FDesignHead;
          FDesignHead := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'DesignHead',FloatToStr(LPrevValue),FloatToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPowerPlant.Set_MaximumNetHead(ANetHead : double);
const OPNAME = 'TPowerPlant.Set_MaximumNetHead';
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
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'MaxNetHead', FloatToStr(ANetHead), FloatToStr(FMaximumNetHead), LContextData) then
        begin
          LPrevValue := FMaximumNetHead;
          FMaximumNetHead := ANetHead;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MaxNetHead',FloatToStr(LPrevValue),FloatToStr(ANetHead));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPowerPlant.Set_MinimumNetHead(ANetHead : double);
const OPNAME = 'TPowerPlant.Set_MinimumNetHead';
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
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'MinNetHead', FloatToStr(ANetHead), FloatToStr(FMinimumNetHead), LContextData) then
        begin
          LPrevValue := FMinimumNetHead;
          FMinimumNetHead := ANetHead;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MinNetHead',FloatToStr(LPrevValue),FloatToStr(ANetHead));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPowerPlant.Set_TailWaterType(AType : integer);
const OPNAME = 'TPowerPlant.Set_TailWaterType';
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
           'TailWaterTypeCode', FloatToStr(AType), FloatToStr(FTailWaterType), LContextData) then
        begin
          LPrevValue := FTailWaterType;
          FTailWaterType := AType;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'TailWaterTypeCode',IntToStr(LPrevValue),IntToStr(AType));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPowerPlant.Set_NetHeadEfficiencyCount (Value : integer);
const OPNAME = 'TPowerPlant.Set_NetHeadEfficiencyCount';
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
        LPrevValue := NetHeadEfficiencyCount;
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'PowerPointsCount', FloatToStr(Value), IntToStr(LPrevValue), LContextData) then
        begin
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'PowerPointsCount',IntToStr(LPrevValue),IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPowerPlant.Set_TailwaterElevationCount (Value : integer);
const OPNAME = 'TPowerPlant.Set_TailwaterElevationCount';
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
        LPrevValue := TailwaterElevationCount;
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'TailWaterCount', FloatToStr(Value), FloatToStr(LPrevValue), LContextData) then
        begin
          FAppModules.Model.StudyDataHasChanged
            (sdccEdit,'TailWaterCount',IntToStr(LPrevValue),IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPowerPlant.Set_DownstreamPowerChannelNrsCount (ACount : integer);
const OPNAME = 'TPowerPlant.Set_DownstreamPowerChannelNrsCount';
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
        LPrevValue := FDownstreamPowerChannelNrs.Count;
        LLoadAgent.LoadContextData_FeatureID
          (LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
          'DownStreamPowerChannelCount', IntToStr(ACount), IntToStr(LPrevValue), LContextData) then
        begin
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'DownStreamPowerChannelCount',IntToStr(LPrevValue),IntToStr(ACount));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPowerPlant.Set_EfficiencyFactorByIndex(AIndex : integer;
                                                 AValue : double);
const OPNAME = 'TPowerPlant.Set_EfficiencyFactorByIndex';
var
  lIndex : integer;
  LEfficiencyFactors: TAbstractFieldProperty;
begin
  try
    LEfficiencyFactors := FAppModules.FieldProperties.FieldProperty('EfficiencyFactor');
    if (AIndex >= LEfficiencyFactors.ArrayLow) and (AIndex <= LEfficiencyFactors.ArrayHigh) then
    begin
      if (FEfficiencyFactors[AIndex] <> AValue) then
      begin
        if (AValue = NullFloat) then
        begin
          for lIndex := AIndex+1 to LEfficiencyFactors.ArrayHigh do
          begin
            WriteEfficiencyFactorToDB (lIndex-1, FEfficiencyFactors[lIndex]);
            WriteNetHeadFactorToDB (lIndex-1, FNetHeadFactors[lIndex]);
          end;
          WriteEfficiencyFactorToDB (LEfficiencyFactors.ArrayHigh, NullFloat);
          WriteNetHeadFactorToDB (LEfficiencyFactors.ArrayHigh, NullFloat);
        end
        else
        begin
          for lIndex := 1 to AIndex-1 do
          begin
            if (EfficiencyFactorByIndex[lIndex] = NullFloat) then
              WriteEfficiencyFactorToDB(lIndex, 0.0);
            if (NetHeadFactorByIndex[lIndex] = NullFloat) then
              WriteNetHeadFactorToDB (lIndex, 0.0);
          end;
          WriteEfficiencyFactorToDB(AIndex, AValue);
          if (NetHeadFactorByIndex[AIndex] = NullFloat) then
            WriteNetHeadFactorToDB (AIndex, 0.0);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPowerPlant.WriteEfficiencyFactorToDB(AIndex : integer;
                                                AValue : double);
const OPNAME = 'TPowerPlant.WriteEfficiencyFactorToDB';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  lOldValue    : string;
  lNewValue    : string;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureIDFieldNameID
          (LContextData, IntToStr(FFeatureID), IntToStr(AIndex));
        LContextData.Add('FactorCode=1');
        if (AValue = NullFloat) then
          lNewValue := ''
        else
          lNewValue := FloatToStr(AValue);
        if (FEfficiencyFactors[AIndex] = NullFloat) then
          lOldValue := ''
        else
          lOldValue := FloatToStr(FEfficiencyFactors[AIndex]);
        if FAppModules.FieldProperties.UpdateFieldValue(
           'EfficiencyFactor', lNewValue, lOldValue, LContextData) then
        begin
          FEfficiencyFactors[AIndex] := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'EfficiencyFactor',lOldValue,lNewValue);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPowerPlant.Set_NetHeadFactorByIndex(AIndex : integer;
                                              AValue : double);
const OPNAME = 'TPowerPlant.Set_NetHeadFactorByIndex';
var
  lIndex : integer;
  LNetHeadFactors: TAbstractFieldProperty;
begin
  try
    LNetHeadFactors := FAppModules.FieldProperties.FieldProperty('NetHeadFactors');
    if (AIndex >= LNetHeadFactors.ArrayLow) and (AIndex <= LNetHeadFactors.ArrayHigh) then
    begin
      if (FNetHeadFactors[AIndex] <> AValue) then
      begin
        if (AValue = NullFloat) then
        begin
          for lIndex := AIndex+1 to LNetHeadFactors.ArrayHigh do
          begin
            WriteEfficiencyFactorToDB (lIndex-1, FEfficiencyFactors[lIndex]);
            WriteNetHeadFactorToDB (lIndex-1, FNetHeadFactors[lIndex]);
          end;
          WriteEfficiencyFactorToDB (LNetHeadFactors.ArrayHigh, NullFloat);
          WriteNetHeadFactorToDB (LNetHeadFactors.ArrayHigh, NullFloat);
        end
        else
        begin
          for lIndex := 1 to AIndex-1 do
          begin
            if (EfficiencyFactorByIndex[lIndex] = NullFloat) then
              WriteEfficiencyFactorToDB(lIndex, 0.0);
            if (NetHeadFactorByIndex[lIndex] = NullFloat) then
              WriteNetHeadFactorToDB (lIndex, 0.0);
          end;
          WriteNetHeadFactorToDB(AIndex, AValue);
          if (EfficiencyFactorByIndex[AIndex] = NullFloat) then
            WriteEfficiencyFactorToDB (AIndex, 0.0);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPowerPlant.WriteNetHeadFactorToDB(AIndex : integer;
                                             AValue : double);
const OPNAME = 'TPowerPlant.WriteNetHeadFactorToDB';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  lOldValue    : string;
  lNewValue    : string;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureIDFieldNameID
          (LContextData, IntToStr(FFeatureID), IntToStr(AIndex));
        LContextData.Add('FactorCode=2');
        if (AValue = NullFloat) then
          lNewValue := ''
        else
          lNewValue := FloatToStr(AValue);
        if (FNetHeadFactors[AIndex] = NullFloat) then
          lOldValue := ''
        else
          lOldValue := FloatToStr(FNetHeadFactors[AIndex]);
        if FAppModules.FieldProperties.UpdateFieldValue(
           'NetHeadFactors', lNewValue, lOldValue, LContextData) then
        begin
          FNetHeadFactors[AIndex] := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'NetHeadFactors',lOldValue,lNewValue);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPowerPlant.Set_DischargeByIndex(AIndex : integer;
                                          AValue : double);
const OPNAME = 'TPowerPlant.Set_DischargeByIndex';
var
  lIndex : integer;
  LDischarges: TAbstractFieldProperty;
begin
  try
    LDischarges := FAppModules.FieldProperties.FieldProperty('DownStreamLevel');
    if (AIndex >= LDischarges.ArrayLow) and (AIndex <= LDischarges.ArrayHigh) then
    begin
      if (FDischarges[AIndex] <> AValue) then
      begin
        if (AValue = NullFloat) then
        begin
          for lIndex := AIndex+1 to LDischarges.ArrayHigh do
          begin
            WriteDischargeToDB (lIndex-1, FDischarges[lIndex]);
            WriteTailwaterElevationToDB (lIndex-1, FTailwaterElevations[lIndex]);
          end;
          WriteDischargeToDB (LDischarges.ArrayHigh, NullFloat);
          WriteTailwaterElevationToDB (LDischarges.ArrayHigh, NullFloat);
        end
        else
        begin
          for lIndex := 1 to AIndex-1 do
          begin
            if (DischargeByIndex[lIndex] = NullFloat) then
              WriteDischargeToDB(lIndex, 0.0);
            if (TailwaterElevationByIndex[lIndex] = NullFloat) then
              WriteTailwaterElevationToDB (lIndex, 0.0);
          end;
          WriteDischargeToDB(AIndex, AValue);
          if (TailwaterElevationByIndex[AIndex] = NullFloat) then
            WriteTailwaterElevationToDB (AIndex, 0.0);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPowerPlant.WriteDischargeToDB(AIndex : integer;
                                         AValue : double);
const OPNAME = 'TPowerPlant.WriteDischargeToDB';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  lOldValue    : string;
  lNewValue    : string;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureIDFieldNameID
          (LContextData, IntToStr(FFeatureID), IntToStr(AIndex));
        LContextData.Add('FactorCode=3');
        if (AValue = NullFloat) then
          lNewValue := ''
        else
          lNewValue := FloatToStr(AValue);
        if (FDischarges[AIndex] = NullFloat) then
          lOldValue := ''
        else
          lOldValue := FloatToStr(FDischarges[AIndex]);
        if FAppModules.FieldProperties.UpdateFieldValue(
           'DownStreamLevel', lNewValue, lOldValue, LContextData) then
        begin
          FDischarges[AIndex] := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'DownStreamLevel',lOldValue,lNewValue);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPowerPlant.Set_TailwaterElevationByIndex(AIndex : integer;
                                                   AValue : double);
const OPNAME = 'TPowerPlant.Set_TailwaterElevationByIndex';
var
  lIndex : integer;
  LTailwaterElevations: TAbstractFieldProperty;
begin
  try
    LTailwaterElevations := FAppModules.FieldProperties.FieldProperty('TailWaterElevation');
    if (AIndex >= LTailwaterElevations.ArrayLow) and (AIndex <= LTailwaterElevations.ArrayHigh) then
    begin
      if (FTailwaterElevations[AIndex] <> AValue) then
      begin
        if (AValue = NullFloat) then
        begin
          for lIndex := AIndex+1 to LTailwaterElevations.ArrayHigh do
          begin
            WriteDischargeToDB (lIndex-1, FDischarges[lIndex]);
            WriteTailwaterElevationToDB (lIndex-1, FTailwaterElevations[lIndex]);
          end;
          WriteDischargeToDB (LTailwaterElevations.ArrayHigh, NullFloat);
          WriteTailwaterElevationToDB (LTailwaterElevations.ArrayHigh, NullFloat);
        end
        else
        begin
          for lIndex := 1 to AIndex-1 do
          begin
            if (DischargeByIndex[lIndex] = NullFloat) then
              WriteDischargeToDB(lIndex, 0.0);
            if (TailwaterElevationByIndex[lIndex] = NullFloat) then
              WriteTailwaterElevationToDB (lIndex, 0.0);
          end;
          WriteTailwaterElevationToDB(AIndex, AValue);
          if (DischargeByIndex[AIndex] = NullFloat) then
            WriteDischargeToDB (AIndex, 0.0);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPowerPlant.WriteTailwaterElevationToDB(AIndex : integer;
                                                  AValue : double);
const OPNAME = 'TPowerPlant.WriteTailwaterElevationToDB';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  lOldValue    : string;
  lNewValue    : string;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureIDFieldNameID
          (LContextData, IntToStr(FFeatureID), IntToStr(AIndex));
        LContextData.Add('FactorCode=4');
        if (AValue = NullFloat) then
          lNewValue := ''
        else
          lNewValue := FloatToStr(AValue);
        if (FTailwaterElevations[AIndex] = NullFloat) then
          lOldValue := ''
        else
          lOldValue := FloatToStr(FTailwaterElevations[AIndex]);
        if FAppModules.FieldProperties.UpdateFieldValue(
           'TailWaterElevation', lNewValue, lOldValue, LContextData) then
        begin
          FTailwaterElevations[AIndex] := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'TailWaterElevation',lOldValue,lNewValue);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPowerPlant.Set_MinimumPowerGenerationByMonth(AMonth : integer;
                                                       AValue : double);
const OPNAME = 'TPowerPlant.Set_MinimumPowerGenerationByMonth';
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
          (LContextData, IntToStr(FFeatureID), IntToStr(AMonth));
        LContextData.Add('PowerCode=1');
        if FAppModules.FieldProperties.UpdateFieldValue(
           'MinEnergyGenerated', FloatToStr(AValue), FloatToStr(FMinimumMonthlyPowerGeneration[AMonth]), LContextData) then
        begin
          LPrevValue := FMinimumMonthlyPowerGeneration[AMonth];
          FMinimumMonthlyPowerGeneration[AMonth] := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MinEnergyGenerated',FloatToStr(LPrevValue),FloatToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPowerPlant.Set_MinimumPowerReleaseByMonth(AMonth : integer;
                                                    AValue : double);
const OPNAME = 'TPowerPlant.Set_MinimumPowerReleaseByMonth';
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
          (LContextData, IntToStr(FFeatureID), IntToStr(AMonth));
        LContextData.Add('PowerCode=2');
        if FAppModules.FieldProperties.UpdateFieldValue(
           'MinPowerChannelRelease', FloatToStr(AValue), FloatToStr(FMinimumMonthlyPowerRelease[AMonth]), LContextData) then
        begin
          LPrevValue := FMinimumMonthlyPowerRelease[AMonth];
          FMinimumMonthlyPowerRelease[AMonth] := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MinPowerChannelRelease',FloatToStr(LPrevValue),FloatToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlant.ValidatePowerPlantName(AErrorMessages : TStrings): WordBool;
const OPNAME = 'TPowerPlant.ValidatePowerPlantName';
var
  lIndex          : integer;
  lPowerPlantList : TPowerPlantList;
  lPowerPlant     : TPowerPlant;
  lMessage        : string;
  lUnique         : Boolean;
begin
  Result := True;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('PowerPlantName', FFeatureName, lMessage)) then
      AErrorMessages.Add('WARNING:' +FFeatureName+ ':'+lMessage)
    else
    begin
      lPowerPlantList := TYieldModelDataObject(FAppModules.Model.ModelData).
                           CastNetworkFeaturesData.CastPowerPlantList;
      lUnique := TRUE;
      lIndex  := 0;
      while (lUnique AND (lIndex < lPowerPlantList.PowerPlantCount)) do
      begin
        lPowerPlant := lPowerPlantList.CastPowerPlantByIndex(lIndex);
        if ((FFeatureID <> lPowerPlant.FeatureID) AND
            (UpperCase(Trim(FFeatureName)) = UpperCase(Trim(lPowerPlant.FeatureName)))) then
        begin
          lMessage := FAppModules.Language.GetString('ContextValidation.DuplicatePowerPlantName');
          AErrorMessages.Add('WARNING:' +Format(lMessage, [FFeatureName]));
          lUnique := FALSE;
        end
        else
          lIndex := lIndex + 1;
      end;
      Result := lUnique;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlant.ValidatePowerSpillUpstreamNode(AErrorMessages : TStrings): WordBool;
const OPNAME = 'TPowerPlant.ValidatePowerSpillUpstreamNode';
var
  lMessage : WideString;
  lChannel : IGeneralFlowChannel;
begin
  Result := True;
  try
    if (FChannelNr <> 0) then
    begin
      lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                    ChannelList.ChannelByChannelNumber[FChannelNr];
      if (lChannel.UpStreamNodeNumber = 0) then
      begin
        lMessage := FAppModules.Language.GetString('ContextValidation.InvalidUpstreamNodeNumber');
        AErrorMessages.Add('ERROR:' +Format(lMessage, [FFeatureName]));
        Result := False;
      end
      else
      begin
        if (NOT lChannel.Validate(lMessage,'UpNodeNumber')) then
        begin
          Result := False;
          AErrorMessages.Add('ERROR:' +FFeatureName+ ':'+lMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlant.ValidatePowerDownstreamNode(AErrorMessages : TStrings): WordBool;
const OPNAME = 'TPowerPlant.ValidatePowerDownstreamNode';
var
  LMessage : WideString;
  lChannel : IGeneralFlowChannel;
begin
  Result := True;
  try
    if (FChannelNr <> 0) then
    begin
      lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                    ChannelList.ChannelByChannelNumber[FChannelNr];
      Result := lChannel.Validate(LMessage,'DownNodeNumber');
      if not Result then
       AErrorMessages.Add('ERROR:' +FFeatureName+ ':'+LMessage);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlant.ValidateSpillDownstreamNode(AErrorMessages : TStrings): WordBool;
const OPNAME = 'TPowerPlant.ValidateSpillDownstreamNode';
var
  LMessage : WideString;
  lChannel : IGeneralFlowChannel;
begin
  Result := True;
  try
    if (FChannelNr <> 0) then
    begin
      lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                    ChannelList.ChannelByChannelNumber[FSpillChannelNr];
      Result := lChannel.Validate(LMessage,'DownNodeNumber');
      if not Result then
       AErrorMessages.Add('ERROR:' +FFeatureName+ ':'+LMessage);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlant.ValidateMaximumGeneratorCapacity(AErrorMessages : TStrings): WordBool;
const OPNAME = 'TPowerPlant.ValidateMaximumGeneratorCapacity';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    Result   := FAppModules.FieldProperties.ValidateFieldProperty
                  ('MaxCapGenerator', FloatToStr(FMaximumGeneratorCapacity), lMessage);
    if (NOT Result) then
      AErrorMessages.Add('ERROR:' +FFeatureName+ ':'+lMessage);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlant.ValidateMaximumTurbineCapacity(AErrorMessages : TStrings): WordBool;
const OPNAME = 'TPowerPlant.ValidateMaximumTurbineCapacity';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    Result   := FAppModules.FieldProperties.ValidateFieldProperty
                  ('MaxCapTurbine', FloatToStr(FMaximumTurbineCapacity), lMessage);
    if (NOT Result) then
      AErrorMessages.Add('ERROR:' +FFeatureName+ ':'+lMessage);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlant.ValidateHeadLoss(AErrorMessages : TStrings): WordBool;
const OPNAME = 'TPowerPlant.ValidateHeadLoss';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    Result   := FAppModules.FieldProperties.ValidateFieldProperty
                  ('HeadLoss', FloatToStr(FHeadLoss), lMessage);
    if (NOT Result) then
      AErrorMessages.Add('ERROR:' +FFeatureName+ ':'+lMessage);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlant.ValidateCombinedEfficiency(AErrorMessages : TStrings): WordBool;
const OPNAME = 'TPowerPlant.ValidateCombinedEfficiency';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
                ('PowerEfficiency', FloatToStr(FCombinedEfficiency), lMessage);
    if (NOT Result) then
      AErrorMessages.Add('ERROR:' +FFeatureName+ ':'+lMessage);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlant.ValidateDesignHead(AErrorMessages : TStrings): WordBool;
const OPNAME = 'TPowerPlant.ValidateDesignHead';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    Result   := FAppModules.FieldProperties.ValidateFieldProperty
                  ('DesignHead', FloatToStr(FDesignHead), lMessage);
    if (NOT Result) then
      AErrorMessages.Add('ERROR:' +FFeatureName+ ':'+lMessage);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlant.ValidateMaximumNetHead(AErrorMessages : TStrings): WordBool;
const OPNAME = 'TPowerPlant.ValidateMaximumNetHead';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    Result   := FAppModules.FieldProperties.ValidateFieldProperty
                  ('MaxNetHead', FloatToStr(FMaximumNetHead), lMessage);
    if (NOT Result) then
      AErrorMessages.Add('ERROR:' +FFeatureName+ ':'+lMessage);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlant.ValidateMinimumNetHead(AErrorMessages : TStrings): WordBool;
const OPNAME = 'TPowerPlant.ValidateMinimumNetHead';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    Result   := FAppModules.FieldProperties.ValidateFieldProperty
                  ('MinNetHead', FloatToStr(FMinimumNetHead), lMessage);
    if (NOT Result) then
      AErrorMessages.Add('ERROR:' +FFeatureName+ ':'+lMessage);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlant.ValidateTailwaterType(AErrorMessages : TStrings): WordBool;
const OPNAME = 'TPowerPlant.ValidateTailwaterType';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    Result   := FAppModules.FieldProperties.ValidateFieldProperty
                  ('TailWaterTypeCode', IntToStr(FTailwaterType), lMessage);
    if (NOT Result) then
      AErrorMessages.Add('ERROR:' +FFeatureName+ ':'+lMessage);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlant.ValidateDownstreamPowerPlants(AErrorMessages : TStrings): WordBool;
const OPNAME = 'TPowerPlant.ValidateDownstreamPowerPlants';
var
  lIndex            : integer;
  lMessage          : string;
  lResult           : Boolean;
  lStopOnFirstError : Boolean;
  lChannel          : IGeneralFlowChannel;
  lChannelNr        : integer;
  lValid            : Boolean;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    lIndex := 0;
    while (lIndex < DownstreamPowerChannelNrsCount) do
    begin
      lMessage   := '';
      lChannelNr := DownstreamPowerChannelNrByIndex[lIndex];
      lValid     := FAppModules.FieldProperties.ValidateFieldProperty
                      ('DownStreamPowerChannelNumber', IntToStr(lChannelNr), lMessage, lIndex+1);
      if (NOT lValid) then
      begin
        lResult := FALSE;
        AErrorMessages.Add('ERROR:' +FFeatureName+ ':'+lMessage);
      end;
      if (lResult OR (NOT lStopOnFirstError)) then
      begin
        lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                      NetworkElementData.ChannelList.ChannelByChannelNumber[lChannelNr];
        if (lChannel = nil) then
        begin
          lResult := FALSE;
          lMessage := FAppModules.Language.GetString('ContextValidation.InvalidDownstreamPowerChannelNumber');
          AErrorMessages.Add('ERROR:' +Format(lMessage, [FFeatureName]));
        end;
      end;
      if ((NOT lResult) AND lStopOnFirstError) then
        Break;
      lIndex := lIndex + 1;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlant.ValidateEfficiencyFactors(AErrorMessages : TStrings; AErrorColumns: TStrings): WordBool;
const OPNAME = 'TPowerPlant.ValidateEfficiencyFactors';
var
  lIndex            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lValid            : Boolean;
  lResult           : Boolean;
  LEfficiencyFactors: TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    lStopOnFirstError  := FAppModules.GlobalData.StopOnFirstErr;
    LEfficiencyFactors := FAppModules.FieldProperties.FieldProperty('EfficiencyFactor');
    for lIndex := LEfficiencyFactors.ArrayLow to LEfficiencyFactors.ArrayHigh do
    begin
      lMessage := '';
      lValid := FAppModules.FieldProperties.ValidateFieldProperty
                  ('EfficiencyFactor', FloatToStr(FEfficiencyFactors[lIndex]),
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

function TPowerPlant.ValidateNetHeadFactors(AErrorMessages : TStrings; AErrorColumns: TStrings): WordBool;
const OPNAME = 'TPowerPlant.ValidateNetHeadFactors';
var
  lIndex            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lValid            : Boolean;
  lResult           : Boolean;
  LNetHeadFactors: TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    LNetHeadFactors   := FAppModules.FieldProperties.FieldProperty('NetHeadFactors');
    for lIndex := LNetHeadFactors.ArrayLow to LNetHeadFactors.ArrayHigh do
    begin
      lMessage := '';
      lValid := FAppModules.FieldProperties.ValidateFieldProperty
                  ('NetHeadFactors', FloatToStr(FNetHeadFactors[lIndex]),
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

function TPowerPlant.ValidateTailwaterDischarges(AErrorMessages : TStrings; AErrorColumns: TStrings): WordBool;
const OPNAME = 'TPowerPlant.ValidateTailwaterDischarges';
var
  lIndex            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lValid            : Boolean;
  lResult           : Boolean;
  LDischarges: TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    LDischarges       := FAppModules.FieldProperties.FieldProperty('DownStreamLevel');
    for lIndex := LDischarges.ArrayLow to LDischarges.ArrayHigh do
    begin
      lMessage := '';
      lValid := FAppModules.FieldProperties.ValidateFieldProperty
                  ('DownStreamLevel', FloatToStr(FDischarges[lIndex]),
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

function TPowerPlant.ValidateTailwaterElevations(AErrorMessages : TStrings; AErrorColumns: TStrings): WordBool;
const OPNAME = 'TPowerPlant.ValidateTailwaterElevations';
var
  lIndex            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lValid            : Boolean;
  lResult           : Boolean;
  LTailwaterElevations: TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    lStopOnFirstError    := FAppModules.GlobalData.StopOnFirstErr;
    LTailwaterElevations := FAppModules.FieldProperties.FieldProperty('TailWaterElevation');
    for lIndex := LTailwaterElevations.ArrayLow to LTailwaterElevations.ArrayHigh do
    begin
      lMessage := '';
      lValid := FAppModules.FieldProperties.ValidateFieldProperty
                  ('TailWaterElevation', FloatToStr(FTailwaterElevations[lIndex]),
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

function TPowerPlant.ValidatePowerGenerations(AErrorMessages : TStrings; AErrorColumns: TStrings): WordBool;
const OPNAME = 'TPowerPlant.ValidatePowerGenerations';
var
  lIndex            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lValid            : Boolean;
  lResult           : Boolean;
  LMinimumMonthlyPowerGeneration: TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    LMinimumMonthlyPowerGeneration := FAppModules.FieldProperties.FieldProperty('MinEnergyGenerated');
    for lIndex := LMinimumMonthlyPowerGeneration.ArrayLow to LMinimumMonthlyPowerGeneration.ArrayHigh do
    begin
      lMessage := '';
      lValid := FAppModules.FieldProperties.ValidateFieldProperty
                  ('MinEnergyGenerated', FloatToStr(FMinimumMonthlyPowerGeneration[lIndex]),
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

function TPowerPlant.ValidatePowerReleases(AErrorMessages : TStrings; AErrorColumns: TStrings): WordBool;
const OPNAME = 'TPowerPlant.ValidatePowerReleases';
var
  lIndex            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lValid            : Boolean;
  lResult           : Boolean;
  LMinimumMonthlyPowerRelease: TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    LMinimumMonthlyPowerRelease := FAppModules.FieldProperties.FieldProperty('MinPowerChannelRelease');
    for lIndex := LMinimumMonthlyPowerRelease.ArrayLow to LMinimumMonthlyPowerRelease.ArrayHigh do
    begin
      lMessage := '';
      lValid := FAppModules.FieldProperties.ValidateFieldProperty
                  ('MinPowerChannelRelease', FloatToStr(FMinimumMonthlyPowerRelease[lIndex]),
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

function TPowerPlant.Validate (var AErrors : WideString; const AContext    : WideString='') : WordBool; safecall;
const OPNAME = 'TPowerPlant.Validate';
var
  lStopOnFirstError : Boolean;
  lErrorList        : TStringList;
  LErrorCols        : TStringList;
begin
  Result := FALSE;
  try
    lErrorList := TStringList.Create;
    LErrorCols := TStringList.Create;
    try
      if (AContext = 'PowerPlantName') then
        Result := ValidatePowerPlantName(lErrorList)
      else
      if (AContext = 'PowerSpillUpstreamNode') then
        Result := ValidatePowerSpillUpstreamNode(lErrorList)
      else
      if (AContext = 'PowerDownstreamNode') then
        Result := ValidatePowerDownstreamNode(lErrorList)
      else
      if (AContext = 'SpillDownstreamNode') then
        Result := ValidateSpillDownstreamNode(lErrorList)
      else
      if (AContext = 'MaxCapGenerator') then
        Result := ValidateMaximumGeneratorCapacity(lErrorList)
      else
      if (AContext = 'MaxCapTurbine') then
        Result := ValidateMaximumTurbineCapacity(lErrorList)
      else
      if (AContext = 'HeadLoss') then
        Result := ValidateHeadLoss(lErrorList)
      else
      if (AContext = 'DownstreamPowerPlants') then
        Result := ValidateDownstreamPowerPlants(lErrorList)
      else
      if (AContext = 'PowerEfficiency') then
        Result := ValidateCombinedEfficiency(lErrorList)
      else
      if (AContext = 'DesignHead') then
        Result := ValidateDesignHead(lErrorList)
      else
      if (AContext = 'MaxNetHead') then
        Result := ValidateMaximumNetHead(lErrorList)
      else
      if (AContext = 'MinNetHead') then
        Result := ValidateMinimumNetHead(lErrorList)
      else
      if (AContext = 'EfficiencyFactor') then
      begin
        Result := ValidateEfficiencyFactors(lErrorList,lErrorCols);
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
      if (AContext = 'NetHeadFactors') then
      begin
        Result := ValidateNetHeadFactors(lErrorList,lErrorCols);
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
      if (AContext = 'TailWaterTypeCode') then
        Result := ValidateTailwaterType(lErrorList)
      else
      if (AContext = 'DownStreamLevel') then
      begin
        Result := ValidateTailwaterDischarges(lErrorList,lErrorCols);
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
      if (AContext = 'TailWaterElevation') then
      begin
        Result := ValidateTailwaterElevations(lErrorList,lErrorCols);
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
      if (AContext = 'PowerGenerations') then
      begin
        Result := ValidatePowerGenerations(lErrorList,lErrorCols);
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
      if (AContext = 'PowerRelease') then
      begin
        Result := ValidatePowerReleases(lErrorList,lErrorCols);
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
        if (NOT ValidatePowerPlantName(lErrorList)) then
          Result := FALSE;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateMaximumGeneratorCapacity(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateMaximumTurbineCapacity(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateHeadLoss(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateDownstreamPowerPlants(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateCombinedEfficiency(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateDesignHead(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateMaximumNetHead(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateMinimumNetHead(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateEfficiencyFactors(lErrorList,LErrorCols)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateNetHeadFactors(lErrorList,LErrorCols)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateTailwaterType(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateTailwaterDischarges(lErrorList,LErrorCols)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateTailwaterElevations(lErrorList,LErrorCols)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidatePowerGenerations(lErrorList,LErrorCols)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidatePowerReleases(lErrorList,LErrorCols)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidatePowerSpillUpstreamNode(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidatePowerDownstreamNode(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateSpillDownstreamNode(lErrorList)) then
            Result := FALSE;
        end;
      end;
      AErrors := AErrors + lErrorList.Text;
    finally
      FreeAndNil(lErrorList);
      FreeAndNil(LErrorCols);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlant.GetKeyValues (const AParamField : WideString;
                                   const AFieldIndex : WideString) : WideString;
const OPNAME = 'TPowerPlant.GetKeyValues';
begin
  Result := '';
  try
    Result := GetDefaultKey;
    if (Result = '') then
      Result := 'Identifier=' + IntToStr(FFeatureID)
    else
      Result := Result + ',Identifier=' + IntToStr(FFeatureID);
    if (AParamField = 'EfficiencyFactor') then
      Result := Result + ',FactorCode=1';
    if (AParamField = 'NetHeadFactors') then
      Result := Result + ',FactorCode=2';
    if (AParamField = 'DownStreamLevel') then
      Result := Result + ',FactorCode=3';
    if (AParamField = 'TailWaterElevation') then
      Result := Result + ',FactorCode=4';
    if (AParamField = 'MinEnergyGenerated') then
      Result := Result + ',PowerCode=1';
    if (AParamField = 'MinPowerChannelRelease') then
      Result := Result + ',PowerCode=2';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{******************************************************************************}
{ TPowerPlantList                                                             *}
{******************************************************************************}

function TPowerPlantList._AddRef: Integer;
const OPNAME = 'TPowerPlantList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlantList._Release: Integer;
const OPNAME = 'TPowerPlantList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPowerPlantList.CreateMemberObjects;
const OPNAME = 'TPowerPlantList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPowerPlantList := TObjectList.Create(True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPowerPlantList.DestroyMemberObjects;
const OPNAME = 'TPowerPlantList.DestroyMemberObjects';
begin
  try
    //while (FPowerPlantList.Count > 0) do
    //  DeletePowerPlantWithIndex(0);
    FreeAndNil(FPowerPlantList);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlantList.Initialise: boolean;
const OPNAME = 'TPowerPlantList.Initialise';
begin
  Result := inherited Initialise;
  try
    FPowerPlantList.Clear;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlantList.AddPowerPlant (AFeature : TPowerPlant): boolean;
const OPNAME = 'TPowerPlantList.AddPowerPlant';
begin
  Result := False;
  try
    if (AFeature <> nil) then
    begin
      FPowerPlantList.Add(AFeature);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlantList.NewPowerPlant : TPowerPlant;
const OPNAME = 'TPowerPlantList.NewPowerPlant';
var
  lFeature : TPowerPlant;
begin
  Result := nil;
  try
    lFeature := TPowerPlant.Create(FAppModules);
    AddPowerPlant(lFeature);
    Result := lFeature;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlantList.CreatePowerPlant : IPowerPlant;
const OPNAME = 'TPowerPlantList.CreatePowerPlant';
var
  LFeatureID               : integer;
  LLoadAgent               : TNetworkFeaturesSQLAgent;
  LFeature                 : TPowerPlant;
  lMinPowerGeneration      : TPowMonthlyDoublesArray;
  lMinPowerRelease         : TPowMonthlyDoublesArray;
  LMinPowerGenerationField : TAbstractFieldProperty;
  LMinPowerReleaseField    : TAbstractFieldProperty;
  lIndex                   : integer;
  lPowerChannel            : IGeneralFlowChannel;
  lSpillChannel            : IGeneralFlowChannel;
  lNetworkElementData      : TNetworkElementData;
  lDestroy                 : Boolean;
begin
  Result := nil;
  try
    LMinPowerGenerationField := FAppModules.FieldProperties.FieldProperty('MinEnergyGenerated');
    if not Assigned(LMinPowerGenerationField) then
      raise Exception.Create('Field (MinEnergyGenerated) not found in field properties');
    SetLength(lMinPowerGeneration,LMinPowerGenerationField.ArrayLength);

    LMinPowerReleaseField := FAppModules.FieldProperties.FieldProperty('MinPowerChannelRelease');
    if not Assigned(LMinPowerReleaseField) then
      raise Exception.Create('Field (MinPowerChannelRelease) not found in field properties');
    SetLength(lMinPowerRelease,LMinPowerReleaseField.ArrayLength);

    for lIndex := LMinPowerGenerationField.ArrayLow to LMinPowerGenerationField.ArrayHigh do
      lMinPowerGeneration[lIndex] := 0;
    for lIndex := LMinPowerReleasefield.ArrayLow to LMinPowerReleasefield.ArrayHigh do
     lMinPowerRelease[lIndex]    := 0;

    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      lNetworkElementData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData;
      lPowerChannel       := lNetworkElementData.CastChannelList.CreateNewChannel;
      lSpillChannel       := lNetworkElementData.CastChannelList.CreateNewChannel;

      LFeatureID := 0;
      lDestroy   := TRUE;
      if ((lPowerChannel <> nil) AND
          (lSpillChannel <> nil)) then
      begin
        lPowerChannel.ChannelType    := 3;
        lPowerChannel.ChannelSubType := 1;
        lSpillChannel.ChannelType    := 3;
        lSpillChannel.ChannelSubType := 2;

        if (LLoadAgent.InsertPowerPlant(LFeatureID, lPowerChannel.ChannelNumber, lSpillChannel.ChannelNumber)) then
        begin
          lDestroy := FALSE;
          LFeature := NewPowerPlant;
          LFeature.Initialise;
          LFeature.PopulateSome
            (LFeatureID,
             UpperCase(FAppModules.Language.GetString('NetworkFeatures.PowerPlant')) + ' ' + IntToStr(LFeatureID),
             lPowerChannel.ChannelNumber, 9, 0, lSpillChannel.ChannelNumber, TRUE,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, lMinPowerGeneration, lMinPowerRelease);
          Result := LFeature;
        end;
      end;
      if (lDestroy) then
      begin
        if (lPowerChannel <> nil) then
          lNetworkElementData.ChannelList.RemoveChannelWithID(lPowerChannel.ChannelID);
        if (lSPillChannel <> nil) then
          lNetworkElementData.ChannelList.RemoveChannelWithID(lSpillChannel.ChannelID);
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlantList.RemovePowerPlantWithID (AFeatureID : integer) : WordBool;
const OPNAME = 'TPowerPlantList.RemovePowerPlantWithID';
var
  lLoadAgent : TNetworkFeaturesSQLAgent;
begin
  Result := FALSE;
  try
    if (AFeatureID > 0) then
    begin
      LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
      try
        if LLoadAgent.DeletePowerPlant(AFeatureID) then
        begin
          DeletePowerPlantWithID(AFeatureID);
          Result := TRUE;
        end;  
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlantList.DeletePowerPlantWithID(AFeatureID : integer) : WordBool;
const OPNAME = 'TPowerPlantList.DeletePowerPlantWithID';
var
  lIndex   : integer;
begin
  Result := FALSE;
  try
    for LIndex := 0 to FPowerPlantList.Count -1 do
    begin
      if (TPowerPlant(FPowerPlantList[LIndex]).FeatureID = AFeatureID) then
      begin
        FPowerPlantList.Remove(FPowerPlantList[LIndex]);
        Result := True;
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlantList.DeletePowerPlantWithIndex(AIndex : integer) : WordBool;
const OPNAME = 'TPowerPlantList.DeletePowerPlantWithIndex';
begin
  Result := FALSE;
  try
    if (AIndex >= 0) and (AIndex < FPowerPlantList.Count) then
    begin
      FPowerPlantList.Remove(FPowerPlantList[AIndex]);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlantList.Get_PowerPlantByIndex(AIndex: integer): IPowerPlant;
const OPNAME = 'TPowerPlantList.Get_PowerPlantByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FPowerPlantList.Count) then
      Result := TPowerPlant(FPowerPlantList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlantList.Get_PowerPlantByID(AFeatureID : integer): IPowerPlant;
const OPNAME = 'TPowerPlantList.Get_PowerPlantByID';
var
 lIndex   : integer;
 lFeature : TPowerPlant;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FPowerPlantList.Count)) do
    begin
      lFeature := TPowerPlant(FPowerPlantList.Items[lIndex]);
      if (lFeature.FeatureID = AFeatureID) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlantList.CastPowerPlantByIndex (AIndex : integer): TPowerPlant;
const OPNAME = 'TPowerPlantList.CastPowerPlantByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FPowerPlantList.Count) then
      Result := TPowerPlant(FPowerPlantList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlantList.CastPowerPlantByID(AFeatureID : integer): TPowerPlant;
const OPNAME = 'TPowerPlantList.CastPowerPlantByID';
var
 lIndex   : integer;
 lFeature : TPowerPlant;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FPowerPlantList.Count)) do
    begin
      lFeature := TPowerPlant(FPowerPlantList.Items[lIndex]);
      if (lFeature.FeatureID = AFeatureID) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlantList.Get_PowerPlantCount: integer;
const OPNAME = 'TPowerPlantList.Get_PowerPlantCount';
begin
  Result := 0;
  try
    Result := FPowerPlantList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlantList.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TPowerPlantList.Validate';
var
  lStopOnFirstError : Boolean;
  LIndex            : integer;
begin
  Result := FALSE;
  try
    Result := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    for LIndex := 0 to FPowerPlantList.Count - 1 do
    begin
      if (NOT PowerPlantByIndex[LIndex].Validate(AErrors,AContext)) then
        Result := FALSE;
      if ((NOT Result) AND lStopOnFirstError) then
        Break;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPowerPlantList.GetChannelNumbers(AFeatureID: integer;
          var APowerChannellNumber, ASpillChannelNumber: integer);
const OPNAME = 'TPowerPlantList.GetChannelNumbers';
var
  LPowerPlant: IPowerPlant;
begin
  try
    APowerChannellNumber := NullInteger;
    ASpillChannelNumber  := NullInteger;
    LPowerPlant := PowerPlantByID[AFeatureID];
    if (LPowerPlant <> nil) then
    begin
      APowerChannellNumber := LPowerPlant.PowerChannel.ChannelNumber;
      ASpillChannelNumber   := LPowerPlant.SpillChannel.ChannelNumber;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPowerPlantList.CopyPowerPlant(ANewChannelNumber,AOldChannelNumber: Integer): IPowerPlant;
const OPNAME = 'TPowerPlantList.CopyPowerPlant';
var
  LPowerPlant : TPowerPlant;
  LPowerPlantCopy : TPowerPlant;
begin
  Result := nil;
  try
    LPowerPlant :=  CastPowerPlantByID(AOldChannelNumber);
    if LPowerPlant <> nil then
    begin
      LPowerPlantCopy := CastPowerPlantByID(ANewChannelNumber);
      if LPowerPlantCopy <> nil then
      begin
        LPowerPlantCopy.Assign(LPowerPlant);
        Result := LPowerPlantCopy;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlant.Assign(APowerPlant: TPowerPlant);
const OPNAME = 'TPowerPlant.Assign';
var
  LIndex : integer;
  LChannelList : IChannelList;
  LSourcePowerChannel : IGeneralFlowChannel;
  LDestPowerChannel : IGeneralFlowChannel;
  LSourceSpillChannel : IGeneralFlowChannel;
  LDestSpillChannel : IGeneralFlowChannel;
  LChnnelPenalty : IChannelPenalty;
  LMinimumMonthlyPowerGeneration,
  LMinimumMonthlyPowerRelease,
  LEfficiencyFactors,
  LNetHeadFactors,
  LDischarges,
  LTailwaterElevations: TAbstractFieldProperty;
begin
  try
    LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList;
    FeatureName := 'Copy of '+APowerPlant.FeatureName;
    FeatureType := APowerPlant.FeatureType;
    FeatureSubType := APowerPlant.FeatureSubType;
    LSourceSpillChannel := LChannelList.ChannelByChannelNumber[APowerPlant.SpillChannel.ChannelNumber];
    if LSourceSpillChannel <> nil then
    begin
      LDestSpillChannel := LChannelList.ChannelByChannelNumber[SpillChannel.ChannelNumber];
      if LDestSpillChannel <> nil then
      begin
        LDestSpillChannel.ChannelName := 'Copy of '+LSourceSpillChannel.ChannelName;
        LDestSpillChannel.UpStreamNodeNumber := LSourceSpillChannel.UpStreamNodeNumber;
        LDestSpillChannel.DownStreamNodeNumber := LSourceSpillChannel.DownStreamNodeNumber;
        LDestSpillChannel.SummaryOutputRequired := LSourceSpillChannel.SummaryOutputRequired;
        LDestSpillChannel.ChannelArea := LSourceSpillChannel.ChannelArea;
        LChnnelPenalty := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelPenaltyList.
                          ChannelPenaltyByIdentifier[LSourceSpillChannel.ChannelPenaltyNumber];
        if LChnnelPenalty <> nil then
          LDestSpillChannel.ChannelPenalty := LChnnelPenalty;
      end;

    end;
    LSourcePowerChannel := LChannelList.ChannelByChannelNumber[APowerPlant.PowerChannel.ChannelNumber];
    if LSourcePowerChannel <> nil then
    begin
      LDestPowerChannel := LChannelList.ChannelByChannelNumber[PowerChannel.ChannelNumber];
      if LDestPowerChannel <> nil then
      begin
        LDestPowerChannel.ChannelName := 'Copy of '+LSourcePowerChannel.ChannelName;
        LDestPowerChannel.UpStreamNodeNumber := LSourcePowerChannel.UpStreamNodeNumber;
        LDestPowerChannel.DownStreamNodeNumber := LSourcePowerChannel.DownStreamNodeNumber;
        LDestPowerChannel.SummaryOutputRequired := LSourcePowerChannel.SummaryOutputRequired;
        LDestPowerChannel.ChannelArea := LSourcePowerChannel.ChannelArea;
        LChnnelPenalty := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelPenaltyList.
                            ChannelPenaltyByIdentifier[LSourcePowerChannel.ChannelPenaltyNumber];
        if LChnnelPenalty <> nil then
          LDestPowerChannel.ChannelPenalty := LChnnelPenalty;
      end;
    end;
    MaximumGeneratorCapacity := APowerPlant.MaximumGeneratorCapacity;
    MaximumTurbineCapacity := APowerPlant.MaximumTurbineCapacity;
    CombinedEfficiency := APowerPlant.CombinedEfficiency;
    PowerPlantStatus := APowerPlant.PowerPlantStatus;
    HeadLoss := APowerPlant.HeadLoss;
    DesignHead := APowerPlant.DesignHead;
    MaximumNetHead := APowerPlant.MaximumNetHead;
    MinimumNetHead := APowerPlant.MinimumNetHead;
    TailWaterType := APowerPlant.TailWaterType;
    DownstreamPowerChannelNrs := APowerPlant.DownstreamPowerChannelNrs;
    DownstreamPowerChannelNrsCount := APowerPlant.DownstreamPowerChannelNrsCount;
    NetHeadEfficiencyCount := APowerPlant.NetHeadEfficiencyCount;
    TailwaterElevationCount := APowerPlant.TailwaterElevationCount;
    LMinimumMonthlyPowerGeneration := FAppModules.FieldProperties.FieldProperty('MinEnergyGenerated');
    LMinimumMonthlyPowerRelease    := FAppModules.FieldProperties.FieldProperty('MinPowerChannelRelease');
    LEfficiencyFactors             := FAppModules.FieldProperties.FieldProperty('EfficiencyFactor');
    LNetHeadFactors                := FAppModules.FieldProperties.FieldProperty('NetHeadFactors');
    LDischarges                    := FAppModules.FieldProperties.FieldProperty('DownStreamLevel');
    LTailwaterElevations           := FAppModules.FieldProperties.FieldProperty('TailWaterElevation');
    for LIndex := LEfficiencyFactors.ArrayLow to LEfficiencyFactors.ArrayHigh do
    begin
      if APowerPlant.EfficiencyFactorByIndex[LIndex] <> NullFloat then
        EfficiencyFactorByIndex[LIndex] := APowerPlant.EfficiencyFactorByIndex[LIndex];
    end;
    for lIndex := LNetHeadFactors.ArrayLow to LNetHeadFactors.ArrayHigh do
    begin
      if APowerPlant.NetHeadFactorByIndex[LIndex] <> NullFloat then
        NetHeadFactorByIndex[LIndex] := APowerPlant.NetHeadFactorByIndex[LIndex];
    end;
    for LIndex := LDischarges.ArrayLow to LDischarges.ArrayHigh do
    begin
      if APowerPlant.DischargeByIndex[LIndex] <> NullFloat then
        DischargeByIndex[LIndex] := APowerPlant.DischargeByIndex[LIndex];
    end;
    for LIndex := LTailwaterElevations.ArrayLow to LTailwaterElevations.ArrayHigh do
    begin
      if APowerPlant.TailwaterElevationByIndex[LIndex] <> NullFloat then
        TailwaterElevationByIndex[LIndex] := APowerPlant.TailwaterElevationByIndex[LIndex];
    end;
    for lIndex := LMinimumMonthlyPowerGeneration.ArrayLow to LMinimumMonthlyPowerGeneration.ArrayHigh do
    begin
      if APowerPlant.MinimumPowerGenerationByMonth[LIndex] <> NullFloat then
        MinimumPowerGenerationByMonth[LIndex] := APowerPlant.MinimumPowerGenerationByMonth[LIndex];
    end;

    for LIndex := LMinimumMonthlyPowerRelease.ArrayLow to LMinimumMonthlyPowerRelease.ArrayHigh do
    begin
      if APowerPlant.MinimumPowerReleaseByMonth[LIndex] <> NullFloat then
        MinimumPowerReleaseByMonth[LIndex] := APowerPlant.MinimumPowerReleaseByMonth[LIndex];
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
