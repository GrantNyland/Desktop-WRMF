{******************************************************************************}
{*  UNIT      : Contains the class TPhysicalFlowConstraint.                   *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2004/03/16                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UPhysicalFlowConstraints;

interface

uses
  Classes,
  Contnrs,
  UAbstractObject,
  VoaimsCom_TLB;

type

{******************************************************************************}
{* Physical Flow Constraints                                                  *}
{******************************************************************************}
  TDischargeCurve = class(TAbstractAppObject, IDischargeCurve) //Type 4 & 5 & 7 & 8 & 9 & 14
  protected
    FFeatureID     : integer;
    FChannelNumber : integer;
    FElevations    : TOneDimensionDoubleArray;
    FDischarges    : TOneDimensionDoubleArray;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure WriteElevationToDB (AIndex : integer; AValue : double);
    procedure WriteDischargeToDB (AIndex : integer; AValue : double);
    function ValidateDischarges(AErrorMessages: TStrings): WordBool;
    function ValidateElevations(AErrorMessages: TStrings): WordBool;
  public
    function Initialise: boolean; override;
    function InitialiseArrays(APointsCount: integer):boolean;
    function UpdatePointsCount(APointsCount: integer):boolean;
    function Populate(AFeatureID,AChannelNumber : integer;AElevations,ADischarges: TOneDimensionDoubleArray): WordBool;
    function Get_ElevationByIndex(AIndex: Integer): Double; safecall;
    procedure Set_ElevationByIndex(AIndex: Integer; AValue: Double); safecall;
    function Get_DischargeByIndex(AIndex: Integer): Double; safecall;
    procedure Set_DischargeByIndex(AIndex: Integer; AValue: Double); safecall;
    function Get_CountNrOfPoints: Integer; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    property ElevationByIndex[AIndex: Integer]: Double read Get_ElevationByIndex write Set_ElevationByIndex;
    property DischargeByIndex[AIndex: Integer]: Double read Get_DischargeByIndex write Set_DischargeByIndex;
    property CountNrOfPoints: Integer read Get_CountNrOfPoints;
  end;

  TKFactors = class(TAbstractAppObject, IKFactors)    // Type 10
  protected
    FFeatureID      : integer;
    FChannelNumber  : integer;
    FChannelNumbers : TOneDimensionDoubleArray;
    FKFactors       : TOneDimensionDoubleArray;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure WriteChannelNumberToDB (AIndex : integer; AValue : double);
    procedure WriteKFactorToDB (AIndex : integer; AValue : double);
    function ValidatePipeChannelNumbers(AErrorMessages: TStrings): WordBool;
    function ValidateKFactors(AErrorMessages: TStrings): WordBool;
  public
    function Initialise: boolean; override;
    function InitialiseArrays(APointsCount: integer):boolean;
    function UpdatePointsCount(APointsCount: integer):boolean;
    function Populate(AFeatureID,AChannelNumber : integer;AChannelNumbers,AKFactors: TOneDimensionDoubleArray): WordBool;
    function Get_ChannelNumberByIndex(AIndex: Integer): Double; safecall;
    procedure Set_ChannelNumberByIndex(AIndex: Integer; AValue: Double); safecall;
    function Get_KFactorByIndex(AIndex: Integer): Double; safecall;
    procedure Set_KFactorByIndex(AIndex: Integer; AValue: Double); safecall;
    function Get_CountNrOfPoints: Integer; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    property ChannelNumberByIndex[AIndex: Integer]: Double read Get_ChannelNumberByIndex write Set_ChannelNumberByIndex;
    property KFactorByIndex[AIndex: Integer]: Double read Get_KFactorByIndex write Set_KFactorByIndex;
    property CountNrOfPoints: Integer read Get_CountNrOfPoints;
  end;

  TSandAquifer = class(TAbstractAppObject, ISandAquifer)  // Type 11
  protected
    FFeatureID             : integer;
    FChannelNumber         : integer;
    FHeadDifferences       : TOneDimensionDoubleArray;
    FAquiferFlows          : TOneDimensionDoubleArray;
    FDownStreamNodeInflows : TOneDimensionDoubleArray;
    FRiverDepths           : TOneDimensionDoubleArray;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure WriteHeadDifferencesToDB (AIndex : integer; AValue : double);
    procedure WriteAquiferFlowsToDB (AIndex : integer; AValue : double);
    procedure WriteDownStreamNodeInflowsToDB (AIndex : integer; AValue : double);
    procedure WriteRiverDepthsToDB (AIndex : integer; AValue : double);
    function ValidateHeadDifferences(AErrorMessages: TStrings): WordBool;
    function ValidateAquiferFlows(AErrorMessages: TStrings): WordBool;
    function ValidateDownStreamNodeInflows(AErrorMessages: TStrings): WordBool;
    function ValidateRiverDepths(AErrorMessages: TStrings): WordBool;
  public
    function Initialise: boolean; override;
    function InitialiseArrays(APointsCount: integer):boolean;
    function UpdatePointsCount(APointsCount: integer):boolean;
    function Populate(AFeatureID,AChannelNumber : integer;AHeadDifferences, AAquiferFlows,ADownStreamNodeInflows,ARiverDepths: TOneDimensionDoubleArray): WordBool;
    function Get_HeadDifferenceByIndex(AIndex: Integer): Double; safecall;
    procedure Set_HeadDifferenceByIndex(AIndex: Integer; AValue: Double); safecall;
    function Get_AquiferFlowByIndex(AIndex: Integer): Double; safecall;
    procedure Set_AquiferFlowByIndex(AIndex: Integer; AValue: Double); safecall;
    function Get_DownStreamNodeInflowByIndex(AIndex: Integer): Double; safecall;
    procedure Set_DownStreamNodeInflowByIndex(AIndex: Integer; AValue: Double); safecall;
    function Get_RiverDepthByIndex(AIndex: Integer): Double; safecall;
    procedure Set_RiverDepthByIndex(AIndex: Integer; AValue: Double); safecall;
    function Get_CountNrOfPoints: Integer; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    property HeadDifferenceByIndex[AIndex: Integer]: Double read Get_HeadDifferenceByIndex write Set_HeadDifferenceByIndex;
    property AquiferFlowByIndex[AIndex: Integer]: Double read Get_AquiferFlowByIndex write Set_AquiferFlowByIndex;
    property DownStreamNodeInflowByIndex[AIndex: Integer]: Double read Get_DownStreamNodeInflowByIndex write Set_DownStreamNodeInflowByIndex;
    property RiverDepthByIndex[AIndex: Integer]: Double read Get_RiverDepthByIndex write Set_RiverDepthByIndex;
    property CountNrOfPoints: Integer read Get_CountNrOfPoints;
  end;

  TSubmergedOutlet = class(TAbstractAppObject, ISubmergedOutlet)// Type 12
  protected
    FFeatureID                  : integer;
    FChannelNumber              : integer;
    FElevationDifferences       : TOneDimensionDoubleArray;
    FMonthlyAverageInflows      : TOneDimensionDoubleArray;
    FMonthlyAverageDivertedFlow : TTwoDimensionDoubleArray;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function ValidateElevationDifferences(AErrorMessages: TStrings): WordBool;
    function ValidateMonthlyAverageInflows(AErrorMessages: TStrings): WordBool;
    function ValidateDownStreamNodeInflows(AErrorMessages: TStrings): WordBool;
    function ValidateMonthlyAverageDivertedFlow(AErrorMessages: TStrings): WordBool;
  public
    function Initialise: boolean; override;
    function InitialiseArrays(APointsCount: integer):boolean;
    function UpdatePointsCount(APointsCount: integer):boolean;
    function Populate(AFeatureID,AChannelNumber : integer; AElevationDifferences, AMonthlyAverageInflows: TOneDimensionDoubleArray;
             AMonthlyAverageDivertedFlow:TTwoDimensionDoubleArray): WordBool;
    function Get_ElevationDifferenceByIndex(AIndex: Integer): Double; safecall;
    procedure Set_ElevationDifferenceByIndex(AIndex: Integer; AValue: Double); safecall;
    function Get_MonthlyAverageInflowByIndex(AIndex: Integer): Double; safecall;
    procedure Set_MonthlyAverageInflowByIndex(AIndex: Integer; AValue: Double); safecall;
    function Get_MonthlyAverageDivertedFlowByIndex(ARow: Integer; ACol: Integer): Double; safecall;
    procedure Set_MonthlyAverageDivertedFlowByIndex(ARow: Integer; ACol: Integer; AValue: Double); safecall;
    function Get_CountNrOfPoints: Integer; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    property ElevationDifferenceByIndex[AIndex: Integer]: Double read Get_ElevationDifferenceByIndex write Set_ElevationDifferenceByIndex;
    property MonthlyAverageInflowByIndex[AIndex: Integer]: Double read Get_MonthlyAverageInflowByIndex write Set_MonthlyAverageInflowByIndex;
    property MonthlyAverageDivertedFlowByIndex[ARow: Integer; ACol: Integer]: Double read Get_MonthlyAverageDivertedFlowByIndex write Set_MonthlyAverageDivertedFlowByIndex;
    property CountNrOfPoints: Integer read Get_CountNrOfPoints;
  end;

  TPumpStation = class(TAbstractAppObject, IPumpStation)// Type 13
  protected
    FFeatureID                  : integer;
    FChannelNumber              : integer;
    FPumpingHeads               : TOneDimensionDoubleArray;
    FPumpingDischarges          : TOneDimensionDoubleArray;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function ValidatePumpingDischarges(AErrorMessages: TStrings): WordBool;
    function ValidatePumpingHeads(AErrorMessages: TStrings): WordBool;
    procedure WritePumpingDischargeToDB(AIndex: integer; AValue: double);
    procedure WritePumpingHeadToDB(AIndex: integer; AValue: double);
  public
    function Initialise: boolean; override;
    function InitialiseArrays(APointsCount: integer):boolean;
    function UpdatePointsCount(APointsCount: integer):boolean;
    function Populate(AFeatureID,AChannelNumber : integer; APumpingHeads, ADischarges: TOneDimensionDoubleArray): WordBool;
    function Get_PumpingHeadByIndex(AIndex: Integer): Double; safecall;
    procedure Set_PumpingHeadByIndex(AIndex: Integer; AValue: Double); safecall;
    function Get_PumpingDischargeByIndex(AIndex: Integer): Double; safecall;
    procedure Set_PumpingDischargeByIndex(AIndex: Integer; AValue: Double); safecall;
    function Get_CountNrOfPoints: Integer; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    property PumpingHeadByIndex[AIndex: Integer]: Double read Get_PumpingHeadByIndex write Set_PumpingHeadByIndex;
    property PumpingDischargeByIndex[AIndex: Integer]: Double read Get_PumpingDischargeByIndex write Set_PumpingDischargeByIndex;
    property CountNrOfPoints: Integer read Get_CountNrOfPoints;

  end;

  TPhysicalFlowConstraint = class(TAbstractAppObject, IPhysicalFlowConstraint)
  protected
    FFeatureID                  : integer;
    FFeatureName                : string;
    FChannelNumber              : integer;
    FUpstreamReservoirNr        : integer;
    FDownstreamReservoirNr      : integer;
    FStructureType              : integer;
    FElevationOfSill            : double;
    FMaximumGateHeight          : double;
    FDischargeCoefficient       : double;
    FStructureLength            : double;
    FWaterLevelAtDownstreamNode : double;
    FReferenceElevation         : double;
    FNrOfPoints                 : integer;
    FDischargeCurve             : TDischargeCurve;
    FKFactors                   : TKFactors;
    FSandAquifer                : TSandAquifer;
    FSubmergedOutlet            : TSubmergedOutlet;
    FPumpStation                : TPumpStation;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure CreateStructurePerType;
    procedure DeleteStructurePerType;
    function ValidateFeatureName(AErrorMessages: TStrings): WordBool;
    function ValidateConstraintType(AErrorMessages : TStrings): WordBool;
    function ValidateUpstreamReservoir(AErrorMessages : TStrings): WordBool;
    function ValidateDownstreamReservoir(AErrorMessages : TStrings): WordBool;
    function ValidateElevationOfSill(AErrorMessages : TStrings): WordBool;
    function ValidateMaximumGateHeight(AErrorMessages : TStrings): WordBool;
    function ValidateStructureLength(AErrorMessages : TStrings): WordBool;
    function ValidateDischargeCoefficient(AErrorMessages : TStrings): WordBool;
    function ValidateWaterLevelAtDownstreamNode(AErrorMessages : TStrings): WordBool;
    function ValidateReferenceElevation(AErrorMessages : TStrings): WordBool;
  public
    procedure Assign(AChannelNumber: integer;APhysicalFlowConstraint: TPhysicalFlowConstraint);
    function Initialise: boolean; override;
    function Populate(AFeatureID                  : integer;
                       AFeatureName               : WideString;
                       AChannelNr                 : integer;
                       AUpstreamReservoirNr       : integer;
                       ADownstreamReservoirNr     : integer;
                       AStructureType             : integer;
                       AElevationOfSill           : double;
                       AMaximumGateHeight         : double;
                       ADischargeCoefficient      : double;
                       AStructureLength           : double;
                       ANrOfPoints                : integer;
                       AWaterLevelAtDownstreamNode: double;
                       AReferenceElevation        : double
                     ): WordBool;
    function Get_FeatureID : integer; safecall;
    function Get_FeatureName : WideString; safecall;
    procedure Set_FeatureName (const AName : WideString); safecall;
    function Get_UpstreamReservoirNr : integer; safecall;
    procedure Set_UpstreamReservoirNr(Value: Integer); safecall;
    function Get_DownstreamReservoirNr : integer; safecall;
    procedure Set_DownstreamReservoirNr(Value: Integer); safecall;

    procedure Set_UpstreamReservoir(const AReservoir: IReservoirData); safecall;
    procedure Set_DownstreamReservoir(const AReservoir: IReservoirData); safecall;

    function Get_StructureType: integer; safecall;
    procedure Set_StructureType(AStructureType: integer); safecall;
    function Get_ElevationOfSill: double; safecall;
    procedure Set_ElevationOfSill(AElevation: double); safecall;
    function Get_MaximumGateHeight: double; safecall;
    procedure Set_MaximumGateHeight(AHeight: double); safecall;
    function Get_DischargeCoefficient: double; safecall;
    procedure Set_DischargeCoefficient(ACoefficient: double); safecall;
    function Get_StructureLength: double; safecall;
    procedure Set_StructureLength(ALength: double); safecall;
    function Get_NrOfPoints : integer; safecall;
    procedure Set_NrOfPoints (ACount : integer); safecall;
    function Get_DischargeCurve: IDischargeCurve; safecall;
    function Get_KFactors: IKFactors; safecall;
    function Get_SandAquifer: ISandAquifer; safecall;
    function Get_SubmergedOutlet: ISubmergedOutlet; safecall;
    function Get_PumpStation: IPumpStation; safecall;
    function Get_ChannelNumber: Integer; safecall;
    function Validate (var AErrors : WideString; const AContext : WideString='') : WordBool; safecall;
    function GetKeyValues (const AParamField : WideString; const AFieldIndex: WideString) : WideString; safecall;
    function Get_WaterLevelAtDownstreamNode: Double; safecall;
    procedure Set_WaterLevelAtDownstreamNode(ALevel: Double); safecall;
    function Get_ReferenceElevation : Double; safecall;
    procedure Set_ReferenceElevation(AElevation: Double); Safecall;    
    property FeatureID      : integer read Get_FeatureID;
    property FeatureName    : WideString read Get_FeatureName write Set_FeatureName;
    property UpstreamReservoirNr : integer  read Get_UpstreamReservoirNr;
    property DownstreamReservoirNr : integer read Get_DownstreamReservoirNr;
    property StructureType : integer read Get_StructureType write Set_StructureType;
    property ElevationOfSill : double read Get_ElevationOfSill write Set_ElevationOfSill;
    property MaximumGateHeight : double read Get_MaximumGateHeight write Set_MaximumGateHeight;
    property DischargeCoefficient : double read Get_DischargeCoefficient write Set_DischargeCoefficient;
    property StructureLength : double read Get_StructureLength write Set_StructureLength;
    property NrOfPoints : integer read Get_NrOfPoints write Set_NrOfPoints;
    property DischargeCurve: IDischargeCurve read Get_DischargeCurve;
    property KFactors: IKFactors read Get_KFactors;
    property SandAquifer: ISandAquifer read Get_SandAquifer;
    property SubmergedOutlet: ISubmergedOutlet read Get_SubmergedOutlet;
    property PumpStation: IPumpStation read Get_PumpStation;
    property ChannelNumber: Integer read Get_ChannelNumber;
    property CastDischargeCurve: TDischargeCurve read FDischargeCurve;
    property CastKFactors: TKFactors read FKFactors;
    property CastSandAquifer: TSandAquifer read FSandAquifer;
    property CastSubmergedOutlet: TSubmergedOutlet read FSubmergedOutlet;
    property CastPumpStation: TPumpStation read FPumpStation;
    property WaterLevelAtDownstreamNode : Double read Get_WaterLevelAtDownstreamNode write Set_WaterLevelAtDownstreamNode;
    property ReferenceElevation : Double read Get_ReferenceElevation write Set_ReferenceElevation;
  end;

  TPhysicalFlowConstraintList = class(TAbstractAppObject, IPhysicalFlowConstraintList)
  protected
    FPhysicalFlowConstraintList : TObjectList;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function Initialise: boolean; override;
    function NewPhysicalFlowConstraint : TPhysicalFlowConstraint;
    function CastPhysicalFlowConstraintByID(AFeatureID : integer): TPhysicalFlowConstraint;
    function CastPhysicalFlowConstraintByChannelNr(AChannelNr : integer): TPhysicalFlowConstraint;

    function CastPhysicalFlowConstraintByIndex(AIndex : integer): TPhysicalFlowConstraint;
    function CopyPhysicalFlowConstraint(ANewChannelNumber : integer;AOldChannelNumber : integer) : IPhysicalFlowConstraint; safecall;

    function CreatePhysicalFlowConstraint(AChannelNumber : integer) : IPhysicalFlowConstraint; safecall;
    function RemovePhysicalFlowConstraintWithID (AFeatureID : integer) : WordBool; safecall;
    function Get_PhysicalFlowConstraintByIndex(AIndex: integer): IPhysicalFlowConstraint; safecall;
    function Get_PhysicalFlowConstraintByID(AFeatureID: integer): IPhysicalFlowConstraint; safecall;
    function Get_PhysicalFlowConstraintCount: integer; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;

    property PhysicalFlowConstraintByIndex[AIndex : integer]: IPhysicalFlowConstraint
      read Get_PhysicalFlowConstraintByIndex;
    property PhysicalFlowConstraintByID[AFeatureID: integer]: IPhysicalFlowConstraint
      read Get_PhysicalFlowConstraintByID;
    property PhysicalFlowConstraintCount: integer read Get_PhysicalFlowConstraintCount;
  end;

implementation

uses
  SysUtils,
  Math,
  UConstants,
  UNetworkFeaturesSQLAgent,
  UYieldModelDataObject,
  UErrorHandlingOperations;

{ TDischargeCurve }

function TDischargeCurve._AddRef: Integer;
const OPNAME = 'TDischargeCurve._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDischargeCurve._Release: Integer;
const OPNAME = 'TDischargeCurve._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDischargeCurve.CreateMemberObjects;
const OPNAME = 'TDischargeCurve.CreateMemberObjects';
var
  LFieldProperty : TAbstractFieldProperty;
begin
  inherited;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintElevation');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintElevation) not found in field properties');
    SetLength(FElevations,LFieldProperty.ArrayLength);

    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintDischarge');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintDischarge) not found in field properties');
    SetLength(FDischarges,LFieldProperty.ArrayLength);
    Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDischargeCurve.DestroyMemberObjects;
const OPNAME = 'TDischargeCurve.DestroyMemberObjects';
begin
  inherited;
  try
    Finalize(FElevations);
    Finalize(FDischarges);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDischargeCurve.Initialise: boolean;
const OPNAME = 'TDischargeCurve.Initialise';
var
  LIndex: integer;
begin
  Result := False;
  try
    FFeatureID     := NullInteger;
    FChannelNumber := NullInteger;
    for LIndex := Low(FElevations) to High(FElevations) do
      FElevations[LIndex] := NullFloat;
    for LIndex := Low(FDischarges) to High(FDischarges) do
      FDischarges[LIndex] := NullFloat;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDischargeCurve.Get_CountNrOfPoints: Integer;
const OPNAME = 'TDischargeCurve.Get_CountNrOfPoints';
var
  LIndex: integer;
  LFieldProperty : TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintElevation');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintElevation) not found in field properties');

    for LIndex := LFieldProperty.ArrayLow to LFieldProperty.ArrayHigh do
    begin
      if(FElevations[LIndex] = NullFloat) then
        Break
      else
        Result := Result + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDischargeCurve.Get_DischargeByIndex(AIndex: Integer): Double;
const OPNAME = 'TDischargeCurve.Get_DischargeByIndex';
begin
  Result := NullFloat;
  try
    if(AIndex >= Low(FDischarges)) and (AIndex <= High(FDischarges)) then
      Result := FDischarges[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDischargeCurve.Get_ElevationByIndex(AIndex: Integer): Double;
const OPNAME = 'TDischargeCurve.Get_ElevationByIndex';
begin
  Result := NullFloat;
  try
    if(AIndex >= Low(FElevations)) and (AIndex <= High(FElevations)) then
      Result := FElevations[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDischargeCurve.Set_DischargeByIndex(AIndex: Integer; AValue: Double);
const OPNAME = 'TDischargeCurve.Set_DischargeByIndex';
var
  LIndex: integer;
  LFieldProperty : TAbstractFieldProperty;
begin
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintDischarge');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintDischarge) not found in field properties');
    if (AIndex >= LFieldProperty.ArrayLow) and (AIndex <= LFieldProperty.ArrayHigh) then
    begin
      if (FDischarges[AIndex] <> AValue) then
      begin
        if (AValue = NullFloat) then
        begin
          for LIndex := AIndex+1 to High(FDischarges) do
          begin
            WriteElevationToDB (LIndex-1, FElevations[LIndex]);
            WriteDischargeToDB (LIndex-1, FDischarges[LIndex]);
          end;
          WriteElevationToDB (LFieldProperty.ArrayHigh, NullFloat);
          WriteDischargeToDB (LFieldProperty.ArrayHigh, NullFloat);
        end
        else
        begin
          for LIndex := LFieldProperty.ArrayLow to AIndex-1 do
          begin
            if (FElevations[LIndex] = NullFloat) then
              WriteElevationToDB(LIndex, 0.0);
            if (FDischarges[LIndex] = NullFloat) then
              WriteDischargeToDB(LIndex, 0.0);
          end;
          WriteDischargeToDB(AIndex, AValue);
          if (FElevations[AIndex] = NullFloat) then
            WriteElevationToDB (AIndex, 0.0);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDischargeCurve.Set_ElevationByIndex(AIndex: Integer; AValue: Double);
const OPNAME = 'TDischargeCurve.Set_ElevationByIndex';
var
  LIndex : integer;
  LFieldProperty : TAbstractFieldProperty;
begin
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintElevation');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintElevation) not found in field properties');
    if (AIndex >= LFieldProperty.ArrayLow) and (AIndex <= LFieldProperty.ArrayHigh) then
    begin
      if (FElevations[AIndex] <> AValue) then
      begin
        if (AValue = NullFloat) then
        begin
          for LIndex := AIndex+1 to LFieldProperty.ArrayHigh do
          begin
            WriteElevationToDB (LIndex-1, FElevations[LIndex]);
            WriteDischargeToDB (LIndex-1, FDischarges[LIndex]);
          end;
          WriteElevationToDB (LFieldProperty.ArrayHigh, NullFloat);
          WriteDischargeToDB (LFieldProperty.ArrayHigh, NullFloat);
        end
        else
        begin
          for LIndex := LFieldProperty.ArrayLow to AIndex-1 do
          begin
            if (FElevations[LIndex] = NullFloat) then
              WriteElevationToDB(LIndex, 0.0);
            if (FDischarges[LIndex] = NullFloat) then
              WriteDischargeToDB(LIndex, 0.0);
          end;
          WriteElevationToDB(AIndex, AValue);
          if (FDischarges[AIndex] = NullFloat) then
            WriteDischargeToDB (AIndex, 0.0);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDischargeCurve.WriteElevationToDB (AIndex : integer; AValue : double);
const OPNAME = 'TDischargeCurve.WriteElevationToDB';
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
        LLoadAgent.LoadContextData_PhysicalFlowConstraint(LContextData, FFeatureID,pfcgDischargeCurve,pfcstElevation,0,AIndex);
        if (AValue = NullFloat) then
          lNewValue := ''
        else
          lNewValue := FloatToStr(AValue);
        if (FElevations[AIndex] = NullFloat) then
          lOldValue := ''
        else
          lOldValue := FloatToStr(FElevations[AIndex]);
        if FAppModules.FieldProperties.UpdateFieldValue('ConstraintElevation', lNewValue, lOldValue, LContextData) then
        begin
          LPrevValue     := FElevations[AIndex];
          FElevations[AIndex] := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ConstraintElevation',FloatToStr(LPrevValue),FloatToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDischargeCurve.WriteDischargeToDB (AIndex : integer;AValue : double);
const OPNAME = 'TDischargeCurve.WriteDischargeToDB';
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
        LLoadAgent.LoadContextData_PhysicalFlowConstraint(LContextData, FFeatureID,pfcgDischargeCurve,pfcstDischarge,0,AIndex);
        if (AValue = NullFloat) then
          lNewValue := ''
        else
          lNewValue := FloatToStr(AValue);
        if (FDischarges[AIndex] = NullFloat) then
          lOldValue := ''
        else
          lOldValue := FloatToStr(FDischarges[AIndex]);
        if FAppModules.FieldProperties.UpdateFieldValue('ConstraintDischarge', lNewValue, lOldValue, LContextData) then
        begin
          LPrevValue := FDischarges[AIndex];
          FDischarges[AIndex] := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ConstraintDischarge',FloatToStr(LPrevValue),FloatToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TDischargeCurve.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TDischargeCurve.Validate';
var
  LErrorList : TStringList;
  lStopOnFirstError: boolean;
begin
  Result := False;
  try
    lErrorList := TStringList.Create;
    try
      lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
      if (AContext = 'Elevations') then
        Result := ValidateElevations(LErrorList)
      else
      if (AContext = 'Discharges') then
        Result := ValidateDischarges(LErrorList)
      else
      begin
        Result := ValidateElevations(lErrorList);
        if (Result OR (NOT lStopOnFirstError)) then
          Result := Result and  ValidateDischarges(LErrorList);
      end;
      AErrors := AErrors + lErrorList.CommaText;
    finally
      FreeAndNil(lErrorList);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDischargeCurve.Populate(AFeatureID,AChannelNumber: integer; AElevations,ADischarges: TOneDimensionDoubleArray): WordBool;
const OPNAME = 'TDischargeCurve.Populate';
var
  LIndex: integer;
  LFieldProperty : TAbstractFieldProperty;
begin
  Result := False;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintElevation');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintElevation) not found in field properties');
    FFeatureID     := AFeatureID;
    FChannelNumber := AChannelNumber;
    for LIndex := LFieldProperty.ArrayLow to LFieldProperty.ArrayHigh do
    begin
      if(LIndex > High(FElevations)) then Break;
      FElevations[LIndex] := AElevations[LIndex];
    end;
    for LIndex := LFieldProperty.ArrayLow to LFieldProperty.ArrayHigh do
    begin
      if(LIndex > High(FDischarges)) then Break;
      FDischarges[LIndex] := ADischarges[LIndex];
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDischargeCurve.ValidateElevations(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TDischargeCurve.ValidateElevations';
var
  lMessage          : string;
  lStopOnFirstError : Boolean;
  lValid            : Boolean;
  lResult           : Boolean;
  lIndex            : integer;
  lContinue         : Boolean;
  lCount            : integer;
  LElevations       : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    LElevations := FAppModules.FieldProperties.FieldProperty('ConstraintElevation');
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    lIndex    := LElevations.ArrayLow;
    lContinue := TRUE;
    lCount    := 0;
    while (lContinue AND (lIndex  <= LElevations.ArrayHigh)) do
    begin
      if ((FElevations[lIndex] = NullFloat) AND
          (FDischarges[lIndex] = NullFloat)) then
        lContinue := FALSE
      else
      begin
        lMessage := '';
        lCount  := lCount + 1;
        lValid := FAppModules.FieldProperties.ValidateFieldProperty
                    ('ConstraintElevation', FloatToStr(FElevations[lIndex]),
                    lMessage, lIndex);
        if (NOT lValid) then
        begin
          lResult := FALSE;
          AErrorMessages.Add(lMessage);
          if (lStopOnFirstError) then
            Break;
        end;
        if (lResult OR (NOT lStopOnFirstError)) then
        begin
          if ((FElevations[lIndex] = NullFloat) AND
              (FDischarges[lIndex] <> NullFloat)) then
          begin
            lMessage := FAppModules.Language.GetString('ContextValidation.UnequalNrOfArrayValues');
            AErrorMessages.Add(Format(lMessage, ['Channel ('+IntToStr(FChannelNumber)+')']));
            lResult := FALSE;
          end;
        end;
        lIndex := lIndex + 1;
      end;
    end;
    if (lResult OR (NOT lStopOnFirstError)) then
    begin
      if (lCount < 1) then
      begin
        lResult := FALSE;
        lMessage := FAppModules.Language.GetString('ContextValidation.NotEnoughArrayValues');
        AErrorMessages.Add(Format(lMessage, ['Channel ('+IntToStr(FChannelNumber)+')']));
      end;
    end;

    if (lResult OR (NOT lStopOnFirstError)) then
    begin
      lValid := TRUE;
      lIndex := LElevations.ArrayLow;
      while (lValid AND(lIndex < LElevations.ArrayHigh)) do
      begin
        if ((FElevations[lIndex+1] <> NullFloat) AND
            (FElevations[lIndex] > FElevations[lIndex+1])) then
        begin
          lMessage := FAppModules.Language.GetString('ContextValidation.ElevationRangesNotInDescendingOrder');
          AErrorMessages.Add(Format(lMessage, [IntToStr(FFeatureID)]));
          lValid := FALSE;
        end
        else
          lIndex := lIndex + 1;
      end;
      if (NOT lValid) then
        lResult := FALSE;
    end;

    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDischargeCurve.ValidateDischarges(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TDischargeCurve.ValidateDischarges';
var
  lMessage          : string;
  lStopOnFirstError : Boolean;
  lValid            : Boolean;
  lResult           : Boolean;
  lIndex            : integer;
  lContinue         : Boolean;
  lCount            : integer;
  LElevations       : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    LElevations := FAppModules.FieldProperties.FieldProperty('ConstraintElevation');
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    lIndex    := LElevations.ArrayLow;
    lContinue := TRUE;
    lCount    := 0;
    while (lContinue AND (lIndex  <= LElevations.ArrayHigh)) do
    begin
      if ((FElevations[lIndex] = NullFloat) AND
          (FDischarges[lIndex] = NullFloat)) then
        lContinue := FALSE
      else
      begin
        lMessage := '';
        lCount  := lCount + 1;
        lValid := FAppModules.FieldProperties.ValidateFieldProperty
                    ('ConstraintDischarge', FloatToStr(FDischarges[lIndex]),
                    lMessage, lIndex);
        if (NOT lValid) then
        begin
          lResult := FALSE;
          AErrorMessages.Add(lMessage);
          if (lStopOnFirstError) then
            Break;
        end;
        if (lResult OR (NOT lStopOnFirstError)) then
        begin
          if ((FElevations[lIndex] <> NullFloat) AND
              (FDischarges[lIndex] = NullFloat)) then
          begin
            lMessage := FAppModules.Language.GetString('ContextValidation.UnequalNrOfArrayValues');
            AErrorMessages.Add(Format(lMessage, ['Channel ('+IntToStr(FChannelNumber)+')']));
            lResult := FALSE;
          end;
        end;
        lIndex := lIndex + 1;
      end;
    end;
    if (lResult OR (NOT lStopOnFirstError)) then
    begin
      if (lCount < 1) then
      begin
        lResult := FALSE;
        lMessage := FAppModules.Language.GetString('ContextValidation.NotEnoughArrayValues');
        AErrorMessages.Add(Format(lMessage, ['Channel ('+IntToStr(FChannelNumber)+')']));
      end;
    end;

    if (lResult OR (NOT lStopOnFirstError)) then
      begin
        lValid := TRUE;
        lIndex := LElevations.ArrayLow;
        while (lValid AND(lIndex < LElevations.ArrayHigh)) do
        begin
          if ((FDischarges[lIndex+1] <> NullFloat) AND
              (FDischarges[lIndex] > FDischarges[lIndex+1])) then
          begin
            lMessage := FAppModules.Language.GetString('ContextValidation.ElevationRangesNotInDescendingOrder');
            AErrorMessages.Add(Format(lMessage, [IntToStr(FFeatureID)]));
            lValid := FALSE;
          end
          else
            lIndex := lIndex + 1;
        end;
        if (NOT lValid) then
          lResult := FALSE;
      end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDischargeCurve.InitialiseArrays(APointsCount: integer): boolean;
const OPNAME = 'TDischargeCurve.InitialiseArrays';
var
  LIndex: integer;
  LFieldProperty : TAbstractFieldProperty;
begin
  Result := False;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintElevation');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintElevation) not found in field properties');
    for LIndex := LFieldProperty.ArrayLow to APointsCount do
    begin
      FElevations[LIndex] := 0.0;
      FDischarges[LIndex] := 0.0;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDischargeCurve.UpdatePointsCount(APointsCount: integer): boolean;
const OPNAME = 'TDischargeCurve.UpdatePointsCount';
var
  LIndex: integer;
  LFieldProperty : TAbstractFieldProperty;
begin
  Result := False;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintElevation');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintElevation) not found in field properties');
    for LIndex := LFieldProperty.ArrayLow to APointsCount do
    begin
      if(FElevations[LIndex] = NullFloat) then
        FElevations[LIndex] := 0.0;
      if(FDischarges[LIndex] = NullFloat) then
        FDischarges[LIndex] := 0.0;
    end;
    for LIndex := APointsCount+1 to LFieldProperty.ArrayHigh do
    begin
      FElevations[LIndex] := NullFloat;
      FDischarges[LIndex] := NullFloat;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TKFactors }

function TKFactors._AddRef: Integer;
const OPNAME = 'TKFactors._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TKFactors._Release: Integer;
const OPNAME = 'TKFactors._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TKFactors.CreateMemberObjects;
const OPNAME = 'TKFactors.CreateMemberObjects';
var
  LFieldProperty : TAbstractFieldProperty;
begin
  inherited;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintChannelNumber');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintChannelNumber) not found in field properties');
    SetLength(FChannelNumbers,LFieldProperty.ArrayLength);

    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintKFactor');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintKFactor) not found in field properties');
    SetLength(FKFactors,LFieldProperty.ArrayLength);
    Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TKFactors.DestroyMemberObjects;
const OPNAME = 'TKFactors.DestroyMemberObjects';
begin
  inherited;
  try
    Finalize(FChannelNumbers);
    Finalize(FKFactors);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TKFactors.Initialise: boolean;
const OPNAME = 'TKFactors.Initialise';
var
  LIndex: integer;
begin
  Result := False;
  try
    FFeatureID     := NullInteger;
    FChannelNumber := NullInteger;
    for LIndex := Low(FChannelNumbers) to High(FChannelNumbers) do
      FChannelNumbers[LIndex] := NullFloat;
    for LIndex := Low(FKFactors) to High(FKFactors) do
      FKFactors[LIndex] := NullFloat;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TKFactors.InitialiseArrays(APointsCount: integer): boolean;
const OPNAME = 'TKFactors.InitialiseArrays';
var
  LIndex: integer;
  LFieldProperty : TAbstractFieldProperty;
begin
  Result := False;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintChannelNumber');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintChannelNumber) not found in field properties');
    for LIndex := LFieldProperty.ArrayLow to APointsCount do
    begin
      FChannelNumbers[LIndex] := 0.0;
      FKFactors[LIndex] := 0.0;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TKFactors.UpdatePointsCount(APointsCount: integer): boolean;
const OPNAME = 'TKFactors.UpdatePointsCount';
var
  LIndex: integer;
  LFieldProperty : TAbstractFieldProperty;
begin
  Result := False;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintChannelNumber');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintChannelNumber) not found in field properties');
    for LIndex := LFieldProperty.ArrayLow to APointsCount do
    begin
      if(FChannelNumbers[LIndex] = NullFloat) then
        FChannelNumbers[LIndex] := 0.0;
      if(FKFactors[LIndex] = NullFloat) then
        FKFactors[LIndex] := 0.0;
    end;
    for LIndex := APointsCount+1 to LFieldProperty.ArrayHigh do
    begin
      FChannelNumbers[LIndex] := NullFloat;
      FKFactors[LIndex] := NullFloat;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TKFactors.Get_CountNrOfPoints: Integer;
const OPNAME = 'TKFactors.Get_CountNrOfPoints';
var
  LIndex: integer;
  LFieldProperty : TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintChannelNumber');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintChannelNumber) not found in field properties');
    for LIndex := LFieldProperty.ArrayLow to LFieldProperty.ArrayHigh do
    begin
      if(FChannelNumbers[LIndex] = NullFloat) then
        Break
      else
        Result := Result + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TKFactors.Get_ChannelNumberByIndex(AIndex: Integer): Double;
const OPNAME = 'TKFactors.Get_ChannelNumberByIndex';
begin
  Result := NullInteger;
  try
    if(AIndex >= Low(FChannelNumbers)) and (AIndex <= High(FChannelNumbers)) then
      Result := Trunc(FChannelNumbers[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TKFactors.Get_KFactorByIndex(AIndex: Integer): Double;
const OPNAME = 'TKFactors.Get_KFactorByIndex';
begin
  Result := NullFloat;
  try
    if(AIndex >= Low(FKFactors)) and (AIndex <= High(FKFactors)) then
      Result := FKFactors[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TKFactors.Set_ChannelNumberByIndex(AIndex:integer; AValue: Double);
const OPNAME = 'TKFactors.Set_ChannelNumberByIndex';
var
  LIndex: integer;
  LFieldProperty : TAbstractFieldProperty;
begin
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintChannelNumber');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintChannelNumber) not found in field properties');
    if (AIndex >= LFieldProperty.ArrayLow) and (AIndex <= LFieldProperty.ArrayHigh) then
    begin
      AValue := Trunc(AValue);
      if (FChannelNumbers[AIndex] <> AValue) then
      begin
        if (AValue = NullFloat) then
        begin
          for LIndex := AIndex+1 to LFieldProperty.ArrayHigh do
          begin
            WriteChannelNumberToDB (LIndex-1, FChannelNumbers[LIndex]);
            WriteKFactorToDB (LIndex-1, FKFactors[LIndex]);
          end;
          WriteChannelNumberToDB (LFieldProperty.ArrayHigh, NullFloat);
          WriteKFactorToDB (LFieldProperty.ArrayHigh, NullFloat);
        end
        else
        begin
          for LIndex := LFieldProperty.ArrayLow to AIndex-1 do
          begin
            if (FChannelNumbers[LIndex] = NullFloat) then
              WriteChannelNumberToDB(LIndex, 0.0);
            if (FKFactors[LIndex] = NullFloat) then
              WriteKFactorToDB(LIndex, 0.0);
          end;
          WriteChannelNumberToDB(AIndex, AValue);
          if (FChannelNumbers[AIndex] = NullFloat) then
            WriteChannelNumberToDB (AIndex, 0.0);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TKFactors.Set_KFactorByIndex(AIndex: Integer; AValue: Double);
const OPNAME = 'TKFactors.Set_KFactorByIndex';
var
  LIndex : integer;
  LFieldProperty : TAbstractFieldProperty;
begin
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintKFactor');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintKFactor) not found in field properties');
    if (AIndex >= LFieldProperty.ArrayLow) and (AIndex <= LFieldProperty.ArrayHigh) then
    begin
      if (FKFactors[AIndex] <> AValue) then
      begin
        if (AValue = NullFloat) then
        begin
          for LIndex := AIndex+1 to LFieldProperty.ArrayHigh do
          begin
            WriteKFactorToDB (LIndex-1, FKFactors[LIndex]);
            WriteChannelNumberToDB (LIndex-1, FChannelNumbers[LIndex]);
          end;
          WriteKFactorToDB (LFieldProperty.ArrayHigh, NullFloat);
          WriteChannelNumberToDB (LFieldProperty.ArrayHigh, NullFloat);
        end
        else
        begin
          for LIndex := LFieldProperty.ArrayLow to AIndex-1 do
          begin
            if (FKFactors[LIndex] = NullFloat) then
              WriteKFactorToDB(LIndex, 0.0);
            if (FChannelNumbers[LIndex] = NullFloat) then
              WriteChannelNumberToDB(LIndex, 0.0);
          end;
          WriteKFactorToDB(AIndex, AValue);
          if (FChannelNumbers[AIndex] = NullFloat) then
            WriteChannelNumberToDB (AIndex, 0.0);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TKFactors.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TKFactors.Validate';
var
  LErrorList : TStringList;
  lStopOnFirstError: boolean;
begin
  Result := False;
  try
    lErrorList := TStringList.Create;
    try
      lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
      if (AContext = 'ConstraintChannelNumber') then
        Result := ValidatePipeChannelNumbers(LErrorList)
      else
      if (AContext = 'KFactors') then
        Result := ValidateKFactors(LErrorList)
      else
      begin
        Result := ValidatePipeChannelNumbers(lErrorList);
        if (Result OR (NOT lStopOnFirstError)) then
          Result := Result and  ValidateKFactors(LErrorList);
      end;
      AErrors := AErrors + lErrorList.CommaText;
    finally
      FreeAndNil(lErrorList);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TKFactors.ValidatePipeChannelNumbers(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TKFactors.ValidatePipeChannelNumbers';
var
  lMessage          : string;
  lStopOnFirstError : Boolean;
  lValid            : Boolean;
  lResult           : Boolean;
  lIndex            : integer;
  lContinue         : Boolean;
  lCount            : integer;
  LFieldProperty    : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    LFieldProperty    := FAppModules.FieldProperties.FieldProperty('ConstraintChannelNumber');
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    lIndex            := LFieldProperty.ArrayLow;
    lContinue         := TRUE;
    lCount            := 0;
    while (lContinue AND (lIndex  <= LFieldProperty.ArrayHigh)) do
    begin
      if ((FChannelNumbers[lIndex] = NullFloat) AND
          (FKFactors[lIndex] = NullFloat)) then
        lContinue := FALSE
      else
      begin
        lCount  := lCount + 1;
        lMessage := '';
        lValid := FAppModules.FieldProperties.ValidateFieldProperty
                    ('ConstraintChannelNumber', IntToStr(Trunc(FChannelNumbers[lIndex])),
                    lMessage, lIndex);
        if (NOT lValid) then
        begin
          lResult := FALSE;
          AErrorMessages.Add(lMessage);
          if (lStopOnFirstError) then
            Break;
        end;
        if (lResult OR (NOT lStopOnFirstError)) then
        begin
          if ((FChannelNumbers[lIndex] = NullFloat) AND
              (FKFactors[lIndex] <> NullFloat)) then
          begin
            lMessage := FAppModules.Language.GetString('ContextValidation.UnequalNrOfArrayValues');
            AErrorMessages.Add(Format(lMessage, ['Channel ('+IntToStr(FChannelNumber)+')']));
            lResult := FALSE;
          end;
        end;
        lIndex := lIndex + 1;
      end;
    end;
    if (lResult OR (NOT lStopOnFirstError)) then
    begin
      if (lCount < 1) then
      begin
        lResult := FALSE;
        lMessage := FAppModules.Language.GetString('ContextValidation.NotEnoughArrayValues');
        AErrorMessages.Add(Format(lMessage, ['Channel ('+IntToStr(FChannelNumber)+')']));
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TKFactors.ValidateKFactors(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TKFactors.ValidateKFactors';
var
  lMessage          : string;
  lStopOnFirstError : Boolean;
  lValid            : Boolean;
  lResult           : Boolean;
  lIndex            : integer;
  lContinue         : Boolean;
  lCount            : integer;
  LKFactors         : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    LKFactors := FAppModules.FieldProperties.FieldProperty('ConstraintKFactor');
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    lIndex    := LKFactors.ArrayLow;
    lContinue := TRUE;
    lCount    := 0;
    while (lContinue AND (lIndex  <= LKFactors.ArrayHigh)) do
    begin
      if ((FChannelNumbers[lIndex] = NullFloat) AND
          (FKFactors[lIndex] = NullFloat)) then
        lContinue := FALSE
      else
      begin
        lMessage := '';
        lCount  := lCount + 1;
        lValid := FAppModules.FieldProperties.ValidateFieldProperty
                    ('ConstraintKFactor', FloatToStr(FKFactors[lIndex]),
                    lMessage, lIndex);
        if (NOT lValid) then
        begin
          lResult := FALSE;
          AErrorMessages.Add(lMessage);
          if (lStopOnFirstError) then
            Break;
        end;
        if (lResult OR (NOT lStopOnFirstError)) then
        begin
          if ((FChannelNumbers[lIndex] <> NullFloat) AND
              (FKFactors[lIndex] = NullFloat)) then
          begin
            lMessage := FAppModules.Language.GetString('ContextValidation.UnequalNrOfArrayValues');
            AErrorMessages.Add(Format(lMessage, ['Channel ('+IntToStr(FChannelNumber)+')']));
            lResult := FALSE;
          end;
        end;
        lIndex := lIndex + 1;
      end;
    end;
    if (lResult OR (NOT lStopOnFirstError)) then
    begin
      if (lCount < 1) then
      begin
        lResult := FALSE;
        lMessage := FAppModules.Language.GetString('ContextValidation.NotEnoughArrayValues');
        AErrorMessages.Add(Format(lMessage, ['Channel ('+IntToStr(FChannelNumber)+')']));
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TKFactors.Populate(AFeatureID,AChannelNumber : integer;AChannelNumbers,AKFactors: TOneDimensionDoubleArray): WordBool;
const OPNAME = 'TKFactors.Populate';
var
  LIndex: integer;
  LFieldProperty : TAbstractFieldProperty;
begin
  Result := False;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintChannelNumber');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintChannelNumber) not found in field properties');
    FFeatureID     := AFeatureID;
    FChannelNumber := AChannelNumber;
    for LIndex := LFieldProperty.ArrayLow to LFieldProperty.ArrayHigh do
    begin
      if(LIndex > High(FChannelNumbers)) then Break;
      FChannelNumbers[LIndex] := AChannelNumbers[LIndex];
    end;
    for LIndex := LFieldProperty.ArrayLow to LFieldProperty.ArrayHigh do
    begin
      if(LIndex > High(FKFactors)) then Break;
      FKFactors[LIndex] := AKFactors[LIndex];
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TKFactors.WriteChannelNumberToDB(AIndex: integer; AValue: double);
const OPNAME = 'TKFactors.WriteChannelNumberToDB';
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
        LLoadAgent.LoadContextData_PhysicalFlowConstraint(LContextData, FFeatureID,pfcgKFactors,pfcstChannelNumber,0,AIndex);
        if (AValue = NullFloat) then
          lNewValue := ''
        else
          lNewValue := FloatToStr(AValue);
        if (FChannelNumbers[AIndex] = NullFloat) then
          lOldValue := ''
        else
          lOldValue := FloatToStr(FChannelNumbers[AIndex]);
        if FAppModules.FieldProperties.UpdateFieldValue(
           'ConstraintChannelNumber', lNewValue, lOldValue, LContextData) then
        begin
          LPrevValue     := FChannelNumbers[AIndex];
          FChannelNumbers[AIndex] := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ConstraintChannelNumber',FloatToStr(LPrevValue),FloatToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TKFactors.WriteKFactorToDB(AIndex: integer; AValue: double);
const OPNAME = 'TKFactors.WriteKFactorToDB';
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
        LLoadAgent.LoadContextData_PhysicalFlowConstraint(LContextData, FFeatureID,pfcgKFactors,pfcstKFactor,0,AIndex);
        if (AValue = NullFloat) then
          lNewValue := ''
        else
          lNewValue := FloatToStr(AValue);
        if (FKFactors[AIndex] = NullFloat) then
          lOldValue := ''
        else
          lOldValue := FloatToStr(FKFactors[AIndex]);
        if FAppModules.FieldProperties.UpdateFieldValue(
           'ConstraintKFactor', lNewValue, lOldValue, LContextData) then
        begin
          LPrevValue     := FKFactors[AIndex];
          FKFactors[AIndex] := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ConstraintKFactor',FloatToStr(LPrevValue),FloatToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TSandAquifer }

function TSandAquifer._AddRef: Integer;
const OPNAME = 'TSandAquifer._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSandAquifer._Release: Integer;
const OPNAME = 'TSandAquifer._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSandAquifer.CreateMemberObjects;
const OPNAME = 'TSandAquifer.CreateMemberObjects';
var
  LFieldProperty : TAbstractFieldProperty;
begin
  inherited;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintHeadDifferences');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintHeadDifferences) not found in field properties');
    SetLength(FHeadDifferences,LFieldProperty.ArrayLength);

    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintAquiferFlows');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintAquiferFlows) not found in field properties');
    SetLength(FAquiferFlows,LFieldProperty.ArrayLength);

    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintDownStreamNodeInflows');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintDownStreamNodeInflows) not found in field properties');
    SetLength(FDownStreamNodeInflows,LFieldProperty.ArrayLength);

    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintRiverDepths');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintRiverDepths) not found in field properties');
    SetLength(FRiverDepths,LFieldProperty.ArrayLength);
    Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSandAquifer.DestroyMemberObjects;
const OPNAME = 'TSandAquifer.DestroyMemberObjects';
begin
  inherited;
  try
    Finalize(FHeadDifferences);
    Finalize(FAquiferFlows);
    Finalize(FDownStreamNodeInflows);
    Finalize(FRiverDepths);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSandAquifer.Initialise: boolean;
const OPNAME = 'TSandAquifer.Initialise';
var
  LIndex: integer;
begin
  Result := False;
  try
    FFeatureID     := NullInteger;
    FChannelNumber := NullInteger;
    for LIndex := Low(FHeadDifferences) to High(FHeadDifferences) do
      FHeadDifferences[LIndex] := NullFloat;
    for LIndex := Low(FAquiferFlows) to High(FAquiferFlows) do
      FAquiferFlows[LIndex] := NullFloat;
    for LIndex := Low(FDownStreamNodeInflows) to High(FDownStreamNodeInflows) do
      FDownStreamNodeInflows[LIndex] := NullFloat;
    for LIndex := Low(FRiverDepths) to High(FRiverDepths) do
      FRiverDepths[LIndex] := NullFloat;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSandAquifer.InitialiseArrays(APointsCount: integer): boolean;
const OPNAME = 'TSandAquifer.InitialiseArrays';
var
  LIndex: integer;
  LFieldProperty : TAbstractFieldProperty;
begin
  Result := False;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintAquiferFlows');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintAquiferFlows) not found in field properties');
    for LIndex := LFieldProperty.ArrayLow to APointsCount do
    begin
      FHeadDifferences[LIndex]       := 0.0;
      FAquiferFlows[LIndex]          := 0.0;
      FDownStreamNodeInflows[LIndex] := 0.0;
      FRiverDepths[LIndex]           := 0.0;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSandAquifer.UpdatePointsCount(APointsCount: integer): boolean;
const OPNAME = 'TSandAquifer.UpdatePointsCount';
var
  LIndex: integer;
  LFieldProperty : TAbstractFieldProperty;
begin
  Result := False;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintAquiferFlows');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintAquiferFlows) not found in field properties');
    for LIndex := LFieldProperty.ArrayLow to APointsCount do
    begin
      if(FHeadDifferences[LIndex] = NullFloat) then
        FHeadDifferences[LIndex] := 0.0;
      if(FAquiferFlows[LIndex] = NullFloat) then
        FAquiferFlows[LIndex] := 0.0;
      if(FDownStreamNodeInflows[LIndex] = NullFloat) then
        FDownStreamNodeInflows[LIndex] := 0.0;
      if(FRiverDepths[LIndex] = NullFloat) then
        FRiverDepths[LIndex] := 0.0;
    end;
    for LIndex := APointsCount+1 to LFieldProperty.ArrayHigh do
    begin
      FHeadDifferences[LIndex] := NullFloat;
      FAquiferFlows[LIndex] := NullFloat;
      FDownStreamNodeInflows[LIndex] := NullFloat;
      FRiverDepths[LIndex] := NullFloat;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSandAquifer.Get_CountNrOfPoints: Integer;
const OPNAME = 'TSandAquifer.Get_CountNrOfPoints';
var
  LIndex: integer;
  LFieldProperty : TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintAquiferFlows');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintAquiferFlows) not found in field properties');
    for LIndex := LFieldProperty.ArrayLow to LFieldProperty.ArrayHigh do
    begin
      if(FAquiferFlows[LIndex] = NullFloat) then
        Break
      else
        Result := Result + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSandAquifer.Get_AquiferFlowByIndex(AIndex: Integer): Double;
const OPNAME = 'TSandAquifer.Get_AquiferFlowByIndex';
begin
  Result := NullInteger;
  try
    if(AIndex >= Low(FAquiferFlows)) and (AIndex <= High(FAquiferFlows)) then
      Result := FAquiferFlows[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSandAquifer.Get_DownStreamNodeInflowByIndex(AIndex: Integer): Double;
const OPNAME = 'TSandAquifer.Get_DownStreamNodeInflowByIndex';
begin
  Result := NullInteger;
  try
    if(AIndex >= Low(FDownStreamNodeInflows)) and (AIndex <= High(FDownStreamNodeInflows)) then
      Result := FDownStreamNodeInflows[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSandAquifer.Get_HeadDifferenceByIndex(AIndex: Integer): Double;
const OPNAME = 'TSandAquifer.Get_HeadDifferenceByIndex';
begin
  Result := NullInteger;
  try
    if(AIndex >= Low(FHeadDifferences)) and (AIndex <= High(FHeadDifferences)) then
      Result := FHeadDifferences[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSandAquifer.Get_RiverDepthByIndex(AIndex: Integer): Double;
const OPNAME = 'TSandAquifer.Get_RiverDepthByIndex';
begin
  Result := NullInteger;
  try
    if(AIndex >= Low(FRiverDepths)) and (AIndex <= High(FRiverDepths)) then
      Result := FRiverDepths[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSandAquifer.Set_HeadDifferenceByIndex(AIndex: Integer;  AValue: Double);
const OPNAME = 'TSandAquifer.Set_HeadDifferenceByIndex';
var
  LIndex: integer;
  LFieldProperty : TAbstractFieldProperty;
begin
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintHeadDifferences');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintHeadDifferences) not found in field properties');
    if (AIndex >= LFieldProperty.ArrayLow) and (AIndex <= LFieldProperty.ArrayHigh) then
    begin
      if (FHeadDifferences[AIndex] <> AValue) then
      begin
        if (AValue = NullFloat) then
        begin
          for LIndex := AIndex+1 to LFieldProperty.ArrayHigh do
          begin
            WriteHeadDifferencesToDB(LIndex-1, FHeadDifferences[LIndex]);
            WriteAquiferFlowsToDB(LIndex-1, FAquiferFlows[LIndex]);
            WriteDownStreamNodeInflowsToDB(LIndex-1, FDownStreamNodeInflows[LIndex]);
            WriteRiverDepthsToDB(LIndex-1, FRiverDepths[LIndex]);
          end;
          WriteHeadDifferencesToDB(LFieldProperty.ArrayHigh, NullFloat);
          WriteAquiferFlowsToDB(LFieldProperty.ArrayHigh, NullFloat);
          WriteDownStreamNodeInflowsToDB(LFieldProperty.ArrayHigh, NullFloat);
          WriteRiverDepthsToDB(LFieldProperty.ArrayHigh, NullFloat);
        end
        else
        begin
          for LIndex := LFieldProperty.ArrayLow to AIndex-1 do
          begin
            if (FHeadDifferences[LIndex] = NullFloat) then
              WriteHeadDifferencesToDB(LIndex, 0.0);
            if (FAquiferFlows[LIndex] = NullFloat) then
              WriteAquiferFlowsToDB(LIndex, 0.0);
            if (FDownStreamNodeInflows[LIndex] = NullFloat) then
              WriteDownStreamNodeInflowsToDB(LIndex, 0.0);
            if (FRiverDepths[LIndex] = NullFloat) then
              WriteRiverDepthsToDB(LIndex, 0.0);
          end;
          WriteHeadDifferencesToDB(AIndex, AValue);
          if (FAquiferFlows[AIndex] = NullFloat) then
            WriteAquiferFlowsToDB(AIndex, 0.0);
          if (FDownStreamNodeInflows[AIndex] = NullFloat) then
            WriteDownStreamNodeInflowsToDB(AIndex, 0.0);
          if (FRiverDepths[AIndex] = NullFloat) then
            WriteRiverDepthsToDB(AIndex, 0.0);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSandAquifer.Set_AquiferFlowByIndex(AIndex: Integer;  AValue: Double);
const OPNAME = 'TSandAquifer.Set_AquiferFlowByIndex';
var
  LIndex: integer;
  LFieldProperty : TAbstractFieldProperty;
begin
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintAquiferFlows');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintAquiferFlows) not found in field properties');
    if (AIndex >= LFieldProperty.ArrayLow) and (AIndex <= LFieldProperty.ArrayHigh) then
    begin
      if (FAquiferFlows[AIndex] <> AValue) then
      begin
        if (AValue = NullFloat) then
        begin
          for LIndex := AIndex+1 to LFieldProperty.ArrayHigh do
          begin
            WriteHeadDifferencesToDB(LIndex-1, FHeadDifferences[LIndex]);
            WriteAquiferFlowsToDB(LIndex-1, FAquiferFlows[LIndex]);
            WriteDownStreamNodeInflowsToDB(LIndex-1, FDownStreamNodeInflows[LIndex]);
            WriteRiverDepthsToDB(LIndex-1, FRiverDepths[LIndex]);
          end;
          WriteHeadDifferencesToDB(LFieldProperty.ArrayHigh, NullFloat);
          WriteAquiferFlowsToDB(LFieldProperty.ArrayHigh, NullFloat);
          WriteDownStreamNodeInflowsToDB(LFieldProperty.ArrayHigh, NullFloat);
          WriteRiverDepthsToDB(LFieldProperty.ArrayHigh, NullFloat);
        end
        else
        begin
          for LIndex := LFieldProperty.ArrayLow to AIndex-1 do
          begin
            if (FHeadDifferences[LIndex] = NullFloat) then
              WriteHeadDifferencesToDB(LIndex, 0.0);
            if (FAquiferFlows[LIndex] = NullFloat) then
              WriteAquiferFlowsToDB(LIndex, 0.0);
            if (FDownStreamNodeInflows[LIndex] = NullFloat) then
              WriteDownStreamNodeInflowsToDB(LIndex, 0.0);
            if (FRiverDepths[LIndex] = NullFloat) then
              WriteRiverDepthsToDB(LIndex, 0.0);
          end;
          WriteAquiferFlowsToDB(AIndex, AValue);
          if (FHeadDifferences[AIndex] = NullFloat) then
            WriteHeadDifferencesToDB(AIndex, 0.0);
          if (FDownStreamNodeInflows[AIndex] = NullFloat) then
            WriteDownStreamNodeInflowsToDB(AIndex, 0.0);
          if (FRiverDepths[AIndex] = NullFloat) then
            WriteRiverDepthsToDB(AIndex, 0.0);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSandAquifer.Set_DownStreamNodeInflowByIndex(AIndex: Integer; AValue: Double);
const OPNAME = 'TSandAquifer.Set_DownStreamNodeInflowByIndex';
var
  LIndex: integer;
  LFieldProperty : TAbstractFieldProperty;
begin
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintDownStreamNodeInflows');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintDownStreamNodeInflows) not found in field properties');
    if (AIndex >= LFieldProperty.ArrayLow) and (AIndex <= LFieldProperty.ArrayHigh) then
    begin
      if (FDownStreamNodeInflows[AIndex] <> AValue) then
      begin
        if (AValue = NullFloat) then
        begin
          for LIndex := AIndex+1 to LFieldProperty.ArrayHigh do
          begin
            WriteHeadDifferencesToDB(LIndex-1, FHeadDifferences[LIndex]);
            WriteAquiferFlowsToDB(LIndex-1, FAquiferFlows[LIndex]);
            WriteDownStreamNodeInflowsToDB(LIndex-1, FDownStreamNodeInflows[LIndex]);
            WriteRiverDepthsToDB(LIndex-1, FRiverDepths[LIndex]);
          end;
          WriteHeadDifferencesToDB(LFieldProperty.ArrayHigh, NullFloat);
          WriteAquiferFlowsToDB(LFieldProperty.ArrayHigh, NullFloat);
          WriteDownStreamNodeInflowsToDB(LFieldProperty.ArrayHigh, NullFloat);
          WriteRiverDepthsToDB(LFieldProperty.ArrayHigh, NullFloat);
        end
        else
        begin
          for LIndex := LFieldProperty.ArrayLow to AIndex-1 do
          begin
            if (FHeadDifferences[LIndex] = NullFloat) then
              WriteHeadDifferencesToDB(LIndex, 0.0);
            if (FAquiferFlows[LIndex] = NullFloat) then
              WriteAquiferFlowsToDB(LIndex, 0.0);
            if (FDownStreamNodeInflows[LIndex] = NullFloat) then
              WriteDownStreamNodeInflowsToDB(LIndex, 0.0);
            if (FRiverDepths[LIndex] = NullFloat) then
              WriteRiverDepthsToDB(LIndex, 0.0);
          end;
          WriteDownStreamNodeInflowsToDB(AIndex, AValue);
          if (FHeadDifferences[AIndex] = NullFloat) then
            WriteHeadDifferencesToDB(AIndex, 0.0);
          if (FAquiferFlows[AIndex] = NullFloat) then
            WriteAquiferFlowsToDB(AIndex, 0.0);
          if (FRiverDepths[AIndex] = NullFloat) then
            WriteRiverDepthsToDB(AIndex, 0.0);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSandAquifer.Set_RiverDepthByIndex(AIndex: Integer;  AValue: Double);
const OPNAME = 'TSandAquifer.Set_RiverDepthByIndex';
var
  LIndex: integer;
  LFieldProperty : TAbstractFieldProperty;
begin
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintRiverDepths');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintRiverDepths) not found in field properties');
    if (AIndex >= LFieldProperty.ArrayLow) and (AIndex <= LFieldProperty.ArrayHigh) then
    begin
      if (FRiverDepths[AIndex] <> AValue) then
      begin
        if (AValue = NullFloat) then
        begin
          for LIndex := AIndex+1 to LFieldProperty.ArrayHigh do
          begin
            WriteHeadDifferencesToDB(LIndex-1, FHeadDifferences[LIndex]);
            WriteAquiferFlowsToDB(LIndex-1, FAquiferFlows[LIndex]);
            WriteDownStreamNodeInflowsToDB(LIndex-1, FDownStreamNodeInflows[LIndex]);
            WriteRiverDepthsToDB(LIndex-1, FRiverDepths[LIndex]);
          end;
          WriteHeadDifferencesToDB(LFieldProperty.ArrayHigh, NullFloat);
          WriteAquiferFlowsToDB(LFieldProperty.ArrayHigh, NullFloat);
          WriteDownStreamNodeInflowsToDB(LFieldProperty.ArrayHigh, NullFloat);
          WriteRiverDepthsToDB(LFieldProperty.ArrayHigh, NullFloat);
        end
        else
        begin
          for LIndex := LFieldProperty.ArrayLow to AIndex-1 do
          begin
            if (FHeadDifferences[LIndex] = NullFloat) then
              WriteHeadDifferencesToDB(LIndex, 0.0);
            if (FAquiferFlows[LIndex] = NullFloat) then
              WriteAquiferFlowsToDB(LIndex, 0.0);
            if (FDownStreamNodeInflows[LIndex] = NullFloat) then
              WriteDownStreamNodeInflowsToDB(LIndex, 0.0);
            if (FRiverDepths[LIndex] = NullFloat) then
              WriteRiverDepthsToDB(LIndex, 0.0);
          end;
          WriteRiverDepthsToDB(AIndex, AValue);
          if (FHeadDifferences[AIndex] = NullFloat) then
            WriteHeadDifferencesToDB(AIndex, 0.0);
          if (FDownStreamNodeInflows[AIndex] = NullFloat) then
            WriteDownStreamNodeInflowsToDB(AIndex, 0.0);
          if (FAquiferFlows[AIndex] = NullFloat) then
            WriteAquiferFlowsToDB(AIndex, 0.0);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSandAquifer.Populate(AFeatureID,AChannelNumber : integer;AHeadDifferences, AAquiferFlows,
         ADownStreamNodeInflows,ARiverDepths: TOneDimensionDoubleArray): WordBool;
const OPNAME = 'TSandAquifer.Populate';
var
  LIndex: integer;
begin
  Result := False;
  try
    FFeatureID     := AFeatureID;
    FChannelNumber := AChannelNumber;
    for LIndex := Low(AHeadDifferences) to High(AHeadDifferences) do
    begin
      if(LIndex > High(FHeadDifferences)) then Break;
      FHeadDifferences[LIndex] := AHeadDifferences[LIndex];
    end;
    for LIndex := Low(AAquiferFlows) to High(AAquiferFlows) do
    begin
      if(LIndex > High(FAquiferFlows)) then Break;
      FAquiferFlows[LIndex] := AAquiferFlows[LIndex];
    end;
    for LIndex := Low(ADownStreamNodeInflows) to High(ADownStreamNodeInflows) do
    begin
      if(LIndex > High(FDownStreamNodeInflows)) then Break;
      FDownStreamNodeInflows[LIndex] := ADownStreamNodeInflows[LIndex];
    end;
    for LIndex := Low(ARiverDepths) to High(ARiverDepths) do
    begin
      if(LIndex > High(FRiverDepths)) then Break;
      FRiverDepths[LIndex] := ARiverDepths[LIndex];
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSandAquifer.Validate(var AErrors: WideString;  const AContext: WideString): WordBool;
const OPNAME = 'TSandAquifer.Validate';
var
  LErrorList : TStringList;
  lStopOnFirstError: boolean;
begin
  Result := False;
  try
    lErrorList := TStringList.Create;
    try
      lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
      if (AContext = 'ConstraintHeadDifferences') then
        Result := ValidateHeadDifferences(LErrorList)
      else
      if (AContext = 'ConstraintAquiferFlows') then
        Result := ValidateAquiferFlows(LErrorList)
      else
      if (AContext = 'DownStreamNodeInflow') then
        Result := ValidateDownStreamNodeInflows(LErrorList)
      else
      if (AContext = 'RiverDepth') then
        Result := ValidateRiverDepths(LErrorList)
      else
      begin
        Result := ValidateHeadDifferences(lErrorList);
        if (Result OR (NOT lStopOnFirstError)) then
          Result := Result and  ValidateAquiferFlows(LErrorList);
        if (Result OR (NOT lStopOnFirstError)) then
          Result := Result and  ValidateDownStreamNodeInflows(LErrorList);
        if (Result OR (NOT lStopOnFirstError)) then
          Result := Result and  ValidateRiverDepths(LErrorList);
      end;
      AErrors := AErrors + lErrorList.CommaText;
    finally
      FreeAndNil(lErrorList);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSandAquifer.ValidateHeadDifferences(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TSandAquifer.ValidateHeadDifferences';
var
  lMessage          : string;
  lStopOnFirstError : Boolean;
  lValid            : Boolean;
  lResult           : Boolean;
  lIndex            : integer;
  lContinue         : Boolean;
  lCount            : integer;
  LFieldProperty    : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintHeadDifferences');
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    lIndex    := LFieldProperty.ArrayLow;
    lContinue := TRUE;
    lCount    := 0;
    while (lContinue AND (lIndex  <= LFieldProperty.ArrayHigh)) do
    begin
      if ((FHeadDifferences[lIndex] = NullFloat) AND (FAquiferFlows[lIndex] = NullFloat) AND
         (FDownStreamNodeInflows[lIndex] = NullFloat) AND (FRiverDepths[lIndex] = NullFloat)) then
        lContinue := FALSE
      else
      begin
        lMessage := '';
        lCount  := lCount + 1;
        lValid := FAppModules.FieldProperties.ValidateFieldProperty
                    ('ConstraintHeadDifferences', FloatToStr(FHeadDifferences[lIndex]),
                    lMessage, lIndex);
        if (NOT lValid) then
        begin
          lResult := FALSE;
          AErrorMessages.Add(lMessage);
          if (lStopOnFirstError) then
            Break;
        end;
        if (lResult OR (NOT lStopOnFirstError)) then
        begin
          if (((FAquiferFlows[lIndex] = NullFloat) or
               (FDownStreamNodeInflows[lIndex] = NullFloat) or
               (FRiverDepths[lIndex] = NullFloat)) AND
              (FHeadDifferences[lIndex] = NullFloat)) then
          begin
            lMessage := FAppModules.Language.GetString('ContextValidation.UnequalNrOfArrayValues');
            AErrorMessages.Add(Format(lMessage, ['Channel ('+IntToStr(FChannelNumber)+')']));
            lResult := FALSE;
          end;
        end;
        lIndex := lIndex + 1;
      end;
    end;
    if (lResult OR (NOT lStopOnFirstError)) then
    begin
      if (lCount < 1) then
      begin
        lResult := FALSE;
        lMessage := FAppModules.Language.GetString('ContextValidation.NotEnoughArrayValues');
        AErrorMessages.Add(Format(lMessage, ['Channel ('+IntToStr(FChannelNumber)+')']));
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSandAquifer.ValidateAquiferFlows(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TSandAquifer.ValidateAquiferFlows';
var
  lMessage          : string;
  lStopOnFirstError : Boolean;
  lValid            : Boolean;
  lResult           : Boolean;
  lIndex            : integer;
  lContinue         : Boolean;
  lCount            : integer;
  LFieldProperty    : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintAquiferFlows');
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    lIndex    := LFieldProperty.ArrayLow;
    lContinue := TRUE;
    lCount    := 0;
    while (lContinue AND (lIndex  <= LFieldProperty.ArrayHigh)) do
    begin
      if ((FHeadDifferences[lIndex] = NullFloat) AND (FAquiferFlows[lIndex] = NullFloat) AND
         (FDownStreamNodeInflows[lIndex] = NullFloat) AND (FRiverDepths[lIndex] = NullFloat)) then
        lContinue := FALSE
      else
      begin
        lMessage := '';
        lCount  := lCount + 1;
        lValid := FAppModules.FieldProperties.ValidateFieldProperty
                    ('ConstraintAquiferFlows', FloatToStr(FHeadDifferences[lIndex]),
                    lMessage, lIndex);
        if (NOT lValid) then
        begin
          lResult := FALSE;
          AErrorMessages.Add(lMessage);
          if (lStopOnFirstError) then
            Break;
        end;
        if (lResult OR (NOT lStopOnFirstError)) then
        begin
          if (((FHeadDifferences[lIndex] = NullFloat) or
               (FDownStreamNodeInflows[lIndex] = NullFloat) or
               (FRiverDepths[lIndex] = NullFloat)) AND
              (FAquiferFlows[lIndex] = NullFloat)) then
          begin
            lMessage := FAppModules.Language.GetString('ContextValidation.UnequalNrOfArrayValues');
            AErrorMessages.Add(Format(lMessage, ['Channel ('+IntToStr(FChannelNumber)+')']));
            lResult := FALSE;
          end;
        end;
        lIndex := lIndex + 1;
      end;
    end;
    if (lResult OR (NOT lStopOnFirstError)) then
    begin
      if (lCount < 1) then
      begin
        lResult := FALSE;
        lMessage := FAppModules.Language.GetString('ContextValidation.NotEnoughArrayValues');
        AErrorMessages.Add(Format(lMessage, ['Channel ('+IntToStr(FChannelNumber)+')']));
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSandAquifer.ValidateDownStreamNodeInflows(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TSandAquifer.ValidateDownStreamNodeInflows';
var
  lMessage          : string;
  lStopOnFirstError : Boolean;
  lValid            : Boolean;
  lResult           : Boolean;
  lIndex            : integer;
  lContinue         : Boolean;
  lCount            : integer;
  LFieldProperty    : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintDownStreamNodeInflows');
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    lIndex    := LFieldProperty.ArrayLow;
    lContinue := TRUE;
    lCount    := 0;
    while (lContinue AND (lIndex  <= LFieldProperty.ArrayHigh)) do
    begin
      if ((FDownStreamNodeInflows[lIndex] = NullFloat) AND (FAquiferFlows[lIndex] = NullFloat) AND
         (FHeadDifferences[lIndex] = NullFloat) AND (FRiverDepths[lIndex] = NullFloat)) then
        lContinue := FALSE
      else
      begin
        lMessage := '';
        lCount  := lCount + 1;
        lValid := FAppModules.FieldProperties.ValidateFieldProperty
                    ('ConstraintDownStreamNodeInflows', FloatToStr(FDownStreamNodeInflows[lIndex]),
                    lMessage, lIndex);
        if (NOT lValid) then
        begin
          lResult := FALSE;
          AErrorMessages.Add(lMessage);
          if (lStopOnFirstError) then
            Break;
        end;
        if (lResult OR (NOT lStopOnFirstError)) then
        begin
          if (((FAquiferFlows[lIndex] = NullFloat) or
               (FHeadDifferences[lIndex] = NullFloat) or
               (FRiverDepths[lIndex] = NullFloat)) AND
              (FDownStreamNodeInflows[lIndex] = NullFloat)) then
          begin
            lMessage := FAppModules.Language.GetString('ContextValidation.UnequalNrOfArrayValues');
            AErrorMessages.Add(Format(lMessage, ['Channel ('+IntToStr(FChannelNumber)+')']));
            lResult := FALSE;
          end;
        end;
        lIndex := lIndex + 1;
      end;
    end;
    if (lResult OR (NOT lStopOnFirstError)) then
    begin
      if (lCount < 1) then
      begin
        lResult := FALSE;
        lMessage := FAppModules.Language.GetString('ContextValidation.NotEnoughArrayValues');
        AErrorMessages.Add(Format(lMessage, ['Channel ('+IntToStr(FChannelNumber)+')']));
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSandAquifer.ValidateRiverDepths(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TSandAquifer.ValidateRiverDepths';
var
  lMessage          : string;
  lStopOnFirstError : Boolean;
  lValid            : Boolean;
  lResult           : Boolean;
  lIndex            : integer;
  lContinue         : Boolean;
  lCount            : integer;
  LFieldProperty    : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintRiverDepths');
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    lIndex    := LFieldProperty.ArrayLow;
    lContinue := TRUE;
    lCount    := 0;
    while (lContinue AND (lIndex  <= LFieldProperty.ArrayHigh)) do
    begin
      if ((FRiverDepths[lIndex] = NullFloat) AND (FAquiferFlows[lIndex] = NullFloat) AND
         (FDownStreamNodeInflows[lIndex] = NullFloat) AND (FHeadDifferences[lIndex] = NullFloat)) then
        lContinue := FALSE
      else
      begin
        lMessage := '';
        lCount  := lCount + 1;
        lValid := FAppModules.FieldProperties.ValidateFieldProperty
                    ('ConstraintRiverDepths', FloatToStr(FRiverDepths[lIndex]),
                    lMessage, lIndex);
        if (NOT lValid) then
        begin
          lResult := FALSE;
          AErrorMessages.Add(lMessage);
          if (lStopOnFirstError) then
            Break;
        end;
        if (lResult OR (NOT lStopOnFirstError)) then
        begin
          if (((FAquiferFlows[lIndex] = NullFloat) or
               (FDownStreamNodeInflows[lIndex] = NullFloat) or
               (FHeadDifferences[lIndex] = NullFloat)) AND
              (FRiverDepths[lIndex] = NullFloat)) then
          begin
            lMessage := FAppModules.Language.GetString('ContextValidation.UnequalNrOfArrayValues');
            AErrorMessages.Add(Format(lMessage, ['Channel ('+IntToStr(FChannelNumber)+')']));
            lResult := FALSE;
          end;
        end;
        lIndex := lIndex + 1;
      end;
    end;
    if (lResult OR (NOT lStopOnFirstError)) then
    begin
      if (lCount < 1) then
      begin
        lResult := FALSE;
        lMessage := FAppModules.Language.GetString('ContextValidation.NotEnoughArrayValues');
        AErrorMessages.Add(Format(lMessage, ['Channel ('+IntToStr(FChannelNumber)+')']));
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSandAquifer.WriteHeadDifferencesToDB(AIndex: integer; AValue: double);
const OPNAME = 'TSandAquifer.WriteHeadDifferencesToDB';
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
        LLoadAgent.LoadContextData_PhysicalFlowConstraint(LContextData, FFeatureID,pfcgSandAquifer,pfcstHeadDifference,0,AIndex);
        if (AValue = NullFloat) then
          lNewValue := ''
        else
          lNewValue := FloatToStr(AValue);
        if (FHeadDifferences[AIndex] = NullFloat) then
          lOldValue := ''
        else
          lOldValue := FloatToStr(FHeadDifferences[AIndex]);
        if FAppModules.FieldProperties.UpdateFieldValue(
           'ConstraintHeadDifferences', lNewValue, lOldValue, LContextData) then
        begin
          LPrevValue     := FHeadDifferences[AIndex];
          FHeadDifferences[AIndex] := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ConstraintHeadDifferences',FloatToStr(LPrevValue),FloatToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSandAquifer.WriteAquiferFlowsToDB(AIndex: integer;AValue: double);
const OPNAME = 'TSandAquifer.WriteAquiferFlowsToDB';
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
        LLoadAgent.LoadContextData_PhysicalFlowConstraint(LContextData, FFeatureID,pfcgSandAquifer,pfcstAquiferFlow,0,AIndex);
        if (AValue = NullFloat) then
          lNewValue := ''
        else
          lNewValue := FloatToStr(AValue);
        if (FAquiferFlows[AIndex] = NullFloat) then
          lOldValue := ''
        else
          lOldValue := FloatToStr(FAquiferFlows[AIndex]);
        if FAppModules.FieldProperties.UpdateFieldValue(
           'ConstraintAquiferFlows', lNewValue, lOldValue, LContextData) then
        begin
          LPrevValue     := FAquiferFlows[AIndex];
          FAquiferFlows[AIndex] := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ConstraintAquiferFlows',FloatToStr(LPrevValue),FloatToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSandAquifer.WriteDownStreamNodeInflowsToDB(AIndex: integer; AValue: double);
const OPNAME = 'TSandAquifer.WriteDownStreamNodeInflowsToDB';
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
        LLoadAgent.LoadContextData_PhysicalFlowConstraint(LContextData, FFeatureID,pfcgSandAquifer,pfcstDownStreamNodeInflow,0,AIndex);
        if (AValue = NullFloat) then
          lNewValue := ''
        else
          lNewValue := FloatToStr(AValue);
        if (FDownStreamNodeInflows[AIndex] = NullFloat) then
          lOldValue := ''
        else
          lOldValue := FloatToStr(FDownStreamNodeInflows[AIndex]);
        if FAppModules.FieldProperties.UpdateFieldValue(
           'ConstraintDownStreamNodeInflows', lNewValue, lOldValue, LContextData) then
        begin
          LPrevValue     := FDownStreamNodeInflows[AIndex];
          FDownStreamNodeInflows[AIndex] := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ConstraintDownStreamNodeInflows',FloatToStr(LPrevValue),FloatToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSandAquifer.WriteRiverDepthsToDB(AIndex: integer; AValue: double);
const OPNAME = 'TSandAquifer.WriteRiverDepthsToDB';
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
        LLoadAgent.LoadContextData_PhysicalFlowConstraint(LContextData, FFeatureID,pfcgSandAquifer,pfcstRiverDepth,0,AIndex);
        if (AValue = NullFloat) then
          lNewValue := ''
        else
          lNewValue := FloatToStr(AValue);
        if (FRiverDepths[AIndex] = NullFloat) then
          lOldValue := ''
        else
          lOldValue := FloatToStr(FRiverDepths[AIndex]);
        if FAppModules.FieldProperties.UpdateFieldValue(
           'ConstraintRiverDepths', lNewValue, lOldValue, LContextData) then
        begin
          LPrevValue     := FRiverDepths[AIndex];
          FRiverDepths[AIndex] := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ConstraintRiverDepths',FloatToStr(LPrevValue),FloatToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{TPhysicalFlowConstraint}
function TPhysicalFlowConstraint._AddRef: Integer;
const OPNAME = 'TPhysicalFlowConstraint._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraint._Release: Integer;
const OPNAME = 'TPhysicalFlowConstraint._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPhysicalFlowConstraint.CreateMemberObjects;
const OPNAME = 'TPhysicalFlowConstraint.CreateMemberObjects';
begin
  inherited;
  try
    FDischargeCurve  := nil;
    FKFactors        := nil;
    FSandAquifer     := nil;
    FSubmergedOutlet := nil;
    FPumpStation     := nil;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPhysicalFlowConstraint.DestroyMemberObjects;
const OPNAME = 'TPhysicalFlowConstraint.DestroyMemberObjects';
begin
  try
    FreeAndNil(FDischargeCurve);
    FreeAndNil(FKFactors);
    FreeAndNil(FSandAquifer);
    FreeAndNil(FSubmergedOutlet);
    FreeAndNil(FPumpStation);
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraint.Initialise: boolean;
const OPNAME = 'TPhysicalFlowConstraint.Initialise';
begin
  Result := inherited Initialise;
  try
    FChannelNumber         := 0;
    FFeatureID             := -1;
    FFeatureName           := '';
    FUpstreamReservoirNr   := 0;
    FDownstreamReservoirNr := 0;
    FStructureType         := -1;
    FElevationOfSill       := -1.0;
    FMaximumGateHeight     := -1.0;
    FDischargeCoefficient  := -1.0;
    FStructureLength       := -1.0;
    FNrOfPoints            := -1;
    if Assigned(FDischargeCurve) then
      FDischargeCurve.Initialise;
    if Assigned(FKFactors) then
      FKFactors.Initialise;
    if Assigned(FSandAquifer) then
      FSandAquifer.Initialise;
    if Assigned(FSubmergedOutlet) then
      FSubmergedOutlet.Initialise;
    if Assigned(FPumpStation) then
      FPumpStation.Initialise;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraint.Get_FeatureID : integer;
const OPNAME = 'TPhysicalFlowConstraint.Get_FeatureID';
begin
  Result := 0;
  try
    Result := FFeatureID;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraint.Get_FeatureName : WideString;
const OPNAME = 'TPhysicalFlowConstraint.Get_FeatureName';
begin
  Result := '';
  try
    Result := FFeatureName;
  except on E : Exception do HandleError(E, OPNAME); end;
end;


function TPhysicalFlowConstraint.Populate
                                  (AFeatureID                 : integer;
                                   AFeatureName               : WideString;
                                   AChannelNr                 : integer;
                                   AUpstreamReservoirNr       : integer;
                                   ADownstreamReservoirNr     : integer;
                                   AStructureType             : integer;
                                   AElevationOfSill           : double;
                                   AMaximumGateHeight         : double;
                                   ADischargeCoefficient      : double;
                                   AStructureLength           : double;
                                   ANrOfPoints                : integer;
                                   AWaterlevelAtDownstreamNode: double;
                                   AReferenceElevation        : double
                                   ): WordBool;
const OPNAME = 'TPhysicalFlowConstraint.Populate';
begin
  Result := FALSE;
  try
    FChannelNumber              := AChannelNr;
    FFeatureID                  := AFeatureID;
    FFeatureName                := AFeatureName;
    FUpstreamReservoirNr        := AUpstreamReservoirNr;
    FDownstreamReservoirNr      := ADownstreamReservoirNr;
    FStructureType              := AStructureType;
    FElevationOfSill            := AElevationOfSill;
    FMaximumGateHeight          := AMaximumGateHeight;
    FDischargeCoefficient       := ADischargeCoefficient;
    FStructureLength            := AStructureLength;
    FNrOfPoints                 := ANrOfPoints;
    FWaterLevelAtDownstreamNode := AWaterLevelAtDownstreamNode;
    FReferenceElevation         := AReferenceElevation;
    CreateStructurePerType;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPhysicalFlowConstraint.CreateStructurePerType;
const OPNAME = 'TPhysicalFlowConstraint.CreateStructurePerType';
begin
  try
    case FStructureType of
      1,2,3,6:begin end;
      4,5,7,8,9,14
      :
        begin
          FDischargeCurve  := TDischargeCurve.Create(FAppModules);
          FDischargeCurve.Initialise;
          FDischargeCurve.InitialiseArrays(NrOfPoints);
          FDischargeCurve.FFeatureID := FFeatureID;
          FDischargeCurve.FChannelNumber := FChannelNumber;
        end;
      10:
        begin
          FKFactors        := TKFactors.Create(FAppModules);
          FKFactors.Initialise;
          FKFactors.InitialiseArrays(NrOfPoints);
          FKFactors.FFeatureID := FFeatureID;
          FKFactors.FChannelNumber := FChannelNumber;
        end;
      11:
        begin
          FSandAquifer     := TSandAquifer.Create(FAppModules);
          FSandAquifer.Initialise;
          FSandAquifer.InitialiseArrays(NrOfPoints);
          FSandAquifer.FFeatureID := FFeatureID;
          FSandAquifer.FChannelNumber := FChannelNumber;
        end;
      12:
        begin
          FSubmergedOutlet := TSubmergedOutlet.Create(FAppModules);
          FSubmergedOutlet.Initialise;
          FSubmergedOutlet.InitialiseArrays(NrOfPoints);
          FSubmergedOutlet.FFeatureID := FFeatureID;
          FSubmergedOutlet.FChannelNumber := FChannelNumber;
        end;
      13:
        begin
          FPumpStation     := TPumpStation.Create(FAppModules);
          FPumpStation.Initialise;
          FPumpStation.InitialiseArrays(NrOfPoints);
          FPumpStation.FFeatureID := FFeatureID;
          FPumpStation.FChannelNumber := FChannelNumber;
        end;
      else
        raise Exception.Create('Physical Flow Constraint Structure type('+IntToStr(FStructureType)+') on  channel('+
              IntToStr(FChannelNumber)+') is unknown type');
    end;//case
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPhysicalFlowConstraint.DeleteStructurePerType;
const OPNAME = 'TPhysicalFlowConstraint.DeleteStructurePerType';
begin
  try
    case FStructureType of
      1,2,3,6:begin end;
      4,5,7,8,9,
      14:        FreeAndNil(FDischargeCurve);
      10:        FreeAndNil(FKFactors);
      11:        FreeAndNil(FSandAquifer);
      12:        FreeAndNil(FSubmergedOutlet);
      13:        FreeAndNil(FPumpStation);
      else
        raise Exception.Create('Physical Flow Constraint Structure type('+IntToStr(FStructureType)+') on  channel('+
              IntToStr(FChannelNumber)+') is unknown type');
    end;//case
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraint.Get_UpstreamReservoirNr : integer;
const OPNAME = 'TPhysicalFlowConstraint.Get_UpstreamReservoirNr';
begin
  Result := -1;
  try
    Result := FUpstreamReservoirNr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraint.Get_DownstreamReservoirNr : integer;
const OPNAME = 'TPhysicalFlowConstraint.Get_DownstreamReservoirNr';
begin
  Result := -1;
  try
    Result := FDownstreamReservoirNr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraint.Get_StructureType: integer;
const OPNAME = 'TPhysicalFlowConstraint.Get_StructureType';
begin
  Result := 2;
  try
    Result := FStructureType;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraint.Get_ElevationOfSill: double;
const OPNAME = 'TPhysicalFlowConstraint.Get_ElevationOfSill';
begin
  Result := 0.0;
  try
    Result := FElevationOfSill;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraint.Get_MaximumGateHeight: double;
const OPNAME = 'TPhysicalFlowConstraint.Get_MaximumGateHeight';
begin
  Result := 0.0;
  try
    Result := FMaximumGateHeight;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraint.Get_DischargeCoefficient: double;
const OPNAME = 'TPhysicalFlowConstraint.Get_DischargeCoefficient';
begin
  Result := 0.0;
  try
    Result := FDischargeCoefficient;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraint.Get_StructureLength: double;
const OPNAME = 'TPhysicalFlowConstraint.Get_StructureLength';
begin
  Result := 0.0;
  try
    Result := FStructureLength;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraint.Get_NrOfPoints : integer;
const OPNAME = 'TPhysicalFlowConstraint.Get_NrOfPoints';
begin
  Result := 0;
  try
    if (FStructureType = 12) then
      Result := 10
    else
      Result := FNrOfPoints;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPhysicalFlowConstraint.Set_FeatureName (const AName : WideString);
const OPNAME = 'TPhysicalFlowConstraint.Set_FeatureName';
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
           'PhysicalFlowConstraintFeatureName', AName, FFeatureName, LContextData) then
        begin
          LOldValue    := FFeatureName;
          FFeatureName := AName;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'PhysicalFlowConstraintFeatureName',LOldValue,AName);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPhysicalFlowConstraint.Set_UpstreamReservoir(const AReservoir: IReservoirData);
const OPNAME = 'TPhysicalFlowConstraint.Set_UpstreamReservoir';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  lNewValue    : integer;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        if (AReservoir = nil) then
          lNewValue := 0
        else
          lNewValue := AReservoir.ReservoirConfigurationData.ReservoirIdentifier;
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'UpStreamReservoirNumber', IntToStr(lNewValue), IntToStr(FUpstreamReservoirNr), LContextData) then
        begin
          FUpstreamReservoirNr := lNewValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'UpStreamReservoirNumber',
            IntToStr(FUpstreamReservoirNr),IntToStr(lNewValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPhysicalFlowConstraint.Set_DownstreamReservoir(const AReservoir: IReservoirData);
const OPNAME = 'TPhysicalFlowConstraint.Set_DownstreamReservoir';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  lNewValue    : integer;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        if (AReservoir = nil) then
          lNewValue := 0
        else
          lNewValue := AReservoir.ReservoirConfigurationData.ReservoirIdentifier;
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'DownStreamReservoirNumber', IntToStr(lNewValue), IntToStr(FDownstreamReservoirNr), LContextData) then
        begin
          FDownstreamReservoirNr := lNewValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'DownStreamReservoirNumber',
            IntToStr(FDownstreamReservoirNr),IntToStr(lNewValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPhysicalFlowConstraint.Set_StructureType(AStructureType: integer);
const OPNAME = 'TPhysicalFlowConstraint.Set_StructureType';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  lOldType     : integer;
begin
  try
    if(AStructureType = FStructureType) then Exit;
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        lOldType := FStructureType;
        if FAppModules.FieldProperties.UpdateFieldValue(
           'StructureType', FloatToStr(AStructureType), FloatToStr(FStructureType), LContextData) then
        begin
          Self.ElevationOfSill     := 0;
          Self.MaximumGateHeight   := 0;
          Self.DischargeCoefficient:= 1.7;
          Self.StructureLength     := 0;
          Self.NrOfPoints          := 0; 
          LLoadAgent.DeletePhysicalFlowConstraintValue(Self);
          DeleteStructurePerType;
          FStructureType := AStructureType;
          CreateStructurePerType;
          LLoadAgent.InsertPhysicalFlowConstraintValue(Self);
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'StructureType',IntToStr(lOldType),IntToStr(AStructureType));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPhysicalFlowConstraint.Set_ElevationOfSill(AElevation: double);
const OPNAME = 'TPhysicalFlowConstraint.Set_ElevationOfSill';
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
           'SillElevation', FloatToStr(AElevation), FloatToStr(FElevationOfSill), LContextData) then
        begin
          LPrevValue     := FElevationOfSill;
          FElevationOfSill := AElevation;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SillElevation',FloatToStr(LPrevValue),FloatToStr(AElevation));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPhysicalFlowConstraint.Set_MaximumGateHeight(AHeight: double);
const OPNAME = 'TPhysicalFlowConstraint.Set_MaximumGateHeight';
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
           'GateHeight', FloatToStr(AHeight), FloatToStr(FMaximumGateHeight), LContextData) then
        begin
          LPrevValue     := FMaximumGateHeight;
          FMaximumGateHeight := AHeight;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'GateHeight',FloatToStr(LPrevValue),FloatToStr(AHeight));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPhysicalFlowConstraint.Set_DischargeCoefficient(ACoefficient: double);
const OPNAME = 'TPhysicalFlowConstraint.Set_DischargeCoefficient';
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
           'DischargeCoefficient', FloatToStr(ACoefficient), FloatToStr(FDischargeCoefficient), LContextData) then
        begin
          LPrevValue     := FDischargeCoefficient;
          FDischargeCoefficient := ACoefficient;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'DischargeCoefficient',FloatToStr(LPrevValue),FloatToStr(ACoefficient));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPhysicalFlowConstraint.Set_StructureLength(ALength: double);
const OPNAME = 'TPhysicalFlowConstraint.Set_StructureLength';
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
           'ControlStructureLength', FloatToStr(ALength), FloatToStr(FStructureLength), LContextData) then
        begin
          LPrevValue     := FStructureLength;
          FStructureLength := ALength;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ControlStructureLength',FloatToStr(LPrevValue),FloatToStr(ALength));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPhysicalFlowConstraint.Set_NrOfPoints (ACount : integer);
const OPNAME = 'TPhysicalFlowConstraint.Set_NrOfPoints';
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
           'PointsElevationNumber', IntToStr(ACount), IntToStr(NrOfPoints), LContextData) then
        begin
          LPrevValue       := FNrOfPoints;
          FNrOfPoints := ACount;
          case FStructureType of
            4,5,7,8,9,
            14:        FDischargeCurve.UpdatePointsCount(NrOfPoints);
            10:        FKFactors.UpdatePointsCount(NrOfPoints);
            11:        FSandAquifer.UpdatePointsCount(NrOfPoints);
            12:        FSubmergedOutlet.UpdatePointsCount(NrOfPoints);
            13:        FPumpStation.UpdatePointsCount(NrOfPoints);
          end;//case
          LLoadAgent.InsertPhysicalFlowConstraintValue(Self);
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'PointsElevationNumber',IntToStr(LPrevValue),IntToStr(ACount));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPhysicalFlowConstraint.Set_WaterLevelAtDownstreamNode(ALevel: double);
const OPNAME = 'TPhysicalFlowConstraint.Set_WaterLevelAtDownstreamNode';
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
           'WaterLevelAtDownstreamNode', FloatToStr(ALevel), FloatToStr(FStructureLength), LContextData) then
        begin
          LPrevValue     := FWaterLevelAtDownstreamNode;
          FWaterLevelAtDownstreamNode := ALevel;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'WaterLevelAtDownstreamNode',FloatToStr(LPrevValue),FloatToStr(ALevel));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraint.Get_WaterLevelAtDownstreamNode: double;
const OPNAME = 'TPhysicalFlowConstraint.Get_WaterLevelAtDownstreamNode';
begin
  Result := 0.0;
  try
    Result := FWaterlevelAtDownstreamNode;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPhysicalFlowConstraint.Set_ReferenceElevation(AElevation: double);
const OPNAME = 'TPhysicalFlowConstraint.Set_ReferenceElevation';
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
           'ReferenceElevation', FloatToStr(AElevation), FloatToStr(FReferenceElevation), LContextData) then
        begin
          LPrevValue     := FReferenceElevation;
          FReferenceElevation := AElevation;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ReferenceElevation',FloatToStr(LPrevValue),FloatToStr(AElevation));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraint.Get_ReferenceElevation: double;
const OPNAME = 'TPhysicalFlowConstraint.Get_ReferenceElevation';
begin
  Result := 0.0;
  try
    Result := FReferenceElevation;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraint.Validate (var AErrors    : WideString;
                                           const AContext : WideString='') : WordBool;
const OPNAME = 'TPhysicalFlowConstraint.Validate';
var
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lErrorList        : TStringList;
begin
  Result := FALSE;
  try
    lErrorList := TStringList.Create;
    try
      if (AContext = 'FeatureName') then
        Result := ValidateFeatureName(lErrorList)
      else
      if (AContext = 'StructureType') then
        Result := ValidateConstraintType(lErrorList)
      else
      if (AContext = 'UpStreamReservoirNumber') then
        Result := ValidateUpstreamReservoir(lErrorList)
      else
      if (AContext = 'DownStreamReservoirNumber') then
        Result := ValidateDownstreamReservoir(lErrorList)
      else
      if (AContext = 'ElevationOfSill') then
        Result := ValidateElevationOfSill(lErrorList)
      else
      if (AContext = 'MaximumGateHeight') then
        Result := ValidateMaximumGateHeight(lErrorList)
      else
      if (AContext = 'DischargeCoefficient') then
        Result := ValidateDischargeCoefficient(lErrorList)
      else
      if (AContext = 'StructureLength') then
        Result := ValidateStructureLength(lErrorList)
      else
      begin
        Result := TRUE;
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        if (NOT ValidateFeatureName(lErrorList)) then
          Result := FALSE;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateUpstreamReservoir(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateDownstreamReservoir(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateConstraintType(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (FStructureType in [2, 3]) then
          begin
            if (NOT ValidateElevationOfSill(lErrorList)) then
              Result := FALSE;
          end;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (FStructureType in [2, 3, 10]) then
          begin
            if (NOT ValidateMaximumGateHeight(lErrorList)) then
              Result := FALSE;
          end;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (FStructureType in [2, 3]) then
          begin
            if (NOT ValidateDischargeCoefficient(lErrorList)) then
              Result := FALSE;
          end;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (FStructureType in [2, 3, 6]) then
          begin
            if (NOT ValidateStructureLength(lErrorList)) then
              Result := FALSE;
          end;
        end;
        if (FChannelNumber = 0) then
        begin
          lMessage := FAppModules.Language.GetString('ContextValidation.PhysicalFlowConstraintNotAssignedToChannel');
          lErrorList.Add(Format(lMessage, [IntToStr(FFeatureID)]));
          Result := TRUE;
        end;
      end;
      AErrors := AErrors + lErrorList.Text;
    finally
      FreeAndNil(lErrorList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPhysicalFlowConstraint.ValidateFeatureName(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TPhysicalFlowConstraint.ValidateFeatureName';
var
  lMessage     : string;
  lUnique      : boolean;
  lIndex       : integer;
  lFeature     : TPhysicalFlowConstraint;
  lFeaturelist : TPhysicalFlowConstraintList;
begin
  result := FALSE;
  try
    lMessage := '';
    if ( NOT FAppModules.FieldProperties.ValidateFieldProperty('PhysicalFlowConstraintFeatureName', FFeatureName, lMessage)) then
      AErrorMessages.Add('Channel ('+IntToStr(FChannelNumber)+')' +':'+lMessage)
    else
    begin
      lFeaturelist := TYieldModelDataObject(FAppModules.Model.ModelData).
                        CastNetworkFeaturesData.CastPhysicalFlowConstraintList;
      lUnique := TRUE;
      lIndex  := 0;
      while (lUnique AND (lIndex < lFeaturelist.PhysicalFlowConstraintCount)) do
      begin
        lFeature := lFeaturelist.CastPhysicalFlowConstraintByIndex(lIndex);
        if ((FFeatureID <> lFeature.FeatureID) AND
            (UpperCase(trim(lFeature.FeatureName)) = UpperCase(trim(FFeatureName)))) then
        begin
          lMessage := FAppModules.language.GetString('ContextValidation.DuplicateIFRFeatureName');
          AErrorMessages.Add(Format(lMessage, ['Channel ('+IntToStr(FChannelNumber)+')']));
          lUnique := FALSE;
        end
        else
          lIndex := lIndex + 1;
      end;
      Result := lUnique;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraint.ValidateUpstreamReservoir(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TPhysicalFlowConstraint.ValidateUpstreamReservoir';
var
  lReservoir   : IReservoirData;
  lMessage     : string;
begin
  Result := FALSE;
  try
    lMessage  := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ('UpStreamReservoirNumber', IntToStr(FUpstreamReservoirNr), lMessage);
    if (NOT Result) then
      AErrorMessages.Add(lMessage)
    else
    begin
      if (FUpstreamReservoirNr = 0) then
        Result := TRUE
      else
      begin
        lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[FUpstreamReservoirNr];
        if (lReservoir = nil) then
        begin
          lMessage := FAppModules.Language.GetString('ContextValidation.InvalidUpstreamReservoir');
          AErrorMessages.Add(Format(lMessage, ['Channel ('+IntToStr(FChannelNumber)+')']));
        end
        else
          Result := TRUE;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPhysicalFlowConstraint.ValidateDownstreamReservoir(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TPhysicalFlowConstraint.ValidateDownstreamReservoir';
var
  lReservoir   : IReservoirData;
  lMessage     : string;
begin
  Result := FALSE;
  try
    lMessage  := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ('DownStreamReservoirNumber', IntToStr(FDownstreamReservoirNr), lMessage);
    if (NOT Result) then
      AErrorMessages.Add(lMessage)
    else
    begin
      if (FDownstreamReservoirNr = 0) then
        Result := TRUE
      else
      begin
        lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[FDownstreamReservoirNr];
        if (lReservoir = nil) then
        begin
          lMessage := FAppModules.Language.GetString('ContextValidation.InvalidDownstreamReservoir');
          AErrorMessages.Add(Format(lMessage, ['Channel ('+IntToStr(FChannelNumber)+')']));
        end
        else
          Result := TRUE;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPhysicalFlowConstraint.ValidateConstraintType(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TPhysicalFlowConstraint.ValidateConstraintType';
var
  lMessage    : string;
  lResult     : Boolean;
begin
  Result := FALSE;
  try
    lMessage := '';
    lResult := FAppModules.FieldProperties.ValidateFieldProperty
                 ('StructureType', IntToStr(FStructureType), lMessage);
    if (NOT lResult) then
      AErrorMessages.Add('Channel ('+IntToStr(FChannelNumber)+')'+ ':'+lMessage)
    else
    begin
      {if (FStructureType in [1, 7, 12]) then
      begin
        lMessage := FAppModules.Language.GetString('ContextValidation.StructureTypesNotSupported');
        AErrorMessages.Add(Format(lMessage, ['Channel ('+IntToStr(FChannelNumber)+')']));
        lResult := FALSE;
      end;}
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPhysicalFlowConstraint.ValidateElevationOfSill(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TPhysicalFlowConstraint.ValidateElevationOfSill';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    if (FStructureType in [2, 3]) then
    begin
      lMessage := '';
      Result   := FAppModules.FieldProperties.ValidateFieldProperty
                    ('SillElevation', FloatToStr(FElevationOfSill), lMessage);
      if (NOT Result) then
        AErrorMessages.Add(lMessage)
    end
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPhysicalFlowConstraint.ValidateMaximumGateHeight(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TPhysicalFlowConstraint.ValidateMaximumGateHeight';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    if (FStructureType in [2, 3, 10]) then
    begin
      lMessage := '';
      Result   := FAppModules.FieldProperties.ValidateFieldProperty
                    ('GateHeight', FloatToStr(FMaximumGateHeight), lMessage);
      if ((FStructureType in [2,3])and Result) then
      begin
        lMessage := '';
        if (FMaximumGateHeight <= FElevationOfSill) then
        begin
          lMessage := FAppModules.Language.GetString('ContextValidation.HOSLNotGreaterThanSILL');
          Result := False;
        end;
      end;
      if (NOT Result) then
        AErrorMessages.Add(lMessage)
    end
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPhysicalFlowConstraint.ValidateDischargeCoefficient(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TPhysicalFlowConstraint.ValidateDischargeCoefficient';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    if (FStructureType in [2, 3]) then
    begin
      lMessage := '';
      Result   := FAppModules.FieldProperties.ValidateFieldProperty
                    ('DischargeCoefficient', FloatToStr(FDischargeCoefficient), lMessage);
      if (NOT Result) then
        AErrorMessages.Add(lMessage)
    end
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPhysicalFlowConstraint.ValidateStructureLength(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TPhysicalFlowConstraint.ValidateStructureLength';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    if (FStructureType in [2, 3, 6]) then
    begin
      lMessage := '';
      Result   := FAppModules.FieldProperties.ValidateFieldProperty
                    ('ControlStructureLength', FloatToStr(FStructureLength), lMessage);
      if (NOT Result) then
        AErrorMessages.Add(lMessage)
    end
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPhysicalFlowConstraint.ValidateWaterLevelAtDownstreamNode(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TPhysicalFlowConstraint.ValidateWaterLevelAtDownstreamNode';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    if (FStructureType in [13]) then
    begin
      lMessage := '';
      Result   := FAppModules.FieldProperties.ValidateFieldProperty
                    ('WaterLevelAtDownstreamNode', FloatToStr(FStructureLength), lMessage);
      if (NOT Result) then
        AErrorMessages.Add(lMessage)
    end
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPhysicalFlowConstraint.ValidateReferenceElevation(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TPhysicalFlowConstraint.ValidateReferenceElevation';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    if (FStructureType in [12]) then
    begin
      lMessage := '';
      Result   := FAppModules.FieldProperties.ValidateFieldProperty
                    ('ReferenceElevation', FloatToStr(FReferenceElevation), lMessage);
      if (NOT Result) then
        AErrorMessages.Add(lMessage)
    end
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPhysicalFlowConstraint.GetKeyValues (const AParamField : WideString;
                                               const AFieldIndex : WideString) : WideString;
const OPNAME = 'TPhysicalFlowConstraint.GetKeyValues';
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
    if (AParamField = 'ConstraintElevation') OR
       (AParamField = 'ConstraintDischarge') OR
       (AParamField = 'ConstraintChannelNumber') OR
       (AParamField = 'ConstraintKFactor') OR
       (AParamField = 'ConstraintHeadDifferences') OR
       (AParamField = 'ConstraintAquiferFlows') OR
       (AParamField = 'ConstraintDownStreamNodeInflows') OR
       (AParamField = 'ConstraintRiverDepths') OR
       (AParamField = 'ConstraintElevationDifferences') OR
       (AParamField = 'ConstraintMonthlyAverageInflows') OR
       (AParamField = 'ConstraintMonthlyAverageDivertedFlow') OR
       (AParamField = 'ConstraintPumpingHeads') OR
       (AParamField = 'ConstraintPumpingDischarges') then
    begin
      FAppModules.Changes.GetIndexes(AFieldIndex, lDim1Idx, lDim2Idx);
      Result := Result + ',LineNumber=' + IntToStr(lDim1Idx);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{******************************************************************************}
{ TPhysicalFlowConstraintList                                                  }
{******************************************************************************}

function TPhysicalFlowConstraintList._AddRef: Integer;
const OPNAME = 'TPhysicalFlowConstraintList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraintList._Release: Integer;
const OPNAME = 'TPhysicalFlowConstraintList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPhysicalFlowConstraintList.CreateMemberObjects;
const OPNAME = 'TPhysicalFlowConstraintList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPhysicalFlowConstraintList := TObjectList.Create;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPhysicalFlowConstraintList.DestroyMemberObjects;
const OPNAME = 'TPhysicalFlowConstraintList.DestroyMemberObjects';
begin
  try
    FreeAndNil(FPhysicalFlowConstraintList);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraintList.Initialise: boolean;
const OPNAME = 'TPhysicalFlowConstraintList.Initialise';
begin
  Result := inherited Initialise;
  try
    FPhysicalFlowConstraintList.Clear;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraintList.NewPhysicalFlowConstraint : TPhysicalFlowConstraint;
const OPNAME = 'TPhysicalFlowConstraintList.NewPhysicalFlowConstraint';
var
  lFeature : TPhysicalFlowConstraint;
begin
  Result := nil;
  try
    lFeature := TPhysicalFlowConstraint.Create(FAppModules);
    FPhysicalFlowConstraintList.Add(lFeature);
    Result := lFeature;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraintList.CreatePhysicalFlowConstraint(AChannelNumber : integer) : IPhysicalFlowConstraint;
const OPNAME = 'TPhysicalFlowConstraintList.CreatePhysicalFlowConstraint';
var
  LFeatureID : integer;
  LFeatureName: string;
  LLoadAgent : TNetworkFeaturesSQLAgent;
  LFeature   : TPhysicalFlowConstraint;
begin
  Result := nil;
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LFeature := NewPhysicalFlowConstraint;
      try
        LFeatureID   := LLoadAgent.GetMaxPhysicalFlowConstraintID + 1;
        LFeatureName := UpperCase(FAppModules.Language.GetString('NetworkFeatures.PhysicalFlowConstraint')) +
                        ' ' + IntToStr(LFeatureID);
        LFeature.Initialise;
        LFeature.Populate(LFeatureID, LFeatureName,AChannelNumber,0,0,2,0.0,0.0,0.0,0.0,0,0.0,0.0);
        if not LLoadAgent.InsertPhysicalFlowConstraint(LFeature) then
          FPhysicalFlowConstraintList.Delete(FPhysicalFlowConstraintList.IndexOf(LFeature))
        else
          Result := LFeature;
      except
        FPhysicalFlowConstraintList.Delete(FPhysicalFlowConstraintList.IndexOf(LFeature));
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraintList.RemovePhysicalFlowConstraintWithID (AFeatureID : integer) : WordBool;
const OPNAME = 'TPhysicalFlowConstraintList.RemovePhysicalFlowConstraintWithID';
var
  lLoadAgent : TNetworkFeaturesSQLAgent;
  LFeature   : TPhysicalFlowConstraint;
begin
  Result := FALSE;
  try
    LFeature := CastPhysicalFlowConstraintByID(AFeatureID);
    if (LFeature <> nil) then
    begin
      LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
      try
        if LLoadAgent.DeletePhysicalFlowConstraint(LFeature) then
        begin
          if(FPhysicalFlowConstraintList.IndexOf(LFeature) >= 0) then
            FPhysicalFlowConstraintList.Delete(FPhysicalFlowConstraintList.IndexOf(LFeature));
          Result := TRUE;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraintList.Get_PhysicalFlowConstraintByIndex(AIndex: integer): IPhysicalFlowConstraint;
const OPNAME = 'TPhysicalFlowConstraintList.Get_PhysicalFlowConstraintByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FPhysicalFlowConstraintList.Count) then
      Result := TPhysicalFlowConstraint(FPhysicalFlowConstraintList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraintList.Get_PhysicalFlowConstraintByID(AFeatureID : integer): IPhysicalFlowConstraint;
const OPNAME = 'TPhysicalFlowConstraintList.Get_PhysicalFlowConstraintByID';
var
 lIndex   : integer;
 lFeature : TPhysicalFlowConstraint;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FPhysicalFlowConstraintList.Count)) do
    begin
      lFeature := TPhysicalFlowConstraint(FPhysicalFlowConstraintList.Items[lIndex]);
      if (lFeature.FeatureID = AFeatureID) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraintList.CastPhysicalFlowConstraintByID(AFeatureID : integer): TPhysicalFlowConstraint;
const OPNAME = 'TPhysicalFlowConstraintList.CastPhysicalFlowConstraintByID';
var
 lIndex   : integer;
 lFeature : TPhysicalFlowConstraint;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FPhysicalFlowConstraintList.Count)) do
    begin
      lFeature := TPhysicalFlowConstraint(FPhysicalFlowConstraintList.Items[lIndex]);
      if (lFeature.FeatureID = AFeatureID) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraintList.CastPhysicalFlowConstraintByChannelNr(AChannelNr : integer): TPhysicalFlowConstraint;
const OPNAME = 'TPhysicalFlowConstraintList.CastPhysicalFlowConstraintByChannelNr';
var
 lIndex   : integer;
 lFeature : TPhysicalFlowConstraint;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FPhysicalFlowConstraintList.Count)) do
    begin
      lFeature := TPhysicalFlowConstraint(FPhysicalFlowConstraintList.Items[lIndex]);
      if (lFeature.FChannelNumber = AChannelNr) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TPhysicalFlowConstraintList.CastPhysicalFlowConstraintByIndex(AIndex: integer): TPhysicalFlowConstraint;
const OPNAME = 'TPhysicalFlowConstraintList.CastPhysicalFlowConstraintByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FPhysicalFlowConstraintList.Count) then
      Result := TPhysicalFlowConstraint(FPhysicalFlowConstraintList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraintList.Get_PhysicalFlowConstraintCount: integer;
const OPNAME = 'TPhysicalFlowConstraintList.Get_PhysicalFlowConstraintCount';
begin
  Result := 0;
  try
    Result := FPhysicalFlowConstraintList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraintList.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TPhysicalFlowConstraintList.Validate';
var
  lStopOnFirstError : Boolean;
  LIndex            : integer;
begin
  Result := FALSE;
  try
    Result := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    for LIndex := 0 to FPhysicalFlowConstraintList.Count - 1 do
    begin
      if (NOT PhysicalFlowConstraintByIndex[LIndex].Validate(AErrors,AContext)) then
        Result := FALSE;
      if ((NOT Result) AND lStopOnFirstError) then
        Break;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraint.Get_DischargeCurve: IDischargeCurve;
const OPNAME = 'TPhysicalFlowConstraint.Get_DischargeCurve';
begin
  Result := nil;
  try
    Result := FDischargeCurve;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraint.Get_KFactors: IKFactors;
const OPNAME = 'TPhysicalFlowConstraint.Get_KFactors';
begin
  Result := nil;
  try
    Result := FKFactors;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraint.Get_PumpStation: IPumpStation;
const OPNAME = 'TPhysicalFlowConstraint.Get_PumpStation';
begin
  Result := nil;
  try
    Result := FPumpStation;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraint.Get_SandAquifer: ISandAquifer;
const OPNAME = 'TPhysicalFlowConstraint.Get_SandAquifer';
begin
  Result := nil;
  try
    Result := FSandAquifer;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraint.Get_SubmergedOutlet: ISubmergedOutlet;
const OPNAME = 'TPhysicalFlowConstraint.Get_SubmergedOutlet';
begin
  Result := nil;
  try
    Result := FSubmergedOutlet;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraint.Get_ChannelNumber: Integer;
const OPNAME = 'TPhysicalFlowConstraint.Get_ChannelNumber';
begin
  Result := FChannelNumber;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPhysicalFlowConstraint.Set_DownstreamReservoirNr(Value: Integer);
const OPNAME = 'TPhysicalFlowConstraint.Set_DownstreamReservoirNr';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  lNewValue    : integer;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        lNewValue := Value;
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'DownStreamReservoirNumber', IntToStr(lNewValue), IntToStr(FDownstreamReservoirNr), LContextData) then
        begin
          FDownstreamReservoirNr := lNewValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'DownStreamReservoirNumber',
            IntToStr(FDownstreamReservoirNr),IntToStr(lNewValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPhysicalFlowConstraint.Set_UpstreamReservoirNr(Value: Integer);
const OPNAME = 'TPhysicalFlowConstraint.Set_UpstreamReservoirNr';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  lNewValue    : integer;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        lNewValue := Value;
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'UpStreamReservoirNumber', IntToStr(lNewValue), IntToStr(FUpstreamReservoirNr), LContextData) then
        begin
          FUpstreamReservoirNr := lNewValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'UpStreamReservoirNumber',
            IntToStr(FUpstreamReservoirNr),IntToStr(lNewValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPhysicalFlowConstraint.Assign(AChannelNumber: integer;APhysicalFlowConstraint: TPhysicalFlowConstraint);
const OPNAME = 'TPhysicalFlowConstraint.Assign';
begin
  try
    if (APhysicalFlowConstraint <> nil) and (AChannelNumber > 0) then
    begin
      FChannelNumber := APhysicalFlowConstraint.ChannelNumber;
      FeatureName := 'Copy of '+APhysicalFlowConstraint.FeatureName;
      Set_UpstreamReservoirNr(APhysicalFlowConstraint.UpstreamReservoirNr);
      Set_DownstreamReservoirNr(APhysicalFlowConstraint.DownstreamReservoirNr);
      StructureType := APhysicalFlowConstraint.StructureType;
      ElevationOfSill := APhysicalFlowConstraint.ElevationOfSill;
      MaximumGateHeight := APhysicalFlowConstraint.MaximumGateHeight;
      DischargeCoefficient := APhysicalFlowConstraint.DischargeCoefficient;
      StructureLength := APhysicalFlowConstraint.StructureLength;
      NrOfPoints := APhysicalFlowConstraint.NrOfPoints;
      CreateStructurePerType;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TPumpStation }
function TPumpStation._AddRef: Integer;
const OPNAME = 'TPumpStation._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPumpStation._Release: Integer;
const OPNAME = 'TPumpStation._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPumpStation.CreateMemberObjects;
const OPNAME = 'TPumpStation.CreateMemberObjects';
var
  LFieldProperty : TAbstractFieldProperty;
begin
  inherited;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintPumpingHeads');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintPumpingHeads) not found in field properties');
    SetLength(FPumpingHeads,LFieldProperty.ArrayLength);

    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintPumpingDischarges');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintPumpingDischarges) not found in field properties');
    SetLength(FPumpingDischarges,LFieldProperty.ArrayLength);
    Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPumpStation.DestroyMemberObjects;
const OPNAME = 'TPumpStation.DestroyMemberObjects';
begin
  inherited;
  try
    Finalize(FPumpingHeads);
    Finalize(FPumpingDischarges);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPumpStation.Initialise: boolean;
const OPNAME = 'TPumpStation.Initialise';
var
  LIndex: integer;
begin
  Result := False;
  try
    FFeatureID     := NullInteger;
    FChannelNumber := NullInteger;
    for LIndex := Low(FPumpingHeads) to High(FPumpingHeads) do
      FPumpingHeads[LIndex] := NullFloat;
    for LIndex := Low(FPumpingDischarges) to High(FPumpingDischarges) do
      FPumpingDischarges[LIndex] := NullFloat;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPumpStation.InitialiseArrays(APointsCount: integer): boolean;
const OPNAME = 'TPumpStation.InitialiseArrays';
var
  LIndex: integer;
  LFieldProperty : TAbstractFieldProperty;
begin
  Result := False;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintPumpingHeads');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintPumpingHeads) not found in field properties');
    for LIndex := LFieldProperty.ArrayLow to APointsCount do
    begin
      FPumpingHeads[LIndex] := 0.0;
      FPumpingDischarges[LIndex] := 0.0;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPumpStation.UpdatePointsCount(APointsCount: integer): boolean;
const OPNAME = 'TPumpStation.UpdatePointsCount';
var
  LIndex: integer;
  LFieldProperty : TAbstractFieldProperty;
begin
  Result := False;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintPumpingHeads');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintPumpingHeads) not found in field properties');
    for LIndex := LFieldProperty.ArrayLow to APointsCount do
    begin
      if(FPumpingHeads[LIndex] = NullFloat) then
        FPumpingHeads[LIndex] := 0.0;
      if(FPumpingDischarges[LIndex] = NullFloat) then
        FPumpingDischarges[LIndex] := 0.0;
    end;
    for LIndex := APointsCount+1 to LFieldProperty.ArrayHigh do
    begin
      FPumpingHeads[LIndex] := NullFloat;
      FPumpingDischarges[LIndex] := NullFloat;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPumpStation.Get_CountNrOfPoints: Integer;
const OPNAME = 'TPumpStation.Get_CountNrOfPoints';
var
  LIndex: integer;
  LFieldProperty : TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintPumpingHeads');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintPumpingHeads) not found in field properties');
    for LIndex := LFieldProperty.ArrayLow to LFieldProperty.ArrayHigh do
    begin
      if(FPumpingHeads[LIndex] = NullFloat) then
        Break
      else
        Result := Result + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPumpStation.Get_PumpingDischargeByIndex(AIndex: Integer): Double;
const OPNAME = 'TPumpStation.Get_PumpingDischargeByIndex';
begin
  Result := NullFloat;
  try
    if(AIndex >= Low(FPumpingDischarges)) and (AIndex <= High(FPumpingDischarges)) then
      Result := FPumpingDischarges[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPumpStation.Get_PumpingHeadByIndex(AIndex: Integer): Double;
const OPNAME = 'TPumpStation.Get_PumpingHeadByIndex';
begin
  Result := NullFloat;
  try
    if(AIndex >= Low(FPumpingHeads)) and (AIndex <= High(FPumpingHeads)) then
      Result := FPumpingHeads[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPumpStation.Set_PumpingHeadByIndex(AIndex: Integer; AValue: Double);
const OPNAME = 'TPumpStation.Set_PumpingHeadByIndex';
var
  LIndex : integer;
  LFieldProperty : TAbstractFieldProperty;
begin
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintPumpingHeads');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintPumpingHeads) not found in field properties');
    if (AIndex >= LFieldProperty.ArrayLow) and (AIndex <= LFieldProperty.ArrayHigh) then
    begin
      if (FPumpingHeads[AIndex] <> AValue) then
      begin
        if (AValue = NullFloat) then
        begin
          for LIndex := AIndex+1 to LFieldProperty.ArrayHigh do
          begin
            WritePumpingHeadToDB (LIndex-1, FPumpingHeads[LIndex]);
            WritePumpingDischargeToDB (LIndex-1, FPumpingDischarges[LIndex]);
          end;
          WritePumpingHeadToDB (High(FPumpingHeads), NullFloat);
          WritePumpingDischargeToDB (High(FPumpingDischarges), NullFloat);
        end
        else
        begin
          for LIndex := LFieldProperty.ArrayLow to AIndex-1 do
          begin
            if (FPumpingHeads[LIndex] = NullFloat) then
              WritePumpingHeadToDB(LIndex, 0.0);
            if (FPumpingDischarges[LIndex] = NullFloat) then
              WritePumpingDischargeToDB(LIndex, 0.0);
          end;
          WritePumpingHeadToDB(AIndex, AValue);
          if (FPumpingDischarges[AIndex] = NullFloat) then
            WritePumpingDischargeToDB (AIndex, 0.0);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPumpStation.Set_PumpingDischargeByIndex(AIndex: Integer; AValue: Double);
const OPNAME = 'TPumpStation.Set_PumpingDischargeByIndex';
var
  LIndex: integer;
  LFieldProperty : TAbstractFieldProperty;
begin
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintPumpingHeads');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintPumpingHeads) not found in field properties');
    if (AIndex >= LFieldProperty.ArrayLow) and (AIndex <= LFieldProperty.ArrayHigh) then
    begin
      if (FPumpingDischarges[AIndex] <> AValue) then
      begin
        if (AValue = NullFloat) then
        begin
          for LIndex := AIndex+1 to LFieldProperty.ArrayHigh do
          begin
            WritePumpingHeadToDB (LIndex-1, FPumpingHeads[LIndex]);
            WritePumpingDischargeToDB (LIndex-1, FPumpingDischarges[LIndex]);
          end;
          WritePumpingHeadToDB (High(FPumpingHeads), NullFloat);
          WritePumpingDischargeToDB (High(FPumpingDischarges), NullFloat);
        end
        else
        begin
          for LIndex := LFieldProperty.ArrayLow to AIndex-1 do
          begin
            if (FPumpingHeads[LIndex] = NullFloat) then
              WritePumpingHeadToDB(LIndex, 0.0);
            if (FPumpingDischarges[LIndex] = NullFloat) then
              WritePumpingDischargeToDB(LIndex, 0.0);
          end;
          WritePumpingDischargeToDB(AIndex, AValue);
          if (FPumpingHeads[AIndex] = NullFloat) then
            WritePumpingHeadToDB (AIndex, 0.0);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPumpStation.WritePumpingHeadToDB (AIndex : integer; AValue : double);
const OPNAME = 'TPumpStation.WritePumpingHeadToDB';
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
        LLoadAgent.LoadContextData_PhysicalFlowConstraint(LContextData, FFeatureID,pfcgPumpStation,pfcstPumpingHead,0,AIndex);
        if (AValue = NullFloat) then
          lNewValue := ''
        else
          lNewValue := FloatToStr(AValue);
        if (FPumpingHeads[AIndex] = NullFloat) then
          lOldValue := ''
        else
          lOldValue := FloatToStr(FPumpingHeads[AIndex]);
        if FAppModules.FieldProperties.UpdateFieldValue(
           'ConstraintPumpingHeads', lNewValue, lOldValue, LContextData) then
        begin
          LPrevValue     := FPumpingHeads[AIndex];
          FPumpingHeads[AIndex] := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ConstraintPumpingHeads',FloatToStr(LPrevValue),FloatToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPumpStation.WritePumpingDischargeToDB (AIndex : integer;AValue : double);
const OPNAME = 'TPumpStation.WritePumpingDischargeToDB';
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
        LLoadAgent.LoadContextData_PhysicalFlowConstraint(LContextData, FFeatureID,pfcgPumpStation,pfcstPumpingDischarge,0,AIndex);
        if (AValue = NullFloat) then
          lNewValue := ''
        else
          lNewValue := FloatToStr(AValue);
        if (FPumpingDischarges[AIndex] = NullFloat) then
          lOldValue := ''
        else
          lOldValue := FloatToStr(FPumpingDischarges[AIndex]);
        if FAppModules.FieldProperties.UpdateFieldValue(
           'ConstraintPumpingDischarges', lNewValue, lOldValue, LContextData) then
        begin
          LPrevValue := FPumpingDischarges[AIndex];
          FPumpingDischarges[AIndex] := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ConstraintPumpingDischarges',FloatToStr(LPrevValue),FloatToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TPumpStation.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TPumpStation.Validate';
var
  LErrorList : TStringList;
  lStopOnFirstError: boolean;
begin
  Result := False;
  try
    lErrorList := TStringList.Create;
    try
      lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
      if (AContext = 'ConstraintPumpingHead') then
        Result := ValidatePumpingHeads(LErrorList)
      else
      if (AContext = 'ConstraintPumpingDischarge') then
        Result := ValidatePumpingDischarges(LErrorList)
      else
      begin
        Result := ValidatePumpingHeads(lErrorList);
        if (Result OR (NOT lStopOnFirstError)) then
          Result := Result and  ValidatePumpingDischarges(LErrorList);
      end;
      AErrors := AErrors + lErrorList.CommaText;
    finally
      FreeAndNil(lErrorList);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPumpStation.Populate(AFeatureID,AChannelNumber : integer; APumpingHeads, ADischarges: TOneDimensionDoubleArray): WordBool;
const OPNAME = 'TPumpStation.Populate';
var
  LIndex: integer;
begin
  Result := False;
  try
    FFeatureID     := AFeatureID;
    FChannelNumber := AChannelNumber;
    for LIndex := Low(APumpingHeads) to High(APumpingHeads) do
    begin
      if(LIndex > High(FPumpingHeads)) then Break;
      FPumpingHeads[LIndex] := APumpingHeads[LIndex];
    end;
    for LIndex := Low(ADischarges) to High(ADischarges) do
    begin
      if(LIndex > High(FPumpingDischarges)) then Break;
      FPumpingDischarges[LIndex] := ADischarges[LIndex];
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPumpStation.ValidatePumpingHeads(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TPumpStation.ValidatePumpingHeads';
var
  lMessage          : string;
  lStopOnFirstError : Boolean;
  lValid            : Boolean;
  lResult           : Boolean;
  lIndex            : integer;
  lContinue         : Boolean;
  lCount            : integer;
  LElevations: TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    LElevations := FAppModules.FieldProperties.FieldProperty('ConstraintPumpingHeads');
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    lIndex    := LElevations.ArrayLow;
    lContinue := TRUE;
    lCount    := 0;
    while (lContinue AND (lIndex  <= LElevations.ArrayHigh)) do
    begin
      if ((FPumpingHeads[lIndex] = NullFloat) AND
          (FPumpingDischarges[lIndex] = NullFloat)) then
        lContinue := FALSE
      else
      begin
        lMessage := '';
        lCount  := lCount + 1;
        lValid := FAppModules.FieldProperties.ValidateFieldProperty
                    ('ConstraintPumpingHeads', FloatToStr(FPumpingHeads[lIndex]),
                    lMessage, lIndex);
        if (NOT lValid) then
        begin
          lResult := FALSE;
          AErrorMessages.Add(lMessage);
          if (lStopOnFirstError) then
            Break;
        end;
        if (lResult OR (NOT lStopOnFirstError)) then
        begin
          if ((FPumpingHeads[lIndex] = NullFloat) AND
              (FPumpingDischarges[lIndex] <> NullFloat)) then
          begin
            lMessage := FAppModules.Language.GetString('ContextValidation.UnequalNrOfArrayValues');
            AErrorMessages.Add(Format(lMessage, ['Channel ('+IntToStr(FChannelNumber)+')']));
            lResult := FALSE;
          end;
        end;
        lIndex := lIndex + 1;
      end;
    end;
    if (lResult OR (NOT lStopOnFirstError)) then
    begin
      if (lCount < 1) then
      begin
        lResult := FALSE;
        lMessage := FAppModules.Language.GetString('ContextValidation.NotEnoughArrayValues');
        AErrorMessages.Add(Format(lMessage, ['Channel ('+IntToStr(FChannelNumber)+')']));
      end;
    end;

    if (lResult OR (NOT lStopOnFirstError)) then
    begin
      lValid := TRUE;
      lIndex := LElevations.ArrayLow;
      while (lValid AND(lIndex < LElevations.ArrayHigh)) do
      begin
        if ((FPumpingHeads[lIndex+1] <> NullFloat) AND
            (FPumpingHeads[lIndex] > FPumpingHeads[lIndex+1])) then
        begin
          lMessage := FAppModules.Language.GetString('ContextValidation.ElevationRangesNotInDescendingOrder');
          AErrorMessages.Add(Format(lMessage, [IntToStr(FFeatureID)]));
          lValid := FALSE;
        end
        else
          lIndex := lIndex + 1;
      end;
      if (NOT lValid) then
        lResult := FALSE;
    end;

    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPumpStation.ValidatePumpingDischarges(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TPumpStation.ValidatePumpingDischarges';
var
  lMessage          : string;
  lStopOnFirstError : Boolean;
  lValid            : Boolean;
  lResult           : Boolean;
  lIndex            : integer;
  lContinue         : Boolean;
  lCount            : integer;
  LElevations       : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    LElevations := FAppModules.FieldProperties.FieldProperty('ConstraintPumpingDischarges');
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    lIndex    := LElevations.ArrayLow;
    lContinue := TRUE;
    lCount    := 0;
    while (lContinue AND (lIndex  <= LElevations.ArrayHigh)) do
    begin
      if ((FPumpingHeads[lIndex] = NullFloat) AND
          (FPumpingDischarges[lIndex] = NullFloat)) then
        lContinue := FALSE
      else
      begin
        lMessage := '';
        lCount  := lCount + 1;
        lValid := FAppModules.FieldProperties.ValidateFieldProperty
                    ('ConstraintPumpingDischarges', FloatToStr(FPumpingDischarges[lIndex]),
                    lMessage, lIndex);
        if (NOT lValid) then
        begin
          lResult := FALSE;
          AErrorMessages.Add(lMessage);
          if (lStopOnFirstError) then
            Break;
        end;
        if (lResult OR (NOT lStopOnFirstError)) then
        begin
          if ((FPumpingHeads[lIndex] <> NullFloat) AND
              (FPumpingDischarges[lIndex] = NullFloat)) then
          begin
            lMessage := FAppModules.Language.GetString('ContextValidation.UnequalNrOfArrayValues');
            AErrorMessages.Add(Format(lMessage, ['Channel ('+IntToStr(FChannelNumber)+')']));
            lResult := FALSE;
          end;
        end;
        lIndex := lIndex + 1;
      end;
    end;
    if (lResult OR (NOT lStopOnFirstError)) then
    begin
      if (lCount < 1) then
      begin
        lResult := FALSE;
        lMessage := FAppModules.Language.GetString('ContextValidation.NotEnoughArrayValues');
        AErrorMessages.Add(Format(lMessage, ['Channel ('+IntToStr(FChannelNumber)+')']));
      end;
    end;

    if (lResult OR (NOT lStopOnFirstError)) then
      begin
        lValid := TRUE;
        lIndex := LElevations.ArrayLow;
        while (lValid AND(lIndex < LElevations.ArrayHigh)) do
        begin
          if ((FPumpingDischarges[lIndex+1] <> NullFloat) AND
              (FPumpingDischarges[lIndex] > FPumpingDischarges[lIndex+1])) then
          begin
            lMessage := FAppModules.Language.GetString('ContextValidation.ElevationRangesNotInDescendingOrder');
            AErrorMessages.Add(Format(lMessage, [IntToStr(FFeatureID)]));
            lValid := FALSE;
          end
          else
            lIndex := lIndex + 1;
        end;
        if (NOT lValid) then
          lResult := FALSE;
      end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TSubmergedOutlet }

function TSubmergedOutlet._AddRef: Integer;
const OPNAME = 'TSubmergedOutlet._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSubmergedOutlet._Release: Integer;
const OPNAME = 'TSubmergedOutlet._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSubmergedOutlet.CreateMemberObjects;
const OPNAME = 'TSubmergedOutlet.CreateMemberObjects';
var
  LFieldProperty : TAbstractFieldProperty;
begin
  inherited;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintElevationDifferences');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintElevationDifferences) not found in field properties');
    SetLength(FElevationDifferences,LFieldProperty.ArrayLength);

    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintMonthlyAverageInflows');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintMonthlyAverageInflows) not found in field properties');
    SetLength(FMonthlyAverageInflows,LFieldProperty.ArrayLength);

    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintMonthlyAverageDivertedFlow');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintMonthlyAverageDivertedFlow) not found in field properties');
    SetLength(FMonthlyAverageDivertedFlow,LFieldProperty.ArrayLength,LFieldProperty.ArrayLength(1));
    Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSubmergedOutlet.DestroyMemberObjects;
const OPNAME = 'TSubmergedOutlet.DestroyMemberObjects';
begin
  inherited;
  try
    Finalize(FElevationDifferences);
    Finalize(FMonthlyAverageInflows);
    Finalize(FMonthlyAverageDivertedFlow);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSubmergedOutlet.Initialise: boolean;
const OPNAME = 'TSubmergedOutlet.Initialise';
var
  LRow,LCol,
  LIndex: integer;
  lField : TAbstractFieldProperty;
begin
  Result := False;
  try
    FFeatureID     := NullInteger;
    FChannelNumber := NullInteger;

    for LIndex := Low(FElevationDifferences) to High(FElevationDifferences) do
      FElevationDifferences[LIndex] := 0.0;
    for LIndex := Low(FMonthlyAverageInflows) to High(FMonthlyAverageInflows) do
      FMonthlyAverageInflows[LIndex] := 0.0;

    lField := FAppModules.FieldProperties.FieldProperty('ConstraintMonthlyAverageDivertedFlow');
    for LRow := lField.ArrayLow to lField.ArrayHigh do
      for LCol := lField.ArrayLowDimTwo to lField.ArrayHighDimTwo do
        FMonthlyAverageDivertedFlow[LRow, LCol] := 0.0;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSubmergedOutlet.InitialiseArrays(APointsCount: integer): boolean;
const OPNAME = 'TSubmergedOutlet.InitialiseArrays';
var
  LRow,LCol,
  LIndex: integer;
begin
  Result := False;
  try
    for LIndex := 0 to APointsCount do
      FElevationDifferences[LIndex] := 0.0;
    for LIndex := 0 to APointsCount do
      FMonthlyAverageInflows[LIndex] := 0.0;

    for LRow := 0 to APointsCount do
      for LCol := 0 to APointsCount do
        FMonthlyAverageDivertedFlow[LRow, LCol] := 0.0;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSubmergedOutlet.UpdatePointsCount( APointsCount: integer): boolean;
const OPNAME = 'TSubmergedOutlet.UpdatePointsCount';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSubmergedOutlet.Get_CountNrOfPoints: Integer;
const OPNAME = 'TSubmergedOutlet.Get_CountNrOfPoints';
var
  LFieldProperty : TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintMonthlyAverageDivertedFlow');
    Result := LFieldProperty.ArrayHigh;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSubmergedOutlet.Get_ElevationDifferenceByIndex(AIndex: Integer): Double;
const OPNAME = 'TSubmergedOutlet.Get_ElevationDifferenceByIndex';
begin
  Result := NullInteger;
  try
    if(AIndex >= Low(FElevationDifferences)) and (AIndex <= High(FElevationDifferences)) then
      Result := FElevationDifferences[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSubmergedOutlet.Get_MonthlyAverageDivertedFlowByIndex(ARow, ACol: Integer): Double;
const OPNAME = 'TSubmergedOutlet.Get_MonthlyAverageDivertedFlowByIndex';
var
  lField : TAbstractFieldProperty;
begin
  Result := NullInteger;
  try
    lField := FAppModules.FieldProperties.FieldProperty('ConstraintMonthlyAverageDivertedFlow');
    if(ARow >= lField.ArrayLow) and (ARow <= lField.ArrayHigh) and
      (ACol >= lField.ArrayLowDimTwo) and (ACol <= lField.ArrayHighDimTwo)then
      Result := FMonthlyAverageDivertedFlow[ARow, ACol];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSubmergedOutlet.Get_MonthlyAverageInflowByIndex(AIndex: Integer): Double;
const OPNAME = 'TSubmergedOutlet.Get_MonthlyAverageInflowByIndex';
begin
  Result := NullInteger;
  try
    if(AIndex >= Low(FMonthlyAverageInflows)) and (AIndex <= High(FMonthlyAverageInflows)) then
      Result := FMonthlyAverageInflows[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSubmergedOutlet.Set_ElevationDifferenceByIndex(AIndex: Integer;AValue: Double);
const OPNAME = 'TSubmergedOutlet.Set_ElevationDifferenceByIndex';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  lOldValue    : string;
  lNewValue    : string;
  LPrevValue   : double;
  LFieldProperty : TAbstractFieldProperty;
begin
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintElevationDifferences');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintElevationDifferences) not found in field properties');
    if (AIndex >= LFieldProperty.ArrayLow) and (AIndex <= LFieldProperty.ArrayHigh) then
    begin
      LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_PhysicalFlowConstraint(LContextData, FFeatureID,pfcgSubmergedOutlet,pfcstElevationDifference,0,AIndex);
          if (AValue = NullFloat) then
            lNewValue := ''
          else
            lNewValue := FloatToStr(AValue);
          if (FElevationDifferences[AIndex] = NullFloat) then
            lOldValue := ''
          else
            lOldValue := FloatToStr(FElevationDifferences[AIndex]);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'ConstraintHeadDifferences', lNewValue, lOldValue, LContextData) then
          begin
            LPrevValue     := FElevationDifferences[AIndex];
            FElevationDifferences[AIndex] := AValue;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'ConstraintHeadDifferences',FloatToStr(LPrevValue),FloatToStr(AValue));
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

procedure TSubmergedOutlet.Set_MonthlyAverageInflowByIndex(AIndex: Integer; AValue: Double);
const OPNAME = 'TSubmergedOutlet.Set_MonthlyAverageInflowByIndex';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  lOldValue    : string;
  lNewValue    : string;
  LPrevValue   : double;
  LFieldProperty : TAbstractFieldProperty;
begin
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintMonthlyAverageInflows');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintMonthlyAverageInflows) not found in field properties');
    if (AIndex >= LFieldProperty.ArrayLow) and (AIndex <= LFieldProperty.ArrayHigh) then
    begin
      LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_PhysicalFlowConstraint(LContextData, FFeatureID,pfcgSubmergedOutlet,pfcstMonthlyAverageInflow,0,AIndex);
          if (AValue = NullFloat) then
            lNewValue := ''
          else
            lNewValue := FloatToStr(AValue);
          if (FMonthlyAverageInflows[AIndex] = NullFloat) then
            lOldValue := ''
          else
            lOldValue := FloatToStr(FMonthlyAverageInflows[AIndex]);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'ConstraintMonthlyAverageInflows', lNewValue, lOldValue, LContextData) then
          begin
            LPrevValue     := FMonthlyAverageInflows[AIndex];
            FMonthlyAverageInflows[AIndex] := AValue;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'ConstraintMonthlyAverageInflows',FloatToStr(LPrevValue),FloatToStr(AValue));
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

procedure TSubmergedOutlet.Set_MonthlyAverageDivertedFlowByIndex(ARow, ACol: Integer; AValue: Double);
const OPNAME = 'TSubmergedOutlet.Set_MonthlyAverageDivertedFlowByIndex';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  lOldValue    : string;
  lNewValue    : string;
  LPrevValue   : double;
  LFieldProperty : TAbstractFieldProperty;
begin
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintMonthlyAverageDivertedFlow');
    if (LFieldProperty = nil) then
      raise Exception.Create('Field (ConstraintMonthlyAverageDivertedFlow) not found in field properties');
    if (ACol >= LFieldProperty.ArrayLow) and (ACol <= LFieldProperty.ArrayHigh) and
       (ACol >= LFieldProperty.ArrayLowDimTwo) and (ACol <= LFieldProperty.ArrayHighDimTwo) then
    begin
      LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_PhysicalFlowConstraint(LContextData, FFeatureID,pfcgSubmergedOutlet,pfcstMonthlyAverageDivertedFlow,ARow,ACol);
          if (AValue = NullFloat) then
            lNewValue := ''
          else
            lNewValue := FloatToStr(AValue);
          if (FMonthlyAverageDivertedFlow[ARow,ACol] = NullFloat) then
            lOldValue := ''
          else
            lOldValue := FloatToStr(FMonthlyAverageDivertedFlow[ARow,ACol]);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'ConstraintMonthlyAverageDivertedFlow', lNewValue, lOldValue, LContextData) then
          begin
            LPrevValue     := FMonthlyAverageDivertedFlow[ARow,ACol];
            FMonthlyAverageDivertedFlow[ARow,ACol] := AValue;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'ConstraintMonthlyAverageDivertedFlow',FloatToStr(LPrevValue),FloatToStr(AValue));
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

function TSubmergedOutlet.Populate(AFeatureID,AChannelNumber : integer; AElevationDifferences, AMonthlyAverageInflows: TOneDimensionDoubleArray;
             AMonthlyAverageDivertedFlow:TTwoDimensionDoubleArray): WordBool;
const OPNAME = 'TSubmergedOutlet.Populate';
var
  LRow,LCol,
  LIndex: integer;
  lField : TAbstractFieldProperty;
begin
  Result := False;
  try
    FFeatureID     := AFeatureID;
    FChannelNumber := AChannelNumber;
    for LIndex := Low(AElevationDifferences) to High(AElevationDifferences) do
    begin
      if(LIndex > High(FElevationDifferences)) then Break;
      FElevationDifferences[LIndex] := AElevationDifferences[LIndex];
    end;
    for LIndex := Low(AMonthlyAverageInflows) to High(AMonthlyAverageInflows) do
    begin
      if(LIndex > High(FMonthlyAverageInflows)) then Break;
      FMonthlyAverageInflows[LIndex] := AMonthlyAverageInflows[LIndex];
    end;

    lField := FAppModules.FieldProperties.FieldProperty('ConstraintMonthlyAverageDivertedFlow');
    for LRow := lField.ArrayLow to lField.ArrayHigh do
      for LCol := lField.ArrayLowDimTwo to lField.ArrayHighDimTwo do
        FMonthlyAverageDivertedFlow[LRow, LCol] := AMonthlyAverageDivertedFlow[LRow, LCol];
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSubmergedOutlet.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TSubmergedOutlet.Validate';
var
  LErrorList : TStringList;
  lStopOnFirstError: boolean;
begin
  Result := False;
  try
    lErrorList := TStringList.Create;
    try
      lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
      if (AContext = 'ElevationDifference') then
        Result := ValidateElevationDifferences(LErrorList)
      else
      if (AContext = 'MonthlyAverageInflow') then
        Result := ValidateMonthlyAverageInflows(LErrorList)
      else
      if (AContext = 'DownStreamNodeInflows') then
        Result := ValidateDownStreamNodeInflows(LErrorList)
      else
      if (AContext = 'MonthlyAverageDivertedFlow') then
        Result := ValidateMonthlyAverageDivertedFlow(LErrorList)
      else
      begin
        Result := ValidateElevationDifferences(lErrorList);
        if (Result OR (NOT lStopOnFirstError)) then
          Result := Result and  ValidateMonthlyAverageInflows(LErrorList);
        if (Result OR (NOT lStopOnFirstError)) then
          Result := Result and  ValidateDownStreamNodeInflows(LErrorList);
        if (Result OR (NOT lStopOnFirstError)) then
          Result := Result and  ValidateMonthlyAverageDivertedFlow(LErrorList);
      end;
      AErrors := AErrors + lErrorList.CommaText;
    finally
      FreeAndNil(lErrorList);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSubmergedOutlet.ValidateDownStreamNodeInflows(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TSubmergedOutlet.ValidateDownStreamNodeInflows';
begin
  Result := True;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSubmergedOutlet.ValidateElevationDifferences(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TSubmergedOutlet.ValidateElevationDifferences';
begin
  Result := True;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSubmergedOutlet.ValidateMonthlyAverageDivertedFlow(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TSubmergedOutlet.ValidateMonthlyAverageDivertedFlow';
begin
  Result := True;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSubmergedOutlet.ValidateMonthlyAverageInflows(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TSubmergedOutlet.ValidateMonthlyAverageInflows';
begin
  Result := True;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPhysicalFlowConstraintList.CopyPhysicalFlowConstraint(ANewChannelNumber, AOldChannelNumber: integer): IPhysicalFlowConstraint;
const OPNAME = 'TPhysicalFlowConstraintList.CopyPhysicalFlowConstraint';
var
  LPhysicalFlowConstraint : TPhysicalFlowConstraint;
  LNewPhysicalFlowConstraint : IPhysicalFlowConstraint;
  LPhysicalFlowConstraintCopy : TPhysicalFlowConstraint;
begin
  Result := nil;
  try
    LPhysicalFlowConstraint := CastPhysicalFlowConstraintByChannelNr(AOldChannelNumber);
    if (LPhysicalFlowConstraint <> nil) then
    begin
      LNewPhysicalFlowConstraint := CreatePhysicalFlowConstraint(ANewChannelNumber);
      if LNewPhysicalFlowConstraint <> nil then
      begin
        LPhysicalFlowConstraintCopy := CastPhysicalFlowConstraintByID(LNewPhysicalFlowConstraint.FeatureID);
        if LPhysicalFlowConstraintCopy <> nil then
        begin
          LPhysicalFlowConstraintCopy.Assign(ANewChannelNumber,LPhysicalFlowConstraint);
          Result := LPhysicalFlowConstraintCopy;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
