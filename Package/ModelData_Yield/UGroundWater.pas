{******************************************************************************}
{*  UNIT      : Contains the class TGroundWater.                              *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2007/09/17                                                    *}
{*  COPYRIGHT : Copyright © 2007 DWAF                                         *}
{******************************************************************************}

unit UGroundWater;

interface

uses
  Classes,
  Contnrs,
  UAbstractObject,
  VoaimsCom_TLB;

type

{******************************************************************************}
{* GroundWater                                                                *}
{******************************************************************************}

  TGroundWater = class(TAbstractAppObject, IGroundWater)
  protected
    FIdentifier                                : Integer;
    FAquiferNodeNumber                         : Integer;
    FDescription                               : WideString;
    FName                                      : WideString;

    FRefNodeNumber                             : Integer;
    FAquiferInflowChannelNr                    : Integer;
    FAquiferExcessInterflowChannelNr           : Integer;
    FGroundWaterBaseflowChannelNr              : Integer;
    FAbstractionFromAquiferChannelNr           : Integer;
    FAbstractionFromBaseflowChannelNr          : Integer;
    FInflowFromUpstreamAquiferChannelNr        : Integer;
    FOutflowToDownstreamAquiferChannelNr       : Integer;
    FSurfaceRunoffAndSoilInterflowChannelNr    : Integer;
    FGroundWaterBaseFlowRemainderChannelNr     : Integer;
    FGroundWaterAbstractionChannelNr           : Integer;
    FOutflowToNetworkChannelNr                 : Integer;

    FAbstractionNodeNr                         : Integer;
    FCollectionNodeNr                          : Integer;
    FBaseFlowNodeNr                            : Integer;

    FAquiferStorativity                        : Double;
    FAquiferStaticWaterLevel                   : Double;
    FUnsaturatedStorageCapacity                : Double;
    FInitialUnsaturatedStorage                 : Double;
    FMaximumDischargeRate                      : Double;
    FMovingAverageRecharge                     : Double;
    FPitmanSoilMoistureCapacity                : Double;
    FPitmanSoilMoistureStorageCapacity         : Double;
    FPitmansoilMoistureFlowState               : Double;
    FPitmanSoilMoistureFlowEquation            : Double;
    FPitmanMaximumGroundwaterFlow              : Double;
    FPitmanSoilMoistureRechargeEquation        : Double;
    FPitmanGroundwaterFlow                     : Double;
    FMaximumRateOfGroundwaterBaseFlow          : Double;
    FPowerHeadDifferenceBaseFlowEquation       : Double;
    FMaximumHydrologicalGradient               : Double;
    FAquiferTransmissivity                     : Double;
    FBoreHoleDistanceToRiver                   : Double;
    FMaximumGroundwaterAbstraction             : Double;
    FParameterK2                               : Double;
    FParameterK3                               : Double;
    FGroundWaterEvaporationArea                : Double;
    FGroundWaterEvaporation                    : TMonthlyDoubleArray;
    FGroundWaterFactors                        : TMonthlyDoubleArray;

    function ValidateGroundWaterDescription(AErrorMessages: TStrings)              : Boolean;
    function ValidateGroundWaterName(AErrorMessages: TStrings)                     : Boolean;
    function ValidateRefNodeNumber(AErrorMessages: TStrings)                       : Boolean;
    function ValidateAquiferStorativity(AErrorMessages: TStrings)                  : Boolean;
    function ValidateAquiferStaticWaterLevel(AErrorMessages: TStrings)             : Boolean;
    function ValidateUnsaturatedStorageCapacity(AErrorMessages: TStrings)          : Boolean;
    function ValidateInitialUnsaturatedStorage(AErrorMessages: TStrings)           : Boolean;
    function ValidateMaximumDischargeRate(AErrorMessages: TStrings)                : Boolean;
    function ValidateMovingAverageRecharge(AErrorMessages: TStrings)               : Boolean;
    function ValidatePitmanSoilMoistureCapacity(AErrorMessages: TStrings)          : Boolean;
    function ValidatePitmanSoilMoistureStorageCapacity(AErrorMessages: TStrings)   : Boolean;
    function ValidatePitmansoilMoistureFlowState(AErrorMessages: TStrings)         : Boolean;
    function ValidatePitmanSoilMoistureFlowEquation(AErrorMessages: TStrings)      : Boolean;
    function ValidatePitmanMaximumGroundwaterFlow(AErrorMessages: TStrings)        : Boolean;
    function ValidatePitmanSoilMoistureRechargeEquation(AErrorMessages: TStrings)  : Boolean;
    function ValidatePitmanGroundwaterFlow(AErrorMessages: TStrings)               : Boolean;
    function ValidateMaximumRateOfGroundwaterBaseFlow(AErrorMessages: TStrings)    : Boolean;
    function ValidatePowerHeadDifferenceBaseFlowEquation(AErrorMessages: TStrings) : Boolean;
    function ValidateMaximumHydrologicalGradient(AErrorMessages: TStrings)         : Boolean;
    function ValidateAquiferTransmissivity(AErrorMessages: TStrings)               : Boolean;
    function ValidateBoreHoleDistanceToRiver(AErrorMessages: TStrings)             : Boolean;
    function ValidateMaximumGroundwaterAbstraction(AErrorMessages: TStrings)       : Boolean;
    function ValidateParameterK2(AErrorMessages : TStrings)                        : Boolean;
    function ValidateParameterK3(AErrorMessages : TStrings)                        : Boolean;
    function ValidateGroundWaterEvaporationArea(AErrorMessages : TStrings)         : Boolean;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

  public
    procedure Assign(ASource : TGroundWater);virtual;
    
    function Initialise : boolean; override;
    function Get_AquiferStorativity: Double; safecall;
    procedure Set_AquiferStorativity(Value: Double); safecall;
    function Get_AquiferStaticWaterLevel: Double; safecall;
    procedure Set_AquiferStaticWaterLevel(Value: Double); safecall;
    function Get_UnsaturatedStorageCapacity: Double; safecall;
    procedure Set_UnsaturatedStorageCapacity(Value: Double); safecall;
    function Get_InitialUnsaturatedStorage: Double; safecall;
    procedure Set_InitialUnsaturatedStorage(Value: Double); safecall;
    function Get_MaximumDischargeRate: Double; safecall;
    procedure Set_MaximumDischargeRate(Value: Double); safecall;
    function Get_MovingAverageRecharge: Double; safecall;
    procedure Set_MovingAverageRecharge(Value: Double); safecall;
    function Get_PitmanSoilMoistureCapacity: Double; safecall;
    procedure Set_PitmanSoilMoistureCapacity(Value: Double); safecall;
    function Get_PitmanSoilMoistureStorageCapacity: Double; safecall;
    procedure Set_PitmanSoilMoistureStorageCapacity(Value: Double); safecall;
    function Get_PitmansoilMoistureFlowState: Double; safecall;
    procedure Set_PitmansoilMoistureFlowState(Value: Double); safecall;
    function Get_PitmanSoilMoistureFlowEquation: Double; safecall;
    procedure Set_PitmanSoilMoistureFlowEquation(Value: Double); safecall;
    function Get_PitmanMaximumGroundwaterFlow: Double; safecall;
    procedure Set_PitmanMaximumGroundwaterFlow(Value: Double); safecall;
    function Get_PitmanSoilMoistureRechargeEquation: Double; safecall;
    procedure Set_PitmanSoilMoistureRechargeEquation(Value: Double); safecall;
    function Get_PitmanGroundwaterFlow: Double; safecall;
    procedure Set_PitmanGroundwaterFlow(Value: Double); safecall;
    function Get_MaximumRateOfGroundwaterBaseFlow: Double; safecall;
    procedure Set_MaximumRateOfGroundwaterBaseFlow(Value: Double); safecall;
    function Get_PowerHeadDifferenceBaseFlowEquation: Double; safecall;
    procedure Set_PowerHeadDifferenceBaseFlowEquation(Value: Double); safecall;
    function Get_MaximumHydrologicalGradient: Double; safecall;
    procedure Set_MaximumHydrologicalGradient(Value: Double); safecall;
    function Get_AquiferTransmissivity: Double; safecall;
    procedure Set_AquiferTransmissivity(Value: Double); safecall;
    function Get_BoreHoleDistanceToRiver: Double; safecall;
    procedure Set_BoreHoleDistanceToRiver(Value: Double); safecall;
    function Get_MaximumGroundwaterAbstraction: Double; safecall;
    procedure Set_MaximumGroundwaterAbstraction(Value: Double); safecall;
    function Get_ParameterK2: Double; safecall;
    procedure Set_ParameterK2(Value: Double); safecall;
    function Get_ParameterK3: Double; safecall;
    procedure Set_ParameterK3(Value: Double); safecall;
    function Get_GroundWaterEvaporationArea: Double; safecall;
    procedure Set_GroundWaterEvaporationArea(Value: Double); safecall;
    function Get_Identifier: Integer; safecall;
    function Get_Description: WideString; safecall;
    procedure Set_Description(const Value: WideString); safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;

    function Get_AbstractionNode: IReservoirData; safecall;
    function Get_CollectionNode: IReservoirData; safecall;
    function Get_BaseFlowNode: IReservoirData; safecall;
    function Get_AquiferNode: IReservoirData; safecall;

    function Get_AquiferInflowChannel: IGeneralFlowChannel; safecall;
    function Get_AquiferExcessInterflowChannel: IGeneralFlowChannel; safecall;
    function Get_GroundWaterBaseflowChannel: IGeneralFlowChannel; safecall;
    function Get_AbstractionFromAquiferChannel: IGeneralFlowChannel; safecall;
    function Get_AbstractionFromBaseFlowChannel: IGeneralFlowChannel; safecall;
    function Get_InflowFromUpstreamAquiferChannel: IGeneralFlowChannel; safecall;
    function Get_OutflowToDownstreamAquiferChannel: IGeneralFlowChannel; safecall;
    function Get_SurfaceRunoffAndSoilInterflowChannel: IGeneralFlowChannel; safecall;
    function Get_GroundWaterBaseFlowRemainderChannel: IGeneralFlowChannel; safecall;
    function Get_GroundWaterAbstractionChannel: IGeneralFlowChannel; safecall;
    function Get_OutflowToNetworkChannel: IGeneralFlowChannel; safecall;

    function Get_AquiferExcessInterflowChannelNr: Integer; safecall;
    function Get_AquiferInflowChannelNr: Integer; safecall;
    function Get_GroundWaterBaseflowChannelNr: Integer; safecall;
    function Get_AbstractionFromAquiferChannelNr: Integer; safecall;
    function Get_AbstractionFromBaseflowChannelNr: Integer; safecall;
    function Get_InflowFromUpstreamAquiferChannelNr: Integer; safecall;
    function Get_OutflowToDownstreamAquiferChannelNr: Integer; safecall;
    function Get_SurfaceRunoffAndSoilInterflowChannelNr: Integer; safecall;
    function Get_GroundWaterBaseFlowRemainderChannelNr: Integer; safecall;
    function Get_GroundWaterAbstractionChannelNr: Integer; safecall;
    function Get_OutflowToNetworkChannelNr: Integer; safecall;

    function Get_AbstractionNodeNr: Integer; safecall;
    function Get_CollectionNodeNr: Integer; safecall;
    function Get_BaseFlowNodeNr: Integer; safecall;
    function Get_AquiferNodeNr: Integer; safecall;
    function Get_RefNodeNumber: Integer; safecall;
    procedure Set_RefNodeNumber(Value: Integer); safecall;

    function Get_MonthlyWaterEvaporation(AIndex: Integer): Double; safecall;
    procedure Set_MonthlyWaterEvaporation(AIndex: Integer; Value: Double); safecall;
    function Get_MonthlyWaterUsageFactors(AIndex: Integer): Double; safecall;
    procedure Set_MonthlyWaterUsageFactors(AIndex: Integer; Value: Double); safecall;
    procedure Populate(AIdentifier                             : integer;
                       ANodeNumber                             : Integer;
                       ADescription                            : WideString;
                       AName                                   : WideString;
                       AAquiferStorativity                     : Double;
                       AAquiferStaticWaterLevel                : Double;
                       AUnsaturatedStorageCapacity             : Double;
                       AInitialUnsaturatedStorage              : Double;
                       AMaximumDischargeRate                   : Double;
                       AMovingAverageRecharge                  : Double;
                       AMaximumRateOfGroundwaterBaseFlow       : Double;
                       APowerHeadDifferenceBaseFlowEquation    : Double;
                       AMaximumHydrologicalGradient            : Double;
                       AAquiferTransmissivity                  : Double;
                       ABoreHoleDistanceToRiver                : Double;
                       AMaximumGroundwaterAbstraction          : Double;
                       AParameterK2                            : Double;
                       AParameterK3                            : Double;
                       AGroundWaterEvaporationArea             : Double);
    procedure PopulateGroundWaterPitman(APitmanSoilMoistureCapacity             : Double;
                                        APitmanSoilMoistureStorageCapacity      : Double;
                                        APitmansoilMoistureFlowState            : Double;
                                        APitmanSoilMoistureFlowEquation         : Double;
                                        APitmanMaximumGroundwaterFlow           : Double;
                                        APitmanSoilMoistureRechargeEquation     : Double;
                                        APitmanGroundwaterFlow                  : Double);
    procedure PopulateGroundWaterSubCatchment(ARefNodeNumber                             : Integer;
                                              AAquiferInflowChannelNr                    : Integer;
                                              AInflowFromUpstreamAquiferChannelNr        : Integer;
                                              AOutflowToDownstreamAquiferChannelNr       : Integer;
                                              AAquiferExcessInterflowChannelNr           : Integer;
                                              AGroundWaterBaseflowChannelNr              : Integer;
                                              AAbstractionFromAquiferChannelNr           : Integer;
                                              AAbstractionFromBaseflowChannelNr          : Integer;
                                              AGroundWaterBaseFlowRemainderChannelNr     : Integer;
                                              ASurfaceRunoffAndSoilInterflowChannelNr    : Integer;
                                              AGroundWaterAbstractionChannelNr           : Integer;
                                              AOutflowToNetworkChannelNr                 : Integer;
                                              ABaseFlowNodeNr                            : integer;
                                              AAbstractionNodeNr                         : integer;
                                              ACollectionNodeNr                          : Integer);
    procedure PopulateGroundWaterEvaporation(AWaterEvaporation: TMonthlyDoubleArray);
    procedure PopulateGroundWaterFactors(AGroundWaterFactors: TMonthlyDoubleArray);
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;

    property AquiferStorativity: Double read Get_AquiferStorativity write Set_AquiferStorativity;
    property AquiferStaticWaterLevel: Double read Get_AquiferStaticWaterLevel write Set_AquiferStaticWaterLevel;
    property UnsaturatedStorageCapacity: Double read Get_UnsaturatedStorageCapacity write Set_UnsaturatedStorageCapacity;
    property InitialUnsaturatedStorage: Double read Get_InitialUnsaturatedStorage write Set_InitialUnsaturatedStorage;
    property MaximumDischargeRate: Double read Get_MaximumDischargeRate write Set_MaximumDischargeRate;
    property MovingAverageRecharge: Double read Get_MovingAverageRecharge write Set_MovingAverageRecharge;
    property PitmanSoilMoistureCapacity: Double read Get_PitmanSoilMoistureCapacity write Set_PitmanSoilMoistureCapacity;
    property PitmanSoilMoistureStorageCapacity: Double read Get_PitmanSoilMoistureStorageCapacity write Set_PitmanSoilMoistureStorageCapacity;
    property PitmansoilMoistureFlowState: Double read Get_PitmansoilMoistureFlowState write Set_PitmansoilMoistureFlowState;
    property PitmanSoilMoistureFlowEquation: Double read Get_PitmanSoilMoistureFlowEquation write Set_PitmanSoilMoistureFlowEquation;
    property PitmanMaximumGroundwaterFlow: Double read Get_PitmanMaximumGroundwaterFlow write Set_PitmanMaximumGroundwaterFlow;
    property PitmanSoilMoistureRechargeEquation: Double read Get_PitmanSoilMoistureRechargeEquation write Set_PitmanSoilMoistureRechargeEquation;
    property PitmanGroundwaterFlow: Double read Get_PitmanGroundwaterFlow write Set_PitmanGroundwaterFlow;
    property MaximumRateOfGroundwaterBaseFlow: Double read Get_MaximumRateOfGroundwaterBaseFlow write Set_MaximumRateOfGroundwaterBaseFlow;
    property PowerHeadDifferenceBaseFlowEquation: Double read Get_PowerHeadDifferenceBaseFlowEquation write Set_PowerHeadDifferenceBaseFlowEquation;
    property MaximumHydrologicalGradient: Double read Get_MaximumHydrologicalGradient write Set_MaximumHydrologicalGradient;
    property AquiferTransmissivity: Double read Get_AquiferTransmissivity write Set_AquiferTransmissivity;
    property BoreHoleDistanceToRiver: Double read Get_BoreHoleDistanceToRiver write Set_BoreHoleDistanceToRiver;
    property MaximumGroundwaterAbstraction: Double read Get_MaximumGroundwaterAbstraction write Set_MaximumGroundwaterAbstraction;
    property ParameterK2: Double read Get_ParameterK2 write Set_ParameterK2;
    property ParameterK3: Double read Get_ParameterK3 write Set_ParameterK3;
    property GroundWaterEvaporationArea: Double read Get_GroundWaterEvaporationArea write Set_GroundWaterEvaporationArea;
    property MonthlyWaterEvaporation[AIndex: Integer]: Double read Get_MonthlyWaterEvaporation write Set_MonthlyWaterEvaporation;
    property MonthlyWaterUsageFactors[AIndex: Integer]: Double read Get_MonthlyWaterUsageFactors write Set_MonthlyWaterUsageFactors;
    property Identifier: Integer read Get_Identifier;
    property Description: WideString read Get_Description write Set_Description;
    property RefNodeNumber : Integer  read Get_RefNodeNumber  write Set_RefNodeNumber;
    property Name: WideString read Get_Name write Set_Name;

    property AbstractionNode: IReservoirData read Get_AbstractionNode;
    property CollectionNode: IReservoirData read Get_CollectionNode;
    property BaseFlowNode: IReservoirData read Get_BaseFlowNode;

    property AbstractionNodeNr: Integer read Get_AbstractionNodeNr;
    property CollectionNodeNr: Integer read Get_CollectionNodeNr;
    property BaseFlowNodeNr: Integer read Get_BaseFlowNodeNr;
    property AquiferNodeNr: Integer read Get_AquiferNodeNr;

    property AquiferInflowChannel: IGeneralFlowChannel read Get_AquiferInflowChannel;
    property AquiferInflowChannelNr: Integer read Get_AquiferInflowChannelNr;
    property InflowFromUpstreamAquiferChannel: IGeneralFlowChannel read Get_InflowFromUpstreamAquiferChannel;
    property InflowFromUpstreamAquiferChannelNr: Integer read Get_InflowFromUpstreamAquiferChannelNr;
    property OutflowToDownstreamAquiferChannel: IGeneralFlowChannel read Get_OutflowToDownstreamAquiferChannel;
    property OutflowToDownstreamAquiferChannelNr: Integer read Get_OutflowToDownstreamAquiferChannelNr;
    property AquiferExcessInterflowChannel: IGeneralFlowChannel read Get_AquiferExcessInterflowChannel;
    property AquiferExcessInterflowChannelNr: Integer read Get_AquiferExcessInterflowChannelNr;
    property GroundWaterBaseflowChannel: IGeneralFlowChannel read Get_GroundWaterBaseflowChannel;
    property GroundWaterBaseflowChannelNr: Integer read Get_GroundWaterBaseflowChannelNr;
    property AbstractionFromAquiferChannel: IGeneralFlowChannel read Get_AbstractionFromAquiferChannel;
    property AbstractionFromAquiferChannelNr: Integer read Get_AbstractionFromAquiferChannelNr;
    property AbstractionFromBaseFlowChannel: IGeneralFlowChannel read Get_AbstractionFromBaseFlowChannel;
    property AbstractionFromBaseflowChannelNr: Integer read Get_AbstractionFromBaseflowChannelNr;
    property SurfaceRunoffAndSoilInterflowChannel: IGeneralFlowChannel read Get_SurfaceRunoffAndSoilInterflowChannel;
    property SurfaceRunoffAndSoilInterflowChannelNr: Integer read Get_SurfaceRunoffAndSoilInterflowChannelNr;
    property GroundWaterBaseFlowRemainderChannel: IGeneralFlowChannel read Get_GroundWaterBaseFlowRemainderChannel;
    property GroundWaterBaseFlowRemainderChannelNr: Integer read Get_GroundWaterBaseFlowRemainderChannelNr;
    property GroundWaterAbstractionChannel: IGeneralFlowChannel read Get_GroundWaterAbstractionChannel;
    property GroundWaterAbstractionChannelNr: Integer read Get_GroundWaterAbstractionChannelNr;
    property OutflowToNetworkChannel: IGeneralFlowChannel read Get_OutflowToNetworkChannel;
    property OutflowToNetworkChannelNr: Integer read Get_OutflowToNetworkChannelNr;
  end;

  TGroundWaterList = class(TAbstractAppObject, IGroundWaterList)
  protected
    FGroundWaterList  : TObjectList;
    FGroundWaterCount : integer;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function GetCastGroundWaterByIndex(AIndex : Integer): TGroundWater;
  public
    function Initialise : boolean; override;
    function NewGroundWater : TGroundWater;
    function CreateNewGroundWater: TGroundWater;
    function DeleteGroundWaterWithID(AGroundWaterID : integer) : WordBool;
    function DeleteGroundWaterWithIndex(AIndex : integer) : WordBool;
    function Get_CastGroundWaterByID(AGroundWaterID: integer): TGroundWater;
    function CastGroundWaterByAquiferNodeNumber(AquiferNodeNr: Integer): TGroundWater;
    function Get_CastGroundWaterByIndex(AIndex: Integer): TGroundWater;

    function Get_GroundWaterCount: Integer; safecall;
    function Get_GroundWaterByIndex(AIndex: Integer): IGroundWater; safecall;
    function Get_GroundWaterByID(AGroundWaterID: Integer): IGroundWater; safecall;
    function Get_GroundWaterByNodeNumber(ANodeNumber: Integer): IGroundWater; safecall;
    function CreateGroundWater: IGroundWater; safecall;
    function RemoveGroundWater(AGroundWaterID: Integer): WordBool; safecall;
    function Get_GroundWaterByBaseFlowNumber(ABaseFlowNumber: Integer): IGroundWater; safecall;
    function Get_GroundWaterByAbstractionNodeNumber(AAbstractionNodeNumber: Integer): IGroundWater; safecall;
    function Get_GroundWaterByCollectionNodeNumber(ACollectionNodeNumber: Integer): IGroundWater; safecall;
    function CopyGroundwater(AGroundwaterID: Integer): IGroundWater; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;

    property GroundWaterCount: Integer read Get_GroundWaterCount;
    property GroundWaterByIndex[AIndex: Integer]: IGroundWater read Get_GroundWaterByIndex;
    property GroundWaterByID[AGroundWaterID: Integer]: IGroundWater read Get_GroundWaterByID;
    property GroundWaterByNodeNumber[ANodeNumber: Integer]: IGroundWater read Get_GroundWaterByNodeNumber;
    property CastGroundWaterByIndex[AIndex: Integer]: TGroundWater read Get_CastGroundWaterByIndex;
    property CastGroundWaterByID[AGroundWaterID: Integer]: TGroundWater read Get_CastGroundWaterByID;
    property GroundWaterByBaseFlowNumber[ABaseFlowNumber: Integer]: IGroundWater read Get_GroundWaterByBaseFlowNumber;
    property GroundWaterByAbstractionNodeNumber[AAbstractionNodeNumber: Integer]: IGroundWater read Get_GroundWaterByAbstractionNodeNumber;
    property GroundWaterByCollectionNodeNumber[ACollectionNodeNumber: Integer]: IGroundWater read Get_GroundWaterByCollectionNodeNumber;
  end;

implementation

uses
  System.Types,
  SysUtils,
  Math,
  UConstants,
  UGroundWaterSQLAgent,
  UReservoirData,
  UYieldModelDataObject,
  UErrorHandlingOperations;

{ TGroundWater }

function TGroundWater._AddRef: Integer;
const OPNAME = 'TGroundWater._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater._Release: Integer;
const OPNAME = 'TGroundWater._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWater.CreateMemberObjects;
const OPNAME = 'TGroundWater.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWater.DestroyMemberObjects;
const OPNAME = 'TGroundWater.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Initialise: boolean;
const OPNAME = 'TGroundWater.Initialise';
var
  LIndex : integer;
begin
  Result := inherited Initialise;
  try
    FIdentifier                                := 0;
    FAquiferNodeNumber                         := 0;
    FDescription                               := '';
    FName                                      := '';

    FRefNodeNumber                             := 0;
    FAquiferInflowChannelNr                    := 0;
    FAquiferExcessInterflowChannelNr           := 0;
    FGroundWaterBaseflowChannelNr              := 0;
    FAbstractionFromAquiferChannelNr           := 0;
    FAbstractionFromBaseflowChannelNr          := 0;
    FInflowFromUpstreamAquiferChannelNr        := 0;
    FOutflowToDownstreamAquiferChannelNr       := 0;
    FSurfaceRunoffAndSoilInterflowChannelNr    := 0;
    FGroundWaterBaseFlowRemainderChannelNr     := 0;
    FGroundWaterAbstractionChannelNr           := 0;
    FOutflowToNetworkChannelNr                 := 0;

    FAbstractionNodeNr                         := 0;
    FCollectionNodeNr                          := 0;
    FBaseFlowNodeNr                            := 0;

    FAquiferStorativity                        := 0.0;
    FAquiferStaticWaterLevel                   := 0.0;
    FUnsaturatedStorageCapacity                := 0.0;
    FInitialUnsaturatedStorage                 := 0.0;
    FMaximumDischargeRate                      := 0.0;
    FMovingAverageRecharge                     := 0.0;
    FPitmanSoilMoistureCapacity                := 0.0;
    FPitmanSoilMoistureStorageCapacity         := 0.0;
    FPitmansoilMoistureFlowState               := 0.0;
    FPitmanSoilMoistureFlowEquation            := 0.0;
    FPitmanMaximumGroundwaterFlow              := 0.0;
    FPitmanSoilMoistureRechargeEquation        := 0.0;
    FPitmanGroundwaterFlow                     := 0.0;
    FMaximumRateOfGroundwaterBaseFlow          := 0.0;
    FPowerHeadDifferenceBaseFlowEquation       := 0.0;
    FMaximumHydrologicalGradient               := 0.0;
    FAquiferTransmissivity                     := 0.0;
    FBoreHoleDistanceToRiver                   := 0.0;
    FMaximumGroundwaterAbstraction             := 0.0;
    FParameterK2                               := 0.0;
    FParameterK3                               := 0.0;
    FGroundWaterEvaporationArea                := 0.0;
    for LIndex := Low(FGroundWaterEvaporation) to High(FGroundWaterEvaporation) do
      FGroundWaterEvaporation[LIndex] := 0.0;
    for LIndex := Low(FGroundWaterFactors) to High(FGroundWaterFactors) do
      FGroundWaterFactors[LIndex] := 0.0;
  except on E: Exception do HandleError(E, OPNAME); end;
end;  

function TGroundWater.Get_AbstractionFromAquiferChannelNr: Integer;
const OPNAME = 'TGroundWater.Get_AbstractionFromAquiferChannelNr';
begin
  Result := 0;
  try
    Result := FAbstractionFromAquiferChannelNr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_AbstractionFromBaseflowChannelNr: Integer;
const OPNAME = 'TGroundWater.Get_AbstractionFromBaseflowChannelNr';
begin
  Result := 0;
  try
    Result := FAbstractionFromBaseflowChannelNr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_AquiferExcessInterflowChannelNr: Integer;
const OPNAME = 'TGroundWater.Get_AquiferExcessInterflowChannelNr';
begin
  Result := 0;
  try
    Result := FAquiferExcessInterflowChannelNr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TGroundWater.Get_RefNodeNumber: Integer;
const OPNAME = 'TGroundWater.Get_RefNodeNumber';
begin
  Result := 0;
  try
    Result := FRefNodeNumber;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWater.Set_RefNodeNumber(Value: Integer);
const OPNAME = 'TGroundWater.Set_RefNodeNumber';
var
  LLoadAgent    : TGroundWaterSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FRefNodeNumber <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TGroundWaterSQLAgent.Create(FAppModules);
      try
        LOldValue := IntToStr(FRefNodeNumber);
        LLoadAgent.LoadContextData_Identifier(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('GroundWaterRefNodeNumber', IntToStr(Value), LOldValue, LContextData ) then
        begin
          FRefNodeNumber := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'GroundWaterRefNodeNumber', LOldValue, IntToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_AquiferInflowChannelNr: Integer;
const OPNAME = 'TGroundWater.Get_AquiferInflowChannelNr';
begin
  Result := 0;
  try
    Result := FAquiferInflowChannelNr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_AquiferStaticWaterLevel: Double;
const OPNAME = 'TGroundWater.Get_AquiferStaticWaterLevel';
begin
  Result := 0.0;
  try
    Result := FAquiferStaticWaterLevel;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWater.Set_AquiferStaticWaterLevel(Value: Double);
const OPNAME = 'TGroundWater.Set_AquiferStaticWaterLevel';
var
  LLoadAgent   : TGroundWaterSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TGroundWaterSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_GroundWaterIdentifier(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'AquiferStaticWaterLevel', FloatToStr(Value), FloatToStr(FAquiferStaticWaterLevel), LContextData) then
        begin
          LOldValue := FAquiferStaticWaterLevel;
          FAquiferStaticWaterLevel := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'AquiferStaticWaterLevel',FloatToStr(LOldValue),FloatToStr(FAquiferStaticWaterLevel));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_AquiferStorativity: Double;
const OPNAME = 'TGroundWater.Get_AquiferStorativity';
begin
  Result := 0.0;
  try
    Result := FAquiferStorativity;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWater.Set_AquiferStorativity(Value: Double);
const OPNAME = 'TGroundWater.Set_AquiferStorativity';
var
  LLoadAgent   : TGroundWaterSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TGroundWaterSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_GroundWaterIdentifier(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'AquiferStorativity', FloatToStr(Value), FloatToStr(FAquiferStorativity), LContextData) then
        begin
          LOldValue := FAquiferStorativity;
          FAquiferStorativity := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'AquiferStorativity',FloatToStr(LOldValue),FloatToStr(FAquiferStorativity));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_AquiferTransmissivity: Double;
const OPNAME = 'TGroundWater.Get_AquiferTransmissivity';
begin
  Result := 0.0;
  try
    Result := FAquiferTransmissivity;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWater.Set_AquiferTransmissivity(Value: Double);
const OPNAME = 'TGroundWater.Set_AquiferTransmissivity';
var
  LLoadAgent   : TGroundWaterSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TGroundWaterSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_GroundWaterIdentifier(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'AquiferTransmissivity', FloatToStr(Value), FloatToStr(FAquiferTransmissivity), LContextData) then
        begin
          LOldValue := FAquiferTransmissivity;
          FAquiferTransmissivity := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'AquiferTransmissivity',FloatToStr(LOldValue),FloatToStr(FAquiferTransmissivity));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_BoreHoleDistanceToRiver: Double;
const OPNAME = 'TGroundWater.Get_BoreHoleDistanceToRiver';
begin
  Result := 0.0;
  try
    Result := FBoreHoleDistanceToRiver;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWater.Set_BoreHoleDistanceToRiver(Value: Double);
const OPNAME = 'TGroundWater.Set_BoreHoleDistanceToRiver';
var
  LLoadAgent   : TGroundWaterSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TGroundWaterSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_GroundWaterIdentifier(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'BoreHoleDistanceToRiver', FloatToStr(Value), FloatToStr(FBoreHoleDistanceToRiver), LContextData) then
        begin
          LOldValue := FBoreHoleDistanceToRiver;
          FBoreHoleDistanceToRiver := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'BoreHoleDistanceToRiver',FloatToStr(LOldValue),FloatToStr(FBoreHoleDistanceToRiver));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_Description: WideString;
const OPNAME = 'TGroundWater.Get_Description';
begin
  Result := '';
  try
    Result := FDescription;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWater.Set_Description(const Value: WideString);
const OPNAME = 'TGroundWater.Set_Description';
var
  LLoadAgent   : TGroundWaterSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TGroundWaterSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_GroundWaterIdentifier(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'GroundWaterDescription', Value, FDescription, LContextData) then
        begin
          LOldValue := FDescription;
          FDescription := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'GroundWaterDescription',LOldValue,FDescription);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;



function TGroundWater.Get_GroundWaterBaseflowChannelNr: Integer;
const OPNAME = 'TGroundWater.Get_GroundWaterBaseflowChannelNr';
begin
  Result := 0;
  try
    Result := FGroundWaterBaseflowChannelNr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_GroundWaterEvaporationArea: Double;
const OPNAME = 'TGroundWater.Get_GroundWaterEvaporationArea';
begin
  Result := 0.0;
  try
    Result := FGroundWaterEvaporationArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWater.Set_GroundWaterEvaporationArea(Value: Double);
const OPNAME = 'TGroundWater.Set_GroundWaterEvaporationArea';
var
  LLoadAgent   : TGroundWaterSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TGroundWaterSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_GroundWaterIdentifier(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'GroundWaterEvaporationArea', FloatToStr(Value), FloatToStr(FGroundWaterEvaporationArea), LContextData) then
        begin
          LOldValue := FGroundWaterEvaporationArea;
          FGroundWaterEvaporationArea := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'GroundWaterEvaporationArea',FloatToStr(LOldValue),FloatToStr(FGroundWaterEvaporationArea));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_Identifier: Integer;
const OPNAME = 'TGroundWater.Get_Identifier';
begin
  Result := 0;
  try
    Result := FIdentifier;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_InitialUnsaturatedStorage: Double;
const OPNAME = 'TGroundWater.Get_InitialUnsaturatedStorage';
begin
  Result := 0.0;
  try
    Result := FInitialUnsaturatedStorage;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWater.Set_InitialUnsaturatedStorage(Value: Double);
const OPNAME = 'TGroundWater.Set_InitialUnsaturatedStorage';
var
  LLoadAgent   : TGroundWaterSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TGroundWaterSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_GroundWaterIdentifier(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'InitialUnsaturatedStorage', FloatToStr(Value), FloatToStr(FInitialUnsaturatedStorage), LContextData) then
        begin
          LOldValue := FInitialUnsaturatedStorage;
          FInitialUnsaturatedStorage := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'InitialUnsaturatedStorage',FloatToStr(LOldValue),FloatToStr(FInitialUnsaturatedStorage));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_MaximumDischargeRate: Double;
const OPNAME = 'TGroundWater.Get_MaximumDischargeRate';
begin
  Result := 0.0;
  try
    Result := FMaximumDischargeRate;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWater.Set_MaximumDischargeRate(Value: Double);
const OPNAME = 'TGroundWater.Set_MaximumDischargeRate';
var
  LLoadAgent   : TGroundWaterSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TGroundWaterSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_GroundWaterIdentifier(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'MaximumDischargeRate', FloatToStr(Value), FloatToStr(FMaximumDischargeRate), LContextData) then
        begin
          LOldValue := FMaximumDischargeRate;
          FMaximumDischargeRate := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MaximumDischargeRate',FloatToStr(LOldValue),FloatToStr(FMaximumDischargeRate));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_MaximumGroundwaterAbstraction: Double;
const OPNAME = 'TGroundWater.Get_MaximumGroundwaterAbstraction';
begin
  Result := 0.0;
  try
    Result := FMaximumGroundwaterAbstraction;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWater.Set_MaximumGroundwaterAbstraction(Value: Double);
const OPNAME = 'TGroundWater.Set_MaximumGroundwaterAbstraction';
var
  LLoadAgent   : TGroundWaterSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TGroundWaterSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_GroundWaterIdentifier(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'MaximumGroundwaterAbstraction', FloatToStr(Value), FloatToStr(FMaximumGroundwaterAbstraction), LContextData) then
        begin
          LOldValue := FMaximumGroundwaterAbstraction;
          FMaximumGroundwaterAbstraction := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MaximumGroundwaterAbstraction',FloatToStr(LOldValue),FloatToStr(FMaximumGroundwaterAbstraction));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TGroundWater.Get_MaximumHydrologicalGradient: Double;
const OPNAME = 'TGroundWater.Get_MaximumHydrologicalGradient';
begin
  Result := 0.0;
  try
    Result := FMaximumHydrologicalGradient;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWater.Set_MaximumHydrologicalGradient(Value: Double);
const OPNAME = 'TGroundWater.Set_MaximumHydrologicalGradient';
var
  LLoadAgent   : TGroundWaterSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TGroundWaterSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_GroundWaterIdentifier(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'MaximumHydrologicalGradient', FloatToStr(Value), FloatToStr(FMaximumHydrologicalGradient), LContextData) then
        begin
          LOldValue := FMaximumHydrologicalGradient;
          FMaximumHydrologicalGradient := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MaximumHydrologicalGradient',FloatToStr(LOldValue),FloatToStr(FMaximumHydrologicalGradient));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TGroundWater.Get_MaximumRateOfGroundwaterBaseFlow: Double;
const OPNAME = 'TGroundWater.Get_MaximumRateOfGroundwaterBaseFlow';
begin
  Result := 0.0;
  try
    Result := FMaximumRateOfGroundwaterBaseFlow;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWater.Set_MaximumRateOfGroundwaterBaseFlow(Value: Double);
const OPNAME = 'TGroundWater.Set_MaximumRateOfGroundwaterBaseFlow';
var
  LLoadAgent   : TGroundWaterSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TGroundWaterSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_GroundWaterIdentifier(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'MaximumRateOfGroundwaterBaseFlow', FloatToStr(Value), FloatToStr(FMaximumRateOfGroundwaterBaseFlow), LContextData) then
        begin
          LOldValue := FMaximumRateOfGroundwaterBaseFlow;
          FMaximumRateOfGroundwaterBaseFlow := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MaximumRateOfGroundwaterBaseFlow',FloatToStr(LOldValue),FloatToStr(FMaximumRateOfGroundwaterBaseFlow));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_MovingAverageRecharge: Double;
const OPNAME = 'TGroundWater.Get_MovingAverageRecharge';
begin
  Result := 0.0;
  try
    Result := FMovingAverageRecharge;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWater.Set_MovingAverageRecharge(Value: Double);
const OPNAME = 'TGroundWater.Set_MovingAverageRecharge';
var
  LLoadAgent   : TGroundWaterSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TGroundWaterSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_GroundWaterIdentifier(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'MovingAverageRecharge', FloatToStr(Value), FloatToStr(FMovingAverageRecharge), LContextData) then
        begin
          LOldValue := FMovingAverageRecharge;
          FMovingAverageRecharge := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MovingAverageRecharge',FloatToStr(LOldValue),FloatToStr(FMovingAverageRecharge));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TGroundWater.Get_Name: WideString;
const OPNAME = 'TGroundWater.Get_Name';
begin
  Result := '';
  try
    Result := FName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWater.Set_Name(const Value: WideString);
const OPNAME = 'TGroundWater.Set_Name';
var
  LLoadAgent   : TGroundWaterSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TGroundWaterSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_GroundWaterIdentifier(LContextData, IntToStr(Identifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'GroundWaterName', Value, FName, LContextData) then
        begin
          LOldValue := FName;
          FName := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'GroundWaterName',LOldValue,FName);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_InflowFromUpstreamAquiferChannelNr: Integer;
const OPNAME = 'TGroundWater.Get_InflowFromUpstreamAquiferChannelNr';
begin
  Result := 0;
  try
    Result := FInflowFromUpstreamAquiferChannelNr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_OutflowToDownstreamAquiferChannelNr: Integer;
const OPNAME = 'TGroundWater.Get_OutflowToDownstreamAquiferChannelNr';
begin
  Result := 0;
  try
    Result := FOutflowToDownstreamAquiferChannelNr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_ParameterK2: Double;
const OPNAME = 'TGroundWater.Get_ParameterK2';
begin
  Result := 0.0;
  try
    Result := FParameterK2;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWater.Set_ParameterK2(Value: Double);
const OPNAME = 'TGroundWater.Set_ParameterK2';
var
  LLoadAgent   : TGroundWaterSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TGroundWaterSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_GroundWaterIdentifier(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'ParameterK2', FloatToStr(Value), FloatToStr(FParameterK2), LContextData) then
        begin
          LOldValue := FParameterK2;
          FParameterK2 := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ParameterK2',FloatToStr(LOldValue),FloatToStr(FParameterK2));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_ParameterK3: Double;
const OPNAME = 'TGroundWater.Get_ParameterK3';
begin
  Result := 0.0;
  try
    Result := FParameterK3;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWater.Set_ParameterK3(Value: Double);
const OPNAME = 'TGroundWater.Set_ParameterK3';
var
  LLoadAgent   : TGroundWaterSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TGroundWaterSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_GroundWaterIdentifier(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'ParameterK3', FloatToStr(Value), FloatToStr(FParameterK3), LContextData) then
        begin
          LOldValue := FParameterK3;
          FParameterK3 := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ParameterK3',FloatToStr(LOldValue),FloatToStr(FParameterK3));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_PitmanGroundwaterFlow: Double;
const OPNAME = 'TGroundWater.Get_PitmanGroundwaterFlow';
begin
  Result := 0.0;
  try
    Result := FPitmanGroundwaterFlow;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWater.Set_PitmanGroundwaterFlow(Value: Double);
const OPNAME = 'TGroundWater.Set_PitmanGroundwaterFlow';
var
  LLoadAgent   : TGroundWaterSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TGroundWaterSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_Identifier(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'PitmanGroundwaterFlow', FloatToStr(Value), FloatToStr(FPitmanGroundwaterFlow), LContextData) then
        begin
          LOldValue := FPitmanGroundwaterFlow;
          FPitmanGroundwaterFlow := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'PitmanGroundwaterFlow',FloatToStr(LOldValue),FloatToStr(FPitmanGroundwaterFlow));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TGroundWater.Get_PitmanMaximumGroundwaterFlow: Double;
const OPNAME = 'TGroundWater.Get_PitmanMaximumGroundwaterFlow';
begin
  Result := 0.0;
  try
    Result := FPitmanMaximumGroundwaterFlow;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWater.Set_PitmanMaximumGroundwaterFlow(Value: Double);
const OPNAME = 'TGroundWater.Set_PitmanMaximumGroundwaterFlow';
var
  LLoadAgent   : TGroundWaterSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TGroundWaterSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_Identifier(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'PitmanMaximumGroundwaterFlow', FloatToStr(Value), FloatToStr(FPitmanMaximumGroundwaterFlow), LContextData) then
        begin
          LOldValue := FPitmanMaximumGroundwaterFlow;
          FPitmanMaximumGroundwaterFlow := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'PitmanMaximumGroundwaterFlow',FloatToStr(LOldValue),FloatToStr(FPitmanMaximumGroundwaterFlow));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_PitmanSoilMoistureCapacity: Double;
const OPNAME = 'TGroundWater.Get_PitmanSoilMoistureCapacity';
begin
  Result := 0.0;
  try
    Result := FPitmanSoilMoistureCapacity;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWater.Set_PitmanSoilMoistureCapacity(Value: Double);
const OPNAME = 'TGroundWater.Set_PitmanSoilMoistureCapacity';
var
  LLoadAgent   : TGroundWaterSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TGroundWaterSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_Identifier(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'PitmanSoilMoistureCapacity', FloatToStr(Value), FloatToStr(FPitmanSoilMoistureCapacity), LContextData) then
        begin
          LOldValue := FPitmanSoilMoistureCapacity;
          FPitmanSoilMoistureCapacity := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'PitmanSoilMoistureCapacity',FloatToStr(LOldValue),FloatToStr(FPitmanSoilMoistureCapacity));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end; 

function TGroundWater.Get_PitmanSoilMoistureFlowEquation: Double;
const OPNAME = 'TGroundWater.Get_PitmanSoilMoistureFlowEquation';
begin
  Result := 0.0;
  try
    Result := FPitmanSoilMoistureFlowEquation;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWater.Set_PitmanSoilMoistureFlowEquation(Value: Double);
const OPNAME = 'TGroundWater.Set_PitmanSoilMoistureFlowEquation';
var
  LLoadAgent   : TGroundWaterSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TGroundWaterSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_Identifier(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'PitmanSoilMoistureFlowEquation', FloatToStr(Value), FloatToStr(FPitmanSoilMoistureFlowEquation), LContextData) then
        begin
          LOldValue := FPitmanSoilMoistureFlowEquation;
          FPitmanSoilMoistureFlowEquation := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'PitmanSoilMoistureFlowEquation',FloatToStr(LOldValue),FloatToStr(FPitmanSoilMoistureFlowEquation));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TGroundWater.Get_PitmansoilMoistureFlowState: Double;
const OPNAME = 'TGroundWater.Get_PitmansoilMoistureFlowState';
begin
  Result := 0.0;
  try
    Result := FPitmansoilMoistureFlowState;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWater.Set_PitmansoilMoistureFlowState(Value: Double);
const OPNAME = 'TGroundWater.Set_PitmansoilMoistureFlowState';
var
  LLoadAgent   : TGroundWaterSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TGroundWaterSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_Identifier(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'PitmansoilMoistureFlowState', FloatToStr(Value), FloatToStr(FPitmansoilMoistureFlowState), LContextData) then
        begin
          LOldValue := FPitmansoilMoistureFlowState;
          FPitmansoilMoistureFlowState := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'PitmansoilMoistureFlowState',FloatToStr(LOldValue),FloatToStr(FPitmansoilMoistureFlowState));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_PitmanSoilMoistureRechargeEquation: Double;
const OPNAME = 'TGroundWater.Get_PitmanSoilMoistureRechargeEquation';
begin
  Result := 0.0;
  try
    Result := FPitmanSoilMoistureRechargeEquation;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWater.Set_PitmanSoilMoistureRechargeEquation(Value: Double);
const OPNAME = 'TGroundWater.Set_PitmanSoilMoistureRechargeEquation';
var
  LLoadAgent   : TGroundWaterSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TGroundWaterSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_Identifier(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'PitmanSoilMoistureRechargeEquation', FloatToStr(Value), FloatToStr(FPitmanSoilMoistureRechargeEquation), LContextData) then
        begin
          LOldValue := FPitmanSoilMoistureRechargeEquation;
          FPitmanSoilMoistureRechargeEquation := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'PitmanSoilMoistureRechargeEquation',FloatToStr(LOldValue),FloatToStr(FPitmanSoilMoistureRechargeEquation));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TGroundWater.Get_PitmanSoilMoistureStorageCapacity: Double;
const OPNAME = 'TGroundWater.Get_PitmanSoilMoistureStorageCapacity';
begin
  Result := 0.0;
  try
    Result := FPitmanSoilMoistureStorageCapacity;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWater.Set_PitmanSoilMoistureStorageCapacity(Value: Double);
const OPNAME = 'TGroundWater.Set_PitmanSoilMoistureStorageCapacity';
var
  LLoadAgent   : TGroundWaterSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TGroundWaterSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_Identifier(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'PitmanSoilMoistureStorageCapacity', FloatToStr(Value), FloatToStr(FPitmanSoilMoistureStorageCapacity), LContextData) then
        begin
          LOldValue := FPitmanSoilMoistureStorageCapacity;
          FPitmanSoilMoistureStorageCapacity := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'PitmanSoilMoistureStorageCapacity',FloatToStr(LOldValue),FloatToStr(FPitmanSoilMoistureStorageCapacity));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TGroundWater.Get_PowerHeadDifferenceBaseFlowEquation: Double;
const OPNAME = 'TGroundWater.Get_PowerHeadDifferenceBaseFlowEquation';
begin
  Result := 0.0;
  try
    Result := FPowerHeadDifferenceBaseFlowEquation;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWater.Set_PowerHeadDifferenceBaseFlowEquation(Value: Double);
const OPNAME = 'TGroundWater.Set_PowerHeadDifferenceBaseFlowEquation';
var
  LLoadAgent   : TGroundWaterSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TGroundWaterSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_GroundWaterIdentifier(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'PowerHeadDifferenceBaseFlowEquation', FloatToStr(Value), FloatToStr(FPowerHeadDifferenceBaseFlowEquation), LContextData) then
        begin
          LOldValue := FPowerHeadDifferenceBaseFlowEquation;
          FPowerHeadDifferenceBaseFlowEquation := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'PowerHeadDifferenceBaseFlowEquation',FloatToStr(LOldValue),FloatToStr(FPowerHeadDifferenceBaseFlowEquation));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_SurfaceRunoffAndSoilInterflowChannelNr: Integer;
const OPNAME = 'TGroundWater.Get_SurfaceRunoffAndSoilInterflowChannelNr';
begin
  Result := 0;
  try
    Result := FSurfaceRunoffAndSoilInterflowChannelNr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_GroundWaterBaseFlowRemainderChannelNr: Integer;
const OPNAME = 'TGroundWater.Get_GroundWaterBaseFlowRemainderChannelNr';
begin
  Result := 0;
  try
    Result := FGroundWaterBaseFlowRemainderChannelNr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_GroundWaterAbstractionChannelNr: Integer;
const OPNAME = 'TGroundWater.Get_GroundWaterAbstractionChannelNr';
begin
  Result := 0;
  try
    Result := FGroundWaterAbstractionChannelNr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_OutflowToNetworkChannelNr: Integer;
const OPNAME = 'TGroundWater.Get_OutflowToNetworkChannelNr';
begin
  Result := 0;
  try
    Result := FOutflowToNetworkChannelNr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_UnsaturatedStorageCapacity: Double;
const OPNAME = 'TGroundWater.Get_UnsaturatedStorageCapacity';
begin
  Result := 0;
  try
    Result := FUnsaturatedStorageCapacity;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWater.Set_UnsaturatedStorageCapacity(Value: Double);
const OPNAME = 'TGroundWater.Set_UnsaturatedStorageCapacity';
var
  LLoadAgent   : TGroundWaterSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TGroundWaterSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_GroundWaterIdentifier(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'UnsaturatedStorageCapacity', FloatToStr(Value), FloatToStr(FUnsaturatedStorageCapacity), LContextData) then
        begin
          LOldValue := FUnsaturatedStorageCapacity;
          FUnsaturatedStorageCapacity := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'UnsaturatedStorageCapacity',FloatToStr(LOldValue),FloatToStr(FUnsaturatedStorageCapacity));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_BaseFlowNode: IReservoirData;
const OPNAME = 'TGroundWater.Get_BaseFlowNode';
begin
  Result := nil;
  try
    Result := (FAppModules.Model.ModelData as IYieldModelData).
              NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[FBaseFlowNodeNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_CollectionNode: IReservoirData;
const OPNAME = 'TGroundWater.Get_CollectionNode';
begin
  Result := nil;
  try
    Result := (FAppModules.Model.ModelData as IYieldModelData).
              NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[FCollectionNodeNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_AbstractionNode: IReservoirData;
const OPNAME = 'TGroundWater.Get_AbstractionNode';
begin
  Result := nil;
  try
    Result := (FAppModules.Model.ModelData as IYieldModelData).
              NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[FAbstractionNodeNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_AquiferExcessInterflowChannel: IGeneralFlowChannel;
const OPNAME = 'TGroundWater.Get_AquiferExcessInterflowChannel';
begin
  Result := nil;
  try
    Result := (FAppModules.Model.ModelData as IYieldModelData).
              NetworkElementData.ChannelList.ChannelByChannelNumber[FAquiferExcessInterflowChannelNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_AquiferInflowChannel: IGeneralFlowChannel;
const OPNAME = 'TGroundWater.Get_AquiferInflowChannel';
begin
  Result := nil;
  try
    Result := (FAppModules.Model.ModelData as IYieldModelData).
              NetworkElementData.ChannelList.ChannelByChannelNumber[FAquiferInflowChannelNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_GroundWaterBaseflowChannel: IGeneralFlowChannel;
const OPNAME = 'TGroundWater.Get_GroundWaterBaseflowChannel';
begin
  Result := nil;
  try
    Result := (FAppModules.Model.ModelData as IYieldModelData).
               NetworkElementData.ChannelList.ChannelByChannelNumber[FGroundWaterBaseflowChannelNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_SurfaceRunoffAndSoilInterflowChannel: IGeneralFlowChannel;
const OPNAME = 'TGroundWater.Get_SurfaceRunoffAndSoilInterflowChannel';
begin
  Result := nil;
  try
    Result := (FAppModules.Model.ModelData as IYieldModelData).
               NetworkElementData.ChannelList.ChannelByChannelNumber[FSurfaceRunoffAndSoilInterflowChannelNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_GroundWaterBaseFlowRemainderChannel: IGeneralFlowChannel;
const OPNAME = 'TGroundWater.Get_GroundWaterBaseFlowRemainderChannel';
begin
  Result := nil;
  try
    Result := (FAppModules.Model.ModelData as IYieldModelData).
               NetworkElementData.ChannelList.ChannelByChannelNumber[FGroundWaterBaseFlowRemainderChannelNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_InflowFromUpstreamAquiferChannel: IGeneralFlowChannel;
const OPNAME = 'TGroundWater.Get_InflowFromUpstreamAquiferChannel';
begin
  Result := nil;
  try
    Result := (FAppModules.Model.ModelData as IYieldModelData).
               NetworkElementData.ChannelList.ChannelByChannelNumber[FInflowFromUpstreamAquiferChannelNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_OutflowToDownstreamAquiferChannel: IGeneralFlowChannel;
const OPNAME = 'TGroundWater.Get_OutflowToDownstreamAquiferChannel';
begin
  Result := nil;
  try
    Result := (FAppModules.Model.ModelData as IYieldModelData).
               NetworkElementData.ChannelList.ChannelByChannelNumber[FOutflowToDownstreamAquiferChannelNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_AbstractionFromAquiferChannel: IGeneralFlowChannel;
const OPNAME = 'TGroundWater.Get_AbstractionFromAquiferChannel';
begin
  Result := nil;
  try
    Result := (FAppModules.Model.ModelData as IYieldModelData).
               NetworkElementData.ChannelList.ChannelByChannelNumber[FAbstractionFromAquiferChannelNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_AbstractionFromBaseFlowChannel: IGeneralFlowChannel;
const OPNAME = 'TGroundWater.Get_AbstractionFromBaseFlowChannel';
begin
  Result := nil;
  try
    Result := (FAppModules.Model.ModelData as IYieldModelData).
              NetworkElementData.ChannelList.ChannelByChannelNumber[FAbstractionFromBaseflowChannelNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_GroundWaterAbstractionChannel: IGeneralFlowChannel;
const OPNAME = 'TGroundWater.Get_GroundWaterAbstractionChannel';
begin
  Result := nil;
  try
    Result := (FAppModules.Model.ModelData as IYieldModelData).
               NetworkElementData.ChannelList.ChannelByChannelNumber[FGroundWaterAbstractionChannelNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_OutflowToNetworkChannel: IGeneralFlowChannel;
const OPNAME = 'TGroundWater.Get_OutflowToNetworkChannel';
begin
  Result := nil;
  try
    Result := (FAppModules.Model.ModelData as IYieldModelData).
               NetworkElementData.ChannelList.ChannelByChannelNumber[FOutflowToNetworkChannelNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Validate(var AErrors: WideString;const AContext: WideString): WordBool;
const OPNAME = 'TGroundWater.Validate';
var
  LErrorMessage     : TStringList;
  LErrorCols        : TStringList;
  LStopOnFirstError : Boolean;
begin
  Result := False;
  try
    LErrorMessage := TStringList.Create;
    LErrorCols    := TStringList.Create;
    try
      if (AContext = 'GroundWaterName') then
        Result := ValidateGroundWaterName(LErrorMessage)
      else
      if (AContext = 'GroundWaterDescription') then
        Result := ValidateGroundWaterDescription(LErrorMessage)
      else
      if (AContext = 'AquiferStorativity') then
        Result := ValidateAquiferStorativity(LErrorMessage)
      else
      if (AContext = 'AquiferStaticWaterlevel') then
        Result := ValidateAquiferStaticWaterLevel(LErrorMessage)
      else
      if (AContext = 'UnsaturatedStorageCapacity') then
        Result := ValidateUnsaturatedStorageCapacity(LErrorMessage)
      else
      if (AContext = 'InitialUnsaturatedStorage') then
        Result := ValidateInitialUnsaturatedStorage(LErrorMessage)
      else
      if (AContext = 'MaximumDischargeRate') then
        Result := ValidateMaximumDischargeRate(LErrorMessage)
      else
      if (AContext = 'MovingAverageRecharge') then
        Result := ValidateMovingAverageRecharge(LErrorMessage)
      else
      if (AContext = 'PitmanSoilMoistureCapacity') then
        Result := ValidatePitmanSoilMoistureCapacity(LErrorMessage)
      else
      if (AContext = 'PitmanSoilMoistureStorageCapacity') then
        Result := ValidatePitmanSoilMoistureStorageCapacity(LErrorMessage)
      else
      if (AContext = 'PitmansoilMoistureFlowState') then
        Result := ValidatePitmansoilMoistureFlowState(LErrorMessage)
      else
      if (AContext = 'PitmanSoilMoistureFlowEquation') then
        Result := ValidatePitmanSoilMoistureFlowEquation(LErrorMessage)
      else
      if (AContext = 'PitmanMaximumGroundwaterFlow') then
        Result := ValidatePitmanMaximumGroundwaterFlow(LErrorMessage)
      else
      if (AContext = 'PitmanSoilMoistureRechargeEquation') then
        Result := ValidatePitmanSoilMoistureRechargeEquation(LErrorMessage)
      else
      if (AContext = 'PitmanGroundwaterFlow') then
        Result := ValidatePitmanGroundwaterFlow(LErrorMessage)
      else
      if (AContext = 'MaximumRateOfGroundwaterBaseFlow') then
        Result := ValidateMaximumRateOfGroundwaterBaseFlow(LErrorMessage)
      else
      if (AContext = 'PowerHeadDifferenceBaseFlowEquation') then
        Result := ValidatePowerHeadDifferenceBaseFlowEquation(LErrorMessage)
      else
      if (AContext = 'MaximumHydrologicalGradient') then
        Result := ValidateMaximumHydrologicalGradient(LErrorMessage)
      else
      if (AContext = 'AquiferTransmissivity') then
        Result := ValidateAquiferTransmissivity(LErrorMessage)
      else
      if (AContext = 'BoreHoleDistanceToRiver') then
        Result := ValidateBoreHoleDistanceToRiver(LErrorMessage)
      else
      if (AContext = 'MaximumGroundwaterAbstraction') then
        Result := ValidateMaximumGroundwaterAbstraction(LErrorMessage)
      else
      if (AContext = 'ParameterK2') then
        Result := ValidateParameterK2(LErrorMessage)
      else
      if (AContext = 'ParameterK3') then
        Result := ValidateParameterK3(LErrorMessage)
      else
      if (AContext = 'GroundWaterEvaporationArea') then
        Result := ValidateGroundWaterEvaporationArea(LErrorMessage)
      else
      if (AContext = 'GroundWaterRefNodeNumber') then
        Result := ValidateRefNodeNumber(LErrorMessage)
      else
      begin
        Result := True;
        LStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        if (not ValidateGroundWaterName(LErrorMessage))           then Result := False;
        if (Result OR (NOT LStopOnFirstError)) then
        begin
          if (not ValidateGroundWaterDescription(LErrorMessage))          then Result := False;
        end;
        if (Result OR (NOT LStopOnFirstError)) then
        begin
          if (not ValidateAquiferStorativity(LErrorMessage))   then Result := False;
        end;
        if (Result OR (NOT LStopOnFirstError)) then
        begin
          if (not ValidateAquiferStaticWaterLevel(LErrorMessage))    then Result := False;
        end;
        if (Result OR (NOT LStopOnFirstError)) then
        begin
          if (not ValidateUnsaturatedStorageCapacity(LErrorMessage))       then Result := False;
        end;
        if (Result OR (NOT LStopOnFirstError)) then
        begin
          if (not ValidateInitialUnsaturatedStorage(LErrorMessage))   then Result := False;
        end;
        if (Result OR (NOT LStopOnFirstError)) then
        begin
          if (not ValidateMaximumDischargeRate(LErrorMessage))   then Result := False;
        end;
        if (Result OR (NOT LStopOnFirstError)) then
        begin
          if (not ValidateMovingAverageRecharge(LErrorMessage))  then Result := False;
        end;  
        if (Result OR (NOT LStopOnFirstError)) then
        begin
          if (not ValidatePitmanSoilMoistureCapacity(LErrorMessage))          then Result := False;
        end;
        if (Result OR (NOT LStopOnFirstError)) then
        begin
          if (not ValidatePitmanSoilMoistureStorageCapacity(LErrorMessage))   then Result := False;
        end;
        if (Result OR (NOT LStopOnFirstError)) then
        begin
          if (not ValidatePitmansoilMoistureFlowState(LErrorMessage))    then Result := False;
        end;
        if (Result OR (NOT LStopOnFirstError)) then
        begin
          if (not ValidatePitmanSoilMoistureFlowEquation(LErrorMessage))       then Result := False;
        end;
        if (Result OR (NOT LStopOnFirstError)) then
        begin
          if (not ValidatePitmanMaximumGroundwaterFlow(LErrorMessage))   then Result := False;
        end;
        if (Result OR (NOT LStopOnFirstError)) then
        begin
          if (not ValidatePitmanSoilMoistureRechargeEquation(LErrorMessage))   then Result := False;
        end;
        if (Result OR (NOT LStopOnFirstError)) then
        begin
          if (not ValidatePitmanGroundwaterFlow(LErrorMessage))  then Result := False;
        end;
        if (Result OR (NOT LStopOnFirstError)) then
        begin
          if (not ValidateMaximumRateOfGroundwaterBaseFlow(LErrorMessage))          then Result := False;
        end;
        if (Result OR (NOT LStopOnFirstError)) then
        begin
          if (not ValidatePowerHeadDifferenceBaseFlowEquation(LErrorMessage))   then Result := False;
        end;
        if (Result OR (NOT LStopOnFirstError)) then
        begin
          if (not ValidateMaximumHydrologicalGradient(LErrorMessage))    then Result := False;
        end;
        if (Result OR (NOT LStopOnFirstError)) then
        begin
          if (not ValidateAquiferTransmissivity(LErrorMessage))       then Result := False;
        end;
        if (Result OR (NOT LStopOnFirstError)) then
        begin
          if (not ValidateBoreHoleDistanceToRiver(LErrorMessage))   then Result := False;
        end;
        if (Result OR (NOT LStopOnFirstError)) then
        begin
          if (not ValidateMaximumGroundwaterAbstraction(LErrorMessage))   then Result := False;
        end;
        if (Result OR (NOT LStopOnFirstError)) then
        begin
          if (not ValidateParameterK2(LErrorMessage))  then Result := False;
        end;  
        if (Result OR (NOT LStopOnFirstError)) then
        begin
          if (not ValidateParameterK3(LErrorMessage))   then Result := False;
        end;
        if (Result OR (NOT LStopOnFirstError)) then
        begin
          if (not ValidateGroundWaterEvaporationArea(LErrorMessage))  then Result := False;
        end;  
        if (Result OR (NOT LStopOnFirstError)) then
        begin
          if (not ValidateRefNodeNumber(LErrorMessage))   then Result := False;
        end;
      end;
      AErrors := AErrors + LErrorMessage.Text;
    finally
      LErrorMessage.Free;
      LErrorCols.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_AbstractionNodeNr: Integer;
const OPNAME = 'TGroundWater.Get_AbstractionNodeNr';
begin
  Result := NullInteger;
  try
    Result := FAbstractionNodeNr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_BaseFlowNodeNr: Integer;
const OPNAME = 'TGroundWater.Get_BaseFlowNodeNr';
begin
  Result := NullInteger;
  try
    Result := FBaseFlowNodeNr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_CollectionNodeNr: Integer;
const OPNAME = 'TGroundWater.Get_CollectionNodeNr';
begin
  Result := NullInteger;
  try
    Result := FCollectionNodeNr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWater.Populate(AIdentifier, ANodeNumber: Integer; ADescription, AName: WideString;
                                AAquiferStorativity,AAquiferStaticWaterLevel,
                                AUnsaturatedStorageCapacity,AInitialUnsaturatedStorage,
                                AMaximumDischargeRate,AMovingAverageRecharge,
                                AMaximumRateOfGroundwaterBaseFlow, APowerHeadDifferenceBaseFlowEquation,
                                AMaximumHydrologicalGradient, AAquiferTransmissivity,
                                ABoreHoleDistanceToRiver, AMaximumGroundwaterAbstraction, AParameterK2,
                                AParameterK3,AGroundWaterEvaporationArea: Double);
const OPNAME = 'TGroundWater.Populate';
begin
  try
    FIdentifier                             := AIdentifier;
    FAquiferNodeNumber                      := ANodeNumber;
    FDescription                            := ADescription;
    FName                                   := AName;

    FAquiferStorativity                     := AAquiferStorativity;
    FAquiferStaticWaterLevel                := AAquiferStaticWaterLevel;
    FUnsaturatedStorageCapacity             := AUnsaturatedStorageCapacity;
    FInitialUnsaturatedStorage              := AInitialUnsaturatedStorage;
    FMaximumDischargeRate                   := AMaximumDischargeRate;
    FMovingAverageRecharge                  := AMovingAverageRecharge;
    FMaximumRateOfGroundwaterBaseFlow       := AMaximumRateOfGroundwaterBaseFlow;
    FPowerHeadDifferenceBaseFlowEquation    := APowerHeadDifferenceBaseFlowEquation;
    FMaximumHydrologicalGradient            := AMaximumHydrologicalGradient;
    FAquiferTransmissivity                  := AAquiferTransmissivity;
    FBoreHoleDistanceToRiver                := ABoreHoleDistanceToRiver;
    FMaximumGroundwaterAbstraction          := AMaximumGroundwaterAbstraction;
    FParameterK2                            := AParameterK2;
    FParameterK3                            := AParameterK3;
    FGroundWaterEvaporationArea             := AGroundWaterEvaporationArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWater.PopulateGroundWaterEvaporation(AWaterEvaporation: TMonthlyDoubleArray);
const OPNAME = 'TGroundWater.PopulateGroundWaterEvaporation';
var
  LIndex: integer;
begin
  try
    for LIndex := Low(FGroundWaterEvaporation) to High(FGroundWaterEvaporation) do
        FGroundWaterEvaporation[LIndex] := AWaterEvaporation[LIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWater.PopulateGroundWaterFactors(AGroundWaterFactors: TMonthlyDoubleArray);
const OPNAME = 'TGroundWater.PopulateGroundWaterFactors';
var
  LIndex: integer;
begin
  try
    for LIndex := Low(FGroundWaterFactors) to High(FGroundWaterFactors) do
        FGroundWaterFactors[LIndex] := AGroundWaterFactors[LIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_MonthlyWaterEvaporation(AIndex: Integer): Double;
const OPNAME = 'TGroundWater.Get_MonthlyWaterEvaporation';
begin
  Result := NullFloat;
  try
    Result := FGroundWaterEvaporation[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWater.Set_MonthlyWaterEvaporation(AIndex: Integer;Value: Double);
const OPNAME = 'TGroundWater.Set_MonthlyWaterEvaporation';
var
  LLoadAgent   : TGroundWaterSQLAgent;
  LContextData : TStringList;
  LPrevValue   : double;
begin
  try
    LLoadAgent := TGroundWaterSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_GroundWaterEvaporation(LContextData, FIdentifier,IntToStr(AIndex));
          if FAppModules.FieldProperties.UpdateFieldValue(
            'MonthlyWaterEvaporation', FloatToStr(Value), FloatToStr(FGroundWaterEvaporation[AIndex]), LContextData) then
          begin
            LPrevValue := FGroundWaterEvaporation[AIndex];
            FGroundWaterEvaporation[AIndex] := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'MonthlyWaterEvaporation',FloatToStr(LPrevValue),FloatToStr(FGroundWaterEvaporation[AIndex]));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWater.Get_MonthlyWaterUsageFactors(AIndex: Integer): Double;
const OPNAME = 'TGroundWater.Get_MonthlyWaterUsageFactors';
begin
  Result := NullFloat;
  try
    Result := FGroundWaterFactors[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWater.Set_MonthlyWaterUsageFactors(AIndex: Integer;Value: Double);
const OPNAME = 'TGroundWater.Set_MonthlyWaterUsageFactors';
var
  LLoadAgent   : TGroundWaterSQLAgent;
  LContextData : TStringList;
  LPrevValue   : double;
begin
  try
    LLoadAgent := TGroundWaterSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_GroundWaterEvaporation(LContextData, FIdentifier,IntToStr(AIndex));
          if FAppModules.FieldProperties.UpdateFieldValue(
            'MonthlyWaterUsageFactors', FloatToStr(Value), FloatToStr(FGroundWaterFactors[AIndex]), LContextData) then
          begin
            LPrevValue := FGroundWaterFactors[AIndex];
            FGroundWaterFactors[AIndex] := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'MonthlyWaterUsageFactors',FloatToStr(LPrevValue),FloatToStr(FGroundWaterFactors[AIndex]));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWater.PopulateGroundWaterPitman(APitmanSoilMoistureCapacity, APitmanSoilMoistureStorageCapacity,
                                                 APitmansoilMoistureFlowState, APitmanSoilMoistureFlowEquation,
                                                 APitmanMaximumGroundwaterFlow, APitmanSoilMoistureRechargeEquation,
                                                 APitmanGroundwaterFlow: Double);
const OPNAME = 'TGroundWater.PopulateGroundWaterPitman';
begin
  try
    FPitmanSoilMoistureCapacity         := APitmanSoilMoistureCapacity;
    FPitmanSoilMoistureStorageCapacity  := APitmanSoilMoistureStorageCapacity;
    FPitmansoilMoistureFlowState        := APitmansoilMoistureFlowState;
    FPitmanSoilMoistureFlowEquation     := APitmanSoilMoistureFlowEquation;
    FPitmanMaximumGroundwaterFlow       := APitmanMaximumGroundwaterFlow;
    FPitmanSoilMoistureRechargeEquation := APitmanSoilMoistureRechargeEquation;
    FPitmanGroundwaterFlow              := APitmanGroundwaterFlow;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWater.PopulateGroundWaterSubCatchment(ARefNodeNumber,
                                                       AAquiferInflowChannelNr,
                                                       AInflowFromUpstreamAquiferChannelNr,
                                                       AOutflowToDownstreamAquiferChannelNr,
                                                       AAquiferExcessInterflowChannelNr,
                                                       AGroundWaterBaseflowChannelNr,
                                                       AAbstractionFromAquiferChannelNr,
                                                       AAbstractionFromBaseflowChannelNr,
                                                       AGroundWaterBaseFlowRemainderChannelNr,
                                                       ASurfaceRunoffAndSoilInterflowChannelNr,
                                                       AGroundWaterAbstractionChannelNr,
                                                       AOutflowToNetworkChannelNr,
                                                       ABaseFlowNodeNr,
                                                       AAbstractionNodeNr,
                                                       ACollectionNodeNr: Integer);
const OPNAME = 'TGroundWater.PopulateGroundWaterSubCatchment';
begin
  try
    FRefNodeNumber                             := ARefNodeNumber;
    FAquiferInflowChannelNr                    := AAquiferInflowChannelNr;
    FInflowFromUpstreamAquiferChannelNr        := AInflowFromUpstreamAquiferChannelNr;
    FOutflowToDownstreamAquiferChannelNr       := AOutflowToDownstreamAquiferChannelNr;
    FAquiferExcessInterflowChannelNr           := AAquiferExcessInterflowChannelNr;
    FGroundWaterBaseflowChannelNr              := AGroundWaterBaseflowChannelNr;
    FAbstractionFromAquiferChannelNr           := AAbstractionFromAquiferChannelNr;
    FAbstractionFromBaseflowChannelNr          := AAbstractionFromBaseflowChannelNr;
    FGroundWaterBaseFlowRemainderChannelNr     := AGroundWaterBaseFlowRemainderChannelNr;
    FSurfaceRunoffAndSoilInterflowChannelNr    := ASurfaceRunoffAndSoilInterflowChannelNr;
    FGroundWaterAbstractionChannelNr           := AGroundWaterAbstractionChannelNr;
    FOutflowToNetworkChannelNr                 := AOutflowToNetworkChannelNr;
    FBaseFlowNodeNr                            := ABaseFlowNodeNr;
    FAbstractionNodeNr                         := AAbstractionNodeNr;
    FCollectionNodeNr                          := ACollectionNodeNr;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWater.Get_AquiferNode: IReservoirData;
const OPNAME = 'TGroundWater.Get_AquiferNode';
begin
  Result := nil;
  try
    Result := (FAppModules.Model.ModelData as IYieldModelData).
              NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[FAquiferNodeNumber];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.Get_AquiferNodeNr: Integer;
const OPNAME = 'TGroundWater.Get_AquiferNodeNr';
begin
  Result := NullInteger;
  try
    Result := FAquiferNodeNumber;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.ValidateGroundWaterDescription(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TGroundWater.ValidateGroundWaterDescription';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('GroundWaterDescription', FDescription, LMessage)) then
      AErrorMessages.Add(FDescription + ':' + LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.ValidateAquiferStaticWaterLevel(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TGroundWater.ValidateAquiferStaticWaterLevel';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('AquiferStaticWaterlevel', FloatToStr(FAquiferStaticWaterLevel), LMessage)) then
      AErrorMessages.Add(FloatToStr(FAquiferStaticWaterLevel) + ':' + LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.ValidateAquiferStorativity(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TGroundWater.ValidateAquiferStorativity';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('AquiferStorativity', FloatToStr(FAquiferStorativity), LMessage)) then
      AErrorMessages.Add(FloatToStr(FAquiferStorativity) + ':' + LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.ValidateAquiferTransmissivity(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TGroundWater.ValidateAquiferTransmissivity';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('AquiferTransmissivity', FloatToStr(FAquiferTransmissivity), LMessage)) then
      AErrorMessages.Add(FloatToStr(FAquiferTransmissivity) + ':' + LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.ValidateBoreHoleDistanceToRiver(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TGroundWater.ValidateBoreHoleDistanceToRiver';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('BoreHoleDistanceToRiver', FloatToStr(FBoreHoleDistanceToRiver), LMessage)) then
      AErrorMessages.Add(FloatToStr(FBoreHoleDistanceToRiver) + ':' + LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.ValidateGroundWaterEvaporationArea(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TGroundWater.ValidateGroundWaterEvaporationArea';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('GroundWaterEvaporationArea', FloatToStr(FGroundWaterEvaporationArea), LMessage)) then
      AErrorMessages.Add(FloatToStr(FGroundWaterEvaporationArea) + ':' + LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.ValidateInitialUnsaturatedStorage(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TGroundWater.ValidateInitialUnsaturatedStorage';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('InitialUnsaturatedStorage', FloatToStr(FInitialUnsaturatedStorage), LMessage)) then
      AErrorMessages.Add(FloatToStr(FInitialUnsaturatedStorage) + ':' + LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.ValidateMaximumDischargeRate(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TGroundWater.ValidateMaximumDischargeRate';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('MaximumDischargeRate', FloatToStr(FMaximumDischargeRate), LMessage)) then
      AErrorMessages.Add(FloatToStr(FMaximumDischargeRate) + ':' + LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.ValidateMaximumGroundwaterAbstraction(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TGroundWater.ValidateMaximumGroundwaterAbstraction';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('MaximumGroundwaterAbstraction', FloatToStr(FMaximumGroundwaterAbstraction), LMessage)) then
      AErrorMessages.Add(FloatToStr(FMaximumGroundwaterAbstraction) + ':' + LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.ValidateMaximumHydrologicalGradient(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TGroundWater.ValidateMaximumHydrologicalGradient';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('MaximumHydrologicalGradient', FloatToStr(FMaximumHydrologicalGradient), LMessage)) then
      AErrorMessages.Add(FloatToStr(FMaximumHydrologicalGradient) + ':' + LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.ValidateMaximumRateOfGroundwaterBaseFlow(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TGroundWater.ValidateMaximumRateOfGroundwaterBaseFlow';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('MaximumRateOfGroundwaterBaseFlow', FloatToStr(FMaximumRateOfGroundwaterBaseFlow), LMessage)) then
      AErrorMessages.Add(FloatToStr(FMaximumRateOfGroundwaterBaseFlow) + ':' + LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.ValidateMovingAverageRecharge(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TGroundWater.ValidateMovingAverageRecharge';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('MovingAverageRecharge', FloatToStr(FMovingAverageRecharge), LMessage)) then
      AErrorMessages.Add(FloatToStr(FMovingAverageRecharge) + ':' + LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.ValidateGroundWaterName(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TGroundWater.ValidateGroundWaterName';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('GroundWaterName', FName, LMessage)) then
      AErrorMessages.Add(FName + ':' + LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.ValidateParameterK2(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TGroundWater.ValidateParameterK2';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('ParameterK2', FloatToStr(FParameterK2), LMessage)) then
      AErrorMessages.Add(FloatToStr(FParameterK2) + ':' + LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.ValidateParameterK3(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TGroundWater.ValidateParameterK3';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('ParameterK3', FloatToStr(FParameterK3), LMessage)) then
      AErrorMessages.Add(FloatToStr(FParameterK3) + ':' + LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.ValidatePitmanGroundwaterFlow(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TGroundWater.ValidatePitmanGroundwaterFlow';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('PitmanGroundwaterFlow', FloatToStr(FPitmanGroundwaterFlow), LMessage)) then
      AErrorMessages.Add(FloatToStr(FPitmanGroundwaterFlow) + ':' + LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.ValidatePitmanMaximumGroundwaterFlow(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TGroundWater.ValidatePitmanMaximumGroundwaterFlow';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('PitmanMaximumGroundwaterFlow', FloatToStr(FPitmanMaximumGroundwaterFlow), LMessage)) then
      AErrorMessages.Add(FloatToStr(FPitmanMaximumGroundwaterFlow) + ':' + LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.ValidatePitmanSoilMoistureCapacity(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TGroundWater.ValidatePitmanSoilMoistureCapacity';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('PitmanSoilMoistureCapacity', FloatToStr(FPitmanSoilMoistureCapacity), LMessage)) then
      AErrorMessages.Add(FloatToStr(FPitmanSoilMoistureCapacity) + ':' + LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.ValidatePitmanSoilMoistureFlowEquation(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TGroundWater.ValidatePitmanSoilMoistureFlowEquation';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('PitmanSoilMoistureFlowEquation', FloatToStr(FPitmanSoilMoistureFlowEquation), LMessage)) then
      AErrorMessages.Add(FloatToStr(FPitmanSoilMoistureFlowEquation) + ':' + LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.ValidatePitmansoilMoistureFlowState(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TGroundWater.ValidatePitmansoilMoistureFlowState';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('PitmansoilMoistureFlowState', FloatToStr(FPitmansoilMoistureFlowState), LMessage)) then
      AErrorMessages.Add(FloatToStr(FPitmansoilMoistureFlowState) + ':' + LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.ValidatePitmanSoilMoistureRechargeEquation(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TGroundWater.ValidatePitmanSoilMoistureRechargeEquation';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('PitmanSoilMoistureRechargeEquation', FloatToStr(FPitmanSoilMoistureRechargeEquation), LMessage)) then
      AErrorMessages.Add(FloatToStr(FPitmanSoilMoistureRechargeEquation) + ':' + LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.ValidatePitmanSoilMoistureStorageCapacity(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TGroundWater.ValidatePitmanSoilMoistureStorageCapacity';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('PitmanSoilMoistureStorageCapacity', FloatToStr(FPitmanSoilMoistureStorageCapacity), LMessage)) then
      AErrorMessages.Add(FloatToStr(FPitmanSoilMoistureStorageCapacity) + ':' + LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.ValidatePowerHeadDifferenceBaseFlowEquation(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TGroundWater.ValidatePowerHeadDifferenceBaseFlowEquation';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('PowerHeadDifferenceBaseFlowEquation', FloatToStr(FPowerHeadDifferenceBaseFlowEquation), LMessage)) then
      AErrorMessages.Add(FloatToStr(FPowerHeadDifferenceBaseFlowEquation) + ':' + LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.ValidateRefNodeNumber(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TGroundWater.ValidateRefNodeNumber';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('GroundWaterRefNodeNumber', FloatToStr(FRefNodeNumber), LMessage)) then
      AErrorMessages.Add(FloatToStr(FRefNodeNumber) + ':' + LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWater.ValidateUnsaturatedStorageCapacity(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TGroundWater.ValidateUnsaturatedStorageCapacity';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('UnsaturatedStorageCapacity', FloatToStr(FUnsaturatedStorageCapacity), LMessage)) then
      AErrorMessages.Add(FloatToStr(FUnsaturatedStorageCapacity) + ':' + LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWater.Assign(ASource: TGroundWater);
const OPNAME = 'TGroundWater.Assign';
var
  LIndex                                     : integer;
  LDestAquiferNode                           : TReservoirData;
  LSourceAquiferNode                         : TReservoirData;
  LDestAbstractionNode                       : TReservoirData;
  LSourceAbstractionNode                     : TReservoirData;
  LDestCollectionNode                        : TReservoirData;
  LSourceCollectionNode                      : TReservoirData;
  LDestBaseFlowNode                          : TReservoirData;
  LSourceBaseFlowNode                        : TReservoirData;

  LChannelList                               : IChannelList;
  LChannelPenalty                            : IChannelPenalty;
  LSourceAquiferInflowChannel                : IGeneralFlowChannel;
  LDestAquiferInflowChannel                  : IGeneralFlowChannel;
  LSourceInflowFromUpstreamAquiferChannel    : IGeneralFlowChannel;
  LDestInflowFromUpstreamAquiferChannel      : IGeneralFlowChannel;
  LSourceOutflowToDownstreamAquiferChannel   : IGeneralFlowChannel;
  LDestOutflowToDownstreamAquiferChannel     : IGeneralFlowChannel;
  LSourceGroundWaterBaseflowChannel          : IGeneralFlowChannel;
  LDestGroundWaterBaseflowChannel            : IGeneralFlowChannel;
  LSourceAbstractionFromBaseFlowChannel      : IGeneralFlowChannel;
  LDestAbstractionFromBaseFlowChannel        : IGeneralFlowChannel;
  LSourceAbstractionFromAquiferChannel       : IGeneralFlowChannel;
  LDestAbstractionFromAquiferChannel         : IGeneralFlowChannel;
  LSourceAquiferExcessInterflowChannel       : IGeneralFlowChannel;
  LDestAquiferExcessInterflowChannel         : IGeneralFlowChannel;
  LSourceSurfaceRunoffChannel                : IGeneralFlowChannel;
  LDestSurfaceRunoffChannel                  : IGeneralFlowChannel;
  LSourceGroundWaterBaseFlowRemainderChannel : IGeneralFlowChannel;
  LDestGroundWaterBaseFlowRemainderChannel   : IGeneralFlowChannel;
  LSourceGroundWaterAbstractionChannel       : IGeneralFlowChannel;
  LDestGroundWaterAbstractionChannel         : IGeneralFlowChannel;
  LSourceOutflowToNetworkChannel             : IGeneralFlowChannel;
  LDestOutflowToNetworkChannel               : IGeneralFlowChannel;
begin
  try
    Name                                := 'Copy of ' + ASource.Name;
    AquiferStorativity                  := ASource.AquiferStorativity;
    AquiferStaticWaterLevel             := ASource.AquiferStaticWaterLevel;
    UnsaturatedStorageCapacity          := ASource.UnsaturatedStorageCapacity;
    InitialUnsaturatedStorage           := ASource.InitialUnsaturatedStorage;
    MaximumDischargeRate                := ASource.MaximumDischargeRate;
    MovingAverageRecharge               := ASource.MovingAverageRecharge;
    PitmanSoilMoistureCapacity          := ASource.PitmanSoilMoistureCapacity;
    PitmanSoilMoistureStorageCapacity   := ASource.PitmanSoilMoistureStorageCapacity;
    PitmansoilMoistureFlowState         := ASource.PitmansoilMoistureFlowState;
    PitmanSoilMoistureFlowEquation      := ASource.PitmanSoilMoistureFlowEquation;
    PitmanMaximumGroundwaterFlow        := ASource.PitmanMaximumGroundwaterFlow;
    PitmanSoilMoistureRechargeEquation  := ASource.PitmanSoilMoistureRechargeEquation;
    PitmanGroundwaterFlow               := ASource.PitmanGroundwaterFlow;
    MaximumRateOfGroundwaterBaseFlow    := ASource.MaximumRateOfGroundwaterBaseFlow;
    PowerHeadDifferenceBaseFlowEquation := ASource.PowerHeadDifferenceBaseFlowEquation;
    MaximumHydrologicalGradient         := ASource.MaximumHydrologicalGradient;
    AquiferTransmissivity               := ASource.AquiferTransmissivity;
    BoreHoleDistanceToRiver             := ASource.BoreHoleDistanceToRiver;
    MaximumGroundwaterAbstraction       := ASource.MaximumGroundwaterAbstraction;
    ParameterK2                         := ASource.ParameterK2;
    ParameterK3                         := ASource.ParameterK3;
    GroundWaterEvaporationArea          := ASource.GroundWaterEvaporationArea;

    for LIndex := 1 to 12 do
    begin
      if(ASource.MonthlyWaterEvaporation[LIndex] <> nullfloat) then
        MonthlyWaterEvaporation[LIndex] := ASource.MonthlyWaterEvaporation[LIndex];
    end;
    for LIndex := 1 to 12 do
    begin
      if(ASource.MonthlyWaterUsageFactors[LIndex] <> nullfloat) then
        MonthlyWaterUsageFactors[LIndex] := ASource.MonthlyWaterUsageFactors[LIndex];
    end;

    LSourceAquiferNode := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                          CastReservoirList.CastReservoirOrNodeByIdentifier[ASource.FAquiferNodeNumber];
    LDestAquiferNode   := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                          CastReservoirList.CastReservoirOrNodeByIdentifier[FAquiferNodeNumber];
    if((LDestAquiferNode <> nil) and (LSourceAquiferNode <> nil)) then
      LDestAquiferNode.Assign(LSourceAquiferNode);


    LSourceAbstractionNode := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                              CastReservoirList.CastReservoirOrNodeByIdentifier[ASource.FAbstractionNodeNr];
    LDestAbstractionNode   := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                              CastReservoirList.CastReservoirOrNodeByIdentifier[FAbstractionNodeNr];
    if((LDestAbstractionNode <> nil) and (LSourceAbstractionNode <> nil)) then
      LDestAbstractionNode.Assign(LSourceAbstractionNode);
      

    LSourceCollectionNode := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                             CastReservoirList.CastReservoirOrNodeByIdentifier[ASource.FCollectionNodeNr];
    LDestCollectionNode   := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                             CastReservoirList.CastReservoirOrNodeByIdentifier[FCollectionNodeNr];
    if((LDestCollectionNode <> nil) and (LSourceCollectionNode <> nil)) then
      LDestCollectionNode.Assign(LSourceCollectionNode);


    LSourceBaseFlowNode := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                           CastReservoirList.CastReservoirOrNodeByIdentifier[ASource.FBaseFlowNodeNr];
    LDestBaseFlowNode   := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                           CastReservoirList.CastReservoirOrNodeByIdentifier[FBaseFlowNodeNr];
    if((LDestBaseFlowNode <> nil) and (LSourceBaseFlowNode <> nil)) then
      LDestBaseFlowNode.Assign(LSourceBaseFlowNode);

    LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList;
    if(LChannelList <> nil) then
    begin
      LDestAquiferInflowChannel   := LChannelList.ChannelByChannelNumber[FAquiferInflowChannelNr];
      LSourceAquiferInflowChannel := LChannelList.ChannelByChannelNumber[ASource.AquiferInflowChannelNr];
      if((LDestAquiferInflowChannel <> nil) and (LSourceAquiferInflowChannel <> nil)) then
      begin
        LDestAquiferInflowChannel.ChannelName := 'Copy of '+ LSourceAquiferInflowChannel.ChannelName;
        LDestAquiferInflowChannel.ChannelType := LSourceAquiferInflowChannel.ChannelType;
        LDestAquiferInflowChannel.ChannelSubType := LSourceAquiferInflowChannel.ChannelSubType;
        LChannelPenalty := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelPenaltyList.
                            ChannelPenaltyByIdentifier[LSourceAquiferInflowChannel.ChannelPenaltyNumber];
        if(LChannelPenalty <> nil) then
          LDestAquiferInflowChannel.ChannelPenalty := LChannelPenalty;
      end;

      LDestInflowFromUpstreamAquiferChannel   := LChannelList.ChannelByChannelNumber[FInflowFromUpstreamAquiferChannelNr];
      LSourceInflowFromUpstreamAquiferChannel := LChannelList.ChannelByChannelNumber[ASource.InflowFromUpstreamAquiferChannelNr];
      if((LDestInflowFromUpstreamAquiferChannel <> nil) and (LSourceInflowFromUpstreamAquiferChannel <> nil)) then
      begin
        LDestInflowFromUpstreamAquiferChannel.ChannelName    := 'Copy of '+ LSourceInflowFromUpstreamAquiferChannel.ChannelName;
        LDestInflowFromUpstreamAquiferChannel.ChannelType    := LSourceInflowFromUpstreamAquiferChannel.ChannelType;
        LDestInflowFromUpstreamAquiferChannel.ChannelSubType := LSourceInflowFromUpstreamAquiferChannel.ChannelSubType;
        LChannelPenalty := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelPenaltyList.
                            ChannelPenaltyByIdentifier[LSourceInflowFromUpstreamAquiferChannel.ChannelPenaltyNumber];
        if(LChannelPenalty <> nil) then
          LDestInflowFromUpstreamAquiferChannel.ChannelPenalty := LChannelPenalty;
      end;

      LDestOutflowToDownstreamAquiferChannel   := LChannelList.ChannelByChannelNumber[FOutflowToDownstreamAquiferChannelNr];
      LSourceOutflowToDownstreamAquiferChannel := LChannelList.ChannelByChannelNumber[ASource.OutflowToDownstreamAquiferChannelNr];
      if((LDestOutflowToDownstreamAquiferChannel <> nil) and (LSourceOutflowToDownstreamAquiferChannel <> nil)) then
      begin
        LDestOutflowToDownstreamAquiferChannel.ChannelName    := 'Copy of '+ LSourceOutflowToDownstreamAquiferChannel.ChannelName;
        LDestOutflowToDownstreamAquiferChannel.ChannelType    := LSourceOutflowToDownstreamAquiferChannel.ChannelType;
        LDestOutflowToDownstreamAquiferChannel.ChannelSubType := LSourceOutflowToDownstreamAquiferChannel.ChannelSubType;
        LChannelPenalty := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelPenaltyList.
                            ChannelPenaltyByIdentifier[LSourceOutflowToDownstreamAquiferChannel.ChannelPenaltyNumber];
        if(LChannelPenalty <> nil) then
          LDestOutflowToDownstreamAquiferChannel.ChannelPenalty := LChannelPenalty;
      end;

      LDestGroundWaterBaseflowChannel   := LChannelList.ChannelByChannelNumber[FGroundWaterBaseflowChannelNr];
      LSourceGroundWaterBaseflowChannel := LChannelList.ChannelByChannelNumber[ASource.GroundWaterBaseflowChannelNr];
      if((LDestGroundWaterBaseflowChannel <> nil) and (LSourceGroundWaterBaseflowChannel <> nil)) then
      begin
        LDestGroundWaterBaseflowChannel.ChannelName    := 'Copy of '+ LSourceGroundWaterBaseflowChannel.ChannelName;
        LDestGroundWaterBaseflowChannel.ChannelType    := LSourceGroundWaterBaseflowChannel.ChannelType;
        LDestGroundWaterBaseflowChannel.ChannelSubType := LSourceGroundWaterBaseflowChannel.ChannelSubType;
        LChannelPenalty := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelPenaltyList.
                            ChannelPenaltyByIdentifier[LSourceGroundWaterBaseflowChannel.ChannelPenaltyNumber];
        if(LChannelPenalty <> nil) then
          LDestGroundWaterBaseflowChannel.ChannelPenalty := LChannelPenalty;
      end;

      LDestAbstractionFromAquiferChannel   := LChannelList.ChannelByChannelNumber[FAbstractionFromAquiferChannelNr];
      LSourceAbstractionFromAquiferChannel := LChannelList.ChannelByChannelNumber[ASource.AbstractionFromAquiferChannelNr];
      if((LDestAbstractionFromAquiferChannel <> nil) and (LSourceAbstractionFromAquiferChannel <> nil)) then
      begin
        LDestAbstractionFromAquiferChannel.ChannelName    := 'Copy of '+ LSourceAbstractionFromAquiferChannel.ChannelName;
        LDestAbstractionFromAquiferChannel.ChannelType    := LSourceAbstractionFromAquiferChannel.ChannelType;
        LDestAbstractionFromAquiferChannel.ChannelSubType := LSourceAbstractionFromAquiferChannel.ChannelSubType;
        LChannelPenalty := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelPenaltyList.
                            ChannelPenaltyByIdentifier[LSourceAbstractionFromAquiferChannel.ChannelPenaltyNumber];
        if(LChannelPenalty <> nil) then
          LDestAbstractionFromAquiferChannel.ChannelPenalty := LChannelPenalty;
      end;

      LDestAbstractionFromBaseFlowChannel   := LChannelList.ChannelByChannelNumber[FAbstractionFromBaseflowChannelNr];
      LSourceAbstractionFromBaseFlowChannel := LChannelList.ChannelByChannelNumber[ASource.AbstractionFromBaseflowChannelNr];
      if((LDestAbstractionFromBaseFlowChannel <> nil) and (LSourceAbstractionFromBaseFlowChannel <> nil)) then
      begin
        LDestAbstractionFromBaseFlowChannel.ChannelName    := 'Copy of '+ LSourceAbstractionFromBaseFlowChannel.ChannelName;
        LDestAbstractionFromBaseFlowChannel.ChannelType    := LSourceAbstractionFromBaseFlowChannel.ChannelType;
        LDestAbstractionFromBaseFlowChannel.ChannelSubType := LSourceAbstractionFromBaseFlowChannel.ChannelSubType;
        LChannelPenalty := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelPenaltyList.
                            ChannelPenaltyByIdentifier[LSourceAbstractionFromBaseFlowChannel.ChannelPenaltyNumber];
        if(LChannelPenalty <> nil) then
          LDestAbstractionFromBaseFlowChannel.ChannelPenalty := LChannelPenalty;
      end;

      LDestAquiferExcessInterflowChannel   := LChannelList.ChannelByChannelNumber[FAquiferExcessInterflowChannelNr];
      LSourceAquiferExcessInterflowChannel := LChannelList.ChannelByChannelNumber[ASource.AquiferExcessInterflowChannelNr];
      if((LDestAquiferExcessInterflowChannel <> nil) and (LSourceAquiferExcessInterflowChannel <> nil)) then
      begin
        LDestAquiferExcessInterflowChannel.ChannelName    := 'Copy of '+ LSourceAquiferExcessInterflowChannel.ChannelName;
        LDestAquiferExcessInterflowChannel.ChannelType    := LSourceAquiferExcessInterflowChannel.ChannelType;
        LDestAquiferExcessInterflowChannel.ChannelSubType := LSourceAquiferExcessInterflowChannel.ChannelSubType;
        LChannelPenalty := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelPenaltyList.
                            ChannelPenaltyByIdentifier[LSourceAquiferExcessInterflowChannel.ChannelPenaltyNumber];
        if(LChannelPenalty <> nil) then
          LDestAquiferExcessInterflowChannel.ChannelPenalty := LChannelPenalty;
      end;

      LDestSurfaceRunoffChannel   := LChannelList.ChannelByChannelNumber[FSurfaceRunoffAndSoilInterflowChannelNr];
      LSourceSurfaceRunoffChannel := LChannelList.ChannelByChannelNumber[ASource.SurfaceRunoffAndSoilInterflowChannelNr];
      if((LDestSurfaceRunoffChannel <> nil) and (LSourceSurfaceRunoffChannel <> nil)) then
      begin
        LDestSurfaceRunoffChannel.ChannelName    := 'Copy of '+ LSourceSurfaceRunoffChannel.ChannelName;
        LDestSurfaceRunoffChannel.ChannelType    := LSourceSurfaceRunoffChannel.ChannelType;
        LDestSurfaceRunoffChannel.ChannelSubType := LSourceSurfaceRunoffChannel.ChannelSubType;
        LChannelPenalty := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelPenaltyList.
                            ChannelPenaltyByIdentifier[LSourceSurfaceRunoffChannel.ChannelPenaltyNumber];
        if(LChannelPenalty <> nil) then
          LDestSurfaceRunoffChannel.ChannelPenalty := LChannelPenalty;
      end;

      LDestGroundWaterBaseFlowRemainderChannel   := LChannelList.ChannelByChannelNumber[FGroundWaterBaseFlowRemainderChannelNr];
      LSourceGroundWaterBaseFlowRemainderChannel := LChannelList.ChannelByChannelNumber[ASource.FGroundWaterBaseFlowRemainderChannelNr];
      if((LDestGroundWaterBaseFlowRemainderChannel <> nil) and (LSourceGroundWaterBaseFlowRemainderChannel <> nil)) then
      begin
        LDestGroundWaterBaseFlowRemainderChannel.ChannelName    := 'Copy of '+ LSourceGroundWaterBaseFlowRemainderChannel.ChannelName;
        LDestGroundWaterBaseFlowRemainderChannel.ChannelType    := LSourceGroundWaterBaseFlowRemainderChannel.ChannelType;
        LDestGroundWaterBaseFlowRemainderChannel.ChannelSubType := LSourceGroundWaterBaseFlowRemainderChannel.ChannelSubType;
        LChannelPenalty := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelPenaltyList.
                            ChannelPenaltyByIdentifier[LSourceGroundWaterBaseFlowRemainderChannel.ChannelPenaltyNumber];
        if(LChannelPenalty <> nil) then
          LDestGroundWaterBaseFlowRemainderChannel.ChannelPenalty := LChannelPenalty;
      end;
      
      LDestGroundWaterAbstractionChannel   := LChannelList.ChannelByChannelNumber[FGroundWaterAbstractionChannelNr];
      LSourceGroundWaterAbstractionChannel := LChannelList.ChannelByChannelNumber[ASource.FGroundWaterAbstractionChannelNr];
      if((LDestGroundWaterAbstractionChannel <> nil) and (LSourceGroundWaterAbstractionChannel <> nil)) then
      begin
        LDestGroundWaterAbstractionChannel.ChannelName    := 'Copy of '+ LSourceGroundWaterAbstractionChannel.ChannelName;
        LDestGroundWaterAbstractionChannel.ChannelType    := LSourceGroundWaterAbstractionChannel.ChannelType;
        LDestGroundWaterAbstractionChannel.ChannelSubType := LSourceGroundWaterAbstractionChannel.ChannelSubType;
        LChannelPenalty := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelPenaltyList.
                            ChannelPenaltyByIdentifier[LSourceGroundWaterAbstractionChannel.ChannelPenaltyNumber];
        if(LChannelPenalty <> nil) then
          LDestGroundWaterAbstractionChannel.ChannelPenalty := LChannelPenalty;
      end;

      LDestOutflowToNetworkChannel   := LChannelList.ChannelByChannelNumber[FOutflowToNetworkChannelNr];
      LSourceOutflowToNetworkChannel := LChannelList.ChannelByChannelNumber[ASource.OutflowToNetworkChannelNr];
      if((LDestOutflowToNetworkChannel <> nil) and (LSourceOutflowToNetworkChannel <> nil)) then
      begin
        LDestOutflowToNetworkChannel.ChannelName    := 'Copy of '+ LSourceOutflowToNetworkChannel.ChannelName;
        LDestOutflowToNetworkChannel.ChannelType    := LSourceOutflowToNetworkChannel.ChannelType;
        LDestOutflowToNetworkChannel.ChannelSubType := LSourceOutflowToNetworkChannel.ChannelSubType;
        LChannelPenalty := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelPenaltyList.
                            ChannelPenaltyByIdentifier[LSourceOutflowToNetworkChannel.ChannelPenaltyNumber];
        if(LChannelPenalty <> nil) then
          LDestOutflowToNetworkChannel.ChannelPenalty := LChannelPenalty;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


{ TGroundWaterList }

function TGroundWaterList._AddRef: Integer;
const OPNAME = 'TGroundWaterList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWaterList._Release: Integer;
const OPNAME = 'TGroundWaterList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWaterList.CreateGroundWater: IGroundWater;
const OPNAME = 'TGroundWaterList.CreateGroundWater';
var
  lGroundWater : TGroundWater;
begin
  Result := nil;
  try
    lGroundWater := CreateNewGroundWater;
    Result := lGroundWater;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWaterList.CreateMemberObjects;
const OPNAME = 'TGroundWaterList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FGroundWaterList := TObjectList.Create;
 except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGroundWaterList.DestroyMemberObjects;
const OPNAME = 'TGroundWaterList.DestroyMemberObjects';
begin
  try
    FreeAndNil(FGroundWaterList);
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWaterList.Get_GroundWaterCount: Integer;
const OPNAME = 'TGroundWaterList.Get_GroundWaterCount';
begin
  Result := 0;
  try
    Result := FGroundWaterList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;  

function TGroundWaterList.Get_CastGroundWaterByID(AGroundWaterID: integer): TGroundWater;
const OPNAME = 'TGroundWaterList.Get_CastGroundWaterByID';
var
 LIndex       : Integer;
 LGroundWater : TGroundWater;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) and (LIndex < FGroundWaterList.Count)) do
    begin
      LGroundWater := TGroundWater(FGroundWaterList.Items[LIndex]);
      if (LGroundWater.FIdentifier = AGroundWaterID) then
        Result := LGroundWater
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWaterList.CastGroundWaterByAquiferNodeNumber(AquiferNodeNr: Integer): TGroundWater;
const OPNAME = 'TGroundWaterList.CastGroundWaterByAquiferNodeNumber';
var
  LIndex       : Integer;
  LGroundWater : TGroundWater;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) and (LIndex < FGroundWaterList.Count)) do
    begin
      LGroundWater := TGroundWater(FGroundWaterList.Items[LIndex]);
      if (LGroundWater.AquiferNodeNr = AquiferNodeNr) then
        Result := LGroundWater
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWaterList.CreateNewGroundWater: TGroundWater;
const OPNAME = 'TGroundWaterList.CreateNewGroundWater';
var
  LSQLAgent        : TGroundWaterSQLAgent;
  LGroundWater     : TGroundWater;

  LAquiferNode     : IReservoirData;
  LAbstractionNode : IReservoirData;
  LCollectionNode  : IReservoirData;
  LBaseFlowNode    : IReservoirData;

  LAquiferInflowChannel                : IGeneralFlowChannel;
  LInflowFromUpstreamAquiferChannel    : IGeneralFlowChannel;
  LOutflowToDownstreamAquiferChannel   : IGeneralFlowChannel;
  LGroundWaterBaseflowChannel          : IGeneralFlowChannel;
  LAbstractionFromAquiferChannel       : IGeneralFlowChannel;
  LAquiferExcessInterflowChannel       : IGeneralFlowChannel;
  LAbstractionFromBaseFlowChannel      : IGeneralFlowChannel;
  LSurfaceRunoffChannel                : IGeneralFlowChannel;
  LGroundWaterBaseFlowRemainderChannel : IGeneralFlowChannel;
  LGroundWaterAbstractionChannel       : IGeneralFlowChannel;
  LOutflowToNetworkChannel             : IGeneralFlowChannel;

  LAquiferNodeNumber     : integer;
  LAbstractionNodeNumber : integer;
  LCollectionNodeNumber  : integer;
  LBaseFlowNodeNumber    : integer;

  LNewGroundWaterID      : integer;
  LIdentifier            : integer;
  LContinue              : boolean;
begin
  Result := nil;
  try
    if(FGroundWaterList.Count >= 100) then
      Exit;

    LGroundWater     := nil;
    LAquiferNode     := nil;
    LAbstractionNode := nil;
    LCollectionNode  := nil;
    LBaseFlowNode    := nil;

    LAquiferInflowChannel                := nil;
    LInflowFromUpstreamAquiferChannel    := nil;
    LOutflowToDownstreamAquiferChannel   := nil;
    LGroundWaterBaseflowChannel          := nil;
    LAbstractionFromAquiferChannel       := nil;
    LAquiferExcessInterflowChannel       := nil;
    LAbstractionFromBaseFlowChannel      := nil;
    LSurfaceRunoffChannel                := nil;
    LGroundWaterBaseFlowRemainderChannel := nil;
    LGroundWaterAbstractionChannel       := nil;
    LOutflowToNetworkChannel             := nil;

    LAquiferNodeNumber     := 0;
    LAbstractionNodeNumber := 0;
    LCollectionNodeNumber  := 0;
    LBaseFlowNodeNumber    := 0;
    LContinue := True;

    //Create Aquifer node
    if LContinue  then
    begin
      LAquiferNode := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.CreateReservoir(ntGroundWater);
      LContinue := (LAquiferNode <> nil);
      if LContinue then
        LAquiferNodeNumber := LAquiferNode.ReservoirConfigurationData.ReservoirIdentifier;
    end;

    //Create BaseFlow Node
    if LContinue  then
    begin
      LBaseFlowNode      := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.CreateNodeWithoutInflow(ntBaseFlowNode);
      LContinue          := (LBaseFlowNode <> nil);
      if LContinue then
        LBaseFlowNodeNumber := LBaseFlowNode.ReservoirConfigurationData.ReservoirIdentifier;
        LBaseFlowNode.ReservoirConfigurationData.ReservoirName := 'Base Flow Node '+ IntToStr(LBaseFlowNodeNumber);
    end;

    //Create Abstraction Node
    if LContinue  then
    begin
      LAbstractionNode    := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.CreateNodeWithoutInflow(ntAbstractionNode);
      LContinue           := (LAbstractionNode <> nil);
      if LContinue then
        LAbstractionNodeNumber := LAbstractionNode.ReservoirConfigurationData.ReservoirIdentifier;
        LAbstractionNode.ReservoirConfigurationData.ReservoirName := 'Abstraction Node '+ IntToStr(LAbstractionNodeNumber);
    end;

    //Create Collection Node
    if LContinue  then
    begin
      LCollectionNode        := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.CreateNodeWithoutInflow(ntCollectionNode);
      LContinue              := (LCollectionNode <> nil);
      if LContinue then
        LCollectionNodeNumber := LCollectionNode.ReservoirConfigurationData.ReservoirIdentifier;
        LCollectionNode.ReservoirConfigurationData.ReservoirName := 'Collection Node '+ IntToStr(LCollectionNodeNumber);
    end;

    //Create channel to Aquifer Inflow Channel
    if LContinue  then
    begin
      LAquiferInflowChannel := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.CreateChannel;
      LContinue             := (LAquiferInflowChannel <> nil);
      if LContinue then
      begin
        LAquiferInflowChannel.UpStreamNodeNumber   := 0;
        LAquiferInflowChannel.DownStreamNodeNumber := LAquiferNodeNumber;
        LAquiferInflowChannel.ChannelType          := ctAquiferInflowChannel;
        LAquiferInflowChannel.ChannelName          := IntToStr(LAquiferInflowChannel.ChannelNumber)+ ' - Aquifer Inflow '+ IntToStr(LAquiferNodeNumber) +' To Ground Water';
      end;
    end;

    // Create Aquifer Excess Interflow Channel
    if LContinue  then
    begin
      LAquiferExcessInterflowChannel := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.CreateChannel;
      LContinue := (LAquiferExcessInterflowChannel <> nil);
      if LContinue then
      begin
        LAquiferExcessInterflowChannel.UpStreamNodeNumber   := LAquiferNodeNumber;
        LAquiferExcessInterflowChannel.DownStreamNodeNumber := LCollectionNodeNumber;
        LAquiferExcessInterflowChannel.ChannelType          := ctAquiferExcessInterflowChannel;
        LAquiferExcessInterflowChannel.ChannelName          := IntToStr(LAquiferExcessInterflowChannel.ChannelNumber)+ ' - Aquifer Excess Interflow '+ IntToStr(LAquiferNodeNumber);
      end;
    end;

    //Create GroundWater BaseFlow Channel
    if LContinue  then
    begin
      LGroundWaterBaseflowChannel := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.CreateChannel;
      LContinue := (LGroundWaterBaseflowChannel <> nil);
      if LContinue then
      begin
        LGroundWaterBaseflowChannel.UpStreamNodeNumber   := LAquiferNodeNumber;
        LGroundWaterBaseflowChannel.DownStreamNodeNumber := LBaseFlowNodeNumber;
        LGroundWaterBaseflowChannel.ChannelType          := ctGroundWaterBaseflowChannel;
        LGroundWaterBaseflowChannel.ChannelName          := IntToStr(LGroundWaterBaseflowChannel.ChannelNumber)+ ' - GroundWater BaseFlow '+ IntToStr(LAquiferNodeNumber);
      end;
    end;  

    //Create Abstraction From Aquifer Channel
    if LContinue  then
    begin
      LAbstractionFromAquiferChannel := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.CreateChannel;
      LContinue := (LAbstractionFromAquiferChannel <> nil);
      if LContinue then
      begin
        LAbstractionFromAquiferChannel.UpStreamNodeNumber   := LAquiferNodeNumber;
        LAbstractionFromAquiferChannel.DownStreamNodeNumber := LAbstractionNodeNumber;
        LAbstractionFromAquiferChannel.ChannelType          := ctAbstractionFromAquiferChannel;
        LAbstractionFromAquiferChannel.ChannelName          := IntToStr(LAbstractionFromAquiferChannel.ChannelNumber)+ ' - Abstraction From Aquifer '+ IntToStr(LAquiferNodeNumber);
      end;
    end;

    //Create Abstraction From BaseFlow Channel
    if LContinue  then
    begin
      LAbstractionFromBaseFlowChannel := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.CreateChannel;
      LContinue := (LAbstractionFromBaseFlowChannel <> nil);
      if LContinue then
      begin
        LAbstractionFromBaseFlowChannel.UpStreamNodeNumber   := LBaseFlowNodeNumber;
        LAbstractionFromBaseFlowChannel.DownStreamNodeNumber := LAbstractionNodeNumber;
        LAbstractionFromBaseFlowChannel.ChannelType          := ctAbstractionFromBaseFlowChannel;
        LAbstractionFromBaseFlowChannel.ChannelName          := IntToStr(LAbstractionFromBaseFlowChannel.ChannelNumber)+ ' - Abstraction From BaseFlow '+ IntToStr(LBaseFlowNodeNumber);
      end;
    end;

    //Create GroundWater base flow remainder Channel
    if LContinue  then
    begin
      LGroundWaterBaseFlowRemainderChannel := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.CreateChannel;
      LContinue := (LGroundWaterBaseFlowRemainderChannel <> nil);
      if LContinue then
      begin
        LGroundWaterBaseFlowRemainderChannel.UpStreamNodeNumber   := LBaseFlowNodeNumber;
        LGroundWaterBaseFlowRemainderChannel.DownStreamNodeNumber := LCollectionNodeNumber;
        LGroundWaterBaseFlowRemainderChannel.ChannelType          := ctGroundWaterBaseFlowRemainderChannel;
        LGroundWaterBaseFlowRemainderChannel.ChannelName          := IntToStr(LGroundWaterBaseFlowRemainderChannel.ChannelNumber)+ ' - Groundwater Base flow remainder '+ IntToStr(LCollectionNodeNumber);
      end;
    end;

    //Create Inflow From Upstream Aquifer Channel
    if LContinue then
    begin
      LInflowFromUpstreamAquiferChannel := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.CreateChannel;
      LContinue := (LInflowFromUpstreamAquiferChannel <> nil);
      if LContinue then
      begin
        LInflowFromUpstreamAquiferChannel.UpStreamNodeNumber   := 0;
        LInflowFromUpstreamAquiferChannel.DownStreamNodeNumber := LAquiferNodeNumber;
        LInflowFromUpstreamAquiferChannel.ChannelType          := ctInflowFromUpstreamAquiferChannel;
        LInflowFromUpstreamAquiferChannel.ChannelName          := IntToStr(LInflowFromUpstreamAquiferChannel.ChannelNumber)+ ' - Inflow '+ IntToStr(LAquiferNodeNumber) +' From Upstream Aquifer';
      end;
    end;
    
    //Create channel To OutflowToDownstreamAquiferChannel
    if LContinue  then
    begin
      LOutflowToDownstreamAquiferChannel := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.CreateChannel;
      LContinue := (LOutflowToDownstreamAquiferChannel <> nil);
      if LContinue then
      begin
        LOutflowToDownstreamAquiferChannel.UpStreamNodeNumber   := LAquiferNodeNumber;
        LOutflowToDownstreamAquiferChannel.DownStreamNodeNumber := 0;
        LOutflowToDownstreamAquiferChannel.ChannelType          := ctOutFlowToDownstreamAquiferChannel;
        LOutflowToDownstreamAquiferChannel.ChannelName          := IntToStr(LOutflowToDownstreamAquiferChannel.ChannelNumber)+ ' - Outflow '+ IntToStr(LAquiferNodeNumber) +' To Downstream Aquifer';
      end;
    end;

    //Create Surface Runoff and soil interflow Channel
    if LContinue  then
    begin
      LSurfaceRunoffChannel := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.CreateChannel;
      LContinue := (LSurfaceRunoffChannel <> nil);
      if LContinue  then
      begin
        LSurfaceRunoffChannel.UpStreamNodeNumber   := 0;
        LSurfaceRunoffChannel.DownStreamNodeNumber := LCollectionNodeNumber;
        LSurfaceRunoffChannel.ChannelType          := ctSurfaceRunoffChannel;
        LSurfaceRunoffChannel.ChannelName          := IntToStr(LSurfaceRunoffChannel.ChannelNumber)+ ' - Surface Runoff Channel '+ IntToStr(LCollectionNodeNumber);
      end;
    end;

    //Create GroundWater Abstraction Channel
    if LContinue  then
    begin
      LGroundWaterAbstractionChannel := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.CreateChannel;
      LContinue := (LGroundWaterAbstractionChannel <> nil);
      if LContinue then
      begin
        LGroundWaterAbstractionChannel.UpStreamNodeNumber   := LAbstractionNodeNumber;
        LGroundWaterAbstractionChannel.DownStreamNodeNumber := 0;
        LGroundWaterAbstractionChannel.ChannelType          := ctGroundWaterAbstractionChannel;
        LGroundWaterAbstractionChannel.ChannelName          := IntToStr(LGroundWaterAbstractionChannel.ChannelNumber)+ ' - GroundWater Abstraction From Abstraction '+ IntToStr(LAbstractionNodeNumber);
      end;
    end;

    //Create Outflow To Network Channel
    if LContinue  then
    begin
      LOutflowToNetworkChannel := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.CreateChannel;
      LContinue := (LOutflowToNetworkChannel <> nil);
      if LContinue then
      begin
        LOutflowToNetworkChannel.UpStreamNodeNumber   := LCollectionNodeNumber;
        LOutflowToNetworkChannel.DownStreamNodeNumber := 0;
        LOutflowToNetworkChannel.ChannelType          := ctOutflowToNetworkChannel;
        LOutflowToNetworkChannel.ChannelName          := IntToStr(LOutflowToNetworkChannel.ChannelNumber)+ ' - Outflow From Collection ' + IntToStr(LCollectionNodeNumber) + ' To Network';
      end;
    end;

    //Create GroundWater
    if LContinue  then
    begin
      LGroundWater := NewGroundWater;
      LContinue    := (LGroundWater <> nil)
    end;

    LSQLAgent := TGroundWaterSQLAgent.Create(FAppModules);
    try
      //Populate GroundWater/channels
      if LContinue  then
      begin
        LNewGroundWaterID                                    := LSQLAgent.GetMaxGroundWaterID + 1;
        LGroundWater.FIdentifier                             := LNewGroundWaterID;
        LGroundWater.FAquiferNodeNumber                      := LAquiferNode.ReservoirConfigurationData.ReservoirIdentifier;
        LGroundWater.FAquiferInflowChannelNr                 := LAquiferInflowChannel.ChannelNumber;
        LGroundWater.FInflowFromUpstreamAquiferChannelNr     := LInflowFromUpstreamAquiferChannel.ChannelNumber;
        LGroundWater.FOutflowToDownstreamAquiferChannelNr    := LOutflowToDownstreamAquiferChannel.ChannelNumber;
        LGroundWater.FAquiferExcessInterflowChannelNr        := LAquiferExcessInterflowChannel.ChannelNumber ;
        LGroundWater.FGroundWaterBaseflowChannelNr           := LGroundWaterBaseflowChannel.ChannelNumber;
        LGroundWater.FAbstractionFromAquiferChannelNr        := LAbstractionFromAquiferChannel.ChannelNumber;
        LGroundWater.FAbstractionFromBaseflowChannelNr       := LAbstractionFromBaseFlowChannel.ChannelNumber;
        LGroundWater.FSurfaceRunoffAndSoilInterflowChannelNr := LSurfaceRunoffChannel.ChannelNumber;
        LGroundWater.FGroundWaterBaseFlowRemainderChannelNr  := LGroundWaterBaseFlowRemainderChannel.ChannelNumber;
        LGroundWater.FGroundWaterAbstractionChannelNr        := LGroundWaterAbstractionChannel.ChannelNumber;
        LGroundWater.FOutflowToNetworkChannelNr              := LOutflowToNetworkChannel.ChannelNumber;

        LGroundWater.FBaseFlowNodeNr                         := LBaseFlowNode.ReservoirConfigurationData.ReservoirIdentifier;
        LGroundWater.FAbstractionNodeNr                      := LAbstractionNode.ReservoirConfigurationData.ReservoirIdentifier;
        LGroundWater.FCollectionNodeNr                       := LCollectionNode.ReservoirConfigurationData.ReservoirIdentifier;

        LGroundWater.FName                                := 'GroundWater '+IntToStr(LNewGroundWaterID);
        LContinue := LSQLAgent.AddGroundWater(LGroundWater);
      end;

      if not LContinue  then
      begin
        if(LGroundWater <> nil) then
        begin
          if LSQLAgent.DeleteGroundWater(LGroundWater) then
          begin
            FGroundWaterList.Remove(LGroundWater);
            LGroundWater := nil;
          end;
        end;

        if(LAquiferNode <> nil) then
        begin
          LIdentifier := LAquiferNode.ReservoirConfigurationData.ReservoirIdentifier;
          LAquiferNode := nil;
          (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.DeleteNodeWithoutInflow(LIdentifier);
        end;

        if(LAbstractionNode <> nil) then
        begin
          LIdentifier := LAbstractionNode.ReservoirConfigurationData.ReservoirIdentifier;
          LAbstractionNode := nil;
          (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.DeleteReservoir(LIdentifier);
        end;

        if(LCollectionNode <> nil) then
        begin
          LIdentifier := LCollectionNode.ReservoirConfigurationData.ReservoirIdentifier;
          LCollectionNode := nil;
          (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.DeleteReservoir(LIdentifier);
        end;

        if(LBaseFlowNode <> nil) then
        begin
          LIdentifier := LBaseFlowNode.ReservoirConfigurationData.ReservoirIdentifier;
          LBaseFlowNode := nil;
          (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.DeleteReservoir(LIdentifier);
        end;

        if(LAquiferInflowChannel <> nil) then
        begin
          LIdentifier := LAquiferInflowChannel.ChannelNumber;
          LAquiferInflowChannel := nil;
          (FAppModules.Model as IYieldModel).DoConvertChannel(LIdentifier);
          (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.RemoveChannelWithNumber(LIdentifier);
        end;

        if(LOutflowToDownstreamAquiferChannel <> nil) then
        begin
          LIdentifier := LOutflowToDownstreamAquiferChannel.ChannelNumber;
          LOutflowToDownstreamAquiferChannel := nil;
          (FAppModules.Model as IYieldModel).DoConvertChannel(LIdentifier);
          (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.RemoveChannelWithNumber(LIdentifier);
        end;

        if(LGroundWaterBaseflowChannel <> nil) then
        begin
          LIdentifier := LGroundWaterBaseflowChannel.ChannelNumber;
          LGroundWaterBaseflowChannel := nil;
          (FAppModules.Model as IYieldModel).DoConvertChannel(LIdentifier);
          (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.RemoveChannelWithNumber(LIdentifier);
        end;

        if(LAbstractionFromAquiferChannel <> nil) then
        begin
          LIdentifier := LAbstractionFromAquiferChannel.ChannelNumber;
          LAbstractionFromAquiferChannel := nil;
          (FAppModules.Model as IYieldModel).DoConvertChannel(LIdentifier);
          (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.RemoveChannelWithNumber(LIdentifier);
        end;

        if(LAquiferExcessInterflowChannel <> nil) then
        begin
          LIdentifier := LAquiferExcessInterflowChannel.ChannelNumber;
          LAquiferExcessInterflowChannel := nil;
          (FAppModules.Model as IYieldModel).DoConvertChannel(LIdentifier);
          (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.RemoveChannelWithNumber(LIdentifier);
        end;

        if(LAbstractionFromBaseFlowChannel <> nil) then
        begin
          LIdentifier := LAbstractionFromBaseFlowChannel.ChannelNumber;
          LAbstractionFromBaseFlowChannel := nil;
          (FAppModules.Model as IYieldModel).DoConvertChannel(LIdentifier);
          (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.RemoveChannelWithNumber(LIdentifier);
        end;

        if(LSurfaceRunoffChannel <> nil) then
        begin
          LIdentifier := LSurfaceRunoffChannel.ChannelNumber;
          LSurfaceRunoffChannel := nil;
          (FAppModules.Model as IYieldModel).DoConvertChannel(LIdentifier);
          (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.RemoveChannelWithNumber(LIdentifier);
        end;

        if(LGroundWaterBaseFlowRemainderChannel <> nil) then
        begin
          LIdentifier := LGroundWaterBaseFlowRemainderChannel.ChannelNumber;
          LGroundWaterBaseFlowRemainderChannel := nil;
          (FAppModules.Model as IYieldModel).DoConvertChannel(LIdentifier);
          (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.RemoveChannelWithNumber(LIdentifier);
        end;

        if(LGroundWaterAbstractionChannel <> nil) then
        begin
          LIdentifier := LGroundWaterAbstractionChannel.ChannelNumber;
          LGroundWaterAbstractionChannel := nil;
          (FAppModules.Model as IYieldModel).DoConvertChannel(LIdentifier);
          (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.RemoveChannelWithNumber(LIdentifier);
        end;
        
        if(LOutflowToNetworkChannel <> nil) then
        begin
          LIdentifier := LOutflowToNetworkChannel.ChannelNumber;
          LOutflowToNetworkChannel := nil;
          (FAppModules.Model as IYieldModel).DoConvertChannel(LIdentifier);
          (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.RemoveChannelWithNumber(LIdentifier);
        end;
      end;
      Result := LGroundWater;
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWaterList.DeleteGroundWaterWithID(AGroundWaterID: integer): WordBool;
const OPNAME = 'TGroundWaterList.DeleteGroundWaterWithID';
var
  LGroundWater : TGroundWater;
  LIndex       : Integer;
begin
  Result := False;
  try
    LGroundWater := Get_CastGroundWaterByID(AGroundWaterID);
    if (LGroundWater <> nil) then
    begin
      LIndex := FGroundWaterList.IndexOf(LGroundWater);
      Result := DeleteGroundWaterWithIndex(LIndex);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWaterList.DeleteGroundWaterWithIndex(AIndex: integer): WordBool;
const OPNAME = 'TGroundWaterList.DeleteGroundWaterWithIndex';
begin
  Result := False;
  try
    if (AIndex >= 0) then
    begin
      FGroundWaterList.Delete(AIndex);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWaterList.Get_GroundWaterByID(AGroundWaterID: Integer): IGroundWater;
const OPNAME = 'TGroundWaterList.Get_GroundWaterByID';
var
  LIndex       : Integer;
  LGroundWater : TGroundWater;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) and (LIndex < FGroundWaterList.Count)) do
    begin
      LGroundWater := TGroundWater(FGroundWaterList.Items[LIndex]);
      if (LGroundWater.FIdentifier = AGroundWaterID) then
        Result := LGroundWater
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWaterList.Get_GroundWaterByIndex(AIndex: Integer): IGroundWater;
const OPNAME = 'TGroundWaterList.Get_GroundWaterByIndex';
begin
  Result := nil;
  try
    if ((AIndex >= 0) and (AIndex < FGroundWaterList.Count)) then
      Result := TGroundWater(FGroundWaterList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWaterList.Get_GroundWaterByNodeNumber(ANodeNumber: Integer): IGroundWater;
const OPNAME = 'TGroundWaterList.Get_GroundWaterByNodeNumber';
var
  LIndex       : Integer;
  LGroundWater : TGroundWater;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) and (LIndex < FGroundWaterList.Count)) do
    begin
      LGroundWater := TGroundWater(FGroundWaterList.Items[LIndex]);
      if (LGroundWater.AquiferNodeNr = ANodeNumber) then
        Result := LGroundWater
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWaterList.GetCastGroundWaterByIndex(AIndex: Integer): TGroundWater;
const OPNAME = 'TGroundWaterList.GetCastGroundWaterByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FGroundWaterList.Count) then
      Result := TGroundWater(FGroundWaterList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWaterList.Initialise: boolean;
const OPNAME = 'TGroundWaterList.Initialise';
begin
  Result := inherited Initialise;
  try
    FGroundWaterList.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWaterList.NewGroundWater: TGroundWater;
const OPNAME = 'TGroundWaterList.NewGroundWater';
begin
  Result := nil;
  try
    Result := TGroundWater.Create(FAppModules);
    FGroundWaterList.Add(Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWaterList.RemoveGroundWater(AGroundWaterID: Integer): WordBool;
const OPNAME = 'TGroundWaterList.RemoveGroundWater';
var
  LGroundWater : TGroundWater;
  LSQLAgent    : TGroundWaterSQLAgent;
begin
  Result := False;
  try
    LSQLAgent := TGroundWaterSQLAgent.Create(FAppModules);
    try
      LGroundWater := Get_CastGroundWaterByID(AGroundWaterID);
      if(LGroundWater <> nil) then
      begin
        (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.DeleteReservoir(LGroundWater.AquiferNodeNr);
        (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.DeleteNodeWithoutInflow(LGroundWater.FBaseFlowNodeNr);
        (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.DeleteNodeWithoutInflow(LGroundWater.FAbstractionNodeNr);
        (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.DeleteNodeWithoutInflow(LGroundWater.FCollectionNodeNr);

        (FAppModules.Model as IYieldModel).DoConvertChannel(LGroundWater.FAquiferInflowChannelNr);
        (FAppModules.Model.ModelData  as IYieldModelData).NetworkElementData.ChannelList.RemoveChannelWithNumber(LGroundWater.FAquiferInflowChannelNr);

        (FAppModules.Model as IYieldModel).DoConvertChannel(LGroundWater.FInflowFromUpstreamAquiferChannelNr);
        (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.RemoveChannelWithNumber(LGroundWater.FInflowFromUpstreamAquiferChannelNr);

        (FAppModules.Model as IYieldModel).DoConvertChannel(LGroundWater.FOutflowToDownstreamAquiferChannelNr);
        (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.RemoveChannelWithNumber(LGroundWater.FOutflowToDownstreamAquiferChannelNr);

        (FAppModules.Model as IYieldModel).DoConvertChannel(LGroundWater.FGroundWaterBaseflowChannelNr);
        (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.RemoveChannelWithNumber(LGroundWater.FGroundWaterBaseflowChannelNr);

        (FAppModules.Model as IYieldModel).DoConvertChannel(LGroundWater.FAbstractionFromAquiferChannelNr);
        (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.RemoveChannelWithNumber(LGroundWater.FAbstractionFromAquiferChannelNr);

        (FAppModules.Model as IYieldModel).DoConvertChannel(LGroundWater.FAbstractionFromBaseflowChannelNr);
        (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.RemoveChannelWithNumber(LGroundWater.FAbstractionFromBaseflowChannelNr);

        (FAppModules.Model as IYieldModel).DoConvertChannel(LGroundWater.FGroundWaterBaseFlowRemainderChannelNr);
        (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.RemoveChannelWithNumber(LGroundWater.FGroundWaterBaseFlowRemainderChannelNr);

        (FAppModules.Model as IYieldModel).DoConvertChannel(LGroundWater.FAquiferExcessInterflowChannelNr);
        (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.RemoveChannelWithNumber(LGroundWater.FAquiferExcessInterflowChannelNr);

        (FAppModules.Model as IYieldModel).DoConvertChannel(LGroundWater.FSurfaceRunoffAndSoilInterflowChannelNr);
        (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.RemoveChannelWithNumber(LGroundWater.FSurfaceRunoffAndSoilInterflowChannelNr);

        (FAppModules.Model as IYieldModel).DoConvertChannel(LGroundWater.FGroundWaterAbstractionChannelNr);
        (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.RemoveChannelWithNumber(LGroundWater.FGroundWaterAbstractionChannelNr);

        (FAppModules.Model as IYieldModel).DoConvertChannel(LGroundWater.FOutflowToNetworkChannelNr);
        (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.RemoveChannelWithNumber(LGroundWater.FOutflowToNetworkChannelNr);

        if (LSQLAgent.DeleteGroundWater(LGroundWater)) then
        begin;
          DeleteGroundWaterWithID(AGroundWaterID);
          Result := True;
        end;
      end;
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWaterList.Validate(var AErrors: WideString;const AContext: WideString): WordBool;
const OPNAME = 'TGroundWaterList.Validate';
var
  LIndex: integer;
begin
  Result := True;
  try
    if (FAppModules.StudyArea.ModelVersion <> '7') then
      Exit;
    for LIndex := 0 to FGroundWaterList.Count - 1 do
    begin
      if not TGroundWater(FGroundWaterList[LIndex]).Validate(AErrors,AContext) then
      begin
        Result := False;
        if FAppModules.GlobalData.StopOnFirstErr then
          Break;
      end
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWaterList.Get_CastGroundWaterByIndex(AIndex: Integer): TGroundWater;
const OPNAME = 'TGroundWaterList.Get_CastGroundWaterByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FGroundWaterList.Count) then
      Result := TGroundWater(FGroundWaterList[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWaterList.Get_GroundWaterByBaseFlowNumber(ABaseFlowNumber: Integer): IGroundWater;
const OPNAME = 'TGroundWaterList.Get_GroundWaterByBaseFlowNumber';
var
  LIndex       : Integer;
  LGroundWater : TGroundWater;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) and (LIndex < FGroundWaterList.Count)) do
    begin
      LGroundWater := TGroundWater(FGroundWaterList.Items[LIndex]);
      if (LGroundWater.BaseFlowNodeNr = ABaseFlowNumber) then
        Result := LGroundWater
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWaterList.Get_GroundWaterByAbstractionNodeNumber(AAbstractionNodeNumber: Integer): IGroundWater;
const OPNAME = 'TGroundWaterList.Get_GroundWaterByAbstractionNodeNumber';
var
  LIndex       : Integer;
  LGroundWater : TGroundWater;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) and (LIndex < FGroundWaterList.Count)) do
    begin
      LGroundWater := TGroundWater(FGroundWaterList.Items[LIndex]);
      if (LGroundWater.AbstractionNodeNr = AAbstractionNodeNumber) then
        Result := LGroundWater
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWaterList.Get_GroundWaterByCollectionNodeNumber(ACollectionNodeNumber: Integer): IGroundWater;
const OPNAME = 'TGroundWaterList.Get_GroundWaterByCollectionNodeNumber';
var
  LIndex       : Integer;
  LGroundWater : TGroundWater;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) and (LIndex < FGroundWaterList.Count)) do
    begin
      LGroundWater := TGroundWater(FGroundWaterList.Items[LIndex]);
      if (LGroundWater.CollectionNodeNr = ACollectionNodeNumber) then
        Result := LGroundWater
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGroundWaterList.CopyGroundwater(AGroundwaterID: Integer): IGroundWater;
const OPNAME = 'TGroundWaterList.CopyGroundwater';
var
  LDestGroundWater   : TGroundWater;
  LSourceGroundWater : TGroundWater;
begin
  Result := nil;
  try
    LSourceGroundWater := CastGroundWaterByID[AGroundwaterID];
    if LSourceGroundWater <> nil then
    begin
      LDestGroundWater := CreateNewGroundWater;
      if LDestGroundWater <> nil then
      begin
        LDestGroundWater.Assign(LSourceGroundWater);
        Result := LDestGroundWater;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
