//
//
//  UNIT      : Contains TGroundWaterFileObject Class
//  AUTHOR    : Presley Mudau(Cornastone)
//  DATE      : 01/10/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UGroundWaterFileObject;

interface

uses
  Classes,
  sysutils,
  contnrs,

  //  DWAF VCL
  UAbstractDataObject,
  UBasicObjects,
  UConstants;

type
  TMonthlyDoubleArray  = array[MinMonths..MaxMonths] of TDouble;

  TGroundWaterFileObject = class(TAbstractDataObject)
  protected
    FIdentifier                             : TInteger;
    FAquiferNodeNumber                      : TInteger;
    FRefNodeNumber                          : TInteger;
    FAquiferInflowChannelNr                 : TInteger;
    FAquiferExcessInterflowChannelNr        : TInteger ;
    FGroundWaterBaseflowChannelNr           : TInteger;
    FAbstractionFromAquiferChannelNr        : TInteger;
    FAbstractionFromBaseflowChannelNr       : TInteger;
    FInflowFromUpstreamAquiferChannelNr     : TInteger;
    FSurfaceRunoffAndSoilInterflowChannelNr : TInteger;
    FGroundWaterBaseFlowRemainderChannelNr  : TInteger;

    FAbstractionNodeNr                      : TInteger;
    FCollectionNodeNr                       : TInteger;
    FBaseFlowNodeNr                         : TInteger;

    FAquiferStorativity                     : TDouble;
    FAquiferStaticWaterLevel                : TDouble;
    FUnsaturatedStorageCapacity             : TDouble;
    FInitialUnsaturatedStorage              : TDouble;
    FMaximumDischargeRate                   : TDouble;
    FMovingAverageRecharge                  : TDouble;
    FPitmanSoilMoistureCapacity             : TDouble;
    FPitmanSoilMoistureStorageCapacity      : TDouble;
    FPitmansoilMoistureFlowState            : TDouble;
    FPitmanSoilMoistureFlowEquation         : TDouble;
    FPitmanMaximumGroundwaterFlow           : TDouble;
    FPitmanSoilMoistureRechargeEquation     : TDouble;
    FPitmanGroundwaterFlow                  : TDouble;
    FMaximumRateOfGroundwaterBaseFlow       : TDouble;
    FPowerHeadDifferenceBaseFlowEquation    : TDouble;
    FMaximumHydrologicalGradient            : TDouble;
    FAquiferTransmissivity                  : TDouble;
    FBoreHoleDistanceToRiver                : TDouble;
    FMaximumGroundwaterAbstraction          : TDouble;
    FParameterK2                            : TDouble;
    FParameterK3                            : TDouble;
    FGroundWaterEvaporationArea             : TDouble;
    FGroundWaterEvaporation                 : TMonthlyDoubleArray;
    FGroundWaterFactors                     : TMonthlyDoubleArray;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset;override;
    function Initialise: boolean;override;

    property AquiferStorativity                  : TDouble  read FAquiferStorativity                  write FAquiferStorativity;
    property AquiferStaticWaterLevel             : TDouble  read FAquiferStaticWaterLevel             write FAquiferStaticWaterLevel;
    property UnsaturatedStorageCapacity          : TDouble  read FUnsaturatedStorageCapacity          write FUnsaturatedStorageCapacity;
    property InitialUnsaturatedStorage           : TDouble  read FInitialUnsaturatedStorage           write FInitialUnsaturatedStorage;
    property MaximumDischargeRate                : TDouble  read FMaximumDischargeRate                write FMaximumDischargeRate;
    property MovingAverageRecharge               : TDouble  read FMovingAverageRecharge               write FMovingAverageRecharge;

    property PitmanSoilMoistureCapacity          : TDouble  read FPitmanSoilMoistureCapacity          write FPitmanSoilMoistureCapacity;
    property PitmanSoilMoistureStorageCapacity   : TDouble  read FPitmanSoilMoistureStorageCapacity   write FPitmanSoilMoistureStorageCapacity;
    property PitmansoilMoistureFlowState         : TDouble  read FPitmansoilMoistureFlowState         write FPitmansoilMoistureFlowState;
    property PitmanSoilMoistureFlowEquation      : TDouble  read FPitmanSoilMoistureFlowEquation      write FPitmanSoilMoistureFlowEquation;
    property PitmanMaximumGroundwaterFlow        : TDouble  read FPitmanMaximumGroundwaterFlow        write FPitmanMaximumGroundwaterFlow;
    property PitmanSoilMoistureRechargeEquation  : TDouble  read FPitmanSoilMoistureRechargeEquation  write FPitmanSoilMoistureRechargeEquation;
    property PitmanGroundwaterFlow               : TDouble  read FPitmanGroundwaterFlow               write FPitmanGroundwaterFlow;
    property MaximumRateOfGroundwaterBaseFlow    : TDouble  read FMaximumRateOfGroundwaterBaseFlow    write FMaximumRateOfGroundwaterBaseFlow;
    property PowerHeadDifferenceBaseFlowEquation : TDouble  read FPowerHeadDifferenceBaseFlowEquation write FPowerHeadDifferenceBaseFlowEquation;
    property MaximumHydrologicalGradient         : TDouble  read FMaximumHydrologicalGradient         write FMaximumHydrologicalGradient;
    property AquiferTransmissivity               : TDouble  read FAquiferTransmissivity               write FAquiferTransmissivity;
    property BoreHoleDistanceToRiver             : TDouble  read FBoreHoleDistanceToRiver             write FBoreHoleDistanceToRiver;
    property MaximumGroundwaterAbstraction       : TDouble  read FMaximumGroundwaterAbstraction       write FMaximumGroundwaterAbstraction;
    property ParameterK2                         : TDouble  read FParameterK2                         write FParameterK2;
    property ParameterK3                         : TDouble  read FParameterK3                         write FParameterK3;
    property GroundWaterEvaporationArea          : TDouble  read FGroundWaterEvaporationArea          write FGroundWaterEvaporationArea;
    property Identifier                          : TInteger read FIdentifier                          write FIdentifier;
    property RefNodeNumber                       : TInteger read FRefNodeNumber                       write FRefNodeNumber;

    property AbstractionNodeNr                   : TInteger read FAbstractionNodeNr                   write FAbstractionNodeNr;
    property CollectionNodeNr                    : TInteger read FCollectionNodeNr                    write FCollectionNodeNr;
    property BaseFlowNodeNr                      : TInteger read FBaseFlowNodeNr                      write FBaseFlowNodeNr;
    property AquiferNodeNumber                   : TInteger read FAquiferNodeNumber                   write FAquiferNodeNumber;

    property AquiferInflowChannelNr                : TInteger read FAquiferInflowChannelNr                 write FAquiferInflowChannelNr;
    property InflowFromUpstreamAquiferChannelNr    : TInteger read FInflowFromUpstreamAquiferChannelNr    write FInflowFromUpstreamAquiferChannelNr;
    property AquiferExcessInterflowChannelNr       : TInteger read FAquiferExcessInterflowChannelNr        write FAquiferExcessInterflowChannelNr;
    property GroundWaterBaseflowChannelNr          : TInteger read FGroundWaterBaseflowChannelNr           write FGroundWaterBaseflowChannelNr;
    property AbstractionFromAquiferChannelNr       : TInteger read FAbstractionFromAquiferChannelNr        write FAbstractionFromAquiferChannelNr;
    property AbstractionFromBaseflowChannelNr      : TInteger read FAbstractionFromBaseflowChannelNr       write FAbstractionFromBaseflowChannelNr;
    property SurfaceRunoffAndSoilInterflowChannelNr: TInteger read FSurfaceRunoffAndSoilInterflowChannelNr write FSurfaceRunoffAndSoilInterflowChannelNr;

    property MonthlyWaterEvaporation  : TMonthlyDoubleArray read FGroundWaterEvaporation write FGroundWaterEvaporation;
    property MonthlyWaterUsageFactors : TMonthlyDoubleArray read FGroundWaterFactors  write FGroundWaterFactors;


  end;

  TGroundWaterListFileObject  = class(TAbstractDataObject)
  protected
    FGroundWaterObjectContainer : TObjectList;
    FComment                    : TStringlist;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetGroundWaterObjectByIndex(AIndex: integer):TGroundWaterFileObject;
  public
    procedure Reset;override;
    function GroundWaterObjectCount: integer;
    function Initialise: boolean;override;
    function AddGroundWaterObject:TGroundWaterFileObject;
    property GroundWaterObjectByIndex[AIndex: integer] :TGroundWaterFileObject read GetGroundWaterObjectByIndex;
    property Comment :TStringlist read FComment;
  end;

implementation


uses UErrorHandlingOperations;

{TGroundWaterObject}

procedure TGroundWaterFileObject.CreateMemberObjects;
const OPNAME = 'TGroundWaterFileObject.CreateMemberObjects';
var
  LIndex : integer;
Begin
  try
    FIdentifier                             := TInteger.Create;
    FAquiferNodeNumber                      := TInteger.Create;
    FRefNodeNumber                          := TInteger.Create;
    FAquiferInflowChannelNr                 := TInteger.Create;
    FAquiferExcessInterflowChannelNr        := TInteger.Create ;
    FGroundWaterBaseflowChannelNr           := TInteger.Create;
    FAbstractionFromAquiferChannelNr        := TInteger.Create;
    FAbstractionFromBaseflowChannelNr       := TInteger.Create;
    FInflowFromUpstreamAquiferChannelNr     := TInteger.Create;
    FSurfaceRunoffAndSoilInterflowChannelNr := TInteger.Create;
    FGroundWaterBaseFlowRemainderChannelNr  := TInteger.Create;

    FAbstractionNodeNr                      := TInteger.Create;
    FCollectionNodeNr                       := TInteger.Create;
    FBaseFlowNodeNr                         := TInteger.Create;

    FAquiferStorativity                     := TDouble.Create;
    FAquiferStaticWaterLevel                := TDouble.Create;
    FUnsaturatedStorageCapacity             := TDouble.Create;
    FInitialUnsaturatedStorage              := TDouble.Create;
    FMaximumDischargeRate                   := TDouble.Create;
    FMovingAverageRecharge                  := TDouble.Create;
    FPitmanSoilMoistureCapacity             := TDouble.Create;
    FPitmanSoilMoistureStorageCapacity      := TDouble.Create;
    FPitmansoilMoistureFlowState            := TDouble.Create;
    FPitmanSoilMoistureFlowEquation         := TDouble.Create;
    FPitmanMaximumGroundwaterFlow           := TDouble.Create;
    FPitmanSoilMoistureRechargeEquation     := TDouble.Create;
    FPitmanGroundwaterFlow                  := TDouble.Create;
    FMaximumRateOfGroundwaterBaseFlow       := TDouble.Create;
    FPowerHeadDifferenceBaseFlowEquation    := TDouble.Create;
    FMaximumHydrologicalGradient            := TDouble.Create;
    FAquiferTransmissivity                  := TDouble.Create;
    FBoreHoleDistanceToRiver                := TDouble.Create;
    FMaximumGroundwaterAbstraction          := TDouble.Create;
    FParameterK2                            := TDouble.Create;
    FParameterK3                            := TDouble.Create;
    FGroundWaterEvaporationArea             := TDouble.Create;

    for LIndex := Low(FGroundWaterEvaporation) to High(FGroundWaterEvaporation) do
    begin
      FGroundWaterEvaporation[LIndex] := TDouble.Create;
    end;

    for LIndex := Low(FGroundWaterFactors) to High(FGroundWaterFactors) do
    begin
      FGroundWaterFactors[LIndex] := TDouble.Create;
    end;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterFileObject.DestroyMemberObjects;
const OPNAME = 'TGroundWaterFileObject.DestroyMemberObjects';
var
  LIndex : integer;
begin
  try
    FIdentifier.Free;
    FAquiferNodeNumber.Free;
    FRefNodeNumber.Free;
    FAquiferInflowChannelNr.Free;
    FAquiferExcessInterflowChannelNr.Free;
    FGroundWaterBaseflowChannelNr.Free;
    FAbstractionFromAquiferChannelNr.Free;
    FAbstractionFromBaseflowChannelNr.Free;
    FInflowFromUpstreamAquiferChannelNr.Free;
    FSurfaceRunoffAndSoilInterflowChannelNr.Free;
    FGroundWaterBaseFlowRemainderChannelNr.Free;

    FAbstractionNodeNr.Free;
    FCollectionNodeNr.Free;
    FBaseFlowNodeNr.Free;

    FAquiferStorativity.Free;
    FAquiferStaticWaterLevel.Free;
    FUnsaturatedStorageCapacity.Free;
    FInitialUnsaturatedStorage.Free;
    FMaximumDischargeRate.Free;
    FMovingAverageRecharge.Free;
    FPitmanSoilMoistureCapacity.Free;
    FPitmanSoilMoistureStorageCapacity.Free;
    FPitmansoilMoistureFlowState.Free;
    FPitmanSoilMoistureFlowEquation.Free;
    FPitmanMaximumGroundwaterFlow.Free;
    FPitmanSoilMoistureRechargeEquation.Free;
    FPitmanGroundwaterFlow.Free;
    FMaximumRateOfGroundwaterBaseFlow.Free;
    FPowerHeadDifferenceBaseFlowEquation.Free;
    FMaximumHydrologicalGradient.Free;
    FAquiferTransmissivity.Free;
    FBoreHoleDistanceToRiver.Free;
    FMaximumGroundwaterAbstraction.Free;
    FParameterK2.Free;
    FParameterK3.Free;
    FGroundWaterEvaporationArea.Free;

    for LIndex := Low(FGroundWaterEvaporation) to High(FGroundWaterEvaporation) do
    begin
      FGroundWaterEvaporation[LIndex].Free;
    end;

    for LIndex := Low(FGroundWaterFactors) to High(FGroundWaterFactors) do
    begin
      FGroundWaterFactors[LIndex].Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterFileObject.Reset;
const OPNAME = 'TGroundWaterFileObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterFileObject.Initialise: boolean;
const OPNAME = 'TGroundWaterFileObject.Initialise';
var
  LIndex : integer;
begin
  Result := False;
  try
    FIdentifier.FData := 0;
    FIdentifier.FInitalised := False;
    FIdentifier.FLength := 4;
    FIdentifier.FDecimal := 0;
    FIdentifier.FDefaultPadding := True;

    FAquiferNodeNumber.FData := 0;
    FAquiferNodeNumber.FInitalised := False;
    FAquiferNodeNumber.FLength := 4;
    FAquiferNodeNumber.FDecimal := 0;
    FAquiferNodeNumber.FDefaultPadding := True;

    FAquiferNodeNumber.FData := 0;
    FAquiferNodeNumber.FInitalised := False;
    FAquiferNodeNumber.FLength := 4;
    FAquiferNodeNumber.FDecimal := 0;
    FAquiferNodeNumber.FDefaultPadding := True;

    FRefNodeNumber.FData := 0;
    FRefNodeNumber.FInitalised := False;
    FRefNodeNumber.FLength := 4;
    FRefNodeNumber.FDecimal := 0;
    FRefNodeNumber.FDefaultPadding := True;

    FAquiferInflowChannelNr.FData := 0;
    FAquiferInflowChannelNr.FInitalised := False;
    FAquiferInflowChannelNr.FLength := 4;
    FAquiferInflowChannelNr.FDecimal := 0;
    FAquiferInflowChannelNr.FDefaultPadding := True;

    FAquiferExcessInterflowChannelNr.FData := 0;
    FAquiferExcessInterflowChannelNr.FInitalised := False;
    FAquiferExcessInterflowChannelNr.FLength := 4;
    FAquiferExcessInterflowChannelNr.FDecimal := 0;
    FAquiferExcessInterflowChannelNr.FDefaultPadding := True;

    FGroundWaterBaseflowChannelNr.FData := 0;
    FGroundWaterBaseflowChannelNr.FInitalised := False;
    FGroundWaterBaseflowChannelNr.FLength := 4;
    FGroundWaterBaseflowChannelNr.FDecimal := 0;
    FGroundWaterBaseflowChannelNr.FDefaultPadding := True;

    FAbstractionFromAquiferChannelNr.FData := 0;
    FAbstractionFromAquiferChannelNr.FInitalised := False;
    FAbstractionFromAquiferChannelNr.FLength := 4;
    FAbstractionFromAquiferChannelNr.FDecimal := 0;
    FAbstractionFromAquiferChannelNr.FDefaultPadding := True;

    FAbstractionFromBaseflowChannelNr.FData := 0;
    FAbstractionFromBaseflowChannelNr.FInitalised := False;
    FAbstractionFromBaseflowChannelNr.FLength := 4;
    FAbstractionFromBaseflowChannelNr.FDecimal := 0;
    FAbstractionFromBaseflowChannelNr.FDefaultPadding := True;

    FInflowFromUpstreamAquiferChannelNr.FData := 0;
    FInflowFromUpstreamAquiferChannelNr.FInitalised := False;
    FInflowFromUpstreamAquiferChannelNr.FLength := 4;
    FInflowFromUpstreamAquiferChannelNr.FDecimal := 0;
    FInflowFromUpstreamAquiferChannelNr.FDefaultPadding := True;

    FSurfaceRunoffAndSoilInterflowChannelNr.FData := 0;
    FSurfaceRunoffAndSoilInterflowChannelNr.FInitalised := False;
    FSurfaceRunoffAndSoilInterflowChannelNr.FLength := 4;
    FSurfaceRunoffAndSoilInterflowChannelNr.FDecimal := 0;
    FSurfaceRunoffAndSoilInterflowChannelNr.FDefaultPadding := True;

    FGroundWaterBaseFlowRemainderChannelNr.FData := 0;
    FGroundWaterBaseFlowRemainderChannelNr.FInitalised := False;
    FGroundWaterBaseFlowRemainderChannelNr.FLength := 4;
    FGroundWaterBaseFlowRemainderChannelNr.FDecimal := 0;
    FGroundWaterBaseFlowRemainderChannelNr.FDefaultPadding := True;

    FAbstractionNodeNr.FData := 0;
    FAbstractionNodeNr.FInitalised := False;
    FAbstractionNodeNr.FLength := 4;
    FAbstractionNodeNr.FDecimal := 0;
    FAbstractionNodeNr.FDefaultPadding := True;

    FCollectionNodeNr.FData := 0;
    FCollectionNodeNr.FInitalised := False;
    FCollectionNodeNr.FLength := 4;
    FCollectionNodeNr.FDecimal := 0;
    FCollectionNodeNr.FDefaultPadding := True;

    FBaseFlowNodeNr.FData := 0;
    FBaseFlowNodeNr.FInitalised := False;
    FBaseFlowNodeNr.FLength := 4;
    FBaseFlowNodeNr.FDecimal := 0;
    FBaseFlowNodeNr.FDefaultPadding := True;

    FAquiferStorativity.FData := 0.0;
    FAquiferStorativity.FInitalised := False;
    FAquiferStorativity.FLength := 10;
    FAquiferStorativity.FDecimal := 3;
    FAquiferStorativity.FDefaultPadding := True;
    FAquiferStorativity.ShowDecimalPoint := True;

    FAquiferStaticWaterLevel.FData := 0.0;
    FAquiferStaticWaterLevel.FInitalised := False;
    FAquiferStaticWaterLevel.FLength := 10;
    FAquiferStaticWaterLevel.FDecimal := 2;
    FAquiferStaticWaterLevel.FDefaultPadding := True;
    FAquiferStaticWaterLevel.ShowDecimalPoint := True;

    FUnsaturatedStorageCapacity.FData := 0.0;
    FUnsaturatedStorageCapacity.FInitalised := False;
    FUnsaturatedStorageCapacity.FLength := 10;
    FUnsaturatedStorageCapacity.FDecimal := 2;
    FUnsaturatedStorageCapacity.FDefaultPadding := True;
    FUnsaturatedStorageCapacity.ShowDecimalPoint := True;

    FInitialUnsaturatedStorage.FData := 0.0;
    FInitialUnsaturatedStorage.FInitalised := False;
    FInitialUnsaturatedStorage.FLength := 10;
    FInitialUnsaturatedStorage.FDecimal := 2;
    FInitialUnsaturatedStorage.FDefaultPadding := True;
    FInitialUnsaturatedStorage.ShowDecimalPoint := True;

    FMaximumDischargeRate.FData := 0.0;
    FMaximumDischargeRate.FInitalised := False;
    FMaximumDischargeRate.FLength := 10;
    FMaximumDischargeRate.FDecimal := 2;
    FMaximumDischargeRate.FDefaultPadding := True;
    FMaximumDischargeRate.ShowDecimalPoint := False;

    FMovingAverageRecharge.FData := 0.0;
    FMovingAverageRecharge.FInitalised := False;
    FMovingAverageRecharge.FLength := 10;
    FMovingAverageRecharge.FDecimal := 2;
    FMovingAverageRecharge.FDefaultPadding := True;
    FMovingAverageRecharge.ShowDecimalPoint := False;

    FPitmanSoilMoistureCapacity.FData := 0.0;
    FPitmanSoilMoistureCapacity.FInitalised := False;
    FPitmanSoilMoistureCapacity.FLength := 10;
    FPitmanSoilMoistureCapacity.FDecimal := 2;
    FPitmanSoilMoistureCapacity.FDefaultPadding := True;
    FPitmanSoilMoistureCapacity.ShowDecimalPoint := True;

    FPitmanSoilMoistureStorageCapacity.FData := 0.0;
    FPitmanSoilMoistureStorageCapacity.FInitalised := False;
    FPitmanSoilMoistureStorageCapacity.FLength := 10;
    FPitmanSoilMoistureStorageCapacity.FDecimal := 2;
    FPitmanSoilMoistureStorageCapacity.FDefaultPadding := True;
    FPitmanSoilMoistureStorageCapacity.ShowDecimalPoint := True;

    FPitmansoilMoistureFlowState.FData := 0.0;
    FPitmansoilMoistureFlowState.FInitalised := False;
    FPitmansoilMoistureFlowState.FLength := 10;
    FPitmansoilMoistureFlowState.FDecimal := 2;
    FPitmansoilMoistureFlowState.FDefaultPadding := True;
    FPitmansoilMoistureFlowState.ShowDecimalPoint := True;

    FPitmanSoilMoistureFlowEquation.FData := 0.0;
    FPitmanSoilMoistureFlowEquation.FInitalised := False;
    FPitmanSoilMoistureFlowEquation.FLength := 10;
    FPitmanSoilMoistureFlowEquation.FDecimal := 2;
    FPitmanSoilMoistureFlowEquation.FDefaultPadding := True;
    FPitmanSoilMoistureFlowEquation.ShowDecimalPoint := True;

    FPitmanMaximumGroundwaterFlow.FData := 0.0;
    FPitmanMaximumGroundwaterFlow.FInitalised := False;
    FPitmanMaximumGroundwaterFlow.FLength := 10;
    FPitmanMaximumGroundwaterFlow.FDecimal := 2;
    FPitmanMaximumGroundwaterFlow.FDefaultPadding := True;
    FPitmanMaximumGroundwaterFlow.ShowDecimalPoint := True;

    FPitmanSoilMoistureRechargeEquation.FData := 0.0;
    FPitmanSoilMoistureRechargeEquation.FInitalised := False;
    FPitmanSoilMoistureRechargeEquation.FLength := 10;
    FPitmanSoilMoistureRechargeEquation.FDecimal := 2;
    FPitmanSoilMoistureRechargeEquation.FDefaultPadding := True;
    FPitmanSoilMoistureRechargeEquation.ShowDecimalPoint := True;

    FPitmanGroundwaterFlow.FData := 0.0;
    FPitmanGroundwaterFlow.FInitalised := False;
    FPitmanGroundwaterFlow.FLength := 10;
    FPitmanGroundwaterFlow.FDecimal := 2;
    FPitmanGroundwaterFlow.FDefaultPadding := True;
    FPitmanGroundwaterFlow.ShowDecimalPoint := True;

    FMaximumRateOfGroundwaterBaseFlow.FData := 0.0;
    FMaximumRateOfGroundwaterBaseFlow.FInitalised := False;
    FMaximumRateOfGroundwaterBaseFlow.FLength := 10;
    FMaximumRateOfGroundwaterBaseFlow.FDecimal := 2;
    FMaximumRateOfGroundwaterBaseFlow.FDefaultPadding := True;
    FMaximumRateOfGroundwaterBaseFlow.ShowDecimalPoint := True;

    FPowerHeadDifferenceBaseFlowEquation.FData := 0.0;
    FPowerHeadDifferenceBaseFlowEquation.FInitalised := False;
    FPowerHeadDifferenceBaseFlowEquation.FLength := 10;
    FPowerHeadDifferenceBaseFlowEquation.FDecimal := 2;
    FPowerHeadDifferenceBaseFlowEquation.FDefaultPadding := True;
    FPowerHeadDifferenceBaseFlowEquation.ShowDecimalPoint := True;

    FMaximumHydrologicalGradient.FData := 0.0;
    FMaximumHydrologicalGradient.FInitalised := False;
    FMaximumHydrologicalGradient.FLength := 10;
    FMaximumHydrologicalGradient.FDecimal := 6;
    FMaximumHydrologicalGradient.FDefaultPadding := True;
    FMaximumHydrologicalGradient.ShowDecimalPoint := True;

    FAquiferTransmissivity.FData := 0.0;
    FAquiferTransmissivity.FInitalised := False;
    FAquiferTransmissivity.FLength := 10;
    FAquiferTransmissivity.FDecimal := 2;
    FAquiferTransmissivity.FDefaultPadding := True;
    FAquiferTransmissivity.ShowDecimalPoint := True;

    FBoreHoleDistanceToRiver.FData := 0.0;
    FBoreHoleDistanceToRiver.FInitalised := False;
    FBoreHoleDistanceToRiver.FLength := 10;
    FBoreHoleDistanceToRiver.FDecimal := 2;
    FBoreHoleDistanceToRiver.FDefaultPadding := True;
    FBoreHoleDistanceToRiver.ShowDecimalPoint := True;

    FMaximumGroundwaterAbstraction.FData := 0.0;
    FMaximumGroundwaterAbstraction.FInitalised := False;
    FMaximumGroundwaterAbstraction.FLength := 10;
    FMaximumGroundwaterAbstraction.FDecimal := 2;
    FMaximumGroundwaterAbstraction.FDefaultPadding := True;
    FMaximumGroundwaterAbstraction.ShowDecimalPoint := True;

    FParameterK2.FData := 0.0;
    FParameterK2.FInitalised := False;
    FParameterK2.FLength := 10;
    FParameterK2.FDecimal := 2;
    FParameterK2.FDefaultPadding := True;
    FParameterK2.ShowDecimalPoint := True;

    FParameterK3.FData := 0.0;
    FParameterK3.FInitalised := False;
    FParameterK3.FLength := 10;
    FParameterK3.FDecimal := 2;
    FParameterK3.FDefaultPadding := True;
    FParameterK3.ShowDecimalPoint := True;

    FGroundWaterEvaporationArea.FData := 0.0;
    FGroundWaterEvaporationArea.FInitalised := False;
    FGroundWaterEvaporationArea.FLength := 10;
    FGroundWaterEvaporationArea.FDecimal := 2;
    FGroundWaterEvaporationArea.FDefaultPadding := True;
    FGroundWaterEvaporationArea.ShowDecimalPoint := True;

    for LIndex := Low(FGroundWaterEvaporation) to High(FGroundWaterEvaporation) do
    begin
      FGroundWaterEvaporation[LIndex].FData := 0.0;
      FGroundWaterEvaporation[LIndex].FInitalised := False;
      FGroundWaterEvaporation[LIndex].FLength := 10;
      FGroundWaterEvaporation[LIndex].FDecimal := 2;
      FGroundWaterEvaporation[LIndex].FDefaultPadding := True;
      FGroundWaterEvaporation[LIndex].ShowDecimalPoint := True;
    end;

    for LIndex := Low(FGroundWaterFactors) to High(FGroundWaterFactors) do
    begin
      FGroundWaterFactors[LIndex].FData := 0.0;
      FGroundWaterFactors[LIndex].FInitalised := False;
      FGroundWaterFactors[LIndex].FLength := 10;
      FGroundWaterFactors[LIndex].FDecimal := 2;
      FGroundWaterFactors[LIndex].FDefaultPadding := True;
      FGroundWaterFactors[LIndex].ShowDecimalPoint := True;
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{TGroundWaterListFileObject }

procedure TGroundWaterListFileObject.CreateMemberObjects;
const OPNAME = 'TGroundWaterListFileObject.CreateMemberObjects';
begin
  try
    inherited;
    FGroundWaterObjectContainer := TObjectList.Create(True);
    FComment                 := TStringlist.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterListFileObject.DestroyMemberObjects;
const OPNAME = 'TGroundWaterListFileObject.DestroyMemberObjects';
begin
  try
    inherited;
    FGroundWaterObjectContainer.Clear;
    FGroundWaterObjectContainer.Free;
    FComment.Clear;
    FComment.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterListFileObject.AddGroundWaterObject: TGroundWaterFileObject;
const OPNAME = 'TGroundWaterListFileObject.AddGroundWaterObject';
begin
  Result := nil;
  try
    Result := TGroundWaterFileObject.Create;
    FGroundWaterObjectContainer.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterListFileObject.GetGroundWaterObjectByIndex(AIndex: integer): TGroundWaterFileObject;
const OPNAME = 'TGroundWaterListFileObject.GetGroundWaterObjectByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FGroundWaterObjectContainer.Count) then
      Result := TGroundWaterFileObject(FGroundWaterObjectContainer[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterListFileObject.Initialise: boolean;
const OPNAME = 'TGroundWaterListFileObject.Initialise';
begin
  Result := False;
  try
    FGroundWaterObjectContainer.Clear;
    FComment.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterListFileObject.Reset;
const OPNAME = 'TGroundWaterListFileObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterListFileObject.GroundWaterObjectCount: integer;
const OPNAME = 'TGroundWaterListFileObject.GroundWaterObjectCount';
begin
  Result := 0;
  try
    Result := FGroundWaterObjectContainer.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
