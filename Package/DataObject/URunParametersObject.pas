//
//
//  UNIT      : Contains TRunParametersObject Class
//  AUTHOR    : Titi Ngubane(PDNA)
//  DATE      : 28/01/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit URunParametersObject;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UAbstractDataObject,
  UBasicObjects,
  UConstants;

type
  TDemandCentreObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FDemandCentreType   :TChar;
    FChannelNumber :TInteger;
    FAnnualDemand  :TDouble;
    FMinimumDemand :TDouble;
    FOutputResults :TChar;
    FComment       : TString;
    function Initialise: boolean;override;
    procedure Reset;override;
  end;

  TInterBasinTransferObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FSummaryRequired   :TChar;
    FChannelNumber :TInteger;
    FUpperTransferLimit  :TDouble;
    FDemandCentreNumber :TInteger;
    FComment            : TString;
    function Initialise: boolean;override;
    procedure Reset;override;
  end;

  TMinMaxBoundsObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    //Line 18a... Bounded Channel Number
    FMinMaxBoundedChannel : TInteger;
    FNoOfRefChannels : TInteger;
    //Line 18b...Ref. Channels
    FRefChannels : TStringList;
    function Initialise: boolean;override;
    procedure Reset;override;
  end;

  TWQConstraintsObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
     // Line 19a... Constraints Min Max Channel Number
    FWQConMinMaxChannel : TInteger;
    FWQConTarget : TDouble;
    FNoOfRefChannelsBlending : TInteger;
    FReservoirRef : TInteger;
    FWQConType : TInteger;
    // Line 19b... Reference Channel Number
    FReferenceChannel : TStringList;
    FRefChannelFactor : TStringList;
    // Line 19c...
    FSlopeLimit : TInteger;
    // Line 19d...
    FEstimatedRelease : TStringList;
    // Line 19e...
    FConcentration : TStringList;
    function Initialise: boolean;override;
    procedure Reset;override;
  end;



  TRunParametersObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    //File F01.dat
    //Line 1..3 : Run Title
    FTitle : Array[1..3] Of TString;

    //Line 4 : Run Title
    FTimePeriods    : TInteger;
    FStartGregorian : TInteger;
    FStartOther : TInteger;
    FStartDebug : TInteger;
    FEndDebug : TInteger;
    FDebugLevel : TInteger;
    FSummaryLevel : TInteger;
    FSummaryOutput : TInteger;

    FStoreYield : TInteger;
    FRandomNumber : TInteger;
    FPlotOption : TChar;
    FLimitOption : TInteger;
    FMultiplePeriods : TInteger;
    FCalcHistYield : TInteger;
    FReduceSequence : TInteger;

    //Planning
    FDetailedOption :TChar;
    FSupplyOption :TChar;
    FAnnualSummary :TChar;
    FEconomicOption :TChar;
    FPlanningSummary :TChar;
    FInputSummary :TChar;
    FWaterQualityOption :TChar;

    //Line 5 : Month names
    FMonthNames : Array[MinMonths..MaxMonths] Of TString;

    //Line 6 : Month days
    FMonthDays : Array[MinMonths..MaxMonths] Of TDouble;

    //Line 7 : Month days
    FHydYears : TInteger;
    FMaxHydSequences : TInteger;
    FNoLoadCases : TInteger;
    FStartMonth : TInteger;
    FHistStoch : TChar;
    FHistRand : TInteger;
    FParamName : TString;

    //Planning
    FEconomicTimePeriods         : TInteger;
    FAnalysisStartYear           : TInteger;
    FNumberOfDemandCentres       : TInteger;
    FNumberOfInterBasinChannels  : TInteger;
    FMonthStartNewYear           : TInteger;
    FShortTermPlanningOption     : TChar;
    FHydroPowerOption            : TChar;
    FAllocationControlOption     : TChar;

    //Line 8 : Analytical sequences
    FAnalSequences : Array[MinLoadCase..MaxLoadCase] Of TInteger;
    //Planning
    FDecisionMonthsNumber : TInteger;
    FDecisionMonths : Array[MinMonths..MaxMonths] Of TInteger;

    //Line 9 : Analytical sequences
    FTargetYield : Array[MinLoadCase..MaxLoadCase] Of TDouble;
    //Planning
    FDecisionMonthsType : Array[MinMonths..MaxMonths] Of TChar;

    //Line 10 : Maximum yield
    FMaxYield : Array[MinLoadCase..MaxLoadCase] Of TDouble;
    //Planning
    FHydroPowerDecision : Array[MinMonths..MaxMonths] Of TChar;

    //Line 11 : Target power
    FTargetPower : Array[MinLoadCase..MaxLoadCase] Of TDouble;

    //Line 12... : TargetRecurrenceInterval
    FTargetRecurrenceInterval : TInteger;
    //Planning
    FDemandCentresList : TObjectList;

    //Line 13... : Extra useless lines
    FF01ExtraLines: TStringList;
    //Planning
    FInterBasinTransferList : TObjectList;

    //Line 14... : Planning model fields only start from here
    FNumberOfDiscountRates  : TInteger;
    FDiscountRates : Array[MinMonths..MaxMonths] Of TDouble;

    //Line 16 & 17... : Historic and Stochastic Manual Analysis
    FHistManualAnalysis : Array[1..5] Of TInteger;
    FStocManualAnalysis : Array[1..5] Of TInteger;
    //Line 18... Flows routing Upper Bound
    FNoOfFlowsUpperBounds : TInteger;
    FMinMaxUpperBoundList : TObjectList;

    // Line 19... Number of Water quality constraints
    FNoOfWQConstraints : TInteger;
    FWQConstraintsList : TObjectList;

    procedure AddDemandCenters;
    procedure AddInterBasinTransferTransfers;
    function AddDemandCenter:TDemandCentreObject;
    function AddInterBasinTransferTransfer:TInterBasinTransferObject;
    function AddMinMaxBoundsObject : TMinMaxBoundsObject;
    function AddWQConstraintsObject : TWQConstraintsObject;
    function Initialise: boolean;override;
    procedure Reset;override;
  end;

implementation


{ TRunParametersObject }
uses UErrorHandlingOperations;

function TRunParametersObject.AddDemandCenter:TDemandCentreObject;
const OPNAME = 'TRunParametersObject.AddDemandCenter';
begin
  Result := nil;
  try
    Result := TDemandCentreObject.Create;
    FDemandCentresList.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunParametersObject.AddDemandCenters;
const OPNAME = 'TRunParametersObject.AddDemandCenters';
var
  LIndex: integer;
  LDemandCentre:TDemandCentreObject;
begin
  try
    FDemandCentresList.Clear;
    for LIndex := 1 to FNumberOfDemandCentres.FData do
    begin
      LDemandCentre := TDemandCentreObject.Create;
      FDemandCentresList.Add(LDemandCentre);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunParametersObject.AddInterBasinTransferTransfer:TInterBasinTransferObject;
const OPNAME = 'TRunParametersObject.AddInterBasinTransferTransfer';
begin
  Result := nil;
  try
    Result := TInterBasinTransferObject.Create;
    FInterBasinTransferList.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunParametersObject.AddInterBasinTransferTransfers;
const OPNAME = 'TRunParametersObject.AddInterBasinTransferTransfers';
var
  LIndex: integer;
  LInterBasinTransfer:TInterBasinTransferObject;
begin
  try
    FInterBasinTransferList.Clear;
    for LIndex := 1 to FNumberOfInterBasinChannels.FData do
    begin
      LInterBasinTransfer := TInterBasinTransferObject.Create;
      FInterBasinTransferList.Add(LInterBasinTransfer);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TRunParametersObject.AddMinMaxBoundsObject : TMinMaxBoundsObject;
const OPNAME = 'TRunParametersObject.AddMinMaxBoundsObject';
begin
  Result := nil;
  try
    Result := TMinMaxBoundsObject.Create;
    FMinMaxUpperBoundList.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunParametersObject.AddWQConstraintsObject : TWQConstraintsObject;
const OPNAME = 'TRunParametersObject.AddWQConstraintsObject';
begin
  Result := nil;
  try
    Result := TWQConstraintsObject.Create;
    FWQConstraintsList.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TRunParametersObject.CreateMemberObjects;
const OPNAME = 'TRunParametersObject.CreateMemberObjects';
var
  LCount: integer;
  LString:TString;
  LDouble : TDouble;
  LInteger: TInteger;
  LChar: TChar;
begin
  try
    //File F01.dat
    for LCount := 1 to 3 do
    begin
      LString := TString.Create;
      FTitle[LCount] := LString;
    end;
    FTimePeriods := TInteger.Create;
    FStartGregorian := TInteger.Create;
    FStartOther := TInteger.Create;
    FStartDebug := TInteger.Create;
    FEndDebug := TInteger.Create;
    FDebugLevel := TInteger.Create;
    FSummaryLevel := TInteger.Create;
    FSummaryOutput := TInteger.Create;
    FStoreYield := TInteger.Create;
    FRandomNumber := TInteger.Create;
    FPlotOption := TChar.Create;
    FLimitOption := TInteger.Create;
    FDecisionMonthsNumber := TInteger.Create;
    FNumberOfDiscountRates := TInteger.Create;;
    for LCount := MinMonths to MaxMonths do
    begin
      LString := TString.Create;
      FMonthNames[LCount] := LString;
      LDouble := TDouble.Create;
      FMonthDays[LCount] := LDouble;
      LInteger := TInteger.Create;
      FDecisionMonths[LCount] := LInteger;
      LChar := TChar.Create;
      FDecisionMonthsType[LCount] := LChar;
      LChar := TChar.Create;
      FHydroPowerDecision[LCount] := LChar;
      LDouble := TDouble.Create;
      FDiscountRates[LCount] := LDouble;
    end;
    FHydYears := TInteger.Create;
    FMaxHydSequences := TInteger.Create;
    FNoLoadCases := TInteger.Create;
    FStartMonth := TInteger.Create;
    FHistStoch := TChar.Create;
    FHistRand := TInteger.Create;
    FParamName := TString.Create;
    for LCount := MinLoadCase to MaxLoadCase do
    begin
      LInteger := TInteger.Create;
      FAnalSequences[LCount] := LInteger;
      LDouble := TDouble.Create;
      FTargetYield[LCount] := LDouble;
      LDouble := TDouble.Create;
      FMaxYield[LCount] := LDouble;
      LDouble := TDouble.Create;
      FTargetPower[LCount] := LDouble;
    end;
    FMultiplePeriods := TInteger.Create;
    FCalcHistYield := TInteger.Create;
    FReduceSequence := TInteger.Create;
    FTargetRecurrenceInterval := TInteger.Create;
    FF01ExtraLines := TStringList.Create;

    //Planning
    FDetailedOption     := TChar.Create;
    FSupplyOption       := TChar.Create;
    FAnnualSummary      := TChar.Create;
    FEconomicOption     := TChar.Create;
    FPlanningSummary    := TChar.Create;
    FInputSummary       := TChar.Create;
    FWaterQualityOption := TChar.Create;

    FDemandCentresList  := TObjectList.Create(True);
    FInterBasinTransferList     := TObjectList.Create(True);
    FMinMaxUpperBoundList     := TObjectList.Create(True);
    FWQConstraintsList     := TObjectList.Create(True);
    
    FEconomicTimePeriods         := TInteger.Create;
    FAnalysisStartYear           := TInteger.Create;
    FNumberOfDemandCentres       := TInteger.Create;
    FNumberOfInterBasinChannels  := TInteger.Create;
    FMonthStartNewYear           := TInteger.Create;
    FShortTermPlanningOption     := TChar.Create;
    FHydroPowerOption            := TChar.Create;
    FAllocationControlOption     := TChar.Create;

    for LCount := 1 to 5 do
    begin
      LInteger := TInteger.Create;
      FHistManualAnalysis[LCount] := LInteger;
      LInteger := TInteger.Create;
      FStocManualAnalysis[LCount] := LInteger;
    end;

    FNoOfFlowsUpperBounds := TInteger.Create;
    
    FNoOfWQConstraints := TInteger.Create;

    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunParametersObject.DestroyMemberObjects;
const OPNAME = 'TRunParametersObject.DestroyMemberObjects';
var
  LCount: integer;
begin
  try
    //File F01.dat
    for LCount := 1 to 3 do
      FTitle[LCount].Free;
    FTimePeriods.Free;
    FStartGregorian.Free;
    FStartOther.Free;
    FStartDebug.Free;
    FEndDebug.Free;
    FDebugLevel.Free;
    FSummaryLevel.Free;
    FSummaryOutput.Free;
    FStoreYield.Free;
    FRandomNumber.Free;
    FPlotOption.Free;
    FLimitOption.Free;
    FDecisionMonthsNumber.Free;
    FNumberOfDiscountRates.Free;
    for LCount := MinMonths to MaxMonths do
    begin
      FMonthNames[LCount].Free;
      FMonthDays[LCount].Free;
      FDecisionMonths[LCount].Free;
      FDecisionMonthsType[LCount].Free;
      FHydroPowerDecision[LCount].Free;
      FDiscountRates[LCount].Free;
    end;
    FHydYears.Free;
    FMaxHydSequences.Free;
    FNoLoadCases.Free;
    FStartMonth.Free;
    FHistStoch.Free;
    FHistRand.Free;
    FParamName.Free;
    for LCount := MinLoadCase to MaxLoadCase do
    begin
      FAnalSequences[LCount].Free;
      FTargetYield[LCount].Free;
      FMaxYield[LCount].Free;
      FTargetPower[LCount].Free;
    end;
    FMultiplePeriods.Free;
    FCalcHistYield.Free;
    FReduceSequence.Free;
    FTargetRecurrenceInterval.Free;
    FF01ExtraLines.Free;

    FDetailedOption.Free;
    FSupplyOption.Free;
    FAnnualSummary.Free;
    FEconomicOption.Free;
    FPlanningSummary.Free;
    FInputSummary.Free;
    FWaterQualityOption.Free;
    FDemandCentresList.Free;
    FInterBasinTransferList.Free;
    FMinMaxUpperBoundList.Free;
    FWQConstraintsList.Free;
    
    FEconomicTimePeriods.Free;
    FAnalysisStartYear.Free;
    FNumberOfDemandCentres.Free;
    FNumberOfInterBasinChannels.Free;
    FMonthStartNewYear.Free;
    FShortTermPlanningOption.Free;
    FHydroPowerOption.Free;
    FAllocationControlOption.Free;

    for LCount := 1 to 5 do
    begin
      FHistManualAnalysis[LCount].Free;
      FStocManualAnalysis[LCount].Free;
    end;

    FNoOfFlowsUpperBounds.Free;
   
    FNoOfWQConstraints.Free;
    
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunParametersObject.Initialise: boolean;
const OPNAME = 'TRunParametersObject.Initialise';
var
  LCount : Integer;
Begin
  Result := False;
  try
    //-----------FData-----------------------------
    //File F01.dat
    for LCount := 1 to 3 do
    begin
      FTitle[LCount].FData := '';
      FTitle[LCount].FInitalised := False;
      FTitle[LCount].FLength  := 80;
      FTitle[LCount].FDecimal := 0;
      FTitle[LCount].FDefaultPadding := True;
    end;

    FTimePeriods.FData := 0;
    FTimePeriods.FInitalised := False;
    FTimePeriods.FLength  := 6;
    FTimePeriods.FDecimal := 0;
    FTimePeriods.FDefaultPadding := True;

    FStartGregorian.FData := 0;
    FStartGregorian.FInitalised := False;
    FStartGregorian.FLength := 6;
    FStartGregorian.FDecimal := 0;
    FStartGregorian.FDefaultPadding := True;

    FStartOther.FData := 0;
    FStartOther.FInitalised := False;
    FStartOther.FLength := 6;
    FStartOther.FDecimal := 0;
    FStartOther.FDefaultPadding := True;

    FStartDebug.FData := 0;
    FStartDebug.FInitalised := False;
    FStartDebug.FLength := 6;
    FStartDebug.FDecimal := 0;
    FStartDebug.FDefaultPadding := True;

    FEndDebug.FData := 0;
    FEndDebug.FInitalised := False;
    FEndDebug.FLength := 6;
    FEndDebug.FDecimal := 0;
    FEndDebug.FDefaultPadding := True;

    FDebugLevel.FData := 0;
    FDebugLevel.FInitalised := False;
    FDebugLevel.FLength := 6;
    FDebugLevel.FDecimal := 0;
    FDebugLevel.FDefaultPadding := True;

    FSummaryLevel.FData := 0;
    FSummaryLevel.FInitalised := False;
    FSummaryLevel.FLength := 6;
    FSummaryLevel.FDecimal := 0;
    FSummaryLevel.FDefaultPadding := True;

    FSummaryOutput.FData := 0;
    FSummaryOutput.FInitalised := False;
    FSummaryOutput.FLength := 6;
    FSummaryOutput.FDecimal := 1;
    FSummaryOutput.FDefaultPadding := True;

    FStoreYield.FData := 0;
    FStoreYield.FInitalised := False;
    FStoreYield.FLength := 6;
    FStoreYield.FDecimal := 1;
    FStoreYield.FDefaultPadding := True;

    FRandomNumber.FData := 0;
    FRandomNumber.FInitalised := False;
    FRandomNumber.FLength := 6;
    FRandomNumber.FDecimal := 0;
    FRandomNumber.FDefaultPadding := True;

    FPlotOption.FData := ' ';
    FPlotOption.FInitalised := False;
    FPlotOption.FLength := 6;
    FPlotOption.FDecimal := 0;
    FPlotOption.FDefaultPadding := False;

    FLimitOption.FData := 0;
    FLimitOption.FInitalised := False;
    FLimitOption.FLength := 6;
    FLimitOption.FDecimal := 0;
    FLimitOption.FDefaultPadding := True;

    FDecisionMonthsNumber.FData := 0;
    FDecisionMonthsNumber.FInitalised := False;
    FDecisionMonthsNumber.FLength := 6;
    FDecisionMonthsNumber.FDecimal := 0;
    FDecisionMonthsNumber.FDefaultPadding := True;

    FNumberOfDiscountRates.FData := 0;
    FNumberOfDiscountRates.FInitalised := False;
    FNumberOfDiscountRates.FLength := 6;
    FNumberOfDiscountRates.FDecimal := 0;
    FNumberOfDiscountRates.FDefaultPadding := True;

    FMonthNames[1].FData := 'OCT';
    FMonthDays[1].FData := 31;
    FMonthNames[2].FData := 'NOV';
    FMonthDays[2].FData := 30;
    FMonthNames[3].FData := 'DEC';
    FMonthDays[3].FData := 31;
    FMonthNames[4].FData := 'JAN';
    FMonthDays[4].FData := 31;
    FMonthNames[5].FData := 'FEB';
    FMonthDays[5].FData := 28.25;
    FMonthNames[6].FData := 'MAR';
    FMonthDays[6].FData := 31;
    FMonthNames[7].FData := 'APR';
    FMonthDays[7].FData := 30;
    FMonthNames[8].FData := 'MAY';
    FMonthDays[8].FData := 31;
    FMonthNames[9].FData := 'JUN';
    FMonthDays[9].FData := 30;
    FMonthNames[10].FData := 'JUL';
    FMonthDays[10].FData := 31;
    FMonthNames[11].FData := 'AUG';
    FMonthDays[11].FData := 31;
    FMonthNames[12].FData := 'SEP';
    FMonthDays[12].FData := 30;
    for LCount := MinMonths to MaxMonths do
    begin
      FMonthNames[LCount].FInitalised := False;
      FMonthNames[LCount].FLength := 6;
      FMonthNames[LCount].FDecimal := 0;
      FMonthNames[LCount].FDefaultPadding := False;

      FMonthDays[LCount].FInitalised := False;
      FMonthDays[LCount].FLength := 6;
      FMonthDays[LCount].FDecimal := 2;
      FMonthDays[LCount].FDefaultPadding := True;
      FMonthDays[LCount].ShowDecimalPoint := True;

      FDecisionMonths[LCount].FData := 0;
      FDecisionMonths[LCount].FInitalised := False;
      FDecisionMonths[LCount].FLength := 6;
      FDecisionMonths[LCount].FDecimal := 0;
      FDecisionMonths[LCount].FDefaultPadding := True;

      FDecisionMonthsType[LCount].FData := ' ';
      FDecisionMonthsType[LCount].FInitalised := False;
      FDecisionMonthsType[LCount].FLength := 6;
      FDecisionMonthsType[LCount].FDecimal := 0;
      FDecisionMonthsType[LCount].FDefaultPadding := False;

      FHydroPowerDecision[LCount].FData := ' ';
      FHydroPowerDecision[LCount].FInitalised := False;
      FHydroPowerDecision[LCount].FLength := 6;
      FHydroPowerDecision[LCount].FDecimal := 0;
      FHydroPowerDecision[LCount].FDefaultPadding := False;

      FDiscountRates[LCount].FData := 0.0;
      FDiscountRates[LCount].FInitalised := False;
      FDiscountRates[LCount].FLength := 6;
      FDiscountRates[LCount].FDecimal := 2;
      FDiscountRates[LCount].FDefaultPadding := True;
      FDiscountRates[LCount].ShowDecimalPoint := True;
    end;

    FHydYears.FData := 0;
    FHydYears.FInitalised := False;
    FHydYears.FLength := 6;
    FHydYears.FDecimal := 0;
    FHydYears.FDefaultPadding := True;

    FMaxHydSequences.FData := 0;
    FMaxHydSequences.FInitalised := False;
    FMaxHydSequences.FLength := 6;
    FMaxHydSequences.FDecimal := 0;
    FMaxHydSequences.FDefaultPadding := True;

    FNoLoadCases.FData := 0;
    FNoLoadCases.FInitalised := False;
    FNoLoadCases.FLength := 6;
    FNoLoadCases.FDecimal := 0;
    FNoLoadCases.FDefaultPadding := True;

    FStartMonth.FData := 0;
    FStartMonth.FInitalised := False;
    FStartMonth.FLength := 6;
    FStartMonth.FDecimal := 0;
    FStartMonth.FDefaultPadding := True;

    FHistStoch.FData := ' ';
    FHistStoch.FInitalised := False;
    FHistStoch.FLength := 6;
    FHistStoch.FDecimal := 0;
    FHistStoch.FDefaultPadding := False;

    FHistRand.FData := 0;
    FHistRand.FInitalised := False;
    FHistRand.FLength := 6;
    FHistRand.FDecimal := 0;
    FHistRand.FDefaultPadding := True;

    FParamName.FData := '';
    FParamName.FInitalised := False;
    FParamName.FLength := 40;
    FParamName.FDecimal := 0;
    FParamName.FDefaultPadding := True;

    for  LCount :=  MinLoadCase to MaxLoadCase do
    begin
      FAnalSequences[LCount].FData := 0;
      FAnalSequences[LCount].FInitalised := False;
      FAnalSequences[LCount].FLength := 6;
      FAnalSequences[LCount].FDecimal := 0;
      FAnalSequences[LCount].FDefaultPadding := True;

      FTargetYield[LCount].FData := 0;
      FTargetYield[LCount].FInitalised := False;
      FTargetYield[LCount].FLength := 6;
      FTargetYield[LCount].FDecimal := 2;
      FTargetYield[LCount].FDefaultPadding := True;
      FTargetYield[LCount].ShowDecimalPoint := True;

      FMaxYield[LCount].FData := 0;
      FMaxYield[LCount].FInitalised := False;
      FMaxYield[LCount].FLength := 6;
      FMaxYield[LCount].FDecimal := 2;
      FMaxYield[LCount].FDefaultPadding := True;
      FMaxYield[LCount].ShowDecimalPoint := True;

      FTargetPower[LCount].FData := 0;
      FTargetPower[LCount].FInitalised := False;
      FTargetPower[LCount].FLength := 6;
      FTargetPower[LCount].FDecimal := 2;
      FTargetPower[LCount].FDefaultPadding := True;
      FTargetPower[LCount].ShowDecimalPoint := False;
    end;

    FMultiplePeriods.FData := 0;
    FMultiplePeriods.FInitalised := False;
    FMultiplePeriods.FLength := 6;
    FMultiplePeriods.FDecimal := 0;
    FMultiplePeriods.FDefaultPadding := True;

    FCalcHistYield.FData := 0;
    FCalcHistYield.FInitalised := False;
    FCalcHistYield.FLength := 6;
    FCalcHistYield.FDecimal := 0;
    FCalcHistYield.FDefaultPadding := True;

    FReduceSequence.FData := 0;
    FReduceSequence.FInitalised := False;
    FReduceSequence.FLength := 6;
    FReduceSequence.FDecimal := 0;
    FReduceSequence.FDefaultPadding := True;

    FTargetRecurrenceInterval.FData := 0;
    FTargetRecurrenceInterval.FInitalised := False;
    FTargetRecurrenceInterval.FLength := 6;
    FTargetRecurrenceInterval.FDecimal := 0;
    FTargetRecurrenceInterval.FDefaultPadding := True;

    FDemandCentresList.Clear;
    FInterBasinTransferList.Clear;
    FMinMaxUpperBoundList.Clear;
    FF01ExtraLines.Clear;
    FWQConstraintsList.Clear;

    FDetailedOption.FData := ' ';
    FDetailedOption.FInitalised := False;
    FDetailedOption.FLength := 6;
    FDetailedOption.FDecimal := 0;
    FDetailedOption.FDefaultPadding := False;

    FSupplyOption.FData := ' ';
    FSupplyOption.FInitalised := False;
    FSupplyOption.FLength := 6;
    FSupplyOption.FDecimal := 0;
    FSupplyOption.FDefaultPadding := False;

    FAnnualSummary.FData := ' ';
    FAnnualSummary.FInitalised := False;
    FAnnualSummary.FLength := 6;
    FAnnualSummary.FDecimal := 0;
    FAnnualSummary.FDefaultPadding := False;

    FEconomicOption.FData := ' ';
    FEconomicOption.FInitalised := False;
    FEconomicOption.FLength := 6;
    FEconomicOption.FDecimal := 0;
    FEconomicOption.FDefaultPadding := False;

    FPlanningSummary.FData := ' ';
    FPlanningSummary.FInitalised := False;
    FPlanningSummary.FLength := 6;
    FPlanningSummary.FDecimal := 0;
    FPlanningSummary.FDefaultPadding := False;

    FInputSummary.FData := ' ';
    FInputSummary.FInitalised := False;
    FInputSummary.FLength := 6;
    FInputSummary.FDecimal := 0;
    FInputSummary.FDefaultPadding := False;

    FWaterQualityOption.FData := ' ';
    FWaterQualityOption.FInitalised := False;
    FWaterQualityOption.FLength := 6;
    FWaterQualityOption.FDecimal := 0;
    FWaterQualityOption.FDefaultPadding := False;

    FEconomicTimePeriods.FData := 0;
    FEconomicTimePeriods.FInitalised := False;
    FEconomicTimePeriods.FLength := 6;
    FEconomicTimePeriods.FDecimal := 0;
    FEconomicTimePeriods.FDefaultPadding := True;

    FAnalysisStartYear.FData := 0;
    FAnalysisStartYear.FInitalised := False;
    FAnalysisStartYear.FLength := 6;
    FAnalysisStartYear.FDecimal := 0;
    FAnalysisStartYear.FDefaultPadding := True;

    FNumberOfDemandCentres.FData := 0;
    FNumberOfDemandCentres.FInitalised := False;
    FNumberOfDemandCentres.FLength := 6;
    FNumberOfDemandCentres.FDecimal := 0;
    FNumberOfDemandCentres.FDefaultPadding := True;

    FNumberOfInterBasinChannels.FData := 0;
    FNumberOfInterBasinChannels.FInitalised := False;
    FNumberOfInterBasinChannels.FLength := 6;
    FNumberOfInterBasinChannels.FDecimal := 0;
    FNumberOfInterBasinChannels.FDefaultPadding := True;

    FMonthStartNewYear.FData := 0;
    FMonthStartNewYear.FInitalised := False;
    FMonthStartNewYear.FLength := 6;
    FMonthStartNewYear.FDecimal := 0;
    FMonthStartNewYear.FDefaultPadding := True;

    FShortTermPlanningOption.FData := ' ';
    FShortTermPlanningOption.FInitalised := False;
    FShortTermPlanningOption.FLength := 6;
    FShortTermPlanningOption.FDecimal := 0;
    FShortTermPlanningOption.FDefaultPadding := False;

    FHydroPowerOption.FData := ' ';
    FHydroPowerOption.FInitalised := False;
    FHydroPowerOption.FLength := 6;
    FHydroPowerOption.FDecimal := 0;
    FHydroPowerOption.FDefaultPadding := False;

    FAllocationControlOption.FData := ' ';
    FAllocationControlOption.FInitalised := False;
    FAllocationControlOption.FLength := 6;
    FAllocationControlOption.FDecimal := 0;
    FAllocationControlOption.FDefaultPadding := False;

//=========================================================

    for LCount := 1 to 5 do
    begin
      FHistManualAnalysis[LCount].FData := 0;
      FHistManualAnalysis[LCount].FInitalised := False;
      FHistManualAnalysis[LCount].FLength := 6;
      FHistManualAnalysis[LCount].FDecimal := 2;
      FHistManualAnalysis[LCount].FDefaultPadding := True;

      FStocManualAnalysis[LCount].FData := 0;
      FStocManualAnalysis[LCount].FInitalised := False;
      FStocManualAnalysis[LCount].FLength := 6;
      FStocManualAnalysis[LCount].FDecimal := 2;
      FStocManualAnalysis[LCount].FDefaultPadding := True;
    end;

    FNoOfFlowsUpperBounds.FData := 0;
    FNoOfFlowsUpperBounds.FInitalised := False;
    FNoOfFlowsUpperBounds.FLength := 6;
    FNoOfFlowsUpperBounds.FDecimal := 0;
    FNoOfFlowsUpperBounds.FDefaultPadding := False;

    

    FNoOfWQConstraints.FData := 0;
    FNoOfWQConstraints.FInitalised := False;
    FNoOfWQConstraints.FLength := 6;
    FNoOfWQConstraints.FDecimal := 0;
    FNoOfWQConstraints.FDefaultPadding := False;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunParametersObject.Reset;
const OPNAME = 'TRunParametersObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TDemandCentreObject }

procedure TDemandCentreObject.CreateMemberObjects;
const OPNAME = 'TDemandCentreObject.CreateMemberObjects';
begin
  inherited;
  try
    FDemandCentreType   := TChar.Create;
    FChannelNumber := TInteger.Create;
    FAnnualDemand  := TDouble.Create;
    FMinimumDemand := TDouble.Create;
    FOutputResults := TChar.Create;
    FComment       := TString.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDemandCentreObject.DestroyMemberObjects;
const OPNAME = 'TDemandCentreObject.DestroyMemberObjects';
begin
  inherited;
  try
    FDemandCentreType.Free;
    FChannelNumber.Free;
    FAnnualDemand.Free;
    FMinimumDemand.Free;
    FOutputResults.Free;
    FComment.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandCentreObject.Initialise: boolean;
const OPNAME = 'TDemandCentreObject.Initialise';
begin
  Result := False;
  try
    FDemandCentreType.FData := ' ';
    FDemandCentreType.FInitalised := False;
    FDemandCentreType.FLength := 2;
    FDemandCentreType.FDecimal := 0;
    FDemandCentreType.FDefaultPadding := False;

    FChannelNumber.FData := 0;
    FChannelNumber.FInitalised := False;
    FChannelNumber.FLength := 4;
    FChannelNumber.FDecimal := 0;
    FChannelNumber.FDefaultPadding := True;

    FAnnualDemand.FInitalised := False;
    FAnnualDemand.FLength := 6;
    FAnnualDemand.FDecimal := 2;
    FAnnualDemand.FDefaultPadding := True;
    FAnnualDemand.ShowDecimalPoint := True;

    FMinimumDemand.FInitalised := False;
    FMinimumDemand.FLength := 6;
    FMinimumDemand.FDecimal := 2;
    FMinimumDemand.FDefaultPadding := True;
    FMinimumDemand.ShowDecimalPoint := True;

    FOutputResults.FData := ' ';
    FOutputResults.FInitalised := False;
    FOutputResults.FLength := 6;
    FOutputResults.FDecimal := 0;
    FOutputResults.FDefaultPadding := False;

    FComment.FInitalised          := False;
    FComment.FData                := '';
    FComment.FDecimal             := 0;
    FComment.FDefaultPadding      := True;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDemandCentreObject.Reset;
const OPNAME = 'TDemandCentreObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TInterBasinTransferObject }

procedure TInterBasinTransferObject.CreateMemberObjects;
const OPNAME = 'TInterBasinTransferObject.CreateMemberObjects';
begin
  inherited;
  try
    FSummaryRequired   := TChar.Create;
    FChannelNumber := TInteger.Create;
    FUpperTransferLimit  := TDouble.Create;
    FDemandCentreNumber := TInteger.Create;
    FComment       := TString.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInterBasinTransferObject.DestroyMemberObjects;
const OPNAME = 'TInterBasinTransferObject.DestroyMemberObjects';
begin
  inherited;
  try
    FSummaryRequired.Free;
    FChannelNumber.Free;
    FUpperTransferLimit.Free;
    FDemandCentreNumber.Free;
    FComment.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TInterBasinTransferObject.Initialise: boolean;
const OPNAME = 'TInterBasinTransferObject.Initialise';
begin
  Result := False;
  try
    FSummaryRequired.FData := ' ';
    FSummaryRequired.FInitalised := False;
    FSummaryRequired.FLength := 2;
    FSummaryRequired.FDecimal := 0;
    FSummaryRequired.FDefaultPadding := False;

    FChannelNumber.FData := 0;
    FChannelNumber.FInitalised := False;
    FChannelNumber.FLength := 4;
    FChannelNumber.FDecimal := 0;
    FChannelNumber.FDefaultPadding := True;

    FUpperTransferLimit.FInitalised := False;
    FUpperTransferLimit.FLength := 7;
    FUpperTransferLimit.FDecimal := 2;
    FUpperTransferLimit.FDefaultPadding := True;
    FUpperTransferLimit.ShowDecimalPoint := True;

    FDemandCentreNumber.FData := 0;
    FDemandCentreNumber.FInitalised := False;
    FDemandCentreNumber.FLength := 6;
    FDemandCentreNumber.FDecimal := 0;
    FDemandCentreNumber.FDefaultPadding := True;

    FComment.FInitalised          := False;
    FComment.FData                := '';
    FComment.FDecimal             := 0;
    FComment.FDefaultPadding      := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInterBasinTransferObject.Reset;
const OPNAME = 'TInterBasinTransferObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TMinMaxBoundsObject }

procedure TMinMaxBoundsObject.CreateMemberObjects;
const OPNAME = 'TMinMaxBoundsObject.CreateMemberObjects';
begin
  inherited;
  try
    FMinMaxBoundedChannel  := TInteger.Create;
    FNoOfRefChannels := TInteger.Create;
    FRefChannels := TStringList.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinMaxBoundsObject.DestroyMemberObjects;
const OPNAME = 'TMinMaxBoundsObject.DestroyMemberObjects';
begin
  inherited;
  try
    FMinMaxBoundedChannel.Free;
    FNoOfRefChannels.Free;
    FRefChannels.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinMaxBoundsObject.Initialise: boolean;
const OPNAME = 'TMinMaxBoundsObject.Initialise';
begin
  Result := False;
  try
    FMinMaxBoundedChannel.FData := 0;
    FMinMaxBoundedChannel.FInitalised := False;
    FMinMaxBoundedChannel.FLength := 6;
    FMinMaxBoundedChannel.FDecimal := 0;
    FMinMaxBoundedChannel.FDefaultPadding := False;

    FNoOfRefChannels.FData := 0;
    FNoOfRefChannels.FInitalised := False;
    FNoOfRefChannels.FLength := 6;
    FNoOfRefChannels.FDecimal := 0;
    FNoOfRefChannels.FDefaultPadding := False;

    FRefChannels.Clear;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinMaxBoundsObject.Reset;
const OPNAME = 'TMinMaxBoundsObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


{ TWQConstraintsObject }

procedure TWQConstraintsObject.CreateMemberObjects;
const OPNAME = 'TWQConstraintsObject.CreateMemberObjects';
begin
  inherited;
  try
    FWQConMinMaxChannel := TInteger.Create;
    FWQConTarget := TDouble.Create;
    FNoOfRefChannelsBlending := TInteger.Create;
    FReservoirRef := TInteger.Create;
    FWQConType := TInteger.Create;
    FReferenceChannel := TStringList.Create;
    FRefChannelFactor := TStringList.Create;
    FSlopeLimit := TInteger.Create;
    FEstimatedRelease := TStringList.Create;
    FConcentration := TStringList.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWQConstraintsObject.DestroyMemberObjects;
const OPNAME = 'TWQConstraintsObject.DestroyMemberObjects';
begin
  inherited;
  try
    FWQConMinMaxChannel.Free;
    FWQConTarget.Free;
    FNoOfRefChannelsBlending.Free;
    FReservoirRef.Free;
    FWQConType.Free;
    FReferenceChannel.Free;
    FRefChannelFactor.Free;
    FSlopeLimit.Free;
    FEstimatedRelease.Free;
    FConcentration.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWQConstraintsObject.Initialise: boolean;
const OPNAME = 'TWQConstraintsObject.Initialise';
begin
  Result := False;
  try
    FWQConMinMaxChannel.FData := 0;
    FWQConMinMaxChannel.FInitalised := False;
    FWQConMinMaxChannel.FLength := 6;
    FWQConMinMaxChannel.FDecimal := 0;
    FWQConMinMaxChannel.FDefaultPadding := False;

    FWQConTarget.FData := 0.;
    FWQConTarget.FInitalised := False;
    FWQConTarget.FLength := 7;
    FWQConTarget.FDecimal := 1;
    FWQConTarget.FDefaultPadding := True;
    FWQConTarget.ShowDecimalPoint := False;

    FNoOfRefChannelsBlending.FData := 0;
    FNoOfRefChannelsBlending.FInitalised := False;
    FNoOfRefChannelsBlending.FLength := 4;
    FNoOfRefChannelsBlending.FDecimal := 0;
    FNoOfRefChannelsBlending.FDefaultPadding := False;

    FReservoirRef.FData := 0;
    FReservoirRef.FInitalised := False;
    FReservoirRef.FLength := 4;
    FReservoirRef.FDecimal := 0;
    FReservoirRef.FDefaultPadding := False;

    FWQConType.FData := 0;
    FWQConType.FInitalised := False;
    FWQConType.FLength := 2;
    FWQConType.FDecimal := 0;
    FWQConType.FDefaultPadding := False;

    FReferenceChannel.Clear;
    FRefChannelFactor.Clear;
    FSlopeLimit.FData := 0;
    FSlopeLimit.FInitalised := False;
    FSlopeLimit.FLength := 6;
    FSlopeLimit.FDecimal := 0;
    FSlopeLimit.FDefaultPadding := False;

    FEstimatedRelease.Clear;
    FConcentration.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWQConstraintsObject.Reset;
const OPNAME = 'TWQConstraintsObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



end.
