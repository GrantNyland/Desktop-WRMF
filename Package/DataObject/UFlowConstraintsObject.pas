//
//
//  UNIT      : Contains TFlowConstraintsObject Class
//  AUTHOR    : Titi Ngubane(PDNA)
//  DATE      : 28/01/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UFlowConstraintsObject;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UAbstractDataObject,
  UBasicObjects,
  UConstants;

type
  TStructure = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FIdentifier: integer;
    //Line 2 : Channel number limited by control structure
    FChannelNum                 :TInteger;
    FUpstreamReservoirNum       :TInteger;
    FDownstreamReservoirNum     :TInteger;
    FPointsElevationNum         :TInteger;
    FSillElevation              :TDouble;
    FComment                    : TString;
    // FF4Line3ExtraChars      : TString;
    FGateHeight                 :TDouble;
    FStructureType              :TInteger;
    FDischargeCoefficient       :TDouble;
    FControlStructureLength     :TDouble;
    FWaterLevelAtDownstreamNode : TDouble;
    FReservoirElevation         : TDouble;
    FElevations                 : array[MinCurvePoints..MaxCurvePoints] of TDouble;
    FDischarges                 : array[MinCurvePoints..MaxCurvePoints] of TDouble;
    FChannelNumbers             : array[MinCurvePoints..MaxCurvePoints] of TDouble;
    FKFactors                   : array[MinCurvePoints..MaxCurvePoints] of TDouble;
    FHeadDifferences            : array[MinCurvePoints..MaxCurvePoints] of TDouble;
    FAquiferFlows               : array[MinCurvePoints..MaxCurvePoints] of TDouble;
    FDownStreamNodeInflows      : array[MinCurvePoints..MaxCurvePoints] of TDouble;
    FRiverDepths                : array[MinCurvePoints..MaxCurvePoints] of TDouble;
    FElevationDifferences       : array[MinCurvePoints..MaxCurvePoints] of TDouble;
    FMonthlyAverageInflows      : array[MinCurvePoints..MaxCurvePoints] of TDouble;
    FMonthlyAverageDivertedFlow : array[MinCurvePoints..MaxCurvePoints,MinCurvePoints..MaxCurvePoints] of TDouble;
    FPumpingHeads               : array[MinCurvePoints..MaxCurvePoints] of TDouble;
    FPumpingDischarges          : array[MinCurvePoints..MaxCurvePoints] of TDouble;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TFlowConstraintsObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    //File F04.dat
    //Line 1 : Control structures count
    FControlStructureCount : TInteger;
    FF4Line1ExtraChars:TString;

    //Line 2..4 : Control structure data
    FStructure: TObjectList;

    //Line 5... : Extra useless lines
    FF04ExtraLines: TStringList;
    procedure Reset;override;
    function Initialise: boolean;override;
    function AddStructures: boolean;
  end;

implementation


{ TFlowConstraintsObject }
uses UErrorHandlingOperations;

procedure TFlowConstraintsObject.CreateMemberObjects;
const OPNAME = 'TFlowConstraintsObject.CreateMemberObjects';
begin
  try
    //File F04.dat
    FControlStructureCount:= TInteger.Create;
    FF4Line1ExtraChars := TString.Create;

    FStructure := TObjectList.Create(True);
    FF04ExtraLines := TStringList.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFlowConstraintsObject.DestroyMemberObjects;
const OPNAME = 'TFlowConstraintsObject.DestroyMemberObjects';
begin
  try
    //File F04.dat
    FControlStructureCount.Free;
    FF4Line1ExtraChars.Free;
    FStructure.Free;
    FF04ExtraLines.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFlowConstraintsObject.Initialise: boolean;
const OPNAME = 'TFlowConstraintsObject.Initialise';
Begin
  Result := False;
  try
    FControlStructureCount.FData := 0;
    FControlStructureCount.FInitalised := False;
    FControlStructureCount.FLength := 5;
    FControlStructureCount.FDecimal := 0;
    FControlStructureCount.FDefaultPadding := True;

    FF4Line1ExtraChars.FData := '';
    FF4Line1ExtraChars.FInitalised := False;
    FF4Line1ExtraChars.FLength := 2;
    FF4Line1ExtraChars.FDecimal := 0;
    FF4Line1ExtraChars.FDefaultPadding := True;

    FStructure.Clear;
    FF04ExtraLines.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFlowConstraintsObject.AddStructures: boolean;
const OPNAME = 'TFlowConstraintsObject.AddStructures';
var
  LCount : Integer;
  LStructure :TStructure;
Begin
  Result := False;
  try
    FStructure.Clear;
    for LCount := 0 to FControlStructureCount.FData-1 do
    begin
      LStructure := TStructure.Create;
      FStructure.Add(LStructure);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TFlowConstraintsObject.Reset;
const OPNAME = 'TFlowConstraintsObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TStructure }
procedure TStructure.CreateMemberObjects;
const OPNAME = 'TStructure.CreateMemberObjects';
var
  LCount,
  LCount2: integer;
  //LDouble : TDouble;
Begin
  try
    FChannelNum:= TInteger.Create;
    FUpstreamReservoirNum := TInteger.Create;
    FDownstreamReservoirNum := TInteger.Create;

    FPointsElevationNum := TInteger.Create;
    FSillElevation := TDouble.Create;
    FGateHeight     := TDouble.Create;
    FStructureType  := TInteger.Create;
    FDischargeCoefficient   := TDouble.Create;
    FControlStructureLength := TDouble.Create;
    FWaterLevelAtDownstreamNode := TDouble.Create;
    FReservoirElevation := TDouble.Create;

    for LCount := MinCurvePoints to MaxCurvePoints do
    begin
      FElevations[LCount]              := TDouble.Create;
      FDischarges[LCount]              := TDouble.Create;
      FChannelNumbers[LCount]          := TDouble.Create;
      FKFactors[LCount]                := TDouble.Create;
      FHeadDifferences[LCount]         := TDouble.Create;
      FAquiferFlows[LCount]            := TDouble.Create;
      FDownStreamNodeInflows[LCount]   := TDouble.Create;
      FRiverDepths[LCount]             := TDouble.Create;
      FElevationDifferences[LCount]    := TDouble.Create;
      FMonthlyAverageInflows[LCount]   := TDouble.Create;
      FPumpingHeads[LCount]            := TDouble.Create;
      FPumpingDischarges[LCount]       := TDouble.Create;
      for LCount2 := MinCurvePoints to MaxCurvePoints do
        FMonthlyAverageDivertedFlow[LCount,LCount2] := TDouble.Create;
    end;

    FComment:=TString.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStructure.DestroyMemberObjects;
const OPNAME = 'TStructure.DestroyMemberObjects';
var
  LCount,
  LCount2 : Integer;
Begin
  try
    FChannelNum.Free;
    FUpstreamReservoirNum.Free;
    FDownstreamReservoirNum.Free;
    FPointsElevationNum.Free;
    FSillElevation.Free;
    FGateHeight.Free;
    FStructureType.Free;
    FDischargeCoefficient.Free;
    FControlStructureLength.Free;
    FWaterLevelAtDownstreamNode.Free;
    FReservoirElevation.Free;

    for LCount := MinCurvePoints to MaxCurvePoints do
    begin
      FElevations[LCount].Free;
      FDischarges[LCount].Free;
      FChannelNumbers[LCount].Free;
      FKFactors[LCount].Free;
      FHeadDifferences[LCount].Free;
      FAquiferFlows[LCount].Free;
      FDownStreamNodeInflows[LCount].Free;
      FRiverDepths[LCount].Free;
      FElevationDifferences[LCount].Free;
      FMonthlyAverageInflows[LCount].Free;
      FPumpingHeads[LCount].Free;
      FPumpingDischarges[LCount].Free;
      for LCount2 := MinCurvePoints to MaxCurvePoints do
        FMonthlyAverageDivertedFlow[LCount,LCount2].Free;
    end;

    FComment.free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStructure.Initialise: boolean;
const OPNAME = 'TStructure.Initialise';
var
  LCount,
  LCount2 : Integer;

Begin
  Result := False;
  try
    FIdentifier := 0;

    //Data and initialised.
    FChannelNum.FData := 0;
    FChannelNum.FInitalised := False;
    FChannelNum.FLength := 5;
    FChannelNum.FDecimal := 0;
    FChannelNum.FDefaultPadding := True;

    FUpstreamReservoirNum.FData := 0;
    FUpstreamReservoirNum.FInitalised := False;
    FUpstreamReservoirNum.FLength := 5;
    FUpstreamReservoirNum.FDecimal := 0;
    FUpstreamReservoirNum.FDefaultPadding := True;

    FDownstreamReservoirNum.FData := 0;
    FDownstreamReservoirNum.FInitalised := False;
    FDownstreamReservoirNum.FLength := 5;
    FDownstreamReservoirNum.FDecimal := 0;
    FDownstreamReservoirNum.FDefaultPadding := True;

    FPointsElevationNum.FData := 0;
    FPointsElevationNum.FInitalised := False;
    FPointsElevationNum.FLength := 5;
    FPointsElevationNum.FDecimal := 0;
    FPointsElevationNum.FDefaultPadding := True;

    FSillElevation.FData := 0;
    FSillElevation.FInitalised := False;
    FSillElevation.FLength := 10;
    FSillElevation.FDecimal := 2;
    FSillElevation.FDefaultPadding := True;
    FSillElevation.ShowDecimalPoint := True;

    FWaterLevelAtDownstreamNode.FData := 0;
    FWaterLevelAtDownstreamNode.FInitalised := False;
    FWaterLevelAtDownstreamNode.FLength := 10;
    FWaterLevelAtDownstreamNode.FDecimal := 2;
    FWaterLevelAtDownstreamNode.FDefaultPadding := True;
    FWaterLevelAtDownstreamNode.ShowDecimalPoint := True;

    FReservoirElevation.FData := 0;
    FReservoirElevation.FInitalised := False;
    FReservoirElevation.FLength := 10;
    FReservoirElevation.FDecimal := 2;
    FReservoirElevation.FDefaultPadding := True;
    FReservoirElevation.ShowDecimalPoint := True;

    FGateHeight.FData := 0;
    FGateHeight.FInitalised := False;
    FGateHeight.FLength := 10;
    FGateHeight.FDecimal := 2;
    FGateHeight.FDefaultPadding := True;
    FGateHeight.ShowDecimalPoint := True;

    FStructureType.FData := 0;
    FStructureType.FInitalised := False;
    FStructureType.FLength := 2;
    FStructureType.FDecimal := 0;
    FStructureType.FDefaultPadding := True;

    FDischargeCoefficient.FData := 0;
    FDischargeCoefficient.FInitalised := False;
    FDischargeCoefficient.FLength := 10;
    FDischargeCoefficient.FDecimal := 2;
    FDischargeCoefficient.FDefaultPadding := True;
    FDischargeCoefficient.ShowDecimalPoint := True;

    FControlStructureLength.FData := 0;
    FControlStructureLength.FInitalised := False;
    FControlStructureLength.FLength := 10;
    FControlStructureLength.FDecimal := 2;
    FControlStructureLength.FDefaultPadding := True;
    FControlStructureLength.ShowDecimalPoint := True;


    for LCount := MinCurvePoints to MaxCurvePoints do
    begin
      FElevations[LCount].FData := 0;
      FElevations[LCount].FInitalised := False;
      FElevations[LCount].FLength := 8;
      FElevations[LCount].FDecimal := 2;
      FElevations[LCount].FDefaultPadding := True;
      FElevations[LCount].ShowDecimalPoint := True;

      FDischarges[LCount].FData := 0;
      FDischarges[LCount].FInitalised := False;
      FDischarges[LCount].FLength := 8;
      FDischarges[LCount].FDecimal := 2;
      FDischarges[LCount].FDefaultPadding := True;
      FDischarges[LCount].ShowDecimalPoint := True;

      FChannelNumbers[LCount].FData := 0;
      FChannelNumbers[LCount].FInitalised := False;
      FChannelNumbers[LCount].FLength := 8;
      FChannelNumbers[LCount].FDecimal := 2;
      FChannelNumbers[LCount].FDefaultPadding := True;
      FChannelNumbers[LCount].ShowDecimalPoint := True;

      FKFactors[LCount].FData := 0;
      FKFactors[LCount].FInitalised := False;
      FKFactors[LCount].FLength := 8;
      FKFactors[LCount].FDecimal := 2;
      FKFactors[LCount].FDefaultPadding := True;
      FKFactors[LCount].ShowDecimalPoint := True;

      FHeadDifferences[LCount].FData := 0;
      FHeadDifferences[LCount].FInitalised := False;
      FHeadDifferences[LCount].FLength := 8;
      FHeadDifferences[LCount].FDecimal := 2;
      FHeadDifferences[LCount].FDefaultPadding := True;
      FHeadDifferences[LCount].ShowDecimalPoint := True;

      FAquiferFlows[LCount].FData := 0;
      FAquiferFlows[LCount].FInitalised := False;
      FAquiferFlows[LCount].FLength := 8;
      FAquiferFlows[LCount].FDecimal := 2;
      FAquiferFlows[LCount].FDefaultPadding := True;
      FAquiferFlows[LCount].ShowDecimalPoint := True;

      FDownStreamNodeInflows[LCount].FData := 0;
      FDownStreamNodeInflows[LCount].FInitalised := False;
      FDownStreamNodeInflows[LCount].FLength := 8;
      FDownStreamNodeInflows[LCount].FDecimal := 2;
      FDownStreamNodeInflows[LCount].FDefaultPadding := True;
      FDownStreamNodeInflows[LCount].ShowDecimalPoint := True;

      FRiverDepths[LCount].FData := 0;
      FRiverDepths[LCount].FInitalised := False;
      FRiverDepths[LCount].FLength := 8;
      FRiverDepths[LCount].FDecimal := 2;
      FRiverDepths[LCount].FDefaultPadding := True;
      FRiverDepths[LCount].ShowDecimalPoint := True;

      FElevationDifferences[LCount].FData := 0;
      FElevationDifferences[LCount].FInitalised := False;
      FElevationDifferences[LCount].FLength := 8;
      FElevationDifferences[LCount].FDecimal := 2;
      FElevationDifferences[LCount].FDefaultPadding := True;
      FElevationDifferences[LCount].ShowDecimalPoint := True;

      FMonthlyAverageInflows[LCount].FData := 0;
      FMonthlyAverageInflows[LCount].FInitalised := False;
      FMonthlyAverageInflows[LCount].FLength := 8;
      FMonthlyAverageInflows[LCount].FDecimal := 2;
      FMonthlyAverageInflows[LCount].FDefaultPadding := True;
      FMonthlyAverageInflows[LCount].ShowDecimalPoint := True;

      FPumpingHeads[LCount].FData := 0;
      FPumpingHeads[LCount].FInitalised := False;
      FPumpingHeads[LCount].FLength := 8;
      FPumpingHeads[LCount].FDecimal := 2;
      FPumpingHeads[LCount].FDefaultPadding := True;
      FPumpingHeads[LCount].ShowDecimalPoint := True;

      FPumpingDischarges[LCount].FData := 0;
      FPumpingDischarges[LCount].FInitalised := False;
      FPumpingDischarges[LCount].FLength := 8;
      FPumpingDischarges[LCount].FDecimal := 3;
      FPumpingDischarges[LCount].FDefaultPadding := True;
      FPumpingDischarges[LCount].ShowDecimalPoint := True;

      for LCount2 := MinCurvePoints to MaxCurvePoints do
      begin
        FMonthlyAverageDivertedFlow[LCount,LCount2].FData := 0;
        FMonthlyAverageDivertedFlow[LCount,LCount2].FInitalised := False;
        FMonthlyAverageDivertedFlow[LCount,LCount2].FLength := 8;
        FMonthlyAverageDivertedFlow[LCount,LCount2].FDecimal := 2;
        FMonthlyAverageDivertedFlow[LCount,LCount2].FDefaultPadding := True;
        FMonthlyAverageDivertedFlow[LCount,LCount2].ShowDecimalPoint := True;
      end;
    end;

    FComment.FData := '';
    FComment.FInitalised := False;
    FComment.FDecimal := 0;
    FComment.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStructure.Reset;
const OPNAME = 'TStructure.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
