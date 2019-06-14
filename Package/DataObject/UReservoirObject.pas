//
//
//  UNIT      : Contains TReservoirObject Class
//  AUTHOR    : Titi Ngubane(PDNA)
//  DATE      : 28/01/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UReservoirObject;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UAbstractDataObject,
  UBasicObjects,
  UConstants,
  VoaimsCom_TLB;

type
  TReservoir = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FIdentifier     :TInteger;

    //Line 2 : Resevior name
    FName : TString;
    FSummaryInclude : TChar;
    FNodeNo : TInteger;
    FPenaltyStructure : TInteger;
    FMaxPoints : TInteger;
    FDrainScale: TDouble;
    FUrbanRunoff: TDouble;
    FForestScale: TDouble;
    FIrrigateScale: TDouble;
    FComment01:TString;
   //FComment02:TString;
    FComment03:TString;
    FNodeType: TInteger;

    //Line 3 : Full reservoir surface area
    FFullSurf: TDouble;
    FRainCoef: TDouble;
    FCatchRef : TInteger;
    FNaturalInflowChannel : TInteger;
    FComment04:TString;

    //Line 4 : Number of power channels downstream of reservoir
    FPowerNum : TInteger;
    FPowerChannels : array[MinPowerChannels..MaxPowerChannels] of TInteger;

    //Line 5 : Surface elevation (m) for each point on the area
    FSurfElev : array[MinPoints..MaxPoints] of TDouble;

    //Line 6 : Volume of reservoir (million m3) corresponding to each point
    FResVol : array[MinPoints..MaxPoints] of TDouble;

    //Line 7 : Surface area (km2) of reservoir corresponding to each point
    FSurfArea : array[MinPoints..MaxPoints] of TDouble;

    //Line 8 : Monthly lake evaporation
    FMonthEvap : array[MinMonths..MaxMonths] of TDouble;

    function Initialise: boolean;override;
    procedure Reset;override;
  end;

  TReservoirObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    //File F02.dat
    //Line 1 : Resevior count and units
    FReserviorNum : TInteger;
    FReserviorAndNodesNum : TInteger;
    FHydroUnits : TString;
    FComment:TString;

    //Line 2..8 : Reseviors data
    FReservoirs: TObjectList;

    //Line 9... : Extra useless lines
    FF02ExtraLines: TStringList;

    procedure Reset;override;
    function Initialise: boolean;override;
    function AddReservoirs: boolean;
    function AddReservoir: TReservoir;
    function CatchmentRefIsUsed(ACatchmentRef: integer): boolean;
    function ReservoirByReservoirNumber(ANodeNumber : integer):TReservoir;
    procedure AddNodeWithoutInflow (ANodeNumber : integer; ANodeType   : integer = ntNodeWithoutInflow);
  end;

implementation


{ TReservoirObject }
uses UErrorHandlingOperations;

procedure TReservoirObject.AddNodeWithoutInflow(ANodeNumber : integer;ANodeType   : integer = ntNodeWithoutInflow);
const OPNAME = 'TReservoirObject.AddNodeWithoutInflow';
var
 LCount: integer;
 LFound: boolean;
 LReservoir :TReservoir;
begin
  try
    LFound := False;
    for LCount := 0 to FReservoirs.Count -1 do
    begin
      LReservoir := TReservoir(FReservoirs.Items[LCount]);
      if (LReservoir.FNodeNo.FData = ANodeNumber) then
      begin
        LFound := True;
        Break;
      end;
    end;
    if not LFound then
    begin
      LReservoir := TReservoir.Create;
      LReservoir.FNodeNo.FData := ANodeNumber;
      LReservoir.FNodeNo.FInitalised := True;
      LReservoir.FName.FData       := IntToStr(ANodeNumber);
      case ANodeType of
        ntReservoir          : LReservoir.FName.FData       := 'Reservoir ' + IntToStr(ANodeNumber);
        ntNodeWithInflow,
        ntNodeWithoutInflow  : LReservoir.FName.FData       := 'Node ' + IntToStr(ANodeNumber);
        ntIrrigationNode     : LReservoir.FName.FData       := 'Irrigation Node ' + IntToStr(ANodeNumber);
        ntWetlandNode        : LReservoir.FName.FData       := 'Wetland Node ' + IntToStr(ANodeNumber);
        ntMineNode           : LReservoir.FName.FData       := 'Mine Node ' + IntToStr(ANodeNumber);
        ntDemandCentreNode   : LReservoir.FName.FData       := 'Demand Centre Node ' + IntToStr(ANodeNumber);
      end;
      LReservoir.FName.FInitalised := True;
      LReservoir.FNodeType.FData := ANodeType;
      LReservoir.FNodeType.FInitalised := True;
      FReservoirs.Add(LReservoir);
      FReserviorAndNodesNum.FData := FReserviorAndNodesNum.FData + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirObject.AddReservoirs: boolean;
const OPNAME = 'TReservoirObject.AddReservoirs';
var
  LCount : Integer;
  LReservoir :TReservoir;
Begin
  Result := False;
  try
    FReservoirs.Clear;
    for LCount := 0 to FReserviorNum.FData do
    begin
      LReservoir := TReservoir.Create;
      FReservoirs.Add(LReservoir);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirObject.AddReservoir: TReservoir;
const OPNAME = 'TReservoirObject.AddReservoir';
var
  LReservoir :TReservoir;
Begin
  Result := nil;
  try
    if(FReservoirs.Count = 0) then
    begin
      LReservoir := TReservoir.Create;
      FReservoirs.Add(LReservoir);
    end;
    LReservoir := TReservoir.Create;
    FReservoirs.Add(LReservoir);
    Result := LReservoir;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirObject.CatchmentRefIsUsed(ACatchmentRef: integer): boolean;
const OPNAME = 'TReservoirObject.CatchmentRefIsUsed';
var
  LCount: integer;
begin
  Result := False;
  try
    for LCount := 0 to FReservoirs.Count -1 do
    begin
      if(TReservoir(FReservoirs.Items[LCount]).FCatchRef.FInitalised) and
        (TReservoir(FReservoirs.Items[LCount]).FCatchRef.FData = ACatchmentRef) then
      begin
        Result := True;
        Break;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirObject.CreateMemberObjects;
const OPNAME = 'TReservoirObject.CreateMemberObjects';
begin
  try
    //File F02.dat
    FReserviorNum := TInteger.Create;
    FReserviorAndNodesNum := TInteger.Create;
    FHydroUnits := TString.Create;
    FComment := TString.Create;

    FReservoirs := TObjectList.Create(True);
    FF02ExtraLines := TStringList.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirObject.DestroyMemberObjects;
const OPNAME = 'TReservoirObject.DestroyMemberObjects';
begin
  try
    //File F02.dat
    FReserviorNum.Free;
    FReserviorAndNodesNum.Free;
    FHydroUnits.Free;
    FComment.Free;
    FReservoirs.Free;
    FF02ExtraLines.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirObject.Initialise: boolean;
const OPNAME = 'TReservoirObject.Initialise';
Begin
  Result := False;
  try
    FReserviorNum.FInitalised := False;
    FReserviorNum.FData := 0;
    FReserviorNum.FLength := 5;
    FReserviorNum.FDecimal := 0;
    FReserviorNum.FDefaultPadding := True;

    FReserviorAndNodesNum.FInitalised := False;
    FReserviorAndNodesNum.FData := 0;
    FReserviorAndNodesNum.FLength := 5;
    FReserviorAndNodesNum.FDecimal := 0;
    FReserviorAndNodesNum.FDefaultPadding := True;

    FHydroUnits.FInitalised := False;
    FHydroUnits.FData := '';
    FHydroUnits.FLength := 3;
    FHydroUnits.FDecimal := 0;
    FHydroUnits.FDefaultPadding := True;

    FComment.FInitalised := False;
    FComment.FData := '';
    FComment.FDecimal := 0;
    FComment.FDefaultPadding := True;

    FReservoirs.Clear;
    FF02ExtraLines.Clear;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TReservoirObject.ReservoirByReservoirNumber(ANodeNumber: integer): TReservoir;
const OPNAME = 'TReservoirObject.ReservoirByReservoirNumber';
var
  LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FReservoirs.Count-1 do
    begin
      if(TReservoir(FReservoirs.Items[LIndex]).FNodeNo.FData = ANodeNumber) then
      begin
        Result := TReservoir(FReservoirs.Items[LIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirObject.Reset;
const OPNAME = 'TReservoirObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TReservoir }
procedure TReservoir.CreateMemberObjects;
const OPNAME = 'TReservoir.CreateMemberObjects';
var
  LCount: integer;
  LDouble : TDouble;
  LInteger: TInteger;
Begin
  try
    FIdentifier  := TInteger.Create;

    FName := TString.Create;
    FSummaryInclude := TChar.Create;
    FNodeNo := TInteger.Create;
    FPenaltyStructure := TInteger.Create;
    FMaxPoints  := TInteger.Create;
    FDrainScale := TDouble.Create;
    FUrbanRunoff := TDouble.Create;
    FForestScale := TDouble.Create;
    FIrrigateScale := TDouble.Create;
    FFullSurf := TDouble.Create;
    FRainCoef := TDouble.Create;
    FCatchRef := TInteger.Create;
    FNaturalInflowChannel := TInteger.Create;
    FPowerNum := TInteger.Create;
    for LCount := 1 to 20 do
    begin
      LInteger := TInteger.Create;
      FPowerChannels[LCount] := LInteger;
    end;
    for LCount := 1 to 15 do
    begin
      LDouble := TDouble.Create;
      FSurfElev[LCount] := LDouble;
      LDouble := TDouble.Create;
      FResVol[LCount] := LDouble;
      LDouble := TDouble.Create;
      FSurfArea[LCount] := LDouble;
    end;
    for LCount := 1 to 12 do
    begin
      LDouble := TDouble.Create;
      FMonthEvap[LCount] := LDouble;
    end;
    FComment01 := TString.Create;
   // FComment02 := TString.Create;
    FComment03 := TString.Create;
    FComment04 := TString.Create;
    FNodeType  := TInteger.Create;

    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoir.DestroyMemberObjects;
const OPNAME = 'TReservoir.DestroyMemberObjects';
var
  LCount : Integer;
Begin
  try
    FIdentifier.Free;

    FName.Free;
    FSummaryInclude.Free;
    FNodeNo.Free;
    FPenaltyStructure.Free;
    FMaxPoints.Free;
    FDrainScale.Free;
    FUrbanRunoff.Free;
    FForestScale.Free;
    FIrrigateScale.Free;
    FFullSurf.Free;
    FRainCoef.Free;
    FCatchRef.Free;
    FNaturalInflowChannel.Free;
    FPowerNum .Free;
    for LCount := 1 to 20 do
      FPowerChannels[LCount].Free;
    for LCount := 1 to 15 do
    begin
      FSurfElev[LCount].Free;
      FResVol[LCount].Free;
      FSurfArea[LCount].Free;
    end;
    for LCount := 1 to 12 do
      FMonthEvap[LCount].Free;

    FComment01.Free;
  //  FComment02.Free;
    FComment03.Free;
    FComment04.Free;
    FNodeType.Free;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoir.Initialise: boolean;
const OPNAME = 'TReservoir.Initialise';
var
  LCount : Integer;
Begin
  Result := False;
  try
    //Data and initialised.
    FIdentifier.FInitalised := False;
    FIdentifier.FData := 0;
    FIdentifier.FLength := 5;
    FIdentifier.FDecimal := 0;
    FIdentifier.FDefaultPadding := True;

    FName.FData := '';
    FName.FInitalised := False;
    FName.FLength := 36;
    FName.FDecimal := 0;
    FName.FDefaultPadding := True;

    FSummaryInclude.FData := ' ';
    FSummaryInclude.FInitalised := False;
    FSummaryInclude.FLength := 1;
    FSummaryInclude.FDecimal := 0;
    FSummaryInclude.FDefaultPadding := True;

    FNodeNo.FData := 0;
    FNodeNo.FInitalised := False;
    FNodeNo.FLength := 5;
    FNodeNo.FDecimal := 0;
    FNodeNo.FDefaultPadding := True;

    FPenaltyStructure.FData := 0;
    FPenaltyStructure.FInitalised := False;
    FPenaltyStructure.FLength := 5;
    FPenaltyStructure.FDecimal := 0;
    FPenaltyStructure.FDefaultPadding := True;

    FMaxPoints.FData := 0;
    FMaxPoints.FInitalised := False;
    FMaxPoints.FLength := 5;
    FMaxPoints.FDecimal := 0;
    FMaxPoints.FDefaultPadding := True;

    FDrainScale.FData := 0;
    FDrainScale.FInitalised := False;
    FDrainScale.FLength := 10;
    FDrainScale.FDecimal := 3;
    FDrainScale.FDefaultPadding := True;
    FDrainScale.ShowDecimalPoint := True;

    FUrbanRunoff.FData := 0;
    FUrbanRunoff.FInitalised := False;
    FUrbanRunoff.FLength := 10;
    FUrbanRunoff.FDecimal := 3;
    FUrbanRunoff.FDefaultPadding := True;
    FUrbanRunoff.ShowDecimalPoint := True;

    FForestScale.FData := 0;
    FForestScale.FInitalised := False;
    FForestScale.FLength := 10;
    FForestScale.FDecimal := 3;
    FForestScale.FDefaultPadding := True;
    FForestScale.ShowDecimalPoint := True;

    FIrrigateScale.FData := 0;
    FIrrigateScale.FInitalised := False;
    FIrrigateScale.FLength := 10;
    FIrrigateScale.FDecimal := 3;
    FIrrigateScale.FDefaultPadding := True;
    FIrrigateScale.ShowDecimalPoint := True;

    FFullSurf.FData := 0;
    FFullSurf.FInitalised := False;
    FFullSurf.FLength := 10;
    FFullSurf.FDecimal := 3;
    FFullSurf.FDefaultPadding := True;
    FFullSurf.ShowDecimalPoint := True;

    FRainCoef.FData := 0;
    FRainCoef.FInitalised := False;
    FRainCoef.FLength := 10;
    FRainCoef.FDecimal := 3;
    FRainCoef.FDefaultPadding := True;
    FRainCoef.ShowDecimalPoint := True;

    FCatchRef.FData := 0;
    FCatchRef.FInitalised := False;
    FCatchRef.FLength := 5;
    FCatchRef.FDecimal := 0;
    FCatchRef.FDefaultPadding := True;

    FNaturalInflowChannel.FData := 0;
    FNaturalInflowChannel.FInitalised := False;
    FNaturalInflowChannel.FLength := 5;
    FNaturalInflowChannel.FDecimal := 0;
    FNaturalInflowChannel.FDefaultPadding := True;

    FPowerNum .FData := 0;
    FPowerNum .FInitalised := False;
    FPowerNum .FLength := 5;
    FPowerNum .FDecimal := 0;
    FPowerNum.FDefaultPadding := True;

    for LCount := MinPowerChannels to MaxPowerChannels do
    begin
      FPowerChannels[LCount].FData := 0;
      FPowerChannels[LCount].FInitalised := False;
      FPowerChannels[LCount].FLength := 5;
      FPowerChannels[LCount].FDecimal := 0;
      FPowerChannels[LCount].FDefaultPadding := True;
    end;

    for LCount := MinPoints to MaxPoints do
    begin
      FSurfElev[LCount].FData := 0;
      FSurfElev[LCount].FInitalised := False;
      FSurfElev[LCount].FLength := 10;
      FSurfElev[LCount].FDecimal := 2;
      FSurfElev[LCount].FDefaultPadding := True;
      FSurfElev[LCount].ShowDecimalPoint := True;

      FResVol[LCount].FData := 0;
      FResVol[LCount].FInitalised := False;
      FResVol[LCount].FLength := 10;
      FResVol[LCount].FDecimal := 3;
      FResVol[LCount].FDefaultPadding := True;
      FResVol[LCount].ShowDecimalPoint := True;

      FSurfArea[LCount].FData := 0;
      FSurfArea[LCount].FInitalised := False;
      FSurfArea[LCount].FLength := 10;
      FSurfArea[LCount].FDecimal := 3;
      FSurfArea[LCount].FDefaultPadding := True;
      FSurfArea[LCount].ShowDecimalPoint := True;
    end;

    for LCount := MinMonths to MaxMonths do
    begin
      FMonthEvap[LCount].FData := 0;
      FMonthEvap[LCount].FInitalised := False;
      FMonthEvap[LCount].FLength := 10;
      FMonthEvap[LCount].FDecimal := 2;
      FMonthEvap[LCount].FDefaultPadding := True;
      FMonthEvap[LCount].ShowDecimalPoint := True;
    end;
    FComment01.FInitalised := False;
    FComment01.FData := '';
    FComment01.FDecimal := 0;
    FComment01.FDefaultPadding := True;

   { FComment02.FInitalised := False;
    FComment02.FData := '';
    FComment02.FDecimal := 0;
    FComment02.FDefaultPadding := True;}

    FComment03.FInitalised := False;
    FComment03.FData := '';
    FComment03.FDecimal := 0;
    FComment03.FDefaultPadding := True;

    FComment04.FInitalised := False;
    FComment04.FData := '';
    FComment04.FDecimal := 0;
    FComment04.FDefaultPadding := True;

    FNodeType.FInitalised := False;
    FNodeType.FData := 0;
    FNodeType.FLength := 5;
    FNodeType.FDecimal := 0;
    FNodeType.FDefaultPadding := False;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoir.Reset;
const OPNAME = 'TReservoir.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
