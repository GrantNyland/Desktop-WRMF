//
//
//  UNIT      : Contains TZoneDefinitionsObject Class
//  AUTHOR    : Titi Ngubane(PDNA)
//  DATE      : 14/02/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UZoneDefinitionsObject;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UAbstractDataObject,
  UBasicObjects,
  UConstants;

type

  TZonePenaltyValue = array[MinPenaltyZone..MaxPenaltyZone] of Double;
  TZonePenaltyName  = array[MinPenaltyZone..MaxPenaltyZone] of string;
  TAverageMonthlyLevel = array[MinAverageMonthlyLevel..MaxAverageMonthlyLevel] of Double;

  TReservoirPenalty = Class(TObject)
  public
    FPenaltyCount: Integer;
    FPenaltyValue: array[MinPenaltyZone..MaxPenaltyZone] of Double;
    FPenaltyDescr: array[MinPenaltyZone..MaxPenaltyZone] of String;
    FPenaltyNames: array[MinPenaltyZone..MaxPenaltyZone] of String;
    procedure Reset;
  end;

  TStorageZones = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    //Line 1a
    FResevoirZoneName       :TString;
    FStrategyIndicator      :TInteger;
    FBalancingVariable      :TInteger;
    FBalancingPolicy        :TInteger;
    FBalanceReference       :array[MinPenaltyStructure..MaxPenaltyStructure] of TDouble;
    FComment: TString;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TNodes = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    //Line 2
    FNodeNumberStorage      :TInteger;
    FStatusIndicator        :TInteger;
    FPlottingOption         :TInteger;
    FReservoirPriority      :TDouble;
    FFullSupplyLevel        :TDouble;
    FDeadStorageLevel       :TDouble;
    FBottomOfReservoir      :TDouble;
    FFullSupplyAllocation   :TDouble;
    FComment                :TString;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TReservoirLevels = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    //Line 3
    FReservoirIdentifier    :TInteger;
    FResLevel               :array[MinReservoirMonths..MaxReservoirMonths] of TDouble;
    FComment: TString;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TZoneDefinitionsObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    //File F05.dat
    //Line 1
    FStorageZoneCount : TInteger;
    FZoneLowerBoundary : TInteger;
    FPenaltyStructureCount : TInteger;
    FNodesCount : TInteger;
    FReservoirLevelsCount : TInteger;
    FComment: TString;
    FZeroComment            : TString;

    //Line 1a
    FStorageZones : TObjectList;

    //Line 2
    FNodes        : TObjectList;

    //Line 3
    FReservoirLevels: TObjectList;

    //Line 4... : Extra useless lines
    FF05ExtraLines: TStringList;

    function AddStorageZones : boolean;
    function AddNode        : boolean;
    function AddReservoirLevels  : boolean;
    function GetReservoirPenalty(AReservoirNumber,APenaltyCol: integer; var AReservoirPenalty: TReservoirPenalty): boolean;
    function GetNode(ANodeNumber: integer): TNodes;
    function GetReservoirLevel(AReservoirNumber: integer):TReservoirLevels;
    function GetZonePenalty(AZoneType: integer;
                            var AZonePenaltyValue:TZonePenaltyValue;
                            var AZonePenaltyName:TZonePenaltyName): boolean;
    function GetAverageMonthlyLevels(AReservoirNumber: integer;var AAverageMonthlyLevel:TAverageMonthlyLevel): boolean;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

implementation


uses UErrorHandlingOperations;

procedure TZoneDefinitionsObject.CreateMemberObjects;
const OPNAME = 'TZoneDefinitionsObject.CreateMemberObjects';
begin
  try
    //File F05.dat
    FStorageZoneCount     := TInteger.Create;
    FZoneLowerBoundary    := TInteger.Create;
    FPenaltyStructureCount:= TInteger.Create;
    FNodesCount           := TInteger.Create;
    FReservoirLevelsCount := TInteger.Create;
    FComment              := TString.Create;
    FZeroComment          := TString.Create;

    FStorageZones         := TObjectList.Create(True);
    FNodes                := TObjectList.Create(True);
    FReservoirLevels      := TObjectList.Create(True);
    FF05ExtraLines        := TStringList.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TZoneDefinitionsObject.DestroyMemberObjects;
const OPNAME = 'TZoneDefinitionsObject.DestroyMemberObjects';
begin
  try
    //File F05.dat
    FStorageZoneCount.Free;
    FZoneLowerBoundary.Free;
    FPenaltyStructureCount.Free;
    FNodesCount.Free;
    FReservoirLevelsCount.Free;
    FComment.Free;
    FZeroComment.Free;

    FStorageZones.Free;
    FNodes.Free;
    FReservoirLevels.Free;
    FF05ExtraLines.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TZoneDefinitionsObject.Initialise: boolean;
const OPNAME = 'TZoneDefinitionsObject.Initialise';
Begin
  Result := False;
  try
    FStorageZoneCount.FData := 0;
    FStorageZoneCount.FInitalised := False;
    FStorageZoneCount.FLength := 5;
    FStorageZoneCount.FDecimal := 0;
    FStorageZoneCount.FDefaultPadding := True;

    FZoneLowerBoundary.FData := 0;
    FZoneLowerBoundary.FInitalised := False;
    FZoneLowerBoundary.FLength := 5;
    FZoneLowerBoundary.FDecimal := 0;
    FZoneLowerBoundary.FDefaultPadding := True;

    FPenaltyStructureCount.FData := 0;
    FPenaltyStructureCount.FInitalised := False;
    FPenaltyStructureCount.FLength := 5;
    FPenaltyStructureCount.FDecimal := 0;
    FPenaltyStructureCount.FDefaultPadding := True;

    FNodesCount.FData := 0;
    FNodesCount.FInitalised := False;
    FNodesCount.FLength := 5;
    FNodesCount.FDecimal := 0;
    FNodesCount.FDefaultPadding := True;

    FReservoirLevelsCount.FData := 0;
    FReservoirLevelsCount.FInitalised := False;
    FReservoirLevelsCount.FLength := 5;
    FReservoirLevelsCount.FDecimal := 0;
    FReservoirLevelsCount.FDefaultPadding := True;

    FComment.FData := '';
    FComment.FInitalised := False;
    FComment.FDecimal := 0;
    FComment.FDefaultPadding := True;

    FZeroComment.FData := '';
    FZeroComment.FInitalised := False;
    FZeroComment.FDecimal := 0;
    FZeroComment.FDefaultPadding := False;

    FStorageZones.Clear;
    FNodes.Clear;
    FReservoirLevels.Clear;
    FF05ExtraLines.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TZoneDefinitionsObject.AddStorageZones: boolean;
const OPNAME = 'TZoneDefinitionsObject.AddStorageZones';
var
  LCount : Integer;
  LStorageZone :TStorageZones;
Begin
  Result := False;
  try
    FStorageZones.Clear;
    for LCount := 0 to FStorageZoneCount.FData do
    begin
      LStorageZone := TStorageZones.Create;
      FStorageZones.Add(LStorageZone);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TZoneDefinitionsObject.AddNode: boolean;
const OPNAME = 'TZoneDefinitionsObject.AddNode';
var
  LNode :TNodes;
Begin
  Result := False;
  try
    if FNodes.Count = 0 then
    begin
      LNode := TNodes.Create;
      FNodes.Add(LNode);
    end;

    LNode := TNodes.Create;
    FNodes.Add(LNode);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TZoneDefinitionsObject.AddReservoirLevels: boolean;
const OPNAME = 'TZoneDefinitionsObject.AddReservoirLevels';
var
  LCount : Integer;
  LReservoirLevel :TReservoirLevels;
Begin
  Result := False;
  try
    FReservoirLevels.Clear;
    for LCount := 0 to FReservoirLevelsCount.FData do
    begin
      LReservoirLevel := TReservoirLevels.Create;
      FReservoirLevels.Add(LReservoirLevel);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TStorageZones.CreateMemberObjects;
const OPNAME = 'TStorageZones.CreateMemberObjects';
var
  LCount: integer;
  LDouble : TDouble;
Begin
  try
    FResevoirZoneName:= TString.Create;
    FStrategyIndicator := TInteger.Create;
    FBalancingVariable := TInteger.Create;
    FBalancingPolicy := TInteger.Create;
    FComment := TString.Create;

    for LCount := MinPenaltyStructure to MaxPenaltyStructure do
    begin
      LDouble := TDouble.Create;
      FBalanceReference[LCount] := LDouble;
    end;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStorageZones.DestroyMemberObjects;
const OPNAME = 'TStorageZones.DestroyMemberObjects';
var
  LCount : Integer;
Begin
  try
    FResevoirZoneName.Free;
    FStrategyIndicator.Free;
    FBalancingVariable.Free;
    FBalancingPolicy.Free;
    FComment.Free;
    for LCount := MinPenaltyStructure to MaxPenaltyStructure do
      FBalanceReference[LCount].Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStorageZones.Initialise: boolean;
const OPNAME = 'TStorageZones.Initialise';
var
  LCount : Integer;
Begin
  Result := False;
  try
    //Data and initialised.
    FResevoirZoneName.FData := '';
    FResevoirZoneName.FInitalised := False;
    FResevoirZoneName.FLength := 6;
    FResevoirZoneName.FDecimal := 0;
    FResevoirZoneName.FDefaultPadding := True;

    FStrategyIndicator.FData := 0;
    FStrategyIndicator.FInitalised := False;
    FStrategyIndicator.FLength := 5;
    FStrategyIndicator.FDecimal := 0;
    FStrategyIndicator.FDefaultPadding := True;

    FBalancingVariable.FData := 0;
    FBalancingVariable.FInitalised := False;
    FBalancingVariable.FLength := 5;
    FBalancingVariable.FDecimal := 0;
    FBalancingVariable.FDefaultPadding := True;

    FBalancingPolicy.FData := 0;
    FBalancingPolicy.FInitalised := False;
    FBalancingPolicy.FLength := 5;
    FBalancingPolicy.FDecimal := 0;
    FBalancingPolicy.FDefaultPadding := True;

    FComment.FData := '';
    FComment.FInitalised := False;
    FComment.FDecimal := 0;
    FComment.FDefaultPadding := True;

    for LCount := MinPenaltyStructure to MaxPenaltyStructure do
    begin
      FBalanceReference[LCount].FData := 0;
      FBalanceReference[LCount].FInitalised := False;
      FBalanceReference[LCount].FLength := 10;
      FBalanceReference[LCount].FDecimal := 0;
      FBalanceReference[LCount].FDefaultPadding := True;
      FBalanceReference[LCount].ShowDecimalPoint := True;
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNodes.CreateMemberObjects;
const OPNAME = 'TNodes.CreateMemberObjects';
Begin
  try
    FNodeNumberStorage:= TInteger.Create;
    FStatusIndicator := TInteger.Create;
    FPlottingOption  := TInteger.Create;
    FReservoirPriority := TDouble.Create;
    FFullSupplyLevel := TDouble.Create;
    FDeadStorageLevel := TDouble.Create;
    FBottomOfReservoir := TDouble.Create;
    FFullSupplyAllocation := TDouble.Create;
    FComment := TString.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNodes.DestroyMemberObjects;
const OPNAME = 'TNodes.DestroyMemberObjects';
Begin
  try
    FNodeNumberStorage.Free;
    FStatusIndicator.Free;
    FPlottingOption.Free;
    FReservoirPriority.Free;
    FFullSupplyLevel.Free;
    FDeadStorageLevel.Free;
    FBottomOfReservoir.Free;
    FFullSupplyAllocation.Free;
    FComment.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNodes.Initialise: boolean;
const OPNAME = 'TNodes.Initialise';
Begin
  Result := False;
  try
    //Data and initialised.
    FNodeNumberStorage.FData := 0;
    FNodeNumberStorage.FInitalised := False;
    FNodeNumberStorage.FLength := 5;
    FNodeNumberStorage.FDecimal := 0;
    FNodeNumberStorage.FDefaultPadding := True;

    FStatusIndicator.FData := 0;
    FStatusIndicator.FInitalised := False;
    FStatusIndicator.FLength := 5;
    FStatusIndicator.FDecimal := 0;
    FStatusIndicator.FDefaultPadding := True;

    FPlottingOption.FData := 0;
    FPlottingOption.FInitalised := False;
    FPlottingOption.FLength := 5;
    FPlottingOption.FDecimal := 0;
    FPlottingOption.FDefaultPadding := True;

    FReservoirPriority.FData := 0;
    FReservoirPriority.FInitalised := False;
    FReservoirPriority.FLength := 10;
    FReservoirPriority.FDecimal := 0;
    FReservoirPriority.FDefaultPadding := True;
    FReservoirPriority.ShowDecimalPoint := True;

    FFullSupplyLevel.FData := 0;
    FFullSupplyLevel.FInitalised := False;
    FFullSupplyLevel.FLength := 10;
    FFullSupplyLevel.FDecimal := 2;
    FFullSupplyLevel.FDefaultPadding := True;
    FFullSupplyLevel.ShowDecimalPoint := True;

    FDeadStorageLevel.FData := 0;
    FDeadStorageLevel.FInitalised := False;
    FDeadStorageLevel.FLength := 10;
    FDeadStorageLevel.FDecimal := 2;
    FDeadStorageLevel.FDefaultPadding := True;
    FDeadStorageLevel.ShowDecimalPoint := True;

    FBottomOfReservoir.FData := 0;
    FBottomOfReservoir.FInitalised := False;
    FBottomOfReservoir.FLength := 10;
    FBottomOfReservoir.FDecimal := 2;
    FBottomOfReservoir.FDefaultPadding := True;
    FBottomOfReservoir.ShowDecimalPoint := True;

    FFullSupplyAllocation.FData := 0;
    FFullSupplyAllocation.FInitalised := False;
    FFullSupplyAllocation.FLength := 10;
    FFullSupplyAllocation.FDecimal := 2;
    FFullSupplyAllocation.FDefaultPadding := True;
    FFullSupplyAllocation.ShowDecimalPoint := True;

    FComment.FData := '';
    FComment.FInitalised := False;
    FComment.FDecimal := 0;
    FComment.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirLevels.CreateMemberObjects;
const OPNAME = 'TReservoirLevels.CreateMemberObjects';
var
  LCount: integer;
  LDouble : TDouble;
Begin
  try
    FReservoirIdentifier:= TInteger.Create;
    FComment := TString.Create;
    for LCount := MinReservoirMonths to MaxReservoirMonths do
    begin
      LDouble := TDouble.Create;
      FResLevel[LCount] := LDouble;
    end;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirLevels.DestroyMemberObjects;
const OPNAME = 'TReservoirLevels.DestroyMemberObjects';
var
  LCount : Integer;
Begin
  try
    FReservoirIdentifier.Free;
    FComment.Free;
    for LCount := MinReservoirMonths to MaxReservoirMonths do
        FResLevel[LCount].Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirLevels.Initialise: boolean;
const OPNAME = 'TReservoirLevels.Initialise';
var
  LCount : Integer;
Begin
  Result := False;
  try
    //Data and initialised.
    FReservoirIdentifier.FData := 0;
    FReservoirIdentifier.FInitalised := False;
    FReservoirIdentifier.FLength := 5;
    FReservoirIdentifier.FDecimal := 0;
    FReservoirIdentifier.FDefaultPadding := True;

    FComment.FData := '';
    FComment.FInitalised := False;
    FComment.FDecimal := 0;
    FComment.FDefaultPadding := True;

    for LCount := MinReservoirMonths to MaxReservoirMonths do
    begin
      FResLevel[LCount].FData := 0;
      FResLevel[LCount].FInitalised := False;
      FResLevel[LCount].FLength := 8;
      FResLevel[LCount].FDecimal := 2;
    FResLevel[LCount].FDefaultPadding := True;
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStorageZones.Reset;
const OPNAME = 'TStorageZones.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNodes.Reset;
const OPNAME = 'TNodes.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirLevels.Reset;
const OPNAME = 'TReservoirLevels.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TZoneDefinitionsObject.Reset;
const OPNAME = 'TZoneDefinitionsObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TZoneDefinitionsObject.GetReservoirPenalty(AReservoirNumber,APenaltyCol: integer;
         var AReservoirPenalty: TReservoirPenalty): boolean;
const OPNAME = 'TZoneDefinitionsObject.GetReservoirPenalty';
var
  LCount,
  LCurrentPos,
  LPenaltyCount: integer;
  LPenalyNode : TNodes;
  LZonePenaltyValue: TZonePenaltyValue;
  LZonePenaltyName : TZonePenaltyName;
  LAverageMonthlyLevel: TAverageMonthlyLevel;
begin
  Result := False;
  try
     AReservoirPenalty.Reset;
     for LCount := Low(LZonePenaltyValue) to High(LZonePenaltyValue) do
        LZonePenaltyValue[LCount] := 0.0;
     for LCount := Low(LZonePenaltyName) to High(LZonePenaltyName) do
        LZonePenaltyName[LCount] := '';
     for LCount := Low(LAverageMonthlyLevel) to High(LAverageMonthlyLevel) do
        LAverageMonthlyLevel[LCount] := 0.0;

     //Find the 3 known penalties( FSL,DSL,BOT)
     LPenalyNode := GetNode(AReservoirNumber);
     if Assigned(LPenalyNode) then
     begin
       if GetZonePenalty(APenaltyCol,LZonePenaltyValue,LZonePenaltyName) then
       begin
         if GetAverageMonthlyLevels(AReservoirNumber,LAverageMonthlyLevel) then
         begin
            AReservoirPenalty.FPenaltyCount := FStorageZoneCount.FData;

            AReservoirPenalty.FPenaltyValue[1] := LPenalyNode.FBottomOfReservoir.FData;
            AReservoirPenalty.FPenaltyDescr[1] := FLoatToStr(LZonePenaltyValue[1]);
            AReservoirPenalty.FPenaltyNames[1] := LZonePenaltyName[1];

            AReservoirPenalty.FPenaltyValue[2] := LPenalyNode.FDeadStorageLevel.FData;
            AReservoirPenalty.FPenaltyDescr[2] := FLoatToStr(LZonePenaltyValue[2]);
            AReservoirPenalty.FPenaltyNames[2] := LZonePenaltyName[2];

            LCurrentPos := 2;
            for LPenaltyCount := 1 to  (FStorageZoneCount.FData -3) do
            begin
              LCurrentPos := LCurrentPos + 1;
              AReservoirPenalty.FPenaltyValue[LCurrentPos] := LAverageMonthlyLevel[LPenaltyCount];
              AReservoirPenalty.FPenaltyDescr[LCurrentPos] := FLoatToStr(LZonePenaltyValue[LCurrentPos]);
              AReservoirPenalty.FPenaltyNames[LCurrentPos] := LZonePenaltyName[LCurrentPos];
            end;

            LCurrentPos := LCurrentPos + 1;
            AReservoirPenalty.FPenaltyValue[LCurrentPos] := LPenalyNode.FFullSupplyLevel.FData;
            AReservoirPenalty.FPenaltyDescr[LCurrentPos] := FLoatToStr(LZonePenaltyValue[LCurrentPos]);
            AReservoirPenalty.FPenaltyNames[LCurrentPos] := LZonePenaltyName[LCurrentPos];

         end;
       end;
     end;
     Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TZoneDefinitionsObject.GetNode(ANodeNumber: integer): TNodes;
const OPNAME = 'TZoneDefinitionsObject.GetNode';
var
  LCount: integer;
begin
  Result := nil;
  try
     for LCount := 0 to FNodes.Count - 1 do
     begin
       if(TNodes(FNodes[LCount]).FNodeNumberStorage.FData = ANodeNumber) then
       begin
         Result := TNodes(FNodes[LCount]);
         Break;
       end;
     end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TZoneDefinitionsObject.GetReservoirLevel(AReservoirNumber: integer): TReservoirLevels;
const OPNAME = 'TZoneDefinitionsObject.GetReservoirLevel';
var
  LCount: integer;
begin
  Result := nil;
  try
     for LCount := 0 to FReservoirLevels.Count - 1 do
     begin
       if(TReservoirLevels(FReservoirLevels[LCount]).FReservoirIdentifier.FData = AReservoirNumber) then
       begin
         Result := TReservoirLevels(FReservoirLevels[LCount]);
         Break;
       end;
     end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TZoneDefinitionsObject.GetZonePenalty(
         AZoneType: integer;
         var AZonePenaltyValue:TZonePenaltyValue;
         var AZonePenaltyName:TZonePenaltyName): boolean;
const OPNAME = 'TZoneDefinitionsObject.GetZonePenalty';
var
  LCount,
  LIndex: integer;
  LStorageZones: TStorageZones;
begin
  Result := False;
  try
    if(AZoneType <= FPenaltyStructureCount.FData) and (AZoneType <= MaxPenaltyZone) then
    begin
      LIndex := 0;
      for LCount := (FStorageZones.Count - 1) downto 0 do
      begin
        LIndex := LIndex + 1;
        if(LIndex <= MaxPenaltyZone) then
        begin
          LStorageZones := TStorageZones(FStorageZones[LCount]);
          AZonePenaltyValue[LIndex] := LStorageZones.FBalanceReference[AZoneType].FData;
          AZonePenaltyName[LIndex]  := LStorageZones.FResevoirZoneName.FData;
        end;
      end;
      Result := True;
     end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TZoneDefinitionsObject.GetAverageMonthlyLevels(AReservoirNumber: integer;
         var AAverageMonthlyLevel: TAverageMonthlyLevel): boolean;
const OPNAME = 'TZoneDefinitionsObject.GetAverageMonthlyLevels';
var
  LCount,
  LCount2,
  LLevelCount: integer;
  LAvarageLevel: Double;
  LReservoirLevels: TReservoirLevels;
begin
  Result := False;
  try
    LCount2 := 0;
    for LCount := (FReservoirLevels.Count - 1) downto 0 do
    begin
      LReservoirLevels := TReservoirLevels(FReservoirLevels[LCount]);
      if(LReservoirLevels.FReservoirIdentifier.FData = AReservoirNumber) then
      begin
        LAvarageLevel := 0.0;
        for LLevelCount := MinReservoirMonths to MaxReservoirMonths do
          LAvarageLevel := LAvarageLevel + LReservoirLevels.FResLevel[LLevelCount].FData;
        LAvarageLevel := LAvarageLevel / 12.0;
        LCount2 := LCount2 + 1;
        AAverageMonthlyLevel[LCount2] := LAvarageLevel;
      end;
    end;
    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TReservoirPenalty }

procedure TReservoirPenalty.Reset;
const OPNAME = 'TReservoirPenalty.Reset';
var
  LCount: integer;
begin
  try
    FPenaltyCount := 0;
    for LCount := MinPenaltyZone to MaxPenaltyZone do
    begin
      FPenaltyValue[LCount] := 0.0;
      FPenaltyDescr[LCount] := '';
      FPenaltyNames[LCount] := '';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
