//
//
//  UNIT      : Contains TPowerChannelObject Class
//  AUTHOR    : Titi Ngubane(Arivia)
//  DATE      : 02/03/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UPowerChannelObject;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UConstants,
  UBasicObjects,
  UAbstractDataObject;

type
  TPowerPlantHeadData = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    //File F07.dat
    //Line 1
    FPowerPlantName : TString;
    FPowerPlantNumber : TInteger;
    FMaxCapGenerator : TDouble;
    FMaxCapTurbine : TDouble;
    FEfficiency : TDouble;
    FPowerPlantStatus : TInteger;

    //Line 1a
    FHeadLoss   :TDouble;
    FDesignHead :TDouble;
    FMaxNetHead : TDouble;
    FMinNetHead : TDouble;
    FHydropowerMinLevel : TDouble;

    //Line 2 & 5
    FPointsCount      : TInteger;
    FTailWaterPointsCount :TInteger;
    FTailWaterTypeCode :TInteger;

    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TPowerPlantHeadDetails = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    //Line 3
    FEfficiencyFactor : Array[MinNumberOfPoints..MaxNumberOfPoints] Of TDouble;

    //Line 4
    FNetHeadFactor : Array[MinNumberOfPoints..MaxNumberOfPoints] Of TDouble;

    //Line 6
    FDischarge : Array[MinNumberOfPoints..MaxNumberOfPoints] Of TDouble;

    //Line 7
    FTailWaterElevation : Array[MinNumberOfPoints..MaxNumberOfPoints] Of TDouble;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TPowerPlantObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FPowerPlantHeadData    : TPowerPlantHeadData;
    FPowerPlantHeadDetails : TPowerPlantHeadDetails;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TPowerChannelsObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FPowerPlantItemList : TObjectList;
    FF07ExtraLines: TStringList;
    procedure Reset;override;
    function Initialise: boolean;override;
    function FindPowerPlant(AChannelNumber: Integer): TPowerPlantObject;
  end;

implementation


uses UErrorHandlingOperations;

procedure TPowerPlantHeadData.CreateMemberObjects;
const OPNAME = 'TPowerPlantHeadData.CreateMemberObjects';
begin
  try
    //File F07.dat
    FPointsCount          := TInteger.Create;
    FPowerPlantName       := TString.Create;
    FPowerPlantNumber     := TInteger.Create;
    FMaxCapGenerator      := TDouble.Create;
    FMaxCapTurbine        := TDouble.Create;
    FEfficiency           := TDouble.Create;
    FPowerPlantStatus     := TInteger.Create;
    FHeadLoss             := TDouble.Create;
    FDesignHead           := TDouble.Create;
    FMaxNetHead           := TDouble.Create;
    FMinNetHead           := TDouble.Create;
    FHydropowerMinLevel   := TDouble.Create;
    FTailWaterPointsCount :=TInteger.Create;
    FTailWaterTypeCode    :=TInteger.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantHeadData.DestroyMemberObjects;
const OPNAME = 'TPowerPlantHeadData.DestroyMemberObjects';
begin
  try
    //File F07.dat
    FPointsCount.Free;
    FPowerPlantName.Free;
    FPowerPlantNumber.Free;
    FMaxCapGenerator.Free;
    FMaxCapTurbine.Free;
    FEfficiency.Free;
    FPowerPlantStatus.Free;
    FHeadLoss.Free;
    FDesignHead.Free;
    FMaxNetHead.Free;
    FMinNetHead.Free;
    FHydropowerMinLevel.Free;
    FTailWaterPointsCount.Free;
    FTailWaterTypeCode.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantHeadData.Initialise: boolean;
const OPNAME = 'TPowerPlantHeadData.Initialise';
Begin
  Result := False;
  try
    FPointsCount.FData := 0;
    FPointsCount.FInitalised := False;
    FPointsCount.FLength := 6;
    FPointsCount.FDecimal := 0;
    FPointsCount.FDefaultPadding := True;

    FPowerPlantName.FData := '';
    FPowerPlantName.FInitalised := False;
    FPowerPlantName.FLength := 36;
    FPowerPlantName.FDecimal := 0;
    FPowerPlantName.FDefaultPadding := True;

    FPowerPlantNumber.FData := 0;
    FPowerPlantNumber.FInitalised := False;
    FPowerPlantNumber.FLength := 6;
    FPowerPlantNumber.FDecimal := 0;
    FPowerPlantNumber.FDefaultPadding := True;

    FMaxCapGenerator.FData := 0;
    FMaxCapGenerator.FInitalised := False;
    FMaxCapGenerator.FLength := 6;
    FMaxCapGenerator.FDecimal := 2;
    FMaxCapGenerator.FDefaultPadding := True;
    FMaxCapGenerator.ShowDecimalPoint := True;

    FMaxCapTurbine.FData := 0;
    FMaxCapTurbine.FInitalised := False;
    FMaxCapTurbine.FLength := 6;
    FMaxCapTurbine.FDecimal := 2;
    FMaxCapTurbine.FDefaultPadding := True;
    FMaxCapTurbine.ShowDecimalPoint := True;

    FEfficiency.FData := 0;
    FEfficiency.FInitalised := False;
    FEfficiency.FLength := 6;
    FEfficiency.FDecimal := 3;
    FEfficiency.FDefaultPadding := True;
    FEfficiency.ShowDecimalPoint := True;

    FPowerPlantStatus.FData := 0;
    FPowerPlantStatus.FInitalised := False;
    FPowerPlantStatus.FLength := 6;
    FPowerPlantStatus.FDecimal := 0;
    FPowerPlantStatus.FDefaultPadding := True;

    FHeadLoss.FData := 0;
    FHeadLoss.FInitalised := False;
    FHeadLoss.FLength := 6;
    FHeadLoss.FDecimal := 3;
    FHeadLoss.FDefaultPadding := True;
    FHeadLoss.ShowDecimalPoint := True;

    FDesignHead.FData := 0;
    FDesignHead.FInitalised := False;
    FDesignHead.FLength := 6;
    FDesignHead.FDecimal := 2;
    FDesignHead.FDefaultPadding := True;
    FDesignHead.ShowDecimalPoint := True;

    FMaxNetHead.FData := 0;
    FMaxNetHead.FInitalised := False;
    FMaxNetHead.FLength := 6;
    FMaxNetHead.FDecimal := 2;
    FMaxNetHead.FDefaultPadding := True;
    FMaxNetHead.ShowDecimalPoint := True;

    FMinNetHead.FData := 0;
    FMinNetHead.FInitalised := False;
    FMinNetHead.FLength := 6;
    FMinNetHead.FDecimal := 2;
    FMinNetHead.FDefaultPadding := True;
    FMinNetHead.ShowDecimalPoint := True;

    FHydropowerMinLevel.FData := 0;
    FHydropowerMinLevel.FInitalised := False;
    FHydropowerMinLevel.FLength := 6;
    FHydropowerMinLevel.FDecimal := 2;
    FHydropowerMinLevel.FDefaultPadding := True;
    FHydropowerMinLevel.ShowDecimalPoint := True;

    FTailWaterPointsCount.FData := 0;
    FTailWaterPointsCount.FInitalised := False;
    FTailWaterPointsCount.FLength := 6;
    FTailWaterPointsCount.FDecimal := 0;
    FTailWaterPointsCount.FDefaultPadding := True;

    FTailWaterTypeCode.FData := 0;
    FTailWaterTypeCode.FInitalised := False;
    FTailWaterTypeCode.FLength := 6;
    FTailWaterTypeCode.FDecimal := 0;
    FTailWaterTypeCode.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantHeadData.Reset;
const OPNAME = 'TPowerPlantHeadData.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantHeadDetails.CreateMemberObjects;
const OPNAME = 'TPowerPlantHeadDetails.CreateMemberObjects';
var
  LCount: integer;
  LDouble : TDouble;
begin
  try
    //File F07.dat
    for LCount := MinNumberOfPoints to MaxNumberOfPoints do
    begin
      LDouble := TDouble.Create;
      FEfficiencyFactor[LCount] := LDouble;
      LDouble := TDouble.Create;
      FNetHeadFactor[LCount] := LDouble;
      LDouble := TDouble.Create;
      FDischarge[LCount] := LDouble;
      LDouble := TDouble.Create;
      FTailWaterElevation[LCount] := LDouble;
    end;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantHeadDetails.DestroyMemberObjects;
const OPNAME = 'TPowerPlantHeadDetails.DestroyMemberObjects';
var
  LCount: integer;
begin
  try
    //File F07.dat
    for LCount := MinNumberOfPoints to MaxNumberOfPoints do
    begin
      FEfficiencyFactor[LCount].Free;
      FNetHeadFactor[LCount].Free;
      FDischarge[LCount].Free;
      FTailWaterElevation[LCount].Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantHeadDetails.Initialise: boolean;
const OPNAME = 'TPowerPlantHeadDetails.Initialise';
var
  LCount: integer;
begin
  Result := False;
  try
    for LCount := MinNumberOfPoints to MaxNumberOfPoints do
    begin
      FEfficiencyFactor[LCount].FData := 0;
      FEfficiencyFactor[LCount].FInitalised := False;
      FEfficiencyFactor[LCount].FLength := 6;
      FEfficiencyFactor[LCount].FDecimal := 3;
      FEfficiencyFactor[LCount].FDefaultPadding := True;
      FEfficiencyFactor[LCount].ShowDecimalPoint := True;

      FNetHeadFactor[LCount].FData := 0;
      FNetHeadFactor[LCount].FInitalised := False;
      FNetHeadFactor[LCount].FLength := 6;
      FNetHeadFactor[LCount].FDecimal := 3;
      FNetHeadFactor[LCount].FDefaultPadding := True;
      FNetHeadFactor[LCount].ShowDecimalPoint := True;

      FDischarge[LCount].FData := 0;
      FDischarge[LCount].FInitalised := False;
      FDischarge[LCount].FLength := 6;
      FDischarge[LCount].FDecimal := 1;
      FDischarge[LCount].FDefaultPadding := True;
      FDischarge[LCount].ShowDecimalPoint := True;

      FTailWaterElevation[LCount].FData := 0;
      FTailWaterElevation[LCount].FInitalised := False;
      FTailWaterElevation[LCount].FLength := 6;
      FTailWaterElevation[LCount].FDecimal := 1;
      FTailWaterElevation[LCount].FDefaultPadding := True;
      FTailWaterElevation[LCount].ShowDecimalPoint := True;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantHeadDetails.Reset;
const OPNAME = 'TPowerPlantHeadDetails.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TPowerPlantObject}

procedure TPowerPlantObject.CreateMemberObjects;
const OPNAME = 'TPowerPlantObject.CreateMemberObjects';
begin
  try
    //File F07.dat
    FPowerPlantHeadDetails := TPowerPlantHeadDetails.Create;
    FPowerPlantHeadData := TPowerPlantHeadData.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantObject.DestroyMemberObjects;
const OPNAME = 'TPowerPlantObject.DestroyMemberObjects';
begin
  try
    //File F07.dat
    FPowerPlantHeadDetails.Free;
    FPowerPlantHeadData.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantObject.Initialise: boolean;
const OPNAME = 'TPowerPlantObject.Initialise';
Begin
  Result := False;
  try
    FPowerPlantHeadDetails.Initialise;
    FPowerPlantHeadData.Initialise;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantObject.Reset;
const OPNAME = 'TPowerPlantObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TPowerChannelsObject}

procedure TPowerChannelsObject.CreateMemberObjects;
const OPNAME = 'TPowerChannelsObject.CreateMemberObjects';
begin
  try
    //File F07.dat
    FPowerPlantItemList    := TObjectList.Create(True);
    FF07ExtraLines         := TStringList.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerChannelsObject.DestroyMemberObjects;
const OPNAME = 'TPowerChannelsObject.DestroyMemberObjects';
begin
  try
    //File F07.dat
    FPowerPlantItemList.Free;
    FF07ExtraLines.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerChannelsObject.Initialise: boolean;
const OPNAME = 'TPowerChannelsObject.Initialise';
Begin
  Result := False;
  try
    FPowerPlantItemList.Clear;
    FF07ExtraLines.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerChannelsObject.FindPowerPlant(AChannelNumber: Integer): TPowerPlantObject;
const OPNAME = 'TPowerChannelsObject.FindPowerPlant';
var
  LCount: integer;
begin
  Result := nil;
  try
    for LCount := 0 to FPowerPlantItemList.Count - 1 do
    begin
      if(TPowerPlantObject(FPowerPlantItemList[LCount]).FPowerPlantHeadData.FPowerPlantNumber.FData = AChannelNumber) then
      begin
        Result := TPowerPlantObject(FPowerPlantItemList[LCount]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerChannelsObject.Reset;
const OPNAME = 'TPowerChannelsObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
