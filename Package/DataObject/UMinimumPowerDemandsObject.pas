//
//
//  UNIT      : Contains TPowerChannelObject Class
//  AUTHOR    : Titi Ngubane(Arivia)
//  DATE      : 02/03/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UMinimumPowerDemandsObject;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UConstants,
  UBasicObjects,
  UAbstractDataObject;

type
  TMinimumPowerData = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    //File F08.dat
    //Line 1
    FPowerChannelName : TString;
    FPowerChannelNumber : TInteger;

    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TMinimumPowerDetails = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    //Line 1a
    FMinMonthlyEnergyGeneration : Array[MinPowerDemand..MaxPowerDemand] Of TDouble;

    //Line 1b
    FMinMonthlyPowerRelease : Array[MinPowerDemand..MaxPowerDemand] Of TDouble;

    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TPowerGenerationObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FMinimumPowerData : TMinimumPowerData;
    FMinimumPowerDetails : TMinimumPowerDetails;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TMinimumPowerDemandsObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FMinimumPowerDetailsList : TObjectList;
    FF08ExtraLines: TStringList;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  implementation


uses UErrorHandlingOperations;

procedure TMinimumPowerData.CreateMemberObjects;
const OPNAME = 'TMinimumPowerData.CreateMemberObjects';
begin
  try
    //File F08.dat
    FPowerChannelName   := TString.Create;
    FPowerChannelNumber := TInteger.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinimumPowerData.DestroyMemberObjects;
const OPNAME = 'TMinimumPowerData.DestroyMemberObjects';
begin
  try
    //File F08.dat
    FPowerChannelName.Free;
    FPowerChannelNumber.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinimumPowerData.Initialise: boolean;
const OPNAME = 'TMinimumPowerData.Initialise';
begin
  Result := False;
  try
    FPowerChannelName.FData := '';
    FPowerChannelName.FInitalised := False;
    FPowerChannelName.FLength := 36;
    FPowerChannelName.FDecimal := 0;
    FPowerChannelName.FDefaultPadding := True;

    FPowerChannelNumber.FData := 0;
    FPowerChannelNumber.FInitalised := False;
    FPowerChannelNumber.FLength := 6;
    FPowerChannelNumber.FDecimal := 0;
    FPowerChannelNumber.FDefaultPadding := True;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinimumPowerData.Reset;
const OPNAME = 'TMinimumPowerData.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{TMinimumPowerDetails}

procedure TMinimumPowerDetails.CreateMemberObjects;
const OPNAME = 'TMinimumPowerDetails.CreateMemberObjects';
var
  LCount: integer;
  LDouble : TDouble;
begin
  try
    //File F08.dat
    for LCount := MinPowerDemand to MaxPowerDemand do
    begin
      LDouble := TDouble.Create;
      FMinMonthlyEnergyGeneration[LCount] := LDouble;
      LDouble := TDouble.Create;
      FMinMonthlyPowerRelease[LCount] := LDouble;
    end;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinimumPowerDetails.DestroyMemberObjects;
const OPNAME = 'TMinimumPowerDetails.DestroyMemberObjects';
var
  LCount: integer;
begin
  try
    //File F08.dat
    for LCount := MinPowerDemand to MaxPowerDemand do
    begin
      FMinMonthlyEnergyGeneration[LCount].Free;
      FMinMonthlyPowerRelease[LCount].Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinimumPowerDetails.Initialise: boolean;
const OPNAME = 'TMinimumPowerDetails.Initialise';
var
  LCount: integer;
begin
  Result := False;
  try
    for LCount := MinPowerDemand to MaxPowerDemand do
    begin
      FMinMonthlyEnergyGeneration[LCount].FData := 0;
      FMinMonthlyEnergyGeneration[LCount].FInitalised := False;
      FMinMonthlyEnergyGeneration[LCount].FLength := 6;
      FMinMonthlyEnergyGeneration[LCount].FDecimal := 2;
      FMinMonthlyEnergyGeneration[LCount].FDefaultPadding := True;
      FMinMonthlyEnergyGeneration[LCount].ShowDecimalPoint := True;

      FMinMonthlyPowerRelease[LCount].FData := 0;
      FMinMonthlyPowerRelease[LCount].FInitalised := False;
      FMinMonthlyPowerRelease[LCount].FLength := 6;
      FMinMonthlyPowerRelease[LCount].FDecimal := 2;
      FMinMonthlyPowerRelease[LCount].FDefaultPadding := True;
      FMinMonthlyPowerRelease[LCount].ShowDecimalPoint := False;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinimumPowerDetails.Reset;
const OPNAME = 'TMinimumPowerDetails.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TPowerGenerationObject}

procedure TPowerGenerationObject.CreateMemberObjects;
const OPNAME = 'TPowerGenerationObject.CreateMemberObjects';
begin
  try
    //File F08.dat
    FMinimumPowerData := TMinimumPowerData.Create;
    FMinimumPowerDetails := TMinimumPowerDetails.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerGenerationObject.DestroyMemberObjects;
const OPNAME = 'TPowerGenerationObject.DestroyMemberObjects';
begin
  try
    //File F07.dat
    FMinimumPowerData.Free;
    FMinimumPowerDetails.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerGenerationObject.Initialise: boolean;
const OPNAME = 'TPowerGenerationObject.Initialise';
Begin
  Result := False;
  try
    FMinimumPowerData.Initialise;
    FMinimumPowerDetails.Initialise;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerGenerationObject.Reset;
const OPNAME = 'TPowerGenerationObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{TMinimumPowerDemandsObject}

procedure TMinimumPowerDemandsObject.CreateMemberObjects;
const OPNAME = 'TMinimumPowerDemandsObject.CreateMemberObjects';
begin
  try
    FMinimumPowerDetailsList    := TObjectList.Create(True);
    FF08ExtraLines         := TStringList.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinimumPowerDemandsObject.DestroyMemberObjects;
const OPNAME = 'TMinimumPowerDemandsObject.DestroyMemberObjects';
begin
  try
    FMinimumPowerDetailsList.Free;
    FF08ExtraLines.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinimumPowerDemandsObject.Initialise: boolean;
const OPNAME = 'TMinimumPowerDemandsObject.Initialise';
Begin
  Result := False;
  try
    FMinimumPowerDetailsList.Clear;
    FF08ExtraLines.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinimumPowerDemandsObject.Reset;
const OPNAME = 'TMinimumPowerDemandsObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
