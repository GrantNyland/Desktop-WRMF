//
//
//  UNIT      : Contains TPowerDemandObject Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 05/08/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UPowerDemandObject;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UAbstractDataObject,
  UBasicObjects,
  UConstants;

type


  TPowerDemand = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FFeatureName         : TString;
    FChannelNumber       : TInteger;
    FDistributionPattern : TInteger;
    FValues              : array[MinPowerControl..MaxPowerControl] of TDouble;
    FStorageFraction     : TDouble;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TPowerDemandObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FPowerDemandsLines : TObjectList;
    FF13ExtraLines: TStringList;
    procedure Reset;override;
    function Initialise: boolean;override;
    function GetPowerDemand(AChannelNumber: integer):TPowerDemand;
  end;

implementation


uses UErrorHandlingOperations;

{TPowerDemand}

procedure TPowerDemand.CreateMemberObjects;
const OPNAME = 'TPowerDemand.CreateMemberObjects';
var
  LCount: integer;
  LDouble : TDouble;
Begin
  try
    FChannelNumber       := TInteger.Create;
    FFeatureName         := TString.Create;
    FDistributionPattern := TInteger.Create;
    FStorageFraction     := TDouble.Create;

    for LCount := MinPowerControl to MaxPowerControl do
    begin
      LDouble := TDouble.Create;
      FValues[LCount] := LDouble;
    end;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerDemand.DestroyMemberObjects;
const OPNAME = 'TPowerDemand.DestroyMemberObjects';
var
  LCount : Integer;
Begin
  try
    FChannelNumber.Free;
    FFeatureName.Free;
    FDistributionPattern.Free;
    FStorageFraction.Free;

    for LCount := MinPowerControl to MaxPowerControl do
      FValues[LCount].Free;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerDemand.Reset;
const OPNAME = 'TPowerDemand.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerDemand.Initialise: boolean;
const OPNAME = 'TPowerDemand.Initialise';
var
  LCount : Integer;
Begin
  Result := False;
  try
    //Data and initialised.
    FChannelNumber.FData := 0;
    FChannelNumber.FInitalised := False;
    FChannelNumber.FLength := 6;
    FChannelNumber.FDecimal := 0;
    FChannelNumber.FDefaultPadding := True;

    FFeatureName.FData := '';
    FFeatureName.FInitalised := False;
    FFeatureName.FLength := 36;
    FFeatureName.FDecimal := 0;
    FFeatureName.FDefaultPadding := True;

    FDistributionPattern.FData := 1;
    FDistributionPattern.FInitalised := False;
    FDistributionPattern.FLength := 6;
    FDistributionPattern.FDecimal := 0;
    FDistributionPattern.FDefaultPadding := True;

    FStorageFraction.FData := 0;
    FStorageFraction.FInitalised := False;
    FStorageFraction.FLength := 10;
    FStorageFraction.FDecimal := 3;
    FStorageFraction.FDefaultPadding := True;
    FStorageFraction.ShowDecimalPoint := True;

    for LCount := MinPowerControl to MaxPowerControl do
    begin
      FValues[LCount].FData := 0;
      FValues[LCount].FInitalised := False;
      FValues[LCount].FLength := 6;
      FValues[LCount].FDecimal := 3;
      FValues[LCount].FDefaultPadding := True;
      FValues[LCount].ShowDecimalPoint := True;
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TPowerDemandObject}

procedure TPowerDemandObject.CreateMemberObjects;
const OPNAME = 'TPowerDemandObject.CreateMemberObjects';
begin
  try
    //File F06.dat
    FPowerDemandsLines := TObjectList.Create(True);
    FF13ExtraLines               := TStringList.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerDemandObject.DestroyMemberObjects;
const OPNAME = 'TPowerDemandObject.DestroyMemberObjects';
begin
  try
    //File F06.dat
    FPowerDemandsLines.Free;
    FF13ExtraLines.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerDemandObject.GetPowerDemand(AChannelNumber: integer): TPowerDemand;
const OPNAME = 'TPowerDemandObject.GetPowerDemand';
var
  LCount: integer;
begin
  Result := nil;
  try
     for LCount := 0 to FPowerDemandsLines.Count - 1 do
     begin
       if(TPowerDemand(FPowerDemandsLines[LCount]).FChannelNumber.FData = AChannelNumber) then
       begin
         Result := TPowerDemand(FPowerDemandsLines[LCount]);
         Break;
       end;
     end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerDemandObject.Initialise: boolean;
const OPNAME = 'TPowerDemandObject.Initialise';
Begin
  Result := False;
  try
    FPowerDemandsLines.Clear;
    FF13ExtraLines.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerDemandObject.Reset;
const OPNAME = 'TPowerDemandObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
