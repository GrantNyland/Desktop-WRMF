//
//
//  UNIT      : Contains TReservoirInitialLevelValuesObject Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 02/07/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UReservoirInitialLevelsObject;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UAbstractDataObject,
  UBasicObjects,
  UConstants;

type


  TReservoirInitialLevelValues = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FReservoirNodeNumber :TInteger;
    FReservoirInitialLevelValues : array[MinReservoirInitialLevel..MaxReservoirInitialLevel] of TDouble;
    FComment: TString;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TReservoirInitialLevelValuesObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FReservoirInitialLevelsLines : TObjectList;
    FF06ExtraLines: TStringList;
    procedure Reset;override;
    function Initialise: boolean;override;
    function GetReservoirInitialLevel(AReservoirNumber: integer):TReservoirInitialLevelValues;
  end;

implementation


uses UErrorHandlingOperations;

{TReservoirInitialLevelValues}

procedure TReservoirInitialLevelValues.CreateMemberObjects;
const OPNAME = 'TReservoirInitialLevelValues.CreateMemberObjects';
var
  LCount: integer;
  LDouble : TDouble;
Begin
  try
    FReservoirNodeNumber := TInteger.Create;
    FComment := TString.Create;

    for LCount := MinReservoirInitialLevel to MaxReservoirInitialLevel do
    begin
      LDouble := TDouble.Create;
      FReservoirInitialLevelValues[LCount] := LDouble;
    end;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirInitialLevelValues.DestroyMemberObjects;
const OPNAME = 'TReservoirInitialLevelValues.DestroyMemberObjects';
var
  LCount : Integer;
Begin
  try
    FReservoirNodeNumber.Free;
    FComment.Free;
    for LCount := MinReservoirInitialLevel to MaxReservoirInitialLevel do
      FReservoirInitialLevelValues[LCount].Free;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirInitialLevelValues.Reset;
const OPNAME = 'TReservoirInitialLevelValues.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirInitialLevelValues.Initialise: boolean;
const OPNAME = 'TReservoirInitialLevelValues.Initialise';
var
  LCount : Integer;
Begin
  Result := False;
  try
    //Data and initialised.
    FReservoirNodeNumber.FData := 0;
    FReservoirNodeNumber.FInitalised := False;
    FReservoirNodeNumber.FLength := 4;
    FReservoirNodeNumber.FDecimal := 0;
    FReservoirNodeNumber.FDefaultPadding := True;

    FComment.FData := '';
    FComment.FInitalised := False;
    FComment.FDecimal := 0;
    FComment.FDefaultPadding := True;

    for LCount := MinReservoirInitialLevel to MaxReservoirInitialLevel do
    begin
      FReservoirInitialLevelValues[LCount].FData := 0;
      FReservoirInitialLevelValues[LCount].FInitalised := False;
      FReservoirInitialLevelValues[LCount].FLength := 8;
      FReservoirInitialLevelValues[LCount].FDecimal := 2;
      FReservoirInitialLevelValues[LCount].FDefaultPadding := True;
      FReservoirInitialLevelValues[LCount].ShowDecimalPoint := True;
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TReservoirInitialLevelValuesObject}

procedure TReservoirInitialLevelValuesObject.CreateMemberObjects;
const OPNAME = 'TReservoirInitialLevelValuesObject.CreateMemberObjects';
begin
  try
    //File F06.dat
    FReservoirInitialLevelsLines := TObjectList.Create(True);
    FF06ExtraLines               := TStringList.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirInitialLevelValuesObject.DestroyMemberObjects;
const OPNAME = 'TReservoirInitialLevelValuesObject.DestroyMemberObjects';
begin
  try
    //File F06.dat
    FReservoirInitialLevelsLines.Free;
    FF06ExtraLines.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirInitialLevelValuesObject.GetReservoirInitialLevel(AReservoirNumber: integer): TReservoirInitialLevelValues;
const OPNAME = 'TReservoirInitialLevelValuesObject.GetReservoirInitialLevel';
var
  LCount: integer;
begin
  Result := nil;
  try
     for LCount := 0 to FReservoirInitialLevelsLines.Count - 1 do
     begin
       if(TReservoirInitialLevelValues(FReservoirInitialLevelsLines[LCount]).FReservoirNodeNumber.FData = AReservoirNumber) then
       begin
         Result := TReservoirInitialLevelValues(FReservoirInitialLevelsLines[LCount]);
         Break;
       end;
     end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirInitialLevelValuesObject.Initialise: boolean;
const OPNAME = 'TReservoirInitialLevelValuesObject.Initialise';
Begin
  Result := False;
  try
    FReservoirInitialLevelsLines.Clear;
    FF06ExtraLines.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirInitialLevelValuesObject.Reset;
const OPNAME = 'TReservoirInitialLevelValuesObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
