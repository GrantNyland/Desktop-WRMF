//
//
//  UNIT      : Contains TIrrigationAreaObject Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 05/08/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UIrrigationAreasObject;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UAbstractDataObject,
  UBasicObjects,
  UConstants;

type


  TIrrigationArea = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FIrrigationAreaName: TString;
    FIrrigationNodeNumber :TInteger;
    FDiversionFlowValues : array[MinDiversionFlow..MaxDiversionFlow] of TDouble;
    FReturnFlowValues : array[MinReturnFlow..MaxReturnFlow] of TDouble;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TIrrigationAreaObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FIrrigationAreasLines : TObjectList;
    FF09ExtraLines: TStringList;
    procedure Reset;override;
    function Initialise: boolean;override;
    function GetIrrigationArea(AIrrigationNodeNumber: integer):TIrrigationArea;
  end;

implementation


uses UErrorHandlingOperations;

{TIrrigationArea}

procedure TIrrigationArea.CreateMemberObjects;
const OPNAME = 'TIrrigationArea.CreateMemberObjects';
var
  LCount: integer;
  LDouble : TDouble;
Begin
  try
    FIrrigationNodeNumber := TInteger.Create;
    FIrrigationAreaName  := TString.Create;

    for LCount := MinDiversionFlow to MaxDiversionFlow do
    begin
      LDouble := TDouble.Create;
      FDiversionFlowValues[LCount] := LDouble;
    end;
    for LCount := MinReturnFlow to MaxReturnFlow do
    begin
      LDouble := TDouble.Create;
      FReturnFlowValues[LCount] := LDouble;
    end;

    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationArea.DestroyMemberObjects;
const OPNAME = 'TIrrigationArea.DestroyMemberObjects';
var
  LCount : Integer;
Begin
  try
    FIrrigationNodeNumber.Free;
    FIrrigationAreaName.Free;
    for LCount := MinDiversionFlow to MaxDiversionFlow do
      FDiversionFlowValues[LCount].Free;
    for LCount := MinReturnFlow to MaxReturnFlow do
      FReturnFlowValues[LCount].Free;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationArea.Reset;
const OPNAME = 'TIrrigationArea.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationArea.Initialise: boolean;
const OPNAME = 'TIrrigationArea.Initialise';
var
  LCount : Integer;
Begin
  Result := False;
  try
    //Data and initialised.
    FIrrigationNodeNumber.FData := 0;
    FIrrigationNodeNumber.FInitalised := False;
    FIrrigationNodeNumber.FLength := 6;
    FIrrigationNodeNumber.FDecimal := 0;
    FIrrigationNodeNumber.FDefaultPadding := True;

    FIrrigationAreaName.FData := '';
    FIrrigationAreaName.FInitalised := False;
    FIrrigationAreaName.FLength := 36;
    FIrrigationAreaName.FDecimal := 0;
    FIrrigationAreaName.FDefaultPadding := True;

    for LCount := MinDiversionFlow to MaxDiversionFlow do
    begin
      FDiversionFlowValues[LCount].FData := 0;
      FDiversionFlowValues[LCount].FInitalised := False;
      FDiversionFlowValues[LCount].FLength := 6;
      FDiversionFlowValues[LCount].FDecimal := 2;
      FDiversionFlowValues[LCount].FDefaultPadding := True;
      FDiversionFlowValues[LCount].ShowDecimalPoint := True;
    end;

    for LCount := MinReturnFlow to MaxReturnFlow do
    begin
      FReturnFlowValues[LCount].FData := 0;
      FReturnFlowValues[LCount].FInitalised := False;
      FReturnFlowValues[LCount].FLength := 6;
      FReturnFlowValues[LCount].FDecimal := 2;
      FReturnFlowValues[LCount].FDefaultPadding := True;
      FReturnFlowValues[LCount].ShowDecimalPoint := True;
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TIrrigationAreaObject}

procedure TIrrigationAreaObject.CreateMemberObjects;
const OPNAME = 'TIrrigationAreaObject.CreateMemberObjects';
begin
  try
    //File F06.dat
    FIrrigationAreasLines := TObjectList.Create(True);
    FF09ExtraLines               := TStringList.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaObject.DestroyMemberObjects;
const OPNAME = 'TIrrigationAreaObject.DestroyMemberObjects';
begin
  try
    //File F06.dat
    FIrrigationAreasLines.Free;
    FF09ExtraLines.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationAreaObject.GetIrrigationArea(AIrrigationNodeNumber: integer): TIrrigationArea;
const OPNAME = 'TIrrigationAreaObject.GetIrrigationArea';
var
  LCount: integer;
begin
  Result := nil;
  try
     for LCount := 0 to FIrrigationAreasLines.Count - 1 do
     begin
       if(TIrrigationArea(FIrrigationAreasLines[LCount]).FIrrigationNodeNumber.FData = AIrrigationNodeNumber) then
       begin
         Result := TIrrigationArea(FIrrigationAreasLines[LCount]);
         Break;
       end;
     end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationAreaObject.Initialise: boolean;
const OPNAME = 'TIrrigationAreaObject.Initialise';
Begin
  Result := False;
  try
    FIrrigationAreasLines.Clear;
    FF09ExtraLines.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaObject.Reset;
const OPNAME = 'TIrrigationAreaObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
