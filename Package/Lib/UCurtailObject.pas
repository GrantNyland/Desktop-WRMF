//
//  Unit      : TCurtailObject
//  AUTHOR    : Phatedi Lethabo
//  DATE      :
//  COPYRIGHT : Copyright © 2016 DWS
//

unit UCurtailObject;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UAbstractDataObject,
  UBasicObjects,
  UConstants;

type

  TCurtailObject = class(TAbstractDataObject)
  public

    //Line 2 :Channel Number / Reservoir number / Decision Month / month after
    FChannelNumber :TInteger;
    FReservoirNumber :TInteger;
    FDecisionMonth :TInteger;
    FStartMonth :TInteger;
    FFileCurtailExtraLines : TStringList;

    //Line 3 : Elevation in Reserviour
    FReservoirElevation: Array[1..10] Of TDouble;

    //Line 4 : Multiplication (restriction)
    FMultiplicationRestriction: Array[1..10] Of TDouble;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;


    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TCurtailDataObject = class(TAbstractDataObject)
  public
    //File Curtail.Dat 25
    //Line 1 :Number of channels
    FNumberOfChannel :TInteger;
    FCurtailObject : TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function AddCurtailData : TCurtailObject;
    function GetCurtailObjectByIndex(AIndex : integer) : TCurtailObject;
    function NumberOfChannel : integer;
    procedure Reset;override;
    function Initialise: Boolean; override;

  end;

implementation

{ TCurtailObject }
uses UErrorHandlingOperations;

function TCurtailDataObject.AddCurtailData: TCurtailObject;
const OPNAME = 'TCurtailObject.CreateMemberObjects';
begin
  Result := nil;
  try
    Result := TCurtailObject.Create;
    FCurtailObject.Add(Result);

 except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCurtailObject.CreateMemberObjects;
const OPNAME = 'TCurtailObject.CreateMemberObjects';
var
  LCount: integer;
  LDouble : TDouble;
 begin
  try
    //File Curtail.Dat 25

    FChannelNumber :=TInteger.Create;
    FReservoirNumber :=TInteger.Create;
    FDecisionMonth :=TInteger.Create;
    FStartMonth :=TInteger.Create;
    FFileCurtailExtraLines := TStringList.Create;

    for LCount := 1 to 10 do
    begin
      LDouble :=TDouble.Create;
      FReservoirElevation[LCount]:= LDouble;
    end;

    for LCount := 1 to 10 do
    begin
      LDouble :=TDouble.Create;
      FMultiplicationRestriction[LCount]:= LDouble;
    end;

     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCurtailObject.DestroyMemberObjects;
const OPNAME = 'TCurtailObject.DestroyMemberObjects';
var
LCount   : Integer;
begin
  try

    //File CUR.dat

    FChannelNumber.free;
    FReservoirNumber.free;
    FDecisionMonth.free;
    FStartMonth.free;
    FFileCurtailExtraLines.Free;

    for LCount := 1 to 10 do
    begin
    FReservoirElevation[LCount].free;
    end;

    for LCount := 1 to 10 do
    begin

    FMultiplicationRestriction[LCount].free;
    end;

    except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCurtailObject.Initialise: boolean;
const OPNAME = 'TCurtailObject.Initialise';
var
  LCount : Integer;
Begin
  Result := False;
  try
    //-----------FData-----------------------------
    //File Curtail.Dat 25

    FChannelNumber.FData := 0;
    FChannelNumber.FInitalised := False;
    FChannelNumber.FLength := 8;
    FChannelNumber.FDecimal := 0;
    FChannelNumber.FDefaultPadding := True;

    FReservoirNumber .FData := 0;
    FReservoirNumber .FInitalised := False;
    FReservoirNumber .FLength := 6;
    FReservoirNumber .FDecimal := 0;
    FReservoirNumber .FDefaultPadding := True;

    FDecisionMonth.FData := 0;
    FDecisionMonth.FInitalised := False;
    FDecisionMonth.FLength := 2;
    FDecisionMonth.FDecimal := 0;
    FDecisionMonth.FDefaultPadding := True;

    FStartMonth.FData := 0;
    FStartMonth.FInitalised := False;
    FStartMonth.FLength := 2;
    FStartMonth.FDecimal := 0;
    FStartMonth.FDefaultPadding := True;

    FFileCurtailExtraLines.Clear;

    for LCount := 1 to 10 do
    begin
      FReservoirElevation[LCount].FData := 0;
      FReservoirElevation[LCount].FInitalised := False;
      FReservoirElevation[LCount].FLength := 6;
      FReservoirElevation[LCount].FDecimal := 2;
      FReservoirElevation[LCount].FDefaultPadding := True;
      FReservoirElevation[LCount].ShowDecimalPoint := True;
    end;

    for LCount := 1 to 10 do
    begin
      FMultiplicationRestriction[LCount].FData := 0;
      FMultiplicationRestriction[LCount].FInitalised := False;
      FMultiplicationRestriction[LCount].FLength := 4;
      FMultiplicationRestriction[LCount].FDecimal := 2;
      FMultiplicationRestriction[LCount].FDefaultPadding := True;
      FMultiplicationRestriction[LCount].ShowDecimalPoint := True;
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCurtailDataObject.CreateMemberObjects;
const OPNAME = 'TCurtailDataObject.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FCurtailObject := TObjectList.Create(True);
    FNumberOfChannel := TInteger.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCurtailDataObject.DestroyMemberObjects;
const OPNAME = 'TCurtailDataObject.DestroyMemberObjects';
begin
  try
    FreeAndNil(FCurtailObject);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCurtailDataObject.GetCurtailObjectByIndex(AIndex : integer): TCurtailObject;
const OPNAME = 'TCurtailDataObject.GetCurtailObjectByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex <= FCurtailObject.Count -1) then
      Result := TCurtailObject(FCurtailObject[AIndex]);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCurtailDataObject.Initialise: Boolean;
const OPNAME = 'TCurtailDataObject.CreateMemberObjects';
begin
  Result := False;
  try
    FNumberOfChannel.FData := 0;
    FNumberOfChannel.FInitalised := False;
    FNumberOfChannel.FLength  := 6;
    FNumberOfChannel.FDecimal := 0;
    FNumberOfChannel.FDefaultPadding := True;

    FCurtailObject.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCurtailDataObject.NumberOfChannel: integer;
const OPNAME = 'TCurtailDataObject.CreateMemberObjects';
begin
  Result := 0;
  try
    Result := FCurtailObject.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCurtailDataObject.Reset;
const OPNAME = 'TCurtailDataObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCurtailObject.Reset;
const OPNAME = 'TCurtailObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
