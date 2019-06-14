//
//
//  UNIT      : Contains TRunParametersObject Class
//  AUTHOR    : Titi Ngubane(PDNA)
//  DATE      : 28/01/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UPlotFileObject;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UAbstractDataObject,
  UBasicObjects,
  UConstants;

type
  TPlotElementType = (pltelNone, pltelReservoir, pltelChannel, pltelMaster,pltelTotal);

  TPlotCountObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FReservoirCount : TInteger;
    FChannelCount   : TInteger;
    FLoadCaseCount   : TInteger;
    FSequenceCount  : TInteger;
    FMonthsCount    : TInteger;
    FComment        : TString;
    function Initialise: boolean;override;
    procedure Reset; override;
  end;

  TPlotElementObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FElementType     : TPlotElementType;
    FElementNumber   : TInteger;
    FElementName     : TString;
    function Initialise: boolean;override;
    procedure Reset; override;
  end;

  TPlotElementListObject = class(TAbstractDataObject)
  protected
    FElementsContainer: TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function Initialise: boolean;override;
    procedure Reset; override;
    function ElementsCount: integer;
    function AddElement(AElementType : TPlotElementType):TPlotElementObject;
    function ElementByIndex(AIndex: integer):TPlotElementObject;
  end;

  TPlotLineDataObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FElementType    : TPlotElementType;
    FElementNumber  : integer;
    FLoadCaseNumber : integer;
    FSequenceNumber : integer;
    FIdentifier     : integer;
    FMonthlyValues  : Array[MinMonths..MaxMonths] of TDouble;
    function Initialise: boolean;override;
    procedure Reset; override;
  end;

implementation


{ TRunParametersObject }
uses UErrorHandlingOperations;


{ TPlotCountObject }

procedure TPlotCountObject.CreateMemberObjects;
const OPNAME = 'TPlotCountObject.CreateMemberObjects';
begin
  inherited;
  try
    FReservoirCount := TInteger.Create;
    FSequenceCount := TInteger.Create;
    FLoadCaseCount   := TInteger.Create;
    FMonthsCount := TInteger.Create;
    FChannelCount := TInteger.Create;
    FComment := TString.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlotCountObject.DestroyMemberObjects;
const OPNAME = 'TPlotCountObject.DestroyMemberObjects';
begin
  inherited;
  try
    FReservoirCount.Free;
    FLoadCaseCount.Free;
    FSequenceCount.Free;
    FMonthsCount.Free;
    FChannelCount.Free;
    FComment.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlotCountObject.Initialise: boolean;
const OPNAME = 'TPlotCountObject.Initialise';
begin
  Result := inherited Initialise;
  try
    FReservoirCount.FData := 0;
    FReservoirCount.FInitalised := False;
    FReservoirCount.FLength := 5;
    FReservoirCount.FDecimal := 0;
    FReservoirCount.FDefaultPadding := True;

    FSequenceCount.FData := 0;
    FSequenceCount.FInitalised := False;
    FSequenceCount.FLength := 5;
    FSequenceCount.FDecimal := 0;
    FSequenceCount.FDefaultPadding := True;

    FLoadCaseCount.FData := 0;
    FLoadCaseCount.FInitalised := False;
    FLoadCaseCount.FLength := 5;
    FLoadCaseCount.FDecimal := 0;
    FLoadCaseCount.FDefaultPadding := True;

    FMonthsCount.FData := 0;
    FMonthsCount.FInitalised := False;
    FMonthsCount.FLength := 5;
    FMonthsCount.FDecimal := 0;
    FMonthsCount.FDefaultPadding := True;

    FChannelCount.FData := 0;
    FChannelCount.FInitalised := False;
    FChannelCount.FLength := 5;
    FChannelCount.FDecimal := 0;
    FChannelCount.FDefaultPadding := True;

    FComment.FData := '';
    FComment.FInitalised := False;
    FComment.FLength  := 8;
    FComment.FDecimal := 0;
    FComment.FDefaultPadding := False;

    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlotCountObject.Reset;
const OPNAME = 'TPlotCountObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TPlotElementObject }

procedure TPlotElementObject.CreateMemberObjects;
const OPNAME = 'TPlotElementObject.CreateMemberObjects';
begin
  inherited;
  try
    FElementNumber := TInteger.Create;
    FElementName   := TString.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlotElementObject.DestroyMemberObjects;
const OPNAME = 'TPlotElementObject.DestroyMemberObjects';
begin
  inherited;
  try
    FElementNumber.Free;
    FElementName.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlotElementObject.Initialise: boolean;
const OPNAME = 'TPlotElementObject.Initialise';
begin
  Result := inherited Initialise;
  try
    FElementType     := pltelNone;

    FElementNumber.FData := 0;
    FElementNumber.FInitalised := False;
    FElementNumber.FLength := 4;
    FElementNumber.FDecimal := 0;
    FElementNumber.FDefaultPadding := False;

    FElementName.FData := '';
    FElementName.FInitalised := False;
    FElementName.FLength  := 30;
    FElementName.FDecimal := 0;
    FElementName.FDefaultPadding := True;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlotElementObject.Reset;
const OPNAME = 'TPlotElementObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TPlotElementListObject }

procedure TPlotElementListObject.CreateMemberObjects;
const OPNAME = 'TPlotElementListObject.CreateMemberObjects';
begin
  inherited;
  try
    FElementsContainer := TObjectList.Create(True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlotElementListObject.DestroyMemberObjects;
const OPNAME = 'TPlotElementListObject.DestroyMemberObjects';
begin
  inherited;
  try
    FElementsContainer.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlotElementListObject.AddElement(AElementType: TPlotElementType): TPlotElementObject;
const OPNAME = 'TPlotElementListObject.AddElement';
begin
  Result := nil;
  try
    if(AElementType <> pltelNone) then
    begin
      Result := TPlotElementObject.Create;
      Result.Initialise;
      Result.FElementType := AElementType;
      FElementsContainer.Add(Result);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlotElementListObject.ElementByIndex(AIndex: integer): TPlotElementObject;
const OPNAME = 'TPlotElementListObject.ElementByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FElementsContainer.Count) then
      Result := TPlotElementObject(FElementsContainer.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlotElementListObject.ElementsCount: integer;
const OPNAME = 'TPlotElementListObject.ElementsCount';
begin
  Result := 0;
  try
    Result := FElementsContainer.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlotElementListObject.Initialise: boolean;
const OPNAME = 'TPlotElementListObject.Initialise';
begin
  Result := inherited Initialise;
  try
    FElementsContainer.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlotElementListObject.Reset;
const OPNAME = 'TPlotElementListObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TPlotLineDataObject }

procedure TPlotLineDataObject.CreateMemberObjects;
const OPNAME = 'TPlotLineDataObject.CreateMemberObjects';
var
  LCount: integer;
  LDouble : TDouble;
begin
  inherited;
  try
    for LCount := MinMonths to MaxMonths do
    begin
      LDouble := TDouble.Create;
      FMonthlyValues[LCount] := LDouble;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlotLineDataObject.DestroyMemberObjects;
const OPNAME = 'TPlotLineDataObject.DestroyMemberObjects';
var
  LCount: integer;
begin
  inherited;
  try
    for LCount := MinMonths to MaxMonths do
    begin
      FMonthlyValues[LCount].Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlotLineDataObject.Initialise: boolean;
const OPNAME = 'TPlotLineDataObject.Initialise';
var
  LCount: integer;
begin
  Result := inherited Initialise;
  try
    FElementType    := pltelNone;
    FElementNumber  := 0;
    FLoadCaseNumber := 0;
    FSequenceNumber := 0;
    FIdentifier     := 0;
    for LCount := MinMonths to MaxMonths do
    begin
      FMonthlyValues[LCount].FInitalised := False;
      FMonthlyValues[LCount].FLength := 11;
      FMonthlyValues[LCount].FDecimal := 3;
      FMonthlyValues[LCount].FDefaultPadding := True;
      FMonthlyValues[LCount].ShowDecimalPoint := True;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlotLineDataObject.Reset;
const OPNAME = 'TPlotLineDataObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
