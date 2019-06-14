//
//
//  UNIT      : Contains TDisbenefitFileDataObject Class
//  AUTHOR    : Dziedzi Ramulondi(Aravia)
//  DATE      : 15/06/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UDisbenefitFileDataObjects;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UBasicObjects,
  UAbstractDataObject,
  UFileNames,
  UDataFileObjects,
  UAbstractObject;

type
  TTDSConcentrationFactors = array[1..4] of TDouble;
  TDisbenefitFileObject = class(TAbstractDataObject)
  protected
    // Line 2
    FChannelNumber           : TInteger;
    FYearChannelActive       : TInteger;
    FMonthChannelActive      : TInteger;
    FYearChannelAbsolete     : TInteger;
    FMonthChannelAbsolete    : TInteger;
    FFunctionX               : TDouble;
    FFunctionY               : TDouble;
    FFunctionNonSupply       : TDouble;
    FFunctionCost            : TDouble;

    // Line 3
    FEscaltionRate           : TString;

    // Line 4
    FWaterQualityConstraint  : TDouble;
    FTDSConcentration        : TTDSConcentrationFactors;

    // Line 5
    FWQDisbenefit            : TString;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset;override;
    function Initialise: boolean;override;
    property ChannelNumber         : TInteger  read FChannelNumber;
    property YearChannelActive     : TInteger  read FYearChannelActive;
    property MonthChannelActive    : TInteger  read FMonthChannelActive ;
    property YearChannelAbsolete   : TInteger  read FYearChannelAbsolete;
    property MonthChannelAbsolete  : TInteger  read FMonthChannelAbsolete;

    property FunctionX               : TDouble   read FFunctionX;
    property FunctionY               : TDouble   read FFunctionY;
    property FunctionNonSupply       : TDouble   read FFunctionNonSupply;
    property FunctionCost            : TDouble   read FFunctionCost;
    property EscaltionRate           : TString   read FEscaltionRate;
    property WaterQualityConstraint  : TDouble   read FWaterQualityConstraint;
    property TDSConcentration        : TTDSConcentrationFactors  read FTDSConcentration;
    property WQDisbenefit            : TString  read FWQDisbenefit;
  end;

  TDisbenefitFileDataObject      = class(TAbstractDataObject)
  protected
    //Line 1
    FDataYears : TInteger;

    // Line 2,3,4
    FDisbenefitList   : TObjectList;

    //Line 6... : Extra useless lines
    FFMExtraLines: TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetDisbenefitByIndex(AIndex: integer):TDisbenefitFileObject;
  public
    function Initialise: boolean;override;
    procedure Reset;override;
    function AddDisbenefit:TDisbenefitFileObject;
    // Line 1
    property DataYears         : TInteger  read FDataYears;
    function DisbenefitCount: integer;
    property DisbenefitByIndex[AIndex: integer] : TDisbenefitFileObject read GetDisbenefitByIndex;
    property FMExtraLines   : TStringList  read FFMExtraLines ;
  end;
implementation

uses
  UErrorHandlingOperations;

{ TDisbenefitFileObject }

procedure TDisbenefitFileObject.CreateMemberObjects;
const OPNAME = 'TDisbenefitFileObject.CreateMemberObjects';
var
  LIndex : integer;
  LDouble: TDouble;
begin
  inherited CreateMemberObjects;
  try
    // Line 2
    FChannelNumber           := TInteger.Create;
    FYearChannelActive       := TInteger.Create;
    FMonthChannelActive      := TInteger.Create;
    FYearChannelAbsolete     := TInteger.Create;
    FMonthChannelAbsolete    := TInteger.Create;
    FFunctionX               := TDouble.Create;
    FFunctionY               := TDouble.Create;
    FFunctionNonSupply       := TDouble.Create;
    FFunctionCost            := TDouble.Create;

    // Line 3
    FEscaltionRate           := TString.Create;

    // Line 4
    FWaterQualityConstraint  := TDouble.Create;
    for LIndex := Low(FTDSConcentration) to High(FTDSConcentration) do
    begin
      LDouble := TDouble.Create;
      FTDSConcentration[LIndex] := LDouble;
    end;

    // Line 5
    FWQDisbenefit            := TString.Create;

    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFileObject.DestroyMemberObjects;
const OPNAME = 'TDisbenefitFileObject.DestroyMemberObjects';
var
  LIndex : integer;
begin
  inherited DestroyMemberObjects;
  try
    // Line 2
    FChannelNumber.Free;
    FYearChannelActive.Free;
    FMonthChannelActive.Free;
    FYearChannelAbsolete.Free;
    FMonthChannelAbsolete.Free;
    FFunctionX.Free;
    FFunctionY.Free;
    FFunctionNonSupply.Free;
    FFunctionCost.Free;

    // Line 3
    FEscaltionRate.Free;

    // Line 4
    FWaterQualityConstraint.Free;
    for LIndex := Low(FTDSConcentration) to High(FTDSConcentration) do
      FTDSConcentration[LIndex].Free;

    // Line 5
    FWQDisbenefit.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDisbenefitFileObject.Initialise: boolean;
const OPNAME = 'TDisbenefitFileObject.Initialise';
var
  LIndex : integer;
begin
  Result := False;
  try
    FChannelNumber.FData := 0;
    FChannelNumber.FInitalised := False;
    FChannelNumber.FLength := 6;
    FChannelNumber.FDecimal := 0;
    FChannelNumber.FDefaultPadding := True;

    FYearChannelActive.FData := 0;
    FYearChannelActive.FInitalised := False;
    FYearChannelActive.FLength := 6;
    FYearChannelActive.FDecimal := 0;
    FYearChannelActive.FDefaultPadding := True;

    FMonthChannelActive.FData := 0;
    FMonthChannelActive.FInitalised := False;
    FMonthChannelActive.FLength := 6;
    FMonthChannelActive.FDecimal := 0;
    FMonthChannelActive.FDefaultPadding := True;

    FYearChannelAbsolete.FData := 0;
    FYearChannelAbsolete.FInitalised := False;
    FYearChannelAbsolete.FLength := 6;
    FYearChannelAbsolete.FDecimal := 0;
    FYearChannelAbsolete.FDefaultPadding := True;

    FMonthChannelAbsolete.FData := 0;
    FMonthChannelAbsolete.FInitalised := False;
    FMonthChannelAbsolete.FLength := 6;
    FMonthChannelAbsolete.FDecimal := 0;
    FMonthChannelAbsolete.FDefaultPadding := True;

    FFunctionX.FData := 0;
    FFunctionX.FInitalised := False;
    FFunctionX.FLength := 8;
    FFunctionX.FDecimal := 4;
    FFunctionX.FDefaultPadding := True;
    FFunctionX.ShowDecimalPoint := True;

    FFunctionY.FData := 0;
    FFunctionY.FInitalised := False;
    FFunctionY.FLength := 8;
    FFunctionY.FDecimal := 4;
    FFunctionY.FDefaultPadding := True;
    FFunctionY.ShowDecimalPoint := True;

    FFunctionNonSupply.FData := 0;
    FFunctionNonSupply.FInitalised := False;
    FFunctionNonSupply.FLength := 8;
    FFunctionNonSupply.FDecimal := 4;
    FFunctionNonSupply.FDefaultPadding := True;
    FFunctionNonSupply.ShowDecimalPoint := True;

    FFunctionCost.FData := 0;
    FFunctionCost.FInitalised := False;
    FFunctionCost.FLength := 8;
    FFunctionCost.FDecimal := 4;
    FFunctionCost.FDefaultPadding := True;
    FFunctionCost.ShowDecimalPoint := True;

    // Line 3
    FEscaltionRate.FData := '';
    FEscaltionRate.FInitalised := False;
    FEscaltionRate.FLength := 0;
    FEscaltionRate.FDecimal := 0;
    FEscaltionRate.FDefaultPadding := False;

    // Line 4
    FWaterQualityConstraint.FData := 0;
    FWaterQualityConstraint.FInitalised := False;
    FWaterQualityConstraint.FLength := 8;
    FWaterQualityConstraint.FDecimal := 2;
    FWaterQualityConstraint.FDefaultPadding := True;
    FWaterQualityConstraint.ShowDecimalPoint := True;
    
    for LIndex := Low(FTDSConcentration) to High(FTDSConcentration) do
    begin
      FTDSConcentration[LIndex].FData := 0;
      FTDSConcentration[LIndex].FInitalised := False;
      FTDSConcentration[LIndex].FLength := 8;
      FTDSConcentration[LIndex].FDecimal := 2;
      FTDSConcentration[LIndex].FDefaultPadding := True;
      FTDSConcentration[LIndex].ShowDecimalPoint := True;
    end;

    // Line 5
    FWQDisbenefit.FData := '';
    FWQDisbenefit.FInitalised := False;
    FWQDisbenefit.FLength := 0;
    FWQDisbenefit.FDecimal := 0;
    FWQDisbenefit.FDefaultPadding := False;

    Result :=  True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFileObject.Reset;
const OPNAME = 'TDisbenefitFileObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TDisbenefitFileDataObject }

procedure TDisbenefitFileDataObject.CreateMemberObjects;
const OPNAME = 'TDisbenefitFileDataObject.CreateMemberObjects';
begin
  inherited;
  try
    FDataYears                 := TInteger.Create;
    FDisbenefitList            := TObjectList.Create(True);
    FFMExtraLines              := TStringList.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFileDataObject.DestroyMemberObjects;
const OPNAME = 'TDisbenefitFileDataObject.DestroyMemberObjects';
begin
  inherited;
  try
    FDataYears.Free;
    FDisbenefitList.Free;
    FFMExtraLines.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDisbenefitFileDataObject.AddDisbenefit: TDisbenefitFileObject;
const OPNAME = 'TDisbenefitFileDataObject.AddDisbenefit';
begin
  Result := nil;
  try
    Result := TDisbenefitFileObject.Create;
    FDisbenefitList.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDisbenefitFileDataObject.DisbenefitCount: integer;
const OPNAME = 'TDisbenefitFileDataObject.DisbenefitCount';
begin
  Result := 0;
  try
    Result := FDisbenefitList.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDisbenefitFileDataObject.GetDisbenefitByIndex(AIndex: integer): TDisbenefitFileObject;
const OPNAME = 'TDisbenefitFileDataObject.GetDisbenefitByIndex';
begin
  Result := nil;
  try
   if(AIndex >= 0) and (AIndex < FDisbenefitList.Count) then
      Result := TDisbenefitFileObject(FDisbenefitList.Items[AIndex])
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDisbenefitFileDataObject.Initialise: boolean;
const OPNAME = 'TDisbenefitFileDataObject.Initialise';
begin
  Result := False;
  try
    FDataYears.FData := 0;
    FDataYears.FInitalised := False;
    FDataYears.FLength := 6;
    FDataYears.FDecimal := 0;
    FDataYears.FDefaultPadding := True;

    FDisbenefitList.Clear;
    FFMExtraLines.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFileDataObject.Reset;
const OPNAME = 'TDisbenefitFileDataObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


