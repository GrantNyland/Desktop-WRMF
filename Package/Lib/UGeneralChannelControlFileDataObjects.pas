//
//
//  UNIT      : Contains TGeneralChannelControlFileDataObject Class
//  AUTHOR    : Dziedzi Ramulondi(Aravia)
//  DATE      : 08/03/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UGeneralChannelControlFileDataObjects;

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

  TGeneralChannelControlObject = class(TAbstractDataObject)
  protected
    // Line 2
    FChannelNumber           : TInteger;
    FYearChannelActive       : TInteger;
    FMonthChannelActive      : TInteger;
    FYearChannelAbsolete     : TInteger;
    FMonthChannelAbsolete    : TInteger;
    FEconomicLifeOfChannel   : TInteger;
    FCapitalCost             : TDouble;
    FFixedMaintenanceCost    : TDouble;
    FVariableMaintenanceCost : TDouble;

    // Line 3
    FYearsInConstruction     : TInteger;
    FCostSchedule            : TString;

    // Line 4
    FEscaltionCosts          : TString;
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
    property EconomicLifeOfChannel : TInteger  read FEconomicLifeOfChannel;
    property CapitalCost           : TDouble   read FCapitalCost;
    property FixedMaintenanceCost  : TDouble   read FFixedMaintenanceCost;
    property VariableMaintenanceCost : TDouble read FVariableMaintenanceCost;

    // Line 3
    property YearsInConstruction   : TInteger  read FYearsInConstruction;
    property CostSchedule          : TString   read FCostSchedule;

    // Line 4
    property EscaltionCosts        : TString   read FEscaltionCosts;
  end;

  TGeneralChannelControlFileDataObject      = class(TAbstractDataObject)
  protected
    //Line 1
    FDataYears : TInteger;

    // Line 2,3,4
    FGeneralChannelControlList   : TObjectList;

    //Line 11... : Extra useless lines
    FFMExtraLines: TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetGeneralChannelControlByIndex(AIndex: integer):TGeneralChannelControlObject;
  public
    function Initialise: boolean;override;
    procedure Reset;override;
    function AddGeneralChannelControl:TGeneralChannelControlObject;
    // Line 1
    property DataYears         : TInteger  read FDataYears;
    function GeneralChannelControlCount: integer;
    property GeneralChannelControlByIndex[AIndex: integer] : TGeneralChannelControlObject read GetGeneralChannelControlByIndex;
    property FMExtraLines                                  : TStringList                  read FFMExtraLines ;
  end;
implementation

uses
  UErrorHandlingOperations;

{ TGeneralChannelControlObject }

procedure TGeneralChannelControlObject.CreateMemberObjects;
const OPNAME = 'TGeneralChannelControlObject.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    // Line 2
    FChannelNumber           := TInteger.Create;
    FYearChannelActive       := TInteger.Create;
    FMonthChannelActive      := TInteger.Create;
    FYearChannelAbsolete     := TInteger.Create;
    FMonthChannelAbsolete    := TInteger.Create;
    FEconomicLifeOfChannel   := TInteger.Create;
    FCapitalCost             := TDouble.Create;
    FFixedMaintenanceCost    := TDouble.Create;
    FVariableMaintenanceCost := TDouble.Create;

    // Line 3
    FYearsInConstruction     := TInteger.Create;
    FCostSchedule            := TString.Create;

    // Line 4
    FEscaltionCosts          := TString.Create;

    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralChannelControlObject.DestroyMemberObjects;
const OPNAME = 'TGeneralChannelControlObject.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    // Line 2
    FChannelNumber.Free;
    FYearChannelActive.Free;
    FMonthChannelActive.Free;
    FYearChannelAbsolete.Free;
    FMonthChannelAbsolete.Free;
    FEconomicLifeOfChannel.Free;
    FCapitalCost.Free;
    FFixedMaintenanceCost.Free;
    FVariableMaintenanceCost.Free;

    // Line 3
    FYearsInConstruction.Free;
    FCostSchedule.Free;

    // Line 4
    FEscaltionCosts.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGeneralChannelControlObject.Initialise: boolean;
const OPNAME = 'TGeneralChannelControlObject.Initialise';
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

    FEconomicLifeOfChannel.FData := 0;
    FEconomicLifeOfChannel.FInitalised := False;
    FEconomicLifeOfChannel.FLength := 6;
    FEconomicLifeOfChannel.FDecimal := 0;
    FEconomicLifeOfChannel.FDefaultPadding := True;

    FCapitalCost.FData := 0;
    FCapitalCost.FInitalised := False;
    FCapitalCost.FLength := 8;
    FCapitalCost.FDecimal := 2;
    FCapitalCost.FDefaultPadding := True;
    FCapitalCost.ShowDecimalPoint := True;

    FFixedMaintenanceCost.FData := 0;
    FFixedMaintenanceCost.FInitalised := False;
    FFixedMaintenanceCost.FLength := 8;
    FFixedMaintenanceCost.FDecimal := 2;
    FFixedMaintenanceCost.FDefaultPadding := True;
    FFixedMaintenanceCost.ShowDecimalPoint := True;

    FVariableMaintenanceCost.FData := 0;
    FVariableMaintenanceCost.FInitalised := False;
    FVariableMaintenanceCost.FLength := 8;
    FVariableMaintenanceCost.FDecimal := 4;
    FVariableMaintenanceCost.FDefaultPadding := True;
    FVariableMaintenanceCost.ShowDecimalPoint := True;

    FYearsInConstruction.FData := 0;
    FYearsInConstruction.FInitalised := False;
    FYearsInConstruction.FLength := 6;
    FYearsInConstruction.FDecimal := 0;
    FYearsInConstruction.FDefaultPadding := True;

    FCostSchedule.FData := '';
    FCostSchedule.FInitalised := False;
    FCostSchedule.FLength := 10;
    FCostSchedule.FDecimal := 0;
    FCostSchedule.FDefaultPadding := True;

    FEscaltionCosts.FData := '';
    FEscaltionCosts.FInitalised := False;
    FEscaltionCosts.FLength := 10;
    FEscaltionCosts.FDecimal := 0;
    FEscaltionCosts.FDefaultPadding := True;

    Result :=  True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralChannelControlObject.Reset;
const OPNAME = 'TGeneralChannelControlObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TGeneralChannelControlFileDataObject }

procedure TGeneralChannelControlFileDataObject.CreateMemberObjects;
const OPNAME = 'TGeneralChannelControlFileDataObject.CreateMemberObjects';
begin
  inherited;
  try
    FDataYears                 := TInteger.Create;
    FGeneralChannelControlList := TObjectList.Create(True);
    FFMExtraLines              := TStringList.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralChannelControlFileDataObject.DestroyMemberObjects;
const OPNAME = 'TGeneralChannelControlFileDataObject.DestroyMemberObjects';
begin
  inherited;
  try
    FDataYears.Free;
    FGeneralChannelControlList.Free;
    FFMExtraLines.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGeneralChannelControlFileDataObject.AddGeneralChannelControl: TGeneralChannelControlObject;
const OPNAME = 'TGeneralChannelControlFileDataObject.AddGeneralChannelControl';
begin
  Result := nil;
  try
    Result := TGeneralChannelControlObject.Create;
    FGeneralChannelControlList.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGeneralChannelControlFileDataObject.GeneralChannelControlCount: integer;
const OPNAME = 'TGeneralChannelControlFileDataObject.GeneralChannelControlCount';
begin
  Result := 0;
  try
    Result := FGeneralChannelControlList.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGeneralChannelControlFileDataObject.GetGeneralChannelControlByIndex(AIndex: integer): TGeneralChannelControlObject;
const OPNAME = 'TGeneralChannelControlFileDataObject.GetGeneralChannelControlByIndex';
begin
  Result := nil;
  try
   if(AIndex >= 0) and (AIndex < FGeneralChannelControlList.Count) then
      Result := TGeneralChannelControlObject(FGeneralChannelControlList.Items[AIndex])
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGeneralChannelControlFileDataObject.Initialise: boolean;
const OPNAME = 'TGeneralChannelControlFileDataObject.Initialise';
begin
  Result := False;
  try
    FDataYears.FData := 0;
    FDataYears.FInitalised := False;
    FDataYears.FLength := 6;
    FDataYears.FDecimal := 0;
    FDataYears.FDefaultPadding := True;

    FGeneralChannelControlList.Clear;
    FFMExtraLines.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralChannelControlFileDataObject.Reset;
const OPNAME = 'TGeneralChannelControlFileDataObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


