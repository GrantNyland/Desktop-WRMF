//
//
//  UNIT      : Contains TMonthlyDamLevels Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 26/07/2005
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UMonthlyDamLevelsObject;

interface
uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UAbstractDataObject,
  UBasicObjects,
  UConstants;
type
  TDamLevels = array [ MinMonths..MaxMonths ] of TDouble;
  TMonthlyDamLevels = class(TAbstractDataObject)
  protected
    FFYear : TInteger;
    FDamLevels : TDamLevels;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset;override;
    function Initialise: boolean;override;
    property FYear : TInteger read FFYear;
    property DamLevels : TDamLevels read FDamLevels;
  end;


  TMonthlyDamLevelsContainer  = class(TAbstractDataObject)
  protected
    FMonthlyDamLevelsObjectContainer : TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GeTMonthlyDamLevelsByIndex(AIndex: integer): TMonthlyDamLevels;
    function GeTMonthlyDamLevelsByYear(AYear: integer): TMonthlyDamLevels;
  public
    procedure Reset;override;
    function ItemsCount: integer;
    function Initialise: boolean;override;
    procedure AddMonthlyDamLevels(AYear,AMonth: integer; ALevel: Double);
    property MonthlyDamLevelsObjectByIndex [ AIndex : integer ] : TMonthlyDamLevels read GeTMonthlyDamLevelsByIndex;
  end;

  TMonthlyDamLevelsObject = class(TAbstractDataObject)
  protected
    FDamWaterLevelFileName : string;
    FGaugeCode : TString;
    FMonthlyDamLevelsContainer : TMonthlyDamLevelsContainer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset;override;
    function Initialise: boolean;override;
    property MonthlyDamLevelsContainer : TMonthlyDamLevelsContainer read FMonthlyDamLevelsContainer;
    property DamWaterLevelFileName : string read FDamWaterLevelFileName write FDamWaterLevelFileName;
    property GaugeCode : TString read FGaugeCode;
  end;

implementation
uses
  UErrorHandlingOperations;

{ TMonthlyDamLevels }

procedure TMonthlyDamLevels.CreateMemberObjects;
const OPNAME = 'TMonthlyDamLevels.CreateMemberObjects';
var
  LIndex : integer;
begin
  try
    FFYear := TInteger.Create;
    for LIndex := MinMonths to MaxMonths do
      FDamLevels [ LIndex ] := TDouble.Create;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyDamLevels.DestroyMemberObjects;
const OPNAME = 'TMonthlyDamLevels.DestroyMemberObjects';
var
  LIndex : integer;
begin
  try
    FreeAndNil ( FFYear );
    for LIndex := MinMonths to MaxMonths do
      FreeAndNil ( FDamLevels [ LIndex ] );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMonthlyDamLevels.Initialise : boolean;
const OPNAME = 'TMonthlyDamLevels.Initialise';
var
  LIndex: integer;
begin
  Result := False;
  try
    for LIndex := MinMonths to MaxMonths do
    begin
      FDamLevels[LIndex].FData := 0;
      FDamLevels[LIndex].FInitalised := False;
      FDamLevels[LIndex].FLength := 12;
      FDamLevels[LIndex].FDecimal := 8;
      FDamLevels[LIndex].FDefaultPadding := True;
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyDamLevels.Reset;
const OPNAME = 'TMonthlyDamLevels.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TMonthlyDamLevelsContainer }

procedure TMonthlyDamLevelsContainer.CreateMemberObjects;
const OPNAME = 'TMonthlyDamLevelsContainer.CreateMemberObjects';
begin
  inherited;
  try
    FMonthlyDamLevelsObjectContainer := TObjectList.Create(True);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyDamLevelsContainer.DestroyMemberObjects;
const OPNAME = 'TMonthlyDamLevelsContainer.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil ( FMonthlyDamLevelsObjectContainer );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMonthlyDamLevelsContainer.GeTMonthlyDamLevelsByYear(AYear: integer): TMonthlyDamLevels;
const OPNAME = 'TMonthlyDamLevelsContainer.GeTMonthlyDamLevelsByYear';
var
  LIndex: integer;
begin
  Result := nil;
  try
    for LIndex := 0  to FMonthlyDamLevelsObjectContainer.Count-1 do
    begin
      if(TMonthlyDamLevels(FMonthlyDamLevelsObjectContainer.Items[LIndex]).FFYear.FData = AYear) then
      begin
        Result := TMonthlyDamLevels(FMonthlyDamLevelsObjectContainer.Items[LIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TMonthlyDamLevelsContainer.AddMonthlyDamLevels(AYear,AMonth: integer; ALevel: Double);
const OPNAME = 'TMonthlyDamLevelsContainer.AddMonthlyDamLevels';
var
  LMonthlyDamLevels: TMonthlyDamLevels;
begin
  try
    LMonthlyDamLevels := GeTMonthlyDamLevelsByYear(AYear);
    if(LMonthlyDamLevels = nil) then
    begin

      LMonthlyDamLevels := TMonthlyDamLevels.Create;
      LMonthlyDamLevels.Reset;
      LMonthlyDamLevels.FFYear.FData := AYear;
      LMonthlyDamLevels.FFYear.FInitalised := True;
      FMonthlyDamLevelsObjectContainer.Add(LMonthlyDamLevels);
    end;
    LMonthlyDamLevels.FDamLevels[AMonth].FData := ALevel;
    LMonthlyDamLevels.FDamLevels[AMonth].FInitalised := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMonthlyDamLevelsContainer.GeTMonthlyDamLevelsByIndex ( AIndex : integer ) : TMonthlyDamLevels;
const OPNAME = 'TMonthlyDamLevelsContainer.GeTMonthlyDamLevelsByIndex';
begin
  Result := nil;
  try
    if (AIndex < FMonthlyDamLevelsObjectContainer.Count) and
      Assigned ( FMonthlyDamLevelsObjectContainer.Items [ AIndex ] ) then
      Result := TMonthlyDamLevels ( FMonthlyDamLevelsObjectContainer.Items [ AIndex ] );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMonthlyDamLevelsContainer.Initialise: boolean;
const OPNAME = 'TMonthlyDamLevelsContainer.Initialise';
begin
  Result := False;
  try
    FMonthlyDamLevelsObjectContainer.Clear;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMonthlyDamLevelsContainer.ItemsCount: integer;
const OPNAME = 'TMonthlyDamLevelsContainer.ItemsCount';
begin
  Result := 0;
  try
    Result := FMonthlyDamLevelsObjectContainer.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyDamLevelsContainer.Reset;
const OPNAME = 'TMonthlyDamLevelsContainer.Reset';
begin
  inherited;
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TMonthlyDamLevelsObject }

procedure TMonthlyDamLevelsObject.CreateMemberObjects;
const OPNAME = 'TMonthlyDamLevelsObject.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FMonthlyDamLevelsContainer := TMonthlyDamLevelsContainer.Create;
    FGaugeCode := TString.Create;
    Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyDamLevelsObject.DestroyMemberObjects;
const OPNAME = 'TMonthlyDamLevelsObject.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil ( FMonthlyDamLevelsContainer );
    FreeAndNil ( FGaugeCode );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMonthlyDamLevelsObject.Initialise: boolean;
const OPNAME = 'TMonthlyDamLevelsObject.Initialise';
begin
  Result := False;
  try
    Result := FMonthlyDamLevelsContainer.Initialise;
    FGaugeCode.FData := '';
    FGaugeCode.FInitalised := False;
    FGaugeCode.FLength := 0;
    FGaugeCode.FDecimal := 0;
    FGaugeCode.FDefaultPadding := True;
    FDamWaterLevelFileName := '';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyDamLevelsObject.Reset;
const OPNAME = 'TMonthlyDamLevelsObject.Reset';
begin
  try
    inherited;
    Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
