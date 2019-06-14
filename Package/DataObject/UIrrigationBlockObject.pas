//  UNIT      : Contains TIrrigationBlockObject Class
//  AUTHOR    : Maurice Marinus
//  DATE      : 06/06/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UIrrigationBlockObject;

interface

uses
  Classes, Sysutils, Contnrs,

  //  DWAF VCL
  UAbstractDataObject,
  UBasicObjects,
  UConstants;

type
  TWaterUsage = class(TAbstractDataObject)
  protected
    FBlockIdentifier: TInteger;
    FIdentifier: TInteger;
    FCropName: TString;
    FPercAreaUnderCropType: TDouble;
    FMonthlyWaterUse : array[MinMonths..MaxMonths] of TDouble;
    function GetMonthlyWaterUse(AIndex: Integer): TDouble;
    procedure SetMonthlyWaterUse(AIndex: Integer; const Value: TDouble);
    procedure SetPercAreaUnderCropType(const Value: TDouble);
    procedure SetCropName(const Value: TString);
    procedure SetIdentifier(const Value: TInteger);
    procedure SetBlockIdentifier(const Value: TInteger);

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    property PercAreaUnderCropType: TDouble read FPercAreaUnderCropType write SetPercAreaUnderCropType;
    property CropName : TString read FCropName write SetCropName;
    property Identifier : TInteger read FIdentifier write SetIdentifier;
    property BlockIdentifier : TInteger read FBlockIdentifier write SetBlockIdentifier;
    property MonthlyWaterUse[AIndex: Integer]: TDouble read GetMonthlyWaterUse write SetMonthlyWaterUse;
    procedure Reset; override;
    function Initialise: Boolean; override;
  end;

  TIrrigationBlock = class(TAbstractDataObject)
  protected
    FBlockIdentifier: TInteger;
    FEfficiencyFactor: TDouble;
    FInitialSoilMoistureStorage: TDouble;
    FReturnFlowLoss: TDouble;
    FLowerZoneReturnFlow: TDouble;
    FUpperZoneSoilMoistureTarget: TDouble;
    FUpperZoneReturnFlow: TDouble;
    FCanalTransportLoss: TDouble;
    FRainCatchmentScalingFactor: TDouble;
    FUpperZoneSoilMoistureCapacity: TDouble;
    FAllocatedIrrigationArea: TDouble;
    FReturnFlowFactor: TDouble;
    FLowerZoneSoilMoistureCapacity: TDouble;
    FRainBelowRainFactor: TDouble;
    FRainAboveRainFactorSpecValue: TDouble;
    FMaxWaterAllocation: TDouble;
    FBlockNodeNumber: TInteger;
    FNumberOfCropTypes: TInteger;
    FCropWaterUseType: TInteger;
    FDroughtApplicable: TInteger;
    FHydrologyNodeNumber: TInteger;
    FBlockName: TString;
    FFileName: TString;

    FAPanConvFactor   : array[MinMonths..MaxMonths] of TDouble;
    FPanEvaporation   : array[MinMonths..MaxMonths] of TDouble;
    FRainfallFactor   : array[MinMonths..MaxMonths] of TDouble;
    FWaterUsageFactor : array[MinNoOfCrops..MaxNoOfCrops] of TWaterUsage;

    function GetAPanConvFactor(AIndex: Integer): TDouble;
    function GetPanEvaporation(AIndex: Integer): TDouble;
    function GetRainfallFactor(AIndex: Integer): TDouble;
    function GetWaterUsageFactor(AIndex: Integer): TWaterUsage;

    procedure SetAPanConvFactor(AIndex: Integer; const Value: TDouble);
    procedure SetRainfallFactor(AIndex: Integer; const Value: TDouble);
    procedure SetPanEvaporation(AIndex: Integer; const Value: TDouble);
    procedure SetWaterUsageFactor(AIndex: Integer; const Value: TWaterUsage);

    procedure SetAllocatedIrrigationArea(const Value: TDouble);
    procedure SetBlockName(const Value: TString);
    procedure SetBlockNodeNumber(const Value: TInteger);
    procedure SetCanalTransportLoss(const Value: TDouble);
    procedure SetEfficiencyFactor(const Value: TDouble);
    procedure SetFileName(const Value: TString);
    procedure SetInitialSoilMoistureStorage(const Value: TDouble);
    procedure SetLowerZoneReturnFlow(const Value: TDouble);
    procedure SetLowerZoneSoilMoistureCapacity(const Value: TDouble);
    procedure SetMaxWaterAllocation(const Value: TDouble);
    procedure SetHydrologyNodeNumber(const Value: TInteger);
    procedure SetNumberOfCropTypes(const Value: TInteger);
    procedure SetCropWaterUseType(const Value: TInteger);
    procedure SetDroughtApplicable(const Value: TInteger);
    procedure SetRainAboveRainFactorSpecValue(const Value: TDouble);
    procedure SetRainBelowRainFactor(const Value: TDouble);
    procedure SetRainCatchmentScalingFactor(const Value: TDouble);
    procedure SetReturnFlowFactor(const Value: TDouble);
    procedure SetReturnFlowLoss(const Value: TDouble);
    procedure SetUpperZoneReturnFlow(const Value: TDouble);
    procedure SetUpperZoneSoilMoistureCapacity(const Value: TDouble);
    procedure SetUpperZoneSoilMoistureTarget(const Value: TDouble);
    procedure SetBlockIdentifier(const Value: TInteger);

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    property BlockNodeNumber                : TInteger read FBlockNodeNumber write SetBlockNodeNumber;
    property BlockName                      : TString  read FBlockName write SetBlockName;
    property MaxWaterAllocation             : TDouble  read FMaxWaterAllocation write SetMaxWaterAllocation;
    property FileName                       : TString  read FFileName write SetFileName;
    property HydrologyNodeNumber            : TInteger read FHydrologyNodeNumber write SetHydrologyNodeNumber;

    property CanalTransportLoss             : TDouble read FCanalTransportLoss write SetCanalTransportLoss;
    property EfficiencyFactor               : TDouble read FEfficiencyFactor write SetEfficiencyFactor;
    property ReturnFlowFactor               : TDouble read FReturnFlowFactor write SetReturnFlowFactor;
    property UpperZoneReturnFlow            : TDouble read FUpperZoneReturnFlow write SetUpperZoneReturnFlow;
    property LowerZoneReturnFlow            : TDouble read FLowerZoneReturnFlow write SetLowerZoneReturnFlow;
    property ReturnFlowLoss                 : TDouble read FReturnFlowLoss write SetReturnFlowLoss;

    property UpperZoneSoilMoistureCapacity  : TDouble read FUpperZoneSoilMoistureCapacity write SetUpperZoneSoilMoistureCapacity;
    property LowerZoneSoilMoistureCapacity  : TDouble read FLowerZoneSoilMoistureCapacity write SetLowerZoneSoilMoistureCapacity;
    property UpperZoneSoilMoistureTarget    : TDouble read FUpperZoneSoilMoistureTarget write SetUpperZoneSoilMoistureTarget;
    property InitialSoilMoistureStorage     : TDouble read FInitialSoilMoistureStorage write SetInitialSoilMoistureStorage;
    property NumberOfCropTypes              : TInteger read FNumberOfCropTypes write SetNumberOfCropTypes;
    property CropWaterUseType               : TInteger read FCropWaterUseType write SetCropWaterUseType;
    property DroughtApplicable             : TInteger read FDroughtApplicable write SetDroughtApplicable;

    property RainAboveRainFactorSpecValue   : TDouble read FRainAboveRainFactorSpecValue write SetRainAboveRainFactorSpecValue;
    property RainBelowRainFactor            : TDouble read FRainBelowRainFactor write SetRainBelowRainFactor;
    property RainCatchmentScalingFactor     : TDouble read FRainCatchmentScalingFactor write SetRainCatchmentScalingFactor;
    property AllocatedIrrigationArea        : TDouble read FAllocatedIrrigationArea write SetAllocatedIrrigationArea;
    property BlockIdentifier                : TInteger read FBlockIdentifier write SetBlockIdentifier;

    property RainfallFactor[AIndex: Integer]    : TDouble read GetRainfallFactor write SetRainfallFactor;
    property PanEvaporation[AIndex: Integer]    : TDouble read GetPanEvaporation write SetPanEvaporation;
    property APanConvFactor[AIndex: Integer]    : TDouble read GetAPanConvFactor write SetAPanConvFactor;
    property WaterUsageFactor[AIndex: Integer]  : TWaterUsage read GetWaterUsageFactor write SetWaterUsageFactor;

    procedure Reset; override;
    function Initialise: Boolean; override;
  end;

  TIrrigationBlockObject = class(TAbstractDataObject)
  protected
    FExtraLines: TStringList;
    FIrrigationBlockObjectContainer : TObjectList;
    function GetIrrigationBlockByIndex(AIndex: Integer): TIrrigationBlock;
    procedure SetExtraLines(const Value: TStringList);
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function IrrigationBlockCount : Integer;
    function AddIrrigationBlock: TIrrigationBlock;
    property IrrigationBlockObjectByIndex[AIndex: Integer] : TIrrigationBlock read GetIrrigationBlockByIndex;
    property ExtraLines : TStringList read FExtraLines write SetExtraLines;
    procedure Reset; override;
    function Initialise: Boolean; override;
  end;

implementation

uses
  UErrorHandlingOperations;

{ TIrrigationBlock }

procedure TIrrigationBlock.CreateMemberObjects;
const OPNAME = 'TIrrigationBlock.CreateMemberObjects';
var
  LCount : Integer;
begin
  try
    inherited CreateMemberObjects;

    FEfficiencyFactor               := TDouble.Create;
    FInitialSoilMoistureStorage     := TDouble.Create;
    FReturnFlowLoss                 := TDouble.Create;
    FLowerZoneReturnFlow            := TDouble.Create;
    FUpperZoneSoilMoistureTarget    := TDouble.Create;
    FUpperZoneReturnFlow            := TDouble.Create;
    FCanalTransportLoss             := TDouble.Create;
    FRainCatchmentScalingFactor     := TDouble.Create;
    FUpperZoneSoilMoistureCapacity  := TDouble.Create;
    FAllocatedIrrigationArea        := TDouble.Create;
    FReturnFlowFactor               := TDouble.Create;
    FLowerZoneSoilMoistureCapacity  := TDouble.Create;
    FRainBelowRainFactor            := TDouble.Create;
    FRainAboveRainFactorSpecValue   := TDouble.Create;
    FMaxWaterAllocation             := TDouble.Create;
    FBlockIdentifier                := TInteger.Create;
    FBlockNodeNumber                := TInteger.Create;
    FNumberOfCropTypes              := TInteger.Create;
    FCropWaterUseType               := TInteger.Create;
    FDroughtApplicable              := TInteger.Create;
    FHydrologyNodeNumber            := TInteger.Create;
    FBlockName                      := TString.Create;
    FFileName                       := TString.Create;

    for LCount := MinMonths to MaxMonths do
      FAPanConvFactor[LCount]   := TDouble.Create;
    for LCount := MinMonths to MaxMonths do
      FPanEvaporation[LCount]   := TDouble.Create;
    for LCount := MinMonths to MaxMonths do
      FRainfallFactor[LCount]   := TDouble.Create;
    for LCount := MinNoOfCrops to MaxNoOfCrops do
      FWaterUsageFactor[LCount] := TWaterUsage.Create;

    Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIrrigationBlock.DestroyMemberObjects;
const OPNAME = 'TIrrigationBlock.DestroyMemberObjects';
var
  LCount : Integer;
begin
  try
    FEfficiencyFactor.Free;
    FInitialSoilMoistureStorage.Free;
    FReturnFlowLoss.Free;
    FLowerZoneReturnFlow.Free;
    FUpperZoneSoilMoistureTarget.Free;
    FUpperZoneReturnFlow.Free;
    FCanalTransportLoss.Free;
    FRainCatchmentScalingFactor.Free;
    FUpperZoneSoilMoistureCapacity.Free;
    FAllocatedIrrigationArea.Free;
    FReturnFlowFactor.Free;
    FLowerZoneSoilMoistureCapacity.Free;
    FRainBelowRainFactor.Free;
    FRainAboveRainFactorSpecValue.Free;
    FMaxWaterAllocation.Free;
    FBlockNodeNumber.Free;
    FNumberOfCropTypes.Free;
    FCropWaterUseType.Free;
    FDroughtApplicable.Free;
    FHydrologyNodeNumber.Free;
    FBlockName.Free;
    FFileName.Free;
    FBlockIdentifier.Free;

    for LCount := MinMonths to MaxMonths do
      FAPanConvFactor[LCount].Free;
    for LCount := MinMonths to MaxMonths do
      FPanEvaporation[LCount].Free;
    for LCount := MinMonths to MaxMonths do
      FRainfallFactor[LCount].Free;
    for LCount := MinNoOfCrops to MaxNoOfCrops do
      FreeAndNil(FWaterUsageFactor[LCount]);

    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.GetAPanConvFactor(AIndex: Integer): TDouble;
const OPNAME = 'TIrrigationBlock.GetAPanConvFactor';
begin
  Result := nil;
  try
    if (AIndex in [MinMonths..MaxMonths]) then
      Result :=  FAPanConvFactor[AIndex];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIrrigationBlock.GetPanEvaporation(AIndex: Integer): TDouble;
const OPNAME = 'TIrrigationBlock.GetPanEvaporation';
begin
  Result := nil;
  try
    if (AIndex in [MinMonths..MaxMonths]) then
      Result :=  FPanEvaporation[AIndex];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIrrigationBlock.GetRainfallFactor(AIndex: Integer): TDouble;
const OPNAME = 'TIrrigationBlock.GetRainfallFactor';
begin
  Result := nil;
  try
    if (AIndex in [MinMonths..MaxMonths]) then
      Result :=  FRainfallFactor[AIndex];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIrrigationBlock.GetWaterUsageFactor(AIndex: Integer): TWaterUsage;
const OPNAME = 'TIrrigationBlock.GetWaterUsageFactor';
begin
  Result := nil;
  try
    if (AIndex in [MinNoOfCrops..MaxNoOfCrops]) then
      Result :=  FWaterUsageFActor[AIndex];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIrrigationBlock.Initialise: Boolean;
const OPNAME = 'TIrrigationBlock.Initialise';
var
  LCount : Integer;
begin
  Result := False;
  try
    //Line 1
    FBlockNodeNumber.FData           := 0;
    FBlockNodeNumber.FInitalised     := False;
    FBlockNodeNumber.FLength         := 3;
    FBlockNodeNumber.FDefaultPadding := True;

    FBlockName.FData           := '';
    FBlockName.FInitalised     := False;
    FBlockName.FLength         := 0;
    FBlockName.FDefaultPadding := True;

    FMaxWaterAllocation.FData             := 0;
    FMaxWaterAllocation.FInitalised       := False;
    FMaxWaterAllocation.FLength           := 10;
    FMaxWaterAllocation.FDefaultPadding   := True;
    FMaxWaterAllocation.FDecimal          := 3;
    FMaxWaterAllocation.ShowDecimalPoint  := True;

    FFileName.FData           := '';
    FFileName.FInitalised     := False;
    FFileName.FLength         := 0;
    FFileName.FDefaultPadding := True;

    FHydrologyNodeNumber.FData           := 0;
    FHydrologyNodeNumber.FInitalised     := False;
    FHydrologyNodeNumber.FLength         := 5;
    FHydrologyNodeNumber.FDefaultPadding := True;

    FBlockIdentifier.FData           := 0;
    FBlockIdentifier.FInitalised     := False;
    FBlockIdentifier.FLength         := 5;
    FBlockIdentifier.FDefaultPadding := True;

    //Line 2
    FCanalTransportLoss.FData             := 0;
    FCanalTransportLoss.FInitalised       := False;
    FCanalTransportLoss.FLength           := 10;
    FCanalTransportLoss.FDefaultPadding   := True;
    FCanalTransportLoss.FDecimal          := 5;
    FCanalTransportLoss.ShowDecimalPoint  := True;

    FEfficiencyFactor.FData             := 0;
    FEfficiencyFactor.FInitalised       := False;
    FEfficiencyFactor.FLength           := 10;
    FEfficiencyFactor.FDefaultPadding   := True;
    FEfficiencyFactor.FDecimal          := 5;
    FEfficiencyFactor.ShowDecimalPoint  := True;

    FReturnFlowFactor.FData             := 0;
    FReturnFlowFactor.FInitalised       := False;
    FReturnFlowFactor.FLength           := 10;
    FReturnFlowFactor.FDefaultPadding   := True;
    FReturnFlowFactor.FDecimal          := 5;
    FReturnFlowFactor.ShowDecimalPoint  := True;

    FUpperZoneReturnFlow.FData             := 0;
    FUpperZoneReturnFlow.FInitalised       := False;
    FUpperZoneReturnFlow.FLength           := 10;
    FUpperZoneReturnFlow.FDefaultPadding   := True;
    FUpperZoneReturnFlow.FDecimal          := 5;
    FUpperZoneReturnFlow.ShowDecimalPoint  := True;

    FLowerZoneReturnFlow.FData             := 0;
    FLowerZoneReturnFlow.FInitalised       := False;
    FLowerZoneReturnFlow.FLength           := 10;
    FLowerZoneReturnFlow.FDefaultPadding   := True;
    FLowerZoneReturnFlow.FDecimal          := 5;
    FLowerZoneReturnFlow.ShowDecimalPoint  := True;

    FReturnFlowLoss.FData             := 0;
    FReturnFlowLoss.FInitalised       := False;
    FReturnFlowLoss.FLength           := 10;
    FReturnFlowLoss.FDefaultPadding   := True;
    FReturnFlowLoss.FDecimal          := 5;
    FReturnFlowLoss.ShowDecimalPoint  := True;

    //Line 3
    FUpperZoneSoilMoistureCapacity.FData             := 0;
    FUpperZoneSoilMoistureCapacity.FInitalised       := False;
    FUpperZoneSoilMoistureCapacity.FLength           := 10;
    FUpperZoneSoilMoistureCapacity.FDefaultPadding   := True;
    FUpperZoneSoilMoistureCapacity.FDecimal          := 3;
    FUpperZoneSoilMoistureCapacity.ShowDecimalPoint  := True;

    FLowerZoneSoilMoistureCapacity.FData             := 0;
    FLowerZoneSoilMoistureCapacity.FInitalised       := False;
    FLowerZoneSoilMoistureCapacity.FLength           := 10;
    FLowerZoneSoilMoistureCapacity.FDefaultPadding   := True;
    FLowerZoneSoilMoistureCapacity.FDecimal          := 3;
    FLowerZoneSoilMoistureCapacity.ShowDecimalPoint  := True;

    FUpperZoneSoilMoistureTarget.FData             := 0;
    FUpperZoneSoilMoistureTarget.FInitalised       := False;
    FUpperZoneSoilMoistureTarget.FLength           := 10;
    FUpperZoneSoilMoistureTarget.FDefaultPadding   := True;
    FUpperZoneSoilMoistureTarget.FDecimal          := 3;
    FUpperZoneSoilMoistureTarget.ShowDecimalPoint  := True;

    FInitialSoilMoistureStorage.FData             := 0;
    FInitialSoilMoistureStorage.FInitalised       := False;
    FInitialSoilMoistureStorage.FLength           := 10;
    FInitialSoilMoistureStorage.FDefaultPadding   := True;
    FInitialSoilMoistureStorage.FDecimal          := 3;
    FInitialSoilMoistureStorage.ShowDecimalPoint  := True;

    //Line 4
    for LCount := MinMonths to MaxMonths do
    begin
      FRainfallFactor[LCount].FData             := 0;
      FRainfallFactor[LCount].FInitalised       := False;
      FRainfallFactor[LCount].FLength           := 10;
      FRainfallFactor[LCount].FDefaultPadding   := True;
      FRainfallFactor[LCount].FDecimal          := 3;
      FRainfallFactor[LCount].ShowDecimalPoint  := True;
    end;

    //Line 5
    for LCount := MinMonths to MaxMonths do
    begin
      FPanEvaporation[LCount].FData             := 0;
      FPanEvaporation[LCount].FInitalised       := False;
      FPanEvaporation[LCount].FLength           := 10;
      FPanEvaporation[LCount].FDefaultPadding   := True;
      FPanEvaporation[LCount].FDecimal          := 3;
      FPanEvaporation[LCount].ShowDecimalPoint  := True;
    end;

    //Line 6
    for LCount := MinMonths to MaxMonths do
    begin
      FAPanConvFactor[LCount].FData             := 0;
      FAPanConvFactor[LCount].FInitalised       := False;
      FAPanConvFactor[LCount].FLength           := 10;
      FAPanConvFactor[LCount].FDefaultPadding   := True;
      FAPanConvFactor[LCount].FDecimal          := 3;
      FAPanConvFactor[LCount].ShowDecimalPoint  := True;
    end;

    //Line 7
    FNumberOfCropTypes.FData           := 0;
    FNumberOfCropTypes.FInitalised     := False;
    FNumberOfCropTypes.FLength         := 5;
    FNumberOfCropTypes.FDefaultPadding := True;

    FCropWaterUseType.FData           := 0;
    FCropWaterUseType.FInitalised     := False;
    FCropWaterUseType.FLength         := 5;
    FCropWaterUseType.FDefaultPadding := True;

    FDroughtApplicable.FData           := 0;
    FDroughtApplicable.FInitalised     := False;
    FDroughtApplicable.FLength         := 5;
    FDroughtApplicable.FDefaultPadding := True;

    //Line 8
    for LCount := MinNoOfCrops to MaxNoOfCrops do
      FWaterUsageFActor[LCount].Initialise;

    //Line 9
    FRainAboveRainFactorSpecValue.FData             := 0;
    FRainAboveRainFactorSpecValue.FInitalised       := False;
    FRainAboveRainFactorSpecValue.FLength           := 10;
    FRainAboveRainFactorSpecValue.FDefaultPadding   := True;
    FRainAboveRainFactorSpecValue.FDecimal          := 3;
    FRainAboveRainFactorSpecValue.ShowDecimalPoint  := True;

    FRainBelowRainFactor.FData             := 0;
    FRainBelowRainFactor.FInitalised       := False;
    FRainBelowRainFactor.FLength           := 10;
    FRainBelowRainFactor.FDefaultPadding   := True;
    FRainBelowRainFactor.FDecimal          := 3;
    FRainBelowRainFactor.ShowDecimalPoint  := True;

    FRainCatchmentScalingFactor.FData             := 0;
    FRainCatchmentScalingFactor.FInitalised       := False;
    FRainCatchmentScalingFactor.FLength           := 10;
    FRainCatchmentScalingFactor.FDefaultPadding   := True;
    FRainCatchmentScalingFactor.FDecimal          := 3;
    FRainCatchmentScalingFactor.ShowDecimalPoint  := True;

    //Line 10
    FAllocatedIrrigationArea.FData             := 0;
    FAllocatedIrrigationArea.FInitalised       := False;
    FAllocatedIrrigationArea.FLength           := 10;
    FAllocatedIrrigationArea.FDefaultPadding   := True;
    FAllocatedIrrigationArea.FDecimal          := 3;
    FAllocatedIrrigationArea.ShowDecimalPoint  := True;

    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIrrigationBlock.Reset;
const OPNAME = 'TIrrigationBlock.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlock.SetAllocatedIrrigationArea(
  const Value: TDouble);
const OPNAME = 'TIrrigationBlock.SetAllocatedIrrigationArea';
begin
  FAllocatedIrrigationArea := Value;
end;

procedure TIrrigationBlock.SetAPanConvFactor(AIndex: Integer;
  const Value: TDouble);
const OPNAME = 'TIrrigationBlock.SetAPanConvFactor';
begin
  try
    if (AIndex in [MinMonths..MaxMonths]) then
      FAPanConvFactor[AIndex].FData := Value.FData;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIrrigationBlock.SetBlockName(const Value: TString);
const OPNAME = 'TIrrigationBlock.SetBlockName';
begin
  FBlockName := Value;
end;

procedure TIrrigationBlock.SetBlockNodeNumber(const Value: TInteger);
const OPNAME = 'TIrrigationBlock.SetBlockNodeNumber';
begin
  FBlockNodeNumber := Value;
end;

procedure TIrrigationBlock.SetCanalTransportLoss(const Value: TDouble);
const OPNAME = 'TIrrigationBlock.SetCanalTransportLoss';
begin
  FCanalTransportLoss := Value;
end;

procedure TIrrigationBlock.SetEfficiencyFactor(const Value: TDouble);
const OPNAME = 'TIrrigationBlock.SetEfficiencyFactor';
begin
  FEfficiencyFactor := Value;
end;

procedure TIrrigationBlock.SetFileName(const Value: TString);
const OPNAME = 'TIrrigationBlock.SetFileName';
begin
  FFileName := Value;
end;

procedure TIrrigationBlock.SetBlockIdentifier(const Value: TInteger);
const OPNAME = 'TIrrigationBlock.SetBlockIdentifier';
begin
  FBlockIdentifier := Value;
end;

procedure TIrrigationBlock.SetInitialSoilMoistureStorage(
  const Value: TDouble);
const OPNAME = 'TIrrigationBlock.SetInitialSoilMoistureStorage';
begin
  FInitialSoilMoistureStorage := Value;
end;

procedure TIrrigationBlock.SetLowerZoneReturnFlow(const Value: TDouble);
const OPNAME = 'TIrrigationBlock.SetLowerZoneReturnFlow';
begin
  FLowerZoneReturnFlow := Value;
end;

procedure TIrrigationBlock.SetLowerZoneSoilMoistureCapacity(
  const Value: TDouble);
const OPNAME = 'TIrrigationBlock.SetLowerZoneSoilMoistureCapacity';
begin
  FLowerZoneSoilMoistureCapacity := Value;
end;

procedure TIrrigationBlock.SetMaxWaterAllocation(const Value: TDouble);
const OPNAME = 'TIrrigationBlock.SetMaxWaterAllocation';
begin
  FMaxWaterAllocation := Value;
end;

procedure TIrrigationBlock.SetHydrologyNodeNumber(const Value: TInteger);
const OPNAME = 'TIrrigationBlock.SetHydrologyNodeNumber';
begin
  FHydrologyNodeNumber := Value;
end;

procedure TIrrigationBlock.SetNumberOfCropTypes(const Value: TInteger);
const OPNAME = 'TIrrigationBlock.SetNumberOfCropTypes';
begin
  FNumberOfCropTypes := Value;
end;

procedure TIrrigationBlock.SetCropWaterUseType(const Value: TInteger);
const OPNAME = 'TIrrigationBlock.SetCropWaterUseType';
begin
  FCropWaterUseType := Value;
end;

procedure TIrrigationBlock.SetDroughtApplicable(const Value: TInteger);
const OPNAME = 'TIrrigationBlock.SetDroughtApplicable';
begin
  FDroughtApplicable := Value;
end;

procedure TIrrigationBlock.SetPanEvaporation(AIndex: Integer;
  const Value: TDouble);
const OPNAME = 'TIrrigationBlock.SetPanEvaporation';
begin
  try
    if (AIndex in [MinMonths..MaxMonths]) then
      FPanEvaporation[AIndex].FData := Value.FData;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIrrigationBlock.SetRainAboveRainFactorSpecValue(
  const Value: TDouble);
const OPNAME = 'TIrrigationBlock.SetRainAboveRainFactorSpecValue';
begin
  FRainAboveRainFactorSpecValue := Value;
end;

procedure TIrrigationBlock.SetRainBelowRainFactor(const Value: TDouble);
const OPNAME = 'TIrrigationBlock.SetRainBelowRainFactor';
begin
  FRainBelowRainFactor := Value;
end;

procedure TIrrigationBlock.SetRainCatchmentScalingFactor(
  const Value: TDouble);
const OPNAME = 'TIrrigationBlock.SetRainCatchmentScalingFactor';
begin
  FRainCatchmentScalingFactor := Value;
end;

procedure TIrrigationBlock.SetRainfallFactor(AIndex: Integer;
  const Value: TDouble);
const OPNAME = 'TIrrigationBlock.SetRainfallFactor';
begin
  try
    if (AIndex in [MinMonths..MaxMonths]) then
      FRainfallFactor[AIndex].FData := Value.FData;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIrrigationBlock.SetReturnFlowFactor(const Value: TDouble);
const OPNAME = 'TIrrigationBlock.SetReturnFlowFactor';
begin
  FReturnFlowFactor := Value;
end;

procedure TIrrigationBlock.SetReturnFlowLoss(const Value: TDouble);
const OPNAME = 'TIrrigationBlock.SetReturnFlowLoss';
begin
  FReturnFlowLoss := Value;
end;

procedure TIrrigationBlock.SetUpperZoneReturnFlow(const Value: TDouble);
const OPNAME = 'TIrrigationBlock.SetUpperZoneReturnFlow';
begin
  FUpperZoneReturnFlow := Value;
end;

procedure TIrrigationBlock.SetUpperZoneSoilMoistureCapacity(
  const Value: TDouble);
const OPNAME = 'TIrrigationBlock.SetUpperZoneSoilMoistureCapacity';
begin
  FUpperZoneSoilMoistureCapacity := Value;
end;

procedure TIrrigationBlock.SetUpperZoneSoilMoistureTarget(
  const Value: TDouble);
const OPNAME = 'TIrrigationBlock.SetUpperZoneSoilMoistureTarget';
begin
  FUpperZoneSoilMoistureTarget := Value;
end;

procedure TIrrigationBlock.SetWaterUsageFactor(AIndex: Integer;
  const Value: TWaterUsage);
const OPNAME = 'TIrrigationBlock.SetWaterUsageFactor';
begin
  try
    if (AIndex in [MinNoOfCrops..MaxNoOfCrops]) then
      FWaterUsageFActor[AIndex] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TIrrigationBlockObject }

function TIrrigationBlockObject.AddIrrigationBlock: TIrrigationBlock;
const OPNAME = 'TIrrigationBlockObject.AddIrrigationBlock';
begin
   Result := TIrrigationBlock.Create;
   FIrrigationBlockObjectContainer.Add(Result);
end;

procedure TIrrigationBlockObject.CreateMemberObjects;
const OPNAME = 'TIrrigationBlockObject.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FExtraLines                     := TStringlist.Create;
    FIrrigationBlockObjectContainer := TObjectList.Create;
    Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIrrigationBlockObject.DestroyMemberObjects;
const OPNAME = 'TIrrigationBlockObject.DestroyMemberObjects';
begin
  try
    FIrrigationBlockObjectContainer.Clear;
    FreeAndNil(FIrrigationBlockObjectContainer);
    FExtraLines.Free;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIrrigationBlockObject.GetIrrigationBlockByIndex(
  AIndex: Integer): TIrrigationBlock;
const OPNAME = 'TIrrigationBlockObject.GetIrrigationBlockByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FIrrigationBlockObjectContainer.Count) then
      Result :=  TIrrigationBlock(FIrrigationBlockObjectContainer[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIrrigationBlockObject.Initialise: Boolean;
const OPNAME = 'TIrrigationBlockObject.Initialise';
begin
  Result := False;
  try
    FIrrigationBlockObjectContainer.Clear;
    FExtraLines.Clear;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIrrigationBlockObject.IrrigationBlockCount: Integer;
const OPNAME = 'TIrrigationBlockObject.IrrigationBlockCount';
begin
  Result := 0;
  try
    Result := FIrrigationBlockObjectContainer.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIrrigationBlockObject.Reset;
const OPNAME = 'TIrrigationBlockObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIrrigationBlockObject.SetExtraLines(const Value: TStringList);
const OPNAME = 'TIrrigationBlockObject.SetExtraLines';
begin
  FExtraLines := Value;
end;

{ TWaterUsage }

procedure TWaterUsage.CreateMemberObjects;
const OPNAME = 'TWaterUsage.CreateMemberObjects';
var
  LCount: Integer;
begin
  try
    inherited CreateMemberObjects;

    FIdentifier             := TInteger.Create;
    FCropName               := TString.Create;
    FPercAreaUnderCropType  := TDouble.Create;
    FBlockIdentifier        := TInteger.Create;
    for LCount := MinMonths to MaxMonths do
      FMonthlyWaterUse[LCount] := TDouble.Create;

    Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWaterUsage.DestroyMemberObjects;
const OPNAME = 'TWaterUsage.DestroyMemberObjects';
var
  LCount: Integer;
begin
  try
    FIdentifier.Free;
    FCropName.Free;
    FPercAreaUnderCropType.Free;
    FBlockIdentifier.Free;
    for LCount := MinMonths to MaxMonths do
      FMonthlyWaterUse[LCount].Free;

    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterUsage.GetMonthlyWaterUse(AIndex: Integer): TDouble;
const OPNAME = 'TWaterUsage.GetMonthlyWaterUse';
begin
  Result := nil;
  try
    if (AIndex in [MinMonths..MaxMonths]) then
      Result :=  FMonthlyWaterUse[AIndex];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWaterUsage.Initialise: Boolean;
const OPNAME = 'TWaterUsage.Initialise';
var
  LCount : Integer;
begin
  Result := False;
  try
    FIdentifier.FData             := 0;
    FIdentifier.FInitalised       := False;
    FIdentifier.FLength           := 3;
    FIdentifier.FDefaultPadding   := True;
    FIdentifier.FDecimal          := 0;

    FBlockIdentifier.FData             := 0;
    FBlockIdentifier.FInitalised       := False;
    FBlockIdentifier.FLength           := 3;
    FBlockIdentifier.FDefaultPadding   := True;

    FCropName.FData             := '';
    FCropName.FInitalised       := False;
    FCropName.FLength           := 0;
    FCropName.FDefaultPadding   := True;
    FCropName.FDecimal          := 0;

    FPercAreaUnderCropType.FData             := 0;
    FPercAreaUnderCropType.FInitalised       := False;
    FPercAreaUnderCropType.FLength           := 10;
    FPercAreaUnderCropType.FDefaultPadding   := True;
    FPercAreaUnderCropType.FDecimal          := 3;
    FPercAreaUnderCropType.ShowDecimalPoint  := True;

    for LCount := MinMonths to MaxMonths do
    begin
      FMonthlyWaterUse[LCount].FData             := 0;
      FMonthlyWaterUse[LCount].FInitalised       := False;
      FMonthlyWaterUse[LCount].FLength           := 10;
      FMonthlyWaterUse[LCount].FDefaultPadding   := True;
      FMonthlyWaterUse[LCount].FDecimal          := 3;
      FMonthlyWaterUse[LCount].ShowDecimalPoint  := True;
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWaterUsage.Reset;
const OPNAME = 'TWaterUsage.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWaterUsage.SetBlockIdentifier(const Value: TInteger);
const OPNAME = 'TWaterUsage.SetBlockIdentifier';
begin
  FBlockIdentifier := Value;
end;

procedure TWaterUsage.SetCropName(const Value: TString);
const OPNAME = 'TWaterUsage.SetCropName';
begin
  FCropName := Value;
end;

procedure TWaterUsage.SetIdentifier(const Value: TInteger);
const OPNAME = 'TWaterUsage.SetIdentifier';
begin
  FIdentifier := Value;
end;

procedure TWaterUsage.SetMonthlyWaterUse(AIndex: Integer;
  const Value: TDouble);
const OPNAME = 'TWaterUsage.SetMonthlyWaterUse';
begin
  try
    if (AIndex in [MinMonths..MaxMonths]) then
      FMonthlyWaterUse[AIndex].FData := Value.FData;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWaterUsage.SetPercAreaUnderCropType(const Value: TDouble);
const OPNAME = 'TWaterUsage.SetPercAreaUnderCropType';
begin
  FPercAreaUnderCropType := Value;
end;

end.