//
//
//  UNIT      : Contains TReleaseStructureObject Class
//  AUTHOR    : Titi Ngubane(Arivia)
//  DATE      : 15/03/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UDemandReconciliationObject;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UConstants,
  UBasicObjects,
  UAbstractDataObject;

type
  TDemandReconciliationData = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    //File F16.dat
    //Line 1
    FUserTypeCount : TInteger;
    FRiskCriteriaCount : TInteger;
    FScenarioCount      : TInteger;

    //Line 4
    FChannelsCount :TInteger;

    //Line 2
    FRecurrenceInterval  : Array[1..10] of TDouble;

    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TDemandPortion  = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    //Line 3 type
    FDemandPortionValues  : Array[1..10] of TDouble;

     procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TDemandChannel  = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public

    FChannelNumber : TInteger;
    FUserType : TInteger;
    //Line 5 type
    FDemandPortionValues  : Array[1..10] of TDouble;

     procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TDemandReconciliationDataObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    //Line 1,2,4
    FDemandReconciliationData : TDemandReconciliationData;

    //Line 3
    FDemandPortionList: TObjectList;
    //Line 5
    FDemandChannelsList : TObjectList;
    FF16ExtraLines: TStringList;
    function AddDemandPortion : TDemandPortion;
    function AddDemandChannel : TDemandChannel;
    function DemandPortionByIndex(AIndex: integer) : TDemandPortion;
    function DemandChannelByIndex(AIndex: integer) : TDemandChannel;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

implementation


uses UErrorHandlingOperations;


{ TDemandReconciliationData }

procedure TDemandReconciliationData.CreateMemberObjects;
const OPNAME = 'TDemandReconciliationData.CreateMemberObjects';
var
  LCount: integer;
  LDouble : TDouble;
begin
  try
    FUserTypeCount    := TInteger.Create;
    FRiskCriteriaCount  := TInteger.Create;
    FScenarioCount    := TInteger.Create;
    FChannelsCount         := TInteger.Create;
    for LCount := 1 to 10 do
    begin
      LDouble := TDouble.Create;
      FRecurrenceInterval[LCount] := LDouble;
    end;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDemandReconciliationData.DestroyMemberObjects;
const OPNAME = 'TDemandReconciliationData.DestroyMemberObjects';
var
  LCount: integer;
begin
  try
    FUserTypeCount.Free;
    FRiskCriteriaCount.Free;
    FScenarioCount.Free;
    FChannelsCount.Free;
    for LCount := 1 to 10 do
      FRecurrenceInterval[LCount].Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandReconciliationData.Initialise: boolean;
const OPNAME = 'TDemandReconciliationData.Initialise';
var
  LCount : Integer;
begin
  Result := False;
  try
    FUserTypeCount.FData := 0;
    FUserTypeCount.FInitalised := False;
    FUserTypeCount.FLength := 6;
    FUserTypeCount.FDecimal := 0;
    FUserTypeCount.FDefaultPadding := True;

    FRiskCriteriaCount.FData := 0;
    FRiskCriteriaCount.FInitalised := False;
    FRiskCriteriaCount.FLength := 6;
    FRiskCriteriaCount.FDecimal := 0;
    FRiskCriteriaCount.FDefaultPadding := True;

    FScenarioCount.FData := 0;
    FScenarioCount.FInitalised := False;
    FScenarioCount.FLength := 6;
    FScenarioCount.FDecimal := 0;
    FScenarioCount.FDefaultPadding := True;

    FChannelsCount.FData := 0;
    FChannelsCount.FInitalised := False;
    FChannelsCount.FLength := 6;
    FChannelsCount.FDecimal := 0;
    FChannelsCount.FDefaultPadding := False;

    for LCount := 1 to 10 do
    begin
      FRecurrenceInterval[LCount].FData := 0;
      FRecurrenceInterval[LCount].FInitalised := False;
      FRecurrenceInterval[LCount].FLength := 8;
      FRecurrenceInterval[LCount].FDecimal := 2;
      FRecurrenceInterval[LCount].FDefaultPadding := False;
      FRecurrenceInterval[LCount].ShowDecimalPoint := True;
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDemandReconciliationData.Reset;
const OPNAME = 'TDemandReconciliationData.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TDemandPortion }

procedure TDemandPortion.CreateMemberObjects;
const OPNAME = 'TDemandPortion.CreateMemberObjects';
var
  LCount: integer;
  LDouble : TDouble;
begin
  try
    for LCount := 1 to 10 do
    begin
      LDouble := TDouble.Create;
      FDemandPortionValues[LCount] := LDouble;
    end;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDemandPortion.DestroyMemberObjects;
const OPNAME = 'TDemandPortion.DestroyMemberObjects';
var
  LCount: integer;
begin
  try
    for LCount := 1 to 10 do
      FDemandPortionValues[LCount].Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandPortion.Initialise: boolean;
const OPNAME = 'TDemandPortion.Initialise';
var
  LCount : Integer;
begin
  Result := False;
  try
    for LCount := 1 to 10 do
    begin
      FDemandPortionValues[LCount].FData := 0;
      FDemandPortionValues[LCount].FInitalised := False;
      FDemandPortionValues[LCount].FLength := 8;
      FDemandPortionValues[LCount].FDecimal := 2;
      FDemandPortionValues[LCount].FDefaultPadding := False;
      FDemandPortionValues[LCount].ShowDecimalPoint := True;
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDemandPortion.Reset;
const OPNAME = 'TDemandPortion.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TDemandChannel }

procedure TDemandChannel.CreateMemberObjects;
const OPNAME = 'TDemandChannel.CreateMemberObjects';
var
  LCount: integer;
  LDouble : TDouble;
begin
  try
    FChannelNumber := TInteger.Create;
    FUserType  := TInteger.Create;
    for LCount := 1 to 10 do
    begin
      LDouble := TDouble.Create;
      FDemandPortionValues[LCount] := LDouble;
    end;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDemandChannel.DestroyMemberObjects;
const OPNAME = 'TDemandChannel.DestroyMemberObjects';
var
  LCount: integer;
begin
  try
    FChannelNumber.Free;
    FUserType.Free;
    for LCount := 1 to 10 do
      FDemandPortionValues[LCount].Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandChannel.Initialise: boolean;
const OPNAME = 'TDemandChannel.Initialise';
var
  LCount : Integer;
begin
  Result := False;
  try
    FChannelNumber.FData := 0;
    FChannelNumber.FInitalised := False;
    FChannelNumber.FLength := 6;
    FChannelNumber.FDecimal := 0;
    FChannelNumber.FDefaultPadding := True;

    FUserType.FData := 0;
    FUserType.FInitalised := False;
    FUserType.FLength := 6;
    FUserType.FDecimal := 0;
    FUserType.FDefaultPadding := True;

    for LCount := 1 to 10 do
    begin
      FDemandPortionValues[LCount].FData := 0;
      FDemandPortionValues[LCount].FInitalised := False;
      FDemandPortionValues[LCount].FLength := 8;
      FDemandPortionValues[LCount].FDecimal := 2;
      FDemandPortionValues[LCount].FDefaultPadding := False;
      FDemandPortionValues[LCount].ShowDecimalPoint := True;
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDemandChannel.Reset;
const OPNAME = 'TDemandChannel.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TDemandReconciliationDataObject }

function TDemandReconciliationDataObject.AddDemandChannel: TDemandChannel;
const OPNAME = 'TDemandReconciliationDataObject.AddDemandChannel';
begin
  Result := nil;
  try
    Result := TDemandChannel.Create;
    FDemandChannelsList.Add(Result)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandReconciliationDataObject.AddDemandPortion: TDemandPortion;
const OPNAME = 'TDemandReconciliationDataObject.AddDemandPortion';
begin
  Result := nil;
  try
    Result := TDemandPortion.Create;
    FDemandPortionList.Add(Result)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDemandReconciliationDataObject.CreateMemberObjects;
const OPNAME = 'TDemandReconciliationDataObject.CreateMemberObjects';
begin
  try
    FDemandReconciliationData := TDemandReconciliationData.Create;
    FDemandPortionList        := TObjectList.Create(True);
    FDemandChannelsList       := TObjectList.Create(True);
    FF16ExtraLines            := TStringList.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDemandReconciliationDataObject.DestroyMemberObjects;
const OPNAME = 'TDemandReconciliationDataObject.DestroyMemberObjects';
begin
  try
    FDemandReconciliationData.Free;
    FDemandPortionList.Free;
    FDemandChannelsList.Free;
    FF16ExtraLines.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandReconciliationDataObject.DemandChannelByIndex(AIndex: integer): TDemandChannel;
const OPNAME = 'TDemandReconciliationDataObject.DemandChannelByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FDemandChannelsList.Count) then
      Result := TDemandChannel(FDemandChannelsList[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandReconciliationDataObject.DemandPortionByIndex(AIndex: integer): TDemandPortion;
const OPNAME = 'TDemandReconciliationDataObject.DemandPortionByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FDemandPortionList.Count) then
      Result := TDemandPortion(FDemandPortionList[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;

end;

function TDemandReconciliationDataObject.Initialise: boolean;
const OPNAME = 'TDemandReconciliationDataObject.Initialise';
begin
  Result := False;
  try
    FDemandReconciliationData.Initialise;
    FDemandPortionList.Clear;
    FDemandChannelsList.Clear;
    FF16ExtraLines.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDemandReconciliationDataObject.Reset;
const OPNAME = 'TDemandReconciliationDataObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
