//
//
//  UNIT      : Contains TReturnFlowFileObject Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 07/06/2006
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UReturnFlowChannelFileDataObjects;

interface
uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UAbstractDataObject,
  UBasicObjects,
  UConstants;
type
  TAssumedFactor = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
   //Line 4...........................
    FChannelNumber : TInteger;
    FAbstractionChannel : TInteger;
    FFactor : TDouble;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TReturnFlow = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    //Line 2.....................
    FDemandChannel : TInteger;
    FNumOfCorrespondingChannels : TInteger;
    FGaugeNumber : TInteger;
    FMonthlyAvrgFactor : TDouble;
    FCalibrationFactor : TDouble;
    FMonthlyAvrgNetEvap : TDouble;
    FRoutingConstant : TDouble;
    FCurtailmentFactor : TDouble;
    FMultiplicationFactor : TDouble;
    //Line 3...........................
    FMonthlyPotentialEvap : array[MinMonths..MaxMonths] of TDouble;
    //Line 4....................................................................
    FAssumedFactorList : TObjectList;
    procedure Reset;override;
    function Initialise: boolean;override;
    function AssumedFactorCount : integer;
    function AddAssumedFactor : TAssumedFactor;
  end;

  TReturnFlowChannelFileObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FExtraLines : TStringList;
//    FIdentifier : TInteger;
    //Line 1....................................................................
    FReturnFlowCount : TInteger;
    // Line 2,3
    FReturnFlowList  : TObjectList;
    procedure Reset;override;
    function Initialise: boolean;override;
    function AddReturnFlow : TReturnFlow;
  end;

implementation

uses UErrorHandlingOperations;

{ TReturnFlowChannelFileObject }

function TReturnFlowChannelFileObject.AddReturnFlow : TReturnFlow;
const OPNAME = 'TReturnFlowChannelFileObject.AddReturnFlow';
begin
  Result := nil;
  try
    Result := TReturnFlow.Create;
    FReturnFlowList.Add(Result);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TReturnFlowChannelFileObject.CreateMemberObjects;
const OPNAME = 'TReturnFlowChannelFileObject.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FReturnFlowList := TObjectList.Create;
    FExtraLines := TStringList.Create;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelFileObject.DestroyMemberObjects;
const OPNAME = 'TReturnFlowChannelFileObject.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
    FreeAndNil(FReturnFlowList);
    FreeAndNil(FExtraLines);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannelFileObject.Initialise: boolean;
const OPNAME = 'TReturnFlowChannelFileObject.Initialise';
begin
  Result := False;
  try
  
    FReturnFlowCount := TInteger.Create;
    FReturnFlowCount.FData := 0;
    FReturnFlowCount.FInitalised := False;
    FReturnFlowCount.FLength := 6;
    FReturnFlowCount.FDecimal := 0;
    FReturnFlowCount.FDefaultPadding := True;

    FReturnFlowList.Clear;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelFileObject.Reset;
const OPNAME = 'TReturnFlowChannelFileObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TAssumedFactor }

procedure TAssumedFactor.CreateMemberObjects;
const OPNAME = 'TAssumedFactor.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TAssumedFactor.DestroyMemberObjects;
const OPNAME = 'TAssumedFactor.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAssumedFactor.Initialise: boolean;
const OPNAME = 'TAssumedFactor.Initialise';
begin
  Result := False;
  try
    FChannelNumber := TInteger.Create;
    FChannelNumber.FData := 0;
    FChannelNumber.FInitalised := False;
    FChannelNumber.FLength := 5;
    FChannelNumber.FDecimal := 0;
    FChannelNumber.FDefaultPadding := True;

    FAbstractionChannel := TInteger.Create;
    FAbstractionChannel.FData := 0;
    FAbstractionChannel.FInitalised := False;
    FAbstractionChannel.FLength := 5;
    FAbstractionChannel.FDecimal := 0;
    FAbstractionChannel.FDefaultPadding := True;

    FFactor := TDouble.Create;
    FFactor.FData := 0;
    FFactor.FInitalised := False;
    FFactor.FLength := 8;
    FFactor.FDecimal := 4;
    FFactor.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TAssumedFactor.Reset;
const OPNAME = 'TAssumedFactor.Reset';
begin
  try
    Initialise
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TReturnFlow }

procedure TReturnFlow.CreateMemberObjects;
const OPNAME = 'TReturnFlow.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FAssumedFactorList := TObjectList.Create;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlow.DestroyMemberObjects;
const OPNAME = 'TReturnFlow.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
    FreeAndNil(FAssumedFactorList);

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlow.Initialise: boolean;
const OPNAME = 'TReturnFlow.Initialise';
var
  LIndex : integer;
begin
  Result := False;
  try
    //Line 2.....................
    FDemandChannel := TInteger.Create;
    FDemandChannel.FData := 0;
    FDemandChannel.FInitalised := False;
    FDemandChannel.FLength := 5;
    FDemandChannel.FDecimal := 0;
    FDemandChannel.FDefaultPadding := True;

    FNumOfCorrespondingChannels := TInteger.Create;
    FNumOfCorrespondingChannels.FData := 0;
    FNumOfCorrespondingChannels.FInitalised := False;
    FNumOfCorrespondingChannels.FLength := 5;
    FNumOfCorrespondingChannels.FDecimal := 0;
    FNumOfCorrespondingChannels.FDefaultPadding := True;

    FGaugeNumber := TInteger.Create;
    FGaugeNumber.FData := 0;
    FGaugeNumber.FInitalised := False;
    FGaugeNumber.FLength := 5;
    FGaugeNumber.FDecimal := 0;
    FGaugeNumber.FDefaultPadding := True;

    FMonthlyAvrgFactor := TDouble.Create;
    FMonthlyAvrgFactor.FData := 0;
    FMonthlyAvrgFactor.FInitalised := False;
    FMonthlyAvrgFactor.FLength := 8;
    FMonthlyAvrgFactor.FDecimal := 5;
    FMonthlyAvrgFactor.FDefaultPadding := True;

    FCalibrationFactor := TDouble.Create;
    FCalibrationFactor.FData := 0;
    FCalibrationFactor.FInitalised := False;
    FCalibrationFactor.FLength := 8;
    FCalibrationFactor.FDecimal := 5;
    FCalibrationFactor.FDefaultPadding := True;

    FMonthlyAvrgNetEvap := TDouble.Create;
    FMonthlyAvrgNetEvap.FData := 0;
    FMonthlyAvrgNetEvap.FInitalised := False;
    FMonthlyAvrgNetEvap.FLength := 8;
    FMonthlyAvrgNetEvap.FDecimal := 5;
    FMonthlyAvrgNetEvap.FDefaultPadding := True;

    FRoutingConstant := TDouble.Create;
    FRoutingConstant.FData := 0;
    FRoutingConstant.FInitalised := False;
    FRoutingConstant.FLength := 5;
    FRoutingConstant.FDecimal := 1;
    FRoutingConstant.FDefaultPadding := True;

    FCurtailmentFactor := TDouble.Create;
    FCurtailmentFactor.FData := 0;
    FCurtailmentFactor.FInitalised := False;
    FCurtailmentFactor.FLength := 5;
    FCurtailmentFactor.FDecimal := 1;
    FCurtailmentFactor.FDefaultPadding := True;

    FMultiplicationFactor := TDouble.Create;
    FMultiplicationFactor.FData := 0;
    FMultiplicationFactor.FInitalised := False;
    FMultiplicationFactor.FLength := 6;
    FMultiplicationFactor.FDecimal := 4;
    FMultiplicationFactor.FDefaultPadding := True;
    FMultiplicationFactor.ShowDecimalPoint := True;
    //Line 3...........................
    for LIndex := MinMonths to MaxMonths do
    begin
      FMonthlyPotentialEvap[LIndex] := TDouble.Create;
      FMonthlyPotentialEvap[LIndex].FData := 0;
      FMonthlyPotentialEvap[LIndex].FInitalised := False;
      FMonthlyPotentialEvap[LIndex].FLength := 5;
      FMonthlyPotentialEvap[LIndex].FDecimal := 0;
      FMonthlyPotentialEvap[LIndex].FDefaultPadding := False;
      FMonthlyPotentialEvap[LIndex].ShowDecimalPoint := True;
    end;
    //Line 4...........................
    FAssumedFactorList.clear;
    
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlow.AddAssumedFactor: TAssumedFactor;
const OPNAME = 'TReturnFlow.AddAssumedFactor';
begin
  Result := nil;
  try
    Result := TAssumedFactor.Create;
    FAssumedFactorList.Add(Result);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TReturnFlow.AssumedFactorCount: integer;
const OPNAME = 'TReturnFlow.AssumedFactorCount';
begin
  Result := 0;
  try
    Result := FAssumedFactorList.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TReturnFlow.Reset;
const OPNAME = 'TReturnFlow.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
