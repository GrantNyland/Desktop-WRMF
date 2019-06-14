//
//
//  UNIT      : Contains TMinFlowChannelObject Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 05/08/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UMinFlowChannelObject;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UAbstractDataObject,
  UBasicObjects,
  UConstants;

type


  TMinFlowChannel = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FMinFlowChannelName: TString;
    FMinFlowChannelNumber :TInteger;
    FMinFlowValues : array[MinMinFlow..MaxMinFlow] of TDouble;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TLossChannel = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FLossChannelName: TString;
    FLossChannelNumber :TInteger;
    FLossValues : array[MinLoss..MaxLoss] of TDouble;
    FDivertedValues : array[MinDiverted..MaxDiverted] of TDouble;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TMinFlowAndLossChannelObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FMinFlowChannelsLines : TObjectList;
    FLossChannelsLines : TObjectList;
    FF11ExtraLines: TStringList;
    procedure Reset;override;
    function Initialise: boolean;override;
    function GetMinFlowChannel(AChannelNumber: integer):TMinFlowChannel;
    function GetLossChannel(AChannelNumber: integer):TLossChannel;
  end;

implementation


uses UErrorHandlingOperations;

{TMinFlowChannel}

procedure TMinFlowChannel.CreateMemberObjects;
const OPNAME = 'TMinFlowChannel.CreateMemberObjects';
var
  LCount: integer;
  LDouble : TDouble;
Begin
  try
    FMinFlowChannelNumber := TInteger.Create;
    FMinFlowChannelName  := TString.Create;

    for LCount := MinMinFlow to MaxMinFlow do
    begin
      LDouble := TDouble.Create;
      FMinFlowValues[LCount] := LDouble;
    end;

    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinFlowChannel.DestroyMemberObjects;
const OPNAME = 'TMinFlowChannel.DestroyMemberObjects';
var
  LCount : Integer;
Begin
  try
    FMinFlowChannelNumber.Free;
    FMinFlowChannelName.Free;

    for LCount := MinMinFlow to MaxMinFlow do
      FMinFlowValues[LCount].Free;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinFlowChannel.Reset;
const OPNAME = 'TMinFlowChannel.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinFlowChannel.Initialise: boolean;
const OPNAME = 'TMinFlowChannel.Initialise';
var
  LCount : Integer;
Begin
  Result := False;
  try
    //Data and initialised.
    FMinFlowChannelNumber.FData := 0;
    FMinFlowChannelNumber.FInitalised := False;
    FMinFlowChannelNumber.FLength := 6;
    FMinFlowChannelNumber.FDecimal := 0;
    FMinFlowChannelNumber.FDefaultPadding := True;

    FMinFlowChannelName.FData := '';
    FMinFlowChannelName.FInitalised := False;
    FMinFlowChannelName.FLength := 36;
    FMinFlowChannelName.FDecimal := 0;
    FMinFlowChannelName.FDefaultPadding := True;

    for LCount := MinMinFlow to MaxMinFlow do
    begin
      FMinFlowValues[LCount].FData := 0;
      FMinFlowValues[LCount].FInitalised := False;
      FMinFlowValues[LCount].FLength := 6;
      FMinFlowValues[LCount].FDecimal := 3;
      FMinFlowValues[LCount].FDefaultPadding := True;
      FMinFlowValues[LCount].ShowDecimalPoint := True;
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TMinFlowAndLossChannelObject}

procedure TMinFlowAndLossChannelObject.CreateMemberObjects;
const OPNAME = 'TMinFlowAndLossChannelObject.CreateMemberObjects';
begin
  try
    //File F11.dat
    FMinFlowChannelsLines := TObjectList.Create(True);
    FLossChannelsLines := TObjectList.Create(True);
    FF11ExtraLines   := TStringList.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinFlowAndLossChannelObject.DestroyMemberObjects;
const OPNAME = 'TMinFlowAndLossChannelObject.DestroyMemberObjects';
begin
  try
    //File F06.dat
    FMinFlowChannelsLines.Free;
    FLossChannelsLines.Free;
    FF11ExtraLines.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinFlowAndLossChannelObject.GetLossChannel(AChannelNumber: integer): TLossChannel;
const OPNAME = 'TMinFlowAndLossChannelObject.GetLossChannel';
var
  LCount: integer;
begin
  Result := nil;
  try
     for LCount := 0 to FLossChannelsLines.Count - 1 do
     begin
       if(TLossChannel(FLossChannelsLines[LCount]).FLossChannelNumber.FData = AChannelNumber) then
       begin
         Result := TLossChannel(FLossChannelsLines[LCount]);
         Break;
       end;
     end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinFlowAndLossChannelObject.GetMinFlowChannel(AChannelNumber: integer): TMinFlowChannel;
const OPNAME = 'TMinFlowAndLossChannelObject.GetMinFlowChannel';
var
  LCount: integer;
begin
  Result := nil;
  try
     for LCount := 0 to FMinFlowChannelsLines.Count - 1 do
     begin
       if(TMinFlowChannel(FMinFlowChannelsLines[LCount]).FMinFlowChannelNumber.FData = AChannelNumber) then
       begin
         Result := TMinFlowChannel(FMinFlowChannelsLines[LCount]);
         Break;
       end;
     end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinFlowAndLossChannelObject.Initialise: boolean;
const OPNAME = 'TMinFlowAndLossChannelObject.Initialise';
Begin
  Result := False;
  try
    FMinFlowChannelsLines.Clear;
    FLossChannelsLines.Clear;
    FF11ExtraLines.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinFlowAndLossChannelObject.Reset;
const OPNAME = 'TMinFlowAndLossChannelObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{TLossChannel}

procedure TLossChannel.CreateMemberObjects;
const OPNAME = 'TLossChannel.CreateMemberObjects';
var
  LCount: integer;
  LDouble : TDouble;
Begin
  try
    FLossChannelNumber := TInteger.Create;
    FLossChannelName  := TString.Create;

    for LCount := MinLoss to MaxLoss do
    begin
      LDouble := TDouble.Create;
      FLossValues[LCount] := LDouble;
    end;

    for LCount := MinDiverted to MaxDiverted do
    begin
      LDouble := TDouble.Create;
      FDivertedValues[LCount] := LDouble;
    end;

    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLossChannel.DestroyMemberObjects;
const OPNAME = 'TLossChannel.DestroyMemberObjects';
var
  LCount : Integer;
Begin
  try
    FLossChannelNumber.Free;
    FLossChannelName.Free;

    for LCount := MinLoss to MaxLoss do
      FLossValues[LCount].Free;

    for LCount := MinDiverted to MaxDiverted do
      FDivertedValues[LCount].Free;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLossChannel.Reset;
const OPNAME = 'TLossChannel.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLossChannel.Initialise: boolean;
const OPNAME = 'TLossChannel.Initialise';
var
  LCount : Integer;
Begin
  Result := False;
  try
    //Data and initialised.
    FLossChannelNumber.FData := 0;
    FLossChannelNumber.FInitalised := False;
    FLossChannelNumber.FLength := 6;
    FLossChannelNumber.FDecimal := 0;
    FLossChannelNumber.FDefaultPadding := True;

    FLossChannelName.FData := '';
    FLossChannelName.FInitalised := False;
    FLossChannelName.FLength := 36;
    FLossChannelName.FDecimal := 0;
    FLossChannelName.FDefaultPadding := True;

    for LCount := MinLoss to MaxLoss do
    begin
      FLossValues[LCount].FData := 0;
      FLossValues[LCount].FInitalised := False;
      FLossValues[LCount].FLength := 6;
      FLossValues[LCount].FDecimal := 3;
      FLossValues[LCount].FDefaultPadding := True;
      FLossValues[LCount].ShowDecimalPoint := True;
    end;

    for LCount := MinDiverted to MaxDiverted do
    begin
      FDivertedValues[LCount].FData := 0;
      FDivertedValues[LCount].FInitalised := False;
      FDivertedValues[LCount].FLength := 6;
      FDivertedValues[LCount].FDecimal := 3;
      FDivertedValues[LCount].FDefaultPadding := True;
      FDivertedValues[LCount].ShowDecimalPoint := True;
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
