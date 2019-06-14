//
//
//  UNIT      : Contains TChannelMinMaxObject Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 07/08/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UChannelMinMaxObject;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UAbstractDataObject,
  UBasicObjects,
  UConstants;

type

  TMonthlyFlowConstraints  = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FFlowConstraintsValues : array[MinFlowConstraints..MaxFlowConstraints] of TDouble;
    FComment : TString;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TChannelMinMax = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FChannelMinMaxName: TString;
    FChannelMinMaxNumber :TInteger;
    FMonthlyFlowConstraintsContainer : TObjectList;
    FComment : TString;
    procedure Reset;override;
    function Initialise: boolean;override;
    function AddMonthlyFlowConstraints:TMonthlyFlowConstraints;
  end;

  TChannelMinMaxObject = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FChannelMinMaxContainer : TObjectList;
    FF12ExtraLines: TStringList;
    procedure Reset;override;
    function Initialise: boolean;override;
    function GetMinMaxChannel(AChannelNumber: integer):TChannelMinMax;
  end;

implementation


uses UErrorHandlingOperations;

{TChannelMinMax}

procedure TChannelMinMax.CreateMemberObjects;
const OPNAME = 'TChannelMinMax.CreateMemberObjects';
Begin
  try
    FChannelMinMaxNumber := TInteger.Create;
    FChannelMinMaxName   := TString.Create;
    FComment             := TString.Create;

    FMonthlyFlowConstraintsContainer := TObjectList.Create(True);

    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelMinMax.DestroyMemberObjects;
const OPNAME = 'TChannelMinMax.DestroyMemberObjects';
Begin
  try
    FChannelMinMaxNumber.Free;
    FChannelMinMaxName.Free;
    FComment.Free;
    FMonthlyFlowConstraintsContainer.Free;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelMinMax.Reset;
const OPNAME = 'TChannelMinMax.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelMinMax.Initialise: boolean;
const OPNAME = 'TChannelMinMax.Initialise';
Begin
  Result := False;
  try
    //Data and initialised.
    FChannelMinMaxNumber.FData := 0;
    FChannelMinMaxNumber.FInitalised := False;
    FChannelMinMaxNumber.FLength := 6;
    FChannelMinMaxNumber.FDecimal := 0;
    FChannelMinMaxNumber.FDefaultPadding := True;

    FChannelMinMaxName.FData := '';
    FChannelMinMaxName.FInitalised := False;
    FChannelMinMaxName.FLength := 36;
    FChannelMinMaxName.FDecimal := 0;
    FChannelMinMaxName.FDefaultPadding := True;

    FComment.FData := '';
    FComment.FInitalised := False;
    FComment.FLength := 0;
    FComment.FDecimal := 0;
    FComment.FDefaultPadding := False;

    FMonthlyFlowConstraintsContainer.Clear;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelMinMax.AddMonthlyFlowConstraints: TMonthlyFlowConstraints;
const OPNAME = 'TChannelMinMax.AddMonthlyFlowConstraints';
begin
  Result := nil;
  try
     Result := TMonthlyFlowConstraints.Create;
     Result.Initialise;
     FMonthlyFlowConstraintsContainer.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TChannelMinMaxObject}

procedure TChannelMinMaxObject.CreateMemberObjects;
const OPNAME = 'TChannelMinMaxObject.CreateMemberObjects';
begin
  try
    //File F12.dat
    FChannelMinMaxContainer := TObjectList.Create(True);
    FF12ExtraLines      := TStringList.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelMinMaxObject.DestroyMemberObjects;
const OPNAME = 'TChannelMinMaxObject.DestroyMemberObjects';
begin
  try
    //File F12.dat
    FChannelMinMaxContainer.Free;
    FF12ExtraLines.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelMinMaxObject.Initialise: boolean;
const OPNAME = 'TChannelMinMaxObject.Initialise';
Begin
  Result := False;
  try
    FChannelMinMaxContainer.Clear;
    FF12ExtraLines.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelMinMaxObject.GetMinMaxChannel(AChannelNumber: integer): TChannelMinMax;
const OPNAME = 'TChannelMinMaxObject.GetMinMaxChannel';
var
  LCount: integer;
begin
  Result := nil;
  try
     for LCount := 0 to FChannelMinMaxContainer.Count - 1 do
     begin
       if(TChannelMinMax(FChannelMinMaxContainer[LCount]).FChannelMinMaxNumber.FData = AChannelNumber) then
       begin
         Result := TChannelMinMax(FChannelMinMaxContainer[LCount]);
         Break;
       end;
     end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelMinMaxObject.Reset;
const OPNAME = 'TChannelMinMaxObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TMonthlyFlowConstraints }

procedure TMonthlyFlowConstraints.CreateMemberObjects;
const OPNAME = 'TMonthlyFlowConstraints.CreateMemberObjects';
var
  LCount: integer;
  LDouble : TDouble;
Begin
  try
    for LCount := MinFlowConstraints to MaxFlowConstraints do
    begin
      LDouble := TDouble.Create;
      FFlowConstraintsValues[LCount] := LDouble;
    end;
    FComment  := TString.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMonthlyFlowConstraints.DestroyMemberObjects;
const OPNAME = 'TMonthlyFlowConstraints.DestroyMemberObjects';
var
  LCount: integer;
Begin
  try
    FComment.Free;
    for LCount := MinFlowConstraints to MaxFlowConstraints do
    begin
      FFlowConstraintsValues[LCount].Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMonthlyFlowConstraints.Initialise: boolean;
const OPNAME = 'TMonthlyFlowConstraints.Initialise';
var
  LCount: integer;
begin
  Result := False;
  try
    FComment.FData := '';
    FComment.FInitalised := False;
    FComment.FLength := 0;
    FComment.FDecimal := 0;
    FComment.FDefaultPadding := False;
    for LCount := MinFlowConstraints to MaxFlowConstraints do
    begin
      FFlowConstraintsValues[LCount].FData := 0;
      FFlowConstraintsValues[LCount].FInitalised := False;
      FFlowConstraintsValues[LCount].FLength := 6;
      FFlowConstraintsValues[LCount].FDecimal := 3;
      FFlowConstraintsValues[LCount].FDefaultPadding := True;
      FFlowConstraintsValues[LCount].ShowDecimalPoint := True;
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMonthlyFlowConstraints.Reset;
const OPNAME = 'TMonthlyFlowConstraints.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
