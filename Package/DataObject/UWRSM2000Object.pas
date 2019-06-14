//
//
//  UNIT      : Contains TDWSObject Class
//  AUTHOR    : Titi Ngubane(PDNA)
//  DATE      : 24/04/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UWRSM2000Object;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UAbstractDataObject,
  UBasicObjects,
  UConstants;

type
   TSubModules = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    //Line 7..14
    FShape        : TInteger;
    FSubModuleType      : TString;
    FYChar         : TString;

    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TFlowGaugesDetails = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    //Line 16
    FRoute                 : TInteger;
    FFlowGauge             : TString;
    //Line 17
    FFileName              : TString;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TRouteDetails = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    //Line 20....
    FRouteNumber           : TInteger;
    FFromSubModuleNumber   : TInteger;
    FToSubModuleNumber     : TInteger;
    FZeroValue             : TInteger;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TWRSM2000Object = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    //File DWS.NET
    //Line 1 : Project Identifier
    FProjectNumber               : TString;
    FNumber                      : Tinteger;
   // FF4Line1ExtraChars       :TString;
    //Line 2..3
    FDirectory                   : Array[1..2] Of TString;

    //Line 4
    FChar1                       : TString;

    //Line 5
    FChar2                       : TString;

    //Line 6
    FSubModuleCount              : TInteger;

    //Line 7..14 : Sub Module data
    FSubModules                   : TObjectList;

    //Line 15
    FFlowGaugesCount             : TInteger;
    // line 16..17
    FFlowGaugesDetails           : TObjectList;

    //Line 18
    FYear1                       : TInteger;
    FYear2                       : TInteger;

    //Line 19
    FRoutesCount                 : TInteger;

    //Line 20... : Routes data
    FRouteDetails                : TObjectList;

    //Line 21..final zero
    FLastZero                 : TInteger;

    FWRSMExtraLines              : TStringList;
    function AddSubModules       : boolean;
    function AddFlowGaugesDetails: boolean;
    function AddRouteDetails     : boolean;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;


implementation


{ TWRSM2000Object }
uses UErrorHandlingOperations;

procedure TWRSM2000Object.CreateMemberObjects;
const OPNAME = 'TWRSM2000Object.CreateMemberObjects';
var
  LCount: integer;
  LString:TString;
begin
  try
    //File DWS.NET
    FProjectNumber               := TString.Create;
    FNumber                      := TInteger.Create;
    for LCount := 1 to 2 do
    begin
      LString                    := TString.Create;
      FDirectory[LCount]         := LString;
    end;
    FChar1                       := TString.Create;
    FChar2                       := TString.Create;
    FSubModuleCount              := TInteger.Create;
    FYear1                       := TInteger.Create;
    FYear2                       := TInteger.Create;
    FRoutesCount                 := TInteger.Create;
    FFlowGaugesCount             := TInteger.Create;
    FLastZero                    := TInteger.Create;
    FSubModules                  := TObjectList.Create(True);
    FFlowGaugesDetails           := TObjectList.Create(True);
    FRouteDetails                := TObjectList.Create(True);
    FWRSMExtraLines              := TStringList.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRSM2000Object.DestroyMemberObjects;
const OPNAME = 'TWRSM2000Object.DestroyMemberObjects';
var
  LCount: integer;
begin
  try
    //File DWS.dat
    FProjectNumber.Free;
    FNumber.Free;
    for LCount := 1 to 2 do
    begin
      FDirectory[LCount].Free;
    end;
    FChar1.Free;
    FChar2.Free;
    FSubModuleCount.Free;
    FYear1.Free;
    FYear2.Free;
    FRoutesCount.Free;
    FFlowGaugesCount.Free;
    FSubModules.Free;
    FFlowGaugesDetails.Free;
    FRouteDetails.Free;
    FLastZero.Free;
    FWRSMExtraLines.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRSM2000Object.Initialise: boolean;
const OPNAME = 'TWRSM2000Object.Initialise';
var
  LCount : Integer;
Begin
  Result := False;
  try
    FProjectNumber.FData := '';
    FProjectNumber.FInitalised := False;
    FProjectNumber.FLength := 3;
    FProjectNumber.FDecimal := 0;
    FProjectNumber.FDefaultPadding := True;

    FNumber.FData := 0;
    FNumber.FInitalised := False;
    FNumber.FLength := 3;
    FNumber.FDecimal := 0;
    FNumber.FDefaultPadding := True;

    for LCount := 1 to 2 do
    begin
      FDirectory[LCount].FData := '';
      FDirectory[LCount].FInitalised := False;
      FDirectory[LCount].FLength  := 87;
      FDirectory[LCount].FDecimal := 0;
      FDirectory[LCount].FDefaultPadding := True;
    end;

    FChar1.FData := ' ';
    FChar1.FInitalised := False;
    FChar1.FLength := 1;
    FChar1.FDecimal := 0;
    FChar1.FDefaultPadding := False;

    FChar2.FData := ' ';
    FChar2.FInitalised := False;
    FChar2.FLength := 1;
    FChar2.FDecimal := 0;
    FChar2.FDefaultPadding := False;

    FSubModuleCount.FData := 0;
    FSubModuleCount.FInitalised := False;
    FSubModuleCount.FLength := 4;
    FSubModuleCount.FDecimal := 0;
    FSubModuleCount.FDefaultPadding := True;

    FYear1.FData := 0;
    FYear1.FInitalised := False;
    FYear1.FLength := 4;
    FYear1.FDecimal := 0;
    FYear1.FDefaultPadding := True;

    FYear2.FData := 0;
    FYear2.FInitalised := False;
    FYear2.FLength := 5;
    FYear2.FDecimal := 0;
    FYear2.FDefaultPadding := True;

    FRoutesCount.FData := 0;
    FRoutesCount.FInitalised := False;
    FRoutesCount.FLength := 4;
    FRoutesCount.FDecimal := 0;
    FRoutesCount.FDefaultPadding := True;

    {FComment.FData := '';
    FComment.FInitalised := False;
    FComment.FDecimal := 0;
    FComment.FDefaultPadding := True;

    FZeroComment.FData := '';
    FZeroComment.FInitalised := False;
    FZeroComment.FDecimal := 0;
    FZeroComment.FDefaultPadding := False;}

    FFlowGaugesCount.FData := 0;
    FFlowGaugesCount.FInitalised := False;
    FFlowGaugesCount.FLength := 4;
    FFlowGaugesCount.FDecimal := 0;
    FFlowGaugesCount.FDefaultPadding := True;

    FLastZero.FData := 0;
    FLastZero.FInitalised := False;
    FLastZero.FLength := 5;
    FLastZero.FDecimal := 0;
    FLastZero.FDefaultPadding := True;

    FSubModules.Clear;
    FFlowGaugesDetails.Clear;
    FRouteDetails.Clear;
    FWRSMExtraLines.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRSM2000Object.AddSubModules: boolean;
const OPNAME = 'TWRSM2000Object.AddSubModules';
var
  LCount       : Integer;
  LSubModule   :TSubModules;
Begin
  Result := False;
  try
    FSubModules.Clear;
    for LCount := 0 to FSubModuleCount.FData do
    begin
      LSubModule := TSubModules.Create;
      FSubModules.Add(LSubModule);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRSM2000Object.AddFlowGaugesDetails: boolean;
const OPNAME = 'TWRSM2000Object.AddFlowGaugesDetails';
var
  LFlowGaugesDetails :TFlowGaugesDetails;
  LCount : Integer;
Begin
  Result := False;
  try
    FFlowGaugesDetails.Clear;
    for LCount := 0 to FFlowGaugesCount.FData do
    begin
      LFlowGaugesDetails := TFlowGaugesDetails.Create;
      FFlowGaugesDetails.Add(LFlowGaugesDetails);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRSM2000Object.AddRouteDetails: boolean;
const OPNAME = 'TWRSM2000Object.AddRouteDetails';
var
  LCount : Integer;
  LRouteDetails :TRouteDetails;
Begin
  Result := False;
  try
    FRouteDetails.Clear;
    for LCount := 0 to FRoutesCount.FData do
    begin
      LRouteDetails := TRouteDetails.Create;
      FRouteDetails.Add(LRouteDetails);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSubModules.CreateMemberObjects;
const OPNAME = 'TSubModules.CreateMemberObjects';
Begin
  try
    FShape           := TInteger.Create;
    FSubModuleType         := TString.Create;
    FYChar           := TString.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSubModules.DestroyMemberObjects;
const OPNAME = 'TSubModules.DestroyMemberObjects';
Begin
  try
    FShape.Free;
    FSubModuleType.Free;
    FYChar.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSubModules.Initialise: boolean;
const OPNAME = 'TSubModules.Initialise';
Begin
  Result := False;
  try
    //Data and initialised.
    FShape.FData := 0;
    FShape.FInitalised := False;
    FShape.FLength := 1;
    FShape.FDecimal := 0;
    FShape.FDefaultPadding := True;

    FSubModuleType.FData := '';
    FSubModuleType.FInitalised := False;
    FSubModuleType.FLength := 2;
    FSubModuleType.FDecimal := 0;
    FSubModuleType.FDefaultPadding := True;

    FYChar.FData := '';
    FYChar.FInitalised := False;
    FYChar.FLength := 5;
    FYChar.FDecimal := 0;
    FYChar.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFlowGaugesDetails.CreateMemberObjects;
const OPNAME = 'TFlowGaugesDetails.CreateMemberObjects';
Begin
  try
    FRoute   := TInteger.Create;
    FFlowGauge     := TString.Create;
    FFileName      := TString.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFlowGaugesDetails.DestroyMemberObjects;
const OPNAME = 'TFlowGaugesDetails.DestroyMemberObjects';
Begin
  try
    FRoute.Free;
    FFlowGauge.Free;
    FFileName.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFlowGaugesDetails.Initialise: boolean;
const OPNAME = 'TFlowGaugesDetails.Initialise';
Begin
  Result := False;
  try
    //Data and initialised.
    FRoute.FData := 0;
    FRoute.FInitalised := False;
    FRoute.FLength := 4;
    FRoute.FDecimal := 0;
    FRoute.FDefaultPadding := True;

    FFlowGauge.FData := '';
    FFlowGauge.FInitalised := False;
    FFlowGauge.FLength := 23;
    FFlowGauge.FDecimal := 0;
    FFlowGauge.FDefaultPadding := True;

    FFileName.FData := '';
    FFileName.FInitalised := False;
    FFileName.FLength := 12;
    FFileName.FDecimal := 0;
    FFileName.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRouteDetails.CreateMemberObjects;
const OPNAME = 'TRouteDetails.CreateMemberObjects';
Begin
  try
    FRouteNumber           := TInteger.Create;
    FFromSubModuleNumber   := TInteger.Create;
    FToSubModuleNumber     := TInteger.Create;
    FZeroValue             := TInteger.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRouteDetails.DestroyMemberObjects;
const OPNAME = 'TRouteDetails.DestroyMemberObjects';
Begin
  try
    FRouteNumber.Free;
    FFromSubModuleNumber.Free;
    FToSubModuleNumber.Free;
    FZeroValue.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRouteDetails.Initialise: boolean;
const OPNAME = 'TRouteDetails.Initialise';
begin
  Result := False;
  try
    //Data and initialised.
    FRouteNumber.FData := 0;
    FRouteNumber.FInitalised := False;
    FRouteNumber.FLength := 6;
    FRouteNumber.FDecimal := 0;
    FRouteNumber.FDefaultPadding := True;

    FFromSubModuleNumber.FData := 0;
    FFromSubModuleNumber.FInitalised := False;
    FFromSubModuleNumber.FLength := 6;
    FFromSubModuleNumber.FDecimal := 0;
    FFromSubModuleNumber.FDefaultPadding := True;

    FToSubModuleNumber.FData := 0;
    FToSubModuleNumber.FInitalised := False;
    FToSubModuleNumber.FLength := 6;
    FToSubModuleNumber.FDecimal := 0;
    FToSubModuleNumber.FDefaultPadding := True;

    FZeroValue.FData := 0;
    FZeroValue.FInitalised := False;
    FZeroValue.FLength := 6;
    FZeroValue.FDecimal := 0;
    FZeroValue.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSubModules.Reset;
const OPNAME = 'TSubModules.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFlowGaugesDetails.Reset;
const OPNAME = 'TFlowGaugesDetails.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRouteDetails.Reset;
const OPNAME = 'TRouteDetails.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRSM2000Object.Reset;
const OPNAME = 'TWRSM2000Object.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
