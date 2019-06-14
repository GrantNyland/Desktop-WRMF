//  UNIT      : Contains TYMDemandCentre Class
//  AUTHOR    : Maurice Marinus
//  DATE      : 25/10/2006
//  COPYRIGHT : Copyright © 2006 DWAF

unit UYMDemandCentreObject;

interface

uses
  Classes,
  Sysutils,
  Contnrs,
  UAbstractDataObject,
  UBasicObjects,
  UConstants;

type
  {TYMDemandCentreReclaimationChannel = class(TAbstractDataObject)
  protected
    FTotalReturnFlow  : TDouble;
    FFlowDiversion    : TDouble;
    FChannelNr        : TInteger;
    FIdentifier       : TInteger;
    FDemandCentreID   : TInteger;
    procedure SetChannelNr(const Value: TInteger);
    procedure SetFlowDiversion(const Value: TDouble);
    procedure SetIdentifier(const Value: TInteger);
    procedure SetTotalReturnFlow(const Value: TDouble);
    procedure SetDemandCentreID(const Value: TInteger);

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset; override;
    function Initialise: Boolean; override;

    property Identifier       : TInteger  read FIdentifier      write SetIdentifier;
    property DemandCentreID   : TInteger  read FDemandCentreID  write SetDemandCentreID;
    property ChannelNr        : TInteger  read FChannelNr       write SetChannelNr;
    property TotalReturnFlow  : TDouble   read FTotalReturnFlow write SetTotalReturnFlow;
    property FlowDiversion    : TDouble   read FFlowDiversion   write SetFlowDiversion;
  end; }

  TYMDemandCentreReturnFlowChannel = class(TAbstractDataObject)
  protected
    FTotalReturnFlow  : TDouble;
    FFlowDiversion    : TDouble;
    FChannelNr        : TInteger;
    FIdentifier       : TInteger;
    FDemandCentreID   : TInteger;
    procedure SetChannelNr(const Value: TInteger);
    procedure SetFlowDiversion(const Value: TDouble);
    procedure SetIdentifier(const Value: TInteger);
    procedure SetTotalReturnFlow(const Value: TDouble);
    procedure SetDemandCentreID(const Value: TInteger);

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset; override;
    function Initialise: Boolean; override;

    property Identifier       : TInteger  read FIdentifier      write SetIdentifier;
    property DemandCentreID   : TInteger  read FDemandCentreID  write SetDemandCentreID;
    property ChannelNr        : TInteger  read FChannelNr       write SetChannelNr;
    property TotalReturnFlow  : TDouble   read FTotalReturnFlow write SetTotalReturnFlow;
    property FlowDiversion    : TDouble   read FFlowDiversion   write SetFlowDiversion;
  end;

  TYMDemandCentre = class(TAbstractDataObject)
  protected
    FChannelCount           : Integer;
    FRainfallScalingFactor  : TDouble;
    FNodeRefNr              : TInteger;
    FRoutingConstant        : TDouble;
    FAveReturnFlowFactor    : TDouble;
    FStdDeviationFactor     : TDouble;
    FTotalFlowLost          : TDouble;
    FAveEvaporation         : TDouble;
    FIdentifier             : TInteger;
    FNodeNumber             : TInteger;
    FName                   : TString;
    FReclaimationChannelNr  : TInteger;
    FConsumptiveChannelNr   : TInteger;

    FReturnFlowChannel      : array[MinNoReturnFlowChannels..MaxNoReturnFlowChannels] of TYMDemandCentreReturnFlowChannel;
    FEvapoTranspiration     : array[MinMonths..MaxMonths] of TDouble;
    function GetReturnFlowChannel(AIndex: Integer): TYMDemandCentreReturnFlowChannel;
    function GetEvapoTranspiration(AIndex: Integer): TDouble;

    procedure SetAveEvaporation(const Value: TDouble);
    procedure SetAveReturnFlowFactor(const Value: TDouble);
    procedure SetEvapoTranspiration(AIndex: Integer; const Value: TDouble);
    procedure SetIdentifier(const Value: TInteger);
    procedure SetName(const Value: TString);
    procedure SetNodeNumber(const Value: TInteger);
    procedure SetNodeRefNr(const Value: TInteger);
    procedure SetRainfallScalingFactor(const Value: TDouble);
    procedure SetReturnFlowChannel(AIndex: Integer; const Value: TYMDemandCentreReturnFlowChannel);
    procedure SetRoutingConstant(const Value: TDouble);
    procedure SetStdDeviationFactor(const Value: TDouble);
    procedure SetTotalFlowLost(const Value: TDouble);
    procedure SetChannelCount(const Value: Integer);
    procedure SetConsumptiveChannelNr(const Value: TInteger);
    procedure SetReclaimationChannelNr(const Value: TInteger);

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset; override;
    function Initialise: Boolean; override;

    property Identifier             : TInteger  read FIdentifier            write SetIdentifier;
    property NodeNumber             : TInteger  read FNodeNumber            write SetNodeNumber;
    property Name                   : TString   read FName                  write SetName;
    property NodeRefNr              : TInteger  read FNodeRefNr             write SetNodeRefNr;
    property AveReturnFlowFactor    : TDouble   read FAveReturnFlowFactor   write SetAveReturnFlowFactor;
    property AveEvaporation         : TDouble   read FAveEvaporation        write SetAveEvaporation;
    property StdDeviationFactor     : TDouble   read FStdDeviationFactor    write SetStdDeviationFactor;
    property RoutingConstant        : TDouble   read FRoutingConstant       write SetRoutingConstant;
    property RainfallScalingFactor  : TDouble   read FRainfallScalingFactor write SetRainfallScalingFactor;
    property TotalFlowLost          : TDouble   read FTotalFlowLost         write SetTotalFlowLost;
    property ChannelCount           : Integer   read FChannelCount          write SetChannelCount;
    property ConsumptiveChannelNr   : TInteger  read FConsumptiveChannelNr  write SetConsumptiveChannelNr;
    property ReclaimationChannelNr  : TInteger  read FReclaimationChannelNr write SetReclaimationChannelNr;
    property EvapoTranspiration[AIndex: Integer]  : TDouble   read GetEvapoTranspiration write SetEvapoTranspiration;
    property ReturnFlowChannel[AIndex: Integer]   : TYMDemandCentreReturnFlowChannel read GetReturnFlowChannel write SetReturnFlowChannel;
  end;

  TYMDemandCentreObject = class(TAbstractDataObject)
  protected
    FExtraLines       : TStringList;
    FObjectContainer  : TObjectList;
    function GetDemandCentreByIndex(AIndex: Integer): TYMDemandCentre;
    procedure SetExtraLines(const Value: TStringList);
    procedure CreateMemberObjects;  override;
    procedure DestroyMemberObjects; override;
  public
    function DemandCentreCount  : Integer;
    function AddDemandCentre    : TYMDemandCentre;
    property DemandCentreObjectByIndex[AIndex: Integer] : TYMDemandCentre read GetDemandCentreByIndex;
    property ExtraLines         : TStringList read FExtraLines write SetExtraLines;
    procedure Reset;              override;
    function Initialise: Boolean; override;
  end;

implementation

uses
  UErrorHandlingOperations;


{ TYMDemandCentre }

procedure TYMDemandCentre.CreateMemberObjects;
const OPNAME = 'TYMDemandCentre.CreateMemberObjects';
var
  LCount : Integer;
begin
  try
    inherited CreateMemberObjects;

    FRainfallScalingFactor  := TDouble.Create;
    FNodeRefNr              := TInteger.Create;
    FRoutingConstant        := TDouble.Create;
    FAveReturnFlowFactor    := TDouble.Create;
    FStdDeviationFactor     := TDouble.Create;
    FTotalFlowLost          := TDouble.Create;
    FAveEvaporation         := TDouble.Create;
    FIdentifier             := TInteger.Create;
    FNodeNumber             := TInteger.Create;
    FName                   := TString.Create;
    FConsumptiveChannelNr   := TInteger.Create;
    FReclaimationChannelNr  := TInteger.Create;

    for LCount := MinNoReturnFlowChannels to MaxNoReturnFlowChannels do
      FReturnFlowChannel[LCount] := TYMDemandCentreReturnFlowChannel.Create;

    for LCount := MinMonths to MaxMonths do
      FEvapoTranspiration[LCount] := TDouble.Create;

    Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TYMDemandCentre.DestroyMemberObjects;
const OPNAME = 'TYMDemandCentre.DestroyMemberObjects';
var
  LCount: Integer;
begin
  try
    FRainfallScalingFactor.Free;
    FNodeRefNr.Free;
    FRoutingConstant.Free;
    FAveReturnFlowFactor.Free;
    FStdDeviationFactor.Free;
    FTotalFlowLost.Free;
    FAveEvaporation.Free;
    FIdentifier.Free;
    FNodeNumber.Free;
    FName.Free;
    FConsumptiveChannelNr.Free;
    FReclaimationChannelNr.Free;

    for LCount := MinNoReturnFlowChannels to MaxNoReturnFlowChannels do
      FreeAndNil(FReturnFlowChannel[LCount]);

    for LCount := MinMonths to MaxMonths do
      FreeAndNil(FEvapoTranspiration[LCount]);

    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentre.GetEvapoTranspiration(AIndex: Integer): TDouble;
const OPNAME = 'TYMDemandCentre.GetEvapoTranspiration';
begin
  Result := nil;
  try
    if (AIndex in [MinMonths..MaxMonths]) then
      Result :=  FEvapoTranspiration[AIndex];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TYMDemandCentre.GetReturnFlowChannel(AIndex: Integer): TYMDemandCentreReturnFlowChannel;
const OPNAME = 'TYMDemandCentre.GetReturnFlowChannel';
begin
  Result := nil;
  try
    if (AIndex in [MinNoReturnFlowChannels..MaxNoReturnFlowChannels]) then
      Result :=  FReturnFlowChannel[AIndex];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TYMDemandCentre.Initialise: Boolean;
const OPNAME = 'TYMDemandCentre.Initialise';
var
  LCount : Integer;
begin
  Result := False;
  try
    FIdentifier.FData           := 0;
    FIdentifier.FInitalised     := False;
    FIdentifier.FLength         := 3;
    FIdentifier.FDefaultPadding := True;

    //Line 1
    FNodeNumber.FData           := 0;
    FNodeNumber.FInitalised     := False;
    FNodeNumber.FLength         := 5;
    FNodeNumber.FDefaultPadding := True;

    FName.FData                 := '';
    FName.FInitalised           := False;
    FName.FLength               := 0;
    FName.FDefaultPadding       := True;

    // Line 2
    FNodeRefNr.FData            := 0;
    FNodeRefNr.FInitalised      := False;
    FNodeRefNr.FLength          := 5;
    FNodeRefNr.FDefaultPadding  := True;

    // Line 3
    FAveReturnFlowFactor.FData             := 0;
    FAveReturnFlowFactor.FInitalised       := False;
    FAveReturnFlowFactor.FLength           := 10;
    FAveReturnFlowFactor.FDefaultPadding   := True;
    FAveReturnFlowFactor.FDecimal          := 5;
    FAveReturnFlowFactor.ShowDecimalPoint  := True;

    FAveEvaporation.FData             := 0;
    FAveEvaporation.FInitalised       := False;
    FAveEvaporation.FLength           := 8;
    FAveEvaporation.FDefaultPadding   := True;
    FAveEvaporation.FDecimal          := 2;
    FAveEvaporation.ShowDecimalPoint  := True;

    FStdDeviationFactor.FData             := 0;
    FStdDeviationFactor.FInitalised       := False;
    FStdDeviationFactor.FLength           := 10;
    FStdDeviationFactor.FDefaultPadding   := True;
    FStdDeviationFactor.FDecimal          := 5;
    FStdDeviationFactor.ShowDecimalPoint  := True;

    FRoutingConstant.FData            := 0;
    FRoutingConstant.FInitalised      := False;
    FRoutingConstant.FLength          := 6;
    FRoutingConstant.FDefaultPadding  := True;
    FRoutingConstant.FDecimal         := 2;
    FRoutingConstant.ShowDecimalPoint := True;

    FRainfallScalingFactor.FData            := 0;
    FRainfallScalingFactor.FInitalised      := False;
    FRainfallScalingFactor.FLength          := 8;
    FRainfallScalingFactor.FDefaultPadding  := True;
    FRainfallScalingFactor.FDecimal         := 3;
    FRainfallScalingFactor.ShowDecimalPoint := True;

    FConsumptiveChannelNr.FData            := 0;
    FConsumptiveChannelNr.FInitalised      := False;
    FConsumptiveChannelNr.FLength          := 5;
    FConsumptiveChannelNr.FDefaultPadding  := True;

    FReclaimationChannelNr.FData            := 0;
    FReclaimationChannelNr.FInitalised      := False;
    FReclaimationChannelNr.FLength          := 5;
    FReclaimationChannelNr.FDefaultPadding  := True;

    // Line 4
    for LCount := MinNoReturnFlowChannels to MaxNoReturnFlowChannels do
      FReturnFlowChannel[LCount].Initialise;

    // Line 5
    FTotalFlowLost.FData             := 0;
    FTotalFlowLost.FInitalised       := False;
    FTotalFlowLost.FLength           := 10;
    FTotalFlowLost.FDefaultPadding   := True;
    FTotalFlowLost.FDecimal          := 5;
    FTotalFlowLost.ShowDecimalPoint  := True;

    // Line 6
    for LCount := MinMonths to MaxMonths do
    begin
      FEvapoTranspiration[LCount].FData             := 0;
      FEvapoTranspiration[LCount].FInitalised       := False;
      FEvapoTranspiration[LCount].FLength           := 8;
      FEvapoTranspiration[LCount].FDefaultPadding   := True;
      FEvapoTranspiration[LCount].FDecimal          := 2;
      FEvapoTranspiration[LCount].ShowDecimalPoint  := True;
    end;
    FChannelCount := 0;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TYMDemandCentre.Reset;
const OPNAME = 'TYMDemandCentre.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentre.SetAveEvaporation(const Value: TDouble);
const OPNAME = 'TYMDemandCentre.SetAveEvaporation';
begin
  try
  FAveEvaporation := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TYMDemandCentre.SetAveReturnFlowFactor(const Value: TDouble);
const OPNAME = 'TYMDemandCentre.SetAveReturnFlowFactor';
begin
  try
  FAveReturnFlowFactor := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TYMDemandCentre.SetChannelCount(const Value: Integer);
const OPNAME = 'TYMDemandCentre.SetChannelCount';
begin
  try
  FChannelCount := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TYMDemandCentre.SetConsumptiveChannelNr(const Value: TInteger);
const OPNAME = 'TYMDemandCentre.SetConsumptiveChannelNr';
begin
  try
  FConsumptiveChannelNr := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TYMDemandCentre.SetEvapoTranspiration(AIndex: Integer; const Value: TDouble);
const OPNAME = 'TYMDemandCentre.SetEvapoTranspiration';
begin
  try
    if (AIndex in [MinMonths..MaxMonths]) then
      FEvapoTranspiration[AIndex] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TYMDemandCentre.SetIdentifier(const Value: TInteger);
const OPNAME = 'TYMDemandCentre.SetIdentifier';
begin
  try
  FIdentifier := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TYMDemandCentre.SetName(const Value: TString);
const OPNAME = 'TYMDemandCentre.SetName';
begin
  try
  FName := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TYMDemandCentre.SetNodeNumber(const Value: TInteger);
const OPNAME = 'TYMDemandCentre.SetNodeNumber';
begin
  try
  FNodeNumber := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TYMDemandCentre.SetNodeRefNr(const Value: TInteger);
const OPNAME = 'TYMDemandCentre.SetNodeRefNr';
begin
  try
  FNodeRefNr := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TYMDemandCentre.SetRainfallScalingFactor(const Value: TDouble);
const OPNAME = 'TYMDemandCentre.SetRainfallScalingFactor';
begin
  try
  FRainfallScalingFactor := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TYMDemandCentre.SetReclaimationChannelNr(const Value: TInteger);
const OPNAME = 'TYMDemandCentre.SetReclaimationChannelNr';
begin
  try
  FReclaimationChannelNr := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TYMDemandCentre.SetReturnFlowChannel(AIndex: Integer; const Value: TYMDemandCentreReturnFlowChannel);
const OPNAME = 'TYMDemandCentre.SetReturnFlowChannel';
begin
  try
    if (AIndex in [MinNoReturnFlowChannels..MaxNoReturnFlowChannels]) then
      FReturnFlowChannel[AIndex] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TYMDemandCentre.SetRoutingConstant(const Value: TDouble);
const OPNAME = 'TYMDemandCentre.SetRoutingConstant';
begin
  try
  FRoutingConstant := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TYMDemandCentre.SetStdDeviationFactor(const Value: TDouble);
const OPNAME = 'TYMDemandCentre.SetStdDeviationFactor';
begin
  try
  FStdDeviationFactor := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TYMDemandCentre.SetTotalFlowLost(const Value: TDouble);
const OPNAME = 'TYMDemandCentre.SetTotalFlowLost';
begin
  try
  FTotalFlowLost := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TYMDemandCentreReturnFlowChannel }

procedure TYMDemandCentreReturnFlowChannel.CreateMemberObjects;
const OPNAME = 'TYMDemandCentreReturnFlowChannel.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FIdentifier       := TInteger.Create;
    FDemandCentreID   := TInteger.Create;
    FChannelNr        := TInteger.Create;
    FTotalReturnFlow  := TDouble.Create;
    FFlowDiversion    := TDouble.Create;
    Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TYMDemandCentreReturnFlowChannel.DestroyMemberObjects;
const OPNAME = 'TYMDemandCentreReturnFlowChannel.DestroyMemberObjects';
begin
  try
    FIdentifier.Free;
    FDemandCentreID.Free;
    FFlowDiversion.Free;
    FTotalReturnFlow.Free;
    FChannelNr.Free;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreReturnFlowChannel.Initialise: Boolean;
const OPNAME = 'TYMDemandCentreReturnFlowChannel.Initialise';
begin
  Result := False;
  try
    FIdentifier.FData           := 0;
    FIdentifier.FInitalised     := False;
    FIdentifier.FLength         := 3;
    FIdentifier.FDefaultPadding := True;

    FDemandCentreID.FData           := 0;
    FDemandCentreID.FInitalised     := False;
    FDemandCentreID.FLength         := 3;
    FDemandCentreID.FDefaultPadding := True;

    //Line 4a
    FChannelNr.FData           := 0;
    FChannelNr.FInitalised     := False;
    FChannelNr.FLength         := 5;
    FChannelNr.FDefaultPadding := True;

    // Line 4b
    FTotalReturnFlow.FData             := 0;
    FTotalReturnFlow.FInitalised       := False;
    FTotalReturnFlow.FLength           := 10;
    FTotalReturnFlow.FDefaultPadding   := True;
    FTotalReturnFlow.FDecimal          := 5;
    FTotalReturnFlow.ShowDecimalPoint  := True;

    // Line 4c
    FFlowDiversion.FData             := 0;
    FFlowDiversion.FInitalised       := False;
    FFlowDiversion.FLength           := 10;
    FFlowDiversion.FDefaultPadding   := True;
    FFlowDiversion.FDecimal          := 5;
    FFlowDiversion.ShowDecimalPoint  := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreReturnFlowChannel.Reset;
const OPNAME = 'TYMDemandCentreReturnFlowChannel.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreReturnFlowChannel.SetChannelNr(const Value: TInteger);
const OPNAME = 'TYMDemandCentreReturnFlowChannel.SetChannelNr';
begin
  FChannelNr := Value;
end;

procedure TYMDemandCentreReturnFlowChannel.SetDemandCentreID(const Value: TInteger);
const OPNAME = 'TYMDemandCentreReturnFlowChannel.SetDemandCentreID';
begin
  FDemandCentreID := Value;
end;

procedure TYMDemandCentreReturnFlowChannel.SetFlowDiversion(const Value: TDouble);
const OPNAME = 'TYMDemandCentreReturnFlowChannel.SetFlowDiversion';
begin
  FFlowDiversion := Value;
end;

procedure TYMDemandCentreReturnFlowChannel.SetIdentifier(const Value: TInteger);
const OPNAME = 'TYMDemandCentreReturnFlowChannel.SetIdentifier';
begin
  FIdentifier := Value;
end;

procedure TYMDemandCentreReturnFlowChannel.SetTotalReturnFlow(const Value: TDouble);
const OPNAME = 'TYMDemandCentreReturnFlowChannel.SetTotalReturnFlow';
begin
  FTotalReturnFlow := Value;
end;

{ TYMDemandCentreObject }

function TYMDemandCentreObject.AddDemandCentre: TYMDemandCentre;
const OPNAME = 'TYMDemandCentreObject.AddDemandCentre';
begin
  Result := TYMDemandCentre.Create;
  FObjectContainer.Add(Result);
end;

procedure TYMDemandCentreObject.CreateMemberObjects;
const OPNAME = 'TYMDemandCentreObject.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FExtraLines      := TStringlist.Create;
    FObjectContainer := TObjectList.Create;
    Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TYMDemandCentreObject.DemandCentreCount: Integer;
const OPNAME = 'TYMDemandCentreObject.DemandCentreCount';
begin
  Result := 0;
  try
    Result := FObjectContainer.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TYMDemandCentreObject.DestroyMemberObjects;
const OPNAME = 'TYMDemandCentreObject.DestroyMemberObjects';
begin
  try
    FObjectContainer.Clear;
    FreeAndNil(FObjectContainer);
    FExtraLines.Free;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TYMDemandCentreObject.GetDemandCentreByIndex(AIndex: Integer): TYMDemandCentre;
const OPNAME = 'TYMDemandCentreObject.GetDemandCentreByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FObjectContainer.Count) then
      Result :=  TYMDemandCentre(FObjectContainer[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TYMDemandCentreObject.Initialise: Boolean;
const OPNAME = 'TYMDemandCentreObject.Initialise';
begin
  Result := False;
  try
    FObjectContainer.Clear;
    FExtraLines.Clear;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TYMDemandCentreObject.Reset;
const OPNAME = 'TYMDemandCentreObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TYMDemandCentreObject.SetExtraLines(const Value: TStringList);
const OPNAME = 'TYMDemandCentreObject.SetExtraLines';
begin
  FExtraLines := Value;
end;

end.