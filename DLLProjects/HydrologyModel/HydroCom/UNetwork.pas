(******************************************************************************)
(*  Contains : Class TNetwork.
(******************************************************************************)
unit UNetwork;


interface

uses
  Classes,
  Contnrs,

  UAbstractObject,
  UModule,
  UNetworkRoute,
  UObservationPoint,
  UChannelModule,
  UReservoirModule,
  URunOffModule,
  UMineModule,
  UHydroOutput,
  UIrrigationModule,
  UHydroNVDrawing,
  HydrologyCom_TLB;

type
  TNetwork = class(TAbstractObject, INetwork)
  protected
    FNetworkID              : Integer;
    FNetworkCode            : String;
    FVersionNo              : Integer;
    FInputDirectory         : String;
    FOutputDirectory        : String;
    FDebugRequired          : String;
    FDebugStartPeriod       : Integer;
    FDebugEndPeriod         : Integer;
    FSummaryRequired        : String;
    FSimulationStartYear    : Integer;
    FSimulationEndYear      : Integer;
    FIsReadOnly             : Integer;
    FMinLongitude           : Double;
    FMaxLongitude           : Double;
    FMinLatitude            : Double;
    FMaxLatitude            : Double;
    FNetworkRouteAgent      : TNetworkRouteAgent;
    FObservationPointAgent  : TObservationPointAgent;
    FChannelModuleAgent     : TChannelModuleAgent;
    FReservoirModuleAgent   : TReservoirModuleAgent;
    FRunOffModuleAgent      : TRunOffModuleAgent;
    FMineModuleAgent        : TMineModuleAgent;
    FIrrigationModuleAgent  : TIrrigationModuleAgent;
    FHydroNVDrawingAgent    : THydroNVDrawingAgent;
    FHydroOutputAgent       : THydroOutputAgent;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Initialise : Boolean; override;
    function Get_NetworkID: Integer; safecall;
    function Get_NetworkCode: WideString; safecall;
    procedure Set_NetworkCode(const Value: WideString); safecall;
    function Get_VersionNumber: Integer; safecall;
    procedure Set_VersionNumber(Value: Integer); safecall;
    function Get_InputDirectory: WideString; safecall;
    procedure Set_InputDirectory(const Value: WideString); safecall;
    function Get_OutputDirectory: WideString; safecall;
    procedure Set_OutputDirectory(const Value: WideString); safecall;
    function Get_DebugRequired: WideString; safecall;
    procedure Set_DebugRequired(const Value: WideString); safecall;
    function Get_DebugStartPeriod: Integer; safecall;
    procedure Set_DebugStartPeriod(Value: Integer); safecall;
    function Get_DebugEndPeriod: Integer; safecall;
    procedure Set_DebugEndPeriod(Value: Integer); safecall;
    function Get_SummaryRequired: WideString; safecall;
    procedure Set_SummaryRequired(const Value: WideString); safecall;
    function Get_SimulationStartYear: Integer; safecall;
    procedure Set_SimulationStartYear(Value: Integer); safecall;
    function Get_SimulationEndYear: Integer; safecall;
    procedure Set_SimulationEndYear(Value: Integer); safecall;
    function Get_NoOfIntervals: Integer; safecall;
    function Get_IsReadOnly: Integer; safecall;
    procedure Set_IsReadOnly(Value: Integer); safecall;
    function Get_MinLongitude: Double; safecall;
    procedure Set_MinLongitude(Value: Double); safecall;
    function Get_MaxLongitude: Double; safecall;
    procedure Set_MaxLongitude(Value: Double); safecall;
    function Get_MinLatitude: Double; safecall;
    procedure Set_MinLatitude(Value: Double); safecall;
    function Get_MaxLatitude: Double; safecall;
    procedure Set_MaxLatitude(Value: Double); safecall;
    function Get_NetworkRouteAgent: INetworkRouteAgent; safecall;
    function Get_ReservoirModuleAgent: IReservoirModuleAgent; safecall;
    function Get_ChannelModuleAgent: IChannelModuleAgent; safecall;
    function Get_RunOffModuleAgent: IRunOffModuleAgent; safecall;
    function Get_ObservationPointAgent: IObservationPointAgent; safecall;
    function Get_MineModuleAgent: IMineModuleAgent; safecall;
    function Get_IrrigationModuleAgent: IIrrigationModuleAgent; safecall;
    function Get_HydroNVDrawingAgent: IHydroNVDrawingAgent; safecall;
    function Get_HydroOutputAgent: IHydroOutputAgent; safecall;
    function Get_ModuleBySequenceNumber(ASequence: Integer): INetworkModule; safecall;
    function Get_ModuleTextByID(AModuleID: Integer): WideString; safecall;
    function Get_ModuleByID(AModuleID: Integer): INetworkModule; safecall;
    function Get_IntervalText(AInterval: Integer): WideString; safecall;
    function Populate (ANetworkID             : Integer;
                       const ANetworkCode     : WideString;
                       AVersionNumber         : Integer;
                       const AInputDirectory  : WideString;
                       const AOutputDirectory : WideString;
                       const ADebugRequired   : WideString;
                       ADebugStartPeriod      : Integer;
                       ADebugEndPeriod        : Integer;
                       const ASummaryRequired : WideString;
                       ASimulationStartYear   : Integer;
                       ASimulationEndYear     : Integer;
                       AIsReadOnly            : Integer;
                       AMinLongitude          : Double;
                       AMaxLongitude          : Double;
                       AMinLatitude           : Double;
                       AMaxLatitude           : Double): WordBool; safecall;
    function LoadNetworkWithCode(ANetworkCode : String): Boolean;
    function FindModuleWithID (AModuleID : Integer) : TNetworkModule;
    procedure SetRouteSource(ARouteNo: Integer; ASourceModuleID: Integer); safecall;
    procedure SetRouteSink(ARouteNo: Integer; ASinkModuleID: Integer); safecall;
    function DetermineNetworkSequence : WordBool; safecall;
    procedure AddOutflowRoutesFromModule (AModuleID  : Integer;
                                          ARouteList : TStringList);

    property NetworkID: Integer read Get_NetworkID;
    property NetworkCode: WideString read Get_NetworkCode write Set_NetworkCode;
    property VersionNumber: Integer read Get_VersionNumber write Set_VersionNumber;
    property InputDirectory: WideString read Get_InputDirectory write Set_InputDirectory;
    property OutputDirectory: WideString read Get_OutputDirectory write Set_OutputDirectory;
    property DebugRequired: WideString read Get_DebugRequired write Set_DebugRequired;
    property DebugStartPeriod: Integer read Get_DebugStartPeriod write Set_DebugStartPeriod;
    property DebugEndPeriod: Integer read Get_DebugEndPeriod write Set_DebugEndPeriod;
    property SummaryRequired: WideString read Get_SummaryRequired write Set_SummaryRequired;
    property SimulationStartYear: Integer read Get_SimulationStartYear write Set_SimulationStartYear;
    property SimulationEndYear: Integer read Get_SimulationEndYear write Set_SimulationEndYear;
    property NoOfIntervals: Integer read Get_NoOfIntervals;
    property IsReadOnly: Integer read Get_IsReadOnly write Set_IsReadOnly;
    property MinLongitude: Double read Get_MinLongitude write Set_MinLongitude;
    property MaxLongitude: Double read Get_MaxLongitude write Set_MaxLongitude;
    property MinLatitude: Double read Get_MinLatitude write Set_MinLatitude;
    property MaxLatitude: Double read Get_MaxLatitude write Set_MaxLatitude;
    property NetworkRouteAgent: INetworkRouteAgent read Get_NetworkRouteAgent;
    property ReservoirModuleAgent: IReservoirModuleAgent read Get_ReservoirModuleAgent;
    property ChannelModuleAgent: IChannelModuleAgent read Get_ChannelModuleAgent;
    property RunOffModuleAgent: IRunOffModuleAgent read Get_RunOffModuleAgent;
    property ObservationPointAgent: IObservationPointAgent read Get_ObservationPointAgent;
    property MineModuleAgent: IMineModuleAgent read Get_MineModuleAgent;
    property IrrigationModuleAgent: IIrrigationModuleAgent read Get_IrrigationModuleAgent;
    property HydroNVDrawingAgent: IHydroNVDrawingAgent read Get_HydroNVDrawingAgent;
    property HydroOutputAgent: IHydroOutputAgent read Get_HydroOutputAgent;
    property ModuleBySequenceNumber[ASequence: Integer]: INetworkModule read Get_ModuleBySequenceNumber;
    property ModuleTextByID[ASequenceNo: Integer]: WideString read Get_ModuleTextByID;
    property ModuleByID[AModuleID: Integer]: INetworkModule read Get_ModuleByID;
    property IntervalText[AInterval: Integer]: WideString read Get_IntervalText;
    property FindReservoirModuleAgent  : TReservoirModuleAgent  read FReservoirModuleAgent;
    property FindRunOffModuleAgent     : TRunOffModuleAgent     read FRunOffModuleAgent;
    property FindChannelModuleAgent    : TChannelModuleAgent    read FChannelModuleAgent;
    property FindIrrigationModuleAgent : TIrrigationModuleAgent read FIrrigationModuleAgent;
    property FindNetworkRouteAgent     : TNetworkRouteAgent     read FNetworkRouteAgent;
    property FindObservationPointAgent : TObservationPointAgent read FObservationPointAgent;
    property FindMineModuleAgent       : TMineModuleAgent       read FMineModuleAgent;
    property FindHydroOutputAgent      : THydroOutputAgent      read FHydroOutputAgent;
    property FindHydroNVDrawingAgent   : THydroNVDrawingAgent   read FHydroNVDrawingAgent;

  public
  end;


implementation

uses

  SysUtils,
  Windows,
  VCL.Forms,
  Math,
  VCL.Dialogs,

  UFlowRoute,
  UModuleDBManager,
  UErrorHandlingOperations;

{ TNetwork ********************************************************************}

function TNetwork._AddRef: Integer;
const OPNAME = 'TNetwork._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetwork._Release: Integer;
const OPNAME = 'TNetwork._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TNetwork.CreateMemberObjects;
const OPNAME = 'TNetwork.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    GModuleDBManager       := TModuleDBManager.Create;
    FNetworkRouteAgent     := TNetworkRouteAgent.Create;
    FObservationPointAgent := TObservationPointAgent.Create;
    FChannelModuleAgent    := TChannelModuleAgent.Create;
    FReservoirModuleAgent  := TReservoirModuleAgent.Create;
    FRunOffModuleAgent     := TRunOffModuleAgent.Create;
    FMineModuleAgent       := TMineModuleAgent.Create;
    FIrrigationModuleAgent := TIrrigationModuleAgent.Create;
    FHydroNVDrawingAgent   := THydroNVDrawingAgent.Create;
    FHydroOutputAgent      := THydroOutputAgent.Create;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TNetwork.DestroyMemberObjects;
const OPNAME = 'TNetwork.DestroyMemberObjects';
begin
  try
    FreeAndNil(FHydroOutputAgent);
    FreeAndNil(FHydroNVDrawingAgent);
    FreeAndNil(FNetworkRouteAgent);
    FreeAndNil(FObservationPointAgent);
    FreeAndNil(FChannelModuleAgent);
    FreeAndNil(FReservoirModuleAgent);
    FreeAndNil(FRunOffModuleAgent);
    FreeAndNil(FMineModuleAgent);
    FreeAndNil(FIrrigationModuleAgent);
    FreeAndNil(GModuleDBManager);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetwork.Initialise : Boolean;
const OPNAME = 'TNetwork.Initialise';
begin
  Result := FALSE;
  try
    Result := TRUE;
    Result := Result AND FObservationPointAgent.Initialise;
    Result := Result AND FChannelModuleAgent.Initialise;
    Result := Result AND FReservoirModuleAgent.Initialise;
    Result := Result AND FRunOffModuleAgent.Initialise;
    Result := Result AND FMineModuleAgent.Initialise;
    Result := Result AND FIrrigationModuleAgent.Initialise;
    Result := Result AND FNetworkRouteAgent.Initialise;
    Result := Result AND FHydroNVDrawingAgent.Initialise;
    Result := Result AND FHydroOutputAgent.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetwork.Get_NetworkID: Integer;
const OPNAME = 'TNetwork.Get_NetworkID';
begin
  Result := 0;
  try
    Result := FNetworkID;;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetwork.Get_NetworkCode: WideString;
const OPNAME = 'TNetwork.Get_NetworkCode';
begin
  Result := '';
  try
    Result := FNetworkCode;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetwork.Set_NetworkCode(const Value: WideString);
const OPNAME = 'TNetwork.Set_NetworkCode';
begin
  try
    FNetworkCode := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetwork.Get_VersionNumber: Integer;
const OPNAME = 'TNetworkRoute.Get_VersionNumber';
begin
  Result := 0;
  try
    Result := FVersionNo;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetwork.Set_VersionNumber(Value: Integer);
const OPNAME = 'TNetwork.Set_VersionNumber';
begin
  try
    FVersionNo := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetwork.Get_InputDirectory: WideString;
const OPNAME = 'TNetwork.Get_InputDirectory';
begin
  Result := '';
  try
    Result := FInputDirectory;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetwork.Set_InputDirectory(const Value: WideString);
const OPNAME = 'TNetwork.Set_InputDirectory';
begin
  try
    FInputDirectory := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetwork.Get_OutputDirectory: WideString;
const OPNAME = 'TNetwork.Get_OutputDirectory';
begin
  Result := '';
  try
    Result := FOutputDirectory;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetwork.Set_OutputDirectory(const Value: WideString);
const OPNAME = 'TNetwork.Set_OutputDirectory';
begin
  try
    FOutputDirectory := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetwork.Get_DebugRequired: WideString;
const OPNAME = 'TNetwork.Get_DebugRequired';
begin
  Result := '';
  try
    Result := FDebugRequired;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetwork.Set_DebugRequired(const Value: WideString);
const OPNAME = 'TNetwork.Set_DebugRequired';
begin
  try
    FDebugRequired := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetwork.Get_DebugStartPeriod: Integer;
const OPNAME = 'TNetwork.Get_DebugStartPeriod';
begin
  Result := 0;
  try
    Result := FDebugStartPeriod;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetwork.Set_DebugStartPeriod(Value: Integer);
const OPNAME = 'TNetwork.Set_DebugStartPeriod';
begin
  try
    FDebugStartPeriod := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetwork.Get_DebugEndPeriod: Integer;
const OPNAME = 'TNetwork.Get_DebugEndPeriod';
begin
  Result := 0;
  try
    Result := FDebugEndPeriod;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetwork.Set_DebugEndPeriod(Value: Integer);
const OPNAME = 'TNetwork.Set_DebugEndPeriod';
begin
  try
    FDebugEndPeriod := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetwork.Get_SummaryRequired: WideString;
const OPNAME = 'TNetwork.Get_SummaryRequired';
begin
  Result := '';
  try
    Result := FSummaryRequired;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetwork.Set_SummaryRequired(const Value: WideString);
const OPNAME = 'TNetwork.Set_SummaryRequired';
begin
  try
    FSummaryRequired := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetwork.Get_SimulationStartYear: Integer;
const OPNAME = 'TNetwork.Get_SimulationStartYear';
begin
  Result := 0;
  try
    Result := FSimulationStartYear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetwork.Set_SimulationStartYear(Value: Integer);
const OPNAME = 'TNetwork.Set_SimulationStartYear';
begin
  try
    FSimulationStartYear := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetwork.Get_SimulationEndYear: Integer;
const OPNAME = 'TNetwork.Get_SimulationEndYear';
begin
  Result := 0;
  try
    Result := FSimulationEndYear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetwork.Set_SimulationEndYear(Value: Integer);
const OPNAME = 'TNetwork.Set_SimulationEndYear';
begin
  try
    FSimulationEndYear := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetwork.Get_NoOfIntervals: Integer;
const OPNAME = 'TNetwork.Get_NoOfIntervals';
begin
  Result := 0;
  try
    Result := (FSimulationEndYear - FSimulationStartYear + 1) * 12;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetwork.Get_IntervalText (AInterval : Integer) : WideString;
const OPNAME = 'TNetwork.Get_IntervalText';
var
  LYear  : Integer;
  LMonth : Integer;
begin
  Result := '';
  try
    LYear  := AInterval div 12;
    LMonth := (AInterval mod 12) + 10;
    if (LMonth > 12) then
    begin
      LMonth := LMonth - 12;
      LYear  := LYear + 1;
    end;  
    Result := FormatSettings.ShortMonthNames[LMonth] + ' ' + IntToStr(FSimulationStartYear + LYear);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetwork.Get_IsReadOnly: Integer;
const OPNAME = 'TNetwork.Get_IsReadOnly';
begin
  Result := 0;
  try
    Result := FIsReadOnly;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetwork.Set_IsReadOnly(Value: Integer);
const OPNAME = 'TNetwork.Set_IsReadOnly';
begin
  try
    FIsReadOnly := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetwork.Get_MinLongitude: Double;
const OPNAME = 'TNetwork.Get_MinLongitude';
begin
  Result := 0.0;
  try
    Result := FMinLongitude;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetwork.Set_MinLongitude(Value: Double);
const OPNAME = 'TNetwork.Set_MinLongitude';
begin
  try
    FMinLongitude := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetwork.Get_MaxLongitude: Double;
const OPNAME = 'TNetwork.Get_MaxLongitude';
begin
  Result := 0.0;
  try
    Result := FMaxLongitude;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetwork.Set_MaxLongitude(Value: Double);
const OPNAME = 'TNetwork.Set_MaxLongitude';
begin
  try
    FMaxLongitude := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetwork.Get_MinLatitude: Double;
const OPNAME = 'TNetwork.Get_MinLatitude';
begin
  Result := 0.0;
  try
    Result := FMinLatitude;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetwork.Set_MinLatitude(Value: Double);
const OPNAME = 'TNetwork.Set_MinLatitude';
begin
  try
    FMinLatitude := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetwork.Get_MaxLatitude: Double;
const OPNAME = 'TNetwork.Get_MaxLatitude';
begin
  Result := 0.0;
  try
    Result := FMaxLatitude;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetwork.Set_MaxLatitude(Value: Double);
const OPNAME = 'TNetwork.Set_MaxLatitude';
begin
  try
    FMaxLatitude := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetwork.Get_NetworkRouteAgent: INetworkRouteAgent;
const OPNAME = 'TNetwork.Get_NetworkRouteAgent';
begin
  Result := nil;
  try
    Result := FNetworkRouteAgent;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetwork.Get_ReservoirModuleAgent: IReservoirModuleAgent;
const OPNAME = 'TNetwork.Get_ReservoirModuleAgent';
begin
  Result := nil;
  try
    Result := FReservoirModuleAgent;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetwork.Get_ChannelModuleAgent: IChannelModuleAgent;
const OPNAME = 'TNetwork.Get_ChannelModuleAgent';
begin
  Result := nil;
  try
    Result := FChannelModuleAgent;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetwork.Get_RunOffModuleAgent: IRunOffModuleAgent;
const OPNAME = 'TNetwork.Get_RunOffModuleAgent';
begin
  Result := nil;
  try
    Result := FRunOffModuleAgent;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetwork.Get_ObservationPointAgent: IObservationPointAgent;
const OPNAME = 'TNetwork.Get_ObservationPointAgent';
begin
  Result := nil;
  try
    Result := FObservationPointAgent;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetwork.Get_MineModuleAgent: IMineModuleAgent;
const OPNAME = 'TNetwork.Get_MineModuleAgent';
begin
  Result := nil;
  try
    Result :=  FMineModuleAgent;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetwork.Get_IrrigationModuleAgent: IIrrigationModuleAgent;
const OPNAME = 'TNetwork.Get_IrrigationModuleAgent';
begin
  Result := nil;
  try
    Result :=  FIrrigationModuleAgent;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetwork.Get_HydroNVDrawingAgent : IHydroNVDrawingAgent;
const OPNAME = 'TNetwork.Get_HydroNVDrawingAgent';
begin
  Result := nil;
  try
    Result :=  FHydroNVDrawingAgent;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetwork.Get_HydroOutputAgent : IHydroOutputAgent;
const OPNAME = 'TNetwork.Get_HydroNVDrawingAgent';
begin
  Result := nil;
  try
    Result :=  FHydroOutputAgent;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetwork.Populate (ANetworkID             : Integer;
                            const ANetworkCode     : WideString;
                            AVersionNumber         : Integer;
                            const AInputDirectory  : WideString;
                            const AOutputDirectory : WideString;
                            const ADebugRequired   : WideString;
                            ADebugStartPeriod      : Integer;
                            ADebugEndPeriod        : Integer;
                            const ASummaryRequired : WideString;
                            ASimulationStartYear   : Integer;
                            ASimulationEndYear     : Integer;
                            AIsReadOnly            : Integer;
                            AMinLongitude          : Double;
                            AMaxLongitude          : Double;
                            AMinLatitude           : Double;
                            AMaxLatitude           : Double): WordBool;
const OPNAME = 'TNetwork.Populate';
begin
  Result := FALSE;
  try
    FNetworkID           := ANetworkID;
    FNetworkCode         := ANetworkCode;
    FVersionNo           := AVersionNumber;
    FInputDirectory      := AInputDirectory;
    FOutputDirectory     := AOutputDirectory;
    FDebugRequired       := ADebugRequired;
    FDebugStartPeriod    := ADebugStartPeriod;
    FDebugEndPeriod      := ADebugEndPeriod;
    FSummaryRequired     := ASummaryRequired;
    FSimulationStartYear := ASimulationStartYear;
    FSimulationEndYear   := ASimulationEndYear;
    FIsReadOnly          := AIsReadOnly;
    FMinLongitude        := AMinLongitude;
    FMaxLongitude        := AMaxLongitude;
    FMinLatitude         := AMinLatitude;
    FMaxLatitude         := AMaxLatitude;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetwork.LoadNetworkWithCode (ANetworkCode : String) : Boolean;
const OPNAME = 'TNetwork.LoadNetworkWithCode';
begin
  Result := FALSE;
  try
    Initialise;
    Result := GModuleDBManager.LoadNetworkWithCodeFromDB(Self, ANetworkCode);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetwork.Get_ModuleTextByID (AModuleID : Integer): WideString;
const OPNAME = 'TNetwork.Get_ModuleTextByID';
var
  LNetworkModule : INetworkModule;
begin
  Result := '';
  try
    LNetworkModule := ModuleByID[AModuleID];
    if (LNetworkModule <> nil) then
      Result := LNetworkModule.ModuleType + IntToStr(LNetworkModule.ModuleNumber)
    else
      Result := '0';
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetwork.Get_ModuleBySequenceNumber (ASequence: Integer): INetworkModule;
const OPNAME = 'TNetwork.Get_ModuleBySequenceNumber';
var
  LIndex            : Integer;
  LReservoirModule  : IReservoirModule;
  LRunOffModule     : IRunOffModule;
  LChannelModule    : IChannelModule;
  LIrrigationModule : IIrrigationModule;
  LMineModule       : IMineModule;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FReservoirModuleAgent.ReservoirModuleCount)) do
    begin
      LReservoirModule := FReservoirModuleAgent.ReservoirModuleByIndex[LIndex];
      if (LReservoirModule.NetworkSequence = ASequence) then
        Result := LReservoirModule
      else
        LIndex := LIndex + 1;
    end;

    LIndex := 0;
    while ((Result = nil) AND (LIndex < FRunOffModuleAgent.RunOffModuleCount)) do
    begin
      LRunOffModule := FRunOffModuleAgent.RunOffModuleByIndex[LIndex];
      if (LRunOffModule.NetworkSequence = ASequence) then
        Result := LRunOffModule
      else
        LIndex := LIndex + 1;
    end;

    LIndex := 0;
    while ((Result = nil) AND (LIndex < FChannelModuleAgent.ChannelModuleCount)) do
    begin
      LChannelModule := FChannelModuleAgent.ChannelModuleByIndex[LIndex];
      if (LChannelModule.NetworkSequence = ASequence) then
        Result := LChannelModule
      else
        LIndex := LIndex + 1;
    end;

    LIndex := 0;
    while ((Result = nil) AND (LIndex < FIrrigationModuleAgent.IrrigationModuleCount)) do
    begin
      LIrrigationModule := FIrrigationModuleAgent.IrrigationModuleByIndex[LIndex];
      if (LIrrigationModule.NetworkSequence = ASequence) then
        Result := LIrrigationModule
      else
        LIndex := LIndex + 1;
    end;

    LIndex := 0;
    while ((Result = nil) AND (LIndex < FMineModuleAgent.MineModuleCount)) do
    begin
      LMineModule := FMineModuleAgent.MineModuleByIndex[LIndex];
      if (LMineModule.NetworkSequence = ASequence) then
        Result := LMineModule
      else
        LIndex := LIndex + 1;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetwork.Get_ModuleByID (AModuleID: Integer): INetworkModule;
const OPNAME = 'TNetwork.Get_ModuleByID';
var
  LIndex            : Integer;
  LReservoirModule  : IReservoirModule;
  LRunOffModule     : IRunOffModule;
  LChannelModule    : IChannelModule;
  LIrrigationModule : IIrrigationModule;
  LMineModule       : IMineModule;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FReservoirModuleAgent.ReservoirModuleCount)) do
    begin
      LReservoirModule := FReservoirModuleAgent.ReservoirModuleByIndex[LIndex];
      if (LReservoirModule.ModuleID = AModuleID) then
        Result := LReservoirModule
      else
        LIndex := LIndex + 1;
    end;

    LIndex := 0;
    while ((Result = nil) AND (LIndex < FRunOffModuleAgent.RunOffModuleCount)) do
    begin
      LRunOffModule := FRunOffModuleAgent.RunOffModuleByIndex[LIndex];
      if (LRunOffModule.ModuleID = AModuleID) then
        Result := LRunOffModule
      else
        LIndex := LIndex + 1;
    end;

    LIndex := 0;
    while ((Result = nil) AND (LIndex < FChannelModuleAgent.ChannelModuleCount)) do
    begin
      LChannelModule := FChannelModuleAgent.ChannelModuleByIndex[LIndex];
      if (LChannelModule.ModuleID = AModuleID) then
        Result := LChannelModule
      else
        LIndex := LIndex + 1;
    end;

    LIndex := 0;
    while ((Result = nil) AND (LIndex < FIrrigationModuleAgent.IrrigationModuleCount)) do
    begin
      LIrrigationModule := FIrrigationModuleAgent.IrrigationModuleByIndex[LIndex];
      if (LIrrigationModule.ModuleID = AModuleID) then
        Result := LIrrigationModule
      else
        LIndex := LIndex + 1;
    end;

    LIndex := 0;
    while ((Result = nil) AND (LIndex < FMineModuleAgent.MineModuleCount)) do
    begin
      LMineModule := FMineModuleAgent.MineModuleByIndex[LIndex];
      if (LMineModule.ModuleID = AModuleID) then
        Result := LMineModule
      else
        LIndex := LIndex + 1;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetwork.FindModuleWithID (AModuleID : Integer) : TNetworkModule;
const OPNAME = 'TNetwork.FindModuleWithID';
var
  LModule : TNetworkModule;
begin
  Result := nil;
  try
    LModule := FReservoirModuleAgent.FindReservoirModuleByID(AModuleID);
    if (LModule = nil) then
      LModule := FRunOffModuleAgent.FindRunOffModuleByID(AModuleID);
    if (LModule = nil) then
      LModule := FChannelModuleAgent.FindChannelModuleByID(AModuleID);
    if (LModule = nil) then
      LModule := FIrrigationModuleAgent.FindIrrigationModuleByID(AModuleID);
    if (LModule = nil) then
      LModule := FMineModuleAgent.FindMineModuleByID(AModuleID);
    Result := LModule;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TNetwork.SetRouteSource (ARouteNo: Integer; ASourceModuleID: Integer);
const OPNAME = 'TNetwork.SetRouteSource';
var
  LNetworkRoute      : INetworkRoute;
  LOldSourceModuleID : Integer;
  LOldSourceModule   : IModule;
  LNewSourceModule   : IModule;
  LOldRVModule       : TReservoirModule;
  LNewRVModule       : TReservoirModule;
  LOldCRModule       : TChannelModule;
  LNewCRModule       : TChannelModule;
  LOldRUModule       : TRunOffModule;
  LNewRUModule       : TRunOffModule;
  LOutflowRoute      : TOutFlowRoute;
  LRUOutflowRoute    : TRunOffOutflowRoute;
  LFileName          : String;
  LOutflowPerc       : Double;
begin
  try
    LNetworkRoute := FNetworkRouteAgent.NetworkRouteByRouteNo[ARouteNo];
    if ((LNetworkRoute <> nil) AND (LNetworkRoute.SourceModuleID <> ASourceModuleID)) then
    begin
      LOldSourceModuleID := LNetworkRoute.SourceModuleID;
      LNetworkRoute.SourceModuleID := ASourceModuleID;
      LOldSourceModule := ModuleByID[LOldSourceModuleID];
      LNewSourceModule := ModuleByID[ASourceModuleID];
      LFileName        := '';
      LOutflowPerc     := 0;
      if (LOldSourceModule <> nil) then
      begin
        if (LOldSourceModule.ModuleType = 'RV') then
        begin
          LOldRVModule  := FindReservoirModuleAgent.FindReservoirModuleByID(LOldSourceModule.ModuleID);
          LOutflowRoute := LOldRVModule.FindOutFlowRouteByRouteNo(ARouteNo);
          if (LOutflowRoute <> nil) then
            LFileName := LOutflowRoute.FileName;
          LOldRVModule.DeleteOutflowRoute(ARouteNo);
        end
        else if (LOldSourceModule.ModuleType = 'CR') then
        begin
          LOldCRModule  := FChannelModuleAgent.FindChannelModuleByID(LOldSourceModule.ModuleID);
          LOutflowRoute := LOldCRModule.FindOutFlowRouteByRouteNo(ARouteNo);
          if (LOutflowRoute <> nil) then
            LFileName := LOutflowRoute.FileName;
          LOldCRModule.DeleteOutflowRoute(ARouteNo);
        end
        else if (LOldSourceModule.ModuleType = 'RU') then
        begin
          LOldRUModule  := FRunOffModuleAgent.FindRunOffModuleByID(LOldSourceModule.ModuleID);
          LRUOutflowRoute := LOldRUModule.FindOutFlowRouteByRouteNo(ARouteNo);
          if (LRUOutflowRoute <> nil) then
            LOutflowPerc := LRUOutflowRoute.OutflowPercentage;
          LOldRUModule.DeleteOutflowRoute(ARouteNo);
        end;
      end;
      if (LNewSourceModule <> nil) then
      begin
        if (LNewSourceModule.ModuleType = 'RV') then
        begin
          LNewRVModule := FindReservoirModuleAgent.FindReservoirModuleByID(LNewSourceModule.ModuleID);
          LNewRVModule.CreateNewOutflowRoute(ARouteNo, LFileName);
        end
        else if (LNewSourceModule.ModuleType = 'CR') then
        begin
          LNewCRModule := FChannelModuleAgent.FindChannelModuleByID(LNewSourceModule.ModuleID);
          LNewCRModule.CreateNewOutflowRoute(ARouteNo, LFileName);
        end
        else if (LNewSourceModule.ModuleType = 'RU') then
        begin
          LNewRUModule := FRunOffModuleAgent.FindRunOffModuleByID(LNewSourceModule.ModuleID);
          LNewRUModule.CreateNewOutflowRoute(ARouteNo, LOutflowPerc);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetwork.SetRouteSink (ARouteNo: Integer; ASinkModuleID: Integer);
const OPNAME = 'TNetwork.SetRouteSink';
var
  LNetworkRoute    : INetworkRoute;
  LOldSinkModuleID : Integer;
  LOldSinkModule   : IModule;
  LNewSinkModule   : IModule;
  LOldRVModule     : TReservoirModule;
  LNewRVModule     : TReservoirModule;
  LOldCRModule     : TChannelModule;
  LNewCRModule     : TChannelModule;
  LInflowRoute     : TInflowRoute;
  LFileName        : String;
begin
  try
    LNetworkRoute := FNetworkRouteAgent.NetworkRouteByRouteNo[ARouteNo];
    if ((LNetworkRoute <> nil) AND (LNetworkRoute.SinkModuleID <> ASinkModuleID)) then
    begin
      LOldSinkModuleID  := LNetworkRoute.SinkModuleID;
      LNetworkRoute.SinkModuleID := ASinkModuleID;
      LOldSinkModule := ModuleByID[LOldSinkModuleID];
      LNewSinkModule := ModuleByID[ASinkModuleID];
      LFileName        := '';
      if (LOldSinkModule <> nil) then
      begin
        if (LOldSinkModule.ModuleType = 'RV') then
        begin
          LOldRVModule := FindReservoirModuleAgent.FindReservoirModuleByID(LOldSinkModule.ModuleID);
          LInflowRoute := LOldRVModule.FindInflowRouteByRouteNo(ARouteNo);
          if (LInflowRoute <> nil) then
            LFileName := LInflowRoute.FileName;
          LOldRVModule.DeleteInflowRoute(ARouteNo);
        end
        else if (LOldSinkModule.ModuleType = 'CR') then
        begin
          LOldCRModule := FChannelModuleAgent.FindChannelModuleByID(LOldSinkModule.ModuleID);
          LInflowRoute := LOldCRModule.FindInflowRouteByRouteNo(ARouteNo);
          if (LInflowRoute <> nil) then
            LFileName := LInflowRoute.FileName;
          LOldCRModule.DeleteInflowRoute(ARouteNo);
        end;
      end;
      if (LNewSinkModule <> nil) then
      begin
        if (LNewSinkModule.ModuleType = 'RV') then
        begin
          LNewRVModule := FindReservoirModuleAgent.FindReservoirModuleByID(LNewSinkModule.ModuleID);
          LNewRVModule.CreateNewInflowRoute(ARouteNo, LFileName);
        end
        else if (LNewSinkModule.ModuleType = 'CR') then
        begin
          LNewCRModule := FChannelModuleAgent.FindChannelModuleByID(LNewSinkModule.ModuleID);
          LNewCRModule.CreateNewInflowRoute(ARouteNo, LFileName);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetwork.AddOutflowRoutesFromModule (AModuleID  : Integer;
                                               ARouteList : TStringList);
const OPNAME = 'TNetwork.AddOutflowRoutesFromModule';
var
  LIndex : Integer;
  LRoute : INetworkRoute;
begin
  try
    for LIndex := 0 to FNetworkRouteAgent.NetworkRouteCount - 1 do
    begin
      LRoute := FNetworkRouteAgent.NetworkRouteByIndex[LIndex];
      if (LRoute.SourceModuleID = AModuleID) then
        ARouteList.Add(IntToStr(LRoute.RouteNo));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetwork.DetermineNetworkSequence : WordBool;
const OPNAME = 'TNetwork.DetermineNetworkSequence';
var
  LModuleList     : TStringList;
  LRouteList      : TStringList;
  LAllRVCRList    : TStringList;
  LIndex          : Integer;
  LCount          : Integer;
  LRunOff         : IRunOffModule;
  LIrrigation     : IIrrigationModule;
  LMine           : IMineModule;
  LReservoir      : IReservoirModule;
  LChannel        : IChannelModule;
  LStop           : Boolean;
  LFound          : Boolean;
  LInflowRouteNo  : Integer;
  LNetworkModule  : INetworkModule;
  LModuleID       : Integer;
  LMessage        : String;
  LRoute          : INetworkRoute;
begin
  Result := FALSE;
  try
    LModuleList  := TStringList.Create;
    LRouteList   := TStringList.Create;
    LAllRVCRList := TStringList.Create;
    try
      // Add all reservoir and channel reach module ids to temporary list. When this list is empty all
      // modules have been assigned a sequence.
      for LIndex := 0 to FReservoirModuleAgent.ReservoirModuleCount - 1 do
        LAllRVCRList.Add(IntToStr(FReservoirModuleAgent.ReservoirModuleByIndex[LIndex].ModuleID));
      for LIndex := 0 to FChannelModuleAgent.ChannelModuleCount - 1 do
        LAllRVCRList.Add(IntToStr(FChannelModuleAgent.ChannelModuleByIndex[LIndex].ModuleID));

      // Add all routes with source = 0 to the routes list;
      for LIndex := 0 to FNetworkRouteAgent.NetworkRouteCount - 1 do
      begin
        LRoute := FNetworkRouteAgent.NetworkRouteByIndex[LIndex];
        if (LRoute.SourceModuleID = 0) then
          LRouteList.Add(IntToStr(LRoute.RouteNo));
      end;

      // Add all the RunOff modules to the module list. Add all the RunOff outflow routes to the routes list.
      for LIndex := 0 to FRunOffModuleAgent.RunOffModuleCount - 1 do
      begin
        LRunOff := FRunOffModuleAgent.RunOffModuleByIndex[LIndex];
        AddOutflowRoutesFromModule(LRunOff.ModuleID, LRouteList);
        LModuleList.Add(IntToStr(LRunOff.ModuleID));
      end;

      // Add all the Irrigation modules to the module list. Add all the Irrigation return flow routes to the routes list.
      for LIndex := 0 to FIrrigationModuleAgent.IrrigationModuleCount - 1 do
      begin
        LIrrigation := FIrrigationModuleAgent.IrrigationModuleByIndex[LIndex];
        AddOutflowRoutesFromModule(LIrrigation.ModuleID, LRouteList);
        LModuleList.Add(IntToStr(LIrrigation.ModuleID));
      end;

      // Add all the Mine modules to the module list. Add all the Mine outflow routes to river and central PCD to the routes list.
      for LIndex := 0 to FMineModuleAgent.MineModuleCount - 1 do
      begin
        LMine := FMineModuleAgent.MineModuleByIndex[LIndex];
        AddOutflowRoutesFromModule(LMine.ModuleID, LRouteList);
        LModuleList.Add(IntToStr(LMine.ModuleID));
      end;

      LStop := FALSE;
      while ((NOT LStop) AND (LAllRVCRList.Count > 0)) do
      begin
        LFound := FALSE;
        LIndex := 0;
        while ((NOT LFound) AND (LIndex < LAllRVCRList.Count)) do
        begin
          LModuleID      := StrToInt(LAllRVCRList.Strings[LIndex]);
          LNetworkModule := FindModuleWithID(LModuleID);
          LFound         := TRUE;
          LCount         := 0;
          if (LNetworkModule.ModuleType = 'RV') then
          begin
            LReservoir := FReservoirModuleAgent.ReservoirModuleByID[LModuleID];
            while (LFound AND (LCount < LReservoir.NoOfInFlowRoutes)) do
            begin
              LInflowRouteNo := LReservoir.InflowRouteByIndex[LCount].RouteNo;
              if (LRouteList.IndexOf(IntToStr(LInflowRouteNo)) < 0) then
                LFound := FALSE
              else
                LCount := LCount + 1;
            end;
            if (LFound) then
            begin
              for LCount := 0 to LReservoir.NoOfOutFlowRoutes - 1 do
                LRouteList.Add(IntToStr(LReservoir.OutFlowRouteByIndex[LCount].RouteNo));
            end;
          end
          else
          begin
            LChannel := FChannelModuleAgent.ChannelModuleByID[LModuleID];
            while (LFound AND (LCount < LChannel.NoOfInFlowRoutes)) do
            begin
              LInflowRouteNo := LChannel.InflowRouteByIndex[LCount].RouteNo;
              if (LRouteList.IndexOf(IntToStr(LInflowRouteNo)) < 0) then
                LFound := FALSE
              else
                LCount := LCount + 1;
            end;
            if (LFound) then
            begin
              for LCount := 0 to LChannel.NoOfOutFlowRoutes - 1 do
                LRouteList.Add(IntToStr(LChannel.OutFlowRouteByIndex[LCount].RouteNo));
            end;
          end;
          if (LFound) then
          begin
            LModuleList.Add(IntToStr(LModuleID));
            LAllRVCRList.Delete(LIndex);
          end
          else
            LIndex := LIndex + 1;
        end;
        LStop := NOT LFound;
      end;

      if (LStop) then
      begin
        LMessage := 'Can not assign network sequence automatically!';
        ShowMessage(LMessage);
      end
      else
      begin
        for LIndex := 0 to LModuleList.Count - 1 do
        begin
          LModuleID := StrToInt(LModuleList.Strings[LIndex]);
          LNetworkModule := FindModuleWithID(LModuleID);
          LNetworkModule.NetworkSequence := LIndex + 1;
        end;
        Result := TRUE;
      end;
    finally
      LModuleList.Free;
      LRouteList.Free;
      LAllRVCRList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
