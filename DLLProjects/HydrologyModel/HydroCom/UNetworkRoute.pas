(******************************************************************************)
(*  Contains : Class TNetworkRoute.
(******************************************************************************)
unit UNetworkRoute;


interface

uses
  Classes,
  Contnrs,
  XMLIntf,

  UModule,
  UAbstractObject,
  HydrologyCom_TLB;

type
  TNetworkRoute = class(TAbstractObject, INetworkRoute)
  protected
    FNetworkID      : Integer;
    FRouteID        : Integer;
    FRouteNo        : Integer;
    FSourceModuleID : Integer;
    FSinkModuleID   : Integer;
    FRouteCost      : Integer;
    function _AddRef  : Integer; stdcall;
    function _Release : Integer; stdcall;
    function DeleteNetworkRoute : Boolean;
  public
    function Get_RouteID: Integer; safecall;
    function Get_RouteNo: Integer; safecall;
    procedure Set_RouteNo(Value: Integer); safecall;
    function Get_SourceModuleID: Integer; safecall;
    procedure Set_SourceModuleID(Value: Integer); safecall;
    function Get_SinkModuleID: Integer; safecall;
    procedure Set_SinkModuleID(Value: Integer); safecall;
    function Get_RouteCost: Integer; safecall;
    procedure Set_RouteCost(Value: Integer); safecall;
    function Populate (ANetworkID      : Integer;
                       ARouteID        : Integer;
                       ARouteNo        : Integer;
                       ASourceModuleID : Integer;
                       ASinkModuleID   : Integer;
                       ARouteCost      : Integer): WordBool; safecall;
    function UpdatePropertiesData (ARootNode : IXMLNode): Boolean;
    property RouteID: Integer read Get_RouteID;
    property RouteNo: Integer read Get_RouteNo write Set_RouteNo;
    property SourceModuleID: Integer read Get_SourceModuleID write Set_SourceModuleID;
    property SinkModuleID: Integer read Get_SinkModuleID write Set_SinkModuleID;
    property RouteCost: Integer read Get_RouteCost write Set_RouteCost;
  end;

  TNetworkRouteAgent = class(TModuleAgent, INetworkRouteAgent)
  protected
    function _AddRef  : Integer; stdcall;
    function _Release : Integer; stdcall;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function FindNetworkRouteByNo(ARouteNo: Integer): TNetworkRoute;
    function Get_NetworkRouteCount: Integer; safecall;
    function Get_NetworkRouteByRouteNo(ARouteNo: Integer): INetworkRoute; safecall;
    function Get_NetworkRouteByIndex(AIndex: Integer): INetworkRoute; safecall;
    function LoadNetworkRoutes (ANetworkID: Integer) : Boolean;
    function AddNetworkRoute: TNetworkRoute;
    function CreateNewNetworkRoute(ANetworkID: Integer): INetworkRoute; safecall;
    function RemoveNetworkRoute(ANetworkID : Integer;
                                ARouteNo   : Integer): WordBool; safecall;
    property NetworkRouteCount: Integer read Get_NetworkRouteCount;
    property NetworkRouteByRouteNo[ARouteNo: Integer]: INetworkRoute read Get_NetworkRouteByRouteNo;
    property NetworkRouteByIndex[AIndex: Integer]: INetworkRoute read Get_NetworkRouteByIndex;
  end;


implementation

uses

  SysUtils,
  Windows,
  VCL.Forms,
  Math,

  UModuleDBManager,
  URouteDBManager,
  UErrorHandlingOperations;

{ TNetworkRoute ***************************************************************}

function TNetworkRoute._AddRef: Integer;
const OPNAME = 'TNetworkRoute._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkRoute._Release: Integer;
const OPNAME = 'TNetworkRoute._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkRoute.Get_RouteID: Integer;
const OPNAME = 'TNetworkRoute.Get_RouteID';
begin
  Result := 0;
  try
    Result := FRouteID;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkRoute.Get_RouteNo: Integer;
const OPNAME = 'TNetworkRoute.Get_RouteNo';
begin
  Result := 0;
  try
    Result := FRouteNo;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkRoute.Set_RouteNo(Value: Integer);
const OPNAME = 'TNetworkRoute.Set_RouteNo';
begin
  try
    FRouteNo := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkRoute.Get_SourceModuleID: Integer;
const OPNAME = 'TNetworkRoute.Get_SourceModuleID';
begin
  Result := 0;
  try
    Result := FSourceModuleID;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkRoute.Set_SourceModuleID(Value: Integer);
const OPNAME = 'TNetworkRoute.Set_SourceModuleID';
begin
  try
    if (GRouteDBManager.UpdateSourceModuleIDInDB(FRouteID, Value)) then
      FSourceModuleID := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkRoute.Get_SinkModuleID: Integer;
const OPNAME = 'TNetworkRoute.Get_SinkModuleID';
begin
  Result := 0;
  try
    Result := FSinkModuleID;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkRoute.Set_SinkModuleID(Value: Integer);
const OPNAME = 'TNetworkRoute.Set_SinkModuleID';
begin
  try
    if (GRouteDBManager.UpdateSinkModuleIDInDB(FRouteID, Value)) then
      FSinkModuleID := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkRoute.Get_RouteCost: Integer;
const OPNAME = 'TNetworkRoute.Get_RouteCost';
begin
  Result := 0;
  try
    Result := FRouteCost;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkRoute.Set_RouteCost(Value: Integer);
const OPNAME = 'TNetworkRoute.Set_RouteCost';
begin
  try
    FRouteCost := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkRoute.Populate (ANetworkID      : Integer;
                                 ARouteID        : Integer;
                                 ARouteNo        : Integer;
                                 ASourceModuleID : Integer;
                                 ASinkModuleID   : Integer;
                                 ARouteCost      : Integer): WordBool;
const OPNAME = 'TNetworkRoute.Populate';
begin
  Result := FALSE;
  try
    FNetworkID      := ANetworkID;
    FRouteID        := ARouteID;
    FRouteNo        := ARouteNo;
    FSourceModuleID := ASourceModuleID;
    FSinkModuleID   := ASinkModuleID;
    FRouteCost      := ARouteCost;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkRoute.UpdatePropertiesData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TNetworkRoute.UpdatePropertiesData';
var
  LSourceModuleID : Integer;
  LSinkModuleID   : Integer;
  LDataNode       : IXMLNode;
begin
  Result := FALSE;
  try
    LDataNode := ARootNode.ChildNodes['NetworkRoute'];
    LSourceModuleID := StrToInt(LDataNode.ChildNodes['SourceModuleID'].Text);
    LSinkModuleID   := StrToInt(LDataNode.ChildNodes['SinkModuleID'].Text);

    if (GRouteDBManager.UpdatePropertiesDataInDB(FRouteID, LSourceModuleID, LSinkModuleID)) then
    begin
      FSourceModuleID := LSourceModuleID;
      FSinkModuleID   := LSinkModuleID;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkRoute.DeleteNetworkRoute : Boolean;
const OPNAME = 'TNetworkRoute.DeleteNetworkRoute';
begin
  Result := FALSE;
  try
    Result := GRouteDBManager.DeleteNetworkRouteFromDB(FRouteID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TNetworkRouteAgent **********************************************************}

function TNetworkRouteAgent._AddRef: Integer;
const OPNAME = 'TNetworkRouteAgent._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkRouteAgent._Release: Integer;
const OPNAME = 'TNetworkRouteAgent._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TNetworkRouteAgent.CreateMemberObjects;
const OPNAME = 'TNetworkRouteAgent.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    GRouteDBManager := TRouteDBManager.Create;
    GRouteDBManager.ModuleAgent := Self;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TNetworkRouteAgent.DestroyMemberObjects;
const OPNAME = 'TNetworkRouteAgent.DestroyMemberObjects';
begin
  try
    GRouteDBManager.ModuleAgent := nil;
    FreeAndNil(GRouteDBManager);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkRouteAgent.AddNetworkRoute : TNetworkRoute;
const OPNAME = 'TNetworkRouteAgent.AddNetworkRoute';
var
  LNetworkRoute : TNetworkRoute;
begin
  Result := nil;
  try
    LNetworkRoute := TNetworkRoute.Create;
    FList.Add(LNetworkRoute);
    Result := LNetworkRoute;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkRouteAgent.CreateNewNetworkRoute (ANetworkID: Integer): INetworkRoute;
const OPNAME = 'TNetworkRouteAgent.CreateNewNetworkRoute';
begin
  Result := nil;
  try
    Result := GRouteDBManager.CreateNewNetworkRouteInDB(ANetworkID);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkRouteAgent.RemoveNetworkRoute (ANetworkID : Integer;
                                                ARouteNo   : Integer): WordBool;
const OPNAME = 'TNetworkRouteAgent.RemoveNetworkRoute';
var
  LRoute  : TNetworkRoute;
  LIndex  : Integer;
begin
  Result := FALSE;
  try
    LRoute  := FindNetworkRouteByNo(ARouteNo);
    if (LRoute <> nil) then
    begin
      if (GRouteDBManager.DeleteNetworkRouteFromDB(LRoute.RouteID)) then
      begin
        Result := TRUE;
        LIndex := FList.IndexOf(LRoute);
        FList.Delete(LIndex);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkRouteAgent.FindNetworkRouteByNo (ARouteNo: Integer): TNetworkRoute;
const OPNAME = 'TNetworkRouteAgent.FindNetworkRouteByNo';
var
  LNetworkRoute : TNetworkRoute;
  LIndex           : Integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FList.Count)) do
    begin
      LNetworkRoute := TNetworkRoute(FList.Items[LIndex]);
      if (LNetworkRoute.RouteNo = ARouteNo) then
        Result := LNetworkRoute
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkRouteAgent.Get_NetworkRouteCount: Integer;
const OPNAME = 'TNetworkRouteAgent.Get_NetworkRouteCount';
begin
  Result := 0;
  try
    Result := FList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkRouteAgent.Get_NetworkRouteByRouteNo (ARouteNo: Integer): INetworkRoute;
const OPNAME = 'TNetworkRouteAgent.Get_NetworkRouteByRouteNo';
var
  LNetworkRoute : TNetworkRoute;
  LIndex           : Integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FList.Count)) do
    begin
      LNetworkRoute := TNetworkRoute(FList.Items[LIndex]);
      if (LNetworkRoute.RouteNo = ARouteNo) then
        Result := LNetworkRoute
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkRouteAgent.Get_NetworkRouteByIndex(AIndex: Integer): INetworkRoute;
const OPNAME = 'TNetworkRouteAgent.Get_NetworkRouteByIndex';
begin
  Result := nil;
  try
    if (AIndex < FList.Count) then
      Result := TNetworkRoute(FList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkRouteAgent.LoadNetworkRoutes (ANetworkID : Integer) : Boolean;
const OPNAME = 'TNetworkRouteAgent.LoadNetworkRoutes';
begin
  Result := FALSE;
  try
    Result := GRouteDBManager.LoadNetworkRoutesFromDB(ANetworkID);
  except on E: Exception do HandleError(E, OPNAME); end;
end;


end.
