(******************************************************************************)
(* Contains : TRouteDBManager.                                                *)
(* Contains Database functionality for Network Routes.                        *)
(******************************************************************************)

unit URouteDBManager;

interface

uses
  Classes, Contnrs,

  UModuleDBManager,
  UNetworkRoute;

type

  TRouteDBManager = class(TModuleDBManager)
  protected
    function GetNextRouteNo (ANetworkID : Integer) : Integer;
  public
    function UpdateSourceModuleIDInDB (ARouteID, ASourceModuleID : Integer) : Boolean;
    function UpdateSinkModuleIDInDB (ARouteID, ASinkModuleID : Integer) : Boolean;
    function UpdatePropertiesDataInDB (ARouteID, ASourceModuleID, ASinkModuleID : Integer): Boolean;
    function DeleteNetworkRouteFromDB (ARouteID : Integer) : Boolean;
    function InsertNetworkRouteIntoDB (ANetworkID, ARouteID, ARouteNo : Integer) : Boolean;
    function LoadNetworkRoutesFromDB (ANetworkID : Integer) : Boolean;

    function CreateNewNetworkRouteInDB (ANetworkID: Integer): TNetworkRoute;
  end;

var
  GRouteDBManager : TRouteDBManager;

implementation


uses
  SysUtils,
  Windows,
  DB,

  UHydroDBAgent,
  UErrorHandlingOperations;

(* TRouteDBManager ************************************************************)

function TRouteDBManager.UpdateSourceModuleIDInDB (ARouteID, ASourceModuleID : Integer) : Boolean;
const OPNAME = 'TRouteDBManager.UpdateSourceModuleIDInDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'UPDATE NetworkRoutes SET SourceModuleID = ' + IntToStr(ASourceModuleID) +
            ' WHERE RouteID = ' + IntToStr(ARouteID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, TRUE);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRouteDBManager.UpdateSinkModuleIDInDB (ARouteID, ASinkModuleID : Integer) : Boolean;
const OPNAME = 'TRouteDBManager.UpdateSinkModuleIDInDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'UPDATE NetworkRoutes SET SinkModuleID = ' + IntToStr(ASinkModuleID) +
            ' WHERE RouteID = ' + IntToStr(ARouteID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, TRUE);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRouteDBManager.UpdatePropertiesDataInDB (ARouteID, ASourceModuleID, ASinkModuleID : Integer): Boolean;
const OPNAME = 'TRouteDBManager.UpdatePropertiesDataInDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'UPDATE NetworkRoutes SET' +
            ' SourceModuleID = ' + IntToStr(ASourceModuleID) +
            ', NetworkRoutes.SinkModuleID = ' + IntToStr(ASinkModuleID) +
            ' WHERE RouteID = ' + IntToStr(ARouteID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRouteDBManager.DeleteNetworkRouteFromDB (ARouteID : Integer) : Boolean;
const OPNAME = 'TRouteDBManager.DeleteNetworkRouteFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE FROM NetworkRoutes WHERE RouteID = ' + IntToStr(ARouteID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRouteDBManager.GetNextRouteNo (ANetworkID : Integer) : Integer;
const OPNAME = 'TRouteDBManager.GetNextRouteNo';
var
  LSQL    : String;
  LQuery  : TDataSet;
begin
  Result := 0;
  try
    LSQL := 'SELECT MAX(RouteNo) AS LastRouteNo FROM NetworkRoutes WHERE NetworkID = ' + IntToStr(ANetworkID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      if (NOT LQuery.Eof) then
        Result := LQuery.FieldByName('LastRouteNo').AsInteger + 1
      else
        Result := 1;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRouteDBManager.InsertNetworkRouteIntoDB (ANetworkID, ARouteID, ARouteNo : Integer) : Boolean;
const OPNAME = 'TRouteDBManager.InsertNetworkRouteIntoDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'INSERT INTO NetworkRoutes (NetworkID, RouteID, RouteNo) VALUES (' +
            IntToStr(ANetworkID) + ', ' + IntToStr(ARouteID) + ', ' + IntToStr(ARouteNo) + ')';
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRouteDBManager.CreateNewNetworkRouteInDB (ANetworkID: Integer) : TNetworkRoute;
const OPNAME = 'TRouteDBManager.CreateNewNetworkRouteInDB';
var
  LRoute   : TNetworkRoute;
  LResult  : Boolean;
  LRouteNo : Integer;
  LRouteID : Integer;
begin
  Result := nil;
  try
    LRouteID := GHydroDBAgent.GetNextID('NetworkRoutes', 'RouteID');
    LRouteNo := GetNextRouteNo(ANetworkID);

    LResult := InsertNetworkRouteIntoDB(ANetworkID, LRouteID, LRouteNo);
    if (LResult) then
    begin
      LRoute := TNetworkRouteAgent(ModuleAgent).AddNetworkRoute;
      LRoute.Populate(ANetworkID, LRouteID, LRouteNo, 0, 0, 0);
      Result := LRoute;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRouteDBManager.LoadNetworkRoutesFromDB (ANetworkID : Integer) : Boolean;
const OPNAME = 'TRouteDBManager.LoadNetworkRoutesFromDB';
var
  LQuery         : TDataSet;
  LSQL           : String;
  LNetworkRoute  : TNetworkRoute;
begin
  Result := FALSE;
  try
    LSQL := 'SELECT * FROM NetworkRoutes WHERE NetworkID = ' + IntToStr(ANetworkID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        LNetworkRoute := TNetworkRouteAgent(ModuleAgent).AddNetworkRoute;
        LNetworkRoute.Populate(LQuery.FieldByName('NetworkID').AsInteger,
                               LQuery.FieldByName('RouteID').AsInteger,
                               LQuery.FieldByName('RouteNo').AsInteger,
                               LQuery.FieldByName('SourceModuleID').AsInteger,
                               LQuery.FieldByName('SinkModuleID').AsInteger,
                               LQuery.FieldByName('RouteCost').AsInteger);
        LQuery.Next;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


end.

