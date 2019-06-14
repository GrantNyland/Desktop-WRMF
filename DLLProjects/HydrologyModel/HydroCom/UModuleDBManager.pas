(******************************************************************************)
(* Contains : TModuleDBManager.                                               *)
(******************************************************************************)

unit UModuleDBManager;

interface

uses
  Classes,

  UModule,
  UNetwork;

type

  TModuleDBManager = class(TObject)
  protected
  public
    ModuleAgent : TModuleAgent;
    function LoadPanDataFromDB (AModule : TModule): Boolean;
    function UpdatePanDataInDB (AModuleID, AMonth : Integer;
                                AEvaporation, APanFactor : Double): Boolean;
    function InsertModuleIntoDB (AModuleID   : Integer;
                                 AModuleType : String;
                                 ALatitude   : Double;
                                 ALongitude  : Double) : Boolean;
    function InsertNetworkModuleIntoDB (ANetworkID       : Integer;
                                        AModuleID        : Integer;
                                        AModuleNumber    : Integer;
                                        ANetworkSequence : Integer;
                                        AActive          : String) : Boolean;
    function InsertPanIntoDB (AModuleID : Integer) : Boolean;
    function InsertInflowRouteIntoDB (AModuleID, ARouteNo  : Integer;
                                      AFileName : String) : Boolean;
    function InsertOutflowRouteIntoDB (AModuleID, ARouteNo  : Integer;
                                       AFileName : String) : Boolean;
    function DeleteModuleFromDB (AModuleID : Integer) : Boolean;
    function DeleteNetworkModuleFromDB (AModuleID : Integer) : Boolean;
    function DeletePanFromDB (AModuleID : Integer) : Boolean;
    function DeleteModuleInflowRoutesFromDB (AModuleID : Integer) : Boolean;
    function DeleteModuleOutflowRoutesFromDB (AModuleID : Integer) : Boolean;
    function DeleteInflowRouteFromDB (AModuleID, ARouteNo : Integer) : Boolean;
    function DeleteOutflowRouteFromDB (AModuleID, ARouteNo : Integer) : Boolean;
    function GetNextNetworkSequence (ANetworkID : Integer) : Integer;
    function GetNextModuleNumber (ANetworkID  : Integer;
                                  AModuleType : String) : Integer;
    function UpdateInflowRoutesDataInDB (AModuleID, ARouteNo : Integer; AFileName : String): Boolean;
    function UpdateOutflowRoutesDataInDB (AModuleID, ARouteNo : Integer; AFileName : String): Boolean;
    function UpdateLongitudeInDB (AModuleID : Integer; AValue: Double) : Boolean;
    function UpdateLatitudeInDB (AModuleID : Integer; AValue: Double) : Boolean;
    function UpdateNetworkSequenceInDB (AModuleID : Integer; AValue: Integer) : Boolean;
    function LoadNetworkWithCodeFromDB (ANetwork : TNetwork; ANetworkCode : String) : Boolean;
    function GetNetworkSequenceDataFromDB (ANetworkID           : Integer;
                                           AModuleIDList        : TStringList;
                                           AModuleNumberList    : TStringList;
                                           ANetworkSequenceList : TStringList;
                                           AModuleTypeList      : TStringList;
                                           AModuleTextList      : TStringList) : Boolean;
  end;


var
  GModuleDBManager : TModuleDBManager;

implementation


uses
  SysUtils,
  Windows,
  DB,

  UHydroDBAgent,
  UErrorHandlingOperations;

(* TModuleDBManager ***********************************************************)

function TModuleDBManager.LoadPanDataFromDB (AModule : TModule): Boolean;
const OPNAME = 'TModuleDBManager.LoadPanDataFromDB';
var
  LQuery : TDataSet;
  LSQL   : String;
begin
  Result := FALSE;
  try
    LSQL := 'SELECT * FROM Pan WHERE ModuleID = ' +  IntToStr(AModule.ModuleID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        AModule.AddPan(LQuery.FieldByName('TheMonth').AsInteger,
                       LQuery.FieldByName('Evaporation').AsFloat,
                       LQuery.FieldByName('PanFactor').AsFloat);
        LQuery.Next;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModuleDBManager.UpdatePanDataInDB (AModuleID, AMonth : Integer;
                                             AEvaporation, APanFactor : Double): Boolean;
const OPNAME = 'TModuleDBManager.UpdatePanDataInDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'UPDATE Pan SET Evaporation = ' + FloatToStr(AEvaporation) +
            ', PanFactor = ' + FloatToStr(APanFactor) +
            ' WHERE ModuleID = ' + IntToStr(AModuleID) + ' AND TheMonth = ' + IntToStr(AMonth);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModuleDBManager.InsertModuleIntoDB (AModuleID   : Integer;
                                              AModuleType : String;
                                              ALatitude   : Double;
                                              ALongitude  : Double) : Boolean;
const OPNAME = 'TModuleDBManager.InsertModuleIntoDB';
var
  LSQL    : String;
begin
  Result := FALSE;
  try
    LSQL := 'INSERT INTO Modules (ModuleID, ModuleType, Longitude, Latitude) VALUES (' +
            IntToStr(AModuleID) + ', ' + QuotedStr(AModuleType) + ', ' +
            FloatToStr(ALongitude) + ', ' + FloatToStr(ALatitude) + ')';
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModuleDBManager.DeleteModuleFromDB (AModuleID : Integer) : Boolean;
const OPNAME = 'TModuleDBManager.InsertModuleIntoDB';
var
  LSQL    : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE FROM Modules WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModuleDBManager.InsertPanIntoDB (AModuleID : Integer) : Boolean;
const OPNAME = 'TModuleDBManager.InsertPanIntoDB';
var
  LSQL    : String;
  LMonth  : Integer;
  LResult : Boolean;
begin
  Result := FALSE;
  try
    LResult := TRUE;
    for LMonth := 1 to 12 do
    begin
      LSQL := 'INSERT INTO Pan (ModuleID, TheMonth, Evaporation, PanFactor) VALUES (' +
              IntToStr(AModuleID) + ', ' + IntToStr(LMonth) + ', ' +
              FloatToStr(0) + ', ' + FloatToStr(0) +  ')';
      LResult := LResult AND GHydroDBAgent.ExecuteSQL(LSQL, True);
    end;
    Result := LResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModuleDBManager.DeletePanFromDB (AModuleID : Integer) : Boolean;
const OPNAME = 'TModuleDBManager.DeletePanFromDB';
var
  LSQL    : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE FROM Pan WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModuleDBManager.InsertInflowRouteIntoDB (AModuleID, ARouteNo  : Integer;
                                                   AFileName : String) : Boolean;
const OPNAME = 'TModuleDBManager.InsertInflowRouteIntoDB';
var
  LSQL    : String;
begin
  Result := FALSE;
  try
    if (AFileName = '') then
      LSQL := 'INSERT INTO InflowRoutes (ModuleID, InflowRouteNo, InflowFileName) VALUES (' +
              IntToStr(AModuleID) + ', ' + IntToStr(ARouteNo) + ', NULL)'
    else
      LSQL := 'INSERT INTO InflowRoutes (ModuleID, InflowRouteNo, InflowFileName) VALUES (' +
              IntToStr(AModuleID) + ', ' + IntToStr(ARouteNo) + ', ' + QuotedStr(AFileName) + ')';
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModuleDBManager.DeleteModuleInflowRoutesFromDB (AModuleID : Integer) : Boolean;
const OPNAME = 'TModuleDBManager.DeleteModuleInflowRoutesFromDB';
var
  LSQL    : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE FROM InflowRoutes WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModuleDBManager.InsertOutflowRouteIntoDB (AModuleID, ARouteNo  : Integer;
                                                    AFileName : String) : Boolean;
const OPNAME = 'TModuleDBManager.InsertOutflowRouteIntoDB';
var
  LSQL    : String;
begin
  Result := FALSE;
  try
    if (AFileName = '') then
      LSQL := 'INSERT INTO OutflowRoutes (ModuleID, OutflowRouteNo, OutflowFileName) VALUES (' +
              IntToStr(AModuleID) + ', ' + IntToStr(ARouteNo) + ', NULL)'
    else
      LSQL := 'INSERT INTO OutflowRoutes (ModuleID, OutflowRouteNo, OutflowFileName) VALUES (' +
              IntToStr(AModuleID) + ', ' + IntToStr(ARouteNo) + ', ' + QuotedStr(AFileName) + ')';
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModuleDBManager.DeleteModuleOutflowRoutesFromDB (AModuleID : Integer) : Boolean;
const OPNAME = 'TModuleDBManager.DeleteModuleOutflowRoutesFromDB';
var
  LSQL    : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE FROM OutflowRoutes WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModuleDBManager.DeleteInflowRouteFromDB (AModuleID, ARouteNo : Integer) : Boolean;
const OPNAME = 'TModuleDBManager.DeleteInflowRouteFromDB';
var
  LSQL    : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE FROM InflowRoutes WHERE ModuleID = ' + IntToStr(AModuleID) +
            ' AND InflowRouteNo = ' + IntToStr(ARouteNo);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModuleDBManager.DeleteOutflowRouteFromDB (AModuleID, ARouteNo : Integer) : Boolean;
const OPNAME = 'TModuleDBManager.DeleteOutflowRouteFromDB';
var
  LSQL    : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE FROM OutflowRoutes WHERE ModuleID = ' + IntToStr(AModuleID) +
            ' AND OutflowRouteNo = ' + IntToStr(ARouteNo);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModuleDBManager.GetNextNetworkSequence (ANetworkID : Integer) : Integer;
const OPNAME = 'TModuleDBManager.GetNextNetworkSequence';
var
  LSQL    : String;
  LQuery  : TDataSet;
begin
  Result := 0;
  try
    LSQL := 'SELECT MAX(NetworkSequence) AS LastNetworkSequence FROM NetworkModules WHERE NetworkID = ' + IntToStr(ANetworkID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      if (NOT LQuery.Eof) then
        Result := LQuery.FieldByName('LastNetworkSequence').AsInteger + 1
      else
        Result := 1;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModuleDBManager.GetNextModuleNumber (ANetworkID  : Integer;
                                               AModuleType : string) : Integer;
const OPNAME = 'TModuleDBManager.GetNextModuleNumber';
var
  LSQL    : String;
  LQuery  : TDataSet;
begin
  Result := 0;
  try
    LSQL := 'SELECT MAX(ModuleNumber) AS LastModuleNumber FROM NetworkModules A, Modules B ' +
            ' WHERE NetworkID = ' + IntToStr(ANetworkID) + ' AND A.ModuleID = B.ModuleID AND B.ModuleType = ' +
            QuotedStr(AModuleType);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      if (NOT LQuery.Eof) then
        Result := LQuery.FieldByName('LastModuleNumber').AsInteger + 1
      else
        Result := 1;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModuleDBManager.InsertNetworkModuleIntoDB (ANetworkID       : Integer;
                                                     AModuleID        : Integer;
                                                     AModuleNumber    : Integer;
                                                     ANetworkSequence : Integer;
                                                     AActive          : String) : Boolean;
const OPNAME = 'TModuleDBManager.InsertNetworkModuleIntoDB';
var
  LSQL    : String;
begin
  Result := FALSE;
  try
    LSQL := 'INSERT INTO NetworkModules (NetworkID, ModuleID, ModuleNumber, NetworkSequence, Active) VALUES (' +
            IntToStr(ANetworkID) + ', ' + IntToStr(AModuleID) + ', ' + IntToStr(AModuleNumber) + ', ' +
            IntToStr(ANetworkSequence) + ', ' + QuotedStr(AActive) + ')';
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModuleDBManager.DeleteNetworkModuleFromDB (AModuleID : Integer) : Boolean;
const OPNAME = 'TModuleDBManager.DeleteNetworkModuleFromDB';
var
  LSQL    : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE FROM NetworkModules WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModuleDBManager.UpdateInflowRoutesDataInDB (AModuleID, ARouteNo : Integer; AFileName : String): Boolean;
const OPNAME = 'TModuleDBManager.UpdateInflowRoutesDataInDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    if (AFileName = '') then
      LSQL := 'UPDATE InflowRoutes SET InflowFileName = NULL' +
              ' WHERE ModuleID = ' + IntToStr(AModuleID) + ' AND InflowRouteNo = ' +
              IntToStr(ARouteNo)
    else
      LSQL := 'UPDATE InflowRoutes SET InflowFileName = ' + QuotedStr(AFileName) +
              ' WHERE ModuleID = ' + IntToStr(AModuleID) + ' AND InflowRouteNo = ' +
              IntToStr(ARouteNo);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModuleDBManager.UpdateOutflowRoutesDataInDB (AModuleID, ARouteNo : Integer; AFileName : String): Boolean;
const OPNAME = 'TModuleDBManager.UpdateOutflowRoutesDataInDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    if (AFileName = '') then
      LSQL := 'UPDATE OutflowRoutes SET OutflowFileName = NULL' +
              ' WHERE ModuleID = ' + IntToStr(AModuleID) + ' AND OutflowRouteNo = ' +
              IntToStr(ARouteNo)
    else
      LSQL := 'UPDATE OutflowRoutes SET OutflowFileName = ' + QuotedStr(AFileName) +
              ' WHERE ModuleID = ' + IntToStr(AModuleID) + ' AND OutflowRouteNo = ' +
              IntToStr(ARouteNo);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModuleDBManager.UpdateLongitudeInDB (AModuleID : Integer; AValue: Double) : Boolean;
const OPNAME = 'TModuleDBManager.UpdateLongitudeInDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'UPDATE Modules SET Longitude = ' + FloatToStr(AValue) + ' WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, TRUE);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModuleDBManager.UpdateLatitudeInDB (AModuleID : Integer; AValue: Double) : Boolean;
const OPNAME = 'TModuleDBManager.UpdateLatitudeInDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'UPDATE Modules SET Latitude = ' + FloatToStr(AValue) + ' WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, TRUE);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModuleDBManager.UpdateNetworkSequenceInDB (AModuleID : Integer; AValue: Integer) : Boolean;
const OPNAME = 'TModuleDBManager.UpdateNetworkSequenceInDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'UPDATE NetworkModules SET NetworkSequence = ' + IntToStr(AValue) + ' WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, TRUE);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModuleDBManager.LoadNetworkWithCodeFromDB (ANetwork : TNetwork; ANetworkCode : String) : Boolean;
const OPNAME = 'TModuleDBManager.LoadNetworkWithCodeFromDB';
var
  LQuery     : TDataSet;
  LSQL       : String;
  LNetworkID : Integer;
begin
  Result := FALSE;
  try
    LSQL := 'SELECT * FROM Network WHERE NetworkCode = ' + QuotedStr(ANetworkCode);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      if (NOT LQuery.Eof) then
      begin
        LNetworkID := LQuery.FieldByName('NetworkID').AsInteger;
        ANetwork.Populate(LNetworkID,
                          Trim(LQuery.FieldByName('NetworkCode').AsString),
                          LQuery.FieldByName('VersionNo').AsInteger,
                          Trim(LQuery.FieldByName('InputDirectory').AsString),
                          Trim(LQuery.FieldByName('OutputDirectory').AsString),
                          Trim(LQuery.FieldByName('DebugRequired').AsString),
                          LQuery.FieldByName('DebugStartPeriod').AsInteger,
                          LQuery.FieldByName('DebugEndPeriod').AsInteger,
                          Trim(LQuery.FieldByName('SummaryRequired').AsString),
                          LQuery.FieldByName('SimulationStartYear').AsInteger,
                          LQuery.FieldByName('SimulationEndYear').AsInteger,
                          LQuery.FieldByName('IsReadOnly').AsInteger,
                          LQuery.FieldByName('MinLongitude').AsFloat,
                          LQuery.FieldByName('MaxLongitude').AsFloat,
                          LQuery.FieldByName('MinLatitude').AsFloat,
                          LQuery.FieldByName('MaxLatitude').AsFloat);
        ANetwork.FindReservoirModuleAgent.LoadReservoirModules(LNetworkID);
        ANetwork.FindChannelModuleAgent.LoadChannelModules(LNetworkID);
        ANetwork.FindRunOffModuleAgent.LoadRunOffModules(LNetworkID);
        ANetwork.FindNetworkRouteAgent.LoadNetworkRoutes(LNetworkID);
        ANetwork.FindObservationPointAgent.LoadObservationPoints(LNetworkID);
        ANetwork.FindMineModuleAgent.LoadMineModules(LNetworkID);
        ANetwork.FindIrrigationModuleAgent.LoadIrrigationModules(LNetworkID);
        ANetwork.FindHydroNVDrawingAgent.LoadHydroNVDrawings(LNetworkID);
        ANetwork.FindHydroOutputAgent.LoadHydroOutputFromDB(LNetworkID);
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TModuleDBManager.GetNetworkSequenceDataFromDB (ANetworkID           : Integer;
                                                        AModuleIDList        : TStringList;
                                                        AModuleNumberList    : TStringList;
                                                        ANetworkSequenceList : TStringList;
                                                        AModuleTypeList      : TStringList;
                                                        AModuleTextList      : TStringList) : Boolean;
const OPNAME = 'TModuleDBManager.GetNetworkSequenceDataFromDB';
var
  LSQL   : String;
  LQuery : TDataSet;
begin
  Result := FALSE;
  try
    LSQL := 'SELECT A.*, B.ModuleType FROM NetworkModules A, Modules B ' +
            'WHERE A.ModuleID = B.ModuleID AND A.NetworkID = ' + IntToStr(ANetworkID) +
            ' ORDER BY NetworkSequence';
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        AModuleIDList.Add(Trim(LQuery.FieldByName('ModuleID').AsString));
        AModuleNumberList.Add(Trim(LQuery.FieldByName('ModuleNumber').AsString));
        ANetworkSequenceList.Add(Trim(LQuery.FieldByName('NetworkSequence').AsString));
        AModuleTypeList.Add(Trim(LQuery.FieldByName('ModuleType').AsString));
        AModuleTextList.Add(Trim(LQuery.FieldByName('ModuleType').AsString) + Trim(LQuery.FieldByName('ModuleNumber').AsString));
        LQuery.Next;
      end;
      Result := TRUE;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;




end.

