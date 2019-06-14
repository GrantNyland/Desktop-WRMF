(******************************************************************************)
(* Contains : TReservoirDBManager.                                            *)
(* Contains Database functionality for Reservoirs.                            *)
(******************************************************************************)

unit UReservoirDBManager;

interface

uses
  Classes, Contnrs,

  UModuleDBManager,
  UReservoirModule;

type

  TReservoirDBManager = class(TModuleDBManager)
  protected
  public

    function LoadReservoirModulesFromDB (ANetworkID : Integer) : Boolean;
    function LoadVolumeAreaDataFromDB (AReservoir : TReservoirModule) : Boolean;
    function LoadInflowRoutesFromDB (AReservoir : TReservoirModule) : Boolean;
    function LoadOutflowRoutesFromDB (AReservoir : TReservoirModule) : Boolean;

    function DeleteReservoirFromDB (AModuleID : Integer) : Boolean;
    function DeleteVolumeAreaDataFromDB (AModuleID : Integer) : Boolean;

    function InsertReservoirIntoDB (AModuleID            : Integer;
                                    AReservoirName       : String;
                                    AMAP                 : Double;
                                    ARainfallFileName    : String;
                                    AAreaPower           : Double;
                                    ASpillageRouteNo     : Integer;
                                    AInitialStorageState : Double) : Boolean;
    function InsertVolumeAreaDataInDB (AModuleID : Integer; AYear, AVolume, AArea : String) : Boolean;

    function UpdatePropertiesDataInDB (AModuleID            : Integer;
                                       AActive              : String;
                                       ALatitude            : Double;
                                       ALongitude           : Double;
                                       AReservoirName       : String;
                                       AMAP                 : Double;
                                       ARainfallFileName    : String;
                                       AAreaPower           : Double;
                                       ASpillageRouteNo     : Integer;
                                       AInitialStorageState : Double): Boolean;
    function UpdateVolumeAreaDataInDB (AModuleID : Integer;
                                       AYearList, AVolList, AAreaList : TStringList) : Boolean;
    function CreateNewReservoirModuleInDB (ANetworkID : Integer): TReservoirModule;
    function RemoveReservoirModuleFromDB (AModuleID : Integer): Boolean;
  end;

var
  GReservoirDBManager : TReservoirDBManager;

implementation


uses
  SysUtils,
  Windows,
  DB,

  UHydroDBAgent,
  UErrorHandlingOperations;

(* TReservoirDBManager ********************************************************)

function TReservoirDBManager.LoadReservoirModulesFromDB (ANetworkID : Integer) : Boolean;
const OPNAME = 'TReservoirDBManager.LoadReservoirModulesFromDB';
var
  LQuery           : TDataSet;
  LSQL             : String;
  LReservoirModule : TReservoirModule;
begin
  Result := FALSE;
  try
    LSQL := 'SELECT A.ModuleNumber, A.NetworkSequence, A.Active, B.*, C.ModuleType, C.Longitude, C.Latitude ' +
            'FROM (NetworkModules AS A LEFT JOIN ReservoirModules AS B ON A.ModuleID = B.ModuleID) ' +
            'LEFT JOIN Modules AS C ON A.ModuleID = C.ModuleID WHERE C.ModuleType = ' +  QuotedStr('RV') +
            ' AND A.NetworkID = ' + IntToStr(ANetworkID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        LReservoirModule := TReservoirModuleAgent(ModuleAgent).AddReservoirModule;
        LReservoirModule.Populate(ANetworkID,
                                  LQuery.FieldByName('ModuleID').AsInteger,
                                  Trim(LQuery.FieldByName('ModuleType').AsString),
                                  LQuery.FieldByName('ModuleNumber').AsInteger,
                                  LQuery.FieldByName('NetworkSequence').AsInteger,
                                  Trim(LQuery.FieldByName('Active').AsString),
                                  Trim(LQuery.FieldByName('ReservoirName').AsString),
                                  LQuery.FieldByName('MAP').AsFloat,
                                  Trim(LQuery.FieldByName('RainfallFileName').AsString),
                                  LQuery.FieldByName('AreaPower').AsFloat,
                                  LQuery.FieldByName('SpillageRouteNo').AsInteger,
                                  LQuery.FieldByName('InitialStorageState').AsInteger,
                                  LQuery.FieldByName('Longitude').AsFloat,
                                  LQuery.FieldByName('Latitude').AsFloat);
        LoadPanDataFromDB(LReservoirModule);
        LoadVolumeAreaDataFromDB(LReservoirModule);
        LoadInflowRoutesFromDB(LReservoirModule);
        LoadOutflowRoutesFromDB(LReservoirModule);
        LQuery.Next;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirDBManager.LoadVolumeAreaDataFromDB (AReservoir : TReservoirModule) : Boolean;
const OPNAME = 'TReservoirDBManager.LoadVolumeAreaDataFromDB';
var
  LQuery : TDataSet;
  LSQL   : String;
begin
  Result := FALSE;
  try
    AReservoir.ClearVolumeAreaData;
    LSQL := 'SELECT * FROM ReservoirVolumeArea WHERE ModuleID = ' +  IntToStr(AReservoir.ModuleID) +
            ' ORDER BY Year';
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        AReservoir.AddVolumeAreaData(LQuery.FieldByName('Year').AsInteger,
                                     LQuery.FieldByName('Volume').AsFloat,
                                     LQuery.FieldByName('Area').AsFloat);
        LQuery.Next;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDBManager.LoadInflowRoutesFromDB (AReservoir : TReservoirModule) : Boolean;
const OPNAME = 'TReservoirDBManager.LoadInflowRoutesFromDB';
var
  LQuery       : TDataSet;
  LSQL         : String;
begin
  Result := FALSE;
  try
    LSQL := 'SELECT * FROM InflowRoutes WHERE ModuleID = ' +  IntToStr(AReservoir.ModuleID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        AReservoir.AddInflowRoute(LQuery.FieldByName('InflowRouteNo').AsInteger,
                                  Trim(LQuery.FieldByName('InflowFileName').AsString));
        LQuery.Next;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDBManager.LoadOutflowRoutesFromDB (AReservoir : TReservoirModule) : Boolean;
const OPNAME = 'TReservoirDBManager.LoadOutflowRoutesFromDB';
var
  LQuery         : TDataSet;
  LSQL           : String;
  LIndex         : Integer;
  LAbstractions  : String;
  LFieldName     : String;
begin
  Result := FALSE;
  try
    LSQL := 'SELECT * FROM OutflowRoutes WHERE ModuleID = ' +  IntToStr(AReservoir.ModuleID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        LAbstractions := '';
        for LIndex := 1 to 12 do
        begin
          LFieldName   := Format('AbstractionVolume%2.2d', [LIndex]);
          LAbstractions := LAbstractions + ',' + FloatToStr(LQuery.FieldByName(LFieldName).AsFloat);
        end;
        LAbstractions := Copy(LAbstractions, 2, Length(LAbstractions)-1);
        AReservoir.AddOutflowRoute(LQuery.FieldByName('OutflowRouteNo').AsInteger,
                                   Trim(LQuery.FieldByName('OutflowFileName').AsString),
                                   LAbstractions,
                                   LQuery.FieldByName('StorageState').AsFloat,
                                   LQuery.FieldByName('ReductionFactor').AsFloat);
        LQuery.Next;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDBManager.DeleteReservoirFromDB (AModuleID : Integer): Boolean;
const OPNAME = 'TReservoirDBManager.DeleteReservoirFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE FROM ReservoirModules WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDBManager.DeleteVolumeAreaDataFromDB (AModuleID : Integer) : Boolean;
const OPNAME = 'TReservoirDBManager.DeleteVolumeAreaDataFromDB';
var
  LSQL : String;
begin
  Result := TRUE;
  try
    LSQL := 'DELETE * FROM ReservoirVolumeArea WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDBManager.UpdatePropertiesDataInDB (AModuleID            : Integer;
                                                       AActive              : String;
                                                       ALatitude            : Double;
                                                       ALongitude           : Double;
                                                       AReservoirName       : String;
                                                       AMAP                 : Double;
                                                       ARainfallFileName    : String;
                                                       AAreaPower           : Double;
                                                       ASpillageRouteNo     : Integer;
                                                       AInitialStorageState : Double): Boolean;
const OPNAME = 'TReservoirDBManager.UpdatePropertiesDataInDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'UPDATE (Modules INNER JOIN ReservoirModules ON Modules.ModuleID = ReservoirModules.ModuleID) ' +
            'INNER JOIN NetworkModules ON Modules.ModuleID = NetworkModules.ModuleID SET' +
            ' NetworkModules.Active = '                 + QuotedStr(AActive) +
            ', Modules.Latitude = '                     + FloatToStr(ALatitude) +
            ', Modules.Longitude = '                    + FloatToStr(ALongitude) +
            ', ReservoirModules.ReservoirName = '       + QuotedStr(AReservoirName) +
            ', ReservoirModules.MAP = '                 + FloatToStr(AMAP) +
            ', ReservoirModules.AreaPower = '           + FloatToStr(AAreaPower) +
            ', ReservoirModules.SpillageRouteNo = '     + IntToStr(ASpillageRouteNo) +
            ', ReservoirModules.InitialStorageState = ' + FloatToStr(AInitialStorageState);
    if (ARainfallFileName = '') then
      LSQL := LSQL + ', ReservoirModules.RainfallFileName = NULL'
    else
      LSQL := LSQL + ', ReservoirModules.RainfallFileName = ' + QuotedStr(ARainfallFileName);

    LSQL := LSQL + ' WHERE Modules.ModuleID = ' + IntToStr(AModuleID);

    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDBManager.UpdateVolumeAreaDataInDB (AModuleID : Integer;
                                                       AYearList, AVolList, AAreaList : TStringList) : Boolean;
const OPNAME = 'TReservoirDBManager.UpdateVolumeAreaDataInDB';
var
  LIndex     : Integer;
  LYear      : String;
  LVolume    : String;
  LArea      : String;
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      Result := DeleteVolumeAreaDataFromDB(AModuleID);
      LIndex := 0;
      while (Result AND (LIndex < AYearList.Count)) do
      begin
        LYear   := AYearList.Strings[LIndex];
        LVolume := AVolList.Strings[LIndex];
        LArea   := AAreaList.Strings[LIndex];
        Result  := InsertVolumeAreaDataInDB(AModuleID, LYear, LVolume, LArea);
        LIndex  := LIndex + 1
      end;
      if (Result) then
        GHydroDBAgent.CommitTransaction
      else
        GHydroDBAgent.RollbackTransaction;  
    except
      GHydroDBAgent.RollbackTransaction;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDBManager.InsertVolumeAreaDataInDB (AModuleID : Integer; AYear, AVolume, AArea : String) : Boolean;
const OPNAME = 'TReservoirDBManager.InsertVolumeAreaDataInDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'INSERT INTO ReservoirVolumeArea VALUES (' +
            IntToStr(AModuleID) + ', ' + AYear + ', ' + AVolume + ', ' + AArea + ')';
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDBManager.InsertReservoirIntoDB (AModuleID            : Integer;
                                                    AReservoirName       : String;
                                                    AMAP                 : Double;
                                                    ARainfallFileName    : String;
                                                    AAreaPower           : Double;
                                                    ASpillageRouteNo     : Integer;
                                                    AInitialStorageState : Double) : Boolean;
const OPNAME = 'TReservoirDBManager.InsertReservoirIntoDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'INSERT INTO ReservoirModules (ModuleID, ReservoirName, MAP, RainfallFileName, AreaPower, SpillageRouteNo, InitialStorageState) VALUES (' +
            IntToStr(AModuleID) + ', ' + QuotedStr(AReservoirName) + ', ' +
            FloatToStr(AMAP) + ', ' + QuotedStr(ARainfallFileName) + ', ' +
            FloatToStr(AAreaPower) + ', ' + IntToStr(ASpillageRouteNo) + ', ' +
            FloatToStr(AInitialStorageState) + ')';
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDBManager.CreateNewReservoirModuleInDB (ANetworkID : Integer) : TReservoirModule;
const OPNAME = 'TReservoirDBManager.CreateNewReservoirModuleInDB';
var
  LReservoirModule : TReservoirModule;
  LModuleID        : Integer;
  LNetworkSequence : Integer;
  LModuleNumber    : Integer;
  LReservoirName   : String;
begin
  Result := nil;
  try
    LModuleID        := GHydroDBAgent.GetNextID('Modules', 'ModuleID');
    LNetworkSequence := GetNextNetworkSequence(ANetworkID);
    LModuleNumber    := GetNextModuleNumber(ANetworkID, 'RV');
    LReservoirName   := 'RV' + IntToStr(LModuleNumber);

    GHydroDBAgent.StartTransaction;
    try
      if (InsertModuleIntoDB(LModuleID, 'RV', 0, 0)) AND
         (InsertPanIntoDB(LModuleID)) AND
         (InsertNetworkModuleIntoDB(ANetworkID, LModuleID, LModuleNumber, LNetworkSequence, 'Y')) AND
         (InsertReservoirIntoDB(LModuleID, LReservoirName, 0, '', 0, 0, 0)) then
      begin
        GHydroDBAgent.CommitTransaction;
        LReservoirModule := TReservoirModuleAgent(ModuleAgent).AddReservoirModule;
        LReservoirModule.Populate(ANetworkID, LModuleID, 'RV', LModuleNumber, LNetworkSequence, 'Y',
                                  LReservoirName, 0, '', 0, 0, 0, 0, 0);
        LoadPanDataFromDB(LReservoirModule);
        LoadVolumeAreaDataFromDB(LReservoirModule);
        LoadInflowRoutesFromDB(LReservoirModule);
        LoadOutflowRoutesFromDB(LReservoirModule);
        Result := LReservoirModule;
      end
      else
        GHydroDBAgent.RollbackTransaction;
    except
      GHydroDBAgent.RollbackTransaction;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirDBManager.RemoveReservoirModuleFromDB (AModuleID : Integer) : Boolean;
const OPNAME = 'TReservoirDBManager.RemoveReservoirModuleFromDB';
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      if (DeleteModuleInflowRoutesFromDB(AModuleID))  AND
         (DeleteModuleOutflowRoutesFromDB(AModuleID)) AND
         (DeleteVolumeAreaDataFromDB(AModuleID))      AND
         (DeletePanFromDB(AModuleID))                 AND
         (DeleteReservoirFromDB(AModuleID))           AND
         (DeleteNetworkModuleFromDB(AModuleID))       AND
         (DeleteModuleFromDB(AModuleID)) then
      begin
        Result := TRUE;
        GHydroDBAgent.CommitTransaction;
      end
      else
        GHydroDBAgent.RollbackTransaction;
    except
      GHydroDBAgent.RollbackTransaction;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


end.

