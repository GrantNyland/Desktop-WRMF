(******************************************************************************)
(* Contains : TChannelDBManager.                                              *)
(* Contains Database functionality for Channels.                              *)
(******************************************************************************)

unit UChannelDBManager;

interface

uses
  Classes, Contnrs,

  UModuleDBManager,
  UChannelModule;

type

  TChannelDBManager = class(TModuleDBManager)
  protected
  public
    function MakeOutflowRoutePrincipalInDB (AModuleID, ARouteNo : Integer) : Boolean;
    function UpdatePropertiesDataInDB (AModuleID                : Integer;
                                       AActive                  : String;
                                       ALatitude                : Double;
                                       ALongitude               : Double;
                                       AChannelName             : String;
                                       AVersionNo               : Integer;
                                       AWetlandMAP              : Double;
                                       ARainfallFileName        : String;
                                       AMonthlyBedLoss          : Double;
                                       AWetlandStorage          : Double;
                                       AWetlandArea             : Double;
                                       AWetlandRechargeCoef     : Double;
                                       APrincipalOutflowRouteNo : Integer;
                                       AWetlandsInflowRouteNo   : Integer;
                                       AWetlandsOutflowRouteNo  : Integer;
                                       ADiversionRouteNo        : Integer;
                                       ABankfillCapacity        : Double;
                                       ADiversionEfficiency     : Double;
                                       AMaxMonthlyDivCapacity   : Double;
                                       AOldWetlandType          : Integer;
                                       ANewWetlandType          : Integer;
                                       ABankfillArea            : Double;
                                       ABankfillVolume          : Double;
                                       APowerOfAreaCapCurve     : Double;
                                       ABankfillCapacityComp    : Double;
                                       AWetlandInflowProportion : Double;
                                       AChannelInflowProportion : Double;
                                       AQDiv                    : Double): Boolean;
    function DeleteChannelFromDB (AModuleID : Integer) : Boolean;
    function DeleteCompWetlandParamsFromDB (AModuleID : Integer) : Boolean;
    function LoadChannelModulesFromDB (ANetworkID : Integer) : Boolean;
    function LoadComprehensiveWetlandParamsFromDB (AModuleID : Integer): Boolean;
    function LoadInflowRoutesFromDB (AChannelModule : TChannelModule) : Boolean;
    function LoadOutflowRoutesFromDB (AChannelModule : TChannelModule) : Boolean;
    function InsertChannelIntoDB (AModuleID : Integer;
                                  AName     : String) : Boolean;
    function CreateNewChannelModuleInDB (ANetworkID: Integer) : TChannelModule;
    function RemoveChannelModuleInDB (AModuleID : Integer) : Boolean;

  end;

var
  GChannelDBManager : TChannelDBManager;

implementation


uses
  SysUtils,
  Windows,
  DB,

  UHydroDBAgent,
  UErrorHandlingOperations;

(* TChannelDBManager **********************************************************)

function TChannelDBManager.MakeOutflowRoutePrincipalInDB (AModuleID, ARouteNo : Integer) : Boolean;
const OPNAME = 'TChannelDBManager.MakeOutflowRoutePrincipalInDB';
var
  LSQL   : string;
begin
  Result := FALSE;
  try
    LSQL := 'UPDATE ChannelModules SET PrincipalOutflowRouteNo = ' + IntToStr(ARouteNo) +
            ' WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelDBManager.UpdatePropertiesDataInDB (AModuleID                : Integer;
                                                     AActive                  : String;
                                                     ALatitude                : Double;
                                                     ALongitude               : Double;
                                                     AChannelName             : String;
                                                     AVersionNo               : Integer;
                                                     AWetlandMAP              : Double;
                                                     ARainfallFileName        : String;
                                                     AMonthlyBedLoss          : Double;
                                                     AWetlandStorage          : Double;
                                                     AWetlandArea             : Double;
                                                     AWetlandRechargeCoef     : Double;
                                                     APrincipalOutflowRouteNo : Integer;
                                                     AWetlandsInflowRouteNo   : Integer;
                                                     AWetlandsOutflowRouteNo  : Integer;
                                                     ADiversionRouteNo        : Integer;
                                                     ABankfillCapacity        : Double;
                                                     ADiversionEfficiency     : Double;
                                                     AMaxMonthlyDivCapacity   : Double;
                                                     AOldWetlandType          : Integer;
                                                     ANewWetlandType          : Integer;
                                                     ABankfillArea            : Double;
                                                     ABankfillVolume          : Double;
                                                     APowerOfAreaCapCurve     : Double;
                                                     ABankfillCapacityComp    : Double;
                                                     AWetlandInflowProportion : Double;
                                                     AChannelInflowProportion : Double;
                                                     AQDiv                    : Double): Boolean;
const OPNAME = 'TChannelDBManager.UpdatePropertiesDataInDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      LSQL := 'UPDATE (Modules INNER JOIN ChannelModules ON Modules.ModuleID = ChannelModules.ModuleID) ' +
              'INNER JOIN NetworkModules ON Modules.ModuleID = NetworkModules.ModuleID SET' +
              ' NetworkModules.Active = '                       + QuotedStr(AActive) +
              ', Modules.Latitude = '                           + FloatToStr(ALatitude) +
              ', Modules.Longitude = '                          + FloatToStr(ALongitude) +
              ', ChannelModules.ChannelName = '                 + QuotedStr(AChannelName) +
              ', ChannelModules.VersionNo = '                   + IntToStr(AVersionNo) +
              ', ChannelModules.WetlandMAP = '                  + FloatToStr(AWetlandMAP) +
              ', ChannelModules.MonthlyBedLoss = '              + FloatToStr(AMonthlyBedLoss) +
              ', ChannelModules.WetlandStorage = '              + FloatToStr(AWetlandStorage) +
              ', ChannelModules.WetlandArea = '                 + FloatToStr(AWetlandArea) +
              ', ChannelModules.WetlandRechargeCoefficient = '  + FloatToStr(AWetlandRechargeCoef) +
              ', ChannelModules.PrincipalOutflowRouteNo = '     + IntToStr(APrincipalOutflowRouteNo) +
              ', ChannelModules.WetlandsInflowRouteNo = '       + IntToStr(AWetlandsInflowRouteNo) +
              ', ChannelModules.WetlandsOutflowRouteNo = '      + IntToStr(AWetlandsOutflowRouteNo) +
              ', ChannelModules.DiversionRouteNo = '            + IntToStr(ADiversionRouteNo) +
              ', ChannelModules.WetlandType = '                 + IntToStr(ANewWetlandType) +
              ', ChannelModules.QDiv = '                        + FloatToStr(AQDiv) +
              ', ChannelModules.BankfillCapacity = '            + FloatToStr(ABankfillCapacity) +
              ', ChannelModules.DiversionEfficiency = '         + FloatToStr(ADiversionEfficiency) +
              ', ChannelModules.MaxMonthlyDiversionCapacity = ' + FloatToStr(AMaxMonthlyDivCapacity);
      if (ARainfallFileName = '') then
        LSQL := LSQL + ', ChannelModules.RainfallFileName = NULL'
      else
        LSQL := LSQL + ', ChannelModules.RainfallFileName = ' + QuotedStr(ARainfallFileName);

      LSQL := LSQL + ' WHERE Modules.ModuleID = ' + IntToStr(AModuleID);

      Result := GHydroDBAgent.ExecuteSQL(LSQL, True);

      if (Result) then
      begin
        LSQL := '';
        if ((AOldWetlandType <> 2) AND (ANewWetlandType = 2)) then
        begin
          LSQL := 'INSERT INTO CompWetlandParams VALUES (' + IntToStr(AModuleID) + ', ' +
                  FloatToStr(ABankfillArea)            + ', ' +
                  FloatToStr(ABankfillVolume)          + ', ' +
                  FloatToStr(APowerOfAreaCapCurve)     + ', ' +
                  FloatToStr(ABankfillCapacityComp)    + ', ' +
                  FloatToStr(AWetlandInflowProportion) + ', ' +
                  FloatToStr(AChannelInflowProportion) + ')';
          Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
        end
        else if ((AOldWetlandType = 2) AND (ANewWetlandType <> 2)) then
        begin
          LSQL := 'DELETE * FROM CompWetlandParams WHERE ModuleID = ' + IntToStr(AModuleID);
          Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
        end
        else if ((AOldWetlandType = 2) AND (ANewWetlandType = 2)) then
        begin
          LSQL := 'UPDATE CompWetlandParams SET' +
                  ' BankfillArea = '             + FloatToStr(ABankfillArea) +
                  ', BankfillVolume = '          + FloatToStr(ABankfillVolume) +
                  ', PowerOfAreaCapCurve = '     + FloatToStr(APowerOfAreaCapCurve) +
                  ', BankfillCapacity = '        + FloatToStr(ABankfillCapacityComp) +
                  ', WetlandInflowProportion = ' + FloatToStr(AWetlandInflowProportion) +
                  ', ChannelInflowProportion = ' + FloatToStr(AChannelInflowProportion) +
                  ' WHERE CompWetlandParams.ModuleID = ' + IntToStr(AModuleID);
          Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
        end;
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

function TChannelDBManager.DeleteChannelFromDB (AModuleID : Integer) : Boolean;
const OPNAME = 'TChannelDBManager.DeleteChannelFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE FROM ChannelModules WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDBManager.DeleteCompWetlandParamsFromDB (AModuleID : Integer) : Boolean;
const OPNAME = 'TChannelDBManager.DeleteCompWetlandParamsFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE * FROM CompWetlandParams WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDBManager.LoadChannelModulesFromDB (ANetworkID : Integer) : Boolean;
const OPNAME = 'TChannelDBManager.LoadChannelModulesFromDB';
var
  LQuery         : TDataSet;
  LSQL           : String;
  LChannelModule : TChannelModule;
  LWetlandType   : Integer;
  LModuleID      : Integer;
begin
  Result := FALSE;
  try
    LSQL := 'SELECT A.ModuleNumber, A.NetworkSequence, A.Active, B.*, C.ModuleType, C.Longitude, C.Latitude ' +
            'FROM (NetworkModules AS A LEFT JOIN ChannelModules AS B ON A.ModuleID = B.ModuleID) ' +
            'LEFT JOIN Modules AS C ON A.ModuleID = C.ModuleID WHERE C.ModuleType = ' +  QuotedStr('CR') +
            ' AND A.NetworkID = ' + IntToStr(ANetworkID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        LChannelModule := TChannelModuleAgent(ModuleAgent).AddChannelModule;
        LModuleID      := LQuery.FieldByName('ModuleID').AsInteger;
        LWetlandType   := LQuery.FieldByName('WetlandType').AsInteger;
        LChannelModule.Populate(ANetworkID,
                                LModuleID,
                                Trim(LQuery.FieldByName('ModuleType').AsString),
                                LQuery.FieldByName('ModuleNumber').AsInteger,
                                LQuery.FieldByName('NetworkSequence').AsInteger,
                                Trim(LQuery.FieldByName('Active').AsString),
                                Trim(LQuery.FieldByName('ChannelName').AsString),
                                LQuery.FieldByName('VersionNo').AsInteger,
                                LQuery.FieldByName('WetlandMAP').AsFloat,
                                Trim(LQuery.FieldByName('RainfallFileName').AsString),
                                LQuery.FieldByName('MonthlyBedLoss').AsFloat,
                                LQuery.FieldByName('WetlandStorage').AsFloat,
                                LQuery.FieldByName('WetlandArea').AsFloat,
                                LQuery.FieldByName('WetlandRechargeCoefficient').AsFloat,
                                LQuery.FieldByName('PrincipalOutflowRouteNo').AsInteger,
                                LQuery.FieldByName('WetlandType').AsInteger,
                                LQuery.FieldByName('QDiv').AsFloat,
                                LQuery.FieldByName('WetlandsInflowRouteNo').AsInteger,
                                LQuery.FieldByName('WetlandsOutflowRouteNo').AsInteger,
                                LQuery.FieldByName('DiversionRouteNo').AsInteger,
                                LQuery.FieldByName('BankfillCapacity').AsFloat,
                                LQuery.FieldByName('DiversionEfficiency').AsFloat,
                                LQuery.FieldByName('MaxMonthlyDiversionCapacity').AsFloat,
                                LQuery.FieldByName('Longitude').AsFloat,
                                LQuery.FieldByName('Latitude').AsFloat);
        LoadPanDataFromDB(LChannelModule);
        LoadInflowRoutesFromDB(LChannelModule);
        LoadOutflowRoutesFromDB(LChannelModule);
        if (LWetlandType = 2) then
          LoadComprehensiveWetlandParamsFromDB(LModuleID);
        LQuery.Next;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelDBManager.LoadComprehensiveWetlandParamsFromDB (AModuleID : Integer): Boolean;
const OPNAME = 'TChannelDBManager.LoadComprehensiveWetlandParamsFromDB';
var
  LQuery : TDataSet;
  LSQL   : String;
  LChannelModule : TChannelModule;
begin
  Result := FALSE;
  try
    LChannelModule := TChannelModuleAgent(ModuleAgent).FindChannelModuleByID(AModuleID);
    LSQL           := 'SELECT * FROM CompWetlandParams WHERE ModuleID = ' + IntToStr(AModuleID);
    LQuery         := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      if (NOT LQuery.Eof) then
      begin
        LChannelModule.PopulateComprehensiveWetlandParams(LQuery.FieldByName('BankfillArea').AsFloat,
                                                          LQuery.FieldByName('BankfillVolume').AsFloat,
                                                          LQuery.FieldByName('PowerOfAreaCapCurve').AsFloat,
                                                          LQuery.FieldByName('BankfillCapacity').AsFloat,
                                                          LQuery.FieldByName('WetlandInflowProportion').AsFloat,
                                                          LQuery.FieldByName('ChannelInflowProportion').AsFloat);
      end;
      LQuery.Close;
    finally
      LQuery.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelDBManager.LoadInflowRoutesFromDB (AChannelModule : TChannelModule) : Boolean;
const OPNAME = 'TChannelDBManager.LoadInflowRoutesFromDB';
var
  LQuery : TDataSet;
  LSQL   : String;
begin
  Result := FALSE;
  try
    LSQL := 'SELECT * FROM InflowRoutes WHERE ModuleID = ' +  IntToStr(AChannelModule.ModuleID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        AChannelModule.AddInflowRoute(LQuery.FieldByName('InflowRouteNo').AsInteger,
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

function TChannelDBManager.LoadOutflowRoutesFromDB (AChannelModule : TChannelModule) : Boolean;
const OPNAME = 'TChannelDBManager.LoadOutflowRoutesFromDB';
var
  LQuery         : TDataSet;
  LSQL           : String;
  LIndex         : Integer;
  LAbstractions  : String;
  LFieldName     : String;
begin
  Result := FALSE;
  try
    LSQL := 'SELECT * FROM OutflowRoutes WHERE ModuleID = ' +  IntToStr(AChannelModule.ModuleID);
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
        AChannelModule.AddOutflowRoute(LQuery.FieldByName('OutflowRouteNo').AsInteger,
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

function TChannelDBManager.InsertChannelIntoDB (AModuleID : Integer;
                                                  AName     : String) : Boolean;
const OPNAME = 'TChannelDBManager.InsertChannelIntoDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'INSERT INTO ChannelModules (ModuleID, ChannelName) VALUES (' +
            IntToStr(AModuleID) + ', ' + QuotedStr(AName) + ')';
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDBManager.CreateNewChannelModuleInDB (ANetworkID: Integer): TChannelModule;
const OPNAME = 'TChannelDBManager.CreateNewChannelModuleInDB';
var
  LChannelModule   : TChannelModule;
  LModuleID        : Integer;
  LNetworkSequence : Integer;
  LModuleNumber    : Integer;
  LName            : String;
begin
  Result := nil;
  try
    LModuleID        := GHydroDBAgent.GetNextID('Modules', 'ModuleID');
    LNetworkSequence := GetNextNetworkSequence(ANetworkID);
    LModuleNumber    := GetNextModuleNumber(ANetworkID, 'CR');
    LName            := 'CR' + IntToStr(LModuleNumber);

    GHydroDBAgent.StartTransaction;
    try
      if (InsertModuleIntoDB(LModuleID, 'CR', 0, 0)) AND
         (InsertPanIntoDB(LModuleID)) AND
         (InsertNetworkModuleIntoDB(ANetworkID, LModuleID, LModuleNumber, LNetworkSequence, 'Y')) AND
         (InsertChannelIntoDB(LModuleID, LName)) then
      begin
        GHydroDBAgent.CommitTransaction;
        LChannelModule := TChannelModuleAgent(ModuleAgent).AddChannelModule;
        LChannelModule.Populate(ANetworkID, LModuleID, 'CR', LModuleNumber, LNetworkSequence, 'Y', LName,
                                0, 0, '', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
        LoadPanDataFromDB(LChannelModule);
        LoadInflowRoutesFromDB(LChannelModule);
        LoadOutflowRoutesFromDB(LChannelModule);
        Result := LChannelModule;
      end
      else
        GHydroDBAgent.RollbackTransaction;
    except
      GHydroDBAgent.RollbackTransaction;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelDBManager.RemoveChannelModuleInDB (AModuleID : Integer) : Boolean;
const OPNAME = 'TChannelDBManager.RemoveChannelModuleInDB';
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      if (DeleteModuleInflowRoutesFromDB(AModuleID))  AND
         (DeleteModuleOutflowRoutesFromDB(AModuleID)) AND
         (DeletePanFromDB(AModuleID))                 AND
         (DeleteCompWetlandParamsFromDB(AModuleID))   AND
         (DeleteChannelFromDB(AModuleID))             AND
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

