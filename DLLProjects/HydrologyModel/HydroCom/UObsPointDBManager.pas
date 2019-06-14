(******************************************************************************)
(* Contains : TObsPointDBManager.                                             *)
(* Contains Database functionality for Observation Points.                    *)
(******************************************************************************)

unit UObsPointDBManager;

interface

uses
  Classes, Contnrs,

  UModuleDBManager,
  UObservationPoint;

type

  TObsPointDBManager = class(TModuleDBManager)
  protected
  public
    function DeleteObservationPointFromDB (ANetworkID, ARouteNo : Integer) : Boolean;
    function UpdatePropertiesDataInDB (ANetworkID        : Integer;
                                       ARouteNo          : Integer;
                                       AName             : String;
                                       AFlowDataFileName : String): Boolean;
    function LoadFlowFileData (ANetworkID : Integer; AFlowFileName : String) : Boolean;
    function IsFlowDataLoaded (AFlowFileName : String) : Boolean;
    function IsFlowFileUsed (AFlowFileName : String) : Boolean;
    function DeleteFlowDataFromDB (AObsPoint : TObservationPoint): Boolean;
    function LoadFlowData (AObsPoint : TObservationPoint) : Boolean;
    function LoadObservationPointsFromDB (ANetworkID : Integer) : Boolean;
    function InsertObservationPointIntoDB (ANetworkID, ARouteNo : Integer) : Boolean;

  end;

var
  GObsPointDBManager : TObsPointDBManager;

implementation


uses
  SysUtils,
  Windows,
  DB,

  UFileReader,
  UHydroDBAgent,
  UErrorHandlingOperations;

(* TObsPointDBManager *********************************************************)

function TObsPointDBManager.DeleteObservationPointFromDB (ANetworkID, ARouteNo : Integer) : Boolean;
const OPNAME = 'TObsPointDBManager.DeleteObservationPointFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE FROM ObservationPoints WHERE NetworkID = ' + IntToStr(ANetworkID) +
            ' AND RouteNo = ' + IntToStr(ARouteNo);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TObsPointDBManager.UpdatePropertiesDataInDB (ANetworkID        : Integer;
                                                      ARouteNo          : Integer;
                                                      AName             : String;
                                                      AFlowDataFileName : String): Boolean;
const OPNAME = 'TObsPointDBManager.UpdatePropertiesData';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'UPDATE ObservationPoints SET' +
            ' RouteNo = ' + IntToStr(ARouteNo);
    if (AName = '') then
      LSQL := LSQL + ', Name = NULL'
    else
      LSQL := LSQL + ', Name = ' + QuotedStr(AName);
    if (AFlowDataFileName = '') then
      LSQL := LSQL + ', FlowDataFileName = NULL'
    else
      LSQL := LSQL + ', FlowDataFileName = ' + QuotedStr(AFlowDataFileName);

    LSQL := LSQL + ' WHERE RouteNo = ' + IntToStr(ARouteNo) + ' AND NetworkID = ' + IntToStr(ANetworkID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TObsPointDBManager.LoadFlowFileData (ANetworkID : Integer; AFlowFileName : String) : Boolean;
const OPNAME = 'TObsPointDBManager.LoadFlowFileData';
var
  LSQL         : String;
  LQuery       : TDataSet;
  LFlowFile    : TFileReader;
  LFileData    : TStringList;
  LInputDir    : String;
  LFileName    : String;
  LLineIndex   : Integer;
  LSummaryLine : Boolean;
  LYear        : Integer;
  LBuffer      : String;
  LMonthIndex  : Integer;
  LFlow        : Double;
  LFlag        : Char;
  LResult      : Boolean;
begin
  Result := FALSE;
  try
    LInputDir := '';
    LSQL := 'SELECT InputDirectory FROM Network WHERE NetworkID = ' + IntToStr(ANetworkID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      if (NOT LQuery.Eof) then
        LInputDir := Trim(LQuery.FieldByName('InputDirectory').AsString);
    finally
      LQuery.Close;
      LQuery.Free;
    end;
    if (LInputDir <> '') then
    begin
      LFileName := LInputDir + AFlowFileName;
      if (FileExists(LFileName)) then
      begin
        LFileData := TStringList.Create;
        try
          LFileData.LoadFromFile(LFileName);
          LFlowFile := TFileReader.Create;
          try

            // Discard all empty and header lines.
            for LLineIndex := LFileData.Count - 1 downto 0 do
              if (Length(LFileData[LLineIndex]) < 72) then
                LFileData.Delete(LLineIndex);

            // Make sure there is data.
            if (LFileData.Count > 0) then
            begin
              LFlowFile.LoadFromStrings(LFileData);
              LResult := TRUE;
              LLineIndex := 0;
              while (LResult AND (LLineIndex < LFlowFile.LineCount)) do
              begin
                // The line must be ignored if it does not start with a year.
                LSummaryLine := False;
                LFlowFile.Read(LBuffer, 9, 'Year[%d]', [LLineIndex]);
                LYear := 9999;
                try
                  LYear := StrToInt(Trim(LBuffer));
                except
                  LSummaryLine := True;
                end;
                if (NOT LSummaryLine) then
                begin
                  LSQL := 'INSERT INTO FlowFileData (FileCode, [Year], ' +
                          'Flow01, Flag01, Flow02, Flag02, Flow03, Flag03, Flow04, Flag04, ' +
                          'Flow05, Flag05, Flow06, Flag06, Flow07, Flag07, Flow08, Flag08, ' +
                          'Flow09, Flag09, Flow10, Flag10, Flow11, Flag11, Flow12, Flag12) ' +
                          'VALUES (' +
                          QuotedStr(AFlowFileName) + ', ' + IntToStr(LYear);
                  // Loop for the 12 months.
                  for LMonthIndex := 1 to 12 do
                  begin
                    LFlow := 0.0;
                    LFlag := ' ';
                    if (LFlowFile.CurrentLineRemainingCharacters > 0) then
                    begin
                      LFlowFile.Read(LFlow, 7, 'MonthlyFlow[%d,%d]', [LLineIndex,LMonthIndex]);
                      if (LFlowFile.CurrentLineRemainingCharacters > 0) then
                        LFlowFile.Read(LFlag, 'MonthlyFlowFlag[%d,%d]', [LLineIndex,LMonthIndex]);
                    end;
                    LSQL := LSQL + ', ' + FloatToStr(LFlow) + ', ' + QuotedStr(LFlag);
                  end;
                  LSQL := LSQL + ')';
                  LResult := GHydroDBAgent.ExecuteSQL(LSQL, True);
                end;
                LFlowFile.NextLine;
                LLineIndex := LLineIndex + 1;
              end;
            end;
          finally
            LFlowFile.Free;
          end;
        finally
          LFileData.Free;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TObsPointDBManager.IsFlowDataLoaded (AFlowFileName : String) : Boolean;
const OPNAME = 'TObsPointDBManager.IsFlowDataLoaded';
var
  LSQL     : String;
  LQuery   : TDataSet;
begin
  Result := FALSE;
  try
    LSQL := 'SELECT * FROM FlowFileData WHERE FileCode = ' + QuotedStr(AFlowFileName);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      if (NOT LQuery.Eof) then
        Result := TRUE;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TObsPointDBManager.IsFlowFileUsed (AFlowFileName : String) : Boolean;
const OPNAME = 'TObsPointDBManager.IsFlowFileUsed';
var
  LSQL     : String;
  LQuery   : TDataSet;
begin
  Result := FALSE;
  try
    LSQL := 'SELECT * FROM InflowRoutes WHERE InflowFileName = ' + QuotedStr(AFlowFileName);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      if (NOT LQuery.Eof) then
        Result := TRUE;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
    if (NOT Result) then
    begin
      LSQL := 'SELECT * FROM OutflowRoutes WHERE OutflowFileName = ' + QuotedStr(AFlowFileName);
      LQuery := GHydroDBAgent.CreateQuery(LSQL);
      try
        LQuery.Open;
        if (NOT LQuery.Eof) then
          Result := TRUE;
      finally
        LQuery.Close;
        LQuery.Free;
      end;
    end;
    if (NOT Result) then
    begin
      LSQL := 'SELECT * FROM ObservationPoints WHERE FlowDataFileName = ' + QuotedStr(AFlowFileName);
      LQuery := GHydroDBAgent.CreateQuery(LSQL);
      try
        LQuery.Open;
        if (NOT LQuery.Eof) then
          Result := TRUE;
      finally
        LQuery.Close;
        LQuery.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TObsPointDBManager.DeleteFlowDataFromDB (AObsPoint : TObservationPoint) : Boolean;
const OPNAME = 'TObsPointDBManager.DeleteFlowDataFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    AObsPoint.ClearFlowData;
    LSQL := 'DELETE FROM FlowFileData WHERE FileCode = ' + QuotedStr(AObsPoint.FlowDataFileName);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TObsPointDBManager.LoadFlowData (AObsPoint : TObservationPoint) : Boolean;
const OPNAME = 'TObsPointDBManager.LoadFlowData';
var
  LQuery         : TDataSet;
  LSQL           : String;
  LMonth         : Integer;
  LFieldName     : String;
  LYear          : Integer;
  LValue         : Double;
begin
  Result := FALSE;
  try
    AObsPoint.ClearFlowData;
    LSQL := 'SELECT * FROM FlowFileData WHERE FileCode = ' + QuotedStr(AObsPoint.FlowDataFileName) +
            ' ORDER BY Year';
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        LYear := LQuery.FieldByName('Year').AsInteger;
        for LMonth := 1 to 12 do
        begin
          LFieldName := 'Flow' + Format('%2.2d', [LMonth]);
          LValue     := LQuery.FieldByName(LFieldName).AsFloat;
          AObsPoint.AddFlowDataPeriodValue(LYear, LValue);
        end;
        LQuery.Next;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
    AObsPoint.CalculateFlowDataTotals;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TObsPointDBManager.LoadObservationPointsFromDB (ANetworkID : Integer) : Boolean;
const OPNAME = 'TObsPointDBManager.LoadObservationPointsFromDB';
var
  LQuery             : TDataSet;
  LSQL               : String;
  LObservationPoint  : TObservationPoint;
begin
  Result := FALSE;
  try
    LSQL := 'SELECT * FROM ObservationPoints WHERE NetworkID = ' + IntToStr(ANetworkID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        LObservationPoint := TObservationPointAgent(ModuleAgent).AddObservationPoint;
        LObservationPoint.Populate(LQuery.FieldByName('NetworkID').AsInteger,
                                   LQuery.FieldByName('RouteNo').AsInteger,
                                   Trim(LQuery.FieldByName('Name').AsString),
                                   Trim(LQuery.FieldByName('FlowDataFileName').AsString));
        GObsPointDBManager.LoadFlowData(LObservationPoint);
        LQuery.Next;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TObsPointDBManager.InsertObservationPointIntoDB (ANetworkID, ARouteNo : Integer) : Boolean;
const OPNAME = 'TObsPointDBManager.InsertObservationPointIntoDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'INSERT INTO ObservationPoints (NetworkID, RouteNo, Name) VALUES (' +
            IntToStr(ANetworkID) +  ', ' + IntToStr(ARouteNo) + ', ' +
            QuotedStr('OBS' + IntToStr(ARouteNo)) + ')';
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.

