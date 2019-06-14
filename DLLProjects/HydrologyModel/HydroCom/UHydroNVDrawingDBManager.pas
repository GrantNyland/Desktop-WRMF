(******************************************************************************)
(* Contains : THydroNVDrawingDBManager.                                       *)
(* Contains Database functionality for Hydro NV Drawings.                     *)
(******************************************************************************)

unit UHydroNVDrawingDBManager;

interface

uses
  Classes, Contnrs,

  UModuleDBManager,
  UHydroNVDrawing;

type

  THydroNVDrawingDBManager = class(TModuleDBManager)
  protected
  public
    function UpdateDrawingNameInDB (ADrawingID : Integer; AName : String) : Boolean;
    function UpdateGISDrawingInDB (ADrawingID : Integer; AValue: Integer) : Boolean;
    function UpdateReadOnlyInDB (ADrawingID : Integer; AValue: Integer) : Boolean;
    function CreateNewDrawingInDB (ANetworkID  : Integer;
                                   AGISDrawing : Integer;
                                   AShortName  : String) : THydroNVDrawing;
    function DeleteHydroNVDrawingIdDB (ADrawingID : Integer) : Boolean;
    function LoadHydroNVDrawingsFromDB (ANetworkID : Integer) : Boolean;

  end;

var
  GHydroNVDrawingDBManager : THydroNVDrawingDBManager;

implementation


uses
  SysUtils,
  Windows,
  DB,

  UHydroDBAgent,
  UErrorHandlingOperations;

(* THydroNVDrawingDBManager ***************************************************)

function THydroNVDrawingDBManager.UpdateDrawingNameInDB (ADrawingID : Integer; AName : String) : Boolean;
const OPNAME = 'THydroNVDrawingDBManager.UpdateDrawingNameInDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'UPDATE HydroNVDrawings SET DrawingName = ' + QuotedStr(AName) + ' WHERE DrawingID = ' + IntToStr(ADrawingID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, TRUE);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVDrawingDBManager.UpdateGISDrawingInDB (ADrawingID : Integer; AValue: Integer) : Boolean;
const OPNAME = 'THydroNVDrawingDBManager.UpdateGISDrawingInDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'UPDATE HydroNVDrawings SET GISDrawing = ' + IntToStr(AValue) + ' WHERE DrawingID = ' + IntToStr(ADrawingID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, TRUE);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVDrawingDBManager.UpdateReadOnlyInDB (ADrawingID : Integer; AValue: Integer) : Boolean;
const OPNAME = 'THydroNVDrawingDBManager.UpdateReadOnlyInDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'UPDATE HydroNVDrawings SET ReadOnly = ' + IntToStr(AValue) + ' WHERE DrawingID = ' + IntToStr(ADrawingID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, TRUE);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVDrawingDBManager.CreateNewDrawingInDB (ANetworkID  : Integer;
                                                        AGISDrawing : Integer;
                                                        AShortName  : String) : THydroNVDrawing;
const OPNAME = 'THydroNVDrawingDBManager.CreateNewDrawingInDB';
var
  LQuery          : TDataSet;
  LSQL            : String;
  LDrawingID      : Integer;
  LHydroNVDrawing : THydroNVDrawing;
begin
  Result := nil;
  try
    LSQL := 'INSERT INTO HydroNVDrawings (NetworkID, DrawingName, GISDrawing) VALUES ('  + IntToStr(ANetworkID) + ',' +
            QuotedStr(AShortName) + ', ' + IntToStr(AGISDrawing) + ')';
    if (GHydroDBAgent.ExecuteSQL(LSQL, TRUE)) then
    begin
      LSQL   := 'SELECT DrawingID FROM HydroNVDrawings WHERE NetworkID = ' + IntToStr(ANetworkID) +
                ' AND DrawingName = ' + QuotedStr(AShortName);
      LQuery := GHydroDBAgent.CreateQuery(LSQL);
      try
        LQuery.Open;
        if (NOT LQuery.Eof) then
        begin
          LDrawingID := LQuery.FieldByName('DrawingID').AsInteger;
          LHydroNVDrawing := THydroNVDrawingAgent(ModuleAgent).AddHydroNVDrawing;
          LHydroNVDrawing.Populate(ANetworkID, LDrawingID, AShortName, AGISDrawing, 0);
          Result := LHydroNVDrawing;
        end;
      finally
        LQuery.Close;
        LQuery.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVDrawingDBManager.DeleteHydroNVDrawingIdDB (ADrawingID : Integer) : Boolean;
const OPNAME = 'THydroNVDrawingDBManager.DeleteHydroNVDrawingIdDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE * FROM HydroNVDrawings WHERE DrawingID = ' + IntToStr(ADrawingID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, TRUE);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVDrawingDBManager.LoadHydroNVDrawingsFromDB (ANetworkID : Integer) : Boolean;
const OPNAME = 'THydroNVDrawingDBManager.LoadHydroNVDrawingsFromDB';
var
  LQuery          : TDataSet;
  LSQL            : String;
  LHydroNVDrawing : THydroNVDrawing;
begin
  Result := FALSE;
  try
    LSQL := 'SELECT * FROM HydroNVDrawings WHERE NetworkID = ' + IntToStr(ANetworkID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        LHydroNVDrawing := THydroNVDrawingAgent(ModuleAgent).AddHydroNVDrawing;
        LHydroNVDrawing.Populate(ANetworkID,
                                 LQuery.FieldByName('DrawingID').AsInteger,
                                 Trim(LQuery.FieldByName('DrawingName').AsString),
                                 LQuery.FieldByName('GISDrawing').AsInteger,
                                 LQuery.FieldByName('ReadOnly').AsInteger);
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

