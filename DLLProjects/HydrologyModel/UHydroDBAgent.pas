(******************************************************************************)
(*  Contains : UHydroDBAgent.
(******************************************************************************)
unit UHydroDBAgent;


interface

uses
  Data.DB,
  Data.Win.ADODB,
  UUtilities,
  UDWADBComponents,
  UTimeSeries;

type
  THydroDBAgent = class(TObject)
  protected
    function OpenADOConnection : Boolean;
    procedure CloseADOConnection;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateQuery (ASQL : String) : TDataSet;
    function CreateTable (ATableName : String) : TDataSet;
    function ExecuteSQL (ASQL : string; AShowError: boolean): boolean;
    function GetNextID (ATableName : String;
                        AFieldName : String) : Integer;
    function StartTransaction : Integer;
    procedure CommitTransaction;
    procedure RollbackTransaction;
    function InTransaction : Boolean;
    function WriteBufferToBlobField (ATableName, AIDField, ABlobField : String;
                                     AID, ABufferSize : Integer; ABuffer : PChar) : Boolean;
    function CopyBlobField (ATableName, AIDField, ABlobField : String;
                            AFromID, AToID : Integer) : Boolean;
  end;

var
  GHydroDBAgent : THydroDBAgent;

implementation

uses
  SysUtils,
  Classes,
  VCL.Forms,
  UErrorHandlingOperations;

var
  GConnection : TADOConnection;

  {**************************************************************}

constructor THydroDBAgent.Create;
const OPNAME = 'THydroDBAgent.Create';
begin
  try
    inherited Create;
    OpenADOConnection;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

destructor THydroDBAgent.Destroy;
const OPNAME = 'THydroDBAgent.Destroy';
begin
  try
    CloseADOConnection;
    inherited Destroy;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroDBAgent.OpenADOConnection : boolean;
const OPNAME = 'THydroDBAgent.OpenADOConnection';
var
  LDatabaseName : String;
begin
  Result := False;
  try

    // Destroy the database connection if it exists.
    CloseADOConnection;

//    LDatabaseName := 'C:\WINT\projects\WRSM2000\Database\WRSM2000.mdb';

    LDatabaseName := GetAppDataLocalDir + '\Database\WRSM2000.mdb'; //ExtractFilePath(ApplicationExeName) + 'Database\WRSM2000.mdb';
    // Create the connection object.
    GConnection := TADOConnection.Create(nil);
    GConnection.LoginPrompt := False;                    
    GConnection.Provider := 'Microsoft.Jet.OLEDB.4.0';
    GConnection.ConnectionString := 'Data Source=' + LDatabaseName;
    GConnection.CursorLocation := clUseServer;
    GConnection.Open;

    // Check the connection.
    if GConnection.Connected then
      Result := True;

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroDBAgent.CloseADOConnection;
const OPNAME = 'THydroDBAgent.CloseADOConnection';
begin
  try
//    Clear;
    if Assigned(GConnection) then
    begin
      GConnection.Close;
      FreeAndNil(GConnection);
    end;

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroDBAgent.CreateQuery  (ASQL : String) : TDataSet;
const OPNAME = 'THydroDBAgent.CreateQuery';
var
  LADOQuery : TADOQuery;
begin
  Result := nil;
  try
    if (GConnection = nil) then
    begin
      OpenADOConnection;
    end;
    if (GConnection <> nil) then
    begin
      LADOQuery := TADOQuery.Create(nil);
      try
        LADOQuery.Connection := GConnection;
        LADOQuery.CursorType := ctOpenForwardOnly;
        LADOQuery.LockType   := ltReadOnly;
        LADOQuery.SQL.Text   := ASQL;
        LADOQuery.Prepared   := True;
        Result := LADOQuery;
      finally
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroDBAgent.CreateTable  (ATableName : String) : TDataSet;
const OPNAME = 'THydroDBAgent.CreateTable';
var
  LADOTable : TADOTable;
begin
  Result := nil;
  try
    if (GConnection = nil) then
    begin
      OpenADOConnection;
    end;
    if (GConnection <> nil) then
    begin
      LADOTable := TADOTable.Create(nil);
      try
        LADOTable.Connection := GConnection;
        LADOTable.TableName  := ATableName;
        Result := LADOTable;
      finally
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

//
// Executes the SQL. Traps any exceptions and appends the actual SQL.
//
function THydroDBAgent.ExecuteSQL (ASQL : string; AShowError: boolean): boolean;
const OPNAME = 'THydroDBAgent.ExecuteSQL';
begin
  Result := False;
  try
    if (GConnection = nil) then
    begin
      OpenADOConnection;
    end;
    if (GConnection <> nil) then
    begin
      try
        GConnection.Execute(ASQL, cmdText, [eoExecuteNoRecords]);
        Result := True;
      except on E: Exception do
        if AShowError then
          raise Exception.Create(E.Message + '. SQL = ' + ASQL);
      end;
    end;
  // Handle exceptions
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroDBAgent.GetNextID (ATableName : String;
                                  AFieldName : String) : Integer;
const OPNAME = 'THydroDBAgent.GetNextID';
var
  LSQL     : String;
  LQuery   : TDataSet;
begin
  Result := 0;
  try
    LSQL := 'SELECT MAX(' + AFieldName + ') AS NewID FROM ' + ATableName;
    LQuery := CreateQuery(LSQL);
    try
      LQuery.Open;
      if (NOT LQuery.Eof) then
        Result := LQuery.FieldByName('NewID').AsInteger + 1;

    finally
      LQuery.Close;
      LQuery.Free;
   end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroDBAgent.StartTransaction : Integer;
const OPNAME = 'THydroDBAgent.StartTransaction';
begin
  Result := 0;
  try
    Result := GConnection.BeginTrans;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroDBAgent.CommitTransaction;
const OPNAME = 'THydroDBAgent.CommitTransaction';
begin
  try
    GConnection.CommitTrans;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroDBAgent.RollbackTransaction;
const OPNAME = 'THydroDBAgent.RollbackTransaction';
begin
  try
    GConnection.RollbackTrans;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroDBAgent.InTransaction : Boolean;
const OPNAME = 'THydroDBAgent.RollbackTransaction';
begin
  Result := FALSE;
  try
    Result := GConnection.InTransaction;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroDBAgent.WriteBufferToBlobField (ATableName, AIDField, ABlobField : String;
                                               AID, ABufferSize : Integer; ABuffer : PChar) : Boolean;
const OPNAME = 'THydroDBAgent.WriteBufferToBlobField';
var
  LTable : TADOTable;
  LBlobStream : TDWABlobStream;
  LBytes : Integer;
begin
  Result := FALSE;
  try
    // Write the time series data to the table by using a blob stream.
    LTable := TADOTable(CreateTable(ATableName));
    try
      LTable.Filter := AIDField + ' = ' + IntToStr(AID);
      LTable.Filtered := True;
      LTable.Open;
      try
        LTable.Edit;
        LTable.FieldByName(ABlobField).Clear;
        LBlobStream   := TDWABlobStream(LTable.CreateBlobStream(LTable.FieldByName(ABlobField),bmWrite));
        try
          LBytes := LBlobStream.Write(ABuffer[0], ABufferSize);
          Result := LBytes = ABufferSize;
        finally
          LBlobStream.Free;
        end;
        LTable.Post;
      finally
        LTable.Close;
      end;
    finally
      LTable.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroDBAgent.CopyBlobField (ATableName, AIDField, ABlobField : String;
                                      AFromID, AToID : Integer) : Boolean;
const OPNAME = 'THydroDBAgent.CopyBlobField';
var
  LSQL : String;
  LQuery : TDataSet;
  LTable : TADOTable;
  LToStream : TDWABlobStream;
  LFromStream : TStream;
  LBytes, LBlobSize : Integer;
begin
  Result := FALSE;
  try
    LSQL := 'SELECT * FROM ' + ATableName + ' WHERE ' + AIDField + ' = ' + IntToStr(AFromID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      if (NOT LQuery.Eof) then
      begin
        LTable := TADOTable(GHydroDBAgent.CreateTable(ATableName));
        try
          LTable.Filter := AIDField + ' = ' + IntToStr(AToID);
          LTable.Filtered := True;
          LTable.Open;
          try
            LTable.Edit;
            LTable.FieldByName(ABlobField).Clear;

            LFromStream := TDWABlobStream(LQuery.CreateBlobStream(LQuery.FieldByName(ABlobField), bmRead));
            try
              LFromStream.Seek(0, soFromBeginning);
              LBlobSize := LFromStream.Size;
              LFromStream.Seek(0, soFromBeginning);

              LToStream := TDWABlobStream(LTable.CreateBlobStream(LTable.FieldByName(ABlobField), bmWrite));
              try
                LBytes := LToStream.CopyFrom(LFromStream,0);
                Result := LBytes = LBlobSize;
              finally
                LToStream.Free;
              end;
            finally
              LFromStream.Free;
            end;
            LTable.Post;
          finally
            LTable.Close;
          end;
        finally
          LTable.Free;
        end;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

//finalization
//  freeandnil(GConnection);

end.
