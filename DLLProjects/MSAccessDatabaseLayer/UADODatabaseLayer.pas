//
//
//  UNIT      : Contains TADODatabaseLayer Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/02/06
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UADODatabaseLayer;

interface

uses
  Classes,
  Data.DB,
  Data.Win.ADODB,
  UDWADBComponents,
  UDBConnection,
  UAbstractObject,
  USQLDatabaseLayer;

type
  TADODatabaseLayer = class(TSQLDatabaseLayer)
  protected
    FDatabase     : TDWAConnection;
    FDBConnection : TDBConnection;
    function GetConnected: boolean; override;
  public
    constructor Create(AAppModules : TAppModules);
    destructor Destroy; override;
    function ExecSQL(ASQL: string; AShowErrors: boolean = True): boolean; override;
    function ExecMultipleSQL(AMultipleSQL: array of string; AShowErrors: boolean = True): boolean; override;
    function DropTable(ATableName: string): boolean; override;
    function CreateTable(ATableName: string; AFieldNames: TArrayOfString): boolean; override;
    function LoadTable(ALoadDataSQL: TArrayOfString): boolean; override;
    function CreateQueryDataset: TDWAQuery; override;
    function CreateTableDataset: TDWATable; override;
    function CreateDatasetDataset: TDWADataSet; override;
    function DatabaseName: string; override;
    function Connection: TDWAConnection; override;
    function Commit: boolean; override;
    function Rollback: boolean; override;
    function StartTransaction: boolean; override;
    function InTransaction: boolean; override;
    function GetTableNames ( AContainer : TStrings ) : boolean; override;
    function UpdateBlobField(ATableName,AFieldName,AWhereClause: string; ABlobStream : TStream): boolean; override;
    function UpdateMemoField(ATableName,AFieldName,AWhereClause,AMemoText: string): boolean; override;
  end;

implementation

uses
  UDataSetType,
  SysUtils,
  UErrorHandlingOperations;

function TADODatabaseLayer.Connection: TDWAConnection;
const OPNAME = 'TADODatabaseLayer.DatabaseName';
begin
  Result := nil;
  try
    Result := FDatabase;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TADODatabaseLayer.Create(AAppModules : TAppModules);
const OPNAME = 'TADODatabaseLayer.Create';
begin
  try
    inherited Create(AAppModules);
    FDBConnection         := TDBConnection.Create;
    FDatabase             := TDWAConnection.Create(nil);
    FDatabase.LoginPrompt := False;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

destructor TADODatabaseLayer.Destroy;
const OPNAME = 'TADODatabaseLayer.Destroy';
begin
  try
    Connected := False;
    FDatabase.Close;
    FreeAndNil(FDatabase);
    FreeAndNil(FDBConnection);
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TADODatabaseLayer.DatabaseName: string;
const OPNAME = 'TADODatabaseLayer.DatabaseName';
begin
  Result := '';
  try
    if Connected then
      Result := FDBConnection.ConnectionName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TADODatabaseLayer.GetConnected: boolean;
const OPNAME = 'TADODatabaseLayer.GetConnected';
begin
  Result := False;
  try
    Result := FDatabase.Connected;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TADODatabaseLayer.CreateDatasetDataset: TDWADataSet;
const OPNAME = 'TADODatabaseLayer.CreateQueryDataset';
begin
  Result := nil;
  try
    Result            := TDWADataSet.Create(nil);
    Result.Connection := FDatabase;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TADODatabaseLayer.CreateQueryDataset: TDWAQuery;
const OPNAME = 'TADODatabaseLayer.CreateQueryDataset';
begin
  Result := nil;
  try
    Result            := TDWAQuery.Create(nil);
    Result.Connection := FDatabase;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TADODatabaseLayer.CreateTableDataset: TDWATable;
const OPNAME = 'TADODatabaseLayer.CreateTableDataset';
begin
  Result := nil;
  try
    Result            := TDWATable.Create(nil);
    Result.Connection := FDatabase;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TADODatabaseLayer.ExecSQL(ASQL: string; AShowErrors: boolean = True): boolean;
const OPNAME = 'TADODatabaseLayer.ExecSQL';
var
  LExecSQLQuery: TDWAQuery;
begin
  Result := False;
  try
    LExecSQLQuery := CreateQueryDataset;
    try
      LExecSQLQuery.SQL.Text := ASQL;
      LExecSQLQuery.Prepared := True;
      if AShowErrors then
      begin
        LExecSQLQuery.ExecSQL;
      end
      else
      begin
        try
          LExecSQLQuery.ExecSQL;
        except on E: Exception do
          begin
            FAppModules.GlobalData.SetLastErrorMessage(E.Message);
            FAppModules.GlobalData.SetLastError(0);
            Exit;
          end;
        end;
      end;
      Result := True;
    finally
      LExecSQLQuery.Free;
    end;
  except on E: Exception do
    begin
      E.Message := E.Message + #13#10 + ASQL;
      HandleError(E, OPNAME);
    end;
  end;
end;

function TADODatabaseLayer.ExecMultipleSQL(AMultipleSQL: array of string; AShowErrors: boolean = True): boolean;
const OPNAME = 'TADODatabaseLayer.ExecMultipleSQL';
var
  LIndex: integer;
begin
  Result := True;
  try
    for LIndex := Low(AMultipleSQL) to High(AMultipleSQL) do
    begin
      if (not ExecSQL(AMultipleSQL[LIndex], AShowErrors)) then
      begin
        Result := False;
        break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TADODatabaseLayer.DropTable(ATableName: string): boolean;
const OPNAME = 'TADODatabaseLayer.DropTable';
begin
  Result := False;
  try
    Result := ExecSQL(GetDropTableSQL(ATableName), False);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TADODatabaseLayer.CreateTable(ATableName: string; AFieldNames: TArrayOfString): boolean;
const OPNAME = 'TADODatabaseLayer.CreateTable';
begin
  Result := False;
  try
    Result := ExecSQL(GetCreateTableSQL(ATableName, AFieldNames));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TADODatabaseLayer.LoadTable(ALoadDataSQL: TArrayOfString): boolean;
const OPNAME = 'TADODatabaseLayer.LoadTable';
begin
  Result := False;
  try
    Result := ExecMultipleSQL(ALoadDataSQL);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TADODatabaseLayer.Commit: boolean;
const OPNAME = 'TADODatabaseLayer.Commit';
begin
  Result := False;
  try
    FDatabase.CommitTrans;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TADODatabaseLayer.Rollback: boolean;
const OPNAME = 'TADODatabaseLayer.Rollback';
begin
  Result := False;
  try
    FDatabase.RollbackTrans;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TADODatabaseLayer.StartTransaction: boolean;
const OPNAME = 'TADODatabaseLayer.StartTransaction';
begin
  Result := False;
  try
    FDatabase.BeginTrans;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TADODatabaseLayer.InTransaction: boolean;
const OPNAME = 'TADODatabaseLayer.InTransaction';
begin
  Result := False;
  try
    Result := FDatabase.InTransaction;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TADODatabaseLayer.GetTableNames ( AContainer : TStrings ) : boolean;
const OPNAME = 'TADODatabaseLayer.GetTableNames';
begin
  Result := False;
  try
    FDatabase.GetTableNames ( AContainer );
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;

end;

function TADODatabaseLayer.UpdateMemoField(ATableName, AFieldName,AWhereClause, AMemoText: string): boolean;
const OPNAME = 'TADODatabaseLayer.UpdateMemoField';
var
  LDataSet      : TAbstractModelDataset;
  //LBlobStream   : TStream;
  //LMStream      : TMemoryStream;
  //LDataContainer : TStringList;
  LSQL          : string;
begin
  Result := False;
  try
    ATableName   := Trim(ATableName);
    AFieldName   := Trim(AFieldName);
    AWhereClause := Trim(AWhereClause);
    if(ATableName = '') or (AFieldName = '') then Exit;

    FDatabase.Close;
    FDatabase.Open;
    if CreateDataset(integer(dtExecSQL), LDataSet) then
    begin
      LSQL := UpperCase(AWhereClause);
      if(Pos('WHERE',LSQL) = 1) then
        AWhereClause := Trim(Copy(AWhereClause,6,Length(AWhereClause)));
      LSQL := 'SELECT * FROM '+ ATableName;
      if(AWhereClause <> '') then
        LSQL := LSQL + ' WHERE ' + AWhereClause;

      LDataSet.SetSQL(LSQL);
      LDataSet.SetReadOnly(False);
      LDataSet.DataSet.Open;
      if not (LDataset.DataSet.Eof and LDataset.DataSet.Bof) then
      begin
        while not LDataset.DataSet.Eof do
        begin
          LDataSet.DataSet.Edit;
          if(Trim(AMemoText) = '') then
          begin
            LDataSet.DataSet.FieldByName(AFieldName).Clear;
            LDataSet.DataSet.Post;
            Result := True;
          end
          else
          begin
            LDataSet.DataSet.FieldByName(AFieldName).AsWideString := AMemoText;
            LDataSet.DataSet.Post;
            {LDataContainer := TStringList.Create;
            LMStream := TMemoryStream.Create;
            LBlobStream := LDataSet.DataSet.CreateBlobStream(LDataSet.DataSet.FieldByName(AFieldName),bmWrite);
            try
              LDataContainer.Add(AMemoText);
              LMStream.Clear;
              LMStream.Position := 0;
              LDataContainer.SaveToStream ( LMStream );
              LMStream.Position := 0;
              LBlobStream.CopyFrom(LMStream,0 );
              //LBlobStream.CopyFrom(LMStream,LMStream.Size );
              LDataSet.DataSet.Post;
              Result := True;
            finally
              LDataContainer.Free;
              LMStream.Free;
              //LBlobStream.Free;
            end;}
          end;
          LDataset.DataSet.Next;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TADODatabaseLayer.UpdateBlobField(ATableName, AFieldName, AWhereClause: string; ABlobStream: TStream): boolean;
const OPNAME = 'TADODatabaseLayer.UpdateBlobField';
var
  LTable        : TADOTable;
  LBlobStream   : TADOBlobStream;
begin
  Result := False;
  try
    ATableName   := Trim(ATableName);
    AFieldName   := Trim(AFieldName);
    AWhereClause := Trim(AWhereClause);
    if(ATableName = '') or (AFieldName = '') then Exit;
    if(ABlobStream = nil) then Exit;

    LTable := TADOTable.Create(nil);
    try
      LTable.Connection := FDatabase;
      LTable.TableName    := ATableName;
      LTable.Filter       := AWhereClause;
      LTable.Filtered     := True;
      LTable.Open;
      while not LTable.Eof do
      begin
        LTable.Edit;
        if(ABlobStream.Size = 0) then
        begin
          LTable.FieldByName(AFieldName).Clear;
          LTable.Post;
          Result := True;
        end
        else
        begin
          LBlobStream := TADOBlobStream(LTable.CreateBlobStream(LTable.FieldByName(AFieldName),bmWrite));
          try
            ABlobStream.Position := 0;
            LBlobStream.CopyFrom(ABlobStream,ABlobStream.Size );
            LTable.Post;
            Result := True;
          finally
            LBlobStream.Free;
          end;
        end;
        LTable.Next;
      end;
    finally
      LTable.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
