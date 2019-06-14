//
//
//  UNIT      : Contains TBDEDatabaseLayer Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/02/06
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UBDEDatabaseLayer;

interface

uses
  Classes,
  DB,
  UDWADBComponents,
  UAbstractObject,
  USQLDatabaseLayer;

type
  TBDEDatabaseLayer = class(TSQLDatabaseLayer)
  protected
    FDatabase: TDatabase;
    function GetConnected: boolean; override;
    function AliasNameExist(AAliasName: string) : boolean;
  public
    constructor Create(AAppModules : TAppModules);
    destructor Destroy; override;
    function ExecSQL(ASQL: string; AShowErrors: boolean = True): boolean; override;
    function ExecMultipleSQL(AMultipleSQL: array of string; AShowErrors: boolean = True): boolean; override;
    function DropTable(ATableName: string): boolean; override;
    function CreateTable(ATableName: string; AFieldNames: TArrayOfString): boolean; override;
    function LoadTable(ALoadDataSQL: TArrayOfString): boolean; override;
    function CreateQuery: TDWAQuery; override;
    function DatabaseName: string; override;
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

constructor TBDEDatabaseLayer.Create(AAppModules : TAppModules);
const
  OPNAME = 'TBDEDatabaseLayer.Create';
begin
  try
    inherited Create(AAppModules);
    FDatabase := TDatabase.Create(nil);
    FDatabase.HandleShared := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

destructor TBDEDatabaseLayer.Destroy;
const
  OPNAME = 'TBDEDatabaseLayer.Destroy';
begin
  try
    Connected := False;
    FreeAndNil(FDatabase);
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TBDEDatabaseLayer.DatabaseName: string;
const
  OPNAME = 'TBDEDatabaseLayer.DatabaseName';
begin
  Result := '';
  try
    if Assigned(FDatabase) then
      Result := FDatabase.DatabaseName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TBDEDatabaseLayer.GetConnected: boolean;
const
  OPNAME = 'TBDEDatabaseLayer.GetConnected';
begin
  Result := False;
  try
    if Assigned(FDatabase) then
      if FDatabase.Connected then
        Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TBDEDatabaseLayer.CreateQuery: TDWAQuery;
const
  OPNAME = 'TBDEDatabaseLayer.CreateQuery';
begin
  Result := nil;
  try
    Result := TDWAQuery.Create(nil);
    //TDWAQuery(Result).DatabaseName := FDatabase.DatabaseName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TBDEDatabaseLayer.ExecSQL(ASQL: string; AShowErrors: boolean = True): boolean;
const
  OPNAME = 'TBDEDatabaseLayer.ExecSQL';
var
  LExecSQLQuery: TDWAQuery;
begin
  Result := False;
  try
    LExecSQLQuery := TDWAQuery(CreateQuery);
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

function TBDEDatabaseLayer.ExecMultipleSQL(AMultipleSQL: array of string; AShowErrors: boolean = True): boolean;
const
  OPNAME = 'TBDEDatabaseLayer.ExecMultipleSQL';
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

function TBDEDatabaseLayer.DropTable(ATableName: string): boolean;
const
  OPNAME = 'TBDEDatabaseLayer.DropTable';
begin
  Result := False;
  try
    Result := ExecSQL(GetDropTableSQL(ATableName), False);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TBDEDatabaseLayer.CreateTable(ATableName: string; AFieldNames: TArrayOfString): boolean;
const
  OPNAME = 'TBDEDatabaseLayer.CreateTable';
begin
  Result := False;
  try
    Result := ExecSQL(GetCreateTableSQL(ATableName, AFieldNames));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TBDEDatabaseLayer.LoadTable(ALoadDataSQL: TArrayOfString): boolean;
const OPNAME = 'TBDEDatabaseLayer.LoadTable';
begin
  Result := False;
  try
    Result := ExecMultipleSQL(ALoadDataSQL);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TBDEDatabaseLayer.Commit: boolean;
const OPNAME = 'TBDEDatabaseLayer.Commit';
begin
  Result := False;
  try
    FDatabase.Commit;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TBDEDatabaseLayer.Rollback: boolean;
const OPNAME = 'TBDEDatabaseLayer.Rollback';
begin
  Result := False;
  try
    FDatabase.Rollback;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TBDEDatabaseLayer.StartTransaction: boolean;
const OPNAME = 'TBDEDatabaseLayer.StartTransaction';
begin
  Result := False;
  try
    FDatabase.StartTransaction;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TBDEDatabaseLayer.InTransaction: boolean;
const OPNAME = 'TBDEDatabaseLayer.InTransaction';
begin
  Result := False;
  try
    Result := FDatabase.InTransaction;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TBDEDatabaseLayer.GetTableNames ( AContainer : TStrings ) : boolean;
const OPNAME = 'TBDEDatabaseLayer.GetTableNames';
begin
  Result := False;
  try
    FDatabase.GetTableNames ( AContainer );
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;

end;

function TBDEDatabaseLayer.AliasNameExist(AAliasName: string): boolean;
const OPNAME = 'TBDEDatabaseLayer.AliasNameExist';
var
  LAliasNamesContainer: TStringList;
  LIndex : integer;
begin
  Result := False;
  try
    if(Trim(AAliasName) <> '') then
    begin
      LAliasNamesContainer := TStringList.Create;
      try
        AAliasName := UpperCase(AAliasName);
        Session.GetAliasNames(LAliasNamesContainer);
        for LIndex := 0 to LAliasNamesContainer.Count-1 do
        begin
          if(AAliasName = UpperCase(LAliasNamesContainer[LIndex])) then
          begin
            Result := True;
            Break;
          end;
        end;
      finally
        LAliasNamesContainer.Free;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TBDEDatabaseLayer.UpdateMemoField(ATableName, AFieldName,AWhereClause, AMemoText: string): boolean;
const OPNAME = 'TBDEDatabaseLayer.UpdateMemoField';
var
  LDataSet      : TAbstractModelDataset;
  LBlobStream   : TStream;
  LMStream      : TMemoryStream;
  LDataContainer : TStringList;
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
            LDataContainer := TStringList.Create;
            LMStream := TMemoryStream.Create;
            LBlobStream := LDataSet.DataSet.CreateBlobStream(LDataSet.DataSet.FieldByName(AFieldName),bmWrite);
            try
              LDataContainer.Add(AMemoText);
              LMStream.Clear;
              LMStream.Position := 0;
              LDataContainer.SaveToStream ( LMStream );
              LMStream.Position := 0;
              LBlobStream.CopyFrom(LMStream,LMStream.Size );
              LDataSet.DataSet.Post;
              Result := True;
            finally
              LDataContainer.Free;
              LMStream.Free;
              LBlobStream.Free;
            end;
          end;
          LDataset.DataSet.Next;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TBDEDatabaseLayer.UpdateBlobField(ATableName, AFieldName, AWhereClause: string; ABlobStream: TStream): boolean;
const OPNAME = 'TBDEDatabaseLayer.UpdateBlobField';
var
  LTable        : TDWATable;
  LBlobStream   : TBlobStream;
begin
  Result := False;
  try
    ATableName   := Trim(ATableName);
    AFieldName   := Trim(AFieldName);
    AWhereClause := Trim(AWhereClause);
    if(ATableName = '') or (AFieldName = '') then Exit;
    if(ABlobStream = nil) then Exit;

    LTable := TDWATable.Create(nil);
    try
      LTable.DatabaseName := FDatabase.DatabaseName;
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
          LBlobStream := TBlobStream(LTable.CreateBlobStream(LTable.FieldByName(AFieldName),bmWrite));
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
