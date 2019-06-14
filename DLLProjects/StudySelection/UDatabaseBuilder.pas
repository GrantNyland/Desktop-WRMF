//
//  UNIT      : Contains Class TDatabaseBuilder.
//  AUTHOR    : Philemon Setshedi.
//  DATE      : 2004/03/30
//  COPYRIGHT : Copyright © 2004 DWAF
//
unit UDatabaseBuilder;

interface

uses                                                  
  Classes,
  ComCtrls,
  ExtCtrls,
  SysUtils,
  //arivia.com
  UAbstractObject,
  UProgressDialog,
  USQLScriptList;

type
  TDatabaseBuilder = class(TAbstractAppObject)
  protected
    FStop: boolean;
    procedure HandleSQLException(AProgressUpdateFuntion: TProgressUpdateFuntion; AError,ASQLStatement: string);
    procedure DoScriptRow(AScriptIndex: integer; AScript : TSQLScriptList; AProgressUpdateFuntion: TProgressUpdateFuntion);
    procedure DoScriptEntryGeneral(AScriptIndex: integer; AScript :TSQLScriptList; AProgressUpdateFuntion: TProgressUpdateFuntion);
    procedure DoScriptEntryLoadDataSpecial(AScriptIndex: integer; AScript :TSQLScriptList; AProgressUpdateFuntion: TProgressUpdateFuntion);

  public
    procedure BuildDatabase(AScript: TSQLScriptList; AProgressUpdateFuntion: TProgressUpdateFuntion);
  end;
implementation

uses
  Forms,
  //arivia.com
  UDataSetType,
  UStringFieldOperations,
  UErrorHandlingOperations;//, DB;

procedure TDatabaseBuilder.BuildDatabase(AScript: TSQLScriptList; AProgressUpdateFuntion: TProgressUpdateFuntion);
const OPNAME = 'TDatabaseBuilder.BuildDatabase';
var
  LIndex: integer;
  LStop: boolean;

begin
  try
    FStop := False;
    // Process every script entry.
    for LIndex := 0 to AScript.RowCount - 1 do
    begin
      AProgressUpdateFuntion('Processing script: ' + ExtractFileName(AScript.ScriptFilename[LIndex]), ptNone, LStop);
      if LStop then Exit;
      DoScriptRow(LIndex, AScript,AProgressUpdateFuntion);
      if FStop then
        Break;
    end;

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDatabaseBuilder.DoScriptRow(AScriptIndex: integer; AScript :TSQLScriptList;
          AProgressUpdateFuntion: TProgressUpdateFuntion);
const OPNAME = 'TDatabaseBuilder.DoScriptRow';
begin
  try
    case AScript.ScriptType[AScriptIndex] of
      sftCreateIndex,
      sftForeignKey,
      sftStoredProcedure,
      sftLoadData         : DoScriptEntryGeneral(AScriptIndex, AScript,AProgressUpdateFuntion);
      sftLoadDataSpecial  : DoScriptEntryLoadDataSpecial(AScriptIndex, AScript,AProgressUpdateFuntion);
    else
      raise Exception.CreateFmt('Illegal SQL type [%d].', [integer(AScript.ScriptType[AScriptIndex])]);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDatabaseBuilder.DoScriptEntryGeneral(AScriptIndex: integer;
          AScript :TSQLScriptList; AProgressUpdateFuntion: TProgressUpdateFuntion);
const OPNAME = 'TDatabaseBuilder.DoScriptEntryGeneral';
var
  LScriptEntry: TSQLScriptEntry;
  LSQLStatmentIndex: integer;
begin
  try
    if AScript.IsScriptEnabled(AScriptIndex) then
    begin
      try
        AScript.EndOfFile := false;
        AScript.LineEndedOn := 0;
        while (true) do
        begin
          if (AScript.EndOfFile) then
            break;
          if AScript.LoadScriptEntry(AScriptIndex) then
          begin
            for LSQLStatmentIndex := 0 to AScript.RowStatementsCount(AScriptIndex) -1 do
            begin
              LScriptEntry := AScript.ScriptEntry(AScriptIndex, LSQLStatmentIndex);
              if LScriptEntry.Enabled then
              begin
                if not  FAppModules.Database.ExecSQL(LScriptEntry.SQL,False) then
                  HandleSQLException(AProgressUpdateFuntion,
                                     FAppModules.GlobalData.GetLastErrorMessage,
                                     LScriptEntry.SQL);
              end;
              if FStop then
                Break;
            end;
          end;
        end;
      finally
        AScript.AddDefaultScriptEntry(AScriptIndex);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDatabaseBuilder.DoScriptEntryLoadDataSpecial(AScriptIndex: integer;
          AScript: TSQLScriptList; AProgressUpdateFuntion: TProgressUpdateFuntion);
const OPNAME = 'TDatabaseBuilder.DoScriptEntryLoadDataSpecial';
var
  LScriptEntry: TSQLScriptEntry;
  LSQLStatmentIndex, LFieldIndex: integer;
  LDataFields: TStringList;
  LDataSet: TAbstractModelDataset;
begin
  try
    if AScript.IsScriptEnabled(AScriptIndex) then
    begin
      try
        if AScript.LoadScriptEntry(AScriptIndex) then
        begin
          LDataFields := TStringList.Create;
          try
            if FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet) then
            begin
              try
                for LSQLStatmentIndex := 0 to AScript.RowStatementsCount(AScriptIndex) - 1 do
                begin
                  LScriptEntry := AScript.ScriptEntry(AScriptIndex, LSQLStatmentIndex);
                  if LScriptEntry.Enabled then
                  begin
                    // Extract the fields for the record.
                    ExtractFields(LScriptEntry.SQL, '|', LDataFields);
                    if (LDataFields.Count > 0) then
                      LDataFields.Delete(0);
                    if (LDataFields.Count > 1) then
                      LDataFields.Delete(LDataFields.Count - 1);

                    // Create a generic SQL.
                    LDataSet.SetSQL(' SELECT * FROM ' + LScriptEntry.TableName + ' WHERE (1 <> 1) ');
                    LDataSet.SetReadOnly(True);
                    //LDataSet.SetReadOnly(False);
                    LDataSet.DataSet.Open;
                    try
                      LDataSet.DataSet.Append;
                      for LFieldIndex := 0 to LDataSet.DataSet.FieldCount - 1 do
                        if (LFieldIndex < LDataFields.Count) then
                          LDataSet.DataSet.Fields[LFieldIndex].AsString := LDataFields[LFieldIndex];
                      try
                        LDataSet.DataSet.Post;
                      except
                        on E: Exception do
                        HandleSQLException(AProgressUpdateFuntion,E.Message,
                         FAppModules.Language.GetString('DatabaseBuilder.AppendTable')+LScriptEntry.TableName+FAppModules.Language.GetString('DatabaseBuilder.Failed'));
                      end;
                    finally
                      LDataSet.DataSet.Close;
                    end;
                    if FStop then
                      Break;
                  end;
                end;
              finally
                LDataSet.Free;
              end;
            end;
          finally
            LDataFields.Free;
          end;
        end;
      finally
        AScript.AddDefaultScriptEntry(AScriptIndex);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDatabaseBuilder.HandleSQLException(AProgressUpdateFuntion: TProgressUpdateFuntion;
          AError,ASQLStatement: string);
const OPNAME = 'TDatabaseBuilder.HandleSQLException';
var
  LError: string;
begin
  try
    LError := #13#10 + AError + #13#10 + ASQLStatement;
    if Assigned(AProgressUpdateFuntion) then
    begin
      AProgressUpdateFuntion(LError,ptError,FStop);
    end
    else
      Raise Exception.Create(LError);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
