unit UStudyMetaDataSQLAgent;

interface

uses
  Classes,
  UStudyMetaData,
  UAbstractObject;

type
  TStudyMetaDataSQLAgent = class(TAbstractSQLAgent)
  protected
    function GetScenarioWhereClause: string;
  public
    procedure LoadContextData_StudyMetaData (AContextData : TStringList; AIdentifier : integer);
    function GetStudyMetaDataSQL: string;
    function InsertStudyMetaDataSQL(AIdentifier : integer): string;
    function GetDeleteStudyMetaDataSQL(AIdentifier : integer): string;
    function AddStudyMetaData(AStudyMetaData : TStudyMetaData) : boolean;
    function DeleteStudyMetaData(AStudyMetaData : TStudyMetaData): boolean;
    function GetMaxStudyMetaDataID : integer;
  end;

implementation
uses
  Math,
  SysUtils,
  UConstants,
  UDataSetType,
  UErrorHandlingOperations;

{ TStudyMetaDataSQLAgent }

function TStudyMetaDataSQLAgent.AddStudyMetaData(AStudyMetaData: TStudyMetaData): boolean;
const OPNAME = 'TStudyMetaDataSQLAgent.AddStudyMetaData';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    if(AStudyMetaData = nil) then Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        FAppModules.Database.StartTransaction;
        try
          LDataSet.SetSQL(InsertStudyMetaDataSQL((AStudyMetaData.Identifier)));
          LDataSet.SetParams(['ImportedBy'],       [AStudyMetaData.ImportedBy]);
          LDataSet.SetParams(['ErrorType'],        [AStudyMetaData.ErrorType]);
          LDataSet.SetParams(['ErrorDescription'], [AStudyMetaData.ErrorDescription]);
          LDataSet.SetParams(['StudyErrors'],      [IntToStr(AStudyMetaData.StudyErrors)]);
          LDataSet.SetParams(['CorrectiveAction'], [AStudyMetaData.CorrectiveAction]);
          LDataSet.SetParams(['ReadOnly'],         [BoolToStr(AStudyMetaData.RecordReadOnly,False)]);

          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          FAppModules.StudyArea.LastUpdateDate := Now();

          LImportDate := FAppModules.StudyArea.GetStudyImportDate;
          if LImportDate = NullDateTime then
            FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);
          FAppModules.Database.Commit;
          Result := True;
        except
          FAppModules.Database.Rollback;
          raise;
        end;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudyMetaDataSQLAgent.DeleteStudyMetaData(AStudyMetaData: TStudyMetaData): boolean;
const OPNAME = 'TStudyMetaDataSQLAgent.DeleteStudyMetaData';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    if(AStudyMetaData = nil) then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        FAppModules.Database.StartTransaction;
        try
          LDataSet.SetSQL(GetDeleteStudyMetaDataSQL(AStudyMetaData.Identifier));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          FAppModules.StudyArea.LastUpdateDate := Now();

          LImportDate := FAppModules.StudyArea.GetStudyImportDate;
          if LImportDate = NullDateTime then
            FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);
          FAppModules.Database.Commit;
        except
          FAppModules.Database.Rollback;
          raise;
        end;
        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudyMetaDataSQLAgent.GetDeleteStudyMetaDataSQL(AIdentifier: integer): string;
const OPNAME = 'TStudyMetaDataSQLAgent.GetDeleteStudyMetaDataSQL';
begin
  Result := '';
  try
    Result := 'DELETE * FROM StudyMetaData WHERE ' +
              GetScenarioWhereClause +
              ' AND Identifier = ' + IntToStr(AIdentifier) + 
              ' AND ReadOnly = False';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyMetaDataSQLAgent.GetMaxStudyMetaDataID: integer;
const OPNAME = 'TStudyMetaDataSQLAgent.GetMaxStudyMetaDataID';
var
  LDataSet : TAbstractModelDataset;
  LSQL     : string;
begin
  Result := 0;
  try
    LSQL := 'SELECT MAX(Identifier) AS MaxID FROM StudyMetaData WHERE ' + GetScenarioWhereClause;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(LSQL);
        LDataset.DataSet.Open;
        if not LDataset.DataSet.FieldByName('MaxID').IsNull then
          Result := LDataset.DataSet.FieldByName('MaxID').AsInteger;
        LDataset.DataSet.Close;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyMetaDataSQLAgent.GetScenarioWhereClause: string;
const OPNAME = 'TStudyMetaDataSQLAgent.GetScenarioWhereClause';
begin
  Result := '';
  try
    Result :=
      ' (Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
      ' (StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      ' (SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      ' (Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyMetaDataSQLAgent.GetStudyMetaDataSQL: string;
const OPNAME = 'TStudyMetaDataSQLAgent.GetStudyMetaDataSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
              ' ImportedBy, ErrorType, ErrorDescription, StudyErrors, CorrectiveAction, ReadOnly' +
              ' FROM StudyMetaData A' +
              ' WHERE ' + GetScenarioWhereClause +
              ' ORDER BY A.Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyMetaDataSQLAgent.InsertStudyMetaDataSQL(AIdentifier: integer): string;
const OPNAME = 'TStudyMetaDataSQLAgent.InsertStudyMetaDataSQL';
begin
  Result := '';
  try
    Result := ' INSERT INTO StudyMetaData '+
              ' (Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
              ' ImportedBy, ErrorType, ErrorDescription, StudyErrors, CorrectiveAction, ReadOnly' +
              ') VALUES (' +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ',' +
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ',' +
              IntToStr(AIdentifier) + ',' +
              ' :ImportedBy, :ErrorType, :ErrorDescription, :StudyErrors, :CorrectiveAction, :ReadOnly) ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyMetaDataSQLAgent.LoadContextData_StudyMetaData(AContextData: TStringList; AIdentifier: integer);
const OPNAME = 'TStudyMetaDataSQLAgent.LoadContextData_StudyMetaData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='    + IntToStr(AIdentifier));
  except on E : Exception do HandleError(E,OPNAME); end;
end;

end.
