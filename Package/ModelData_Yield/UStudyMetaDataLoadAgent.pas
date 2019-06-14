unit UStudyMetaDataLoadAgent;

interface
uses
  Classes,
  DBClient,
  UStudyMetaDataSQLAgent,
  UStudyMetaData,
  UAbstractObject;

type
  TStudyMetaDataLoadAgent = class(TAbstractAppObject)
  protected
    FSQLAgent : TStudyMetaDataSQLAgent;
    
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function ConstructData(AStudyMetaDataList : TStudyMetaDataList) : boolean;
    function ConstructDataFromDataSet(AStudyMetaDataList: TStudyMetaDataList;AClientDataSet : TClientDataSet) : boolean;
  end;

implementation
uses
  SysUtils,
  UDataSetType,
  UErrorHandlingOperations, DB;

{ TStudyMetaDataLoadAgent }

function TStudyMetaDataLoadAgent.ConstructData(AStudyMetaDataList: TStudyMetaDataList): boolean;
const OPNAME = 'TStudyMetaDataLoadAgent.ConstructData';
var
  LDataSet          : TAbstractModelDataset;
  LStudyMetaData    : TStudyMetaData;
  LIdentifier,
  LStudyErrors      : integer;
  LStudyAreaName,
  LImportedBy,
  LErrorType,
  LErrorDescription,
  LCorrectiveAction : string;
  LReadOnly         : boolean;
begin
  Result := False;
  try
    if(AStudyMetaDataList = nil) then Exit;
    AStudyMetaDataList.Initialise;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(FSQLAgent.GetStudyMetaDataSQL);
      LDataset.DataSet.Open;
      while not LDataSet.DataSet.Eof do
      begin
        LIdentifier       := LDataset.DataSet.FieldByName('Identifier').AsInteger;
        LStudyErrors      := LDataset.DataSet.FieldByName('StudyErrors').AsInteger;
        LStudyAreaName    := Trim(LDataset.DataSet.FieldByName('StudyAreaName').AsString);
        LImportedBy       := Trim(LDataset.DataSet.FieldByName('ImportedBy').AsString);
        LErrorType        := Trim(LDataset.DataSet.FieldByName('ErrorType').AsString);
        LErrorDescription := Trim(LDataset.DataSet.FieldByName('ErrorDescription').AsString);
        LCorrectiveAction := Trim(LDataset.DataSet.FieldByName('CorrectiveAction').AsString);
        LReadOnly         := LDataset.DataSet.FieldByName('ReadOnly').AsBoolean; 

        LStudyMetaData := AStudyMetaDataList.NewStudyMetaData;
        LStudyMetaData.Populate(LIdentifier,LImportedBy,LErrorType,LErrorDescription,LStudyErrors,LCorrectiveAction,LStudyAreaName,LReadOnly);

        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;

      Result := True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TStudyMetaDataLoadAgent.ConstructDataFromDataSet(AStudyMetaDataList: TStudyMetaDataList; AClientDataSet: TClientDataSet): boolean;
const OPNAME = 'TStudyMetaDataLoadAgent.ConstructDataFromDataSet';
var
  LIdentifier,
  LStudyErrors      : integer;
  LStudyAreaName,
  LImportedBy,
  LErrorType,
  LErrorDescription,
  LCorrectiveAction : string;
  LStudyMetaData    : TStudyMetaData;
  LReadOnly         : boolean;
begin
  Result := False;
  try
    if not Assigned(AClientDataSet) and (AStudyMetaDataList = nil) then exit;

    AClientDataSet.Active := True;
    while not AClientDataSet.Eof do
    begin
      LIdentifier       := AClientDataSet.FieldByName('Identifier').AsInteger;
      LStudyErrors      := AClientDataSet.FieldByName('StudyErrors').AsInteger;
      LStudyAreaName    := Trim(AClientDataSet.FieldByName('StudyAreaName').AsString);
      LImportedBy       := Trim(AClientDataSet.FieldByName('ImportedBy').AsString);
      LErrorType        := Trim(AClientDataSet.FieldByName('ErrorType').AsString);
      LErrorDescription := Trim(AClientDataSet.FieldByName('ErrorDescription').AsString);
      LCorrectiveAction := Trim(AClientDataSet.FieldByName('CorrectiveAction').AsString);
      LReadOnly         := AClientDataSet.FieldByName('ReadOnly').AsBoolean; 

      LStudyMetaData := AStudyMetaDataList.NewStudyMetaData;
      LStudyMetaData.Populate(LIdentifier,LImportedBy,LErrorType,LErrorDescription,LStudyErrors,LCorrectiveAction,LStudyAreaName,LReadOnly);
      AClientDataSet.Next;
    end;
    AClientDataSet.Close;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudyMetaDataLoadAgent.CreateMemberObjects;
const OPNAME = 'TStudyMetaDataLoadAgent.CreateMemberObjects';
begin
  inherited;
  try
    FSQLAgent :=  TStudyMetaDataSQLAgent.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudyMetaDataLoadAgent.DestroyMemberObjects;
const OPNAME = 'TStudyMetaDataLoadAgent.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FSQLAgent);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
 