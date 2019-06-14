{******************************************************************************}
{*  UNIT      : Contains the class TSwitchDefinitionLoadAgent.                *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2006/02/27                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit USwitchDefinitionLoadAgent;

interface

uses
  Classes,
  USwitchDefinition,
  UAbstractObject,
  VoaimsCom_TLB;

type
  TSwitchDefinitionLoadAgent = class(TAbstractAppObject)
  protected
    function GetScenarioWhereClause: string;
    function PopulateSwitchDefinition (ADataSet   : TAbstractModelDataset;
                                       ASwitchDef : TSwitchDefinition): boolean;
    function GetLastSwitchDefinitionID : integer;
    function GetFileNameSQL: string;
    function InsertFileNameSQL: string;
    function DeleteFileNamesTableSQL: string;
  public
    function LoadSwitchDefinitions (ASwitchDefList : TSwitchDefinitionsList): boolean;
    function InsertSwitchDefinition (var ASwitchDefID : integer) : Boolean;
    function DeleteSwitchDefinition (ASwitchDefID : integer): boolean;
    function InsertFileToDatabase(AFileNumber : integer;AFileName:string): boolean;
    function DeleteFileNamesTable(AFileNumber : integer; AFileName : string) : boolean;

    procedure LoadContextData_SwitchDefID (AContextData : TStringList;
                                           ASwitchDefID : string);
  end;

implementation

uses
  Math,
  SysUtils,
  UStringDateTimeOperations,
  UConstants,
  UDataSetType,
  UErrorHandlingOperations;

procedure TSwitchDefinitionLoadAgent.LoadContextData_SwitchDefID (AContextData : TStringList;
                                                                  ASwitchDefID : string);
const OPNAME = 'TSwitchDefinitionLoadAgent.LoadContextData_SwitchDefID';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='         + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='       + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='      + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('SwitchDefID='   + ASwitchDefID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TSwitchDefinitionLoadAgent.GetScenarioWhereClause : string;
const OPNAME = 'TSwitchDefinitionLoadAgent.GetScenarioWhereClause';
begin
  Result := '';
  try
    Result :=
      ' (Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
      ' (StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      ' (SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      ' (Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSwitchDefinitionLoadAgent.LoadSwitchDefinitions (ASwitchDefList : TSwitchDefinitionsList): boolean;
const OPNAME = 'TSwitchDefinitionLoadAgent.LoadSwitchDefinitions';
var
  lDataSet     : TAbstractModelDataset;
  lSwitchDef   : TSwitchDefinition;
  lSQL         : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) AND (ASwitchDefList <> nil) then
      begin
        lSQL := 'SELECT * FROM ChannelSwitchDefinition WHERE ' +
                GetScenarioWhereClause ;
        lDataSet.SetSQL(lSQL);
        lDataset.DataSet.Open;
        Result := True;
        ASwitchDefList.Initialise;
        while (NOT lDataset.DataSet.EOF) do
        begin
          lSwitchDef := ASwitchDefList.CreateSwitchDefinition;
          if (NOT PopulateSwitchDefinition(lDataset, lSwitchDef)) then
            ASwitchDefList.DeleteSwitchDefinitionWithID(lSwitchDef.SwitchDefID);
          lDataset.DataSet.Next;
        end;
        lDataset.DataSet.Close;
      end;
    finally
      lDataset.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TSwitchDefinitionLoadAgent.DeleteFileNamesTableSQL: string;
const OPNAME = 'TSwitchDefinitionLoadAgent.DeleteFileNamesTableSQL';
begin
  Result := '';
  try
    Result := ' DELETE * FROM FileNames WHERE '+
              GetScenarioWhereClause + ' AND ( Identifier = :Identifier ) AND ( FileName = :FileName )AND ( FileGroup = :FileGroup ) ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TSwitchDefinitionLoadAgent.DeleteFileNamesTable(AFileNumber : integer; AFileName : string) : boolean;
const OPNAME = 'TSwitchDefinitionLoadAgent.DeleteFileNamesTable';
var
  LDataSet : TAbstractModelDataset;
begin
   Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(DeleteFileNamesTableSQL);
      LDataSet.ClearQueryParams();
      LDataSet.SetParams(['Identifier'], [IntToStr(AFileNumber)]);
      LDataSet.SetParams(['FileName'], [AFileName]);
      LDataSet.SetParams(['FileGroup'], ['19']);
      LDataSet.ExecSQL;
    finally
      LDataSet.Free;
    end;

    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TSwitchDefinitionLoadAgent.InsertFileNameSQL: string;
const OPNAME = 'TSwitchDefinitionLoadAgent.InsertFileNameSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO FileNames'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,FileName,FileGroup,ImportDate,FileDate)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:FileName,:FileGroup,:ImportDate,:FileDate)';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSwitchDefinitionLoadAgent.GetFileNameSQL: string;
const OPNAME = 'TSwitchDefinitionLoadAgent.GetFileNameSQL';
begin
  Result := '';
  try
    Result :='SELECT * FROM FileNames WHERE ' +
                GetScenarioWhereClause + ' AND ( FileName = :FileName ) AND ( FileGroup = :FileGroup ) ';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSwitchDefinitionLoadAgent.InsertFileToDatabase(AFileNumber : integer;AFileName:string): boolean;
const OPNAME = 'TSwitchDefinitionLoadAgent.InsertFileToDatabase';
var
  LDataSet : TAbstractModelDataset;
begin
   Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(GetFileNameSQL);
      LDataSet.ClearQueryParams();
      LDataSet.SetParams(['FileName'], [AFileName]);
      LDataSet.SetParams(['FileGroup'], ['19']);
      lDataset.DataSet.Open;
      lDataset.DataSet.Last;
      lDataset.DataSet.First;
      if (lDataset.DataSet.RecordCount = 0) then
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(InsertFileNameSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(AFileNumber)]);
        LDataSet.SetParams(['FileName'], [AFileName]);
        LDataSet.SetParams(['FileGroup'], ['19']);
        LDataSet.SetParams(['ImportDate'], [DateTimeToStamp(Now)]);
        LDataSet.SetParams(['FileDate'], [DateTimeToStamp(Now)]);
        LDataSet.ExecSQL;
      end;
    finally
      LDataSet.Free;
    end;

    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TSwitchDefinitionLoadAgent.PopulateSwitchDefinition
                                            (ADataSet  : TAbstractModelDataset;
                                             ASwitchDef : TSwitchDefinition): boolean;
const OPNAME = 'TSwitchDefinitionLoadAgent.PopulateSwitchDefinition';
var
  lSwitchDefID              : integer;
  lStartYear                : integer;
  lStartMonth               : integer;
  lFileName                 : string;
begin
  Result := FALSE;
  try
    if Assigned(ADataSet) and Assigned(ASwitchDef) then
    begin
      lSwitchDefID   := ADataSet.DataSet.FieldByName('SwitchDefID').AsInteger;
      lStartYear     := ADataSet.DataSet.FieldByName('SwitchDefStartYear').AsInteger;
      lStartMonth    := ADataSet.DataSet.FieldByName('SwitchDefStartMonth').AsInteger;
      lFileName      := Trim(ADataSet.DataSet.FieldByName('SwitchDefFileName').AsString);
      Result :=  ASwitchDef.Populate
                   (lSwitchDefID, lStartYear, lStartMonth, lFileName);
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TSwitchDefinitionLoadAgent.InsertSwitchDefinition (var ASwitchDefID : integer): boolean;
const OPNAME = 'TSwitchDefinitionLoadAgent.InsertSwitchDefinition';
var
  lDataSet       : TAbstractModelDataset;
  lSwitchDefID    : integer;
  lImportDate    : TDateTime;
  lSQL           : string;
  LDataFilePrefix : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin

        lSwitchDefID := GetLastSwitchDefinitionID + 1;
        lSQL:= 'INSERT  INTO ChannelSwitchDefinition ' +
              '(Model, StudyAreaName, SubArea, Scenario, SwitchDefID, SwitchDefFileName) ' +
              'VALUES (' +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
              IntToStr(lSwitchDefID) + ','+
              QuotedStr('SW' + IntToStr(lSwitchDefID) + '.DAT') + ')';

        lDataSet.SetSQL(lSQL);
        lDataset.ExecSQL;
        lDataset.DataSet.Close;

        ASwitchDefID := lSwitchDefID;
        FAppModules.StudyArea.LastUpdateDate := Now();

        lImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if (lImportDate = NullDateTime) then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        LDataFilePrefix := Uppercase((FAppModules.Model.ModelData as IYieldModelData).DataFilePaths.DataFilePrefix);
        Result := InsertFileToDatabase(lSwitchDefID, LDataFilePrefix+ 'SW' + IntToStr(lSwitchDefID) + '.DAT')
      end;
    finally
      lDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TSwitchDefinitionLoadAgent.GetLastSwitchDefinitionID : integer;
const OPNAME = 'TSwitchDefinitionLoadAgent.GetLastSwitchDefinitionID';
var
  lDataSet : TAbstractModelDataset;
  lSQL     : string;
begin
  Result := 0;
  try
    lSQL := 'SELECT MAX(SwitchDefID) AS LastID FROM ChannelSwitchDefinition WHERE ' +
            GetScenarioWhereClause;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(lSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('LastID').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSwitchDefinitionLoadAgent.DeleteSwitchDefinition (ASwitchDefID : integer): boolean;
const OPNAME = 'TSwitchDefinitionLoadAgent.DeleteSwitchDefinition';
var
  lDataSet    : TAbstractModelDataset;
  lImportDate : TDateTime;
  lSQL        : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        FAppModules.Database.StartTransaction;
        try
          lSQL := 'DELETE * FROM ChannelSwitchDefinition WHERE ' +
                  GetScenarioWhereClause + ' AND SwitchDefID = ' + IntToStr(ASwitchDefID);
          LDataSet.SetSQL(lSQL);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          FAppModules.StudyArea.LastUpdateDate := Now();
          lImportDate := FAppModules.StudyArea.GetStudyImportDate;
          if (lImportDate = NullDateTime) then
            FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

          FAppModules.Database.Commit;
          Result := TRUE;
        except
          FAppModules.Database.Rollback;
          raise;
        end;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

end.
