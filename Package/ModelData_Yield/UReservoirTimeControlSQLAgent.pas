//
//
//  UNIT      : Contains TReservoirHeadingSQLAgent Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/02/11
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UReservoirTimeControlSQLAgent;

interface

uses
  Classes,
  VoaimsCom_TLB,
  UAbstractObject;

type
  TReservoirTimeControlSQLAgent = class(TAbstractSQLAgent)
  private
  protected
    function GetScenarioWhereClause: string;
    function GetNoAliasScenarioWhereClause: string;
    function GetMaxRecordIdentifier: integer;
  public
    procedure LoadContextData_ReservoirTimeControl (AContextData      : TStringList;
                                                    ARecordIdentifier : string);
    function GetReservoirTimeControlSQL: string;
    function GetReservoirTimeControlByBaseresevoirNrSQL(ABaseNodeNumber : string): string;
    function InsertReservoirTimeControl(AResNr: integer): boolean;
    function DeleteReservoirTimeControl(AResNr: integer): boolean;

    function GetDeleteTimeControlSQL(AIdentifier: integer): string;
  end;

implementation

uses
  Math,
  SysUtils,
  UConstants,
  UDataSetType,
  UErrorHandlingOperations;

function TReservoirTimeControlSQLAgent.GetScenarioWhereClause: string;
const OPNAME = 'TReservoirTimeControlSQLAgent.GetScenarioWhereClause';
begin
  Result := '';
  try
    Result :=
      ' (A.Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
      ' (A.StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      ' (A.SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      ' (A.Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirTimeControlSQLAgent.GetNoAliasScenarioWhereClause: string;
const OPNAME = 'TReservoirTimeControlSQLAgent.GetNoAliasScenarioWhereClause';
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


procedure TReservoirTimeControlSQLAgent.LoadContextData_ReservoirTimeControl (AContextData      : TStringList;
                                                                              ARecordIdentifier : string);
const OPNAME = 'TReservoirTimeControlSQLAgent.LoadContextData_ReservoirTimeControl';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('ReservoirNumber='    + ARecordIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;


function TReservoirTimeControlSQLAgent.GetReservoirTimeControlSQL: string;
const OPNAME = 'TReservoirTimeControlSQLAgent.GetReservoirTimeControlSQL';
begin
  Result := '';
  try
    Result := 'SELECT ' +
      'Model, StudyAreaName, SubArea, Scenario, ReservoirNumber, BaseNodeNumber, ' +
      'ReservoirStartYear, ReservoirStartMonth, ReservoirEndYear, ReservoirEndMonth, ' +
      'ReservoirEconomicLife, ReservoirCapitalCost, ReservoirOMCost, ' +
      ' ReservoirCostSchedule ' +
      ' FROM  ReservoirTimeControl '+
      ' WHERE Model       =  ' + QuotedStr(FAppModules.StudyArea.ModelCode)+
      ' AND StudyAreaName =  ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
      ' AND SubArea       =  ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)+
      ' AND Scenario      =  ' + QuotedStr(FAppModules.StudyArea.ScenarioCode) +
      ' ORDER BY ReservoirNumber';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirTimeControlSQLAgent.GetReservoirTimeControlByBaseresevoirNrSQL(ABaseNodeNumber : string): string;
const OPNAME = 'TReservoirTimeControlSQLAgent.GetReservoirTimeControlByBaseresevoirNrSQL';
begin
  Result := '';
  try
    Result := 'SELECT ' +
      'Model, StudyAreaName, SubArea, Scenario, ReservoirNumber, BaseNodeNumber, ' +
      'ReservoirStartYear, ReservoirStartMonth, ReservoirEndYear, ReservoirEndMonth, ' +
      'ReservoirEconomicLife, ReservoirCapitalCost, ReservoirOMCost, ' +
      ' ReservoirCostSchedule ' +
      ' FROM  ReservoirTimeControl '+
      ' WHERE Model       =  ' + QuotedStr(FAppModules.StudyArea.ModelCode)+
      ' AND StudyAreaName =  ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
      ' AND SubArea       =  ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)+
      ' AND Scenario      =  ' + QuotedStr(FAppModules.StudyArea.ScenarioCode) +
      ' AND BaseNodeNumber = ' +ABaseNodeNumber+
      ' ORDER BY ReservoirNumber';
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TReservoirTimeControlSQLAgent.InsertReservoirTimeControl(AResNr: integer): boolean;
const OPNAME = 'TReservoirTimeControlSQLAgent.InsertReservoirTimeControl';
var
  LDataSet     : TAbstractModelDataset;
  LImportDate  : TDateTime;
  lSQL         : string;
  LMaxRecordID : integer;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LMaxRecordID := GetMaxRecordIdentifier+1;
      if Assigned(LDataSet) then
      begin
        lSQL := 'INSERT INTO ReservoirTimeControl '+
                  '(Model, StudyAreaName, SubArea, Scenario,Identifier,ReservoirNumber, BaseNodeNumber, ' +
                  'ReservoirStartYear, ReservoirStartMonth, ReservoirEndYear, ReservoirEndMonth, ' +
                  'ReservoirEconomicLife, ReservoirCapitalCost, ReservoirOMCost' +
                  ') VALUES (' +
                  QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
                  QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
                  QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
                  QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
                  IntToStr(LMaxRecordID) + ',' + IntToStr(AResNr) + ',' + IntToStr(AResNr) + ','+
                  '0,0,0,0,0,0.0,0.0)';

        LDataSet.SetSQL(lSQL);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirTimeControlSQLAgent.GetDeleteTimeControlSQL(AIdentifier: integer): string;
const OPNAME = 'TReservoirTimeControlSQLAgent.GetDeleteTimeControlSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM ReservoirTimeControl A WHERE ' +
                GetScenarioWhereClause +
                'AND A.ReservoirNumber = '+ IntToStr(AIdentifier);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirTimeControlSQLAgent.GetMaxRecordIdentifier: integer;
const OPNAME = 'TWetlandSQLAgent.GetMaxRecordIdentifier';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL('SELECT MAX(Identifier) AS MaxIdentifier FROM ReservoirTimeControl A WHERE ' + GetScenarioWhereClause);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxIdentifier').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirTimeControlSQLAgent.DeleteReservoirTimeControl(AResNr: integer): boolean;
const OPNAME = 'TReservoirTimeControlSQLAgent.DeleteReservoirTimeControl';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetDeleteTimeControlSQL(AResNr));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

end.


