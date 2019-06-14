//
//
//  UNIT      : Contains TStomsaDataSQLAgent Class
//  AUTHOR    : Titi Ngubane (Arivia)
//  DATE      : 2003/07/10
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UStomsaDataSQLAgent;

interface

uses
  Classes,
  UAbstractObject;

type
  TStomsaDataSQLAgent = class(TAbstractSQLAgent)
  protected
    function GetScenarioWhereClause: string;
  public
    function GetAreaDataSQL: string;
    function GetDeleteAreaDataSQL: string;
    function GetNewAreaDataSQL: string;
    function GetHydrologyFileNamesSQL: string;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

function TStomsaDataSQLAgent.GetScenarioWhereClause: string;
const OPNAME = 'TStomsaDataSQLAgent.GetScenarioWhereClause';
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

function TStomsaDataSQLAgent.GetAreaDataSQL: string;
const OPNAME = 'TStomsaDataSQLAgent.GetAreaDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT  FileName, Area FROM StomsaArea WHERE ' +
              GetScenarioWhereClause ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStomsaDataSQLAgent.GetDeleteAreaDataSQL: string;
const OPNAME = 'TStomsaDataSQLAgent.GetDeleteAreaDataSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM StomsaArea WHERE ' +
              GetScenarioWhereClause ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStomsaDataSQLAgent.GetNewAreaDataSQL: string;
const OPNAME = 'TStomsaDataSQLAgent.GetNewAreaDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO StomsaArea '+
                '(Model, StudyAreaName, SubArea, Scenario, FileName, Area) '+
                'Values '+
                '(:Model, :StudyAreaName, :SubArea, :Scenario, :FileName, :Area)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStomsaDataSQLAgent.GetHydrologyFileNamesSQL: String;
const OPNAME = 'TStomsaDataSQLAgent.GetHydrologyFileNamesSQL';
begin
  Result := '';
  try
    if (LoadSQLFromResource('GetHydrologyFileNames', Result, OPNAME)) then
      ReplaceScenarioParameters(Result, OPNAME);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


