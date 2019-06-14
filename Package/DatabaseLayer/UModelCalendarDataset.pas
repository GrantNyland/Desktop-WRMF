//
//
//  UNIT      : Contains TModelCalendarDataset Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 05/12/2001
//  COPYRIGHT : Copyright © 2001 DWAF
//
//
unit UModelCalendarDataset;

interface

uses
  UModelDataset;

type
  TModelCalendarDataset = class(TModelDataset)
  protected
    function GetSelectClause: string; override;
    function GetFromClause: string; override;
    function GetWhereClause: string; override;
  public
    function DataSetType: integer; override;
  end;

implementation

uses SysUtils, UDataSetType, UErrorHandlingOperations;

function TModelCalendarDataset.DataSetType: integer;
const OPNAME = 'TModelCalendarDataset.DataSetType';
begin
  Result := integer(dtModelCalendarSelect);
end;

function TModelCalendarDataset.GetSelectClause: string;
const OPNAME = 'TModelCalendarDataset.GetSelectClause';
begin
  Result := '';
  try
    Result :=
    'SELECT 		'+
    ' M.Model	        '+
    ',M.StudyAreaName	'+
    ',M.SubArea	        '+
    ',M.Scenario        '+
    ',M.Month1		'+
    ',M.Month2		'+
    ',M.Month3		'+
    ',M.Month4		'+
    ',M.Month5		'+
    ',M.Month6		'+
    ',M.Month7		'+
    ',M.Month8		'+
    ',M.Month9		'+
    ',M.Month10		'+
    ',M.Month11		'+
    ',M.Month12		'+
    ',D.Days1		'+
    ',D.Days2		'+
    ',D.Days3		'+
    ',D.Days4		'+
    ',D.Days5		'+
    ',D.Days6		'+
    ',D.Days7		'+
    ',D.Days8		'+
    ',D.Days9		'+
    ',D.Days10		'+
    ',D.Days11		'+
    ',D.Days12		';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelCalendarDataset.GetFromClause: string;
const OPNAME = 'TModelCalendarDataset.GetFromClause';
begin
  Result := '';
  try
    Result :=
    'FROM MonthNames M	'+
    ',DaysPerMonth D	';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelCalendarDataset.GetWhereClause: string;
const OPNAME = 'TModelCalendarDataset.GetWhereClause';
begin
  Result := '';
  try
    Result :=
    'WHERE M.Model        = '+ QuotedStr(FAppModules.StudyArea.ModelCode)+
    ' AND M.StudyAreaName = '+ QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
    ' AND M.SubArea	  = '+ QuotedStr(FAppModules.StudyArea.SubAreaCode)+
    ' AND M.Scenario      = '+ QuotedStr(FAppModules.StudyArea.ScenarioCode)+
    ' AND D.Model         = M.Model'+
    ' AND D.StudyAreaName = M.StudyAreaName'+
    ' AND D.SubArea       = M.SubArea'+
    ' AND D.Scenario      = M.Scenario;';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
