//
//
//  UNIT      : Contains TDemandHydrologyDataSQLAgent Class
//  AUTHOR    : Dziedzi Ramulondi (Arivia)
//  DATE      : 2003/07/22
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UDemandHydrologyDataSQLAgent;

interface

uses
  Classes,
  UAbstractObject;

type
  TDemandHydrologyDataSQLAgent = class(TAbstractSQLAgent)
  protected
    function GetScenarioWhereClause: string;
  public

    function GetHydrologyDataSQL(AHydrologyFileName: string): string;
    function GetDemandDataSQL(ADemandFileName: string): string;
    function GetNodeReferenceFlowDataSQL(AHydrologyFileName: string): string;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

function TDemandHydrologyDataSQLAgent.GetScenarioWhereClause: string;
const OPNAME = 'TDemandHydrologyDataSQLAgent.GetScenarioWhereClause';
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

function TDemandHydrologyDataSQLAgent.GetHydrologyDataSQL(AHydrologyFileName: string): string;
const OPNAME = 'TDemandHydrologyDataSQLAgent.GetHydrologyDataSQL';
begin
  Result := '';
  try
    AHydrologyFileName := UpperCase(ExtractFileName(Trim(AHydrologyFileName)));
    Result := 'SELECT StudyAreaName,FileName, Identifier, HydroYearValue,' +
              '  HydroMonthValue01, HydroMonthValue02, HydroMonthValue03, HydroMonthValue04, HydroMonthValue05'+
              ', HydroMonthValue06, HydroMonthValue07, HydroMonthValue08, HydroMonthValue09, HydroMonthValue10' +
              ', HydroMonthValue11, HydroMonthValue12, HydroTotalValue'+
              ' FROM HydrologyFileData '+
              ' WHERE FileName = ' + QuotedStr(AHydrologyFileName) + ' AND Identifier IS NOT NULL'+
              ' AND StudyAreaName = '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' ORDER BY FileName, Identifier';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandHydrologyDataSQLAgent.GetDemandDataSQL(ADemandFileName: string): string;
const OPNAME = 'TDemandHydrologyDataSQLAgent.GetDemandDataSQL';
begin
  Result := '';
  try
    ADemandFileName := UpperCase(ExtractFilename(ADemandFileName));
    Result :=
      ' SELECT           ' +
      '   A.Model,A.StudyAreaName, A.Scenario, A.Identifier, A.FileNumber, B.FileName, A.DemandYearValue,'+
      '   A.DemandMonthValue01, A.DemandMonthValue02, A.DemandMonthValue03, A.DemandMonthValue04,' +
      '   A.DemandMonthValue05, A.DemandMonthValue06, A.DemandMonthValue07, A.DemandMonthValue08,' +
      '   A.DemandMonthValue09, A.DemandMonthValue10, A.DemandMonthValue11, A.DemandMonthValue12, A.DemandTotalValue' +
      '   FROM DemandFileData A,FileNames B '+
      '   WHERE '+ GetScenarioWhereClause + ' AND ' +
      '  (A.FileType > 100) AND' +
      '  (B.Model             = A.Model) AND ' +
      '  (B.StudyAreaName     = A.StudyAreaName) AND ' +
      '  (B.SubArea           = A.SubArea) AND ' +
      '  (B.Scenario          = A.Scenario) AND ' +
      '  (A.FileNumber        = B.Identifier)  AND ' +
      '  (B.FileName          = ' + QuotedStr(ADemandFileName) + ' AND' +
      '  (B.FileGroup = 4)' +
      '  ORDER BY A.FileNumber, A.DemandYearValue';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandHydrologyDataSQLAgent.GetNodeReferenceFlowDataSQL(AHydrologyFileName: string): string;
const OPNAME = 'TDemandHydrologyDataSQLAgent.GetNodeReferenceFlowDataSQL';
begin
  Result := '';
  try
    if(Trim(AHydrologyFileName) = '') then Exit;
    AHydrologyFileName := UpperCase(ExtractFileName(Trim(AHydrologyFileName)));
    Result := 'SELECT HydroYearValue,' +
              '  HydroMonthValue01, HydroMonthValue02, HydroMonthValue03, HydroMonthValue04, HydroMonthValue05'+
              ', HydroMonthValue06, HydroMonthValue07, HydroMonthValue08, HydroMonthValue09, HydroMonthValue10' +
              ', HydroMonthValue11, HydroMonthValue12'+
              ' FROM HydrologyFileData '+
              ' WHERE FileName = ' + QuotedStr(AHydrologyFileName) +
              ' AND StudyAreaName = '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' ORDER BY HydroYearValue';

  except on E: Exception do HandleError(E, OPNAME) end;
end;
end.


