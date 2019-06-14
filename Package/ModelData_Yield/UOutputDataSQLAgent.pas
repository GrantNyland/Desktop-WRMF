//
//
//  UNIT      : Contains TOutputDataSQLAgent Class
//  AUTHOR    : Dziedzi Ramulondi (Aravia)
//  DATE      : 2005/05/19
//  COPYRIGHT : Copyright © 2005 DWAF
//
//
unit UOutputDataSQLAgent;

interface

uses
  Classes,
  UAbstractObject;

type
  TOutputDataSQLAgent = class(TAbstractSQLAgent)
  protected
    function GetScenarioWhereClause: string;
  public
    function GetGenericBlockDescriptionSQL: string;
    function GetPlotElemetSQL: string;
    function GetBlockHeaderSQL(ABlockNumber,ALoadCaseNumber,ASequenceNumber: integer): string;
    function GetGenericBlockValuesSQL(ABlockNumber,ALoadCaseNumber,ASequenceNumber: integer): string;
    function GetBlockAverageChannelFlowSQL(ABlockNumber,ALoadCaseNumber,ASequenceNumber: integer): string;
    function GetSumOutBlobSQL: string;
  end;

implementation

uses
  SysUtils,
  UDataSetType,
  UErrorHandlingOperations;

function TOutputDataSQLAgent.GetScenarioWhereClause: string;
const OPNAME = 'TOutputDataSQLAgent.GetScenarioWhereClause';
begin
  Result := '';
  try
    Result :=
      ' (A.Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
      ' (A.StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      ' (A.SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      ' (A.Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDataSQLAgent.GetGenericBlockDescriptionSQL: string;
const OPNAME = 'TOutputDataSQLAgent.GetGenericBlockDescriptionSQL';
begin
  Result := '';
  try
    Result := 'SELECT A.Model,A.StudyAreaName,A.SubArea,A.Scenario,A.BlockNumber,A.LoadCaseNumber,A.SequenceNumber,'+
              'A.BlockType,A.ElementID,A.BlockHeading,A.BlockTitle,AnnualWaterDemand,AnnualPowerDemand  FROM suBlockDescription A'+
              ' WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDataSQLAgent.GetBlockHeaderSQL(ABlockNumber,ALoadCaseNumber,ASequenceNumber: integer): string;
const OPNAME = 'TOutputDataSQLAgent.GetBlockHeaderSQL';
begin
  Result := '';
  try
    Result := 'SELECT A.Model,A.StudyAreaName,A.SubArea,A.Scenario,A.BlockNumber,A.LoadCaseNumber,A.SequenceNumber,A.Identifier,' +
              ' A.LineData  FROM suBlockHeader A'+
              ' WHERE ' +
              GetScenarioWhereClause +
              ' AND (A.BlockNumber = ' + IntToStr(ABlockNumber) + ')'+
              ' AND (A.LoadCaseNumber   = ' + IntToStr(ALoadCaseNumber) + ')'+
              ' AND (A.SequenceNumber   = ' + IntToStr(ASequenceNumber) + ')'+
              ' AND (A.Identifier  IN (2,8) )'+
              ' ORDER BY Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDataSQLAgent.GetGenericBlockValuesSQL(ABlockNumber,ALoadCaseNumber,ASequenceNumber: integer): string;
const OPNAME = 'TOutputDataSQLAgent.GetGenericBlockValuesSQL';
begin
  Result := '';
  try
    Result := 'SELECT A.Model,A.StudyAreaName,A.SubArea,A.Scenario,A.BlockNumber,A.LoadCaseNumber,A.SequenceNumber,A.Identifier,' +
              ' A.GenericValue01,A.GenericValue02,A.GenericValue03,A.GenericValue04,A.GenericValue05,'+
              ' A.GenericValue06,A.GenericValue07,A.GenericValue08,A.GenericValue09,A.GenericValue10,'+
              ' A.GenericValue11,A.GenericValue12,A.GenericValue13,A.GenericValue14,A.GenericValue15'+
              ' FROM suBlockGenericValues A'+
              ' WHERE ' +
              GetScenarioWhereClause +
              ' AND (A.BlockNumber = ' + IntToStr(ABlockNumber) + ')'+
              ' AND (A.LoadCaseNumber   = ' + IntToStr(ALoadCaseNumber) + ')'+
              ' AND (A.SequenceNumber   = ' + IntToStr(ASequenceNumber) + ')'+
              ' ORDER BY Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDataSQLAgent.GetBlockAverageChannelFlowSQL(ABlockNumber,ALoadCaseNumber,ASequenceNumber: integer): string;
const OPNAME = 'TOutputDataSQLAgent.GetBlockAverageChannelFlowSQL';
begin
  Result := '';
  try
    Result := 'SELECT A.Model,A.StudyAreaName,A.SubArea,A.Scenario,A.BlockNumber,A.LoadCaseNumber,A.SequenceNumber,' +
              ' A.AverageValue01,A.AverageValue02,A.AverageValue03,A.AverageValue04,A.AverageValue05,'+
              ' A.AverageValue06,A.AverageValue07,A.AverageValue08,A.AverageValue09,A.AverageValue10,'+
              ' A.AverageValue11,A.AverageValue12,A.AverageValue13,A.AverageValue14,A.AverageValue15'+
              ' FROM suBlockAvarageValues A'+
              ' WHERE ' +
              GetScenarioWhereClause +
              ' AND (A.BlockNumber = ' + IntToStr(ABlockNumber) + ')'+
              ' AND (A.LoadCaseNumber   = ' + IntToStr(ALoadCaseNumber) + ')'+
              ' AND (A.SequenceNumber   = ' + IntToStr(ASequenceNumber) + ')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDataSQLAgent.GetPlotElemetSQL: string;
const OPNAME = 'TOutputDataSQLAgent.GetPlotElemetSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,Identifier,ElementType,ElementNumber,ElementName'+
         ' FROM pltElement'+
         ' WHERE  (Model            ='+ QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
         '        (StudyAreaName    ='+QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
         '        (SubArea          ='+QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
         '        (Scenario         ='+QuotedStr(FAppModules.StudyArea.ScenarioCode)  +')'+
         ' ORDER BY Model,StudyAreaName,SubArea,Scenario,Identifier';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDataSQLAgent.GetSumOutBlobSQL: string;
const OPNAME = 'TOutputDataSQLAgent.GetSumOutBlobSQL';
begin
  Result := '';
  try
    Result := 'SELECT * FROM suComplete'+
              ' WHERE  (Model            ='+ QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
              '        (StudyAreaName    ='+QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
              '        (SubArea          ='+QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
              '        (Scenario         ='+QuotedStr(FAppModules.StudyArea.ScenarioCode)  +')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.



