//
//
//  UNIT      : Contains TDisbenefitFunctionDefinitionSQLAgent Class
//  AUTHOR    : Presley Mudau
//  DATE      : 2006/06/08
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UDisbenefitFunctionDefinitionSQLAgent;

interface

uses
  Classes,
  VoaimsCom_TLB,
  UAbstractObject;

type
  TDisbenefitFunctionDefinitionSQLAgent = class(TAbstractSQLAgent)
  private
  protected
    function GetDeleteDisbenefitDefinitionDataSQL(AChannelNumber: integer): string;
    function GetScenarioWhereClause: string;
  public
    procedure LoadContextData_ChannelNo(AContextData: TStringList;
                              AChannelNumber : string);
    procedure LoadContextData_TDSConcentration(AContextData: TStringList; AChannelNumber,AFieldNameID : string);
    function GetDisbenefitDefinitionDataSQL: string;
    function InsertDisbenefitFunction(AChannelID,
                                      AChannelNr : integer): boolean;
    function DeleteDisbenefitFunctionDefinition(AChannelNumber: integer): boolean;


  end;

implementation

uses
  Math,
  SysUtils,
  UConstants,
  UDataSetType,
  UErrorHandlingOperations;

function TDisbenefitFunctionDefinitionSQLAgent.GetDisbenefitDefinitionDataSQL: string;
const OPNAME = 'TDisbenefitFunctionDefinitionSQLAgent.GetDisbenefitDefinitionDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT ' +
      ' Model, StudyAreaName, SubArea, Scenario, Identifier, ChannelNumber,' +
      ' EquationDisbenefitX, EquationDisbenefitY,' +
      ' EquationDisbenefitCost, EquationDisbenefitNonSupply,' +
      ' EscalationRate,YearActive,MonthActive,YearObsolete,MonthObsolete,WQConstraint, ' +
      ' TDSConcentration01, TDSConcentration02,TDSConcentration03,TDSConcentration04 ' +
      ' FROM DisbenefitFunction A WHERE '+
       GetScenarioWhereClause;;
    except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFunctionDefinitionSQLAgent.LoadContextData_ChannelNo(AContextData : TStringList;
                                                                AChannelNumber   : string);
const OPNAME = 'TDisbenefitFunctionDefinitionSQLAgent.LoadContextData_ChannelNo';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='         +  FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +  FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='       +  FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='      +  FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('ChannelNumber=' +  AChannelNumber);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TDisbenefitFunctionDefinitionSQLAgent.LoadContextData_TDSConcentration(AContextData: TStringList; AChannelNumber,AFieldNameID : string);
const OPNAME = 'TDisbenefitFunctionDefinitionSQLAgent.LoadContextData_TDSConcentration';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='          + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='  + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='        + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='       + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('ChannelNumber=' +  AChannelNumber);
    AContextData.Add('FieldNameIdentifier=' + AFieldNameID);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TDisbenefitFunctionDefinitionSQLAgent.DeleteDisbenefitFunctionDefinition(AChannelNumber: integer): boolean;
const OPNAME = 'TDisbenefitFunctionDefinitionSQLAgent.DeleteDisbenefitFunctionDefinition';
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
        LDataSet.SetSQL(GetDeleteDisbenefitDefinitionDataSQL(AChannelNumber));
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

function TDisbenefitFunctionDefinitionSQLAgent.GetDeleteDisbenefitDefinitionDataSQL(AChannelNumber: integer): string;
const OPNAME = 'TDisbenefitFunctionDefinitionSQLAgent.GetDeleteDisbenefitDefinitionDataSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM DisbenefitFunction ' +
      ' WHERE Model       =  ' + QuotedStr(FAppModules.StudyArea.ModelCode)+
      ' AND StudyAreaName =  ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
      ' AND SubArea       =  ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)+
      ' AND Scenario      =  ' + QuotedStr(FAppModules.StudyArea.ScenarioCode) +
      ' AND Channelnumber    =  ' + IntToStr(AChannelNumber);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDisbenefitFunctionDefinitionSQLAgent.GetScenarioWhereClause: string;
const OPNAME = 'TDisbenefitFunctionDefinitionSQLAgent.GetScenarioWhereClause';
begin
  try
    Result :=
      ' (A.Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
      ' (A.StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      ' (A.SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      ' (A.Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDisbenefitFunctionDefinitionSQLAgent.InsertDisbenefitFunction(AChannelID,
                                                                        AChannelNr: integer): boolean;
const OPNAME = 'TDisbenefitFunctionDefinitionSQLAgent.InsertDisbenefitFunction';
var
  LDataSet     : TAbstractModelDataset;
  LImportDate  : TDateTime;
  lSQL         : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        lSQL := 'INSERT INTO DisbenefitFunction '+
                  '(Model, StudyAreaName, SubArea, Scenario, Identifier, ChannelNumber,'                 +
                  'YearActive, MonthActive, YearObsolete, MonthObsolete, EquationDisbenefitX,'           +
                  'EquationDisbenefitY, EquationDisbenefitCost, EquationDisbenefitNonSupply, WQConstraint,' +
                  'TDSConcentration01, TDSConcentration02, TDSConcentration03, TDSConcentration04'      +
                  ') VALUES (' +
                  QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
                  QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
                  QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
                  QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
                  IntToStr(AChannelID) + ',' + IntToStr(AChannelNr) + ','+
                  '0,0,0,0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0'+')';

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


end.


