//
//
//  UNIT      : Contains TParameterDataSQLAgent Class
//  AUTHOR    : Dziedzi Ramulondi (Aravia)
//  DATE      : 2003/03/15
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UParameterDataSQLAgent;

interface

uses
  Classes,
  UAbstractObject;

type
  TParameterDataSQLAgent = class(TAbstractSQLAgent)
  protected
    function GetScenarioWhereClause: string;
  public
    procedure LoadContextData_FeatureID (AContextData : TStringList; AFeatureID: string);
    
    function GetParameterSQL: string;
    function GetParamHeaderSQL: string;
    function GetParamMatrixSQL (AMatrixType : Integer): string;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

function TParameterDataSQLAgent.GetScenarioWhereClause: string;
const OPNAME = 'TParameterDataSQLAgent.GetScenarioWhereClause';
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

function TParameterDataSQLAgent.GetParameterSQL: string;
const OPNAME = 'TParameterDataSQLAgent.GetParameterSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model, StudyAreaName, SubArea, Scenario, Identifier, GaugePathName, YearsNumber, YearStart,' +
              ' Residual1, Residual2, Variate1, Variate2, TransformType, TransformGamma, TransformDelta,' +
              ' TransformXlam, TransformXi, ResidualMean, ResidualStdDev, ArmaPhi1, ArmaPhi2, ArmaTheta1,' +
              ' ArmaTheta2, PhiZero, ZTVariates, ParamXA, ParamXSD, ParamAIC, ParamANC, CatchmentArea' +
              ' FROM ParamStochastics            ' +
              ' WHERE ' + GetScenarioWhereClause   +
              ' ORDER BY Identifier              ' ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TParameterDataSQLAgent.GetParamHeaderSQL: string;
const OPNAME = 'TParameterDataSQLAgent.GetParamHeaderSQL';
begin
  Result := '';
  try
    Result :=
      ' SELECT KeyGaugeCount, KeyGauges' +
      ' FROM ParamHeader'+
      ' WHERE ' + GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TParameterDataSQLAgent.GetParamMatrixSQL (AMatrixType : Integer): string;
const OPNAME = 'TParameterDataSQLAgent.GetParamMatrixSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model, StudyAreaName, SubArea, Scenario, Identifier, MatrixType,'+
              ' Matrix01, Matrix02, Matrix03, Matrix04, Matrix05,'+
              ' Matrix06, Matrix07, Matrix08, Matrix09, Matrix10'+
              ' FROM ParamMatrix'+
              ' WHERE ' + GetScenarioWhereClause +
              ' AND MatrixType = ' + IntToStr(AMatrixType) +
              ' ORDER BY Identifier' ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TParameterDataSQLAgent.LoadContextData_FeatureID(AContextData: TStringList; AFeatureID: string);
const OPNAME = 'TParameterDataSQLAgent.LoadContextData_FeatureID';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='         + AFeatureID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

end.
