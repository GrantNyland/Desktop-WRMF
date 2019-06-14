//
//
//  UNIT      : Contains TDDTSDamDataSQLAgent Class
//  AUTHOR    : Sam Dhlamini (bcx)
//  DATE      : 07/04/2014
//  COPYRIGHT : Copyright © 2004 DWAF
//
//
unit UDDTSDamDataSQLAgent;

interface
uses
  Classes,
  Contnrs,
  VoaimsCom_TLB,

  UFilesActionAbstractManager,
  UFileNames,
  UFileNameConstants,

  UFilePathsDatabaseAgent,
  UDDTSData,

  UUtilities,
  UNetworkFeaturesSQLAgent,
//  UNetworkFeaturesLoadAgent,
  UDataFileObjects,
  UAbstractObject;
type
 TDDTSDamDataSQLAgent = class(TAbstractSQLAgent)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetScenarioWhereClause: string;
  public
    procedure LoadContextData(AContextData: TStringList; AIdentifier: integer);
    function GetDDTSDamDataSQL(AIdentifier : integer): string;
    function InsertDamDataSQL(AIdentifier : integer) : string;
    function InsertInputMinMaxSQL(AIdentifier : integer): string;
    function DeleteDamDataSQL(AIdentifier : integer) : string;
    function DeleteInputMinMaxSQL(AIdentifier : integer): string;
    function GetDDTSInputMinMaxSQL(AIdentifier : integer): string;
    function GetDDTSInputData: string;

 end;

implementation
uses
  Math,
  SysUtils,
  UConstants,
  UDataSetType,
  UErrorHandlingOperations;

{ TDDTSDamDataSQLAgent }

procedure TDDTSDamDataSQLAgent.CreateMemberObjects;
const OPNAME = 'TDDTSDamDataSQLAgent.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try

  except on E : Exception do HandleError(E,OPNAME); end;
end;


procedure TDDTSDamDataSQLAgent.LoadContextData(AContextData: TStringList; AIdentifier: integer);
const OPNAME = 'TDDTSDamDataSQLAgent.LoadContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier=' + IntToStr(AIdentifier));
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TDDTSDamDataSQLAgent.DestroyMemberObjects;
const OPNAME = 'TDDTSDamDataSQLAgent.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try

  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TDDTSDamDataSQLAgent.GetScenarioWhereClause: string;
const OPNAME = 'TDDTSDamDataSQLAgent.GetScenarioWhereClause';
begin
  Result := '';
  try
    Result :=
      ' (A.Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
      ' (A.StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      ' (A.SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      ' (A.Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ')';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDDTSDamDataSQLAgent.GetDDTSInputMinMaxSQL(AIdentifier : integer): string;
const OPNAME = 'TDDTSDamDataSQLAgent.GetDDTSInputMinMaxSQL';
begin
  Result := '';
  try
    Result := 'SELECT A.* FROM DDTSInputMinMax A WHERE ' +
              GetScenarioWhereClause   + ' AND ' +
              ' (A.Identifier = ' + IntToStr(AIdentifier) +')';

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TDDTSDamDataSQLAgent.GetDDTSDamDataSQL(AIdentifier : integer): string;
const OPNAME = 'TDDTSDamDataSQLAgent.GetDDTSDamDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT A.* FROM DDTSDetails A WHERE ' +
              GetScenarioWhereClause   + ' AND ' +
              ' (A.Identifier = ' + IntToStr(AIdentifier) +')';

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TDDTSDamDataSQLAgent.GetDDTSInputData: string;
const OPNAME = 'TDDTSDamDataSQLAgent.GetDDTSInputData';
begin
  Result := '';
  try
    Result := 'SELECT A.* FROM DDTSInputData A WHERE ' +
              GetScenarioWhereClause+
              ' ORDER BY ROWID ';

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDDTSDamDataSQLAgent.InsertDamDataSQL(AIdentifier : integer): string;
const OPNAME = 'TDDTSDamDataSQLAgent.InsertDamDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT  INTO DDTSDetails '+
              '(Model, StudyAreaName, SubArea, Scenario, Identifier, RunoffScaleFactor,OtherInflowScaleFactor,EWRScaleFactor,TargetDraft,DSRequirment' +
              ' ,DSPercRelease,SpillPercRelease,EWRPercRelease,ImportHeadlines '+
              ' ) ' +
              'VALUES ('+
    QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
    QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
    QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
    QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
    IntToStr(AIdentifier)+',0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,'+QuotedStr('N')+')';

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TDDTSDamDataSQLAgent.InsertInputMinMaxSQL(AIdentifier : integer): string;
const OPNAME = 'TDDTSDamDataSQLAgent.InsertInputMinMaxSQL';
begin
  Result := '';
  try
    Result := 'INSERT  INTO DDTSInputMinMax '+
              '(Model, StudyAreaName, SubArea, Scenario, Identifier, MinRunoff,MaxRunoff,MinOtherInflow,MaxOtherInflow,MinRainfall' +
              ' ,MaxRainfall,MaxEvaporation,MinEvaporation,MaxIncreamentalRunoff,MinIncreamentalRunoff,MaxEWR,MinEWR '+
              ' ) ' +
              'VALUES ('+
    QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
    QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
    QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
    QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
    IntToStr(AIdentifier)+',0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0)';

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDDTSDamDataSQLAgent.DeleteDamDataSQL(AIdentifier: integer): string;
const OPNAME = 'TDDTSDamDataSQLAgent.DeleteDamDataSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM DDTSDetails WHERE '+
      ' (Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
      ' (StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      ' (SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      ' (Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND ' +
      ' (Identifier    = ' + IntToStr(AIdentifier)+')';

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDDTSDamDataSQLAgent.DeleteInputMinMaxSQL(AIdentifier: integer): string;
const OPNAME = 'TDDTSDamDataSQLAgent.DeleteDamDataSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM DDTSInputMinMax WHERE '+
      ' (Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
      ' (StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      ' (SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      ' (Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND ' +
      ' (Identifier    = ' + IntToStr(AIdentifier)+')';

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
