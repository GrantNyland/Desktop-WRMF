//
//
//  UNIT      : Contains TReturnFlowChannelSQLAgent Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 14/06/2006
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UReturnFlowChannelSQLAgent;

interface
uses
  Classes,
  Contnrs,
  UAbstractObject;

type
  TReturnFlowChannelSQLAgent = class(TAbstractSQLAgent)
  protected
    function GetScenarioWhereClause: string;
    function GetLastReturnFlowChannelIdentifier : integer;
    function InsertReturnFlowChannelDetailSQL : string;
    function InsertReturnFlowCorrespondingChannelSQL: string;
    function InsertReturnFlowMonthlyEvaporationSQL : string;
    function DeleteReturnFlowByChannelSQL(ADemandChannel : integer) : string;
    function DeleteReturnFlowMonthlyEvaporationByChannelSQL(ADemandChannel : integer) : string;
    function DeleteReturnFlowCorrespondingChannelByChannelSQL(ADemandChannel : integer) : string;
    function DeleteCorrespondingChannelByChannelSQL(ACorrespondingChannel:integer) : string;
  public
    function GetReturnFlowChannelSQL : string;
    function GetReturnFlowMonthlyEvaporationSQL : string;
    function GetReturnFlowCorrespondingChannelSQL : string;
    function GetLastReturnFlowChannelIdentifierSQL : string;
    function InsertReturnFlowChannelDetail(ADemandChannel : integer; var AIdentifier: integer) : boolean;
    function InsertReturnFlowCorrespondingChannel(ADemandChannel,AChannel,AIdentifier: integer): boolean;
    function DeleteReturnFlowByChannel(ADemandChannel : integer) : boolean;
    function DeleteCorrespondingChannelByChannel(ACorrespondingChannel : integer) : boolean;
    procedure LoadContextData(AContextData : TStringList;AIdentifier: string);
    procedure LoadContextData_DemandChannel(AContextData: TStringList; AIdentifier, AChannel, AFieldNameID: string);
    procedure LoadContextData_CorrespondingChannels(AContextData: TStringList;
                                                    AIdentifier, ADemandChannel, AChannelNumber : string);

end;
implementation
uses
  SysUtils,
  UConstants,
  UDataSetType,
  UErrorHandlingOperations;

{ TReturnFlowChannelSQLAgent }

function TReturnFlowChannelSQLAgent.DeleteReturnFlowByChannel(ADemandChannel: integer): boolean;
const OPNAME = 'TReturnFlowChannelSQLAgent.DeleteReturnFlowByChannel';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
  lSQL        : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL := DeleteReturnFlowByChannelSQL(ADemandChannel);
        LDataSet.SetSQL(LSQL);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        LSQL := DeleteReturnFlowMonthlyEvaporationByChannelSQL(ADemandChannel);
        LDataSet.ClearSQL;
        LDataSet.SetSQL(LSQL);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        LSQL := DeleteReturnFlowCorrespondingChannelByChannelSQL(ADemandChannel);
        LDataSet.ClearSQL;
        LDataSet.SetSQL(LSQL);
        LDataset.ExecSQL;

        FAppModules.StudyArea.LastUpdateDate := Now();
        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannelSQLAgent.DeleteCorrespondingChannelByChannel(ACorrespondingChannel : integer) : boolean;
const OPNAME = 'TReturnFlowChannelSQLAgent.DeleteCorrespondingChannelByChannel';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
  lSQL        : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL := DeleteCorrespondingChannelByChannelSQL(ACorrespondingChannel);
        LDataSet.ClearSQL;
        LDataSet.SetSQL(LSQL);
        LDataset.ExecSQL;
        FAppModules.StudyArea.LastUpdateDate := Now();
        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);
        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannelSQLAgent.DeleteReturnFlowByChannelSQL(ADemandChannel: integer): string;
const OPNAME = 'TReturnFlowChannelSQLAgent.DeleteReturnFlowByChannelSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM ReturnFlowChannelDetail A WHERE ' +
               GetScenarioWhereClause +
               ' AND A.DemandChannel = ' + IntToStr(ADemandChannel);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannelSQLAgent.GetReturnFlowChannelSQL : string;
const OPNAME = 'TReturnFlowChannelSQLAgent.GetReturnFlowChannelSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model,StudyAreaName,SubArea,Scenario,Identifier,DemandChannel,'+  //CorrespondingChannels,
              ' GaugeNumber,MonthlyAvrgFactor,CalibrationFactor,MonthlyAvrgFactorEvap,RoutingConstant,'+
              ' CurtailmentFactor,MultiplicationFactor '+
              ' FROM ReturnFlowChannelDetail A WHERE '+
              GetScenarioWhereClause;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannelSQLAgent.GetReturnFlowMonthlyEvaporationSQL : string;
const OPNAME = 'TReturnFlowChannelSQLAgent.GetReturnFlowMonthlyEvaporationSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model,StudyAreaName,SubArea,Scenario,Identifier,DemandChannel,'+
              ' PotentialMonthlyEvap01,PotentialMonthlyEvap02,PotentialMonthlyEvap03,'+
              ' PotentialMonthlyEvap04,PotentialMonthlyEvap05,PotentialMonthlyEvap06,PotentialMonthlyEvap07,'+
              ' PotentialMonthlyEvap08,PotentialMonthlyEvap09,PotentialMonthlyEvap10,PotentialMonthlyEvap11,'+
              ' PotentialMonthlyEvap12 '+
              ' FROM ReturnFlowMonthlyEvaporation A WHERE '+
                GetScenarioWhereClause;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannelSQLAgent.GetReturnFlowCorrespondingChannelSQL : string;
const OPNAME = 'TReturnFlowChannelSQLAgent.GetReturnFlowCorrespondingChannelSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model,StudyAreaName,SubArea,Scenario,Identifier,DemandChannel,'+
              ' ChannelNumber,AbstractionChannel,AssumedFactor '+
              ' FROM ReturnFlowCorrespondingChannel A WHERE '+
              GetScenarioWhereClause;
     except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TReturnFlowChannelSQLAgent.DeleteReturnFlowMonthlyEvaporationByChannelSQL(ADemandChannel : integer) : string;
const OPNAME = 'TReturnFlowChannelSQLAgent.DeleteReturnFlowMonthlyEvaporationByChannelSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM ReturnFlowMonthlyEvaporation A WHERE ' +
               GetScenarioWhereClause +
               ' AND DemandChannel = ' + IntToStr(ADemandChannel);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannelSQLAgent.DeleteReturnFlowCorrespondingChannelByChannelSQL(ADemandChannel : integer) : string;
const OPNAME = 'TReturnFlowChannelSQLAgent.DeleteReturnFlowCorrespondingChannelByChannelSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM ReturnFlowCorrespondingChannel A WHERE ' +
               GetScenarioWhereClause +
               ' AND DemandChannel = ' + IntToStr(ADemandChannel);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannelSQLAgent.DeleteCorrespondingChannelByChannelSQL(ACorrespondingChannel:integer) : string;
const OPNAME = 'TReturnFlowChannelSQLAgent.DeleteCorrespondingChannelByChannelSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM ReturnFlowCorrespondingChannel A WHERE ' +
               GetScenarioWhereClause +
               ' AND ChannelNumber = ' + IntToStr(ACorrespondingChannel);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;



function TReturnFlowChannelSQLAgent.GetLastReturnFlowChannelIdentifier: integer;
const OPNAME = 'TReturnFlowChannelSQLAgent.GetLastReturnFlowChannelIdentifier';
var
  LDataSet : TAbstractModelDataset;
  LSQL     : string;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL := GetLastReturnFlowChannelIdentifierSQL;
        LDataSet.SetSQL(LSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('LastID').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannelSQLAgent.GetLastReturnFlowChannelIdentifierSQL: string;
const OPNAME = 'TReturnFlowChannelSQLAgent.GetLastReturnFlowChannelIdentifierSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS LastID FROM ReturnFlowChannelDetail A WHERE ' +
               GetScenarioWhereClause;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannelSQLAgent.GetScenarioWhereClause: string;
const OPNAME = 'TReturnFlowChannelSQLAgent.GetScenarioWhereClause';
begin
  Result := '';
  try
    Result :=
      ' (A.Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
      ' (A.StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      ' (A.SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      ' (A.Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') ';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannelSQLAgent.InsertReturnFlowChannelDetail(ADemandChannel: integer; var AIdentifier: integer): boolean;
const OPNAME = 'TReturnFlowChannelSQLAgent.InsertReturnFlowChannelDetail';
var
  LDataSet : TAbstractModelDataset;
  LIdentifier : integer;
  LImportDate : TDateTime;
  LSQL : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LIdentifier := GetLastReturnFlowChannelIdentifier + 1;
        LSQL:= InsertReturnFlowChannelDetailSQL +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
              IntToStr(LIdentifier) + ','+
              IntToStr(ADemandChannel)+',0,0,0,0,0,0,0)';

        LDataSet.SetSQL(lSQL);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;
        LSQL:= InsertReturnFlowMonthlyEvaporationSQL +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
              IntToStr(LIdentifier) + ','+
              IntToStr(ADemandChannel)+',0,0,0,0,0,0,0,0,0,0,0,0)';

        LDataSet.ClearSQL;
        LDataSet.SetSQL(lSQL);
        LDataset.ExecSQL;

        AIdentifier := LIdentifier;
        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if (lImportDate = NullDateTime) then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannelSQLAgent.InsertReturnFlowCorrespondingChannel(ADemandChannel,AChannel,AIdentifier: integer): boolean;
const OPNAME = 'TReturnFlowChannelSQLAgent.InsertReturnFlowCorrespondingChannel';
var
  LDataSet : TAbstractModelDataset;
  LImportDate : TDateTime;
  LSQL : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL:= InsertReturnFlowCorrespondingChannelSQL +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
              IntToStr(AIdentifier) + ','+
              IntToStr(ADemandChannel)+','+
               IntToStr(AChannel)+',0,0)';

        LDataSet.SetSQL(lSQL);
        LDataset.ExecSQL;
        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if (lImportDate = NullDateTime) then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TReturnFlowChannelSQLAgent.InsertReturnFlowChannelDetailSQL: string;
const OPNAME = 'TReturnFlowChannelSQLAgent.InsertReturnFlowChannelDetailSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO ReturnFlowChannelDetail '+
              '(Model,StudyAreaName,SubArea,Scenario,Identifier,DemandChannel,GaugeNumber,MonthlyAvrgFactor,'+
              ' CalibrationFactor,MonthlyAvrgFactorEvap,RoutingConstant,CurtailmentFactor,MultiplicationFactor)'+
              ' VALUES (';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannelSQLAgent.InsertReturnFlowMonthlyEvaporationSQL : string;
const OPNAME = 'TReturnFlowChannelSQLAgent.InsertReturnFlowMonthlyEvaporationSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO ReturnFlowMonthlyEvaporation '+
              '(Model,StudyAreaName,SubArea,Scenario,Identifier,DemandChannel,PotentialMonthlyEvap01,'+
              ' PotentialMonthlyEvap02,PotentialMonthlyEvap03,PotentialMonthlyEvap04,PotentialMonthlyEvap05,'+
              ' PotentialMonthlyEvap06,PotentialMonthlyEvap07,PotentialMonthlyEvap08,PotentialMonthlyEvap09,'+
              ' PotentialMonthlyEvap10,PotentialMonthlyEvap11,PotentialMonthlyEvap12)'+
              ' VALUES (';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannelSQLAgent.InsertReturnFlowCorrespondingChannelSQL: string;
const OPNAME = 'TReturnFlowChannelSQLAgent.InsertReturnFlowCorrespondingChannelSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO ReturnFlowCorrespondingChannel '+
              '(Model,StudyAreaName,SubArea,Scenario,Identifier,DemandChannel,ChannelNumber,AbstractionChannel,AssumedFactor)'+
              ' VALUES (';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelSQLAgent.LoadContextData(AContextData: TStringList; AIdentifier: string);
const OPNAME = 'TReturnFlowChannelSQLAgent.LoadContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='          + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='  + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='        + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='       + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='     + AIdentifier);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelSQLAgent.LoadContextData_DemandChannel(AContextData: TStringList; AIdentifier, AChannel,AFieldNameID : string);
const OPNAME = 'TReturnFlowChannelSQLAgent.LoadContextData_DemandChannel';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='          + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='  + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='        + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='       + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='     + AIdentifier);
    AContextData.Add('DemandChannel='  + AChannel);
    AContextData.Add('FieldNameIdentifier=' + AFieldNameID);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelSQLAgent.LoadContextData_CorrespondingChannels(AContextData: TStringList;
                                                                           AIdentifier, ADemandChannel, AChannelNumber : string);
const OPNAME = 'TReturnFlowChannelSQLAgent.LoadContextData_CorrespondingChannels';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='          + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='  + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='        + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='       + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='     + AIdentifier);
    AContextData.Add('DemandChannel='  + ADemandChannel);
    AContextData.Add('ChannelNumber='  + AChannelNumber);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
