//
//
//  UNIT      : Contains TTariffCalculationSQLAgent Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 14/06/2006
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UTariffCalculationSQLAgent;

interface
uses
  Classes,
  Contnrs,
  UAbstractObject;

type
  TTariffCalculationSQLAgent = class(TAbstractSQLAgent)
  protected
    function GetScenarioWhereClause: string;
    function GetLastTariffCalculationlIdentifierSQL : string;
    function GetLastTariffCalculationlIdentifier: integer;
    function InsertTariffCalculationSQL: string;
    function DeleteTariffCalculationSQL(AIdentifier: integer): string;
  public
    function GetTariffCalculationSQL : string;
    function GetTariffConfigurationSQL : string;
    procedure LoadContextData(AContextData : TStringList;AIdentifier: string);
    function InsertTariffCalculation(AChannelNumber : integer; var AIdentifier: integer) : boolean;
    function DeleteTariffCalculation(AIdentifier : integer) : boolean;
end;

implementation

uses
  SysUtils,
  UConstants,
  UDataSetType,
  UErrorHandlingOperations;

{ TTariffCalculationSQLAgent }

function TTariffCalculationSQLAgent.GetScenarioWhereClause: string;
const OPNAME = 'TTariffCalculationSQLAgent.GetScenarioWhereClause';
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

function TTariffCalculationSQLAgent.GetTariffCalculationSQL : string;
const OPNAME = 'TTariffCalculationSQLAgent.GetTariffCalculationSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model,StudyAreaName,SubArea,Scenario,Identifier,ChannelNumber,'+
              ' Tariff,EscalationFactors'+
              ' FROM TariffCalculation A WHERE '+
              GetScenarioWhereClause;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TTariffCalculationSQLAgent.GetTariffConfigurationSQL: string;
const OPNAME = 'TTariffCalculationSQLAgent.GetTariffConfigurationSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model,StudyAreaName,SubArea,Scenario,DataYears'+
              ' FROM TariffCalculationConfig A WHERE '+
              GetScenarioWhereClause;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TTariffCalculationSQLAgent.LoadContextData(AContextData: TStringList; AIdentifier: string);
const OPNAME = 'TTariffCalculationSQLAgent.LoadContextData';
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

function TTariffCalculationSQLAgent.GetLastTariffCalculationlIdentifierSQL: string;
const OPNAME = 'TTariffCalculationSQLAgent.GetLastTariffCalculationlIdentifierSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS LastID FROM TariffCalculation A WHERE ' +
               GetScenarioWhereClause;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TTariffCalculationSQLAgent.InsertTariffCalculationSQL: string;
const OPNAME = 'TTariffCalculationSQLAgent.InsertTariffCalculationSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO TariffCalculation '+
              '(Model,StudyAreaName,SubArea,Scenario,Identifier,ChannelNumber,Tariff)'+
              ' VALUES (';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TTariffCalculationSQLAgent.DeleteTariffCalculationSQL(AIdentifier: integer): string;
const OPNAME = 'TTariffCalculationSQLAgent.DeleteTariffCalculationSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM TariffCalculation A WHERE ' +
               GetScenarioWhereClause +
               ' AND A.Identifier = ' + IntToStr(AIdentifier);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TTariffCalculationSQLAgent.GetLastTariffCalculationlIdentifier: integer;
const OPNAME = 'TTariffCalculationSQLAgent.GetLastTariffCalculationlIdentifier';
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
        LSQL := GetLastTariffCalculationlIdentifierSQL;
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

function TTariffCalculationSQLAgent.InsertTariffCalculation(AChannelNumber : integer; var AIdentifier: integer) : boolean;
const OPNAME = 'TTariffCalculationSQLAgent.InsertTariffCalculation';
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
        LIdentifier := GetLastTariffCalculationlIdentifier + 1;
        LSQL:= InsertTariffCalculationSQL +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
              IntToStr(LIdentifier) + ','+
              IntToStr(AChannelNumber)+',0.0)';

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

function TTariffCalculationSQLAgent.DeleteTariffCalculation(AIdentifier: integer): boolean;
const OPNAME = 'TTariffCalculationSQLAgent.DeleteTariffCalculation';
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
        LSQL := DeleteTariffCalculationSQL(AIdentifier);
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
{
function TTariffCalculationSQLAgent.DeleteCorrespondingChannelByChannel(ACorrespondingChannel : integer) : boolean;
const OPNAME = 'TTariffCalculationSQLAgent.DeleteCorrespondingChannelByChannel';
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

function TTariffCalculationSQLAgent.GetReturnFlowMonthlyEvaporationSQL : string;
const OPNAME = 'TTariffCalculationSQLAgent.GetReturnFlowMonthlyEvaporationSQL';
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

function TTariffCalculationSQLAgent.GetReturnFlowCorrespondingChannelSQL : string;
const OPNAME = 'TTariffCalculationSQLAgent.GetReturnFlowCorrespondingChannelSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model,StudyAreaName,SubArea,Scenario,Identifier,DemandChannel,'+
              ' ChannelNumber,AbstractionChannel,AssumedFactor '+
              ' FROM ReturnFlowCorrespondingChannel A WHERE '+
              GetScenarioWhereClause;
     except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TTariffCalculationSQLAgent.DeleteReturnFlowMonthlyEvaporationByChannelSQL(ADemandChannel : integer) : string;
const OPNAME = 'TTariffCalculationSQLAgent.DeleteReturnFlowMonthlyEvaporationByChannelSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM ReturnFlowMonthlyEvaporation A WHERE ' +
               GetScenarioWhereClause +
               ' AND DemandChannel = ' + IntToStr(ADemandChannel);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TTariffCalculationSQLAgent.DeleteReturnFlowCorrespondingChannelByChannelSQL(ADemandChannel : integer) : string;
const OPNAME = 'TTariffCalculationSQLAgent.DeleteReturnFlowCorrespondingChannelByChannelSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM ReturnFlowCorrespondingChannel A WHERE ' +
               GetScenarioWhereClause +
               ' AND DemandChannel = ' + IntToStr(ADemandChannel);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TTariffCalculationSQLAgent.DeleteCorrespondingChannelByChannelSQL(ACorrespondingChannel:integer) : string;
const OPNAME = 'TTariffCalculationSQLAgent.DeleteCorrespondingChannelByChannelSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM ReturnFlowCorrespondingChannel A WHERE ' +
               GetScenarioWhereClause +
               ' AND ChannelNumber = ' + IntToStr(ACorrespondingChannel);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TTariffCalculationSQLAgent.InsertReturnFlowCorrespondingChannel(ADemandChannel,AChannel,AIdentifier: integer): boolean;
const OPNAME = 'TTariffCalculationSQLAgent.InsertReturnFlowCorrespondingChannel';
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

function TTariffCalculationSQLAgent.InsertReturnFlowMonthlyEvaporationSQL : string;
const OPNAME = 'TTariffCalculationSQLAgent.InsertReturnFlowMonthlyEvaporationSQL';
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

function TTariffCalculationSQLAgent.InsertReturnFlowCorrespondingChannelSQL: string;
const OPNAME = 'TTariffCalculationSQLAgent.InsertReturnFlowCorrespondingChannelSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO ReturnFlowCorrespondingChannel '+
              '(Model,StudyAreaName,SubArea,Scenario,Identifier,DemandChannel,ChannelNumber,AbstractionChannel,AssumedFactor)'+
              ' VALUES (';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TTariffCalculationSQLAgent.LoadContextData_DemandChannel(AContextData: TStringList; AIdentifier, AChannel,AFieldNameID : string);
const OPNAME = 'TTariffCalculationSQLAgent.LoadContextData_DemandChannel';
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

procedure TTariffCalculationSQLAgent.LoadContextData_CorrespondingChannels(AContextData: TStringList;
                                                                           AIdentifier, ADemandChannel, AChannelNumber : string);
const OPNAME = 'TTariffCalculationSQLAgent.LoadContextData_CorrespondingChannels';
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
end;}

end.
