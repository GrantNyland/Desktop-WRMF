//
//
//  UNIT      : Contains  TWQConstraintSQLAgent   Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 17/02/2014
//  COPYRIGHT : Copyright © 2004 DWAF
//
//


unit UWQConstraintSQLAgent;

interface
uses
  Classes,
  Contnrs,
  VoaimsCom_TLB,
  UAbstractObject;

type
  TWQConstraintSQLAgent = class(TAbstractSQLAgent)
  protected
    function GetScenarioWhereClause: string;
    function GetLastWQConChannelIdentifier : integer;
    function GetLastMinMaxBoundChannelIdentifier : integer;
  public
    procedure LoadContextData(AContextData : TStringList;AIdentifier: string);
    procedure LoadConfigContextData(AContextData : TStringList);

    function InsertMinMaxWQConstrainSQL : string;
    function InsertMinMaxBoundChannelSQL : string;
    function InsertWQConChannel(var AIdentifier,AChannelNumber : integer) : boolean;
    function InsertMinMaxBoundChannel(var AIdentifier,AChannelNumber : integer) : boolean;
    function DeleteMinMaxWQConstrainByChannelSQL(AMinMaxChannel:integer) : string;
    function DeleteMinMaxBoundChannelSQL(AMinMaxChannel:integer) : string;
    function DeleteMinMaxWQConstrainChannelByChannel(AMinMaxChannel:integer):boolean;
    function DeleteMinMaxBoundChannel(AMinMaxChannel:integer):boolean;
    function GetMinMaxWQConstrainSQL : string;
    function GetBoundChannelSQL : string;
    function GetLastMinMaxWQConstrainIdentifierSQL : string;
    function GetLastMinMaxBoundChannelIdentifierSQL : string;

  end;

implementation

uses
  Math,
  SysUtils,
  UConstants,
  UDataSetType,
  UGrowthFactorData,
  UGrowthFactorsExcelData,
  UErrorHandlingOperations;

{ TWQConstraintSQLAgent }



function TWQConstraintSQLAgent.GetMinMaxWQConstrainSQL: string;
const OPNAME = 'TWQConstraintSQLAgent.GetMinMaxWQConstrainSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model,StudyAreaName,SubArea,Scenario,Identifier,ChannelNumber,WQTarget,BlendingRefChannelCount,ReservoirRef,'+
              ' WQConType,ReferenceChannels,ReferenceChannelFactors,SlopeLimit,EstimatedRelease,Concentration '+
              ' FROM MinMaxWQConstrain A WHERE '+
              GetScenarioWhereClause;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWQConstraintSQLAgent.GetBoundChannelSQL: string;
const OPNAME = 'TWQConstraintSQLAgent.GetBoundChannelSQL';
begin
  Result := '';
  try
    Result := ' SELECT A.Model,A.StudyAreaName,A.SubArea,A.Scenario,A.Identifier,A.ChannelNumber,A.ReferenceChannelCount,A.ReferenceChannels '+
              ' FROM MinMaxBoundChannel A WHERE '+
              GetScenarioWhereClause;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TWQConstraintSQLAgent.GetLastMinMaxBoundChannelIdentifierSQL: string;
const OPNAME = 'TWQConstraintSQLAgent.GetLastMinMaxBoundChannelIdentifierSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(A.Identifier) AS LastID FROM MinMaxBoundChannel A WHERE ' +
               GetScenarioWhereClause;
  except on E: Exception do HandleError ( E, OPNAME ) end;

end;

function TWQConstraintSQLAgent.GetLastMinMaxWQConstrainIdentifierSQL: string;
const OPNAME = 'TWQConstraintSQLAgent.GetLastMinMaxWQConstrainIdentifierSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(A.Identifier) AS LastID FROM MinMaxWQConstrain A WHERE ' +
               GetScenarioWhereClause;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWQConstraintSQLAgent.GetLastWQConChannelIdentifier: integer;
const OPNAME = 'TWQConstraintSQLAgent.GetLastWQConChannelIdentifier';
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
        LSQL := GetLastMinMaxWQConstrainIdentifierSQL;
        LDataSet.SetSQL(LSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('LastID').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TWQConstraintSQLAgent.GetLastMinMaxBoundChannelIdentifier: integer;
const OPNAME = 'TWQConstraintSQLAgent.GetLastMinMaxBoundChannelIdentifier';
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
        LSQL := GetLastMinMaxBoundChannelIdentifierSQL;
        LDataSet.SetSQL(LSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('LastID').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



function TWQConstraintSQLAgent.GetScenarioWhereClause: string;
const OPNAME = 'TWQConstraintSQLAgent.GetScenarioWhereClause';
begin
  try
    Result :=
      ' (A.Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
      ' (A.StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      ' (A.SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      ' (A.Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') ';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWQConstraintSQLAgent.InsertMinMaxWQConstrainSQL: string;
const OPNAME = 'TWQConstraintSQLAgent.InsertMinMaxWQConstrainSQL';
begin
  Result := '';
  try
    Result := 'INSERT  INTO MinMaxWQConstrain ' +
              '(Model, StudyAreaName, SubArea, Scenario, Identifier, ChannelNumber,WQTarget,BlendingRefChannelCount,ReservoirRef,WQConType,SlopeLimit) ' +
              'VALUES (';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TWQConstraintSQLAgent.InsertMinMaxBoundChannelSQL: string;
const OPNAME = 'TWQConstraintSQLAgent.InsertMinMaxBoundChannelSQL';
begin
  Result := '';
  try
    Result := 'INSERT  INTO MinMaxBoundChannel ' +
              '(Model, StudyAreaName, SubArea, Scenario, Identifier, ChannelNumber,ReferenceChannelCount) ' +
              'VALUES (';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWQConstraintSQLAgent.LoadConfigContextData(AContextData: TStringList);
const OPNAME = 'TWQConstraintSQLAgent.LoadConfigContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWQConstraintSQLAgent.LoadContextData(AContextData: TStringList; AIdentifier: string);
const OPNAME = 'TWQConstraintSQLAgent.LoadContextData';
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


function TWQConstraintSQLAgent.DeleteMinMaxWQConstrainByChannelSQL(AMinMaxChannel:integer) : string;
const OPNAME = 'TWQConstraintSQLAgent.DeleteMinMaxWQConstrainByChannelSQL';
begin
  Result := '';
  try
    Result := 'DELETE * FROM MinMaxWQConstrain A WHERE ' +
               GetScenarioWhereClause +
               'AND (A.ChannelNumber = ' + IntToStr(AMinMaxChannel)+' )';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWQConstraintSQLAgent.DeleteMinMaxBoundChannelSQL(AMinMaxChannel:integer) : string;
const OPNAME = 'TWQConstraintSQLAgent.DeleteMinMaxBoundChannelSQL';
begin
  Result := '';
  try
    Result := 'DELETE * FROM MinMaxBoundChannel A WHERE ' +
               GetScenarioWhereClause +
               'AND (A.ChannelNumber = ' + IntToStr(AMinMaxChannel)+' )';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


//_____________________________________________________________________________________________//

function TWQConstraintSQLAgent.InsertWQConChannel(var AIdentifier,AChannelNumber : integer) : boolean;
const OPNAME = 'TWQConstraintSQLAgent.InsertWQConChannel';
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
        LIdentifier := GetLastWQConChannelIdentifier + 1;
        lSQL:= InsertMinMaxWQConstrainSQL +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
              IntToStr(LIdentifier) +','+IntToStr(AChannelNumber)+',0,0,0,0,0)';
        lDataSet.SetSQL(lSQL);
        lDataset.ExecSQL;
        lDataset.DataSet.Close;

        AIdentifier := LIdentifier;
        FAppModules.StudyArea.LastUpdateDate := Now();

        lImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if (lImportDate = NullDateTime) then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := TRUE;
      end;
    finally
      lDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;



function TWQConstraintSQLAgent.InsertMinMaxBoundChannel(var AIdentifier,AChannelNumber : integer) : boolean;
const OPNAME = 'TWQConstraintSQLAgent.InsertMinMaxBoundChannel';
var
  LDataSet : TAbstractModelDataset;
  LIdentifier : integer;
  LImportDate : TDateTime;
  LSQL : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LIdentifier := GetLastMinMaxBoundChannelIdentifier + 1;
        LSQL:= InsertMinMaxBoundChannelSQL +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
              IntToStr(LIdentifier) + ','+IntToStr(AChannelNumber)+',0 )';

        LDataSet.SetSQL(lSQL);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

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
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;


function TWQConstraintSQLAgent.DeleteMinMaxWQConstrainChannelByChannel(AMinMaxChannel:integer):boolean;
const OPNAME = 'TWQConstraintSQLAgent.DeleteMinMaxWQConstrainChannelByChannel';
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
        lSQL := DeleteMinMaxWQConstrainByChannelSQL(AMinMaxChannel);
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

function TWQConstraintSQLAgent.DeleteMinMaxBoundChannel(AMinMaxChannel:integer):boolean;
const OPNAME = 'TWQConstraintSQLAgent.DeleteMinMaxBoundChannel';
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
        lSQL := DeleteMinMaxBoundChannelSQL(AMinMaxChannel);
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
