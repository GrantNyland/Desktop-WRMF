//
//
//  UNIT      : Contains TRWHDataLoadAgent Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 09/01/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//

unit URWHDataLoadAgent;

interface

uses
  Classes,
  Contnrs,
  URWHDataObject,
  UAbstractObject;

type
  TRWHDataLoadAgent = class(TAbstractAppObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetScenarioWhereClause: string;
    function LoadAllRainfallStations(AModelData : TRWHModelData): boolean;
    function LoadSelectedRainfallStations(AModelData : TRWHModelData): boolean;
    function LoadRunConfigurationData(AModelData : TRWHModelData): boolean;
  public
    function LoadRWHData(AModelData : TRWHModelData): boolean;
  end;
implementation
uses
  VCL.Controls,
  SysUtils,
  VCL.Dialogs,
  UUtilities,
  UDataSetType,
  UConstants,
  URWHDataSQLAgent,
  UErrorHandlingOperations,
  DB;

{ TRWHDataLoadAgent }

procedure TRWHDataLoadAgent.CreateMemberObjects;
const OPNAME = 'TRWHDataLoadAgent.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRWHDataLoadAgent.DestroyMemberObjects;
const OPNAME = 'TRWHDataLoadAgent.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRWHDataLoadAgent.LoadRWHData(AModelData: TRWHModelData): boolean;
const OPNAME = 'TRWHDataLoadAgent.LoadRWHData';
begin
  Result := False;
  try
    Result := LoadAllRainfallStations(AModelData) and
              LoadSelectedRainfallStations(AModelData) and
              LoadRunConfigurationData(AModelData);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRWHDataLoadAgent.LoadAllRainfallStations(AModelData: TRWHModelData): boolean;
const OPNAME = 'TAllocationDefLoadAgent.LoadAllocationDefinitions';
var
  lDataSet     : TAbstractModelDataset;
  lSQL         : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) and ( AModelData <> nil ) then
      begin
        lSQL := 'SELECT * FROM RainfallStations' ;
        lDataSet.SetSQL(lSQL);
        lDataset.DataSet.Open;
        AModelData.PopulateAllRainfallStationListFromDataset(lDataset.DataSet);
        Result := True;
        lDataset.DataSet.Close;
      end;
    finally
      lDataset.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TRWHDataLoadAgent.LoadSelectedRainfallStations(AModelData: TRWHModelData): boolean;
const OPNAME = 'TAllocationDefLoadAgent.LoadSelectedRainfallStations';
var
  lDataSet     : TAbstractModelDataset;
  lSQL         : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) and ( AModelData <> nil ) then
      begin
        lSQL := 'SELECT * FROM RWHSelectedStations WHERE ' + GetScenarioWhereClause ;
        lDataSet.SetSQL(lSQL);
        lDataset.DataSet.Open;
        AModelData.PopulateSelectedRainfallStationListFromDataset(lDataset.DataSet);
        Result := True;
        lDataset.DataSet.Close;
      end;
    finally
      lDataset.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TRWHDataLoadAgent.LoadRunConfigurationData(AModelData: TRWHModelData): boolean;
const OPNAME = 'TAllocationDefLoadAgent.LoadAllocationDefinitions';
var
  lDataSet     : TAbstractModelDataset;
  lSQL         : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) and ( AModelData <> nil ) then
      begin
        lSQL := 'SELECT * FROM RWHRunConfig WHERE ' + GetScenarioWhereClause +
                ' ORDER BY Model,StudyAreaName,SubArea,Scenario,Identifier';
        lDataSet.SetSQL(lSQL);
        lDataset.DataSet.Open;
        AModelData.PopulateRunConfigurationDataFromDataset(lDataset.DataSet);
        Result := True;
        lDataset.DataSet.Close;
      end;
    finally
      lDataset.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TRWHDataLoadAgent.GetScenarioWhereClause: string;
const OPNAME = 'TRWHDataLoadAgent.GetScenarioWhereClause';
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

end.
