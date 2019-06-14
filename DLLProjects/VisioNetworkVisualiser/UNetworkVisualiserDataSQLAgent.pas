//
//
//  UNIT      : Contains TNetworkVisualiserDataSQLAgent Class
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 2005/02/28
//  COPYRIGHT : Copyright © 2005 DWAF
//
//
unit UNetworkVisualiserDataSQLAgent;

interface

uses
  Classes,
  UAbstractObject,
  UNetworkVisualiserData;

type
  TNetworkVisualiserDataSQLAgent = class(TAbstractSQLAgent)
  protected
  public
  	function GetScenarioWhereClause: string;
    function GetDrawingGroupSQL: string;
    function GetDrawingSQL: string;
    function GetMaxDrawingGroupIdSQL: string;
    function GetMaxDrawingIdSQL: string;
    function CreateDrawingGroupSQL: string;
    function DeleteDrawingGroupSQL: string;
    function DeleteDrawingInGroupSQL: string;
    function CreateDrawingSQL: string;
    function DeleteDrawingSQL: string;
    function UpdateDrawingNameSQL: string;
    function UpdateDrawingGISSQL: string;
    function UpdateGroupNameSQL: string;
    function SetDrawingReadOnlySQL: string;
    function RemoveDrawingReadOnlySQL: string;
    function PopulateDrawing(ADrawing:TDrawing; AGroupName, ADrawingName:string):boolean;
    function UpdateDrawingGIS(ADrawing:TDrawing):boolean;
  end;

implementation

uses
  SysUtils,
  UDataSetType,
  UErrorHandlingOperations;

function TNetworkVisualiserDataSQLAgent.GetScenarioWhereClause: string;
const OPNAME = 'TNetworkVisualiserDataSQLAgent.GetScenarioWhereClause';
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

function TNetworkVisualiserDataSQLAgent.GetDrawingGroupSQL: string;
const OPNAME = 'TNetworkVisualiserDataSQLAgent.GetDrawingGroupSQL';
begin
  Result := '';
  try
    Result := 'SELECT DrawingGroupID,DrawingGroupName FROM VNVDrawingGroup WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserDataSQLAgent.GetDrawingSQL: string;
const OPNAME = 'TNetworkVisualiserDataSQLAgent.GetDrawingSQL';
begin
  Result := '';
  try
    Result := 'SELECT DrawingGroupID,DrawingID,DrawingName,GISMode FROM VNVDrawing WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserDataSQLAgent.GetMaxDrawingGroupIdSQL: string;
const OPNAME = 'TNetworkVisualiserDataSQLAgent.GetMaxDrawingGroupIdSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(DrawingGroupID) AS MAXDrawingGroupID FROM VNVDrawingGroup WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserDataSQLAgent.GetMaxDrawingIdSQL: string;
const OPNAME = 'TNetworkVisualiserDataSQLAgent.GetMaxDrawingIdSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(DrawingID) AS MAXDrawingID FROM VNVDrawing WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserDataSQLAgent.CreateDrawingGroupSQL: string;
const OPNAME = 'TNetworkVisualiserDataSQLAgent.CreateDrawingGroupSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO VNVDrawingGroup'+
              ' (Model,StudyAreaName,SubArea,Scenario,DrawingGroupID,DrawingGroupName)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:DrawingGroupID,:DrawingGroupName)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserDataSQLAgent.DeleteDrawingGroupSQL: string;
const OPNAME = 'TNetworkVisualiserDataSQLAgent.DeleteDrawingGroupSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM VNVDrawingGroup WHERE '+
              ' Model              = '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName  = '+ QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea        = '+ QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario       = '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND DrawingGroupID = :DrawingGroupID';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserDataSQLAgent.CreateDrawingSQL: string;
const OPNAME = 'TNetworkVisualiserDataSQLAgent.CreateDrawingSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO VNVDrawing'+
              ' (Model,StudyAreaName,SubArea,Scenario,DrawingGroupID,DrawingID,DrawingName,GISMode)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:DrawingGroupID,:DrawingID,:DrawingName,:GISMode)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserDataSQLAgent.DeleteDrawingSQL: string;
const OPNAME = 'TNetworkVisualiserDataSQLAgent.DeleteDrawingSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM VNVDrawing WHERE '+
              ' Model              = '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName  = '+ QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea        = '+ QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario       = '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND DrawingGroupID = :DrawingGroupID'+
              ' AND DrawingID      = :DrawingID';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserDataSQLAgent.SetDrawingReadOnlySQL: string;
const OPNAME = 'TNetworkVisualiserDataSQLAgent.SetDrawingReadOnlySQL';
begin
  Result := '';
  try
    Result := 'UPDATE VNVDrawing '+
              ' SET ReadOnly = 1 WHERE '+
              ' Model              = '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName  = '+ QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea        = '+ QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario       = '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND DrawingGroupID = :DrawingGroupID'+
              ' AND DrawingID      = :DrawingID';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserDataSQLAgent.RemoveDrawingReadOnlySQL: string;
const OPNAME = 'TNetworkVisualiserDataSQLAgent.RemoveDrawingReadOnlySQL';
begin
  Result := '';
  try
    Result := 'UPDATE VNVDrawing '+
              ' SET ReadOnly = 0 WHERE '+
              ' Model              = '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName  = '+ QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea        = '+ QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario       = '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND DrawingGroupID = :DrawingGroupID'+
              ' AND DrawingID      = :DrawingID';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserDataSQLAgent.DeleteDrawingInGroupSQL: string;
const OPNAME = 'TNetworkVisualiserDataSQLAgent.DeleteDrawingInGroupSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM VNVDrawing WHERE '+
              ' Model              = '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName  = '+ QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea        = '+ QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario       = '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND DrawingGroupID = :DrawingGroupID';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserDataSQLAgent.UpdateDrawingNameSQL: string;
const OPNAME = 'TNetworkVisualiserDataSQLAgent.UpdateDrawingNameSQL';
begin
  Result := '';
  try
    Result := 'UPDATE VNVDrawing'+
              ' SET DrawingName =  :NewName WHERE '+
              ' Model              = '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName  = '+ QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea        = '+ QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario       = '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND DrawingGroupID = :DrawingGroupID'+
              ' AND DrawingID      = :DrawingID';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserDataSQLAgent.UpdateDrawingGISSQL: string;
const OPNAME = 'TNetworkVisualiserDataSQLAgent.UpdateDrawingGISSQL';
begin
  Result := '';
  try
    Result := 'UPDATE VNVDrawing'+
              ' SET GISMode =  :AGISMode WHERE '+
              ' Model              = '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName  = '+ QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea        = '+ QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario       = '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND DrawingGroupID = :DrawingGroupID'+
              ' AND DrawingID      = :DrawingID';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserDataSQLAgent.UpdateGroupNameSQL: string;
const OPNAME = 'TNetworkVisualiserDataSQLAgent.UpdateGroupNameSQL';
begin
  Result := '';
  try
    Result := 'UPDATE  VNVDrawingGroup'+
              ' SET DrawingGroupName = :NewName WHERE '+
              ' Model              = '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName  = '+ QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea        = '+ QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario       = '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND DrawingGroupID = :DrawingGroupID';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserDataSQLAgent.PopulateDrawing(ADrawing: TDrawing; AGroupName, ADrawingName: string): boolean;
const OPNAME = 'TNetworkVisualiserDataSQLAgent.PopulateDrawing';
var
  LSQL: string;
  LDataSet : TAbstractModelDataset;
  LDrawingID,
  LDrawingGroupID: integer;
  LDrawingName: string;
  LGISMode: boolean;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL := GetDrawingGroupSQL + ' AND DrawingGroupName = '+ QuotedStr(AGroupName);
        LDataSet.SetSQL(LSQL);
        LDataSet.DataSet.Open;
        if not LDataSet.DataSet.Eof then
        begin
          LDrawingGroupID := LDataSet.DataSet.FieldByName('DrawingGroupID').AsInteger;
          LDataSet.DataSet.Close;

          LSQL := GetDrawingSQL;
          LSQL := LSQL + ' AND DrawingGroupID = '+IntToStr(LDrawingGroupID) + ' AND DrawingName = '+ QuotedStr(ADrawingName);
          LDataSet.SetSQL(LSQL);
          LDataSet.DataSet.Open;
          if not LDataSet.DataSet.Eof then
          begin
            LDrawingGroupID := LDataSet.DataSet.FieldByName('DrawingGroupID').AsInteger;
            LDrawingID      := LDataSet.DataSet.FieldByName('DrawingID').AsInteger;
            LDrawingName    := Trim(LDataSet.DataSet.FieldByName('DrawingName').AsString);
            LGISMode          := (Trim(LDataSet.DataSet.FieldByName('GISMode').AsString) = 'Y');
            ADrawing.Populate(LDrawingGroupID,LDrawingID,LDrawingName,LGISMode);
            Result := True;
          end;
        end;
      end;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserDataSQLAgent.UpdateDrawingGIS(ADrawing:TDrawing) : boolean;
const OPNAME = 'TNetworkVisualiserDataSQLAgent.UpdateDrawingGIS';
var
  LDataSet : TAbstractModelDataset;
  LGISMode : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        if ADrawing.GISMode then
          LGISMode := 'Y'
        else
          LGISMode := 'N';

        LDataSet.SetSQL(UpdateDrawingGISSQL);
        LDataSet.ClearQueryParams;
        LDataSet.SetParams(['DrawingGroupID'], [IntToStr(ADrawing.DrawingGroupID)]);
        LDataSet.SetParams(['DrawingID'], [IntToStr(ADrawing.DrawingID)]);
        LDataSet.SetParams(['AGISMode'], [LGISMode]);
        if LDataSet.AreAllParamsBound then
        begin
          LDataSet.ExecSQL;
          Result := True;
        end;
      end;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
